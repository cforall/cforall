//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Corun.cpp -- generate code needed by the actor system
//
// Author           : Colby Parsons
// Created On       : Monday October 9 15:16:42 2023
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Dec 14 17:32:17 2023
// Update Count     : 1
//

#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"
#include "Common/UniqueName.hpp"
using namespace ast;
using namespace std;

namespace Concurrency {

struct CorunKeyword : public WithDeclsToAdd<>, public WithStmtsToAdd<> {
	UniqueName CorunFnNamer = "__CFA_corun_lambda_"s;
	UniqueName CoforFnNamer = "__CFA_cofor_lambda_"s;
	// UniqueName CoforFnVarNamer = "__CFA_cofor_lambda_var"s;
	UniqueName RunnerBlockNamer = "__CFA_corun_block_"s;

	string coforArgName = "__CFA_cofor_lambda_arg";
	string numProcsName = "__CFA_cofor_num_procs";
	string currProcsName = "__CFA_cofor_curr_procs";
	string thdArrName = "__CFA_cofor_thread_array";
	string loopTempName = "__CFA_cofor_loop_temp";


	const StructDecl * runnerBlockDecl = nullptr;
	const StructDecl * coforRunnerDecl = nullptr;

	// Finds runner_block (corun task) and cofor_runner (cofor task) decls
	void previsit( const StructDecl * decl ) {
		if ( !decl->body ) {
			return;
		} else if ( "runner_block" == decl->name ) {
			assert( !runnerBlockDecl );
			runnerBlockDecl = decl;
		} else if ( "cofor_runner" == decl->name ) {
			assert( !coforRunnerDecl );
			coforRunnerDecl = decl;
		}
	}

	// codegen for cofor statements
	Stmt * postvisit( const CoforStmt * stmt ) {
		if ( !runnerBlockDecl || !coforRunnerDecl )
			SemanticError( stmt->location, "To use cofor statements add #include <cofor.hfa>" );

		if ( stmt->inits.size() != 1 )
			SemanticError( stmt->location, "Cofor statements must have a single initializer in the loop control" );

		if ( !stmt->body )
			return nullptr;

		const CodeLocation & loc = stmt->location;
		const string fnName = CoforFnNamer.newName();

		CompoundStmt * body = new CompoundStmt( loc );

		// push back cofor initializer to generated body
		body->push_back( deepCopy( stmt->inits.at(0) ) );

		CompoundStmt * fnBody = new CompoundStmt( loc );

		const DeclStmt * declStmtPtr = dynamic_cast<const DeclStmt *>(stmt->inits.at(0).get());
		if ( ! declStmtPtr )
			SemanticError( stmt->location, "Cofor statement initializer is somehow not a decl statement?" );

		const Decl * declPtr = dynamic_cast<const Decl *>(declStmtPtr->decl.get());
		if ( ! declPtr )
			SemanticError( stmt->location, "Cofor statement initializer is somehow not a decl?" );

		Type * initType = new TypeofType( new NameExpr( loc, declPtr->name ) );

		// Generates:
		// typeof(init) __CFA_cofor_lambda_var = *((typeof(init) *)val);
		fnBody->push_back( new DeclStmt( loc,
			new ObjectDecl( loc,
				declPtr->name,
				initType,
				new SingleInit( loc,
					UntypedExpr::createDeref( loc,
						new CastExpr( loc,
							new NameExpr( loc, coforArgName ),
							new PointerType( initType ), ExplicitCast
						)
					)
				)
			)
		));

		// push rest of cofor body into loop lambda
		fnBody->push_back( deepCopy( stmt->body ) );

		// Generates:
		// void __CFA_cofor_lambda_() {
		//    typeof(init) __CFA_cofor_lambda_var = *((typeof(init) *)val);
		//    stmt->body;
		// }
		Stmt * coforLambda = new DeclStmt( loc,
			new FunctionDecl( loc,
				fnName,                                             // name
				{
					new ObjectDecl( loc,
						coforArgName,
						new ast::PointerType( new ast::VoidType() )
					)
				},                                                  // params
				{},                                                 // return
				fnBody   // body
			)
		);
		body->push_back( coforLambda );

		// Generates:
		// unsigned __CFA_cofor_num_procs = get_proc_count();
		body->push_back( new DeclStmt( loc,
				new ObjectDecl( loc,
					numProcsName,
					new BasicType( BasicKind::UnsignedInt ),
					new SingleInit( loc,
						new UntypedExpr( loc,
							new NameExpr( loc, "get_proc_count" ),
							{}
						)
					)
				)
			)
		);

		// Generates:
		// unsigned __CFA_cofor_curr_procs = 0;
		body->push_back( new DeclStmt( loc,
				new ObjectDecl( loc,
					currProcsName,
					new BasicType( BasicKind::UnsignedInt ),
					new SingleInit( loc, ConstantExpr::from_int( loc, 0 ) )
				)
			)
		);

		// Generates:
		// unsigned cofor_runner __CFA_cofor_thread_array[nprocs];
		body->push_back( new DeclStmt( loc,
				new ObjectDecl( loc,
					thdArrName,
					new ast::ArrayType(
						new StructInstType( coforRunnerDecl ),
						new NameExpr( loc, numProcsName ),
						ast::FixedLen,
						ast::DynamicDim
					)
				)
			)
		);

		// Generates:
		// start_runners( __CFA_cofor_thread_array, __CFA_cofor_num_procs, __CFA_cofor_lambda_ );
		body->push_back( new ExprStmt( loc,
			new UntypedExpr( loc,
				new NameExpr( loc, "start_runners" ),
				{
					new NameExpr( loc, thdArrName ),
					new NameExpr( loc, numProcsName ),
					new NameExpr( loc, fnName )
				}
			)
		));

		// Generates:
		// typeof(initializer) * __CFA_cofor_loop_temp = malloc();
		CompoundStmt * forLoopBody = new CompoundStmt( loc );
		forLoopBody->push_back( new DeclStmt( loc,
				new ObjectDecl( loc,
					loopTempName,
					new PointerType( initType ),
					new SingleInit( loc,
						new UntypedExpr( loc,
							new NameExpr( loc, "malloc" ),
							{}
						)
					)
				)
			)
		);

		// Generates:
		// *__CFA_cofor_loop_temp = initializer;
		forLoopBody->push_back( new ExprStmt( loc,
			UntypedExpr::createAssign( loc,
				UntypedExpr::createDeref( loc, new NameExpr( loc, loopTempName ) ),
				new NameExpr( loc, declPtr->name )
			)
		));

		// Generates:
		// send_work( __CFA_cofor_thread_array, __CFA_cofor_num_procs,
		//     __CFA_cofor_curr_procs, __CFA_cofor_loop_temp );
		forLoopBody->push_back( new ExprStmt( loc,
			new UntypedExpr( loc,
				new NameExpr( loc, "send_work" ),
				{
					new NameExpr( loc, thdArrName ),
					new NameExpr( loc, numProcsName ),
					new NameExpr( loc, currProcsName ),
					new NameExpr( loc, loopTempName )
				}
			)
		));

		body->push_back( new ForStmt( loc,
			{},
			deepCopy( stmt->cond ),
			deepCopy( stmt->inc ),
			forLoopBody
		));

		// Generates:
		// end_runners( __CFA_cofor_thread_array, __CFA_cofor_num_procs );
		body->push_back( new ExprStmt( loc,
			new UntypedExpr( loc,
				new NameExpr( loc, "end_runners" ),
				{
					new NameExpr( loc, thdArrName ),
					new NameExpr( loc, numProcsName )
				}
			)
		));

		return body;
	}

	// codegen for corun statements
	Stmt * postvisit( const CorunStmt * stmt ) {
		if ( !runnerBlockDecl || !coforRunnerDecl )
			SemanticError( stmt->location, "To use corun statements add #include <cofor.hfa>" );

		if ( !stmt->stmt )
			return nullptr;

		const CodeLocation & loc = stmt->location;
		const string fnName = CorunFnNamer.newName();
		const string objName = RunnerBlockNamer.newName();

		// Generates:
		// void __CFA_corun_lambda_() { ... stmt->stmt ... }
		Stmt * runnerLambda = new DeclStmt( loc,
			new FunctionDecl( loc,
				fnName,                                             // name
				{},                                                 // params
				{},                                                 // return
				new CompoundStmt( loc, { deepCopy(stmt->stmt) } )   // body
			)
		);

		// Generates:
		// runner_block __CFA_corun_block_;
		Stmt * objDecl = new DeclStmt( loc,
			new ObjectDecl( loc,
				objName,
				new StructInstType( runnerBlockDecl )
			)
		);

		// Generates:
		// __CFA_corun_block_{ __CFA_corun_lambda_ };
		Stmt * threadStart = new ExprStmt( loc,
			new UntypedExpr ( loc,
				new NameExpr( loc, "?{}" ),
				{
					new NameExpr( loc, objName ),
					new NameExpr( loc, fnName )
				}
			)
		);

		stmtsToAddBefore.push_back( runnerLambda );
		stmtsToAddBefore.push_back( objDecl );

		return threadStart;
	}
};

void implementCorun( TranslationUnit & translationUnit ) {
	Pass<CorunKeyword>::run( translationUnit );
}

} // namespace Concurrency
