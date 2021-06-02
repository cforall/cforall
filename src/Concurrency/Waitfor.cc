//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Waitfor.cc --
//
// Author           : Thierry Delisle
// Created On       : Mon Aug 28 11:06:52 2017
// Last Modified By :
// Last Modified On :
// Update Count     : 12
//

#include "Concurrency/Keywords.h"

#include <cassert>                 // for assert
#include <string>                  // for string, operator==

using namespace std::string_literals;

#include "Common/PassVisitor.h"    // for PassVisitor
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/UniqueName.h"	   // for UniqueName
#include "Common/utility.h"        // for deleteAll, map_range
#include "CodeGen/OperatorTable.h" // for isConstructor
#include "InitTweak/InitTweak.h"   // for getPointerBase
#include "ResolvExpr/Resolver.h"   // for findVoidExpression
#include "SynTree/LinkageSpec.h"   // for Cforall
#include "SynTree/Constant.h"      // for Constant
#include "SynTree/Declaration.h"   // for StructDecl, FunctionDecl, ObjectDecl
#include "SynTree/Expression.h"    // for VariableExpr, ConstantExpr, Untype...
#include "SynTree/Initializer.h"   // for SingleInit, ListInit, Initializer ...
#include "SynTree/Label.h"         // for Label
#include "SynTree/Statement.h"     // for CompoundStmt, DeclStmt, ExprStmt
#include "SynTree/Type.h"          // for StructInstType, Type, PointerType
#include "SynTree/Visitor.h"       // for Visitor, acceptAll

class Attribute;
/*
void foo() {
	while( true ) {
		when( a < 1 ) waitfor( f : a ) { bar(); }
		or timeout( swagl() );
		or waitfor( g : a ) { baz(); }
		or waitfor( ^?{} : a ) { break; }
		or waitfor( ^?{} ) { break; }
	}
}

void f(int i, float f, A & mutex b, struct foo *  );
void f(int );


                      |  |
                      |  |
			    |  |
                      |  |
                      |  |
                    \ |  | /
                     \    /
                      \  /
                       \/


void foo() {
	while( true ) {
		{
			acceptable_t acceptables[3];
			if( a < 1 ) {
				acceptables[0].func = f;
				acceptables[0].mon = a;
			}
			acceptables[1].func = g;
			acceptables[1].mon = a;

			acceptables[2].func = f;
			acceptables[2].mon = a;
			acceptables[2].is_dtor = true;

			int ret = waitfor_internal( acceptables, swagl() );

			switch( ret ) {
				case 0:
				{
					bar();
				}
				case 1:
				{
					baz();
				}
				case 2:
					signal(a);
					{
						break;
					}
			}
		}
	}
}*/

namespace Concurrency {
	//=============================================================================================
	// Pass declarations
	//=============================================================================================

	class GenerateWaitForPass final : public WithIndexer {
	  public:

		void premutate( FunctionDecl * decl );
		void premutate( StructDecl   * decl );

		Statement * postmutate( WaitForStmt * stmt );

		static void generate( std::list< Declaration * > & translationUnit ) {
			PassVisitor< GenerateWaitForPass > impl;
			acceptAll( translationUnit, impl );
		}

		ObjectDecl * declare( unsigned long count, CompoundStmt * stmt );
		ObjectDecl * declareFlag( CompoundStmt * stmt );
		Statement  * makeSetter( ObjectDecl * flag );
		ObjectDecl * declMon( WaitForStmt::Clause & clause, CompoundStmt * stmt );
		void         init( ObjectDecl * acceptables, int index, WaitForStmt::Clause & clause, Statement * settter, CompoundStmt * stmt );
		Expression * init_timeout( Expression *& time, Expression *& time_cond, bool has_else, Expression *& else_cond, Statement * settter, CompoundStmt * stmt );
		Expression * call(size_t count, ObjectDecl * acceptables, Expression * timeout, CompoundStmt * stmt);
		void         choose( WaitForStmt * waitfor, Expression  * result, CompoundStmt * stmt );

		static void implement( std::list< Declaration * > & translationUnit ) {
			PassVisitor< GenerateWaitForPass > impl;
			mutateAll( translationUnit, impl );
		}


	  private:
	  	FunctionDecl        * decl_waitfor    = nullptr;
	  	StructDecl          * decl_mask       = nullptr;
		StructDecl          * decl_acceptable = nullptr;
		StructDecl          * decl_monitor    = nullptr;

		static std::unique_ptr< Type > generic_func;

		UniqueName namer_acc = "__acceptables_"s;
		UniqueName namer_idx = "__index_"s;
		UniqueName namer_flg = "__do_run_"s;
		UniqueName namer_msk = "__mask_"s;
		UniqueName namer_mon = "__monitors_"s;
		UniqueName namer_tim = "__timeout_"s;
	};

	//=============================================================================================
	// General entry routine
	//=============================================================================================
	void generateWaitFor( std::list< Declaration * > & translationUnit ) {
		GenerateWaitForPass	::implement( translationUnit );
	}

	//=============================================================================================
	// Generic helper routine
	//=============================================================================================

	namespace {
		Expression * makeOpIndex( DeclarationWithType * array, unsigned long index ) {
			return new UntypedExpr(
				new NameExpr( "?[?]" ),
				{
					new VariableExpr( array ),
					new ConstantExpr( Constant::from_ulong( index ) )
				}
			);
		}

		Expression * makeOpAssign( Expression * lhs, Expression * rhs ) {
			return new UntypedExpr(
					new NameExpr( "?=?" ),
					{ lhs, rhs }
			);
		}

		Expression * makeOpMember( Expression * sue, const std::string & mem ) {
			return new UntypedMemberExpr( new NameExpr( mem ), sue );
		}

		Statement * makeAccStatement( DeclarationWithType * object, unsigned long index, const std::string & member, Expression * value, const SymTab::Indexer & indexer ) {
			Expression * expr = makeOpAssign(
				makeOpMember(
					makeOpIndex(
						object,
						index
					),
					member
				),
				value
			);

			ResolvExpr::findVoidExpression( expr, indexer );

			return new ExprStmt( expr );
		}

		Expression * safeCond( Expression * expr, bool ifnull = true ) {
			if( expr ) return expr;

			return new ConstantExpr( Constant::from_bool( ifnull ) );
		}

		VariableExpr * extractVariable( Expression * func ) {
			if( VariableExpr * var = dynamic_cast< VariableExpr * >( func ) ) {
				return var;
			}

			CastExpr * cast = strict_dynamic_cast< CastExpr * >( func );
			return strict_dynamic_cast< VariableExpr * >( cast->arg );
		}

		Expression * detectIsDtor( Expression * func ) {
			VariableExpr * typed_func = extractVariable( func );
			bool is_dtor = InitTweak::isDestructor( typed_func->var );
			return new ConstantExpr( Constant::from_bool( is_dtor ) );
		}
	};


	//=============================================================================================
	// Generate waitfor implementation
	//=============================================================================================

	void GenerateWaitForPass::premutate( FunctionDecl * decl) {
		if( decl->name != "__waitfor_internal" ) return;

		decl_waitfor = decl;
	}

	void GenerateWaitForPass::premutate( StructDecl   * decl ) {
		if( ! decl->body ) return;

		if( decl->name == "__acceptable_t" ) {
			assert( !decl_acceptable );
			decl_acceptable = decl;
		}
		else if( decl->name == "__waitfor_mask_t" ) {
			assert( !decl_mask );
			decl_mask = decl;
		}
		else if( decl->name == "$monitor" ) {
			assert( !decl_monitor );
			decl_monitor = decl;
		}
	}

	Statement * GenerateWaitForPass::postmutate( WaitForStmt * waitfor ) {
		if( !decl_monitor || !decl_acceptable || !decl_mask )
			SemanticError( waitfor, "waitfor keyword requires monitors to be in scope, add #include <monitor.hfa>" );

		CompoundStmt * stmt = new CompoundStmt();

		ObjectDecl * acceptables = declare( waitfor->clauses.size(), stmt );
		ObjectDecl * flag        = declareFlag( stmt );
		Statement  * setter      = makeSetter( flag );

		int index = 0;
		for( auto & clause : waitfor->clauses ) {
			init( acceptables, index, clause, setter, stmt );

			index++;
		}

		Expression * timeout = init_timeout(
			waitfor->timeout.time,
			waitfor->timeout.condition,
			waitfor->orelse .statement,
			waitfor->orelse .condition,
			setter,
			stmt
		);

		CompoundStmt * compound = new CompoundStmt();
		stmt->push_back( new IfStmt(
			safeCond( new VariableExpr( flag ) ),
			compound,
			nullptr
		));

		Expression * result = call( waitfor->clauses.size(), acceptables, timeout, compound );

		choose( waitfor, result, compound );

		return stmt;
	}

	ObjectDecl * GenerateWaitForPass::declare( unsigned long count, CompoundStmt * stmt )
	{
		ObjectDecl * acceptables = ObjectDecl::newObject(
			namer_acc.newName(),
			new ArrayType(
				noQualifiers,
				new StructInstType(
					noQualifiers,
					decl_acceptable
				),
				new ConstantExpr( Constant::from_ulong( count ) ),
				false,
				false
			),
			nullptr
		);

		stmt->push_back( new DeclStmt( acceptables) );

		Expression * set = new UntypedExpr(
			new NameExpr( "__builtin_memset" ),
			{
				new VariableExpr( acceptables ),
				new ConstantExpr( Constant::from_int( 0 ) ),
				new SizeofExpr( new VariableExpr( acceptables ) )
			}
		);

		ResolvExpr::findVoidExpression( set, indexer );

		stmt->push_back( new ExprStmt( set ) );

		return acceptables;
	}

	ObjectDecl * GenerateWaitForPass::declareFlag( CompoundStmt * stmt ) {
		ObjectDecl * flag = ObjectDecl::newObject(
			namer_flg.newName(),
			new BasicType(
				noQualifiers,
				BasicType::Bool
			),
			new SingleInit( new ConstantExpr( Constant::from_ulong( 0 ) ) )
		);

		stmt->push_back( new DeclStmt( flag) );

		return flag;
	}

	Statement * GenerateWaitForPass::makeSetter( ObjectDecl * flag ) {
		Expression * expr = new UntypedExpr(
			new NameExpr( "?=?" ),
			{
				new VariableExpr( flag ),
				new ConstantExpr( Constant::from_ulong( 1 ) )
			}
		);

		ResolvExpr::findVoidExpression( expr, indexer );

		return new ExprStmt( expr );
	}

	ObjectDecl * GenerateWaitForPass::declMon( WaitForStmt::Clause & clause, CompoundStmt * stmt ) {

		ObjectDecl * mon = ObjectDecl::newObject(
			namer_mon.newName(),
			new ArrayType(
				noQualifiers,
				new PointerType(
					noQualifiers,
					new StructInstType(
						noQualifiers,
						decl_monitor
					)
				),
				new ConstantExpr( Constant::from_ulong( clause.target.arguments.size() ) ),
				false,
				false
			),
			new ListInit(
				map_range < std::list<Initializer*> > ( clause.target.arguments, [this](Expression * expr ){
					Expression * init = new CastExpr(
						new UntypedExpr(
							new NameExpr( "get_monitor" ),
							{ expr }
						),
						new PointerType(
							noQualifiers,
							new StructInstType(
								noQualifiers,
								decl_monitor
							)
						),
						false
					);

					ResolvExpr::findSingleExpression( init, indexer );
					return new SingleInit( init );
				})
			)
		);

		stmt->push_back( new DeclStmt( mon) );

		return mon;
	}

	void GenerateWaitForPass::init( ObjectDecl * acceptables, int index, WaitForStmt::Clause & clause, Statement * setter, CompoundStmt * stmt ) {

		ObjectDecl * monitors = declMon( clause, stmt );

		Type * fptr_t = new PointerType( noQualifiers, new FunctionType( noQualifiers, true ) );

		stmt->push_back( new IfStmt(
			safeCond( clause.condition ),
			new CompoundStmt({
				makeAccStatement( acceptables, index, "is_dtor", detectIsDtor( clause.target.function )                                    , indexer ),
				makeAccStatement( acceptables, index, "func"   , new CastExpr( clause.target.function, fptr_t, false )                     , indexer ),
				makeAccStatement( acceptables, index, "data"   , new VariableExpr( monitors )                                              , indexer ),
				makeAccStatement( acceptables, index, "size"   , new ConstantExpr( Constant::from_ulong( clause.target.arguments.size() ) ), indexer ),
				setter->clone()
			}),
			nullptr
		));

		clause.target.function = nullptr;
		clause.target.arguments.empty();
		clause.condition = nullptr;
	}

	Expression * GenerateWaitForPass::init_timeout(
		Expression *& time,
		Expression *& time_cond,
		bool has_else,
		Expression *& else_cond,
		Statement * setter,
		CompoundStmt * stmt
	) {
		ObjectDecl * timeout = ObjectDecl::newObject(
			namer_tim.newName(),
			new BasicType(
				noQualifiers,
				BasicType::LongLongUnsignedInt
			),
			new SingleInit(
				new ConstantExpr( Constant::from_int( -1 ) )
			)
		);

		stmt->push_back( new DeclStmt( timeout ) );

		if( time ) {
			stmt->push_back( new IfStmt(
				safeCond( time_cond ),
				new CompoundStmt({
					new ExprStmt(
						makeOpAssign(
							new VariableExpr( timeout ),
							time
						)
					),
					setter->clone()
				}),
				nullptr
			));

			time = time_cond = nullptr;
		}

		if( has_else ) {
			stmt->push_back( new IfStmt(
				safeCond( else_cond ),
				new CompoundStmt({
					new ExprStmt(
						makeOpAssign(
							new VariableExpr( timeout ),
							new ConstantExpr( Constant::from_ulong( 0 ) )
						)
					),
					setter->clone()
				}),
				nullptr
			));

			else_cond = nullptr;
		}

		delete setter;

		return new VariableExpr( timeout );
	}

	Expression * GenerateWaitForPass::call(
		size_t count,
		ObjectDecl * acceptables,
		Expression * timeout,
		CompoundStmt * stmt
	) {
		ObjectDecl * index = ObjectDecl::newObject(
			namer_idx.newName(),
			new BasicType(
				noQualifiers,
				BasicType::ShortSignedInt
			),
			new SingleInit(
				new ConstantExpr( Constant::from_int( -1 ) )
			)
		);

		stmt->push_back( new DeclStmt( index ) );

		ObjectDecl * mask = ObjectDecl::newObject(
			namer_msk.newName(),
			new StructInstType(
				noQualifiers,
				decl_mask
			),
			new ListInit({
				new SingleInit( new AddressExpr( new VariableExpr( index ) ) ),
				new ListInit({
					new SingleInit( new VariableExpr( acceptables ) ),
					new SingleInit( new ConstantExpr( Constant::from_ulong( count ) ) )
				})
			})
		);

		stmt->push_back( new DeclStmt( mask ) );

		stmt->push_back( new ExprStmt(
			new ApplicationExpr(
				VariableExpr::functionPointer( decl_waitfor ),
				{
					new CastExpr(
						new VariableExpr( mask ),
						new ReferenceType(
							noQualifiers,
							new StructInstType(
								noQualifiers,
								decl_mask
							)
						),
						false
					),
					timeout
				}
			)
		));

		return new VariableExpr( index );
	}

	void GenerateWaitForPass::choose(
		WaitForStmt * waitfor,
		Expression  * result,
		CompoundStmt * stmt
	) {
		SwitchStmt * swtch = new SwitchStmt(
			result,
			std::list<Statement *>()
		);

		unsigned long i = 0;
		for( auto & clause : waitfor->clauses ) {
			swtch->statements.push_back(
				new CaseStmt(
					new ConstantExpr( Constant::from_ulong( i++ ) ),
					{
						new CompoundStmt({
							clause.statement,
							new BranchStmt(
								"",
								BranchStmt::Break
							)
						})
					}
				)
			);
		}

		if(waitfor->timeout.statement) {
			swtch->statements.push_back(
				new CaseStmt(
					new ConstantExpr( Constant::from_int( -2 ) ),
					{
						new CompoundStmt({
							waitfor->timeout.statement,
							new BranchStmt(
								"",
								BranchStmt::Break
							)
						})
					}
				)
			);
		}

		if(waitfor->orelse.statement) {
			swtch->statements.push_back(
				new CaseStmt(
					new ConstantExpr( Constant::from_int( -1 ) ),
					{
						new CompoundStmt({
							waitfor->orelse.statement,
							new BranchStmt(
								"",
								BranchStmt::Break
							)
						})
					}
				)
			);
		}

		stmt->push_back( swtch );
	}
};

// Local Variables: //
// mode: c //
// tab-width: 4 //
// End: //
