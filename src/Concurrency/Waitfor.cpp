//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Waitfor.cpp -- Expand waitfor clauses into code.
//
// Author           : Andrew Beach
// Created On       : Fri May 27 10:31:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jun 13 13:30:00 2022
// Update Count     : 0
//

#include "Waitfor.hpp"

#include <string>

#include "AST/Pass.hpp"
#include "Common/UniqueName.hpp"
#include "InitTweak/InitTweak.hpp"
#include "ResolvExpr/Resolver.hpp"

#include "AST/Print.hpp"

using namespace std::string_literals;
using ResolvExpr::ResolveContext;

/* So this is what this file dones:

void f(int i, float f, A & mutex b, struct foo *  );
void f(int );

...{
	when ( a < 1 ) waitfor( f : a ) { fee(); }
	or timeout( getWaitTime() ) { fy(); }
	or waitfor( g : a ) { foe(); }
	or waitfor( ^?{} : a ) { break; }
	or waitfor( ^?{} ) { break; }
	or when ( a < 1 ) else { fum(); }
}...

		 ||
		 ||
		\||/
		 \/

...{
	{
		__acceptable_t __acceptables_#[4 <num-clauses>];
		bool __do_run_# = false;

		monitor$ * __monitors_#[1 <num-monitors>] = { a };
		if ( a < 1) {
			void (*__function_#)() = <casts> f;
			__acceptables_#[0].is_dtor = false;
			__acceptables_#[0].func = __function_#;
			__acceptables_#[0].data = __monitors_#;
			__acceptables_#[0].size = 1;
			__do_run_# = true;
		}

		// Remaining waitfor clauses go here.

		long long unsigned int __timeout_# = -1;
		if ( true ) {
			__timeout_# = getWaitTime();
			__do_run_# = true;
		}

		if ( a < 1 ) {
			__timeout_# = 0
			__do_run_# = true;
		}

		short int __index_# = -1;
		__waitfor_mask_t __mask_# = {&__index_#, {__acceptables_#, ?}};
		__waitfor_internal((__waitfor_mask_t&)__mask_#, __timeout_#);

		switch (__index_#) {
		case 0:
			{ { fee(); } break; }
		case 1:
			{ { foe(); } break; }
		case 2:
			{ <modified-break> break; }
		case 3:
			{ <modified-break> break; }
		case -2:
			{ { fy(); } break; }
		case -1:
			{ { foe(); } break; }
		}
	}
}...
*/

namespace Concurrency {

namespace {

class GenerateWaitForCore final :
		public ast::WithSymbolTable, public ast::WithConstTranslationUnit {
	const ast::FunctionDecl * decl_waitfor    = nullptr;
	const ast::StructDecl   * decl_mask       = nullptr;
	const ast::StructDecl   * decl_acceptable = nullptr;
	const ast::StructDecl   * decl_monitor    = nullptr;

	UniqueName namer_acc = "__acceptables_"s;
	UniqueName namer_idx = "__index_"s;
	UniqueName namer_flg = "__do_run_"s;
	UniqueName namer_msk = "__mask_"s;
	UniqueName namer_mon = "__monitors_"s;
	UniqueName namer_tim = "__timeout_"s;
	UniqueName namer_fun = "__function_"s;

	ast::ObjectDecl * declareAcceptables( ast::CompoundStmt * out,
		const CodeLocation & location, unsigned long numClauses );
	ast::ObjectDecl * declareFlag(
		ast::CompoundStmt * out, const CodeLocation & location );
	ast::ExprStmt * makeSetter(
		const CodeLocation & location, ast::ObjectDecl * flag );
	ast::ObjectDecl * declMonitors(
		ast::CompoundStmt * out, const ast::WaitForClause * clause );
	void init_clause( ast::CompoundStmt * out, ast::ObjectDecl * acceptables,
		int index, const ast::WaitForClause * clause, ast::Stmt * setter );
	ast::Expr * init_timeout(
		ast::CompoundStmt * out, const CodeLocation & topLocation,
		const ast::Expr * timeout_time, const ast::Expr * timeout_cond,
		const ast::Stmt * else_stmt, const ast::Expr * else_cond,
		const ast::Stmt * setter );
	ast::Expr * call(
		ast::CompoundStmt * out, const CodeLocation & location,
		size_t numClauses, ast::ObjectDecl * acceptables,
		ast::Expr * timeout );
public:
	void previsit( const ast::FunctionDecl * decl );
	void previsit( const ast::StructDecl * decl );
	ast::Stmt * postvisit( const ast::WaitForStmt * stmt );
};

ast::Expr * makeOpIndex( const CodeLocation & location,
		const ast::DeclWithType * array, unsigned long index ) {
	return new ast::UntypedExpr( location,
		new ast::NameExpr( location, "?[?]" ),
		{
			new ast::VariableExpr( location, array ),
			ast::ConstantExpr::from_ulong( location, index ),
		}
	);
}

ast::Expr * makeOpAssign( const CodeLocation & location,
		const ast::Expr * lhs, const ast::Expr * rhs ) {
	return new ast::UntypedExpr( location,
		new ast::NameExpr( location, "?=?" ),
		{ lhs, rhs }
	);
}

ast::Expr * makeOpMember( const CodeLocation & location,
		const std::string & mem, const ast::Expr * sue ) {
	return new ast::UntypedMemberExpr( location,
		new ast::NameExpr( location, mem ),
		sue
	);
}

ast::Stmt * makeAccStmt(
		const CodeLocation & location, ast::DeclWithType * object,
		unsigned long index, const std::string & member,
		const ast::Expr * value, const ResolveContext & context
) {
	ast::Expr * expr = makeOpAssign( location,
		makeOpMember( location,
			member,
			makeOpIndex( location,
				object,
				index
			)
		),
		value
	);

	auto result = ResolvExpr::findVoidExpression( expr, context );
	return new ast::ExprStmt( location, result.get() );
}

const ast::Stmt * maybeCond( const CodeLocation & location,
		const ast::Expr * cond, std::list<ast::ptr<ast::Stmt>> && stmts ) {
	ast::Stmt * block = new ast::CompoundStmt( location, std::move( stmts ) );
	return (cond) ? new ast::IfStmt( location, cond, block ) : block;
}

const ast::VariableExpr * extractVariable( const ast::Expr * func ) {
	if ( auto var = dynamic_cast<const ast::VariableExpr *>( func ) ) {
		return var;
	}
	auto cast = strict_dynamic_cast<const ast::CastExpr *>( func );
	return cast->arg.strict_as<ast::VariableExpr>();
}

const ast::Expr * detectIsDtor(
		const CodeLocation & location, const ast::Expr * func ) {
	const ast::VariableExpr * typed_func = extractVariable( func );
	bool is_dtor = InitTweak::isDestructor(
		typed_func->var.strict_as<ast::FunctionDecl>() );
	return ast::ConstantExpr::from_bool( location, is_dtor );
}

ast::ObjectDecl * GenerateWaitForCore::declareAcceptables(
		ast::CompoundStmt * out,
		const CodeLocation & location, unsigned long numClauses ) {
	ast::ObjectDecl * acceptables = new ast::ObjectDecl( location,
		namer_acc.newName(),
		new ast::ArrayType(
			new ast::StructInstType( decl_acceptable ),
			ast::ConstantExpr::from_ulong( location, numClauses ),
			ast::FixedLen,
			ast::DynamicDim
		)
	);
	out->push_back( new ast::DeclStmt( location, acceptables ) );

	ast::Expr * set = new ast::UntypedExpr( location,
		new ast::NameExpr( location, "__builtin_memset" ),
		{
			new ast::VariableExpr( location, acceptables ),
			ast::ConstantExpr::from_int( location, 0 ),
			new ast::SizeofExpr( location,
				new ast::TypeofType(
					new ast::VariableExpr( location, acceptables ) ) ),
		}
	);
	ResolveContext context{ symtab, transUnit().global };
	auto result = ResolvExpr::findVoidExpression( set, context );
	out->push_back( new ast::ExprStmt( location, result.get() ) );

	return acceptables;
}

ast::ObjectDecl * GenerateWaitForCore::declareFlag(
		ast::CompoundStmt * out, const CodeLocation & location ) {
	ast::ObjectDecl * flag = new ast::ObjectDecl( location,
		namer_flg.newName(),
		new ast::BasicType( ast::BasicKind::Bool ),
		new ast::SingleInit( location,
			ast::ConstantExpr::from_ulong( location, 0 )
		)
	);
	out->push_back( new ast::DeclStmt( location, flag ) );
	return flag;
}

ast::ExprStmt * GenerateWaitForCore::makeSetter(
		const CodeLocation & location, ast::ObjectDecl * flag ) {
	ast::Expr * expr = new ast::UntypedExpr( location,
		new ast::NameExpr( location, "?=?" ),
		{
			new ast::VariableExpr( location, flag ),
			ast::ConstantExpr::from_ulong( location, 1 ),
		}
	);
	ResolveContext context{ symtab, transUnit().global };
	auto result = ResolvExpr::findVoidExpression( expr, context );
	return new ast::ExprStmt( location, result.get() );
}

ast::ObjectDecl * GenerateWaitForCore::declMonitors(
		ast::CompoundStmt * out,
		const ast::WaitForClause * clause ) {
	const CodeLocation & location = clause->location;
	ast::ObjectDecl * monitor = new ast::ObjectDecl( location,
		namer_mon.newName(),
		new ast::ArrayType(
			new ast::PointerType(
				new ast::StructInstType( decl_monitor )
			),
			ast::ConstantExpr::from_ulong( location, clause->target_args.size() ),
			ast::FixedLen,
			ast::DynamicDim
		),
		new ast::ListInit( location,
			map_range<std::vector<ast::ptr<ast::Init>>>(
				clause->target_args,
				[]( const ast::Expr * expr ){
					return new ast::SingleInit( expr->location, expr ); }
			)
		)
	);
	out->push_back( new ast::DeclStmt( location, monitor ) );
	return monitor;
}

void GenerateWaitForCore::init_clause(
		ast::CompoundStmt * out,
		ast::ObjectDecl * acceptables,
		int index,
		const ast::WaitForClause * clause,
		ast::Stmt * setter ) {
	const CodeLocation & location = clause->location;
	const ast::ObjectDecl * monitors = declMonitors( out, clause );
	ast::Type * fptr_t = new ast::PointerType(
			new ast::FunctionType( ast::FixedArgs ) );

	const ast::VariableExpr * variableExpr =
		clause->target.as<ast::VariableExpr>();
	ast::Expr * castExpr = new ast::CastExpr(
		location,
		new ast::CastExpr(
			location,
			clause->target,
			ast::deepCopy( variableExpr->result.get() ),
			ast::GeneratedCast ),
		fptr_t,
		ast::GeneratedCast );

	ast::ObjectDecl * funcDecl = new ast::ObjectDecl( location,
		namer_fun.newName(),
		ast::deepCopy( fptr_t ),
		new ast::SingleInit( location, castExpr )
		);
	ast::Expr * funcExpr = new ast::VariableExpr( location, funcDecl );
	out->push_back( new ast::DeclStmt( location, funcDecl ) );

	ResolveContext context{ symtab, transUnit().global };
	out->push_back( maybeCond( location, clause->when_cond.get(), {
		makeAccStmt( location, acceptables, index, "is_dtor",
			detectIsDtor( location, clause->target ), context ),
		makeAccStmt( location, acceptables, index, "func",
			funcExpr, context ),
		makeAccStmt( location, acceptables, index, "data",
			new ast::VariableExpr( location, monitors ), context ),
		makeAccStmt( location, acceptables, index, "size",
			ast::ConstantExpr::from_ulong( location,
				clause->target_args.size() ), context ),
		ast::deepCopy( setter ),
	} ) );
}

ast::Expr * GenerateWaitForCore::init_timeout(
		ast::CompoundStmt * out,
		const CodeLocation & topLocation,
		const ast::Expr * timeout_time,
		const ast::Expr * timeout_cond,
		const ast::Stmt * else_stmt,
		const ast::Expr * else_cond,
		const ast::Stmt * setter ) {
	ast::ObjectDecl * timeout = new ast::ObjectDecl( topLocation,
		namer_tim.newName(),
		new ast::BasicType( ast::BasicKind::LongLongUnsignedInt ),
		new ast::SingleInit( topLocation,
			ast::ConstantExpr::from_int( topLocation, -1 )
		)
	);
	out->push_back( new ast::DeclStmt( topLocation, timeout ) );

	if ( timeout_time ) {
		const CodeLocation & location = timeout_time->location;
		out->push_back( maybeCond( location, timeout_cond, {
			new ast::ExprStmt( location,
				makeOpAssign(
					location,
					new ast::VariableExpr( location, timeout ),
					timeout_time
				)
			),
			ast::deepCopy( setter ),
		} ) );
	}

	// We only care about the else_stmt's presence and location.
	if ( else_stmt ) {
		const CodeLocation & location = else_stmt->location;
		out->push_back( maybeCond( location, else_cond, {
			new ast::ExprStmt( location,
				makeOpAssign(
					location,
					new ast::VariableExpr( location, timeout ),
					ast::ConstantExpr::from_ulong( location, 0 )
				)
			),
			ast::deepCopy( setter ),
		} ) );
	}

	return new ast::VariableExpr( topLocation, timeout );
}

ast::Expr * GenerateWaitForCore::call(
	ast::CompoundStmt * out,
	const CodeLocation & location,
	size_t numClauses,
	ast::ObjectDecl * acceptables,
	ast::Expr * timeout
) {
	ast::ObjectDecl * index = new ast::ObjectDecl( location,
		namer_idx.newName(),
		new ast::BasicType( ast::BasicKind::ShortSignedInt ),
		new ast::SingleInit( location,
			ast::ConstantExpr::from_int( location, -1 )
		)
	);
	out->push_back( new ast::DeclStmt( location, index ) );

	ast::ObjectDecl * mask = new ast::ObjectDecl( location,
		namer_msk.newName(),
		new ast::StructInstType( decl_mask ),
		new ast::ListInit( location, {
			new ast::SingleInit( location,
				new ast::AddressExpr( location,
					new ast::VariableExpr( location, index )
				)
			),
			new ast::ListInit( location, {
				new ast::SingleInit( location,
					new ast::VariableExpr( location, acceptables )
				),
				new ast::SingleInit( location,
					ast::ConstantExpr::from_ulong( location, numClauses )
				),
			}),
		})
	);
	out->push_back( new ast::DeclStmt( location, mask ) );

	ast::ApplicationExpr * waitforMask = new ast::ApplicationExpr( location,
		ast::VariableExpr::functionPointer( location, decl_waitfor ),
		{
			new ast::CastExpr(
				new ast::VariableExpr( location, mask ),
				new ast::ReferenceType(
					new ast::StructInstType( decl_mask )
				)
			),
			timeout
		}
	);
	out->push_back( new ast::ExprStmt( location, waitforMask ) );

	return new ast::VariableExpr( location, index );
}

ast::Stmt * choose( const ast::WaitForStmt * waitfor, ast::Expr * result ) {
	const CodeLocation & location = waitfor->location;

	ast::SwitchStmt * theSwitch = new ast::SwitchStmt( location,
		result,
		std::vector<ast::ptr<ast::CaseClause>>()
	);

	for ( const auto & [i, clause] : enumerate( waitfor->clauses ) ) {
		theSwitch->cases.push_back(
			new ast::CaseClause( location,
				ast::ConstantExpr::from_ulong( location, i ),
				{
					new ast::CompoundStmt( location, {
						clause->stmt,
						new ast::BranchStmt( location,
							ast::BranchStmt::Break,
							ast::Label( location )
						)
					})
				}
			)
		);
	}

	if ( waitfor->timeout_stmt ) {
		theSwitch->cases.push_back(
			new ast::CaseClause( location,
				ast::ConstantExpr::from_int( location, -2 ),
				{
					new ast::CompoundStmt( location, {
						waitfor->timeout_stmt,
						new ast::BranchStmt( location,
							ast::BranchStmt::Break,
							ast::Label( location )
						)
					})
				}
			)
		);
	}

	if ( waitfor->else_stmt ) {
		theSwitch->cases.push_back(
			new ast::CaseClause( location,
				ast::ConstantExpr::from_int( location, -1 ),
				{
					new ast::CompoundStmt( location, {
						waitfor->else_stmt,
						new ast::BranchStmt( location,
							ast::BranchStmt::Break,
							ast::Label( location )
						)
					})
				}
			)
		);
	}

	return theSwitch;
}

void GenerateWaitForCore::previsit( const ast::FunctionDecl * decl ) {
	if ( "__waitfor_internal" == decl->name ) {
		decl_waitfor = decl;
	}
}

void GenerateWaitForCore::previsit( const ast::StructDecl * decl ) {
	if ( !decl->body ) {
		return;
	} else if ( "__acceptable_t" == decl->name ) {
		assert( !decl_acceptable );
		decl_acceptable = decl;
	} else if ( "__waitfor_mask_t" == decl->name ) {
		assert( !decl_mask );
		decl_mask = decl;
	} else if ( "monitor$" == decl->name ) {
		assert( !decl_monitor );
		decl_monitor = decl;
	}
}

ast::Stmt * GenerateWaitForCore::postvisit( const ast::WaitForStmt * stmt ) {
	if ( !decl_monitor || !decl_acceptable || !decl_mask ) {
		SemanticError( stmt, "waitfor keyword requires monitors to be in scope, add #include <monitor.hfa>" );
	}

	const CodeLocation & location = stmt->location;
	ast::CompoundStmt * comp = new ast::CompoundStmt( location );

	ast::ObjectDecl * acceptables = declareAcceptables( comp, location, stmt->clauses.size() );
	ast::ObjectDecl * flag        = declareFlag( comp, location );
	ast::Stmt       * setter      = makeSetter( location, flag );

	for ( const auto & [i, clause] : enumerate( stmt->clauses ) ) {
		init_clause( comp, acceptables, i, clause, setter );
	}

	ast::Expr * timeout = init_timeout(
		comp,
		location,
		stmt->timeout_time,
		stmt->timeout_cond,
		stmt->else_stmt,
		stmt->else_cond,
		setter
	);

	ast::CompoundStmt * compound = new ast::CompoundStmt( location );
	comp->push_back( new ast::IfStmt( location,
		new ast::VariableExpr( location, flag ),
		compound,
		nullptr
	));

	ast::Expr * result = call(
		compound, location, stmt->clauses.size(), acceptables, timeout );
	compound->push_back( choose( stmt, result ) );
	return comp;
}

} // namespace

void generateWaitFor( ast::TranslationUnit & translationUnit ) {
	ast::Pass<GenerateWaitForCore>::run( translationUnit );
}

} // namespace Concurrency

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
