//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Inspect.cpp -- Helpers to get information from the AST.
//
// Author           : Thierry Delisle
// Created On       : Fri Jun 24 13:16:31 2022
// Last Modified By : Andrew Beach
// Last Modified On : Wed Sep 22 13:50:00 2022
// Update Count     : 2
//

#include "Inspect.hpp"

#include <iostream>

#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Print.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"
#include "CodeGen/OperatorTable.h"

namespace ast {

const Type * getPointerBase( const Type * t ) {
	if ( const auto * p = dynamic_cast< const PointerType * >( t ) ) {
		return p->base;
	} else if ( const auto * a = dynamic_cast< const ArrayType * >( t ) ) {
		return a->base;
	} else if ( const auto * r = dynamic_cast< const ReferenceType * >( t ) ) {
		return r->base;
	} else {
		return nullptr;
	}
}

template<typename CallExpr>
static const Expr * callArg( const CallExpr * call, unsigned int pos ) {
	assertf( pos < call->args.size(),
		"getCallArg for argument that doesn't exist: (%u); %s.",
		pos, toString( call ).c_str() );
	for ( const Expr * arg : call->args ) {
		if ( 0 == pos ) return arg;
		--pos;
	}
	assert( false );
}

template<typename CallExpr, typename Ret>
static Ret throughDeref( const CallExpr * expr, Ret(*func)( const Expr * ) ) {
	// In `(*f)(x)` the function we want is `f`.
	std::string name = getFunctionName( expr );
	assertf( name == "*?", "Unexpected untyped expression: %s", name.c_str() );
	assertf( !expr->args.empty(), "Cannot get function name from dereference with no arguments" );
	return func( expr->args.front() );
}

static const DeclWithType * getCalledFunction( const Expr * expr ) {
	assert( expr );
	if ( const ast::VariableExpr * varExpr = dynamic_cast< const ast::VariableExpr * >( expr ) ) {
		return varExpr->var;
	} else if ( const ast::MemberExpr * memberExpr = dynamic_cast< const ast::MemberExpr * >( expr ) ) {
		return memberExpr->member;
	} else if ( const ast::CastExpr * castExpr = dynamic_cast< const ast::CastExpr * >( expr ) ) {
		return getCalledFunction( castExpr->arg );
	} else if ( const ast::UntypedExpr * untypedExpr = dynamic_cast< const ast::UntypedExpr * >( expr ) ) {
		return throughDeref( untypedExpr, getCalledFunction );
	} else if ( const ast::ApplicationExpr * appExpr = dynamic_cast< const ast::ApplicationExpr * > ( expr ) ) {
		return throughDeref( appExpr, getCalledFunction );
	} else if ( const ast::AddressExpr * addrExpr = dynamic_cast< const ast::AddressExpr * >( expr ) ) {
		return getCalledFunction( addrExpr->arg );
	} else if ( const ast::CommaExpr * commaExpr = dynamic_cast< const ast::CommaExpr * >( expr ) ) {
		return getCalledFunction( commaExpr->arg2 );
	}
	return nullptr;
}

const DeclWithType * getFunction( const Expr * expr ) {
	if ( auto app = dynamic_cast< const ApplicationExpr * >( expr ) ) {
		return getCalledFunction( app->func );
	} else if ( auto untyped = dynamic_cast< const UntypedExpr * >( expr ) ) {
		return getCalledFunction( untyped->func );
	}
	assertf( false, "getFunction received unknown expression: %s", toString( expr ).c_str() );
}

// There is a lot of overlap with getCalledFunction. Ideally it would use
// it as a helper function and return the name of the DeclWithType. But the
// NameExpr and UntypedMemberExpr only work on this version.
static std::string funcName( const Expr * func ) {
	assert( func );
	if ( const ast::NameExpr * nameExpr = dynamic_cast< const ast::NameExpr * >( func ) ) {
		return nameExpr->name;
	} else if ( const ast::VariableExpr * varExpr = dynamic_cast< const ast::VariableExpr * >( func ) ) {
		return varExpr->var->name;
	} else if ( const ast::CastExpr * castExpr = dynamic_cast< const ast::CastExpr * >( func ) ) {
		return funcName( castExpr->arg );
	} else if ( const ast::MemberExpr * memberExpr = dynamic_cast< const ast::MemberExpr * >( func ) ) {
		return memberExpr->member->name;
	} else if ( const ast::UntypedMemberExpr * memberExpr = dynamic_cast< const ast::UntypedMemberExpr * > ( func ) ) {
		return funcName( memberExpr->member );
	} else if ( const ast::UntypedExpr * untypedExpr = dynamic_cast< const ast::UntypedExpr * >( func ) ) {
		return throughDeref( untypedExpr, funcName );
	} else if ( const ast::ApplicationExpr * appExpr = dynamic_cast< const ast::ApplicationExpr * >( func ) ) {
		return throughDeref( appExpr, funcName );
	} else if ( const ast::ConstructorExpr * ctorExpr = dynamic_cast< const ast::ConstructorExpr * >( func ) ) {
		return funcName( getCallArg( ctorExpr->callExpr, 0 ) );
	} else {
		assertf( false, "Unexpected expression type being called as a function in call expression: %s", toString( func ).c_str() );
	}
}

std::string getFunctionName( const Expr * expr ) {
	// There's some unforunate overlap here with getCalledFunction. Ideally
	// this would be able to use getCalledFunction and return the name of the
	// DeclWithType, but this needs to work for NameExpr and UntypedMemberExpr,
	// where getCalledFunction can't possibly do anything reasonable.
	if ( auto app = dynamic_cast< const ApplicationExpr * >( expr ) ) {
		return funcName( app->func );
	} else if ( auto untyped = dynamic_cast< const UntypedExpr * >( expr ) ) {
		return funcName( untyped->func );
	} else {
		assertf( false, "Unexpected expression type passed to getFunctionName: %s", toString( expr ).c_str() );
	}
}

const Expr * getCallArg( const Expr * call, unsigned int pos ) {
	if ( auto app = dynamic_cast< const ApplicationExpr * >( call ) ) {
		return callArg( app, pos );
	} else if ( auto untyped = dynamic_cast< const UntypedExpr * >( call ) ) {
		return callArg( untyped, pos );
	} else if ( auto tupleAssn = dynamic_cast< const TupleAssignExpr * >( call ) ) {
		const std::list<ptr<Stmt>>& stmts = tupleAssn->stmtExpr->stmts->kids;
		assertf( !stmts.empty(), "TupleAssignExpr missing statements." );
		auto stmt  = strict_dynamic_cast< const ExprStmt * >( stmts.back().get() );
		auto tuple = strict_dynamic_cast< const TupleExpr * >( stmt->expr.get() );
		assertf( !tuple->exprs.empty(), "TupleAssignExpr has empty tuple expr." );
		return getCallArg( tuple->exprs.front(), pos );
	} else if ( auto ctor = dynamic_cast< const ImplicitCopyCtorExpr * >( call ) ) {
		return getCallArg( ctor->callExpr, pos );
	} else {
		assertf( false, "Unexpected expression type passed to getCallArg: %s",
			toString( call ).c_str() );
	}
}

bool structHasFlexibleArray( const ast::StructDecl * decl ) {
	if(decl->members.size() == 0) return false;
	const auto & last = *decl->members.rbegin();
	auto lastd = last.as<ast::DeclWithType>();
	// I don't know what this is possible, but it might be.
	if(!lastd) return false;
	auto atype = dynamic_cast<const ast::ArrayType *>(lastd->get_type());
	if(!atype) return false;
	return !atype->isVarLen && !atype->dimension;
}

const ApplicationExpr * isIntrinsicCallExpr( const Expr * expr ) {
	auto appExpr = dynamic_cast< const ApplicationExpr * >( expr );
	if ( !appExpr ) return nullptr;

	const DeclWithType * func = getCalledFunction( appExpr->func );
	assertf( func,
		"getCalledFunction returned nullptr: %s", toString( appExpr->func ).c_str() );

	return func->linkage == ast::Linkage::Intrinsic ? appExpr : nullptr;
}

} // namespace ast
