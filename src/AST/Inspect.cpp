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
// Last Modified On : Fri Apr 14 15:09:00 2023
// Update Count     : 4
//

#include "Inspect.hpp"

#include <iostream>
#include <iterator>

#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Print.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"
#include "CodeGen/OperatorTable.h"

namespace ast {

const Type * getPointerBase( const Type * type ) {
	if ( const auto * p = dynamic_cast< const PointerType * >( type ) ) {
		return p->base;
	} else if ( auto a = dynamic_cast< const ArrayType * >( type ) ) {
		return a->base;
	} else if ( auto r = dynamic_cast< const ReferenceType * >( type ) ) {
		return r->base;
	} else {
		return nullptr;
	}
}

template<typename CallExpr, typename Ret>
static Ret throughDeref( const CallExpr * expr, Ret(*func)( const Expr * ) ) {
	// In `(*f)(x)` the function we want is `f`.
	std::string name = getFunctionName( expr );
	assertf( name == "*?", "Unexpected untyped expression: %s", name.c_str() );
	assertf( !expr->args.empty(), "Cannot pass through dereference with no arguments." );
	return func( expr->args.front() );
}

static const DeclWithType * getCalledFunction( const Expr * expr ) {
	assert( expr );
	if ( const auto * varExpr = dynamic_cast< const VariableExpr * >( expr ) ) {
		return varExpr->var;
	} else if ( auto memberExpr = dynamic_cast< const MemberExpr * >( expr ) ) {
		return memberExpr->member;
	} else if ( auto castExpr = dynamic_cast< const CastExpr * >( expr ) ) {
		return getCalledFunction( castExpr->arg );
	} else if ( auto untypedExpr = dynamic_cast< const UntypedExpr * >( expr ) ) {
		return throughDeref( untypedExpr, getCalledFunction );
	} else if ( auto appExpr = dynamic_cast< const ApplicationExpr * > ( expr ) ) {
		return throughDeref( appExpr, getCalledFunction );
	} else if ( auto addrExpr = dynamic_cast< const AddressExpr * >( expr ) ) {
		return getCalledFunction( addrExpr->arg );
	} else if ( auto commaExpr = dynamic_cast< const CommaExpr * >( expr ) ) {
		return getCalledFunction( commaExpr->arg2 );
	} else {
		return nullptr;
	}
}

const DeclWithType * getFunction( const Expr * expr ) {
	if ( auto app = dynamic_cast< const ApplicationExpr * >( expr ) ) {
		return getCalledFunction( app->func );
	} else if ( auto untyped = dynamic_cast< const UntypedExpr * >( expr ) ) {
		return getCalledFunction( untyped->func );
	} else {
		assertf( false, "getFunction received unknown expression: %s", toString( expr ).c_str() );
	}
}

// There is a lot of overlap with getCalledFunction. Ideally it would use
// it as a helper function and return the name of the DeclWithType. But the
// NameExpr and UntypedMemberExpr only work on this version.
static std::string funcName( const Expr * func ) {
	assert( func );
	if ( const auto * nameExpr = dynamic_cast< const NameExpr * >( func ) ) {
		return nameExpr->name;
	} else if ( auto varExpr = dynamic_cast< const VariableExpr * >( func ) ) {
		return varExpr->var->name;
	} else if ( auto castExpr = dynamic_cast< const CastExpr * >( func ) ) {
		return funcName( castExpr->arg );
	} else if ( auto memberExpr = dynamic_cast< const MemberExpr * >( func ) ) {
		return memberExpr->member->name;
	} else if ( auto memberExpr = dynamic_cast< const UntypedMemberExpr * >( func ) ) {
		return funcName( memberExpr->member );
	} else if ( auto untypedExpr = dynamic_cast< const UntypedExpr * >( func ) ) {
		return throughDeref( untypedExpr, funcName );
	} else if ( auto appExpr = dynamic_cast< const ApplicationExpr * >( func ) ) {
		return throughDeref( appExpr, funcName );
	} else if ( auto ctorExpr = dynamic_cast< const ConstructorExpr * >( func ) ) {
		return funcName( getCallArg( ctorExpr->callExpr, 0 ) );
	} else {
		assertf( false, "Unexpected expression type being called as a function in call expression: %s", toString( func ).c_str() );
	}
}

std::string getFunctionName( const Expr * expr ) {
	// There's some unforunate overlap here with getFunction. See above.
	if ( auto app = dynamic_cast< const ApplicationExpr * >( expr ) ) {
		return funcName( app->func );
	} else if ( auto untyped = dynamic_cast< const UntypedExpr * >( expr ) ) {
		return funcName( untyped->func );
	} else {
		assertf( false, "getFunctionName received unknown expression: %s", toString( expr ).c_str() );
	}
}

template<typename CallExpr>
static const Expr * callArg( const CallExpr * call, unsigned int pos ) {
	assertf( pos < call->args.size(),
		"callArg for argument that doesn't exist: (%u); %s.",
		pos, toString( call ).c_str() );
	auto it = call->args.begin();
	std::advance( it, pos );
	return *it;
}

const Expr * getCallArg( const Expr * call, unsigned int pos ) {
	if ( auto app = dynamic_cast< const ApplicationExpr * >( call ) ) {
		return callArg( app, pos );
	} else if ( auto untyped = dynamic_cast< const UntypedExpr * >( call ) ) {
		return callArg( untyped, pos );
	} else if ( auto tupleAssn = dynamic_cast< const TupleAssignExpr * >( call ) ) {
		const std::list<ptr<Stmt>>& stmts = tupleAssn->stmtExpr->stmts->kids;
		assertf( !stmts.empty(), "TupleAssignExpr missing statements." );
		auto stmt  = stmts.back().strict_as< ExprStmt >();
		auto tuple = stmt->expr.strict_as< TupleExpr >();
		assertf( !tuple->exprs.empty(), "TupleAssignExpr has empty tuple expr." );
		return getCallArg( tuple->exprs.front(), pos );
	} else if ( auto ctor = dynamic_cast< const ImplicitCopyCtorExpr * >( call ) ) {
		return getCallArg( ctor->callExpr, pos );
	} else {
		assertf( false, "Unexpected expression type passed to getCallArg: %s", toString( call ).c_str() );
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
	assertf( func, "getCalledFunction returned nullptr: %s",
		toString( appExpr->func ).c_str() );

	return func->linkage == Linkage::Intrinsic ? appExpr : nullptr;
}

bool isUnnamedBitfield( const ast::ObjectDecl * obj ) {
	return obj && obj->name.empty() && obj->bitfieldWidth;
}

} // namespace ast
