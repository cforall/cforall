//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Unify.hpp --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 13:09:04 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jan 17 11:12:00 2023
// Update Count     : 5
//

#pragma once

#include "AST/Node.hpp"             // for ptr
#include "AST/TypeEnvironment.hpp"  // for TypeEnvironment, AssertionSet, OpenVarSet
#include "WidenMode.hpp"            // for WidenMode

namespace ast {
	class SymbolTable;
	class Type;
}

namespace ResolvExpr {

bool unify(
	const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2,
	ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
	ast::OpenVarSet & open );

bool unify(
	const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2,
	ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
	ast::OpenVarSet & open, ast::ptr<ast::Type> & common );

bool unifyExact(
	const ast::Type * type1, const ast::Type * type2, ast::TypeEnvironment & env,
	ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open,
	WidenMode widen );

bool unifyInexact(
	const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2,
	ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
	const ast::OpenVarSet & open, WidenMode widen,
	ast::ptr<ast::Type> & common );

bool typesCompatible(
	const ast::Type *, const ast::Type *,
	const ast::TypeEnvironment & env = {} );

bool typesCompatibleIgnoreQualifiers(
	const ast::Type *, const ast::Type *,
	const ast::TypeEnvironment & env = {} );

/// Creates or extracts the type represented by returns in a `FunctionType`.
ast::ptr<ast::Type> extractResultType( const ast::FunctionType * func );

std::vector<ast::ptr<ast::Type>> flattenList(
	const std::vector<ast::ptr<ast::Type>> & src, ast::TypeEnvironment & env
);

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
