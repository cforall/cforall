//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Unify.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 13:09:04 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jan 17 11:12:00 2023
// Update Count     : 5
//

#pragma once

#include <list>                   // for list

#include "AST/Node.hpp"             // for ptr
#include "AST/TypeEnvironment.hpp"  // for TypeEnvironment, AssertionSet, OpenVarSet
#include "Common/utility.h"       // for deleteAll
#include "SynTree/Declaration.h"  // for TypeDecl, TypeDecl::Data
#include "TypeEnvironment.h"      // for AssertionSet, OpenVarSet
#include "WidenMode.h"              // for WidenMode

class Type;
class TypeInstType;
namespace SymTab {
	class Indexer;
}

namespace ast {
	class SymbolTable;
	class Type;
}

namespace ResolvExpr {

bool unify( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer );
bool unify( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer, Type *&commonType );
bool unifyExact( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer );
bool unifyInexact( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer, Type *&common );

bool typesCompatible( const Type *, const Type *, const SymTab::Indexer & indexer, const TypeEnvironment & env );
bool typesCompatibleIgnoreQualifiers( const Type *, const Type *, const SymTab::Indexer & indexer, const TypeEnvironment & env );

inline bool typesCompatible( const Type * t1, const Type * t2, const SymTab::Indexer & indexer ) {
	TypeEnvironment env;
	return typesCompatible( t1, t2, indexer, env );
}

inline bool typesCompatibleIgnoreQualifiers( const Type * t1, const Type * t2, const SymTab::Indexer & indexer ) {
	TypeEnvironment env;
	return typesCompatibleIgnoreQualifiers( t1, t2, indexer, env );
}

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

/// Creates the type represented by the list of returnVals in a FunctionType.
/// The caller owns the return value.
Type * extractResultType( FunctionType * functionType );
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
