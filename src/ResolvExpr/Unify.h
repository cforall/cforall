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
// Last Modified By : Aaron B. Moss
// Last Modified On : Mon Jun 18 11:58:00 2018
// Update Count     : 4
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

	template< typename Iterator1, typename Iterator2 >
	bool unifyList( Iterator1 list1Begin, Iterator1 list1End, Iterator2 list2Begin, Iterator2 list2End, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer, std::list< Type* > &commonTypes ) {
		for ( ; list1Begin != list1End && list2Begin != list2End; ++list1Begin, ++list2Begin ) {
			Type *commonType = 0;
			if ( ! unify( *list1Begin, *list2Begin, env, needAssertions, haveAssertions, openVars, indexer, commonType ) ) {
				return false;
			} // if
			commonTypes.push_back( commonType );
		} // for
		if ( list1Begin != list1End || list2Begin != list2End ) {
			return false;
		} else {
			return true;
		} // if
	}

	template< typename Iterator1, typename Iterator2 >
	bool unifyList( Iterator1 list1Begin, Iterator1 list1End, Iterator2 list2Begin, Iterator2 list2End, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer ) {
		std::list< Type* > commonTypes;
		if ( unifyList( list1Begin, list1End, list2Begin, list2End, env, needAssertions, haveAssertions, openVars, indexer, commonTypes ) ) {
			deleteAll( commonTypes );
			return true;
		} else {
			return false;
		} // if
	}

	bool unify( 
		const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2, 
		ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have, 
		ast::OpenVarSet & open, const ast::SymbolTable & symtab );

	bool unify( 
		const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2, 
		ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have, 
		ast::OpenVarSet & open, const ast::SymbolTable & symtab, ast::ptr<ast::Type> & common );

	bool unifyExact( 
		const ast::Type * type1, const ast::Type * type2, ast::TypeEnvironment & env, 
		ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open, 
		WidenMode widen, const ast::SymbolTable & symtab );

	bool unifyInexact( 
		const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2, 
		ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have, 
		const ast::OpenVarSet & open, WidenMode widen, const ast::SymbolTable & symtab, 
		ast::ptr<ast::Type> & common );

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
