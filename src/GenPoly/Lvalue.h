//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Lvalue.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Thu Sep 15 14:13:00 2022
// Update Count     : 3
//

#pragma once

#include <list>  // for list

class Declaration;
class Expression;
namespace ast {
	class Expr;
	class TranslationUnit;
}

namespace GenPoly {
	/// replaces return type of `lvalue T` with `T*`, along with appropriate address-of and dereference operators
	void convertLvalue( std::list< Declaration* >& translationUnit );
	void convertLvalue( ast::TranslationUnit & translationUnit );

	/// true after reference types have been eliminated from the source code. After this point, reference types should not be added to the AST.
	bool referencesPermissable();

	/// applies transformations that allow GCC to accept more complicated lvalue expressions, e.g. &(a, b)
	Expression * generalizedLvalue( Expression * expr );
	ast::Expr const * generalizedLvalue( ast::Expr const * expr );
} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
