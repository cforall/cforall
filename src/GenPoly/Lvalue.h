//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Lvalue.h -- Clean up lvalues and remove references.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Thu Sep 15 14:13:00 2022
// Update Count     : 3
//

#pragma once

namespace ast {
	class Expr;
	class TranslationUnit;
}

namespace GenPoly {

/// Replaces return type of `T&` with `T*`, along with appropriate address-of and dereference operators.
void convertLvalue( ast::TranslationUnit & translationUnit );

/// Returns true until reference types have been eliminated from the source code. After this point, reference types should not be added to the AST.
bool referencesPermissable();

/// Applies transformations that allow GCC to accept more complicated lvalue expressions, e.g. &(a, b).
ast::Expr const * generalizedLvalue( ast::Expr const * expr );

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
