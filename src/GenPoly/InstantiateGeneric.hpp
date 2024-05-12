//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// InstantiateGeneric.hpp -- Create concrete instances of generic types.
//
// Author           : Aaron B. Moss
// Created On       : Thu Aug 04 18:33:00 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:22:42 2017
// Update Count     : 2
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace GenPoly {

void instantiateGeneric( ast::TranslationUnit & translationUnit );
/// Replaces all generic types that have static layout with concrete
/// instantiations. Sized types are replaced with the concrete argument types
/// while unsized types are erased to a void type.
/// This pass can cause designators to ignore the pretty print option.

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
