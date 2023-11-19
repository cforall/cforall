//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Box.h -- Implement polymorphic function calls and types.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Thr Oct  6 13:37:00 2022
// Update Count     : 7
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace GenPoly {

void box( ast::TranslationUnit & translationUnit );

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
