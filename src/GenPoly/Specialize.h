//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Specialize.h -- Generate thunks to specialize polymorphic functions.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:22:31 2017
// Update Count     : 2
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace GenPoly {

void convertSpecializations( ast::TranslationUnit & translationUnit );

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
