//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// EnumAndPointerDecay.hpp --
//
// Author           : Andrew Beach
// Created On       : Tue Jun 28 15:48:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jul  4 13:12:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

void decayEnumsAndPointers( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
