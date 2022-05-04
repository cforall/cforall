//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// HoistStruct.hpp -- Flattens nested type declarations.
//
// Author           : Andrew Beach
// Created On       : Thr Apr 21 10:33:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Thr Apr 21 10:39:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Flattens nested type declarations.
void hoistStruct( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
