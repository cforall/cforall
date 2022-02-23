//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ForallPointerDecay.hpp --
//
// Author           : Andrew Beach
// Created On       : Tue Dec  7 16:15:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Tue Dec  8 11:50:00 2021
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Cleans up assertion lists and expands traits.
/// Also checks that operator names are used properly on functions and
/// assigns unique IDs. This is a "legacy" pass.
void decayForallPointers( ast::TranslationUnit & transUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
