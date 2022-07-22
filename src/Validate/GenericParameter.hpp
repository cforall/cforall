//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenericParameter.hpp -- Generic parameter related passes.
//
// Author           : Andrew Beach
// Created On       : Fri Mar 21  9:55:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr 13 14:45:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Perform substutions for generic parameters and fill in defaults.
/// Check as early as possible, but it can't happen before Link References to
/// Types and observed failing when attempted before eliminate typedef.
void fillGenericParameters( ast::TranslationUnit & translationUnit );

/// Replace dimension generic parameters with a fixed type of that size.
void translateDimensionParameters( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
