//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CompoundLiteral.hpp -- Use variables to implement compound literals.
//
// Author           : Andrew Beach
// Created On       : Mon Nov 15 16:37:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Mon Nov 15 17:56:00 2021
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Use variables to implement compound literals.
/// Must happen after auto-gen routines are added.
void handleCompoundLiterals( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
