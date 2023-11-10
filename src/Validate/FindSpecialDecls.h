//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindSpecialDeclarations.h -- Find special declarations used in the compiler.
//
// Author           : Rob Schluntz
// Created On       : Thu Aug 30 09:49:02 2018
// Last Modified By : Andrew Beach
// Last Modified On : Wed Nov 10 15:16:00 2021
// Update Count     : 3
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Find and remember some of the special declarations that are useful for
/// generating code, so that they do not have to be discovered multiple times.
void findGlobalDecls( ast::TranslationUnit & translationUnit );

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
