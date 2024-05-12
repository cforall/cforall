//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixNames.hpp -- Adjustments to typed declarations.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Oct 26 13:47:00 2021
// Update Count     : 4
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace CodeGen {

/// Sets scope levels and fills in main's default return.
void fixNames( ast::TranslationUnit & translationUnit );

} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
