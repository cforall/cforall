//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclStats.hpp -- Print statistics about a translation unit's declarations.
//
// Author           : Andrew Beach
// Created On       : Fri Oct  1 14:20:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Fri Oct  1 14:28:00 2021
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

/// Print statistics about a translation unit's declarations.
void printDeclStats( ast::TranslationUnit &translationUnit );

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
