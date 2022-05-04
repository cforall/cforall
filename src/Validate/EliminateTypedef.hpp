//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// EliminateTypedef.hpp -- Removes TypedefDecl nodes from the AST.
//
// Author           : Andrew Beach
// Created On       : Wed Apr 20 16:35:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr 20 16:38:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Removes TypedefDecl nodes from the AST.
void eliminateTypedef( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
