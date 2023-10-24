//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// EraseWith.hpp -- After resolution, erase all with constructs.
//
// Author           : Andrew Beach
// Created On       : Sun Oct  8  9:36:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Sun Oct  8  9:47:00 2023
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace ResolvExpr {

void eraseWith( ast::TranslationUnit & translationUnit );
/// Remove withExprs from functions and any WithStmt nodes.
/// This must be done after all resolution that needs to see the names from
/// a with. We put it after the last pass to use WithSymbolTable.

}
