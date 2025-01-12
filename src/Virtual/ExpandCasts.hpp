//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExpandCasts.hpp -- Expand virtual casts into lower level code.
//
// Author           : Andrew Beach
// Created On       : Mon Jul 24 13:54:00 2017
// Last Modified By : Andrew Beach
// Last Modified On : Fri Jan 10 14:34:00 2025
// Update Count     : 2
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Virtual {
void expandCasts( ast::TranslationUnit & translationUnit );
// Breaks all virtual cast nodes up into translatable nodes.

}
