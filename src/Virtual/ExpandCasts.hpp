//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExpandCasts.hpp --
//
// Author           : Andrew Beach
// Created On       : Mon Jul 24 13:54:00 2017
// Last Modified By : Andrew Beach
// Last Modified On : Fri Jul 29 14:40:00 2022
// Update Count     : 1
//

#pragma once

#include <list>  // for list

class Declaration;
namespace ast {
	class TranslationUnit;
}

namespace Virtual {
void expandCasts( std::list< Declaration * > & translationUnit );
void expandCasts( ast::TranslationUnit & translationUnit );
// Breaks all virtual cast nodes up into translatable nodes.

// Later this might just set some information so it can happen at CodeGen.

}
