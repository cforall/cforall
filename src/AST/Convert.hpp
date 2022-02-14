//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Convert.hpp -- Convert between the new and old syntax trees.
//
// Author           : Thierry Delisle
// Created On       : Thu May 09 15::37::05 2019
// Last Modified By : Andrew Beach
// Last Modified On : Fri May 17 11:25:00 2019
// Update Count     : 1
//

#pragma once

#include <list>

class Declaration;
namespace ast {
	class TranslationUnit;
};

std::list< Declaration * > convert( const ast::TranslationUnit && translationUnit );
ast::TranslationUnit convert( const std::list< Declaration * > && translationUnit );
