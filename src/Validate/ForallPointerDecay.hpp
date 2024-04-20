//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ForallPointerDecay.hpp --
//
// Author           : Andrew Beach
// Created On       : Tue Dec  7 16:15:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Sat Apr 23 13:13:00 2022
// Update Count     : 1
//

#pragma once

#include <vector>
#include "AST/Node.hpp"

namespace ast {
	class DeclWithType;
	class TranslationUnit;
}

namespace Validate {

/// Cleans up assertion lists and expands traits.
/// Also checks that operator names are used properly on functions.
/// This is a "legacy" pass.
/// Must happen before auto-gen routines are added.
void decayForallPointers( ast::TranslationUnit & transUnit );

/// Sets uniqueIds on any declarations that do not have one set.
/// Must be after implement concurrent keywords; because uniqueIds must be
/// set on declaration before resolution.
void fixUniqueIds( ast::TranslationUnit & transUnit );

/// Expand all traits in an assertion list.
std::vector<ast::ptr<ast::DeclWithType>> expandAssertions(
		std::vector<ast::ptr<ast::DeclWithType>> const & );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
