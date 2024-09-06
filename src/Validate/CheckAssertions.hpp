//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CheckAssertions.hpp -- Check for invalid assertions.
//
// Author           : Andrew Beach
// Created On       : Thu Sep  5 14:41:00 2024
// Last Modified By : Andrew Beach
// Last Modified On : Thu Sep  5 14:41:00 2024
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Checks for problems in an assertion list.
void checkAssertions( ast::TranslationUnit & transUnit );

}
