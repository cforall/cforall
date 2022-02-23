//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Util.hpp -- General utilities for working with the AST.
//
// Author           : Andrew Beach
// Created On       : Wed Jan 19  9:37:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri Feb 18  9:43:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {

class TranslationUnit;

/// Check anything that should always be true of the AST between passes.
/// Insert this whenever you want additional debugging checks.
void checkInvariants( TranslationUnit & transUnit );

}
