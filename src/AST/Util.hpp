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

#include "Fwd.hpp"

namespace ast {

class TranslationUnit;

/// Check anything that should always be true of the AST between passes.
/// Insert this whenever you want additional debugging checks.
void checkInvariants( TranslationUnit & );

/// Maintains an AST-module state for contextual information needed in
/// ast::* implementations, notably constructors:
///    early: while parsing, use bootstrap versions
///    late: once a whole TranslationUnit exists, use its answers
/// When the program is in the later state, ast::* construcors effectively get
/// the benefit of WithTranslationUnit, without having to pass them one.
class TranslationDeps {

    TranslationDeps() = delete;

    friend class SizeofExpr;
    friend class AlignofExpr;
    friend class CountofExpr;
    friend class OffsetofExpr;
    friend class OffsetPackExpr;

    /// Appropriate return type for built-in expressions that report on sizes
    static const Type * getSizeType();

  public:
    /// Transition from early to late states
    static void evolve( TranslationUnit & );
};

}
