//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ReturnCheck.hpp -- Run simple (non-typed) checks on return statements.
//
// Author           : Andrew Beach
// Created On       : Fri Mar 18 14:05:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri Mar 18 14:26:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
    class TranslationUnit;
}

namespace Validate {

/// Check that return statements have an expression when they must have one.
void checkReturnStatements( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //

