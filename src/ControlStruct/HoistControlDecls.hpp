//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// HoistControlDecls.hpp -- Desugar Cforall control structures.
//
// Author           : Andrew Beach
// Created On       : Fri Dec  3 15:31:00 2021
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Jan 31 22:25:07 2022
// Update Count     : 3
//

#pragma once

namespace ast {
class TranslationUnit;
}

namespace ControlStruct {
/// Hoist declarations out of control flow statements into compound statement.
/// Must happen before auto-gen routines are added.
void hoistControlDecls( ast::TranslationUnit & translationUnit );
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
