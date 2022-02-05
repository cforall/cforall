//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Autogen.hpp -- Generate automatic routines for data types.
//
// Author           : Andrew Beach
// Created On       : Wed Dec  1 13:42:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Thr Dec  2 13:56:00 2021
// Update Count     : 0
//

#pragma once

namespace ast {
    class TranslationUnit;
}

namespace Validate {

void autogenerateRoutines( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
