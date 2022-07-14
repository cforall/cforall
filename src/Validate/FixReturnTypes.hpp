//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixReturnTypes.hpp --
//
// Author           : Andrew Beach
// Created On       : Tue Jun 29 11:01:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jun 29 11:01:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
    class TranslationUnit;
}

namespace Validate {

// This pass needs to happen early so that other passes can find tuple types
// in the right places, especially for function return types.
void fixReturnTypes( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
