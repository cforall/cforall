//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LinkReferenceToTypes.hpp -- Connect instance types to declarations.
//
// Author           : Andrew Beach
// Created On       : Thr Apr 21 11:41:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jun 28 14:58:00 2022
// Update Count     : 1
//

#pragma once

namespace ast {
    class TranslationUnit;
}

namespace Validate {

void linkReferenceToTypes( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
