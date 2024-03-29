//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixLabels.hpp -- Normalizes labels and handles multi-level exit labels.
//
// Author           : Andrew Beach
// Created On       : Mon Nov  1 09:36:00 2021
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Jan 31 22:18:43 2022
// Update Count     : 2
//

#pragma once

namespace ast {
class TranslationUnit;
}

namespace ControlStruct {
// normalizes label definitions and generates multi-level exit labels
void fixLabels( ast::TranslationUnit & translationUnit );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
