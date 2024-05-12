//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExceptDecl.hpp -- Handles declarations of exception types.
//
// Author           : Henry Xue
// Created On       : Tue Jul 20 04:10:50 2021
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jul 12 15:49:00 2022
// Update Count     : 2
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace ControlStruct {

/// Unfold exception declarations into raw structure declarations.
/// Also builds vtable declarations and converts vtable types.
void translateExcept( ast::TranslationUnit & translationUnit );

}
