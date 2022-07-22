//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ReplaceTypedef.hpp -- Fill in all typedefs with the underlying type.
//
// Author           : Andrew Beach
// Created On       : Tue Jun 29 14:58:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jul  4 13:12:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Uses of typedef are replaced with the type in the typedef.
void replaceTypedef( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
