//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixQualifiedTypes.hpp -- Replace the qualified type with a direct type.
//
// Author           : Andrew Beach
// Created On       : Thr Apr 21 11:11:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Thr Apr 21 11:11:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Replaces qualified types with an unqualified NamedTypeDecl.
/// Must happen after Link References To Types,
/// because aggregate members are accessed.
void fixQualifiedTypes( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
