//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// VerifyCtorDtorAssign.hpp -- Check the form of operators.
//
// Author           : Andrew Beach
// Created On       : Mon Jul  4 10:25:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jul  4 13:13:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// Check that constructors, destructors and assignments all have the correct
/// form. Must happen before auto-gen or anything that examines operators.
void verifyCtorDtorAssign( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
