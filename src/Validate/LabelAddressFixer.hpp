//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LabelAddressFixer.hpp --
//
// Author           : Andrew Beach
// Created On       : Fri Nov 12 16:29:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Fri Nov 12 16:35:00 2021
// Update Count     : 0
//

namespace ast {
	class TranslationUnit;
}

namespace Validate {

void fixLabelAddresses( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
