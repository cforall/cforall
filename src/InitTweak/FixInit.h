//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixInit.h --
//
// Author           : Rob Schluntz
// Created On       : Wed Jan 13 16:29:30 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 16 07:54:50 2020
// Update Count     : 8
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace InitTweak {

/// Replace constructor initializers with expression statements and unwrap basic C-style initializers.
void fix( ast::TranslationUnit & translationUnit, bool inLibrary);

} // namespace

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
