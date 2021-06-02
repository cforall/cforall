//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixGlobalInit.h --
//
// Author           : Rob Schluntz
// Created On       : Mon May 04 15:14:56 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:30:54 2017
// Update Count     : 3
//

#pragma once

#include <list>    // for list
#include <string>  // for string

#include <AST/Fwd.hpp>


class Declaration;

namespace InitTweak {
	/// Moves global initialization into an _init function that is unique to the translation unit.
	/// Sets the priority of the initialization function depending on whether the initialization
	/// function is for library code.
	void fixGlobalInit( std::list< Declaration * > & translationUnit, bool inLibrary );
	void fixGlobalInit( ast::TranslationUnit & translationUnit, bool inLibrary );
} // namespace

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
