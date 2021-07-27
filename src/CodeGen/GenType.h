//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenType.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 16 04:11:40 2020
// Update Count     : 5
//

#pragma once

#include <string>  // for string

#include "CodeGen/Options.h" // for Options

class Type;

namespace CodeGen {
	std::string genType( Type *type, const std::string &baseString, const Options &options );
	std::string genType( Type *type, const std::string &baseString, bool pretty = false, bool genC = false, bool lineMarks = false );
	std::string genPrettyType( Type * type, const std::string & baseString );
} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //