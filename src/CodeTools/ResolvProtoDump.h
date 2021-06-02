//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolvProtoDump.h -- Translates CFA resolver instances into resolv-proto instances
//
// Author           : Aaron Moss
// Created On       : Tue Sep 11 09:04:00 2018
// Last Modified By : Aaron Moss
// Last Modified On : Tue Sep 11 09:04:00 2018
// Update Count     : 1
//

#pragma once

#include <list>

class Declaration;

namespace CodeTools {

	/// Prints a translation unit in the input format of the resolv-proto tool
	void dumpAsResolvProto( std::list< Declaration * > &translationUnit );

}  // namespace CodeTools

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
