//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclStats.h -- Prints summary information about a translation unit's declarations.
//
// Author           : Aaron Moss
// Created On       : Wed Jan 31 16:40:00 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul 21 22:17:56 2017
// Update Count     : 2
//

#pragma once

#include <list>  // for list

class Declaration;

namespace CodeTools {

	/// Prints summary information about a translation unit's function declarations and calls
	void printDeclStats( std::list< Declaration * > &translationUnit );
	
}  // namespace CodeTools

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
