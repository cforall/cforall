//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TrackLoc.h -- Track CodeLocation information in a translation unit's declarations.
//
// Author           : Andrew Beach
// Created On       : Tues May 2 15:40:00 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul 21 22:17:44 2017
// Update Count     : 1
//

#pragma once

#include <cstddef>   // for size_t
#include <list>      // for list

class Declaration;

namespace CodeTools {

	/// Fill in an approximate CodeLocation for each syntax node.
	// printLevel: how much printing while filling in the node locations.
	// 0 - No Printing, 1 - Print Location, 2 - Print Node Type and Location
	void fillLocations( std::list< Declaration * > &translationUnit, size_t printLevel = 0 );

}  // namespace CodeTools

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
