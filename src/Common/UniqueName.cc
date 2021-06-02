//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// UniqueName.cc -- 
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Jun  8 14:47:49 2015
// Update Count     : 3
//

#include <string>
#include <sstream>

#include "UniqueName.h"

UniqueName::UniqueName( const std::string &base ) : base( base ), count( 0 ) {
}

std::string UniqueName::newName( const std::string &additional ) {
	std::ostringstream os;
	os << base << additional << count++;
	return os.str();
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
