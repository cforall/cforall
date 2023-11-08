//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// UniqueName.cc -- Create a unique variants of a base name with a counter.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Nov  7 15:04:00 2023
// Update Count     : 4
//

#include "UniqueName.h"

#include "Common/ToString.hpp"

UniqueName::UniqueName( const std::string &base ) : base( base ), count( 0 ) {
}

std::string UniqueName::newName( const std::string &additional ) {
	return toString( base, additional, count++ );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
