//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Init.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Fri May 10 10:30:00 2019
// Last Modified By : Aaron B. Moss
// Created On       : Fri May 10 10:30:00 2019
// Update Count     : 1
//

#include "Init.hpp"

#include <cassert>
#include <utility>  // for move
#include <vector>

namespace ast {

ListInit::ListInit( const CodeLocation& loc, std::vector<ptr<Init>>&& is,
	std::vector<ptr<Designation>>&& ds, ConstructFlag mc)
: Init( loc, mc ), initializers( std::move(is) ), designations( std::move(ds) ) {
	// handle common case where ListInit is created without designations by making an
	// equivalent-length empty list
	if ( designations.empty() ) {
		for ( unsigned i = 0; i < initializers.size(); ++i ) {
			designations.emplace_back( new Designation{ loc } );
		}
	}

	assertf( initializers.size() == designations.size(), "Created ListInit with mismatching "
		"initializers (%zd) and designations (%zd)", initializers.size(), designations.size() );
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
