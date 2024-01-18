//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Attribute.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Fri May 10 10:30:00 2019
// Last Modified By : Aaron B. Moss
// Created On       : Fri May 10 10:30:00 2019
// Update Count     : 1
//

#include "Attribute.hpp"

#include <algorithm> // for transform
#include <cctype>    // for tolower
#include <iterator>  // for back_inserter
#include <string>

namespace ast {

std::string Attribute::normalizedName() const {
	// trim underscores
	auto begin = name.find_first_not_of('_');
	auto end = name.find_last_not_of('_');
	if ( begin == std::string::npos || end == std::string::npos ) return "";

	// convert to lowercase
	std::string ret;
	ret.reserve( end-begin+1 );
	int (*tolower)(int) = std::tolower;
	std::transform( &name[begin], &name[end+1], back_inserter( ret ), tolower );
	return ret;
}

bool Attribute::isValidOnFuncParam() const {
	// Attributes produce GCC errors when they appear on function
	// parameters. Names on the previous allow-list implementation:
	// unused, noreturn, __vector_size__
	std::string norm = normalizedName();
	return norm != "aligned" && norm != "packed" && norm != "used";
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
