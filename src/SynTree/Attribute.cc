//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Attribute.cc --
//
// Author           : Rob Schluntz
// Created On       : Mon June 06 14:51:16 2016
// Last Modified By : Rob Schluntz
// Last Modified On : Mon June 06 14:54:48 2016
// Update Count     : 1
//

#include <ostream>           // for operator<<, ostream, basic_ostream, endl
#include <set>

#include "Attribute.h"
#include "Common/utility.h"  // for cloneAll, deleteAll, printAll
#include "Expression.h"      // for Expression

Attribute::Attribute( const Attribute &other ) : BaseSyntaxNode( other ), name( other.name ) {
	cloneAll( other.parameters, parameters );
}

Attribute::~Attribute() {
	deleteAll( parameters );
}

bool Attribute::isValidOnFuncParam() const {
	// attributes such as aligned, cleanup, etc. produce GCC errors when they appear
	// on function parameters. Maintain here a whitelist of attribute names that are
	// allowed to appear on parameters.
	static std::set< std::string > valid = {
		"noreturn", "unused"
	};
	return valid.count( normalizedName() );
}

std::string Attribute::normalizedName() const {
	// trim beginning/ending _, convert to lowercase
	auto begin = name.find_first_not_of('_');
	auto end = name.find_last_not_of('_');
	if (begin == std::string::npos || end == std::string::npos) return "";
	std::string ret;
	ret.reserve( end-begin+1 );
	std::transform( &name[begin], &name[end+1], back_inserter( ret ), tolower );
	return ret;
}

void Attribute::print( std::ostream &os, Indenter indent ) const {
	using std::endl;
	using std::string;

	if ( ! empty() ) {
		os << "Attribute with name: " << name;
		if ( ! parameters.empty() ) {
			os << " with parameters: " << endl;
			printAll( parameters, os, indent+1 );
		}
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
