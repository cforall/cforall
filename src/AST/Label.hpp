//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Label.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 8 13:00:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Wed May 8 13:00:00 2019
// Update Count     : 1
//

#pragma once

#include <iostream>
#include <string>
#include <vector>

#include "Node.hpp"
#include "Common/CodeLocation.hpp"

namespace ast {

class Attribute;

/// Named labels for statements
class Label {
public:
	CodeLocation location;
	std::string name;
	std::vector< ptr<Attribute> > attributes;

	Label( const CodeLocation& loc, const std::string& name = "",
		std::vector<ptr<Attribute>> && attrs = std::vector<ptr<Attribute>>{} )
	: location( loc ), name( name ), attributes( attrs ) {}

	operator std::string () const { return name; }
	bool empty() const { return name.empty(); }
};

inline bool operator== ( const Label& l1, const Label& l2 ) { return l1.name == l2.name; }
inline bool operator!= ( const Label& l1, const Label& l2 ) { return !(l1 == l2); }
inline bool operator<  ( const Label& l1, const Label& l2 ) { return l1.name < l2.name; }

inline std::ostream& operator<< ( std::ostream& out, const Label& l ) { return out << l.name; }

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
