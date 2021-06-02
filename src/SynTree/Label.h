//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Label.h --
//
// Author           : Rob Schluntz
// Created On       : Wed Jun 8 12:53:12 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:52:44 2017
// Update Count     : 3
//

#pragma once

#include <string>
#include <list>
#include <iostream>
#include "SynTree.h"

class Label {
  public:
	Label( const std::string & name = "", Statement * labelled = 0, const std::list< Attribute * > & attributes = std::list< Attribute * >() ) : name( name ), labelled( labelled ), attributes( attributes ) {}
	Label( const char * name, Statement * labelled = 0 ) : name( name ), labelled( labelled ) {}

	const std::string & get_name() const { return name; }
	void set_name( const std::string & newValue ) { name = newValue; }

	Statement * get_statement() const { return labelled; }
	void set_statement( Statement * newValue ) { labelled = newValue; }
	std::list< Attribute * >& get_attributes() { return attributes; }

	operator std::string() const { return name; }
	bool empty() { return name.empty(); }

	std::string name;
	Statement * labelled;
	std::list< Attribute * > attributes;
};

inline bool operator==( Label l1, Label l2 ) { return l1.get_name() == l2.get_name(); }
inline bool operator!=( Label l1, Label l2 ) { return ! (l1 == l2); }
inline bool operator<( Label l1, Label l2 ) { return l1.get_name() < l2.get_name(); }
// inline Label operator+( Label l1, Label l2 ) { return l1.get_name() + l2.get_name(); }
// inline Label operator+( Label l1, const char * str ) { return l1.get_name() + Label( str ); }
inline std::ostream & operator<<( std::ostream & out, const Label & l ) { return out << l.get_name(); }

static const std::list< Label > noLabels;

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
