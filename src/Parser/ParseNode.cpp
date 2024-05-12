//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ParseNode.cpp --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 13:26:29 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Oct  1 23:10:43 2016
// Update Count     : 127
//

#include "ParseNode.hpp"
using namespace std;

int ParseNode::indent_by = 4;

std::ostream & operator<<( std::ostream & out, const ParseNode * node ) {
	node->print( out );
	return out;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
