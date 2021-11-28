//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Print.hpp -- Print an AST (or sub-tree) to a stream.
//
// Author           : Thierry Delisle
// Created On       : Tue May 21 16:20:15 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once

#include <iostream>
#include <utility>   // for forward

#include "AST/Node.hpp"
#include "Common/Indenter.h"

namespace ast {

class Decl;

/// Print a node with the given indenter
void print( std::ostream & os, const ast::Node * node, Indenter indent = {} );

/// Print a declaration in its short form
void printShort( std::ostream & os, const ast::Decl * node, Indenter indent = {} );

/// Print a collection of items
template< typename Coll >
void printAll( std::ostream & os, const Coll & c, Indenter indent = {} ) {
	for ( const auto & i : c ) {
		if ( ! i ) continue;

		os << indent;
		print( os, i, indent );
		os << std::endl;
	}
}

}
