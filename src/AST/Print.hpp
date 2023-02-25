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

#include <iosfwd>

#include "AST/Fwd.hpp"
#include "Common/Indenter.h"

namespace ast {

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

/// Print each cv-qualifier used in the set, followed by a space.
void print( std::ostream & os, CV::Qualifiers );
/// Print each function specifier used in the set, followed by a space.
void print( std::ostream & os, Function::Specs );
/// Print each storage class used in the set, followed by a space.
void print( std::ostream & os, Storage::Classes );

}
