//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ParseNode.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 8 11:00:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Wed May 8 11:00:00 2019
// Update Count     : 1
//

#pragma once

#include "Node.hpp"

#include "Common/CodeLocation.h"

namespace ast {

/// AST node with an included source location
class ParseNode : public Node {
public:
	CodeLocation location;

	// Default constructor is deliberately omitted, all ParseNodes must have a location.
	// Escape hatch if needed is to explicitly pass a default-constructed location, but
	// this should be used sparingly.

	ParseNode( const CodeLocation& loc ) : Node(), location(loc) {}

	ParseNode( const ParseNode& o ) = default;
private:
	ParseNode * clone() const override = 0;

	/// Must be copied in ALL derived classes
	template<typename node_t>
	friend node_t * mutate(const node_t * node);
	template<typename node_t>
	friend node_t * shallowCopy(const node_t * node);
};

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
