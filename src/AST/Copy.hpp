//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Copy.hpp -- Provides functions to copy the AST.
//
// Author           : Andrew Beach
// Created On       : Wed Jul 10 16:13:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Thr Nov 11  9:22:00 2021
// Update Count     : 2
//

#pragma once

#include "Node.hpp"
#include <cassert>

namespace ast {

template<typename node_t>
node_t * shallowCopy( const node_t * node );
/* Create a shallow copy of the node given.
 *
 * The new node has all the same primitive field values and points to the
 * same children nodes as the parent.
 */

template<typename node_t>
node_t * deepCopy( const node_t * localRoot );
/* Create a deep copy of the tree rooted at localRoot.
 *
 * This creates a copy of every node in the sub-tree (reachable by strong
 * reference from local_root) and updates any readonly pointers on those nodes
 * that point to another node in the sub-tree to the new version of that node.
 */

// Implementations:
template<typename node_t>
node_t * shallowCopy( const node_t * localRoot ) {
	return localRoot->clone();
}

template<typename node_t>
node_t * deepCopy( const node_t * localRoot ) {
	return strict_dynamic_cast<node_t *>( deepCopy<Node>( localRoot ) );
}

template<>
Node * deepCopy<Node>( const Node * localRoot );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
