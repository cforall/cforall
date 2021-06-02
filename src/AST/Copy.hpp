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
// Last Modified On : Fri Jun 19 16:43:00 2020
// Update Count     : 1
//

#pragma once

#include "Decl.hpp"
#include "Expr.hpp"
#include "Pass.hpp"
#include "Stmt.hpp"
#include "Type.hpp"
#include <unordered_set>
#include <unordered_map>

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

class DeepCopyCore {
	std::unordered_map< const Node *, const Node * > nodeCache;
	std::unordered_set< readonly<Node> * > readonlyCache;

	template<typename node_t>
	void readonlyInsert( const readonly<node_t> * ptrptr ) {
		readonlyCache.insert( (readonly<Node> *) ptrptr );
	}

public:
	template<typename node_t>
	const node_t * previsit( const node_t * node ) {
		const node_t * copy = shallowCopy( node );
		nodeCache.insert( std::make_pair( node, copy ) );
		return copy;
	}

	void postvisit( const AggregateDecl * node ) {
		readonlyInsert( &node->parent );
	}

	void postvisit( const StructInstType * node ) {
		readonlyInsert( &node->base );
	}

	void postvisit( const UnionInstType * node ) {
		readonlyInsert( &node->base );
	}

	void postvisit( const EnumInstType * node ) {
		readonlyInsert( &node->base );
	}

	void postvisit( const TraitInstType * node ) {
		readonlyInsert( &node->base );
	}

	void postvisit( const TypeInstType * node ) {
		readonlyInsert( &node->base );
	}

	void postvisit( const ImplicitCtorDtorStmt * node ) {
		readonlyInsert( (const readonly<Stmt> *) &node->callStmt );
	}

	void postvisit( const MemberExpr * node ) {
		readonlyInsert( &node->member );
	}

	void postvisit( const VariableExpr * node ) {
		readonlyInsert( &node->var );
	}

	void postvisit( const OffsetofExpr * node ) {
		readonlyInsert( &node->member );
	}

	void postvisit( const DeletedExpr * node ) {
		readonlyInsert( &node->deleteStmt );
	}

	void readonlyUpdates() {
		for ( readonly<Node> * ptr : readonlyCache ) {
			auto it = nodeCache.find( ptr->get() );
			if ( nodeCache.end() != it ) {
				*ptr = it->second;
			}
		}
	}
};

template<typename node_t>
node_t * shallowCopy( const node_t * localRoot ) {
	return localRoot->clone();
}

template<typename node_t>
node_t * deepCopy( const node_t * localRoot ) {
	Pass< DeepCopyCore > dc;
	node_t const * newRoot = localRoot->accept( dc );
	dc.core.readonlyUpdates();
	return const_cast< node_t * >( newRoot );
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
