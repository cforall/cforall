//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Copy.cpp -- Provides functions to copy the AST.
//
// Author           : Andrew Beach
// Created On       : Thr Nov 11  9:16:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Thr Nov 11  9:28:00 2021
// Update Count     : 0
//

#include "Copy.hpp"

#include "Decl.hpp"
#include "Expr.hpp"
#include "Pass.hpp"
#include "Stmt.hpp"
#include "Type.hpp"
#include <unordered_set>
#include <unordered_map>

namespace ast {

namespace {

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

	void postvisit( const StmtExpr * node ) {
		readonlyInsert( &node->resultExpr );
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

}

Node * deepCopyNode( const Node * localRoot ) {
	Pass< DeepCopyCore > dc;
	Node const * newRoot = localRoot->accept( dc );
	dc.core.readonlyUpdates();
	return const_cast< Node * >( newRoot );
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
