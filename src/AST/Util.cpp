//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Util.hpp -- General utilities for working with the AST.
//
// Author           : Andrew Beach
// Created On       : Wed Jan 19  9:46:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri Mar 11 18:07:00 2022
// Update Count     : 1
//

#include "Util.hpp"

#include "Node.hpp"
#include "ParseNode.hpp"
#include "Pass.hpp"
#include "TranslationUnit.hpp"

#include <vector>

namespace ast {

namespace {

/// Check that ast::ptr/strong references do not form a cycle.
struct NoStrongCyclesCore {
	std::vector<const Node *> parents;

	void previsit( const Node * node ) {
		for ( auto & parent : parents ) {
			assert( parent != node );
		}
		parents.push_back( node );
	}

	void postvisit( const Node * node ) {
		assert( !parents.empty() );
		assert( parents.back() == node );
		parents.pop_back();
	}
};

/// Check that every note that can has a set CodeLocation.
struct SetCodeLocationsCore {
	void previsit( const ParseNode * node ) {
		assert( node->location.isSet() );
	}
};

struct InvariantCore {
	// To save on the number of visits: this is a kind of composed core.
	// None of the passes should make changes so ordering doesn't matter.
	NoStrongCyclesCore no_strong_cycles;
	SetCodeLocationsCore set_code_locations;

	void previsit( const Node * node ) {
		no_strong_cycles.previsit( node );
	}

	void previsit( const ParseNode * node ) {
		no_strong_cycles.previsit( node );
		set_code_locations.previsit( node );
	}

	void postvisit( const Node * node ) {
		no_strong_cycles.postvisit( node );
	}
};

} // namespace

void checkInvariants( TranslationUnit & transUnit ) {
	ast::Pass<InvariantCore>::run( transUnit );
}

} // namespace ast
