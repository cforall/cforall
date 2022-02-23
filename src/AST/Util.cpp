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
// Last Modified On : Fri Feb 18  9:42:00 2022
// Update Count     : 0
//

#include "Util.hpp"

#include "Decl.hpp"
#include "Node.hpp"
#include "Pass.hpp"
#include "TranslationUnit.hpp"
#include "Common/ScopedMap.h"

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

struct InvariantCore {
	// To save on the number of visits: this is a kind of composed core.
	// None of the passes should make changes so ordering doesn't matter.
	NoStrongCyclesCore no_strong_cycles;

	void previsit( const Node * node ) {
		no_strong_cycles.previsit( node );
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
