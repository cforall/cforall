//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AssertAcyclic.cpp -- Check that ast::ptr does not form a cycle.
//
// Author           : Andrew Beach
// Created On       : Thu Jun 06 15:00:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Fri Jun 07 14:32:00 2019
// Update Count     : 1
//

#include "AssertAcyclic.hpp"

#include "AST/Pass.hpp"

namespace {

class NoStrongCyclesCore {
    std::vector<const ast::Node *> parents;
public:
	void previsit( const ast::Node * node ) {
		for (auto & parent : parents) {
			assert(parent != node);
		}
		parents.push_back(node);
	}
	void postvisit( const ast::Node * ) {
		parents.pop_back();
	}
};

}

namespace ast {

void assertAcyclic( const std::list< ast::ptr< ast::Decl > > & translationUnit ) {
   	Pass<NoStrongCyclesCore> visitor;
	for ( auto & decl : translationUnit ) {
		decl->accept( visitor );
	}
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
