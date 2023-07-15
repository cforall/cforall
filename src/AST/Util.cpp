//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Util.cpp -- General utilities for working with the AST.
//
// Author           : Andrew Beach
// Created On       : Wed Jan 19  9:46:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Wed May 11 16:16:00 2022
// Update Count     : 3
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
void isCodeLocationSet( const ParseNode * node ) {
	assert( node->location.isSet() );
}

void areLabelLocationsSet( const Stmt * stmt ) {
	for ( const Label& label : stmt->labels ) {
		assert( label.location.isSet() );
	}
}

/// Make sure the reference counts are in a valid combination.
void isStable( const Node * node ) {
	assert( node->isStable() );
}

/// Check that a FunctionDecl is synchronized with it's FunctionType.
void functionDeclMatchesType( const FunctionDecl * decl ) {
	// The type is a cache of sorts, if it is missing that is only a
	// problem if isTypeFixed is set.
	if ( decl->isTypeFixed ) {
		assert( decl->type );
	} else if ( !decl->type ) {
		return;
	}

	const FunctionType * type = decl->type;

	// Check that `type->forall` corresponds with `decl->type_params`.
	assert( type->forall.size() == decl->type_params.size() );
	// Check that `type->assertions` corresponds with `decl->assertions`.
	assert( type->assertions.size() == decl->assertions.size() );
	// Check that `type->params` corresponds with `decl->params`.
	assert( type->params.size() == decl->params.size() );
	// Check that `type->returns` corresponds with `decl->returns`.
	assert( type->returns.size() == decl->returns.size() );
}

/// Check that the MemberExpr has an aggregate type and matching member.
void memberMatchesAggregate( const MemberExpr * expr ) {
	const Type * aggrType = expr->aggregate->result->stripReferences();
	const AggregateDecl * decl = nullptr;
	if ( auto inst = dynamic_cast<const StructInstType *>( aggrType ) ) {
		decl = inst->base;
	} else if ( auto inst = dynamic_cast<const UnionInstType *>( aggrType ) ) {
		decl = inst->base;
	}
	assertf( decl, "Aggregate of member not correct type." );

	for ( auto aggrMember : decl->members ) {
		if ( expr->member == aggrMember ) {
			return;
		}
	}
	assertf( false, "Member not found." );
}

/// Check for Floating Nodes:
/// Every node should be reachable from a root (the TranslationUnit) via a
/// chain of structural references (tracked with ptr). This cannot check all
/// of that, it just checks if a given node's field has a strong reference.
template<typename node_t, typename field_t>
void noFloatingNode( const node_t * node, field_t node_t::*field_ptr ) {
	const field_t & field = node->*field_ptr;
	if ( nullptr == field ) return;
	assertf( field->isManaged(), "Floating node found." );
}

struct InvariantCore {
	// To save on the number of visits: this is a kind of composed core.
	// None of the passes should make changes so ordering doesn't matter.
	NoStrongCyclesCore no_strong_cycles;

	void previsit( const Node * node ) {
		no_strong_cycles.previsit( node );
		isStable( node );
	}

	void previsit( const ParseNode * node ) {
		previsit( (const Node *)node );
		isCodeLocationSet( node );
	}

	void previsit( const FunctionDecl * node ) {
		previsit( (const ParseNode *)node );
		functionDeclMatchesType( node );
	}

	void previsit( const Stmt * node ) {
		previsit( (const ParseNode *)node );
		areLabelLocationsSet( node );
	}

	void previsit( const VariableExpr * node ) {
		previsit( (const ParseNode *)node );
		noFloatingNode( node, &VariableExpr::var );
	}

	void previsit( const MemberExpr * node ) {
		previsit( (const ParseNode *)node );
		memberMatchesAggregate( node );
	}

	void previsit( const StructInstType * node ) {
		previsit( (const Node *)node );
		noFloatingNode( node, &StructInstType::base );
	}

	void previsit( const UnionInstType * node ) {
		previsit( (const Node *)node );
		noFloatingNode( node, &UnionInstType::base );
	}

	void previsit( const EnumInstType * node ) {
		previsit( (const Node *)node );
		noFloatingNode( node, &EnumInstType::base );
	}

	void previsit( const TypeInstType * node ) {
		previsit( (const Node *)node );
		noFloatingNode( node, &TypeInstType::base );
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
