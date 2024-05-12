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
#include "Common/Utility.hpp"
#include "GenPoly/ScopedSet.hpp"

#include <vector>

using GenPoly::ScopedSet;

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

template<typename node_t>
void oneOfExprOrType( const node_t * node ) {
	if ( node->expr ) {
		assertf( node->expr && !node->type, "Exactly one of expr or type should be set." );
	} else {
		assertf( !node->expr && node->type, "Exactly one of expr or type should be set." );
	}
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

	void previsit( const SizeofExpr * node ) {
		previsit( (const ParseNode *)node );
		oneOfExprOrType( node );
	}

	void previsit( const AlignofExpr * node ) {
		previsit( (const ParseNode *)node );
		oneOfExprOrType( node );
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

/// Checks that referred to nodes are in scope.
/// This checks many readonly pointers to see if the declaration they are
/// referring to is in scope by the structural rules of code.
// Any escapes marked with a bug should be removed once the bug is fixed.
// This is a separate pass because of it changes the visit pattern and
// must always be run on the entire translation unit.
struct InScopeCore : public ast::WithShortCircuiting {
	ScopedSet<DeclWithType const *> typedDecls;
	ScopedSet<TypeDecl const *> typeDecls;
	// These 3 are really hard to check, because uses that originally ref. at
	// a forward declaration can be rewired to point a later full definition.
	ScopedSet<StructDecl const *> structDecls;
	ScopedSet<UnionDecl const *> unionDecls;
	ScopedSet<EnumDecl const *> enumDecls;
	ScopedSet<TraitDecl const *> traitDecls;

	bool isInGlobal = false;

	void beginScope() {
		typedDecls.beginScope();
		typeDecls.beginScope();
		structDecls.beginScope();
		unionDecls.beginScope();
		enumDecls.beginScope();
		traitDecls.beginScope();
	}

	void endScope() {
		typedDecls.endScope();
		typeDecls.endScope();
		structDecls.endScope();
		unionDecls.endScope();
		enumDecls.endScope();
		traitDecls.endScope();
	}

	void previsit( ApplicationExpr const * expr ) {
		// All isInGlobal manipulation is just to isolate this check.
		// The invalid compound literals lead to bad ctor/dtors. [#280]
		VariableExpr const * func = nullptr;
		CastExpr const * cast = nullptr;
		VariableExpr const * arg = nullptr;
		if ( isInGlobal
				&& 1 == expr->args.size()
				&& ( func = expr->func.as<VariableExpr>() )
				&& ( "?{}" == func->var->name || "^?{}" == func->var->name )
				&& ( cast = expr->args[0].as<CastExpr>() )
				&& ( arg = cast->arg.as<VariableExpr>() )
				&& isPrefix( arg->var->name, "_compLit" ) ) {
			visit_children = false;
		}
	}

	void previsit( VariableExpr const * expr ) {
		if ( !expr->var ) return;
		// bitwise assignment escape [#281]
		if ( expr->var->location.isUnset() ) return;
		assert( typedDecls.contains( expr->var ) );
	}

	void previsit( FunctionType const * type ) {
		// This is to avoid checking the assertions, which can point at the
		// function's declaration and not the enclosing function.
		for ( auto type_param : type->forall ) {
			if ( type_param->formal_usage ) {
				visit_children = false;
				// We could check non-assertion fields here.
			}
		}
	}

	void previsit( TypeInstType const * type ) {
		if ( !type->base ) return;
		assertf( type->base->isManaged(), "Floating Node" );

		// bitwise assignment escape [#281]
		if ( type->base->location.isUnset() ) return;
		// Formal types can actually look at out of scope variables.
		if ( type->formal_usage ) return;
		assert( typeDecls.contains( type->base ) );
	}

	void previsit( TraitInstType const * type ) {
		if ( !type->base ) return;
		assert( traitDecls.contains( type->base ) );
	}

	void previsit( ObjectDecl const * decl ) {
		typedDecls.insert( decl );
		// There are some ill-formed compound literals. [#280]
		// The only known problem cases are at the top level.
		if ( isPrefix( decl->name, "_compLit" ) ) {
			visit_children = false;
		}
	}

	void previsit( FunctionDecl const * decl ) {
		typedDecls.insert( decl );
		beginScope();
		for ( auto & type_param : decl->type_params ) {
			typeDecls.insert( type_param );
		}
		for ( auto & assertion : decl->assertions ) {
			typedDecls.insert( assertion );
		}
		for ( auto & param : decl->params ) {
			typedDecls.insert( param );
		}
		for ( auto & ret : decl->returns ) {
			typedDecls.insert( ret );
		}
		// No special handling of withExprs.

		// Part of the compound literal escape. [#280]
		if ( "__global_init__" == decl->name
				|| "__global_destroy__" == decl->name ) {
			assert( !isInGlobal );
			isInGlobal = true;
		}
	}

	void postvisit( FunctionDecl const * decl ) {
		endScope();
		// Part of the compound literal escape. [#280]
		if ( isInGlobal && ( "__global_init__" == decl->name
				|| "__global_destroy__" == decl->name ) ) {
			isInGlobal = false;
		}
	}

	void previsit( StructDecl const * decl ) {
		structDecls.insert( decl );
		beginScope();
		for ( auto & type_param : decl->params ) {
			typeDecls.insert( type_param );
		}
	}

	void postvisit( StructDecl const * ) {
		endScope();
	}

	void previsit( UnionDecl const * decl ) {
		unionDecls.insert( decl );
		beginScope();
		for ( auto & type_param : decl->params ) {
			typeDecls.insert( type_param );
		}
	}

	void postvisit( UnionDecl const * ) {
		endScope();
	}

	void previsit( EnumDecl const * decl ) {
		enumDecls.insert( decl );
		if ( ast::EnumDecl::EnumHiding::Visible == decl->hide ) {
			for ( auto & member : decl->members ) {
				typedDecls.insert( member.strict_as<ast::DeclWithType>() );
			}
		}
		beginScope();
		for ( auto & type_param : decl->params ) {
			typeDecls.insert( type_param );
		}
	}

	void postvisit( EnumDecl const * ) {
		endScope();
	}

	void previsit( TraitDecl const * decl ) {
		traitDecls.insert( decl );
		beginScope();
		for ( auto & type_param : decl->params ) {
			typeDecls.insert( type_param );
		}
	}

	void postvisit( TraitDecl const * ) {
		endScope();
	}

	void previsit( Designation const * ) {
		visit_children = false;
	}
};

} // namespace

void checkInvariants( TranslationUnit & transUnit ) {
	Pass<InvariantCore>::run( transUnit );
	Pass<InScopeCore>::run( transUnit );
}

} // namespace ast
