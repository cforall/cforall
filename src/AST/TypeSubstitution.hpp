//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeSubstitution.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Thr May 25 12:31:00 2023
// Update Count     : 10
//

#pragma once

#include <cassert>                 // for assert
#include <list>                    // for list<>::iterator, _List_iterator
#include <unordered_map>
#include <unordered_set>
#include <string>                  // for string, operator!=
#include <utility>                 // for pair

#include "Fwd.hpp"        // for UniqueId
#include "ParseNode.hpp"
#include "Type.hpp"
#include "Common/SemanticError.h"  // for SemanticError
#include "Visitor.hpp"
#include "Decl.hpp"
#include "Expr.hpp"
#include "Node.hpp"

namespace ast {

class TypeSubstitution : public Node {
  public:
	TypeSubstitution();
	template< typename FormalContainer, typename ActualContainer >
	TypeSubstitution( FormalContainer formals, ActualContainer actuals );
	template< typename FormalIterator, typename ActualIterator >
	TypeSubstitution( FormalIterator formalBegin, FormalIterator formalEnd, ActualIterator actualBegin );
	TypeSubstitution( const TypeSubstitution &other );
	virtual ~TypeSubstitution();

	TypeSubstitution &operator=( const TypeSubstitution &other );

	template< typename node_t >
	struct ApplyResult {
		ast::ptr<node_t> node;
		int count;
	};

	template< typename node_t >
	ApplyResult<node_t> apply( const node_t * input ) const {
		ApplyResult<Node> ret = applyBase( input, false );
		return { ret.node.strict_as<node_t>(), ret.count };
	}

	template< typename node_t, enum Node::ref_type ref_t >
	int apply( ptr_base< node_t, ref_t > & input ) const {
		ApplyResult<Node> ret = applyBase( input.get(), false );
		input = ret.node.strict_as<node_t>();
		return ret.count;
	}

	template< typename node_t >
	ApplyResult<node_t> applyFree( const node_t * input ) const {
		ApplyResult<Node> ret = applyBase( input, true );
		return { ret.node.strict_as<node_t>(), ret.count };
	}

	template< typename node_t, enum Node::ref_type ref_t >
	int applyFree( ptr_base< node_t, ref_t > & input ) const {
		ApplyResult<Node> ret = applyBase( input.get(), true );
		input = ret.node.strict_as<node_t>();
		return ret.count;
	}

	void add( const TypeInstType * formalType, const Type *actualType );
	void add( const TypeEnvKey & key, const Type *actualType );
	void add( const TypeSubstitution &other );
	void remove( const TypeInstType * formalType );
	const Type *lookup( const TypeEnvKey & formalType ) const;
	const Type *lookup( const TypeInstType * formalType ) const;
	bool empty() const;

	template< typename FormalContainer, typename ActualContainer >
	void addAll( FormalContainer formals, ActualContainer actuals );
	template< typename FormalIterator, typename ActualIterator >
	void addAll( FormalIterator formalBegin, FormalIterator formalEnd, ActualIterator actualBegin );

	/// create a new TypeSubstitution using bindings from env containing all of the type variables in expr
	static TypeSubstitution * newFromExpr( const Expr * expr, const TypeSubstitution * env );

	void normalize();

	const TypeSubstitution * accept( Visitor & v ) const override { return v.visit( this ); }

	TypeSubstitution * clone() const override { return new TypeSubstitution( *this ); }

  private:

	// Mutator that performs the substitution
	struct Substituter;
	ApplyResult<Node> applyBase( const Node * input, bool isFree ) const;

	// TODO: worry about traversing into a forall-qualified function type or type decl with assertions

	void initialize( const TypeSubstitution &src, TypeSubstitution &dest );

	template<typename core_t>
	friend class Pass;

	typedef std::unordered_map< TypeEnvKey, ptr<Type> > TypeMap;
	TypeMap typeMap;

  public:
	// has to come after declaration of typeMap
	auto begin()       -> decltype( typeMap.begin() ) { return typeMap.begin(); }
	auto   end()       -> decltype( typeMap.  end() ) { return typeMap.  end(); }
	auto begin() const -> decltype( typeMap.begin() ) { return typeMap.begin(); }
	auto   end() const -> decltype( typeMap.  end() ) { return typeMap.  end(); }

};

template< typename FormalContainer, typename ActualContainer >
TypeSubstitution::TypeSubstitution( FormalContainer formals, ActualContainer actuals ) {
	assert( formals.size() == actuals.size() );
	addAll( formals.begin(), formals.end(), actuals.begin() );
}

template< typename FormalIterator, typename ActualIterator >
TypeSubstitution::TypeSubstitution( FormalIterator formalBegin, FormalIterator formalEnd, ActualIterator actualBegin ) {
	addAll( formalBegin, formalEnd, actualBegin );
}

template< typename FormalContainer, typename ActualContainer >
void TypeSubstitution::addAll( FormalContainer formals, ActualContainer actuals ) {
	assert( formals.size() == actuals.size() );
	addAll( formals.begin(), formals.end(), actuals.begin() );
}

// this is the only place where type parameters outside a function formal may be substituted.
template< typename FormalIterator, typename ActualIterator >
void TypeSubstitution::addAll( FormalIterator formalBegin, FormalIterator formalEnd, ActualIterator actualBegin ) {
	// FormalIterator points to a TypeDecl
	// ActualIterator points to a Type
	FormalIterator formalIt = formalBegin;
	ActualIterator actualIt = actualBegin;
	for ( ; formalIt != formalEnd; ++formalIt, ++actualIt ) {
		if ( const TypeDecl *formal = formalIt->template as<TypeDecl>() ) {
			if ( const TypeExpr *actual = actualIt->template as<TypeExpr>() ) {
				if ( formal->name != "" ) {
					typeMap[ formal ] = actual->type;
				} // if
			} else {
				SemanticError( formal, toString( "Attempt to provide non-type parameter: ", toString( *actualIt ).c_str(), " for type parameter " ) );
			} // if
		} else {
			// Is this an error?
		} // if
	} // for
}

} // namespace ast

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
