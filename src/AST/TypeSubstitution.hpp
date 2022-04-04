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
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Apr 30 22:52:47 2019
// Update Count     : 9
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

	template< typename SynTreeClass >
	struct ApplyResult {
		ast::ptr<SynTreeClass> node;
		int count;
	};

	template< typename SynTreeClass > ApplyResult<SynTreeClass> apply( const SynTreeClass * input ) const;
	template< typename SynTreeClass > ApplyResult<SynTreeClass> applyFree( const SynTreeClass * input ) const;

	template< typename node_t, enum Node::ref_type ref_t >
	int apply( ptr_base< node_t, ref_t > & input ) const {
		const node_t * p = input.get();
		auto ret = apply(p);
		input = ret.node;
		return ret.count;
	}

	template< typename node_t, enum Node::ref_type ref_t >
	int applyFree( ptr_base< node_t, ref_t > & input ) const {
		const node_t * p = input.get();
		auto ret = applyFree(p);
		input = ret.node;
		return ret.count;
	}

	void add( const TypeInstType * formalType, const Type *actualType );
	void add( const TypeInstType::TypeEnvKey & key, const Type *actualType );
	void add( const TypeSubstitution &other );
	void remove( const TypeInstType * formalType );
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

	// TODO: worry about traversing into a forall-qualified function type or type decl with assertions

	void initialize( const TypeSubstitution &src, TypeSubstitution &dest );

	template<typename core_t>
	friend class Pass;

	typedef std::unordered_map< TypeInstType::TypeEnvKey, ptr<Type> > TypeEnvType;
	TypeEnvType typeEnv;

  public:
	// has to come after declaration of typeEnv
	auto begin()       -> decltype( typeEnv.begin() ) { return typeEnv.begin(); }
	auto   end()       -> decltype( typeEnv.  end() ) { return typeEnv.  end(); }
	auto begin() const -> decltype( typeEnv.begin() ) { return typeEnv.begin(); }
	auto   end() const -> decltype( typeEnv.  end() ) { return typeEnv.  end(); }

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
					typeEnv[ formal ] = actual->type;
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

// include needs to happen after TypeSubstitution is defined so that both TypeSubstitution and
// PassVisitor are defined before PassVisitor implementation accesses TypeSubstitution internals.
#include "Pass.hpp"
#include "Copy.hpp"

namespace ast {

// definitition must happen after PassVisitor is included so that WithGuards can be used
struct TypeSubstitution::Substituter : public WithGuards, public WithVisitorRef<Substituter>, public PureVisitor {
		static size_t traceId;

		Substituter( const TypeSubstitution & sub, bool freeOnly ) : sub( sub ), freeOnly( freeOnly ) {}

		const Type * postvisit( const TypeInstType * aggregateUseType );

		/// Records type variable bindings from forall-statements
		void previsit( const FunctionType * type );
		/// Records type variable bindings from forall-statements and instantiations of generic types
		// void handleAggregateType( const BaseInstType * type );

		// void previsit( const StructInstType * aggregateUseType );
		// void previsit( const UnionInstType * aggregateUseType );

		const TypeSubstitution & sub;
		int subCount = 0;
		bool freeOnly;
		typedef std::unordered_set< TypeInstType::TypeEnvKey > BoundVarsType;
		BoundVarsType boundVars;

};

template< typename SynTreeClass >
TypeSubstitution::ApplyResult<SynTreeClass> TypeSubstitution::apply( const SynTreeClass * input ) const {
	assert( input );
	Pass<Substituter> sub( *this, false );
	input = strict_dynamic_cast< const SynTreeClass * >( input->accept( sub ) );
	return { input, sub.core.subCount };
}

template< typename SynTreeClass >
TypeSubstitution::ApplyResult<SynTreeClass> TypeSubstitution::applyFree( const SynTreeClass * input ) const {
	assert( input );
	Pass<Substituter> sub( *this, true );
	input = strict_dynamic_cast< const SynTreeClass * >( input->accept( sub ) );
	return { input, sub.core.subCount };
}

} // namespace ast

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
