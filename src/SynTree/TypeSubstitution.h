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
#include <iosfwd>                  // for ostream
#include <list>                    // for list<>::iterator, _List_iterator
#include <unordered_map>
#include <unordered_set>
#include <string>                  // for string, operator!=
#include <utility>                 // for pair

#include "Common/SemanticError.h"  // for SemanticError
#include "SynTree/Declaration.h"   // for TypeDecl, Declaration (ptr only)
#include "SynTree/Expression.h"    // for Expression (ptr only), NameExpr (p...
#include "SynTree/Type.h"          // for Type, ArrayType (ptr only), BasicT...

class TypeSubstitution {
  public:
	TypeSubstitution();
	template< typename FormalIterator, typename ActualIterator >
	TypeSubstitution( FormalIterator formalBegin, FormalIterator formalEnd, ActualIterator actualBegin );
	TypeSubstitution( const TypeSubstitution &other );
	virtual ~TypeSubstitution();

	TypeSubstitution &operator=( const TypeSubstitution &other );

	template< typename SynTreeClass > int apply( SynTreeClass *&input ) const;
	template< typename SynTreeClass > int applyFree( SynTreeClass *&input ) const;

	void add( std::string formalType, Type *actualType );
	void add( const TypeSubstitution &other );
	void remove( std::string formalType );
	Type *lookup( std::string formalType ) const;
	bool empty() const;

	void addVar( std::string formalExpr, Expression *actualExpr );

	template< typename FormalIterator, typename ActualIterator >
	void add( FormalIterator formalBegin, FormalIterator formalEnd, ActualIterator actualBegin );

	/// this function is unused...
	template< typename TypeInstListIterator >
	void extract( TypeInstListIterator begin, TypeInstListIterator end, TypeSubstitution &result );

	/// create a new TypeSubstitution using bindings from env containing all of the type variables in expr
	static TypeSubstitution * newFromExpr( Expression * expr, const TypeSubstitution * env );

	void normalize();

	TypeSubstitution * acceptMutator( Mutator & m ) { return m.mutate( this ); }

	void print( std::ostream &os, Indenter indent = {} ) const;
	TypeSubstitution *clone() const { return new TypeSubstitution( *this ); }
  private:

	// Mutator that performs the substitution
	struct Substituter;

	// TODO: worry about traversing into a forall-qualified function type or type decl with assertions

	void initialize( const TypeSubstitution &src, TypeSubstitution &dest );

	friend class Mutator;

	template<typename pass_type>
	friend class PassVisitor;

	typedef std::unordered_map< std::string, Type * > TypeEnvType;
	typedef std::unordered_map< std::string, Expression * > VarEnvType;
	TypeEnvType typeEnv;
	VarEnvType varEnv;

  public:
	// has to come after declaration of typeEnv
	auto begin()       -> decltype( typeEnv.begin() ) { return typeEnv.begin(); }
	auto   end()       -> decltype( typeEnv.  end() ) { return typeEnv.  end(); }
	auto begin() const -> decltype( typeEnv.begin() ) { return typeEnv.begin(); }
	auto   end() const -> decltype( typeEnv.  end() ) { return typeEnv.  end(); }

	auto beginVar()       -> decltype( varEnv.begin() ) { return varEnv.begin(); }
	auto   endVar()       -> decltype( varEnv.  end() ) { return varEnv.  end(); }
	auto beginVar() const -> decltype( varEnv.begin() ) { return varEnv.begin(); }
	auto   endVar() const -> decltype( varEnv.  end() ) { return varEnv.  end(); }
};

template< typename FormalIterator, typename ActualIterator >
void TypeSubstitution::add( FormalIterator formalBegin, FormalIterator formalEnd, ActualIterator actualBegin ) {
	// FormalIterator points to a TypeDecl
	// ActualIterator points to a Type
	FormalIterator formalIt = formalBegin;
	ActualIterator actualIt = actualBegin;
	for ( ; formalIt != formalEnd; ++formalIt, ++actualIt ) {
		if ( TypeDecl *formal = dynamic_cast< TypeDecl * >( *formalIt ) ) {
			if ( TypeExpr *actual = dynamic_cast< TypeExpr * >( *actualIt ) ) {
				if ( formal->get_name() != "" ) {
					TypeEnvType::iterator i = typeEnv.find( formal->get_name() );
					if ( i != typeEnv.end() ) {
						delete i->second;
					} // if
					typeEnv[ formal->get_name() ] = actual->get_type()->clone();
				} // if
			} else {
				SemanticError( formal, toString( "Attempt to provide non-type parameter: ", toString( *actualIt ).c_str(), " for type parameter " ) );
			} // if
		} else {
			// TODO: type check the formal and actual parameters
			if ( (*formalIt)->get_name() != "" ) {
				varEnv[ (*formalIt)->get_name() ] = (*actualIt)->clone();
			} // if
		} // if
	} // for
}

template< typename FormalIterator, typename ActualIterator >
TypeSubstitution::TypeSubstitution( FormalIterator formalBegin, FormalIterator formalEnd, ActualIterator actualBegin ) {
	add( formalBegin, formalEnd, actualBegin );
}

// include needs to happen after TypeSubstitution is defined so that both TypeSubstitution and
// PassVisitor are defined before PassVisitor implementation accesses TypeSubstitution internals.
#include "Common/PassVisitor.h"

// definitition must happen after PassVisitor is included so that WithGuards can be used
struct TypeSubstitution::Substituter : public WithGuards, public WithVisitorRef<Substituter> {
		Substituter( const TypeSubstitution & sub, bool freeOnly ) : sub( sub ), freeOnly( freeOnly ) {}

		Type * postmutate( TypeInstType * aggregateUseType );
		Expression * postmutate( NameExpr * nameExpr );

		/// Records type variable bindings from forall-statements
		void premutate( Type * type );
		/// Records type variable bindings from forall-statements and instantiations of generic types
		template< typename TypeClass > void handleAggregateType( TypeClass * type );

		void premutate( StructInstType * aggregateUseType );
		void premutate( UnionInstType * aggregateUseType );

		const TypeSubstitution & sub;
		int subCount = 0;
		bool freeOnly;
		typedef std::unordered_set< std::string > BoundVarsType;
		BoundVarsType boundVars;
};

template< typename SynTreeClass >
int TypeSubstitution::apply( SynTreeClass *&input ) const {
	assert( input );
	PassVisitor<Substituter> sub( *this, false );
	input = dynamic_cast< SynTreeClass * >( input->acceptMutator( sub ) );
	assert( input );
///	std::cerr << "substitution result is: ";
///	newType->print( std::cerr );
///	std::cerr << std::endl;
	return sub.pass.subCount;
}

template< typename SynTreeClass >
int TypeSubstitution::applyFree( SynTreeClass *&input ) const {
	assert( input );
	PassVisitor<Substituter> sub( *this, true );
	input = dynamic_cast< SynTreeClass * >( input->acceptMutator( sub ) );
	assert( input );
///	std::cerr << "substitution result is: ";
///	newType->print( std::cerr );
///	std::cerr << std::endl;
	return sub.pass.subCount;
}

template< typename TypeInstListIterator >
void TypeSubstitution::extract( TypeInstListIterator begin, TypeInstListIterator end, TypeSubstitution &result ) {
	// xxx - this function doesn't extract varEnv - is this intentional?
	while ( begin != end ) {
		TypeEnvType::iterator cur = typeEnv.find( (*begin++)->get_name() );
		if ( cur != typeEnv.end() ) {
			result.typeEnv[ cur->first ] = cur->second;
			typeEnv.erase( cur );
		} // if
	} // while
}

/// Instantiate each member of the context given the actual parameters specified, and store the
/// instantiations for use by the indexer
template< typename FormalIterator, typename ActualIterator, typename MemberIterator, typename OutputIterator >
void applySubstitution( FormalIterator formalBegin, FormalIterator formalEnd, ActualIterator actual, MemberIterator memberBegin, MemberIterator memberEnd, OutputIterator out ) {
	TypeSubstitution sub = TypeSubstitution( formalBegin, formalEnd, actual );
	for ( auto i = memberBegin; i != memberEnd; ++i ) {
		sub.apply( *i );
		*out++ = *i;
	} // for
}

std::ostream & operator<<( std::ostream & out, const TypeSubstitution & sub );

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
