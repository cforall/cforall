//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeEnvironment.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 29 11:00:00 2019
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Dec 11 21:55:54 2019
// Update Count     : 3
//

#pragma once

#include <iostream>
#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "Decl.hpp"
#include "Node.hpp"                // for ptr_base, ptr, readonly
#include "SymbolTable.hpp"
#include "Type.hpp"
#include "TypeSubstitution.hpp"
#include "Common/Indenter.h"
#include "ResolvExpr/WidenMode.h"

namespace ast {

/// Comparator/uniqueness operator for assertion sets.
///
/// Adding this comparison operator significantly improves assertion satisfaction run time for
/// some cases. The current satisfaction algorithm's speed partially depends on the order of
/// assertions. Assertions which have fewer possible matches should appear before assertions
/// which have more possible matches. This seems to imply that this could be further improved
/// by providing an indexer as an additional argument and ordering based on the number of
/// matches of the same kind (object, function) for the names of the declarations.
///
/// I've seen a TU go from 54 minutes to 1 minute 34 seconds with the addition of this
/// comparator.
///
/// Note: since this compares pointers for position, minor changes in the source file that
/// affect memory layout can alter compilation time in unpredictable ways. For example, the
/// placement of a line directive can reorder type pointers with respect to each other so that
/// assertions are seen in different orders, causing a potentially different number of
/// unification calls when resolving assertions. I've seen a TU go from 36 seconds to 27
/// seconds by reordering line directives alone, so it would be nice to fix this comparison so
/// that assertions compare more consistently. I've tried to modify this to compare on mangle
/// name instead of type as the second comparator, but this causes some assertions to never be
/// recorded. More investigation is needed.
struct AssertCompare {
	bool operator()( const VariableExpr * d1, const VariableExpr * d2 ) const {
		auto kind1 = ast::SymbolTable::getSpecialFunctionKind(d1->var->name);
		auto kind2 = ast::SymbolTable::getSpecialFunctionKind(d2->var->name);
		// heuristics optimization: force special functions to go last
		if (kind1 > kind2) return true;
		else if (kind1 < kind2) return false;

		int cmp = d1->var->name.compare( d2->var->name );
		return cmp < 0 || ( cmp == 0 && d1->result < d2->result );
	}
};

/// Data for pending assertion satisfaction
struct AssertionSetValue {
	bool isUsed;        ///< True if assertion needs to be satisfied
	UniqueId resnSlot;  ///< ID of slot assertion belongs to

	AssertionSetValue() : isUsed(false), resnSlot(0) {}
};

/// Set of assertions pending satisfaction
using AssertionSet = std::map< const VariableExpr *, AssertionSetValue, AssertCompare >;

/// Set of open variables
using OpenVarSet = std::unordered_map< TypeEnvKey, TypeData >;

/// Merges one set of open vars into another
/// merges one set of open vars into another
static inline void mergeOpenVars( OpenVarSet& dst, const OpenVarSet& src ) {
	for ( const auto& entry : src ) { dst[ entry.first ] = entry.second; }
}

/// Print an assertion set
void print( std::ostream &, const AssertionSet &, Indenter indent = {} );
/// Print an open variable set
void print( std::ostream &, const OpenVarSet &, Indenter indent = {} );

/// Represents an equivalence class of bound type variables, optionally with the concrete type
/// they bind to.
struct EqvClass {
	std::unordered_set< TypeEnvKey > vars;
	ptr<Type> bound;
	bool allowWidening;
	TypeData data;

	EqvClass() : vars(), bound(), allowWidening( true ), data() {}

	/// Copy-with-bound constructor
	EqvClass( const EqvClass & o, const Type * b )
	: vars( o.vars ), bound( b ), allowWidening( o.allowWidening ), data( o.data ) {}

	/// Singleton class constructor from TypeDecl
	EqvClass( const TypeInstType * inst )
	: vars{ *inst }, bound(), allowWidening( true ), data( inst->base ) {}

	/// Singleton class constructor from substitution
	EqvClass( const TypeEnvKey & v, const Type * b )
	: vars{ v }, bound( b ), allowWidening( false ), data( TypeDecl::Dtype, false ) {}

	/// Single-var constructor (strips qualifiers from bound type)
	EqvClass( const TypeEnvKey & v, const Type * b, bool w, const TypeData & d )
	: vars{ v }, bound( b ), allowWidening( w ), data( d ) {
		reset_qualifiers( bound );
	}

	/// Double-var constructor
	EqvClass( const TypeEnvKey & v, const TypeEnvKey & u, bool w, const TypeData & d )
	: vars{ v, u }, bound(), allowWidening( w ), data( d ) {}

};

void print( std::ostream & out, const EqvClass & clz, Indenter indent = {} );

/// A partitioning of type variables into equivalence classes
class TypeEnvironment {
	/// The underlying list of equivalence classes
	using ClassList = std::list< EqvClass >;

	ClassList env;

public:
	/// Finds the equivalence class containing a variable; nullptr for none such
	const EqvClass * lookup( const TypeEnvKey & var ) const;

	/// Add a new equivalence class for each type variable
	void add( const FunctionType::ForallList & tyDecls );

	/// Add a new equivalence class for each branch of the substitution, checking for conflicts
	void add( const TypeSubstitution & sub );

	/// Writes all the substitutions in this environment into a substitution
	void writeToSubstitution( TypeSubstitution & sub ) const;

	template< typename node_t >
	auto apply( node_t && type ) const {
		TypeSubstitution sub;
		writeToSubstitution( sub );
		return sub.apply( std::forward<node_t>(type) );
	}

	template< typename node_t >
	auto applyFree( node_t && type ) const {
		TypeSubstitution sub;
		writeToSubstitution( sub );
		return sub.applyFree( std::forward<node_t>(type) );
	}

	bool empty() const { return env.empty(); }

	/// Concatenate environment onto this one; no safety checks performed
	void simpleCombine( const TypeEnvironment & o );

	/// Merge environment with this one, checking compatibility.
	/// Returns false if fails, but does NOT roll back partial changes.
	bool combine( const TypeEnvironment & o, OpenVarSet & openVars );

	/// Add all type variables in environment to open var list
	void extractOpenVars( OpenVarSet & openVars ) const;

	/// Iteratively adds the environment of a new actual (with allowWidening = false),
	/// and extracts open variables.
	void addActual( const TypeEnvironment & actualEnv, OpenVarSet & openVars );

	/// Binds the type class represented by `typeInst` to the type `bindTo`; will add the class if
	/// needed. Returns false on failure.
	bool bindVar(
		const TypeInstType * typeInst, const Type * bindTo, const TypeData & data,
		AssertionSet & need, AssertionSet & have, const OpenVarSet & openVars,
		ResolvExpr::WidenMode widen );

	/// Binds the type classes represented by `var1` and `var2` together; will add one or both
	/// classes if needed. Returns false on failure.
	bool bindVarToVar(
		const TypeInstType * var1, const TypeInstType * var2, TypeData && data,
		AssertionSet & need, AssertionSet & have, const OpenVarSet & openVars,
		ResolvExpr::WidenMode widen );

	/// Disallows widening for all bindings in the environment
	void forbidWidening();

	using iterator = ClassList::const_iterator;
	iterator begin() const { return env.begin(); }
	iterator end() const { return env.end(); }

private:
	/// Add an equivalence class to the environment, checking for existing conflicting classes
	void add( EqvClass && eqvClass );

	/// Unifies the type bound of `to` with the type bound of `from`, returning false if fails
	bool mergeBound(
		EqvClass & to, const EqvClass & from, OpenVarSet & openVars );

	/// Merges two type classes from local environment, returning false if fails
	bool mergeClasses(
		ClassList::iterator to, ClassList::iterator from, OpenVarSet & openVars);

	/// Private lookup API; returns array index of string, or env.size() for not found
	ClassList::iterator internal_lookup( const TypeEnvKey & );
};

void print( std::ostream & out, const TypeEnvironment & env, Indenter indent = {} );

std::ostream & operator<<( std::ostream & out, const TypeEnvironment & env );

} // namespace ast

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
