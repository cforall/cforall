//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeEnvironment.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:24:58 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri Jul 19 17:00:10 2019
// Update Count     : 10
//

#pragma once

#include <iostream>                    // for ostream
#include <list>                        // for list, list<>::iterator, list<>...
#include <map>						   // for map, map<>::value_compare
#include <unordered_map>
#include <set>						   // for set
#include <string>                      // for string
#include <utility>                     // for move, swap

#include "WidenMode.h"                 // for WidenMode

#include "SynTree/Declaration.h"       // for TypeDecl::Data, DeclarationWit...
#include "SynTree/SynTree.h"           // for UniqueId
#include "SynTree/Type.h"              // for Type, Type::ForallList
#include "SynTree/TypeSubstitution.h"  // for TypeSubstitution

namespace ResolvExpr {
	// adding this comparison operator significantly improves assertion resolution run time for
	// some cases. The current resolution algorithm's speed partially depends on the order of
	// assertions. Assertions which have fewer possible matches should appear before
	// assertions which have more possible matches. This seems to imply that this could
	// be further improved by providing an indexer as an additional argument and ordering based
	// on the number of matches of the same kind (object, function) for the names of the
	// declarations.
	//
	// I've seen a TU go from 54 minutes to 1 minute 34 seconds with the addition of this
	// comparator.
	//
	// Note: since this compares pointers for position, minor changes in the source file that affect
	// memory layout can alter compilation time in unpredictable ways. For example, the placement
	// of a line directive can reorder type pointers with respect to each other so that assertions
	// are seen in different orders, causing a potentially different number of unification calls
	// when resolving assertions. I've seen a TU go from 36 seconds to 27 seconds by reordering
	// line directives alone, so it would be nice to fix this comparison so that assertions compare
	// more consistently. I've tried to modify this to compare on mangle name instead of type as
	// the second comparator, but this causes some assertions to never be recorded. More
	// investigation is needed.
	struct AssertCompare {
		bool operator()( const DeclarationWithType * d1, const DeclarationWithType * d2 ) const {
			int cmp = d1->get_name().compare( d2->get_name() );
			return cmp < 0 ||
				( cmp == 0 && d1->get_type() < d2->get_type() );
		}
	};
	struct AssertionSetValue {
		bool isUsed;        ///< True if assertion needs to be resolved
		UniqueId resnSlot;  ///< ID of slot assertion belongs to

		AssertionSetValue() : isUsed(false), resnSlot(0) {}
	};
	typedef std::map< const DeclarationWithType *, AssertionSetValue, AssertCompare > AssertionSet;
	typedef std::unordered_map< std::string, TypeDecl::Data > OpenVarSet;

	/// merges one set of open vars into another
	static inline void mergeOpenVars( OpenVarSet& dst, const OpenVarSet& src ) {
		for ( const auto& entry : src ) { dst[ entry.first ] = entry.second; }
	}

	void printAssertionSet( const AssertionSet &, std::ostream &, int indent = 0 );
	void printOpenVarSet( const OpenVarSet &, std::ostream &, int indent = 0 );

	struct EqvClass {
		std::set< std::string > vars;
		Type * type;
		bool allowWidening;
		TypeDecl::Data data;

		void initialize( const EqvClass &src, EqvClass &dest );
		void initialize( const EqvClass &src, EqvClass &dest, const Type *ty );
		EqvClass();
		EqvClass( const EqvClass &other );
		EqvClass( const EqvClass &other, const Type *ty );
		EqvClass( EqvClass &&other );
		EqvClass &operator=( const EqvClass &other );
		EqvClass &operator=( EqvClass &&other );
		~EqvClass();
		void print( std::ostream &os, Indenter indent = {} ) const;

		/// Takes ownership of `ty`, freeing old `type`
		void set_type(Type* ty);
	};

	class TypeEnvironment {
		using ClassList = std::list< EqvClass >;
	  public:
		const EqvClass* lookup( const std::string &var ) const;
	  private:
		void add( EqvClass &&eqvClass  );
	  public:
		void add( const Type::ForallList &tyDecls );
		void add( const TypeSubstitution & sub );
		template< typename SynTreeClass > int apply( SynTreeClass *&type ) const;
		template< typename SynTreeClass > int applyFree( SynTreeClass *&type ) const;
		void makeSubstitution( TypeSubstitution &result ) const;
		bool isEmpty() const { return env.empty(); }
		void print( std::ostream &os, Indenter indent = {} ) const;

		/// Simply concatenate the second environment onto this one; no safety checks performed
		void simpleCombine( const TypeEnvironment &second );

	  private:
		/// Unifies the type bound of to with the type bound of from, returning false if fails
		bool mergeBound( EqvClass& to, const EqvClass& from, OpenVarSet& openVars, const SymTab::Indexer& indexer );

		/// Merges two type classes from local environment, returning false if fails
		bool mergeClasses( ClassList::iterator to, ClassList::iterator from, OpenVarSet& openVars, const SymTab::Indexer& indexer );

	  public:
		/// Merges the second environment with this one, checking compatibility.
		/// Returns false if fails, but does NOT roll back partial changes.
		bool combine( const TypeEnvironment& second, OpenVarSet& openVars, const SymTab::Indexer& indexer );

		void extractOpenVars( OpenVarSet &openVars ) const;
		TypeEnvironment *clone() const { return new TypeEnvironment( *this ); }

		/// Iteratively adds the environment of a new actual (with allowWidening = false),
		/// and extracts open variables.
		void addActual( const TypeEnvironment& actualEnv, OpenVarSet& openVars );

		/// Binds the type class represented by `typeInst` to the type `bindTo`; will add
		/// the class if needed. Returns false on failure.
		bool bindVar( const TypeInstType * typeInst, Type * bindTo, const TypeDecl::Data & data, AssertionSet &need, AssertionSet &have, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer );

		/// Binds the type classes represented by `var1` and `var2` together; will add
		/// one or both classes if needed. Returns false on failure.
		bool bindVarToVar( const TypeInstType * var1, const TypeInstType * var2, TypeDecl::Data && data, AssertionSet &need, AssertionSet &have, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer );

		/// Disallows widening for all bindings in the environment
		void forbidWidening();

		using iterator = ClassList::const_iterator;
		iterator begin() const { return env.begin(); }
		iterator end() const { return env.end(); }

		auto size() const { return env.size(); }

	  private:
		ClassList env;

		ClassList::iterator internal_lookup( const std::string &var );
	};

	template< typename SynTreeClass >
	int TypeEnvironment::apply( SynTreeClass *&type ) const {
		TypeSubstitution sub;
		makeSubstitution( sub );
		return sub.apply( type );
	}

	template< typename SynTreeClass >
	int TypeEnvironment::applyFree( SynTreeClass *&type ) const {
		TypeSubstitution sub;
		makeSubstitution( sub );
		return sub.applyFree( type );
	}

	std::ostream & operator<<( std::ostream & out, const TypeEnvironment & env );
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
