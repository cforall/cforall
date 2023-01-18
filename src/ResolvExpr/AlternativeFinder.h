//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AlternativeFinder.h --
//
// Author           : Richard C. Bilson
// Created On       : Sat May 16 23:56:12 2015
// Last Modified By : Aaron B. Moss
// Last Modified On : Fri Oct -5 10:01:00 2018
// Update Count     : 5
//

#pragma once

#include <algorithm>                     // for copy
#include <list>                          // for list
#include <string>                        // for string

#include "Alternative.h"                 // for AltList, Alternative
#include "ExplodedActual.h"              // for ExplodedActual
#include "ResolvExpr/Cost.h"             // for Cost, Cost::infinity
#include "ResolvExpr/TypeEnvironment.h"  // for AssertionSet, OpenVarSet
#include "ResolvMode.h"                  // for ResolvMode
#include "SynTree/Visitor.h"             // for Visitor
#include "SynTree/SynTree.h"             // for Visitor Nodes

namespace SymTab {
class Indexer;
}  // namespace SymTab

namespace ResolvExpr {
	struct ArgPack;

	Cost computeConversionCost( Type * actualType, Type * formalType, bool actualIsLvalue,
		const SymTab::Indexer & indexer, const TypeEnvironment & env );

	void referenceToRvalueConversion( Expression *& expr, Cost & cost );

	/// First index is which argument, second index is which alternative for that argument,
	/// third index is which exploded element of that alternative
	using ExplodedArgs_old = std::vector< std::vector< ExplodedActual > >;

	class AlternativeFinder {
	  public:
		AlternativeFinder( const SymTab::Indexer &indexer, const TypeEnvironment &env );

		AlternativeFinder( const AlternativeFinder& o )
			: indexer(o.indexer), alternatives(o.alternatives), env(o.env),
			  targetType(o.targetType) {}

		AlternativeFinder( AlternativeFinder&& o )
			: indexer(o.indexer), alternatives(std::move(o.alternatives)), env(o.env),
			  targetType(o.targetType) {}

		AlternativeFinder& operator= ( const AlternativeFinder& o ) {
			if (&o == this) return *this;

			// horrific nasty hack to rebind references...
			alternatives.~AltList();
			new(this) AlternativeFinder(o);
			return *this;
		}

		AlternativeFinder& operator= ( AlternativeFinder&& o ) {
			if (&o == this) return *this;

			// horrific nasty hack to rebind references...
			alternatives.~AltList();
			new(this) AlternativeFinder(std::move(o));
			return *this;
		}

		void find( Expression *expr, ResolvMode mode = ResolvMode{} );
		/// Calls find with the adjust flag set; adjustment turns array and function types into equivalent pointer types
		void findWithAdjustment( Expression *expr );
		/// Calls find with the adjust flag set and prune flag unset; pruning ensures there is at most one alternative per result type
		void findWithoutPrune( Expression *expr );
		/// Calls find with the adjust and prune flags set, failFast flags unset; fail fast ensures that there is at least one resulting alternative
		void maybeFind( Expression *expr );
		AltList &get_alternatives() { return alternatives; }

		// make this look like an STL container so that we can apply generic algorithms
		typedef Alternative value_type;
		typedef AltList::iterator iterator;
		typedef AltList::const_iterator const_iterator;
		AltList::iterator begin() { return alternatives.begin(); }
		AltList::iterator end() { return alternatives.end(); }
		AltList::const_iterator begin() const { return alternatives.begin(); }
		AltList::const_iterator end() const { return alternatives.end(); }

		const SymTab::Indexer &get_indexer() const { return indexer; }
		const TypeEnvironment &get_environ() const { return env; }

		/// Runs a new alternative finder on each element in [begin, end)
		/// and writes each alternative finder to out.
		template< typename InputIterator, typename OutputIterator >
		void findSubExprs( InputIterator begin, InputIterator end, OutputIterator out );
	  private:
		struct Finder;
		const SymTab::Indexer &indexer;
		AltList alternatives;
		const TypeEnvironment &env;
		Type * targetType = nullptr;
	}; // AlternativeFinder

	Expression *resolveInVoidContext( Expression *expr, const SymTab::Indexer &indexer, TypeEnvironment &env );

	template< typename InputIterator, typename OutputIterator >
	void findMinCost( InputIterator begin, InputIterator end, OutputIterator out ) {
		AltList alternatives;

		// select the alternatives that have the minimum parameter cost
		Cost minCost = Cost::infinity;
		for ( InputIterator i = begin; i != end; ++i ) {
			if ( i->cost < minCost ) {
				minCost = i->cost;
				i->cost = i->cvtCost;
				alternatives.clear();
				alternatives.push_back( *i );
			} else if ( i->cost == minCost ) {
				i->cost = i->cvtCost;
				alternatives.push_back( *i );
			}
		}
		std::copy( alternatives.begin(), alternatives.end(), out );
	}

	Cost sumCost( const AltList &in );
	void printAlts( const AltList &list, std::ostream &os, unsigned int indentAmt = 0 );

	/// Adds type variables to the open variable set and marks their assertions
	void makeUnifiableVars( Type *type, OpenVarSet &unifiableVars, AssertionSet &needAssertions );

	template< typename InputIterator >
	void simpleCombineEnvironments( InputIterator begin, InputIterator end, TypeEnvironment &result ) {
		while ( begin != end ) {
			result.simpleCombine( (*begin++).env );
		}
	}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
