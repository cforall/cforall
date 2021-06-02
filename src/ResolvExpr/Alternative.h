//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Alternative.h --
//
// Author           : Richard C. Bilson
// Created On       : Sat May 16 23:45:43 2015
// Last Modified By : Aaron B. Moss
// Last Modified On : Thu Oct 11 10:55:00 2018
// Update Count     : 4
//

#pragma once

#include <iosfwd>             // for ostream
#include <vector>             // for vector

#include "Cost.h"             // for Cost
#include "TypeEnvironment.h"  // for TypeEnvironment, AssertionSetValue

#include "Common/utility.h"   // for maybeClone

class Expression;

namespace ResolvExpr {
	/// One assertion to resolve
	struct AssertionItem {
		const DeclarationWithType* decl;
		AssertionSetValue info;

		AssertionItem() = default;
		AssertionItem( const DeclarationWithType* decl, const AssertionSetValue& info )
		: decl(decl), info(info) {}
		AssertionItem( const AssertionSet::value_type& e ) : decl(e.first), info(e.second) {}
		operator AssertionSet::value_type () const { return { decl, info }; }

		// to support cloneAll
		AssertionItem clone() const { return { maybeClone(decl), info }; }
	};
	/// A list of unresolved assertions
	using AssertionList = std::vector<AssertionItem>;

	/// Clones an assertion list into an assertion set
	static inline void cloneAll( const AssertionList& src, AssertionSet& dst ) {
		for ( const AssertionItem& item : src ) {
			dst.emplace( maybeClone(item.decl), item.info );
		}
	}

	/// Clones an assertion set into an assertion list
	static inline void cloneAll( const AssertionSet& src, AssertionList& dst ) {
		dst.reserve( dst.size() + src.size() );
		for ( const auto& entry : src ) {
			dst.emplace_back( maybeClone(entry.first), entry.second );
		}
	}

	/// Clones an assertion list into an assertion list
	static inline void cloneAll( const AssertionList& src, AssertionList& dst ) {
		dst.reserve( dst.size() + src.size() );
		for ( const AssertionItem& item : src ) {
			dst.emplace_back( maybeClone(item.decl), item.info );
		}
	}

	/// One option for resolution of an expression
	struct Alternative {
		Alternative();
		Alternative( Expression *expr, const TypeEnvironment &env );
		Alternative( const Alternative &o, Expression *expr, const Cost &cost );
		Alternative( Expression *expr, const TypeEnvironment &env, const OpenVarSet& openVars, 
			const AssertionList& need, const Cost &cost );
		Alternative( Expression *expr, const TypeEnvironment &env, const OpenVarSet& openVars, 
			const AssertionList& need, const Cost &cost, const Cost &cvtCost );
		Alternative( Expression *expr, const TypeEnvironment &env, const OpenVarSet &openVars, 
			const AssertionSet &need, const Cost &cost);
		Alternative( Expression *expr, const TypeEnvironment &env, const OpenVarSet &openVars, 
			const AssertionSet &need, const Cost &cost, const Cost& cvtCost );
		Alternative( Expression *expr, TypeEnvironment &&env, OpenVarSet &&openVars, 
			AssertionSet &&need, const Cost &cost );
		Alternative( Expression *expr, TypeEnvironment &&env, OpenVarSet &&openVars, 
			AssertionSet &&need, const Cost &cost, const Cost &cvtCost );
		Alternative( const Alternative &other );
		Alternative &operator=( const Alternative &other );
		Alternative( Alternative && other );
		Alternative &operator=( Alternative && other );
		~Alternative();

		void print( std::ostream &os, Indenter indent = {} ) const;

		/// Returns the stored expression, but released from management of this Alternative
		Expression* release_expr() {
			Expression* tmp = expr;
			expr = nullptr;
			return tmp;
		}

		/// Sorts by cost
		bool operator< ( const Alternative& o ) const { return cost < o.cost; }

		Cost cost;            ///< Cost of the whole expression
		Cost cvtCost;         ///< Cost of conversions to the satisfying expression
		Expression *expr;     ///< Satisfying expression
		TypeEnvironment env;  ///< Containing type environment
		OpenVarSet openVars;  ///< Open variables for environment
		AssertionList need;   ///< Assertions which need to be resolved
	};

	typedef std::vector< Alternative > AltList;

	static inline std::ostream & operator<<(std::ostream & os, const ResolvExpr::Alternative & alt) {
		alt.print( os );
		return os;
	}
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
