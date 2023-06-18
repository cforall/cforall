//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Candidate.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed Jun 5 14:30:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jun 12 14:15:00 2019
// Update Count     : 2
//

#pragma once

#include <iosfwd>
#include <memory>        // for shared_ptr
#include <vector>

#include "Cost.h"
#include "AST/Node.hpp"
#include "AST/TypeEnvironment.hpp"
#include "Common/Indenter.h"

namespace ast {
	class Expr;

	/// A list of unresolved assertions
	using AssertionList = std::vector<AssertionSet::value_type>;

	/// Convenience to merge AssertionList into AssertionSet
	static inline void mergeAssertionSet( AssertionSet & dst, const AssertionList & src ) {
		for ( const auto & s : src ) { dst.emplace( s ); }
	}
}

namespace ResolvExpr {

/// One option for resolution of an expression
struct Candidate {
	ast::ptr<ast::Expr> expr;  ///< Satisfying expression
	Cost cost;                 ///< Cost of the whole expression
	Cost cvtCost;              ///< Cost of conversions to satisfying expression
	ast::TypeEnvironment env;  ///< Containing type environment
	ast::OpenVarSet open;      ///< Open variables for environment
	ast::AssertionList need;   ///< Assertions which need to be resolved

	Candidate() : expr(), cost( Cost::zero ), cvtCost( Cost::zero ), env(), open(), need() {}

	Candidate( const ast::Expr * x, const ast::TypeEnvironment & e )
	: expr( x ), cost( Cost::zero ), cvtCost( Cost::zero ), env( e ), open(), need() {
		assert(x->result);
	}

	Candidate( const Candidate & o, const ast::Expr * x, const Cost & addedCost = Cost::zero )
	: expr( x ), cost( o.cost + addedCost ), cvtCost( Cost::zero ), env( o.env ), open( o.open ),
	  need( o.need ) {
		assert(x->result);
	}

	Candidate(
		const ast::Expr * x, const ast::TypeEnvironment & e, const ast::OpenVarSet & o,
		const ast::AssertionSet & n, const Cost & c, const Cost & cvt = Cost::zero )
	: expr( x ), cost( c ), cvtCost( cvt ), env( e ), open( o ), need( n.begin(), n.end() ) {
		assert(x->result);
	}

	Candidate(
		const ast::Expr * x, ast::TypeEnvironment && e, ast::OpenVarSet && o,
		ast::AssertionSet && n, const Cost & c, const Cost & cvt = Cost::zero )
	: expr( x ), cost( c ), cvtCost( cvt ), env( std::move( e ) ), open( std::move( o ) ),
	  need( n.begin(), n.end() ) {
		assert(x->result);
	}
};

/// Shared reference to a candidate
using CandidateRef = std::shared_ptr< Candidate >;

/// List of candidates
using CandidateList = std::vector< CandidateRef >;

/// Sum the cost of a list of candidates
static inline Cost sumCost( const CandidateList & candidates ) {
	Cost total = Cost::zero;
	for ( const CandidateRef & r : candidates ) { total += r->cost; }
	return total;
}

/// Holdover behaviour from old `findMinCost` -- xxx -- can maybe be eliminated?
/*
static inline void promoteCvtCost( CandidateList & candidates ) {
	for ( CandidateRef & r : candidates ) {
		r->cost = r->cvtCost;
	}
}
*/

void print( std::ostream & os, const Candidate & cand, Indenter indent = {} );

void print( std::ostream & os, const CandidateList & cands, Indenter indent = {} );

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
