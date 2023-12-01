//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CandidateFinder.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed Jun 5 14:30:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Wed Mar 16 15:22:00 2022
// Update Count     : 3
//

#pragma once

#include "Candidate.hpp"
#include "ResolveMode.hpp"
#include "AST/Fwd.hpp"
#include "AST/Node.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/TypeEnvironment.hpp"

namespace ResolvExpr {

struct ResolveContext;

/// Data to perform expression resolution
struct CandidateFinder {
	CandidateList candidates;          ///< List of candidate resolutions
	const ResolveContext & context;  ///< Information about where the canditates are being found.
	const ast::TypeEnvironment & env;  ///< Substitutions performed in this resolution
	ast::ptr< ast::Type > targetType;  ///< Target type for resolution
	bool strictMode = false;           ///< If set to true, requires targetType to be exact match (inside return cast)
	bool allowVoid = false;            ///< If set to true, allow void-returning function calls (only top level, cast to void and first in comma)
	std::set< std::string > otypeKeys;  /// different type may map to same key

	CandidateFinder(
		const ResolveContext & context, const ast::TypeEnvironment & env,
		const ast::Type * tt = nullptr )
	: candidates(), context( context ), env( env ), targetType( tt ) {}

	/// Fill candidates with feasible resolutions for `expr`
	void find( const ast::Expr * expr, ResolveMode mode = {} );
	bool pruneCandidates( CandidateList & candidates, CandidateList & out, std::vector<std::string> & errors );

	/// Runs new candidate finder on each element in xs, returning the list of finders
	std::vector< CandidateFinder > findSubExprs( const std::vector< ast::ptr< ast::Expr > > & xs );

	using value_type = CandidateList::value_type;
	using iterator = CandidateList::iterator;
	using const_iterator = CandidateList::const_iterator;

	iterator begin() { return candidates.begin(); }
	const_iterator begin() const { return candidates.begin(); }

	iterator end() { return candidates.end(); }
	const_iterator end() const { return candidates.end(); }
};

/// Computes conversion cost between two types
Cost computeConversionCost(
	const ast::Type * argType, const ast::Type * paramType, bool argIsLvalue,
	const ast::SymbolTable & symtab, const ast::TypeEnvironment & env );

/// Create an expression that preforms reference to rvalue conversion on
/// the given expression and update the cost of the expression.
const ast::Expr * referenceToRvalueConversion(
	const ast::Expr * expr, Cost & cost );

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
