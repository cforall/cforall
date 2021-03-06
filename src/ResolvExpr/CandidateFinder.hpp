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
// Last Modified On : Tue Oct  1  9:51:00 2019
// Update Count     : 2
//

#pragma once

#include "Candidate.hpp"
#include "ResolvMode.h"
#include "AST/Fwd.hpp"
#include "AST/Node.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/TypeEnvironment.hpp"

namespace ResolvExpr {

/// Data to perform expression resolution
struct CandidateFinder {
	CandidateList candidates;          ///< List of candidate resolutions
	const ast::SymbolTable & localSyms;   ///< Symbol table to lookup candidates
	const ast::TypeEnvironment & env;  ///< Substitutions performed in this resolution
	ast::ptr< ast::Type > targetType;  ///< Target type for resolution
	std::set< std::string > otypeKeys;  /// different type may map to same key

	CandidateFinder(
		const ast::SymbolTable & syms, const ast::TypeEnvironment & env,
		const ast::Type * tt = nullptr )
	: candidates(), localSyms( syms ), env( env ), targetType( tt ) {}

	/// Fill candidates with feasible resolutions for `expr`
	void find( const ast::Expr * expr, ResolvMode mode = {} );
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

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
