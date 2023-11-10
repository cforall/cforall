//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Explode.h --
//
// Author           : Rob Schluntz
// Created On       : Wed Nov 9 13:12:24 2016
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jun 17 14:36:00 2019
// Update Count     : 4
//

#pragma once

#include <iterator>                     // for back_inserter, back_insert_iterator
#include <utility>                      // for forward

#include "AST/Expr.hpp"
#include "ResolvExpr/Candidate.hpp"     // for Candidate, CandidateList
#include "ResolvExpr/ExplodedArg.hpp"   // for ExplodedArg
#include "Tuples.h"                     // for maybeImpure

namespace ast {
	class SymbolTable;
}

namespace Tuples {

const ast::Expr * distributeReference( const ast::Expr * );

/// Append candidate to an OutputIterator of Candidates.
template<typename OutputIterator>
void append( OutputIterator out, const ast::Expr * expr, const ast::TypeEnvironment & env,
		const ast::OpenVarSet & open, const ast::AssertionList & need,
		const ResolvExpr::Cost & cost, const ResolvExpr::Cost & cvtCost ) {
	ast::TypeEnvironment copyEnv = env;
	ast::OpenVarSet copyOpen = open;
	ast::AssertionSet set;
	mergeAssertionSet( set, need );
	*out++ = std::make_shared<ResolvExpr::Candidate>( expr, std::move( copyEnv ),
		std::move( copyOpen ), std::move( set ), cost, cvtCost );
}

/// Append candidate to an ExplodedArg.
static inline void append( ResolvExpr::ExplodedArg& ea, const ast::Expr * expr,
		const ast::TypeEnvironment&, const ast::OpenVarSet&,
		const ast::AssertionList&, const ResolvExpr::Cost&, const ResolvExpr::Cost& ) {
	// I'm not sure why most of the arguments are unused. But they were in the old version.
	ea.exprs.emplace_back( expr );
}

/// Check if the expression is a cast to a reference type, return it if it is.
static inline const ast::CastExpr * isReferenceCast( const ast::Expr * expr ) {
	if ( const ast::CastExpr * cast = dynamic_cast< const ast::CastExpr * >( expr ) ) {
		if ( dynamic_cast< const ast::ReferenceType * >( cast->result.get() ) ) {
			return cast;
		}
	}
	return nullptr;
}

/// helper function (indirectely) used by explode
template< typename Output >
void explodeRecursive(
	const ast::CastExpr *, const ResolvExpr::Candidate &,
	const ast::SymbolTable &, Output &&
) {
}

/// helper function used by explode
template< typename Output >
void explodeUnique(
	const ast::ptr< ast::Expr > & expr, const ResolvExpr::Candidate & arg,
	const ast::SymbolTable & symtab, Output && out, bool isTupleAssign
) {
	// Tuple assignment can use a faster method if it is cast. Uses recursive exploding.
	if ( isTupleAssign ) if ( const ast::CastExpr * castExpr = isReferenceCast( expr ) ) {
		ResolvExpr::CandidateList candidates;
		explodeUnique( castExpr->arg, arg, symtab, back_inserter( candidates ), true );
		for ( ResolvExpr::CandidateRef & cand : candidates ) {
			// Distribute the reference cast over all components of the candidate.
			append( std::forward<Output>(out), distributeReference( cand->expr ), cand->env,
				cand->open, cand->need, cand->cost, cand->cvtCost );
		}
		return;
	}
	const ast::Type * res = expr->result->stripReferences();
	if ( const ast::TupleType * tupleType = dynamic_cast< const ast::TupleType * >( res ) ) {
		if ( const ast::ptr< ast::TupleExpr > & tupleExpr = expr.as< ast::TupleExpr >() ) {
			// Open the tuple expr and continue on its components.
			for ( const ast::Expr * expr : tupleExpr->exprs ) {
				explodeUnique( expr, arg, symtab, std::forward<Output>(out), isTupleAssign );
			}
		} else {
			ast::ptr< ast::Expr > local = expr;
			// Expressions which may have side effects require a single unique instance.
			if ( Tuples::maybeImpureIgnoreUnique( local ) ) {
				local = new ast::UniqueExpr( local->location, local );
			}
			// Cast a reference away to a value-type to allow further explosion.
			if ( local->result.as< ast::ReferenceType >() ) {
				local = new ast::CastExpr{ local, tupleType };
			}
			// Now we have to go across the tuple via indexing.
			for ( unsigned int i = 0 ; i < tupleType->size() ; ++i ) {
				ast::TupleIndexExpr * idx = new ast::TupleIndexExpr( local->location, local, i );
				explodeUnique( idx, arg, symtab, std::forward<Output>(out), isTupleAssign );
				// TODO: We need more input to figure out the exact lifetimes of these types.
				// delete idx;
			}
		}
	} else {
		// For atomic/non-tuple types, no explosion is used.
		append( std::forward<Output>(out), expr, arg.env, arg.open, arg.need, arg.cost,
			arg.cvtCost );
	}
}

/// expands a tuple-valued candidate into multiple candidates, each with a non-tuple type
template< typename Output >
void explode(
	const ResolvExpr::Candidate & arg, const ast::SymbolTable & symtab, Output && out,
	bool isTupleAssign = false
) {
	explodeUnique( arg.expr, arg, symtab, std::forward< Output >( out ), isTupleAssign );
}

/// explode list of candidates into flattened list of candidates
template< typename Output >
void explode(
	const ResolvExpr::CandidateList & cands, const ast::SymbolTable & symtab, Output && out,
	bool isTupleAssign = false
) {
	for ( const ResolvExpr::CandidateRef & cand : cands ) {
		explode( *cand, symtab, std::forward< Output >( out ), isTupleAssign );
	}
}

} // namespace Tuples

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
