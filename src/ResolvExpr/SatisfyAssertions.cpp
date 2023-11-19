//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SatisfyAssertions.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Mon Jun 10 17:45:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Tue Oct  1 13:56:00 2019
// Update Count     : 2
//

#include "SatisfyAssertions.hpp"

#include <iostream>
#include <algorithm>
#include <cassert>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include "AdjustExprType.hpp"
#include "Candidate.hpp"
#include "CandidateFinder.hpp"
#include "CommonType.hpp"
#include "Cost.h"
#include "RenameVars.h"
#include "SpecCost.hpp"
#include "typeops.h"
#include "Unify.h"
#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/TypeEnvironment.hpp"
#include "FindOpenVars.h"
#include "Common/FilterCombos.h"
#include "Common/Indenter.h"
#include "GenPoly/GenPoly.h"
#include "SymTab/Mangler.h"

namespace ResolvExpr {

// in CandidateFinder.cpp; unique ID for assertion satisfaction
extern ast::UniqueId globalResnSlot;

namespace {
	/// Post-unification assertion satisfaction candidate
	struct AssnCandidate {
		ast::SymbolTable::IdData cdata;  ///< Satisfying declaration
		ast::ptr< ast::Type > adjType;   ///< Satisfying type
		ast::TypeEnvironment env;        ///< Post-unification environment
		ast::AssertionSet have;          ///< Post-unification have-set
		ast::AssertionSet need;          ///< Post-unification need-set
		ast::OpenVarSet open;            ///< Post-unification open-var-set
		ast::UniqueId resnSlot;          ///< Slot for any recursive assertion IDs

		AssnCandidate(
			const ast::SymbolTable::IdData c, const ast::Type * at, ast::TypeEnvironment && e,
			ast::AssertionSet && h, ast::AssertionSet && n, ast::OpenVarSet && o, ast::UniqueId rs )
		: cdata( c ), adjType( at ), env( std::move( e ) ), have( std::move( h ) ),
		  need( std::move( n ) ), open( std::move( o ) ), resnSlot( rs ) {
			if (!have.empty()) {
				// std::cerr << c.id->location << ':' << c.id->name << std::endl; // I think this was debugging code so I commented it
			}
		  }
	};

	/// List of assertion satisfaction candidates
	using AssnCandidateList = std::vector< AssnCandidate >;

	/// Reference to a single deferred item
	struct DeferRef {
		const ast::VariableExpr * expr;
		const ast::AssertionSetValue & info;
		const AssnCandidate & match;
	};

	/// Wrapper for the deferred items from a single assertion satisfaction.
	/// Acts like an indexed list of DeferRef
	struct DeferItem {
		const ast::VariableExpr * expr;
		const ast::AssertionSetValue & info;
		AssnCandidateList matches;

		DeferItem(
			const ast::VariableExpr * d, const ast::AssertionSetValue & i, AssnCandidateList && ms )
		: expr( d ), info( i ), matches( std::move( ms ) ) {}

		bool empty() const { return matches.empty(); }

		AssnCandidateList::size_type size() const { return matches.size(); }

		DeferRef operator[] ( unsigned i ) const { return { expr, info, matches[i] }; }
	};

	/// List of deferred satisfaction items
	using DeferList = std::vector< DeferItem >;

	/// Set of assertion satisfactions, grouped by resolution ID
	using InferCache = std::unordered_map< ast::UniqueId, ast::InferredParams >;

	/// Lexicographically-ordered vector of costs.
	/// Lexicographic order comes from default operator< on std::vector.
	using CostVec = std::vector< Cost >;

	/// Flag for state iteration
	enum IterateFlag { IterateState };

	/// Intermediate state for satisfying a set of assertions
	struct SatState {
		CandidateRef cand;          ///< Candidate assertion is rooted on
		ast::AssertionList need;    ///< Assertions to find
		ast::AssertionSet newNeed;  ///< Recursive assertions from current satisfied assertions
		DeferList deferred;         ///< Deferred matches
		InferCache inferred;        ///< Cache of already-inferred assertions
		CostVec costs;              ///< Disambiguating costs of recursive assertion satisfaction
		ast::SymbolTable symtab;    ///< Name lookup (depends on previous assertions)

		/// Initial satisfaction state for a candidate
		SatState( CandidateRef & c, const ast::SymbolTable & syms )
		: cand( c ), need(), newNeed(), deferred(), inferred(), costs{ Cost::zero },
		  symtab( syms ) { need.swap( c->need ); }

		/// Update satisfaction state for next step after previous state
		SatState( SatState && o, IterateFlag )
		: cand( std::move( o.cand ) ), need( o.newNeed.begin(), o.newNeed.end() ), newNeed(),
		  deferred(), inferred( std::move( o.inferred ) ), costs( std::move( o.costs ) ),
		  symtab( o.symtab ) { costs.emplace_back( Cost::zero ); }

		/// Field-wise next step constructor
		SatState(
			CandidateRef && c, ast::AssertionSet && nn, InferCache && i, CostVec && cs,
			ast::SymbolTable && syms )
		: cand( std::move( c ) ), need( nn.begin(), nn.end() ), newNeed(), deferred(),
		  inferred( std::move( i ) ), costs( std::move( cs ) ), symtab( std::move( syms ) )
		  { costs.emplace_back( Cost::zero ); }
	};

	enum AssertionResult {Fail, Skip, Success} ;

	/// Binds a single assertion, updating satisfaction state
	void bindAssertion(
		const ast::VariableExpr * expr, const ast::AssertionSetValue & info, CandidateRef & cand,
		AssnCandidate & match, InferCache & inferred
	) {
		const ast::DeclWithType * candidate = match.cdata.id;
		assertf( candidate->uniqueId,
			"Assertion candidate does not have a unique ID: %s", toString( candidate ).c_str() );

		ast::Expr * varExpr = match.cdata.combine( cand->expr->location, cand->cost );
		varExpr->result = match.adjType;
		if ( match.resnSlot ) { varExpr->inferred.resnSlots().emplace_back( match.resnSlot ); }

		// place newly-inferred assertion in proper location in cache
		inferred[ info.resnSlot ][ expr->var->uniqueId ] = ast::ParamEntry{
			candidate->uniqueId, candidate, match.adjType, expr->result, varExpr };
	}

	/// Satisfy a single assertion
	AssertionResult satisfyAssertion( ast::AssertionList::value_type & assn, SatState & sat, bool skipUnbound = false) {
		// skip unused assertions
		// static unsigned int cnt = 0; // I think this was debugging code so I commented it
		if ( ! assn.second.isUsed ) return AssertionResult::Success;

		// if (assn.first->var->name[1] == '|') std::cerr << ++cnt << std::endl; // I think this was debugging code so I commented it

		// find candidates that unify with the desired type
		AssnCandidateList matches, inexactMatches;

		std::vector<ast::SymbolTable::IdData> candidates;
		auto kind = ast::SymbolTable::getSpecialFunctionKind(assn.first->var->name);
		if (kind != ast::SymbolTable::SpecialFunctionKind::NUMBER_OF_KINDS) {
			// prefilter special decls by argument type, if already known
			ast::ptr<ast::Type> thisArgType = assn.first->result.strict_as<ast::PointerType>()->base
				.strict_as<ast::FunctionType>()->params[0]
				.strict_as<ast::ReferenceType>()->base;
			// sat.cand->env.apply(thisArgType);

			if (auto inst = thisArgType.as<ast::TypeInstType>()) {
				auto cls = sat.cand->env.lookup(*inst);
				if (cls && cls->bound) thisArgType = cls->bound;
			}

			std::string otypeKey = "";
			if (thisArgType.as<ast::PointerType>()) otypeKey = Mangle::Encoding::pointer;
			else if (!isUnboundType(thisArgType)) otypeKey = Mangle::mangle(thisArgType, Mangle::Type | Mangle::NoGenericParams);
			else if (skipUnbound) return AssertionResult::Skip;

			candidates = sat.symtab.specialLookupId(kind, otypeKey);
		}
		else {
			candidates = sat.symtab.lookupId(assn.first->var->name);
		}
		for ( const ast::SymbolTable::IdData & cdata : candidates ) {
			const ast::DeclWithType * candidate = cdata.id;

			// ignore deleted candidates.
			// NOTE: this behavior is different from main resolver.
			// further investigations might be needed to determine
			// if we should implement the same rule here
			// (i.e. error if unique best match is deleted)
			if (candidate->isDeleted && candidate->linkage == ast::Linkage::AutoGen) continue;

			// build independent unification context for candidate
			ast::AssertionSet have, newNeed;
			ast::TypeEnvironment newEnv{ sat.cand->env };
			ast::OpenVarSet newOpen{ sat.cand->open };
			ast::ptr< ast::Type > toType = assn.first->result;
			ast::ptr< ast::Type > adjType =
				renameTyVars( adjustExprType( candidate->get_type(), newEnv, sat.symtab ), GEN_USAGE, false );

			// only keep candidates which unify

			ast::OpenVarSet closed;
			// findOpenVars( toType, newOpen, closed, newNeed, have, FirstClosed );
			findOpenVars( adjType, newOpen, closed, newNeed, have, newEnv, FirstOpen );
			ast::TypeEnvironment tempNewEnv {newEnv};

			if ( unifyExact( toType, adjType, tempNewEnv, newNeed, have, newOpen, WidenMode {true, true} ) ) {
				// set up binding slot for recursive assertions
				ast::UniqueId crntResnSlot = 0;
				if ( ! newNeed.empty() ) {
					crntResnSlot = ++globalResnSlot;
					for ( auto & a : newNeed ) { a.second.resnSlot = crntResnSlot; }
				}

				matches.emplace_back(
					cdata, adjType, std::move( tempNewEnv ), std::move( have ), std::move( newNeed ),
					std::move( newOpen ), crntResnSlot );
			}
			else if ( matches.empty() ) {
				// restore invalidated env
				// newEnv = sat.cand->env;
				// newNeed.clear();
				if ( auto c = commonType( toType, adjType, newEnv, newNeed, have, newOpen, WidenMode {true, true} ) ) {
					// set up binding slot for recursive assertions
					ast::UniqueId crntResnSlot = 0;
					if ( ! newNeed.empty() ) {
						crntResnSlot = ++globalResnSlot;
						for ( auto & a : newNeed ) { a.second.resnSlot = crntResnSlot; }
					}

					inexactMatches.emplace_back(
						cdata, adjType, std::move( newEnv ), std::move( have ), std::move( newNeed ),
						std::move( newOpen ), crntResnSlot );
				}
			}
		}

		// break if no satisfying match
		if ( matches.empty() ) matches = std::move(inexactMatches);
		if ( matches.empty() ) return AssertionResult::Fail;

		// defer if too many satisfying matches
		if ( matches.size() > 1 ) {
			sat.deferred.emplace_back( assn.first, assn.second, std::move( matches ) );
			return AssertionResult::Success;
		}

		// otherwise bind unique match in ongoing scope
		AssnCandidate & match = matches.front();
		// addToSymbolTable( match.have, sat.symtab );
		sat.newNeed.insert( match.need.begin(), match.need.end() );
		sat.cand->env = std::move( match.env );
		sat.cand->open = std::move( match.open );

		bindAssertion( assn.first, assn.second, sat.cand, match, sat.inferred );
		return AssertionResult::Success;
	}

	/// Map of candidate return types to recursive assertion satisfaction costs
	using PruneMap = std::unordered_map< std::string, CostVec >;

	/// Gets the pruning key for a candidate (derived from environment-adjusted return type)
	std::string pruneKey( const Candidate & cand ) {
		ast::ptr< ast::Type > resType = cand.expr->result;
		cand.env.apply( resType );
		return Mangle::mangleType( resType );
	}

	/// Associates inferred parameters with an expression
	struct InferMatcher final {
		InferCache & inferred;

		InferMatcher( InferCache & inferred ) : inferred( inferred ) {}

		const ast::Expr * postvisit( const ast::Expr * expr ) {
			// Skip if no slots to find
			if ( !expr->inferred.hasSlots() ) return expr;
			// if ( expr->inferred.mode != ast::Expr::InferUnion::Slots ) return expr;
			std::vector<ast::UniqueId> missingSlots;
			// find inferred parameters for resolution slots
			ast::InferredParams * newInferred = new ast::InferredParams();
			for ( ast::UniqueId slot : expr->inferred.resnSlots() ) {
				// fail if no matching assertions found
				auto it = inferred.find( slot );
				if ( it == inferred.end() ) {
					// std::cerr << "missing assertion " << slot << std::endl;
					missingSlots.push_back(slot);
					continue;
				}

				// place inferred parameters into new map
				for ( auto & entry : it->second ) {
					// recurse on inferParams of resolved expressions
					entry.second.expr = postvisit( entry.second.expr );
					auto res = newInferred->emplace( entry );
					assert( res.second && "all assertions newly placed" );
				}
			}

			ast::Expr * ret = mutate( expr );
			ret->inferred.set_inferParams( newInferred );
			if (!missingSlots.empty()) ret->inferred.resnSlots() = missingSlots;
			return ret;
		}
	};

	/// Replace ResnSlots with InferParams and add alternative to output list, if it meets pruning
	/// threshold.
	void finalizeAssertions(
		CandidateRef & cand, InferCache & inferred, PruneMap & thresholds, CostVec && costs,
		CandidateList & out
	) {
		// prune if cheaper alternative for same key has already been generated
		std::string key = pruneKey( *cand );
		auto it = thresholds.find( key );
		if ( it != thresholds.end() ) {
			if ( it->second < costs ) return;
		} else {
			thresholds.emplace_hint( it, key, std::move( costs ) );
		}

		// replace resolution slots with inferred parameters, add to output
		ast::Pass< InferMatcher > matcher{ inferred };
		cand->expr = cand->expr->accept( matcher );
		out.emplace_back( cand );
	}

	/// Combo iterator that combines candidates into an output list, merging their environments.
	/// Rejects an appended candidate if environments cannot be merged. See `Common/FilterCombos.h`
	/// for description of "combo iterator".
	class CandidateEnvMerger {
		/// Current list of merged candidates
		std::vector< DeferRef > crnt;
		/// Stack of environments to support backtracking
		std::vector< ast::TypeEnvironment > envs;
		/// Stack of open variables to support backtracking
		std::vector< ast::OpenVarSet > opens;
		/// Symbol table to use for merges
		const ast::SymbolTable & symtab;

	public:
		/// The merged environment/open variables and the list of candidates
		struct OutType {
			ast::TypeEnvironment env;
			ast::OpenVarSet open;
			std::vector< DeferRef > assns;
			Cost cost;

			OutType(
				const ast::TypeEnvironment & e, const ast::OpenVarSet & o,
				const std::vector< DeferRef > & as, const ast::SymbolTable & symtab )
			: env( e ), open( o ), assns( as ), cost( Cost::zero ) {
				// compute combined conversion cost
				for ( const DeferRef & assn : assns ) {
					// compute conversion cost from satisfying decl to assertion
					cost += computeConversionCost(
						assn.match.adjType, assn.expr->result, false, symtab, env );

					// mark vars+specialization on function-type assertions
					const ast::FunctionType * func =
						GenPoly::getFunctionType( assn.match.cdata.id->get_type() );
					if ( ! func ) continue;

					for ( const auto & param : func->params ) {
						cost.decSpec( specCost( param ) );
					}

					cost.incVar( func->forall.size() );

					cost.decSpec( func->assertions.size() );
				}
			}

			bool operator< ( const OutType & o ) const { return cost < o.cost; }
		};

		CandidateEnvMerger(
			const ast::TypeEnvironment & env, const ast::OpenVarSet & open,
			const ast::SymbolTable & syms )
		: crnt(), envs{ env }, opens{ open }, symtab( syms ) {}

		bool append( DeferRef i ) {
			ast::TypeEnvironment env = envs.back();
			ast::OpenVarSet open = opens.back();
			mergeOpenVars( open, i.match.open );

			if ( ! env.combine( i.match.env, open ) ) return false;

			crnt.emplace_back( i );
			envs.emplace_back( std::move( env ) );
			opens.emplace_back( std::move( open ) );
			return true;
		}

		void backtrack() {
			crnt.pop_back();
			envs.pop_back();
			opens.pop_back();
		}

		OutType finalize() { return { envs.back(), opens.back(), crnt, symtab }; }
	};

	/// Limit to depth of recursion of assertion satisfaction
	static const int recursionLimit = 8;
	/// Maximum number of simultaneously-deferred assertions to attempt concurrent satisfaction of
	static const int deferLimit = 10;
} // anonymous namespace

void satisfyAssertions(
	CandidateRef & cand, const ast::SymbolTable & symtab, CandidateList & out,
	std::vector<std::string> & errors
) {
	// finish early if no assertions to satisfy
	if ( cand->need.empty() ) {
		out.emplace_back( cand );
		return;
	}

	// build list of possible combinations of satisfying declarations
	std::vector< SatState > sats{ SatState{ cand, symtab } };
	std::vector< SatState > nextSats{};

	// pruning thresholds by result type of output candidates.
	// Candidates *should* be generated in sorted order, so no need to retroactively prune
	PruneMap thresholds;

	// satisfy assertions in breadth-first order over the recursion tree of assertion satisfaction.
	// Stop recursion at a limited number of levels deep to avoid infinite loops.
	for ( unsigned level = 0; level < recursionLimit; ++level ) {
		// for each current mutually-compatible set of assertions
		for ( SatState & sat : sats ) {
			// stop this branch if a better option is already found
			auto it = thresholds.find( pruneKey( *sat.cand ) );
			if ( it != thresholds.end() && it->second < sat.costs ) goto nextSat;

			// should a limit be imposed? worst case here is O(n^2) but very unlikely to happen.

			for (unsigned resetCount = 0; ; ++resetCount) {
				ast::AssertionList next;
				// make initial pass at matching assertions
				for ( auto & assn : sat.need ) {
					resetTyVarRenaming();
					// fail early if any assertion is not satisfiable
					auto result = satisfyAssertion( assn, sat, !next.empty() );
					if ( result == AssertionResult::Fail ) {
						Indenter tabs{ 3 };
						std::ostringstream ss;
						ss << tabs << "Unsatisfiable alternative:\n";
						print( ss, *sat.cand, ++tabs );
						ss << (tabs-1) << "Could not satisfy assertion:\n";
						ast::print( ss, assn.first, tabs );

						errors.emplace_back( ss.str() );
						goto nextSat;
					} else if ( result == AssertionResult::Skip ) {
						next.emplace_back(assn);
						// goto nextSat;
					}
				}
				// success
				if (next.empty()) break;

				sat.need = std::move(next);
			}

			if ( sat.deferred.empty() ) {
				// either add successful match or push back next state
				if ( sat.newNeed.empty() ) {
					finalizeAssertions(
						sat.cand, sat.inferred, thresholds, std::move( sat.costs ), out );
				} else {
					nextSats.emplace_back( std::move( sat ), IterateState );
				}
			} else if ( sat.deferred.size() > deferLimit ) {
				// too many deferred assertions to attempt mutual compatibility
				Indenter tabs{ 3 };
				std::ostringstream ss;
				ss << tabs << "Unsatisfiable alternative:\n";
				print( ss, *sat.cand, ++tabs );
				ss << (tabs-1) << "Too many non-unique satisfying assignments for assertions:\n";
				for ( const auto & d : sat.deferred ) {
					ast::print( ss, d.expr, tabs );
				}

				errors.emplace_back( ss.str() );
				goto nextSat;
			} else {
				// combine deferred assertions by mutual compatibility
				std::vector< CandidateEnvMerger::OutType > compatible = filterCombos(
					sat.deferred, CandidateEnvMerger{ sat.cand->env, sat.cand->open, sat.symtab } );

				// fail early if no mutually-compatible assertion satisfaction
				if ( compatible.empty() ) {
					Indenter tabs{ 3 };
					std::ostringstream ss;
					ss << tabs << "Unsatisfiable alternative:\n";
					print( ss, *sat.cand, ++tabs );
					ss << (tabs-1) << "No mutually-compatible satisfaction for assertions:\n";
					for ( const auto& d : sat.deferred ) {
						ast::print( ss, d.expr, tabs );
					}

					errors.emplace_back( ss.str() );
					goto nextSat;
				}

				// sort by cost (for overall pruning order)
				std::sort( compatible.begin(), compatible.end() );

				// process mutually-compatible combinations
				for ( auto & compat : compatible ) {
					// set up next satisfaction state
					CandidateRef nextCand = std::make_shared<Candidate>(
						sat.cand->expr, std::move( compat.env ), std::move( compat.open ),
						ast::AssertionSet{} /* need moved into satisfaction state */,
						sat.cand->cost );

					ast::AssertionSet nextNewNeed{ sat.newNeed };
					InferCache nextInferred{ sat.inferred };

					CostVec nextCosts{ sat.costs };
					nextCosts.back() += compat.cost;

					ast::SymbolTable nextSymtab{ sat.symtab };

					// add compatible assertions to new satisfaction state
					for ( DeferRef r : compat.assns ) {
						AssnCandidate match = r.match;
						// addToSymbolTable( match.have, nextSymtab );
						nextNewNeed.insert( match.need.begin(), match.need.end() );

						bindAssertion( r.expr, r.info, nextCand, match, nextInferred );
					}

					// either add successful match or push back next state
					if ( nextNewNeed.empty() ) {
						finalizeAssertions(
							nextCand, nextInferred, thresholds, std::move( nextCosts ), out );
					} else {
						nextSats.emplace_back(
							std::move( nextCand ), std::move( nextNewNeed ),
							std::move( nextInferred ), std::move( nextCosts ),
							std::move( nextSymtab ) );
					}
				}
			}
		nextSat:; }

		// finish or reset for next round
		if ( nextSats.empty() ) return;
		sats.swap( nextSats );
		nextSats.clear();
	}

	// exceeded recursion limit if reaches here
	if ( out.empty() ) {
		SemanticError( cand->expr->location, "Too many recursive assertions" );
	}
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
