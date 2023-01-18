//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolveAssertions.cc --
//
// Author           : Aaron B. Moss
// Created On       : Fri Oct 05 13:46:00 2018
// Last Modified By : Andrew Beach
// Last Modified On : Thu Aug  8 16:47:00 2019
// Update Count     : 3
//

#include "ResolveAssertions.h"

#include <algorithm>                // for sort
#include <cassert>                  // for assertf
#include <list>                     // for list
#include <memory>                   // for unique_ptr
#include <sstream>                  // for ostringstream
#include <string>                   // for string
#include <unordered_map>            // for unordered_map, unordered_multimap
#include <utility>                  // for move
#include <vector>                   // for vector

#include "Alternative.h"            // for Alternative, AssertionItem, AssertionList
#include "Common/FilterCombos.h"    // for filterCombos
#include "Common/Indenter.h"        // for Indenter
#include "Common/utility.h"         // for sort_mins
#include "GenPoly/GenPoly.h"        // for getFunctionType
#include "ResolvExpr/AlternativeFinder.h"  // for computeConversionCost
#include "ResolvExpr/RenameVars.h"  // for renameTyVars
#include "SymTab/Indexer.h"         // for Indexer
#include "SymTab/Mangler.h"         // for Mangler
#include "SynTree/Expression.h"     // for InferredParams
#include "TypeEnvironment.h"        // for TypeEnvironment, etc.
#include "typeops.h"                // for adjustExprType, specCost
#include "Unify.h"                  // for unify

namespace ResolvExpr {
	/// Unified assertion candidate
	struct AssnCandidate {
		SymTab::Indexer::IdData cdata;  ///< Satisfying declaration
		Type* adjType;                  ///< Satisfying type
		TypeEnvironment env;            ///< Post-unification environment
		AssertionSet have;              ///< Post-unification have-set
		AssertionSet need;              ///< Post-unification need-set
		OpenVarSet openVars;            ///< Post-unification open-var set
		UniqueId resnSlot;              ///< Slot for any recursive assertion IDs

		AssnCandidate( const SymTab::Indexer::IdData& cdata, Type* adjType, TypeEnvironment&& env,
			AssertionSet&& have, AssertionSet&& need, OpenVarSet&& openVars, UniqueId resnSlot )
		: cdata(cdata), adjType(adjType), env(std::move(env)), have(std::move(have)),
			need(std::move(need)), openVars(std::move(openVars)), resnSlot(resnSlot) {}
	};

	/// List of candidate assertion resolutions
	using CandidateList = std::vector<AssnCandidate>;

	/// Reference to single deferred item
	struct DeferRef {
		const DeclarationWithType* decl;
		const AssertionSetValue& info;
		const AssnCandidate& match;
	};

	/// Wrapper for the deferred items from a single assertion resolution.
	/// Acts like indexed list of DeferRef
	struct DeferItem {
		const DeclarationWithType* decl;
		const AssertionSetValue& info;
		CandidateList matches;

		DeferItem( const DeclarationWithType* decl, const AssertionSetValue& info, CandidateList&& matches )
		: decl(decl), info(info), matches(std::move(matches)) {}

		bool empty() const { return matches.empty(); }

		CandidateList::size_type size() const { return matches.size(); }

		DeferRef operator[] ( unsigned i ) const { return { decl, info, matches[i] }; }
	};

	/// List of deferred resolution items
	using DeferList = std::vector<DeferItem>;

	/// Combo iterator that combines candidates into an output list, merging their environments.
	/// Rejects an appended candidate if the environments cannot be merged.
	class CandidateEnvMerger {
		/// Current list of merged candidates
		std::vector<DeferRef> crnt;
		/// Stack of environments to support backtracking
		std::vector<TypeEnvironment> envs;
		/// Stack of open variables to support backtracking
		std::vector<OpenVarSet> varSets;
		/// Indexer to use for merges
		const SymTab::Indexer& indexer;

	public:
		/// The merged environment/open variables and the list of candidates
		struct OutType {
			TypeEnvironment env;
			OpenVarSet openVars;
			std::vector<DeferRef> assns;
			Cost cost;

			OutType( const TypeEnvironment& env, const OpenVarSet& openVars,
				const std::vector<DeferRef>& assns )
			: env(env), openVars(openVars), assns(assns), cost(Cost::infinity) {}
		};

		CandidateEnvMerger( const TypeEnvironment& env, const OpenVarSet& openVars,
			const SymTab::Indexer& indexer )
		: crnt(), envs{ env }, varSets{ openVars }, indexer(indexer) {}

		bool append( DeferRef i ) {
			TypeEnvironment env = envs.back();
			OpenVarSet openVars = varSets.back();
			mergeOpenVars( openVars, i.match.openVars );

			if ( ! env.combine( i.match.env, openVars, indexer ) ) return false;

			crnt.emplace_back( i );
			envs.emplace_back( env );
			varSets.emplace_back( openVars );
			return true;
		}

		void backtrack() {
			crnt.pop_back();
			envs.pop_back();
			varSets.pop_back();
		}

		OutType finalize() { return { envs.back(), varSets.back(), crnt }; }
	};

	/// Comparator for CandidateEnvMerger outputs that sums their costs and caches the stored
	/// sums
	struct CandidateCost {
		using Element = CandidateEnvMerger::OutType;
	private:
		const SymTab::Indexer& indexer;  ///< Indexer for costing

	public:
		CandidateCost( const SymTab::Indexer& indexer ) : indexer(indexer) {}

		/// reports the cost of an element
		Cost get( Element& x ) const {
			// check cached cost
			if ( x.cost != Cost::infinity ) return x.cost;

			// generate cost
			Cost k = Cost::zero;
			for ( const auto& assn : x.assns ) {
				// compute conversion cost from satisfying decl to assertion
				k += computeConversionCost(
					assn.match.adjType, assn.decl->get_type(), false, indexer, x.env );

				// mark vars+specialization cost on function-type assertions
				FunctionType* func = GenPoly::getFunctionType( assn.match.cdata.id->get_type() );
				if ( ! func ) continue;

				for ( DeclarationWithType* formal : func->parameters ) {
					k.decSpec( specCost( formal->get_type() ) );
				}
				k.incVar( func->forall.size() );
				for ( TypeDecl* td : func->forall ) {
					k.decSpec( td->assertions.size() );
				}
			}

			// cache and return
			x.cost = k;
			return k;
		}

		/// compares elements by cost
		bool operator() ( Element& a, Element& b ) const {
			return get( a ) < get( b );
		}
	};

	/// Set of assertion resolutions, grouped by resolution ID
	using InferCache = std::unordered_map< UniqueId, InferredParams >;

	/// Lexicographically-ordered vector of costs
	using CostVec = std::vector< Cost >;

	int compare( const CostVec & a, const CostVec & b ) {
		unsigned i = 0;
		do {
			// lex-compare where shorter one is less
			if ( i == a.size() ) {
				return i == b.size() ? 0 : -1;
			}
			if ( i == b.size() /* && i < a.size() */ ) return 1;

			int c = a[i].compare( b[i] );
			if ( c != 0 ) return c;
		} while ( ++i );
		assert(!"unreachable");
	}

	bool operator< ( const CostVec & a, const CostVec & b ) { return compare( a, b ) < 0; }

	/// Flag for state iteration
	enum IterateFlag { IterateState };

	/// State needed to resolve a set of assertions
	struct ResnState {
		Alternative alt;           ///< Alternative assertion is rooted on
		AssertionList need;        ///< Assertions to find
		AssertionSet newNeed;      ///< New assertions for current resolutions
		DeferList deferred;        ///< Deferred matches
		InferCache inferred;       ///< Cache of already-inferred parameters
		CostVec costs;             ///< Costs of recursive assertion satisfaction for disambiguation
		SymTab::Indexer& indexer;  ///< Name lookup (depends on previous assertions)

		/// Initial resolution state for an alternative
		ResnState( Alternative & a, SymTab::Indexer & indexer )
		: alt(a), need(), newNeed(), deferred(), inferred(), costs{ Cost::zero }, indexer(indexer) {
			need.swap( a.need );
		}

		/// Updated resolution state with new need-list
		ResnState( ResnState && o, IterateFlag )
		: alt(std::move(o.alt)), need(o.newNeed.begin(), o.newNeed.end()), newNeed(), deferred(),
		  inferred(std::move(o.inferred)), costs(o.costs), indexer(o.indexer) {
			costs.emplace_back( Cost::zero );
		}
	};

	/// Binds a single assertion, updating resolution state
	void bindAssertion( const DeclarationWithType * decl, AssertionSetValue info, Alternative & alt,
			AssnCandidate & match, InferCache & inferred ) {

		const DeclarationWithType * candidate = match.cdata.id;
		assertf( candidate->uniqueId, "Assertion candidate does not have a unique ID: %s", toString( candidate ).c_str() );

		Expression * varExpr = match.cdata.combine( alt.cvtCost );
		delete varExpr->result;
		varExpr->result = match.adjType->clone();
		if ( match.resnSlot ) { varExpr->resnSlots.push_back( match.resnSlot ); }

		// place newly-inferred assertion in proper place in cache
		inferred[ info.resnSlot ][ decl->get_uniqueId() ] = ParamEntry{
				candidate->uniqueId, candidate->clone(), match.adjType->clone(), decl->get_type()->clone(),
				varExpr };
	}

	/// Adds a captured assertion to the symbol table
	void addToIndexer( AssertionSet & assertSet, SymTab::Indexer & indexer ) {
		for ( auto&  i : assertSet ) {
			if ( i.second.isUsed ) {
				indexer.addId( i.first );
			}
		}
	}

	// in AlternativeFinder.cc; unique ID for assertion resolutions
	extern UniqueId globalResnSlot;

	/// Resolve a single assertion, in context
	bool resolveAssertion( AssertionItem & assn, ResnState & resn ) {
		// skip unused assertions
		if ( ! assn.info.isUsed ) return true;

		// lookup candidates for this assertion
		std::list< SymTab::Indexer::IdData > candidates;
		resn.indexer.lookupId( assn.decl->name, candidates );

		// find the candidates that unify with the desired type
		CandidateList matches;
		for ( const auto & cdata : candidates ) {
			const DeclarationWithType * candidate = cdata.id;

			// ignore deleted candidates.
			// NOTE: this behavior is different from main resolver.
			// further investigations might be needed to determine
			// if we should implement the same rule here
			// (i.e. error if unique best match is deleted)
			if (candidate->isDeleted) continue;

			// build independent unification context. for candidate
			AssertionSet have, newNeed;
			TypeEnvironment newEnv{ resn.alt.env };
			OpenVarSet newOpenVars{ resn.alt.openVars };
			Type * adjType = candidate->get_type()->clone();
			adjustExprType( adjType, newEnv, resn.indexer );
			renameTyVars( adjType );

			// keep unifying candidates
			if ( unify( assn.decl->get_type(), adjType, newEnv, newNeed, have, newOpenVars,
					resn.indexer ) ) {
				// set up binding slot for recursive assertions
				UniqueId crntResnSlot = 0;
				if ( ! newNeed.empty() ) {
					crntResnSlot = ++globalResnSlot;
					for ( auto& a : newNeed ) {
						a.second.resnSlot = crntResnSlot;
					}
				}

				matches.emplace_back( cdata, adjType, std::move(newEnv), std::move(have),
					std::move(newNeed), std::move(newOpenVars), crntResnSlot );
			} else {
				delete adjType;
			}
		}

		// break if no suitable assertion
		if ( matches.empty() ) return false;

		// defer if too many suitable assertions
		if ( matches.size() > 1 ) {
			resn.deferred.emplace_back( assn.decl, assn.info, std::move(matches) );
			return true;
		}

		// otherwise bind current match in ongoing scope
		AssnCandidate& match = matches.front();
		addToIndexer( match.have, resn.indexer );
		resn.newNeed.insert( match.need.begin(), match.need.end() );
		resn.alt.env = std::move(match.env);
		resn.alt.openVars = std::move(match.openVars);

		bindAssertion( assn.decl, assn.info, resn.alt, match, resn.inferred );
		return true;
	}

	/// Associates inferred parameters with an expression
	struct InferMatcher {
		InferCache& inferred;

		InferMatcher( InferCache& inferred ) : inferred( inferred ) {}

		Expression* postmutate( Expression* expr ) {
			// defer missing inferred parameters until they are hopefully found later
			std::vector<UniqueId> missingSlots;
			// place inferred parameters into resolution slots
			for ( UniqueId slot : expr->resnSlots ) {
				// fail if no matching parameters found
				auto it = inferred.find( slot );
				if ( it == inferred.end() ) {
					missingSlots.push_back( slot );
					continue;
				}
				InferredParams& inferParams = it->second;

				// place inferred parameters into proper place in expression
				for ( auto& entry : inferParams ) {
					// recurse on inferParams of resolved expressions
					entry.second.expr = postmutate( entry.second.expr );
					// xxx - look at entry.second.inferParams?
					auto res = expr->inferParams.emplace( entry.first, entry.second );
					assert(res.second);
				}
			}

			// clear resolution slots and return
			expr->resnSlots.swap( missingSlots );
			return expr;
		}
	};

	/// Map of alternative return types to recursive assertion satisfaction costs
	using PruneMap = std::unordered_map<std::string, CostVec>;

	/// Gets the pruning key for an alternative
	std::string pruneKey( const Alternative & alt ) {
		Type* resType = alt.expr->result->clone();
		alt.env.apply( resType );
		std::string resKey = SymTab::Mangler::mangleType( resType );
		delete resType;
		return resKey;
	}

	/// Replace resnSlots with inferParams and add alternative to output list, if meets pruning
	/// threshold.
	void finalizeAssertions( ResnState& resn, PruneMap & pruneThresholds, AltList& out ) {
		// prune if cheaper alternative for same key has already been generated
		std::string resKey = pruneKey( resn.alt );
		auto it = pruneThresholds.find( resKey );
		if ( it != pruneThresholds.end() ) {
			if ( it->second < resn.costs ) return;
		} else {
			pruneThresholds.emplace_hint( it, resKey, resn.costs );
		}

		// replace resolution slots with inferred params, add to output
		PassVisitor<InferMatcher> matcher{ resn.inferred };
		resn.alt.expr = resn.alt.expr->acceptMutator( matcher );
		out.emplace_back( resn.alt );
	}

	/// Limit to depth of recursion of assertion satisfaction
	static const int recursionLimit = 7;
	/// Maximum number of simultaneously-deferred assertions to attempt concurrent satisfaction of
	static const int deferLimit = 10;

	void resolveAssertions( Alternative& alt, const SymTab::Indexer& indexer, AltList& out, std::list<std::string>& errors ) {
		// finish early if no assertions to resolve
		if ( alt.need.empty() ) {
			out.emplace_back( alt );
			return;
		}

		// build list of possible resolutions
		using ResnList = std::vector<ResnState>;
		SymTab::Indexer root_indexer{ indexer };
		ResnList resns{ ResnState{ alt, root_indexer } };
		ResnList new_resns{};

		// Pruning thresholds by result type of the output alternatives.
		// Alternatives *should* be generated in sorted order, so no need to retroactively prune
		PruneMap thresholds;

		// resolve assertions in breadth-first-order up to a limited number of levels deep
		for ( unsigned level = 0; level < recursionLimit; ++level ) {
			// scan over all mutually-compatible resolutions
			for ( auto& resn : resns ) {
				// stop this branch if already found a better option
				auto it = thresholds.find( pruneKey( resn.alt ) );
				if ( it != thresholds.end() && it->second < resn.costs ) goto nextResn;

				// make initial pass at matching assertions
				for ( auto& assn : resn.need ) {
					// fail early if any assertion is not resolvable
					if ( ! resolveAssertion( assn, resn ) ) {
						Indenter tabs{ 3 };
						std::ostringstream ss;
						ss << tabs << "Unsatisfiable alternative:\n";
						resn.alt.print( ss, ++tabs );
						ss << (tabs-1) << "Could not satisfy assertion:\n";
						assn.decl->print( ss, tabs );

						errors.emplace_back( ss.str() );
						goto nextResn;
					}
				}

				if ( resn.deferred.empty() ) {
					// either add successful match or push back next state
					if ( resn.newNeed.empty() ) {
						finalizeAssertions( resn, thresholds, out );
					} else {
						new_resns.emplace_back( std::move(resn), IterateState );
					}
				} else if ( resn.deferred.size() > deferLimit ) {
					// too many deferred assertions to attempt mutual compatibility
					Indenter tabs{ 3 };
					std::ostringstream ss;
					ss << tabs << "Unsatisfiable alternative:\n";
					resn.alt.print( ss, ++tabs );
					ss << (tabs-1) << "Too many non-unique satisfying assignments for "
						"assertions:\n";
					for ( const auto& d : resn.deferred ) {
						d.decl->print( ss, tabs );
					}

					errors.emplace_back( ss.str() );
					goto nextResn;
				} else {
					// resolve deferred assertions by mutual compatibility
					std::vector<CandidateEnvMerger::OutType> compatible = filterCombos(
						resn.deferred,
						CandidateEnvMerger{ resn.alt.env, resn.alt.openVars, resn.indexer } );
					// fail early if no mutually-compatible assertion satisfaction
					if ( compatible.empty() ) {
						Indenter tabs{ 3 };
						std::ostringstream ss;
						ss << tabs << "Unsatisfiable alternative:\n";
						resn.alt.print( ss, ++tabs );
						ss << (tabs-1) << "No mutually-compatible satisfaction for assertions:\n";
						for ( const auto& d : resn.deferred ) {
							d.decl->print( ss, tabs );
						}

						errors.emplace_back( ss.str() );
						goto nextResn;
					}
					// sort by cost for overall pruning
					CandidateCost coster{ resn.indexer };
					std::sort( compatible.begin(), compatible.end(), coster );

					for ( auto& compat : compatible ) {
						ResnState new_resn = resn;

						// add compatible assertions to new resolution state
						for ( DeferRef r : compat.assns ) {
							AssnCandidate match = r.match;
							addToIndexer( match.have, new_resn.indexer );
							new_resn.newNeed.insert( match.need.begin(), match.need.end() );

							bindAssertion( r.decl, r.info, new_resn.alt, match, new_resn.inferred );
						}

						// set mutual environment into resolution state
						new_resn.alt.env = std::move(compat.env);
						new_resn.alt.openVars = std::move(compat.openVars);

						// mark cost of this path
						new_resn.costs.back() += compat.cost;

						// either add sucessful match or push back next state
						if ( new_resn.newNeed.empty() ) {
							finalizeAssertions( new_resn, thresholds, out );
						} else {
							new_resns.emplace_back( std::move(new_resn), IterateState );
						}
					}
				}
			nextResn:; }

			// finish or reset for next round
			if ( new_resns.empty() ) return;
			resns.swap( new_resns );
			new_resns.clear();
		}

		// exceeded recursion limit if reaches here
		if ( out.empty() ) {
			SemanticError( alt.expr->location, "Too many recursive assertions" );
		}
	}
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
