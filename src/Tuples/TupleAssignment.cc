//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TupleAssignment.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Mar 16 14:06:00 2022
// Update Count     : 10
//

#include <algorithm>                       // for transform
#include <cassert>                         // for assert
#include <iterator>                        // for back_insert_iterator, back...
#include <list>                            // for _List_const_iterator, _Lis...
#include <memory>                          // for unique_ptr, allocator_trai...
#include <string>                          // for string
#include <vector>

#include "AST/Decl.hpp"
#include "AST/Init.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/TypeEnvironment.hpp"
#include "CodeGen/OperatorTable.h"
#include "Common/UniqueName.h"             // for UniqueName
#include "Common/utility.h"                // for splice, zipWith
#include "Explode.h"                       // for explode
#include "InitTweak/GenInit.h"             // for genCtorInit
#include "InitTweak/InitTweak.h"           // for getPointerBase, isAssignment
#include "ResolvExpr/Cost.h"               // for Cost
#include "ResolvExpr/Resolver.h"           // for resolveCtorInit
#include "ResolvExpr/typeops.h"            // for combos

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace Tuples {

namespace {

/// Checks if `expr` is of tuple type.
bool isTuple( const ast::Expr * expr ) {
	if ( !expr ) return false;
	assert( expr->result );
	return dynamic_cast< const ast::TupleType * >( expr->result->stripReferences() );
}

/// Checks if `expr` is of tuple type or a cast to one.
bool refToTuple( const ast::Expr * expr ) {
	assert( expr->result );
	// Check for function returning tuple of reference types.
	if ( auto castExpr = dynamic_cast< const ast::CastExpr * >( expr ) ) {
		return refToTuple( castExpr->arg );
	} else {
		return isTuple( expr );
	}
}

/// Dispatcher for tuple (multiple and mass) assignment operations.
class TupleAssignSpotter final {
	/// Actually finds tuple assignment operations, by subclass.
	struct Matcher {
		ResolvExpr::CandidateList lhs, rhs;
		TupleAssignSpotter & spotter;
		CodeLocation location;
		ResolvExpr::Cost baseCost;
		std::vector< ast::ptr< ast::ObjectDecl > > tmpDecls;
		ast::TypeEnvironment env;
		ast::OpenVarSet open;
		ast::AssertionSet need;

		void combineState( const ResolvExpr::Candidate & cand ) {
			env.simpleCombine( cand.env );
			ast::mergeOpenVars( open, cand.open );
			need.insert( cand.need.begin(), cand.need.end() );
		}

		Matcher(
			TupleAssignSpotter & s, const CodeLocation & loc,
			const ResolvExpr::CandidateList & l, const ResolvExpr::CandidateList & r )
		: lhs( l ), rhs( r ), spotter( s ), location( loc ),
		  baseCost( ResolvExpr::sumCost( lhs ) + ResolvExpr::sumCost( rhs ) ), tmpDecls(),
		  env(), open(), need() {
			for ( auto & cand : lhs ) combineState( *cand );
			for ( auto & cand : rhs ) combineState( *cand );
		}
		virtual ~Matcher() = default;

		virtual std::vector< ast::ptr< ast::Expr > > match() = 0;

		/// Removes environments from subexpressions within statement expressions, which could
		/// throw off later passes like those in Box which rely on PolyMutator, and adds the
		/// bindings to the env.
		struct EnvRemover {
			/// Environment to hoist ExprStmt environments to.
			ast::TypeEnvironment & tenv;

			EnvRemover( ast::TypeEnvironment & e ) : tenv( e ) {}

			const ast::ExprStmt * previsit( const ast::ExprStmt * stmt ) {
				if ( stmt->expr->env ) {
					tenv.add( *stmt->expr->env );
					ast::ExprStmt * mut = mutate( stmt );
					mut->expr.get_and_mutate()->env = nullptr;
					return mut;
				}
				return stmt;
			}
		};

		ast::ObjectDecl * newObject( UniqueName & namer, const ast::Expr * expr ) {
			assert( expr->result && !expr->result->isVoid() );

			ast::ObjectDecl * ret = new ast::ObjectDecl(
				location, namer.newName(), expr->result, new ast::SingleInit( location, expr ),
				ast::Storage::Classes{}, ast::Linkage::Cforall );

			// If expression type is a reference, just need an initializer, otherwise construct.
			if ( ! expr->result.as< ast::ReferenceType >() ) {
				// Resolve ctor/dtor for the new object.
				ast::ptr< ast::Init > ctorInit = ResolvExpr::resolveCtorInit(
						InitTweak::genCtorInit( location, ret ), spotter.crntFinder.context );
				// Remove environments from subexpressions of stmtExpr.
				ast::Pass< EnvRemover > rm( env );
				ret->init = ctorInit->accept( rm );
			}

			PRINT( std::cerr << "new object: " << ret << std::endl; )
			return ret;
		}

		ast::UntypedExpr * createFunc(
			const std::string & fname, const ast::ObjectDecl * left,
			const ast::ObjectDecl * right
		) {
			assert( left );
			std::vector< ast::ptr< ast::Expr > > args;
			args.emplace_back( new ast::VariableExpr( location, left ) );
			if ( right ) { args.emplace_back( new ast::VariableExpr( location, right ) ); }

			if ( left->type->referenceDepth() > 1 && CodeGen::isConstructor( fname ) ) {
				args.front() = new ast::AddressExpr( location, args.front() );
				if ( right ) { args.back() = new ast::AddressExpr( location, args.back() ); }
				return new ast::UntypedExpr(
					location, new ast::NameExpr( location, "?=?" ), std::move( args ) );
			} else {
				return new ast::UntypedExpr(
					location, new ast::NameExpr( location, fname ), std::move( args ) );
			}
		}
	};

	/// Finds mass-assignment operations.
	struct MassAssignMatcher final : public Matcher {
		MassAssignMatcher(
			TupleAssignSpotter & s, const CodeLocation & loc,
			const ResolvExpr::CandidateList & l, const ResolvExpr::CandidateList & r )
		: Matcher( s, loc, l, r ) {}

		std::vector< ast::ptr< ast::Expr > > match() override {
			static UniqueName lhsNamer( "__massassign_L" );
			static UniqueName rhsNamer( "__massassign_R" );
			// Empty tuple case falls into this matcher.
			assert( lhs.empty() ? rhs.empty() : rhs.size() <= 1 );

			ast::ptr< ast::ObjectDecl > rtmp =
				1 == rhs.size() ? newObject( rhsNamer, rhs.front()->expr ) : nullptr;

			std::vector< ast::ptr< ast::Expr > > out;
			for ( ResolvExpr::CandidateRef & lhsCand : lhs ) {
				// Create a temporary object for each value in
				// the LHS and create a call involving the RHS.
				ast::ptr< ast::ObjectDecl > ltmp = newObject( lhsNamer, lhsCand->expr );
				out.emplace_back( createFunc( spotter.fname, ltmp, rtmp ) );
				tmpDecls.emplace_back( std::move( ltmp ) );
			}
			if ( rtmp ) tmpDecls.emplace_back( std::move( rtmp ) );

			return out;
		}
	};

	/// Finds multiple-assignment operations.
	struct MultipleAssignMatcher final : public Matcher {
		MultipleAssignMatcher(
			TupleAssignSpotter & s, const CodeLocation & loc,
			const ResolvExpr::CandidateList & l, const ResolvExpr::CandidateList & r )
		: Matcher( s, loc, l, r ) {}

		std::vector< ast::ptr< ast::Expr > > match() override {
			static UniqueName lhsNamer( "__multassign_L" );
			static UniqueName rhsNamer( "__multassign_R" );

			if ( lhs.size() != rhs.size() ) return {};

			// Produce a new temporary object for each value in
			// the LHS and RHS and pairwise create the calls.
			std::vector< ast::ptr< ast::ObjectDecl > > ltmp, rtmp;

			std::vector< ast::ptr< ast::Expr > > out;
			for ( unsigned i = 0; i < lhs.size(); ++i ) {
				ResolvExpr::CandidateRef & lhsCand = lhs[i];
				ResolvExpr::CandidateRef & rhsCand = rhs[i];

				// Convert RHS to LHS type minus one reference --
				// important for case where LHS is && and RHS is lvalue.
				auto lhsType = lhsCand->expr->result.strict_as< ast::ReferenceType >();
				rhsCand->expr = new ast::CastExpr( rhsCand->expr, lhsType->base );
				ast::ptr< ast::ObjectDecl > lobj = newObject( lhsNamer, lhsCand->expr );
				ast::ptr< ast::ObjectDecl > robj = newObject( rhsNamer, rhsCand->expr );
				out.emplace_back( createFunc( spotter.fname, lobj, robj ) );
				ltmp.emplace_back( std::move( lobj ) );
				rtmp.emplace_back( std::move( robj ) );

				// Resolve the cast expression so that rhsCand return type is bound
				// by the cast type as needed, and transfer the resulting environment.
				ResolvExpr::CandidateFinder finder( spotter.crntFinder.context, env );
				finder.find( rhsCand->expr, ResolvExpr::ResolvMode::withAdjustment() );
				assert( 1 == finder.candidates.size() );
				env = std::move( finder.candidates.front()->env );
			}

			splice( tmpDecls, ltmp );
			splice( tmpDecls, rtmp );

			return out;
		}
	};

	ResolvExpr::CandidateFinder & crntFinder;
	std::string fname;
	std::unique_ptr< Matcher > matcher;

public:
	TupleAssignSpotter( ResolvExpr::CandidateFinder & f )
	: crntFinder( f ), fname(), matcher() {}

	// Find left- and right-hand-sides for mass or multiple assignment.
	void spot(
		const ast::UntypedExpr * expr, std::vector< ResolvExpr::CandidateFinder > & args
	) {
		if ( auto op = expr->func.as< ast::NameExpr >() ) {
			// Skip non-assignment functions.
			if ( !CodeGen::isCtorDtorAssign( op->name ) ) return;
			fname = op->name;

			// Handled by CandidateFinder if applicable (both odd cases).
			if ( args.empty() || ( 1 == args.size() && CodeGen::isAssignment( fname ) ) ) {
				return;
			}

			// Look over all possible left-hand-side.
			for ( ResolvExpr::CandidateRef & lhsCand : args[0] ) {
				// Skip non-tuple LHS.
				if ( !refToTuple( lhsCand->expr ) ) continue;

				// Explode is aware of casts - ensure every LHS
				// is sent into explode with a reference cast.
				if ( !lhsCand->expr.as< ast::CastExpr >() ) {
					lhsCand->expr = new ast::CastExpr(
						lhsCand->expr, new ast::ReferenceType( lhsCand->expr->result ) );
				}

				// Explode the LHS so that each field of a tuple-valued expr is assigned.
				ResolvExpr::CandidateList lhs;
				explode( *lhsCand, crntFinder.context.symtab, back_inserter(lhs), true );
				for ( ResolvExpr::CandidateRef & cand : lhs ) {
					// Each LHS value must be a reference - some come in
					// with a cast, if not just cast to reference here.
					if ( !cand->expr->result.as< ast::ReferenceType >() ) {
						cand->expr = new ast::CastExpr(
							cand->expr, new ast::ReferenceType( cand->expr->result ) );
					}
				}

				if ( 1 == args.size() ) {
					// Mass default-initialization/destruction.
					ResolvExpr::CandidateList rhs{};
					matcher.reset( new MassAssignMatcher( *this, expr->location, lhs, rhs ) );
					match();
				} else if ( 2 == args.size() ) {
					for ( const ResolvExpr::CandidateRef & rhsCand : args[1] ) {
						ResolvExpr::CandidateList rhs;
						if ( isTuple( rhsCand->expr ) ) {
							// Multiple assignment:
							explode( *rhsCand, crntFinder.context.symtab, back_inserter( rhs ), true );
							matcher.reset(
								new MultipleAssignMatcher( *this, expr->location, lhs, rhs ) );
						} else {
							// Mass assignment:
							rhs.emplace_back( rhsCand );
							matcher.reset(
								new MassAssignMatcher( *this, expr->location, lhs, rhs ) );
						}
						match();
					}
				} else {
					// Expand all possible RHS possibilities.
					std::vector< ResolvExpr::CandidateList > rhsCands;
					combos(
						std::next( args.begin(), 1 ), args.end(), back_inserter( rhsCands ) );
					for ( const ResolvExpr::CandidateList & rhsCand : rhsCands ) {
						// Multiple assignment:
						ResolvExpr::CandidateList rhs;
						explode( rhsCand, crntFinder.context.symtab, back_inserter( rhs ), true );
						matcher.reset(
							new MultipleAssignMatcher( *this, expr->location, lhs, rhs ) );
						match();
					}
				}
			}
		}
	}

	void match() {
		assert( matcher );

		std::vector< ast::ptr< ast::Expr > > newAssigns = matcher->match();

		if ( !( matcher->lhs.empty() && matcher->rhs.empty() ) ) {
			// If both LHS and RHS are empty than this is the empty tuple
			// case, wherein it's okay for newAssigns to be empty. Otherwise,
			// return early so that no new candidates are generated.
			if ( newAssigns.empty() ) return;
		}

		ResolvExpr::CandidateList crnt;
		// Now resolve new assignments.
		for ( const ast::Expr * expr : newAssigns ) {
			PRINT(
				std::cerr << "== resolving tuple assign ==" << std::endl;
				std::cerr << expr << std::endl;
			)

			ResolvExpr::CandidateFinder finder( crntFinder.context, matcher->env );
			finder.allowVoid = true;

			try {
				finder.find( expr, ResolvExpr::ResolvMode::withAdjustment() );
			} catch (...) {
				// No match is not failure, just that this tuple assignment is invalid.
				return;
			}

			ResolvExpr::CandidateList & cands = finder.candidates;
			assert( 1 == cands.size() );
			assert( cands.front()->expr );
			crnt.emplace_back( std::move( cands.front() ) );
		}

		// extract expressions from the assignment candidates to produce a list of assignments
		// that together form a sigle candidate
		std::vector< ast::ptr< ast::Expr > > solved;
		for ( ResolvExpr::CandidateRef & cand : crnt ) {
			solved.emplace_back( cand->expr );
			matcher->combineState( *cand );
		}

		crntFinder.candidates.emplace_back( std::make_shared< ResolvExpr::Candidate >(
			new ast::TupleAssignExpr(
				matcher->location, std::move( solved ), std::move( matcher->tmpDecls ) ),
			std::move( matcher->env ), std::move( matcher->open ), std::move( matcher->need ),
			ResolvExpr::sumCost( crnt ) + matcher->baseCost ) );
	}
};

} // anonymous namespace

void handleTupleAssignment(
	ResolvExpr::CandidateFinder & finder, const ast::UntypedExpr * assign,
	std::vector< ResolvExpr::CandidateFinder > & args
) {
	TupleAssignSpotter spotter( finder );
	spotter.spot( assign, args );
}

} // namespace Tuples

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
