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
#include "Common/PassVisitor.h"
#include "Common/UniqueName.h"             // for UniqueName
#include "Common/utility.h"                // for splice, zipWith
#include "Explode.h"                       // for explode
#include "InitTweak/GenInit.h"             // for genCtorInit
#include "InitTweak/InitTweak.h"           // for getPointerBase, isAssignment
#include "ResolvExpr/Alternative.h"        // for AltList, Alternative
#include "ResolvExpr/AlternativeFinder.h"  // for AlternativeFinder, simpleC...
#include "ResolvExpr/Cost.h"               // for Cost
#include "ResolvExpr/Resolver.h"           // for resolveCtorInit
#include "ResolvExpr/TypeEnvironment.h"    // for TypeEnvironment
#include "ResolvExpr/typeops.h"            // for combos
#include "SynTree/LinkageSpec.h"           // for Cforall
#include "SynTree/Declaration.h"           // for ObjectDecl
#include "SynTree/Expression.h"            // for Expression, CastExpr, Name...
#include "SynTree/Initializer.h"           // for ConstructorInit, SingleInit
#include "SynTree/Statement.h"             // for ExprStmt
#include "SynTree/Type.h"                  // for Type, Type::Qualifiers
#include "SynTree/TypeSubstitution.h"      // for TypeSubstitution
#include "SynTree/Visitor.h"               // for Visitor

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace Tuples {
	class TupleAssignSpotter_old {
	  public:
		// dispatcher for Tuple (multiple and mass) assignment operations
		TupleAssignSpotter_old( ResolvExpr::AlternativeFinder & );
		void spot( UntypedExpr * expr, std::vector<ResolvExpr::AlternativeFinder> &args );

	  private:
		void match();

		struct Matcher {
		  public:
			Matcher( TupleAssignSpotter_old &spotter, const ResolvExpr::AltList& lhs,
				const ResolvExpr::AltList& rhs );
			virtual ~Matcher() {}

			virtual void match( std::list< Expression * > &out ) = 0;
			ObjectDecl * newObject( UniqueName & namer, Expression * expr );

			void combineState( const ResolvExpr::Alternative& alt ) {
				compositeEnv.simpleCombine( alt.env );
				ResolvExpr::mergeOpenVars( openVars, alt.openVars );
				cloneAll( alt.need, need );
			}

			void combineState( const ResolvExpr::AltList& alts ) {
				for ( const ResolvExpr::Alternative& alt : alts ) { combineState( alt ); }
			}

			ResolvExpr::AltList lhs, rhs;
			TupleAssignSpotter_old &spotter;
			ResolvExpr::Cost baseCost;
			std::list< ObjectDecl * > tmpDecls;
			ResolvExpr::TypeEnvironment compositeEnv;
			ResolvExpr::OpenVarSet openVars;
			ResolvExpr::AssertionSet need;
		};

		struct MassAssignMatcher : public Matcher {
		  public:
			MassAssignMatcher( TupleAssignSpotter_old &spotter, const ResolvExpr::AltList& lhs,
				const ResolvExpr::AltList& rhs ) : Matcher(spotter, lhs, rhs) {}
			virtual void match( std::list< Expression * > &out );
		};

		struct MultipleAssignMatcher : public Matcher {
		  public:
			MultipleAssignMatcher( TupleAssignSpotter_old &spotter, const ResolvExpr::AltList& lhs,
				const ResolvExpr::AltList& rhs ) : Matcher(spotter, lhs, rhs) {}
			virtual void match( std::list< Expression * > &out );
		};

		ResolvExpr::AlternativeFinder &currentFinder;
		std::string fname;
		std::unique_ptr< Matcher > matcher;
	};

	/// true if expr is an expression of tuple type
	bool isTuple( Expression *expr ) {
		if ( ! expr ) return false;
		assert( expr->result );
		return dynamic_cast< TupleType * >( expr->get_result()->stripReferences() );
	}

	template< typename AltIter >
	bool isMultAssign( AltIter begin, AltIter end ) {
		// multiple assignment if more than one alternative in the range or if
		// the alternative is a tuple
		if ( begin == end ) return false;
		if ( isTuple( begin->expr ) ) return true;
		return ++begin != end;
	}

	bool refToTuple( Expression *expr ) {
		assert( expr->get_result() );
		// also check for function returning tuple of reference types
		if ( CastExpr * castExpr = dynamic_cast< CastExpr * >( expr ) ) {
			return refToTuple( castExpr->get_arg() );
		} else {
			return isTuple( expr );
		}
		return false;
	}

	void handleTupleAssignment( ResolvExpr::AlternativeFinder & currentFinder, UntypedExpr * expr,
				std::vector<ResolvExpr::AlternativeFinder> &args ) {
		TupleAssignSpotter_old spotter( currentFinder );
		spotter.spot( expr, args );
	}

	TupleAssignSpotter_old::TupleAssignSpotter_old( ResolvExpr::AlternativeFinder &f )
		: currentFinder(f) {}

	void TupleAssignSpotter_old::spot( UntypedExpr * expr,
			std::vector<ResolvExpr::AlternativeFinder> &args ) {
		if (  NameExpr *op = dynamic_cast< NameExpr * >(expr->get_function()) ) {
			if ( CodeGen::isCtorDtorAssign( op->get_name() ) ) {
				fname = op->get_name();

				// AlternativeFinder will naturally handle this case case, if it's legal
				if ( args.size() == 0 ) return;

				// if an assignment only takes 1 argument, that's odd, but maybe someone wrote
				// the function, in which case AlternativeFinder will handle it normally
				if ( args.size() == 1 && CodeGen::isAssignment( fname ) ) return;

				// look over all possible left-hand-sides
				for ( ResolvExpr::Alternative& lhsAlt : args[0] ) {
					// skip non-tuple LHS
					if ( ! refToTuple(lhsAlt.expr) ) continue;

					// explode is aware of casts - ensure every LHS expression is sent into explode
					// with a reference cast
					// xxx - this seems to change the alternatives before the normal
					//  AlternativeFinder flow; maybe this is desired?
					if ( ! dynamic_cast<CastExpr*>( lhsAlt.expr ) ) {
						lhsAlt.expr = new CastExpr( lhsAlt.expr,
								new ReferenceType( Type::Qualifiers(),
									lhsAlt.expr->result->clone() ) );
					}

					// explode the LHS so that each field of a tuple-valued-expr is assigned
					ResolvExpr::AltList lhs;
					explode( lhsAlt, currentFinder.get_indexer(), back_inserter(lhs), true );
					for ( ResolvExpr::Alternative& alt : lhs ) {
						// each LHS value must be a reference - some come in with a cast expression,
						// if not just cast to reference here
						if ( ! dynamic_cast<ReferenceType*>( alt.expr->get_result() ) ) {
							alt.expr = new CastExpr( alt.expr,
								new ReferenceType( Type::Qualifiers(),
									alt.expr->get_result()->clone() ) );
						}
					}

					if ( args.size() == 1 ) {
						// mass default-initialization/destruction
						ResolvExpr::AltList rhs{};
						matcher.reset( new MassAssignMatcher( *this, lhs, rhs ) );
						match();
					} else if ( args.size() > 2 ) {
						// expand all possible RHS possibilities
						// TODO build iterative version of this instead of using combos
						std::vector< ResolvExpr::AltList > rhsAlts;
						combos( std::next(args.begin(), 1), args.end(),
							std::back_inserter( rhsAlts ) );
						for ( const ResolvExpr::AltList& rhsAlt : rhsAlts ) {
							// multiple assignment
							ResolvExpr::AltList rhs;
							explode( rhsAlt, currentFinder.get_indexer(),
								std::back_inserter(rhs), true );
							matcher.reset( new MultipleAssignMatcher( *this, lhs, rhs ) );
							match();
						}
					} else {
						for ( const ResolvExpr::Alternative& rhsAlt : args[1] ) {
							ResolvExpr::AltList rhs;
							if ( isTuple(rhsAlt.expr) ) {
								// multiple assignment
								explode( rhsAlt, currentFinder.get_indexer(),
									std::back_inserter(rhs), true );
								matcher.reset( new MultipleAssignMatcher( *this, lhs, rhs ) );
							} else {
								// mass assignment
								rhs.push_back( rhsAlt );
								matcher.reset( new MassAssignMatcher( *this, lhs, rhs ) );
							}
							match();
						}
					}
				}
			}
		}
	}

	void TupleAssignSpotter_old::match() {
		assert ( matcher != 0 );

		std::list< Expression * > new_assigns;
		matcher->match( new_assigns );

		if ( ! matcher->lhs.empty() || ! matcher->rhs.empty() ) {
			// if both lhs and rhs are empty then this is the empty tuple case, wherein it's okay for new_assigns to be empty.
			// if not the empty tuple case, return early so that no new alternatives are generated.
			if ( new_assigns.empty() ) return;
		}
		ResolvExpr::AltList current;
		// now resolve new assignments
		for ( std::list< Expression * >::iterator i = new_assigns.begin();
				i != new_assigns.end(); ++i ) {
			PRINT(
				std::cerr << "== resolving tuple assign ==" << std::endl;
				std::cerr << *i << std::endl;
			)

			ResolvExpr::AlternativeFinder finder{ currentFinder.get_indexer(),
				matcher->compositeEnv };

			try {
				finder.findWithAdjustment(*i);
			} catch (...) {
				return; // no match should not mean failure, it just means this particular tuple assignment isn't valid
			}
			// prune expressions that don't coincide with
			ResolvExpr::AltList alts = finder.get_alternatives();
			assert( alts.size() == 1 );
			assert( alts.front().expr != 0 );
			current.push_back( alts.front() );
		}

		// extract expressions from the assignment alternatives to produce a list of assignments
		// that together form a single alternative
		std::list< Expression *> solved_assigns;
		for ( ResolvExpr::Alternative & alt : current ) {
			solved_assigns.push_back( alt.expr->clone() );
			matcher->combineState( alt );
		}

		// xxx -- was push_front
		currentFinder.get_alternatives().push_back( ResolvExpr::Alternative{
			new TupleAssignExpr{ solved_assigns, matcher->tmpDecls }, matcher->compositeEnv,
			matcher->openVars,
			ResolvExpr::AssertionList( matcher->need.begin(), matcher->need.end() ),
			ResolvExpr::sumCost( current ) + matcher->baseCost } );
	}

	TupleAssignSpotter_old::Matcher::Matcher( TupleAssignSpotter_old &spotter,
		const ResolvExpr::AltList &lhs, const ResolvExpr::AltList &rhs )
	: lhs(lhs), rhs(rhs), spotter(spotter),
	  baseCost( ResolvExpr::sumCost( lhs ) + ResolvExpr::sumCost( rhs ) ) {
		combineState( lhs );
		combineState( rhs );
	}

	UntypedExpr * createFunc( const std::string &fname, ObjectDecl *left, ObjectDecl *right ) {
		assert( left );
		std::list< Expression * > args;
		args.push_back( new VariableExpr( left ) );
		// args.push_back( new AddressExpr( new VariableExpr( left ) ) );
		if ( right ) args.push_back( new VariableExpr( right ) );
		if ( left->type->referenceDepth() > 1 && CodeGen::isConstructor( fname ) ) {
			args.front() = new AddressExpr( args.front() );
			if ( right ) args.back() = new AddressExpr( args.back() );
			return new UntypedExpr( new NameExpr( "?=?" ), args );
		} else {
			return new UntypedExpr( new NameExpr( fname ), args );
		}
	}

	// removes environments from subexpressions within statement exprs, which could throw off later passes like those in Box which rely on PolyMutator, and adds the bindings to the compositeEnv
	// xxx - maybe this should happen in alternative finder for every StmtExpr?
	struct EnvRemover {
		void previsit( ExprStmt * stmt ) {
			assert( compositeEnv );
			if ( stmt->expr->env ) {
				compositeEnv->add( *stmt->expr->env );
				delete stmt->expr->env;
				stmt->expr->env = nullptr;
			}
		}

		ResolvExpr::TypeEnvironment * compositeEnv = nullptr;
	};

	ObjectDecl * TupleAssignSpotter_old::Matcher::newObject( UniqueName & namer, Expression * expr ) {
		assert( expr->result && ! expr->get_result()->isVoid() );
		ObjectDecl * ret = new ObjectDecl( namer.newName(), Type::StorageClasses(), LinkageSpec::Cforall, nullptr, expr->result->clone(), new SingleInit( expr->clone() ) );
		// if expression type is a reference, don't need to construct anything, a simple initializer is sufficient.
		if ( ! dynamic_cast< ReferenceType * >( expr->result ) ) {
			ConstructorInit * ctorInit = InitTweak::genCtorInit( ret );
			ret->init = ctorInit;
			ResolvExpr::resolveCtorInit( ctorInit, spotter.currentFinder.get_indexer() ); // resolve ctor/dtors for the new object
			PassVisitor<EnvRemover> rm; // remove environments from subexpressions of StmtExprs
			rm.pass.compositeEnv = &compositeEnv;
			ctorInit->accept( rm );
		}
		PRINT( std::cerr << "new object: " << ret << std::endl; )
		return ret;
	}

	void TupleAssignSpotter_old::MassAssignMatcher::match( std::list< Expression * > &out ) {
		static UniqueName lhsNamer( "__massassign_L" );
		static UniqueName rhsNamer( "__massassign_R" );
		// empty tuple case falls into this matcher, hence the second part of the assert
		assert( (! lhs.empty() && rhs.size() <= 1) || (lhs.empty() && rhs.empty()) );

		// xxx - may need to split this up into multiple declarations, because potential conversion to references
		//  probably should not reference local variable - see MultipleAssignMatcher::match
		ObjectDecl * rtmp = rhs.size() == 1 ? newObject( rhsNamer, rhs.front().expr ) : nullptr;
		for ( ResolvExpr::Alternative & lhsAlt : lhs ) {
			// create a temporary object for each value in the lhs and create a call involving the rhs
			ObjectDecl * ltmp = newObject( lhsNamer, lhsAlt.expr );
			out.push_back( createFunc( spotter.fname, ltmp, rtmp ) );
			tmpDecls.push_back( ltmp );
		}
		if ( rtmp ) tmpDecls.push_back( rtmp );
	}

	void TupleAssignSpotter_old::MultipleAssignMatcher::match( std::list< Expression * > &out ) {
		static UniqueName lhsNamer( "__multassign_L" );
		static UniqueName rhsNamer( "__multassign_R" );

		if ( lhs.size() == rhs.size() ) {
			// produce a new temporary object for each value in the lhs and rhs and pairwise create the calls
			std::list< ObjectDecl * > ltmp;
			std::list< ObjectDecl * > rtmp;
			for ( auto p : group_iterate( lhs, rhs ) ) {
				ResolvExpr::Alternative & lhsAlt = std::get<0>(p);
				ResolvExpr::Alternative & rhsAlt = std::get<1>(p);
				// convert RHS to LHS type minus one reference -- important for the case where LHS is && and RHS is lvalue, etc.
				ReferenceType * lhsType = strict_dynamic_cast<ReferenceType *>( lhsAlt.expr->result );
				rhsAlt.expr = new CastExpr( rhsAlt.expr, lhsType->base->clone() );
				ObjectDecl * lobj = newObject( lhsNamer, lhsAlt.expr );
				ObjectDecl * robj = newObject( rhsNamer, rhsAlt.expr );
				out.push_back( createFunc(spotter.fname, lobj, robj) );
				ltmp.push_back( lobj );
				rtmp.push_back( robj );

				// resolve the cast expression so that rhsAlt return type is bound by the cast type as needed, and transfer the resulting environment
				ResolvExpr::AlternativeFinder finder{ spotter.currentFinder.get_indexer(), compositeEnv };
				finder.findWithAdjustment( rhsAlt.expr );
				assert( finder.get_alternatives().size() == 1 );
				compositeEnv = std::move( finder.get_alternatives().front().env );
			}
			tmpDecls.splice( tmpDecls.end(), ltmp );
			tmpDecls.splice( tmpDecls.end(), rtmp );
		}
	}

namespace {
	/// true if `expr` is of tuple type
	bool isTuple( const ast::Expr * expr ) {
		if ( ! expr ) return false;
		assert( expr->result );
		return dynamic_cast< const ast::TupleType * >( expr->result->stripReferences() );
	}

	/// true if `expr` is of tuple type or a reference to one
	bool refToTuple( const ast::Expr * expr ) {
		assert( expr->result );
		// check for function returning tuple of reference types
		if ( auto castExpr = dynamic_cast< const ast::CastExpr * >( expr ) ) {
			return refToTuple( castExpr->arg );
		} else {
			return isTuple( expr );
		}
	}

	/// Dispatcher for tuple (multiple and mass) assignment operations
	class TupleAssignSpotter_new final {
		/// Actually finds tuple assignment operations, by subclass
		struct Matcher {
			ResolvExpr::CandidateList lhs, rhs;
			TupleAssignSpotter_new & spotter;
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
				TupleAssignSpotter_new & s, const CodeLocation & loc,
				const ResolvExpr::CandidateList & l, const ResolvExpr::CandidateList & r )
			: lhs( l ), rhs( r ), spotter( s ), location( loc ),
			  baseCost( ResolvExpr::sumCost( lhs ) + ResolvExpr::sumCost( rhs ) ), tmpDecls(),
			  env(), open(), need() {
				for ( auto & cand : lhs ) combineState( *cand );
				for ( auto & cand : rhs ) combineState( *cand );
			}
			virtual ~Matcher() = default;

			virtual std::vector< ast::ptr< ast::Expr > > match() = 0;

			/// removes environments from subexpressions within statement expressions, which could
			/// throw off later passes like those in Box which rely on PolyMutator, and adds the
			/// bindings to the env
			struct EnvRemover {
				/// environment to hoist ExprStmt environments to
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
				assert( expr->result && ! expr->result->isVoid() );

				ast::ObjectDecl * ret = new ast::ObjectDecl{
					location, namer.newName(), expr->result, new ast::SingleInit{ location, expr },
					ast::Storage::Classes{}, ast::Linkage::Cforall };

				// if expression type is a reference, just need an initializer, otherwise construct
				if ( ! expr->result.as< ast::ReferenceType >() ) {
					// resolve ctor/dtor for the new object
					ast::ptr< ast::Init > ctorInit = ResolvExpr::resolveCtorInit(
							InitTweak::genCtorInit( location, ret ), spotter.crntFinder.context );
					// remove environments from subexpressions of stmtExpr
					ast::Pass< EnvRemover > rm{ env };
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
				args.emplace_back( new ast::VariableExpr{ location, left } );
				if ( right ) { args.emplace_back( new ast::VariableExpr{ location, right } ); }

				if ( left->type->referenceDepth() > 1 && CodeGen::isConstructor( fname ) ) {
					args.front() = new ast::AddressExpr{ location, args.front() };
					if ( right ) { args.back() = new ast::AddressExpr{ location, args.back() }; }
					return new ast::UntypedExpr{
						location, new ast::NameExpr{ location, "?=?" }, std::move(args) };
				} else {
					return new ast::UntypedExpr{
						location, new ast::NameExpr{ location, fname }, std::move(args) };
				}
			}
		};

		/// Finds mass-assignment operations
		struct MassAssignMatcher final : public Matcher {
			MassAssignMatcher(
				TupleAssignSpotter_new & s, const CodeLocation & loc,
				const ResolvExpr::CandidateList & l, const ResolvExpr::CandidateList & r )
			: Matcher( s, loc, l, r ) {}

			std::vector< ast::ptr< ast::Expr > > match() override {
				static UniqueName lhsNamer( "__massassign_L" );
				static UniqueName rhsNamer( "__massassign_R" );
				// empty tuple case falls into this matcher
				assert( lhs.empty() ? rhs.empty() : rhs.size() <= 1 );

				ast::ptr< ast::ObjectDecl > rtmp =
					rhs.size() == 1 ? newObject( rhsNamer, rhs.front()->expr ) : nullptr;

				std::vector< ast::ptr< ast::Expr > > out;
				for ( ResolvExpr::CandidateRef & lhsCand : lhs ) {
					// create a temporary object for each value in the LHS and create a call
					// involving the RHS
					ast::ptr< ast::ObjectDecl > ltmp = newObject( lhsNamer, lhsCand->expr );
					out.emplace_back( createFunc( spotter.fname, ltmp, rtmp ) );
					tmpDecls.emplace_back( std::move( ltmp ) );
				}
				if ( rtmp ) tmpDecls.emplace_back( std::move( rtmp ) );

				return out;
			}
		};

		/// Finds multiple-assignment operations
		struct MultipleAssignMatcher final : public Matcher {
			MultipleAssignMatcher(
				TupleAssignSpotter_new & s, const CodeLocation & loc,
				const ResolvExpr::CandidateList & l, const ResolvExpr::CandidateList & r )
			: Matcher( s, loc, l, r ) {}

			std::vector< ast::ptr< ast::Expr > > match() override {
				static UniqueName lhsNamer( "__multassign_L" );
				static UniqueName rhsNamer( "__multassign_R" );

				if ( lhs.size() != rhs.size() ) return {};

				// produce a new temporary object for each value in the LHS and RHS and pairwise
				// create the calls
				std::vector< ast::ptr< ast::ObjectDecl > > ltmp, rtmp;

				std::vector< ast::ptr< ast::Expr > > out;
				for ( unsigned i = 0; i < lhs.size(); ++i ) {
					ResolvExpr::CandidateRef & lhsCand = lhs[i];
					ResolvExpr::CandidateRef & rhsCand = rhs[i];

					// convert RHS to LHS type minus one reference -- important for case where LHS
					// is && and RHS is lvalue
					auto lhsType = lhsCand->expr->result.strict_as< ast::ReferenceType >();
					rhsCand->expr = new ast::CastExpr{ rhsCand->expr, lhsType->base };
					ast::ptr< ast::ObjectDecl > lobj = newObject( lhsNamer, lhsCand->expr );
					ast::ptr< ast::ObjectDecl > robj = newObject( rhsNamer, rhsCand->expr );
					out.emplace_back( createFunc( spotter.fname, lobj, robj ) );
					ltmp.emplace_back( std::move( lobj ) );
					rtmp.emplace_back( std::move( robj ) );

					// resolve the cast expression so that rhsCand return type is bound by the cast
					// type as needed, and transfer the resulting environment
					ResolvExpr::CandidateFinder finder( spotter.crntFinder.context, env );
					finder.find( rhsCand->expr, ResolvExpr::ResolvMode::withAdjustment() );
					assert( finder.candidates.size() == 1 );
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
		TupleAssignSpotter_new( ResolvExpr::CandidateFinder & f )
		: crntFinder( f ), fname(), matcher() {}

		// find left- and right-hand-sides for mass or multiple assignment
		void spot(
			const ast::UntypedExpr * expr, std::vector< ResolvExpr::CandidateFinder > & args
		) {
			if ( auto op = expr->func.as< ast::NameExpr >() ) {
				// skip non-assignment functions
				if ( ! CodeGen::isCtorDtorAssign( op->name ) ) return;
				fname = op->name;

				// handled by CandidateFinder if applicable (both odd cases)
				if ( args.empty() || ( args.size() == 1 && CodeGen::isAssignment( fname ) ) ) {
					return;
				}

				// look over all possible left-hand-side
				for ( ResolvExpr::CandidateRef & lhsCand : args[0] ) {
					// skip non-tuple LHS
					if ( ! refToTuple( lhsCand->expr ) ) continue;

					// explode is aware of casts - ensure every LHS is sent into explode with a
					// reference cast
					if ( ! lhsCand->expr.as< ast::CastExpr >() ) {
						lhsCand->expr = new ast::CastExpr{
							lhsCand->expr, new ast::ReferenceType{ lhsCand->expr->result } };
					}

					// explode the LHS so that each field of a tuple-valued expr is assigned
					ResolvExpr::CandidateList lhs;
					explode( *lhsCand, crntFinder.context.symtab, back_inserter(lhs), true );
					for ( ResolvExpr::CandidateRef & cand : lhs ) {
						// each LHS value must be a reference - some come in with a cast, if not
						// just cast to reference here
						if ( ! cand->expr->result.as< ast::ReferenceType >() ) {
							cand->expr = new ast::CastExpr{
								cand->expr, new ast::ReferenceType{ cand->expr->result } };
						}
					}

					if ( args.size() == 1 ) {
						// mass default-initialization/destruction
						ResolvExpr::CandidateList rhs{};
						matcher.reset( new MassAssignMatcher{ *this, expr->location, lhs, rhs } );
						match();
					} else if ( args.size() == 2 ) {
						for ( const ResolvExpr::CandidateRef & rhsCand : args[1] ) {
							ResolvExpr::CandidateList rhs;
							if ( isTuple( rhsCand->expr ) ) {
								// multiple assignment
								explode( *rhsCand, crntFinder.context.symtab, back_inserter(rhs), true );
								matcher.reset(
									new MultipleAssignMatcher{ *this, expr->location, lhs, rhs } );
							} else {
								// mass assignment
								rhs.emplace_back( rhsCand );
								matcher.reset(
									new MassAssignMatcher{ *this, expr->location, lhs, rhs } );
							}
							match();
						}
					} else {
						// expand all possible RHS possibilities
						std::vector< ResolvExpr::CandidateList > rhsCands;
						combos(
							std::next( args.begin(), 1 ), args.end(), back_inserter( rhsCands ) );
						for ( const ResolvExpr::CandidateList & rhsCand : rhsCands ) {
							// multiple assignment
							ResolvExpr::CandidateList rhs;
							explode( rhsCand, crntFinder.context.symtab, back_inserter(rhs), true );
							matcher.reset(
								new MultipleAssignMatcher{ *this, expr->location, lhs, rhs } );
							match();
						}
					}
				}
			}
		}

		void match() {
			assert( matcher );

			std::vector< ast::ptr< ast::Expr > > newAssigns = matcher->match();

			if ( ! ( matcher->lhs.empty() && matcher->rhs.empty() ) ) {
				// if both LHS and RHS are empty than this is the empty tuple case, wherein it's
				// okay for newAssigns to be empty. Otherwise, return early so that no new
				// candidates are generated
				if ( newAssigns.empty() ) return;
			}

			ResolvExpr::CandidateList crnt;
			// now resolve new assignments
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
					// no match is not failure, just that this tuple assignment is invalid
					return;
				}

				ResolvExpr::CandidateList & cands = finder.candidates;
				assert( cands.size() == 1 );
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
				new ast::TupleAssignExpr{
					matcher->location, std::move( solved ), std::move( matcher->tmpDecls ) },
				std::move( matcher->env ), std::move( matcher->open ), std::move( matcher->need ),
				ResolvExpr::sumCost( crnt ) + matcher->baseCost ) );
		}
	};
} // anonymous namespace

void handleTupleAssignment(
	ResolvExpr::CandidateFinder & finder, const ast::UntypedExpr * assign,
	std::vector< ResolvExpr::CandidateFinder > & args
) {
	TupleAssignSpotter_new spotter{ finder };
	spotter.spot( assign, args );
}

} // namespace Tuples

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
