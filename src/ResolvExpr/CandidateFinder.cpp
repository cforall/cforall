//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CandidateFinder.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed Jun 5 14:30:00 2019
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Sep  9 11:30:11 2024
// Update Count     : 5
//

#include "CandidateFinder.hpp"

#include <deque>
#include <iterator>                 // for back_inserter
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include "AdjustExprType.hpp"
#include "Candidate.hpp"
#include "CastCost.hpp"             // for castCost
#include "CompilationState.hpp"
#include "ConversionCost.hpp"       // for conversionCast
#include "Cost.hpp"
#include "ExplodedArg.hpp"
#include "PolyCost.hpp"
#include "RenameVars.hpp"           // for renameTyVars
#include "Resolver.hpp"
#include "ResolveTypeof.hpp"
#include "SatisfyAssertions.hpp"
#include "SpecCost.hpp"
#include "Typeops.hpp"              // for combos
#include "Unify.hpp"
#include "WidenMode.hpp"
#include "AST/Expr.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/Type.hpp"
#include "Common/Utility.hpp"       // for move, copy
#include "SymTab/Mangler.hpp"
#include "Tuples/Tuples.hpp"        // for handleTupleAssignment
#include "InitTweak/InitTweak.hpp"  // for getPointerBase

#include "Common/Stats/Counter.hpp"

#include "AST/Inspect.hpp"             // for getFunctionName

#define PRINT( text ) if ( resolvep ) { text }

namespace ResolvExpr {

/// Unique identifier for matching expression resolutions to their requesting expression
ast::UniqueId globalResnSlot = 0;

namespace {
	/// First index is which argument, second is which alternative, third is which exploded element
	using ExplodedArgs = std::deque< std::vector< ExplodedArg > >;

	/// Returns a list of alternatives with the minimum cost in the given list
	CandidateList findMinCost( const CandidateList & candidates ) {
		CandidateList out;
		Cost minCost = Cost::infinity;
		for ( const CandidateRef & r : candidates ) {
			if ( r->cost < minCost ) {
				minCost = r->cost;
				out.clear();
				out.emplace_back( r );
			} else if ( r->cost == minCost ) {
				out.emplace_back( r );
			}
		}
		return out;
	}

	/// Computes conversion cost for a given expression to a given type
	const ast::Expr * computeExpressionConversionCost(
		const ast::Expr * arg, const ast::Type * paramType, const ast::SymbolTable & symtab, const ast::TypeEnvironment & env, Cost & outCost
	) {
		Cost convCost = computeConversionCost(
				arg->result, paramType, arg->get_lvalue(), symtab, env );
		outCost += convCost;

		// If there is a non-zero conversion cost, ignoring poly cost, then the expression requires
		// conversion. Ignore poly cost for now, since this requires resolution of the cast to
		// infer parameters and this does not currently work for the reason stated below
		Cost tmpCost = convCost;
		tmpCost.incPoly( -tmpCost.get_polyCost() );
		if ( tmpCost != Cost::zero ) {
			ast::ptr< ast::Type > newType = paramType;
			env.apply( newType );
			return new ast::CastExpr{ arg, newType };

			// xxx - *should* be able to resolve this cast, but at the moment pointers are not
			// castable to zero_t, but are implicitly convertible. This is clearly inconsistent,
			// once this is fixed it should be possible to resolve the cast.
			// xxx - this isn't working, it appears because type1 (parameter) is seen as widenable,
			// but it shouldn't be because this makes the conversion from DT* to DT* since
			// commontype(zero_t, DT*) is DT*, rather than nothing

			// CandidateFinder finder{ symtab, env };
			// finder.find( arg, ResolveMode::withAdjustment() );
			// assertf( finder.candidates.size() > 0,
			// 	"Somehow castable expression failed to find alternatives." );
			// assertf( finder.candidates.size() == 1,
			// 	"Somehow got multiple alternatives for known cast expression." );
			// return finder.candidates.front()->expr;
		}

		return arg;
	}

	/// Computes conversion cost for a given candidate
	Cost computeApplicationConversionCost(
		CandidateRef cand, const ast::SymbolTable & symtab
	) {
		auto appExpr = cand->expr.strict_as< ast::ApplicationExpr >();
		auto pointer = appExpr->func->result.strict_as< ast::PointerType >();
		auto function = pointer->base.strict_as< ast::FunctionType >();

		Cost convCost = Cost::zero;
		const auto & params = function->params;
		auto param = params.begin();
		auto & args = appExpr->args;

		for ( unsigned i = 0; i < args.size(); ++i ) {
			const ast::Type * argType = args[i]->result;
			PRINT(
				std::cerr << "arg expression:" << std::endl;
				ast::print( std::cerr, args[i], 2 );
				std::cerr << "--- results are" << std::endl;
				ast::print( std::cerr, argType, 2 );
			)

			if ( param == params.end() ) {
				if ( function->isVarArgs ) {
					convCost.incUnsafe();
					PRINT( std::cerr << "end of params with varargs function: inc unsafe: "
						<< convCost << std::endl; ; )
					// convert reference-typed expressions into value-typed expressions
					cand->expr = ast::mutate_field_index(
						appExpr, &ast::ApplicationExpr::args, i,
						referenceToRvalueConversion( args[i], convCost ) );
					continue;
				} else return Cost::infinity;
			}

			if ( auto def = args[i].as< ast::DefaultArgExpr >() ) {
				// Default arguments should be free - don't include conversion cost.
				// Unwrap them here because they are not relevant to the rest of the system
				cand->expr = ast::mutate_field_index(
					appExpr, &ast::ApplicationExpr::args, i, def->expr );
				++param;
				continue;
			}

			// mark conversion cost and also specialization cost of param type
			// const ast::Type * paramType = (*param)->get_type();
			cand->expr = ast::mutate_field_index(
				appExpr, &ast::ApplicationExpr::args, i,
				computeExpressionConversionCost(
					args[i], *param, symtab, cand->env, convCost ) );
			convCost.decSpec( specCost( *param ) );
			++param;  // can't be in for-loop update because of the continue
		}

		if ( param != params.end() ) return Cost::infinity;

		// specialization cost of return types can't be accounted for directly, it disables
		// otherwise-identical calls, like this example based on auto-newline in the I/O lib:
		//
		//   forall(otype OS) {
		//     void ?|?(OS&, int);  // with newline
		//     OS&  ?|?(OS&, int);  // no newline, always chosen due to more specialization
		//   }

		// mark type variable and specialization cost of forall clause
		convCost.incVar( function->forall.size() );
		convCost.decSpec( function->assertions.size() );

		return convCost;
	}

	void makeUnifiableVars(
		const ast::FunctionType * type, ast::OpenVarSet & unifiableVars,
		ast::AssertionSet & need
	) {
		for ( auto & tyvar : type->forall ) {
			unifiableVars[ *tyvar ] = ast::TypeData{ tyvar->base };
		}
		for ( auto & assn : type->assertions ) {
			need[ assn ].isUsed = true;
		}
	}

	/// Gets a default value from an initializer, nullptr if not present
	const ast::ConstantExpr * getDefaultValue( const ast::Init * init ) {
		if ( auto si = dynamic_cast< const ast::SingleInit * >( init ) ) {
			if ( auto ce = si->value.as< ast::CastExpr >() ) {
				return ce->arg.as< ast::ConstantExpr >();
			} else {
				return si->value.as< ast::ConstantExpr >();
			}
		}
		return nullptr;
	}

	/// State to iteratively build a match of parameter expressions to arguments
	struct ArgPack {
		std::size_t parent;          ///< Index of parent pack
		ast::ptr< ast::Expr > expr;  ///< The argument stored here
		Cost cost;                   ///< The cost of this argument
		ast::TypeEnvironment env;    ///< Environment for this pack
		ast::AssertionSet need;      ///< Assertions outstanding for this pack
		ast::AssertionSet have;      ///< Assertions found for this pack
		ast::OpenVarSet open;        ///< Open variables for this pack
		unsigned nextArg;            ///< Index of next argument in arguments list
		unsigned tupleStart;         ///< Number of tuples that start at this index
		unsigned nextExpl;           ///< Index of next exploded element
		unsigned explAlt;            ///< Index of alternative for nextExpl > 0

		ArgPack()
		: parent( 0 ), expr(), cost( Cost::zero ), env(), need(), have(), open(), nextArg( 0 ),
		  tupleStart( 0 ), nextExpl( 0 ), explAlt( 0 ) {}

		ArgPack(
			const ast::TypeEnvironment & env, const ast::AssertionSet & need,
			const ast::AssertionSet & have, const ast::OpenVarSet & open )
		: parent( 0 ), expr(), cost( Cost::zero ), env( env ), need( need ), have( have ),
		  open( open ), nextArg( 0 ), tupleStart( 0 ), nextExpl( 0 ), explAlt( 0 ) {}

		ArgPack(
			std::size_t parent, const ast::Expr * expr, ast::TypeEnvironment && env,
			ast::AssertionSet && need, ast::AssertionSet && have, ast::OpenVarSet && open,
			unsigned nextArg, unsigned tupleStart = 0, Cost cost = Cost::zero,
			unsigned nextExpl = 0, unsigned explAlt = 0 )
		: parent(parent), expr( expr ), cost( cost ), env( std::move( env ) ), need( std::move( need ) ),
		  have( std::move( have ) ), open( std::move( open ) ), nextArg( nextArg ), tupleStart( tupleStart ),
		  nextExpl( nextExpl ), explAlt( explAlt ) {}

		ArgPack(
			const ArgPack & o, ast::TypeEnvironment && env, ast::AssertionSet && need,
			ast::AssertionSet && have, ast::OpenVarSet && open, unsigned nextArg, Cost added )
		: parent( o.parent ), expr( o.expr ), cost( o.cost + added ), env( std::move( env ) ),
		  need( std::move( need ) ), have( std::move( have ) ), open( std::move( open ) ), nextArg( nextArg ),
		  tupleStart( o.tupleStart ), nextExpl( 0 ), explAlt( 0 ) {}

		/// true if this pack is in the middle of an exploded argument
		bool hasExpl() const { return nextExpl > 0; }

		/// Gets the list of exploded candidates for this pack
		const ExplodedArg & getExpl( const ExplodedArgs & args ) const {
			return args[ nextArg-1 ][ explAlt ];
		}

		/// Ends a tuple expression, consolidating the appropriate args
		void endTuple( const std::vector< ArgPack > & packs ) {
			// add all expressions in tuple to list, summing cost
			std::deque< const ast::Expr * > exprs;
			const ArgPack * pack = this;
			if ( expr ) { exprs.emplace_front( expr ); }
			while ( pack->tupleStart == 0 ) {
				pack = &packs[pack->parent];
				exprs.emplace_front( pack->expr );
				cost += pack->cost;
			}
			// reset pack to appropriate tuple
			std::vector< ast::ptr< ast::Expr > > exprv( exprs.begin(), exprs.end() );
			expr = new ast::TupleExpr{ expr->location, std::move( exprv ) };
			tupleStart = pack->tupleStart - 1;
			parent = pack->parent;
		}
	};

	/// Instantiates an argument to match a parameter, returns false if no matching results left
	bool instantiateArgument(
		const CodeLocation & location,
		const ast::Type * paramType, const ast::Init * init, const ExplodedArgs & args,
		std::vector< ArgPack > & results, std::size_t & genStart, const ResolveContext & context,
		unsigned nTuples = 0
	) {
		if ( auto tupleType = dynamic_cast< const ast::TupleType * >( paramType ) ) {
			// paramType is a TupleType -- group args into a TupleExpr
			++nTuples;
			for ( const ast::Type * type : *tupleType ) {
				// xxx - dropping initializer changes behaviour from previous, but seems correct
				// ^^^ need to handle the case where a tuple has a default argument
				if ( ! instantiateArgument( location,
					type, nullptr, args, results, genStart, context, nTuples ) ) return false;
				nTuples = 0;
			}
			// re-constitute tuples for final generation
			for ( auto i = genStart; i < results.size(); ++i ) {
				results[i].endTuple( results );
			}
			return true;
		} else if ( const ast::TypeInstType * ttype = Tuples::isTtype( paramType ) ) {
			// paramType is a ttype, consumes all remaining arguments

			// completed tuples; will be spliced to end of results to finish
			std::vector< ArgPack > finalResults{};

			// iterate until all results completed
			std::size_t genEnd;
			++nTuples;
			do {
				genEnd = results.size();

				// add another argument to results
				for ( std::size_t i = genStart; i < genEnd; ++i ) {
					unsigned nextArg = results[i].nextArg;

					// use next element of exploded tuple if present
					if ( results[i].hasExpl() ) {
						const ExplodedArg & expl = results[i].getExpl( args );

						unsigned nextExpl = results[i].nextExpl + 1;
						if ( nextExpl == expl.exprs.size() ) { nextExpl = 0; }

						results.emplace_back(
							i, expl.exprs[ results[i].nextExpl ], copy( results[i].env ),
							copy( results[i].need ), copy( results[i].have ),
							copy( results[i].open ), nextArg, nTuples, Cost::zero, nextExpl,
							results[i].explAlt );

						continue;
					}

					// finish result when out of arguments
					if ( nextArg >= args.size() ) {
						ArgPack newResult{
							results[i].env, results[i].need, results[i].have, results[i].open };
						newResult.nextArg = nextArg;
						const ast::Type * argType = nullptr;

						if ( nTuples > 0 || ! results[i].expr ) {
							// first iteration or no expression to clone,
							// push empty tuple expression
							newResult.parent = i;
							newResult.expr = new ast::TupleExpr( location, {} );
							argType = newResult.expr->result;
						} else {
							// clone result to collect tuple
							newResult.parent = results[i].parent;
							newResult.cost = results[i].cost;
							newResult.tupleStart = results[i].tupleStart;
							newResult.expr = results[i].expr;
							argType = newResult.expr->result;

							if ( results[i].tupleStart > 0 && Tuples::isTtype( argType ) ) {
								// the case where a ttype value is passed directly is special,
								// e.g. for argument forwarding purposes
								// xxx - what if passing multiple arguments, last of which is
								//       ttype?
								// xxx - what would happen if unify was changed so that unifying
								//       tuple
								// types flattened both before unifying lists? then pass in
								// TupleType (ttype) below.
								--newResult.tupleStart;
							} else {
								// collapse leftover arguments into tuple
								newResult.endTuple( results );
								argType = newResult.expr->result;
							}
						}

						// check unification for ttype before adding to final
						if (
							unify(
								ttype, argType, newResult.env, newResult.need, newResult.have,
								newResult.open )
						) {
							finalResults.emplace_back( std::move( newResult ) );
						}

						continue;
					}

					// add each possible next argument
					for ( std::size_t j = 0; j < args[nextArg].size(); ++j ) {
						const ExplodedArg & expl = args[nextArg][j];

						// fresh copies of parent parameters for this iteration
						ast::TypeEnvironment env = results[i].env;
						ast::OpenVarSet open = results[i].open;

						env.addActual( expl.env, open );

						// skip empty tuple arguments by (nearly) cloning parent into next gen
						if ( expl.exprs.empty() ) {
							results.emplace_back(
								results[i], std::move( env ), copy( results[i].need ),
								copy( results[i].have ), std::move( open ), nextArg + 1, expl.cost );

							continue;
						}

						// add new result
						results.emplace_back(
							i, expl.exprs.front(), std::move( env ), copy( results[i].need ),
							copy( results[i].have ), std::move( open ), nextArg + 1, nTuples,
							expl.cost, expl.exprs.size() == 1 ? 0 : 1, j );
					}
				}

				// reset for next round
				genStart = genEnd;
				nTuples = 0;
			} while ( genEnd != results.size() );

			// splice final results onto results
			for ( std::size_t i = 0; i < finalResults.size(); ++i ) {
				results.emplace_back( std::move( finalResults[i] ) );
			}
			return ! finalResults.empty();
		}

		// iterate each current subresult
		std::size_t genEnd = results.size();
		for ( std::size_t i = genStart; i < genEnd; ++i ) {
			unsigned nextArg = results[i].nextArg;

			// use remainder of exploded tuple if present
			if ( results[i].hasExpl() ) {
				const ExplodedArg & expl = results[i].getExpl( args );
				const ast::Expr * expr = expl.exprs[ results[i].nextExpl ];

				ast::TypeEnvironment env = results[i].env;
				ast::AssertionSet need = results[i].need, have = results[i].have;
				ast::OpenVarSet open = results[i].open;

				const ast::Type * argType = expr->result;

				PRINT(
					std::cerr << "param type is ";
					ast::print( std::cerr, paramType );
					std::cerr << std::endl << "arg type is ";
					ast::print( std::cerr, argType );
					std::cerr << std::endl;
				)

				if ( unify( paramType, argType, env, need, have, open ) ) {
					unsigned nextExpl = results[i].nextExpl + 1;
					if ( nextExpl == expl.exprs.size() ) { nextExpl = 0; }

					results.emplace_back(
						i, expr, std::move( env ), std::move( need ), std::move( have ), std::move( open ), nextArg,
						nTuples, Cost::zero, nextExpl, results[i].explAlt );
				}

				continue;
			}

			// use default initializers if out of arguments
			if ( nextArg >= args.size() ) {
				if ( const ast::ConstantExpr * cnst = getDefaultValue( init ) ) {
					ast::TypeEnvironment env = results[i].env;
					ast::AssertionSet need = results[i].need, have = results[i].have;
					ast::OpenVarSet open = results[i].open;

					if ( unify( paramType, cnst->result, env, need, have, open ) ) {
						results.emplace_back(
							i, new ast::DefaultArgExpr{ cnst->location, cnst }, std::move( env ),
							std::move( need ), std::move( have ), std::move( open ), nextArg, nTuples );
					}
				}

				continue;
			}

			// Check each possible next argument
			for ( std::size_t j = 0; j < args[nextArg].size(); ++j ) {
				const ExplodedArg & expl = args[nextArg][j];

				// fresh copies of parent parameters for this iteration
				ast::TypeEnvironment env = results[i].env;
				ast::AssertionSet need = results[i].need, have = results[i].have;
				ast::OpenVarSet open = results[i].open;

				env.addActual( expl.env, open );

				// skip empty tuple arguments by (nearly) cloning parent into next gen
				if ( expl.exprs.empty() ) {
					results.emplace_back(
						results[i], std::move( env ), std::move( need ), std::move( have ), std::move( open ),
						nextArg + 1, expl.cost );

					continue;
				}

				// consider only first exploded arg
				const ast::Expr * expr = expl.exprs.front();
				const ast::Type * argType = expr->result;

				PRINT(
					std::cerr << "param type is ";
					ast::print( std::cerr, paramType );
					std::cerr << std::endl << "arg type is ";
					ast::print( std::cerr, argType );
					std::cerr << std::endl;
				)

				// attempt to unify types
				ast::ptr<ast::Type> common;
				if ( unify( paramType, argType, env, need, have, open, common ) ) {
					// add new result
					assert( common );

					auto paramAsEnum = dynamic_cast<const ast::EnumInstType *>(paramType);
					auto argAsEnum =dynamic_cast<const ast::EnumInstType *>(argType);
					if (paramAsEnum && argAsEnum) {
						if (paramAsEnum->base->name != argAsEnum->base->name) {
							Cost c = castCost(argType, paramType, expr, context.symtab, env);
							if (c < Cost::infinity) {
								CandidateFinder subFinder( context, env );
								expr = subFinder.makeEnumOffsetCast(argAsEnum, paramAsEnum, expr, c);
								if ( expr )
									results.emplace_back(
										i, expr, std::move( env ), std::move( need ), std::move( have ), std::move( open ),
										nextArg + 1, nTuples, expl.cost + c, expl.exprs.size() == 1 ? 0 : 1, j );
								continue;
							} else {
								std::cerr << "Cannot instantiate " << paramAsEnum->base->name <<  " with " << argAsEnum->base->name << std::endl;
							}
						}
					}
					results.emplace_back(
						i, expr, std::move( env ), std::move( need ), std::move( have ), std::move( open ),
						nextArg + 1, nTuples, expl.cost, expl.exprs.size() == 1 ? 0 : 1, j );
				}
			}
		}

		// reset for next parameter
		genStart = genEnd;

		return genEnd != results.size();  // were any new results added?
	}

	/// Generate a cast expression from `arg` to `toType`
	const ast::Expr * restructureCast(
		ast::ptr< ast::Expr > & arg, const ast::Type * toType, ast::GeneratedFlag isGenerated = ast::GeneratedCast
	) {
		if (
			arg->result->size() > 1
			&& ! toType->isVoid()
			&& ! dynamic_cast< const ast::ReferenceType * >( toType )
		) {
			// Argument is a tuple and the target type is neither void nor a reference. Cast each
			// member of the tuple to its corresponding target type, producing the tuple of those
			// cast expressions. If there are more components of the tuple than components in the
			// target type, then excess components do not come out in the result expression (but
			// UniqueExpr ensures that the side effects will still be produced)
			if ( Tuples::maybeImpureIgnoreUnique( arg ) ) {
				// expressions which may contain side effects require a single unique instance of
				// the expression
				arg = new ast::UniqueExpr{ arg->location, arg };
			}
			std::vector< ast::ptr< ast::Expr > > components;
			for ( unsigned i = 0; i < toType->size(); ++i ) {
				// cast each component
				ast::ptr< ast::Expr > idx = new ast::TupleIndexExpr{ arg->location, arg, i };
				components.emplace_back(
					restructureCast( idx, toType->getComponent( i ), isGenerated ) );
			}
			return new ast::TupleExpr{ arg->location, std::move( components ) };
		} else {
			// handle normally
			return new ast::CastExpr{ arg->location, arg, toType, isGenerated };
		}
	}

	/// Gets the name from an untyped member expression (must be NameExpr)
	const std::string & getMemberName( const ast::UntypedMemberExpr * memberExpr ) {
		if ( memberExpr->member.as< ast::ConstantExpr >() ) {
			SemanticError( memberExpr, "Indexed access to struct fields unsupported: " );
		}

		return memberExpr->member.strict_as< ast::NameExpr >()->name;
	}

	/// Actually visits expressions to find their candidate interpretations
	class Finder final : public ast::WithShortCircuiting {
		const ResolveContext & context;
		const ast::SymbolTable & symtab;
	public:
		// static size_t traceId;
		CandidateFinder & selfFinder;
		CandidateList & candidates;
		const ast::TypeEnvironment & tenv;
		ast::ptr< ast::Type > & targetType;

		enum Errors {
			NotFound,
			NoMatch,
			ArgsToFew,
			ArgsToMany,
			RetsToFew,
			RetsToMany,
			NoReason
		};

		struct {
			Errors code = NotFound;
		} reason;

		Finder( CandidateFinder & f )
		: context( f.context ), symtab( context.symtab ), selfFinder( f ),
		  candidates( f.candidates ), tenv( f.env ), targetType( f.targetType ) {}

		void previsit( const ast::Node * ) { visit_children = false; }

		/// Convenience to add candidate to list
		template<typename... Args>
		void addCandidate( Args &&... args ) {
			candidates.emplace_back( new Candidate{ std::forward<Args>( args )... } );
			reason.code = NoReason;
		}

		void postvisit( const ast::ApplicationExpr * applicationExpr ) {
			addCandidate( applicationExpr, tenv );
		}

		/// Set up candidate assertions for inference
		void inferParameters( CandidateRef & newCand, CandidateList & out );

		/// Completes a function candidate with arguments located
		void validateFunctionCandidate(
			const CandidateRef & func, ArgPack & result, const std::vector< ArgPack > & results,
			CandidateList & out );

		/// Builds a list of candidates for a function, storing them in out
		void makeFunctionCandidates(
			const CodeLocation & location,
			const CandidateRef & func, const ast::FunctionType * funcType,
			const ExplodedArgs & args, CandidateList & out );

		/// Adds implicit struct-conversions to the alternative list
		void addAnonConversions( const CandidateRef & cand );

		/// Adds aggregate member interpretations
		void addAggMembers(
			const ast::BaseInstType * aggrInst, const ast::Expr * expr,
			const Candidate & cand, const Cost & addedCost, const std::string & name
		);

		void addEnumValueAsCandidate(const ast::EnumInstType * instType, const ast::Expr * expr,
			const Cost & addedCost
		);

		/// Adds tuple member interpretations
		void addTupleMembers(
			const ast::TupleType * tupleType, const ast::Expr * expr, const Candidate & cand,
			const Cost & addedCost, const ast::Expr * member
		);

		/// true if expression is an lvalue
		static bool isLvalue( const ast::Expr * x ) {
			return x->result && ( x->get_lvalue() || x->result.as< ast::ReferenceType >() );
		}

		void postvisit( const ast::UntypedExpr * untypedExpr );
		void postvisit( const ast::VariableExpr * variableExpr );
		void postvisit( const ast::ConstantExpr * constantExpr );
		void postvisit( const ast::SizeofExpr * sizeofExpr );
		void postvisit( const ast::AlignofExpr * alignofExpr );
		void postvisit( const ast::AddressExpr * addressExpr );
		void postvisit( const ast::LabelAddressExpr * labelExpr );
		void postvisit( const ast::CastExpr * castExpr );
		void postvisit( const ast::VirtualCastExpr * castExpr );
		void postvisit( const ast::KeywordCastExpr * castExpr );
		void postvisit( const ast::UntypedMemberExpr * memberExpr );
		void postvisit( const ast::MemberExpr * memberExpr );
		void postvisit( const ast::NameExpr * nameExpr );
		void postvisit( const ast::UntypedOffsetofExpr * offsetofExpr );
		void postvisit( const ast::OffsetofExpr * offsetofExpr );
		void postvisit( const ast::OffsetPackExpr * offsetPackExpr );
		void postvisit( const ast::LogicalExpr * logicalExpr );
		void postvisit( const ast::ConditionalExpr * conditionalExpr );
		void postvisit( const ast::CommaExpr * commaExpr );
		void postvisit( const ast::ImplicitCopyCtorExpr * ctorExpr );
		void postvisit( const ast::ConstructorExpr * ctorExpr );
		void postvisit( const ast::RangeExpr * rangeExpr );
		void postvisit( const ast::UntypedTupleExpr * tupleExpr );
		void postvisit( const ast::TupleExpr * tupleExpr );
		void postvisit( const ast::TupleIndexExpr * tupleExpr );
		void postvisit( const ast::TupleAssignExpr * tupleExpr );
		void postvisit( const ast::UniqueExpr * unqExpr );
		void postvisit( const ast::StmtExpr * stmtExpr );
		void postvisit( const ast::UntypedInitExpr * initExpr );
		void postvisit( const ast::QualifiedNameExpr * qualifiedExpr );
		void postvisit( const ast::CountExpr * countExpr );

		void postvisit( const ast::InitExpr * ) {
			assertf( false, "CandidateFinder should never see a resolved InitExpr." );
		}

		void postvisit( const ast::DeletedExpr * ) {
			assertf( false, "CandidateFinder should never see a DeletedExpr." );
		}

		void postvisit( const ast::GenericExpr * ) {
			assertf( false, "_Generic is not yet supported." );
		}
	};

	/// Set up candidate assertions for inference
	void Finder::inferParameters( CandidateRef & newCand, CandidateList & out ) {
		// Set need bindings for any unbound assertions
		ast::UniqueId crntResnSlot = 0; // matching ID for this expression's assertions
		for ( auto & assn : newCand->need ) {
			// skip already-matched assertions
			if ( assn.second.resnSlot != 0 ) continue;
			// assign slot for expression if needed
			if ( crntResnSlot == 0 ) { crntResnSlot = ++globalResnSlot; }
			// fix slot to assertion
			assn.second.resnSlot = crntResnSlot;
		}
		// pair slot to expression
		if ( crntResnSlot != 0 ) {
			newCand->expr.get_and_mutate()->inferred.resnSlots().emplace_back( crntResnSlot );
		}

		// add to output list; assertion satisfaction will occur later
		out.emplace_back( newCand );
	}

	/// Completes a function candidate with arguments located
	void Finder::validateFunctionCandidate(
		const CandidateRef & func, ArgPack & result, const std::vector< ArgPack > & results,
		CandidateList & out
	) {
		ast::ApplicationExpr * appExpr =
			new ast::ApplicationExpr{ func->expr->location, func->expr };
		// sum cost and accumulate arguments
		std::deque< const ast::Expr * > args;
		Cost cost = func->cost;
		const ArgPack * pack = &result;
		while ( pack->expr ) {
			args.emplace_front( pack->expr );
			cost += pack->cost;
			pack = &results[pack->parent];
		}
		std::vector< ast::ptr< ast::Expr > > vargs( args.begin(), args.end() );
		appExpr->args = std::move( vargs );
		// build and validate new candidate
		auto newCand =
			std::make_shared<Candidate>( appExpr, result.env, result.open, result.need, cost );
		PRINT(
			std::cerr << "instantiate function success: " << appExpr << std::endl;
			std::cerr << "need assertions:" << std::endl;
			ast::print( std::cerr, result.need, 2 );
		)
		inferParameters( newCand, out );
	}

	/// Builds a list of candidates for a function, storing them in out
	void Finder::makeFunctionCandidates(
		const CodeLocation & location,
		const CandidateRef & func, const ast::FunctionType * funcType,
		const ExplodedArgs & args, CandidateList & out
	) {
		ast::OpenVarSet funcOpen;
		ast::AssertionSet funcNeed, funcHave;
		ast::TypeEnvironment funcEnv{ func->env };
		makeUnifiableVars( funcType, funcOpen, funcNeed );
		// add all type variables as open variables now so that those not used in the
		// parameter list are still considered open
		funcEnv.add( funcType->forall );

		if ( targetType && ! targetType->isVoid() && ! funcType->returns.empty() ) {
			// attempt to narrow based on expected target type
			const ast::Type * returnType = funcType->returns.front();
			if ( selfFinder.strictMode ) {
				if ( !unifyExact(
					returnType, targetType, funcEnv, funcNeed, funcHave, funcOpen, noWiden() ) // xxx - is no widening correct?
				) {
					// unification failed, do not pursue this candidate
					return;
				}
			} else {
				if ( !unify(
					returnType, targetType, funcEnv, funcNeed, funcHave, funcOpen )
				) {
					// unification failed, do not pursue this candidate
					return;
				}
			}
		}

		// iteratively build matches, one parameter at a time
		std::vector< ArgPack > results;
		results.emplace_back( funcEnv, funcNeed, funcHave, funcOpen );
		std::size_t genStart = 0;

		// xxx - how to handle default arg after change to ftype representation?
		if (const ast::VariableExpr * varExpr = func->expr.as<ast::VariableExpr>()) {
			if (const ast::FunctionDecl * funcDecl = varExpr->var.as<ast::FunctionDecl>()) {
				// function may have default args only if directly calling by name
				// must use types on candidate however, due to RenameVars substitution
				auto nParams = funcType->params.size();

				for (size_t i=0; i<nParams; ++i) {
					auto obj = funcDecl->params[i].strict_as<ast::ObjectDecl>();
					if ( !instantiateArgument( location,
						funcType->params[i], obj->init, args, results, genStart, context)) return;
				}
				goto endMatch;
			}
		}
		for ( const auto & param : funcType->params ) {
			// Try adding the arguments corresponding to the current parameter to the existing
			// matches
			// no default args for indirect calls
			if ( !instantiateArgument( location,
				param, nullptr, args, results, genStart, context ) ) return;
		}

		endMatch:
		if ( funcType->isVarArgs ) {
			// append any unused arguments to vararg pack
			std::size_t genEnd;
			do {
				genEnd = results.size();

				// iterate results
				for ( std::size_t i = genStart; i < genEnd; ++i ) {
					unsigned nextArg = results[i].nextArg;

					// use remainder of exploded tuple if present
					if ( results[i].hasExpl() ) {
						const ExplodedArg & expl = results[i].getExpl( args );

						unsigned nextExpl = results[i].nextExpl + 1;
						if ( nextExpl == expl.exprs.size() ) { nextExpl = 0; }

						results.emplace_back(
							i, expl.exprs[ results[i].nextExpl ], copy( results[i].env ),
							copy( results[i].need ), copy( results[i].have ),
							copy( results[i].open ), nextArg, 0, Cost::zero, nextExpl,
							results[i].explAlt );

						continue;
					}

					// finish result when out of arguments
					if ( nextArg >= args.size() ) {
						validateFunctionCandidate( func, results[i], results, out );

						continue;
					}

					// add each possible next argument
					for ( std::size_t j = 0; j < args[nextArg].size(); ++j ) {
						const ExplodedArg & expl = args[nextArg][j];

						// fresh copies of parent parameters for this iteration
						ast::TypeEnvironment env = results[i].env;
						ast::OpenVarSet open = results[i].open;

						env.addActual( expl.env, open );

						// skip empty tuple arguments by (nearly) cloning parent into next gen
						if ( expl.exprs.empty() ) {
							results.emplace_back(
								results[i], std::move( env ), copy( results[i].need ),
								copy( results[i].have ), std::move( open ), nextArg + 1,
								expl.cost );

							continue;
						}

						// add new result
						results.emplace_back(
							i, expl.exprs.front(), std::move( env ), copy( results[i].need ),
							copy( results[i].have ), std::move( open ), nextArg + 1, 0, expl.cost,
							expl.exprs.size() == 1 ? 0 : 1, j );
					}
				}

				genStart = genEnd;
			} while( genEnd != results.size() );
		} else {
			// filter out the results that don't use all the arguments
			for ( std::size_t i = genStart; i < results.size(); ++i ) {
				ArgPack & result = results[i];
				if ( ! result.hasExpl() && result.nextArg >= args.size() ) {
					validateFunctionCandidate( func, result, results, out );
				}
			}
		}
	}

	void Finder::addEnumValueAsCandidate( const ast::EnumInstType * enumInst, const ast::Expr * expr,
		const Cost & addedCost
	) {
		if ( enumInst->base->base ) {
			CandidateFinder finder( context, tenv );
			auto location = expr->location;
			auto callExpr = new ast::UntypedExpr(
				location, new ast::NameExpr( location, "value" ), {expr}
			);
			finder.find( callExpr );
			CandidateList winners = findMinCost( finder.candidates );
			if (winners.size() != 1) {
				SemanticError( callExpr, "Ambiguous expression in value..." );
			}
			CandidateRef & choice = winners.front();
			choice->cost += addedCost;
			addAnonConversions(choice);
			candidates.emplace_back( std::move(choice) );
		}
	}

	/// Adds implicit struct-conversions to the alternative list
	void Finder::addAnonConversions( const CandidateRef & cand ) {
		// adds anonymous member interpretations whenever an aggregate value type is seen.
		// it's okay for the aggregate expression to have reference type -- cast it to the
		// base type to treat the aggregate as the referenced value
		ast::ptr< ast::Expr > aggrExpr( cand->expr );
		ast::ptr< ast::Type > & aggrType = aggrExpr.get_and_mutate()->result;
		cand->env.apply( aggrType );

		if ( aggrType.as< ast::ReferenceType >() ) {
			aggrExpr = new ast::CastExpr{ aggrExpr, aggrType->stripReferences() };
		}

		if ( auto structInst = aggrExpr->result.as< ast::StructInstType >() ) {
			addAggMembers( structInst, aggrExpr, *cand, Cost::unsafe, "" );
		} else if ( auto unionInst = aggrExpr->result.as< ast::UnionInstType >() ) {
			addAggMembers( unionInst, aggrExpr, *cand, Cost::unsafe, "" );
		} else if ( auto enumInst = aggrExpr->result.as< ast::EnumInstType >() ) {
			addEnumValueAsCandidate( enumInst, aggrExpr, Cost::implicit );
		}
	}
	

	/// Adds aggregate member interpretations
	void Finder::addAggMembers(
		const ast::BaseInstType * aggrInst, const ast::Expr * expr,
		const Candidate & cand, const Cost & addedCost, const std::string & name
	) {
		for ( const ast::Decl * decl : aggrInst->lookup( name ) ) {
			auto dwt = strict_dynamic_cast< const ast::DeclWithType * >( decl );
			CandidateRef newCand = std::make_shared<Candidate>(
				cand, new ast::MemberExpr{ expr->location, dwt, expr }, addedCost );
			// add anonymous member interpretations whenever an aggregate value type is seen
			// as a member expression
			addAnonConversions( newCand );
			candidates.emplace_back( std::move( newCand ) );
		}
	}

	/// Adds tuple member interpretations
	void Finder::addTupleMembers(
		const ast::TupleType * tupleType, const ast::Expr * expr, const Candidate & cand,
		const Cost & addedCost, const ast::Expr * member
	) {
		if ( auto constantExpr = dynamic_cast< const ast::ConstantExpr * >( member ) ) {
			// get the value of the constant expression as an int, must be between 0 and the
			// length of the tuple to have meaning
			long long val = constantExpr->intValue();
			if ( val >= 0 && (unsigned long long)val < tupleType->size() ) {
				addCandidate(
					cand, new ast::TupleIndexExpr{ expr->location, expr, (unsigned)val },
					addedCost );
			}
		}
	}

	void Finder::postvisit( const ast::UntypedExpr * untypedExpr ) {
		std::vector< CandidateFinder > argCandidates =
			selfFinder.findSubExprs( untypedExpr->args );

		// take care of possible tuple assignments
		// if not tuple assignment, handled as normal function call
		Tuples::handleTupleAssignment( selfFinder, untypedExpr, argCandidates );

		CandidateFinder funcFinder( context, tenv );
		std::string funcName;
		if (auto nameExpr = untypedExpr->func.as<ast::NameExpr>()) {
			funcName = nameExpr->name;
			auto kind = ast::SymbolTable::getSpecialFunctionKind(nameExpr->name);
			if (kind != ast::SymbolTable::SpecialFunctionKind::NUMBER_OF_KINDS) {
				assertf(!argCandidates.empty(), "special function call without argument");
				for (auto & firstArgCand: argCandidates[0]) {
					ast::ptr<ast::Type> argType = firstArgCand->expr->result;
					firstArgCand->env.apply(argType);
					// strip references
					// xxx - is this correct?
					while (argType.as<ast::ReferenceType>()) argType = argType.as<ast::ReferenceType>()->base;

					// convert 1-tuple to plain type
					if (auto tuple = argType.as<ast::TupleType>()) {
						if (tuple->size() == 1) {
							argType = tuple->types[0];
						}
					}

					// if argType is an unbound type parameter, all special functions need to be searched.
					if (isUnboundType(argType)) {
						funcFinder.otypeKeys.clear();
						break;
					}

					if (argType.as<ast::PointerType>()) funcFinder.otypeKeys.insert(Mangle::Encoding::pointer);
					else funcFinder.otypeKeys.insert(Mangle::mangle(argType, Mangle::NoGenericParams | Mangle::Type));
				}
			}
		}
		// if candidates are already produced, do not fail
		// xxx - is it possible that handleTupleAssignment and main finder both produce candidates?
		// this means there exists ctor/assign functions with a tuple as first parameter.
		ResolveMode mode = {
			true, // adjust
			!untypedExpr->func.as<ast::NameExpr>(), // prune if not calling by name
			selfFinder.candidates.empty() // failfast if other options are not found
		};
		funcFinder.find( untypedExpr->func, mode );
		// short-circuit if no candidates
		// if ( funcFinder.candidates.empty() ) return;

		reason.code = NoMatch;

		// find function operators
		ast::ptr< ast::Expr > opExpr = new ast::NameExpr{ untypedExpr->location, "?()" }; // ??? why not ?{}
		CandidateFinder opFinder( context, tenv );
		// okay if there aren't any function operations
		opFinder.find( opExpr, ResolveMode::withoutFailFast() );
		PRINT(
			std::cerr << "known function ops:" << std::endl;
			print( std::cerr, opFinder.candidates, 1 );
		)

		// pre-explode arguments
		ExplodedArgs argExpansions;
		for ( const CandidateFinder & args : argCandidates ) {
			argExpansions.emplace_back();
			auto & argE = argExpansions.back();
			for ( const CandidateRef & arg : args ) { argE.emplace_back( *arg, symtab ); }
		}

		// Find function matches
		CandidateList found;
		SemanticErrorException errors;
		
		for ( CandidateRef & func : funcFinder ) {
			try {
				PRINT(
					std::cerr << "working on alternative:" << std::endl;
					print( std::cerr, *func, 2 );
				)

				// check if the type is a pointer to function
				const ast::Type * funcResult = func->expr->result->stripReferences();
				if ( auto pointer = dynamic_cast< const ast::PointerType * >( funcResult ) ) {
					if ( auto function = pointer->base.as< ast::FunctionType >() ) {
						// if (!selfFinder.allowVoid && function->returns.empty()) continue;
						CandidateRef newFunc{ new Candidate{ *func } };
						newFunc->expr =
							referenceToRvalueConversion( newFunc->expr, newFunc->cost );
						makeFunctionCandidates( untypedExpr->location,
							newFunc, function, argExpansions, found );
					}
				} else if (
					auto inst = dynamic_cast< const ast::TypeInstType * >( funcResult )
				) {
					if ( const ast::EqvClass * clz = func->env.lookup( *inst ) ) {
						if ( auto function = clz->bound.as< ast::FunctionType >() ) {
							CandidateRef newFunc( new Candidate( *func ) );
							newFunc->expr =
								referenceToRvalueConversion( newFunc->expr, newFunc->cost );
							makeFunctionCandidates( untypedExpr->location,
								newFunc, function, argExpansions, found );
						}
					}
				}
			} catch ( SemanticErrorException & e ) { errors.append( e ); }
		}

		// Find matches on function operators `?()`
		if ( ! opFinder.candidates.empty() ) {
			// add exploded function alternatives to front of argument list
			std::vector< ExplodedArg > funcE;
			funcE.reserve( funcFinder.candidates.size() );
			for ( const CandidateRef & func : funcFinder ) {
				funcE.emplace_back( *func, symtab );
			}
			argExpansions.emplace_front( std::move( funcE ) );

			for ( const CandidateRef & op : opFinder ) {
				try {
					// check if type is pointer-to-function
					const ast::Type * opResult = op->expr->result->stripReferences();
					if ( auto pointer = dynamic_cast< const ast::PointerType * >( opResult ) ) {
						if ( auto function = pointer->base.as< ast::FunctionType >() ) {
							CandidateRef newOp{ new Candidate{ *op} };
							newOp->expr =
								referenceToRvalueConversion( newOp->expr, newOp->cost );
							makeFunctionCandidates( untypedExpr->location,
								newOp, function, argExpansions, found );
						}
					}
				} catch ( SemanticErrorException & e ) { errors.append( e ); }
			}
		}

		// Implement SFINAE; resolution errors are only errors if there aren't any non-error
		// candidates
		if ( found.empty() ) errors.throwIfNonEmpty();

		// only keep the best matching intrinsic result to match C semantics (no unexpected narrowing/widening)
		// TODO: keep one for each set of argument candidates?
		Cost intrinsicCost = Cost::infinity;
		CandidateList intrinsicResult;

		// Compute conversion costs
		for ( CandidateRef & withFunc : found ) {
			Cost cvtCost = computeApplicationConversionCost( withFunc, symtab );

			if (funcName == "?|?") {
			PRINT(
				auto appExpr = withFunc->expr.strict_as< ast::ApplicationExpr >();
				auto pointer = appExpr->func->result.strict_as< ast::PointerType >();
				auto function = pointer->base.strict_as< ast::FunctionType >();

				std::cerr << "Case +++++++++++++ " << appExpr->func << std::endl;
				std::cerr << "parameters are:" << std::endl;
				ast::printAll( std::cerr, function->params, 2 );
				std::cerr << "arguments are:" << std::endl;
				ast::printAll( std::cerr, appExpr->args, 2 );
				std::cerr << "bindings are:" << std::endl;
				ast::print( std::cerr, withFunc->env, 2 );
				std::cerr << "cost is: " << withFunc->cost << std::endl;
				std::cerr << "cost of conversion is:" << cvtCost << std::endl;
			)
			}
			if ( cvtCost != Cost::infinity ) {
				withFunc->cvtCost = cvtCost;
				withFunc->cost += cvtCost;
				auto func = withFunc->expr.strict_as<ast::ApplicationExpr>()->func.as<ast::VariableExpr>();
				if (func && func->var->linkage == ast::Linkage::Intrinsic) {
					if (withFunc->cost < intrinsicCost) {
						intrinsicResult.clear();
						intrinsicCost = withFunc->cost;
					}
					if (withFunc->cost == intrinsicCost) {
						intrinsicResult.emplace_back(std::move(withFunc));
					}
				} else {
					candidates.emplace_back( std::move( withFunc ) );
				}
			}
		}
		spliceBegin( candidates, intrinsicResult );
		found = std::move( candidates );

		// use a new list so that candidates are not examined by addAnonConversions twice
		// CandidateList winners = findMinCost( found );
		// promoteCvtCost( winners );

		// function may return a struct/union value, in which case we need to add candidates
		// for implicit conversions to each of the anonymous members, which must happen after
		// `findMinCost`, since anon conversions are never the cheapest
		for ( const CandidateRef & c : found ) {
			addAnonConversions( c );
		}
		// would this be too slow when we don't check cost anymore?
		spliceBegin( candidates, found );

		if ( candidates.empty() && targetType && ! targetType->isVoid() && !selfFinder.strictMode ) {
			// If resolution is unsuccessful with a target type, try again without, since it
			// will sometimes succeed when it wouldn't with a target type binding.
			// For example:
			//   forall( otype T ) T & ?[]( T *, ptrdiff_t );
			//   const char * x = "hello world";
			//   unsigned char ch = x[0];
			// Fails with simple return type binding (xxx -- check this!) as follows:
			// * T is bound to unsigned char
			// * (x: const char *) is unified with unsigned char *, which fails
			// xxx -- fix this better
			targetType = nullptr;
			postvisit( untypedExpr );
		}
	}

	void Finder::postvisit( const ast::AddressExpr * addressExpr ) {
		CandidateFinder finder( context, tenv );
		finder.find( addressExpr->arg );

		if ( finder.candidates.empty() ) return;

		reason.code = NoMatch;

		for ( CandidateRef & r : finder.candidates ) {
			if ( !isLvalue( r->expr ) ) continue;
			addCandidate( *r, new ast::AddressExpr{ addressExpr->location, r->expr } );
		}
	}

	void Finder::postvisit( const ast::LabelAddressExpr * labelExpr ) {
		addCandidate( labelExpr, tenv );
	}

	void Finder::postvisit( const ast::CastExpr * castExpr ) {
		ast::ptr< ast::Type > toType = castExpr->result;
		assert( toType );
		toType = resolveTypeof( toType, context );
		toType = adjustExprType( toType, tenv, symtab );

		CandidateFinder finder( context, tenv, toType );
		if (toType->isVoid()) {
			finder.allowVoid = true;
		}
		if ( castExpr->kind == ast::CastExpr::Return ) {
			finder.strictMode = true;
			finder.find( castExpr->arg, ResolveMode::withAdjustment() );

			// return casts are eliminated (merely selecting an overload, no actual operation)
			candidates = std::move(finder.candidates);
			return;
		}
		else if (toType->isVoid()) {
			finder.find( castExpr->arg ); // no adjust
		}
		else {
			finder.find( castExpr->arg, ResolveMode::withAdjustment() );
		}

		if ( !finder.candidates.empty() ) reason.code = NoMatch;

		CandidateList matches;
		Cost minExprCost = Cost::infinity;
		// Cost minCastCost = Cost::infinity;
		for ( CandidateRef & cand : finder.candidates ) {
			ast::ptr< ast::Type > fromType = cand->expr->result;
			assert( fromType );
			fromType = resolveTypeof( fromType, context );
			fromType = adjustExprType( fromType, tenv, symtab );

			ast::AssertionSet need( cand->need.begin(), cand->need.end() ), have;
			ast::OpenVarSet open( cand->open );

			cand->env.extractOpenVars( open );

			// It is possible that a cast can throw away some values in a multiply-valued
			// expression, e.g. cast-to-void, one value to zero. Figure out the prefix of the
			// subexpression results that are cast directly. The candidate is invalid if it
			// has fewer results than there are types to cast to.
			int discardedValues = fromType->size() - toType->size();
			if ( discardedValues < 0 ) continue;

			// unification run for side-effects
			unify( toType, fromType, cand->env, need, have, open );
			Cost thisCost =
				(castExpr->isGenerated == ast::GeneratedFlag::GeneratedCast)
					? conversionCost( fromType, toType, cand->expr->get_lvalue(), symtab, cand->env )
					: castCost( fromType, toType, cand->expr->get_lvalue(), symtab, cand->env );
			
			// Redefine enum cast
			auto argAsEnum = fromType.as<ast::EnumInstType>();
			auto toAsEnum = toType.as<ast::EnumInstType>();
			if ( argAsEnum && toAsEnum && argAsEnum->name != toAsEnum->name ) {
				CandidateFinder subFinder(context, tenv);
				ast::ptr<ast::Expr> offsetExpr = subFinder.makeEnumOffsetCast(argAsEnum, toAsEnum, cand->expr, thisCost);
				if ( offsetExpr )
					cand->expr = offsetExpr;
			}

			PRINT(
				std::cerr << "working on cast with result: " << toType << std::endl;
				std::cerr << "and expr type: " << fromType << std::endl;
				std::cerr << "env: " << cand->env << std::endl;
			)
			if ( thisCost != Cost::infinity ) {
				PRINT(
					std::cerr << "has finite cost." << std::endl;
				)
				// count one safe conversion for each value that is thrown away
				thisCost.incSafe( discardedValues );

				// See Aaron Moss, page 47; this reasoning does not hold since implicit conversions
				// can create the same resolution issue. The C intrinsic interpretations are pruned
				// immediately for the lowest cost option regardless of result type. Related code in
				// postvisit (UntypedExpr).
				// Cast expression costs are updated now to use the general rules.
				/*
				// select first on argument cost, then conversion cost
				if ( cand->cost < minExprCost || ( cand->cost == minExprCost && thisCost < minCastCost ) ) {
					minExprCost = cand->cost;
					minCastCost = thisCost;
					matches.clear();
				}
				// ambigious case, still output candidates to print in error message
				if ( cand->cost == minExprCost && thisCost == minCastCost ) {
				*/
				cand->cost += thisCost;
				if (cand->cost < minExprCost) {
					minExprCost = cand->cost;
					matches.clear();
				}
				if (cand->cost == minExprCost) {
					CandidateRef newCand = std::make_shared<Candidate>(
						restructureCast( cand->expr, toType, castExpr->isGenerated ),
						copy( cand->env ), std::move( open ), std::move( need ), cand->cost);
					// currently assertions are always resolved immediately so this should have no effect.
					// if this somehow changes in the future (e.g. delayed by indeterminate return type)
					// we may need to revisit the logic.
					inferParameters( newCand, matches );
				}
				// else skip, better alternatives found

			}
		}
		candidates = std::move(matches);
		//CandidateList minArgCost = findMinCost( matches );
		//promoteCvtCost( minArgCost );
		//candidates = findMinCost( minArgCost );
	}

	void Finder::postvisit( const ast::VirtualCastExpr * castExpr ) {
		assertf( castExpr->result, "Implicit virtual cast targets not yet supported." );
		CandidateFinder finder( context, tenv );
		// don't prune here, all alternatives guaranteed to have same type
		finder.find( castExpr->arg, ResolveMode::withoutPrune() );
		for ( CandidateRef & r : finder.candidates ) {
			addCandidate(
				*r,
				new ast::VirtualCastExpr{ castExpr->location, r->expr, castExpr->result } );
		}
	}

	void Finder::postvisit( const ast::KeywordCastExpr * castExpr ) {
		const auto & loc = castExpr->location;
		assertf( castExpr->result, "Cast target should have been set in Validate." );
		auto ref = castExpr->result.strict_as<ast::ReferenceType>();
		auto inst = ref->base.strict_as<ast::StructInstType>();
		auto target = inst->base.get();

		CandidateFinder finder( context, tenv );

		auto pick_alternatives = [target, this](CandidateList & found, bool expect_ref) {
			for (auto & cand : found) {
				const ast::Type * expr = cand->expr->result.get();
				if (expect_ref) {
					auto res = dynamic_cast<const ast::ReferenceType*>(expr);
					if (!res) { continue; }
					expr = res->base.get();
				}

				if (auto insttype = dynamic_cast<const ast::TypeInstType*>(expr)) {
					auto td = cand->env.lookup(*insttype);
					if (!td) { continue; }
					expr = td->bound.get();
				}

				if (auto base = dynamic_cast<const ast::StructInstType*>(expr)) {
					if (base->base == target) {
						candidates.push_back( std::move(cand) );
						reason.code = NoReason;
					}
				}
			}
		};

		try {
			// Attempt 1 : turn (thread&)X into (thread$&)X.__thrd
			// Clone is purely for memory management
			std::unique_ptr<const ast::Expr> tech1 { new ast::UntypedMemberExpr(loc, new ast::NameExpr(loc, castExpr->concrete_target.field), castExpr->arg) };

			// don't prune here, since it's guaranteed all alternatives will have the same type
			finder.find( tech1.get(), ResolveMode::withoutPrune() );
			pick_alternatives(finder.candidates, false);

			return;
		} catch(SemanticErrorException & ) {}

		// Fallback : turn (thread&)X into (thread$&)get_thread(X)
		std::unique_ptr<const ast::Expr> fallback { ast::UntypedExpr::createDeref(loc,  new ast::UntypedExpr(loc, new ast::NameExpr(loc, castExpr->concrete_target.getter), { castExpr->arg })) };
		// don't prune here, since it's guaranteed all alternatives will have the same type
		finder.find( fallback.get(), ResolveMode::withoutPrune() );

		pick_alternatives(finder.candidates, true);

		// Whatever happens here, we have no more fallbacks
	}

	void Finder::postvisit( const ast::UntypedMemberExpr * memberExpr ) {
		CandidateFinder aggFinder( context, tenv );
		aggFinder.find( memberExpr->aggregate, ResolveMode::withAdjustment() );
		for ( CandidateRef & agg : aggFinder.candidates ) {
			// it's okay for the aggregate expression to have reference type -- cast it to the
			// base type to treat the aggregate as the referenced value
			Cost addedCost = Cost::zero;
			agg->expr = referenceToRvalueConversion( agg->expr, addedCost );

			// find member of the given type
			if ( auto structInst = agg->expr->result.as< ast::StructInstType >() ) {
				addAggMembers(
					structInst, agg->expr, *agg, addedCost, getMemberName( memberExpr ) );
			} else if ( auto unionInst = agg->expr->result.as< ast::UnionInstType >() ) {
				addAggMembers(
					unionInst, agg->expr, *agg, addedCost, getMemberName( memberExpr ) );
			} else if ( auto tupleType = agg->expr->result.as< ast::TupleType >() ) {
				addTupleMembers( tupleType, agg->expr, *agg, addedCost, memberExpr->member );
			}
		}
	}

	void Finder::postvisit( const ast::MemberExpr * memberExpr ) {
		addCandidate( memberExpr, tenv );
	}

	void Finder::postvisit( const ast::NameExpr * nameExpr ) {
		std::vector< ast::SymbolTable::IdData > declList;
		if (!selfFinder.otypeKeys.empty()) {
			auto kind = ast::SymbolTable::getSpecialFunctionKind(nameExpr->name);
			assertf(kind != ast::SymbolTable::SpecialFunctionKind::NUMBER_OF_KINDS, "special lookup with non-special target: %s", nameExpr->name.c_str());

			for (auto & otypeKey: selfFinder.otypeKeys) {
				auto result = symtab.specialLookupId(kind, otypeKey);
				declList.insert(declList.end(), std::make_move_iterator(result.begin()), std::make_move_iterator(result.end()));
			}
		} else {
			declList = symtab.lookupIdIgnoreHidden( nameExpr->name );
		}
		PRINT( std::cerr << "nameExpr is " << nameExpr->name << std::endl; )

		if ( declList.empty() ) return;

		reason.code = NoMatch;

		for ( auto & data : declList ) {
			Cost cost = Cost::zero;
			ast::Expr * newExpr = data.combine( nameExpr->location, cost );

			CandidateRef newCand = std::make_shared<Candidate>(
				newExpr, copy( tenv ), ast::OpenVarSet{}, ast::AssertionSet{}, Cost::zero,
				cost );
			if (newCand->expr->env) {
				newCand->env.add(*newCand->expr->env);
				auto mutExpr = newCand->expr.get_and_mutate();
				mutExpr->env  = nullptr;
				newCand->expr = mutExpr;
			}

			PRINT(
				std::cerr << "decl is ";
				ast::print( std::cerr, data.id );
				std::cerr << std::endl;
				std::cerr << "newExpr is ";
				ast::print( std::cerr, newExpr );
				std::cerr << std::endl;
			)
			newCand->expr = ast::mutate_field(
				newCand->expr.get(), &ast::Expr::result,
				renameTyVars( newCand->expr->result ) );
			// add anonymous member interpretations whenever an aggregate value type is seen
			// as a name expression
			addAnonConversions( newCand );
			candidates.emplace_back( std::move( newCand ) );
		}
	}

	void Finder::postvisit(const ast::VariableExpr *variableExpr) {
		// not sufficient to just pass `variableExpr` here, type might have changed

		auto cand = new Candidate(variableExpr, tenv);
		candidates.emplace_back(cand);
	}

	void Finder::postvisit( const ast::ConstantExpr * constantExpr ) {
		addCandidate( constantExpr, tenv );
	}

	void Finder::postvisit( const ast::SizeofExpr * sizeofExpr ) {
		addCandidate(
			new ast::SizeofExpr{
				sizeofExpr->location, resolveTypeof( sizeofExpr->type, context ) },
			tenv );
	}

	void Finder::postvisit( const ast::CountExpr * countExpr ) {
		const ast::UntypedExpr * untyped = nullptr;
		if ( countExpr->type ) {
			auto enumInst = countExpr->type.as<ast::EnumInstType>();
			if ( enumInst ) {
				addCandidate( ast::ConstantExpr::from_ulong(countExpr->location, enumInst->base->members.size()), tenv );
				return;
			}
			auto untypedFirst = ast::UntypedExpr::createCall( countExpr->location, "lowerBound", {} );
			auto castFirst = new ast::CastExpr( countExpr->location, untypedFirst , countExpr->type );
			untyped = ast::UntypedExpr::createCall(
				countExpr->location, "Countof", { castFirst }
			);
		}
		if (!untyped) untyped = ast::UntypedExpr::createCall(
				countExpr->location, "Countof", { countExpr->expr }
		);
		CandidateFinder finder( context, tenv );
		finder.find( untyped );
		CandidateList winners = findMinCost( finder.candidates );
		if ( winners.size() == 0 ) {
			SemanticError( countExpr->expr, "Countof is not implemented for operand: " );
		}
		if ( winners.size() !=  1 ) {
			SemanticError( countExpr->expr, "Ambiguous expression in countof operand: " );
		}
		CandidateRef & choice = winners.front();
		choice->expr = referenceToRvalueConversion( choice->expr, choice->cost );
		addCandidate( *choice, choice->expr );
	}

	void Finder::postvisit( const ast::AlignofExpr * alignofExpr ) {
		addCandidate(
			new ast::AlignofExpr{
				alignofExpr->location, resolveTypeof( alignofExpr->type, context ) },
			tenv );
	}

	void Finder::postvisit( const ast::UntypedOffsetofExpr * offsetofExpr ) {
		const ast::BaseInstType * aggInst;
		if (( aggInst = offsetofExpr->type.as< ast::StructInstType >() )) ;
		else if (( aggInst = offsetofExpr->type.as< ast::UnionInstType >() )) ;
		else return;

		for ( const ast::Decl * member : aggInst->lookup( offsetofExpr->member ) ) {
			auto dwt = strict_dynamic_cast< const ast::DeclWithType * >( member );
			addCandidate(
				new ast::OffsetofExpr{ offsetofExpr->location, aggInst, dwt }, tenv );
		}
	}

	void Finder::postvisit( const ast::OffsetofExpr * offsetofExpr ) {
		addCandidate( offsetofExpr, tenv );
	}

	void Finder::postvisit( const ast::OffsetPackExpr * offsetPackExpr ) {
		addCandidate( offsetPackExpr, tenv );
	}

	void Finder::postvisit( const ast::LogicalExpr * logicalExpr ) {
		CandidateFinder finder1( context, tenv );
		ast::ptr<ast::Expr> arg1 = createCondExpr( logicalExpr->arg1 );
		finder1.find( arg1, ResolveMode::withAdjustment() );
		if ( finder1.candidates.empty() ) return;

		CandidateFinder finder2( context, tenv );
		ast::ptr<ast::Expr> arg2 = createCondExpr( logicalExpr->arg2 );
		finder2.find( arg2, ResolveMode::withAdjustment() );
		if ( finder2.candidates.empty() ) return;

		reason.code = NoMatch;

		for ( const CandidateRef & r1 : finder1.candidates ) {
			for ( const CandidateRef & r2 : finder2.candidates ) {
				ast::TypeEnvironment env{ r1->env };
				env.simpleCombine( r2->env );
				ast::OpenVarSet open{ r1->open };
				mergeOpenVars( open, r2->open );
				ast::AssertionSet need;
				mergeAssertionSet( need, r1->need );
				mergeAssertionSet( need, r2->need );

				addCandidate(
					new ast::LogicalExpr{
						logicalExpr->location, r1->expr, r2->expr, logicalExpr->isAnd },
					std::move( env ), std::move( open ), std::move( need ), r1->cost + r2->cost );
			}
		}
	}

	void Finder::postvisit( const ast::ConditionalExpr * conditionalExpr ) {
		// candidates for condition
		ast::ptr<ast::Expr> arg1 = createCondExpr( conditionalExpr->arg1 );
		CandidateFinder finder1( context, tenv );
		finder1.find( arg1, ResolveMode::withAdjustment() );
		if ( finder1.candidates.empty() ) return;

		// candidates for true result
		// FIX ME: resolves and runs arg1 twice when arg2 is missing.
		ast::Expr const * arg2 = conditionalExpr->arg2;
		arg2 = arg2 ? arg2 : conditionalExpr->arg1.get();
		CandidateFinder finder2( context, tenv );
		finder2.allowVoid = true;
		finder2.find( arg2, ResolveMode::withAdjustment() );
		if ( finder2.candidates.empty() ) return;

		// candidates for false result
		CandidateFinder finder3( context, tenv );
		finder3.allowVoid = true;
		finder3.find( conditionalExpr->arg3, ResolveMode::withAdjustment() );
		if ( finder3.candidates.empty() ) return;

		reason.code = NoMatch;

		for ( const CandidateRef & r1 : finder1.candidates ) {
			for ( const CandidateRef & r2 : finder2.candidates ) {
				for ( const CandidateRef & r3 : finder3.candidates ) {
					ast::TypeEnvironment env{ r1->env };
					env.simpleCombine( r2->env );
					env.simpleCombine( r3->env );
					ast::OpenVarSet open{ r1->open };
					mergeOpenVars( open, r2->open );
					mergeOpenVars( open, r3->open );
					ast::AssertionSet need;
					mergeAssertionSet( need, r1->need );
					mergeAssertionSet( need, r2->need );
					mergeAssertionSet( need, r3->need );
					ast::AssertionSet have;

					// unify true and false results, then infer parameters to produce new
					// candidates
					ast::ptr< ast::Type > common;
					if (
						unify(
							r2->expr->result, r3->expr->result, env, need, have, open,
							common )
					) {
						// generate typed expression
						ast::ConditionalExpr * newExpr = new ast::ConditionalExpr{
							conditionalExpr->location, r1->expr, r2->expr, r3->expr };
						newExpr->result = common ? common : r2->expr->result;
						// convert both options to result type
						Cost cost = r1->cost + r2->cost + r3->cost;
						newExpr->arg2 = computeExpressionConversionCost(
							newExpr->arg2, newExpr->result, symtab, env, cost );
						newExpr->arg3 = computeExpressionConversionCost(
							newExpr->arg3, newExpr->result, symtab, env, cost );
						// output candidate
						CandidateRef newCand = std::make_shared<Candidate>(
							newExpr, std::move( env ), std::move( open ), std::move( need ), cost );
						inferParameters( newCand, candidates );
					}
				}
			}
		}
	}

	void Finder::postvisit( const ast::CommaExpr * commaExpr ) {
		ast::TypeEnvironment env{ tenv };
		ast::ptr< ast::Expr > arg1 = resolveInVoidContext( commaExpr->arg1, context, env );

		CandidateFinder finder2( context, env );
		finder2.find( commaExpr->arg2, ResolveMode::withAdjustment() );

		for ( const CandidateRef & r2 : finder2.candidates ) {
			addCandidate( *r2, new ast::CommaExpr{ commaExpr->location, arg1, r2->expr } );
		}
	}

	void Finder::postvisit( const ast::ImplicitCopyCtorExpr * ctorExpr ) {
		addCandidate( ctorExpr, tenv );
	}

	void Finder::postvisit( const ast::ConstructorExpr * ctorExpr ) {
		CandidateFinder finder( context, tenv );
		finder.allowVoid = true;
		finder.find( ctorExpr->callExpr, ResolveMode::withoutPrune() );
		for ( CandidateRef & r : finder.candidates ) {
			addCandidate( *r, new ast::ConstructorExpr{ ctorExpr->location, r->expr } );
		}
	}

	void Finder::postvisit( const ast::RangeExpr * rangeExpr ) {
		// resolve low and high, accept candidates where low and high types unify
		CandidateFinder finder1( context, tenv );
		finder1.find( rangeExpr->low, ResolveMode::withAdjustment() );
		if ( finder1.candidates.empty() ) return;

		CandidateFinder finder2( context, tenv );
		finder2.find( rangeExpr->high, ResolveMode::withAdjustment() );
		if ( finder2.candidates.empty() ) return;

		reason.code = NoMatch;

		for ( const CandidateRef & r1 : finder1.candidates ) {
			for ( const CandidateRef & r2 : finder2.candidates ) {
				ast::TypeEnvironment env{ r1->env };
				env.simpleCombine( r2->env );
				ast::OpenVarSet open{ r1->open };
				mergeOpenVars( open, r2->open );
				ast::AssertionSet need;
				mergeAssertionSet( need, r1->need );
				mergeAssertionSet( need, r2->need );
				ast::AssertionSet have;

				ast::ptr< ast::Type > common;
				if (
					unify(
						r1->expr->result, r2->expr->result, env, need, have, open,
						common )
				) {
					// generate new expression
					ast::RangeExpr * newExpr =
						new ast::RangeExpr{ rangeExpr->location, r1->expr, r2->expr };
					newExpr->result = common ? common : r1->expr->result;
					// add candidate
					CandidateRef newCand = std::make_shared<Candidate>(
						newExpr, std::move( env ), std::move( open ), std::move( need ),
						r1->cost + r2->cost );
					inferParameters( newCand, candidates );
				}
			}
		}
	}

	void Finder::postvisit( const ast::UntypedTupleExpr * tupleExpr ) {
		std::vector< CandidateFinder > subCandidates =
			selfFinder.findSubExprs( tupleExpr->exprs );
		std::vector< CandidateList > possibilities;
		combos( subCandidates.begin(), subCandidates.end(), back_inserter( possibilities ) );

		for ( const CandidateList & subs : possibilities ) {
			std::vector< ast::ptr< ast::Expr > > exprs;
			exprs.reserve( subs.size() );
			for ( const CandidateRef & sub : subs ) { exprs.emplace_back( sub->expr ); }

			ast::TypeEnvironment env;
			ast::OpenVarSet open;
			ast::AssertionSet need;
			for ( const CandidateRef & sub : subs ) {
				env.simpleCombine( sub->env );
				mergeOpenVars( open, sub->open );
				mergeAssertionSet( need, sub->need );
			}

			addCandidate(
				new ast::TupleExpr{ tupleExpr->location, std::move( exprs ) },
				std::move( env ), std::move( open ), std::move( need ), sumCost( subs ) );
		}
	}

	void Finder::postvisit( const ast::TupleExpr * tupleExpr ) {
		addCandidate( tupleExpr, tenv );
	}

	void Finder::postvisit( const ast::TupleIndexExpr * tupleExpr ) {
		addCandidate( tupleExpr, tenv );
	}

	void Finder::postvisit( const ast::TupleAssignExpr * tupleExpr ) {
		addCandidate( tupleExpr, tenv );
	}

	void Finder::postvisit( const ast::UniqueExpr * unqExpr ) {
		CandidateFinder finder( context, tenv );
		finder.find( unqExpr->expr, ResolveMode::withAdjustment() );
		for ( CandidateRef & r : finder.candidates ) {
			// ensure that the the id is passed on so that the expressions are "linked"
			addCandidate( *r, new ast::UniqueExpr{ unqExpr->location, r->expr, unqExpr->id } );
		}
	}

	void Finder::postvisit( const ast::StmtExpr * stmtExpr ) {
		addCandidate( resolveStmtExpr( stmtExpr, context ), tenv );
	}

	void Finder::postvisit( const ast::UntypedInitExpr * initExpr ) {
		// handle each option like a cast
		CandidateList matches;
		PRINT(
			std::cerr << "untyped init expr: " << initExpr << std::endl;
		)
		// O(n^2) checks of d-types with e-types
		for ( const ast::InitAlternative & initAlt : initExpr->initAlts ) {
			// calculate target type
			const ast::Type * toType = resolveTypeof( initAlt.type, context );
			toType = adjustExprType( toType, tenv, symtab );
			// The call to find must occur inside this loop, otherwise polymorphic return
			// types are not bound to the initialization type, since return type variables are
			// only open for the duration of resolving the UntypedExpr.
			CandidateFinder finder( context, tenv, toType );
			finder.find( initExpr->expr, ResolveMode::withAdjustment() );

			Cost minExprCost = Cost::infinity;
			Cost minCastCost = Cost::infinity;
			for ( CandidateRef & cand : finder.candidates ) {
				if (reason.code == NotFound) reason.code = NoMatch;

				ast::TypeEnvironment env{ cand->env };
				ast::AssertionSet need( cand->need.begin(), cand->need.end() ), have;
				ast::OpenVarSet open{ cand->open };

				PRINT(
					std::cerr << "  @ " << toType << " " << initAlt.designation << std::endl;
				)

				// It is possible that a cast can throw away some values in a multiply-valued
				// expression, e.g. cast-to-void, one value to zero. Figure out the prefix of
				// the subexpression results that are cast directly. The candidate is invalid
				// if it has fewer results than there are types to cast to.
				int discardedValues = cand->expr->result->size() - toType->size();
				if ( discardedValues < 0 ) continue;

				// unification run for side-effects
				ast::ptr<ast::Type> common;
				bool canUnify = unify( toType, cand->expr->result, env, need, have, open, common );
				(void) canUnify;
				Cost thisCost = computeConversionCost( cand->expr->result, toType, cand->expr->get_lvalue(),
					symtab, env );
				PRINT(
					Cost legacyCost = castCost( cand->expr->result, toType, cand->expr->get_lvalue(),
						symtab, env );
					std::cerr << "Considering initialization:";
					std::cerr << std::endl << "  FROM: " << cand->expr->result << std::endl;
					std::cerr << std::endl << "  TO: "   << toType             << std::endl;
					std::cerr << std::endl << "  Unification " << (canUnify ? "succeeded" : "failed");
					std::cerr << std::endl << "  Legacy cost " << legacyCost;
					std::cerr << std::endl << "  New cost " << thisCost;
					std::cerr << std::endl;
				)
				if ( thisCost != Cost::infinity ) {
					// count one safe conversion for each value that is thrown away
					thisCost.incSafe( discardedValues );
					if ( cand->cost < minExprCost || ( cand->cost == minExprCost && thisCost < minCastCost ) ) {
						minExprCost = cand->cost;
						minCastCost = thisCost;
						matches.clear();
					}
					CandidateRef newCand = std::make_shared<Candidate>(
						new ast::InitExpr{
							initExpr->location,
							restructureCast( cand->expr, toType ),
							initAlt.designation },
						std::move(env), std::move( open ), std::move( need ), cand->cost + thisCost );
					// currently assertions are always resolved immediately so this should have no effect.
					// if this somehow changes in the future (e.g. delayed by indeterminate return type)
					// we may need to revisit the logic.
					inferParameters( newCand, matches );
				}
			}
		}

		// select first on argument cost, then conversion cost
		// CandidateList minArgCost = findMinCost( matches );
		// promoteCvtCost( minArgCost );
		// candidates = findMinCost( minArgCost );
		candidates = std::move(matches);
	}

	void Finder::postvisit( const ast::QualifiedNameExpr * expr ) {
		std::vector< ast::SymbolTable::IdData > declList = symtab.lookupId( expr->name );
		if ( declList.empty() ) return;

		for ( ast::SymbolTable::IdData & data: declList ) {
			const ast::Type * t = data.id->get_type()->stripReferences();
			if ( const ast::EnumInstType * enumInstType =
				dynamic_cast<const ast::EnumInstType *>( t ) ) {
				if ( (enumInstType->base->name == expr->type_name)
					|| (expr->type_decl && enumInstType->base->name == expr->type_decl->name) ) {
					Cost cost = Cost::zero;
					ast::Expr * newExpr = data.combine( expr->location, cost );
					CandidateRef newCand =
						std::make_shared<Candidate>(
							newExpr, copy( tenv ), ast::OpenVarSet{},
							ast::AssertionSet{}, Cost::zero, cost
						);
					if (newCand->expr->env) {
						newCand->env.add(*newCand->expr->env);
						auto mutExpr = newCand->expr.get_and_mutate();
						mutExpr->env  = nullptr;
						newCand->expr = mutExpr;
					}

					newCand->expr = ast::mutate_field(
						newCand->expr.get(), &ast::Expr::result,
						renameTyVars( newCand->expr->result ) );
					addAnonConversions( newCand );
					candidates.emplace_back( std::move( newCand ) );
				}
			}
		}
	}
	// size_t Finder::traceId = Stats::Heap::new_stacktrace_id("Finder");
	/// Prunes a list of candidates down to those that have the minimum conversion cost for a given
	/// return type. Skips ambiguous candidates.

} // anonymous namespace

bool CandidateFinder::pruneCandidates( CandidateList & candidates, CandidateList & out, std::vector<std::string> & errors ) {
	struct PruneStruct {
		CandidateRef candidate;
		bool ambiguous;

		PruneStruct() = default;
		PruneStruct( const CandidateRef & c ) : candidate( c ), ambiguous( false ) {}
	};

	// find lowest-cost candidate for each type
	std::unordered_map< std::string, PruneStruct > selected;
	// attempt to skip satisfyAssertions on more expensive alternatives if better options have been found
	std::sort(candidates.begin(), candidates.end(), [](const CandidateRef & x, const CandidateRef & y){return x->cost < y->cost;});
	for ( CandidateRef & candidate : candidates ) {
		std::string mangleName;
		{
			ast::ptr< ast::Type > newType = candidate->expr->result;
			assertf(candidate->expr->result, "Result of expression %p for candidate is null", candidate->expr.get());
			candidate->env.apply( newType );
			mangleName = Mangle::mangle( newType );
		}

		auto found = selected.find( mangleName );
		if (found != selected.end() && found->second.candidate->cost < candidate->cost) {
			PRINT(
				std::cerr << "cost " << candidate->cost << " loses to "
					<< found->second.candidate->cost << std::endl;
			)
			continue;
		}

		// xxx - when do satisfyAssertions produce more than 1 result?
		// this should only happen when initial result type contains
		// unbound type parameters, then it should never be pruned by
		// the previous step, since renameTyVars guarantees the mangled name
		// is unique.
		CandidateList satisfied;
		bool needRecomputeKey = false;
		if (candidate->need.empty()) {
			satisfied.emplace_back(candidate);
		}
		else {
			satisfyAssertions(candidate, context.symtab, satisfied, errors);
			needRecomputeKey = true;
		}

		for (auto & newCand : satisfied) {
			// recomputes type key, if satisfyAssertions changed it
			if (needRecomputeKey)
			{
				ast::ptr< ast::Type > newType = newCand->expr->result;
				assertf(newCand->expr->result, "Result of expression %p for candidate is null", newCand->expr.get());
				newCand->env.apply( newType );
				mangleName = Mangle::mangle( newType );
			}
			auto found = selected.find( mangleName );
			if ( found != selected.end() ) {
				// tiebreaking by picking the lower cost on CURRENT expression
				// NOTE: this behavior is different from C semantics.
				// Specific remediations are performed for C operators at postvisit(UntypedExpr).
				// Further investigations may take place.
				if ( newCand->cost < found->second.candidate->cost
					|| (newCand->cost == found->second.candidate->cost && newCand->cvtCost < found->second.candidate->cvtCost) ) {
					PRINT(
						std::cerr << "cost " << newCand->cost << " beats "
							<< found->second.candidate->cost << std::endl;
					)

					found->second = PruneStruct{ newCand };
				} else if ( newCand->cost == found->second.candidate->cost && newCand->cvtCost == found->second.candidate->cvtCost ) {
					// if one of the candidates contains a deleted identifier, can pick the other,
					// since deleted expressions should not be ambiguous if there is another option
					// that is at least as good
					if ( findDeletedExpr( newCand->expr ) ) {
						// do nothing
						PRINT( std::cerr << "candidate is deleted" << std::endl; )
					} else if ( findDeletedExpr( found->second.candidate->expr ) ) {
						PRINT( std::cerr << "current is deleted" << std::endl; )
						found->second = PruneStruct{ newCand };
					} else {
						PRINT( std::cerr << "marking ambiguous" << std::endl; )
						found->second.ambiguous = true;
					}
				} else {
					// xxx - can satisfyAssertions increase the cost?
					PRINT(
						std::cerr << "cost " << newCand->cost << " loses to "
							<< found->second.candidate->cost << std::endl;
					)
				}
			} else {
				selected.emplace_hint( found, mangleName, newCand );
			}
		}
	}

	// report unambiguous min-cost candidates
	// CandidateList out;
	for ( auto & target : selected ) {
		if ( target.second.ambiguous ) continue;

		CandidateRef cand = target.second.candidate;

		ast::ptr< ast::Type > newResult = cand->expr->result;
		cand->env.applyFree( newResult );
		cand->expr = ast::mutate_field(
			cand->expr.get(), &ast::Expr::result, std::move( newResult ) );

		out.emplace_back( cand );
	}
	// if everything is lost in satisfyAssertions, report the error
	return !selected.empty();
}

void CandidateFinder::find( const ast::Expr * expr, ResolveMode mode ) {
	// Find alternatives for expression
	ast::Pass<Finder> finder{ *this };
	expr->accept( finder );

	if ( mode.failFast && candidates.empty() ) {
		switch(finder.core.reason.code) {
		case Finder::NotFound:
			{ SemanticError( expr, "No alternatives for expression " ); break; }
		case Finder::NoMatch:
			{ SemanticError( expr, "Invalid application of existing declaration(s) in expression " ); break; }
		case Finder::ArgsToFew:
		case Finder::ArgsToMany:
		case Finder::RetsToFew:
		case Finder::RetsToMany:
		case Finder::NoReason:
		default:
			{ SemanticError( expr->location, "No reasonable alternatives for expression : reasons unkown" ); }
		}
	}

	/*
	if ( mode.satisfyAssns || mode.prune ) {
		// trim candidates to just those where the assertions are satisfiable
		// - necessary pre-requisite to pruning
		CandidateList satisfied;
		std::vector< std::string > errors;
		for ( CandidateRef & candidate : candidates ) {
			satisfyAssertions( candidate, localSyms, satisfied, errors );
		}

		// fail early if none such
		if ( mode.failFast && satisfied.empty() ) {
			std::ostringstream stream;
			stream << "No alternatives with satisfiable assertions for " << expr << "\n";
			for ( const auto& err : errors ) {
				stream << err;
			}
			SemanticError( expr->location, stream.str() );
		}

		// reset candidates
		candidates = move( satisfied );
	}
	*/

	// optimization: don't prune for NameExpr since it never has cost
	if ( mode.prune && !dynamic_cast<const ast::NameExpr *>(expr) ) {
		// trim candidates to single best one
		PRINT(
			std::cerr << "alternatives before prune:" << std::endl;
			print( std::cerr, candidates );
		)

		CandidateList pruned;
		std::vector<std::string> errors;
		bool found = pruneCandidates( candidates, pruned, errors );

		if ( mode.failFast && pruned.empty() ) {
			std::ostringstream stream;
			if (found) {
				CandidateList winners = findMinCost( candidates );
				stream << "Cannot choose between " << winners.size() << " alternatives for "
					"expression\n";
				ast::print( stream, expr );
				stream << " Alternatives are:\n";
				print( stream, winners, 1 );
				SemanticError( expr->location, stream.str() );
			}
			else {
				stream << "No alternatives with satisfiable assertions for " << expr << "\n";
				for ( const auto& err : errors ) {
					stream << err;
				}
				SemanticError( expr->location, stream.str() );
			}
		}

		auto oldsize = candidates.size();
		candidates = std::move( pruned );

		PRINT(
			std::cerr << "there are " << oldsize << " alternatives before elimination" << std::endl;
		)
		PRINT(
			std::cerr << "there are " << candidates.size() << " alternatives after elimination"
				<< std::endl;
		)
	}

	// adjust types after pruning so that types substituted by pruneAlternatives are correctly
	// adjusted
	if ( mode.adjust ) {
		for ( CandidateRef & r : candidates ) {
			r->expr = ast::mutate_field(
				r->expr.get(), &ast::Expr::result,
				adjustExprType( r->expr->result, r->env, context.symtab ) );
		}
	}

	// Central location to handle gcc extension keyword, etc. for all expressions
	for ( CandidateRef & r : candidates ) {
		if ( r->expr->extension != expr->extension ) {
			r->expr.get_and_mutate()->extension = expr->extension;
		}
	}
}

std::vector< CandidateFinder > CandidateFinder::findSubExprs(
	const std::vector< ast::ptr< ast::Expr > > & xs
) {
	std::vector< CandidateFinder > out;

	for ( const auto & x : xs ) {
		out.emplace_back( context, env );
		out.back().find( x, ResolveMode::withAdjustment() );

		PRINT(
			std::cerr << "findSubExprs" << std::endl;
			print( std::cerr, out.back().candidates );
		)
	}

	return out;
}

const ast::Expr * referenceToRvalueConversion( const ast::Expr * expr, Cost & cost ) {
	if ( expr->result.as< ast::ReferenceType >() ) {
		// cast away reference from expr
		cost.incReference();
		return new ast::CastExpr{ expr, expr->result->stripReferences() };
	}

	return expr;
}

const ast::Expr * CandidateFinder::makeEnumOffsetCast( const ast::EnumInstType * src, 
	const ast::EnumInstType * dst, const ast::Expr * expr, Cost minCost ) {
	auto srcDecl = src->base;
	auto dstDecl = dst->base;

	if (srcDecl->name == dstDecl->name) return expr;

	for (auto& dstChild: dstDecl->inlinedDecl) {
		Cost c = castCost(src, dstChild, false, context.symtab, env);
		ast::CastExpr * castToDst;
		if (c<minCost) {
			unsigned offset = dstDecl->calChildOffset(dstChild.get());
			if (offset > 0) {
				auto untyped = ast::UntypedExpr::createCall(
					expr->location, 
					"?+?", 
					{ new ast::CastExpr( expr->location,
						expr,
						new ast::BasicType(ast::BasicKind::SignedInt),
						ast::GeneratedFlag::ExplicitCast ),
						ast::ConstantExpr::from_int(expr->location, offset)} );
				CandidateFinder finder(context, env);
				finder.find( untyped );
				CandidateList winners = findMinCost( finder.candidates );
				CandidateRef & choice = winners.front();
				choice->expr = new ast::CastExpr(expr->location, choice->expr, dstChild, ast::GeneratedFlag::ExplicitCast);
				auto destExpr = makeEnumOffsetCast( src, dstChild, choice->expr, minCost );
				if ( !destExpr ) continue;
				castToDst = new ast::CastExpr( destExpr, dst );
			} else {
				castToDst = new ast::CastExpr( expr, dst );
			}
			return castToDst;
		}
	}
	return nullptr;
}

Cost computeConversionCost(
	const ast::Type * argType, const ast::Type * paramType, bool argIsLvalue,
	const ast::SymbolTable & symtab, const ast::TypeEnvironment & env
) {
	PRINT(
		std::cerr << std::endl << "converting ";
		ast::print( std::cerr, argType, 2 );
		std::cerr << std::endl << " to ";
		ast::print( std::cerr, paramType, 2 );
		std::cerr << std::endl << "environment is: ";
		ast::print( std::cerr, env, 2 );
		std::cerr << std::endl;
	)
	Cost convCost = conversionCost( argType, paramType, argIsLvalue, symtab, env );
	PRINT(
		std::cerr << std::endl << "cost is " << convCost << std::endl;
	)
	if ( convCost == Cost::infinity ) return convCost;
	convCost.incPoly( polyCost( paramType, symtab, env ) + polyCost( argType, symtab, env ) );
	PRINT(
		std::cerr << "cost with polycost is " << convCost << std::endl;
	)
	return convCost;
}

const ast::Expr * createCondExpr( const ast::Expr * expr ) {
	assert( expr );
	return new ast::CastExpr( expr->location,
		ast::UntypedExpr::createCall( expr->location,
			"?!=?",
			{
				expr,
				new ast::ConstantExpr( expr->location,
					new ast::ZeroType(), "0", std::make_optional( 0ull )
				),
			}
		),
		new ast::BasicType( ast::BasicKind::SignedInt )
	);
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
