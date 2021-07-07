//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AlternativeFinder.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sat May 16 23:52:08 2015
// Last Modified By : Andrew Beach
// Last Modified On : Thu Aug  8 16:35:00 2019
// Update Count     : 38
//

#include <algorithm>               // for copy
#include <cassert>                 // for strict_dynamic_cast, assert, assertf
#include <cstddef>                 // for size_t
#include <iostream>                // for operator<<, cerr, ostream, endl
#include <iterator>                // for back_insert_iterator, back_inserter
#include <list>                    // for _List_iterator, list, _List_const_...
#include <map>                     // for _Rb_tree_iterator, map, _Rb_tree_c...
#include <memory>                  // for allocator_traits<>::value_type, unique_ptr
#include <utility>                 // for pair
#include <vector>                  // for vector

#include "CompilationState.h"      // for resolvep
#include "Alternative.h"           // for AltList, Alternative
#include "AlternativeFinder.h"
#include "AST/Expr.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/Type.hpp"
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for deleteAll, printAll, CodeLocation
#include "Cost.h"                  // for Cost, Cost::zero, operator<<, Cost...
#include "ExplodedActual.h"        // for ExplodedActual
#include "InitTweak/InitTweak.h"   // for getFunctionName
#include "RenameVars.h"            // for RenameVars, global_renamer
#include "ResolveAssertions.h"     // for resolveAssertions
#include "ResolveTypeof.h"         // for resolveTypeof
#include "Resolver.h"              // for resolveStmtExpr
#include "SymTab/Indexer.h"        // for Indexer
#include "SymTab/Mangler.h"        // for Mangler
#include "SymTab/Validate.h"       // for validateType
#include "SynTree/Constant.h"      // for Constant
#include "SynTree/Declaration.h"   // for DeclarationWithType, TypeDecl, Dec...
#include "SynTree/Expression.h"    // for Expression, CastExpr, NameExpr
#include "SynTree/Initializer.h"   // for SingleInit, operator<<, Designation
#include "SynTree/SynTree.h"       // for UniqueId
#include "SynTree/Type.h"          // for Type, FunctionType, PointerType
#include "Tuples/Explode.h"        // for explode
#include "Tuples/Tuples.h"         // for isTtype, handleTupleAssignment
#include "Unify.h"                 // for unify
#include "typeops.h"               // for adjustExprType, polyCost, castCost

#define PRINT( text ) if ( resolvep ) { text }
//#define DEBUG_COST

namespace ResolvExpr {
	struct AlternativeFinder::Finder : public WithShortCircuiting {
		Finder( AlternativeFinder & altFinder ) : altFinder( altFinder ), indexer( altFinder.indexer ), alternatives( altFinder.alternatives ), env( altFinder.env ), targetType( altFinder.targetType )  {}

		void previsit( BaseSyntaxNode * ) { visit_children = false; }

		void postvisit( ApplicationExpr * applicationExpr );
		void postvisit( UntypedExpr * untypedExpr );
		void postvisit( AddressExpr * addressExpr );
		void postvisit( LabelAddressExpr * labelExpr );
		void postvisit( CastExpr * castExpr );
		void postvisit( VirtualCastExpr * castExpr );
		void postvisit( KeywordCastExpr * castExpr );
		void postvisit( UntypedMemberExpr * memberExpr );
		void postvisit( MemberExpr * memberExpr );
		void postvisit( NameExpr * variableExpr );
		void postvisit( VariableExpr * variableExpr );
		void postvisit( ConstantExpr * constantExpr );
		void postvisit( SizeofExpr * sizeofExpr );
		void postvisit( AlignofExpr * alignofExpr );
		void postvisit( UntypedOffsetofExpr * offsetofExpr );
		void postvisit( OffsetofExpr * offsetofExpr );
		void postvisit( OffsetPackExpr * offsetPackExpr );
		void postvisit( LogicalExpr * logicalExpr );
		void postvisit( ConditionalExpr * conditionalExpr );
		void postvisit( CommaExpr * commaExpr );
		void postvisit( ImplicitCopyCtorExpr  * impCpCtorExpr );
		void postvisit( ConstructorExpr  * ctorExpr );
		void postvisit( RangeExpr  * rangeExpr );
		void postvisit( UntypedTupleExpr * tupleExpr );
		void postvisit( TupleExpr * tupleExpr );
		void postvisit( TupleIndexExpr * tupleExpr );
		void postvisit( TupleAssignExpr * tupleExpr );
		void postvisit( UniqueExpr * unqExpr );
		void postvisit( StmtExpr * stmtExpr );
		void postvisit( UntypedInitExpr * initExpr );
		void postvisit( InitExpr * initExpr );
		void postvisit( DeletedExpr * delExpr );
		void postvisit( GenericExpr * genExpr );

		/// Adds alternatives for anonymous members
		void addAnonConversions( const Alternative & alt );
		/// Adds alternatives for member expressions, given the aggregate, conversion cost for that aggregate, and name of the member
		template< typename StructOrUnionType > void addAggMembers( StructOrUnionType *aggInst, Expression *expr, const Alternative &alt, const Cost &newCost, const std::string & name );
		/// Adds alternatives for member expressions where the left side has tuple type
		void addTupleMembers( TupleType *tupleType, Expression *expr, const Alternative &alt, const Cost &newCost, Expression *member );
		/// Adds alternatives for offsetof expressions, given the base type and name of the member
		template< typename StructOrUnionType > void addOffsetof( StructOrUnionType *aggInst, const std::string &name );
		/// Takes a final result and checks if its assertions can be satisfied
		template<typename OutputIterator>
		void validateFunctionAlternative( const Alternative &func, ArgPack& result, const std::vector<ArgPack>& results, OutputIterator out );
		/// Finds matching alternatives for a function, given a set of arguments
		template<typename OutputIterator>
		void makeFunctionAlternatives( const Alternative &func, FunctionType *funcType, const ExplodedArgs_old& args, OutputIterator out );
		/// Sets up parameter inference for an output alternative
		template< typename OutputIterator >
		void inferParameters( Alternative &newAlt, OutputIterator out );
	private:
		AlternativeFinder & altFinder;
		const SymTab::Indexer &indexer;
		AltList & alternatives;
		const TypeEnvironment &env;
		Type *& targetType;
	};

	Cost sumCost( const AltList &in ) {
		Cost total = Cost::zero;
		for ( AltList::const_iterator i = in.begin(); i != in.end(); ++i ) {
			total += i->cost;
		}
		return total;
	}

	void printAlts( const AltList &list, std::ostream &os, unsigned int indentAmt ) {
		std::vector<std::string> sorted;
		sorted.reserve(list.size());
		for(const auto & c : list) {
			std::stringstream ss;
			c.print( ss, indentAmt );
			sorted.push_back(ss.str());
		}

		std::sort(sorted.begin(), sorted.end());

		for ( const auto & s : sorted ) {
			os << s << std::endl;
		}
	}

	namespace {
		void makeExprList( const AltList &in, std::list< Expression* > &out ) {
			for ( AltList::const_iterator i = in.begin(); i != in.end(); ++i ) {
				out.push_back( i->expr->clone() );
			}
		}

		struct PruneStruct {
			bool isAmbiguous;
			AltList::iterator candidate;
			PruneStruct() {}
			PruneStruct( AltList::iterator candidate ): isAmbiguous( false ), candidate( candidate ) {}
		};

		/// Prunes a list of alternatives down to those that have the minimum conversion cost for a given return type; skips ambiguous interpretations
		template< typename InputIterator, typename OutputIterator >
		void pruneAlternatives( InputIterator begin, InputIterator end, OutputIterator out ) {
			// select the alternatives that have the minimum conversion cost for a particular set of result types
			std::map< std::string, PruneStruct > selected;
			for ( AltList::iterator candidate = begin; candidate != end; ++candidate ) {
				PruneStruct current( candidate );
				std::string mangleName;
				{
					Type * newType = candidate->expr->get_result()->clone();
					candidate->env.apply( newType );
					mangleName = SymTab::Mangler::mangle( newType );
					delete newType;
				}
				std::map< std::string, PruneStruct >::iterator mapPlace = selected.find( mangleName );
				if ( mapPlace != selected.end() ) {
					if ( candidate->cost < mapPlace->second.candidate->cost ) {
						PRINT(
							std::cerr << "cost " << candidate->cost << " beats " << mapPlace->second.candidate->cost << std::endl;
						)
						selected[ mangleName ] = current;
					} else if ( candidate->cost == mapPlace->second.candidate->cost ) {
						// if one of the candidates contains a deleted identifier, can pick the other, since
						// deleted expressions should not be ambiguous if there is another option that is at least as good
						if ( findDeletedExpr( candidate->expr ) ) {
							// do nothing
							PRINT( std::cerr << "candidate is deleted" << std::endl; )
						} else if ( findDeletedExpr( mapPlace->second.candidate->expr ) ) {
							PRINT( std::cerr << "current is deleted" << std::endl; )
							selected[ mangleName ] = current;
						} else {
							PRINT(
								std::cerr << "marking ambiguous" << std::endl;
							)
							mapPlace->second.isAmbiguous = true;
						}
					} else {
						PRINT(
							std::cerr << "cost " << candidate->cost << " loses to " << mapPlace->second.candidate->cost << std::endl;
						)
					}
				} else {
					selected[ mangleName ] = current;
				}
			}

			// accept the alternatives that were unambiguous
			for ( std::map< std::string, PruneStruct >::iterator target = selected.begin(); target != selected.end(); ++target ) {
				if ( ! target->second.isAmbiguous ) {
					Alternative &alt = *target->second.candidate;
					alt.env.applyFree( alt.expr->get_result() );
					*out++ = alt;
				}
			}
		}

		void renameTypes( Expression *expr ) {
			renameTyVars( expr->result );
		}
	} // namespace

	void referenceToRvalueConversion( Expression *& expr, Cost & cost ) {
		if ( dynamic_cast< ReferenceType * >( expr->get_result() ) ) {
			// cast away reference from expr
			expr = new CastExpr( expr, expr->get_result()->stripReferences()->clone() );
			cost.incReference();
		}
	}

	template< typename InputIterator, typename OutputIterator >
	void AlternativeFinder::findSubExprs( InputIterator begin, InputIterator end, OutputIterator out ) {
		while ( begin != end ) {
			AlternativeFinder finder( indexer, env );
			finder.findWithAdjustment( *begin );
			// XXX  either this
			//Designators::fixDesignations( finder, (*begin++)->get_argName() );
			// or XXX this
			begin++;
			PRINT(
				std::cerr << "findSubExprs" << std::endl;
				printAlts( finder.alternatives, std::cerr );
			)
			*out++ = finder;
		}
	}

	AlternativeFinder::AlternativeFinder( const SymTab::Indexer &indexer, const TypeEnvironment &env )
		: indexer( indexer ), env( env ) {
	}

	void AlternativeFinder::find( Expression *expr, ResolvMode mode ) {
		PassVisitor<Finder> finder( *this );
		expr->accept( finder );
		if ( mode.failFast && alternatives.empty() ) {
			PRINT(
				std::cerr << "No reasonable alternatives for expression " << expr << std::endl;
			)
			SemanticError( expr, "No reasonable alternatives for expression " );
		}
		if ( mode.prune ) {
			// trim candidates just to those where the assertions resolve
			// - necessary pre-requisite to pruning
			AltList candidates;
			std::list<std::string> errors;
			for ( unsigned i = 0; i < alternatives.size(); ++i ) {
				resolveAssertions( alternatives[i], indexer, candidates, errors );
			}
			// fail early if none such
			if ( mode.failFast && candidates.empty() ) {
				std::ostringstream stream;
				stream << "No alternatives with satisfiable assertions for " << expr << "\n";
				//        << "Alternatives with failing assertions are:\n";
				// printAlts( alternatives, stream, 1 );
				for ( const auto& err : errors ) {
					stream << err;
				}
				SemanticError( expr->location, stream.str() );
			}
			// reset alternatives
			alternatives = std::move( candidates );
		}
		if ( mode.prune ) {
			auto oldsize = alternatives.size();
			PRINT(
				std::cerr << "alternatives before prune:" << std::endl;
				printAlts( alternatives, std::cerr );
			)
			AltList pruned;
			pruneAlternatives( alternatives.begin(), alternatives.end(), back_inserter( pruned ) );
			if ( mode.failFast && pruned.empty() ) {
				std::ostringstream stream;
				AltList winners;
				findMinCost( alternatives.begin(), alternatives.end(), back_inserter( winners ) );
				stream << "Cannot choose between " << winners.size() << " alternatives for expression\n";
				expr->print( stream );
				stream << " Alternatives are:\n";
				printAlts( winners, stream, 1 );
				SemanticError( expr->location, stream.str() );
			}
			alternatives = move(pruned);
			PRINT(
				std::cerr << "there are " << oldsize << " alternatives before elimination" << std::endl;
			)
			PRINT(
				std::cerr << "there are " << alternatives.size() << " alternatives after elimination" << std::endl;
			)
		}
		// adjust types after pruning so that types substituted by pruneAlternatives are correctly adjusted
		if ( mode.adjust ) {
			for ( Alternative& i : alternatives ) {
				adjustExprType( i.expr->get_result(), i.env, indexer );
			}
		}

		// Central location to handle gcc extension keyword, etc. for all expression types.
		for ( Alternative &iter: alternatives ) {
			iter.expr->set_extension( expr->get_extension() );
			iter.expr->location = expr->location;
		} // for
	}

	void AlternativeFinder::findWithAdjustment( Expression *expr ) {
		find( expr, ResolvMode::withAdjustment() );
	}

	void AlternativeFinder::findWithoutPrune( Expression * expr ) {
		find( expr, ResolvMode::withoutPrune() );
	}

	void AlternativeFinder::maybeFind( Expression * expr ) {
		find( expr, ResolvMode::withoutFailFast() );
	}

	void AlternativeFinder::Finder::addAnonConversions( const Alternative & alt ) {
		// adds anonymous member interpretations whenever an aggregate value type is seen.
		// it's okay for the aggregate expression to have reference type -- cast it to the base type to treat the aggregate as the referenced value
		std::unique_ptr<Expression> aggrExpr( alt.expr->clone() );
		alt.env.apply( aggrExpr->result );
		Type * aggrType = aggrExpr->result;
		if ( dynamic_cast< ReferenceType * >( aggrType ) ) {
			aggrType = aggrType->stripReferences();
			aggrExpr.reset( new CastExpr( aggrExpr.release(), aggrType->clone() ) );
		}

		if ( StructInstType * structInst = dynamic_cast< StructInstType* >( aggrExpr->result ) ) {
			addAggMembers( structInst, aggrExpr.get(), alt, alt.cost+Cost::safe, "" );
		} else if ( UnionInstType * unionInst = dynamic_cast< UnionInstType* >( aggrExpr->result ) ) {
			addAggMembers( unionInst, aggrExpr.get(), alt, alt.cost+Cost::safe, "" );
		} // if
	}

	template< typename StructOrUnionType >
	void AlternativeFinder::Finder::addAggMembers( StructOrUnionType * aggInst, Expression * expr, const Alternative& alt, const Cost &newCost, const std::string & name ) {
		std::list< Declaration* > members;
		aggInst->lookup( name, members );

		for ( Declaration * decl : members ) {
			if ( DeclarationWithType * dwt = dynamic_cast< DeclarationWithType* >( decl ) ) {
				// addAnonAlternatives uses vector::push_back, which invalidates references to existing elements, so
				// can't construct in place and use vector::back
				Alternative newAlt{ alt, new MemberExpr{ dwt, expr->clone() }, newCost };
				renameTypes( newAlt.expr );
				addAnonConversions( newAlt ); // add anonymous member interpretations whenever an aggregate value type is seen as a member expression.
				alternatives.push_back( std::move(newAlt) );
			} else {
				assert( false );
			}
		}
	}

	void AlternativeFinder::Finder::addTupleMembers( TupleType * tupleType, Expression * expr, const Alternative &alt, const Cost &newCost, Expression * member ) {
		if ( ConstantExpr * constantExpr = dynamic_cast< ConstantExpr * >( member ) ) {
			// get the value of the constant expression as an int, must be between 0 and the length of the tuple type to have meaning
			auto val = constantExpr->intValue();
			std::string tmp;
			if ( val >= 0 && (unsigned long long)val < tupleType->size() ) {
				alternatives.push_back( Alternative{
					alt, new TupleIndexExpr( expr->clone(), val ), newCost } );
			} // if
		} // if
	}

	void AlternativeFinder::Finder::postvisit( ApplicationExpr * applicationExpr ) {
		alternatives.push_back( Alternative{ applicationExpr->clone(), env } );
	}

	Cost computeConversionCost( Type * actualType, Type * formalType, bool actualIsLvalue,
			const SymTab::Indexer &indexer, const TypeEnvironment & env ) {
		PRINT(
			std::cerr << std::endl << "converting ";
			actualType->print( std::cerr, 8 );
			std::cerr << std::endl << " to ";
			formalType->print( std::cerr, 8 );
			std::cerr << std::endl << "environment is: ";
			env.print( std::cerr, 8 );
			std::cerr << std::endl;
		)
		Cost convCost = conversionCost( actualType, formalType, actualIsLvalue, indexer, env );
		PRINT(
			std::cerr << std::endl << "cost is " << convCost << std::endl;
		)
		if ( convCost == Cost::infinity ) {
			return convCost;
		}
		convCost.incPoly( polyCost( formalType, env, indexer ) + polyCost( actualType, env, indexer ) );
		PRINT(
			std::cerr << "cost with polycost is " << convCost << std::endl;
		)
		return convCost;
	}

	Cost computeExpressionConversionCost( Expression *& actualExpr, Type * formalType, const SymTab::Indexer &indexer, const TypeEnvironment & env ) {
		Cost convCost = computeConversionCost(
			actualExpr->result, formalType, actualExpr->get_lvalue(), indexer, env );

		// if there is a non-zero conversion cost, ignoring poly cost, then the expression requires conversion.
		// ignore poly cost for now, since this requires resolution of the cast to infer parameters and this
		// does not currently work for the reason stated below.
		Cost tmpCost = convCost;
		tmpCost.incPoly( -tmpCost.get_polyCost() );
		if ( tmpCost != Cost::zero ) {
			Type *newType = formalType->clone();
			env.apply( newType );
			actualExpr = new CastExpr( actualExpr, newType );
			// xxx - SHOULD be able to resolve this cast, but at the moment pointers are not castable to zero_t, but are implicitly convertible. This is clearly
			// inconsistent, once this is fixed it should be possible to resolve the cast.
			// xxx - this isn't working, it appears because type1 (the formal type) is seen as widenable, but it shouldn't be, because this makes the conversion from DT* to DT* since commontype(zero_t, DT*) is DT*, rather than just nothing.

			// AlternativeFinder finder( indexer, env );
			// finder.findWithAdjustment( actualExpr );
			// assertf( finder.get_alternatives().size() > 0, "Somehow castable expression failed to find alternatives." );
			// assertf( finder.get_alternatives().size() == 1, "Somehow got multiple alternatives for known cast expression." );
			// Alternative & alt = finder.get_alternatives().front();
			// delete actualExpr;
			// actualExpr = alt.expr->clone();
		}
		return convCost;
	}

	Cost computeApplicationConversionCost( Alternative &alt, const SymTab::Indexer &indexer ) {
		ApplicationExpr *appExpr = strict_dynamic_cast< ApplicationExpr* >( alt.expr );
		PointerType *pointer = strict_dynamic_cast< PointerType* >( appExpr->function->result );
		FunctionType *function = strict_dynamic_cast< FunctionType* >( pointer->base );

		Cost convCost = Cost::zero;
		std::list< DeclarationWithType* >& formals = function->parameters;
		std::list< DeclarationWithType* >::iterator formal = formals.begin();
		std::list< Expression* >& actuals = appExpr->args;

		for ( Expression*& actualExpr : actuals ) {
			Type * actualType = actualExpr->result;
			PRINT(
				std::cerr << "actual expression:" << std::endl;
				actualExpr->print( std::cerr, 8 );
				std::cerr << "--- results are" << std::endl;
				actualType->print( std::cerr, 8 );
			)
			if ( formal == formals.end() ) {
				if ( function->isVarArgs ) {
					convCost.incUnsafe();
					PRINT( std::cerr << "end of formals with varargs function: inc unsafe: " << convCost << std::endl; ; )
					// convert reference-typed expressions to value-typed expressions
					referenceToRvalueConversion( actualExpr, convCost );
					continue;
				} else {
					return Cost::infinity;
				}
			}
			if ( DefaultArgExpr * def = dynamic_cast< DefaultArgExpr * >( actualExpr ) ) {
				// default arguments should be free - don't include conversion cost.
				// Unwrap them here because they are not relevant to the rest of the system.
				actualExpr = def->expr;
				++formal;
				continue;
			}
			// mark conversion cost to formal and also specialization cost of formal type
			Type * formalType = (*formal)->get_type();
			convCost += computeExpressionConversionCost( actualExpr, formalType, indexer, alt.env );
			convCost.decSpec( specCost( formalType ) );
			++formal; // can't be in for-loop update because of the continue
		}
		if ( formal != formals.end() ) {
			return Cost::infinity;
		}

		// specialization cost of return types can't be accounted for directly, it disables
		// otherwise-identical calls, like this example based on auto-newline in the I/O lib:
		//
		//   forall(otype OS) {
		//     void ?|?(OS&, int);  // with newline
		//     OS&  ?|?(OS&, int);  // no newline, always chosen due to more specialization
		//   }

		// mark type variable and specialization cost of forall clause
		convCost.incVar( function->forall.size() );
		for ( TypeDecl* td : function->forall ) {
			convCost.decSpec( td->assertions.size() );
		}

		return convCost;
	}

	/// Adds type variables to the open variable set and marks their assertions
	void makeUnifiableVars( Type *type, OpenVarSet &unifiableVars, AssertionSet &needAssertions ) {
		for ( Type::ForallList::const_iterator tyvar = type->forall.begin(); tyvar != type->forall.end(); ++tyvar ) {
			unifiableVars[ (*tyvar)->get_name() ] = TypeDecl::Data{ *tyvar };
			for ( std::list< DeclarationWithType* >::iterator assert = (*tyvar)->assertions.begin(); assert != (*tyvar)->assertions.end(); ++assert ) {
				needAssertions[ *assert ].isUsed = true;
			}
		}
	}

	/// Unique identifier for matching expression resolutions to their requesting expression (located in CandidateFinder.cpp)
	extern UniqueId globalResnSlot;

	template< typename OutputIterator >
	void AlternativeFinder::Finder::inferParameters( Alternative &newAlt, OutputIterator out ) {
		// Set need bindings for any unbound assertions
		UniqueId crntResnSlot = 0;  // matching ID for this expression's assertions
		for ( auto& assn : newAlt.need ) {
			// skip already-matched assertions
			if ( assn.info.resnSlot != 0 ) continue;
			// assign slot for expression if needed
			if ( crntResnSlot == 0 ) { crntResnSlot = ++globalResnSlot; }
			// fix slot to assertion
			assn.info.resnSlot = crntResnSlot;
		}
		// pair slot to expression
		if ( crntResnSlot != 0 ) { newAlt.expr->resnSlots.push_back( crntResnSlot ); }

		// add to output list, assertion resolution is deferred
		*out++ = newAlt;
	}

	/// Gets a default value from an initializer, nullptr if not present
	ConstantExpr* getDefaultValue( Initializer* init ) {
		if ( SingleInit* si = dynamic_cast<SingleInit*>( init ) ) {
			if ( CastExpr* ce = dynamic_cast<CastExpr*>( si->value ) ) {
				return dynamic_cast<ConstantExpr*>( ce->arg );
			} else {
				return dynamic_cast<ConstantExpr*>( si->value );
			}
		}
		return nullptr;
	}

	/// State to iteratively build a match of parameter expressions to arguments
	struct ArgPack {
		std::size_t parent;                ///< Index of parent pack
		std::unique_ptr<Expression> expr;  ///< The argument stored here
		Cost cost;                         ///< The cost of this argument
		TypeEnvironment env;               ///< Environment for this pack
		AssertionSet need;                 ///< Assertions outstanding for this pack
		AssertionSet have;                 ///< Assertions found for this pack
		OpenVarSet openVars;               ///< Open variables for this pack
		unsigned nextArg;                  ///< Index of next argument in arguments list
		unsigned tupleStart;               ///< Number of tuples that start at this index
		unsigned nextExpl;                 ///< Index of next exploded element
		unsigned explAlt;                  ///< Index of alternative for nextExpl > 0

		ArgPack()
			: parent(0), expr(), cost(Cost::zero), env(), need(), have(), openVars(), nextArg(0),
			  tupleStart(0), nextExpl(0), explAlt(0) {}

		ArgPack(const TypeEnvironment& env, const AssertionSet& need, const AssertionSet& have,
				const OpenVarSet& openVars)
			: parent(0), expr(), cost(Cost::zero), env(env), need(need), have(have),
			  openVars(openVars), nextArg(0), tupleStart(0), nextExpl(0), explAlt(0) {}

		ArgPack(std::size_t parent, Expression* expr, TypeEnvironment&& env, AssertionSet&& need,
				AssertionSet&& have, OpenVarSet&& openVars, unsigned nextArg,
				unsigned tupleStart = 0, Cost cost = Cost::zero, unsigned nextExpl = 0,
				unsigned explAlt = 0 )
			: parent(parent), expr(expr->clone()), cost(cost), env(move(env)), need(move(need)),
			  have(move(have)), openVars(move(openVars)), nextArg(nextArg), tupleStart(tupleStart),
			  nextExpl(nextExpl), explAlt(explAlt) {}

		ArgPack(const ArgPack& o, TypeEnvironment&& env, AssertionSet&& need, AssertionSet&& have,
				OpenVarSet&& openVars, unsigned nextArg, Cost added )
			: parent(o.parent), expr(o.expr ? o.expr->clone() : nullptr), cost(o.cost + added),
			  env(move(env)), need(move(need)), have(move(have)), openVars(move(openVars)),
			  nextArg(nextArg), tupleStart(o.tupleStart), nextExpl(0), explAlt(0) {}

		/// true iff this pack is in the middle of an exploded argument
		bool hasExpl() const { return nextExpl > 0; }

		/// Gets the list of exploded alternatives for this pack
		const ExplodedActual& getExpl( const ExplodedArgs_old& args ) const {
			return args[nextArg-1][explAlt];
		}

		/// Ends a tuple expression, consolidating the appropriate actuals
		void endTuple( const std::vector<ArgPack>& packs ) {
			// add all expressions in tuple to list, summing cost
			std::list<Expression*> exprs;
			const ArgPack* pack = this;
			if ( expr ) { exprs.push_front( expr.release() ); }
			while ( pack->tupleStart == 0 ) {
				pack = &packs[pack->parent];
				exprs.push_front( pack->expr->clone() );
				cost += pack->cost;
			}
			// reset pack to appropriate tuple
			expr.reset( new TupleExpr( exprs ) );
			tupleStart = pack->tupleStart - 1;
			parent = pack->parent;
		}
	};

	/// Instantiates an argument to match a formal, returns false if no results left
	bool instantiateArgument( Type* formalType, Initializer* initializer,
			const ExplodedArgs_old& args, std::vector<ArgPack>& results, std::size_t& genStart,
			const SymTab::Indexer& indexer, unsigned nTuples = 0 ) {
		if ( TupleType * tupleType = dynamic_cast<TupleType*>( formalType ) ) {
			// formalType is a TupleType - group actuals into a TupleExpr
			++nTuples;
			for ( Type* type : *tupleType ) {
				// xxx - dropping initializer changes behaviour from previous, but seems correct
				// ^^^ need to handle the case where a tuple has a default argument
				if ( ! instantiateArgument(
						type, nullptr, args, results, genStart, indexer, nTuples ) )
					return false;
				nTuples = 0;
			}
			// re-consititute tuples for final generation
			for ( auto i = genStart; i < results.size(); ++i ) {
				results[i].endTuple( results );
			}
			return true;
		} else if ( TypeInstType * ttype = Tuples::isTtype( formalType ) ) {
			// formalType is a ttype, consumes all remaining arguments
			// xxx - mixing default arguments with variadic??

			// completed tuples; will be spliced to end of results to finish
			std::vector<ArgPack> finalResults{};

			// iterate until all results completed
			std::size_t genEnd;
			++nTuples;
			do {
				genEnd = results.size();

				// add another argument to results
				for ( std::size_t i = genStart; i < genEnd; ++i ) {
					auto nextArg = results[i].nextArg;

					// use next element of exploded tuple if present
					if ( results[i].hasExpl() ) {
						const ExplodedActual& expl = results[i].getExpl( args );

						unsigned nextExpl = results[i].nextExpl + 1;
						if ( nextExpl == expl.exprs.size() ) {
							nextExpl = 0;
						}

						results.emplace_back(
							i, expl.exprs[results[i].nextExpl].get(), copy(results[i].env),
							copy(results[i].need), copy(results[i].have),
							copy(results[i].openVars), nextArg, nTuples, Cost::zero, nextExpl,
							results[i].explAlt );

						continue;
					}

					// finish result when out of arguments
					if ( nextArg >= args.size() ) {
						ArgPack newResult{
							results[i].env, results[i].need, results[i].have,
							results[i].openVars };
						newResult.nextArg = nextArg;
						Type* argType;

						if ( nTuples > 0 || ! results[i].expr ) {
							// first iteration or no expression to clone,
							// push empty tuple expression
							newResult.parent = i;
							std::list<Expression*> emptyList;
							newResult.expr.reset( new TupleExpr( emptyList ) );
							argType = newResult.expr->get_result();
						} else {
							// clone result to collect tuple
							newResult.parent = results[i].parent;
							newResult.cost = results[i].cost;
							newResult.tupleStart = results[i].tupleStart;
							newResult.expr.reset( results[i].expr->clone() );
							argType = newResult.expr->get_result();

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
								argType = newResult.expr->get_result();
							}
						}

						// check unification for ttype before adding to final
						if ( unify( ttype, argType, newResult.env, newResult.need, newResult.have,
								newResult.openVars, indexer ) ) {
							finalResults.push_back( move(newResult) );
						}

						continue;
					}

					// add each possible next argument
					for ( std::size_t j = 0; j < args[nextArg].size(); ++j ) {
						const ExplodedActual& expl = args[nextArg][j];

						// fresh copies of parent parameters for this iteration
						TypeEnvironment env = results[i].env;
						OpenVarSet openVars = results[i].openVars;

						env.addActual( expl.env, openVars );

						// skip empty tuple arguments by (near-)cloning parent into next gen
						if ( expl.exprs.empty() ) {
							results.emplace_back(
								results[i], move(env), copy(results[i].need),
								copy(results[i].have), move(openVars), nextArg + 1, expl.cost );

							continue;
						}

						// add new result
						results.emplace_back(
							i, expl.exprs.front().get(), move(env), copy(results[i].need),
							copy(results[i].have), move(openVars), nextArg + 1,
							nTuples, expl.cost, expl.exprs.size() == 1 ? 0 : 1, j );
					}
				}

				// reset for next round
				genStart = genEnd;
				nTuples = 0;
			} while ( genEnd != results.size() );

			// splice final results onto results
			for ( std::size_t i = 0; i < finalResults.size(); ++i ) {
				results.push_back( move(finalResults[i]) );
			}
			return ! finalResults.empty();
		}

		// iterate each current subresult
		std::size_t genEnd = results.size();
		for ( std::size_t i = genStart; i < genEnd; ++i ) {
			auto nextArg = results[i].nextArg;

			// use remainder of exploded tuple if present
			if ( results[i].hasExpl() ) {
				const ExplodedActual& expl = results[i].getExpl( args );
				Expression* expr = expl.exprs[results[i].nextExpl].get();

				TypeEnvironment env = results[i].env;
				AssertionSet need = results[i].need, have = results[i].have;
				OpenVarSet openVars = results[i].openVars;

				Type* actualType = expr->get_result();

				PRINT(
					std::cerr << "formal type is ";
					formalType->print( std::cerr );
					std::cerr << std::endl << "actual type is ";
					actualType->print( std::cerr );
					std::cerr << std::endl;
				)

				if ( unify( formalType, actualType, env, need, have, openVars, indexer ) ) {
					unsigned nextExpl = results[i].nextExpl + 1;
					if ( nextExpl == expl.exprs.size() ) {
						nextExpl = 0;
					}

					results.emplace_back(
						i, expr, move(env), move(need), move(have), move(openVars), nextArg,
						nTuples, Cost::zero, nextExpl, results[i].explAlt );
				}

				continue;
			}

			// use default initializers if out of arguments
			if ( nextArg >= args.size() ) {
				if ( ConstantExpr* cnstExpr = getDefaultValue( initializer ) ) {
					if ( Constant* cnst = dynamic_cast<Constant*>( cnstExpr->get_constant() ) ) {
						TypeEnvironment env = results[i].env;
						AssertionSet need = results[i].need, have = results[i].have;
						OpenVarSet openVars = results[i].openVars;

						if ( unify( formalType, cnst->get_type(), env, need, have, openVars,
								indexer ) ) {
							results.emplace_back(
								i, new DefaultArgExpr( cnstExpr ), move(env), move(need), move(have),
								move(openVars), nextArg, nTuples );
						}
					}
				}

				continue;
			}

			// Check each possible next argument
			for ( std::size_t j = 0; j < args[nextArg].size(); ++j ) {
				const ExplodedActual& expl = args[nextArg][j];

				// fresh copies of parent parameters for this iteration
				TypeEnvironment env = results[i].env;
				AssertionSet need = results[i].need, have = results[i].have;
				OpenVarSet openVars = results[i].openVars;

				env.addActual( expl.env, openVars );

				// skip empty tuple arguments by (near-)cloning parent into next gen
				if ( expl.exprs.empty() ) {
					results.emplace_back(
						results[i], move(env), move(need), move(have), move(openVars),
						nextArg + 1, expl.cost );

					continue;
				}

				// consider only first exploded actual
				Expression* expr = expl.exprs.front().get();
				Type* actualType = expr->result->clone();

				PRINT(
					std::cerr << "formal type is ";
					formalType->print( std::cerr );
					std::cerr << std::endl << "actual type is ";
					actualType->print( std::cerr );
					std::cerr << std::endl;
				)

				// attempt to unify types
				if ( unify( formalType, actualType, env, need, have, openVars, indexer ) ) {
					// add new result
					results.emplace_back(
						i, expr, move(env), move(need), move(have), move(openVars), nextArg + 1,
						nTuples, expl.cost, expl.exprs.size() == 1 ? 0 : 1, j );
				}
			}
		}

		// reset for next parameter
		genStart = genEnd;

		return genEnd != results.size();
	}

	template<typename OutputIterator>
	void AlternativeFinder::Finder::validateFunctionAlternative( const Alternative &func, ArgPack& result,
			const std::vector<ArgPack>& results, OutputIterator out ) {
		ApplicationExpr *appExpr = new ApplicationExpr( func.expr->clone() );
		// sum cost and accumulate actuals
		std::list<Expression*>& args = appExpr->args;
		Cost cost = func.cost;
		const ArgPack* pack = &result;
		while ( pack->expr ) {
			args.push_front( pack->expr->clone() );
			cost += pack->cost;
			pack = &results[pack->parent];
		}
		// build and validate new alternative
		Alternative newAlt{ appExpr, result.env, result.openVars, result.need, cost };
		PRINT(
			std::cerr << "instantiate function success: " << appExpr << std::endl;
			std::cerr << "need assertions:" << std::endl;
			printAssertionSet( result.need, std::cerr, 8 );
		)
		inferParameters( newAlt, out );
	}

	template<typename OutputIterator>
	void AlternativeFinder::Finder::makeFunctionAlternatives( const Alternative &func,
			FunctionType *funcType, const ExplodedArgs_old &args, OutputIterator out ) {
		OpenVarSet funcOpenVars;
		AssertionSet funcNeed, funcHave;
		TypeEnvironment funcEnv( func.env );
		makeUnifiableVars( funcType, funcOpenVars, funcNeed );
		// add all type variables as open variables now so that those not used in the parameter
		// list are still considered open.
		funcEnv.add( funcType->forall );

		if ( targetType && ! targetType->isVoid() && ! funcType->returnVals.empty() ) {
			// attempt to narrow based on expected target type
			Type * returnType = funcType->returnVals.front()->get_type();
			if ( ! unify( returnType, targetType, funcEnv, funcNeed, funcHave, funcOpenVars,
					indexer ) ) {
				// unification failed, don't pursue this function alternative
				return;
			}
		}

		// iteratively build matches, one parameter at a time
		std::vector<ArgPack> results;
		results.push_back( ArgPack{ funcEnv, funcNeed, funcHave, funcOpenVars } );
		std::size_t genStart = 0;

		for ( DeclarationWithType* formal : funcType->parameters ) {
			ObjectDecl* obj = strict_dynamic_cast< ObjectDecl* >( formal );
			if ( ! instantiateArgument(
					obj->type, obj->init, args, results, genStart, indexer ) )
				return;
		}

		if ( funcType->get_isVarArgs() ) {
			// append any unused arguments to vararg pack
			std::size_t genEnd;
			do {
				genEnd = results.size();

				// iterate results
				for ( std::size_t i = genStart; i < genEnd; ++i ) {
					auto nextArg = results[i].nextArg;

					// use remainder of exploded tuple if present
					if ( results[i].hasExpl() ) {
						const ExplodedActual& expl = results[i].getExpl( args );

						unsigned nextExpl = results[i].nextExpl + 1;
						if ( nextExpl == expl.exprs.size() ) {
							nextExpl = 0;
						}

						results.emplace_back(
							i, expl.exprs[results[i].nextExpl].get(), copy(results[i].env),
							copy(results[i].need), copy(results[i].have),
							copy(results[i].openVars), nextArg, 0, Cost::zero, nextExpl,
							results[i].explAlt );

						continue;
					}

					// finish result when out of arguments
					if ( nextArg >= args.size() ) {
						validateFunctionAlternative( func, results[i], results, out );

						continue;
					}

					// add each possible next argument
					for ( std::size_t j = 0; j < args[nextArg].size(); ++j ) {
						const ExplodedActual& expl = args[nextArg][j];

						// fresh copies of parent parameters for this iteration
						TypeEnvironment env = results[i].env;
						OpenVarSet openVars = results[i].openVars;

						env.addActual( expl.env, openVars );

						// skip empty tuple arguments by (near-)cloning parent into next gen
						if ( expl.exprs.empty() ) {
							results.emplace_back(
								results[i], move(env), copy(results[i].need),
								copy(results[i].have), move(openVars), nextArg + 1, expl.cost );

							continue;
						}

						// add new result
						results.emplace_back(
							i, expl.exprs.front().get(), move(env), copy(results[i].need),
							copy(results[i].have), move(openVars), nextArg + 1, 0,
							expl.cost, expl.exprs.size() == 1 ? 0 : 1, j );
					}
				}

				genStart = genEnd;
			} while ( genEnd != results.size() );
		} else {
			// filter out results that don't use all the arguments
			for ( std::size_t i = genStart; i < results.size(); ++i ) {
				ArgPack& result = results[i];
				if ( ! result.hasExpl() && result.nextArg >= args.size() ) {
					validateFunctionAlternative( func, result, results, out );
				}
			}
		}
	}

	void AlternativeFinder::Finder::postvisit( UntypedExpr *untypedExpr ) {
		AlternativeFinder funcFinder( indexer, env );
		funcFinder.findWithAdjustment( untypedExpr->function );
		// if there are no function alternatives, then proceeding is a waste of time.
		// xxx - findWithAdjustment throws, so this check and others like it shouldn't be necessary.
		if ( funcFinder.alternatives.empty() ) return;

		std::vector< AlternativeFinder > argAlternatives;
		altFinder.findSubExprs( untypedExpr->begin_args(), untypedExpr->end_args(),
			back_inserter( argAlternatives ) );

		// take care of possible tuple assignments
		// if not tuple assignment, assignment is taken care of as a normal function call
		Tuples::handleTupleAssignment( altFinder, untypedExpr, argAlternatives );

		// find function operators
		static NameExpr *opExpr = new NameExpr( "?()" );
		AlternativeFinder funcOpFinder( indexer, env );
		// it's ok if there aren't any defined function ops
		funcOpFinder.maybeFind( opExpr );
		PRINT(
			std::cerr << "known function ops:" << std::endl;
			printAlts( funcOpFinder.alternatives, std::cerr, 1 );
		)

		// pre-explode arguments
		ExplodedArgs_old argExpansions;
		argExpansions.reserve( argAlternatives.size() );

		for ( const AlternativeFinder& arg : argAlternatives ) {
			argExpansions.emplace_back();
			auto& argE = argExpansions.back();
			// argE.reserve( arg.alternatives.size() );

			for ( const Alternative& actual : arg ) {
				argE.emplace_back( actual, indexer );
			}
		}

		AltList candidates;
		SemanticErrorException errors;
		for ( AltList::iterator func = funcFinder.alternatives.begin(); func != funcFinder.alternatives.end(); ++func ) {
			try {
				PRINT(
					std::cerr << "working on alternative: " << std::endl;
					func->print( std::cerr, 8 );
				)
				// check if the type is pointer to function
				if ( PointerType *pointer = dynamic_cast< PointerType* >( func->expr->result->stripReferences() ) ) {
					if ( FunctionType *function = dynamic_cast< FunctionType* >( pointer->base ) ) {
						Alternative newFunc( *func );
						referenceToRvalueConversion( newFunc.expr, newFunc.cost );
						makeFunctionAlternatives( newFunc, function, argExpansions,
							std::back_inserter( candidates ) );
					}
				} else if ( TypeInstType *typeInst = dynamic_cast< TypeInstType* >( func->expr->result->stripReferences() ) ) { // handle ftype (e.g. *? on function pointer)
					if ( const EqvClass *eqvClass = func->env.lookup( typeInst->name ) ) {
						if ( FunctionType *function = dynamic_cast< FunctionType* >( eqvClass->type ) ) {
							Alternative newFunc( *func );
							referenceToRvalueConversion( newFunc.expr, newFunc.cost );
							makeFunctionAlternatives( newFunc, function, argExpansions,
								std::back_inserter( candidates ) );
						} // if
					} // if
				}
			} catch ( SemanticErrorException &e ) {
				errors.append( e );
			}
		} // for

		// try each function operator ?() with each function alternative
		if ( ! funcOpFinder.alternatives.empty() ) {
			// add exploded function alternatives to front of argument list
			std::vector<ExplodedActual> funcE;
			funcE.reserve( funcFinder.alternatives.size() );
			for ( const Alternative& actual : funcFinder ) {
				funcE.emplace_back( actual, indexer );
			}
			argExpansions.insert( argExpansions.begin(), move(funcE) );

			for ( AltList::iterator funcOp = funcOpFinder.alternatives.begin();
					funcOp != funcOpFinder.alternatives.end(); ++funcOp ) {
				try {
					// check if type is a pointer to function
					if ( PointerType* pointer = dynamic_cast<PointerType*>(
							funcOp->expr->result->stripReferences() ) ) {
						if ( FunctionType* function =
								dynamic_cast<FunctionType*>( pointer->base ) ) {
							Alternative newFunc( *funcOp );
							referenceToRvalueConversion( newFunc.expr, newFunc.cost );
							makeFunctionAlternatives( newFunc, function, argExpansions,
								std::back_inserter( candidates ) );
						}
					}
				} catch ( SemanticErrorException &e ) {
					errors.append( e );
				}
			}
		}

		// Implement SFINAE; resolution errors are only errors if there aren't any non-erroneous resolutions
		if ( candidates.empty() && ! errors.isEmpty() ) { throw errors; }

		// compute conversionsion costs
		for ( Alternative& withFunc : candidates ) {
			Cost cvtCost = computeApplicationConversionCost( withFunc, indexer );

			PRINT(
				ApplicationExpr *appExpr = strict_dynamic_cast< ApplicationExpr* >( withFunc.expr );
				PointerType *pointer = strict_dynamic_cast< PointerType* >( appExpr->function->result );
				FunctionType *function = strict_dynamic_cast< FunctionType* >( pointer->base );
				std::cerr << "Case +++++++++++++ " << appExpr->function << std::endl;
				std::cerr << "formals are:" << std::endl;
				printAll( function->parameters, std::cerr, 8 );
				std::cerr << "actuals are:" << std::endl;
				printAll( appExpr->args, std::cerr, 8 );
				std::cerr << "bindings are:" << std::endl;
				withFunc.env.print( std::cerr, 8 );
				std::cerr << "cost is: " << withFunc.cost << std::endl;
				std::cerr << "cost of conversion is:" << cvtCost << std::endl;
			)
			if ( cvtCost != Cost::infinity ) {
				withFunc.cvtCost = cvtCost;
				alternatives.push_back( withFunc );
			} // if
		} // for

		candidates = move(alternatives);

		// use a new list so that alternatives are not examined by addAnonConversions twice.
		AltList winners;
		findMinCost( candidates.begin(), candidates.end(), std::back_inserter( winners ) );

		// function may return struct or union value, in which case we need to add alternatives
		// for implicit conversions to each of the anonymous members, must happen after findMinCost
		// since anon conversions are never the cheapest expression
		for ( const Alternative & alt : winners ) {
			addAnonConversions( alt );
		}
		spliceBegin( alternatives, winners );

		if ( alternatives.empty() && targetType && ! targetType->isVoid() ) {
			// xxx - this is a temporary hack. If resolution is unsuccessful with a target type, try again without a
			// target type, since it will sometimes succeed when it wouldn't easily with target type binding. For example,
			//   forall( otype T ) lvalue T	?[?]( T *, ptrdiff_t );
			//   const char * x = "hello world";
			//   unsigned char ch = x[0];
			// Fails with simple return type binding. First, T is bound to unsigned char, then (x: const char *) is unified
			// with unsigned char *, which fails because pointer base types must be unified exactly. The new resolver should
			// fix this issue in a more robust way.
			targetType = nullptr;
			postvisit( untypedExpr );
		}
	}

	bool isLvalue( Expression *expr ) {
		// xxx - recurse into tuples?
		return expr->result && ( expr->get_lvalue() || dynamic_cast< ReferenceType * >( expr->result ) );
	}

	void AlternativeFinder::Finder::postvisit( AddressExpr *addressExpr ) {
		AlternativeFinder finder( indexer, env );
		finder.find( addressExpr->get_arg() );
		for ( Alternative& alt : finder.alternatives ) {
			if ( isLvalue( alt.expr ) ) {
				alternatives.push_back(
					Alternative{ alt, new AddressExpr( alt.expr->clone() ), alt.cost } );
			} // if
		} // for
	}

	void AlternativeFinder::Finder::postvisit( LabelAddressExpr * expr ) {
		alternatives.push_back( Alternative{ expr->clone(), env } );
	}

	Expression * restructureCast( Expression * argExpr, Type * toType, bool isGenerated ) {
		if ( argExpr->get_result()->size() > 1 && ! toType->isVoid() && ! dynamic_cast<ReferenceType *>( toType ) ) {
			// Argument expression is a tuple and the target type is not void and not a reference type.
			// Cast each member of the tuple to its corresponding target type, producing the tuple of those
			// cast expressions. If there are more components of the tuple than components in the target type,
			// then excess components do not come out in the result expression (but UniqueExprs ensure that
			// side effects will still be done).
			if ( Tuples::maybeImpureIgnoreUnique( argExpr ) ) {
				// expressions which may contain side effects require a single unique instance of the expression.
				argExpr = new UniqueExpr( argExpr );
			}
			std::list< Expression * > componentExprs;
			for ( unsigned int i = 0; i < toType->size(); i++ ) {
				// cast each component
				TupleIndexExpr * idx = new TupleIndexExpr( argExpr->clone(), i );
				componentExprs.push_back( restructureCast( idx, toType->getComponent( i ), isGenerated ) );
			}
			delete argExpr;
			assert( componentExprs.size() > 0 );
			// produce the tuple of casts
			return new TupleExpr( componentExprs );
		} else {
			// handle normally
			CastExpr * ret = new CastExpr( argExpr, toType->clone() );
			ret->isGenerated = isGenerated;
			return ret;
		}
	}

	void AlternativeFinder::Finder::postvisit( CastExpr *castExpr ) {
		Type *& toType = castExpr->get_result();
		assert( toType );
		toType = resolveTypeof( toType, indexer );
		assert(!dynamic_cast<TypeofType *>(toType));
		SymTab::validateType( toType, &indexer );
		adjustExprType( toType, env, indexer );

		AlternativeFinder finder( indexer, env );
		finder.targetType = toType;
		finder.findWithAdjustment( castExpr->arg );

		AltList candidates;
		for ( Alternative & alt : finder.alternatives ) {
			AssertionSet needAssertions( alt.need.begin(), alt.need.end() );
			AssertionSet haveAssertions;
			OpenVarSet openVars{ alt.openVars };

			alt.env.extractOpenVars( openVars );

			// It's possible that a cast can throw away some values in a multiply-valued expression.  (An example is a
			// cast-to-void, which casts from one value to zero.)  Figure out the prefix of the subexpression results
			// that are cast directly.  The candidate is invalid if it has fewer results than there are types to cast
			// to.
			int discardedValues = alt.expr->result->size() - castExpr->result->size();
			if ( discardedValues < 0 ) continue;
			// xxx - may need to go into tuple types and extract relevant types and use unifyList. Note that currently, this does not
			// allow casting a tuple to an atomic type (e.g. (int)([1, 2, 3]))
			// unification run for side-effects
			unify( castExpr->result, alt.expr->result, alt.env, needAssertions,
				haveAssertions, openVars, indexer );
			Cost thisCost =
				castExpr->isGenerated
				? conversionCost( alt.expr->result, castExpr->result, alt.expr->get_lvalue(),	indexer, alt.env )
				: castCost( alt.expr->result, castExpr->result, alt.expr->get_lvalue(),	indexer, alt.env );
			PRINT(
				std::cerr << "working on cast with result: " << castExpr->result << std::endl;
				std::cerr << "and expr type: " << alt.expr->result << std::endl;
				std::cerr << "env: " << alt.env << std::endl;
			)
			if ( thisCost != Cost::infinity ) {
				PRINT(
					std::cerr << "has finite cost." << std::endl;
				)
				// count one safe conversion for each value that is thrown away
				thisCost.incSafe( discardedValues );
				Alternative newAlt{
					restructureCast( alt.expr->clone(), toType, castExpr->isGenerated ),
					alt.env, openVars, needAssertions, alt.cost, alt.cost + thisCost };
				inferParameters( newAlt, back_inserter( candidates ) );
			} // if
		} // for

		// findMinCost selects the alternatives with the lowest "cost" members, but has the side effect of copying the
		// cvtCost member to the cost member (since the old cost is now irrelevant).  Thus, calling findMinCost twice
		// selects first based on argument cost, then on conversion cost.
		AltList minArgCost;
		findMinCost( candidates.begin(), candidates.end(), std::back_inserter( minArgCost ) );
		findMinCost( minArgCost.begin(), minArgCost.end(), std::back_inserter( alternatives ) );
	}

	void AlternativeFinder::Finder::postvisit( VirtualCastExpr * castExpr ) {
		assertf( castExpr->get_result(), "Implicit virtual cast targets not yet supported." );
		AlternativeFinder finder( indexer, env );
		// don't prune here, since it's guaranteed all alternatives will have the same type
		finder.findWithoutPrune( castExpr->get_arg() );
		for ( Alternative & alt : finder.alternatives ) {
			alternatives.push_back( Alternative{
				alt, new VirtualCastExpr{ alt.expr->clone(), castExpr->get_result()->clone() },
				alt.cost } );
		}
	}

	void AlternativeFinder::Finder::postvisit( KeywordCastExpr * castExpr ) {
		assertf( castExpr->get_result(), "Cast target should have been set in Validate." );
		auto ref = dynamic_cast<ReferenceType*>(castExpr->get_result());
		assert(ref);
		auto inst = dynamic_cast<StructInstType*>(ref->base);
		assert(inst);
		auto target = inst->baseStruct;

		AlternativeFinder finder( indexer, env );

		auto pick_alternatives = [target, this](AltList & found, bool expect_ref) {
			for(auto & alt : found) {
				Type * expr = alt.expr->get_result();
				if(expect_ref) {
					auto res = dynamic_cast<ReferenceType*>(expr);
					if(!res) { continue; }
					expr = res->base;
				}

				if(auto insttype = dynamic_cast<TypeInstType*>(expr)) {
					auto td = alt.env.lookup(insttype->name);
					if(!td) { continue; }
					expr = td->type;
				}

				if(auto base = dynamic_cast<StructInstType*>(expr)) {
					if(base->baseStruct == target) {
						alternatives.push_back(
							std::move(alt)
						);
					}
				}
			}
		};

		try {
			// Attempt 1 : turn (thread&)X into (thread$&)X.__thrd
			// Clone is purely for memory management
			std::unique_ptr<Expression> tech1 { new UntypedMemberExpr(new NameExpr(castExpr->concrete_target.field), castExpr->arg->clone()) };

			// don't prune here, since it's guaranteed all alternatives will have the same type
			finder.findWithoutPrune( tech1.get() );
			pick_alternatives(finder.alternatives, false);

			return;
		} catch(SemanticErrorException & ) {}

		// Fallback : turn (thread&)X into (thread$&)get_thread(X)
		std::unique_ptr<Expression> fallback { UntypedExpr::createDeref( new UntypedExpr(new NameExpr(castExpr->concrete_target.getter), { castExpr->arg->clone() })) };
		// don't prune here, since it's guaranteed all alternatives will have the same type
		finder.findWithoutPrune( fallback.get() );

		pick_alternatives(finder.alternatives, true);

		// Whatever happens here, we have no more fallbacks
	}

	namespace {
		/// Gets name from untyped member expression (member must be NameExpr)
		const std::string& get_member_name( UntypedMemberExpr *memberExpr ) {
			if ( dynamic_cast< ConstantExpr * >( memberExpr->get_member() ) ) {
				SemanticError( memberExpr, "Indexed access to struct fields unsupported: " );
			} // if
			NameExpr * nameExpr = dynamic_cast< NameExpr * >( memberExpr->get_member() );
			assert( nameExpr );
			return nameExpr->get_name();
		}
	}

	void AlternativeFinder::Finder::postvisit( UntypedMemberExpr *memberExpr ) {
		AlternativeFinder funcFinder( indexer, env );
		funcFinder.findWithAdjustment( memberExpr->get_aggregate() );
		for ( AltList::const_iterator agg = funcFinder.alternatives.begin(); agg != funcFinder.alternatives.end(); ++agg ) {
			// it's okay for the aggregate expression to have reference type -- cast it to the base type to treat the aggregate as the referenced value
			Cost cost = agg->cost;
			Expression * aggrExpr = agg->expr->clone();
			referenceToRvalueConversion( aggrExpr, cost );
			std::unique_ptr<Expression> guard( aggrExpr );

			// find member of the given type
			if ( StructInstType *structInst = dynamic_cast< StructInstType* >( aggrExpr->get_result() ) ) {
				addAggMembers( structInst, aggrExpr, *agg, cost, get_member_name(memberExpr) );
			} else if ( UnionInstType *unionInst = dynamic_cast< UnionInstType* >( aggrExpr->get_result() ) ) {
				addAggMembers( unionInst, aggrExpr, *agg, cost, get_member_name(memberExpr) );
			} else if ( TupleType * tupleType = dynamic_cast< TupleType * >( aggrExpr->get_result() ) ) {
				addTupleMembers( tupleType, aggrExpr, *agg, cost, memberExpr->get_member() );
			} // if
		} // for
	}

	void AlternativeFinder::Finder::postvisit( MemberExpr *memberExpr ) {
		alternatives.push_back( Alternative{ memberExpr->clone(), env } );
	}

	void AlternativeFinder::Finder::postvisit( NameExpr *nameExpr ) {
		std::list< SymTab::Indexer::IdData > declList;
		indexer.lookupId( nameExpr->name, declList );
		PRINT( std::cerr << "nameExpr is " << nameExpr->name << std::endl; )
		for ( auto & data : declList ) {
			Cost cost = Cost::zero;
			Expression * newExpr = data.combine( cost );

			// addAnonAlternatives uses vector::push_back, which invalidates references to existing elements, so
			// can't construct in place and use vector::back
			Alternative newAlt{ newExpr, env, OpenVarSet{}, AssertionList{}, Cost::zero, cost };
			PRINT(
				std::cerr << "decl is ";
				data.id->print( std::cerr );
				std::cerr << std::endl;
				std::cerr << "newExpr is ";
				newExpr->print( std::cerr );
				std::cerr << std::endl;
			)
			renameTypes( newAlt.expr );
			addAnonConversions( newAlt ); // add anonymous member interpretations whenever an aggregate value type is seen as a name expression.
			alternatives.push_back( std::move(newAlt) );
		} // for
	}

	void AlternativeFinder::Finder::postvisit( VariableExpr *variableExpr ) {
		// not sufficient to clone here, because variable's type may have changed
		// since the VariableExpr was originally created.
		alternatives.push_back( Alternative{ new VariableExpr{ variableExpr->var }, env } );
	}

	void AlternativeFinder::Finder::postvisit( ConstantExpr *constantExpr ) {
		alternatives.push_back( Alternative{ constantExpr->clone(), env } );
	}

	void AlternativeFinder::Finder::postvisit( SizeofExpr *sizeofExpr ) {
		if ( sizeofExpr->get_isType() ) {
			Type * newType = sizeofExpr->get_type()->clone();
			alternatives.push_back( Alternative{
				new SizeofExpr{ resolveTypeof( newType, indexer ) }, env } );
		} else {
			// find all alternatives for the argument to sizeof
			AlternativeFinder finder( indexer, env );
			finder.find( sizeofExpr->get_expr() );
			// find the lowest cost alternative among the alternatives, otherwise ambiguous
			AltList winners;
			findMinCost( finder.alternatives.begin(), finder.alternatives.end(), back_inserter( winners ) );
			if ( winners.size() != 1 ) {
				SemanticError( sizeofExpr->get_expr(), "Ambiguous expression in sizeof operand: " );
			} // if
			// return the lowest cost alternative for the argument
			Alternative &choice = winners.front();
			referenceToRvalueConversion( choice.expr, choice.cost );
			alternatives.push_back( Alternative{
				choice, new SizeofExpr( choice.expr->clone() ), Cost::zero } );
		} // if
	}

	void AlternativeFinder::Finder::postvisit( AlignofExpr *alignofExpr ) {
		if ( alignofExpr->get_isType() ) {
			Type * newType = alignofExpr->get_type()->clone();
			alternatives.push_back( Alternative{
				new AlignofExpr{ resolveTypeof( newType, indexer ) }, env } );
		} else {
			// find all alternatives for the argument to sizeof
			AlternativeFinder finder( indexer, env );
			finder.find( alignofExpr->get_expr() );
			// find the lowest cost alternative among the alternatives, otherwise ambiguous
			AltList winners;
			findMinCost( finder.alternatives.begin(), finder.alternatives.end(), back_inserter( winners ) );
			if ( winners.size() != 1 ) {
				SemanticError( alignofExpr->get_expr(), "Ambiguous expression in alignof operand: " );
			} // if
			// return the lowest cost alternative for the argument
			Alternative &choice = winners.front();
			referenceToRvalueConversion( choice.expr, choice.cost );
			alternatives.push_back( Alternative{
				choice, new AlignofExpr{ choice.expr->clone() }, Cost::zero } );
		} // if
	}

	template< typename StructOrUnionType >
	void AlternativeFinder::Finder::addOffsetof( StructOrUnionType *aggInst, const std::string &name ) {
		std::list< Declaration* > members;
		aggInst->lookup( name, members );
		for ( std::list< Declaration* >::const_iterator i = members.begin(); i != members.end(); ++i ) {
			if ( DeclarationWithType *dwt = dynamic_cast< DeclarationWithType* >( *i ) ) {
				alternatives.push_back( Alternative{
					new OffsetofExpr{ aggInst->clone(), dwt }, env } );
				renameTypes( alternatives.back().expr );
			} else {
				assert( false );
			}
		}
	}

	void AlternativeFinder::Finder::postvisit( UntypedOffsetofExpr *offsetofExpr ) {
		AlternativeFinder funcFinder( indexer, env );
		// xxx - resolveTypeof?
		if ( StructInstType *structInst = dynamic_cast< StructInstType* >( offsetofExpr->get_type() ) ) {
			addOffsetof( structInst, offsetofExpr->member );
		} else if ( UnionInstType *unionInst = dynamic_cast< UnionInstType* >( offsetofExpr->get_type() ) ) {
			addOffsetof( unionInst, offsetofExpr->member );
		}
	}

	void AlternativeFinder::Finder::postvisit( OffsetofExpr *offsetofExpr ) {
		alternatives.push_back( Alternative{ offsetofExpr->clone(), env } );
	}

	void AlternativeFinder::Finder::postvisit( OffsetPackExpr *offsetPackExpr ) {
		alternatives.push_back( Alternative{ offsetPackExpr->clone(), env } );
	}

	void AlternativeFinder::Finder::postvisit( LogicalExpr * logicalExpr ) {
		AlternativeFinder firstFinder( indexer, env );
		firstFinder.findWithAdjustment( logicalExpr->get_arg1() );
		if ( firstFinder.alternatives.empty() ) return;
		AlternativeFinder secondFinder( indexer, env );
		secondFinder.findWithAdjustment( logicalExpr->get_arg2() );
		if ( secondFinder.alternatives.empty() ) return;
		for ( const Alternative & first : firstFinder.alternatives ) {
			for ( const Alternative & second : secondFinder.alternatives ) {
				TypeEnvironment compositeEnv{ first.env };
				compositeEnv.simpleCombine( second.env );
				OpenVarSet openVars{ first.openVars };
				mergeOpenVars( openVars, second.openVars );
				AssertionSet need;
				cloneAll( first.need, need );
				cloneAll( second.need, need );

				LogicalExpr *newExpr = new LogicalExpr{
					first.expr->clone(), second.expr->clone(), logicalExpr->get_isAnd() };
				alternatives.push_back( Alternative{
					newExpr, std::move(compositeEnv), std::move(openVars),
					AssertionList( need.begin(), need.end() ), first.cost + second.cost } );
			}
		}
	}

	void AlternativeFinder::Finder::postvisit( ConditionalExpr *conditionalExpr ) {
		// find alternatives for condition
		AlternativeFinder firstFinder( indexer, env );
		firstFinder.findWithAdjustment( conditionalExpr->arg1 );
		if ( firstFinder.alternatives.empty() ) return;
		// find alternatives for true expression
		AlternativeFinder secondFinder( indexer, env );
		secondFinder.findWithAdjustment( conditionalExpr->arg2 );
		if ( secondFinder.alternatives.empty() ) return;
		// find alterantives for false expression
		AlternativeFinder thirdFinder( indexer, env );
		thirdFinder.findWithAdjustment( conditionalExpr->arg3 );
		if ( thirdFinder.alternatives.empty() ) return;
		for ( const Alternative & first : firstFinder.alternatives ) {
			for ( const Alternative & second : secondFinder.alternatives ) {
				for ( const Alternative & third : thirdFinder.alternatives ) {
					TypeEnvironment compositeEnv{ first.env };
					compositeEnv.simpleCombine( second.env );
					compositeEnv.simpleCombine( third.env );
					OpenVarSet openVars{ first.openVars };
					mergeOpenVars( openVars, second.openVars );
					mergeOpenVars( openVars, third.openVars );
					AssertionSet need;
					cloneAll( first.need, need );
					cloneAll( second.need, need );
					cloneAll( third.need, need );
					AssertionSet have;

					// unify true and false types, then infer parameters to produce new alternatives
					Type* commonType = nullptr;
					if ( unify( second.expr->result, third.expr->result, compositeEnv,
							need, have, openVars, indexer, commonType ) ) {
						ConditionalExpr *newExpr = new ConditionalExpr{
							first.expr->clone(), second.expr->clone(), third.expr->clone() };
						newExpr->result = commonType ? commonType : second.expr->result->clone();
						// convert both options to the conditional result type
						Cost cost = first.cost + second.cost + third.cost;
						cost += computeExpressionConversionCost(
							newExpr->arg2, newExpr->result, indexer, compositeEnv );
						cost += computeExpressionConversionCost(
							newExpr->arg3, newExpr->result, indexer, compositeEnv );
						// output alternative
						Alternative newAlt{
							newExpr, std::move(compositeEnv), std::move(openVars),
							AssertionList( need.begin(), need.end() ), cost };
						inferParameters( newAlt, back_inserter( alternatives ) );
					} // if
				} // for
			} // for
		} // for
	}

	void AlternativeFinder::Finder::postvisit( CommaExpr *commaExpr ) {
		TypeEnvironment newEnv( env );
		Expression *newFirstArg = resolveInVoidContext( commaExpr->get_arg1(), indexer, newEnv );
		AlternativeFinder secondFinder( indexer, newEnv );
		secondFinder.findWithAdjustment( commaExpr->get_arg2() );
		for ( const Alternative & alt : secondFinder.alternatives ) {
			alternatives.push_back( Alternative{
				alt, new CommaExpr{ newFirstArg->clone(), alt.expr->clone() }, alt.cost } );
		} // for
		delete newFirstArg;
	}

	void AlternativeFinder::Finder::postvisit( RangeExpr * rangeExpr ) {
		// resolve low and high, accept alternatives whose low and high types unify
		AlternativeFinder firstFinder( indexer, env );
		firstFinder.findWithAdjustment( rangeExpr->low );
		if ( firstFinder.alternatives.empty() ) return;
		AlternativeFinder secondFinder( indexer, env );
		secondFinder.findWithAdjustment( rangeExpr->high );
		if ( secondFinder.alternatives.empty() ) return;
		for ( const Alternative & first : firstFinder.alternatives ) {
			for ( const Alternative & second : secondFinder.alternatives ) {
				TypeEnvironment compositeEnv{ first.env };
				compositeEnv.simpleCombine( second.env );
				OpenVarSet openVars{ first.openVars };
				mergeOpenVars( openVars, second.openVars );
				AssertionSet need;
				cloneAll( first.need, need );
				cloneAll( second.need, need );
				AssertionSet have;

				Type* commonType = nullptr;
				if ( unify( first.expr->result, second.expr->result, compositeEnv, need, have,
						openVars, indexer, commonType ) ) {
					RangeExpr * newExpr =
						new RangeExpr{ first.expr->clone(), second.expr->clone() };
					newExpr->result = commonType ? commonType : first.expr->result->clone();
					Alternative newAlt{
						newExpr, std::move(compositeEnv), std::move(openVars),
						AssertionList( need.begin(), need.end() ), first.cost + second.cost };
					inferParameters( newAlt, back_inserter( alternatives ) );
				} // if
			} // for
		} // for
	}

	void AlternativeFinder::Finder::postvisit( UntypedTupleExpr *tupleExpr ) {
		std::vector< AlternativeFinder > subExprAlternatives;
		altFinder.findSubExprs( tupleExpr->get_exprs().begin(), tupleExpr->get_exprs().end(),
			back_inserter( subExprAlternatives ) );
		std::vector< AltList > possibilities;
		combos( subExprAlternatives.begin(), subExprAlternatives.end(),
			back_inserter( possibilities ) );
		for ( const AltList& alts : possibilities ) {
			std::list< Expression * > exprs;
			makeExprList( alts, exprs );

			TypeEnvironment compositeEnv;
			OpenVarSet openVars;
			AssertionSet need;
			for ( const Alternative& alt : alts ) {
				compositeEnv.simpleCombine( alt.env );
				mergeOpenVars( openVars, alt.openVars );
				cloneAll( alt.need, need );
			}

			alternatives.push_back( Alternative{
				new TupleExpr{ exprs }, std::move(compositeEnv), std::move(openVars),
				AssertionList( need.begin(), need.end() ), sumCost( alts ) } );
		} // for
	}

	void AlternativeFinder::Finder::postvisit( TupleExpr *tupleExpr ) {
		alternatives.push_back( Alternative{ tupleExpr->clone(), env } );
	}

	void AlternativeFinder::Finder::postvisit( ImplicitCopyCtorExpr * impCpCtorExpr ) {
		alternatives.push_back( Alternative{ impCpCtorExpr->clone(), env } );
	}

	void AlternativeFinder::Finder::postvisit( ConstructorExpr * ctorExpr ) {
		AlternativeFinder finder( indexer, env );
		// don't prune here, since it's guaranteed all alternatives will have the same type
		// (giving the alternatives different types is half of the point of ConstructorExpr nodes)
		finder.findWithoutPrune( ctorExpr->get_callExpr() );
		for ( Alternative & alt : finder.alternatives ) {
			alternatives.push_back( Alternative{
				alt, new ConstructorExpr( alt.expr->clone() ), alt.cost } );
		}
	}

	void AlternativeFinder::Finder::postvisit( TupleIndexExpr *tupleExpr ) {
		alternatives.push_back( Alternative{ tupleExpr->clone(), env } );
	}

	void AlternativeFinder::Finder::postvisit( TupleAssignExpr *tupleAssignExpr ) {
		alternatives.push_back( Alternative{ tupleAssignExpr->clone(), env } );
	}

	void AlternativeFinder::Finder::postvisit( UniqueExpr *unqExpr ) {
		AlternativeFinder finder( indexer, env );
		finder.findWithAdjustment( unqExpr->get_expr() );
		for ( Alternative & alt : finder.alternatives ) {
			// ensure that the id is passed on to the UniqueExpr alternative so that the expressions are "linked"
			UniqueExpr * newUnqExpr = new UniqueExpr( alt.expr->clone(), unqExpr->get_id() );
			alternatives.push_back( Alternative{ alt, newUnqExpr, alt.cost } );
		}
	}

	void AlternativeFinder::Finder::postvisit( StmtExpr *stmtExpr ) {
		StmtExpr * newStmtExpr = stmtExpr->clone();
		ResolvExpr::resolveStmtExpr( newStmtExpr, indexer );
		// xxx - this env is almost certainly wrong, and needs to somehow contain the combined environments from all of the statements in the stmtExpr...
		alternatives.push_back( Alternative{ newStmtExpr, env } );
	}

	void AlternativeFinder::Finder::postvisit( UntypedInitExpr *initExpr ) {
		// handle each option like a cast
		AltList candidates;
		PRINT(
			std::cerr << "untyped init expr: " << initExpr << std::endl;
		)
		// O(N^2) checks of d-types with e-types
		for ( InitAlternative & initAlt : initExpr->get_initAlts() ) {
			Type * toType = resolveTypeof( initAlt.type->clone(), indexer );
			SymTab::validateType( toType, &indexer );
			adjustExprType( toType, env, indexer );
			// Ideally the call to findWithAdjustment could be moved out of the loop, but unfortunately it currently has to occur inside or else
			// polymorphic return types are not properly bound to the initialization type, since return type variables are only open for the duration of resolving
			// the UntypedExpr. This is only actually an issue in initialization contexts that allow more than one possible initialization type, but it is still suboptimal.
			AlternativeFinder finder( indexer, env );
			finder.targetType = toType;
			finder.findWithAdjustment( initExpr->expr );
			for ( Alternative & alt : finder.get_alternatives() ) {
				TypeEnvironment newEnv( alt.env );
				AssertionSet need;
				cloneAll( alt.need, need );
				AssertionSet have;
				OpenVarSet openVars( alt.openVars );
				// xxx - find things in env that don't have a "representative type" and claim
				// those are open vars?
				PRINT(
					std::cerr << "  @ " << toType << " " << initAlt.designation << std::endl;
				)
				// It's possible that a cast can throw away some values in a multiply-valued
				// expression. (An example is a cast-to-void, which casts from one value to
				// zero.)  Figure out the prefix of the subexpression results that are cast
				// directly.  The candidate is invalid if it has fewer results than there are
				// types to cast to.
				int discardedValues = alt.expr->result->size() - toType->size();
				if ( discardedValues < 0 ) continue;
				// xxx - may need to go into tuple types and extract relevant types and use
				// unifyList. Note that currently, this does not allow casting a tuple to an
				// atomic type (e.g. (int)([1, 2, 3]))

				// unification run for side-effects
				bool canUnify = unify( toType, alt.expr->result, newEnv, need, have, openVars, indexer );
				(void) canUnify;
				// xxx - do some inspecting on this line... why isn't result bound to initAlt.type?

				Cost thisCost = computeConversionCost( alt.expr->result, toType, alt.expr->get_lvalue(),
					indexer, newEnv );

				PRINT(
					Cost legacyCost = castCost( alt.expr->result, toType, alt.expr->get_lvalue(),
						indexer, newEnv );
					std::cerr << "Considering initialization:";
					std::cerr << std::endl << "  FROM: "; alt.expr->result->print(std::cerr);
					std::cerr << std::endl << "  TO: ";   toType          ->print(std::cerr);
					std::cerr << std::endl << "  Unification " << (canUnify ? "succeeded" : "failed");
					std::cerr << std::endl << "  Legacy cost " << legacyCost;
					std::cerr << std::endl << "  New cost " << thisCost;
					std::cerr << std::endl;
				)

				if ( thisCost != Cost::infinity ) {
					// count one safe conversion for each value that is thrown away
					thisCost.incSafe( discardedValues );
					Alternative newAlt{
						new InitExpr{
							restructureCast( alt.expr->clone(), toType, true ), initAlt.designation->clone() },
						std::move(newEnv), std::move(openVars),
						AssertionList( need.begin(), need.end() ), alt.cost, thisCost };
					inferParameters( newAlt, back_inserter( candidates ) );
				}
			}
		}

		// findMinCost selects the alternatives with the lowest "cost" members, but has the side effect of copying the
		// cvtCost member to the cost member (since the old cost is now irrelevant).  Thus, calling findMinCost twice
		// selects first based on argument cost, then on conversion cost.
		AltList minArgCost;
		findMinCost( candidates.begin(), candidates.end(), std::back_inserter( minArgCost ) );
		findMinCost( minArgCost.begin(), minArgCost.end(), std::back_inserter( alternatives ) );
	}

	void AlternativeFinder::Finder::postvisit( InitExpr * ) {
		assertf( false, "AlternativeFinder should never see a resolved InitExpr." );
	}

	void AlternativeFinder::Finder::postvisit( DeletedExpr * ) {
		assertf( false, "AlternativeFinder should never see a DeletedExpr." );
	}

	void AlternativeFinder::Finder::postvisit( GenericExpr * ) {
		assertf( false, "_Generic is not yet supported." );
	}
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
