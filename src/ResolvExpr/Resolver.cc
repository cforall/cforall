//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Resolver.cc --
//
// Author           : Aaron B. Moss
// Created On       : Sun May 17 12:17:01 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr 20 10:41:00 2022
// Update Count     : 248
//

#include <cassert>                       // for strict_dynamic_cast, assert
#include <memory>                        // for allocator, allocator_traits<...
#include <tuple>                         // for get
#include <vector>                        // for vector

#include "Alternative.h"                 // for Alternative, AltList
#include "AlternativeFinder.h"           // for AlternativeFinder, resolveIn...
#include "Candidate.hpp"
#include "CandidateFinder.hpp"
#include "CurrentObject.h"               // for CurrentObject
#include "RenameVars.h"                  // for RenameVars, global_renamer
#include "Resolver.h"
#include "ResolveTypeof.h"
#include "ResolvMode.h"                  // for ResolvMode
#include "typeops.h"                     // for extractResultType
#include "Unify.h"                       // for unify
#include "CompilationState.h"
#include "AST/Chain.hpp"
#include "AST/Decl.hpp"
#include "AST/Init.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/Type.hpp"
#include "Common/Eval.h"                 // for eval
#include "Common/Iterate.hpp"            // for group_iterate
#include "Common/PassVisitor.h"          // for PassVisitor
#include "Common/SemanticError.h"        // for SemanticError
#include "Common/Stats/ResolveTime.h"    // for ResolveTime::start(), ResolveTime::stop()
#include "Common/ToString.hpp"           // for toCString
#include "InitTweak/GenInit.h"
#include "InitTweak/InitTweak.h"         // for isIntrinsicSingleArgCallStmt
#include "ResolvExpr/TypeEnvironment.h"  // for TypeEnvironment
#include "SymTab/Autogen.h"              // for SizeType
#include "SymTab/Indexer.h"              // for Indexer
#include "SymTab/Mangler.h"              // for Mangler
#include "SynTree/Declaration.h"         // for ObjectDecl, TypeDecl, Declar...
#include "SynTree/Expression.h"          // for Expression, CastExpr, InitExpr
#include "SynTree/Initializer.h"         // for ConstructorInit, SingleInit
#include "SynTree/Statement.h"           // for ForStmt, Statement, BranchStmt
#include "SynTree/Type.h"                // for Type, BasicType, PointerType
#include "SynTree/TypeSubstitution.h"    // for TypeSubstitution
#include "SynTree/Visitor.h"             // for acceptAll, maybeAccept
#include "Tuples/Tuples.h"
#include "Validate/FindSpecialDecls.h"   // for SizeType

using namespace std;

namespace ResolvExpr {
	struct Resolver_old final : public WithIndexer, public WithGuards, public WithVisitorRef<Resolver_old>, public WithShortCircuiting, public WithStmtsToAdd {
		Resolver_old() {}
		Resolver_old( const SymTab::Indexer & other ) {
			indexer = other;
		}

		void previsit( FunctionDecl * functionDecl );
		void postvisit( FunctionDecl * functionDecl );
		void previsit( ObjectDecl * objectDecll );
		void previsit( EnumDecl * enumDecl );
		void previsit( StaticAssertDecl * assertDecl );

		void previsit( ArrayType * at );
		void previsit( PointerType * at );

		void previsit( ExprStmt * exprStmt );
		void previsit( AsmExpr * asmExpr );
		void previsit( AsmStmt * asmStmt );
		void previsit( IfStmt * ifStmt );
		void previsit( WhileDoStmt * whileDoStmt );
		void previsit( ForStmt * forStmt );
		void previsit( SwitchStmt * switchStmt );
		void previsit( CaseStmt * caseStmt );
		void previsit( BranchStmt * branchStmt );
		void previsit( ReturnStmt * returnStmt );
		void previsit( ThrowStmt * throwStmt );
		void previsit( CatchStmt * catchStmt );
		void postvisit( CatchStmt * catchStmt );
		void previsit( WaitForStmt * stmt );

		void previsit( SingleInit * singleInit );
		void previsit( ListInit * listInit );
		void previsit( ConstructorInit * ctorInit );
	  private:
		typedef std::list< Initializer * >::iterator InitIterator;

		template< typename PtrType >
		void handlePtrType( PtrType * type );

		void fallbackInit( ConstructorInit * ctorInit );

		Type * functionReturn = nullptr;
		CurrentObject currentObject = nullptr;
		bool inEnumDecl = false;
	};

	struct ResolveWithExprs : public WithIndexer, public WithGuards, public WithVisitorRef<ResolveWithExprs>, public WithShortCircuiting, public WithStmtsToAdd {
		void previsit( FunctionDecl * );
		void previsit( WithStmt * );

		void resolveWithExprs( std::list< Expression * > & withExprs, std::list< Statement * > & newStmts );
	};

	void resolve( std::list< Declaration * > translationUnit ) {
		PassVisitor<Resolver_old> resolver;
		acceptAll( translationUnit, resolver );
	}

	void resolveDecl( Declaration * decl, const SymTab::Indexer & indexer ) {
		PassVisitor<Resolver_old> resolver( indexer );
		maybeAccept( decl, resolver );
	}

	namespace {
		struct DeleteFinder_old : public WithShortCircuiting	{
			DeletedExpr * delExpr = nullptr;
			void previsit( DeletedExpr * expr ) {
				if ( delExpr ) visit_children = false;
				else delExpr = expr;
			}

			void previsit( Expression * ) {
				if ( delExpr ) visit_children = false;
			}
		};
	}

	DeletedExpr * findDeletedExpr( Expression * expr ) {
		PassVisitor<DeleteFinder_old> finder;
		expr->accept( finder );
		return finder.pass.delExpr;
	}

	namespace {
		struct StripCasts_old {
			Expression * postmutate( CastExpr * castExpr ) {
				if ( castExpr->isGenerated && ResolvExpr::typesCompatible( castExpr->arg->result, castExpr->result, SymTab::Indexer() ) ) {
					// generated cast is to the same type as its argument, so it's unnecessary -- remove it
					Expression * expr = castExpr->arg;
					castExpr->arg = nullptr;
					std::swap( expr->env, castExpr->env );
					return expr;
				}
				return castExpr;
			}

			static void strip( Expression *& expr ) {
				PassVisitor<StripCasts_old> stripper;
				expr = expr->acceptMutator( stripper );
			}
		};

		void finishExpr( Expression *& expr, const TypeEnvironment & env, TypeSubstitution * oldenv = nullptr ) {
			expr->env = oldenv ? oldenv->clone() : new TypeSubstitution;
			env.makeSubstitution( *expr->env );
			StripCasts_old::strip( expr ); // remove unnecessary casts that may be buried in an expression
		}

		void removeExtraneousCast( Expression *& expr, const SymTab::Indexer & indexer ) {
			if ( CastExpr * castExpr = dynamic_cast< CastExpr * >( expr ) ) {
				if ( typesCompatible( castExpr->arg->result, castExpr->result, indexer ) ) {
					// cast is to the same type as its argument, so it's unnecessary -- remove it
					expr = castExpr->arg;
					castExpr->arg = nullptr;
					std::swap( expr->env, castExpr->env );
					delete castExpr;
				}
			}
		}
	} // namespace

	namespace {
		void findUnfinishedKindExpression(Expression * untyped, Alternative & alt, const SymTab::Indexer & indexer, const std::string & kindStr, std::function<bool(const Alternative &)> pred, ResolvMode mode = ResolvMode{} ) {
			assertf( untyped, "expected a non-null expression." );

			// xxx - this isn't thread-safe, but should work until we parallelize the resolver
			static unsigned recursion_level = 0;

			++recursion_level;
			TypeEnvironment env;
			AlternativeFinder finder( indexer, env );
			finder.find( untyped, recursion_level == 1 ? mode.atTopLevel() : mode );
			--recursion_level;

			#if 0
			if ( finder.get_alternatives().size() != 1 ) {
				std::cerr << "untyped expr is ";
				untyped->print( std::cerr );
				std::cerr << std::endl << "alternatives are:";
				for ( const Alternative & alt : finder.get_alternatives() ) {
					alt.print( std::cerr );
				} // for
			} // if
			#endif

			// produce filtered list of alternatives
			AltList candidates;
			for ( Alternative & alt : finder.get_alternatives() ) {
				if ( pred( alt ) ) {
					candidates.push_back( std::move( alt ) );
				}
			}

			// produce invalid error if no candidates
			if ( candidates.empty() ) {
				SemanticError( untyped, toString( "No reasonable alternatives for ", kindStr, (kindStr != "" ? " " : ""), "expression: ") );
			}

			// search for cheapest candidate
			AltList winners;
			bool seen_undeleted = false;
			for ( unsigned i = 0; i < candidates.size(); ++i ) {
				int c = winners.empty() ? -1 : candidates[i].cost.compare( winners.front().cost );

				if ( c > 0 ) continue; // skip more expensive than winner

				if ( c < 0 ) {
					// reset on new cheapest
					seen_undeleted = ! findDeletedExpr( candidates[i].expr );
					winners.clear();
				} else /* if ( c == 0 ) */ {
					if ( findDeletedExpr( candidates[i].expr ) ) {
						// skip deleted expression if already seen one equivalent-cost not
						if ( seen_undeleted ) continue;
					} else if ( ! seen_undeleted ) {
						// replace list of equivalent-cost deleted expressions with one non-deleted
						winners.clear();
						seen_undeleted = true;
					}
				}

				winners.emplace_back( std::move( candidates[i] ) );
			}

			// promote alternative.cvtCost to .cost
			// xxx - I don't know why this is done, but I'm keeping the behaviour from findMinCost
			for ( Alternative& winner : winners ) {
				winner.cost = winner.cvtCost;
			}

			// produce ambiguous errors, if applicable
			if ( winners.size() != 1 ) {
				std::ostringstream stream;
				stream << "Cannot choose between " << winners.size() << " alternatives for " << kindStr << (kindStr != "" ? " " : "") << "expression\n";
				untyped->print( stream );
				stream << " Alternatives are:\n";
				printAlts( winners, stream, 1 );
				SemanticError( untyped->location, stream.str() );
			}

			// single selected choice
			Alternative& choice = winners.front();

			// fail on only expression deleted
			if ( ! seen_undeleted ) {
				SemanticError( untyped->location, choice.expr, "Unique best alternative includes deleted identifier in " );
			}

			// xxx - check for ambiguous expressions

			// output selected choice
			alt = std::move( choice );
		}

		/// resolve `untyped` to the expression whose alternative satisfies `pred` with the lowest cost; kindStr is used for providing better error messages
		void findKindExpression(Expression *& untyped, const SymTab::Indexer & indexer, const std::string & kindStr, std::function<bool(const Alternative &)> pred, ResolvMode mode = ResolvMode{}) {
			if ( ! untyped ) return;
			Alternative choice;
			findUnfinishedKindExpression( untyped, choice, indexer, kindStr, pred, mode );
			finishExpr( choice.expr, choice.env, untyped->env );
			delete untyped;
			untyped = choice.expr;
			choice.expr = nullptr;
		}

		bool standardAlternativeFilter( const Alternative & ) {
			// currently don't need to filter, under normal circumstances.
			// in the future, this may be useful for removing deleted expressions
			return true;
		}
	} // namespace

	// used in resolveTypeof
	Expression * resolveInVoidContext( Expression * expr, const SymTab::Indexer & indexer ) {
		TypeEnvironment env;
		return resolveInVoidContext( expr, indexer, env );
	}

	Expression * resolveInVoidContext( Expression * expr, const SymTab::Indexer & indexer, TypeEnvironment & env ) {
		// it's a property of the language that a cast expression has either 1 or 0 interpretations; if it has 0
		// interpretations, an exception has already been thrown.
		assertf( expr, "expected a non-null expression." );

		CastExpr * untyped = new CastExpr( expr ); // cast to void
		untyped->location = expr->location;

		// set up and resolve expression cast to void
		Alternative choice;
		findUnfinishedKindExpression( untyped, choice, indexer, "", standardAlternativeFilter, ResolvMode::withAdjustment() );
		CastExpr * castExpr = strict_dynamic_cast< CastExpr * >( choice.expr );
		assert( castExpr );
		env = std::move( choice.env );

		// clean up resolved expression
		Expression * ret = castExpr->arg;
		castExpr->arg = nullptr;

		// unlink the arg so that it isn't deleted twice at the end of the program
		untyped->arg = nullptr;
		return ret;
	}

	void findVoidExpression( Expression *& untyped, const SymTab::Indexer & indexer ) {
		resetTyVarRenaming();
		TypeEnvironment env;
		Expression * newExpr = resolveInVoidContext( untyped, indexer, env );
		finishExpr( newExpr, env, untyped->env );
		delete untyped;
		untyped = newExpr;
	}

	void findSingleExpression( Expression *& untyped, const SymTab::Indexer & indexer ) {
		findKindExpression( untyped, indexer, "", standardAlternativeFilter );
	}

	void findSingleExpression( Expression *& untyped, Type * type, const SymTab::Indexer & indexer ) {
		assert( untyped && type );
		// transfer location to generated cast for error purposes
		CodeLocation location = untyped->location;
		untyped = new CastExpr( untyped, type );
		untyped->location = location;
		findSingleExpression( untyped, indexer );
		removeExtraneousCast( untyped, indexer );
	}

	namespace {
		bool isIntegralType( const Alternative & alt ) {
			Type * type = alt.expr->result;
			if ( dynamic_cast< EnumInstType * >( type ) ) {
				return true;
			} else if ( BasicType * bt = dynamic_cast< BasicType * >( type ) ) {
				return bt->isInteger();
			} else if ( dynamic_cast< ZeroType* >( type ) != nullptr || dynamic_cast< OneType* >( type ) != nullptr ) {
				return true;
			} else {
				return false;
			} // if
		}

		void findIntegralExpression( Expression *& untyped, const SymTab::Indexer & indexer ) {
			findKindExpression( untyped, indexer, "condition", isIntegralType );
		}
	}


	bool isStructOrUnion( const Alternative & alt ) {
		Type * t = alt.expr->result->stripReferences();
		return dynamic_cast< StructInstType * >( t ) || dynamic_cast< UnionInstType * >( t );
	}

	void resolveWithExprs( std::list< Declaration * > & translationUnit ) {
		PassVisitor<ResolveWithExprs> resolver;
		acceptAll( translationUnit, resolver );
	}

	void ResolveWithExprs::resolveWithExprs( std::list< Expression * > & withExprs, std::list< Statement * > & newStmts ) {
		for ( Expression *& expr : withExprs )  {
			// only struct- and union-typed expressions are viable candidates
			findKindExpression( expr, indexer, "with statement", isStructOrUnion );

			// if with expression might be impure, create a temporary so that it is evaluated once
			if ( Tuples::maybeImpure( expr ) ) {
				static UniqueName tmpNamer( "_with_tmp_" );
				ObjectDecl * tmp = ObjectDecl::newObject( tmpNamer.newName(), expr->result->clone(), new SingleInit( expr ) );
				expr = new VariableExpr( tmp );
				newStmts.push_back( new DeclStmt( tmp ) );
				if ( InitTweak::isConstructable( tmp->type ) ) {
					// generate ctor/dtor and resolve them
					tmp->init = InitTweak::genCtorInit( tmp );
					tmp->accept( *visitor );
				}
			}
		}
	}

	void ResolveWithExprs::previsit( WithStmt * withStmt ) {
		resolveWithExprs( withStmt->exprs, stmtsToAddBefore );
	}

	void ResolveWithExprs::previsit( FunctionDecl * functionDecl ) {
		{
			// resolve with-exprs with parameters in scope and add any newly generated declarations to the
			// front of the function body.
			auto guard = makeFuncGuard( [this]() { indexer.enterScope(); }, [this](){ indexer.leaveScope(); } );
			indexer.addFunctionType( functionDecl->type );
			std::list< Statement * > newStmts;
			resolveWithExprs( functionDecl->withExprs, newStmts );
			if ( functionDecl->statements ) {
				functionDecl->statements->kids.splice( functionDecl->statements->kids.begin(), newStmts );
			} else {
				assertf( functionDecl->withExprs.empty() && newStmts.empty(), "Function %s without a body has with-clause and/or generated with declarations.", functionDecl->name.c_str() );
			}
		}
	}

	void Resolver_old::previsit( ObjectDecl * objectDecl ) {
		// To handle initialization of routine pointers, e.g., int (*fp)(int) = foo(), means that
		// class-variable initContext is changed multiple time because the LHS is analysed twice.
		// The second analysis changes initContext because of a function type can contain object
		// declarations in the return and parameter types. So each value of initContext is
		// retained, so the type on the first analysis is preserved and used for selecting the RHS.
		GuardValue( currentObject );
		currentObject = CurrentObject( objectDecl->get_type() );
		if ( inEnumDecl && dynamic_cast< EnumInstType * >( objectDecl->get_type() ) ) {
			// enumerator initializers should not use the enum type to initialize, since
			// the enum type is still incomplete at this point. Use signed int instead.
			// TODO: BasicType::SignedInt may not longer be true
			currentObject = CurrentObject( new BasicType( Type::Qualifiers(), BasicType::SignedInt ) );
		}
	}

	template< typename PtrType >
	void Resolver_old::handlePtrType( PtrType * type ) {
		if ( type->get_dimension() ) {
			findSingleExpression( type->dimension, Validate::SizeType->clone(), indexer );
		}
	}

	void Resolver_old::previsit( ArrayType * at ) {
		handlePtrType( at );
	}

	void Resolver_old::previsit( PointerType * pt ) {
		handlePtrType( pt );
	}

	void Resolver_old::previsit( FunctionDecl * functionDecl ) {
#if 0
		std::cerr << "resolver visiting functiondecl ";
		functionDecl->print( std::cerr );
		std::cerr << std::endl;
#endif
		GuardValue( functionReturn );
		functionReturn = ResolvExpr::extractResultType( functionDecl->type );
	}

	void Resolver_old::postvisit( FunctionDecl * functionDecl ) {
		// default value expressions have an environment which shouldn't be there and trips up
		// later passes.
		// xxx - it might be necessary to somehow keep the information from this environment, but I
		// can't currently see how it's useful.
		for ( Declaration * d : functionDecl->type->parameters ) {
			if ( ObjectDecl * obj = dynamic_cast< ObjectDecl * >( d ) ) {
				if ( SingleInit * init = dynamic_cast< SingleInit * >( obj->init ) ) {
					delete init->value->env;
					init->value->env = nullptr;
				}
			}
		}
	}

	void Resolver_old::previsit( EnumDecl * ) {
		// in case we decide to allow nested enums
		GuardValue( inEnumDecl );
		inEnumDecl = true;
	}

	void Resolver_old::previsit( StaticAssertDecl * assertDecl ) {
		findIntegralExpression( assertDecl->condition, indexer );
	}

	void Resolver_old::previsit( ExprStmt * exprStmt ) {
		visit_children = false;
		assertf( exprStmt->expr, "ExprStmt has null Expression in resolver" );
		findVoidExpression( exprStmt->expr, indexer );
	}

	void Resolver_old::previsit( AsmExpr * asmExpr ) {
		visit_children = false;
		findVoidExpression( asmExpr->operand, indexer );
	}

	void Resolver_old::previsit( AsmStmt * asmStmt ) {
		visit_children = false;
		acceptAll( asmStmt->get_input(), *visitor );
		acceptAll( asmStmt->get_output(), *visitor );
	}

	void Resolver_old::previsit( IfStmt * ifStmt ) {
		findIntegralExpression( ifStmt->condition, indexer );
	}

	void Resolver_old::previsit( WhileDoStmt * whileDoStmt ) {
		findIntegralExpression( whileDoStmt->condition, indexer );
	}

	void Resolver_old::previsit( ForStmt * forStmt ) {
		if ( forStmt->condition ) {
			findIntegralExpression( forStmt->condition, indexer );
		} // if

		if ( forStmt->increment ) {
			findVoidExpression( forStmt->increment, indexer );
		} // if
	}

	void Resolver_old::previsit( SwitchStmt * switchStmt ) {
		GuardValue( currentObject );
		findIntegralExpression( switchStmt->condition, indexer );

		currentObject = CurrentObject( switchStmt->condition->result );
	}

	void Resolver_old::previsit( CaseStmt * caseStmt ) {
		if ( caseStmt->condition ) {
			std::list< InitAlternative > initAlts = currentObject.getOptions();
			assertf( initAlts.size() == 1, "SwitchStmt did not correctly resolve an integral expression." );
			// must remove cast from case statement because RangeExpr cannot be cast.
			Expression * newExpr = new CastExpr( caseStmt->condition, initAlts.front().type->clone() );
			findSingleExpression( newExpr, indexer );
			// case condition cannot have a cast in C, so it must be removed, regardless of whether it performs a conversion.
			// Ideally we would perform the conversion internally here.
			if ( CastExpr * castExpr = dynamic_cast< CastExpr * >( newExpr ) ) {
				newExpr = castExpr->arg;
				castExpr->arg = nullptr;
				std::swap( newExpr->env, castExpr->env );
				delete castExpr;
			}
			caseStmt->condition = newExpr;
		}
	}

	void Resolver_old::previsit( BranchStmt * branchStmt ) {
		visit_children = false;
		// must resolve the argument for a computed goto
		if ( branchStmt->get_type() == BranchStmt::Goto ) { // check for computed goto statement
			if ( branchStmt->computedTarget ) {
				// computed goto argument is void *
				findSingleExpression( branchStmt->computedTarget, new PointerType( Type::Qualifiers(), new VoidType( Type::Qualifiers() ) ), indexer );
			} // if
		} // if
	}

	void Resolver_old::previsit( ReturnStmt * returnStmt ) {
		visit_children = false;
		if ( returnStmt->expr ) {
			findSingleExpression( returnStmt->expr, functionReturn->clone(), indexer );
		} // if
	}

	void Resolver_old::previsit( ThrowStmt * throwStmt ) {
		visit_children = false;
		// TODO: Replace *exception type with &exception type.
		if ( throwStmt->get_expr() ) {
			const StructDecl * exception_decl = indexer.lookupStruct( "__cfaehm_base_exception_t" );
			assert( exception_decl );
			Type * exceptType = new PointerType( noQualifiers, new StructInstType( noQualifiers, const_cast<StructDecl *>(exception_decl) ) );
			findSingleExpression( throwStmt->expr, exceptType, indexer );
		}
	}

	void Resolver_old::previsit( CatchStmt * catchStmt ) {
		// Until we are very sure this invarent (ifs that move between passes have then)
		// holds, check it. This allows a check for when to decode the mangling.
		if ( IfStmt * ifStmt = dynamic_cast<IfStmt *>( catchStmt->body ) ) {
			assert( ifStmt->then );
		}
		// Encode the catchStmt so the condition can see the declaration.
		if ( catchStmt->cond ) {
			IfStmt * ifStmt = new IfStmt( catchStmt->cond, nullptr, catchStmt->body );
			catchStmt->cond = nullptr;
			catchStmt->body = ifStmt;
		}
	}

	void Resolver_old::postvisit( CatchStmt * catchStmt ) {
		// Decode the catchStmt so everything is stored properly.
		IfStmt * ifStmt = dynamic_cast<IfStmt *>( catchStmt->body );
		if ( nullptr != ifStmt && nullptr == ifStmt->then ) {
			assert( ifStmt->condition );
			assert( ifStmt->else_ );
			catchStmt->cond = ifStmt->condition;
			catchStmt->body = ifStmt->else_;
			ifStmt->condition = nullptr;
			ifStmt->else_ = nullptr;
			delete ifStmt;
		}
	}

	template< typename iterator_t >
	inline bool advance_to_mutex( iterator_t & it, const iterator_t & end ) {
		while( it != end && !(*it)->get_type()->get_mutex() ) {
			it++;
		}

		return it != end;
	}

	void Resolver_old::previsit( WaitForStmt * stmt ) {
		visit_children = false;

		// Resolve all clauses first
		for( auto& clause : stmt->clauses ) {

			TypeEnvironment env;
			AlternativeFinder funcFinder( indexer, env );

			// Find all alternatives for a function in canonical form
			funcFinder.findWithAdjustment( clause.target.function );

			if ( funcFinder.get_alternatives().empty() ) {
				stringstream ss;
				ss << "Use of undeclared indentifier '";
				ss << strict_dynamic_cast<NameExpr*>( clause.target.function )->name;
				ss << "' in call to waitfor";
				SemanticError( stmt->location, ss.str() );
			}

			if(clause.target.arguments.empty()) {
				SemanticError( stmt->location, "Waitfor clause must have at least one mutex parameter");
			}

			// Find all alternatives for all arguments in canonical form
			std::vector< AlternativeFinder > argAlternatives;
			funcFinder.findSubExprs( clause.target.arguments.begin(), clause.target.arguments.end(), back_inserter( argAlternatives ) );

			// List all combinations of arguments
			std::vector< AltList > possibilities;
			combos( argAlternatives.begin(), argAlternatives.end(), back_inserter( possibilities ) );

			AltList                func_candidates;
			std::vector< AltList > args_candidates;

			// For every possible function :
			// 	try matching the arguments to the parameters
			// 	not the other way around because we have more arguments than parameters
			SemanticErrorException errors;
			for ( Alternative & func : funcFinder.get_alternatives() ) {
				try {
					PointerType * pointer = dynamic_cast< PointerType* >( func.expr->get_result()->stripReferences() );
					if( !pointer ) {
						SemanticError( func.expr->get_result(), "candidate not viable: not a pointer type\n" );
					}

					FunctionType * function = dynamic_cast< FunctionType* >( pointer->get_base() );
					if( !function ) {
						SemanticError( pointer->get_base(), "candidate not viable: not a function type\n" );
					}


					{
						auto param     = function->parameters.begin();
						auto param_end = function->parameters.end();

						if( !advance_to_mutex( param, param_end ) ) {
							SemanticError(function, "candidate function not viable: no mutex parameters\n");
						}
					}

					Alternative newFunc( func );
					// Strip reference from function
					referenceToRvalueConversion( newFunc.expr, newFunc.cost );

					// For all the set of arguments we have try to match it with the parameter of the current function alternative
					for ( auto & argsList : possibilities ) {

						try {
							// Declare data structures need for resolution
							OpenVarSet openVars;
							AssertionSet resultNeed, resultHave;
							TypeEnvironment resultEnv( func.env );
							makeUnifiableVars( function, openVars, resultNeed );
							// add all type variables as open variables now so that those not used in the parameter
							// list are still considered open.
							resultEnv.add( function->forall );

							// Load type variables from arguemnts into one shared space
							simpleCombineEnvironments( argsList.begin(), argsList.end(), resultEnv );

							// Make sure we don't widen any existing bindings
							resultEnv.forbidWidening();

							// Find any unbound type variables
							resultEnv.extractOpenVars( openVars );

							auto param     = function->parameters.begin();
							auto param_end = function->parameters.end();

							int n_mutex_param = 0;

							// For every arguments of its set, check if it matches one of the parameter
							// The order is important
							for( auto & arg : argsList ) {

								// Ignore non-mutex arguments
								if( !advance_to_mutex( param, param_end ) ) {
									// We ran out of parameters but still have arguments
									// this function doesn't match
									SemanticError( function, toString("candidate function not viable: too many mutex arguments, expected ", n_mutex_param, "\n" ));
								}

								n_mutex_param++;

								// Check if the argument matches the parameter type in the current scope
								if( ! unify( arg.expr->get_result(), (*param)->get_type(), resultEnv, resultNeed, resultHave, openVars, this->indexer ) ) {
									// Type doesn't match
									stringstream ss;
									ss << "candidate function not viable: no known convertion from '";
									(*param)->get_type()->print( ss );
									ss << "' to '";
									arg.expr->get_result()->print( ss );
									ss << "' with env '";
									resultEnv.print(ss);
									ss << "'\n";
									SemanticError( function, ss.str() );
								}

								param++;
							}

							// All arguments match !

							// Check if parameters are missing
							if( advance_to_mutex( param, param_end ) ) {
								do {
									n_mutex_param++;
									param++;
								} while( advance_to_mutex( param, param_end ) );

								// We ran out of arguments but still have parameters left
								// this function doesn't match
								SemanticError( function, toString("candidate function not viable: too few mutex arguments, expected ", n_mutex_param, "\n" ));
							}

							// All parameters match !

							// Finish the expressions to tie in the proper environments
							finishExpr( newFunc.expr, resultEnv );
							for( Alternative & alt : argsList ) {
								finishExpr( alt.expr, resultEnv );
							}

							// This is a match store it and save it for later
							func_candidates.push_back( newFunc );
							args_candidates.push_back( argsList );

						}
						catch( SemanticErrorException & e ) {
							errors.append( e );
						}
					}
				}
				catch( SemanticErrorException & e ) {
					errors.append( e );
				}
			}

			// Make sure we got the right number of arguments
			if( func_candidates.empty() )    { SemanticErrorException top( stmt->location, "No alternatives for function in call to waitfor"  ); top.append( errors ); throw top; }
			if( args_candidates.empty() )    { SemanticErrorException top( stmt->location, "No alternatives for arguments in call to waitfor" ); top.append( errors ); throw top; }
			if( func_candidates.size() > 1 ) { SemanticErrorException top( stmt->location, "Ambiguous function in call to waitfor"            ); top.append( errors ); throw top; }
			if( args_candidates.size() > 1 ) { SemanticErrorException top( stmt->location, "Ambiguous arguments in call to waitfor"           ); top.append( errors ); throw top; }
			// TODO: need to use findDeletedExpr to ensure no deleted identifiers are used.

			// Swap the results from the alternative with the unresolved values.
			// Alternatives will handle deletion on destruction
			std::swap( clause.target.function, func_candidates.front().expr );
			for( auto arg_pair : group_iterate( clause.target.arguments, args_candidates.front() ) ) {
				std::swap ( std::get<0>( arg_pair), std::get<1>( arg_pair).expr );
			}

			// Resolve the conditions as if it were an IfStmt
			// Resolve the statments normally
			findSingleExpression( clause.condition, this->indexer );
			clause.statement->accept( *visitor );
		}


		if( stmt->timeout.statement ) {
			// Resolve the timeout as an size_t for now
			// Resolve the conditions as if it were an IfStmt
			// Resolve the statments normally
			findSingleExpression( stmt->timeout.time, new BasicType( noQualifiers, BasicType::LongLongUnsignedInt ), this->indexer );
			findSingleExpression( stmt->timeout.condition, this->indexer );
			stmt->timeout.statement->accept( *visitor );
		}

		if( stmt->orelse.statement ) {
			// Resolve the conditions as if it were an IfStmt
			// Resolve the statments normally
			findSingleExpression( stmt->orelse.condition, this->indexer );
			stmt->orelse.statement->accept( *visitor );
		}
	}

	bool isCharType( Type * t ) {
		if ( BasicType * bt = dynamic_cast< BasicType * >( t ) ) {
			return bt->get_kind() == BasicType::Char || bt->get_kind() == BasicType::SignedChar ||
				bt->get_kind() == BasicType::UnsignedChar;
		}
		return false;
	}

	void Resolver_old::previsit( SingleInit * singleInit ) {
		visit_children = false;
		// resolve initialization using the possibilities as determined by the currentObject cursor
		Expression * newExpr = new UntypedInitExpr( singleInit->value, currentObject.getOptions() );
		findSingleExpression( newExpr, indexer );
		InitExpr * initExpr = strict_dynamic_cast< InitExpr * >( newExpr );

		// move cursor to the object that is actually initialized
		currentObject.setNext( initExpr->get_designation() );

		// discard InitExpr wrapper and retain relevant pieces
		newExpr = initExpr->expr;
		initExpr->expr = nullptr;
		std::swap( initExpr->env, newExpr->env );
		// InitExpr may have inferParams in the case where the expression specializes a function
		// pointer, and newExpr may already have inferParams of its own, so a simple swap is not
		// sufficient.
		newExpr->spliceInferParams( initExpr );
		delete initExpr;

		// get the actual object's type (may not exactly match what comes back from the resolver
		// due to conversions)
		Type * initContext = currentObject.getCurrentType();

		removeExtraneousCast( newExpr, indexer );

		// check if actual object's type is char[]
		if ( ArrayType * at = dynamic_cast< ArrayType * >( initContext ) ) {
			if ( isCharType( at->get_base() ) ) {
				// check if the resolved type is char *
				if ( PointerType * pt = dynamic_cast< PointerType *>( newExpr->get_result() ) ) {
					if ( isCharType( pt->get_base() ) ) {
						if ( CastExpr * ce = dynamic_cast< CastExpr * >( newExpr ) ) {
							// strip cast if we're initializing a char[] with a char *,
							// e.g.  char x[] = "hello";
							newExpr = ce->get_arg();
							ce->set_arg( nullptr );
							std::swap( ce->env, newExpr->env );
							delete ce;
						}
					}
				}
			}
		}

		// set initializer expr to resolved express
		singleInit->value = newExpr;

		// move cursor to next object in preparation for next initializer
		currentObject.increment();
	}

	void Resolver_old::previsit( ListInit * listInit ) {
		visit_children = false;
		// move cursor into brace-enclosed initializer-list
		currentObject.enterListInit();
		// xxx - fix this so that the list isn't copied, iterator should be used to change current
		// element
		std::list<Designation *> newDesignations;
		for ( auto p : group_iterate(listInit->get_designations(), listInit->get_initializers()) ) {
			// iterate designations and initializers in pairs, moving the cursor to the current
			// designated object and resolving the initializer against that object.
			Designation * des = std::get<0>(p);
			Initializer * init = std::get<1>(p);
			newDesignations.push_back( currentObject.findNext( des ) );
			init->accept( *visitor );
		}
		// set the set of 'resolved' designations and leave the brace-enclosed initializer-list
		listInit->get_designations() = newDesignations; // xxx - memory management
		currentObject.exitListInit();

		// xxx - this part has not be folded into CurrentObject yet
		// } else if ( TypeInstType * tt = dynamic_cast< TypeInstType * >( initContext ) ) {
		// 	Type * base = tt->get_baseType()->get_base();
		// 	if ( base ) {
		// 		// know the implementation type, so try using that as the initContext
		// 		ObjectDecl tmpObj( "", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, base->clone(), nullptr );
		// 		currentObject = &tmpObj;
		// 		visit( listInit );
		// 	} else {
		// 		// missing implementation type -- might be an unknown type variable, so try proceeding with the current init context
		// 		Parent::visit( listInit );
		// 	}
		// } else {
	}

	// ConstructorInit - fall back on C-style initializer
	void Resolver_old::fallbackInit( ConstructorInit * ctorInit ) {
		// could not find valid constructor, or found an intrinsic constructor
		// fall back on C-style initializer
		delete ctorInit->get_ctor();
		ctorInit->set_ctor( nullptr );
		delete ctorInit->get_dtor();
		ctorInit->set_dtor( nullptr );
		maybeAccept( ctorInit->get_init(), *visitor );
	}

	// needs to be callable from outside the resolver, so this is a standalone function
	void resolveCtorInit( ConstructorInit * ctorInit, const SymTab::Indexer & indexer ) {
		assert( ctorInit );
		PassVisitor<Resolver_old> resolver( indexer );
		ctorInit->accept( resolver );
	}

	void resolveStmtExpr( StmtExpr * stmtExpr, const SymTab::Indexer & indexer ) {
		assert( stmtExpr );
		PassVisitor<Resolver_old> resolver( indexer );
		stmtExpr->accept( resolver );
		stmtExpr->computeResult();
		// xxx - aggregate the environments from all statements? Possibly in AlternativeFinder instead?
	}

	void Resolver_old::previsit( ConstructorInit * ctorInit ) {
		visit_children = false;
		// xxx - fallback init has been removed => remove fallbackInit function and remove complexity from FixInit and remove C-init from ConstructorInit
		maybeAccept( ctorInit->ctor, *visitor );
		maybeAccept( ctorInit->dtor, *visitor );

		// found a constructor - can get rid of C-style initializer
		delete ctorInit->init;
		ctorInit->init = nullptr;

		// intrinsic single parameter constructors and destructors do nothing. Since this was
		// implicitly generated, there's no way for it to have side effects, so get rid of it
		// to clean up generated code.
		if ( InitTweak::isIntrinsicSingleArgCallStmt( ctorInit->ctor ) ) {
			delete ctorInit->ctor;
			ctorInit->ctor = nullptr;
		}

		if ( InitTweak::isIntrinsicSingleArgCallStmt( ctorInit->dtor ) ) {
			delete ctorInit->dtor;
			ctorInit->dtor = nullptr;
		}

		// xxx - todo -- what about arrays?
		// if ( dtor == nullptr && InitTweak::isIntrinsicCallStmt( ctorInit->get_ctor() ) ) {
		// 	// can reduce the constructor down to a SingleInit using the
		// 	// second argument from the ctor call, since
		// 	delete ctorInit->get_ctor();
		// 	ctorInit->set_ctor( nullptr );

		// 	Expression * arg =
		// 	ctorInit->set_init( new SingleInit( arg ) );
		// }
	}

	///////////////////////////////////////////////////////////////////////////
	//
	// *** NEW RESOLVER ***
	//
	///////////////////////////////////////////////////////////////////////////

	namespace {
		/// Finds deleted expressions in an expression tree
		struct DeleteFinder_new final : public ast::WithShortCircuiting, public ast::WithVisitorRef<DeleteFinder_new> {
			const ast::DeletedExpr * result = nullptr;

			void previsit( const ast::DeletedExpr * expr ) {
				if ( result ) { visit_children = false; }
				else { result = expr; }
			}

			void previsit( const ast::Expr * expr ) {
				if ( result ) { visit_children = false; }
				if (expr->inferred.hasParams()) {
					for (auto & imp : expr->inferred.inferParams() ) {
						imp.second.expr->accept(*visitor);
					}
				}
			}
		};
	} // anonymous namespace
	/// Check if this expression is or includes a deleted expression
	const ast::DeletedExpr * findDeletedExpr( const ast::Expr * expr ) {
		return ast::Pass<DeleteFinder_new>::read( expr );
	}

	namespace {
		/// always-accept candidate filter
		bool anyCandidate( const Candidate & ) { return true; }

		/// Calls the CandidateFinder and finds the single best candidate
		CandidateRef findUnfinishedKindExpression(
			const ast::Expr * untyped, const ResolveContext & context, const std::string & kind,
			std::function<bool(const Candidate &)> pred = anyCandidate, ResolvMode mode = {}
		) {
			if ( ! untyped ) return nullptr;

			// xxx - this isn't thread-safe, but should work until we parallelize the resolver
			static unsigned recursion_level = 0;

			++recursion_level;
			ast::TypeEnvironment env;
			CandidateFinder finder( context, env );
			finder.allowVoid = true;
			finder.find( untyped, recursion_level == 1 ? mode.atTopLevel() : mode );
			--recursion_level;

			// produce a filtered list of candidates
			CandidateList candidates;
			for ( auto & cand : finder.candidates ) {
				if ( pred( *cand ) ) { candidates.emplace_back( cand ); }
			}

			// produce invalid error if no candidates
			if ( candidates.empty() ) {
				SemanticError( untyped,
					toString( "No reasonable alternatives for ", kind, (kind != "" ? " " : ""),
					"expression: ") );
			}

			// search for cheapest candidate
			CandidateList winners;
			bool seen_undeleted = false;
			for ( CandidateRef & cand : candidates ) {
				int c = winners.empty() ? -1 : cand->cost.compare( winners.front()->cost );

				if ( c > 0 ) continue;  // skip more expensive than winner

				if ( c < 0 ) {
					// reset on new cheapest
					seen_undeleted = ! findDeletedExpr( cand->expr );
					winners.clear();
				} else /* if ( c == 0 ) */ {
					if ( findDeletedExpr( cand->expr ) ) {
						// skip deleted expression if already seen one equivalent-cost not
						if ( seen_undeleted ) continue;
					} else if ( ! seen_undeleted ) {
						// replace list of equivalent-cost deleted expressions with one non-deleted
						winners.clear();
						seen_undeleted = true;
					}
				}

				winners.emplace_back( std::move( cand ) );
			}

			// promote candidate.cvtCost to .cost
			// promoteCvtCost( winners );

			// produce ambiguous errors, if applicable
			if ( winners.size() != 1 ) {
				std::ostringstream stream;
				stream << "Cannot choose between " << winners.size() << " alternatives for "
					<< kind << (kind != "" ? " " : "") << "expression\n";
				ast::print( stream, untyped );
				stream << " Alternatives are:\n";
				print( stream, winners, 1 );
				SemanticError( untyped->location, stream.str() );
			}

			// single selected choice
			CandidateRef & choice = winners.front();

			// fail on only expression deleted
			if ( ! seen_undeleted ) {
				SemanticError( untyped->location, choice->expr.get(), "Unique best alternative "
				"includes deleted identifier in " );
			}

			return std::move( choice );
		}

		/// Strips extraneous casts out of an expression
		struct StripCasts_new final {
			const ast::Expr * postvisit( const ast::CastExpr * castExpr ) {
				if (
					castExpr->isGenerated == ast::GeneratedCast
					&& typesCompatible( castExpr->arg->result, castExpr->result )
				) {
					// generated cast is the same type as its argument, remove it after keeping env
					return ast::mutate_field(
						castExpr->arg.get(), &ast::Expr::env, castExpr->env );
				}
				return castExpr;
			}

			static void strip( ast::ptr< ast::Expr > & expr ) {
				ast::Pass< StripCasts_new > stripper;
				expr = expr->accept( stripper );
			}
		};

		/// Swaps argument into expression pointer, saving original environment
		void swap_and_save_env( ast::ptr< ast::Expr > & expr, const ast::Expr * newExpr ) {
			ast::ptr< ast::TypeSubstitution > env = expr->env;
			expr.set_and_mutate( newExpr )->env = env;
		}

		/// Removes cast to type of argument (unlike StripCasts, also handles non-generated casts)
		void removeExtraneousCast( ast::ptr<ast::Expr> & expr ) {
			if ( const ast::CastExpr * castExpr = expr.as< ast::CastExpr >() ) {
				if ( typesCompatible( castExpr->arg->result, castExpr->result ) ) {
					// cast is to the same type as its argument, remove it
					swap_and_save_env( expr, castExpr->arg );
				}
			}
		}


	} // anonymous namespace
/// Establish post-resolver invariants for expressions
		void finishExpr(
			ast::ptr< ast::Expr > & expr, const ast::TypeEnvironment & env,
			const ast::TypeSubstitution * oldenv = nullptr
		) {
			// set up new type substitution for expression
			ast::ptr< ast::TypeSubstitution > newenv =
				 oldenv ? oldenv : new ast::TypeSubstitution{};
			env.writeToSubstitution( *newenv.get_and_mutate() );
			expr.get_and_mutate()->env = std::move( newenv );
			// remove unncecessary casts
			StripCasts_new::strip( expr );
		}

	ast::ptr< ast::Expr > resolveInVoidContext(
		const ast::Expr * expr, const ResolveContext & context,
		ast::TypeEnvironment & env
	) {
		assertf( expr, "expected a non-null expression" );

		// set up and resolve expression cast to void
		ast::ptr< ast::CastExpr > untyped = new ast::CastExpr{ expr };
		CandidateRef choice = findUnfinishedKindExpression(
			untyped, context, "", anyCandidate, ResolvMode::withAdjustment() );

		// a cast expression has either 0 or 1 interpretations (by language rules);
		// if 0, an exception has already been thrown, and this code will not run
		const ast::CastExpr * castExpr = choice->expr.strict_as< ast::CastExpr >();
		env = std::move( choice->env );

		return castExpr->arg;
	}

	/// Resolve `untyped` to the expression whose candidate is the best match for a `void`
		/// context.
		ast::ptr< ast::Expr > findVoidExpression(
			const ast::Expr * untyped, const ResolveContext & context
		) {
			ast::TypeEnvironment env;
			ast::ptr< ast::Expr > newExpr = resolveInVoidContext( untyped, context, env );
			finishExpr( newExpr, env, untyped->env );
			return newExpr;
		}

	namespace {


		/// resolve `untyped` to the expression whose candidate satisfies `pred` with the
		/// lowest cost, returning the resolved version
		ast::ptr< ast::Expr > findKindExpression(
			const ast::Expr * untyped, const ResolveContext & context,
			std::function<bool(const Candidate &)> pred = anyCandidate,
			const std::string & kind = "", ResolvMode mode = {}
		) {
			if ( ! untyped ) return {};
			CandidateRef choice =
				findUnfinishedKindExpression( untyped, context, kind, pred, mode );
			ResolvExpr::finishExpr( choice->expr, choice->env, untyped->env );
			return std::move( choice->expr );
		}

		/// Resolve `untyped` to the single expression whose candidate is the best match
		ast::ptr< ast::Expr > findSingleExpression(
			const ast::Expr * untyped, const ResolveContext & context
		) {
			Stats::ResolveTime::start( untyped );
			auto res = findKindExpression( untyped, context );
			Stats::ResolveTime::stop();
			return res;
		}
	} // anonymous namespace

	ast::ptr< ast::Expr > findSingleExpression(
		const ast::Expr * untyped, const ast::Type * type,
		const ResolveContext & context
	) {
		assert( untyped && type );
		ast::ptr< ast::Expr > castExpr = new ast::CastExpr{ untyped, type };
		ast::ptr< ast::Expr > newExpr = findSingleExpression( castExpr, context );
		removeExtraneousCast( newExpr );
		return newExpr;
	}

	namespace {
		bool structOrUnion( const Candidate & i ) {
			const ast::Type * t = i.expr->result->stripReferences();
			return dynamic_cast< const ast::StructInstType * >( t ) || dynamic_cast< const ast::UnionInstType * >( t );
		}
		/// Predicate for "Candidate has integral type"
		bool hasIntegralType( const Candidate & i ) {
			const ast::Type * type = i.expr->result;

			if ( auto bt = dynamic_cast< const ast::BasicType * >( type ) ) {
				return bt->isInteger();
			} else if (
				dynamic_cast< const ast::EnumInstType * >( type )
				|| dynamic_cast< const ast::ZeroType * >( type )
				|| dynamic_cast< const ast::OneType * >( type )
			) {
				return true;
			} else return false;
		}

		/// Resolve `untyped` as an integral expression, returning the resolved version
		ast::ptr< ast::Expr > findIntegralExpression(
			const ast::Expr * untyped, const ResolveContext & context
		) {
			return findKindExpression( untyped, context, hasIntegralType, "condition" );
		}

		/// check if a type is a character type
		bool isCharType( const ast::Type * t ) {
			if ( auto bt = dynamic_cast< const ast::BasicType * >( t ) ) {
				return bt->kind == ast::BasicType::Char
					|| bt->kind == ast::BasicType::SignedChar
					|| bt->kind == ast::BasicType::UnsignedChar;
			}
			return false;
		}

		/// Advance a type itertor to the next mutex parameter
		template<typename Iter>
		inline bool nextMutex( Iter & it, const Iter & end ) {
			while ( it != end && ! (*it)->is_mutex() ) { ++it; }
			return it != end;
		}
	}

	class Resolver_new final
	: public ast::WithSymbolTable, public ast::WithGuards,
	  public ast::WithVisitorRef<Resolver_new>, public ast::WithShortCircuiting,
	  public ast::WithStmtsToAdd<> {

		ast::ptr< ast::Type > functionReturn = nullptr;
		ast::CurrentObject currentObject;
		// for work previously in GenInit
		static InitTweak::ManagedTypes_new managedTypes;
		ResolveContext context;

		bool inEnumDecl = false;

	public:
		static size_t traceId;
		Resolver_new( const ast::TranslationGlobal & global ) :
			ast::WithSymbolTable(ast::SymbolTable::ErrorDetection::ValidateOnAdd),
			context{ symtab, global } {}
		Resolver_new( const ResolveContext & context ) :
			ast::WithSymbolTable{ context.symtab },
			context{ symtab, context.global } {}

		const ast::FunctionDecl * previsit( const ast::FunctionDecl * );
		const ast::FunctionDecl * postvisit( const ast::FunctionDecl * );
		const ast::ObjectDecl * previsit( const ast::ObjectDecl * );
		void previsit( const ast::AggregateDecl * );
		void previsit( const ast::StructDecl * );
		void previsit( const ast::EnumDecl * );
		const ast::StaticAssertDecl * previsit( const ast::StaticAssertDecl * );

		const ast::ArrayType * previsit( const ast::ArrayType * );
		const ast::PointerType * previsit( const ast::PointerType * );

		const ast::ExprStmt *        previsit( const ast::ExprStmt * );
		const ast::AsmExpr *         previsit( const ast::AsmExpr * );
		const ast::AsmStmt *         previsit( const ast::AsmStmt * );
		const ast::IfStmt *          previsit( const ast::IfStmt * );
		const ast::WhileDoStmt *     previsit( const ast::WhileDoStmt * );
		const ast::ForStmt *         previsit( const ast::ForStmt * );
		const ast::SwitchStmt *      previsit( const ast::SwitchStmt * );
		const ast::CaseClause *      previsit( const ast::CaseClause * );
		const ast::BranchStmt *      previsit( const ast::BranchStmt * );
		const ast::ReturnStmt *      previsit( const ast::ReturnStmt * );
		const ast::ThrowStmt *       previsit( const ast::ThrowStmt * );
		const ast::CatchClause *     previsit( const ast::CatchClause * );
		const ast::CatchClause *     postvisit( const ast::CatchClause * );
		const ast::WaitForStmt *     previsit( const ast::WaitForStmt * );
		const ast::WithStmt *        previsit( const ast::WithStmt * );

		const ast::SingleInit *      previsit( const ast::SingleInit * );
		const ast::ListInit *        previsit( const ast::ListInit * );
		const ast::ConstructorInit * previsit( const ast::ConstructorInit * );

		void resolveWithExprs(std::vector<ast::ptr<ast::Expr>> & exprs, std::list<ast::ptr<ast::Stmt>> & stmtsToAdd);

		void beginScope() { managedTypes.beginScope(); }
		void endScope() { managedTypes.endScope(); }
		bool on_error(ast::ptr<ast::Decl> & decl);
	};
	// size_t Resolver_new::traceId = Stats::Heap::new_stacktrace_id("Resolver");

	InitTweak::ManagedTypes_new Resolver_new::managedTypes;

	void resolve( ast::TranslationUnit& translationUnit ) {
		ast::Pass< Resolver_new >::run( translationUnit, translationUnit.global );
	}

	ast::ptr< ast::Init > resolveCtorInit(
		const ast::ConstructorInit * ctorInit, const ResolveContext & context
	) {
		assert( ctorInit );
		ast::Pass< Resolver_new > resolver( context );
		return ctorInit->accept( resolver );
	}

	const ast::Expr * resolveStmtExpr(
		const ast::StmtExpr * stmtExpr, const ResolveContext & context
	) {
		assert( stmtExpr );
		ast::Pass< Resolver_new > resolver( context );
		auto ret = mutate(stmtExpr->accept(resolver));
		strict_dynamic_cast< ast::StmtExpr * >( ret )->computeResult();
		return ret;
	}

	namespace {
		const ast::Attribute * handleAttribute(const CodeLocation & loc, const ast::Attribute * attr, const ResolveContext & context) {
			std::string name = attr->normalizedName();
			if (name == "constructor" || name == "destructor") {
				if (attr->params.size() == 1) {
					auto arg = attr->params.front();
					auto resolved = ResolvExpr::findSingleExpression( arg, new ast::BasicType( ast::BasicType::LongLongSignedInt ), context );
					auto result = eval(arg);

					auto mutAttr = mutate(attr);
					mutAttr->params.front() = resolved;
					if (! result.hasKnownValue) {
						SemanticWarning(loc, Warning::GccAttributes,
							toCString( name, " priorities must be integers from 0 to 65535 inclusive: ", arg ) );
					}
					else {
						auto priority = result.knownValue;
						if (priority < 101) {
							SemanticWarning(loc, Warning::GccAttributes,
								toCString( name, " priorities from 0 to 100 are reserved for the implementation" ) );
						} else if (priority < 201 && ! buildingLibrary()) {
							SemanticWarning(loc, Warning::GccAttributes,
								toCString( name, " priorities from 101 to 200 are reserved for the implementation" ) );
						}
					}
					return mutAttr;
				} else if (attr->params.size() > 1) {
					SemanticWarning(loc, Warning::GccAttributes, toCString( "too many arguments to ", name, " attribute" ) );
				} else {
					SemanticWarning(loc, Warning::GccAttributes, toCString( "too few arguments to ", name, " attribute" ) );
				}
			}
			return attr;
		}
	}

	const ast::FunctionDecl * Resolver_new::previsit( const ast::FunctionDecl * functionDecl ) {
		GuardValue( functionReturn );

		assert (functionDecl->unique());
		if (!functionDecl->has_body() && !functionDecl->withExprs.empty()) {
			SemanticError(functionDecl->location, functionDecl, "Function without body has with declarations");
		}

		if (!functionDecl->isTypeFixed) {
			auto mutDecl = mutate(functionDecl);
			auto mutType = mutDecl->type.get_and_mutate();

			for (auto & attr: mutDecl->attributes) {
				attr = handleAttribute(mutDecl->location, attr, context );
			}

			// handle assertions

			symtab.enterScope();
			mutType->forall.clear();
			mutType->assertions.clear();
			for (auto & typeParam : mutDecl->type_params) {
				symtab.addType(typeParam);
				mutType->forall.emplace_back(new ast::TypeInstType(typeParam));
			}
			for (auto & asst : mutDecl->assertions) {
				asst = fixObjectType(asst.strict_as<ast::ObjectDecl>(), context);
				symtab.addId(asst);
				mutType->assertions.emplace_back(new ast::VariableExpr(functionDecl->location, asst));
			}

			// temporarily adds params to symbol table.
			// actual scoping rules for params and withexprs differ - see Pass::visit(FunctionDecl)

			std::vector<ast::ptr<ast::Type>> paramTypes;
			std::vector<ast::ptr<ast::Type>> returnTypes;

			for (auto & param : mutDecl->params) {
				param = fixObjectType(param.strict_as<ast::ObjectDecl>(), context);
				symtab.addId(param);
				paramTypes.emplace_back(param->get_type());
			}
			for (auto & ret : mutDecl->returns) {
				ret = fixObjectType(ret.strict_as<ast::ObjectDecl>(), context);
				returnTypes.emplace_back(ret->get_type());
			}
			// since function type in decl is just a view of param types, need to update that as well
			mutType->params = std::move(paramTypes);
			mutType->returns = std::move(returnTypes);

			auto renamedType = strict_dynamic_cast<const ast::FunctionType *>(renameTyVars(mutType, RenameMode::GEN_EXPR_ID));

			std::list<ast::ptr<ast::Stmt>> newStmts;
			resolveWithExprs (mutDecl->withExprs, newStmts);

			if (mutDecl->stmts) {
				auto mutStmt = mutDecl->stmts.get_and_mutate();
				mutStmt->kids.splice(mutStmt->kids.begin(), std::move(newStmts));
				mutDecl->stmts = mutStmt;
			}

			symtab.leaveScope();

			mutDecl->type = renamedType;
			mutDecl->mangleName = Mangle::mangle(mutDecl);
			mutDecl->isTypeFixed = true;
			functionDecl = mutDecl;
		}
		managedTypes.handleDWT(functionDecl);

		functionReturn = extractResultType( functionDecl->type );
		return functionDecl;
	}

	const ast::FunctionDecl * Resolver_new::postvisit( const ast::FunctionDecl * functionDecl ) {
		// default value expressions have an environment which shouldn't be there and trips up
		// later passes.
		assert( functionDecl->unique() );
		ast::FunctionType * mutType = mutate( functionDecl->type.get() );

		for ( unsigned i = 0 ; i < mutType->params.size() ; ++i ) {
			if ( const ast::ObjectDecl * obj = mutType->params[i].as< ast::ObjectDecl >() ) {
				if ( const ast::SingleInit * init = obj->init.as< ast::SingleInit >() ) {
					if ( init->value->env == nullptr ) continue;
					// clone initializer minus the initializer environment
					auto mutParam = mutate( mutType->params[i].strict_as< ast::ObjectDecl >() );
					auto mutInit = mutate( mutParam->init.strict_as< ast::SingleInit >() );
					auto mutValue = mutate( mutInit->value.get() );

					mutValue->env = nullptr;
					mutInit->value = mutValue;
					mutParam->init = mutInit;
					mutType->params[i] = mutParam;

					assert( ! mutType->params[i].strict_as< ast::ObjectDecl >()->init.strict_as< ast::SingleInit >()->value->env);
				}
			}
		}
		mutate_field(functionDecl, &ast::FunctionDecl::type, mutType);
		return functionDecl;
	}

	const ast::ObjectDecl * Resolver_new::previsit( const ast::ObjectDecl * objectDecl ) {
		// To handle initialization of routine pointers [e.g. int (*fp)(int) = foo()],
		// class-variable `initContext` is changed multiple times because the LHS is analyzed
		// twice. The second analysis changes `initContext` because a function type can contain
		// object declarations in the return and parameter types. Therefore each value of
		// `initContext` is retained so the type on the first analysis is preserved and used for
		// selecting the RHS.
		GuardValue( currentObject );

		if ( inEnumDecl && dynamic_cast< const ast::EnumInstType * >( objectDecl->get_type() ) ) {
			// enumerator initializers should not use the enum type to initialize, since the
			// enum type is still incomplete at this point. Use `int` instead.

			if ( auto enumBase = dynamic_cast< const ast::EnumInstType * >
				( objectDecl->get_type() )->base->base ) {
				objectDecl = fixObjectType( objectDecl, context );
				currentObject = ast::CurrentObject{ 
					objectDecl->location, 
					enumBase
				};
			} else {
				objectDecl = fixObjectType( objectDecl, context );
				currentObject = ast::CurrentObject{
					objectDecl->location, new ast::BasicType{ ast::BasicType::SignedInt } };
			}

		}
		else {
			if ( !objectDecl->isTypeFixed ) {
				auto newDecl = fixObjectType(objectDecl, context);
				auto mutDecl = mutate(newDecl);

				// generate CtorInit wrapper when necessary.
				// in certain cases, fixObjectType is called before reaching
				// this object in visitor pass, thus disabling CtorInit codegen.
				// this happens on aggregate members and function parameters.
				if ( InitTweak::tryConstruct( mutDecl ) && ( managedTypes.isManaged( mutDecl ) || ((! isInFunction() || mutDecl->storage.is_static ) && ! InitTweak::isConstExpr( mutDecl->init ) ) ) ) {
					// constructed objects cannot be designated
					// if ( InitTweak::isDesignated( mutDecl->init ) ) SemanticError( mutDecl, "Cannot include designations in the initializer for a managed Object. If this is really what you want, then initialize with @=.\n" );
					if ( InitTweak::isDesignated( mutDecl->init ) ) {
						SemanticError( mutDecl, "Cannot include designations in the initializer for a managed Object. If this is really what you want, then initialize with @=.\n" );
					}
					// constructed objects should not have initializers nested too deeply
					if ( ! InitTweak::checkInitDepth( mutDecl ) ) SemanticError( mutDecl, "Managed object's initializer is too deep " );

					mutDecl->init = InitTweak::genCtorInit( mutDecl->location, mutDecl );
				}

				objectDecl = mutDecl;
			}
			currentObject = ast::CurrentObject{ objectDecl->location, objectDecl->get_type() };
		}

		return objectDecl;
	}

	void Resolver_new::previsit( const ast::AggregateDecl * _aggDecl ) {
		auto aggDecl = mutate(_aggDecl);
		assertf(aggDecl == _aggDecl, "type declarations must be unique");

		for (auto & member: aggDecl->members) {
			// nested type decls are hoisted already. no need to do anything
			if (auto obj = member.as<ast::ObjectDecl>()) {
				member = fixObjectType(obj, context);
			}
		}
	}

	void Resolver_new::previsit( const ast::StructDecl * structDecl ) {
		previsit(static_cast<const ast::AggregateDecl *>(structDecl));
		managedTypes.handleStruct(structDecl);
	}

	void Resolver_new::previsit( const ast::EnumDecl * ) {
		// in case we decide to allow nested enums
		GuardValue( inEnumDecl );
		inEnumDecl = true;
		// don't need to fix types for enum fields
	}

	const ast::StaticAssertDecl * Resolver_new::previsit(
		const ast::StaticAssertDecl * assertDecl
	) {
		return ast::mutate_field(
			assertDecl, &ast::StaticAssertDecl::cond,
			findIntegralExpression( assertDecl->cond, context ) );
	}

	template< typename PtrType >
	const PtrType * handlePtrType( const PtrType * type, const ResolveContext & context ) {
		if ( type->dimension ) {
			const ast::Type * sizeType = context.global.sizeType.get();
			ast::ptr< ast::Expr > dimension = findSingleExpression( type->dimension, sizeType, context );
			assertf(dimension->env->empty(), "array dimension expr has nonempty env");
			dimension.get_and_mutate()->env = nullptr;
			ast::mutate_field( type, &PtrType::dimension, dimension );
		}
		return type;
	}

	const ast::ArrayType * Resolver_new::previsit( const ast::ArrayType * at ) {
		return handlePtrType( at, context );
	}

	const ast::PointerType * Resolver_new::previsit( const ast::PointerType * pt ) {
		return handlePtrType( pt, context );
	}

	const ast::ExprStmt * Resolver_new::previsit( const ast::ExprStmt * exprStmt ) {
		visit_children = false;
		assertf( exprStmt->expr, "ExprStmt has null expression in resolver" );

		return ast::mutate_field(
			exprStmt, &ast::ExprStmt::expr, findVoidExpression( exprStmt->expr, context ) );
	}

	const ast::AsmExpr * Resolver_new::previsit( const ast::AsmExpr * asmExpr ) {
		visit_children = false;

		asmExpr = ast::mutate_field(
			asmExpr, &ast::AsmExpr::operand, findVoidExpression( asmExpr->operand, context ) );

		return asmExpr;
	}

	const ast::AsmStmt * Resolver_new::previsit( const ast::AsmStmt * asmStmt ) {
		visitor->maybe_accept( asmStmt, &ast::AsmStmt::input );
		visitor->maybe_accept( asmStmt, &ast::AsmStmt::output );
		visit_children = false;
		return asmStmt;
	}

	const ast::IfStmt * Resolver_new::previsit( const ast::IfStmt * ifStmt ) {
		return ast::mutate_field(
			ifStmt, &ast::IfStmt::cond, findIntegralExpression( ifStmt->cond, context ) );
	}

	const ast::WhileDoStmt * Resolver_new::previsit( const ast::WhileDoStmt * whileDoStmt ) {
		return ast::mutate_field(
			whileDoStmt, &ast::WhileDoStmt::cond, findIntegralExpression( whileDoStmt->cond, context ) );
	}

	const ast::ForStmt * Resolver_new::previsit( const ast::ForStmt * forStmt ) {
		if ( forStmt->cond ) {
			forStmt = ast::mutate_field(
				forStmt, &ast::ForStmt::cond, findIntegralExpression( forStmt->cond, context ) );
		}

		if ( forStmt->inc ) {
			forStmt = ast::mutate_field(
				forStmt, &ast::ForStmt::inc, findVoidExpression( forStmt->inc, context ) );
		}

		return forStmt;
	}

	const ast::SwitchStmt * Resolver_new::previsit( const ast::SwitchStmt * switchStmt ) {
		GuardValue( currentObject );
		switchStmt = ast::mutate_field(
			switchStmt, &ast::SwitchStmt::cond,
			findIntegralExpression( switchStmt->cond, context ) );
		currentObject = ast::CurrentObject{ switchStmt->location, switchStmt->cond->result };
		return switchStmt;
	}

	const ast::CaseClause * Resolver_new::previsit( const ast::CaseClause * caseStmt ) {
		if ( caseStmt->cond ) {
			std::deque< ast::InitAlternative > initAlts = currentObject.getOptions();
			assertf( initAlts.size() == 1, "SwitchStmt did not correctly resolve an integral "
				"expression." );

			ast::ptr< ast::Expr > untyped =
				new ast::CastExpr{ caseStmt->location, caseStmt->cond, initAlts.front().type };
			ast::ptr< ast::Expr > newExpr = findSingleExpression( untyped, context );

			// case condition cannot have a cast in C, so it must be removed here, regardless of
			// whether it would perform a conversion.
			if ( const ast::CastExpr * castExpr = newExpr.as< ast::CastExpr >() ) {
				swap_and_save_env( newExpr, castExpr->arg );
			}

			caseStmt = ast::mutate_field( caseStmt, &ast::CaseClause::cond, newExpr );
		}
		return caseStmt;
	}

	const ast::BranchStmt * Resolver_new::previsit( const ast::BranchStmt * branchStmt ) {
		visit_children = false;
		// must resolve the argument of a computed goto
		if ( branchStmt->kind == ast::BranchStmt::Goto && branchStmt->computedTarget ) {
			// computed goto argument is void*
			ast::ptr< ast::Type > target = new ast::PointerType{ new ast::VoidType{} };
			branchStmt = ast::mutate_field(
				branchStmt, &ast::BranchStmt::computedTarget,
				findSingleExpression( branchStmt->computedTarget, target, context ) );
		}
		return branchStmt;
	}

	const ast::ReturnStmt * Resolver_new::previsit( const ast::ReturnStmt * returnStmt ) {
		visit_children = false;
		if ( returnStmt->expr ) {
			returnStmt = ast::mutate_field(
				returnStmt, &ast::ReturnStmt::expr,
				findSingleExpression( returnStmt->expr, functionReturn, context ) );
		}
		return returnStmt;
	}

	const ast::ThrowStmt * Resolver_new::previsit( const ast::ThrowStmt * throwStmt ) {
		visit_children = false;
		if ( throwStmt->expr ) {
			const ast::StructDecl * exceptionDecl =
				symtab.lookupStruct( "__cfaehm_base_exception_t" );
			assert( exceptionDecl );
			ast::ptr< ast::Type > exceptType =
				new ast::PointerType{ new ast::StructInstType{ exceptionDecl } };
			throwStmt = ast::mutate_field(
				throwStmt, &ast::ThrowStmt::expr,
				findSingleExpression( throwStmt->expr, exceptType, context ) );
		}
		return throwStmt;
	}

	const ast::CatchClause * Resolver_new::previsit( const ast::CatchClause * catchClause ) {
		// Until we are very sure this invarent (ifs that move between passes have then)
		// holds, check it. This allows a check for when to decode the mangling.
		if ( auto ifStmt = catchClause->body.as<ast::IfStmt>() ) {
			assert( ifStmt->then );
		}
		// Encode the catchStmt so the condition can see the declaration.
		if ( catchClause->cond ) {
			ast::CatchClause * clause = mutate( catchClause );
			clause->body = new ast::IfStmt( clause->location, clause->cond, nullptr, clause->body );
			clause->cond = nullptr;
			return clause;
		}
		return catchClause;
	}

	const ast::CatchClause * Resolver_new::postvisit( const ast::CatchClause * catchClause ) {
		// Decode the catchStmt so everything is stored properly.
		const ast::IfStmt * ifStmt = catchClause->body.as<ast::IfStmt>();
		if ( nullptr != ifStmt && nullptr == ifStmt->then ) {
			assert( ifStmt->cond );
			assert( ifStmt->else_ );
			ast::CatchClause * clause = ast::mutate( catchClause );
			clause->cond = ifStmt->cond;
			clause->body = ifStmt->else_;
			// ifStmt should be implicately deleted here.
			return clause;
		}
		return catchClause;
	}

	const ast::WaitForStmt * Resolver_new::previsit( const ast::WaitForStmt * stmt ) {
		visit_children = false;

		// Resolve all clauses first
		for ( unsigned i = 0; i < stmt->clauses.size(); ++i ) {
			const ast::WaitForClause & clause = *stmt->clauses[i];

			ast::TypeEnvironment env;
			CandidateFinder funcFinder( context, env );

			// Find all candidates for a function in canonical form
			funcFinder.find( clause.target, ResolvMode::withAdjustment() );

			if ( funcFinder.candidates.empty() ) {
				stringstream ss;
				ss << "Use of undeclared indentifier '";
				ss << clause.target.strict_as< ast::NameExpr >()->name;
				ss << "' in call to waitfor";
				SemanticError( stmt->location, ss.str() );
			}

			if ( clause.target_args.empty() ) {
				SemanticError( stmt->location,
					"Waitfor clause must have at least one mutex parameter");
			}

			// Find all alternatives for all arguments in canonical form
			std::vector< CandidateFinder > argFinders =
				funcFinder.findSubExprs( clause.target_args );

			// List all combinations of arguments
			std::vector< CandidateList > possibilities;
			combos( argFinders.begin(), argFinders.end(), back_inserter( possibilities ) );

			// For every possible function:
			// * try matching the arguments to the parameters, not the other way around because
			//   more arguments than parameters
			CandidateList funcCandidates;
			std::vector< CandidateList > argsCandidates;
			SemanticErrorException errors;
			for ( CandidateRef & func : funcFinder.candidates ) {
				try {
					auto pointerType = dynamic_cast< const ast::PointerType * >(
						func->expr->result->stripReferences() );
					if ( ! pointerType ) {
						SemanticError( stmt->location, func->expr->result.get(),
							"candidate not viable: not a pointer type\n" );
					}

					auto funcType = pointerType->base.as< ast::FunctionType >();
					if ( ! funcType ) {
						SemanticError( stmt->location, func->expr->result.get(),
							"candidate not viable: not a function type\n" );
					}

					{
						auto param    = funcType->params.begin();
						auto paramEnd = funcType->params.end();

						if( ! nextMutex( param, paramEnd ) ) {
							SemanticError( stmt->location, funcType,
								"candidate function not viable: no mutex parameters\n");
						}
					}

					CandidateRef func2{ new Candidate{ *func } };
					// strip reference from function
					func2->expr = referenceToRvalueConversion( func->expr, func2->cost );

					// Each argument must be matched with a parameter of the current candidate
					for ( auto & argsList : possibilities ) {
						try {
							// Declare data structures needed for resolution
							ast::OpenVarSet open;
							ast::AssertionSet need, have;
							ast::TypeEnvironment resultEnv{ func->env };
							// Add all type variables as open so that those not used in the
							// parameter list are still considered open
							resultEnv.add( funcType->forall );

							// load type variables from arguments into one shared space
							for ( auto & arg : argsList ) {
								resultEnv.simpleCombine( arg->env );
							}

							// Make sure we don't widen any existing bindings
							resultEnv.forbidWidening();

							// Find any unbound type variables
							resultEnv.extractOpenVars( open );

							auto param = funcType->params.begin();
							auto paramEnd = funcType->params.end();

							unsigned n_mutex_param = 0;

							// For every argument of its set, check if it matches one of the
							// parameters. The order is important
							for ( auto & arg : argsList ) {
								// Ignore non-mutex arguments
								if ( ! nextMutex( param, paramEnd ) ) {
									// We ran out of parameters but still have arguments.
									// This function doesn't match
									SemanticError( stmt->location, funcType,
										toString("candidate function not viable: too many mutex "
										"arguments, expected ", n_mutex_param, "\n" ) );
								}

								++n_mutex_param;

								// Check if the argument matches the parameter type in the current
								// scope
								// ast::ptr< ast::Type > paramType = (*param)->get_type();
								if (
									! unify(
										arg->expr->result, *param, resultEnv, need, have, open )
								) {
									// Type doesn't match
									stringstream ss;
									ss << "candidate function not viable: no known conversion "
										"from '";
									ast::print( ss, *param );
									ss << "' to '";
									ast::print( ss, arg->expr->result );
									ss << "' with env '";
									ast::print( ss, resultEnv );
									ss << "'\n";
									SemanticError( stmt->location, funcType, ss.str() );
								}

								++param;
							}

							// All arguments match!

							// Check if parameters are missing
							if ( nextMutex( param, paramEnd ) ) {
								do {
									++n_mutex_param;
									++param;
								} while ( nextMutex( param, paramEnd ) );

								// We ran out of arguments but still have parameters left; this
								// function doesn't match
								SemanticError( stmt->location, funcType,
									toString( "candidate function not viable: too few mutex "
									"arguments, expected ", n_mutex_param, "\n" ) );
							}

							// All parameters match!

							// Finish the expressions to tie in proper environments
							finishExpr( func2->expr, resultEnv );
							for ( CandidateRef & arg : argsList ) {
								finishExpr( arg->expr, resultEnv );
							}

							// This is a match, store it and save it for later
							funcCandidates.emplace_back( std::move( func2 ) );
							argsCandidates.emplace_back( std::move( argsList ) );

						} catch ( SemanticErrorException & e ) {
							errors.append( e );
						}
					}
				} catch ( SemanticErrorException & e ) {
					errors.append( e );
				}
			}

			// Make sure correct number of arguments
			if( funcCandidates.empty() ) {
				SemanticErrorException top( stmt->location,
					"No alternatives for function in call to waitfor" );
				top.append( errors );
				throw top;
			}

			if( argsCandidates.empty() ) {
				SemanticErrorException top( stmt->location,
					"No alternatives for arguments in call to waitfor" );
				top.append( errors );
				throw top;
			}

			if( funcCandidates.size() > 1 ) {
				SemanticErrorException top( stmt->location,
					"Ambiguous function in call to waitfor" );
				top.append( errors );
				throw top;
			}
			if( argsCandidates.size() > 1 ) {
				SemanticErrorException top( stmt->location,
					"Ambiguous arguments in call to waitfor" );
				top.append( errors );
				throw top;
			}
			// TODO: need to use findDeletedExpr to ensure no deleted identifiers are used.

			// build new clause
			auto clause2 = new ast::WaitForClause( clause.location );

			clause2->target = funcCandidates.front()->expr;

			clause2->target_args.reserve( clause.target_args.size() );
			const ast::StructDecl * decl_monitor = symtab.lookupStruct( "monitor$" );
			for ( auto arg : argsCandidates.front() ) {
				const auto & loc = stmt->location;

				ast::Expr * init = new ast::CastExpr( loc,
					new ast::UntypedExpr( loc,
						new ast::NameExpr( loc, "get_monitor" ),
						{ arg->expr }
					),
					new ast::PointerType(
						new ast::StructInstType(
							decl_monitor
						)
					)
				);

				clause2->target_args.emplace_back( findSingleExpression( init, context ) );
			}

			// Resolve the conditions as if it were an IfStmt, statements normally
			clause2->when_cond = findSingleExpression( clause.when_cond, context );
			clause2->stmt = clause.stmt->accept( *visitor );

			// set results into stmt
			auto n = mutate( stmt );
			n->clauses[i] = clause2;
			stmt = n;
		}

		if ( stmt->timeout_stmt ) {
			// resolve the timeout as a size_t, the conditions like IfStmt, and stmts normally
			ast::ptr< ast::Type > target =
				new ast::BasicType{ ast::BasicType::LongLongUnsignedInt };
			auto timeout_time = findSingleExpression( stmt->timeout_time, target, context );
			auto timeout_cond = findSingleExpression( stmt->timeout_cond, context );
			auto timeout_stmt = stmt->timeout_stmt->accept( *visitor );

			// set results into stmt
			auto n = mutate( stmt );
			n->timeout_time = std::move( timeout_time );
			n->timeout_cond = std::move( timeout_cond );
			n->timeout_stmt = std::move( timeout_stmt );
			stmt = n;
		}

		if ( stmt->else_stmt ) {
			// resolve the condition like IfStmt, stmts normally
			auto else_cond = findSingleExpression( stmt->else_cond, context );
			auto else_stmt = stmt->else_stmt->accept( *visitor );

			// set results into stmt
			auto n = mutate( stmt );
			n->else_cond = std::move( else_cond );
			n->else_stmt = std::move( else_stmt );
			stmt = n;
		}

		return stmt;
	}

	const ast::WithStmt * Resolver_new::previsit( const ast::WithStmt * withStmt ) {
		auto mutStmt = mutate(withStmt);
		resolveWithExprs(mutStmt->exprs, stmtsToAddBefore);
		return mutStmt;
	}

	void Resolver_new::resolveWithExprs(std::vector<ast::ptr<ast::Expr>> & exprs, std::list<ast::ptr<ast::Stmt>> & stmtsToAdd) {
		for (auto & expr : exprs) {
			// only struct- and union-typed expressions are viable candidates
			expr = findKindExpression( expr, context, structOrUnion, "with expression" );

			// if with expression might be impure, create a temporary so that it is evaluated once
			if ( Tuples::maybeImpure( expr ) ) {
				static UniqueName tmpNamer( "_with_tmp_" );
				const CodeLocation loc = expr->location;
				auto tmp = new ast::ObjectDecl(loc, tmpNamer.newName(), expr->result, new ast::SingleInit(loc, expr ) );
				expr = new ast::VariableExpr( loc, tmp );
				stmtsToAdd.push_back( new ast::DeclStmt(loc, tmp ) );
				if ( InitTweak::isConstructable( tmp->type ) ) {
					// generate ctor/dtor and resolve them
					tmp->init = InitTweak::genCtorInit( loc, tmp );
				}
				// since tmp is freshly created, this should modify tmp in-place
				tmp->accept( *visitor );
			}
			else if (expr->env && expr->env->empty()) {
				expr = ast::mutate_field(expr.get(), &ast::Expr::env, nullptr);
			}
		}
	}


	const ast::SingleInit * Resolver_new::previsit( const ast::SingleInit * singleInit ) {
		visit_children = false;
		// resolve initialization using the possibilities as determined by the `currentObject`
		// cursor.
		ast::ptr< ast::Expr > untyped = new ast::UntypedInitExpr{
			singleInit->location, singleInit->value, currentObject.getOptions() };
		ast::ptr<ast::Expr> newExpr = findSingleExpression( untyped, context );
		const ast::InitExpr * initExpr = newExpr.strict_as< ast::InitExpr >();

		// move cursor to the object that is actually initialized
		currentObject.setNext( initExpr->designation );

		// discard InitExpr wrapper and retain relevant pieces.
		// `initExpr` may have inferred params in the case where the expression specialized a
		// function pointer, and newExpr may already have inferParams of its own, so a simple
		// swap is not sufficient
		ast::Expr::InferUnion inferred = initExpr->inferred;
		swap_and_save_env( newExpr, initExpr->expr );
		newExpr.get_and_mutate()->inferred.splice( std::move(inferred) );

		// get the actual object's type (may not exactly match what comes back from the resolver
		// due to conversions)
		const ast::Type * initContext = currentObject.getCurrentType();

		removeExtraneousCast( newExpr );

		// check if actual object's type is char[]
		if ( auto at = dynamic_cast< const ast::ArrayType * >( initContext ) ) {
			if ( isCharType( at->base ) ) {
				// check if the resolved type is char*
				if ( auto pt = newExpr->result.as< ast::PointerType >() ) {
					if ( isCharType( pt->base ) ) {
						// strip cast if we're initializing a char[] with a char*
						// e.g. char x[] = "hello"
						if ( auto ce = newExpr.as< ast::CastExpr >() ) {
							swap_and_save_env( newExpr, ce->arg );
						}
					}
				}
			}
		}

		// move cursor to next object in preparation for next initializer
		currentObject.increment();

		// set initializer expression to resolved expression
		return ast::mutate_field( singleInit, &ast::SingleInit::value, std::move(newExpr) );
	}

	const ast::ListInit * Resolver_new::previsit( const ast::ListInit * listInit ) {
		// move cursor into brace-enclosed initializer-list
		currentObject.enterListInit( listInit->location );

		assert( listInit->designations.size() == listInit->initializers.size() );
		for ( unsigned i = 0; i < listInit->designations.size(); ++i ) {
			// iterate designations and initializers in pairs, moving the cursor to the current
			// designated object and resolving the initializer against that object
			listInit = ast::mutate_field_index(
				listInit, &ast::ListInit::designations, i,
				currentObject.findNext( listInit->designations[i] ) );
			listInit = ast::mutate_field_index(
				listInit, &ast::ListInit::initializers, i,
				listInit->initializers[i]->accept( *visitor ) );
		}

		// move cursor out of brace-enclosed initializer-list
		currentObject.exitListInit();

		visit_children = false;
		return listInit;
	}

	const ast::ConstructorInit * Resolver_new::previsit( const ast::ConstructorInit * ctorInit ) {
		visitor->maybe_accept( ctorInit, &ast::ConstructorInit::ctor );
		visitor->maybe_accept( ctorInit, &ast::ConstructorInit::dtor );

		// found a constructor - can get rid of C-style initializer
		// xxx - Rob suggests this field is dead code
		ctorInit = ast::mutate_field( ctorInit, &ast::ConstructorInit::init, nullptr );

		// intrinsic single-parameter constructors and destructors do nothing. Since this was
		// implicitly generated, there's no way for it to have side effects, so get rid of it to
		// clean up generated code
		if ( InitTweak::isIntrinsicSingleArgCallStmt( ctorInit->ctor ) ) {
			ctorInit = ast::mutate_field( ctorInit, &ast::ConstructorInit::ctor, nullptr );
		}
		if ( InitTweak::isIntrinsicSingleArgCallStmt( ctorInit->dtor ) ) {
			ctorInit = ast::mutate_field( ctorInit, &ast::ConstructorInit::dtor, nullptr );
		}

		return ctorInit;
	}

	// suppress error on autogen functions and mark invalid autogen as deleted.
	bool Resolver_new::on_error(ast::ptr<ast::Decl> & decl) {
		if (auto functionDecl = decl.as<ast::FunctionDecl>()) {
			// xxx - can intrinsic gen ever fail?
			if (functionDecl->linkage == ast::Linkage::AutoGen) {
				auto mutDecl = mutate(functionDecl);
				mutDecl->isDeleted = true;
				mutDecl->stmts = nullptr;
				decl = mutDecl;
				return false;
			}
		}
		return true;
	}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
