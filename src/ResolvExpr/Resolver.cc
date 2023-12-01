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

#include "Candidate.hpp"
#include "CandidateFinder.hpp"
#include "CurrentObject.h"               // for CurrentObject
#include "RenameVars.h"                  // for RenameVars, global_renamer
#include "Resolver.h"
#include "ResolveTypeof.h"
#include "ResolveMode.hpp"               // for ResolveMode
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
#include "Common/SemanticError.h"        // for SemanticError
#include "Common/Stats/ResolveTime.h"    // for ResolveTime::start(), ResolveTime::stop()
#include "Common/ToString.hpp"           // for toCString
#include "Common/UniqueName.h"           // for UniqueName
#include "InitTweak/GenInit.h"
#include "InitTweak/InitTweak.h"         // for isIntrinsicSingleArgCallStmt
#include "SymTab/Mangler.h"              // for Mangler
#include "Tuples/Tuples.h"
#include "Validate/FindSpecialDecls.h"   // for SizeType

using namespace std;

namespace ResolvExpr {
	template< typename iterator_t >
	inline bool advance_to_mutex( iterator_t & it, const iterator_t & end ) {
		while( it != end && !(*it)->get_type()->get_mutex() ) {
			it++;
		}

		return it != end;
	}

	namespace {
		/// Finds deleted expressions in an expression tree
		struct DeleteFinder final : public ast::WithShortCircuiting, public ast::WithVisitorRef<DeleteFinder> {
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

		struct ResolveDesignators final : public ast::WithShortCircuiting {
			ResolveContext& context;
			bool result = false;

			ResolveDesignators( ResolveContext& _context ): context{_context} {};

			void previsit( const ast::Node * ) {
				// short circuit if we already know there are designations
				if ( result ) visit_children = false;
			}

			void previsit( const ast::Designation * des ) {
				if ( result ) visit_children = false;
				else if ( ! des->designators.empty() ) {
					if ( (des->designators.size() == 1) ) {
						const ast::Expr * designator = des->designators.at(0);
						if ( const ast::NameExpr * designatorName = dynamic_cast<const ast::NameExpr *>(designator) ) {
							auto candidates = context.symtab.lookupId(designatorName->name);
							for ( auto candidate : candidates ) {
								if ( dynamic_cast<const ast::EnumInstType *>(candidate.id->get_type()) ) {
									result = true;
									break;
								}
							}
						} 
					} 
					visit_children = false;
				}
			}
		};
	} // anonymous namespace
	/// Check if this expression is or includes a deleted expression
	const ast::DeletedExpr * findDeletedExpr( const ast::Expr * expr ) {
		return ast::Pass<DeleteFinder>::read( expr );
	}

	namespace {
		/// always-accept candidate filter
		bool anyCandidate( const Candidate & ) { return true; }

		/// Calls the CandidateFinder and finds the single best candidate
		CandidateRef findUnfinishedKindExpression(
			const ast::Expr * untyped, const ResolveContext & context, const std::string & kind,
			std::function<bool(const Candidate &)> pred = anyCandidate, ResolveMode mode = {}
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
		struct StripCasts final {
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
				ast::Pass< StripCasts > stripper;
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
			StripCasts::strip( expr );
		}

	ast::ptr< ast::Expr > resolveInVoidContext(
		const ast::Expr * expr, const ResolveContext & context,
		ast::TypeEnvironment & env
	) {
		assertf( expr, "expected a non-null expression" );

		// set up and resolve expression cast to void
		ast::ptr< ast::CastExpr > untyped = new ast::CastExpr{ expr };
		CandidateRef choice = findUnfinishedKindExpression(
			untyped, context, "", anyCandidate, ResolveMode::withAdjustment() );

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
			const std::string & kind = "", ResolveMode mode = {}
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

	class Resolver final
	: public ast::WithSymbolTable, public ast::WithGuards,
	  public ast::WithVisitorRef<Resolver>, public ast::WithShortCircuiting,
	  public ast::WithStmtsToAdd<> {

		ast::ptr< ast::Type > functionReturn = nullptr;
		ast::CurrentObject currentObject;
		// for work previously in GenInit
		static InitTweak::ManagedTypes managedTypes;
		ResolveContext context;

		bool inEnumDecl = false;

	public:
		static size_t traceId;
		Resolver( const ast::TranslationGlobal & global ) :
			ast::WithSymbolTable(ast::SymbolTable::ErrorDetection::ValidateOnAdd),
			context{ symtab, global } {}
		Resolver( const ResolveContext & context ) :
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
	// size_t Resolver::traceId = Stats::Heap::new_stacktrace_id("Resolver");

	InitTweak::ManagedTypes Resolver::managedTypes;

	void resolve( ast::TranslationUnit& translationUnit ) {
		ast::Pass< Resolver >::run( translationUnit, translationUnit.global );
	}

	ast::ptr< ast::Init > resolveCtorInit(
		const ast::ConstructorInit * ctorInit, const ResolveContext & context
	) {
		assert( ctorInit );
		ast::Pass< Resolver > resolver( context );
		return ctorInit->accept( resolver );
	}

	const ast::Expr * resolveStmtExpr(
		const ast::StmtExpr * stmtExpr, const ResolveContext & context
	) {
		assert( stmtExpr );
		ast::Pass< Resolver > resolver( context );
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

	const ast::FunctionDecl * Resolver::previsit( const ast::FunctionDecl * functionDecl ) {
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

	const ast::FunctionDecl * Resolver::postvisit( const ast::FunctionDecl * functionDecl ) {
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

	const ast::ObjectDecl * Resolver::previsit( const ast::ObjectDecl * objectDecl ) {
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
					if ( InitTweak::isDesignated( mutDecl->init ) ) {
						ast::Pass<ResolveDesignators> res( context );
						maybe_accept( mutDecl->init.get(), res );
						if ( !res.core.result ) {
							SemanticError( mutDecl, "Cannot include designations in the initializer for a managed Object. If this is really what you want, then initialize with @=.\n" );
						}
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

	void Resolver::previsit( const ast::AggregateDecl * _aggDecl ) {
		auto aggDecl = mutate(_aggDecl);
		assertf(aggDecl == _aggDecl, "type declarations must be unique");

		for (auto & member: aggDecl->members) {
			// nested type decls are hoisted already. no need to do anything
			if (auto obj = member.as<ast::ObjectDecl>()) {
				member = fixObjectType(obj, context);
			}
		}
	}

	void Resolver::previsit( const ast::StructDecl * structDecl ) {
		previsit(static_cast<const ast::AggregateDecl *>(structDecl));
		managedTypes.handleStruct(structDecl);
	}

	void Resolver::previsit( const ast::EnumDecl * ) {
		// in case we decide to allow nested enums
		GuardValue( inEnumDecl );
		inEnumDecl = true;
		// don't need to fix types for enum fields
	}

	const ast::StaticAssertDecl * Resolver::previsit(
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

	const ast::ArrayType * Resolver::previsit( const ast::ArrayType * at ) {
		return handlePtrType( at, context );
	}

	const ast::PointerType * Resolver::previsit( const ast::PointerType * pt ) {
		return handlePtrType( pt, context );
	}

	const ast::ExprStmt * Resolver::previsit( const ast::ExprStmt * exprStmt ) {
		visit_children = false;
		assertf( exprStmt->expr, "ExprStmt has null expression in resolver" );

		return ast::mutate_field(
			exprStmt, &ast::ExprStmt::expr, findVoidExpression( exprStmt->expr, context ) );
	}

	const ast::AsmExpr * Resolver::previsit( const ast::AsmExpr * asmExpr ) {
		visit_children = false;

		asmExpr = ast::mutate_field(
			asmExpr, &ast::AsmExpr::operand, findVoidExpression( asmExpr->operand, context ) );

		return asmExpr;
	}

	const ast::AsmStmt * Resolver::previsit( const ast::AsmStmt * asmStmt ) {
		visitor->maybe_accept( asmStmt, &ast::AsmStmt::input );
		visitor->maybe_accept( asmStmt, &ast::AsmStmt::output );
		visit_children = false;
		return asmStmt;
	}

	const ast::IfStmt * Resolver::previsit( const ast::IfStmt * ifStmt ) {
		return ast::mutate_field(
			ifStmt, &ast::IfStmt::cond, findIntegralExpression( ifStmt->cond, context ) );
	}

	const ast::WhileDoStmt * Resolver::previsit( const ast::WhileDoStmt * whileDoStmt ) {
		return ast::mutate_field(
			whileDoStmt, &ast::WhileDoStmt::cond, findIntegralExpression( whileDoStmt->cond, context ) );
	}

	const ast::ForStmt * Resolver::previsit( const ast::ForStmt * forStmt ) {
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

	const ast::SwitchStmt * Resolver::previsit( const ast::SwitchStmt * switchStmt ) {
		GuardValue( currentObject );
		switchStmt = ast::mutate_field(
			switchStmt, &ast::SwitchStmt::cond,
			findIntegralExpression( switchStmt->cond, context ) );
		currentObject = ast::CurrentObject{ switchStmt->location, switchStmt->cond->result };
		return switchStmt;
	}

	const ast::CaseClause * Resolver::previsit( const ast::CaseClause * caseStmt ) {
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

	const ast::BranchStmt * Resolver::previsit( const ast::BranchStmt * branchStmt ) {
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

	const ast::ReturnStmt * Resolver::previsit( const ast::ReturnStmt * returnStmt ) {
		visit_children = false;
		if ( returnStmt->expr ) {
			returnStmt = ast::mutate_field(
				returnStmt, &ast::ReturnStmt::expr,
				findSingleExpression( returnStmt->expr, functionReturn, context ) );
		}
		return returnStmt;
	}

	const ast::ThrowStmt * Resolver::previsit( const ast::ThrowStmt * throwStmt ) {
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

	const ast::CatchClause * Resolver::previsit( const ast::CatchClause * catchClause ) {
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

	const ast::CatchClause * Resolver::postvisit( const ast::CatchClause * catchClause ) {
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

	const ast::WaitForStmt * Resolver::previsit( const ast::WaitForStmt * stmt ) {
		visit_children = false;

		// Resolve all clauses first
		for ( unsigned i = 0; i < stmt->clauses.size(); ++i ) {
			const ast::WaitForClause & clause = *stmt->clauses[i];

			ast::TypeEnvironment env;
			CandidateFinder funcFinder( context, env );

			// Find all candidates for a function in canonical form
			funcFinder.find( clause.target, ResolveMode::withAdjustment() );

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

	const ast::WithStmt * Resolver::previsit( const ast::WithStmt * withStmt ) {
		auto mutStmt = mutate(withStmt);
		resolveWithExprs(mutStmt->exprs, stmtsToAddBefore);
		return mutStmt;
	}

	void Resolver::resolveWithExprs(std::vector<ast::ptr<ast::Expr>> & exprs, std::list<ast::ptr<ast::Stmt>> & stmtsToAdd) {
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


	const ast::SingleInit * Resolver::previsit( const ast::SingleInit * singleInit ) {
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

	const ast::ListInit * Resolver::previsit( const ast::ListInit * listInit ) {
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

	const ast::ConstructorInit * Resolver::previsit( const ast::ConstructorInit * ctorInit ) {
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
	bool Resolver::on_error(ast::ptr<ast::Decl> & decl) {
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
