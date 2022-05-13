#include "FixInit.h"

#include <stddef.h>                    // for NULL
#include <algorithm>                   // for set_difference, copy_if
#include <cassert>                     // for assert, strict_dynamic_cast
#include <iostream>                    // for operator<<, ostream, basic_ost...
#include <iterator>                    // for insert_iterator, back_inserter
#include <list>                        // for _List_iterator, list, list<>::...
#include <map>                         // for _Rb_tree_iterator, _Rb_tree_co...
#include <memory>                      // for allocator_traits<>::value_type
#include <set>                         // for set, set<>::value_type
#include <unordered_map>               // for unordered_map, unordered_map<>...
#include <unordered_set>               // for unordered_set
#include <utility>                     // for pair

#include "CodeGen/GenType.h"           // for genPrettyType
#include "CodeGen/OperatorTable.h"
#include "Common/CodeLocationTools.hpp"
#include "Common/PassVisitor.h"        // for PassVisitor, WithStmtsToAdd
#include "Common/SemanticError.h"      // for SemanticError
#include "Common/UniqueName.h"         // for UniqueName
#include "Common/utility.h"            // for CodeLocation, ValueGuard, toSt...
#include "FixGlobalInit.h"             // for fixGlobalInit
#include "GenInit.h"                   // for genCtorDtor
#include "GenPoly/GenPoly.h"           // for getFunctionType
#include "InitTweak.h"                 // for getFunctionName, getCallArg
#include "ResolvExpr/Resolver.h"       // for findVoidExpression
#include "ResolvExpr/typeops.h"        // for typesCompatible
#include "SymTab/Autogen.h"            // for genImplicitCall
#include "SymTab/Indexer.h"            // for Indexer
#include "SymTab/Mangler.h"            // for Mangler
#include "SynTree/LinkageSpec.h"       // for C, Spec, Cforall, isBuiltin
#include "SynTree/Attribute.h"         // for Attribute
#include "SynTree/Constant.h"          // for Constant
#include "SynTree/Declaration.h"       // for ObjectDecl, FunctionDecl, Decl...
#include "SynTree/Expression.h"        // for UniqueExpr, VariableExpr, Unty...
#include "SynTree/Initializer.h"       // for ConstructorInit, SingleInit
#include "SynTree/Label.h"             // for Label, operator<
#include "SynTree/Mutator.h"           // for mutateAll, Mutator, maybeMutate
#include "SynTree/Statement.h"         // for ExprStmt, CompoundStmt, Branch...
#include "SynTree/Type.h"              // for Type, Type::StorageClasses
#include "SynTree/TypeSubstitution.h"  // for TypeSubstitution, operator<<
#include "SynTree/DeclReplacer.h"      // for DeclReplacer
#include "SynTree/Visitor.h"           // for acceptAll, maybeAccept
#include "Validate/FindSpecialDecls.h" // for dtorStmt, dtorStructDestroy

#include "AST/Expr.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/Type.hpp"
#include "AST/DeclReplacer.hpp"

extern bool ctordtorp; // print all debug
extern bool ctorp; // print ctor debug
extern bool cpctorp; // print copy ctor debug
extern bool dtorp; // print dtor debug
#define PRINT( text ) if ( ctordtorp ) { text }
#define CP_CTOR_PRINT( text ) if ( ctordtorp || cpctorp ) { text }
#define DTOR_PRINT( text ) if ( ctordtorp || dtorp ) { text }

namespace InitTweak {
namespace {
	struct SelfAssignChecker {
		void previsit( const ast::ApplicationExpr * appExpr );
	};

	struct StmtExprResult {
		const ast::StmtExpr * previsit( const ast::StmtExpr * stmtExpr );
	};

	/// wrap function application expressions as ImplicitCopyCtorExpr nodes so that it is easy to identify which
	/// function calls need their parameters to be copy constructed
	struct InsertImplicitCalls : public ast::WithConstTypeSubstitution, public ast::WithShortCircuiting {
		const ast::Expr * postvisit( const ast::ApplicationExpr * appExpr );

		// only handles each UniqueExpr once
		// if order of visit does not change, this should be safe
		void previsit (const ast::UniqueExpr *);

		std::unordered_set<decltype(ast::UniqueExpr::id)> visitedIds;
	};

	/// generate temporary ObjectDecls for each argument and return value of each ImplicitCopyCtorExpr,
	/// generate/resolve copy construction expressions for each, and generate/resolve destructors for both
	/// arguments and return value temporaries
	struct ResolveCopyCtors final : public ast::WithGuards, public ast::WithStmtsToAdd<>, public ast::WithSymbolTable, public ast::WithShortCircuiting, public ast::WithVisitorRef<ResolveCopyCtors>, public ast::WithConstTranslationUnit {
		const ast::Expr * postvisit( const ast::ImplicitCopyCtorExpr * impCpCtorExpr );
		const ast::StmtExpr * previsit( const ast::StmtExpr * stmtExpr );
		const ast::UniqueExpr * previsit( const ast::UniqueExpr * unqExpr );

		/// handles distant mutations of environment manually.
		/// WithConstTypeSubstitution cannot remember where the environment is from

		/// MUST be called at start of overload previsit
		void previsit( const ast::Expr * expr);
		/// MUST be called at return of overload postvisit
		const ast::Expr * postvisit(const ast::Expr * expr);

		/// create and resolve ctor/dtor expression: fname(var, [cpArg])
		const ast::Expr * makeCtorDtor( const std::string & fname, const ast::ObjectDecl * var, const ast::Expr * cpArg = nullptr );
		/// true if type does not need to be copy constructed to ensure correctness
		bool skipCopyConstruct( const ast::Type * type );
		ast::ptr< ast::Expr > copyConstructArg( const ast::Expr * arg, const ast::ImplicitCopyCtorExpr * impCpCtorExpr, const ast::Type * formal );
		ast::Expr * destructRet( const ast::ObjectDecl * ret, const ast::Expr * arg );
	private:
		/// hack to implement WithTypeSubstitution while conforming to mutation safety.
		ast::TypeSubstitution * env;
		bool                    envModified;
	};

	/// collects constructed object decls - used as a base class
	struct ObjDeclCollector : public ast::WithGuards, public ast::WithShortCircuiting {
		// use ordered data structure to maintain ordering for set_difference and for consistent error messages
		typedef std::list< const ast::ObjectDecl * > ObjectSet;
		void previsit( const ast::CompoundStmt *compoundStmt );
		void previsit( const ast::DeclStmt *stmt );

		// don't go into other functions
		void previsit( const ast::FunctionDecl * ) { visit_children = false; }

	  protected:
		ObjectSet curVars;
	};

	// debug
	template<typename ObjectSet>
	struct PrintSet {
		PrintSet( const ObjectSet & objs ) : objs( objs ) {}
		const ObjectSet & objs;
	};
	template<typename ObjectSet>
	PrintSet<ObjectSet> printSet( const ObjectSet & objs ) { return PrintSet<ObjectSet>( objs ); }
	template<typename ObjectSet>
	std::ostream & operator<<( std::ostream & out, const PrintSet<ObjectSet> & set) {
		out << "{ ";
		for ( auto & obj : set.objs ) {
			out << obj->name << ", " ;
		} // for
		out << " }";
		return out;
	}

	struct LabelFinder final : public ObjDeclCollector {
		typedef std::map< std::string, ObjectSet > LabelMap;
		// map of Label -> live variables at that label
		LabelMap vars;

		typedef ObjDeclCollector Parent;
		using Parent::previsit;
		void previsit( const ast::Stmt * stmt );

		void previsit( const ast::CompoundStmt *compoundStmt );
		void previsit( const ast::DeclStmt *stmt );
	};

	/// insert destructor calls at the appropriate places.  must happen before CtorInit nodes are removed
	/// (currently by FixInit)
	struct InsertDtors final : public ObjDeclCollector, public ast::WithStmtsToAdd<> {
		typedef std::list< ObjectDecl * > OrderedDecls;
		typedef std::list< OrderedDecls > OrderedDeclsStack;

		InsertDtors( ast::Pass<LabelFinder> & finder ) : finder( finder ), labelVars( finder.core.vars ) {}

		typedef ObjDeclCollector Parent;
		using Parent::previsit;

		void previsit( const ast::FunctionDecl * funcDecl );

		void previsit( const ast::BranchStmt * stmt );
	private:
		void handleGoto( const ast::BranchStmt * stmt );

		ast::Pass<LabelFinder> & finder;
		LabelFinder::LabelMap & labelVars;
		OrderedDeclsStack reverseDeclOrder;
	};

	/// expand each object declaration to use its constructor after it is declared.
	struct FixInit : public ast::WithStmtsToAdd<> {
		static void fixInitializers( ast::TranslationUnit &translationUnit );

		const ast::DeclWithType * postvisit( const ast::ObjectDecl *objDecl );

		std::list< ast::ptr< ast::Decl > > staticDtorDecls;
	};

	/// generate default/copy ctor and dtor calls for user-defined struct ctor/dtors
	/// for any member that is missing a corresponding ctor/dtor call.
	/// error if a member is used before constructed
	struct GenStructMemberCalls final : public ast::WithGuards, public ast::WithShortCircuiting, public ast::WithSymbolTable, public ast::WithVisitorRef<GenStructMemberCalls>, public ast::WithConstTranslationUnit {
		void previsit( const ast::FunctionDecl * funcDecl );
		const ast::DeclWithType * postvisit( const ast::FunctionDecl * funcDecl );

		void previsit( const ast::MemberExpr * memberExpr );
		void previsit( const ast::ApplicationExpr * appExpr );

		/// Note: this post mutate used to be in a separate visitor. If this pass breaks, one place to examine is whether it is
		/// okay for this part of the recursion to occur alongside the rest.
		const ast::Expr * postvisit( const ast::UntypedExpr * expr );

		SemanticErrorException errors;
	  private:
		template< typename... Params >
		void emit( CodeLocation, const Params &... params );

		ast::FunctionDecl * function = nullptr;
		std::set< const ast::DeclWithType * > unhandled;
		std::map< const ast::DeclWithType *, CodeLocation > usedUninit;
		const ast::ObjectDecl * thisParam = nullptr;
		bool isCtor = false; // true if current function is a constructor
		const ast::StructDecl * structDecl = nullptr;
	};

	/// expands ConstructorExpr nodes into comma expressions, using a temporary for the first argument
	struct FixCtorExprs final : public ast::WithDeclsToAdd<>, public ast::WithSymbolTable, public ast::WithShortCircuiting, public ast::WithConstTranslationUnit {
		const ast::Expr * postvisit( const ast::ConstructorExpr * ctorExpr );
	};

	/// add CompoundStmts around top-level expressions so that temporaries are destroyed in the correct places.
	struct SplitExpressions : public ast::WithShortCircuiting {
		ast::Stmt * postvisit( const ast::ExprStmt * stmt );
		void previsit( const ast::TupleAssignExpr * expr );
	};
} // namespace

void fix( ast::TranslationUnit & translationUnit, bool inLibrary ) {
	ast::Pass<SelfAssignChecker>::run( translationUnit );

	// fixes StmtExpr to properly link to their resulting expression
	ast::Pass<StmtExprResult>::run( translationUnit );

	// fixes ConstructorInit for global variables. should happen before fixInitializers.
	InitTweak::fixGlobalInit( translationUnit, inLibrary );

	// must happen before ResolveCopyCtors because temporaries have to be inserted into the correct scope
	ast::Pass<SplitExpressions>::run( translationUnit );

	ast::Pass<InsertImplicitCalls>::run( translationUnit );

	// Needs to happen before ResolveCopyCtors, because argument/return temporaries should not be considered in
	// error checking branch statements
	{
		ast::Pass<LabelFinder> finder;
		ast::Pass<InsertDtors>::run( translationUnit, finder );
	}

	ast::Pass<ResolveCopyCtors>::run( translationUnit );
	FixInit::fixInitializers( translationUnit );
	ast::Pass<GenStructMemberCalls>::run( translationUnit );

	// Needs to happen after GenStructMemberCalls, since otherwise member constructors exprs
	// don't have the correct form, and a member can be constructed more than once.
	ast::Pass<FixCtorExprs>::run( translationUnit );
}

namespace {
	/// find and return the destructor used in `input`. If `input` is not a simple destructor call, generate a thunk
	/// that wraps the destructor, insert it into `stmtsToAdd` and return the new function declaration
	const ast::DeclWithType * getDtorFunc( const ast::ObjectDecl * objDecl, const ast::Stmt * input, std::list< ast::ptr<ast::Stmt> > & stmtsToAdd ) {
		const CodeLocation loc = input->location;
		// unwrap implicit statement wrapper
		// Statement * dtor = input;
		assert( input );
		// std::list< const ast::Expr * > matches;
		auto matches = collectCtorDtorCalls( input );

		if ( dynamic_cast< const ast::ExprStmt * >( input ) ) {
			// only one destructor call in the expression
			if ( matches.size() == 1 ) {
				auto func = getFunction( matches.front() );
				assertf( func, "getFunction failed to find function in %s", toString( matches.front() ).c_str() );

				// cleanup argument must be a function, not an object (including function pointer)
				if ( auto dtorFunc = dynamic_cast< const ast::FunctionDecl * > ( func ) ) {
					if ( dtorFunc->type->forall.empty() ) {
						// simple case where the destructor is a monomorphic function call - can simply
						// use that function as the cleanup function.
						return func;
					}
				}
			}
		}

		// otherwise the cleanup is more complicated - need to build a single argument cleanup function that
		// wraps the more complicated code.
		static UniqueName dtorNamer( "__cleanup_dtor" );
		std::string name = dtorNamer.newName();
		ast::FunctionDecl * dtorFunc = SymTab::genDefaultFunc( loc, name, objDecl->type->stripReferences(), false );
		stmtsToAdd.push_back( new ast::DeclStmt(loc, dtorFunc ) );

		// the original code contains uses of objDecl - replace them with the newly generated 'this' parameter.
		const ast::ObjectDecl * thisParam = getParamThis( dtorFunc );
		const ast::Expr * replacement = new ast::VariableExpr( loc, thisParam );

		auto base = replacement->result->stripReferences();
		if ( dynamic_cast< const ast::ArrayType * >( base ) || dynamic_cast< const ast::TupleType * > ( base ) ) {
			// need to cast away reference for array types, since the destructor is generated without the reference type,
			// and for tuple types since tuple indexing does not work directly on a reference
			replacement = new ast::CastExpr( replacement, base );
		}
		auto dtor = ast::DeclReplacer::replace( input, ast::DeclReplacer::ExprMap{ std::make_pair( objDecl, replacement ) } );
		auto mutStmts = dtorFunc->stmts.get_and_mutate();
		mutStmts->push_back(strict_dynamic_cast<const ast::Stmt *>( dtor ));
		dtorFunc->stmts = mutStmts;

		return dtorFunc;
	}

	void FixInit::fixInitializers( ast::TranslationUnit & translationUnit ) {
		ast::Pass<FixInit> fixer;

		// can't use mutateAll, because need to insert declarations at top-level
		// can't use DeclMutator, because sometimes need to insert IfStmt, etc.
		SemanticErrorException errors;
		for ( auto i = translationUnit.decls.begin(); i != translationUnit.decls.end(); ++i ) {
			try {
				// maybeAccept( *i, fixer ); translationUnit should never contain null
				*i = (*i)->accept(fixer);
				translationUnit.decls.splice( i, fixer.core.staticDtorDecls );
			} catch( SemanticErrorException &e ) {
				errors.append( e );
			} // try
		} // for
		if ( ! errors.isEmpty() ) {
			throw errors;
		} // if
	}

	const ast::StmtExpr * StmtExprResult::previsit( const ast::StmtExpr * stmtExpr ) {
		// we might loose the result expression here so add a pointer to trace back
		assert( stmtExpr->result );
		const ast::Type * result = stmtExpr->result;
		if ( ! result->isVoid() ) {
			auto mutExpr = mutate(stmtExpr);
			const ast::CompoundStmt * body = mutExpr->stmts;
			assert( ! body->kids.empty() );
			mutExpr->resultExpr = body->kids.back().strict_as<ast::ExprStmt>();
			return mutExpr;
		}
		return stmtExpr;
	}

	ast::Stmt * SplitExpressions::postvisit( const ast::ExprStmt * stmt ) {
		// wrap each top-level ExprStmt in a block so that destructors for argument and return temporaries are destroyed
		// in the correct places
		ast::CompoundStmt * ret = new ast::CompoundStmt( stmt->location, { stmt } );
		return ret;
	}

	void SplitExpressions::previsit( const ast::TupleAssignExpr * ) {
		// don't do this within TupleAssignExpr, since it is already broken up into multiple expressions
		visit_children = false;
	}

	// Relatively simple structural comparison for expressions, needed to determine
	// if two expressions are "the same" (used to determine if self assignment occurs)
	struct StructuralChecker {
		// Strip all casts and then dynamic_cast.
		template<typename T>
		static const T * cast( const ast::Expr * expr ) {
			// this might be too permissive. It's possible that only particular casts are relevant.
			while ( auto cast = dynamic_cast< const ast::CastExpr * >( expr ) ) {
				expr = cast->arg;
			}
			return dynamic_cast< const T * >( expr );
		}

		void previsit( const ast::Expr * ) {
			// anything else does not qualify
			result = false;
		}

		// ignore casts
		void previsit( const ast::CastExpr * ) {}

		void previsit( const ast::MemberExpr * memExpr ) {
			if ( auto otherMember = cast< ast::MemberExpr >( other ) ) {
				if ( otherMember->member == memExpr->member ) {
					other = otherMember->aggregate;
					return;
				}
			}
			result = false;
		}

		void previsit( const ast::VariableExpr * varExpr ) {
			if ( auto otherVar = cast< ast::VariableExpr >( other ) ) {
				if ( otherVar->var == varExpr->var ) {
					return;
				}
			}
			result = false;
		}

		void previsit( const ast::AddressExpr * ) {
			if ( auto addrExpr = cast< ast::AddressExpr >( other ) ) {
				other = addrExpr->arg;
				return;
			}
			result = false;
		}

		const ast::Expr * other;
		bool result = true;
		StructuralChecker( const ast::Expr * other ) : other(other) {}
	};

	bool structurallySimilar( const ast::Expr * e1, const ast::Expr * e2 ) {
		return ast::Pass<StructuralChecker>::read( e1, e2 );
	}

	void SelfAssignChecker::previsit( const ast::ApplicationExpr * appExpr ) {
		auto function = getFunction( appExpr );
		// Doesn't use isAssignment, because ?+=?, etc. should not count as self-assignment.
		if ( function->name == "?=?" && appExpr->args.size() == 2
				// Check for structural similarity (same variable use, ignore casts, etc.
				// (but does not look too deeply, anything looking like a function is off limits).
				&& structurallySimilar( appExpr->args.front(), appExpr->args.back() ) ) {
			SemanticWarning( appExpr->location, Warning::SelfAssignment, toCString( appExpr->args.front() ) );
		}
	}

	const ast::Expr * InsertImplicitCalls::postvisit( const ast::ApplicationExpr * appExpr ) {
		if ( auto function = appExpr->func.as<ast::VariableExpr>() ) {
			if ( function->var->linkage.is_builtin ) {
				// optimization: don't need to copy construct in order to call intrinsic functions
				return appExpr;
			} else if ( auto funcDecl = function->var.as<ast::DeclWithType>() ) {
				auto ftype = dynamic_cast< const ast::FunctionType * >( GenPoly::getFunctionType( funcDecl->get_type() ) );
				assertf( ftype, "Function call without function type: %s", toString( funcDecl ).c_str() );
				if ( CodeGen::isConstructor( funcDecl->name ) && ftype->params.size() == 2 ) {
					auto t1 = getPointerBase( ftype->params.front() );
					auto t2 = ftype->params.back();
					assert( t1 );

					if ( ResolvExpr::typesCompatible( t1, t2 ) ) {
						// optimization: don't need to copy construct in order to call a copy constructor
						return appExpr;
					} // if
				} else if ( CodeGen::isDestructor( funcDecl->name ) ) {
					// correctness: never copy construct arguments to a destructor
					return appExpr;
				} // if
			} // if
		} // if
		CP_CTOR_PRINT( std::cerr << "InsertImplicitCalls: adding a wrapper " << appExpr << std::endl; )

		// wrap each function call so that it is easy to identify nodes that have to be copy constructed
		ast::ptr<ast::TypeSubstitution> tmp = appExpr->env;
		auto mutExpr = mutate(appExpr);
		mutExpr->env = nullptr;

		auto expr = new ast::ImplicitCopyCtorExpr( appExpr->location, mutExpr );
		// Move the type substitution to the new top-level. The substitution
		// is needed to obtain the type of temporary variables so that copy
		// constructor calls can be resolved.
		assert( typeSubs );
		expr->env = tmp;
		return expr;
	}

	void ResolveCopyCtors::previsit(const ast::Expr * expr) {
		if ( nullptr == expr->env ) {
			return;
		}
		GuardValue( env ) = expr->env->clone();
		GuardValue( envModified ) = false;
	}

	const ast::Expr * ResolveCopyCtors::postvisit(const ast::Expr * expr) {
		// No local environment, skip.
		if ( nullptr == expr->env ) {
			return expr;
		// Environment was modified, mutate and replace.
		} else if ( envModified ) {
			auto mutExpr = mutate(expr);
			mutExpr->env = env;
			return mutExpr;
		// Environment was not mutated, delete the shallow copy before guard.
		} else {
			delete env;
			return expr;
		}
	}

	bool ResolveCopyCtors::skipCopyConstruct( const ast::Type * type ) { return ! isConstructable( type ); }

	const ast::Expr * ResolveCopyCtors::makeCtorDtor( const std::string & fname, const ast::ObjectDecl * var, const ast::Expr * cpArg ) {
		assert( var );
		assert( var->isManaged() );
		assert( !cpArg || cpArg->isManaged() );
		// arrays are not copy constructed, so this should always be an ExprStmt
		ast::ptr< ast::Stmt > stmt = genCtorDtor(var->location, fname, var, cpArg );
		assertf( stmt, "ResolveCopyCtors: genCtorDtor returned nullptr: %s / %s / %s", fname.c_str(), toString( var ).c_str(), toString( cpArg ).c_str() );
		auto exprStmt = stmt.strict_as<ast::ImplicitCtorDtorStmt>()->callStmt.strict_as<ast::ExprStmt>();
		ast::ptr<ast::Expr> untyped = exprStmt->expr; // take ownership of expr

		// resolve copy constructor
		// should only be one alternative for copy ctor and dtor expressions, since all arguments are fixed
		// (VariableExpr and already resolved expression)
		CP_CTOR_PRINT( std::cerr << "ResolvingCtorDtor " << untyped << std::endl; )
		ast::ptr<ast::Expr> resolved = ResolvExpr::findVoidExpression(untyped, { symtab, transUnit().global } );
		assert( resolved );
		if ( resolved->env ) {
			// Extract useful information and discard new environments. Keeping them causes problems in PolyMutator passes.
			env->add( *resolved->env );
			envModified = true;
			auto mut = mutate(resolved.get());
			assertf(mut == resolved.get(), "newly resolved expression must be unique");
			mut->env = nullptr;
		} // if
		if ( auto assign = resolved.as<ast::TupleAssignExpr>() ) {
			// fix newly generated StmtExpr
			previsit( assign->stmtExpr );
		}
		return resolved.release();
	}

	ast::ptr<ast::Expr> ResolveCopyCtors::copyConstructArg(
		const ast::Expr * arg, const ast::ImplicitCopyCtorExpr * impCpCtorExpr, const ast::Type * formal )
	{
		static UniqueName tempNamer("_tmp_cp");
		assert( env );
		const CodeLocation loc = impCpCtorExpr->location;
		// CP_CTOR_PRINT( std::cerr << "Type Substitution: " << *env << std::endl; )
		assert( arg->result );
		ast::ptr<ast::Type> result = arg->result;
		if ( skipCopyConstruct( result ) ) return arg; // skip certain non-copyable types

		// type may involve type variables, so apply type substitution to get temporary variable's actual type,
		// since result type may not be substituted (e.g., if the type does not appear in the parameter list)
		// Use applyFree so that types bound in function pointers are not substituted, e.g. in forall(dtype T) void (*)(T).

		// xxx - this originally mutates arg->result in place. is it correct?
		result = env->applyFree( result.get() ).node;
		auto mutResult = result.get_and_mutate();
		mutResult->set_const(false);

		auto mutArg = mutate(arg);
		mutArg->result = mutResult;

		ast::ptr<ast::Expr> guard = mutArg;

		ast::ptr<ast::ObjectDecl> tmp = new ast::ObjectDecl(loc, "__tmp", mutResult, nullptr );

		// create and resolve copy constructor
		CP_CTOR_PRINT( std::cerr << "makeCtorDtor for an argument" << std::endl; )
		auto cpCtor = makeCtorDtor( "?{}", tmp, mutArg );

		if ( auto appExpr = dynamic_cast< const ast::ApplicationExpr * >( cpCtor ) ) {
			// if the chosen constructor is intrinsic, the copy is unnecessary, so
			// don't create the temporary and don't call the copy constructor
			auto function = appExpr->func.strict_as<ast::VariableExpr>();
			if ( function->var->linkage == ast::Linkage::Intrinsic ) {
				// arguments that need to be boxed need a temporary regardless of whether the copy constructor is intrinsic,
				// so that the object isn't changed inside of the polymorphic function
				if ( ! GenPoly::needsBoxing( formal, result, impCpCtorExpr->callExpr, env ) ) {
					// xxx - should arg->result be mutated? see comment above.
					return guard;
				}
			}
		}

		// set a unique name for the temporary once it's certain the call is necessary
		auto mut = tmp.get_and_mutate();
		assertf (mut == tmp, "newly created ObjectDecl must be unique");
		mut->name = tempNamer.newName();

		// replace argument to function call with temporary
		stmtsToAddBefore.push_back( new ast::DeclStmt(loc, tmp ) );
		arg = cpCtor;
		return destructRet( tmp, arg );

		// impCpCtorExpr->dtors.push_front( makeCtorDtor( "^?{}", tmp ) );
	}

	ast::Expr * ResolveCopyCtors::destructRet( const ast::ObjectDecl * ret, const ast::Expr * arg ) {
		auto global = transUnit().global;
		// TODO: refactor code for generating cleanup attribute, since it's common and reused in ~3-4 places
		// check for existing cleanup attribute before adding another(?)
		// need to add __Destructor for _tmp_cp variables as well

		assertf( global.dtorStruct, "Destructor generation requires __Destructor definition." );
		assertf( global.dtorStruct->members.size() == 2, "__Destructor definition does not have expected fields." );
		assertf( global.dtorDestroy, "Destructor generation requires __destroy_Destructor." );

		const CodeLocation loc = ret->location;

		// generate a __Destructor for ret that calls the destructor
		auto res = makeCtorDtor( "^?{}", ret );
		auto dtor = mutate(res);

		// if the chosen destructor is intrinsic, elide the generated dtor handler
		if ( arg && isIntrinsicCallExpr( dtor ) ) {
			return new ast::CommaExpr(loc, arg, new ast::VariableExpr(loc, ret ) );
			// return;
		}

		if ( ! dtor->env ) dtor->env = maybeClone( env );
		auto dtorFunc = getDtorFunc( ret, new ast::ExprStmt(loc, dtor ), stmtsToAddBefore );

		auto dtorStructType = new ast::StructInstType( global.dtorStruct );

		// what does this do???
		dtorStructType->params.push_back( new ast::TypeExpr(loc, new ast::VoidType() ) );

		// cast destructor pointer to void (*)(void *), to silence GCC incompatible pointer warnings
		auto dtorFtype = new ast::FunctionType();
		dtorFtype->params.push_back( new ast::PointerType(new ast::VoidType( ) ) );
		auto dtorType = new ast::PointerType( dtorFtype );

		static UniqueName namer( "_ret_dtor" );
		auto retDtor = new ast::ObjectDecl(loc, namer.newName(), dtorStructType, new ast::ListInit(loc, { new ast::SingleInit(loc, ast::ConstantExpr::null(loc) ), new ast::SingleInit(loc, new ast::CastExpr( new ast::VariableExpr(loc, dtorFunc ), dtorType ) ) } ) );
		retDtor->attributes.push_back( new ast::Attribute( "cleanup", { new ast::VariableExpr(loc, global.dtorDestroy ) } ) );
		stmtsToAddBefore.push_back( new ast::DeclStmt(loc, retDtor ) );

		if ( arg ) {
			auto member = new ast::MemberExpr(loc, global.dtorStruct->members.front().strict_as<ast::DeclWithType>(), new ast::VariableExpr(loc, retDtor ) );
			auto object = new ast::CastExpr( new ast::AddressExpr( new ast::VariableExpr(loc, ret ) ), new ast::PointerType(new ast::VoidType() ) );
			ast::Expr * assign = createBitwiseAssignment( member, object );
			return new ast::CommaExpr(loc, new ast::CommaExpr(loc, arg, assign ), new ast::VariableExpr(loc, ret ) );
		}
		return nullptr;
		// impCpCtorExpr->get_dtors().push_front( makeCtorDtor( "^?{}", ret ) );
	}

	const ast::Expr * ResolveCopyCtors::postvisit( const ast::ImplicitCopyCtorExpr *impCpCtorExpr ) {
		CP_CTOR_PRINT( std::cerr << "ResolveCopyCtors: " << impCpCtorExpr << std::endl; )

		ast::ApplicationExpr * appExpr = mutate(impCpCtorExpr->callExpr.get());
		const ast::ObjectDecl * returnDecl = nullptr;
		const CodeLocation loc = appExpr->location;

		// take each argument and attempt to copy construct it.
		auto ftype = GenPoly::getFunctionType( appExpr->func->result );
		assert( ftype );
		auto & params = ftype->params;
		auto iter = params.begin();
		for ( auto & arg : appExpr->args ) {
			const ast::Type * formal = nullptr;
			if ( iter != params.end() ) { // does not copy construct C-style variadic arguments
				// DeclarationWithType * param = *iter++;
				formal = *iter++;
			}

			arg = copyConstructArg( arg, impCpCtorExpr, formal );
		} // for

		// each return value from the call needs to be connected with an ObjectDecl at the call site, which is
		// initialized with the return value and is destructed later
		// xxx - handle named return values?
		const ast::Type * result = appExpr->result;
		if ( ! result->isVoid() ) {
			static UniqueName retNamer("_tmp_cp_ret");
			// result = result->clone();
			auto subResult = env->apply( result ).node;
			auto ret = new ast::ObjectDecl(loc, retNamer.newName(), subResult, nullptr );
			auto mutType = mutate(ret->type.get());
			mutType->set_const( false );
			ret->type = mutType;
			returnDecl = ret;
			stmtsToAddBefore.push_back( new ast::DeclStmt(loc, ret ) );
			CP_CTOR_PRINT( std::cerr << "makeCtorDtor for a return" << std::endl; )
		} // for
		CP_CTOR_PRINT( std::cerr << "after Resolving: " << impCpCtorExpr << std::endl; )
		// ------------------------------------------------------

		CP_CTOR_PRINT( std::cerr << "Coming out the back..." << impCpCtorExpr << std::endl; )

		// detach fields from wrapper node so that it can be deleted without deleting too much

		// xxx - actual env might be somewhere else, need to keep invariant

		// deletion of wrapper should be handled by pass template now

		// impCpCtorExpr->callExpr = nullptr;
		assert (appExpr->env == nullptr);
		appExpr->env = impCpCtorExpr->env;
		// std::swap( impCpCtorExpr->env, appExpr->env );
		// assert( impCpCtorExpr->env == nullptr );
		// delete impCpCtorExpr;

		if ( returnDecl ) {
			ast::Expr * assign = createBitwiseAssignment( new ast::VariableExpr(loc, returnDecl ), appExpr );
			if ( ! dynamic_cast< const ast::ReferenceType * >( result ) ) {
				// destructing reference returns is bad because it can cause multiple destructor calls to the same object - the returned object is not a temporary
				assign = destructRet( returnDecl, assign );
				assert(assign);
			} else {
				assign = new ast::CommaExpr(loc, assign, new ast::VariableExpr(loc, returnDecl ) );
			}
			// move env from appExpr to retExpr
			// std::swap( assign->env, appExpr->env );
			assign->env = appExpr->env;
			// actual env is handled by common routine that replaces WithTypeSubstitution
			return postvisit((const ast::Expr *)assign);
		} else {
			return postvisit((const ast::Expr *)appExpr);
		} // if
	}

	const ast::StmtExpr * ResolveCopyCtors::previsit( const ast::StmtExpr * _stmtExpr ) {
		// function call temporaries should be placed at statement-level, rather than nested inside of a new statement expression,
		// since temporaries can be shared across sub-expressions, e.g.
		//   [A, A] f();       // decl
		//   g([A] x, [A] y);  // decl
		//   g(f());           // call
		// f is executed once, so the return temporary is shared across the tuple constructors for x and y.
		// Explicitly mutating children instead of mutating the inner compound statement forces the temporaries to be added
		// to the outer context, rather than inside of the statement expression.

		// call the common routine that replaces WithTypeSubstitution
		previsit((const ast::Expr *) _stmtExpr);

		visit_children = false;
		const CodeLocation loc = _stmtExpr->location;

		assert( env );

		symtab.enterScope();
		// visit all statements
		auto stmtExpr = mutate(_stmtExpr);
		auto mutStmts = mutate(stmtExpr->stmts.get());

		auto & stmts = mutStmts->kids;
		for ( auto & stmt : stmts ) {
			stmt = stmt->accept( *visitor );
		} // for
		stmtExpr->stmts = mutStmts;
		symtab.leaveScope();

		assert( stmtExpr->result );
		// const ast::Type * result = stmtExpr->result;
		if ( ! stmtExpr->result->isVoid() ) {
			static UniqueName retNamer("_tmp_stmtexpr_ret");

			// result = result->clone();
			auto result = env->apply( stmtExpr->result.get() ).node;
			if ( ! InitTweak::isConstructable( result ) ) {
				// delete result;
				return stmtExpr;
			}
			auto mutResult = result.get_and_mutate();
			mutResult->set_const(false);

			// create variable that will hold the result of the stmt expr
			auto ret = new ast::ObjectDecl(loc, retNamer.newName(), mutResult, nullptr );
			stmtsToAddBefore.push_back( new ast::DeclStmt(loc, ret ) );

			assertf(
				stmtExpr->resultExpr,
				"Statement-Expression should have a resulting expression at %s:%d",
				stmtExpr->location.filename.c_str(),
				stmtExpr->location.first_line
			);

			const ast::ExprStmt * last = stmtExpr->resultExpr;
			// xxx - if this is non-unique, need to copy while making resultExpr ref
			assertf(last->unique(), "attempt to modify weakly shared statement");
			auto mutLast = mutate(last);
			// above assertion means in-place mutation is OK
			try {
				mutLast->expr = makeCtorDtor( "?{}", ret, mutLast->expr );
			} catch(...) {
				std::cerr << "*CFA internal error: ";
				std::cerr << "can't resolve implicit constructor";
				std::cerr << " at " << stmtExpr->location.filename;
				std::cerr << ":" << stmtExpr->location.first_line << std::endl;

				abort();
			}

			// add destructors after current statement
			stmtsToAddAfter.push_back( new ast::ExprStmt(loc, makeCtorDtor( "^?{}", ret ) ) );

			// must have a non-empty body, otherwise it wouldn't have a result
			assert( ! stmts.empty() );

			// if there is a return decl, add a use as the last statement; will not have return decl on non-constructable returns
			stmts.push_back( new ast::ExprStmt(loc, new ast::VariableExpr(loc, ret ) ) );
		} // if

		assert( stmtExpr->returnDecls.empty() );
		assert( stmtExpr->dtors.empty() );

		return stmtExpr;
	}

	// to prevent warnings ('_unq0' may be used uninitialized in this function),
	// insert an appropriate zero initializer for UniqueExpr temporaries.
	ast::Init * makeInit( const ast::Type * t, CodeLocation const & loc ) {
		if ( auto inst = dynamic_cast< const ast::StructInstType * >( t ) ) {
			// initizer for empty struct must be empty
			if ( inst->base->members.empty() ) {
				return new ast::ListInit( loc, {} );
			}
		} else if ( auto inst = dynamic_cast< const ast::UnionInstType * >( t ) ) {
			// initizer for empty union must be empty
			if ( inst->base->members.empty() ) {
				return new ast::ListInit( loc, {} );
			}
		}

		return new ast::ListInit( loc, {
			new ast::SingleInit( loc, ast::ConstantExpr::from_int( loc, 0 ) )
		} );
	}

	const ast::UniqueExpr * ResolveCopyCtors::previsit( const ast::UniqueExpr * unqExpr ) {
		visit_children = false;
		// xxx - hack to prevent double-handling of unique exprs, otherwise too many temporary variables and destructors are generated
		static std::unordered_map< int, const ast::UniqueExpr * > unqMap;
		auto mutExpr = mutate(unqExpr);
		if ( ! unqMap.count( unqExpr->id ) ) {
			// resolve expr and find its

			auto impCpCtorExpr = mutExpr->expr.as<ast::ImplicitCopyCtorExpr>();
			// PassVisitor<ResolveCopyCtors> fixer;

			mutExpr->expr = mutExpr->expr->accept( *visitor );
			// it should never be necessary to wrap a void-returning expression in a UniqueExpr - if this assumption changes, this needs to be rethought
			assert( unqExpr->result );
			if ( impCpCtorExpr ) {
				auto comma = unqExpr->expr.strict_as<ast::CommaExpr>();
				auto var = comma->arg2.strict_as<ast::VariableExpr>();
				// note the variable used as the result from the call
				mutExpr->var = var;
			} else {
				// expr isn't a call expr, so create a new temporary variable to use to hold the value of the unique expression
				mutExpr->object = new ast::ObjectDecl( mutExpr->location, toString("_unq", mutExpr->id), mutExpr->result, makeInit( mutExpr->result, mutExpr->location ) );
				mutExpr->var = new ast::VariableExpr( mutExpr->location, mutExpr->object );
			}

			// stmtsToAddBefore.splice( stmtsToAddBefore.end(), fixer.pass.stmtsToAddBefore );
			// stmtsToAddAfter.splice( stmtsToAddAfter.end(), fixer.pass.stmtsToAddAfter );
			unqMap[mutExpr->id] = mutExpr;
		} else {
			// take data from other UniqueExpr to ensure consistency
			// delete unqExpr->get_expr();
			mutExpr->expr = unqMap[mutExpr->id]->expr;
			// delete unqExpr->result;
			mutExpr->result = mutExpr->expr->result;
		}
		return mutExpr;
	}

	const ast::DeclWithType * FixInit::postvisit( const ast::ObjectDecl *_objDecl ) {
		const CodeLocation loc = _objDecl->location;

		// since this removes the init field from objDecl, it must occur after children are mutated (i.e. postvisit)
		if ( ast::ptr<ast::ConstructorInit> ctorInit = _objDecl->init.as<ast::ConstructorInit>() ) {
			auto objDecl = mutate(_objDecl);

			// could this be non-unique?
			if (objDecl != _objDecl) {
				std::cerr << "FixInit: non-unique object decl " << objDecl->location << objDecl->name << std::endl;
			}
			// a decision should have been made by the resolver, so ctor and init are not both non-NULL
			assert( ! ctorInit->ctor || ! ctorInit->init );
			if ( const ast::Stmt * ctor = ctorInit->ctor ) {
				if ( objDecl->storage.is_static ) {
					addDataSectionAttribute(objDecl);
					// originally wanted to take advantage of gcc nested functions, but
					// we get memory errors with this approach. To remedy this, the static
					// variable is hoisted when the destructor needs to be called.
					//
					// generate:
					// static T __objName_static_varN;
					// void __objName_dtor_atexitN() {
					//   __dtor__...;
					// }
					// int f(...) {
					//   ...
					//   static bool __objName_uninitialized = true;
					//   if (__objName_uninitialized) {
					//     __ctor(__objName);
					//     __objName_uninitialized = false;
					//     atexit(__objName_dtor_atexitN);
					//   }
					//   ...
					// }

					static UniqueName dtorCallerNamer( "_dtor_atexit" );

					// static bool __objName_uninitialized = true
					auto boolType = new ast::BasicType( ast::BasicType::Kind::Bool );
					auto boolInitExpr = new ast::SingleInit(loc, ast::ConstantExpr::from_int(loc, 1 ) );
					auto isUninitializedVar = new ast::ObjectDecl(loc, objDecl->mangleName + "_uninitialized", boolType, boolInitExpr, ast::Storage::Static, ast::Linkage::Cforall);
					isUninitializedVar->fixUniqueId();

					// __objName_uninitialized = false;
					auto setTrue = new ast::UntypedExpr(loc, new ast::NameExpr(loc, "?=?" ) );
					setTrue->args.push_back( new ast::VariableExpr(loc, isUninitializedVar ) );
					setTrue->args.push_back( ast::ConstantExpr::from_int(loc, 0 ) );

					// generate body of if
					auto initStmts = new ast::CompoundStmt(loc);
					auto & body = initStmts->kids;
					body.push_back( ctor );
					body.push_back( new ast::ExprStmt(loc, setTrue ) );

					// put it all together
					auto ifStmt = new ast::IfStmt(loc, new ast::VariableExpr(loc, isUninitializedVar ), initStmts, 0 );
					stmtsToAddAfter.push_back( new ast::DeclStmt(loc, isUninitializedVar ) );
					stmtsToAddAfter.push_back( ifStmt );

					const ast::Stmt * dtor = ctorInit->dtor;

					// these should be automatically managed once reassigned
					// objDecl->set_init( nullptr );
					// ctorInit->set_ctor( nullptr );
					// ctorInit->set_dtor( nullptr );
					if ( dtor ) {
						// if the object has a non-trivial destructor, have to
						// hoist it and the object into the global space and
						// call the destructor function with atexit.

						// Statement * dtorStmt = dtor->clone();

						// void __objName_dtor_atexitN(...) {...}
						ast::FunctionDecl * dtorCaller = new ast::FunctionDecl(loc, objDecl->mangleName + dtorCallerNamer.newName(), {}, {}, {}, new ast::CompoundStmt(loc, {dtor}), ast::Storage::Static, ast::Linkage::C );
						dtorCaller->fixUniqueId();
						// dtorCaller->stmts->push_back( dtor );

						// atexit(dtor_atexit);
						auto callAtexit = new ast::UntypedExpr(loc, new ast::NameExpr(loc, "atexit" ) );
						callAtexit->args.push_back( new ast::VariableExpr(loc, dtorCaller ) );

						body.push_back( new ast::ExprStmt(loc, callAtexit ) );

						// hoist variable and dtor caller decls to list of decls that will be added into global scope
						staticDtorDecls.push_back( objDecl );
						staticDtorDecls.push_back( dtorCaller );

						// need to rename object uniquely since it now appears
						// at global scope and there could be multiple function-scoped
						// static variables with the same name in different functions.
						// Note: it isn't sufficient to modify only the mangleName, because
						// then subsequent Indexer passes can choke on seeing the object's name
						// if another object has the same name and type. An unfortunate side-effect
						// of renaming the object is that subsequent NameExprs may fail to resolve,
						// but there shouldn't be any remaining past this point.
						static UniqueName staticNamer( "_static_var" );
						objDecl->name = objDecl->name + staticNamer.newName();
						objDecl->mangleName = Mangle::mangle( objDecl );
						objDecl->init = nullptr;

						// xxx - temporary hack: need to return a declaration, but want to hoist the current object out of this scope
						// create a new object which is never used
						static UniqueName dummyNamer( "_dummy" );
						auto dummy = new ast::ObjectDecl(loc, dummyNamer.newName(), new ast::PointerType(new ast::VoidType()), nullptr, ast::Storage::Static, ast::Linkage::Cforall, 0, { new ast::Attribute("unused") } );
						// delete ctorInit;
						return dummy;
					} else {
						objDecl->init = nullptr;
						return objDecl;
					}
				} else {
					auto implicit = strict_dynamic_cast< const ast::ImplicitCtorDtorStmt * > ( ctor );
					auto ctorStmt = implicit->callStmt.as<ast::ExprStmt>();
					const ast::ApplicationExpr * ctorCall = nullptr;
					if ( ctorStmt && (ctorCall = isIntrinsicCallExpr( ctorStmt->expr )) && ctorCall->args.size() == 2 ) {
						// clean up intrinsic copy constructor calls by making them into SingleInits
						const ast::Expr * ctorArg = ctorCall->args.back();
						// ctorCall should be gone afterwards
						auto mutArg = mutate(ctorArg);
						mutArg->env = ctorCall->env;
						// std::swap( ctorArg->env, ctorCall->env );
						objDecl->init = new ast::SingleInit(loc, mutArg );

						// ctorCall->args.pop_back();
					} else {
						stmtsToAddAfter.push_back( ctor );
						objDecl->init = nullptr;
						// ctorInit->ctor = nullptr;
					}

					const ast::Stmt * dtor = ctorInit->dtor;
					if ( dtor ) {
						auto implicit = strict_dynamic_cast< const ast::ImplicitCtorDtorStmt * >( dtor );
						const ast::Stmt * dtorStmt = implicit->callStmt;

						// don't need to call intrinsic dtor, because it does nothing, but
						// non-intrinsic dtors must be called
						if ( ! isIntrinsicSingleArgCallStmt( dtorStmt ) ) {
							// set dtor location to the object's location for error messages
							auto dtorFunc = getDtorFunc( objDecl, dtorStmt, stmtsToAddBefore );
							objDecl->attributes.push_back( new ast::Attribute( "cleanup", { new ast::VariableExpr(loc, dtorFunc ) } ) );
							// ctorInit->dtor = nullptr;
						} // if
					}
				} // if
			} else if ( const ast::Init * init = ctorInit->init ) {
				objDecl->init = init;
				// ctorInit->init = nullptr;
			} else {
				// no constructor and no initializer, which is okay
				objDecl->init = nullptr;
			} // if
			// delete ctorInit;
			return objDecl;
		} // if
		return _objDecl;
	}

	void ObjDeclCollector::previsit( const ast::CompoundStmt * ) {
		GuardValue( curVars );
	}

	void ObjDeclCollector::previsit( const ast::DeclStmt * stmt ) {
		// keep track of all variables currently in scope
		if ( auto objDecl = stmt->decl.as<ast::ObjectDecl>() ) {
			curVars.push_back( objDecl );
		} // if
	}

	void LabelFinder::previsit( const ast::Stmt * stmt ) {
		// for each label, remember the variables in scope at that label.
		for ( auto l : stmt->labels ) {
			vars[l] = curVars;
		} // for
	}

	void LabelFinder::previsit( const ast::CompoundStmt * stmt ) {
		previsit( (const ast::Stmt *) stmt );
		Parent::previsit( stmt );
	}

	void LabelFinder::previsit( const ast::DeclStmt * stmt ) {
		previsit( (const ast::Stmt *)stmt );
		Parent::previsit( stmt );
	}


	void InsertDtors::previsit( const ast::FunctionDecl * funcDecl ) {
		// each function needs to have its own set of labels
		GuardValue( labelVars );
		labelVars.clear();
		// LabelFinder does not recurse into FunctionDecl, so need to visit
		// its children manually.
		if (funcDecl->type) funcDecl->type->accept(finder);
		// maybeAccept( funcDecl->type, finder );
		if (funcDecl->stmts) funcDecl->stmts->accept(finder) ;

		// all labels for this function have been collected, insert destructors as appropriate via implicit recursion.
	}

	// Handle break/continue/goto in the same manner as C++.  Basic idea: any objects that are in scope at the
	// BranchStmt but not at the labelled (target) statement must be destructed.  If there are any objects in scope
	// at the target location but not at the BranchStmt then those objects would be uninitialized so notify the user
	// of the error.  See C++ Reference 6.6 Jump Statements for details.
	void InsertDtors::handleGoto( const ast::BranchStmt * stmt ) {
		// can't do anything for computed goto
		if ( stmt->computedTarget ) return;

		assertf( stmt->target.name != "", "BranchStmt missing a label: %s", toString( stmt ).c_str() );
		// S_L = lvars = set of objects in scope at label definition
		// S_G = curVars = set of objects in scope at goto statement
		ObjectSet & lvars = labelVars[ stmt->target ];

		DTOR_PRINT(
			std::cerr << "at goto label: " << stmt->target.name << std::endl;
			std::cerr << "S_G = " << printSet( curVars ) << std::endl;
			std::cerr << "S_L = " << printSet( lvars ) << std::endl;
		)


		// std::set_difference requires that the inputs be sorted.
		lvars.sort();
		curVars.sort();

		ObjectSet diff;
		// S_L-S_G results in set of objects whose construction is skipped - it's an error if this set is non-empty
		std::set_difference( lvars.begin(), lvars.end(), curVars.begin(), curVars.end(), std::inserter( diff, diff.begin() ) );
		DTOR_PRINT(
			std::cerr << "S_L-S_G = " << printSet( diff ) << std::endl;
		)
		if ( ! diff.empty() ) {
			SemanticError( stmt, std::string("jump to label '") + stmt->target.name + "' crosses initialization of " + (*diff.begin())->name + " " );
		} // if
	}

	void InsertDtors::previsit( const ast::BranchStmt * stmt ) {
		switch( stmt->kind ) {
		  case ast::BranchStmt::Continue:
		  case ast::BranchStmt::Break:
			// could optimize the break/continue case, because the S_L-S_G check is unnecessary (this set should
			// always be empty), but it serves as a small sanity check.
		  case ast::BranchStmt::Goto:
			handleGoto( stmt );
			break;
		  default:
			assert( false );
		} // switch
	}

	bool checkWarnings( const ast::FunctionDecl * funcDecl ) {
		// only check for warnings if the current function is a user-defined
		// constructor or destructor
		if ( ! funcDecl ) return false;
		if ( ! funcDecl->stmts ) return false;
		return CodeGen::isCtorDtor( funcDecl->name ) && ! funcDecl->linkage.is_overrideable;
	}

	void GenStructMemberCalls::previsit( const ast::FunctionDecl * funcDecl ) {
		GuardValue( function );
		GuardValue( unhandled );
		GuardValue( usedUninit );
		GuardValue( thisParam );
		GuardValue( isCtor );
		GuardValue( structDecl );
		errors = SemanticErrorException();  // clear previous errors

		// need to start with fresh sets
		unhandled.clear();
		usedUninit.clear();

		function = mutate(funcDecl);
		// could this be non-unique?
		if (function != funcDecl) {
			std::cerr << "GenStructMemberCalls: non-unique FunctionDecl " << funcDecl->location << funcDecl->name << std::endl;
		}

		isCtor = CodeGen::isConstructor( function->name );
		if ( checkWarnings( function ) ) {
			// const ast::FunctionType * type = function->type;
			// assert( ! type->params.empty() );
			thisParam = function->params.front().strict_as<ast::ObjectDecl>();
			auto thisType = getPointerBase( thisParam->get_type() );
			auto structType = dynamic_cast< const ast::StructInstType * >( thisType );
			if ( structType ) {
				structDecl = structType->base;
				for ( auto & member : structDecl->members ) {
					if ( auto field = member.as<ast::ObjectDecl>() ) {
						// record all of the struct type's members that need to be constructed or
						// destructed by the end of the function
						unhandled.insert( field );
					}
				}
			}
		}
	}

	const ast::DeclWithType * GenStructMemberCalls::postvisit( const ast::FunctionDecl * funcDecl ) {
		// remove the unhandled objects from usedUninit, because a call is inserted
		// to handle them - only objects that are later constructed are used uninitialized.
		std::map< const ast::DeclWithType *, CodeLocation > diff;
		// need the comparator since usedUninit and unhandled have different types
		struct comp_t {
			typedef decltype(usedUninit)::value_type usedUninit_t;
			typedef decltype(unhandled)::value_type unhandled_t;
			bool operator()(usedUninit_t x, unhandled_t y) { return x.first < y; }
			bool operator()(unhandled_t x, usedUninit_t y) { return x < y.first; }
		} comp;
		std::set_difference( usedUninit.begin(), usedUninit.end(), unhandled.begin(), unhandled.end(), std::inserter( diff, diff.begin() ), comp );
		for ( auto p : diff ) {
			auto member = p.first;
			auto loc = p.second;
			// xxx - make error message better by also tracking the location that the object is constructed at?
			emit( loc, "in ", function->name, ", field ", member->name, " used before being constructed" );
		}

		const CodeLocation loc = funcDecl->location;

		if ( ! unhandled.empty() ) {
			auto mutStmts = function->stmts.get_and_mutate();
			// need to explicitly re-add function parameters to the indexer in order to resolve copy constructors
			auto guard = makeFuncGuard( [this]() { symtab.enterScope(); }, [this]() { symtab.leaveScope(); } );
			symtab.addFunction( function );
			auto global = transUnit().global;

			// need to iterate through members in reverse in order for
			// ctor/dtor statements to come out in the right order
			for ( auto & member : reverseIterate( structDecl->members ) ) {
				auto field = member.as<ast::ObjectDecl>();
				// skip non-DWT members
				if ( ! field ) continue;
				// skip non-constructable members
				if ( ! tryConstruct( field ) ) continue;
				// skip handled members
				if ( ! unhandled.count( field ) ) continue;

				// insert and resolve default/copy constructor call for each field that's unhandled
				// std::list< const ast::Stmt * > stmt;
				ast::Expr * arg2 = nullptr;
				if ( function->name == "?{}" && isCopyFunction( function ) ) {
					// if copy ctor, need to pass second-param-of-this-function.field
					// std::list< DeclarationWithType * > & params = function->get_functionType()->get_parameters();
					assert( function->params.size() == 2 );
					arg2 = new ast::MemberExpr(funcDecl->location, field, new ast::VariableExpr(funcDecl->location, function->params.back() ) );
				}
				InitExpander_new srcParam( arg2 );
				// cast away reference type and construct field.
				ast::Expr * thisExpr = new ast::CastExpr(funcDecl->location, new ast::VariableExpr(funcDecl->location, thisParam ), thisParam->get_type()->stripReferences());
				ast::Expr * memberDest = new ast::MemberExpr(funcDecl->location, field, thisExpr );
				ast::ptr<ast::Stmt> callStmt = SymTab::genImplicitCall( srcParam, memberDest, loc, function->name, field, static_cast<SymTab::LoopDirection>(isCtor) );

				if ( callStmt ) {
					// auto & callStmt = stmt.front();

					try {
						callStmt = callStmt->accept( *visitor );
						if ( isCtor ) {
							mutStmts->push_front( callStmt );
						} else { // TODO: don't generate destructor function/object for intrinsic calls
							// destructor statements should be added at the end
							// function->get_statements()->push_back( callStmt );

							// Optimization: do not need to call intrinsic destructors on members
							if ( isIntrinsicSingleArgCallStmt( callStmt ) ) continue;

							// __Destructor _dtor0 = { (void *)&b.a1, (void (*)(void *)_destroy_A };
							std::list< ast::ptr<ast::Stmt> > stmtsToAdd;

							static UniqueName memberDtorNamer = { "__memberDtor" };
							assertf( global.dtorStruct, "builtin __Destructor not found." );
							assertf( global.dtorDestroy, "builtin __destroy_Destructor not found." );

							ast::Expr * thisExpr = new ast::CastExpr( new ast::AddressExpr( new ast::VariableExpr(loc, thisParam ) ), new ast::PointerType( new ast::VoidType(), ast::CV::Qualifiers() ) );
							ast::Expr * dtorExpr = new ast::VariableExpr(loc, getDtorFunc( thisParam, callStmt, stmtsToAdd ) );

							// cast destructor pointer to void (*)(void *), to silence GCC incompatible pointer warnings
							auto dtorFtype = new ast::FunctionType();
							dtorFtype->params.emplace_back( new ast::PointerType( new ast::VoidType() ) );
							auto dtorType = new ast::PointerType( dtorFtype );

							auto destructor = new ast::ObjectDecl(loc, memberDtorNamer.newName(), new ast::StructInstType( global.dtorStruct ), new ast::ListInit(loc, { new ast::SingleInit(loc, thisExpr ), new ast::SingleInit(loc, new ast::CastExpr( dtorExpr, dtorType ) ) } ) );
							destructor->attributes.push_back( new ast::Attribute( "cleanup", { new ast::VariableExpr( loc, global.dtorDestroy ) } ) );
							mutStmts->push_front( new ast::DeclStmt(loc, destructor ) );
							mutStmts->kids.splice( mutStmts->kids.begin(), stmtsToAdd );
						}
					} catch ( SemanticErrorException & error ) {
						emit( funcDecl->location, "in ", function->name , ", field ", field->name, " not explicitly ", isCtor ? "constructed" : "destructed",  " and no ", isCtor ? "default constructor" : "destructor", " found" );
					}
				}
			}
			function->stmts = mutStmts;
		}
		if (! errors.isEmpty()) {
			throw errors;
		}
		// return funcDecl;
		return function;
	}

	/// true if expr is effectively just the 'this' parameter
	bool isThisExpression( const ast::Expr * expr, const ast::DeclWithType * thisParam ) {
		// TODO: there are more complicated ways to pass 'this' to a constructor, e.g. &*, *&, etc.
		if ( auto varExpr = dynamic_cast< const ast::VariableExpr * >( expr ) ) {
			return varExpr->var == thisParam;
		} else if ( auto castExpr = dynamic_cast< const ast::CastExpr * > ( expr ) ) {
			return isThisExpression( castExpr->arg, thisParam );
		}
		return false;
	}

	/// returns a MemberExpr if expr is effectively just member access on the 'this' parameter, else nullptr
	const ast::MemberExpr * isThisMemberExpr( const ast::Expr * expr, const ast::DeclWithType * thisParam ) {
		if ( auto memberExpr = dynamic_cast< const ast::MemberExpr * >( expr ) ) {
			if ( isThisExpression( memberExpr->aggregate, thisParam ) ) {
				return memberExpr;
			}
		} else if ( auto castExpr = dynamic_cast< const ast::CastExpr * >( expr ) ) {
			return isThisMemberExpr( castExpr->arg, thisParam );
		}
		return nullptr;
	}

	void GenStructMemberCalls::previsit( const ast::ApplicationExpr * appExpr ) {
		if ( ! checkWarnings( function ) ) {
			visit_children = false;
			return;
		}

		std::string fname = getFunctionName( appExpr );
		if ( fname == function->name ) {
			// call to same kind of function
			const ast::Expr * firstParam = appExpr->args.front();

			if ( isThisExpression( firstParam, thisParam ) ) {
				// if calling another constructor on thisParam, assume that function handles
				// all members - if it doesn't a warning will appear in that function.
				unhandled.clear();
			} else if ( auto memberExpr = isThisMemberExpr( firstParam, thisParam ) ) {
				// if first parameter is a member expression on the this parameter,
				// then remove the member from unhandled set.
				if ( isThisExpression( memberExpr->aggregate, thisParam ) ) {
					unhandled.erase( memberExpr->member );
				}
			}
		}
	}

	void GenStructMemberCalls::previsit( const ast::MemberExpr * memberExpr ) {
		if ( ! checkWarnings( function ) || ! isCtor ) {
			visit_children = false;
			return;
		}

		if ( isThisExpression( memberExpr->aggregate, thisParam ) ) {
			if ( unhandled.count( memberExpr->member ) ) {
				// emit a warning because a member was used before it was constructed
				usedUninit.insert( { memberExpr->member, memberExpr->location } );
			}
		}
	}

	template< typename Visitor, typename... Params >
	void error( Visitor & v, CodeLocation loc, const Params &... params ) {
		SemanticErrorException err( loc, toString( params... ) );
		v.errors.append( err );
	}

	template< typename... Params >
	void GenStructMemberCalls::emit( CodeLocation loc, const Params &... params ) {
		// toggle warnings vs. errors here.
		// warn( params... );
		error( *this, loc, params... );
	}

	const ast::Expr * GenStructMemberCalls::postvisit( const ast::UntypedExpr * untypedExpr ) {
		// xxx - functions returning ast::ptr seems wrong...
		auto res = ResolvExpr::findVoidExpression( untypedExpr, { symtab, transUnit().global } );
		// Fix CodeLocation (at least until resolver is fixed).
		auto fix = localFillCodeLocations( untypedExpr->location, res.release() );
		return strict_dynamic_cast<const ast::Expr *>( fix );
	}

	void InsertImplicitCalls::previsit(const ast::UniqueExpr * unqExpr) {
		if (visitedIds.count(unqExpr->id)) visit_children = false;
		else visitedIds.insert(unqExpr->id);
	}

	const ast::Expr * FixCtorExprs::postvisit( const ast::ConstructorExpr * ctorExpr ) {
		const CodeLocation loc = ctorExpr->location;
		static UniqueName tempNamer( "_tmp_ctor_expr" );
		// xxx - is the size check necessary?
		assert( ctorExpr->result && ctorExpr->result->size() == 1 );

		// xxx - this can be TupleAssignExpr now. Need to properly handle this case.
		// take possession of expr and env
		ast::ptr<ast::ApplicationExpr> callExpr = ctorExpr->callExpr.strict_as<ast::ApplicationExpr>();
		ast::ptr<ast::TypeSubstitution> env = ctorExpr->env;
		// ctorExpr->set_callExpr( nullptr );
		// ctorExpr->set_env( nullptr );

		// xxx - ideally we would reuse the temporary generated from the copy constructor passes from within firstArg if it exists and not generate a temporary if it's unnecessary.
		auto tmp = new ast::ObjectDecl(loc, tempNamer.newName(), callExpr->args.front()->result );
		declsToAddBefore.push_back( tmp );
		// delete ctorExpr;

		// build assignment and replace constructor's first argument with new temporary
		auto mutCallExpr = callExpr.get_and_mutate();
		const ast::Expr * firstArg = callExpr->args.front();
		ast::Expr * assign = new ast::UntypedExpr(loc, new ast::NameExpr(loc, "?=?" ), { new ast::AddressExpr(loc, new ast::VariableExpr(loc, tmp ) ), new ast::AddressExpr( firstArg ) } );
		firstArg = new ast::VariableExpr(loc, tmp );
		mutCallExpr->args.front() = firstArg;

		// resolve assignment and dispose of new env
		auto resolved = ResolvExpr::findVoidExpression( assign, { symtab, transUnit().global } );
		auto mut = resolved.get_and_mutate();
		assertf(resolved.get() == mut, "newly resolved expression must be unique");
		mut->env = nullptr;

		// for constructor expr:
		//   T x;
		//   x{};
		// results in:
		//   T x;
		//   T & tmp;
		//   &tmp = &x, ?{}(tmp), tmp
		ast::CommaExpr * commaExpr = new ast::CommaExpr(loc, resolved, new ast::CommaExpr(loc, mutCallExpr, new ast::VariableExpr(loc, tmp ) ) );
		commaExpr->env = env;
		return commaExpr;
	}
} // namespace
} // namespace InitTweak

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
