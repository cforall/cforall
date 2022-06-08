//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixInit.cc --
//
// Author           : Rob Schluntz
// Created On       : Wed Jan 13 16:29:30 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 16 04:17:07 2020
// Update Count     : 82
//
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

bool ctordtorp = false; // print all debug
bool ctorp = false; // print ctor debug
bool cpctorp = false; // print copy ctor debug
bool dtorp = false; // print dtor debug
#define PRINT( text ) if ( ctordtorp ) { text }
#define CP_CTOR_PRINT( text ) if ( ctordtorp || cpctorp ) { text }
#define DTOR_PRINT( text ) if ( ctordtorp || dtorp ) { text }

namespace InitTweak {
	namespace {
		struct SelfAssignChecker {
			void previsit( ApplicationExpr * appExpr );
		};

		struct StmtExprResult {
			static void link( std::list< Declaration * > & translationUnit );

			void previsit( StmtExpr * stmtExpr );
		};

		struct InsertImplicitCalls : public WithConstTypeSubstitution {
			/// wrap function application expressions as ImplicitCopyCtorExpr nodes so that it is easy to identify which
			/// function calls need their parameters to be copy constructed
			static void insert( std::list< Declaration * > & translationUnit );

			Expression * postmutate( ApplicationExpr * appExpr );
		};

		struct ResolveCopyCtors final : public WithStmtsToAdd, public WithIndexer, public WithShortCircuiting, public WithTypeSubstitution, public WithVisitorRef<ResolveCopyCtors> {
			/// generate temporary ObjectDecls for each argument and return value of each ImplicitCopyCtorExpr,
			/// generate/resolve copy construction expressions for each, and generate/resolve destructors for both
			/// arguments and return value temporaries
			static void resolveImplicitCalls( std::list< Declaration * > & translationUnit );

			Expression * postmutate( ImplicitCopyCtorExpr * impCpCtorExpr );
			void premutate( StmtExpr * stmtExpr );
			void premutate( UniqueExpr * unqExpr );

			/// create and resolve ctor/dtor expression: fname(var, [cpArg])
			Expression * makeCtorDtor( const std::string & fname, ObjectDecl * var, Expression * cpArg = NULL );
			/// true if type does not need to be copy constructed to ensure correctness
			bool skipCopyConstruct( Type * type );
			void copyConstructArg( Expression *& arg, ImplicitCopyCtorExpr * impCpCtorExpr, Type * formal );
			void destructRet( ObjectDecl * ret, ImplicitCopyCtorExpr * impCpCtorExpr, Expression *& arg );
		};

		/// collects constructed object decls - used as a base class
		struct ObjDeclCollector : public WithGuards, public WithShortCircuiting {
			// use ordered data structure to maintain ordering for set_difference and for consistent error messages
			typedef std::list< ObjectDecl * > ObjectSet;
			void previsit( CompoundStmt *compoundStmt );
			void previsit( DeclStmt *stmt );

			// don't go into other functions
			void previsit( FunctionDecl * ) { visit_children = false; }

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
			for ( ObjectDecl * obj : set.objs ) {
				out << obj->get_name() << ", " ;
			} // for
			out << " }";
			return out;
		}

		struct LabelFinder final : public ObjDeclCollector {
			typedef std::map< Label, ObjectSet > LabelMap;
			// map of Label -> live variables at that label
			LabelMap vars;

			typedef ObjDeclCollector Parent;
			using Parent::previsit;
			void previsit( Statement * stmt );

			void previsit( CompoundStmt *compoundStmt );
			void previsit( DeclStmt *stmt );
		};

		struct InsertDtors final : public ObjDeclCollector, public WithStmtsToAdd {
			/// insert destructor calls at the appropriate places.  must happen before CtorInit nodes are removed
			/// (currently by FixInit)
			static void insert( std::list< Declaration * > & translationUnit );

			typedef std::list< ObjectDecl * > OrderedDecls;
			typedef std::list< OrderedDecls > OrderedDeclsStack;

			InsertDtors( PassVisitor<LabelFinder> & finder ) : finder( finder ), labelVars( finder.pass.vars ) {}

			typedef ObjDeclCollector Parent;
			using Parent::previsit;

			void previsit( FunctionDecl * funcDecl );

			void previsit( BranchStmt * stmt );
		private:
			void handleGoto( BranchStmt * stmt );

			PassVisitor<LabelFinder> & finder;
			LabelFinder::LabelMap & labelVars;
			OrderedDeclsStack reverseDeclOrder;
		};

		class FixInit : public WithStmtsToAdd {
		  public:
			/// expand each object declaration to use its constructor after it is declared.
			static void fixInitializers( std::list< Declaration * > &translationUnit );

			DeclarationWithType * postmutate( ObjectDecl *objDecl );

			std::list< Declaration * > staticDtorDecls;
		};

		struct GenStructMemberCalls final : public WithGuards, public WithShortCircuiting, public WithIndexer, public WithVisitorRef<GenStructMemberCalls> {
			/// generate default/copy ctor and dtor calls for user-defined struct ctor/dtors
			/// for any member that is missing a corresponding ctor/dtor call.
			/// error if a member is used before constructed
			static void generate( std::list< Declaration * > & translationUnit );

			void premutate( FunctionDecl * funcDecl );
			DeclarationWithType * postmutate( FunctionDecl * funcDecl );

			void premutate( MemberExpr * memberExpr );
			void premutate( ApplicationExpr * appExpr );

			/// Note: this post mutate used to be in a separate visitor. If this pass breaks, one place to examine is whether it is
			/// okay for this part of the recursion to occur alongside the rest.
			Expression * postmutate( UntypedExpr * expr );

			SemanticErrorException errors;
		  private:
			template< typename... Params >
			void emit( CodeLocation, const Params &... params );

			FunctionDecl * function = nullptr;
			std::set< DeclarationWithType * > unhandled;
			std::map< DeclarationWithType *, CodeLocation > usedUninit;
			ObjectDecl * thisParam = nullptr;
			bool isCtor = false; // true if current function is a constructor
			StructDecl * structDecl = nullptr;
		};

		struct FixCtorExprs final : public WithDeclsToAdd, public WithIndexer {
			/// expands ConstructorExpr nodes into comma expressions, using a temporary for the first argument
			static void fix( std::list< Declaration * > & translationUnit );

			Expression * postmutate( ConstructorExpr * ctorExpr );
		};

		struct SplitExpressions : public WithShortCircuiting, /*public WithTypeSubstitution, */public WithStmtsToAdd {
			/// add CompoundStmts around top-level expressions so that temporaries are destroyed in the correct places.
			static void split( std::list< Declaration * > &translationUnit );

			Statement * postmutate( ExprStmt * stmt );
			void premutate( TupleAssignExpr * expr );
		};
	} // namespace

	void fix( std::list< Declaration * > & translationUnit, bool inLibrary ) {
		PassVisitor<SelfAssignChecker> checker;
		acceptAll( translationUnit, checker );

		// fixes StmtExpr to properly link to their resulting expression
		StmtExprResult::link( translationUnit );

		// fixes ConstructorInit for global variables. should happen before fixInitializers.
		InitTweak::fixGlobalInit( translationUnit, inLibrary );

		// must happen before ResolveCopyCtors because temporaries have to be inserted into the correct scope
		SplitExpressions::split( translationUnit );

		InsertImplicitCalls::insert( translationUnit );

		// Needs to happen before ResolveCopyCtors, because argument/return temporaries should not be considered in
		// error checking branch statements
		InsertDtors::insert( translationUnit );

		ResolveCopyCtors::resolveImplicitCalls( translationUnit );
		FixInit::fixInitializers( translationUnit );
		GenStructMemberCalls::generate( translationUnit );

		// Needs to happen after GenStructMemberCalls, since otherwise member constructors exprs
		// don't have the correct form, and a member can be constructed more than once.
		FixCtorExprs::fix( translationUnit );
	}

	namespace {
		/// find and return the destructor used in `input`. If `input` is not a simple destructor call, generate a thunk
		/// that wraps the destructor, insert it into `stmtsToAdd` and return the new function declaration
		DeclarationWithType * getDtorFunc( ObjectDecl * objDecl, Statement * input, std::list< Statement * > & stmtsToAdd ) {
			// unwrap implicit statement wrapper
			Statement * dtor = input;
			assert( dtor );
			std::list< Expression * > matches;
			collectCtorDtorCalls( dtor, matches );

			if ( dynamic_cast< ExprStmt * >( dtor ) ) {
				// only one destructor call in the expression
				if ( matches.size() == 1 ) {
					DeclarationWithType * func = getFunction( matches.front() );
					assertf( func, "getFunction failed to find function in %s", toString( matches.front() ).c_str() );

					// cleanup argument must be a function, not an object (including function pointer)
					if ( FunctionDecl * dtorFunc = dynamic_cast< FunctionDecl * > ( func ) ) {
						if ( dtorFunc->type->forall.empty() ) {
							// simple case where the destructor is a monomorphic function call - can simply
							// use that function as the cleanup function.
							delete dtor;
							return func;
						}
					}
				}
			}

			// otherwise the cleanup is more complicated - need to build a single argument cleanup function that
			// wraps the more complicated code.
			static UniqueName dtorNamer( "__cleanup_dtor" );
			std::string name = dtorNamer.newName();
			FunctionDecl * dtorFunc = FunctionDecl::newFunction( name, SymTab::genDefaultType( objDecl->type->stripReferences(), false ), new CompoundStmt() );
			stmtsToAdd.push_back( new DeclStmt( dtorFunc ) );

			// the original code contains uses of objDecl - replace them with the newly generated 'this' parameter.
			ObjectDecl * thisParam = getParamThis( dtorFunc->type );
			Expression * replacement = new VariableExpr( thisParam );

			Type * base = replacement->result->stripReferences();
			if ( dynamic_cast< ArrayType * >( base ) || dynamic_cast< TupleType * > ( base ) ) {
				// need to cast away reference for array types, since the destructor is generated without the reference type,
				// and for tuple types since tuple indexing does not work directly on a reference
				replacement = new CastExpr( replacement, base->clone() );
			}
			DeclReplacer::replace( dtor, { std::make_pair( objDecl, replacement ) } );
			dtorFunc->statements->push_back( strict_dynamic_cast<Statement *>( dtor ) );

			return dtorFunc;
		}

		void StmtExprResult::link( std::list< Declaration * > & translationUnit ) {
			PassVisitor<StmtExprResult> linker;
			acceptAll( translationUnit, linker );
		}

		void SplitExpressions::split( std::list< Declaration * > & translationUnit ) {
			PassVisitor<SplitExpressions> splitter;
			mutateAll( translationUnit, splitter );
		}

		void InsertImplicitCalls::insert( std::list< Declaration * > & translationUnit ) {
			PassVisitor<InsertImplicitCalls> inserter;
			mutateAll( translationUnit, inserter );
		}

		void ResolveCopyCtors::resolveImplicitCalls( std::list< Declaration * > & translationUnit ) {
			PassVisitor<ResolveCopyCtors> resolver;
			mutateAll( translationUnit, resolver );
		}

		void FixInit::fixInitializers( std::list< Declaration * > & translationUnit ) {
			PassVisitor<FixInit> fixer;

			// can't use mutateAll, because need to insert declarations at top-level
			// can't use DeclMutator, because sometimes need to insert IfStmt, etc.
			SemanticErrorException errors;
			for ( std::list< Declaration * >::iterator i = translationUnit.begin(); i != translationUnit.end(); ++i ) {
				try {
					maybeMutate( *i, fixer );
					translationUnit.splice( i, fixer.pass.staticDtorDecls );
				} catch( SemanticErrorException &e ) {
					errors.append( e );
				} // try
			} // for
			if ( ! errors.isEmpty() ) {
				throw errors;
			} // if
		}

		void InsertDtors::insert( std::list< Declaration * > & translationUnit ) {
			PassVisitor<LabelFinder> finder;
			PassVisitor<InsertDtors> inserter( finder );
			acceptAll( translationUnit, inserter );
		}

		void GenStructMemberCalls::generate( std::list< Declaration * > & translationUnit ) {
			PassVisitor<GenStructMemberCalls> warner;
			mutateAll( translationUnit, warner );
		}

		void FixCtorExprs::fix( std::list< Declaration * > & translationUnit ) {
			PassVisitor<FixCtorExprs> fixer;
			mutateAll( translationUnit, fixer );
		}

		void StmtExprResult::previsit( StmtExpr * stmtExpr ) {
			// we might loose the result expression here so add a pointer to trace back
			assert( stmtExpr->result );
			Type * result = stmtExpr->result;
			if ( ! result->isVoid() ) {
				CompoundStmt * body = stmtExpr->statements;
				assert( ! body->kids.empty() );
				stmtExpr->resultExpr = strict_dynamic_cast< ExprStmt * >( body->kids.back() );
			}
		}

		Statement * SplitExpressions::postmutate( ExprStmt * stmt ) {
			// wrap each top-level ExprStmt in a block so that destructors for argument and return temporaries are destroyed
			// in the correct places
			CompoundStmt * ret = new CompoundStmt( { stmt } );
			return ret;
		}

		void SplitExpressions::premutate( TupleAssignExpr * ) {
			// don't do this within TupleAssignExpr, since it is already broken up into multiple expressions
			visit_children = false;
		}

		// Relatively simple structural comparison for expressions, needed to determine
		// if two expressions are "the same" (used to determine if self assignment occurs)
		struct StructuralChecker {
			Expression * stripCasts( Expression * expr ) {
				// this might be too permissive. It's possible that only particular casts are relevant.
				while ( CastExpr * cast = dynamic_cast< CastExpr * >( expr ) ) {
					expr = cast->arg;
				}
				return expr;
			}

			void previsit( Expression * ) {
				// anything else does not qualify
				isSimilar = false;
			}

			template<typename T>
			T * cast( Expression * node ) {
				// all expressions need to ignore casts, so this bit has been factored out
				return dynamic_cast< T * >( stripCasts( node ) );
			}

			// ignore casts
			void previsit( CastExpr * ) {}

			void previsit( MemberExpr * memExpr ) {
				if ( MemberExpr * otherMember = cast< MemberExpr >( other ) ) {
					if ( otherMember->member == memExpr->member ) {
						other = otherMember->aggregate;
						return;
					}
				}
				isSimilar = false;
			}

			void previsit( VariableExpr * varExpr ) {
				if ( VariableExpr * otherVar = cast< VariableExpr >( other ) ) {
					if ( otherVar->var == varExpr->var ) {
						return;
					}
				}
				isSimilar = false;
			}

			void previsit( AddressExpr * ) {
				if ( AddressExpr * addrExpr = cast< AddressExpr >( other ) ) {
					other = addrExpr->arg;
					return;
				}
				isSimilar = false;
			}

			Expression * other = nullptr;
			bool isSimilar = true;
		};

		bool structurallySimilar( Expression * e1, Expression * e2 ) {
			PassVisitor<StructuralChecker> checker;
			checker.pass.other = e2;
			e1->accept( checker );
			return checker.pass.isSimilar;
		}

		void SelfAssignChecker::previsit( ApplicationExpr * appExpr ) {
			DeclarationWithType * function = getFunction( appExpr );
			if ( function->name == "?=?" ) { // doesn't use isAssignment, because ?+=?, etc. should not count as self-assignment
				if ( appExpr->args.size() == 2 ) {
					// check for structural similarity (same variable use, ignore casts, etc. - but does not look too deeply, anything looking like a function is off limits)
					if ( structurallySimilar( appExpr->args.front(), appExpr->args.back() ) ) {
						SemanticWarning( appExpr->location, Warning::SelfAssignment, toCString( appExpr->args.front() ) );
					}
				}
			}
		}

		Expression * InsertImplicitCalls::postmutate( ApplicationExpr * appExpr ) {
			if ( VariableExpr * function = dynamic_cast< VariableExpr * > ( appExpr->get_function() ) ) {
				if ( function->var->linkage.is_builtin ) {
					// optimization: don't need to copy construct in order to call intrinsic functions
					return appExpr;
				} else if ( DeclarationWithType * funcDecl = dynamic_cast< DeclarationWithType * > ( function->get_var() ) ) {
					FunctionType * ftype = dynamic_cast< FunctionType * >( GenPoly::getFunctionType( funcDecl->get_type() ) );
					assertf( ftype, "Function call without function type: %s", toString( funcDecl ).c_str() );
					if ( CodeGen::isConstructor( funcDecl->get_name() ) && ftype->parameters.size() == 2 ) {
						Type * t1 = getPointerBase( ftype->parameters.front()->get_type() );
						Type * t2 = ftype->parameters.back()->get_type();
						assert( t1 );

						if ( ResolvExpr::typesCompatible( t1, t2, SymTab::Indexer() ) ) {
							// optimization: don't need to copy construct in order to call a copy constructor
							return appExpr;
						} // if
					} else if ( CodeGen::isDestructor( funcDecl->get_name() ) ) {
						// correctness: never copy construct arguments to a destructor
						return appExpr;
					} // if
				} // if
			} // if
			CP_CTOR_PRINT( std::cerr << "InsertImplicitCalls: adding a wrapper " << appExpr << std::endl; )

			// wrap each function call so that it is easy to identify nodes that have to be copy constructed
			ImplicitCopyCtorExpr * expr = new ImplicitCopyCtorExpr( appExpr );
			// Move the type substitution to the new top-level, if it is attached to the appExpr.
			// Ensure it is not deleted with the ImplicitCopyCtorExpr by removing it before deletion.
			// The substitution is needed to obtain the type of temporary variables so that copy constructor
			// calls can be resolved.
			assert( env );
			std::swap( expr->env, appExpr->env );
			return expr;
		}

		bool ResolveCopyCtors::skipCopyConstruct( Type * type ) { return ! isConstructable( type ); }

		Expression * ResolveCopyCtors::makeCtorDtor( const std::string & fname, ObjectDecl * var, Expression * cpArg ) {
			assert( var );
			// arrays are not copy constructed, so this should always be an ExprStmt
			ImplicitCtorDtorStmt * stmt = genCtorDtor( fname, var, cpArg );
			assertf( stmt, "ResolveCopyCtors: genCtorDtor returned nullptr: %s / %s / %s", fname.c_str(), toString( var ).c_str(), toString( cpArg ).c_str() );
			ExprStmt * exprStmt = strict_dynamic_cast< ExprStmt * >( stmt->callStmt );
			Expression * resolved = exprStmt->expr;
			exprStmt->expr = nullptr; // take ownership of expr

			// resolve copy constructor
			// should only be one alternative for copy ctor and dtor expressions, since all arguments are fixed
			// (VariableExpr and already resolved expression)
			CP_CTOR_PRINT( std::cerr << "ResolvingCtorDtor " << resolved << std::endl; )
			ResolvExpr::findVoidExpression( resolved, indexer );
			assert( resolved );
			if ( resolved->env ) {
				// Extract useful information and discard new environments. Keeping them causes problems in PolyMutator passes.
				env->add( *resolved->env );
				delete resolved->env;
				resolved->env = nullptr;
			} // if
			delete stmt;
			if ( TupleAssignExpr * assign = dynamic_cast< TupleAssignExpr * >( resolved ) ) {
				// fix newly generated StmtExpr
				premutate( assign->stmtExpr );
			}
			return resolved;
		}

		void ResolveCopyCtors::copyConstructArg( Expression *& arg, ImplicitCopyCtorExpr * impCpCtorExpr, Type * formal ) {
			static UniqueName tempNamer("_tmp_cp");
			assert( env );
			CP_CTOR_PRINT( std::cerr << "Type Substitution: " << *env << std::endl; )
			assert( arg->result );
			Type * result = arg->result;
			if ( skipCopyConstruct( result ) ) return; // skip certain non-copyable types

			// type may involve type variables, so apply type substitution to get temporary variable's actual type,
			// since result type may not be substituted (e.g., if the type does not appear in the parameter list)
			// Use applyFree so that types bound in function pointers are not substituted, e.g. in forall(dtype T) void (*)(T).
			env->applyFree( result );
			ObjectDecl * tmp = ObjectDecl::newObject( "__tmp", result, nullptr );
			tmp->get_type()->set_const( false );

			// create and resolve copy constructor
			CP_CTOR_PRINT( std::cerr << "makeCtorDtor for an argument" << std::endl; )
			Expression * cpCtor = makeCtorDtor( "?{}", tmp, arg );

			if ( ApplicationExpr * appExpr = dynamic_cast< ApplicationExpr * >( cpCtor ) ) {
				// if the chosen constructor is intrinsic, the copy is unnecessary, so
				// don't create the temporary and don't call the copy constructor
				VariableExpr * function = strict_dynamic_cast< VariableExpr * >( appExpr->function );
				if ( function->var->linkage == LinkageSpec::Intrinsic ) {
					// arguments that need to be boxed need a temporary regardless of whether the copy constructor is intrinsic,
					// so that the object isn't changed inside of the polymorphic function
					if ( ! GenPoly::needsBoxing( formal, result, impCpCtorExpr->callExpr, env ) ) return;
					// xxx - leaking tmp
				}
			}

			// set a unique name for the temporary once it's certain the call is necessary
			tmp->name = tempNamer.newName();

			// replace argument to function call with temporary
			stmtsToAddBefore.push_back( new DeclStmt( tmp ) );
			arg = cpCtor;
			destructRet( tmp, impCpCtorExpr, arg );

			// impCpCtorExpr->dtors.push_front( makeCtorDtor( "^?{}", tmp ) );
		}

		void ResolveCopyCtors::destructRet( ObjectDecl * ret, ImplicitCopyCtorExpr * /*impCpCtorExpr*/, Expression *& arg ) {
			// TODO: refactor code for generating cleanup attribute, since it's common and reused in ~3-4 places
			// check for existing cleanup attribute before adding another(?)
			// need to add __Destructor for _tmp_cp variables as well

			assertf( Validate::dtorStruct && Validate::dtorStruct->members.size() == 2, "Destructor generation requires __Destructor definition." );
			assertf( Validate::dtorStructDestroy, "Destructor generation requires __destroy_Destructor." );

			// generate a __Destructor for ret that calls the destructor
			Expression * dtor = makeCtorDtor( "^?{}", ret );

			// if the chosen destructor is intrinsic, elide the generated dtor handler
			if ( arg && isIntrinsicCallExpr( dtor ) ) {
				arg = new CommaExpr( arg, new VariableExpr( ret ) );
				return;
			}

			if ( ! dtor->env ) dtor->env = maybeClone( env );
			DeclarationWithType * dtorFunc = getDtorFunc( ret, new ExprStmt( dtor ), stmtsToAddBefore );

			StructInstType * dtorStructType = new StructInstType( Type::Qualifiers(), Validate::dtorStruct );
			dtorStructType->parameters.push_back( new TypeExpr( new VoidType( Type::Qualifiers() ) ) );

			// cast destructor pointer to void (*)(void *), to silence GCC incompatible pointer warnings
			FunctionType * dtorFtype = new FunctionType( Type::Qualifiers(), false );
			dtorFtype->parameters.push_back( ObjectDecl::newObject( "", new PointerType( Type::Qualifiers(), new VoidType( Type::Qualifiers() ) ), nullptr ) );
			Type * dtorType = new PointerType( Type::Qualifiers(), dtorFtype );

			static UniqueName namer( "_ret_dtor" );
			ObjectDecl * retDtor = ObjectDecl::newObject( namer.newName(), dtorStructType, new ListInit( { new SingleInit( new ConstantExpr( Constant::null() ) ), new SingleInit( new CastExpr( new VariableExpr( dtorFunc ), dtorType ) ) } ) );
			retDtor->attributes.push_back( new Attribute( "cleanup", { new VariableExpr( Validate::dtorStructDestroy ) } ) );
			stmtsToAddBefore.push_back( new DeclStmt( retDtor ) );

			if ( arg ) {
				Expression * member = new MemberExpr( strict_dynamic_cast<DeclarationWithType *>( Validate::dtorStruct->members.front() ), new VariableExpr( retDtor ) );
				Expression * object = new CastExpr( new AddressExpr( new VariableExpr( ret ) ), new PointerType( Type::Qualifiers(), new VoidType( Type::Qualifiers() ) ) );
				Expression * assign = createBitwiseAssignment( member, object );
				arg = new CommaExpr( new CommaExpr( arg, assign ), new VariableExpr( ret ) );
			}

			// impCpCtorExpr->get_dtors().push_front( makeCtorDtor( "^?{}", ret ) );
		}

		Expression * ResolveCopyCtors::postmutate( ImplicitCopyCtorExpr *impCpCtorExpr ) {
			CP_CTOR_PRINT( std::cerr << "ResolveCopyCtors: " << impCpCtorExpr << std::endl; )

			ApplicationExpr * appExpr = impCpCtorExpr->callExpr;
			ObjectDecl * returnDecl = nullptr;

			// take each argument and attempt to copy construct it.
			FunctionType * ftype = GenPoly::getFunctionType( appExpr->function->result );
			assert( ftype );
			auto & params = ftype->parameters;
			auto iter = params.begin();
			for ( Expression * & arg : appExpr->args ) {
				Type * formal = nullptr;
				if ( iter != params.end() ) { // does not copy construct C-style variadic arguments
					DeclarationWithType * param = *iter++;
					formal = param->get_type();
				}

				copyConstructArg( arg, impCpCtorExpr, formal );
			} // for

			// each return value from the call needs to be connected with an ObjectDecl at the call site, which is
			// initialized with the return value and is destructed later
			// xxx - handle named return values?
			Type * result = appExpr->result;
			if ( ! result->isVoid() ) {
				static UniqueName retNamer("_tmp_cp_ret");
				result = result->clone();
				env->apply( result );
				ObjectDecl * ret = ObjectDecl::newObject( retNamer.newName(), result, nullptr );
				ret->type->set_const( false );
				returnDecl = ret;
				stmtsToAddBefore.push_back( new DeclStmt( ret ) );
				CP_CTOR_PRINT( std::cerr << "makeCtorDtor for a return" << std::endl; )
			} // for
			CP_CTOR_PRINT( std::cerr << "after Resolving: " << impCpCtorExpr << std::endl; )
			// ------------------------------------------------------

			CP_CTOR_PRINT( std::cerr << "Coming out the back..." << impCpCtorExpr << std::endl; )

			// detach fields from wrapper node so that it can be deleted without deleting too much
			impCpCtorExpr->callExpr = nullptr;
			std::swap( impCpCtorExpr->env, appExpr->env );
			assert( impCpCtorExpr->env == nullptr );
			delete impCpCtorExpr;

			if ( returnDecl ) {
				Expression * assign = createBitwiseAssignment( new VariableExpr( returnDecl ), appExpr );
				if ( ! dynamic_cast< ReferenceType * >( result ) ) {
					// destructing reference returns is bad because it can cause multiple destructor calls to the same object - the returned object is not a temporary
					destructRet( returnDecl, impCpCtorExpr, assign );
				} else {
					assign = new CommaExpr( assign, new VariableExpr( returnDecl ) );
				}
				// move env from appExpr to retExpr
				std::swap( assign->env, appExpr->env );
				return assign;
			} else {
				return appExpr;
			} // if
		}

		void ResolveCopyCtors::premutate( StmtExpr * stmtExpr ) {
			// function call temporaries should be placed at statement-level, rather than nested inside of a new statement expression,
			// since temporaries can be shared across sub-expressions, e.g.
			//   [A, A] f();       // decl
			//   g([A] x, [A] y);  // decl
			//   g(f());           // call
			// f is executed once, so the return temporary is shared across the tuple constructors for x and y.
			// Explicitly mutating children instead of mutating the inner compound statement forces the temporaries to be added
			// to the outer context, rather than inside of the statement expression.
			visit_children = false;

			assert( env );

			indexer.enterScope();
			// visit all statements
			std::list< Statement * > & stmts = stmtExpr->statements->get_kids();
			for ( Statement *& stmt : stmts ) {
				stmt = stmt->acceptMutator( *visitor );
			} // for
			indexer.leaveScope();

			assert( stmtExpr->result );
			Type * result = stmtExpr->result;
			if ( ! result->isVoid() ) {
				static UniqueName retNamer("_tmp_stmtexpr_ret");

				result = result->clone();
				env->apply( result );
				if ( ! InitTweak::isConstructable( result ) ) {
					delete result;
					return;
				}

				// create variable that will hold the result of the stmt expr
				ObjectDecl * ret = ObjectDecl::newObject( retNamer.newName(), result, nullptr );
				ret->type->set_const( false );
				stmtsToAddBefore.push_back( new DeclStmt( ret ) );

				assertf(
					stmtExpr->resultExpr,
					"Statement-Expression should have a resulting expression at %s:%d",
					stmtExpr->location.filename.c_str(),
					stmtExpr->location.first_line
				);

				ExprStmt * last = stmtExpr->resultExpr;
				try {
					last->expr = makeCtorDtor( "?{}", ret, last->expr );
				} catch(...) {
					std::cerr << "*CFA internal error: ";
					std::cerr << "can't resolve implicit constructor";
					std::cerr << " at " << stmtExpr->location.filename;
					std::cerr << ":" << stmtExpr->location.first_line << std::endl;

					abort();
				}

				// add destructors after current statement
				stmtsToAddAfter.push_back( new ExprStmt( makeCtorDtor( "^?{}", ret ) ) );

				// must have a non-empty body, otherwise it wouldn't have a result
				assert( ! stmts.empty() );

				// if there is a return decl, add a use as the last statement; will not have return decl on non-constructable returns
				stmts.push_back( new ExprStmt( new VariableExpr( ret ) ) );
			} // if

			assert( stmtExpr->returnDecls.empty() );
			assert( stmtExpr->dtors.empty() );
		}

		// to prevent warnings ('_unq0' may be used uninitialized in this function),
		// insert an appropriate zero initializer for UniqueExpr temporaries.
		Initializer * makeInit( Type * t ) {
			if ( StructInstType * inst = dynamic_cast< StructInstType * >( t ) ) {
				// initizer for empty struct must be empty
				if ( inst->baseStruct->members.empty() ) return new ListInit({});
			} else if ( UnionInstType * inst = dynamic_cast< UnionInstType * >( t ) ) {
				// initizer for empty union must be empty
				if ( inst->baseUnion->members.empty() ) return new ListInit({});
			}

			return new ListInit( { new SingleInit( new ConstantExpr( Constant::from_int( 0 ) ) ) } );
		}

		void ResolveCopyCtors::premutate( UniqueExpr * unqExpr ) {
			visit_children = false;
			// xxx - hack to prevent double-handling of unique exprs, otherwise too many temporary variables and destructors are generated
			static std::unordered_map< int, UniqueExpr * > unqMap;
			if ( ! unqMap.count( unqExpr->get_id() ) ) {
				// resolve expr and find its

				ImplicitCopyCtorExpr * impCpCtorExpr = dynamic_cast< ImplicitCopyCtorExpr * >( unqExpr->expr );
				// PassVisitor<ResolveCopyCtors> fixer;
				unqExpr->expr = unqExpr->expr->acceptMutator( *visitor );

				// it should never be necessary to wrap a void-returning expression in a UniqueExpr - if this assumption changes, this needs to be rethought
				assert( unqExpr->result );
				if ( impCpCtorExpr ) {
					CommaExpr * comma = strict_dynamic_cast< CommaExpr * >( unqExpr->expr );
					VariableExpr * var = strict_dynamic_cast<VariableExpr *>( comma->arg2 );
					// note the variable used as the result from the call
					unqExpr->var = var->clone();
				} else {
					// expr isn't a call expr, so create a new temporary variable to use to hold the value of the unique expression
					unqExpr->object = ObjectDecl::newObject( toString("_unq", unqExpr->get_id()), unqExpr->result->clone(), makeInit( unqExpr->result ) );
					unqExpr->var = new VariableExpr( unqExpr->object );
				}

				// stmtsToAddBefore.splice( stmtsToAddBefore.end(), fixer.pass.stmtsToAddBefore );
				// stmtsToAddAfter.splice( stmtsToAddAfter.end(), fixer.pass.stmtsToAddAfter );
				unqMap[unqExpr->get_id()] = unqExpr;
			} else {
				// take data from other UniqueExpr to ensure consistency
				delete unqExpr->get_expr();
				unqExpr->expr = unqMap[unqExpr->get_id()]->expr->clone();
				delete unqExpr->result;
				unqExpr->result = maybeClone( unqExpr->expr->result );
			}
		}

		DeclarationWithType * FixInit::postmutate( ObjectDecl *objDecl ) {
			// since this removes the init field from objDecl, it must occur after children are mutated (i.e. postmutate)
			if ( ConstructorInit * ctorInit = dynamic_cast< ConstructorInit * >( objDecl->get_init() ) ) {
				// a decision should have been made by the resolver, so ctor and init are not both non-NULL
				assert( ! ctorInit->get_ctor() || ! ctorInit->get_init() );
				if ( Statement * ctor = ctorInit->get_ctor() ) {
					if ( objDecl->get_storageClasses().is_static ) {

						// The ojbect needs to go in the data section, regardless of dtor complexity below.
						// The attribute works, and is meant to apply, both for leaving the static local alone,
						// and for hoisting it out as a static global.
						addDataSectionAttribute( objDecl );

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
						BasicType * boolType = new BasicType( Type::Qualifiers(), BasicType::Bool );
						SingleInit * boolInitExpr = new SingleInit( new ConstantExpr( Constant::from_int( 1 ) ) );
						ObjectDecl * isUninitializedVar = new ObjectDecl( objDecl->get_mangleName() + "_uninitialized", Type::StorageClasses( Type::Static ), LinkageSpec::Cforall, 0, boolType, boolInitExpr );
						isUninitializedVar->fixUniqueId();

						// __objName_uninitialized = false;
						UntypedExpr * setTrue = new UntypedExpr( new NameExpr( "?=?" ) );
						setTrue->get_args().push_back( new VariableExpr( isUninitializedVar ) );
						setTrue->get_args().push_back( new ConstantExpr( Constant::from_int( 0 ) ) );

						// generate body of if
						CompoundStmt * initStmts = new CompoundStmt();
						std::list< Statement * > & body = initStmts->get_kids();
						body.push_back( ctor );
						body.push_back( new ExprStmt( setTrue ) );

						// put it all together
						IfStmt * ifStmt = new IfStmt( new VariableExpr( isUninitializedVar ), initStmts, 0 );
						stmtsToAddAfter.push_back( new DeclStmt( isUninitializedVar ) );
						stmtsToAddAfter.push_back( ifStmt );

						Statement * dtor = ctorInit->get_dtor();
						objDecl->set_init( nullptr );
						ctorInit->set_ctor( nullptr );
						ctorInit->set_dtor( nullptr );
						if ( dtor ) {
							// if the object has a non-trivial destructor, have to
							// hoist it and the object into the global space and
							// call the destructor function with atexit.

							Statement * dtorStmt = dtor->clone();

							// void __objName_dtor_atexitN(...) {...}
							FunctionDecl * dtorCaller = new FunctionDecl( objDecl->get_mangleName() + dtorCallerNamer.newName(), Type::StorageClasses( Type::Static ), LinkageSpec::C, new FunctionType( Type::Qualifiers(), false ), new CompoundStmt() );
							dtorCaller->fixUniqueId();
							dtorCaller->get_statements()->push_back( dtorStmt );

							// atexit(dtor_atexit);
							UntypedExpr * callAtexit = new UntypedExpr( new NameExpr( "atexit" ) );
							callAtexit->get_args().push_back( new VariableExpr( dtorCaller ) );

							body.push_back( new ExprStmt( callAtexit ) );

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
							objDecl->set_name( objDecl->get_name() + staticNamer.newName() );
							objDecl->set_mangleName( SymTab::Mangler::mangle( objDecl ) );

							// xxx - temporary hack: need to return a declaration, but want to hoist the current object out of this scope
							// create a new object which is never used
							static UniqueName dummyNamer( "_dummy" );
							ObjectDecl * dummy = new ObjectDecl( dummyNamer.newName(), Type::StorageClasses( Type::Static ), LinkageSpec::Cforall, 0, new PointerType( Type::Qualifiers(), new VoidType( Type::Qualifiers() ) ), 0, std::list< Attribute * >{ new Attribute("unused") } );
							delete ctorInit;
							return dummy;
						}
					} else {
						ImplicitCtorDtorStmt * implicit = strict_dynamic_cast< ImplicitCtorDtorStmt * > ( ctor );
						ExprStmt * ctorStmt = dynamic_cast< ExprStmt * >( implicit->callStmt );
						ApplicationExpr * ctorCall = nullptr;
						if ( ctorStmt && (ctorCall = isIntrinsicCallExpr( ctorStmt->expr )) && ctorCall->get_args().size() == 2 ) {
							// clean up intrinsic copy constructor calls by making them into SingleInits
							Expression * ctorArg = ctorCall->args.back();
							std::swap( ctorArg->env, ctorCall->env );
							objDecl->init = new SingleInit( ctorArg );

							ctorCall->args.pop_back();
						} else {
							stmtsToAddAfter.push_back( ctor );
							objDecl->init = nullptr;
							ctorInit->ctor = nullptr;
						}

						Statement * dtor = ctorInit->dtor;
						if ( dtor ) {
							ImplicitCtorDtorStmt * implicit = strict_dynamic_cast< ImplicitCtorDtorStmt * >( dtor );
							Statement * dtorStmt = implicit->callStmt;

							// don't need to call intrinsic dtor, because it does nothing, but
							// non-intrinsic dtors must be called
							if ( ! isIntrinsicSingleArgCallStmt( dtorStmt ) ) {
								// set dtor location to the object's location for error messages
								DeclarationWithType * dtorFunc = getDtorFunc( objDecl, dtorStmt, stmtsToAddBefore );
								objDecl->attributes.push_back( new Attribute( "cleanup", { new VariableExpr( dtorFunc ) } ) );
								ctorInit->dtor = nullptr;
							} // if
						}
					} // if
				} else if ( Initializer * init = ctorInit->init ) {
					objDecl->init = init;
					ctorInit->init = nullptr;
				} else {
					// no constructor and no initializer, which is okay
					objDecl->init = nullptr;
				} // if
				delete ctorInit;
			} // if
			return objDecl;
		}

		void ObjDeclCollector::previsit( CompoundStmt * ) {
			GuardValue( curVars );
		}

		void ObjDeclCollector::previsit( DeclStmt * stmt ) {
			// keep track of all variables currently in scope
			if ( ObjectDecl * objDecl = dynamic_cast< ObjectDecl * > ( stmt->get_decl() ) ) {
				curVars.push_back( objDecl );
			} // if
		}

		void LabelFinder::previsit( Statement * stmt ) {
			// for each label, remember the variables in scope at that label.
			for ( Label l : stmt->get_labels() ) {
				vars[l] = curVars;
			} // for
		}

		void LabelFinder::previsit( CompoundStmt * stmt ) {
			previsit( (Statement *)stmt );
			Parent::previsit( stmt );
		}

		void LabelFinder::previsit( DeclStmt * stmt ) {
			previsit( (Statement *)stmt );
			Parent::previsit( stmt );
		}


		void InsertDtors::previsit( FunctionDecl * funcDecl ) {
			// each function needs to have its own set of labels
			GuardValue( labelVars );
			labelVars.clear();
			// LabelFinder does not recurse into FunctionDecl, so need to visit
			// its children manually.
			maybeAccept( funcDecl->type, finder );
			maybeAccept( funcDecl->statements, finder );

			// all labels for this function have been collected, insert destructors as appropriate via implicit recursion.
		}

		// Handle break/continue/goto in the same manner as C++.  Basic idea: any objects that are in scope at the
		// BranchStmt but not at the labelled (target) statement must be destructed.  If there are any objects in scope
		// at the target location but not at the BranchStmt then those objects would be uninitialized so notify the user
		// of the error.  See C++ Reference 6.6 Jump Statements for details.
		void InsertDtors::handleGoto( BranchStmt * stmt ) {
			// can't do anything for computed goto
			if ( stmt->computedTarget ) return;

			assertf( stmt->get_target() != "", "BranchStmt missing a label: %s", toString( stmt ).c_str() );
			// S_L = lvars = set of objects in scope at label definition
			// S_G = curVars = set of objects in scope at goto statement
			ObjectSet & lvars = labelVars[ stmt->get_target() ];

			DTOR_PRINT(
				std::cerr << "at goto label: " << stmt->get_target().get_name() << std::endl;
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
				SemanticError( stmt, std::string("jump to label '") + stmt->get_target().get_name() + "' crosses initialization of " + (*diff.begin())->get_name() + " " );
			} // if
		}

		void InsertDtors::previsit( BranchStmt * stmt ) {
			switch( stmt->get_type() ) {
			  case BranchStmt::Continue:
			  case BranchStmt::Break:
				// could optimize the break/continue case, because the S_L-S_G check is unnecessary (this set should
				// always be empty), but it serves as a small sanity check.
			  case BranchStmt::Goto:
				handleGoto( stmt );
				break;
			  default:
				assert( false );
			} // switch
		}

		bool checkWarnings( FunctionDecl * funcDecl ) {
			// only check for warnings if the current function is a user-defined
			// constructor or destructor
			if ( ! funcDecl ) return false;
			if ( ! funcDecl->get_statements() ) return false;
			return CodeGen::isCtorDtor( funcDecl->get_name() ) && ! LinkageSpec::isOverridable( funcDecl->get_linkage() );
		}

		void GenStructMemberCalls::premutate( FunctionDecl * funcDecl ) {
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

			function = funcDecl;
			isCtor = CodeGen::isConstructor( function->get_name() );
			if ( checkWarnings( function ) ) {
				FunctionType * type = function->get_functionType();
				assert( ! type->get_parameters().empty() );
				thisParam = strict_dynamic_cast< ObjectDecl * >( type->get_parameters().front() );
				Type * thisType = getPointerBase( thisParam->get_type() );
				StructInstType * structType = dynamic_cast< StructInstType * >( thisType );
				if ( structType ) {
					structDecl = structType->get_baseStruct();
					for ( Declaration * member : structDecl->get_members() ) {
						if ( ObjectDecl * field = dynamic_cast< ObjectDecl * >( member ) ) {
							// record all of the struct type's members that need to be constructed or
							// destructed by the end of the function
							unhandled.insert( field );
						}
					}
				}
			}
		}

		DeclarationWithType * GenStructMemberCalls::postmutate( FunctionDecl * funcDecl ) {
			// remove the unhandled objects from usedUninit, because a call is inserted
			// to handle them - only objects that are later constructed are used uninitialized.
			std::map< DeclarationWithType *, CodeLocation > diff;
			// need the comparator since usedUninit and unhandled have different types
			struct comp_t {
				typedef decltype(usedUninit)::value_type usedUninit_t;
				typedef decltype(unhandled)::value_type unhandled_t;
				bool operator()(usedUninit_t x, unhandled_t y) { return x.first < y; }
				bool operator()(unhandled_t x, usedUninit_t y) { return x < y.first; }
			} comp;
			std::set_difference( usedUninit.begin(), usedUninit.end(), unhandled.begin(), unhandled.end(), std::inserter( diff, diff.begin() ), comp );
			for ( auto p : diff ) {
				DeclarationWithType * member = p.first;
				CodeLocation loc = p.second;
				// xxx - make error message better by also tracking the location that the object is constructed at?
				emit( loc, "in ", CodeGen::genPrettyType( function->get_functionType(), function->get_name() ), ", field ", member->get_name(), " used before being constructed" );
			}

			if ( ! unhandled.empty() ) {
				// need to explicitly re-add function parameters to the indexer in order to resolve copy constructors
				auto guard = makeFuncGuard( [this]() { indexer.enterScope(); }, [this]() { indexer.leaveScope(); } );
				indexer.addFunctionType( function->type );

				// need to iterate through members in reverse in order for
				// ctor/dtor statements to come out in the right order
				for ( Declaration * member : reverseIterate( structDecl->get_members() ) ) {
					DeclarationWithType * field = dynamic_cast< DeclarationWithType * >( member );
					// skip non-DWT members
					if ( ! field ) continue;
					// skip non-constructable members
					if ( ! tryConstruct( field ) ) continue;
					// skip handled members
					if ( ! unhandled.count( field ) ) continue;

					// insert and resolve default/copy constructor call for each field that's unhandled
					std::list< Statement * > stmt;
					Expression * arg2 = nullptr;
					if ( isCopyConstructor( function ) ) {
						// if copy ctor, need to pass second-param-of-this-function.field
						std::list< DeclarationWithType * > & params = function->get_functionType()->get_parameters();
						assert( params.size() == 2 );
						arg2 = new MemberExpr( field, new VariableExpr( params.back() ) );
					}
					InitExpander_old srcParam( arg2 );
					// cast away reference type and construct field.
					Expression * thisExpr = new CastExpr( new VariableExpr( thisParam ), thisParam->get_type()->stripReferences()->clone() );
					Expression * memberDest = new MemberExpr( field, thisExpr );
					SymTab::genImplicitCall( srcParam, memberDest, function->get_name(), back_inserter( stmt ), field, isCtor );

					assert( stmt.size() <= 1 );
					if ( stmt.size() == 1 ) {
						Statement * callStmt = stmt.front();

						try {
							callStmt->acceptMutator( *visitor );
							if ( isCtor ) {
								function->statements->push_front( callStmt );
							} else { // TODO: don't generate destructor function/object for intrinsic calls
								// destructor statements should be added at the end
								// function->get_statements()->push_back( callStmt );

								// Optimization: do not need to call intrinsic destructors on members
								if ( isIntrinsicSingleArgCallStmt( callStmt ) ) continue;;

								// __Destructor _dtor0 = { (void *)&b.a1, (void (*)(void *)_destroy_A };
								std::list< Statement * > stmtsToAdd;

								static UniqueName memberDtorNamer = { "__memberDtor" };
								assertf( Validate::dtorStruct, "builtin __Destructor not found." );
								assertf( Validate::dtorStructDestroy, "builtin __destroy_Destructor not found." );

								Expression * thisExpr = new CastExpr( new AddressExpr( new VariableExpr( thisParam ) ), new PointerType( Type::Qualifiers(), new VoidType( Type::Qualifiers() ) ) );
								Expression * dtorExpr = new VariableExpr( getDtorFunc( thisParam, callStmt, stmtsToAdd ) );

								// cast destructor pointer to void (*)(void *), to silence GCC incompatible pointer warnings
								FunctionType * dtorFtype = new FunctionType( Type::Qualifiers(), false );
								dtorFtype->parameters.push_back( ObjectDecl::newObject( "", new PointerType( Type::Qualifiers(), new VoidType( Type::Qualifiers() ) ), nullptr ) );
								Type * dtorType = new PointerType( Type::Qualifiers(), dtorFtype );

								ObjectDecl * destructor = ObjectDecl::newObject( memberDtorNamer.newName(), new StructInstType( Type::Qualifiers(), Validate::dtorStruct ), new ListInit( { new SingleInit( thisExpr ), new SingleInit( new CastExpr( dtorExpr, dtorType ) ) } ) );
								function->statements->push_front( new DeclStmt( destructor ) );
								destructor->attributes.push_back( new Attribute( "cleanup", { new VariableExpr( Validate::dtorStructDestroy ) } ) );

								function->statements->kids.splice( function->statements->kids.begin(), stmtsToAdd );
							}
						} catch ( SemanticErrorException & error ) {
							emit( funcDecl->location, "in ", CodeGen::genPrettyType( function->get_functionType(), function->get_name() ), ", field ", field->get_name(), " not explicitly ", isCtor ? "constructed" : "destructed",  " and no ", isCtor ? "default constructor" : "destructor", " found" );
						}
					}
				}
			}
			if (! errors.isEmpty()) {
				throw errors;
			}
			return funcDecl;
		}

		/// true if expr is effectively just the 'this' parameter
		bool isThisExpression( Expression * expr, DeclarationWithType * thisParam ) {
			// TODO: there are more complicated ways to pass 'this' to a constructor, e.g. &*, *&, etc.
			if ( VariableExpr * varExpr = dynamic_cast< VariableExpr * >( expr ) ) {
				return varExpr->get_var() == thisParam;
			} else if ( CastExpr * castExpr = dynamic_cast< CastExpr * > ( expr ) ) {
				return isThisExpression( castExpr->get_arg(), thisParam );
			}
			return false;
		}

		/// returns a MemberExpr if expr is effectively just member access on the 'this' parameter, else nullptr
		MemberExpr * isThisMemberExpr( Expression * expr, DeclarationWithType * thisParam ) {
			if ( MemberExpr * memberExpr = dynamic_cast< MemberExpr * >( expr ) ) {
				if ( isThisExpression( memberExpr->get_aggregate(), thisParam ) ) {
					return memberExpr;
				}
			} else if ( CastExpr * castExpr = dynamic_cast< CastExpr * >( expr ) ) {
				return isThisMemberExpr( castExpr->get_arg(), thisParam );
			}
			return nullptr;
		}

		void GenStructMemberCalls::premutate( ApplicationExpr * appExpr ) {
			if ( ! checkWarnings( function ) ) {
				visit_children = false;
				return;
			}

			std::string fname = getFunctionName( appExpr );
			if ( fname == function->name ) {
				// call to same kind of function
				Expression * firstParam = appExpr->args.front();

				if ( isThisExpression( firstParam, thisParam ) ) {
					// if calling another constructor on thisParam, assume that function handles
					// all members - if it doesn't a warning will appear in that function.
					unhandled.clear();
				} else if ( MemberExpr * memberExpr = isThisMemberExpr( firstParam, thisParam ) ) {
					// if first parameter is a member expression on the this parameter,
					// then remove the member from unhandled set.
					if ( isThisExpression( memberExpr->aggregate, thisParam ) ) {
						unhandled.erase( memberExpr->member );
					}
				}
			}
		}

		void GenStructMemberCalls::premutate( MemberExpr * memberExpr ) {
			if ( ! checkWarnings( function ) || ! isCtor ) {
				visit_children = false;
				return;
			}

			if ( isThisExpression( memberExpr->get_aggregate(), thisParam ) ) {
				if ( unhandled.count( memberExpr->get_member() ) ) {
					// emit a warning because a member was used before it was constructed
					usedUninit.insert( { memberExpr->get_member(), memberExpr->location } );
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

		Expression * GenStructMemberCalls::postmutate( UntypedExpr * untypedExpr ) {
			Expression * newExpr = untypedExpr;
			ResolvExpr::findVoidExpression( newExpr, indexer );
			return newExpr;
		}

		Expression * FixCtorExprs::postmutate( ConstructorExpr * ctorExpr ) {
			static UniqueName tempNamer( "_tmp_ctor_expr" );
			// xxx - is the size check necessary?
			assert( ctorExpr->result && ctorExpr->get_result()->size() == 1 );

			// xxx - this can be TupleAssignExpr now. Need to properly handle this case.
			ApplicationExpr * callExpr = strict_dynamic_cast< ApplicationExpr * > ( ctorExpr->get_callExpr() );
			TypeSubstitution * env = ctorExpr->get_env();
			ctorExpr->set_callExpr( nullptr );
			ctorExpr->set_env( nullptr );

			// xxx - ideally we would reuse the temporary generated from the copy constructor passes from within firstArg if it exists and not generate a temporary if it's unnecessary.
			ObjectDecl * tmp = ObjectDecl::newObject( tempNamer.newName(), callExpr->args.front()->result->clone(), nullptr );
			declsToAddBefore.push_back( tmp );
			delete ctorExpr;

			// build assignment and replace constructor's first argument with new temporary
			Expression *& firstArg = callExpr->get_args().front();
			Expression * assign = new UntypedExpr( new NameExpr( "?=?" ), { new AddressExpr( new VariableExpr( tmp ) ), new AddressExpr( firstArg ) } );
			firstArg = new VariableExpr( tmp );

			// resolve assignment and dispose of new env
			ResolvExpr::findVoidExpression( assign, indexer );
			delete assign->env;
			assign->env = nullptr;

			// for constructor expr:
			//   T x;
			//   x{};
			// results in:
			//   T x;
			//   T & tmp;
			//   &tmp = &x, ?{}(tmp), tmp
			CommaExpr * commaExpr = new CommaExpr( assign, new CommaExpr( callExpr, new VariableExpr( tmp ) ) );
			commaExpr->set_env( env );
			return commaExpr;
		}
	} // namespace
} // namespace InitTweak

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
