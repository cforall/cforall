//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Box.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:40:34 2019
// Update Count     : 347
//

#include <algorithm>                     // for mismatch
#include <cassert>                       // for assert, strict_dynamic_cast
#include <iostream>                      // for operator<<, stringstream
#include <list>                          // for list, list<>::iterator, _Lis...
#include <map>                           // for _Rb_tree_const_iterator, map
#include <memory>                        // for auto_ptr
#include <set>                           // for set
#include <string>                        // for string, allocator, basic_string
#include <utility>                       // for pair

#include "Box.h"

#include "CodeGen/OperatorTable.h"
#include "Common/PassVisitor.h"          // for PassVisitor
#include "Common/ScopedMap.h"            // for ScopedMap, ScopedMap<>::iter...
#include "Common/SemanticError.h"        // for SemanticError
#include "Common/UniqueName.h"           // for UniqueName
#include "Common/utility.h"              // for toString
#include "FindFunction.h"                // for findFunction, findAndReplace...
#include "GenPoly/ErasableScopedMap.h"   // for ErasableScopedMap<>::const_i...
#include "GenPoly/GenPoly.h"             // for TyVarMap, isPolyType, mangle...
#include "InitTweak/InitTweak.h"         // for getFunctionName, isAssignment
#include "Lvalue.h"                      // for generalizedLvalue
#include "ResolvExpr/TypeEnvironment.h"  // for EqvClass
#include "ResolvExpr/typeops.h"          // for typesCompatible
#include "ScopedSet.h"                   // for ScopedSet, ScopedSet<>::iter...
#include "ScrubTyVars.h"                 // for ScrubTyVars
#include "SymTab/Indexer.h"              // for Indexer
#include "SymTab/Mangler.h"              // for Mangler
#include "SynTree/LinkageSpec.h"         // for C, Spec, Cforall, Intrinsic
#include "SynTree/Attribute.h"           // for Attribute
#include "SynTree/Constant.h"            // for Constant
#include "SynTree/Declaration.h"         // for DeclarationWithType, ObjectDecl
#include "SynTree/Expression.h"          // for ApplicationExpr, UntypedExpr
#include "SynTree/Initializer.h"         // for SingleInit, Initializer, Lis...
#include "SynTree/Label.h"               // for Label
#include "SynTree/Mutator.h"             // for maybeMutate, Mutator, mutateAll
#include "SynTree/Statement.h"           // for ExprStmt, DeclStmt, ReturnStmt
#include "SynTree/SynTree.h"             // for UniqueId
#include "SynTree/Type.h"                // for Type, FunctionType, PointerType
#include "SynTree/TypeSubstitution.h"    // for TypeSubstitution, operator<<

namespace GenPoly {
	namespace {
		FunctionType *makeAdapterType( FunctionType *adaptee, const TyVarMap &tyVars );

		class BoxPass {
		protected:
			BoxPass() : scopeTyVars( TypeDecl::Data{} ) {}
			TyVarMap scopeTyVars;
		};

		/// Adds layout-generation functions to polymorphic types.
		class LayoutFunctionBuilder final : public WithDeclsToAdd, public WithVisitorRef<LayoutFunctionBuilder>, public WithShortCircuiting {
			// Current level of nested functions:
			unsigned int functionNesting = 0;
		public:
			void previsit( FunctionDecl *functionDecl );
			void previsit( StructDecl *structDecl );
			void previsit( UnionDecl *unionDecl );
		};

		/// Replaces polymorphic return types with out-parameters,
		/// replaces calls to polymorphic functions with adapter calls,
		/// and adds appropriate type variables to the function call.
		class Pass1 final : public BoxPass, public WithConstTypeSubstitution, public WithStmtsToAdd, public WithGuards, public WithVisitorRef<Pass1>, public WithShortCircuiting {
		  public:
			Pass1();

			void premutate( FunctionDecl * functionDecl );
			void premutate( TypeDecl * typeDecl );
			void premutate( CommaExpr * commaExpr );
			Expression * postmutate( ApplicationExpr * appExpr );
			Expression * postmutate( UntypedExpr *expr );
			void premutate( AddressExpr * addrExpr );
			Expression * postmutate( AddressExpr * addrExpr );
			void premutate( ReturnStmt * returnStmt );
			void premutate( PointerType * pointerType );
			void premutate( FunctionType * functionType );

			void beginScope();
			void endScope();
		  private:
			/// Pass the extra type parameters from polymorphic generic arguments or return types into a function application
			void passArgTypeVars( ApplicationExpr *appExpr, Type *parmType, Type *argBaseType, std::list< Expression *>::iterator &arg, const TyVarMap &exprTyVars, std::set< std::string > &seenTypes );
			/// passes extra type parameters into a polymorphic function application
			void passTypeVars( ApplicationExpr *appExpr, Type *polyRetType, std::list< Expression *>::iterator &arg, const TyVarMap &exprTyVars );
			/// wraps a function application with a new temporary for the out-parameter return value
			Expression *addRetParam( ApplicationExpr *appExpr, Type *retType, std::list< Expression *>::iterator &arg );
			/// Replaces all the type parameters of a generic type with their concrete equivalents under the current environment
			void replaceParametersWithConcrete( ApplicationExpr *appExpr, std::list< Expression* >& params );
			/// Replaces a polymorphic type with its concrete equivalant under the current environment (returns itself if concrete).
			/// If `doClone` is set to false, will not clone interior types
			Type *replaceWithConcrete( ApplicationExpr *appExpr, Type *type, bool doClone = true );
			/// wraps a function application returning a polymorphic type with a new temporary for the out-parameter return value
			Expression *addDynRetParam( ApplicationExpr *appExpr, Type *polyType, std::list< Expression *>::iterator &arg );
			Expression *applyAdapter( ApplicationExpr *appExpr, FunctionType *function, std::list< Expression *>::iterator &arg, const TyVarMap &exprTyVars );
			void boxParam( Type *formal, Expression *&arg, const TyVarMap &exprTyVars );
			void boxParams( ApplicationExpr *appExpr, FunctionType *function, std::list< Expression *>::iterator &arg, const TyVarMap &exprTyVars );
			void addInferredParams( ApplicationExpr *appExpr, FunctionType *functionType, std::list< Expression *>::iterator &arg, const TyVarMap &tyVars );
			/// Stores assignment operators from assertion list in local map of assignment operations
			void passAdapters( ApplicationExpr *appExpr, FunctionType *functionType, const TyVarMap &exprTyVars );
			FunctionDecl *makeAdapter( FunctionType *adaptee, FunctionType *realType, const std::string &mangleName, const TyVarMap &tyVars );
			/// Replaces intrinsic operator functions with their arithmetic desugaring
			Expression *handleIntrinsics( ApplicationExpr *appExpr );
			/// Inserts a new temporary variable into the current scope with an auto-generated name
			ObjectDecl *makeTemporary( Type *type );

			ScopedMap< std::string, DeclarationWithType* > adapters;     ///< Set of adapter functions in the current scope

			std::map< ApplicationExpr *, Expression * > retVals;

			DeclarationWithType *retval;
			UniqueName tempNamer;
		};

		/// * Moves polymorphic returns in function types to pointer-type parameters
		/// * adds type size and assertion parameters to parameter lists
		struct Pass2 final : public BoxPass, public WithGuards {
			void handleAggDecl();

			DeclarationWithType * postmutate( FunctionDecl *functionDecl );
			void premutate( StructDecl *structDecl );
			void premutate( UnionDecl *unionDecl );
			void premutate( TraitDecl *unionDecl );
			void premutate( TypeDecl *typeDecl );
			void premutate( PointerType *pointerType );
			void premutate( FunctionType *funcType );

		  private:
			void addAdapters( FunctionType *functionType );

			std::map< UniqueId, std::string > adapterName;
		};

		/// * Replaces member and size/align/offsetof expressions on polymorphic generic types with calculated expressions.
		/// * Replaces member expressions for polymorphic types with calculated add-field-offset-and-dereference
		/// * Calculates polymorphic offsetof expressions from offset array
		/// * Inserts dynamic calculation of polymorphic type layouts where needed
		class PolyGenericCalculator final : public BoxPass, public WithGuards, public WithVisitorRef<PolyGenericCalculator>, public WithStmtsToAdd, public WithDeclsToAdd, public WithConstTypeSubstitution {
		public:
			PolyGenericCalculator();

			void premutate( ObjectDecl *objectDecl );
			void premutate( FunctionDecl *functionDecl );
			void premutate( TypedefDecl *objectDecl );
			void premutate( TypeDecl *objectDecl );
			Declaration * postmutate( TypeDecl *TraitDecl );
			void premutate( PointerType *pointerType );
			void premutate( FunctionType *funcType );
			void premutate( DeclStmt *declStmt );
			Expression *postmutate( MemberExpr *memberExpr );
			void premutate( AddressExpr *addrExpr );
			Expression *postmutate( AddressExpr *addrExpr );
			Expression *postmutate( SizeofExpr *sizeofExpr );
			Expression *postmutate( AlignofExpr *alignofExpr );
			Expression *postmutate( OffsetofExpr *offsetofExpr );
			Expression *postmutate( OffsetPackExpr *offsetPackExpr );
			void premutate( StructDecl * );
			void premutate( UnionDecl * );

			void beginScope();
			void endScope();

		private:
			/// Makes a new variable in the current scope with the given name, type & optional initializer
			ObjectDecl *makeVar( const std::string &name, Type *type, Initializer *init = 0 );
			/// returns true if the type has a dynamic layout; such a layout will be stored in appropriately-named local variables when the function returns
			bool findGeneric( Type *ty );
			/// adds type parameters to the layout call; will generate the appropriate parameters if needed
			void addOtypeParamsToLayoutCall( UntypedExpr *layoutCall, const std::list< Type* > &otypeParams );
			/// change the type of generic aggregate members to char[]
			void mutateMembers( AggregateDecl * aggrDecl );
			/// returns the calculated sizeof expression for ty, or nullptr for use C sizeof()
			Expression* genSizeof( Type* ty );

			/// Enters a new scope for type-variables, adding the type variables from ty
			void beginTypeScope( Type *ty );
			/// Enters a new scope for knowLayouts and knownOffsets and queues exit calls
			void beginGenericScope();

			ScopedSet< std::string > knownLayouts;          ///< Set of generic type layouts known in the current scope, indexed by sizeofName
			ScopedSet< std::string > knownOffsets;          ///< Set of non-generic types for which the offset array exists in the current scope, indexed by offsetofName
			UniqueName bufNamer;                           ///< Namer for VLA buffers
			Expression * addrMember = nullptr;             ///< AddressExpr argument is MemberExpr?
			bool expect_func_type = false;                 ///< used to avoid recursing too deep in type decls
		};

		/// Replaces initialization of polymorphic values with alloca,
		/// declaration of dtype/ftype with appropriate void expression,
		/// sizeof expressions of polymorphic types with the proper variable,
		/// and strips fields from generic struct declarations.
		struct Pass3 final : public BoxPass, public WithGuards {
			template< typename DeclClass >
			void handleDecl( DeclClass * decl, Type * type );

			void premutate( ObjectDecl * objectDecl );
			void premutate( FunctionDecl * functionDecl );
			void premutate( TypedefDecl * typedefDecl );
			void premutate( StructDecl * structDecl );
			void premutate( UnionDecl * unionDecl );
			void premutate( TypeDecl * typeDecl );
			void premutate( PointerType * pointerType );
			void premutate( FunctionType * funcType );
		};
	} // anonymous namespace

	void box( std::list< Declaration *>& translationUnit ) {
		PassVisitor<LayoutFunctionBuilder> layoutBuilder;
		PassVisitor<Pass1> pass1;
		PassVisitor<Pass2> pass2;
		PassVisitor<PolyGenericCalculator> polyCalculator;
		PassVisitor<Pass3> pass3;

		acceptAll( translationUnit, layoutBuilder );
		mutateAll( translationUnit, pass1 );
		mutateAll( translationUnit, pass2 );
		mutateAll( translationUnit, polyCalculator );
		mutateAll( translationUnit, pass3 );
	}

	////////////////////////////////// LayoutFunctionBuilder ////////////////////////////////////////////

	void LayoutFunctionBuilder::previsit( FunctionDecl *functionDecl ) {
		visit_children = false;
		maybeAccept( functionDecl->get_functionType(), *visitor );
		++functionNesting;
		maybeAccept( functionDecl->get_statements(), *visitor );
		--functionNesting;
	}

	/// Get a list of type declarations that will affect a layout function
	std::list< TypeDecl* > takeOtypeOnly( std::list< TypeDecl* > &decls ) {
		std::list< TypeDecl * > otypeDecls;

		for ( TypeDecl * const decl : decls ) {
			if ( decl->isComplete() ) {
				otypeDecls.push_back( decl );
			}
		}

		return otypeDecls;
	}

	/// Adds parameters for otype layout to a function type
	void addOtypeParams( FunctionType *layoutFnType, std::list< TypeDecl* > &otypeParams ) {
		BasicType sizeAlignType( Type::Qualifiers(), BasicType::LongUnsignedInt );

		for ( TypeDecl * const param : otypeParams ) {
			TypeInstType paramType( Type::Qualifiers(), param->get_name(), param );
			std::string paramName = mangleType( &paramType );
			layoutFnType->get_parameters().push_back( new ObjectDecl( sizeofName( paramName ), Type::StorageClasses(), LinkageSpec::Cforall, 0, sizeAlignType.clone(), 0 ) );
			layoutFnType->get_parameters().push_back( new ObjectDecl( alignofName( paramName ), Type::StorageClasses(), LinkageSpec::Cforall, 0, sizeAlignType.clone(), 0 ) );
		}
	}

	/// Builds a layout function declaration
	FunctionDecl *buildLayoutFunctionDecl( AggregateDecl *typeDecl, unsigned int functionNesting, FunctionType *layoutFnType ) {
		// Routines at global scope marked "static" to prevent multiple definitions is separate translation units
		// because each unit generates copies of the default routines for each aggregate.
		FunctionDecl *layoutDecl = new FunctionDecl( layoutofName( typeDecl ),
													 functionNesting > 0 ? Type::StorageClasses() : Type::StorageClasses( Type::Static ),
													 LinkageSpec::AutoGen, layoutFnType, new CompoundStmt(),
													 std::list< Attribute * >(), Type::FuncSpecifiers( Type::Inline ) );
		layoutDecl->fixUniqueId();
		return layoutDecl;
	}

	/// Makes a binary operation
	Expression *makeOp( const std::string &name, Expression *lhs, Expression *rhs ) {
		UntypedExpr *expr = new UntypedExpr( new NameExpr( name ) );
		expr->args.push_back( lhs );
		expr->args.push_back( rhs );
		return expr;
	}

	/// Returns the dereference of a local pointer variable
	Expression *derefVar( ObjectDecl *var ) {
		return UntypedExpr::createDeref( new VariableExpr( var ) );
	}

	/// makes an if-statement with a single-expression if-block and no then block
	Statement *makeCond( Expression *cond, Expression *ifPart ) {
		return new IfStmt( cond, new ExprStmt( ifPart ), 0 );
	}

	/// makes a statement that assigns rhs to lhs if lhs < rhs
	Statement *makeAssignMax( Expression *lhs, Expression *rhs ) {
		return makeCond( makeOp( "?<?", lhs, rhs ), makeOp( "?=?", lhs->clone(), rhs->clone() ) );
	}

	/// makes a statement that aligns lhs to rhs (rhs should be an integer power of two)
	Statement *makeAlignTo( Expression *lhs, Expression *rhs ) {
		// check that the lhs is zeroed out to the level of rhs
		Expression *ifCond = makeOp( "?&?", lhs, makeOp( "?-?", rhs, new ConstantExpr( Constant::from_ulong( 1 ) ) ) );
		// if not aligned, increment to alignment
		Expression *ifExpr = makeOp( "?+=?", lhs->clone(), makeOp( "?-?", rhs->clone(), ifCond->clone() ) );
		return makeCond( ifCond, ifExpr );
	}

	/// adds an expression to a compound statement
	void addExpr( CompoundStmt *stmts, Expression *expr ) {
		stmts->get_kids().push_back( new ExprStmt( expr ) );
	}

	/// adds a statement to a compound statement
	void addStmt( CompoundStmt *stmts, Statement *stmt ) {
		stmts->get_kids().push_back( stmt );
	}

	void LayoutFunctionBuilder::previsit( StructDecl *structDecl ) {
		// do not generate layout function for "empty" tag structs
		visit_children = false;
		if ( structDecl->get_members().empty() ) return;

		// get parameters that can change layout, exiting early if none
		std::list< TypeDecl* > otypeParams = takeOtypeOnly( structDecl->get_parameters() );
		if ( otypeParams.empty() ) return;

		// build layout function signature
		FunctionType *layoutFnType = new FunctionType( Type::Qualifiers(), false );
		BasicType *sizeAlignType = new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt );
		PointerType *sizeAlignOutType = new PointerType( Type::Qualifiers(), sizeAlignType );

		ObjectDecl *sizeParam = new ObjectDecl( sizeofName( structDecl->get_name() ), Type::StorageClasses(), LinkageSpec::Cforall, 0, sizeAlignOutType, 0 );
		layoutFnType->get_parameters().push_back( sizeParam );
		ObjectDecl *alignParam = new ObjectDecl( alignofName( structDecl->get_name() ), Type::StorageClasses(), LinkageSpec::Cforall, 0, sizeAlignOutType->clone(), 0 );
		layoutFnType->get_parameters().push_back( alignParam );
		ObjectDecl *offsetParam = new ObjectDecl( offsetofName( structDecl->get_name() ), Type::StorageClasses(), LinkageSpec::Cforall, 0, sizeAlignOutType->clone(), 0 );
		layoutFnType->get_parameters().push_back( offsetParam );
		addOtypeParams( layoutFnType, otypeParams );

		// build function decl
		FunctionDecl *layoutDecl = buildLayoutFunctionDecl( structDecl, functionNesting, layoutFnType );

		// calculate struct layout in function body

		// initialize size and alignment to 0 and 1 (will have at least one member to re-edit size)
		addExpr( layoutDecl->get_statements(), makeOp( "?=?", derefVar( sizeParam ), new ConstantExpr( Constant::from_ulong( 0 ) ) ) );
		addExpr( layoutDecl->get_statements(), makeOp( "?=?", derefVar( alignParam ), new ConstantExpr( Constant::from_ulong( 1 ) ) ) );
		unsigned long n_members = 0;
		bool firstMember = true;
		for ( Declaration* member : structDecl->get_members() ) {
			DeclarationWithType *dwt = dynamic_cast< DeclarationWithType * >( member );
			assert( dwt );
			Type *memberType = dwt->get_type();

			if ( firstMember ) {
				firstMember = false;
			} else {
				// make sure all members after the first (automatically aligned at 0) are properly padded for alignment
				addStmt( layoutDecl->get_statements(), makeAlignTo( derefVar( sizeParam ), new AlignofExpr( memberType->clone() ) ) );
			}

			// place current size in the current offset index
			addExpr( layoutDecl->get_statements(), makeOp( "?=?", makeOp( "?[?]", new VariableExpr( offsetParam ), new ConstantExpr( Constant::from_ulong( n_members ) ) ),
			                                                      derefVar( sizeParam ) ) );
			++n_members;

			// add member size to current size
			addExpr( layoutDecl->get_statements(), makeOp( "?+=?", derefVar( sizeParam ), new SizeofExpr( memberType->clone() ) ) );

			// take max of member alignment and global alignment
			addStmt( layoutDecl->get_statements(), makeAssignMax( derefVar( alignParam ), new AlignofExpr( memberType->clone() ) ) );
		}
		// make sure the type is end-padded to a multiple of its alignment
		addStmt( layoutDecl->get_statements(), makeAlignTo( derefVar( sizeParam ), derefVar( alignParam ) ) );

		declsToAddAfter.push_back( layoutDecl );
	}

	void LayoutFunctionBuilder::previsit( UnionDecl *unionDecl ) {
		// do not generate layout function for "empty" tag unions
		visit_children = false;
		if ( unionDecl->get_members().empty() ) return;

		// get parameters that can change layout, exiting early if none
		std::list< TypeDecl* > otypeParams = takeOtypeOnly( unionDecl->get_parameters() );
		if ( otypeParams.empty() ) return;

		// build layout function signature
		FunctionType *layoutFnType = new FunctionType( Type::Qualifiers(), false );
		BasicType *sizeAlignType = new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt );
		PointerType *sizeAlignOutType = new PointerType( Type::Qualifiers(), sizeAlignType );

		ObjectDecl *sizeParam = new ObjectDecl( sizeofName( unionDecl->get_name() ), Type::StorageClasses(), LinkageSpec::Cforall, 0, sizeAlignOutType, 0 );
		layoutFnType->get_parameters().push_back( sizeParam );
		ObjectDecl *alignParam = new ObjectDecl( alignofName( unionDecl->get_name() ), Type::StorageClasses(), LinkageSpec::Cforall, 0, sizeAlignOutType->clone(), 0 );
		layoutFnType->get_parameters().push_back( alignParam );
		addOtypeParams( layoutFnType, otypeParams );

		// build function decl
		FunctionDecl *layoutDecl = buildLayoutFunctionDecl( unionDecl, functionNesting, layoutFnType );

		// calculate union layout in function body
		addExpr( layoutDecl->get_statements(), makeOp( "?=?", derefVar( sizeParam ), new ConstantExpr( Constant::from_ulong( 1 ) ) ) );
		addExpr( layoutDecl->get_statements(), makeOp( "?=?", derefVar( alignParam ), new ConstantExpr( Constant::from_ulong( 1 ) ) ) );
		for ( Declaration * const member : unionDecl->members ) {
			DeclarationWithType *dwt = dynamic_cast< DeclarationWithType * >( member );
			assert( dwt );
			Type *memberType = dwt->get_type();

			// take max member size and global size
			addStmt( layoutDecl->get_statements(), makeAssignMax( derefVar( sizeParam ), new SizeofExpr( memberType->clone() ) ) );

			// take max of member alignment and global alignment
			addStmt( layoutDecl->get_statements(), makeAssignMax( derefVar( alignParam ), new AlignofExpr( memberType->clone() ) ) );
		}
		// make sure the type is end-padded to a multiple of its alignment
		addStmt( layoutDecl->get_statements(), makeAlignTo( derefVar( sizeParam ), derefVar( alignParam ) ) );

		declsToAddAfter.push_back( layoutDecl );
	}

	////////////////////////////////////////// Pass1 ////////////////////////////////////////////////////

	namespace {
		std::string makePolyMonoSuffix( FunctionType * function, const TyVarMap &tyVars ) {
			std::stringstream name;

			// NOTE: this function previously used isPolyObj, which failed to produce
			// the correct thing in some situations. It's not clear to me why this wasn't working.

			// if the return type or a parameter type involved polymorphic types, then the adapter will need
			// to take those polymorphic types as pointers. Therefore, there can be two different functions
			// with the same mangled name, so we need to further mangle the names.
			for ( DeclarationWithType * const ret : function->get_returnVals() ) {
				if ( isPolyType( ret->get_type(), tyVars ) ) {
					name << "P";
				} else {
					name << "M";
				}
			}
			name << "_";
			for ( DeclarationWithType * const arg : function->get_parameters() ) {
				if ( isPolyType( arg->get_type(), tyVars ) ) {
					name << "P";
				} else {
					name << "M";
				}
			} // for
			return name.str();
		}

		std::string mangleAdapterName( FunctionType * function, const TyVarMap &tyVars ) {
			return SymTab::Mangler::mangle( function ) + makePolyMonoSuffix( function, tyVars );
		}

		std::string makeAdapterName( const std::string &mangleName ) {
			return "_adapter" + mangleName;
		}

		Pass1::Pass1() : tempNamer( "_temp" ) {}

		void Pass1::premutate( FunctionDecl *functionDecl ) {
			if ( functionDecl->get_statements() ) {		// empty routine body ?
				// std::cerr << "mutating function: " << functionDecl->get_mangleName() << std::endl;
				GuardScope( scopeTyVars );
				GuardValue( retval );

				// process polymorphic return value
				retval = nullptr;
				FunctionType *functionType = functionDecl->type;
				if ( isDynRet( functionType ) && functionDecl->linkage != LinkageSpec::C ) {
					retval = functionType->returnVals.front();

					// give names to unnamed return values
					if ( retval->name == "" ) {
						retval->name = "_retparm";
						retval->linkage = LinkageSpec::C;
					} // if
				} // if

				makeTyVarMap( functionType, scopeTyVars );

				std::list< DeclarationWithType *> &paramList = functionType->parameters;
				std::list< FunctionType *> functions;
				for ( TypeDecl * const tyVar : functionType->forall ) {
					for ( DeclarationWithType * const assert : tyVar->assertions ) {
						findFunction( assert->get_type(), functions, scopeTyVars, needsAdapter );
					} // for
				} // for
				for ( DeclarationWithType * const arg : paramList ) {
					findFunction( arg->get_type(), functions, scopeTyVars, needsAdapter );
				} // for

				for ( FunctionType * const funType : functions ) {
					std::string mangleName = mangleAdapterName( funType, scopeTyVars );
					if ( adapters.find( mangleName ) == adapters.end() ) {
						std::string adapterName = makeAdapterName( mangleName );
						adapters.insert( std::pair< std::string, DeclarationWithType *>( mangleName, new ObjectDecl( adapterName, Type::StorageClasses(), LinkageSpec::C, nullptr, new PointerType( Type::Qualifiers(), makeAdapterType( funType, scopeTyVars ) ), nullptr ) ) );
					} // if
				} // for
				// std::cerr << "end function: " << functionDecl->get_mangleName() << std::endl;
			} // if
		}

		void Pass1::premutate( TypeDecl *typeDecl ) {
			addToTyVarMap( typeDecl, scopeTyVars );
		}

		void Pass1::premutate( CommaExpr *commaExpr ) {
			// Attempting to find application expressions that were mutated by the copy constructor passes
			// to use an explicit return variable, so that the variable can be reused as a parameter to the
			// call rather than creating a new temp variable. Previously this step was an optimization, but
			// with the introduction of tuples and UniqueExprs, it is necessary to ensure that they use the same variable.
			// Essentially, looking for pattern: (x=f(...), x)
			// To compound the issue, the right side can be *x, etc. because of lvalue-returning functions
			if ( UntypedExpr * assign = dynamic_cast< UntypedExpr * >( commaExpr->get_arg1() ) ) {
				if ( CodeGen::isAssignment( InitTweak::getFunctionName( assign ) ) ) {
					assert( assign->get_args().size() == 2 );
					if ( ApplicationExpr * appExpr = dynamic_cast< ApplicationExpr * > ( assign->get_args().back() ) ) {
						// first argument is assignable, so it must be an lvalue, so it should be legal to take its address.
						retVals[appExpr] = assign->get_args().front();
					}
				}
			}
		}

		void Pass1::passArgTypeVars( ApplicationExpr *appExpr, Type *parmType, Type *argBaseType, std::list< Expression *>::iterator &arg, const TyVarMap &exprTyVars, std::set< std::string > &seenTypes ) {
			Type *polyType = isPolyType( parmType, exprTyVars );
			if ( polyType && ! dynamic_cast< TypeInstType* >( polyType ) ) {
				std::string typeName = mangleType( polyType );
				if ( seenTypes.count( typeName ) ) return;

				arg = appExpr->get_args().insert( arg, new SizeofExpr( argBaseType->clone() ) );
				arg++;
				arg = appExpr->get_args().insert( arg, new AlignofExpr( argBaseType->clone() ) );
				arg++;
				if ( dynamic_cast< StructInstType* >( polyType ) ) {
					if ( StructInstType *argBaseStructType = dynamic_cast< StructInstType* >( argBaseType ) ) {
						// zero-length arrays are forbidden by C, so don't pass offset for empty struct
						if ( ! argBaseStructType->get_baseStruct()->get_members().empty() ) {
							arg = appExpr->get_args().insert( arg, new OffsetPackExpr( argBaseStructType->clone() ) );
							arg++;
						}
					} else {
						SemanticError( argBaseType, "Cannot pass non-struct type for generic struct: " );
					}
				}

				seenTypes.insert( typeName );
			}
		}

		void Pass1::passTypeVars( ApplicationExpr *appExpr, Type *polyRetType, std::list< Expression *>::iterator &arg, const TyVarMap &exprTyVars ) {
			// pass size/align for type variables
			for ( std::pair<std::string, TypeDecl::Data> const & tyParam : exprTyVars ) {
				ResolvExpr::EqvClass eqvClass;
				assert( env );
				if ( tyParam.second.isComplete ) {
					Type *concrete = env->lookup( tyParam.first );
					if ( concrete ) {
						arg = appExpr->get_args().insert( arg, new SizeofExpr( concrete->clone() ) );
						arg++;
						arg = appExpr->get_args().insert( arg, new AlignofExpr( concrete->clone() ) );
						arg++;
					} else {
						// xxx - should this be an assertion?
						SemanticError( appExpr, toString( *env, "\nunbound type variable: ", tyParam.first, " in application " ) );
					} // if
				} // if
			} // for

			// add size/align for generic types to parameter list
			if ( ! appExpr->get_function()->result ) return;
			FunctionType *funcType = getFunctionType( appExpr->get_function()->get_result() );
			assert( funcType );

			std::list< DeclarationWithType* >::const_iterator fnParm = funcType->get_parameters().begin();
			std::list< Expression* >::const_iterator fnArg = arg;
			std::set< std::string > seenTypes; ///< names for generic types we've seen

			// a polymorphic return type may need to be added to the argument list
			if ( polyRetType ) {
				Type *concRetType = replaceWithConcrete( appExpr, polyRetType );
				passArgTypeVars( appExpr, polyRetType, concRetType, arg, exprTyVars, seenTypes );
				++fnArg; // skip the return parameter in the argument list
			}

			// add type information args for presently unseen types in parameter list
			for ( ; fnParm != funcType->get_parameters().end() && fnArg != appExpr->get_args().end(); ++fnParm, ++fnArg ) {
				if ( ! (*fnArg)->get_result() ) continue;
				Type * argType = (*fnArg)->get_result();
				passArgTypeVars( appExpr, (*fnParm)->get_type(), argType, arg, exprTyVars, seenTypes );
			}
		}

		ObjectDecl *Pass1::makeTemporary( Type *type ) {
			ObjectDecl *newObj = new ObjectDecl( tempNamer.newName(), Type::StorageClasses(), LinkageSpec::C, 0, type, 0 );
			stmtsToAddBefore.push_back( new DeclStmt( newObj ) );
			return newObj;
		}

		Expression *Pass1::addRetParam( ApplicationExpr *appExpr, Type *retType, std::list< Expression *>::iterator &arg ) {
			// Create temporary to hold return value of polymorphic function and produce that temporary as a result
			// using a comma expression.
			assert( retType );

			Expression * paramExpr = nullptr;
			// try to use existing return value parameter if it exists, otherwise create a new temporary
			if ( retVals.count( appExpr ) ) {
				paramExpr = retVals[appExpr]->clone();
			} else {
				ObjectDecl *newObj = makeTemporary( retType->clone() );
				paramExpr = new VariableExpr( newObj );
			}
			Expression * retExpr = paramExpr->clone();

			// If the type of the temporary is not polymorphic, box temporary by taking its address;
			// otherwise the temporary is already boxed and can be used directly.
			if ( ! isPolyType( paramExpr->get_result(), scopeTyVars, env ) ) {
				paramExpr = new AddressExpr( paramExpr );
			} // if
			arg = appExpr->args.insert( arg, paramExpr ); // add argument to function call
			arg++;
			// Build a comma expression to call the function and emulate a normal return.
			CommaExpr *commaExpr = new CommaExpr( appExpr, retExpr );
			commaExpr->env = appExpr->env;
			appExpr->env = nullptr;
			return commaExpr;
		}

		void Pass1::replaceParametersWithConcrete( ApplicationExpr *appExpr, std::list< Expression* >& params ) {
			for ( std::list< Expression* >::iterator param = params.begin(); param != params.end(); ++param ) {
				TypeExpr *paramType = dynamic_cast< TypeExpr* >( *param );
				assertf(paramType, "Aggregate parameters should be type expressions");
				paramType->set_type( replaceWithConcrete( appExpr, paramType->get_type(), false ) );
			}
		}

		Type *Pass1::replaceWithConcrete( ApplicationExpr *appExpr, Type *type, bool doClone ) {
			if ( TypeInstType *typeInst = dynamic_cast< TypeInstType * >( type ) ) {
				Type *concrete = env->lookup( typeInst->get_name() );
				if ( concrete == 0 ) {
					return typeInst;
				} // if
				return concrete;
			} else if ( StructInstType *structType = dynamic_cast< StructInstType* >( type ) ) {
				if ( doClone ) {
					structType = structType->clone();
				}
				replaceParametersWithConcrete( appExpr, structType->get_parameters() );
				return structType;
			} else if ( UnionInstType *unionType = dynamic_cast< UnionInstType* >( type ) ) {
				if ( doClone ) {
					unionType = unionType->clone();
				}
				replaceParametersWithConcrete( appExpr, unionType->get_parameters() );
				return unionType;
			}
			return type;
		}

		Expression *Pass1::addDynRetParam( ApplicationExpr *appExpr, Type *dynType, std::list< Expression *>::iterator &arg ) {
			assert( env );
			Type *concrete = replaceWithConcrete( appExpr, dynType );
			// add out-parameter for return value
			return addRetParam( appExpr, concrete, arg );
		}

		Expression *Pass1::applyAdapter( ApplicationExpr *appExpr, FunctionType *function, std::list< Expression *>::iterator &arg, const TyVarMap &tyVars ) {
			Expression *ret = appExpr;
//			if ( ! function->get_returnVals().empty() && isPolyType( function->get_returnVals().front()->get_type(), tyVars ) ) {
			if ( isDynRet( function, tyVars ) ) {
				ret = addRetParam( appExpr, function->returnVals.front()->get_type(), arg );
			} // if
			std::string mangleName = mangleAdapterName( function, tyVars );
			std::string adapterName = makeAdapterName( mangleName );

			// cast adaptee to void (*)(), since it may have any type inside a polymorphic function
			Type * adapteeType = new PointerType( Type::Qualifiers(), new FunctionType( Type::Qualifiers(), true ) );
			appExpr->get_args().push_front( new CastExpr( appExpr->function, adapteeType ) );
			appExpr->set_function( new NameExpr( adapterName ) ); // xxx - result is never set on NameExpr

			return ret;
		}

		void Pass1::boxParam( Type *param, Expression *&arg, const TyVarMap &exprTyVars ) {
			assertf( arg->result, "arg does not have result: %s", toString( arg ).c_str() );
			if ( ! needsBoxing( param, arg->result, exprTyVars, env ) ) return;

			if ( arg->get_lvalue() ) {
				// argument expression may be CFA lvalue, but not C lvalue -- apply generalizedLvalue transformations.
				// if ( VariableExpr * varExpr = dynamic_cast< VariableExpr * >( arg ) ) {
				// 	if ( dynamic_cast<ArrayType *>( varExpr->var->get_type() ) ){
				// 		// temporary hack - don't box arrays, because &arr is not the same as &arr[0]
				// 		return;
				// 	}
				// }
				arg =  generalizedLvalue( new AddressExpr( arg ) );
				if ( ! ResolvExpr::typesCompatible( param, arg->get_result(), SymTab::Indexer() ) ) {
					// silence warnings by casting boxed parameters when the actual type does not match up with the formal type.
					arg = new CastExpr( arg, param->clone() );
				}
			} else {
				// use type computed in unification to declare boxed variables
				Type * newType = param->clone();
				if ( env ) env->apply( newType );
				ObjectDecl *newObj = ObjectDecl::newObject( tempNamer.newName(), newType, nullptr );
				newObj->get_type()->get_qualifiers() = Type::Qualifiers(); // TODO: is this right???
				stmtsToAddBefore.push_back( new DeclStmt( newObj ) );
				UntypedExpr *assign = new UntypedExpr( new NameExpr( "?=?" ) ); // TODO: why doesn't this just use initialization syntax?
				assign->get_args().push_back( new VariableExpr( newObj ) );
				assign->get_args().push_back( arg );
				stmtsToAddBefore.push_back( new ExprStmt( assign ) );
				arg = new AddressExpr( new VariableExpr( newObj ) );
			} // if
		}

		// find instances of polymorphic type parameters
		struct PolyFinder {
			const TyVarMap * tyVars = nullptr;
			bool found = false;

			void previsit( TypeInstType * t ) {
				if ( isPolyType( t, *tyVars ) ) {
					found = true;
				}
			}
		};

		// true if there is an instance of a polymorphic type parameter in t
		bool hasPolymorphism( Type * t, const TyVarMap &tyVars ) {
			PassVisitor<PolyFinder> finder;
			finder.pass.tyVars = &tyVars;
			maybeAccept( t, finder );
			return finder.pass.found;
		}

		/// cast parameters to polymorphic functions so that types are replaced with
		/// void * if they are type parameters in the formal type.
		/// this gets rid of warnings from gcc.
		void addCast( Expression *&actual, Type *formal, const TyVarMap &tyVars ) {
			// type contains polymorphism, but isn't exactly a polytype, in which case it
			// has some real actual type (e.g. unsigned int) and casting to void * is wrong
			if ( hasPolymorphism( formal, tyVars ) && ! isPolyType( formal, tyVars ) ) {
				Type * newType = formal->clone();
				newType = ScrubTyVars::scrub( newType, tyVars );
				actual = new CastExpr( actual, newType );
			} // if
		}

		void Pass1::boxParams( ApplicationExpr *appExpr, FunctionType *function, std::list< Expression *>::iterator &arg, const TyVarMap &exprTyVars ) {
			for ( std::list< DeclarationWithType *>::const_iterator param = function->get_parameters().begin(); param != function->parameters.end(); ++param, ++arg ) {
				assertf( arg != appExpr->args.end(), "boxParams: missing argument for param %s to %s in %s", toString( *param ).c_str(), toString( function ).c_str(), toString( appExpr ).c_str() );
				addCast( *arg, (*param)->get_type(), exprTyVars );
				boxParam( (*param)->get_type(), *arg, exprTyVars );
			} // for
		}

		void Pass1::addInferredParams( ApplicationExpr *appExpr, FunctionType *functionType, std::list< Expression *>::iterator &arg, const TyVarMap &tyVars ) {
			std::list< Expression *>::iterator cur = arg;
			for ( Type::ForallList::iterator tyVar = functionType->get_forall().begin(); tyVar != functionType->get_forall().end(); ++tyVar ) {
				for ( std::list< DeclarationWithType *>::iterator assert = (*tyVar)->assertions.begin(); assert != (*tyVar)->assertions.end(); ++assert ) {
					InferredParams::const_iterator inferParam = appExpr->inferParams.find( (*assert)->get_uniqueId() );
					assertf( inferParam != appExpr->inferParams.end(), "addInferredParams missing inferred parameter: %s in: %s", toString( *assert ).c_str(), toString( appExpr ).c_str() );
					Expression *newExpr = inferParam->second.expr->clone();
					addCast( newExpr, (*assert)->get_type(), tyVars );
					boxParam( (*assert)->get_type(), newExpr, tyVars );
					appExpr->get_args().insert( cur, newExpr );
				} // for
			} // for
		}

		void makeRetParm( FunctionType *funcType ) {
			DeclarationWithType *retParm = funcType->returnVals.front();

			// make a new parameter that is a pointer to the type of the old return value
			retParm->set_type( new PointerType( Type::Qualifiers(), retParm->get_type() ) );
			funcType->get_parameters().push_front( retParm );

			// we don't need the return value any more
			funcType->get_returnVals().clear();
		}

		FunctionType *makeAdapterType( FunctionType *adaptee, const TyVarMap &tyVars ) {
			// actually make the adapter type
			FunctionType *adapter = adaptee->clone();
			if ( isDynRet( adapter, tyVars ) ) {
				makeRetParm( adapter );
			} // if
			adapter->get_parameters().push_front( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::C, 0, new PointerType( Type::Qualifiers(), new FunctionType( Type::Qualifiers(), true ) ), 0 ) );
			return adapter;
		}

		Expression *makeAdapterArg( DeclarationWithType *param, DeclarationWithType *arg, DeclarationWithType *realParam, const TyVarMap &tyVars ) {
			assert( param );
			assert( arg );
			if ( isPolyType( realParam->get_type(), tyVars ) ) {
				if ( ! isPolyType( arg->get_type() ) ) {
					UntypedExpr *deref = new UntypedExpr( new NameExpr( "*?" ) );
					deref->args.push_back( new CastExpr( new VariableExpr( param ), new PointerType( Type::Qualifiers(), arg->get_type()->clone() ) ) );
					deref->result = arg->get_type()->clone();
					return deref;
				} // if
			} // if
			return new VariableExpr( param );
		}

		void addAdapterParams( ApplicationExpr *adapteeApp, std::list< DeclarationWithType *>::iterator arg, std::list< DeclarationWithType *>::iterator param, std::list< DeclarationWithType *>::iterator paramEnd, std::list< DeclarationWithType *>::iterator realParam, const TyVarMap &tyVars ) {
			UniqueName paramNamer( "_p" );
			for ( ; param != paramEnd; ++param, ++arg, ++realParam ) {
				if ( (*param)->get_name() == "" ) {
					(*param)->set_name( paramNamer.newName() );
					(*param)->set_linkage( LinkageSpec::C );
				} // if
				adapteeApp->get_args().push_back( makeAdapterArg( *param, *arg, *realParam, tyVars ) );
			} // for
		}

		FunctionDecl *Pass1::makeAdapter( FunctionType *adaptee, FunctionType *realType, const std::string &mangleName, const TyVarMap &tyVars ) {
			FunctionType *adapterType = makeAdapterType( adaptee, tyVars );
			adapterType = ScrubTyVars::scrub( adapterType, tyVars );
			DeclarationWithType *adapteeDecl = adapterType->get_parameters().front();
			adapteeDecl->set_name( "_adaptee" );
			// do not carry over attributes to real type parameters/return values
			for ( DeclarationWithType * dwt : realType->parameters ) {
				deleteAll( dwt->get_type()->attributes );
				dwt->get_type()->attributes.clear();
			}
			for ( DeclarationWithType * dwt : realType->returnVals ) {
				deleteAll( dwt->get_type()->attributes );
				dwt->get_type()->attributes.clear();
			}
			ApplicationExpr *adapteeApp = new ApplicationExpr( new CastExpr( new VariableExpr( adapteeDecl ), new PointerType( Type::Qualifiers(), realType ) ) );
			Statement *bodyStmt;

			Type::ForallList::iterator tyArg = realType->get_forall().begin();
			Type::ForallList::iterator tyParam = adapterType->get_forall().begin();
			Type::ForallList::iterator realTyParam = adaptee->get_forall().begin();
			for ( ; tyParam != adapterType->get_forall().end(); ++tyArg, ++tyParam, ++realTyParam ) {
				assert( tyArg != realType->get_forall().end() );
				std::list< DeclarationWithType *>::iterator assertArg = (*tyArg)->get_assertions().begin();
				std::list< DeclarationWithType *>::iterator assertParam = (*tyParam)->get_assertions().begin();
				std::list< DeclarationWithType *>::iterator realAssertParam = (*realTyParam)->get_assertions().begin();
				for ( ; assertParam != (*tyParam)->get_assertions().end(); ++assertArg, ++assertParam, ++realAssertParam ) {
					assert( assertArg != (*tyArg)->get_assertions().end() );
					adapteeApp->get_args().push_back( makeAdapterArg( *assertParam, *assertArg, *realAssertParam, tyVars ) );
				} // for
			} // for

			std::list< DeclarationWithType *>::iterator arg = realType->get_parameters().begin();
			std::list< DeclarationWithType *>::iterator param = adapterType->get_parameters().begin();
			std::list< DeclarationWithType *>::iterator realParam = adaptee->get_parameters().begin();
			param++;		// skip adaptee parameter in the adapter type
			if ( realType->get_returnVals().empty() ) {
				// void return
				addAdapterParams( adapteeApp, arg, param, adapterType->get_parameters().end(), realParam, tyVars );
				bodyStmt = new ExprStmt( adapteeApp );
			} else if ( isDynType( adaptee->get_returnVals().front()->get_type(), tyVars ) ) {
				// return type T
				if ( (*param)->get_name() == "" ) {
					(*param)->set_name( "_ret" );
					(*param)->set_linkage( LinkageSpec::C );
				} // if
				UntypedExpr *assign = new UntypedExpr( new NameExpr( "?=?" ) );
				UntypedExpr *deref = UntypedExpr::createDeref( new CastExpr( new VariableExpr( *param++ ), new PointerType( Type::Qualifiers(), realType->get_returnVals().front()->get_type()->clone() ) ) );
				assign->get_args().push_back( deref );
				addAdapterParams( adapteeApp, arg, param, adapterType->get_parameters().end(), realParam, tyVars );
				assign->get_args().push_back( adapteeApp );
				bodyStmt = new ExprStmt( assign );
			} else {
				// adapter for a function that returns a monomorphic value
				addAdapterParams( adapteeApp, arg, param, adapterType->get_parameters().end(), realParam, tyVars );
				bodyStmt = new ReturnStmt( adapteeApp );
			} // if
			CompoundStmt *adapterBody = new CompoundStmt();
			adapterBody->get_kids().push_back( bodyStmt );
			std::string adapterName = makeAdapterName( mangleName );
			return new FunctionDecl( adapterName, Type::StorageClasses(), LinkageSpec::C, adapterType, adapterBody );
		}

		void Pass1::passAdapters( ApplicationExpr * appExpr, FunctionType * functionType, const TyVarMap & exprTyVars ) {
			// collect a list of function types passed as parameters or implicit parameters (assertions)
			std::list< FunctionType*> functions;
			for ( TypeDecl * const tyVar : functionType->get_forall() ) {
				for ( DeclarationWithType * const assert : tyVar->get_assertions() ) {
					findFunction( assert->get_type(), functions, exprTyVars, needsAdapter );
				} // for
			} // for
			for ( DeclarationWithType * const arg : functionType->get_parameters() ) {
				findFunction( arg->get_type(), functions, exprTyVars, needsAdapter );
			} // for

			// parameter function types for which an appropriate adapter has been generated.  we cannot use the types
			// after applying substitutions, since two different parameter types may be unified to the same type
			std::set< std::string > adaptersDone;

			for ( FunctionType * const funType : functions ) {
				FunctionType *originalFunction = funType->clone();
				FunctionType *realFunction = funType->clone();
				std::string mangleName = SymTab::Mangler::mangle( realFunction );

				// only attempt to create an adapter or pass one as a parameter if we haven't already done so for this
				// pre-substitution parameter function type.
				if ( adaptersDone.find( mangleName ) == adaptersDone.end() ) {
					adaptersDone.insert( adaptersDone.begin(), mangleName );

					// apply substitution to type variables to figure out what the adapter's type should look like
					assert( env );
					env->apply( realFunction );
					mangleName = SymTab::Mangler::mangle( realFunction );
					mangleName += makePolyMonoSuffix( originalFunction, exprTyVars );

					typedef ScopedMap< std::string, DeclarationWithType* >::iterator AdapterIter;
					AdapterIter adapter = adapters.find( mangleName );
					if ( adapter == adapters.end() ) {
						// adapter has not been created yet in the current scope, so define it
						FunctionDecl *newAdapter = makeAdapter( funType, realFunction, mangleName, exprTyVars );
						std::pair< AdapterIter, bool > answer = adapters.insert( std::pair< std::string, DeclarationWithType *>( mangleName, newAdapter ) );
						adapter = answer.first;
						stmtsToAddBefore.push_back( new DeclStmt( newAdapter ) );
					} // if
					assert( adapter != adapters.end() );

					// add the appropriate adapter as a parameter
					appExpr->get_args().push_front( new VariableExpr( adapter->second ) );
				} // if
			} // for
		} // passAdapters

		Expression *makeIncrDecrExpr( ApplicationExpr *appExpr, Type *polyType, bool isIncr ) {
			NameExpr *opExpr;
			if ( isIncr ) {
				opExpr = new NameExpr( "?+=?" );
			} else {
				opExpr = new NameExpr( "?-=?" );
			} // if
			UntypedExpr *addAssign = new UntypedExpr( opExpr );
			if ( AddressExpr *address = dynamic_cast< AddressExpr *>( appExpr->get_args().front() ) ) {
				addAssign->get_args().push_back( address->get_arg() );
			} else {
				addAssign->get_args().push_back( appExpr->get_args().front() );
			} // if
			addAssign->get_args().push_back( new NameExpr( sizeofName( mangleType( polyType ) ) ) );
			addAssign->set_result( appExpr->get_result()->clone() );
			if ( appExpr->get_env() ) {
				addAssign->set_env( appExpr->get_env() );
				appExpr->set_env( 0 );
			} // if
			appExpr->get_args().clear();
			delete appExpr;
			return addAssign;
		}

		Expression *Pass1::handleIntrinsics( ApplicationExpr *appExpr ) {
			if ( VariableExpr *varExpr = dynamic_cast< VariableExpr *>( appExpr->function ) ) {
				if ( varExpr->var->linkage == LinkageSpec::Intrinsic ) {
					if ( varExpr->var->name == "?[?]" ) {
						assert( appExpr->result );
						assert( appExpr->get_args().size() == 2 );
						Type *baseType1 = isPolyPtr( appExpr->args.front()->result, scopeTyVars, env );
						Type *baseType2 = isPolyPtr( appExpr->args.back()->result, scopeTyVars, env );
						assert( ! baseType1 || ! baseType2 ); // the arguments cannot both be polymorphic pointers
						UntypedExpr *ret = 0;
						if ( baseType1 || baseType2 ) { // one of the arguments is a polymorphic pointer
							ret = new UntypedExpr( new NameExpr( "?+?" ) );
						} // if
						if ( baseType1 ) {
							UntypedExpr *multiply = new UntypedExpr( new NameExpr( "?*?" ) );
							multiply->get_args().push_back( appExpr->get_args().back() );
							multiply->get_args().push_back( new SizeofExpr( baseType1->clone() ) );
							ret->get_args().push_back( appExpr->get_args().front() );
							ret->get_args().push_back( multiply );
						} else if ( baseType2 ) {
							UntypedExpr *multiply = new UntypedExpr( new NameExpr( "?*?" ) );
							multiply->get_args().push_back( appExpr->get_args().front() );
							multiply->get_args().push_back( new SizeofExpr( baseType2->clone() ) );
							ret->get_args().push_back( multiply );
							ret->get_args().push_back( appExpr->get_args().back() );
						} // if
						if ( baseType1 || baseType2 ) {
							delete ret->get_result();
							ret->set_result( appExpr->get_result()->clone() );
							if ( appExpr->get_env() ) {
								ret->set_env( appExpr->get_env() );
								appExpr->set_env( 0 );
							} // if
							appExpr->get_args().clear();
							delete appExpr;
							return ret;
						} // if
					} else if ( varExpr->get_var()->get_name() == "*?" ) {
						assert( appExpr->result );
						assert( ! appExpr->get_args().empty() );
						if ( isPolyType( appExpr->get_result(), scopeTyVars, env ) ) {
							// remove dereference from polymorphic types since they are boxed.
							Expression *ret = appExpr->get_args().front();
							// fix expr type to remove pointer
							delete ret->get_result();
							ret->set_result( appExpr->get_result()->clone() );
							if ( appExpr->get_env() ) {
								ret->set_env( appExpr->get_env() );
								appExpr->set_env( 0 );
							} // if
							appExpr->get_args().clear();
							delete appExpr;
							return ret;
						} // if
					} else if ( varExpr->get_var()->get_name() == "?++" || varExpr->get_var()->get_name() == "?--" ) {
						assert( appExpr->result );
						assert( appExpr->get_args().size() == 1 );
						if ( Type *baseType = isPolyPtr( appExpr->get_result(), scopeTyVars, env ) ) {
							Type *tempType = appExpr->get_result()->clone();
							if ( env ) {
								env->apply( tempType );
							} // if
							ObjectDecl *newObj = makeTemporary( tempType );
							VariableExpr *tempExpr = new VariableExpr( newObj );
							UntypedExpr *assignExpr = new UntypedExpr( new NameExpr( "?=?" ) );
							assignExpr->get_args().push_back( tempExpr->clone() );
							if ( AddressExpr *address = dynamic_cast< AddressExpr *>( appExpr->get_args().front() ) ) {
								assignExpr->get_args().push_back( address->get_arg()->clone() );
							} else {
								assignExpr->get_args().push_back( appExpr->get_args().front()->clone() );
							} // if
							CommaExpr *firstComma = new CommaExpr( assignExpr, makeIncrDecrExpr( appExpr, baseType, varExpr->get_var()->get_name() == "?++" ) );
							return new CommaExpr( firstComma, tempExpr );
						} // if
					} else if ( varExpr->get_var()->get_name() == "++?" || varExpr->get_var()->get_name() == "--?" ) {
						assert( appExpr->result );
						assert( appExpr->get_args().size() == 1 );
						if ( Type *baseType = isPolyPtr( appExpr->get_result(), scopeTyVars, env ) ) {
							return makeIncrDecrExpr( appExpr, baseType, varExpr->get_var()->get_name() == "++?" );
						} // if
					} else if ( varExpr->get_var()->get_name() == "?+?" || varExpr->get_var()->get_name() == "?-?" ) {
						assert( appExpr->result );
						assert( appExpr->get_args().size() == 2 );
						Type *baseType1 = isPolyPtr( appExpr->get_args().front()->get_result(), scopeTyVars, env );
						Type *baseType2 = isPolyPtr( appExpr->get_args().back()->get_result(), scopeTyVars, env );
						if ( baseType1 && baseType2 ) {
							UntypedExpr *divide = new UntypedExpr( new NameExpr( "?/?" ) );
							divide->get_args().push_back( appExpr );
							divide->get_args().push_back( new SizeofExpr( baseType1->clone() ) );
							divide->set_result( appExpr->get_result()->clone() );
							if ( appExpr->get_env() ) {
								divide->set_env( appExpr->get_env() );
								appExpr->set_env( 0 );
							} // if
							return divide;
						} else if ( baseType1 ) {
							UntypedExpr *multiply = new UntypedExpr( new NameExpr( "?*?" ) );
							multiply->get_args().push_back( appExpr->get_args().back() );
							multiply->get_args().push_back( new SizeofExpr( baseType1->clone() ) );
							appExpr->get_args().back() = multiply;
						} else if ( baseType2 ) {
							UntypedExpr *multiply = new UntypedExpr( new NameExpr( "?*?" ) );
							multiply->get_args().push_back( appExpr->get_args().front() );
							multiply->get_args().push_back( new SizeofExpr( baseType2->clone() ) );
							appExpr->get_args().front() = multiply;
						} // if
					} else if ( varExpr->get_var()->get_name() == "?+=?" || varExpr->get_var()->get_name() == "?-=?" ) {
						assert( appExpr->result );
						assert( appExpr->get_args().size() == 2 );
						Type *baseType = isPolyPtr( appExpr->get_result(), scopeTyVars, env );
						if ( baseType ) {
							UntypedExpr *multiply = new UntypedExpr( new NameExpr( "?*?" ) );
							multiply->get_args().push_back( appExpr->get_args().back() );
							multiply->get_args().push_back( new SizeofExpr( baseType->clone() ) );
							appExpr->get_args().back() = multiply;
						} // if
					} // if
					return appExpr;
				} // if
			} // if
			return 0;
		}

		Expression *Pass1::postmutate( ApplicationExpr *appExpr ) {
			// std::cerr << "mutate appExpr: " << InitTweak::getFunctionName( appExpr ) << std::endl;
			// for ( TyVarMap::iterator i = scopeTyVars.begin(); i != scopeTyVars.end(); ++i ) {
			// 	std::cerr << i->first << " ";
			// }
			// std::cerr << "\n";

			assert( appExpr->function->result );
			FunctionType * function = getFunctionType( appExpr->function->result );
			assertf( function, "ApplicationExpr has non-function type: %s", toString( appExpr->function->result ).c_str() );

			if ( Expression *newExpr = handleIntrinsics( appExpr ) ) {
				return newExpr;
			} // if

			Expression *ret = appExpr;

			std::list< Expression *>::iterator arg = appExpr->get_args().begin();
			std::list< Expression *>::iterator paramBegin = appExpr->get_args().begin();

			TyVarMap exprTyVars( TypeDecl::Data{} );
			makeTyVarMap( function, exprTyVars ); // xxx - should this take into account the variables already bound in scopeTyVars (i.e. remove them from exprTyVars?)
			ReferenceToType *dynRetType = isDynRet( function, exprTyVars );

			// std::cerr << function << std::endl;
			// std::cerr << "scopeTyVars: ";
			// printTyVarMap( std::cerr, scopeTyVars );
			// std::cerr << "exprTyVars: ";
			// printTyVarMap( std::cerr, exprTyVars );
			// std::cerr << "env: " << *env << std::endl;
			// std::cerr << needsAdapter( function, scopeTyVars ) << ! needsAdapter( function, exprTyVars) << std::endl;

			// NOTE: addDynRetParam needs to know the actual (generated) return type so it can make a temp variable, so pass the result type from the appExpr
			// passTypeVars needs to know the program-text return type (i.e. the distinction between _conc_T30 and T3(int))
			// concRetType may not be a good name in one or both of these places. A more appropriate name change is welcome.
			if ( dynRetType ) {
				// std::cerr << "dynRetType: " << dynRetType << std::endl;
				Type *concRetType = appExpr->get_result()->isVoid() ? nullptr : appExpr->get_result();
				ret = addDynRetParam( appExpr, concRetType, arg ); // xxx - used to use dynRetType instead of concRetType
			} else if ( needsAdapter( function, scopeTyVars ) && ! needsAdapter( function, exprTyVars) ) { // xxx - exprTyVars is used above...?
				// xxx - the ! needsAdapter check may be incorrect. It seems there is some situation where an adapter is applied where it shouldn't be, and this fixes it for some cases. More investigation is needed.

				// std::cerr << "needs adapter: ";
				// printTyVarMap( std::cerr, scopeTyVars );
				// std::cerr << *env << std::endl;
				// change the application so it calls the adapter rather than the passed function
				ret = applyAdapter( appExpr, function, arg, scopeTyVars );
			} // if
			arg = appExpr->get_args().begin();

			Type *concRetType = replaceWithConcrete( appExpr, dynRetType );
			passTypeVars( appExpr, concRetType, arg, exprTyVars ); // xxx - used to use dynRetType instead of concRetType; this changed so that the correct type paramaters are passed for return types (it should be the concrete type's parameters, not the formal type's)
			addInferredParams( appExpr, function, arg, exprTyVars );

			arg = paramBegin;

			boxParams( appExpr, function, arg, exprTyVars );
			passAdapters( appExpr, function, exprTyVars );

			return ret;
		}

		Expression * Pass1::postmutate( UntypedExpr *expr ) {
			if ( expr->result && isPolyType( expr->result, scopeTyVars, env ) ) {
				if ( NameExpr *name = dynamic_cast< NameExpr *>( expr->function ) ) {
					if ( name->name == "*?" ) {
						Expression *ret = expr->args.front();
						expr->args.clear();
						delete expr;
						return ret;
					} // if
				} // if
			} // if
			return expr;
		}

		void Pass1::premutate( AddressExpr * ) { visit_children = false; }
		Expression * Pass1::postmutate( AddressExpr * addrExpr ) {
			assert( addrExpr->arg->result && ! addrExpr->arg->result->isVoid() );

			bool needs = false;
			if ( UntypedExpr *expr = dynamic_cast< UntypedExpr *>( addrExpr->arg ) ) {
				if ( expr->result && isPolyType( expr->result, scopeTyVars, env ) ) {
					if ( NameExpr *name = dynamic_cast< NameExpr *>( expr->function ) ) {
						if ( name->name == "*?" ) {
							if ( ApplicationExpr * appExpr = dynamic_cast< ApplicationExpr * >( expr->args.front() ) ) {
								assert( appExpr->function->result );
								FunctionType *function = getFunctionType( appExpr->function->result );
								assert( function );
								needs = needsAdapter( function, scopeTyVars );
							} // if
						} // if
					} // if
				} // if
			} // if
			// isPolyType check needs to happen before mutating addrExpr arg, so pull it forward
			// out of the if condition.
			addrExpr->arg = addrExpr->arg->acceptMutator( *visitor );
			// ... but must happen after mutate, since argument might change (e.g. intrinsic *?, ?[?]) - re-evaluate above comment
			bool polytype = isPolyType( addrExpr->arg->result, scopeTyVars, env );
			if ( polytype || needs ) {
				Expression *ret = addrExpr->arg;
				delete ret->result;
				ret->result = addrExpr->result->clone();
				addrExpr->arg = nullptr;
				delete addrExpr;
				return ret;
			} else {
				return addrExpr;
			} // if
		}

		void Pass1::premutate( ReturnStmt *returnStmt ) {
			if ( retval && returnStmt->expr ) {
				assert( returnStmt->expr->result && ! returnStmt->expr->result->isVoid() );
				delete returnStmt->expr;
				returnStmt->expr = nullptr;
			} // if
		}

		void Pass1::premutate( PointerType *pointerType ) {
			GuardScope( scopeTyVars );
			makeTyVarMap( pointerType, scopeTyVars );
		}

		void Pass1::premutate( FunctionType *functionType ) {
			GuardScope( scopeTyVars );
			makeTyVarMap( functionType, scopeTyVars );
		}

		void Pass1::beginScope() {
			adapters.beginScope();
		}

		void Pass1::endScope() {
			adapters.endScope();
		}

////////////////////////////////////////// Pass2 ////////////////////////////////////////////////////

		void Pass2::addAdapters( FunctionType *functionType ) {
			std::list< DeclarationWithType *> &paramList = functionType->parameters;
			std::list< FunctionType *> functions;
			for (  DeclarationWithType * const arg : functionType->parameters ) {
				Type *orig = arg->get_type();
				findAndReplaceFunction( orig, functions, scopeTyVars, needsAdapter );
				arg->set_type( orig );
			}
			std::set< std::string > adaptersDone;
			for ( FunctionType * const funType : functions ) {
				std::string mangleName = mangleAdapterName( funType, scopeTyVars );
				if ( adaptersDone.find( mangleName ) == adaptersDone.end() ) {
					std::string adapterName = makeAdapterName( mangleName );
					// adapter may not be used in body, pass along with unused attribute.
					paramList.push_front( new ObjectDecl( adapterName, Type::StorageClasses(), LinkageSpec::C, 0, new PointerType( Type::Qualifiers(), makeAdapterType( funType, scopeTyVars ) ), 0, { new Attribute( "unused" ) } ) );
					adaptersDone.insert( adaptersDone.begin(), mangleName );
				}
			}
//  deleteAll( functions );
		}

		DeclarationWithType * Pass2::postmutate( FunctionDecl *functionDecl ) {
			FunctionType * ftype = functionDecl->type;
			if ( ! ftype->returnVals.empty() && functionDecl->statements ) {
				// intrinsic functions won't be using the _retval so no need to generate it.
				if ( functionDecl->linkage != LinkageSpec::Intrinsic && !isPrefix( functionDecl->name, "_thunk" ) && ! isPrefix( functionDecl->name, "_adapter" ) ) { // xxx - remove check for prefix once thunks properly use ctor/dtors
					assert( ftype->returnVals.size() == 1 );
					DeclarationWithType * retval = ftype->returnVals.front();
					if ( retval->name == "" ) {
						retval->name = "_retval";
					}
					functionDecl->statements->kids.push_front( new DeclStmt( retval ) );
					DeclarationWithType * newRet = retval->clone(); // for ownership purposes
					ftype->returnVals.front() = newRet;
				}
			}
			// errors should have been caught by this point, remove initializers from parameters to allow correct codegen of default arguments
			for ( Declaration * param : functionDecl->type->parameters ) {
				if ( ObjectDecl * obj = dynamic_cast< ObjectDecl * >( param ) ) {
					delete obj->init;
					obj->init = nullptr;
				}
			}
			return functionDecl;
		}

		void Pass2::premutate( StructDecl * ) {
			// prevent tyVars from leaking into containing scope
			GuardScope( scopeTyVars );
		}

		void Pass2::premutate( UnionDecl * ) {
			// prevent tyVars from leaking into containing scope
			GuardScope( scopeTyVars );
		}

		void Pass2::premutate( TraitDecl * ) {
			// prevent tyVars from leaking into containing scope
			GuardScope( scopeTyVars );
		}

		void Pass2::premutate( TypeDecl *typeDecl ) {
			addToTyVarMap( typeDecl, scopeTyVars );
		}

		void Pass2::premutate( PointerType *pointerType ) {
			GuardScope( scopeTyVars );
			makeTyVarMap( pointerType, scopeTyVars );
		}

		void Pass2::premutate( FunctionType *funcType ) {
			GuardScope( scopeTyVars );
			makeTyVarMap( funcType, scopeTyVars );

			// move polymorphic return type to parameter list
			if ( isDynRet( funcType ) ) {
				ObjectDecl *ret = strict_dynamic_cast< ObjectDecl* >( funcType->get_returnVals().front() );
				ret->set_type( new PointerType( Type::Qualifiers(), ret->get_type() ) );
				funcType->get_parameters().push_front( ret );
				funcType->get_returnVals().pop_front();
				ret->set_init( nullptr ); // xxx - memory leak?
			}

			// add size/align and assertions for type parameters to parameter list
			std::list< DeclarationWithType *>::iterator last = funcType->get_parameters().begin();
			std::list< DeclarationWithType *> inferredParams;
			// size/align/offset parameters may not be used in body, pass along with unused attribute.
			ObjectDecl newObj( "", Type::StorageClasses(), LinkageSpec::C, 0, new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ), 0,
			                   { new Attribute( "unused" ) } );
			ObjectDecl newPtr( "", Type::StorageClasses(), LinkageSpec::C, 0,
			                   new PointerType( Type::Qualifiers(), new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ) ), 0 );
			for ( TypeDecl * const tyParam : funcType->get_forall() ) {
				ObjectDecl *sizeParm, *alignParm;
				// add all size and alignment parameters to parameter list
				if ( tyParam->isComplete() ) {
					TypeInstType parmType( Type::Qualifiers(), tyParam->get_name(), tyParam );
					std::string parmName = mangleType( &parmType );

					sizeParm = newObj.clone();
					sizeParm->set_name( sizeofName( parmName ) );
					last = funcType->get_parameters().insert( last, sizeParm );
					++last;

					alignParm = newObj.clone();
					alignParm->set_name( alignofName( parmName ) );
					last = funcType->get_parameters().insert( last, alignParm );
					++last;
				}
				// move all assertions into parameter list
				for ( DeclarationWithType * const assert : tyParam->get_assertions() ) {
					// assertion parameters may not be used in body, pass along with unused attribute.
					assert->get_attributes().push_back( new Attribute( "unused" ) );
					inferredParams.push_back( assert );
				}
				tyParam->get_assertions().clear();
			}

			// add size/align for generic parameter types to parameter list
			std::set< std::string > seenTypes; // sizeofName for generic types we've seen
			for ( DeclarationWithType * const fnParam : funcType->get_parameters() ) {
				Type *polyType = isPolyType( fnParam->get_type(), scopeTyVars );
				if ( polyType && ! dynamic_cast< TypeInstType* >( polyType ) ) {
					std::string typeName = mangleType( polyType );
					if ( seenTypes.count( typeName ) ) continue;

					ObjectDecl *sizeParm, *alignParm, *offsetParm;
					sizeParm = newObj.clone();
					sizeParm->set_name( sizeofName( typeName ) );
					last = funcType->get_parameters().insert( last, sizeParm );
					++last;

					alignParm = newObj.clone();
					alignParm->set_name( alignofName( typeName ) );
					last = funcType->get_parameters().insert( last, alignParm );
					++last;

					if ( StructInstType *polyBaseStruct = dynamic_cast< StructInstType* >( polyType ) ) {
						// NOTE zero-length arrays are illegal in C, so empty structs have no offset array
						if ( ! polyBaseStruct->get_baseStruct()->get_members().empty() ) {
							offsetParm = newPtr.clone();
							offsetParm->set_name( offsetofName( typeName ) );
							last = funcType->get_parameters().insert( last, offsetParm );
							++last;
						}
					}
					seenTypes.insert( typeName );
				}
			}

			// splice assertion parameters into parameter list
			funcType->get_parameters().splice( last, inferredParams );
			addAdapters( funcType );
		}

////////////////////////////////////////// PolyGenericCalculator ////////////////////////////////////////////////////

		PolyGenericCalculator::PolyGenericCalculator()
			: knownLayouts(), knownOffsets(), bufNamer( "_buf" ) {}

		void PolyGenericCalculator::beginTypeScope( Type *ty ) {
			GuardScope( scopeTyVars );
			makeTyVarMap( ty, scopeTyVars );
		}

		void PolyGenericCalculator::beginGenericScope() {
			GuardScope( *this );
			// We expect the first function type see to be the type relating to this scope
			// but any further type is probably some unrelated function pointer
			// keep track of which is the first
			GuardValue( expect_func_type );
			expect_func_type = true;
		}

		void PolyGenericCalculator::premutate( ObjectDecl *objectDecl ) {
			beginTypeScope( objectDecl->get_type() );
		}

		void PolyGenericCalculator::premutate( FunctionDecl *functionDecl ) {
			beginGenericScope();

			beginTypeScope( functionDecl->get_functionType() );
		}

		void PolyGenericCalculator::premutate( TypedefDecl *typedefDecl ) {
			assert(false);
			beginTypeScope( typedefDecl->get_base() );
		}

		void PolyGenericCalculator::premutate( TypeDecl * typeDecl ) {
			addToTyVarMap( typeDecl, scopeTyVars );
		}

		Declaration * PolyGenericCalculator::postmutate( TypeDecl *typeDecl ) {
			if ( Type * base = typeDecl->base ) {
				// add size/align variables for opaque type declarations
				TypeInstType inst( Type::Qualifiers(), typeDecl->name, typeDecl );
				std::string typeName = mangleType( &inst );
				Type *layoutType = new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt );

				ObjectDecl * sizeDecl = ObjectDecl::newObject( sizeofName( typeName ), layoutType, new SingleInit( new SizeofExpr( base->clone() ) ) );
				ObjectDecl * alignDecl = ObjectDecl::newObject( alignofName( typeName ), layoutType->clone(), new SingleInit( new AlignofExpr( base->clone() ) ) );

				// ensure that the initializing sizeof/alignof exprs are properly mutated
				sizeDecl->acceptMutator( *visitor );
				alignDecl->acceptMutator( *visitor );

				// can't use makeVar, because it inserts into stmtsToAdd and TypeDecls can occur at global scope
				declsToAddAfter.push_back( alignDecl );
				// replace with sizeDecl
				return sizeDecl;
			}
			return typeDecl;
		}

		void PolyGenericCalculator::premutate( PointerType *pointerType ) {
			beginTypeScope( pointerType );
		}

		void PolyGenericCalculator::premutate( FunctionType *funcType ) {
			beginTypeScope( funcType );

			GuardValue( expect_func_type );

			if(!expect_func_type) {
				GuardAction( [this]() {
					knownLayouts.endScope();
					knownOffsets.endScope();
				});
				// If this is the first function type we see
				// Then it's the type of the declaration and we care about it
				knownLayouts.beginScope();
				knownOffsets.beginScope();
			}

			// The other functions type we will see in this scope are probably functions parameters
			// they don't help us with the layout and offsets so don't mark them as known in this scope
			expect_func_type = false;

			// make sure that any type information passed into the function is accounted for
			for ( DeclarationWithType * const fnParam : funcType->get_parameters() ) {
				// condition here duplicates that in Pass2::mutate( FunctionType* )
				Type *polyType = isPolyType( fnParam->get_type(), scopeTyVars );
				if ( polyType && ! dynamic_cast< TypeInstType* >( polyType ) ) {
					knownLayouts.insert( mangleType( polyType ) );
				}
			}
		}

		/// converts polymorphic type T into a suitable monomorphic representation, currently: __attribute__((aligned(8)) char[size_T]
		Type * polyToMonoType( Type * declType ) {
			Type * charType = new BasicType( Type::Qualifiers(), BasicType::Kind::Char);
			Expression * size = new NameExpr( sizeofName( mangleType(declType) ) );
			Attribute * aligned = new Attribute( "aligned", std::list<Expression*>{ new ConstantExpr( Constant::from_int(8) ) } );
			return new ArrayType( Type::Qualifiers(), charType, size,
				true, false, std::list<Attribute *>{ aligned } );
		}

		void PolyGenericCalculator::mutateMembers( AggregateDecl * aggrDecl ) {
			std::set< std::string > genericParams;
			for ( TypeDecl * td : aggrDecl->parameters ) {
				genericParams.insert( td->name );
			}
			for ( Declaration * decl : aggrDecl->members ) {
				if ( ObjectDecl * field = dynamic_cast< ObjectDecl * >( decl ) ) {
					Type * ty = replaceTypeInst( field->type, env );
					if ( TypeInstType *typeInst = dynamic_cast< TypeInstType* >( ty ) ) {
						// do not try to monomorphize generic parameters
						if ( scopeTyVars.find( typeInst->get_name() ) != scopeTyVars.end() && ! genericParams.count( typeInst->name ) ) {
							// polymorphic aggregate members should be converted into monomorphic members.
							// Using char[size_T] here respects the expected sizing rules of an aggregate type.
							Type * newType = polyToMonoType( field->type );
							delete field->type;
							field->type = newType;
						}
					}
				}
			}
		}

		void PolyGenericCalculator::premutate( StructDecl * structDecl ) {
			mutateMembers( structDecl );
		}

		void PolyGenericCalculator::premutate( UnionDecl * unionDecl ) {
			mutateMembers( unionDecl );
		}

		void PolyGenericCalculator::premutate( DeclStmt *declStmt ) {
			if ( ObjectDecl *objectDecl = dynamic_cast< ObjectDecl *>( declStmt->get_decl() ) ) {
				if ( findGeneric( objectDecl->get_type() ) ) {
					// change initialization of a polymorphic value object to allocate via a VLA
					// (alloca was previously used, but can't be safely used in loops)
					ObjectDecl *newBuf = ObjectDecl::newObject( bufNamer.newName(), polyToMonoType( objectDecl->type ), nullptr );
					stmtsToAddBefore.push_back( new DeclStmt( newBuf ) );

					// if the object has a cleanup attribute, the cleanup should be on the buffer, not the pointer
					auto matchAndMove = [newBuf](Attribute * attr){
						if(attr->name == "cleanup") {
							newBuf->attributes.push_back(attr);
							return true;
						}
						return false;
					};

					objectDecl->attributes.remove_if(matchAndMove);

					delete objectDecl->get_init();
					objectDecl->set_init( new SingleInit( new VariableExpr( newBuf ) ) );
				}
			}
		}

		/// Finds the member in the base list that matches the given declaration; returns its index, or -1 if not present
		long findMember( DeclarationWithType *memberDecl, std::list< Declaration* > &baseDecls ) {
			for ( auto pair : enumerate( baseDecls ) ) {
				Declaration * decl = pair.val;
				size_t i = pair.idx;
				if ( memberDecl->get_name() != decl->get_name() )
					continue;

				if ( memberDecl->get_name().empty() ) {
					// plan-9 field: match on unique_id
					if ( memberDecl->get_uniqueId() == decl->get_uniqueId() )
						return i;
					else
						continue;
				}

				DeclarationWithType *declWithType = strict_dynamic_cast< DeclarationWithType* >( decl );

				if ( memberDecl->get_mangleName().empty() || declWithType->get_mangleName().empty() ) {
					// tuple-element field: expect neither had mangled name; accept match on simple name (like field_2) only
					assert( memberDecl->get_mangleName().empty() && declWithType->get_mangleName().empty() );
					return i;
				}

				// ordinary field: use full name to accommodate overloading
				if ( memberDecl->get_mangleName() == declWithType->get_mangleName() )
					return i;
				else
					continue;
			}
			return -1;
		}

		/// Returns an index expression into the offset array for a type
		Expression *makeOffsetIndex( Type *objectType, long i ) {
			ConstantExpr *fieldIndex = new ConstantExpr( Constant::from_ulong( i ) );
			UntypedExpr *fieldOffset = new UntypedExpr( new NameExpr( "?[?]" ) );
			fieldOffset->get_args().push_back( new NameExpr( offsetofName( mangleType( objectType ) ) ) );
			fieldOffset->get_args().push_back( fieldIndex );
			return fieldOffset;
		}

		Expression *PolyGenericCalculator::postmutate( MemberExpr *memberExpr ) {
			// only mutate member expressions for polymorphic types
			int tyDepth;
			Type *objectType = hasPolyBase( memberExpr->aggregate->result, scopeTyVars, &tyDepth );
			if ( ! objectType ) return memberExpr;
			findGeneric( objectType ); // ensure layout for this type is available

			// replace member expression with dynamically-computed layout expression
			Expression *newMemberExpr = nullptr;
			if ( StructInstType *structType = dynamic_cast< StructInstType* >( objectType ) ) {
				// look up offset index
				long i = findMember( memberExpr->member, structType->baseStruct->members );
				if ( i == -1 ) return memberExpr;

				// replace member expression with pointer to base plus offset
				UntypedExpr *fieldLoc = new UntypedExpr( new NameExpr( "?+?" ) );
				Expression * aggr = memberExpr->aggregate->clone();
				delete aggr->env; // xxx - there's a problem with keeping the env for some reason, so for now just get rid of it
				aggr->env = nullptr;
				fieldLoc->get_args().push_back( aggr );
				fieldLoc->get_args().push_back( makeOffsetIndex( objectType, i ) );
				fieldLoc->set_result( memberExpr->result->clone() );
				newMemberExpr = fieldLoc;
			} else if ( dynamic_cast< UnionInstType* >( objectType ) ) {
				// union members are all at offset zero, so just use the aggregate expr
				Expression * aggr = memberExpr->aggregate->clone();
				delete aggr->env; // xxx - there's a problem with keeping the env for some reason, so for now just get rid of it
				aggr->env= nullptr;
				newMemberExpr = aggr;
				newMemberExpr->result = memberExpr->result->clone();
			} else return memberExpr;
			assert( newMemberExpr );

			// Must apply the generic substitution to the member type to handle cases where the member is a generic parameter substituted by a known concrete type, e.g.
			//   forall(otype T) struct Box { T x; }
			//   forall(otype T) f() {
			//     Box(T *) b; b.x;
			//   }
			// TODO: memberExpr->result should be exactly memberExpr->member->get_type() after substitution, so it doesn't seem like it should be necessary to apply the substitution manually. For some reason this is not currently the case. This requires more investigation.
			Type *memberType = memberExpr->member->get_type()->clone();
			TypeSubstitution sub = objectType->genericSubstitution();
			sub.apply( memberType );
			if ( ! isPolyType( memberType, scopeTyVars ) ) {
				// Not all members of a polymorphic type are themselves of polymorphic type; in this case the member expression should be wrapped and dereferenced to form an lvalue
				CastExpr *ptrCastExpr = new CastExpr( newMemberExpr, new PointerType( Type::Qualifiers(), memberType->clone() ) );
				UntypedExpr *derefExpr = UntypedExpr::createDeref( ptrCastExpr );
				newMemberExpr = derefExpr;
			}

			delete memberType;
			delete memberExpr;
			return newMemberExpr;
		}

		void PolyGenericCalculator::premutate( AddressExpr * addrExpr ) {
			GuardValue( addrMember );
			// is the argument a MemberExpr before mutating?
			addrMember = dynamic_cast< MemberExpr * >( addrExpr->arg );
		}

		Expression * PolyGenericCalculator::postmutate( AddressExpr * addrExpr ) {
			if ( addrMember && addrMember != addrExpr->arg ) {
				// arg was a MemberExpr and has been mutated
				if ( UntypedExpr * untyped = dynamic_cast< UntypedExpr * >( addrExpr->arg ) ) {
					if ( InitTweak::getFunctionName( untyped ) == "?+?" ) {
						// MemberExpr was converted to pointer+offset, and it is not valid C to take the address of an addition, so strip the address-of
						// TODO: should  addrExpr->arg->result be changed to addrExpr->result?
						Expression * ret = addrExpr->arg;
						addrExpr->arg = nullptr;
						std::swap( addrExpr->env, ret->env );
						delete addrExpr;
						return ret;
					}
				}
			}
			return addrExpr;
		}

		ObjectDecl *PolyGenericCalculator::makeVar( const std::string &name, Type *type, Initializer *init ) {
			ObjectDecl *newObj = new ObjectDecl( name, Type::StorageClasses(), LinkageSpec::C, nullptr, type, init );
			stmtsToAddBefore.push_back( new DeclStmt( newObj ) );
			return newObj;
		}

		void PolyGenericCalculator::addOtypeParamsToLayoutCall( UntypedExpr *layoutCall, const std::list< Type* > &otypeParams ) {
			for ( Type * const param : otypeParams ) {
				if ( findGeneric( param ) ) {
					// push size/align vars for a generic parameter back
					std::string paramName = mangleType( param );
					layoutCall->get_args().push_back( new NameExpr( sizeofName( paramName ) ) );
					layoutCall->get_args().push_back( new NameExpr( alignofName( paramName ) ) );
				} else {
					layoutCall->get_args().push_back( new SizeofExpr( param->clone() ) );
					layoutCall->get_args().push_back( new AlignofExpr( param->clone() ) );
				}
			}
		}

		/// returns true if any of the otype parameters have a dynamic layout and puts all otype parameters in the output list
		bool findGenericParams( std::list< TypeDecl* > &baseParams, std::list< Expression* > &typeParams, std::list< Type* > &out ) {
			bool hasDynamicLayout = false;

			std::list< TypeDecl* >::const_iterator baseParam = baseParams.begin();
			std::list< Expression* >::const_iterator typeParam = typeParams.begin();
			for ( ; baseParam != baseParams.end() && typeParam != typeParams.end(); ++baseParam, ++typeParam ) {
				// skip non-otype parameters
				if ( ! (*baseParam)->isComplete() ) continue;
				TypeExpr *typeExpr = dynamic_cast< TypeExpr* >( *typeParam );
				assert( typeExpr && "all otype parameters should be type expressions" );

				Type *type = typeExpr->get_type();
				out.push_back( type );
				if ( isPolyType( type ) ) hasDynamicLayout = true;
			}
			assert( baseParam == baseParams.end() && typeParam == typeParams.end() );

			return hasDynamicLayout;
		}

		bool PolyGenericCalculator::findGeneric( Type *ty ) {
			ty = replaceTypeInst( ty, env );

			if ( TypeInstType *typeInst = dynamic_cast< TypeInstType* >( ty ) ) {
				if ( scopeTyVars.find( typeInst->get_name() ) != scopeTyVars.end() ) {
					// NOTE assumes here that getting put in the scopeTyVars included having the layout variables set
					return true;
				}
				return false;
			} else if ( StructInstType *structTy = dynamic_cast< StructInstType* >( ty ) ) {
				// check if this type already has a layout generated for it
				std::string typeName = mangleType( ty );
				if ( knownLayouts.find( typeName ) != knownLayouts.end() ) return true;

				// check if any of the type parameters have dynamic layout; if none do, this type is (or will be) monomorphized
				std::list< Type* > otypeParams;
				if ( ! findGenericParams( *structTy->get_baseParameters(), structTy->get_parameters(), otypeParams ) ) return false;

				// insert local variables for layout and generate call to layout function
				knownLayouts.insert( typeName );  // done early so as not to interfere with the later addition of parameters to the layout call
				Type *layoutType = new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt );

				int n_members = structTy->get_baseStruct()->get_members().size();
				if ( n_members == 0 ) {
					// all empty structs have the same layout - size 1, align 1
					makeVar( sizeofName( typeName ), layoutType, new SingleInit( new ConstantExpr( Constant::from_ulong( (unsigned long)1 ) ) ) );
					makeVar( alignofName( typeName ), layoutType->clone(), new SingleInit( new ConstantExpr( Constant::from_ulong( (unsigned long)1 ) ) ) );
					// NOTE zero-length arrays are forbidden in C, so empty structs have no offsetof array
				} else {
					ObjectDecl *sizeVar = makeVar( sizeofName( typeName ), layoutType );
					ObjectDecl *alignVar = makeVar( alignofName( typeName ), layoutType->clone() );
					ObjectDecl *offsetVar = makeVar( offsetofName( typeName ), new ArrayType( Type::Qualifiers(), layoutType->clone(), new ConstantExpr( Constant::from_int( n_members ) ), false, false ) );

					// generate call to layout function
					UntypedExpr *layoutCall = new UntypedExpr( new NameExpr( layoutofName( structTy->get_baseStruct() ) ) );
					layoutCall->get_args().push_back( new AddressExpr( new VariableExpr( sizeVar ) ) );
					layoutCall->get_args().push_back( new AddressExpr( new VariableExpr( alignVar ) ) );
					layoutCall->get_args().push_back( new VariableExpr( offsetVar ) );
					addOtypeParamsToLayoutCall( layoutCall, otypeParams );

					stmtsToAddBefore.push_back( new ExprStmt( layoutCall ) );
				}

				// std::cout << "TRUE 2" << std::endl;

				return true;
			} else if ( UnionInstType *unionTy = dynamic_cast< UnionInstType* >( ty ) ) {
				// check if this type already has a layout generated for it
				std::string typeName = mangleType( ty );
				if ( knownLayouts.find( typeName ) != knownLayouts.end() ) return true;

				// check if any of the type parameters have dynamic layout; if none do, this type is (or will be) monomorphized
				std::list< Type* > otypeParams;
				if ( ! findGenericParams( *unionTy->get_baseParameters(), unionTy->get_parameters(), otypeParams ) ) return false;

				// insert local variables for layout and generate call to layout function
				knownLayouts.insert( typeName );  // done early so as not to interfere with the later addition of parameters to the layout call
				Type *layoutType = new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt );

				ObjectDecl *sizeVar = makeVar( sizeofName( typeName ), layoutType );
				ObjectDecl *alignVar = makeVar( alignofName( typeName ), layoutType->clone() );

				// generate call to layout function
				UntypedExpr *layoutCall = new UntypedExpr( new NameExpr( layoutofName( unionTy->get_baseUnion() ) ) );
				layoutCall->get_args().push_back( new AddressExpr( new VariableExpr( sizeVar ) ) );
				layoutCall->get_args().push_back( new AddressExpr( new VariableExpr( alignVar ) ) );
				addOtypeParamsToLayoutCall( layoutCall, otypeParams );

				stmtsToAddBefore.push_back( new ExprStmt( layoutCall ) );

				return true;
			}

			return false;
		}

		Expression * PolyGenericCalculator::genSizeof( Type* ty ) {
			if ( ArrayType * aty = dynamic_cast<ArrayType *>(ty) ) {
				// generate calculated size for possibly generic array
				Expression * sizeofBase = genSizeof( aty->get_base() );
				if ( ! sizeofBase ) return nullptr;
				Expression * dim = aty->get_dimension();
				aty->set_dimension( nullptr );
				return makeOp( "?*?", sizeofBase, dim );
			} else if ( findGeneric( ty ) ) {
				// generate calculated size for generic type
				return new NameExpr( sizeofName( mangleType( ty ) ) );
			} else return nullptr;
		}

		Expression *PolyGenericCalculator::postmutate( SizeofExpr *sizeofExpr ) {
			Type *ty = sizeofExpr->get_isType() ?
				sizeofExpr->get_type() : sizeofExpr->get_expr()->get_result();

			Expression * gen = genSizeof( ty );
			if ( gen ) {
				delete sizeofExpr;
				return gen;
			} else return sizeofExpr;
		}

		Expression *PolyGenericCalculator::postmutate( AlignofExpr *alignofExpr ) {
			Type *ty = alignofExpr->get_isType() ? alignofExpr->get_type() : alignofExpr->get_expr()->get_result();
			if ( findGeneric( ty ) ) {
				Expression *ret = new NameExpr( alignofName( mangleType( ty ) ) );
				delete alignofExpr;
				return ret;
			}
			return alignofExpr;
		}

		Expression *PolyGenericCalculator::postmutate( OffsetofExpr *offsetofExpr ) {
			// only mutate expressions for polymorphic structs/unions
			Type *ty = offsetofExpr->get_type();
			if ( ! findGeneric( ty ) ) return offsetofExpr;

			if ( StructInstType *structType = dynamic_cast< StructInstType* >( ty ) ) {
				// replace offsetof expression by index into offset array
				long i = findMember( offsetofExpr->get_member(), structType->get_baseStruct()->get_members() );
				if ( i == -1 ) return offsetofExpr;

				Expression *offsetInd = makeOffsetIndex( ty, i );
				delete offsetofExpr;
				return offsetInd;
			} else if ( dynamic_cast< UnionInstType* >( ty ) ) {
				// all union members are at offset zero
				delete offsetofExpr;
				return new ConstantExpr( Constant::from_ulong( 0 ) );
			} else return offsetofExpr;
		}

		Expression *PolyGenericCalculator::postmutate( OffsetPackExpr *offsetPackExpr ) {
			StructInstType *ty = offsetPackExpr->get_type();

			Expression *ret = 0;
			if ( findGeneric( ty ) ) {
				// pull offset back from generated type information
				ret = new NameExpr( offsetofName( mangleType( ty ) ) );
			} else {
				std::string offsetName = offsetofName( mangleType( ty ) );
				if ( knownOffsets.find( offsetName ) != knownOffsets.end() ) {
					// use the already-generated offsets for this type
					ret = new NameExpr( offsetName );
				} else {
					knownOffsets.insert( offsetName );

					std::list< Declaration* > &baseMembers = ty->get_baseStruct()->get_members();
					Type *offsetType = new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt );

					// build initializer list for offset array
					std::list< Initializer* > inits;
					for ( std::list< Declaration* >::const_iterator member = baseMembers.begin(); member != baseMembers.end(); ++member ) {
						if ( DeclarationWithType *memberDecl = dynamic_cast< DeclarationWithType* >( *member ) ) {
							inits.push_back( new SingleInit( new OffsetofExpr( ty->clone(), memberDecl ) ) );
						} else {
							assertf( false, "Requesting offset of Non-DWT member: %s", toString( *member ).c_str() );
						}
					}

					// build the offset array and replace the pack with a reference to it
					ObjectDecl *offsetArray = makeVar( offsetName, new ArrayType( Type::Qualifiers(), offsetType, new ConstantExpr( Constant::from_ulong( baseMembers.size() ) ), false, false ),
							new ListInit( inits ) );
					ret = new VariableExpr( offsetArray );
				}
			}

			delete offsetPackExpr;
			return ret;
		}

		void PolyGenericCalculator::beginScope() {
			knownLayouts.beginScope();
			knownOffsets.beginScope();
		}

		void PolyGenericCalculator::endScope() {
			knownLayouts.endScope();
			knownOffsets.endScope();
		}

////////////////////////////////////////// Pass3 ////////////////////////////////////////////////////

		template< typename DeclClass >
		void Pass3::handleDecl( DeclClass * decl, Type * type ) {
			GuardScope( scopeTyVars );
			makeTyVarMap( type, scopeTyVars );
			ScrubTyVars::scrubAll( decl );
		}

		void Pass3::premutate( ObjectDecl * objectDecl ) {
			handleDecl( objectDecl, objectDecl->type );
		}

		void Pass3::premutate( FunctionDecl * functionDecl ) {
			handleDecl( functionDecl, functionDecl->type );
		}

		void Pass3::premutate( TypedefDecl * typedefDecl ) {
			handleDecl( typedefDecl, typedefDecl->base );
		}

		/// Strips the members from a generic aggregate
		void stripGenericMembers(AggregateDecl * decl) {
			if ( ! decl->parameters.empty() ) decl->members.clear();
		}

		void Pass3::premutate( StructDecl * structDecl ) {
			stripGenericMembers( structDecl );
		}

		void Pass3::premutate( UnionDecl * unionDecl ) {
			stripGenericMembers( unionDecl );
		}

		void Pass3::premutate( TypeDecl * typeDecl ) {
			addToTyVarMap( typeDecl, scopeTyVars );
		}

		void Pass3::premutate( PointerType * pointerType ) {
			GuardScope( scopeTyVars );
			makeTyVarMap( pointerType, scopeTyVars );
		}

		void Pass3::premutate( FunctionType * functionType ) {
			GuardScope( scopeTyVars );
			makeTyVarMap( functionType, scopeTyVars );
		}
	} // anonymous namespace
} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //

