//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenInit.cc -- Generate initializers, and other stuff.
//
// Author           : Rob Schluntz
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Oct 25 13:53:00 2021
// Update Count     : 186
//
#include "GenInit.h"

#include <stddef.h>                    // for NULL
#include <algorithm>                   // for any_of
#include <cassert>                     // for assert, strict_dynamic_cast, assertf
#include <deque>
#include <iterator>                    // for back_inserter, inserter, back_inse...
#include <list>                        // for _List_iterator, list

#include "AST/Decl.hpp"
#include "AST/Init.hpp"
#include "AST/Pass.hpp"
#include "AST/Node.hpp"
#include "AST/Stmt.hpp"
#include "CompilationState.h"
#include "CodeGen/OperatorTable.h"
#include "Common/PassVisitor.h"        // for PassVisitor, WithGuards, WithShort...
#include "Common/SemanticError.h"      // for SemanticError
#include "Common/ToString.hpp"         // for toCString
#include "Common/UniqueName.h"         // for UniqueName
#include "Common/utility.h"            // for ValueGuard, maybeClone
#include "GenPoly/GenPoly.h"           // for getFunctionType, isPolyType
#include "GenPoly/ScopedSet.h"         // for ScopedSet, ScopedSet<>::const_iter...
#include "InitTweak.h"                 // for isConstExpr, InitExpander, checkIn...
#include "ResolvExpr/Resolver.h"
#include "SymTab/Autogen.h"            // for genImplicitCall
#include "SymTab/GenImplicitCall.hpp"  // for genImplicitCall
#include "SymTab/Mangler.h"            // for Mangler
#include "SynTree/LinkageSpec.h"       // for isOverridable, C
#include "SynTree/Declaration.h"       // for ObjectDecl, DeclarationWithType
#include "SynTree/Expression.h"        // for VariableExpr, UntypedExpr, Address...
#include "SynTree/Initializer.h"       // for ConstructorInit, SingleInit, Initi...
#include "SynTree/Label.h"             // for Label
#include "SynTree/Mutator.h"           // for mutateAll
#include "SynTree/Statement.h"         // for CompoundStmt, ImplicitCtorDtorStmt
#include "SynTree/Type.h"              // for Type, ArrayType, Type::Qualifiers
#include "SynTree/Visitor.h"           // for acceptAll, maybeAccept
#include "Tuples/Tuples.h"             // for maybeImpure
#include "Validate/FindSpecialDecls.h" // for SizeType

namespace InitTweak {
	namespace {
		const std::list<Label> noLabels;
		const std::list<Expression *> noDesignators;
	}

	struct ReturnFixer : public WithStmtsToAdd, public WithGuards {
		/// consistently allocates a temporary variable for the return value
		/// of a function so that anything which the resolver decides can be constructed
		/// into the return type of a function can be returned.
		static void makeReturnTemp( std::list< Declaration * > &translationUnit );

		void premutate( FunctionDecl *functionDecl );
		void premutate( ReturnStmt * returnStmt );

	  protected:
		FunctionType * ftype = nullptr;
		std::string funcName;
	};

	struct CtorDtor : public WithGuards, public WithShortCircuiting, public WithVisitorRef<CtorDtor>  {
		/// create constructor and destructor statements for object declarations.
		/// the actual call statements will be added in after the resolver has run
		/// so that the initializer expression is only removed if a constructor is found
		/// and the same destructor call is inserted in all of the appropriate locations.
		static void generateCtorDtor( std::list< Declaration * > &translationUnit );

		void previsit( ObjectDecl * );
		void previsit( FunctionDecl *functionDecl );

		// should not traverse into any of these declarations to find objects
		// that need to be constructed or destructed
		void previsit( StructDecl *aggregateDecl );
		void previsit( AggregateDecl * ) { visit_children = false; }
		void previsit( NamedTypeDecl * ) { visit_children = false; }
		void previsit( FunctionType * ) { visit_children = false; }

		void previsit( CompoundStmt * compoundStmt );

	  private:
		// set of mangled type names for which a constructor or destructor exists in the current scope.
		// these types require a ConstructorInit node to be generated, anything else is a POD type and thus
		// should not have a ConstructorInit generated.

		ManagedTypes managedTypes;
		bool inFunction = false;
	};

	struct HoistArrayDimension final : public WithDeclsToAdd, public WithShortCircuiting, public WithGuards, public WithIndexer {
		/// hoist dimension from array types in object declaration so that it uses a single
		/// const variable of type size_t, so that side effecting array dimensions are only
		/// computed once.
		static void hoistArrayDimension( std::list< Declaration * > & translationUnit );

		void premutate( ObjectDecl * objectDecl );
		DeclarationWithType * postmutate( ObjectDecl * objectDecl );
		void premutate( FunctionDecl *functionDecl );
		// should not traverse into any of these declarations to find objects
		// that need to be constructed or destructed
		void premutate( AggregateDecl * ) { visit_children = false; }
		void premutate( NamedTypeDecl * ) { visit_children = false; }
		void premutate( FunctionType * ) { visit_children = false; }

		// need this so that enumerators are added to the indexer, due to premutate(AggregateDecl *)
		void premutate( EnumDecl * ) {}

		void hoist( Type * type );

		Type::StorageClasses storageClasses;
		bool inFunction = false;
	};

	struct HoistArrayDimension_NoResolve final : public WithDeclsToAdd, public WithShortCircuiting, public WithGuards {
		/// hoist dimension from array types in object declaration so that it uses a single
		/// const variable of type size_t, so that side effecting array dimensions are only
		/// computed once.
		static void hoistArrayDimension( std::list< Declaration * > & translationUnit );

		void premutate( ObjectDecl * objectDecl );
		DeclarationWithType * postmutate( ObjectDecl * objectDecl );
		void premutate( FunctionDecl *functionDecl );
		// should not traverse into any of these declarations to find objects
		// that need to be constructed or destructed
		void premutate( AggregateDecl * ) { visit_children = false; }
		void premutate( NamedTypeDecl * ) { visit_children = false; }
		void premutate( FunctionType * ) { visit_children = false; }

		void hoist( Type * type );

		Type::StorageClasses storageClasses;
		bool inFunction = false;
	};

	void genInit( std::list< Declaration * > & translationUnit ) {
		if (!useNewAST) {
			HoistArrayDimension::hoistArrayDimension( translationUnit );
		}
		else {
			HoistArrayDimension_NoResolve::hoistArrayDimension( translationUnit );
		}
		fixReturnStatements( translationUnit );

		if (!useNewAST) {
			CtorDtor::generateCtorDtor( translationUnit );
		}
	}

	void fixReturnStatements( std::list< Declaration * > & translationUnit ) {
		PassVisitor<ReturnFixer> fixer;
		mutateAll( translationUnit, fixer );
	}

	void ReturnFixer::premutate( ReturnStmt *returnStmt ) {
		std::list< DeclarationWithType * > & returnVals = ftype->get_returnVals();
		assert( returnVals.size() == 0 || returnVals.size() == 1 );
		// hands off if the function returns a reference - we don't want to allocate a temporary if a variable's address
		// is being returned
		if ( returnStmt->expr && returnVals.size() == 1 && isConstructable( returnVals.front()->get_type() ) ) {
			// explicitly construct the return value using the return expression and the retVal object
			assertf( returnVals.front()->name != "", "Function %s has unnamed return value\n", funcName.c_str() );

			ObjectDecl * retVal = strict_dynamic_cast< ObjectDecl * >( returnVals.front() );
			if ( VariableExpr * varExpr = dynamic_cast< VariableExpr * >( returnStmt->expr ) ) {
				// return statement has already been mutated - don't need to do it again
				if ( varExpr->var == retVal ) return;
			}
			Statement * stmt = genCtorDtor( "?{}", retVal, returnStmt->expr );
			assertf( stmt, "ReturnFixer: genCtorDtor returned nullptr: %s / %s", toString( retVal ).c_str(), toString( returnStmt->expr ).c_str() );
			stmtsToAddBefore.push_back( stmt );

			// return the retVal object
			returnStmt->expr = new VariableExpr( returnVals.front() );
		} // if
	}

	void ReturnFixer::premutate( FunctionDecl *functionDecl ) {
		GuardValue( ftype );
		GuardValue( funcName );

		ftype = functionDecl->type;
		funcName = functionDecl->name;
	}

	// precompute array dimension expression, because constructor generation may duplicate it,
	// which would be incorrect if it is a side-effecting computation.
	void HoistArrayDimension::hoistArrayDimension( std::list< Declaration * > & translationUnit ) {
		PassVisitor<HoistArrayDimension> hoister;
		mutateAll( translationUnit, hoister );
	}

	void HoistArrayDimension::premutate( ObjectDecl * objectDecl ) {
		GuardValue( storageClasses );
		storageClasses = objectDecl->get_storageClasses();
	}

	DeclarationWithType * HoistArrayDimension::postmutate( ObjectDecl * objectDecl ) {
		hoist( objectDecl->get_type() );
		return objectDecl;
	}

	void HoistArrayDimension::hoist( Type * type ) {
		// if in function, generate const size_t var
		static UniqueName dimensionName( "_array_dim" );

		// C doesn't allow variable sized arrays at global scope or for static variables, so don't hoist dimension.
		if ( ! inFunction ) return;
		if ( storageClasses.is_static ) return;

		if ( ArrayType * arrayType = dynamic_cast< ArrayType * >( type ) ) {
			if ( ! arrayType->get_dimension() ) return; // xxx - recursive call to hoist?

			// need to resolve array dimensions in order to accurately determine if constexpr
			ResolvExpr::findSingleExpression( arrayType->dimension, Validate::SizeType->clone(), indexer );
			// array is variable-length when the dimension is not constexpr
			arrayType->isVarLen = ! isConstExpr( arrayType->dimension );
			// don't need to hoist dimension if it's definitely pure - only need to if there's potential for side effects.
			// xxx - hoisting has no side effects anyways, so don't skip since we delay resolve
			// still try to detect constant expressions
			if ( ! Tuples::maybeImpure( arrayType->dimension ) ) return;

			ObjectDecl * arrayDimension = new ObjectDecl( dimensionName.newName(), storageClasses, LinkageSpec::C, 0, Validate::SizeType->clone(), new SingleInit( arrayType->get_dimension() ) );
			arrayDimension->get_type()->set_const( true );

			arrayType->set_dimension( new VariableExpr( arrayDimension ) );
			declsToAddBefore.push_back( arrayDimension );

			hoist( arrayType->get_base() );
			return;
		}
	}

	void HoistArrayDimension::premutate( FunctionDecl * ) {
		GuardValue( inFunction );
		inFunction = true;
	}

	// precompute array dimension expression, because constructor generation may duplicate it,
	// which would be incorrect if it is a side-effecting computation.
	void HoistArrayDimension_NoResolve::hoistArrayDimension( std::list< Declaration * > & translationUnit ) {
		PassVisitor<HoistArrayDimension_NoResolve> hoister;
		mutateAll( translationUnit, hoister );
	}

	void HoistArrayDimension_NoResolve::premutate( ObjectDecl * objectDecl ) {
		GuardValue( storageClasses );
		storageClasses = objectDecl->get_storageClasses();
	}

	DeclarationWithType * HoistArrayDimension_NoResolve::postmutate( ObjectDecl * objectDecl ) {
		hoist( objectDecl->get_type() );
		return objectDecl;
	}

	void HoistArrayDimension_NoResolve::hoist( Type * type ) {
		// if in function, generate const size_t var
		static UniqueName dimensionName( "_array_dim" );

		// C doesn't allow variable sized arrays at global scope or for static variables, so don't hoist dimension.
		if ( ! inFunction ) return;
		if ( storageClasses.is_static ) return;

		if ( ArrayType * arrayType = dynamic_cast< ArrayType * >( type ) ) {
			if ( ! arrayType->get_dimension() ) return; // xxx - recursive call to hoist?
			// don't need to hoist dimension if it's definitely pure - only need to if there's potential for side effects.
			// xxx - hoisting has no side effects anyways, so don't skip since we delay resolve
			// still try to detect constant expressions
			if ( ! Tuples::maybeImpure( arrayType->dimension ) ) return;

			ObjectDecl * arrayDimension = new ObjectDecl( dimensionName.newName(), storageClasses, LinkageSpec::C, 0, Validate::SizeType->clone(), new SingleInit( arrayType->get_dimension() ) );
			arrayDimension->get_type()->set_const( true );

			arrayType->set_dimension( new VariableExpr( arrayDimension ) );
			declsToAddBefore.push_back( arrayDimension );

			hoist( arrayType->get_base() );
			return;
		}
	}

	void HoistArrayDimension_NoResolve::premutate( FunctionDecl * ) {
		GuardValue( inFunction );
		inFunction = true;
	}

namespace {

#	warning Remove the _New suffix after the conversion is complete.
	struct HoistArrayDimension_NoResolve_New final :
			public ast::WithDeclsToAdd<>, public ast::WithShortCircuiting,
			public ast::WithGuards, public ast::WithConstTranslationUnit,
			public ast::WithVisitorRef<HoistArrayDimension_NoResolve_New> {
		void previsit( const ast::ObjectDecl * decl );
		const ast::DeclWithType * postvisit( const ast::ObjectDecl * decl );
		// Do not look for objects inside there declarations (and type).
		void previsit( const ast::AggregateDecl * ) { visit_children = false; }
		void previsit( const ast::NamedTypeDecl * ) { visit_children = false; }
		void previsit( const ast::FunctionType * ) { visit_children = false; }

		const ast::Type * hoist( const ast::Type * type );

		ast::Storage::Classes storageClasses;
	};

	void HoistArrayDimension_NoResolve_New::previsit(
			const ast::ObjectDecl * decl ) {
		GuardValue( storageClasses ) = decl->storage;
	}

	const ast::DeclWithType * HoistArrayDimension_NoResolve_New::postvisit(
			const ast::ObjectDecl * objectDecl ) {
		return mutate_field( objectDecl, &ast::ObjectDecl::type,
				hoist( objectDecl->type ) );
	}

	const ast::Type * HoistArrayDimension_NoResolve_New::hoist(
			const ast::Type * type ) {
		static UniqueName dimensionName( "_array_dim" );

		if ( !isInFunction() || storageClasses.is_static ) {
			return type;
		}

		if ( auto arrayType = dynamic_cast< const ast::ArrayType * >( type ) ) {
			if ( nullptr == arrayType->dimension ) {
				return type;
			}

			if ( !Tuples::maybeImpure( arrayType->dimension ) ) {
				return type;
			}

			ast::ptr<ast::Type> dimType = transUnit().global.sizeType;
			assert( dimType );
			add_qualifiers( dimType, ast::CV::Qualifiers( ast::CV::Const ) );

			ast::ObjectDecl * arrayDimension = new ast::ObjectDecl(
				arrayType->dimension->location,
				dimensionName.newName(),
				dimType,
				new ast::SingleInit(
					arrayType->dimension->location,
					arrayType->dimension
				)
			);

			ast::ArrayType * mutType = ast::mutate( arrayType );
			mutType->dimension = new ast::VariableExpr(
					arrayDimension->location, arrayDimension );
			declsToAddBefore.push_back( arrayDimension );

			mutType->base = hoist( mutType->base );
			return mutType;
		}
		return type;
	}

	struct ReturnFixer_New final :
			public ast::WithStmtsToAdd<>, ast::WithGuards, ast::WithShortCircuiting {
		void previsit( const ast::FunctionDecl * decl );
		const ast::ReturnStmt * previsit( const ast::ReturnStmt * stmt );
	private:
		const ast::FunctionDecl * funcDecl = nullptr;
	};

	void ReturnFixer_New::previsit( const ast::FunctionDecl * decl ) {
		if (decl->linkage == ast::Linkage::Intrinsic) visit_children = false;
		GuardValue( funcDecl ) = decl;
	}

	const ast::ReturnStmt * ReturnFixer_New::previsit(
			const ast::ReturnStmt * stmt ) {
		auto & returns = funcDecl->returns;
		assert( returns.size() < 2 );
		// Hands off if the function returns a reference.
		// Don't allocate a temporary if the address is returned.
		if ( stmt->expr && 1 == returns.size() ) {
			ast::ptr<ast::DeclWithType> retDecl = returns.front();
			if ( isConstructable( retDecl->get_type() ) ) {
				// Explicitly construct the return value using the return
				// expression and the retVal object.
				assertf( "" != retDecl->name,
					"Function %s has unnamed return value.\n",
					funcDecl->name.c_str() );

				auto retVal = retDecl.strict_as<ast::ObjectDecl>();
				if ( auto varExpr = stmt->expr.as<ast::VariableExpr>() ) {
					// Check if the return statement is already set up.
					if ( varExpr->var == retVal ) return stmt;
				}
				ast::ptr<ast::Stmt> ctorStmt = genCtorDtor(
					retVal->location, "?{}", retVal, stmt->expr );
				assertf( ctorStmt,
					"ReturnFixer: genCtorDtor returned nullptr: %s / %s",
					toString( retVal ).c_str(),
					toString( stmt->expr ).c_str() );
				stmtsToAddBefore.push_back( ctorStmt );

				// Return the retVal object.
				ast::ReturnStmt * mutStmt = ast::mutate( stmt );
				mutStmt->expr = new ast::VariableExpr(
					stmt->location, retDecl );
				return mutStmt;
			}
		}
		return stmt;
	}

} // namespace

	void genInit( ast::TranslationUnit & transUnit ) {
		ast::Pass<HoistArrayDimension_NoResolve_New>::run( transUnit );
		ast::Pass<ReturnFixer_New>::run( transUnit );
	}

	void fixReturnStatements( ast::TranslationUnit & transUnit ) {
		ast::Pass<ReturnFixer_New>::run( transUnit );
	}

	void CtorDtor::generateCtorDtor( std::list< Declaration * > & translationUnit ) {
		PassVisitor<CtorDtor> ctordtor;
		acceptAll( translationUnit, ctordtor );
	}

	bool ManagedTypes::isManaged( Type * type ) const {
		// references are never constructed
		if ( dynamic_cast< ReferenceType * >( type ) ) return false;
		// need to clear and reset qualifiers when determining if a type is managed
		ValueGuard< Type::Qualifiers > qualifiers( type->get_qualifiers() );
		type->get_qualifiers() = Type::Qualifiers();
		if ( TupleType * tupleType = dynamic_cast< TupleType * > ( type ) ) {
			// tuple is also managed if any of its components are managed
			if ( std::any_of( tupleType->types.begin(), tupleType->types.end(), [&](Type * type) { return isManaged( type ); }) ) {
				return true;
			}
		}
		// a type is managed if it appears in the map of known managed types, or if it contains any polymorphism (is a type variable or generic type containing a type variable)
		return managedTypes.find( SymTab::Mangler::mangleConcrete( type ) ) != managedTypes.end() || GenPoly::isPolyType( type );
	}

	bool ManagedTypes::isManaged( ObjectDecl * objDecl ) const {
		Type * type = objDecl->get_type();
		while ( ArrayType * at = dynamic_cast< ArrayType * >( type ) ) {
			// must always construct VLAs with an initializer, since this is an error in C
			if ( at->isVarLen && objDecl->init ) return true;
			type = at->get_base();
		}
		return isManaged( type );
	}

	// why is this not just on FunctionDecl?
	void ManagedTypes::handleDWT( DeclarationWithType * dwt ) {
		// if this function is a user-defined constructor or destructor, mark down the type as "managed"
		if ( ! LinkageSpec::isOverridable( dwt->get_linkage() ) && CodeGen::isCtorDtor( dwt->get_name() ) ) {
			std::list< DeclarationWithType * > & params = GenPoly::getFunctionType( dwt->get_type() )->get_parameters();
			assert( ! params.empty() );
			Type * type = InitTweak::getPointerBase( params.front()->get_type() );
			assert( type );
			managedTypes.insert( SymTab::Mangler::mangleConcrete( type ) );
		}
	}

	void ManagedTypes::handleStruct( StructDecl * aggregateDecl ) {
		// don't construct members, but need to take note if there is a managed member,
		// because that means that this type is also managed
		for ( Declaration * member : aggregateDecl->get_members() ) {
			if ( ObjectDecl * field = dynamic_cast< ObjectDecl * >( member ) ) {
				if ( isManaged( field ) ) {
					// generic parameters should not play a role in determining whether a generic type is constructed - construct all generic types, so that
					// polymorphic constructors make generic types managed types
					StructInstType inst( Type::Qualifiers(), aggregateDecl );
					managedTypes.insert( SymTab::Mangler::mangleConcrete( &inst ) );
					break;
				}
			}
		}
	}

	void ManagedTypes::beginScope() { managedTypes.beginScope(); }
	void ManagedTypes::endScope() { managedTypes.endScope(); }

	bool ManagedTypes_new::isManaged( const ast::Type * type ) const {
		// references are never constructed
		if ( dynamic_cast< const ast::ReferenceType * >( type ) ) return false;
		if ( auto tupleType = dynamic_cast< const ast::TupleType * > ( type ) ) {
			// tuple is also managed if any of its components are managed
			for (auto & component : tupleType->types) {
				if (isManaged(component)) return true;
			}
		}
		// need to clear and reset qualifiers when determining if a type is managed
		// ValueGuard< Type::Qualifiers > qualifiers( type->get_qualifiers() );
		auto tmp = shallowCopy(type);
		tmp->qualifiers = {};
		// delete tmp at return
		ast::ptr<ast::Type> guard = tmp;
		// a type is managed if it appears in the map of known managed types, or if it contains any polymorphism (is a type variable or generic type containing a type variable)
		return managedTypes.find( Mangle::mangle( tmp, {Mangle::NoOverrideable | Mangle::NoGenericParams | Mangle::Type} ) ) != managedTypes.end() || GenPoly::isPolyType( tmp );
	}

	bool ManagedTypes_new::isManaged( const ast::ObjectDecl * objDecl ) const {
		const ast::Type * type = objDecl->type;
		while ( auto at = dynamic_cast< const ast::ArrayType * >( type ) ) {
			// must always construct VLAs with an initializer, since this is an error in C
			if ( at->isVarLen && objDecl->init ) return true;
			type = at->base;
		}
		return isManaged( type );
	}

	void ManagedTypes_new::handleDWT( const ast::DeclWithType * dwt ) {
		// if this function is a user-defined constructor or destructor, mark down the type as "managed"
		if ( ! dwt->linkage.is_overrideable && CodeGen::isCtorDtor( dwt->name ) ) {
			auto & params = GenPoly::getFunctionType( dwt->get_type())->params;
			assert( ! params.empty() );
			// Type * type = InitTweak::getPointerBase( params.front() );
			// assert( type );
			managedTypes.insert( Mangle::mangle( params.front(), {Mangle::NoOverrideable | Mangle::NoGenericParams | Mangle::Type} ) );
		}
	}

	void ManagedTypes_new::handleStruct( const ast::StructDecl * aggregateDecl ) {
		// don't construct members, but need to take note if there is a managed member,
		// because that means that this type is also managed
		for ( auto & member : aggregateDecl->members ) {
			if ( auto field = member.as<ast::ObjectDecl>() ) {
				if ( isManaged( field ) ) {
					// generic parameters should not play a role in determining whether a generic type is constructed - construct all generic types, so that
					// polymorphic constructors make generic types managed types
					ast::StructInstType inst( aggregateDecl );
					managedTypes.insert( Mangle::mangle( &inst, {Mangle::NoOverrideable | Mangle::NoGenericParams | Mangle::Type} ) );
					break;
				}
			}
		}
	}

	void ManagedTypes_new::beginScope() { managedTypes.beginScope(); }
	void ManagedTypes_new::endScope() { managedTypes.endScope(); }

	ImplicitCtorDtorStmt * genCtorDtor( const std::string & fname, ObjectDecl * objDecl, Expression * arg ) {
		// call into genImplicitCall from Autogen.h to generate calls to ctor/dtor
		assertf( objDecl, "genCtorDtor passed null objDecl" );
		std::list< Statement * > stmts;
		InitExpander_old srcParam( maybeClone( arg ) );
		SymTab::genImplicitCall( srcParam, new VariableExpr( objDecl ), fname, back_inserter( stmts ), objDecl );
		assert( stmts.size() <= 1 );
		return stmts.size() == 1 ? strict_dynamic_cast< ImplicitCtorDtorStmt * >( stmts.front() ) : nullptr;

	}

	ast::ptr<ast::Stmt> genCtorDtor (const CodeLocation & loc, const std::string & fname, const ast::ObjectDecl * objDecl, const ast::Expr * arg) {
		assertf(objDecl, "genCtorDtor passed null objDecl");
		InitExpander_new srcParam(arg);
		return SymTab::genImplicitCall(srcParam, new ast::VariableExpr(loc, objDecl), loc, fname, objDecl);
	}

	ConstructorInit * genCtorInit( ObjectDecl * objDecl ) {
		// call into genImplicitCall from Autogen.h to generate calls to ctor/dtor
		// for each constructable object
		std::list< Statement * > ctor;
		std::list< Statement * > dtor;

		InitExpander_old srcParam( objDecl->get_init() );
		InitExpander_old nullParam( (Initializer *)NULL );
		SymTab::genImplicitCall( srcParam, new VariableExpr( objDecl ), "?{}", back_inserter( ctor ), objDecl );
		SymTab::genImplicitCall( nullParam, new VariableExpr( objDecl ), "^?{}", front_inserter( dtor ), objDecl, false );

		// Currently genImplicitCall produces a single Statement - a CompoundStmt
		// which  wraps everything that needs to happen. As such, it's technically
		// possible to use a Statement ** in the above calls, but this is inherently
		// unsafe, so instead we take the slightly less efficient route, but will be
		// immediately informed if somehow the above assumption is broken. In this case,
		// we could always wrap the list of statements at this point with a CompoundStmt,
		// but it seems reasonable at the moment for this to be done by genImplicitCall
		// itself. It is possible that genImplicitCall produces no statements (e.g. if
		// an array type does not have a dimension). In this case, it's fine to ignore
		// the object for the purposes of construction.
		assert( ctor.size() == dtor.size() && ctor.size() <= 1 );
		if ( ctor.size() == 1 ) {
			// need to remember init expression, in case no ctors exist
			// if ctor does exist, want to use ctor expression instead of init
			// push this decision to the resolver
			assert( dynamic_cast< ImplicitCtorDtorStmt * > ( ctor.front() ) && dynamic_cast< ImplicitCtorDtorStmt * > ( dtor.front() ) );
			return new ConstructorInit( ctor.front(), dtor.front(), objDecl->get_init() );
		}
		return nullptr;
	}

	void CtorDtor::previsit( ObjectDecl * objDecl ) {
		managedTypes.handleDWT( objDecl );
		// hands off if @=, extern, builtin, etc.
		// even if unmanaged, try to construct global or static if initializer is not constexpr, since this is not legal C
		if ( tryConstruct( objDecl ) && ( managedTypes.isManaged( objDecl ) || ((! inFunction || objDecl->get_storageClasses().is_static ) && ! isConstExpr( objDecl->get_init() ) ) ) ) {
			// constructed objects cannot be designated
			if ( isDesignated( objDecl->get_init() ) ) SemanticError( objDecl, "Cannot include designations in the initializer for a managed Object. If this is really what you want, then initialize with @=.\n" );
			// constructed objects should not have initializers nested too deeply
			if ( ! checkInitDepth( objDecl ) ) SemanticError( objDecl, "Managed object's initializer is too deep " );

			objDecl->set_init( genCtorInit( objDecl ) );
		}
	}

	void CtorDtor::previsit( FunctionDecl *functionDecl ) {
		visit_children = false;  // do not try and construct parameters or forall parameters
		GuardValue( inFunction );
		inFunction = true;

		managedTypes.handleDWT( functionDecl );

		GuardScope( managedTypes );
		// go through assertions and recursively add seen ctor/dtors
		for ( auto & tyDecl : functionDecl->get_functionType()->get_forall() ) {
			for ( DeclarationWithType *& assertion : tyDecl->get_assertions() ) {
				managedTypes.handleDWT( assertion );
			}
		}

		maybeAccept( functionDecl->get_statements(), *visitor );
	}

	void CtorDtor::previsit( StructDecl *aggregateDecl ) {
		visit_children = false; // do not try to construct and destruct aggregate members

		managedTypes.handleStruct( aggregateDecl );
	}

	void CtorDtor::previsit( CompoundStmt * ) {
		GuardScope( managedTypes );
	}

ast::ConstructorInit * genCtorInit( const CodeLocation & loc, const ast::ObjectDecl * objDecl ) {
	// call into genImplicitCall from Autogen.h to generate calls to ctor/dtor for each
	// constructable object
	InitExpander_new srcParam{ objDecl->init }, nullParam{ (const ast::Init *)nullptr };
	ast::ptr< ast::Expr > dstParam = new ast::VariableExpr(loc, objDecl);

	ast::ptr< ast::Stmt > ctor = SymTab::genImplicitCall(
		srcParam, dstParam, loc, "?{}", objDecl );
	ast::ptr< ast::Stmt > dtor = SymTab::genImplicitCall(
		nullParam, dstParam, loc, "^?{}", objDecl,
		SymTab::LoopBackward );

	// check that either both ctor and dtor are present, or neither
	assert( (bool)ctor == (bool)dtor );

	if ( ctor ) {
		// need to remember init expression, in case no ctors exist. If ctor does exist, want to
		// use ctor expression instead of init.
		ctor.strict_as< ast::ImplicitCtorDtorStmt >();
		dtor.strict_as< ast::ImplicitCtorDtorStmt >();

		return new ast::ConstructorInit{ loc, ctor, dtor, objDecl->init };
	}

	return nullptr;
}

} // namespace InitTweak

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
