//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Box.cpp -- Implement polymorphic function calls and types.
//
// Author           : Andrew Beach
// Created On       : Thr Oct  6 13:39:00 2022
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Dec 14 17:42:17 2023
// Update Count     : 7
//

#include "Box.hpp"

#include "AST/Decl.hpp"                // for Decl, FunctionDecl, ...
#include "AST/Expr.hpp"                // for AlignofExpr, ConstantExpr, ...
#include "AST/Init.hpp"                // for Init, SingleInit
#include "AST/Inspect.hpp"             // for getFunctionName
#include "AST/Pass.hpp"                // for Pass, WithDeclsToAdd, ...
#include "AST/Stmt.hpp"                // for CompoundStmt, ExprStmt, ...
#include "AST/Vector.hpp"              // for vector
#include "AST/GenericSubstitution.hpp" // for genericSubstitution
#include "CodeGen/OperatorTable.hpp"   // for isAssignment
#include "Common/Iterate.hpp"          // for group_iterate
#include "Common/ScopedMap.hpp"        // for ScopedMap
#include "Common/ToString.hpp"         // for toCString
#include "Common/UniqueName.hpp"       // for UniqueName
#include "GenPoly/FindFunction.hpp"    // for findFunction
#include "GenPoly/GenPoly.hpp"         // for getFunctionType, ...
#include "GenPoly/Lvalue.hpp"          // for generalizedLvalue
#include "GenPoly/ScopedSet.hpp"       // for ScopedSet
#include "GenPoly/ScrubTypeVars.hpp"   // for scrubTypeVars, scrubAllTypeVars
#include "ResolvExpr/Unify.hpp"        // for typesCompatible
#include "SymTab/Mangler.hpp"          // for mangle, mangleType

namespace GenPoly {

namespace {

/// The layout type is used to represent sizes, alignments and offsets.
ast::BasicType * makeLayoutType() {
	return new ast::BasicType( ast::BasicKind::LongUnsignedInt );
}

/// Fixed version of layout type (just adding a 'C' in C++ style).
ast::BasicType * makeLayoutCType() {
	return new ast::BasicType( ast::BasicKind::LongUnsignedInt,
		ast::CV::Qualifiers( ast::CV::Const ) );
}

// --------------------------------------------------------------------------
/// Adds layout-generation functions to polymorphic types.
struct LayoutFunctionBuilder final :
		public ast::WithDeclsToAdd<>,
		public ast::WithShortCircuiting,
		public ast::WithVisitorRef<LayoutFunctionBuilder> {
	void previsit( ast::StructDecl const * decl );
	void previsit( ast::UnionDecl const * decl );
};

/// Get all sized type declarations; those that affect a layout function.
ast::vector<ast::TypeDecl> takeSizedParams(
		ast::vector<ast::TypeDecl> const & decls ) {
	ast::vector<ast::TypeDecl> sizedParams;
	for ( ast::ptr<ast::TypeDecl> const & decl : decls ) {
		if ( decl->isComplete() ) {
			sizedParams.emplace_back( decl );
		}
	}
	return sizedParams;
}

/// Adds parameters for otype size and alignment to a function type.
void addSTypeParams(
		ast::vector<ast::DeclWithType> & params,
		ast::vector<ast::TypeDecl> const & sizedParams ) {
	for ( ast::ptr<ast::TypeDecl> const & sizedParam : sizedParams ) {
		ast::TypeInstType inst( sizedParam );
		std::string paramName = Mangle::mangleType( &inst );
		params.emplace_back( new ast::ObjectDecl(
			sizedParam->location,
			sizeofName( paramName ),
			makeLayoutCType()
		) );
		params.emplace_back( new ast::ObjectDecl(
			sizedParam->location,
			alignofName( paramName ),
			makeLayoutCType()
		) );
	}
}

ast::Type * makeLayoutOutType() {
	return new ast::PointerType( makeLayoutType() );
}

struct LayoutData {
	ast::FunctionDecl * function;
	ast::ObjectDecl * sizeofParam;
	ast::ObjectDecl * alignofParam;
	ast::ObjectDecl * offsetofParam;
};

LayoutData buildLayoutFunction(
		CodeLocation const & location, ast::AggregateDecl const * aggr,
		ast::vector<ast::TypeDecl> const & sizedParams,
		bool isInFunction, bool isStruct ) {
	ast::ObjectDecl * sizeParam = new ast::ObjectDecl(
		location,
		sizeofName( aggr->name ),
		makeLayoutOutType()
	);
	ast::ObjectDecl * alignParam = new ast::ObjectDecl(
		location,
		alignofName( aggr->name ),
		makeLayoutOutType()
	);
	ast::ObjectDecl * offsetParam = nullptr;
	ast::vector<ast::DeclWithType> params = { sizeParam, alignParam };
	if ( isStruct ) {
		offsetParam = new ast::ObjectDecl(
			location,
			offsetofName( aggr->name ),
			makeLayoutOutType()
		);
		params.push_back( offsetParam );
	}
	addSTypeParams( params, sizedParams );

	// Routines at global scope marked "static" to prevent multiple
	// definitions is separate translation units because each unit generates
	// copies of the default routines for each aggregate.
	ast::FunctionDecl * layoutDecl = new ast::FunctionDecl(
		location,
		layoutofName( aggr ),
		{}, // forall
		{}, // assertions
		std::move( params ),
		{}, // returns
		new ast::CompoundStmt( location ),
		isInFunction ? ast::Storage::Classes() : ast::Storage::Static,
		ast::Linkage::AutoGen,
		{}, // attrs
		ast::Function::Inline,
		ast::FixedArgs
	);
	layoutDecl->fixUniqueId();
	return LayoutData{ layoutDecl, sizeParam, alignParam, offsetParam };
}

/// Makes a binary operation.
ast::Expr * makeOp( CodeLocation const & location, std::string const & name,
		ast::Expr const * lhs, ast::Expr const * rhs ) {
	return new ast::UntypedExpr( location,
		new ast::NameExpr( location, name ), { lhs, rhs } );
}

/// Make a binary operation and wrap it in a statement.
ast::Stmt * makeOpStmt( CodeLocation const & location, std::string const & name,
		ast::Expr const * lhs, ast::Expr const * rhs ) {
	return new ast::ExprStmt( location, makeOp( location, name, lhs, rhs ) );
}

/// Returns the dereference of a local pointer variable.
ast::Expr * derefVar(
		CodeLocation const & location, ast::ObjectDecl const * var ) {
	return ast::UntypedExpr::createDeref( location,
		new ast::VariableExpr( location, var ) );
}

/// Makes an if-statement with a single-expression then and no else.
ast::Stmt * makeCond( CodeLocation const & location,
		ast::Expr const * cond, ast::Expr const * thenPart ) {
	return new ast::IfStmt( location,
		cond, new ast::ExprStmt( location, thenPart ), nullptr );
}

/// Makes a statement that aligns lhs to rhs (rhs should be an integer
/// power of two).
ast::Stmt * makeAlignTo( CodeLocation const & location,
		ast::Expr const * lhs, ast::Expr const * rhs ) {
	// Check that the lhs is zeroed out to the level of rhs.
	ast::Expr * ifCond = makeOp( location, "?&?", lhs,
		makeOp( location, "?-?", rhs,
				ast::ConstantExpr::from_ulong( location, 1 ) ) );
	// If not aligned, increment to alignment.
	ast::Expr * ifExpr = makeOp( location, "?+=?", ast::deepCopy( lhs ),
		makeOp( location, "?-?", ast::deepCopy( rhs ),
				ast::deepCopy( ifCond ) ) );
	return makeCond( location, ifCond, ifExpr );
}

/// Makes a statement that assigns rhs to lhs if lhs < rhs.
ast::Stmt * makeAssignMax( CodeLocation const & location,
		ast::Expr const * lhs, ast::Expr const * rhs ) {
	return makeCond( location,
		makeOp( location, "?<?", ast::deepCopy( lhs ), ast::deepCopy( rhs ) ),
		makeOp( location, "?=?", lhs, rhs ) );
}

void LayoutFunctionBuilder::previsit( ast::StructDecl const * decl ) {
	// Do not generate layout function for empty tag structures.
	visit_children = false;
	if ( decl->members.empty() ) return;

	// Get parameters that can change layout, exiting early if none.
	ast::vector<ast::TypeDecl> sizedParams =
		takeSizedParams( decl->params );
	if ( sizedParams.empty() ) return;

	CodeLocation const & location = decl->location;

	// Build layout function signature.
	LayoutData layout = buildLayoutFunction(
		location, decl, sizedParams, isInFunction(), true );
	ast::FunctionDecl * layoutDecl = layout.function;
	// Also return these or extract them from the parameter list?
	ast::ObjectDecl const * sizeofParam = layout.sizeofParam;
	ast::ObjectDecl const * alignofParam = layout.alignofParam;
	ast::ObjectDecl const * offsetofParam = layout.offsetofParam;
	assert( nullptr != layout.offsetofParam );

	// Calculate structure layout in function body.
	// Initialize size and alignment to 0 and 1
	// (Will have at least one member to update size).
	auto & kids = layoutDecl->stmts.get_and_mutate()->kids;
	kids.emplace_back( makeOpStmt( location, "?=?",
		derefVar( location, sizeofParam ),
		ast::ConstantExpr::from_ulong( location, 0 )
	) );
	kids.emplace_back( makeOpStmt( location, "?=?",
		derefVar( location, alignofParam ),
		ast::ConstantExpr::from_ulong( location, 1 )
	) );
	// TODO: Polymorphic types will be out of the struct declaration scope.
	// This breaks invariants until it is corrected later.
	for ( auto const & member : enumerate( decl->members ) ) {
		auto dwt = member.val.strict_as<ast::DeclWithType>();
		ast::Type const * memberType = dwt->get_type();

		if ( 0 < member.idx ) {
			// Make sure all later members have padding to align them.
			kids.emplace_back( makeAlignTo( location,
				derefVar( location, sizeofParam ),
				new ast::AlignofExpr( location, ast::deepCopy( memberType ) )
			) );
		}

		// Place current size in the current offset index.
		kids.emplace_back( makeOpStmt( location, "?=?",
			makeOp( location, "?[?]",
				new ast::VariableExpr( location, offsetofParam ),
				ast::ConstantExpr::from_ulong( location, member.idx ) ),
			derefVar( location, sizeofParam ) ) );

		// Add member size to current size.
		kids.emplace_back( makeOpStmt( location, "?+=?",
			derefVar( location, sizeofParam ),
			new ast::SizeofExpr( location, ast::deepCopy( memberType ) ) ) );

		// Take max of member alignment and global alignment.
		// (As align is always 2^n, this will always be a multiple of both.)
		kids.emplace_back( makeAssignMax( location,
			derefVar( location, alignofParam ),
			new ast::AlignofExpr( location, ast::deepCopy( memberType ) ) ) );
	}
	// Make sure the type is end-padded to a multiple of its alignment.
	kids.emplace_back( makeAlignTo( location,
		derefVar( location, sizeofParam ),
		derefVar( location, alignofParam ) ) );

	declsToAddAfter.emplace_back( layoutDecl );
}

void LayoutFunctionBuilder::previsit( ast::UnionDecl const * decl ) {
	visit_children = false;
	// Do not generate layout function for empty tag unions.
	if ( decl->members.empty() ) return;

	// Get parameters that can change layout, exiting early if none.
	ast::vector<ast::TypeDecl> sizedParams =
		takeSizedParams( decl->params );
	if ( sizedParams.empty() ) return;

	CodeLocation const & location = decl->location;

	// Build layout function signature.
	LayoutData layout = buildLayoutFunction(
		location, decl, sizedParams, isInFunction(), false );
	ast::FunctionDecl * layoutDecl = layout.function;
	// Also return these or extract them from the parameter list?
	ast::ObjectDecl const * sizeofParam = layout.sizeofParam;
	ast::ObjectDecl const * alignofParam = layout.alignofParam;
	assert( nullptr == layout.offsetofParam );

	// Calculate union layout in function body.
	// Both are simply the maximum for union (actually align is always the
	// LCM, but with powers of two that is also the maximum).
	auto & kids = layoutDecl->stmts.get_and_mutate()->kids;
	kids.emplace_back( makeOpStmt( location,
		"?=?", derefVar( location, sizeofParam ),
		ast::ConstantExpr::from_ulong( location, 1 )
	) );
	kids.emplace_back( makeOpStmt( location,
		"?=?", derefVar( location, alignofParam ),
		ast::ConstantExpr::from_ulong( location, 1 )
	) );
	// TODO: Polymorphic types will be out of the union declaration scope.
	// This breaks invariants until it is corrected later.
	for ( auto const & member : decl->members ) {
		auto dwt = member.strict_as<ast::DeclWithType>();
		ast::Type const * memberType = dwt->get_type();

		// Take max member size and global size.
		kids.emplace_back( makeAssignMax( location,
			derefVar( location, sizeofParam ),
			new ast::SizeofExpr( location, ast::deepCopy( memberType ) )
		) );

		// Take max of member alignment and global alignment.
		kids.emplace_back( makeAssignMax( location,
			derefVar( location, alignofParam ),
			new ast::AlignofExpr( location, ast::deepCopy( memberType ) )
		) );
	}
	kids.emplace_back( makeAlignTo( location,
		derefVar( location, sizeofParam ),
		derefVar( location, alignofParam ) ) );

	declsToAddAfter.emplace_back( layoutDecl );
}

// --------------------------------------------------------------------------
/// Application expression transformer.
/// * Replaces polymorphic return types with out-parameters.
/// * Replaces call to polymorphic functions with adapter calls which handles
///   dynamic arguments and return values.
/// * Adds appropriate type variables to the function calls.
struct CallAdapter final :
		public ast::WithConstTypeSubstitution,
		public ast::WithGuards,
		public ast::WithShortCircuiting,
		public ast::WithStmtsToAdd<>,
		public ast::WithVisitorRef<CallAdapter> {
	CallAdapter();

	void previsit( ast::Decl const * decl );
	ast::FunctionDecl const * previsit( ast::FunctionDecl const * decl );
	void previsit( ast::TypeDecl const * decl );
	void previsit( ast::CommaExpr const * expr );
	ast::Expr const * postvisit( ast::ApplicationExpr const * expr );
	ast::Expr const * postvisit( ast::UntypedExpr const * expr );
	void previsit( ast::AddressExpr const * expr );
	ast::Expr const * postvisit( ast::AddressExpr const * expr );
	ast::ReturnStmt const * previsit( ast::ReturnStmt const * stmt );

	void beginScope();
	void endScope();
private:
	// Many helpers here use a mutable ApplicationExpr as an in/out parameter
	// instead of using the return value, to save on mutates and free up the
	// return value.

	/// Passes extra layout arguments for sized polymorphic type parameters.
	void passTypeVars(
		ast::ApplicationExpr * expr,
		ast::vector<ast::Expr> & extraArgs,
		ast::FunctionType const * funcType );
	/// Wraps a function application with a new temporary for the
	/// out-parameter return value.
	ast::Expr const * addRetParam(
		ast::ApplicationExpr * expr, ast::Type const * retType );
	/// Wraps a function application returning a polymorphic type with a new
	/// temporary for the out-parameter return value.
	ast::Expr const * addDynRetParam(
		ast::ApplicationExpr * expr, ast::Type const * polyType );
	/// Modify a call so it passes the function through the correct adapter.
	ast::Expr const * applyAdapter(
		ast::ApplicationExpr * expr,
		ast::FunctionType const * function );
	/// Convert a single argument into its boxed form to pass the parameter.
	void boxParam( ast::ptr<ast::Expr> & arg,
		ast::Type const * formal, TypeVarMap const & exprTyVars );
	/// Box every argument from arg forward, matching the functionType
	/// parameter list. arg should point into expr's argument list.
	void boxParams(
		ast::ApplicationExpr * expr,
		ast::Type const * polyRetType,
		ast::FunctionType const * function,
		const TypeVarMap & typeVars );
	/// Adds the inferred parameters derived from the assertions of the
	/// expression to the call.
	void addInferredParams(
		ast::ApplicationExpr * expr,
		ast::vector<ast::Expr> & extraArgs,
		ast::FunctionType const * functionType,
		const TypeVarMap & typeVars );
	/// Stores assignment operators from assertion list in
	/// local map of assignment operations.
	void passAdapters(
		ast::ApplicationExpr * expr,
		ast::FunctionType const * type,
		const TypeVarMap & typeVars );
	/// Create an adapter function based on the type of the adaptee and the
	/// real type with the type substitutions applied.
	ast::FunctionDecl * makeAdapter(
		ast::FunctionType const * adaptee,
		ast::FunctionType const * realType,
		std::string const & mangleName,
		TypeVarMap const & typeVars,
		CodeLocation const & location ) const;
	/// Replaces intrinsic operator functions with their arithmetic desugaring.
	ast::Expr const * handleIntrinsics( ast::ApplicationExpr const * );
	/// Inserts a new temporary variable into the current scope with an
	/// auto-generated name.
	ast::ObjectDecl * makeTemporary(
		CodeLocation const & location, ast::Type const * type );

	TypeVarMap scopeTypeVars;
	ScopedMap< std::string, ast::DeclWithType const * > adapters;
	std::map< ast::ApplicationExpr const *, ast::Expr const * > retVals;
	ast::DeclWithType const * retval;
	UniqueName tmpNamer;
};

/// Replaces a polymorphic type with its concrete equivalant under the
/// current environment (returns itself if concrete).
/// If `doClone` is set to false, will not clone interior types
ast::Type const * replaceWithConcrete(
		ast::Type const * type,
		ast::TypeSubstitution const & typeSubs,
		bool doCopy = true );

/// Replaces all the type parameters of a generic type with their
/// concrete equivalents under the current environment.
void replaceParametersWithConcrete(
		ast::vector<ast::Expr> & params,
		ast::TypeSubstitution const & typeSubs ) {
	for ( ast::ptr<ast::Expr> & paramExpr : params ) {
		ast::TypeExpr const * param = paramExpr.as<ast::TypeExpr>();
		assertf( param, "Aggregate parameters should be type expressions." );
		paramExpr = ast::mutate_field( param, &ast::TypeExpr::type,
			replaceWithConcrete( param->type.get(), typeSubs, false ) );
	}
}

ast::Type const * replaceWithConcrete(
		ast::Type const * type,
		ast::TypeSubstitution const & typeSubs,
		bool doCopy ) {
	if ( auto instType = dynamic_cast<ast::TypeInstType const *>( type ) ) {
		ast::Type const * concrete = typeSubs.lookup( instType );
		return ( nullptr != concrete ) ? concrete : instType;
	} else if ( auto structType =
			dynamic_cast<ast::StructInstType const *>( type ) ) {
		ast::StructInstType * newType =
			doCopy ? ast::deepCopy( structType ) : ast::mutate( structType );
		replaceParametersWithConcrete( newType->params, typeSubs );
		return newType;
	} else if ( auto unionType =
			dynamic_cast<ast::UnionInstType const *>( type ) ) {
		ast::UnionInstType * newType =
			doCopy ? ast::deepCopy( unionType ) : ast::mutate( unionType );
		replaceParametersWithConcrete( newType->params, typeSubs );
		return newType;
	} else {
		return type;
	}
}

std::string makePolyMonoSuffix(
		ast::FunctionType const * function,
		TypeVarMap const & typeVars ) {
	// If the return type or a parameter type involved polymorphic types,
	// then the adapter will need to take those polymorphic types as pointers.
	// Therefore, there can be two different functions with the same mangled
	// name, so we need to further mangle the names.
	std::stringstream name;
	for ( auto ret : function->returns ) {
		name << ( isPolyType( ret, typeVars ) ? 'P' : 'M' );
	}
	name << '_';
	for ( auto arg : function->params ) {
		name << ( isPolyType( arg, typeVars ) ? 'P' : 'M' );
	}
	return name.str();
}

std::string mangleAdapterName(
		ast::FunctionType const * function,
		TypeVarMap const & typeVars ) {
	return Mangle::mangle( function, {} )
		+ makePolyMonoSuffix( function, typeVars );
}

std::string makeAdapterName( std::string const & mangleName ) {
	return "_adapter" + mangleName;
}

void makeRetParam( ast::FunctionType * type ) {
	ast::ptr<ast::Type> & retParam = type->returns.front();

	// Make a new parameter that is a pointer to the type of the old return value.
	retParam = new ast::PointerType( retParam.get() );
	type->params.emplace( type->params.begin(), retParam );

	// We don't need the return value any more.
	type->returns.clear();
}

ast::FunctionType * makeAdapterType(
		ast::FunctionType const * adaptee,
		TypeVarMap const & typeVars ) {
	ast::FunctionType * adapter = ast::deepCopy( adaptee );
	if ( isDynRet( adapter, typeVars ) ) {
		makeRetParam( adapter );
	}
	adapter->params.emplace( adapter->params.begin(),
		new ast::PointerType( new ast::FunctionType( ast::VariableArgs ) )
	);
	return adapter;
}

CallAdapter::CallAdapter() : tmpNamer( "_temp" ) {}

void CallAdapter::previsit( ast::Decl const * ) {
	// Prevent type declaration information from leaking out.
	GuardScope( scopeTypeVars );
}

ast::FunctionDecl const * CallAdapter::previsit( ast::FunctionDecl const * decl ) {
	// Prevent type declaration information from leaking out.
	GuardScope( scopeTypeVars );

	if ( nullptr == decl->stmts ) {
		return decl;
	}

	GuardValue( retval );

	// Process polymorphic return value.
	retval = nullptr;
	ast::FunctionType const * type = decl->type;
	if ( isDynRet( type ) && decl->linkage != ast::Linkage::C ) {
		retval = decl->returns.front();

		// Give names to unnamed return values.
		if ( "" == retval->name ) {
			auto mutRet = ast::mutate( retval );
			mutRet->name = "_retparam";
			mutRet->linkage = ast::Linkage::C;
			retval = mutRet;
			decl = ast::mutate_field_index( decl,
				&ast::FunctionDecl::returns, 0, mutRet );
		}
	}

	// The formal_usage/expr_id values may be off if we get them from the
	// type, trying the declaration instead.
	makeTypeVarMap( type, scopeTypeVars );

	// Get all needed adapters from the call. We will forward them.
	ast::vector<ast::FunctionType> functions;
	for ( ast::ptr<ast::VariableExpr> const & assertion : type->assertions ) {
		auto atype = assertion->result.get();
		findFunction( atype, functions, scopeTypeVars, needsAdapter );
	}

	for ( ast::ptr<ast::Type> const & arg : type->params ) {
		findFunction( arg, functions, scopeTypeVars, needsAdapter );
	}

	for ( auto funcType : functions ) {
		std::string mangleName = mangleAdapterName( funcType, scopeTypeVars );
		if ( adapters.contains( mangleName ) ) continue;
		std::string adapterName = makeAdapterName( mangleName );
		// NODE: This creates floating nodes, breaking invariants.
		// This is corrected in the RewireAdapters sub-pass.
		adapters.insert(
			mangleName,
			new ast::ObjectDecl(
				decl->location,
				adapterName,
				new ast::PointerType(
					makeAdapterType( funcType, scopeTypeVars ) ),
				nullptr, // init
				ast::Storage::Classes(),
				ast::Linkage::C
			)
		);
	}

	return decl;
}

void CallAdapter::previsit( ast::TypeDecl const * decl ) {
	addToTypeVarMap( decl, scopeTypeVars );
}

void CallAdapter::previsit( ast::CommaExpr const * expr ) {
	// Attempting to find application expressions that were mutated by the
	// copy constructor passes to use an explicit return variable, so that
	// the variable can be reused as a parameter to the call rather than
	// creating a new temporary variable. Previously this step was an
	// optimization, but with the introduction of tuples and UniqueExprs,
	// it is necessary to ensure that they use the same variable.
	// Essentially, looking for pattern:
	// (x=f(...), x)
	// To compound the issue, the right side can be *x, etc.
	// because of lvalue-returning functions
	if ( auto assign = expr->arg1.as<ast::UntypedExpr>() ) {
		if ( CodeGen::isAssignment( ast::getFunctionName( assign ) ) ) {
			assert( 2 == assign->args.size() );
			if ( auto app = assign->args.back().as<ast::ApplicationExpr>() ) {
				// First argument is assignable, so it must be an lvalue,
				// so it should be legal to takes its address.
				retVals.insert_or_assign( app, assign->args.front() );
			}
		}
	}
}

ast::Expr const * CallAdapter::postvisit( ast::ApplicationExpr const * expr ) {
	assert( expr->func->result );
	ast::FunctionType const * function = getFunctionType( expr->func->result );
	assertf( function, "ApplicationExpr has non-function type %s",
			toCString( expr->func->result ) );

	if ( auto newExpr = handleIntrinsics( expr ) ) {
		return newExpr;
	}

	ast::ApplicationExpr * mutExpr = ast::mutate( expr );
	ast::Expr const * ret = expr;

	TypeVarMap exprTypeVars;
	makeTypeVarMap( function, exprTypeVars );
	auto dynRetType = isDynRet( function, exprTypeVars );

	// NOTE: addDynRetParam needs to know the actual (generated) return type
	// so it can make a temporary variable, so pass the result type form the
	// `expr` `passTypeVars` needs to know the program-text return type ([ex]
	// the distinction between _conc_T30 and T3(int)) concRetType may not be
	// a good name in one or both of these places.
	if ( dynRetType ) {
		ast::Type const * result = mutExpr->result;
		ast::Type const * concRetType = result->isVoid() ? nullptr : result;
		// [Comment from before translation.]
		// Used to use dynRetType instead of concRetType.
		ret = addDynRetParam( mutExpr, concRetType );
	} else if ( needsAdapter( function, scopeTypeVars )
			&& !needsAdapter( function, exprTypeVars ) ) {
		// Change the application so it calls the adapter rather than the
		// passed function.
		ret = applyAdapter( mutExpr, function );
	}

	ast::vector<ast::Expr> prependArgs;
	passTypeVars( mutExpr, prependArgs, function );
	addInferredParams( mutExpr, prependArgs, function, exprTypeVars );

	boxParams( mutExpr, dynRetType, function, exprTypeVars );
	spliceBegin( mutExpr->args, prependArgs );
	passAdapters( mutExpr, function, exprTypeVars );

	return ret;
}

bool isPolyDeref( ast::UntypedExpr const * expr,
		TypeVarMap const & typeVars,
		ast::TypeSubstitution const * typeSubs ) {
	if ( expr->result && isPolyType( expr->result, typeVars, typeSubs ) ) {
		if ( auto name = expr->func.as<ast::NameExpr>() ) {
			if ( "*?" == name->name ) {
				return true;
			}
		}
	}
	return false;
}

ast::Expr const * CallAdapter::postvisit( ast::UntypedExpr const * expr ) {
	if ( isPolyDeref( expr, scopeTypeVars, typeSubs ) ) {
		return expr->args.front();
	}
	return expr;
}

void CallAdapter::previsit( ast::AddressExpr const * ) {
	visit_children = false;
}

ast::Expr const * CallAdapter::postvisit( ast::AddressExpr const * expr ) {
	assert( expr->arg->result );
	assert( !expr->arg->result->isVoid() );

	bool doesNeedAdapter = false;
	if ( auto un = expr->arg.as<ast::UntypedExpr>() ) {
		if ( isPolyDeref( un, scopeTypeVars, typeSubs ) ) {
			if ( auto app = un->args.front().as<ast::ApplicationExpr>() ) {
				assert( app->func->result );
				auto function = getFunctionType( app->func->result );
				assert( function );
				doesNeedAdapter = needsAdapter( function, scopeTypeVars );
			}
		}
	}
	// isPolyType check needs to happen before mutating expr arg,
	// so pull it forward out of the if condition.
	expr = ast::mutate_field( expr, &ast::AddressExpr::arg,
			expr->arg->accept( *visitor ) );
	// But must happen after mutate, since argument might change
	// (ex. intrinsic *?, ?[?]) re-evaluate above comment.
	bool polyType = isPolyType( expr->arg->result, scopeTypeVars, typeSubs );
	if ( polyType || doesNeedAdapter ) {
		ast::Expr * ret = ast::mutate( expr->arg.get() );
		ret->result = ast::deepCopy( expr->result );
		return ret;
	} else {
		return expr;
	}
}

ast::ReturnStmt const * CallAdapter::previsit( ast::ReturnStmt const * stmt ) {
	// Since retval is set when the return type is dynamic, this function
	// should have been converted to void return & out parameter.
	if ( retval && stmt->expr ) {
		assert( stmt->expr->result );
		assert( !stmt->expr->result->isVoid() );
		return ast::mutate_field( stmt, &ast::ReturnStmt::expr, nullptr );
	}
	return stmt;
}

void CallAdapter::beginScope() {
	adapters.beginScope();
}

void CallAdapter::endScope() {
	adapters.endScope();
}

/// Find instances of polymorphic type parameters.
struct PolyFinder {
	TypeVarMap const & typeVars;
	bool result = false;
	PolyFinder( TypeVarMap const & tvs ) : typeVars( tvs ) {}

	void previsit( ast::TypeInstType const * type ) {
		if ( isPolyType( type, typeVars ) ) result = true;
	}
};

/// True if these is an instance of a polymorphic type parameter in the type.
bool hasPolymorphism( ast::Type const * type, TypeVarMap const & typeVars ) {
	return ast::Pass<PolyFinder>::read( type, typeVars );
}

void CallAdapter::passTypeVars(
		ast::ApplicationExpr * expr,
		ast::vector<ast::Expr> & extraArgs,
		ast::FunctionType const * function ) {
	assert( typeSubs );
	// Pass size/align for type variables.
	for ( ast::ptr<ast::TypeInstType> const & typeVar : function->forall ) {
		if ( !typeVar->base->isComplete() ) continue;
		ast::Type const * concrete = typeSubs->lookup( typeVar );
		if ( !concrete ) {
			// Should this be an assertion?
			SemanticError( expr->location, "\nunbound type variable %s in application %s",
						   toString( typeSubs ).c_str(), typeVar->typeString().c_str() );
		}
		extraArgs.emplace_back(
			new ast::SizeofExpr( expr->location, ast::deepCopy( concrete ) ) );
		extraArgs.emplace_back(
			new ast::AlignofExpr( expr->location, ast::deepCopy( concrete ) ) );
	}
}

ast::Expr const * CallAdapter::addRetParam(
		ast::ApplicationExpr * expr, ast::Type const * retType ) {
	// Create temporary to hold return value of polymorphic function and
	// produce that temporary as a result using a comma expression.
	assert( retType );

	ast::Expr * paramExpr = nullptr;
	// Try to use existing return value parameter if it exists,
	// otherwise create a new temporary.
	if ( retVals.count( expr ) ) {
		paramExpr = ast::deepCopy( retVals[ expr ] );
	} else {
		auto newObj = makeTemporary( expr->location, ast::deepCopy( retType ) );
		paramExpr = new ast::VariableExpr( expr->location, newObj );
	}
	ast::Expr * retExpr = ast::deepCopy( paramExpr );

	// If the type of the temporary is not polpmorphic, box temporary by
	// taking its address; otherwise the temporary is already boxed and can
	// be used directly.
	if ( !isPolyType( paramExpr->result, scopeTypeVars, typeSubs ) ) {
		paramExpr = new ast::AddressExpr( paramExpr->location, paramExpr );
	}
	// Add argument to function call.
	expr->args.insert( expr->args.begin(), paramExpr );
	// Build a comma expression to call the function and return a value.
	ast::CommaExpr * comma = new ast::CommaExpr(
		expr->location, expr, retExpr );
	comma->env = expr->env;
	expr->env = nullptr;
	return comma;
}

ast::Expr const * CallAdapter::addDynRetParam(
		ast::ApplicationExpr * expr, ast::Type const * polyType ) {
	assert( typeSubs );
	ast::Type const * concrete = replaceWithConcrete( polyType, *typeSubs );
	// Add out-parameter for return value.
	return addRetParam( expr, concrete );
}

ast::Expr const * CallAdapter::applyAdapter(
		ast::ApplicationExpr * expr,
		ast::FunctionType const * function ) {
	ast::Expr const * ret = expr;
	if ( isDynRet( function, scopeTypeVars ) ) {
		ret = addRetParam( expr, function->returns.front() );
	}
	std::string mangleName = mangleAdapterName( function, scopeTypeVars );
	std::string adapterName = makeAdapterName( mangleName );

	// Cast adaptee to `void (*)()`, since it may have any type inside a
	// polymorphic function.
	ast::Type const * adapteeType = new ast::PointerType(
		new ast::FunctionType( ast::VariableArgs ) );
	expr->args.insert( expr->args.begin(),
		new ast::CastExpr( expr->location, expr->func, adapteeType ) );
	// The result field is never set on NameExpr. / Now it is.
	auto head = new ast::NameExpr( expr->location, adapterName );
	head->result = ast::deepCopy( adapteeType );
	expr->func = head;

	return ret;
}

/// Cast parameters to polymorphic functions so that types are replaced with
/// `void *` if they are type parameters in the formal type.
/// This gets rid of warnings from gcc.
void addCast(
		ast::ptr<ast::Expr> & actual,
		ast::Type const * formal,
		TypeVarMap const & typeVars ) {
	// Type contains polymorphism, but isn't exactly a polytype, in which
	// case it has some real actual type (ex. unsigned int) and casting to
	// `void *` is wrong.
	if ( hasPolymorphism( formal, typeVars )
			&& !isPolyType( formal, typeVars ) ) {
		ast::Type const * newType = ast::deepCopy( formal );
		newType = scrubTypeVars( newType, typeVars );
		actual = new ast::CastExpr( actual->location, actual, newType );
	}
}

void CallAdapter::boxParam( ast::ptr<ast::Expr> & arg,
		ast::Type const * param, TypeVarMap const & exprTypeVars ) {
	assertf( arg->result, "arg does not have result: %s", toCString( arg ) );
	addCast( arg, param, exprTypeVars );
	if ( !needsBoxing( param, arg->result, exprTypeVars, typeSubs ) ) {
		return;
	}
	CodeLocation const & location = arg->location;

	if ( arg->get_lvalue() ) {
		// The argument expression may be CFA lvalue, but not C lvalue,
		// so apply generalizedLvalue transformations.
		// if ( auto var = dynamic_cast<ast::VariableExpr const *>( arg ) ) {
		//  if ( dynamic_cast<ast::ArrayType const *>( varExpr->var->get_type() ) ){
		//      // temporary hack - don't box arrays, because &arr is not the same as &arr[0]
		//      return;
		//  }
		// }
		arg = generalizedLvalue( new ast::AddressExpr( arg->location, arg ) );
		if ( !ResolvExpr::typesCompatible( param, arg->result ) ) {
			// Silence warnings by casting boxed parameters when the actually
			// type does not match up with the formal type.
			arg = new ast::CastExpr( location, arg, ast::deepCopy( param ) );
		}
	} else {
		// Use type computed in unification to declare boxed variables.
		ast::ptr<ast::Type> newType = ast::deepCopy( param );
		if ( typeSubs ) typeSubs->apply( newType );
		ast::ObjectDecl * newObj = makeTemporary( location, newType );
		auto assign = ast::UntypedExpr::createCall( location, "?=?", {
			new ast::VariableExpr( location, newObj ),
			arg,
		} );
		stmtsToAddBefore.push_back( new ast::ExprStmt( location, assign ) );
		arg = new ast::AddressExpr(
			new ast::VariableExpr( location, newObj ) );
	}
}

void CallAdapter::boxParams(
		ast::ApplicationExpr * expr,
		ast::Type const * polyRetType,
		ast::FunctionType const * function,
		const TypeVarMap & typeVars ) {
	// Start at the beginning, but the return argument may have been added.
	auto arg = expr->args.begin();
	if ( polyRetType ) ++arg;

	for ( auto param : function->params ) {
		assertf( arg != expr->args.end(),
			"boxParams: missing argument for param %s to %s in %s",
			toCString( param ), toCString( function ), toCString( expr ) );
		boxParam( *arg, param, typeVars );
		++arg;
	}
}

void CallAdapter::addInferredParams(
		ast::ApplicationExpr * expr,
		ast::vector<ast::Expr> & extraArgs,
		ast::FunctionType const * functionType,
		TypeVarMap const & typeVars ) {
	for ( auto assertion : functionType->assertions ) {
		auto inferParam = expr->inferred.inferParams().find(
			assertion->var->uniqueId );
		assertf( inferParam != expr->inferred.inferParams().end(),
			"addInferredParams missing inferred parameter: %s in: %s",
			toCString( assertion ), toCString( expr ) );
		ast::ptr<ast::Expr> newExpr = ast::deepCopy( inferParam->second.expr );
		boxParam( newExpr, assertion->result, typeVars );
		extraArgs.emplace_back( newExpr.release() );
	}
}

/// Modifies the ApplicationExpr to accept adapter functions for its
/// assertion and parameters, declares the required adapters.
void CallAdapter::passAdapters(
		ast::ApplicationExpr * expr,
		ast::FunctionType const * type,
		const TypeVarMap & exprTypeVars ) {
	// Collect a list of function types passed as parameters or implicit
	// parameters (assertions).
	ast::vector<ast::Type> const & paramList = type->params;
	ast::vector<ast::FunctionType> functions;

	for ( ast::ptr<ast::VariableExpr> const & assertion : type->assertions ) {
		findFunction( assertion->result, functions, exprTypeVars, needsAdapter );
	}
	for ( ast::ptr<ast::Type> const & arg : paramList ) {
		findFunction( arg, functions, exprTypeVars, needsAdapter );
	}

	// Parameter function types for which an appropriate adapter has been
	// generated. We cannot use the types after applying substitutions,
	// since two different parameter types may be unified to the same type.
	std::set<std::string> adaptersDone;

	CodeLocation const & location = expr->location;

	for ( ast::ptr<ast::FunctionType> const & funcType : functions ) {
		std::string mangleName = Mangle::mangle( funcType );

		// Only attempt to create an adapter or pass one as a parameter if we
		// haven't already done so for this pre-substitution parameter
		// function type.
		// The second part of the result if is if the element was inserted.
		if ( !adaptersDone.insert( mangleName ).second ) continue;

		// Apply substitution to type variables to figure out what the
		// adapter's type should look like. (Copy to make the release safe.)
		assert( typeSubs );
		auto result = typeSubs->apply( ast::deepCopy( funcType ) );
		ast::FunctionType * realType = ast::mutate( result.node.release() );
		mangleName = Mangle::mangle( realType );
		mangleName += makePolyMonoSuffix( funcType, exprTypeVars );

		// Check if the adapter has already been created, or has to be.
		using AdapterIter = decltype(adapters)::iterator;
		AdapterIter adapter = adapters.find( mangleName );
		if ( adapter == adapters.end() ) {
			ast::FunctionDecl * newAdapter = makeAdapter(
				funcType, realType, mangleName, exprTypeVars, location );
			std::pair<AdapterIter, bool> answer =
				adapters.insert( mangleName, newAdapter );
			adapter = answer.first;
			stmtsToAddBefore.push_back(
				new ast::DeclStmt( location, newAdapter ) );
		}
		assert( adapter != adapters.end() );

		// Add the approprate adapter as a parameter.
		expr->args.insert( expr->args.begin(),
			new ast::VariableExpr( location, adapter->second ) );
	}
}

// Parameter and argument may be used wrong around here.
ast::Expr * makeAdapterArg(
		ast::DeclWithType const * param,
		ast::Type const * arg,
		ast::Type const * realParam,
		TypeVarMap const & typeVars,
		CodeLocation const & location ) {
	assert( param );
	assert( arg );
	assert( realParam );
	if ( isPolyType( realParam, typeVars ) && !isPolyType( arg ) ) {
		ast::UntypedExpr * deref = ast::UntypedExpr::createDeref(
			location,
			new ast::CastExpr( location,
				new ast::VariableExpr( location, param ),
				new ast::PointerType( ast::deepCopy( arg ) )
			)
		);
		deref->result = ast::deepCopy( arg );
		return deref;
	}
	return new ast::VariableExpr( location, param );
}

// This seems to be one of the problematic functions.
void addAdapterParams(
		ast::ApplicationExpr * adaptee,
		ast::vector<ast::Type>::const_iterator arg,
		ast::vector<ast::DeclWithType>::iterator param,
		ast::vector<ast::DeclWithType>::iterator paramEnd,
		ast::vector<ast::Type>::const_iterator realParam,
		TypeVarMap const & typeVars,
		CodeLocation const & location ) {
	UniqueName paramNamer( "_p" );
	for ( ; param != paramEnd ; ++param, ++arg, ++realParam ) {
		if ( "" == (*param)->name ) {
			auto mutParam = (*param).get_and_mutate();
			mutParam->name = paramNamer.newName();
			mutParam->linkage = ast::Linkage::C;
		}
		adaptee->args.push_back(
			makeAdapterArg( *param, *arg, *realParam, typeVars, location ) );
	}
}

ast::FunctionDecl * CallAdapter::makeAdapter(
		ast::FunctionType const * adaptee,
		ast::FunctionType const * realType,
		std::string const & mangleName,
		TypeVarMap const & typeVars,
		CodeLocation const & location ) const {
	ast::FunctionType * adapterType = makeAdapterType( adaptee, typeVars );
	adapterType = ast::mutate( scrubTypeVars( adapterType, typeVars ) );

	// Some of these names will be overwritten, but it gives a default.
	UniqueName pNamer( "_param" );
	UniqueName rNamer( "_ret" );

	bool first = true;

	ast::FunctionDecl * adapterDecl = new ast::FunctionDecl( location,
		makeAdapterName( mangleName ),
		{}, // forall
		{}, // assertions
		map_range<ast::vector<ast::DeclWithType>>( adapterType->params,
				[&pNamer, &location, &first]( ast::ptr<ast::Type> const & param ) {
			// [Trying to make the generated code match exactly more often.]
			if ( first ) {
				first = false;
				return new ast::ObjectDecl( location, "_adaptee", param );
			}
			return new ast::ObjectDecl( location, pNamer.newName(), param );
		} ),
		map_range<ast::vector<ast::DeclWithType>>( adapterType->returns,
				[&rNamer, &location]( ast::ptr<ast::Type> const & retval ) {
			return new ast::ObjectDecl( location, rNamer.newName(), retval );
		} ),
		nullptr, // stmts
		{}, // storage
		ast::Linkage::C
	);

	ast::DeclWithType * adapteeDecl =
		adapterDecl->params.front().get_and_mutate();
	adapteeDecl->name = "_adaptee";

	// Do not carry over attributes to real type parameters/return values.
	auto mutRealType = ast::mutate( realType );
	for ( ast::ptr<ast::Type> & decl : mutRealType->params ) {
		if ( decl->attributes.empty() ) continue;
		auto mut = ast::mutate( decl.get() );
		mut->attributes.clear();
		decl = mut;
	}
	for ( ast::ptr<ast::Type> & decl : mutRealType->returns ) {
		if ( decl->attributes.empty() ) continue;
		auto mut = ast::mutate( decl.get() );
		mut->attributes.clear();
		decl = mut;
	}
	realType = mutRealType;

	ast::ApplicationExpr * adapteeApp = new ast::ApplicationExpr( location,
		new ast::CastExpr( location,
			new ast::VariableExpr( location, adapteeDecl ),
			new ast::PointerType( realType )
		)
	);

	for ( auto const & [assertArg, assertParam, assertReal] : group_iterate(
			realType->assertions, adapterType->assertions, adaptee->assertions ) ) {
		adapteeApp->args.push_back( makeAdapterArg(
			assertParam->var, assertArg->var->get_type(),
			assertReal->var->get_type(), typeVars, location
		) );
	}

	ast::vector<ast::Type>::const_iterator
		arg = realType->params.begin(),
		param = adapterType->params.begin(),
		realParam = adaptee->params.begin();
	ast::vector<ast::DeclWithType>::iterator
		paramDecl = adapterDecl->params.begin();
	// Skip adaptee parameter in the adapter type.
	++param;
	++paramDecl;

	ast::Stmt * bodyStmt;
	// Returns void/nothing.
	if ( realType->returns.empty() ) {
		addAdapterParams( adapteeApp, arg, paramDecl, adapterDecl->params.end(),
			realParam, typeVars, location );
		bodyStmt = new ast::ExprStmt( location, adapteeApp );
	// Returns a polymorphic type.
	} else if ( isDynType( adaptee->returns.front(), typeVars ) ) {
		ast::UntypedExpr * assign = new ast::UntypedExpr( location,
			new ast::NameExpr( location, "?=?" ) );
		ast::UntypedExpr * deref = ast::UntypedExpr::createDeref( location,
			new ast::CastExpr( location,
				new ast::VariableExpr( location, *paramDecl++ ),
				new ast::PointerType(
					ast::deepCopy( realType->returns.front() ) ) ) );
		assign->args.push_back( deref );
		addAdapterParams( adapteeApp, arg, paramDecl, adapterDecl->params.end(),
			realParam, typeVars, location );
		assign->args.push_back( adapteeApp );
		bodyStmt = new ast::ExprStmt( location, assign );
	// Adapter for a function that returns a monomorphic value.
	} else {
		addAdapterParams( adapteeApp, arg, paramDecl, adapterDecl->params.end(),
				realParam, typeVars, location );
		bodyStmt = new ast::ReturnStmt( location, adapteeApp );
	}

	adapterDecl->stmts = new ast::CompoundStmt( location, { bodyStmt } );
	return adapterDecl;
}

ast::Expr const * makeIncrDecrExpr(
		CodeLocation const & location,
		ast::ApplicationExpr const * expr,
		ast::Type const * polyType,
		bool isIncr ) {
	ast::NameExpr * opExpr =
			new ast::NameExpr( location, isIncr ? "?+=?" : "?-=?" );
	ast::UntypedExpr * addAssign = new ast::UntypedExpr( location, opExpr );
	if ( auto address = expr->args.front().as<ast::AddressExpr>() ) {
		addAssign->args.push_back( address->arg );
	} else {
		addAssign->args.push_back( expr->args.front() );
	}
	addAssign->args.push_back( new ast::NameExpr( location,
		sizeofName( Mangle::mangleType( polyType ) ) ) );
	addAssign->result = ast::deepCopy( expr->result );
	addAssign->env = expr->env ? expr->env : addAssign->env;
	return addAssign;
}

/// Handles intrinsic functions for postvisit ApplicationExpr.
ast::Expr const * CallAdapter::handleIntrinsics(
		ast::ApplicationExpr const * expr ) {
	auto varExpr = expr->func.as<ast::VariableExpr>();
	if ( !varExpr || varExpr->var->linkage != ast::Linkage::Intrinsic ) {
		return nullptr;
	}
	std::string const & varName = varExpr->var->name;

	// Index Intrinsic:
	if ( "?[?]" == varName ) {
		assert( expr->result );
		assert( 2 == expr->args.size() );

		ast::Type const * baseType1 =
			isPolyPtr( expr->args.front()->result, scopeTypeVars, typeSubs );
		ast::Type const * baseType2 =
			isPolyPtr( expr->args.back()->result, scopeTypeVars, typeSubs );
		// If neither argument is a polymorphic pointer, do nothing.
		if ( !baseType1 && !baseType2 ) {
			return expr;
		}
		// The arguments cannot both be polymorphic pointers.
		assert( !baseType1 || !baseType2 );
		// (So exactly one of the arguments is a polymorphic pointer.)

		CodeLocation const & location = expr->location;
		CodeLocation const & location1 = expr->args.front()->location;
		CodeLocation const & location2 = expr->args.back()->location;

		ast::UntypedExpr * ret = new ast::UntypedExpr( location,
				new ast::NameExpr( location, "?+?" ) );
		if ( baseType1 ) {
			auto multiply = ast::UntypedExpr::createCall( location2, "?*?", {
				expr->args.back(),
				new ast::SizeofExpr( location1, deepCopy( baseType1 ) ),
			} );
			ret->args.push_back( expr->args.front() );
			ret->args.push_back( multiply );
		} else {
			assert( baseType2 );
			auto multiply = ast::UntypedExpr::createCall( location1, "?*?", {
				expr->args.front(),
				new ast::SizeofExpr( location2, deepCopy( baseType2 ) ),
			} );
			ret->args.push_back( multiply );
			ret->args.push_back( expr->args.back() );
		}
		ret->result = ast::deepCopy( expr->result );
		ret->env = expr->env ? expr->env : ret->env;
		return ret;
	// Dereference Intrinsic:
	} else if ( "*?" == varName ) {
		assert( expr->result );
		assert( 1 == expr->args.size() );

		// If this isn't for a poly type, then do nothing.
		if ( !isPolyType( expr->result, scopeTypeVars, typeSubs ) ) {
			return expr;
		}

		// Remove dereference from polymorphic types since they are boxed.
		ast::Expr * ret = ast::deepCopy( expr->args.front() );
		// Fix expression type to remove pointer.
		ret->result = expr->result;
		ret->env = expr->env ? expr->env : ret->env;
		return ret;
	// Post-Increment/Decrement Intrinsics:
	} else if ( "?++" == varName || "?--" == varName ) {
		assert( expr->result );
		assert( 1 == expr->args.size() );

		ast::Type const * baseType =
			isPolyType( expr->result, scopeTypeVars, typeSubs );
		if ( nullptr == baseType ) {
			return expr;
		}
		ast::Type * tempType = ast::deepCopy( expr->result );
		if ( typeSubs ) {
			auto result = typeSubs->apply( tempType );
			tempType = ast::mutate( result.node.release() );
		}
		CodeLocation const & location = expr->location;
		ast::ObjectDecl * newObj = makeTemporary( location, tempType );
		ast::VariableExpr * tempExpr =
			new ast::VariableExpr( location, newObj );
		ast::UntypedExpr * assignExpr = new ast::UntypedExpr( location,
			new ast::NameExpr( location, "?=?" ) );
		assignExpr->args.push_back( ast::deepCopy( tempExpr ) );
		if ( auto address = expr->args.front().as<ast::AddressExpr>() ) {
			assignExpr->args.push_back( ast::deepCopy( address->arg ) );
		} else {
			assignExpr->args.push_back( ast::deepCopy( expr->args.front() ) );
		}
		return new ast::CommaExpr( location,
			new ast::CommaExpr( location,
				assignExpr,
				makeIncrDecrExpr( location, expr, baseType, "?++" == varName )
			),
			tempExpr
		);
	// Pre-Increment/Decrement Intrinsics:
	} else if ( "++?" == varName || "--?" == varName ) {
		assert( expr->result );
		assert( 1 == expr->args.size() );

		ast::Type const * baseType =
			isPolyType( expr->result, scopeTypeVars, typeSubs );
		if ( nullptr == baseType ) {
			return expr;
		}
		return makeIncrDecrExpr(
			expr->location, expr, baseType, "++?" == varName );
	// Addition and Subtration Intrinsics:
	} else if ( "?+?" == varName || "?-?" == varName ) {
		assert( expr->result );
		assert( 2 == expr->args.size() );

		auto baseType1 =
			isPolyPtr( expr->args.front()->result, scopeTypeVars, typeSubs );
		auto baseType2 =
			isPolyPtr( expr->args.back()->result, scopeTypeVars, typeSubs );

		CodeLocation const & location = expr->location;
		CodeLocation const & location1 = expr->args.front()->location;
		CodeLocation const & location2 = expr->args.back()->location;
		// LHS op RHS -> (LHS op RHS) / sizeof(LHS)
		if ( baseType1 && baseType2 ) {
			auto divide = ast::UntypedExpr::createCall( location, "?/?", {
				expr,
				new ast::SizeofExpr( location, deepCopy( baseType1 ) ),
			} );
			if ( expr->env ) divide->env = expr->env;
			return divide;
		// LHS op RHS -> LHS op (RHS * sizeof(LHS))
		} else if ( baseType1 ) {
			auto multiply = ast::UntypedExpr::createCall( location2, "?*?", {
				expr->args.back(),
				new ast::SizeofExpr( location1, deepCopy( baseType1 ) ),
			} );
			return ast::mutate_field_index(
				expr, &ast::ApplicationExpr::args, 1, multiply );
		// LHS op RHS -> (LHS * sizeof(RHS)) op RHS
		} else if ( baseType2 ) {
			auto multiply = ast::UntypedExpr::createCall( location1, "?*?", {
				expr->args.front(),
				new ast::SizeofExpr( location2, deepCopy( baseType2 ) ),
			} );
			return ast::mutate_field_index(
				expr, &ast::ApplicationExpr::args, 0, multiply );
		}
	// Addition and Subtration Relative Assignment Intrinsics:
	} else if ( "?+=?" == varName || "?-=?" == varName ) {
		assert( expr->result );
		assert( 2 == expr->args.size() );

		CodeLocation const & location1 = expr->args.front()->location;
		CodeLocation const & location2 = expr->args.back()->location;
		auto baseType = isPolyPtr( expr->result, scopeTypeVars, typeSubs );
		// LHS op RHS -> LHS op (RHS * sizeof(LHS))
		if ( baseType ) {
			auto multiply = ast::UntypedExpr::createCall( location2, "?*?", {
				expr->args.back(),
				new ast::SizeofExpr( location1, deepCopy( baseType ) ),
			} );
			return ast::mutate_field_index(
				expr, &ast::ApplicationExpr::args, 1, multiply );
		}
	}
	return expr;
}

ast::ObjectDecl * CallAdapter::makeTemporary(
		CodeLocation const & location, ast::Type const * type ) {
	auto newObj = new ast::ObjectDecl( location, tmpNamer.newName(), type );
	stmtsToAddBefore.push_back( new ast::DeclStmt( location, newObj ) );
	return newObj;
}

// --------------------------------------------------------------------------
/// Modifies declarations to accept implicit parameters.
/// * Move polymorphic returns in function types to pointer-type parameters.
/// * Adds type size and assertion parameters to parameter lists.
struct DeclAdapter final {
	ast::FunctionDecl const * previsit( ast::FunctionDecl const * decl );
	ast::FunctionDecl const * postvisit( ast::FunctionDecl const * decl );
private:
	void addAdapters( ast::FunctionDecl * decl, TypeVarMap & localTypeVars );
};

ast::ObjectDecl * makeObj(
		CodeLocation const & location, std::string const & name ) {
	// The size/align parameters may be unused, so add the unused attribute.
	return new ast::ObjectDecl( location, name,
		makeLayoutCType(),
		nullptr, ast::Storage::Classes(), ast::Linkage::C, nullptr,
		{ new ast::Attribute( "unused" ) } );
}

/// A modified and specialized version of ast::add_qualifiers.
ast::Type const * addConst( ast::Type const * type ) {
	ast::CV::Qualifiers cvq = { ast::CV::Const };
	if ( ( type->qualifiers & cvq ) != 0 ) return type;
	auto mutType = ast::mutate( type );
	mutType->qualifiers |= cvq;
	return mutType;
}

ast::FunctionDecl const * DeclAdapter::previsit( ast::FunctionDecl const * decl ) {
	TypeVarMap localTypeVars;
	makeTypeVarMap( decl, localTypeVars );

	auto mutDecl = mutate( decl );

	// Move polymorphic return type to parameter list.
	if ( isDynRet( mutDecl->type ) ) {
		auto ret = strict_dynamic_cast<ast::ObjectDecl *>(
			mutDecl->returns.front().get_and_mutate() );
		ret->set_type( new ast::PointerType( ret->type ) );
		mutDecl->params.insert( mutDecl->params.begin(), ret );
		mutDecl->returns.erase( mutDecl->returns.begin() );
		ret->init = nullptr;
	}

	// Add size/align and assertions for type parameters to parameter list.
	ast::vector<ast::DeclWithType> inferredParams;
	ast::vector<ast::DeclWithType> layoutParams;
	for ( ast::ptr<ast::TypeDecl> & typeParam : mutDecl->type_params ) {
		auto mutParam = mutate( typeParam.get() );
		// Add all size and alignment parameters to parameter list.
		if ( mutParam->isComplete() ) {
			ast::TypeInstType paramType( mutParam );
			std::string paramName = Mangle::mangleType( &paramType );

			auto sizeParam = makeObj( typeParam->location, sizeofName( paramName ) );
			layoutParams.emplace_back( sizeParam );

			auto alignParam = makeObj( typeParam->location, alignofName( paramName ) );
			layoutParams.emplace_back( alignParam );
		}
		// Assertions should be stored in the main list.
		assert( mutParam->assertions.empty() );
		typeParam = mutParam;
	}
	for ( ast::ptr<ast::DeclWithType> & assert : mutDecl->assertions ) {
		ast::DeclWithType * mutAssert = ast::mutate( assert.get() );
		// Assertion parameters may not be used in body,
		// pass along with unused attribute.
		mutAssert->attributes.push_back( new ast::Attribute( "unused" ) );
		mutAssert->set_type( addConst( mutAssert->get_type() ) );
		inferredParams.emplace_back( mutAssert );
	}
	mutDecl->assertions.clear();

	// Prepend each argument group. From last group to first. addAdapters
	// does do the same, it just does it itself and see all other parameters.
	spliceBegin( mutDecl->params, inferredParams );
	spliceBegin( mutDecl->params, layoutParams );
	addAdapters( mutDecl, localTypeVars );

	// Now have to update the type to match the declaration.
	ast::FunctionType * type = new ast::FunctionType(
		mutDecl->type->isVarArgs, mutDecl->type->qualifiers );
	// The forall clauses don't match until Eraser. The assertions are empty.
	for ( auto param : mutDecl->params ) {
		type->params.emplace_back( param->get_type() );
	}
	for ( auto retval : mutDecl->returns ) {
		type->returns.emplace_back( retval->get_type() );
	}
	mutDecl->type = type;

	return mutDecl;
}

ast::FunctionDecl const * DeclAdapter::postvisit(
		ast::FunctionDecl const * decl ) {
	ast::FunctionDecl * mutDecl = mutate( decl );
	if ( !mutDecl->returns.empty() && mutDecl->stmts
			// Intrinsic functions won't be using the _retval so no need to
			// generate it.
			&& mutDecl->linkage != ast::Linkage::Intrinsic
			// Remove check for prefix once thunks properly use ctor/dtors.
			&& !isPrefix( mutDecl->name, "_thunk" )
			&& !isPrefix( mutDecl->name, "_adapter" ) ) {
		assert( 1 == mutDecl->returns.size() );
		ast::DeclWithType const * retval = mutDecl->returns.front();
		if ( "" == retval->name ) {
			retval = ast::mutate_field(
				retval, &ast::DeclWithType::name, "_retval" );
			mutDecl->returns.front() = retval;
		}
		auto stmts = mutDecl->stmts.get_and_mutate();
		stmts->kids.push_front( new ast::DeclStmt( retval->location, retval ) );
		ast::DeclWithType * newRet = ast::deepCopy( retval );
		mutDecl->returns.front() = newRet;
	}
	// Errors should have been caught by this point, remove initializers from
	// parameters to allow correct codegen of default arguments.
	for ( ast::ptr<ast::DeclWithType> & param : mutDecl->params ) {
		if ( auto obj = param.as<ast::ObjectDecl>() ) {
			param = ast::mutate_field( obj, &ast::ObjectDecl::init, nullptr );
		}
	}
	return mutDecl;
}

void DeclAdapter::addAdapters(
		ast::FunctionDecl * mutDecl, TypeVarMap & localTypeVars ) {
	ast::vector<ast::FunctionType> functions;
	for ( ast::ptr<ast::DeclWithType> & arg : mutDecl->params ) {
		ast::Type const * type = arg->get_type();
		type = findAndReplaceFunction( type, functions, localTypeVars, needsAdapter );
		arg.get_and_mutate()->set_type( type );
	}
	std::set<std::string> adaptersDone;
	for ( ast::ptr<ast::FunctionType> const & func : functions ) {
		std::string mangleName = mangleAdapterName( func, localTypeVars );
		if ( adaptersDone.find( mangleName ) != adaptersDone.end() ) {
			continue;
		}
		std::string adapterName = makeAdapterName( mangleName );
		// The adapter may not actually be used, so make sure it has unused.
		mutDecl->params.insert( mutDecl->params.begin(), new ast::ObjectDecl(
			mutDecl->location, adapterName,
			new ast::PointerType( makeAdapterType( func, localTypeVars ) ),
			nullptr, {}, {}, nullptr,
			{ new ast::Attribute( "unused" ) } ) );
		adaptersDone.insert( adaptersDone.begin(), mangleName );
	}
}

// --------------------------------------------------------------------------
/// Corrects the floating nodes created in CallAdapter.
struct RewireAdapters final : public ast::WithGuards {
	ScopedMap<std::string, ast::ObjectDecl const *> adapters;
	void beginScope() { adapters.beginScope(); }
	void endScope() { adapters.endScope(); }
	void previsit( ast::FunctionDecl const * decl );
	ast::VariableExpr const * previsit( ast::VariableExpr const * expr );
};

void RewireAdapters::previsit( ast::FunctionDecl const * decl ) {
	GuardScope( adapters );
	for ( ast::ptr<ast::DeclWithType> const & param : decl->params ) {
		if ( auto objectParam = param.as<ast::ObjectDecl>() ) {
			adapters.insert( objectParam->name, objectParam );
		}
	}
}

ast::VariableExpr const * RewireAdapters::previsit(
		ast::VariableExpr const * expr ) {
	// If the node is not floating, we can skip.
	if ( expr->var->isManaged() ) return expr;
	auto it = adapters.find( expr->var->name );
	assertf( it != adapters.end(), "Could not correct floating node." );
	return ast::mutate_field( expr, &ast::VariableExpr::var, it->second );
}

// --------------------------------------------------------------------------
/// Inserts code to access polymorphic layout inforation.
/// * Replaces member and size/alignment/offsetof expressions on polymorphic
///   generic types with calculated expressions.
/// * Replaces member expressions for polymorphic types with calculated
///   add-field-offset-and-dereference.
/// * Calculates polymorphic offsetof expressions from offset array.
/// * Inserts dynamic calculation of polymorphic type layouts where needed.
struct PolyGenericCalculator final :
		public ast::WithConstTypeSubstitution,
		public ast::WithDeclsToAdd<>,
		public ast::WithGuards,
		public ast::WithStmtsToAdd<>,
		public ast::WithVisitorRef<PolyGenericCalculator> {
	PolyGenericCalculator();

	void previsit( ast::FunctionDecl const * decl );
	void previsit( ast::TypedefDecl const * decl );
	void previsit( ast::TypeDecl const * decl );
	ast::Decl const * postvisit( ast::TypeDecl const * decl );
	ast::StructDecl const * previsit( ast::StructDecl const * decl );
	ast::UnionDecl const * previsit( ast::UnionDecl const * decl );
	ast::DeclStmt const * previsit( ast::DeclStmt const * stmt );
	ast::Expr const * postvisit( ast::MemberExpr const * expr );
	void previsit( ast::AddressExpr const * expr );
	ast::Expr const * postvisit( ast::AddressExpr const * expr );
	ast::Expr const * postvisit( ast::SizeofExpr const * expr );
	ast::Expr const * postvisit( ast::AlignofExpr const * expr );
	ast::Expr const * postvisit( ast::OffsetofExpr const * expr );
	ast::Expr const * postvisit( ast::OffsetPackExpr const * expr );

	void beginScope();
	void endScope();
private:
	/// Makes a new variable in the current scope with the given name,
	/// type and optional initializer.
	ast::ObjectDecl * makeVar(
			CodeLocation const & location, std::string const & name,
			ast::Type const * type, ast::Init const * init = nullptr );
	/// Returns true if the type has a dynamic layout;
	/// such a layout will be stored in appropriately-named local variables
	/// when the function returns.
	bool findGeneric( CodeLocation const & location, ast::Type const * );
	/// Adds type parameters to the layout call; will generate the
	/// appropriate parameters if needed.
	void addSTypeParamsToLayoutCall(
		ast::UntypedExpr * layoutCall,
		const ast::vector<ast::Type> & otypeParams );
	/// Change the type of generic aggregate members to char[].
	void mutateMembers( ast::AggregateDecl * aggr );
	/// Returns the calculated sizeof expression for type, or nullptr for use
	/// C sizeof().
	ast::Expr const * genSizeof( CodeLocation const &, ast::Type const * );
	/// Enters a new scope for type-variables,
	/// adding the type variables from the provided type.
	void beginTypeScope( ast::Type const * );

	/// The type variables and polymorphic parameters currently in scope.
	TypeVarMap scopeTypeVars;
	/// Set of generic type layouts known in the current scope,
	/// indexed by sizeofName.
	ScopedSet<std::string> knownLayouts;
	/// Set of non-generic types for which the offset array exists in the
	/// current scope, indexed by offsetofName.
	ScopedSet<std::string> knownOffsets;
	/// Namer for VLA (variable length array) buffers.
	UniqueName bufNamer;
	/// If the argument of an AddressExpr is MemberExpr, it is stored here.
	ast::MemberExpr const * addrMember = nullptr;
};

PolyGenericCalculator::PolyGenericCalculator() :
	knownLayouts(), knownOffsets(), bufNamer( "_buf" )
{}

/// Converts polymorphic type into a suitable monomorphic representation.
/// Currently: __attribute__(( aligned(8) )) char[size_T];
ast::Type * polyToMonoType( CodeLocation const & location,
		ast::Type const * declType ) {
	auto charType = new ast::BasicType( ast::BasicKind::Char );
	auto size = new ast::NameExpr( location,
		sizeofName( Mangle::mangleType( declType ) ) );
	auto ret = new ast::ArrayType( charType, size,
		ast::VariableLen, ast::DynamicDim, ast::CV::Qualifiers() );
	ret->attributes.emplace_back( new ast::Attribute( "aligned",
		{ ast::ConstantExpr::from_int( location, 8 ) } ) );
	return ret;
}

void PolyGenericCalculator::previsit( ast::FunctionDecl const * decl ) {
	GuardScope( *this );
	beginTypeScope( decl->type );
}

void PolyGenericCalculator::previsit( ast::TypedefDecl const * decl ) {
	assertf( false, "All typedef declarations should be removed." );
	beginTypeScope( decl->base );
}

void PolyGenericCalculator::previsit( ast::TypeDecl const * decl ) {
	addToTypeVarMap( decl, scopeTypeVars );
}

ast::Decl const * PolyGenericCalculator::postvisit(
		ast::TypeDecl const * decl ) {
	ast::Type const * base = decl->base;
	if ( nullptr == base ) return decl;

	// Add size/align variables for opaque type declarations.
	ast::TypeInstType inst( decl->name, decl );
	std::string typeName = Mangle::mangleType( &inst );

	ast::ObjectDecl * sizeDecl = new ast::ObjectDecl( decl->location,
		sizeofName( typeName ), makeLayoutCType(),
		new ast::SingleInit( decl->location,
			new ast::SizeofExpr( decl->location, deepCopy( base ) )
		)
	);
	ast::ObjectDecl * alignDecl = new ast::ObjectDecl( decl->location,
		alignofName( typeName ), makeLayoutCType(),
		new ast::SingleInit( decl->location,
			new ast::AlignofExpr( decl->location, deepCopy( base ) )
		)
	);

	// Ensure that the initializing sizeof/alignof exprs are properly mutated.
	sizeDecl->accept( *visitor );
	alignDecl->accept( *visitor );

	// A little trick to replace this with two declarations.
	// Adding after makes sure that there is no conflict with adding stmts.
	declsToAddAfter.push_back( alignDecl );
	return sizeDecl;
}

ast::StructDecl const * PolyGenericCalculator::previsit(
		ast::StructDecl const * decl ) {
	auto mutDecl = mutate( decl );
	mutateMembers( mutDecl );
	return mutDecl;
}

ast::UnionDecl const * PolyGenericCalculator::previsit(
		ast::UnionDecl const * decl ) {
	auto mutDecl = mutate( decl );
	mutateMembers( mutDecl );
	return mutDecl;
}

ast::DeclStmt const * PolyGenericCalculator::previsit( ast::DeclStmt const * stmt ) {
	ast::ObjectDecl const * decl = stmt->decl.as<ast::ObjectDecl>();
	if ( !decl || !findGeneric( decl->location, decl->type ) ) {
		return stmt;
	}

	// Change initialization of a polymorphic value object to allocate via a
	// variable-length-array (alloca cannot be safely used in loops).
	ast::ObjectDecl * newBuf = new ast::ObjectDecl( decl->location,
		bufNamer.newName(),
		polyToMonoType( decl->location, decl->type ),
		nullptr, {}, ast::Linkage::C
	);
	stmtsToAddBefore.push_back( new ast::DeclStmt( stmt->location, newBuf ) );

	// If the object has a cleanup attribute, the clean-up should be on the
	// buffer, not the pointer. [Perhaps this should be lifted?]
	auto matchAndMove = [newBuf]( ast::ptr<ast::Attribute> & attr ) {
		if ( "cleanup" == attr->name ) {
			newBuf->attributes.push_back( attr );
			return true;
		}
		return false;
	};

	auto mutDecl = mutate( decl );

	// Forally, side effects are not safe in this function. But it works.
	erase_if( mutDecl->attributes, matchAndMove );

	mutDecl->init = new ast::SingleInit( decl->location,
		new ast::VariableExpr( decl->location, newBuf ) );

	return ast::mutate_field( stmt, &ast::DeclStmt::decl, mutDecl );
}

/// Checks if memberDecl matches the decl from an aggregate.
bool isMember( ast::DeclWithType const * memberDecl, ast::Decl const * decl ) {
	// No matter the field, if the name is different it is not the same.
	if ( memberDecl->name != decl->name ) {
		return false;
	}

	if ( memberDecl->name.empty() ) {
		// Plan-9 Field: Match on unique_id.
		return ( memberDecl->uniqueId == decl->uniqueId );
	}

	ast::DeclWithType const * declWithType =
		strict_dynamic_cast<ast::DeclWithType const *>( decl );

	if ( memberDecl->mangleName.empty() || declWithType->mangleName.empty() ) {
		// Tuple-Element Field: Expect neither had mangled name;
		// accept match on simple name (like field_2) only.
		assert( memberDecl->mangleName.empty() );
		assert( declWithType->mangleName.empty() );
		return true;
	}

	// Ordinary Field: Use full name to accommodate overloading.
	return ( memberDecl->mangleName == declWithType->mangleName );
}

/// Finds the member in the base list that matches the given declaration;
/// returns its index, or -1 if not present.
long findMember( ast::DeclWithType const * memberDecl,
		const ast::vector<ast::Decl> & baseDecls ) {
	for ( auto const & [index, value] : enumerate( baseDecls ) ) {
		if ( isMember( memberDecl, value.get() ) ) {
			return index;
		}
	}
	return -1;
}

/// Returns an index expression into the offset array for a type.
ast::Expr * makeOffsetIndex( CodeLocation const & location,
		ast::Type const * objectType, long i ) {
	std::string name = offsetofName( Mangle::mangleType( objectType ) );
	return ast::UntypedExpr::createCall( location, "?[?]", {
		new ast::NameExpr( location, name ),
		ast::ConstantExpr::from_ulong( location, i ),
	} );
}

ast::Expr const * PolyGenericCalculator::postvisit(
		ast::MemberExpr const * expr ) {
	// Only mutate member expressions for polymorphic types.
	ast::Type const * objectType = hasPolyBase(
		expr->aggregate->result, scopeTypeVars
	);
	if ( !objectType ) return expr;
	// Ensure layout for this type is available.
	// The boolean result is ignored.
	findGeneric( expr->location, objectType );

	// Replace member expression with dynamically-computed layout expression.
	ast::Expr * newMemberExpr = nullptr;
	if ( auto structType = dynamic_cast<ast::StructInstType const *>( objectType ) ) {
		long offsetIndex = findMember( expr->member, structType->base->members );
		if ( -1 == offsetIndex ) return expr;

		// Replace member expression with pointer to struct plus offset.
		ast::UntypedExpr * fieldLoc = new ast::UntypedExpr( expr->location,
				new ast::NameExpr( expr->location, "?+?" ) );
		ast::Expr * aggr = deepCopy( expr->aggregate );
		aggr->env = nullptr;
		fieldLoc->args.push_back( aggr );
		fieldLoc->args.push_back(
			makeOffsetIndex( expr->location, objectType, offsetIndex ) );
		fieldLoc->result = deepCopy( expr->result );
		newMemberExpr = fieldLoc;
	// Union members are all at offset zero, so just use the aggregate expr.
	} else if ( dynamic_cast<ast::UnionInstType const *>( objectType ) ) {
		ast::Expr * aggr = deepCopy( expr->aggregate );
		aggr->env = nullptr;
		aggr->result = deepCopy( expr->result );
		newMemberExpr = aggr;
	} else {
		return expr;
	}
	assert( newMemberExpr );

	// Must apply the generic substitution to the member type to handle cases
	// where the member is a generic parameter subsituted by a known concrete
	// type. [ex]
	//	forall( T ) struct Box { T x; }
	//	forall( T ) void f() {
	//		Box( T * ) b; b.x;
	//	}
	// TODO: expr->result should be exactly expr->member->get_type() after
	// substitution, so it doesn't seem like it should be necessary to apply
	// the substitution manually. For some reason this is not currently the
	// case. This requires more investigation.
	ast::ptr<ast::Type> memberType = deepCopy( expr->member->get_type() );
	ast::TypeSubstitution sub = genericSubstitution( objectType );
	sub.apply( memberType );

	// Not all members of a polymorphic type are themselves of a polymorphic
	// type; in this case the member expression should be wrapped and
	// dereferenced to form an lvalue.
	if ( !isPolyType( memberType, scopeTypeVars ) ) {
		auto ptrCastExpr = new ast::CastExpr( expr->location, newMemberExpr,
			new ast::PointerType( memberType ) );
		auto derefExpr = ast::UntypedExpr::createDeref( expr->location,
			ptrCastExpr );
		newMemberExpr = derefExpr;
	}

	return newMemberExpr;
}

void PolyGenericCalculator::previsit( ast::AddressExpr const * expr ) {
	GuardValue( addrMember ) = expr->arg.as<ast::MemberExpr>();
}

ast::Expr const * PolyGenericCalculator::postvisit(
		ast::AddressExpr const * expr ) {
	// arg has to have been a MemberExpr and has been mutated.
	if ( nullptr == addrMember || expr->arg == addrMember ) {
		return expr;
	}
	ast::UntypedExpr const * untyped = expr->arg.as<ast::UntypedExpr>();
	if ( !untyped || getFunctionName( untyped ) != "?+?" ) {
		return expr;
	}
	// MemberExpr was converted to pointer + offset; and it is not valid C to
	// take the address of an addition, so strip away the address-of.
	// It also preserves the env value.
	return ast::mutate_field( expr->arg.get(), &ast::Expr::env, expr->env );
}

ast::Expr const * PolyGenericCalculator::postvisit(
		ast::SizeofExpr const * expr ) {
	ast::Type const * type = expr->type ? expr->type : expr->expr->result;
	ast::Expr const * gen = genSizeof( expr->location, type );
	return ( gen ) ? gen : expr;
}

ast::Expr const * PolyGenericCalculator::postvisit(
		ast::AlignofExpr const * expr ) {
	ast::Type const * type = expr->type ? expr->type : expr->expr->result;
	if ( findGeneric( expr->location, type ) ) {
		return new ast::NameExpr( expr->location,
			alignofName( Mangle::mangleType( type ) ) );
	} else {
		return expr;
	}
}

ast::Expr const * PolyGenericCalculator::postvisit(
		ast::OffsetofExpr const * expr ) {
	ast::Type const * type = expr->type;
	if ( !findGeneric( expr->location, type ) ) return expr;

	// Structures replace offsetof expression with an index into offset array.
	if ( auto structType = dynamic_cast<ast::StructInstType const *>( type ) ) {
		long offsetIndex = findMember( expr->member, structType->base->members );
		if ( -1 == offsetIndex ) return expr;

		return makeOffsetIndex( expr->location, type, offsetIndex );
	// All union members are at offset zero.
	} else if ( dynamic_cast<ast::UnionInstType const *>( type ) ) {
		return ast::ConstantExpr::from_ulong( expr->location, 0 );
	} else {
		return expr;
	}
}

ast::Expr const * PolyGenericCalculator::postvisit(
		ast::OffsetPackExpr const * expr ) {
	ast::StructInstType const * type = expr->type;

	// Pull offset back from generated type information.
	if ( findGeneric( expr->location, type ) ) {
		return new ast::NameExpr( expr->location,
			offsetofName( Mangle::mangleType( type ) ) );
	}

	std::string offsetName = offsetofName( Mangle::mangleType( type ) );
	// Use the already generated offsets for this type.
	if ( knownOffsets.contains( offsetName ) ) {
		return new ast::NameExpr( expr->location, offsetName );
	}

	knownOffsets.insert( offsetName );

	// Build initializer list for offset array.
	ast::vector<ast::Init> inits;
	for ( ast::ptr<ast::Decl> const & member : type->base->members ) {
		auto memberDecl = member.as<ast::DeclWithType>();
		assertf( memberDecl, "Requesting offset of non-DWT member: %s",
			toCString( member ) );
		inits.push_back( new ast::SingleInit( expr->location,
			new ast::OffsetofExpr( expr->location,
				deepCopy( type ),
				memberDecl
			)
		) );
	}

	auto offsetArray = makeVar( expr->location, offsetName,
		new ast::ArrayType(
			makeLayoutType(),
			ast::ConstantExpr::from_ulong( expr->location, inits.size() ),
			ast::FixedLen,
			ast::DynamicDim
		),
		new ast::ListInit( expr->location, std::move( inits ) )
	);

	return new ast::VariableExpr( expr->location, offsetArray );
}

void PolyGenericCalculator::beginScope() {
	knownLayouts.beginScope();
	knownOffsets.beginScope();
}

void PolyGenericCalculator::endScope() {
	knownOffsets.endScope();
	knownLayouts.endScope();
}

ast::ObjectDecl * PolyGenericCalculator::makeVar(
		CodeLocation const & location, std::string const & name,
		ast::Type const * type, ast::Init const * init ) {
	ast::ObjectDecl * ret = new ast::ObjectDecl( location, name, type, init );
	stmtsToAddBefore.push_back( new ast::DeclStmt( location, ret ) );
	return ret;
}

/// Returns true if any of the otype parameters have a dynamic layout; and
/// puts all otype parameters in the output list.
bool findGenericParams(
		ast::vector<ast::Type> & out,
		ast::vector<ast::TypeDecl> const & baseParams,
		ast::vector<ast::Expr> const & typeParams ) {
	bool hasDynamicLayout = false;

	for ( auto const & [baseParam, typeParam] : group_iterate(
			baseParams, typeParams ) ) {
		if ( !baseParam->isComplete() ) continue;
		ast::TypeExpr const * typeExpr = typeParam.as<ast::TypeExpr>();
		assertf( typeExpr, "All type parameters should be type expressions." );

		ast::Type const * type = typeExpr->type.get();
		out.push_back( type );
		if ( isPolyType( type ) ) hasDynamicLayout = true;
	}

	return hasDynamicLayout;
}

bool PolyGenericCalculator::findGeneric(
		CodeLocation const & location, ast::Type const * type ) {
	type = replaceTypeInst( type, typeSubs );

	if ( auto inst = dynamic_cast<ast::TypeInstType const *>( type ) ) {
		// Assumes that getting put in the scopeTypeVars includes having the
		// layout variables set.
		if ( scopeTypeVars.contains( *inst ) ) {
			return true;
		}
	} else if ( auto inst = dynamic_cast<ast::StructInstType const *>( type ) ) {
		// Check if this type already has a layout generated for it.
		std::string typeName = Mangle::mangleType( type );
		if ( knownLayouts.contains( typeName ) ) return true;

		// Check if any type parameters have dynamic layout;
		// If none do, this type is (or will be) monomorphized.
		ast::vector<ast::Type> sizedParams;
		if ( !findGenericParams( sizedParams,
				inst->base->params, inst->params ) ) {
			return false;
		}

		// Insert local variables for layout and generate call to layout
		// function.
		// Done early so as not to interfere with the later addition of
		// parameters to the layout call.
		knownLayouts.insert( typeName );

		int memberCount = inst->base->members.size();
		if ( 0 == memberCount ) {
			// All empty structures have the same layout (size 1, align 1).
			makeVar( location,
				sizeofName( typeName ), makeLayoutType(),
				new ast::SingleInit( location,
						ast::ConstantExpr::from_ulong( location, 1 ) ) );
			makeVar( location,
				alignofName( typeName ), makeLayoutType(),
				new ast::SingleInit( location,
						ast::ConstantExpr::from_ulong( location, 1 ) ) );
			// Since 0-length arrays are forbidden in C, skip the offset array.
		} else {
			ast::ObjectDecl const * sizeofVar = makeVar( location,
				sizeofName( typeName ), makeLayoutType(), nullptr );
			ast::ObjectDecl const * alignofVar = makeVar( location,
				alignofName( typeName ), makeLayoutType(), nullptr );
			ast::ObjectDecl const * offsetofVar = makeVar( location,
				offsetofName( typeName ),
				new ast::ArrayType(
					makeLayoutType(),
					ast::ConstantExpr::from_int( location, memberCount ),
					ast::FixedLen,
					ast::DynamicDim
				),
				nullptr
			);

			// Generate call to layout function.
			ast::UntypedExpr * layoutCall = new ast::UntypedExpr( location,
				new ast::NameExpr( location, layoutofName( inst->base ) ),
				{
					new ast::AddressExpr(
						new ast::VariableExpr( location, sizeofVar ) ),
					new ast::AddressExpr(
						new ast::VariableExpr( location, alignofVar ) ),
					new ast::VariableExpr( location, offsetofVar ),
				} );

			addSTypeParamsToLayoutCall( layoutCall, sizedParams );

			stmtsToAddBefore.emplace_back(
				new ast::ExprStmt( location, layoutCall ) );
		}

		return true;
	} else if ( auto inst = dynamic_cast<ast::UnionInstType const *>( type ) ) {
		// Check if this type already has a layout generated for it.
		std::string typeName = Mangle::mangleType( type );
		if ( knownLayouts.contains( typeName ) ) return true;

		// Check if any type parameters have dynamic layout;
		// If none do, this type is (or will be) monomorphized.
		ast::vector<ast::Type> sizedParams;
		if ( !findGenericParams( sizedParams,
				inst->base->params, inst->params ) ) {
			return false;
		}

		// Insert local variables for layout and generate call to layout
		// function.
		// Done early so as not to interfere with the later addition of
		// parameters to the layout call.
		knownLayouts.insert( typeName );

		ast::ObjectDecl * sizeofVar = makeVar( location,
			sizeofName( typeName ), makeLayoutType() );
		ast::ObjectDecl * alignofVar = makeVar( location,
			alignofName( typeName ), makeLayoutType() );

		ast::UntypedExpr * layoutCall = new ast::UntypedExpr( location,
			new ast::NameExpr( location, layoutofName( inst->base ) ),
			{
				new ast::AddressExpr(
					new ast::VariableExpr( location, sizeofVar ) ),
				new ast::AddressExpr(
					new ast::VariableExpr( location, alignofVar ) ),
			} );

		addSTypeParamsToLayoutCall( layoutCall, sizedParams );

		stmtsToAddBefore.emplace_back(
			new ast::ExprStmt( location, layoutCall ) );

		return true;
	}
	return false;
}

void PolyGenericCalculator::addSTypeParamsToLayoutCall(
		ast::UntypedExpr * layoutCall,
		const ast::vector<ast::Type> & otypeParams ) {
	CodeLocation const & location = layoutCall->location;
	ast::vector<ast::Expr> & args = layoutCall->args;
	for ( ast::ptr<ast::Type> const & param : otypeParams ) {
		if ( findGeneric( location, param ) ) {
			// Push size/align vars for a generic parameter back.
			std::string paramName = Mangle::mangleType( param );
			args.emplace_back(
				new ast::NameExpr( location, sizeofName( paramName ) ) );
			args.emplace_back(
				new ast::NameExpr( location, alignofName( paramName ) ) );
		} else {
			args.emplace_back(
				new ast::SizeofExpr( location, ast::deepCopy( param ) ) );
			args.emplace_back(
				new ast::AlignofExpr( location, ast::deepCopy( param ) ) );
		}
	}
}

void PolyGenericCalculator::mutateMembers( ast::AggregateDecl * aggr ) {
	std::set<std::string> genericParams;
	for ( ast::ptr<ast::TypeDecl> const & decl : aggr->params ) {
		genericParams.insert( decl->name );
	}
	for ( ast::ptr<ast::Decl> & decl : aggr->members ) {
		auto field = decl.as<ast::ObjectDecl>();
		if ( nullptr == field ) continue;

		ast::Type const * type = replaceTypeInst( field->type, typeSubs );
		auto typeInst = dynamic_cast<ast::TypeInstType const *>( type );
		if ( nullptr == typeInst ) continue;

		// Do not try to monoporphize generic parameters.
		if ( scopeTypeVars.contains( ast::TypeEnvKey( *typeInst ) ) &&
				!genericParams.count( typeInst->name ) ) {
			// Polymorphic aggregate members should be converted into
			// monomorphic members. Using char[size_T] here respects
			// the expected sizing rules of an aggregate type.
			decl = ast::mutate_field( field, &ast::ObjectDecl::type,
				polyToMonoType( field->location, field->type ) );
		}
	}
}

ast::Expr const * PolyGenericCalculator::genSizeof(
		CodeLocation const & location, ast::Type const * type ) {
	if ( auto * array = dynamic_cast<ast::ArrayType const *>( type ) ) {
		// Generate calculated size for possibly generic array.
		ast::Expr const * sizeofBase = genSizeof( location, array->base );
		if ( nullptr == sizeofBase ) return nullptr;
		ast::Expr const * dim = array->dimension;
		return makeOp( location, "?*?", sizeofBase, dim );
	} else if ( findGeneric( location, type ) ) {
		// Generate calculated size for generic type.
		return new ast::NameExpr( location, sizeofName(
				Mangle::mangleType( type ) ) );
	} else {
		return nullptr;
	}
}

void PolyGenericCalculator::beginTypeScope( ast::Type const * type ) {
	GuardScope( scopeTypeVars );
	makeTypeVarMap( type, scopeTypeVars );
}

// --------------------------------------------------------------------------
/// Removes unneeded or incorrect type information.
/// * Replaces initialization of polymorphic values with alloca.
/// * Replaces declaration of dtype/ftype with appropriate void expression.
/// * Replaces sizeof expressions of polymorphic types with a variable.
/// * Strips fields from generic structure declarations.
struct Eraser final :
		public ast::WithGuards {
	void guardTypeVarMap( ast::Type const * type ) {
		GuardScope( scopeTypeVars );
		makeTypeVarMap( type, scopeTypeVars );
	}

	ast::ObjectDecl const * previsit( ast::ObjectDecl const * decl );
	ast::FunctionDecl const * previsit( ast::FunctionDecl const * decl );
	ast::FunctionDecl const * postvisit( ast::FunctionDecl const * decl );
	ast::TypedefDecl const * previsit( ast::TypedefDecl const * decl );
	ast::StructDecl const * previsit( ast::StructDecl const * decl );
	ast::UnionDecl const * previsit( ast::UnionDecl const * decl );
	void previsit( ast::TypeDecl const * decl );
	void previsit( ast::PointerType const * type );
	void previsit( ast::FunctionType const * type );
public:
	TypeVarMap scopeTypeVars;
};

ast::ObjectDecl const * Eraser::previsit( ast::ObjectDecl const * decl ) {
	guardTypeVarMap( decl->type );
	return scrubAllTypeVars( decl );
}

ast::FunctionDecl const * Eraser::previsit( ast::FunctionDecl const * decl ) {
	guardTypeVarMap( decl->type );
	return scrubAllTypeVars( decl );
}

ast::FunctionDecl const * Eraser::postvisit( ast::FunctionDecl const * decl ) {
	if ( decl->type_params.empty() ) return decl;
	auto mutDecl = mutate( decl );
	mutDecl->type_params.clear();
	return mutDecl;
}

ast::TypedefDecl const * Eraser::previsit( ast::TypedefDecl const * decl ) {
	guardTypeVarMap( decl->base );
	return scrubAllTypeVars( decl );
}

/// Strips the members from a generic aggregate.
template<typename node_t>
node_t const * stripGenericMembers( node_t const * decl ) {
	if ( decl->params.empty() ) return decl;
	auto mutDecl = ast::mutate( decl );
	mutDecl->members.clear();
	return mutDecl;
}

ast::StructDecl const * Eraser::previsit( ast::StructDecl const * decl ) {
	return stripGenericMembers( decl );
}

ast::UnionDecl const * Eraser::previsit( ast::UnionDecl const * decl ) {
	return stripGenericMembers( decl );
}

void Eraser::previsit( ast::TypeDecl const * decl ) {
	addToTypeVarMap( decl, scopeTypeVars );
}

void Eraser::previsit( ast::PointerType const * type ) {
	guardTypeVarMap( type );
}

void Eraser::previsit( ast::FunctionType const * type ) {
	guardTypeVarMap( type );
}

} // namespace

// --------------------------------------------------------------------------
void box( ast::TranslationUnit & translationUnit ) {
	ast::Pass<LayoutFunctionBuilder>::run( translationUnit );
	ast::Pass<CallAdapter>::run( translationUnit );
	ast::Pass<DeclAdapter>::run( translationUnit );
	ast::Pass<RewireAdapters>::run( translationUnit );
	ast::Pass<PolyGenericCalculator>::run( translationUnit );
	ast::Pass<Eraser>::run( translationUnit );
}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
