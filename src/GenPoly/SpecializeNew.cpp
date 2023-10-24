//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SpecializeNew.cpp -- Generate thunks to specialize polymorphic functions.
//
// Author           : Andrew Beach
// Created On       : Tue Jun  7 13:37:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jun  7 13:37:00 2022
// Update Count     : 0
//

#include "Specialize.h"

#include "AST/Copy.hpp"                  // for deepCopy
#include "AST/Inspect.hpp"               // for isIntrinsicCallExpr
#include "AST/Pass.hpp"                  // for Pass
#include "AST/TypeEnvironment.hpp"       // for OpenVarSet, AssertionSet
#include "Common/UniqueName.h"           // for UniqueName
#include "GenPoly/GenPoly.h"             // for getFunctionType
#include "ResolvExpr/FindOpenVars.h"     // for findOpenVars
#include "ResolvExpr/TypeEnvironment.h"  // for FirstOpen, FirstClosed

namespace GenPoly {

namespace {

struct SpecializeCore final :
		public ast::WithConstTypeSubstitution,
		public ast::WithDeclsToAdd<>,
		public ast::WithVisitorRef<SpecializeCore> {
	std::string paramPrefix = "_p";

	ast::ApplicationExpr * handleExplicitParams(
		const ast::ApplicationExpr * expr );
	const ast::Expr * createThunkFunction(
		const CodeLocation & location,
		const ast::FunctionType * funType,
		const ast::Expr * actual,
		const ast::InferredParams * inferParams );
	const ast::Expr * doSpecialization(
		const CodeLocation & location,
		const ast::Type * formalType,
		const ast::Expr * actual,
		const ast::InferredParams * inferParams );

	const ast::Expr * postvisit( const ast::ApplicationExpr * expr );
	const ast::Expr * postvisit( const ast::CastExpr * expr );
};

const ast::InferredParams * getInferredParams( const ast::Expr * expr ) {
	const ast::Expr::InferUnion & inferred = expr->inferred;
	if ( inferred.hasParams() ) {
		return &inferred.inferParams();
	} else {
		return nullptr;
	}
}

// Check if both types have the same structure. The leaf (non-tuple) types
// don't have to match but the tuples must match.
bool isTupleStructureMatching( const ast::Type * t0, const ast::Type * t1 ) {
	const ast::TupleType * tt0 = dynamic_cast<const ast::TupleType *>( t0 );
	const ast::TupleType * tt1 = dynamic_cast<const ast::TupleType *>( t1 );
	if ( tt0 && tt1 ) {
		if ( tt0->size() != tt1->size() ) {
			return false;
		}
		for ( auto types : group_iterate( tt0->types, tt1->types ) ) {
			if ( !isTupleStructureMatching(
					std::get<0>( types ), std::get<1>( types ) ) ) {
				return false;
			}
		}
		return true;
	}
	return (!tt0 && !tt1);
}

// The number of elements in a list, if all tuples had been flattened.
size_t flatTypeListSize( const std::vector<ast::ptr<ast::Type>> & types ) {
	size_t sum = 0;
	for ( const ast::ptr<ast::Type> & type : types ) {
		if ( const ast::TupleType * tuple = type.as<ast::TupleType>() ) {
			sum += flatTypeListSize( tuple->types );
		} else {
			sum += 1;
		}
	}
	return sum;
}

// Find the total number of components in a parameter list.
size_t functionParameterSize( const ast::FunctionType * type ) {
	return flatTypeListSize( type->params );
}

bool needsPolySpecialization(
		const ast::Type * /*formalType*/,
		const ast::Type * actualType,
		const ast::TypeSubstitution * subs ) {
	if ( !subs ) {
		return false;
	}

	using namespace ResolvExpr;
	ast::OpenVarSet openVars, closedVars;
	ast::AssertionSet need, have; // unused
	ast::TypeEnvironment env; // unused
	// findOpenVars( formalType, openVars, closedVars, need, have, FirstClosed );
	findOpenVars( actualType, openVars, closedVars, need, have, env, FirstOpen );
	for ( const ast::OpenVarSet::value_type & openVar : openVars ) {
		const ast::Type * boundType = subs->lookup( openVar.first );
		// If the variable is not bound, move onto the next variable.
		if ( !boundType ) continue;

		// Is the variable cound to another type variable?
		if ( auto inst = dynamic_cast<const ast::TypeInstType *>( boundType ) ) {
			if ( closedVars.find( *inst ) == closedVars.end() ) {
				return true;
			} else {
				assertf(false, "closed: %s", inst->name.c_str());
			}
		// Otherwise, the variable is bound to a concrete type.
		} else {
			return true;
		}
	}
	// None of the type variables are bound.
	return false;
}

bool needsTupleSpecialization(
		const ast::Type * formalType, const ast::Type * actualType ) {
	// Needs tuple specialization if the structure of the formal type and
	// actual type do not match.

	// This is the case if the formal type has ttype polymorphism, or if the structure  of tuple types
	// between the function do not match exactly.
	if ( const ast::FunctionType * ftype = getFunctionType( formalType ) ) {
		// A pack in the parameter or return type requires specialization.
		if ( ftype->isTtype() ) {
			return true;
		}
		// Conversion of 0 to a function type does not require specialization.
		if ( dynamic_cast<const ast::ZeroType *>( actualType ) ) {
			return false;
		}
		const ast::FunctionType * atype =
			getFunctionType( actualType->stripReferences() );
		assertf( atype,
			"formal type is a function type, but actual type is not: %s",
			toString( actualType ).c_str() );
		// Can't tuple specialize if parameter sizes deeply-differ.
		if ( functionParameterSize( ftype ) != functionParameterSize( atype ) ) {
			return false;
		}
		// If tuple parameter size matches but actual parameter sizes differ
		// then there needs to be specialization.
		if ( ftype->params.size() != atype->params.size() ) {
			return true;
		}
		// Total parameter size can be the same, while individual parameters
		// can have different structure.
		for ( auto pairs : group_iterate( ftype->params, atype->params ) ) {
			if ( !isTupleStructureMatching(
					std::get<0>( pairs ), std::get<1>( pairs ) ) ) {
				return true;
			}
		}
	}
	return false;
}

bool needsSpecialization(
		const ast::Type * formalType, const ast::Type * actualType,
		const ast::TypeSubstitution * subs ) {
	return needsPolySpecialization( formalType, actualType, subs )
		|| needsTupleSpecialization( formalType, actualType );
}

ast::ApplicationExpr * SpecializeCore::handleExplicitParams(
		const ast::ApplicationExpr * expr ) {
	assert( expr->func->result );
	const ast::FunctionType * func = getFunctionType( expr->func->result );
	assert( func );

	ast::ApplicationExpr * mut = ast::mutate( expr );

	std::vector<ast::ptr<ast::Type>>::const_iterator formal;
	std::vector<ast::ptr<ast::Expr>>::iterator actual;
	for ( formal = func->params.begin(), actual = mut->args.begin() ;
			formal != func->params.end() && actual != mut->args.end() ;
			++formal, ++actual ) {
		*actual = doSpecialization( (*actual)->location,
			*formal, *actual, getInferredParams( expr ) );
	}
	return mut;
}

// Explode assuming simple cases: either type is pure tuple (but not tuple
// expr) or type is non-tuple.
template<typename OutputIterator>
void explodeSimple( const CodeLocation & location,
		const ast::Expr * expr, OutputIterator out ) {
	// Recurse on tuple types using index expressions on each component.
	if ( auto tuple = expr->result.as<ast::TupleType>() ) {
		ast::ptr<ast::Expr> cleanup = expr;
		for ( unsigned int i = 0 ; i < tuple->size() ; ++i ) {
			explodeSimple( location,
				new ast::TupleIndexExpr( location, expr, i ), out );
		}
	// For a non-tuple type, output a clone of the expression.
	} else {
		*out++ = expr;
	}
}

// Restructures arguments to match the structure of the formal parameters
// of the actual function. Returns the next structured argument.
template<typename Iterator>
const ast::Expr * structureArg(
		const CodeLocation& location, const ast::ptr<ast::Type> & type,
		Iterator & begin, const Iterator & end ) {
	if ( auto tuple = type.as<ast::TupleType>() ) {
		std::vector<ast::ptr<ast::Expr>> exprs;
		for ( const ast::ptr<ast::Type> & t : *tuple ) {
			exprs.push_back( structureArg( location, t, begin, end ) );
		}
		return new ast::TupleExpr( location, std::move( exprs ) );
	} else {
		assertf( begin != end, "reached the end of the arguments while structuring" );
		return *begin++;
	}
}

struct TypeInstFixer final : public ast::WithShortCircuiting {
	std::map<const ast::TypeDecl *, std::pair<int, int>> typeMap;

	void previsit(const ast::TypeDecl *) { visit_children = false; }
	const ast::TypeInstType * postvisit(const ast::TypeInstType * typeInst) {
		if (typeMap.count(typeInst->base)) {
			ast::TypeInstType * newInst = mutate(typeInst);
			auto const & pair = typeMap[typeInst->base];
			newInst->expr_id = pair.first;
			newInst->formal_usage = pair.second;
			return newInst;
		}
		return typeInst;
	}
};

const ast::Expr * SpecializeCore::createThunkFunction(
		const CodeLocation & location,
		const ast::FunctionType * funType,
		const ast::Expr * actual,
		const ast::InferredParams * inferParams ) {
	// One set of unique names per program.
	static UniqueName thunkNamer("_thunk");

	const ast::FunctionType * newType = ast::deepCopy( funType );
	if ( typeSubs ) {
		// Must replace only occurrences of type variables
		// that occure free in the thunk's type.
		auto result = typeSubs->applyFree( newType );
		newType = result.node.release();
	}

	using DWTVector = std::vector<ast::ptr<ast::DeclWithType>>;
	using DeclVector = std::vector<ast::ptr<ast::TypeDecl>>;

	UniqueName paramNamer( paramPrefix );

	// Create new thunk with same signature as formal type.
	ast::Pass<TypeInstFixer> fixer;
	for (const auto & kv : newType->forall) {
		if (fixer.core.typeMap.count(kv->base)) {
			std::cerr << location << ' ' << kv->base->name
				<< ' ' << kv->expr_id << '_' << kv->formal_usage
				<< ',' << fixer.core.typeMap[kv->base].first
				<< '_' << fixer.core.typeMap[kv->base].second << std::endl;
			assertf(false, "multiple formals in specialize");
		}
		else {
			fixer.core.typeMap[kv->base] = std::make_pair(kv->expr_id, kv->formal_usage);
		}
	}

	ast::CompoundStmt * thunkBody = new ast::CompoundStmt( location );
	ast::FunctionDecl * thunkFunc = new ast::FunctionDecl(
		location,
		thunkNamer.newName(),
		map_range<DeclVector>( newType->forall, []( const ast::TypeInstType * inst ) {
			return ast::deepCopy( inst->base );
		} ),
		map_range<DWTVector>( newType->assertions, []( const ast::VariableExpr * expr ) {
			return ast::deepCopy( expr->var );
		} ),
		map_range<DWTVector>( newType->params, [&location, &paramNamer]( const ast::Type * type ) {
			return new ast::ObjectDecl( location, paramNamer.newName(), ast::deepCopy( type ) );
		} ),
		map_range<DWTVector>( newType->returns, [&location, &paramNamer]( const ast::Type * type ) {
			return new ast::ObjectDecl( location, paramNamer.newName(), ast::deepCopy( type ) );
		} ),
		thunkBody,
		ast::Storage::Classes(),
		ast::Linkage::C
		);

	thunkFunc->fixUniqueId();

	// Thunks may be generated and not used, avoid them.
	thunkFunc->attributes.push_back( new ast::Attribute( "unused" ) );

	// Global thunks must be static to avoid collitions.
	// Nested thunks must not be unique and hence, not static.
	thunkFunc->storage.is_static = !isInFunction();

	// Weave thunk parameters into call to actual function,
	// naming thunk parameters as we go.
	ast::ApplicationExpr * app = new ast::ApplicationExpr( location, actual );

	const ast::FunctionType * actualType = ast::deepCopy( getFunctionType( actual->result ) );
	if ( typeSubs ) {
		// Need to apply the environment to the actual function's type,
		// since it may itself be polymorphic.
		auto result = typeSubs->apply( actualType );
		actualType = result.node.release();
	}

	ast::ptr<ast::FunctionType> actualTypeManager = actualType;

	std::vector<ast::ptr<ast::Expr>> args;
	for ( ast::ptr<ast::DeclWithType> & param : thunkFunc->params ) {
		// Name each thunk parameter and explode it.
		// These are then threaded back into the actual function call.
		ast::DeclWithType * mutParam = ast::mutate( param.get() );
		explodeSimple( location, new ast::VariableExpr( location, mutParam ),
			std::back_inserter( args ) );
	}

	// Walk parameters to the actual function alongside the exploded thunk
	// parameters and restructure the arguments to match the actual parameters.
	std::vector<ast::ptr<ast::Expr>>::iterator
		argBegin = args.begin(), argEnd = args.end();
	for ( const auto & actualArg : actualType->params ) {
		app->args.push_back(
			structureArg( location, actualArg.get(), argBegin, argEnd ) );
	}
	assertf( argBegin == argEnd, "Did not structure all arguments." );

	app->accept(fixer); // this should modify in place

	app->env = ast::TypeSubstitution::newFromExpr( app, typeSubs );
	if ( inferParams ) {
		app->inferred.inferParams() = *inferParams;
	}

	// Handle any specializations that may still be present.
	{
		std::string oldParamPrefix = paramPrefix;
		paramPrefix += "p";
		std::list<ast::ptr<ast::Decl>> oldDecls;
		oldDecls.splice( oldDecls.end(), declsToAddBefore );

		app->accept( *visitor );
		// Write recursive specializations into the thunk body.
		for ( const ast::ptr<ast::Decl> & decl : declsToAddBefore ) {
			thunkBody->push_back( new ast::DeclStmt( decl->location, decl ) );
		}

		declsToAddBefore = std::move( oldDecls );
		paramPrefix = std::move( oldParamPrefix );
	}

	// Add return (or valueless expression) to the thunk.
	ast::Stmt * appStmt;
	if ( funType->returns.empty() ) {
		appStmt = new ast::ExprStmt( app->location, app );
	} else {
		appStmt = new ast::ReturnStmt( app->location, app );
	}
	thunkBody->push_back( appStmt );

	// Add the thunk definition:
	declsToAddBefore.push_back( thunkFunc );

	// Return address of thunk function as replacement expression.
	return new ast::AddressExpr( location,
		new ast::VariableExpr( location, thunkFunc ) );
}

const ast::Expr * SpecializeCore::doSpecialization(
		const CodeLocation & location,
		const ast::Type * formalType,
		const ast::Expr * actual,
		const ast::InferredParams * inferParams ) {
	assertf( actual->result, "attempting to specialize an untyped expression" );
	if ( needsSpecialization( formalType, actual->result, typeSubs ) ) {
		if ( const ast::FunctionType * type = getFunctionType( formalType ) ) {
			if ( const ast::ApplicationExpr * expr =
					dynamic_cast<const ast::ApplicationExpr *>( actual ) ) {
				return createThunkFunction( location, type, expr->func, inferParams );
			} else if ( auto expr =
					dynamic_cast<const ast::VariableExpr *>( actual ) ) {
				return createThunkFunction( location, type, expr, inferParams );
			} else {
				// (I don't even know what that comment means.)
				// This likely won't work, as anything that could build an ApplicationExpr probably hit one of the previous two branches
				return createThunkFunction( location, type, actual, inferParams );
			}
		} else {
			return actual;
		}
	} else {
		return actual;
	}
}

const ast::Expr * SpecializeCore::postvisit(
		const ast::ApplicationExpr * expr ) {
	if ( ast::isIntrinsicCallExpr( expr ) ) {
		return expr;
	}

	// Create thunks for the inferred parameters.
	// This is not needed for intrinsic calls, because they aren't
	// actually passed to the function. It needs to handle explicit params
	// before inferred params so that explicit params do not recieve a
	// changed set of inferParams (and change them again).
	// Alternatively, if order starts to matter then copy expr's inferParams
	// and pass them to handleExplicitParams.
	ast::ApplicationExpr * mut = handleExplicitParams( expr );
	if ( !mut->inferred.hasParams() ) {
		return mut;
	}
	ast::InferredParams & inferParams = mut->inferred.inferParams();
	for ( ast::InferredParams::value_type & inferParam : inferParams ) {
		inferParam.second.expr = doSpecialization(
			inferParam.second.expr->location,
			inferParam.second.formalType,
			inferParam.second.expr,
			getInferredParams( inferParam.second.expr )
		);
	}
	return mut;
}

const ast::Expr * SpecializeCore::postvisit( const ast::CastExpr * expr ) {
	if ( expr->result->isVoid() ) {
		// No specialization if there is no return value.
		return expr;
	}
	const ast::Expr * specialized = doSpecialization(
		expr->location, expr->result, expr->arg, getInferredParams( expr ) );
	if ( specialized != expr->arg ) {
		// Assume that the specialization incorporates the cast.
		return specialized;
	} else {
		return expr;
	}
}

} // namespace

void convertSpecializations( ast::TranslationUnit & translationUnit ) {
	ast::Pass<SpecializeCore>::run( translationUnit );
}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
