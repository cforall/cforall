//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenPoly.cpp -- General GenPoly utilities.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Oct 24 15:19:00 2022
// Update Count     : 17
//

#include "GenPoly.hpp"

#include <cassert>                        // for assertf, assert
#include <iostream>                       // for operator<<, ostream, basic_...
#include <iterator>                       // for back_insert_iterator, back_...
#include <list>                           // for list, _List_iterator, list<...
#include <typeindex>                      // for type_index
#include <utility>                        // for pair
#include <vector>                         // for vector

#include "AST/Expr.hpp"
#include "AST/Type.hpp"
#include "AST/TypeSubstitution.hpp"
#include "Common/Eval.hpp"                // for eval
#include "GenPoly/ErasableScopedMap.hpp"  // for ErasableScopedMap<>::const_...
#include "ResolvExpr/Typeops.hpp"         // for flatten

using namespace std;

namespace GenPoly {

namespace {
	/// Checks a parameter list for polymorphic parameters; will substitute according to env if present.
	bool hasPolyParams( const std::vector<ast::ptr<ast::Expr>> & params, const ast::TypeSubstitution * env ) {
		for ( auto & param : params ) {
			auto paramType = param.as<ast::TypeExpr>();
			assertf( paramType, "Aggregate parameters should be type expressions" );
			if ( isPolyType( paramType->type, env ) ) return true;
		}
		return false;
	}

	/// Checks a parameter list for polymorphic parameters from typeVars; will substitute according to env if present.
	bool hasPolyParams( const std::vector<ast::ptr<ast::Expr>> & params, const TypeVarMap & typeVars, const ast::TypeSubstitution * env ) {
		for ( auto & param : params ) {
			auto paramType = param.as<ast::TypeExpr>();
			assertf( paramType, "Aggregate parameters should be type expressions" );
			if ( isPolyType( paramType->type, typeVars, env ) ) return true;
		}
		return false;
	}

	/// Checks a parameter list for dynamic-layout parameters from tyVars; will substitute according to env if present.
	bool hasDynParams(
			const std::vector<ast::ptr<ast::Expr>> & params,
			const TypeVarMap & typeVars,
			const ast::TypeSubstitution * subst ) {
		for ( ast::ptr<ast::Expr> const & paramExpr : params ) {
			auto param = paramExpr.as<ast::TypeExpr>();
			assertf( param, "Aggregate parameters should be type expressions." );
			if ( isDynType( param->type.get(), typeVars, subst ) ) {
				return true;
			}
		}
		return false;
	}
} // namespace

const ast::Type * replaceTypeInst( const ast::Type * type, const ast::TypeSubstitution * env ) {
	if ( !env ) return type;
	if ( auto typeInst = dynamic_cast<const ast::TypeInstType*>( type ) ) {
		if ( auto newType = env->lookup( typeInst ) ) return newType;
	}
	return type;
}

const ast::Type * isPolyType( const ast::Type * type, const ast::TypeSubstitution * subst ) {
	type = replaceTypeInst( type, subst );

	if ( dynamic_cast< const ast::TypeInstType * >( type ) ) {
		// This case is where the two variants of isPolyType differ.
		return type;
	} else if ( auto arrayType = dynamic_cast< const ast::ArrayType * >( type ) ) {
		return isPolyType( arrayType->base, subst );
	} else if ( auto structType = dynamic_cast< const ast::StructInstType* >( type ) ) {
		if ( hasPolyParams( structType->params, subst ) ) return type;
	} else if ( auto unionType = dynamic_cast< const ast::UnionInstType* >( type ) ) {
		if ( hasPolyParams( unionType->params, subst ) ) return type;
	}
	return nullptr;
}

const ast::Type * isPolyType( const ast::Type * type,
		const TypeVarMap & typeVars, const ast::TypeSubstitution * subst ) {
	type = replaceTypeInst( type, subst );

	if ( auto inst = dynamic_cast< const ast::TypeInstType * >( type ) ) {
		if ( typeVars.contains( *inst ) ) return type;
	} else if ( auto array = dynamic_cast< const ast::ArrayType * >( type ) ) {
		return isPolyType( array->base, typeVars, subst );
	} else if ( auto sue = dynamic_cast< const ast::StructInstType * >( type ) ) {
		if ( hasPolyParams( sue->params, typeVars, subst ) ) return type;
	} else if ( auto sue = dynamic_cast< const ast::UnionInstType * >( type ) ) {
		if ( hasPolyParams( sue->params, typeVars, subst ) ) return type;
	}
	return nullptr;
}

const ast::BaseInstType * isDynType(
		const ast::Type * type, const TypeVarMap & typeVars,
		const ast::TypeSubstitution * subst ) {
	type = replaceTypeInst( type, subst );

	if ( auto inst = dynamic_cast<ast::TypeInstType const *>( type ) ) {
		auto var = typeVars.find( *inst );
		if ( var != typeVars.end() && var->second.isComplete ) {
			return inst;
		}
	} else if ( auto inst = dynamic_cast<ast::StructInstType const *>( type ) ) {
		if ( hasDynParams( inst->params, typeVars, subst ) ) return inst;
	} else if ( auto inst = dynamic_cast<ast::UnionInstType const *>( type ) ) {
		if ( hasDynParams( inst->params, typeVars, subst ) ) return inst;
	}
	return nullptr;
}

const ast::BaseInstType *isDynRet(
		const ast::FunctionType * type, const TypeVarMap & typeVars ) {
	if ( type->returns.empty() ) return nullptr;

	return isDynType( type->returns.front(), typeVars );
}

const ast::BaseInstType *isDynRet( const ast::FunctionType * func ) {
	if ( func->returns.empty() ) return nullptr;

	TypeVarMap forallTypes;
	makeTypeVarMap( func, forallTypes );
	return isDynType( func->returns.front(), forallTypes );
}

bool needsAdapter(
		ast::FunctionType const * adaptee, const TypeVarMap & typeVars ) {
	if ( isDynRet( adaptee, typeVars ) ) return true;

	for ( auto param : adaptee->params ) {
		if ( isDynType( param, typeVars ) ) {
			return true;
		}
	}
	return false;
}

const ast::Type * isPolyPtr(
		const ast::Type * type, const TypeVarMap & typeVars,
		const ast::TypeSubstitution * typeSubs ) {
	type = replaceTypeInst( type, typeSubs );

	if ( auto * ptr = dynamic_cast<ast::PointerType const *>( type ) ) {
		return isPolyType( ptr->base, typeVars, typeSubs );
	}
	return nullptr;
}

ast::Type const * hasPolyBase(
		ast::Type const * type, const TypeVarMap & typeVars,
		int * levels, const ast::TypeSubstitution * subst ) {
	int level_count = 0;

	while ( true ) {
		type = replaceTypeInst( type, subst );

		if ( auto ptr = dynamic_cast<ast::PointerType const *>( type ) ) {
			type = ptr->base;
			++level_count;
		} else {
			break;
		}
	}

	if ( nullptr != levels ) { *levels = level_count; }
	return isPolyType( type, typeVars, subst );
}

const ast::FunctionType * getFunctionType( const ast::Type * ty ) {
	if ( auto pty = dynamic_cast< const ast::PointerType * >( ty ) ) {
		return pty->base.as< ast::FunctionType >();
	} else {
		return dynamic_cast< const ast::FunctionType * >( ty );
	}
}

namespace {
	/// Checks if is a pointer to D
	template<typename D, typename B>
	bool is( const B* p ) { return type_index{typeid(D)} == type_index{typeid(*p)}; }

	/// Converts to a pointer to D without checking for safety
	template<typename D, typename B>
	inline D* as( B* p ) { return reinterpret_cast<D*>(p); }

	template<typename D, typename B>
	inline D const * as( B const * p ) {
		return reinterpret_cast<D const *>( p );
	}

	/// Flattens a list of types.
	void flattenList( vector<ast::ptr<ast::Type>> const & src,
			vector<ast::ptr<ast::Type>> & out ) {
		for ( auto const & type : src ) {
			ResolvExpr::flatten( type, out );
		}
	}

	bool paramListsPolyCompatible(
			std::vector<ast::ptr<ast::Expr>> const & lparams,
			std::vector<ast::ptr<ast::Expr>> const & rparams ) {
		if ( lparams.size() != rparams.size() ) {
			return false;
		}

		for ( auto lparam = lparams.begin(), rparam = rparams.begin() ;
				lparam != lparams.end() ; ++lparam, ++rparam ) {
			ast::TypeExpr const * lexpr = lparam->as<ast::TypeExpr>();
			assertf( lexpr, "Aggregate parameters should be type expressions" );
			ast::TypeExpr const * rexpr = rparam->as<ast::TypeExpr>();
			assertf( rexpr, "Aggregate parameters should be type expressions" );

			// xxx - might need to let VoidType be a wildcard here too; could have some voids
			// stuffed in for dtype-statics.
			// if ( is<VoidType>( lexpr->type() ) || is<VoidType>( bparam->get_type() ) ) continue;
			if ( !typesPolyCompatible( lexpr->type, rexpr->type ) ) {
				return false;
			}
		}

		return true;
	}
} // namespace

// This function, and its helpers following, have logic duplicated from
// unification.  The difference in context is that unification applies where
// the types "must" match, while this variation applies to arbitrary type
// pairs, when an optimization could apply if they happen to match.  This
// variation does not bind type variables.  The helper functions support
// the case for matching ArrayType.
bool typesPolyCompatible( ast::Type const * lhs, ast::Type const * rhs );

static bool exprsPolyCompatibleByStaticValue(
		const ast::Expr * e1, const ast::Expr * e2 ) {
	Evaluation r1 = eval(e1);
	Evaluation r2 = eval(e2);

	if ( !r1.hasKnownValue ) return false;
	if ( !r2.hasKnownValue ) return false;

	if ( r1.knownValue != r2.knownValue ) return false;

	return true;
}

static bool exprsPolyCompatible( ast::Expr const * lhs,
		ast::Expr const * rhs ) {
	type_index const lid = typeid(*lhs);
	type_index const rid = typeid(*rhs);
	if ( lid != rid ) return false;

	if ( exprsPolyCompatibleByStaticValue( lhs, rhs ) ) return true;

	if ( type_index(typeid(ast::CastExpr)) == lid ) {
		ast::CastExpr const * l = as<ast::CastExpr>(lhs);
		ast::CastExpr const * r = as<ast::CastExpr>(rhs);

		// inspect casts' target types
		if ( !typesPolyCompatible(
			l->result, r->result ) ) return false;

		// inspect casts' inner expressions
		return exprsPolyCompatible( l->arg, r->arg );

	} else if ( type_index(typeid(ast::VariableExpr)) == lid ) {
		ast::VariableExpr const * l = as<ast::VariableExpr>(lhs);
		ast::VariableExpr const * r = as<ast::VariableExpr>(rhs);

		assert(l->var);
		assert(r->var);

		// conservative: variable exprs match if their declarations are
		// represented by the same C++ AST object
		return (l->var == r->var);

	} else if ( type_index(typeid(ast::SizeofExpr)) == lid ) {
		ast::SizeofExpr const * l = as<ast::SizeofExpr>(lhs);
		ast::SizeofExpr const * r = as<ast::SizeofExpr>(rhs);

		assert( l->type );
		assert( r->type );

		// mutual recursion with type poly compatibility
		return typesPolyCompatible( l->type, r->type );

	} else {
		// All other forms compare on static value only, done earlier
		return false;
	}
}

bool typesPolyCompatible( ast::Type const * lhs, ast::Type const * rhs ) {
	type_index const lid = typeid(*lhs);

	// Polymorphic types always match:
	if ( type_index(typeid(ast::TypeInstType)) == lid ) return true;

	type_index const rid = typeid(*rhs);
	if ( type_index(typeid(ast::TypeInstType)) == rid ) return true;

	// All other types only match if they are the same type:
	if ( lid != rid ) return false;

	// So remaining types can be examined case by case.
	// Recurse through type structure (conditions duplicated from Unify.cpp).

	if ( type_index(typeid(ast::BasicType)) == lid ) {
		return as<ast::BasicType>(lhs)->kind == as<ast::BasicType>(rhs)->kind;
	} else if ( type_index(typeid(ast::PointerType)) == lid ) {
		ast::PointerType const * l = as<ast::PointerType>(lhs);
		ast::PointerType const * r = as<ast::PointerType>(rhs);

		// void pointers should match any other pointer type.
		return is<ast::VoidType>( l->base.get() )
			|| is<ast::VoidType>( r->base.get() )
			|| typesPolyCompatible( l->base.get(), r->base.get() );
	} else if ( type_index(typeid(ast::ReferenceType)) == lid ) {
		ast::ReferenceType const * l = as<ast::ReferenceType>(lhs);
		ast::ReferenceType const * r = as<ast::ReferenceType>(rhs);

		// void references should match any other reference type.
		return is<ast::VoidType>( l->base.get() )
			|| is<ast::VoidType>( r->base.get() )
			|| typesPolyCompatible( l->base.get(), r->base.get() );
	} else if ( type_index(typeid(ast::ArrayType)) == lid ) {
		ast::ArrayType const * l = as<ast::ArrayType>(lhs);
		ast::ArrayType const * r = as<ast::ArrayType>(rhs);

		if ( l->isVarLen != r->isVarLen ) return false;
		if ( (l->dimension != nullptr) != (r->dimension != nullptr) )
			return false;

		if ( l->dimension ) {
			assert( r->dimension );
			// mutual recursion with expression poly compatibility
			if ( !exprsPolyCompatible(l->dimension, r->dimension) )
				return false;
		}

		return typesPolyCompatible( l->base.get(), r->base.get() );
	} else if ( type_index(typeid(ast::FunctionType)) == lid ) {
		ast::FunctionType const * l = as<ast::FunctionType>(lhs);
		ast::FunctionType const * r = as<ast::FunctionType>(rhs);

		std::vector<ast::ptr<ast::Type>> lparams, rparams;
		flattenList( l->params, lparams );
		flattenList( r->params, rparams );
		if ( lparams.size() != rparams.size() ) return false;
		for ( unsigned i = 0; i < lparams.size(); ++i ) {
			if ( !typesPolyCompatible( lparams[i], rparams[i] ) ) return false;
		}

		std::vector<ast::ptr<ast::Type>> lrets, rrets;
		flattenList( l->returns, lrets );
		flattenList( r->returns, rrets );
		if ( lrets.size() != rrets.size() ) return false;
		for ( unsigned i = 0; i < lrets.size(); ++i ) {
			if ( !typesPolyCompatible( lrets[i], rrets[i] ) ) return false;
		}
		return true;
	} else if ( type_index(typeid(ast::StructInstType)) == lid ) {
		ast::StructInstType const * l = as<ast::StructInstType>(lhs);
		ast::StructInstType const * r = as<ast::StructInstType>(rhs);

		if ( l->name != r->name ) return false;
		return paramListsPolyCompatible( l->params, r->params );
	} else if ( type_index(typeid(ast::UnionInstType)) == lid ) {
		ast::UnionInstType const * l = as<ast::UnionInstType>(lhs);
		ast::UnionInstType const * r = as<ast::UnionInstType>(rhs);

		if ( l->name != r->name ) return false;
		return paramListsPolyCompatible( l->params, r->params );
	} else if ( type_index(typeid(ast::EnumInstType)) == lid ) {
		ast::EnumInstType const * l = as<ast::EnumInstType>(lhs);
		ast::EnumInstType const * r = as<ast::EnumInstType>(rhs);

		return l->name == r->name;
	} else if ( type_index(typeid(ast::TraitInstType)) == lid ) {
		ast::TraitInstType const * l = as<ast::TraitInstType>(lhs);
		ast::TraitInstType const * r = as<ast::TraitInstType>(rhs);

		return l->name == r->name;
	} else if ( type_index(typeid(ast::TupleType)) == lid ) {
		ast::TupleType const * l = as<ast::TupleType>(lhs);
		ast::TupleType const * r = as<ast::TupleType>(rhs);

		std::vector<ast::ptr<ast::Type>> ltypes, rtypes;
		flattenList( l->types, ( ltypes ) );
		flattenList( r->types, ( rtypes ) );
		if ( ltypes.size() != rtypes.size() ) return false;

		for ( unsigned i = 0 ; i < ltypes.size() ; ++i ) {
			if ( !typesPolyCompatible( ltypes[i], rtypes[i] ) ) return false;
		}
		return true;
	// The remaining types (VoidType, VarArgsType, ZeroType & OneType)
	// have no variation so will always be equal.
	} else {
		return true;
	}
}

bool needsBoxing( const ast::Type * param, const ast::Type * arg,
		const TypeVarMap & typeVars, const ast::TypeSubstitution * subst ) {
	// Don't need to box if the parameter is not polymorphic.
	if ( !isPolyType( param, typeVars ) ) return false;

	ast::ptr<ast::Type> newType = arg;
	if ( subst ) {
		int count = subst->apply( newType );
		(void)count;
	}
	// Only need to box if the argument is not also polymorphic.
	return !isPolyType( newType );
}

bool needsBoxing(
		const ast::Type * param, const ast::Type * arg,
		const ast::ApplicationExpr * expr,
		const ast::TypeSubstitution * subst ) {
	const ast::FunctionType * function = getFunctionType( expr->func->result );
	assertf( function, "ApplicationExpr has non-function type: %s", toCString( expr->func->result ) );
	TypeVarMap exprTyVars;
	makeTypeVarMap( function, exprTyVars );
	return needsBoxing( param, arg, exprTyVars, subst );
}

void addToTypeVarMap( const ast::TypeDecl * decl, TypeVarMap & typeVars ) {
	typeVars.insert( ast::TypeEnvKey( decl, 0, 0 ), ast::TypeData( decl ) );
}

void addToTypeVarMap( const ast::TypeInstType * type, TypeVarMap & typeVars ) {
	typeVars.insert( ast::TypeEnvKey( *type ), ast::TypeData( type->base ) );
}

void makeTypeVarMap( const ast::Type * type, TypeVarMap & typeVars ) {
	if ( auto func = dynamic_cast<ast::FunctionType const *>( type ) ) {
		for ( auto & typeVar : func->forall ) {
			assert( typeVar );
			addToTypeVarMap( typeVar, typeVars );
		}
	}
	if ( auto pointer = dynamic_cast<ast::PointerType const *>( type ) ) {
		makeTypeVarMap( pointer->base, typeVars );
	}
}

void makeTypeVarMap( const ast::FunctionDecl * decl, TypeVarMap & typeVars ) {
	for ( auto & typeDecl : decl->type_params ) {
		addToTypeVarMap( typeDecl, typeVars );
	}
}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
