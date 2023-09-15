//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenPoly.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Oct 24 15:19:00 2022
// Update Count     : 17
//

#include "GenPoly.h"

#include <cassert>                      // for assertf, assert
#include <iostream>                     // for operator<<, ostream, basic_os...
#include <iterator>                     // for back_insert_iterator, back_in...
#include <list>                         // for list, _List_iterator, list<>:...
#include <typeindex>                    // for type_index
#include <utility>                      // for pair
#include <vector>                       // for vector

#include "AST/Expr.hpp"
#include "AST/Type.hpp"
#include "AST/TypeSubstitution.hpp"
#include "GenPoly/ErasableScopedMap.h"  // for ErasableScopedMap<>::const_it...
#include "ResolvExpr/typeops.h"         // for flatten
#include "SynTree/Constant.h"           // for Constant
#include "SynTree/Expression.h"         // for Expression, TypeExpr, Constan...
#include "SynTree/Type.h"               // for Type, StructInstType, UnionIn...
#include "SynTree/TypeSubstitution.h"   // for TypeSubstitution

using namespace std;

namespace GenPoly {
	namespace {
		/// Checks a parameter list for polymorphic parameters; will substitute according to env if present
		bool hasPolyParams( std::list< Expression* >& params, const TypeSubstitution *env ) {
			for ( std::list< Expression* >::iterator param = params.begin(); param != params.end(); ++param ) {
				TypeExpr *paramType = dynamic_cast< TypeExpr* >( *param );
				assertf(paramType, "Aggregate parameters should be type expressions");
				if ( isPolyType( paramType->get_type(), env ) ) return true;
			}
			return false;
		}

		bool hasPolyParams( const std::vector<ast::ptr<ast::Expr>> & params, const ast::TypeSubstitution * env ) {
			for ( auto &param : params ) {
				auto paramType = param.as<ast::TypeExpr>();
				assertf( paramType, "Aggregate parameters should be type expressions" );
				if ( isPolyType( paramType->type, env ) ) return true;
			}
			return false;
		}

		/// Checks a parameter list for polymorphic parameters from tyVars; will substitute according to env if present
		bool hasPolyParams( std::list< Expression* >& params, const TyVarMap &tyVars, const TypeSubstitution *env ) {
			for ( std::list< Expression* >::iterator param = params.begin(); param != params.end(); ++param ) {
				TypeExpr *paramType = dynamic_cast< TypeExpr* >( *param );
				assertf(paramType, "Aggregate parameters should be type expressions");
				if ( isPolyType( paramType->get_type(), tyVars, env ) ) return true;
			}
			return false;
		}

		bool hasPolyParams( const std::vector<ast::ptr<ast::Expr>> & params, const TypeVarMap & typeVars, const ast::TypeSubstitution * env ) {
			for ( auto & param : params ) {
				auto paramType = param.as<ast::TypeExpr>();
				assertf( paramType, "Aggregate parameters should be type expressions" );
				if ( isPolyType( paramType->type, typeVars, env ) ) return true;
			}
			return false;
		}

		/// Checks a parameter list for dynamic-layout parameters from tyVars; will substitute according to env if present
		bool hasDynParams( std::list< Expression* >& params, const TyVarMap &tyVars, const TypeSubstitution *env ) {
			for ( std::list< Expression* >::iterator param = params.begin(); param != params.end(); ++param ) {
				TypeExpr *paramType = dynamic_cast< TypeExpr* >( *param );
				assertf(paramType, "Aggregate parameters should be type expressions");
				if ( isDynType( paramType->get_type(), tyVars, env ) ) return true;
			}
			return false;
		}

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

		/// Checks a parameter list for inclusion of polymorphic parameters; will substitute according to env if present
		bool includesPolyParams( std::list< Expression* >& params, const TypeSubstitution *env ) {
			for ( std::list< Expression* >::iterator param = params.begin(); param != params.end(); ++param ) {
				TypeExpr *paramType = dynamic_cast< TypeExpr* >( *param );
				assertf(paramType, "Aggregate parameters should be type expressions");
				if ( includesPolyType( paramType->get_type(), env ) ) return true;
			}
			return false;
		}

		/// Checks a parameter list for inclusion of polymorphic parameters from tyVars; will substitute according to env if present
		bool includesPolyParams( std::list< Expression* >& params, const TyVarMap &tyVars, const TypeSubstitution *env ) {
			for ( std::list< Expression* >::iterator param = params.begin(); param != params.end(); ++param ) {
				TypeExpr *paramType = dynamic_cast< TypeExpr* >( *param );
				assertf(paramType, "Aggregate parameters should be type expressions");
				if ( includesPolyType( paramType->get_type(), tyVars, env ) ) return true;
			}
			return false;
		}
	}

	Type* replaceTypeInst( Type* type, const TypeSubstitution* env ) {
		if ( ! env ) return type;
		if ( TypeInstType *typeInst = dynamic_cast< TypeInstType* >( type ) ) {
			Type *newType = env->lookup( typeInst->get_name() );
			if ( newType ) return newType;
		}
		return type;
	}

	const Type* replaceTypeInst( const Type* type, const TypeSubstitution* env ) {
		if ( ! env ) return type;
		if ( auto typeInst = dynamic_cast< const TypeInstType* >( type ) ) {
			Type *newType = env->lookup( typeInst->get_name() );
			if ( newType ) return newType;
		}
		return type;
	}

	const ast::Type * replaceTypeInst(const ast::Type * type, const ast::TypeSubstitution * env) {
		if (!env) return type;
		if ( auto typeInst = dynamic_cast<const ast::TypeInstType*>(type) ) {
			auto newType = env->lookup(typeInst);
			if (newType) return newType;
		}
		return type;
	}

	Type *isPolyType( Type *type, const TypeSubstitution *env ) {
		type = replaceTypeInst( type, env );

		if ( dynamic_cast< TypeInstType * >( type ) ) {
			return type;
		} else if ( ArrayType * arrayType = dynamic_cast< ArrayType * >( type ) ) {
			return isPolyType( arrayType->base, env );
		} else if ( StructInstType *structType = dynamic_cast< StructInstType* >( type ) ) {
			if ( hasPolyParams( structType->get_parameters(), env ) ) return type;
		} else if ( UnionInstType *unionType = dynamic_cast< UnionInstType* >( type ) ) {
			if ( hasPolyParams( unionType->get_parameters(), env ) ) return type;
		}
		return 0;
	}

	const ast::Type * isPolyType(const ast::Type * type, const ast::TypeSubstitution * env) {
		type = replaceTypeInst( type, env );

		if ( dynamic_cast< const ast::TypeInstType * >( type ) ) {
			return type;
		} else if ( auto arrayType = dynamic_cast< const ast::ArrayType * >( type ) ) {
			return isPolyType( arrayType->base, env );
		} else if ( auto structType = dynamic_cast< const ast::StructInstType* >( type ) ) {
			if ( hasPolyParams( structType->params, env ) ) return type;
		} else if ( auto unionType = dynamic_cast< const ast::UnionInstType* >( type ) ) {
			if ( hasPolyParams( unionType->params, env ) ) return type;
		}
		return 0;
	}

	Type *isPolyType( Type *type, const TyVarMap &tyVars, const TypeSubstitution *env ) {
		type = replaceTypeInst( type, env );

		if ( TypeInstType *typeInst = dynamic_cast< TypeInstType * >( type ) ) {
			if ( tyVars.contains( typeInst->get_name() ) ) {
				return type;
			}
		} else if ( ArrayType * arrayType = dynamic_cast< ArrayType * >( type ) ) {
			return isPolyType( arrayType->base, tyVars, env );
		} else if ( StructInstType *structType = dynamic_cast< StructInstType* >( type ) ) {
			if ( hasPolyParams( structType->get_parameters(), tyVars, env ) ) return type;
		} else if ( UnionInstType *unionType = dynamic_cast< UnionInstType* >( type ) ) {
			if ( hasPolyParams( unionType->get_parameters(), tyVars, env ) ) return type;
		}
		return 0;
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

	ReferenceToType *isDynType( Type *type, const TyVarMap &tyVars, const TypeSubstitution *env ) {
		type = replaceTypeInst( type, env );

		if ( TypeInstType *typeInst = dynamic_cast< TypeInstType * >( type ) ) {
			auto var = tyVars.find( typeInst->get_name() );
			if ( var != tyVars.end() && var->second.isComplete ) {
				return typeInst;
			}
		} else if ( StructInstType *structType = dynamic_cast< StructInstType* >( type ) ) {
			if ( hasDynParams( structType->get_parameters(), tyVars, env ) ) return structType;
		} else if ( UnionInstType *unionType = dynamic_cast< UnionInstType* >( type ) ) {
			if ( hasDynParams( unionType->get_parameters(), tyVars, env ) ) return unionType;
		}
		return 0;
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
		if ( hasDynParams( inst->params, typeVars, subst ) ) {
			return inst;
		}
	} else if ( auto inst = dynamic_cast<ast::UnionInstType const *>( type ) ) {
		if ( hasDynParams( inst->params, typeVars, subst ) ) {
			return inst;
		}
	}
	return nullptr;
}

	ReferenceToType *isDynRet( FunctionType *function, const TyVarMap &forallTypes ) {
		if ( function->get_returnVals().empty() ) return 0;

		return (ReferenceToType*)isDynType( function->get_returnVals().front()->get_type(), forallTypes );
	}

const ast::BaseInstType *isDynRet(
		const ast::FunctionType * type, const TypeVarMap & typeVars ) {
	if ( type->returns.empty() ) return nullptr;

	return isDynType( type->returns.front(), typeVars );
}

	ReferenceToType *isDynRet( FunctionType *function ) {
		if ( function->get_returnVals().empty() ) return 0;

		TyVarMap forallTypes( TypeDecl::Data{} );
		makeTyVarMap( function, forallTypes );
		return (ReferenceToType*)isDynType( function->get_returnVals().front()->get_type(), forallTypes );
	}

const ast::BaseInstType *isDynRet( const ast::FunctionType * func ) {
	if ( func->returns.empty() ) return nullptr;

	TypeVarMap forallTypes = { ast::TypeData() };
	makeTypeVarMap( func, forallTypes );
	return isDynType( func->returns.front(), forallTypes );
}

	bool needsAdapter( FunctionType *adaptee, const TyVarMap &tyVars ) {
// 		if ( ! adaptee->get_returnVals().empty() && isPolyType( adaptee->get_returnVals().front()->get_type(), tyVars ) ) {
// 			return true;
// 		} // if
		if ( isDynRet( adaptee, tyVars ) ) return true;

		for ( std::list< DeclarationWithType* >::const_iterator innerArg = adaptee->get_parameters().begin(); innerArg != adaptee->get_parameters().end(); ++innerArg ) {
// 			if ( isPolyType( (*innerArg)->get_type(), tyVars ) ) {
			if ( isDynType( (*innerArg)->get_type(), tyVars ) ) {
				return true;
			} // if
		} // for
		return false;
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

	Type *isPolyPtr( Type *type, const TypeSubstitution *env ) {
		type = replaceTypeInst( type, env );

		if ( PointerType *ptr = dynamic_cast< PointerType *>( type ) ) {
			return isPolyType( ptr->get_base(), env );
		}
		return 0;
	}

	Type *isPolyPtr( Type *type, const TyVarMap &tyVars, const TypeSubstitution *env ) {
		type = replaceTypeInst( type, env );

		if ( PointerType *ptr = dynamic_cast< PointerType *>( type ) ) {
			return isPolyType( ptr->get_base(), tyVars, env );
		}
		return 0;
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

	Type * hasPolyBase( Type *type, int *levels, const TypeSubstitution *env ) {
		int dummy;
		if ( ! levels ) { levels = &dummy; }
		*levels = 0;

		while ( true ) {
			type = replaceTypeInst( type, env );

			if ( PointerType *ptr = dynamic_cast< PointerType *>( type ) ) {
				type = ptr->get_base();
				++(*levels);
			} else break;
		}

		return isPolyType( type, env );
	}

	Type * hasPolyBase( Type *type, const TyVarMap &tyVars, int *levels, const TypeSubstitution *env ) {
		int dummy;
		if ( ! levels ) { levels = &dummy; }
		*levels = 0;

		while ( true ) {
			type = replaceTypeInst( type, env );

			if ( PointerType *ptr = dynamic_cast< PointerType *>( type ) ) {
				type = ptr->get_base();
				++(*levels);
			} else break;
		}

		return isPolyType( type, tyVars, env );
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

	bool includesPolyType( Type *type, const TypeSubstitution *env ) {
		type = replaceTypeInst( type, env );

		if ( dynamic_cast< TypeInstType * >( type ) ) {
			return true;
		} else if ( PointerType *pointerType = dynamic_cast< PointerType* >( type ) ) {
			if ( includesPolyType( pointerType->get_base(), env ) ) return true;
		} else if ( StructInstType *structType = dynamic_cast< StructInstType* >( type ) ) {
			if ( includesPolyParams( structType->get_parameters(), env ) ) return true;
		} else if ( UnionInstType *unionType = dynamic_cast< UnionInstType* >( type ) ) {
			if ( includesPolyParams( unionType->get_parameters(), env ) ) return true;
		}
		return false;
	}

	bool includesPolyType( Type *type, const TyVarMap &tyVars, const TypeSubstitution *env ) {
		type = replaceTypeInst( type, env );

		if ( TypeInstType *typeInstType = dynamic_cast< TypeInstType * >( type ) ) {
			if ( tyVars.contains( typeInstType->get_name() ) ) {
				return true;
			}
		} else if ( PointerType *pointerType = dynamic_cast< PointerType* >( type ) ) {
			if ( includesPolyType( pointerType->get_base(), tyVars, env ) ) return true;
		} else if ( StructInstType *structType = dynamic_cast< StructInstType* >( type ) ) {
			if ( includesPolyParams( structType->get_parameters(), tyVars, env ) ) return true;
		} else if ( UnionInstType *unionType = dynamic_cast< UnionInstType* >( type ) ) {
			if ( includesPolyParams( unionType->get_parameters(), tyVars, env ) ) return true;
		}
		return false;
	}

	FunctionType * getFunctionType( Type *ty ) {
		PointerType *ptrType;
		if ( ( ptrType = dynamic_cast< PointerType* >( ty ) ) ) {
			return dynamic_cast< FunctionType* >( ptrType->get_base() ); // pointer if FunctionType, NULL otherwise
		} else {
			return dynamic_cast< FunctionType* >( ty ); // pointer if FunctionType, NULL otherwise
		}
	}

	const ast::FunctionType * getFunctionType( const ast::Type * ty ) {
		if ( auto pty = dynamic_cast< const ast::PointerType * >( ty ) ) {
			return pty->base.as< ast::FunctionType >();
		} else {
			return dynamic_cast< const ast::FunctionType * >( ty );
		}
	}

	VariableExpr * getBaseVar( Expression *expr, int *levels ) {
		int dummy;
		if ( ! levels ) { levels = &dummy; }
		*levels = 0;

		while ( true ) {
			if ( VariableExpr *varExpr = dynamic_cast< VariableExpr* >( expr ) ) {
				return varExpr;
			} else if ( MemberExpr *memberExpr = dynamic_cast< MemberExpr* >( expr ) ) {
				expr = memberExpr->get_aggregate();
			} else if ( AddressExpr *addressExpr = dynamic_cast< AddressExpr* >( expr ) ) {
				expr = addressExpr->get_arg();
			} else if ( UntypedExpr *untypedExpr = dynamic_cast< UntypedExpr* >( expr ) ) {
				// look for compiler-inserted dereference operator
				NameExpr *fn = dynamic_cast< NameExpr* >( untypedExpr->get_function() );
				if ( ! fn || fn->get_name() != std::string("*?") ) return 0;
				expr = *untypedExpr->begin_args();
			} else if ( CommaExpr *commaExpr = dynamic_cast< CommaExpr* >( expr ) ) {
				// copy constructors insert comma exprs, look at second argument which contains the variable
				expr = commaExpr->get_arg2();
				continue;
			} else if ( ConditionalExpr * condExpr = dynamic_cast< ConditionalExpr * >( expr ) ) {
				int lvl1;
				int lvl2;
				VariableExpr * var1 = getBaseVar( condExpr->get_arg2(), &lvl1 );
				VariableExpr * var2 = getBaseVar( condExpr->get_arg3(), &lvl2 );
				if ( lvl1 == lvl2 && var1 && var2 && var1->get_var() == var2->get_var() ) {
					*levels = lvl1;
					return var1;
				}
				break;
			} else break;

			++(*levels);
		}

		return 0;
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

		/// Flattens a declaration list
		template<typename Output>
		void flattenList( list< DeclarationWithType* > src, Output out ) {
			for ( DeclarationWithType* decl : src ) {
				ResolvExpr::flatten( decl->get_type(), out );
			}
		}

		/// Flattens a list of types
		template<typename Output>
		void flattenList( list< Type* > src, Output out ) {
			for ( Type* ty : src ) {
				ResolvExpr::flatten( ty, out );
			}
		}

		/// Flattens a list of types.
		// There is another flattenList in Unify.
		void flattenList( vector<ast::ptr<ast::Type>> const & src,
				vector<ast::ptr<ast::Type>> & out ) {
			for ( auto const & type : src ) {
				ResolvExpr::flatten( type, out );
			}
		}

		/// Checks if two lists of parameters are equal up to polymorphic substitution.
		bool paramListsPolyCompatible( const list< Expression* >& aparams, const list< Expression* >& bparams ) {
			if ( aparams.size() != bparams.size() ) return false;

			for ( list< Expression* >::const_iterator at = aparams.begin(), bt = bparams.begin();
					at != aparams.end(); ++at, ++bt ) {
				TypeExpr *aparam = dynamic_cast< TypeExpr* >(*at);
				assertf(aparam, "Aggregate parameters should be type expressions");
				TypeExpr *bparam = dynamic_cast< TypeExpr* >(*bt);
				assertf(bparam, "Aggregate parameters should be type expressions");

				// xxx - might need to let VoidType be a wildcard here too; could have some voids
				// stuffed in for dtype-statics.
				// if ( is<VoidType>( aparam->get_type() ) || is<VoidType>( bparam->get_type() ) ) continue;
				if ( ! typesPolyCompatible( aparam->get_type(), bparam->get_type() ) ) return false;
			}

			return true;
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
	}

	bool typesPolyCompatible( Type *a, Type *b ) {
		type_index aid{ typeid(*a) };
		// polymorphic types always match
		if ( aid == type_index{typeid(TypeInstType)} ) return true;

		type_index bid{ typeid(*b) };
		// polymorphic types always match
		if ( bid == type_index{typeid(TypeInstType)} ) return true;

		// can't match otherwise if different types
		if ( aid != bid ) return false;

		// recurse through type structure (conditions borrowed from Unify.cc)
		if ( aid == type_index{typeid(BasicType)} ) {
			return as<BasicType>(a)->get_kind() == as<BasicType>(b)->get_kind();
		} else if ( aid == type_index{typeid(PointerType)} ) {
			PointerType *ap = as<PointerType>(a), *bp = as<PointerType>(b);

			// void pointers should match any other pointer type
			return is<VoidType>( ap->get_base() ) || is<VoidType>( bp->get_base() )
				|| typesPolyCompatible( ap->get_base(), bp->get_base() );
		} else if ( aid == type_index{typeid(ReferenceType)} ) {
			ReferenceType *ap = as<ReferenceType>(a), *bp = as<ReferenceType>(b);
			return is<VoidType>( ap->get_base() ) || is<VoidType>( bp->get_base() )
				|| typesPolyCompatible( ap->get_base(), bp->get_base() );
		} else if ( aid == type_index{typeid(ArrayType)} ) {
			ArrayType *aa = as<ArrayType>(a), *ba = as<ArrayType>(b);

			if ( aa->get_isVarLen() ) {
				if ( ! ba->get_isVarLen() ) return false;
			} else {
				if ( ba->get_isVarLen() ) return false;

				ConstantExpr *ad = dynamic_cast<ConstantExpr*>( aa->get_dimension() );
				ConstantExpr *bd = dynamic_cast<ConstantExpr*>( ba->get_dimension() );
				if ( ad && bd
						&& ad->get_constant()->get_value() != bd->get_constant()->get_value() )
					return false;
			}

			return typesPolyCompatible( aa->get_base(), ba->get_base() );
		} else if ( aid == type_index{typeid(FunctionType)} ) {
			FunctionType *af = as<FunctionType>(a), *bf = as<FunctionType>(b);

			vector<Type*> aparams, bparams;
			flattenList( af->get_parameters(), back_inserter( aparams ) );
			flattenList( bf->get_parameters(), back_inserter( bparams ) );
			if ( aparams.size() != bparams.size() ) return false;

			vector<Type*> areturns, breturns;
			flattenList( af->get_returnVals(), back_inserter( areturns ) );
			flattenList( bf->get_returnVals(), back_inserter( breturns ) );
			if ( areturns.size() != breturns.size() ) return false;

			for ( unsigned i = 0; i < aparams.size(); ++i ) {
				if ( ! typesPolyCompatible( aparams[i], bparams[i] ) ) return false;
			}
			for ( unsigned i = 0; i < areturns.size(); ++i ) {
				if ( ! typesPolyCompatible( areturns[i], breturns[i] ) ) return false;
			}
			return true;
		} else if ( aid == type_index{typeid(StructInstType)} ) {
			StructInstType *aa = as<StructInstType>(a), *ba = as<StructInstType>(b);

			if ( aa->get_name() != ba->get_name() ) return false;
			return paramListsPolyCompatible( aa->get_parameters(), ba->get_parameters() );
		} else if ( aid == type_index{typeid(UnionInstType)} ) {
			UnionInstType *aa = as<UnionInstType>(a), *ba = as<UnionInstType>(b);

			if ( aa->get_name() != ba->get_name() ) return false;
			return paramListsPolyCompatible( aa->get_parameters(), ba->get_parameters() );
		} else if ( aid == type_index{typeid(EnumInstType)} ) {
			return as<EnumInstType>(a)->get_name() == as<EnumInstType>(b)->get_name();
		} else if ( aid == type_index{typeid(TraitInstType)} ) {
			return as<TraitInstType>(a)->get_name() == as<TraitInstType>(b)->get_name();
		} else if ( aid == type_index{typeid(TupleType)} ) {
			TupleType *at = as<TupleType>(a), *bt = as<TupleType>(b);

			vector<Type*> atypes, btypes;
			flattenList( at->get_types(), back_inserter( atypes ) );
			flattenList( bt->get_types(), back_inserter( btypes ) );
			if ( atypes.size() != btypes.size() ) return false;

			for ( unsigned i = 0; i < atypes.size(); ++i ) {
				if ( ! typesPolyCompatible( atypes[i], btypes[i] ) ) return false;
			}
			return true;
		} else return true; // VoidType, VarArgsType, ZeroType & OneType just need the same type
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
	// Recurse through type structure (conditions borrowed from Unify.cc).

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

		if ( l->isVarLen ) {
			if ( !r->isVarLen ) return false;
		} else {
			if ( r->isVarLen ) return false;

			auto lc = l->dimension.as<ast::ConstantExpr>();
			auto rc = r->dimension.as<ast::ConstantExpr>();
			if ( lc && rc && lc->intValue() != rc->intValue() ) {
				return false;
			}
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

	bool needsBoxing( Type * param, Type * arg, const TyVarMap &exprTyVars, const TypeSubstitution * env ) {
		// is parameter is not polymorphic, don't need to box
		if ( ! isPolyType( param, exprTyVars ) ) return false;
		Type * newType = arg->clone();
		if ( env ) env->apply( newType );
		std::unique_ptr<Type> manager( newType );
		// if the argument's type is polymorphic, we don't need to box again!
		return ! isPolyType( newType );
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

	bool needsBoxing( Type * param, Type * arg, ApplicationExpr * appExpr, const TypeSubstitution * env ) {
		FunctionType * function = getFunctionType( appExpr->function->result );
		assertf( function, "ApplicationExpr has non-function type: %s", toString( appExpr->function->result ).c_str() );
		TyVarMap exprTyVars( TypeDecl::Data{} );
		makeTyVarMap( function, exprTyVars );
		return needsBoxing( param, arg, exprTyVars, env );
	}

bool needsBoxing(
		const ast::Type * param, const ast::Type * arg,
		const ast::ApplicationExpr * expr,
		const ast::TypeSubstitution * subst ) {
	const ast::FunctionType * function = getFunctionType( expr->func->result );
	assertf( function, "ApplicationExpr has non-function type: %s", toString( expr->func->result ).c_str() );
	TypeVarMap exprTyVars = { ast::TypeData() };
	makeTypeVarMap( function, exprTyVars );
	return needsBoxing( param, arg, exprTyVars, subst );
}

	void addToTyVarMap( TypeDecl * tyVar, TyVarMap &tyVarMap ) {
		tyVarMap.insert( tyVar->name, TypeDecl::Data{ tyVar } );
	}

void addToTypeVarMap( const ast::TypeDecl * decl, TypeVarMap & typeVars ) {
	typeVars.insert( ast::TypeEnvKey( decl, 0, 0 ), ast::TypeData( decl ) );
}

void addToTypeVarMap( const ast::TypeInstType * type, TypeVarMap & typeVars ) {
	typeVars.insert( ast::TypeEnvKey( *type ), ast::TypeData( type->base ) );
}

	void makeTyVarMap( Type *type, TyVarMap &tyVarMap ) {
		for ( Type::ForallList::const_iterator tyVar = type->get_forall().begin(); tyVar != type->get_forall().end(); ++tyVar ) {
			assert( *tyVar );
			addToTyVarMap( *tyVar, tyVarMap );
		}
		if ( PointerType *pointer = dynamic_cast< PointerType* >( type ) ) {
			makeTyVarMap( pointer->get_base(), tyVarMap );
		}
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

	void printTyVarMap( std::ostream &os, const TyVarMap &tyVarMap ) {
		for ( TyVarMap::const_iterator i = tyVarMap.begin(); i != tyVarMap.end(); ++i ) {
			os << i->first << " (" << i->second << ") ";
		} // for
		os << std::endl;
	}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
