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
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Jun 29 21:45:53 2016
// Update Count     : 14
//

#include "GenPoly.h"

#include <cassert>                      // for assertf, assert
#include <iostream>                     // for operator<<, ostream, basic_os...
#include <iterator>                     // for back_insert_iterator, back_in...
#include <list>                         // for list, _List_iterator, list<>:...
#include <typeindex>                    // for type_index
#include <utility>                      // for pair
#include <vector>                       // for vector

#include "AST/Type.hpp"
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

		bool hasPolyParams( const std::vector<ast::ptr<ast::Expr>> & params, const ast::TypeSubstitution * env) {
			for (auto &param : params) {
				auto paramType = param.strict_as<ast::TypeExpr>();
				if (isPolyType(paramType->type, env)) return true;
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

		__attribute__((ununsed))
		bool hasPolyParams( const std::vector<ast::ptr<ast::Expr>> & params, const TyVarMap & tyVars, const ast::TypeSubstitution * env) {
			for (auto &param : params) {
				auto paramType = param.strict_as<ast::TypeExpr>();
				if (isPolyType(paramType->type, tyVars, env)) return true;
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

	const ast::Type * replaceTypeInst(const ast::Type * type, const ast::TypeSubstitution * env) {
		if (!env) return type;
		if (auto typeInst = dynamic_cast<const ast::TypeInstType*> (type)) {
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
			if ( tyVars.find( typeInst->get_name() ) != tyVars.end() ) {
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

	const ast::Type * isPolyType(const ast::Type * type, const TyVarMap & tyVars, const ast::TypeSubstitution * env) {
		type = replaceTypeInst( type, env );

		if ( auto typeInst = dynamic_cast< const ast::TypeInstType * >( type ) ) {
			return tyVars.find(typeInst->typeString()) != tyVars.end() ? type : nullptr;
		} else if ( auto arrayType = dynamic_cast< const ast::ArrayType * >( type ) ) {
			return isPolyType( arrayType->base, env );
		} else if ( auto structType = dynamic_cast< const ast::StructInstType* >( type ) ) {
			if ( hasPolyParams( structType->params, env ) ) return type;
		} else if ( auto unionType = dynamic_cast< const ast::UnionInstType* >( type ) ) {
			if ( hasPolyParams( unionType->params, env ) ) return type;
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

	ReferenceToType *isDynRet( FunctionType *function, const TyVarMap &forallTypes ) {
		if ( function->get_returnVals().empty() ) return 0;

		return (ReferenceToType*)isDynType( function->get_returnVals().front()->get_type(), forallTypes );
	}

	ReferenceToType *isDynRet( FunctionType *function ) {
		if ( function->get_returnVals().empty() ) return 0;

		TyVarMap forallTypes( TypeDecl::Data{} );
		makeTyVarMap( function, forallTypes );
		return (ReferenceToType*)isDynType( function->get_returnVals().front()->get_type(), forallTypes );
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
			if ( tyVars.find( typeInstType->get_name() ) != tyVars.end() ) {
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

	namespace {
		// temporary hack to avoid re-implementing anything related to TyVarMap
		// does this work? these two structs have identical definitions.
		inline TypeDecl::Data convData(const ast::TypeDecl::Data & data) {
			return *reinterpret_cast<const TypeDecl::Data *>(&data);
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

	bool needsBoxing( const ast::Type * param, const ast::Type * arg, const TyVarMap &exprTyVars, const ast::TypeSubstitution * env) {
		// is parameter is not polymorphic, don't need to box
		if ( ! isPolyType( param, exprTyVars ) ) return false;
		ast::ptr<ast::Type> newType = arg;
		if ( env ) env->apply( newType );
		// if the argument's type is polymorphic, we don't need to box again!
		return ! isPolyType( newType );
	}

	bool needsBoxing( Type * param, Type * arg, ApplicationExpr * appExpr, const TypeSubstitution * env ) {
		FunctionType * function = getFunctionType( appExpr->function->result );
		assertf( function, "ApplicationExpr has non-function type: %s", toString( appExpr->function->result ).c_str() );
		TyVarMap exprTyVars( TypeDecl::Data{} );
		makeTyVarMap( function, exprTyVars );
		return needsBoxing( param, arg, exprTyVars, env );
	}

	bool needsBoxing( const ast::Type * param, const ast::Type * arg, const ast::ApplicationExpr * appExpr, const ast::TypeSubstitution * env) {
		const ast::FunctionType * function = getFunctionType(appExpr->func->result);
		assertf( function, "ApplicationExpr has non-function type: %s", toString( appExpr->func->result ).c_str() );
		TyVarMap exprTyVars(TypeDecl::Data{});
		makeTyVarMap(function, exprTyVars);
		return needsBoxing(param, arg, exprTyVars, env);

	}

	void addToTyVarMap( TypeDecl * tyVar, TyVarMap &tyVarMap ) {
		tyVarMap.insert( tyVar->name, TypeDecl::Data{ tyVar } );
	}

	void addToTyVarMap( const ast::TypeInstType * tyVar, TyVarMap & tyVarMap) {
		tyVarMap.insert(tyVar->typeString(), convData(ast::TypeDecl::Data{tyVar->base}));
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

	void makeTyVarMap(const ast::Type * type, TyVarMap & tyVarMap) {
		if (auto ptype = dynamic_cast<const ast::FunctionType *>(type)) {
 			for (auto & tyVar : ptype->forall) {
				assert (tyVar);
				addToTyVarMap(tyVar, tyVarMap);
			}
		}
		if (auto pointer = dynamic_cast<const ast::PointerType *>(type)) {
			makeTyVarMap(pointer->base, tyVarMap);
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
