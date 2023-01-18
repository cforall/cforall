//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Unify.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:27:10 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:43:05 2019
// Update Count     : 46
//

#include "Unify.h"

#include <cassert>                  // for assertf, assert
#include <iterator>                 // for back_insert_iterator, back_inserter
#include <map>                      // for _Rb_tree_const_iterator, _Rb_tree_i...
#include <memory>                   // for unique_ptr
#include <set>                      // for set
#include <string>                   // for string, operator==, operator!=, bas...
#include <utility>                  // for pair, move
#include <vector>

#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"
#include "Common/PassVisitor.h"     // for PassVisitor
#include "FindOpenVars.h"           // for findOpenVars
#include "SynTree/LinkageSpec.h"    // for C
#include "SynTree/Constant.h"       // for Constant
#include "SynTree/Declaration.h"    // for TypeDecl, TypeDecl::Data, Declarati...
#include "SynTree/Expression.h"     // for TypeExpr, Expression, ConstantExpr
#include "SynTree/Mutator.h"        // for Mutator
#include "SynTree/Type.h"           // for Type, TypeInstType, FunctionType
#include "SynTree/Visitor.h"        // for Visitor
#include "Tuples/Tuples.h"          // for isTtype
#include "TypeEnvironment.h"        // for EqvClass, AssertionSet, OpenVarSet
#include "typeops.h"                // for flatten, occurs, commonType

namespace ast {
	class SymbolTable;
}

namespace SymTab {
	class Indexer;
}  // namespace SymTab

// #define DEBUG

namespace ResolvExpr {

// Template Helpers:
template< typename Iterator1, typename Iterator2 >
bool unifyList( Iterator1 list1Begin, Iterator1 list1End, Iterator2 list2Begin, Iterator2 list2End, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer, std::list< Type* > &commonTypes ) {
	for ( ; list1Begin != list1End && list2Begin != list2End; ++list1Begin, ++list2Begin ) {
		Type *commonType = 0;
		if ( ! unify( *list1Begin, *list2Begin, env, needAssertions, haveAssertions, openVars, indexer, commonType ) ) {
			return false;
		} // if
		commonTypes.push_back( commonType );
	} // for
	return ( list1Begin == list1End && list2Begin == list2End );
}

template< typename Iterator1, typename Iterator2 >
bool unifyList( Iterator1 list1Begin, Iterator1 list1End, Iterator2 list2Begin, Iterator2 list2End, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer ) {
	std::list< Type* > commonTypes;
	if ( unifyList( list1Begin, list1End, list2Begin, list2End, env, needAssertions, haveAssertions,  openVars, indexer, commonTypes ) ) {
		deleteAll( commonTypes );
		return true;
	} else {
		return false;
	} // if
}

	struct Unify_old : public WithShortCircuiting {
		Unify_old( Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer );

		bool get_result() const { return result; }

		void previsit( BaseSyntaxNode * ) { visit_children = false; }

		void postvisit( VoidType * voidType );
		void postvisit( BasicType * basicType );
		void postvisit( PointerType * pointerType );
		void postvisit( ArrayType * arrayType );
		void postvisit( ReferenceType * refType );
		void postvisit( FunctionType * functionType );
		void postvisit( StructInstType * aggregateUseType );
		void postvisit( UnionInstType * aggregateUseType );
		void postvisit( EnumInstType * aggregateUseType );
		void postvisit( TraitInstType * aggregateUseType );
		void postvisit( TypeInstType * aggregateUseType );
		void postvisit( TupleType * tupleType );
		void postvisit( VarArgsType * varArgsType );
		void postvisit( ZeroType * zeroType );
		void postvisit( OneType * oneType );

	  private:
		template< typename RefType > void handleRefType( RefType *inst, Type *other );
		template< typename RefType > void handleGenericRefType( RefType *inst, Type *other );

		bool result;
		Type *type2;				// inherited
		TypeEnvironment &env;
		AssertionSet &needAssertions;
		AssertionSet &haveAssertions;
		const OpenVarSet &openVars;
		WidenMode widen;
		const SymTab::Indexer &indexer;
	};

	/// Attempts an inexact unification of type1 and type2.
	/// Returns false if no such unification; if the types can be unified, sets common (unless they unify exactly and have identical type qualifiers)
	bool unifyInexact( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer, Type *&common );
	bool unifyExact( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer );

	bool unifyExact(
		const ast::Type * type1, const ast::Type * type2, ast::TypeEnvironment & env,
		ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open,
		WidenMode widen, const ast::SymbolTable & symtab );

	bool typesCompatible( const Type * first, const Type * second, const SymTab::Indexer & indexer, const TypeEnvironment & env ) {
		TypeEnvironment newEnv;
		OpenVarSet openVars, closedVars; // added closedVars
		AssertionSet needAssertions, haveAssertions;
		Type * newFirst = first->clone(), * newSecond = second->clone();
		env.apply( newFirst );
		env.apply( newSecond );

		// do we need to do this? Seems like we do, types should be able to be compatible if they
		// have free variables that can unify
		findOpenVars( newFirst, openVars, closedVars, needAssertions, haveAssertions, false );
		findOpenVars( newSecond, openVars, closedVars, needAssertions, haveAssertions, true );

		bool result = unifyExact( newFirst, newSecond, newEnv, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
		delete newFirst;
		delete newSecond;
		return result;
	}

	bool typesCompatible(
			const ast::Type * first, const ast::Type * second, const ast::SymbolTable & symtab,
			const ast::TypeEnvironment & env ) {
		ast::TypeEnvironment newEnv;
		ast::OpenVarSet open, closed;
		ast::AssertionSet need, have;

		ast::ptr<ast::Type> newFirst{ first }, newSecond{ second };
		env.apply( newFirst );
		env.apply( newSecond );

		findOpenVars( newFirst, open, closed, need, have, FirstClosed );
		findOpenVars( newSecond, open, closed, need, have, FirstOpen );

		return unifyExact(newFirst, newSecond, newEnv, need, have, open, noWiden(), symtab );
	}

	bool typesCompatibleIgnoreQualifiers( const Type * first, const Type * second, const SymTab::Indexer &indexer, const TypeEnvironment &env ) {
		TypeEnvironment newEnv;
		OpenVarSet openVars;
		AssertionSet needAssertions, haveAssertions;
		Type *newFirst = first->clone(), *newSecond = second->clone();
		env.apply( newFirst );
		env.apply( newSecond );
		newFirst->get_qualifiers() = Type::Qualifiers();
		newSecond->get_qualifiers() = Type::Qualifiers();

		bool result = unifyExact( newFirst, newSecond, newEnv, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
		delete newFirst;
		delete newSecond;
		return result;
	}

	bool typesCompatibleIgnoreQualifiers(
			const ast::Type * first, const ast::Type * second, const ast::SymbolTable & symtab,
			const ast::TypeEnvironment & env ) {
		ast::TypeEnvironment newEnv;
		ast::OpenVarSet open;
		ast::AssertionSet need, have;

		ast::Type * newFirst  = shallowCopy( first  );
		ast::Type * newSecond = shallowCopy( second );
		if ( auto temp = dynamic_cast<const ast::EnumInstType *>(first) ) {
			if ( !dynamic_cast< const ast::EnumInstType * >( second ) ) {
				const ast::EnumDecl * baseEnum = dynamic_cast<const ast::EnumDecl *>(temp->base.get());
				if ( auto t = baseEnum->base.get() ) {
					newFirst = ast::shallowCopy( t );
				}
			}
		} else if ( auto temp = dynamic_cast<const ast::EnumInstType *>(second) ) {
			const ast::EnumDecl * baseEnum = dynamic_cast<const ast::EnumDecl *>(temp->base.get());
			if ( auto t = baseEnum->base.get() ) {
				newSecond = ast::shallowCopy( t );
			}
		}

		newFirst ->qualifiers = {};
		newSecond->qualifiers = {};
		ast::ptr< ast::Type > t1_(newFirst );
		ast::ptr< ast::Type > t2_(newSecond);

		ast::ptr< ast::Type > subFirst = env.apply(newFirst).node;
		ast::ptr< ast::Type > subSecond = env.apply(newSecond).node;

		return unifyExact(
			subFirst,
			subSecond,
			newEnv, need, have, open, noWiden(), symtab );
	}

	bool unify( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer ) {
		OpenVarSet closedVars;
		findOpenVars( type1, openVars, closedVars, needAssertions, haveAssertions, false );
		findOpenVars( type2, openVars, closedVars, needAssertions, haveAssertions, true );
		Type *commonType = 0;
		if ( unifyInexact( type1, type2, env, needAssertions, haveAssertions, openVars, WidenMode( true, true ), indexer, commonType ) ) {
			if ( commonType ) {
				delete commonType;
			} // if
			return true;
		} else {
			return false;
		} // if
	}

	bool unify( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer, Type *&commonType ) {
		OpenVarSet closedVars;
		findOpenVars( type1, openVars, closedVars, needAssertions, haveAssertions, false );
		findOpenVars( type2, openVars, closedVars, needAssertions, haveAssertions, true );
		return unifyInexact( type1, type2, env, needAssertions, haveAssertions, openVars, WidenMode( true, true ), indexer, commonType );
	}

	bool unifyExact( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer ) {
#ifdef DEBUG
		TypeEnvironment debugEnv( env );
#endif
		if ( type1->get_qualifiers() != type2->get_qualifiers() ) {
			return false;
		}

		bool result;
		TypeInstType *var1 = dynamic_cast< TypeInstType* >( type1 );
		TypeInstType *var2 = dynamic_cast< TypeInstType* >( type2 );
		OpenVarSet::const_iterator entry1, entry2;
		if ( var1 ) {
			entry1 = openVars.find( var1->get_name() );
		} // if
		if ( var2 ) {
			entry2 = openVars.find( var2->get_name() );
		} // if
		bool isopen1 = var1 && ( entry1 != openVars.end() );
		bool isopen2 = var2 && ( entry2 != openVars.end() );

		if ( isopen1 && isopen2 ) {
			if ( entry1->second.kind != entry2->second.kind ) {
				result = false;
			} else {
				result = env.bindVarToVar(
					var1, var2, TypeDecl::Data{ entry1->second, entry2->second }, needAssertions,
					haveAssertions, openVars, widen, indexer );
			}
		} else if ( isopen1 ) {
			result = env.bindVar( var1, type2, entry1->second, needAssertions, haveAssertions, openVars, widen, indexer );
		} else if ( isopen2 ) { // TODO: swap widen values in call, since type positions are flipped?
			result = env.bindVar( var2, type1, entry2->second, needAssertions, haveAssertions, openVars, widen, indexer );
		} else {
			PassVisitor<Unify_old> comparator( type2, env, needAssertions, haveAssertions, openVars, widen, indexer );
			type1->accept( comparator );
			result = comparator.pass.get_result();
		} // if
#ifdef DEBUG
		std::cerr << "============ unifyExact" << std::endl;
		std::cerr << "type1 is ";
		type1->print( std::cerr );
		std::cerr << std::endl << "type2 is ";
		type2->print( std::cerr );
		std::cerr << std::endl << "openVars are ";
		printOpenVarSet( openVars, std::cerr, 8 );
		std::cerr << std::endl << "input env is " << std::endl;
		debugEnv.print( std::cerr, 8 );
		std::cerr << std::endl << "result env is " << std::endl;
		env.print( std::cerr, 8 );
		std::cerr << "result is " << result << std::endl;
#endif
		return result;
	}

	bool unifyExact( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, OpenVarSet &openVars, const SymTab::Indexer &indexer ) {
		return unifyExact( type1, type2, env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
	}

	bool unifyInexact( Type *type1, Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer, Type *&common ) {
		Type::Qualifiers tq1 = type1->get_qualifiers(), tq2 = type2->get_qualifiers();
		type1->get_qualifiers() = Type::Qualifiers();
		type2->get_qualifiers() = Type::Qualifiers();
		bool result;
#ifdef DEBUG
		std::cerr << "unifyInexact type 1 is ";
		type1->print( std::cerr );
		std::cerr << " type 2 is ";
		type2->print( std::cerr );
		std::cerr << std::endl;
#endif
		if ( ! unifyExact( type1, type2, env, needAssertions, haveAssertions, openVars, widen, indexer ) ) {
#ifdef DEBUG
			std::cerr << "unifyInexact: no exact unification found" << std::endl;
#endif
			if ( ( common = commonType( type1, type2, widen.first, widen.second, indexer, env, openVars ) ) ) {
				common->tq = tq1.unify( tq2 );
#ifdef DEBUG
				std::cerr << "unifyInexact: common type is ";
				common->print( std::cerr );
				std::cerr << std::endl;
#endif
				result = true;
			} else {
#ifdef DEBUG
				std::cerr << "unifyInexact: no common type found" << std::endl;
#endif
				result = false;
			} // if
		} else {
			if ( tq1 != tq2 ) {
				if ( ( tq1 > tq2 || widen.first ) && ( tq2 > tq1 || widen.second ) ) {
					common = type1->clone();
					common->tq = tq1.unify( tq2 );
					result = true;
				} else {
					result = false;
				} // if
			} else {
				common = type1->clone();
				common->tq = tq1.unify( tq2 );
				result = true;
			} // if
		} // if
		type1->get_qualifiers() = tq1;
		type2->get_qualifiers() = tq2;
		return result;
	}

	Unify_old::Unify_old( Type *type2, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, const OpenVarSet &openVars, WidenMode widen, const SymTab::Indexer &indexer )
		: result( false ), type2( type2 ), env( env ), needAssertions( needAssertions ), haveAssertions( haveAssertions ), openVars( openVars ), widen( widen ), indexer( indexer ) {
	}

	void Unify_old::postvisit( __attribute__((unused)) VoidType *voidType) {
		result = dynamic_cast< VoidType* >( type2 );
	}

	void Unify_old::postvisit(BasicType *basicType) {
		if ( BasicType *otherBasic = dynamic_cast< BasicType* >( type2 ) ) {
			result = basicType->get_kind() == otherBasic->get_kind();
		} // if
	}

	void markAssertionSet( AssertionSet &assertions, DeclarationWithType *assert ) {
		AssertionSet::iterator i = assertions.find( assert );
		if ( i != assertions.end() ) {
			i->second.isUsed = true;
		} // if
	}

	void markAssertions( AssertionSet &assertion1, AssertionSet &assertion2, Type *type ) {
		for ( std::list< TypeDecl* >::const_iterator tyvar = type->get_forall().begin(); tyvar != type->get_forall().end(); ++tyvar ) {
			for ( std::list< DeclarationWithType* >::const_iterator assert = (*tyvar)->get_assertions().begin(); assert != (*tyvar)->get_assertions().end(); ++assert ) {
				markAssertionSet( assertion1, *assert );
				markAssertionSet( assertion2, *assert );
			} // for
		} // for
	}

	void Unify_old::postvisit(PointerType *pointerType) {
		if ( PointerType *otherPointer = dynamic_cast< PointerType* >( type2 ) ) {
			result = unifyExact( pointerType->get_base(), otherPointer->get_base(), env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			markAssertions( haveAssertions, needAssertions, pointerType );
			markAssertions( haveAssertions, needAssertions, otherPointer );
		} // if
	}

	void Unify_old::postvisit(ReferenceType *refType) {
		if ( ReferenceType *otherRef = dynamic_cast< ReferenceType* >( type2 ) ) {
			result = unifyExact( refType->get_base(), otherRef->get_base(), env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			markAssertions( haveAssertions, needAssertions, refType );
			markAssertions( haveAssertions, needAssertions, otherRef );
		} // if
	}

	void Unify_old::postvisit(ArrayType *arrayType) {
		ArrayType *otherArray = dynamic_cast< ArrayType* >( type2 );
		// to unify, array types must both be VLA or both not VLA
		// and must both have a dimension expression or not have a dimension
		if ( otherArray && arrayType->get_isVarLen() == otherArray->get_isVarLen() ) {

			if ( ! arrayType->get_isVarLen() && ! otherArray->get_isVarLen() &&
				arrayType->get_dimension() != 0 && otherArray->get_dimension() != 0 ) {
				ConstantExpr * ce1 = dynamic_cast< ConstantExpr * >( arrayType->get_dimension() );
				ConstantExpr * ce2 = dynamic_cast< ConstantExpr * >( otherArray->get_dimension() );
				// see C11 Reference Manual 6.7.6.2.6
				// two array types with size specifiers that are integer constant expressions are
				// compatible if both size specifiers have the same constant value
				if ( ce1 && ce2 ) {
					Constant * c1 = ce1->get_constant();
					Constant * c2 = ce2->get_constant();

					if ( c1->get_value() != c2->get_value() ) {
						// does not unify if the dimension is different
						return;
					}
				}
			}

			result = unifyExact( arrayType->get_base(), otherArray->get_base(), env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
		} // if
	}

	template< typename Iterator, typename Func >
	std::unique_ptr<Type> combineTypes( Iterator begin, Iterator end, Func & toType ) {
		std::list< Type * > types;
		for ( ; begin != end; ++begin ) {
			// it's guaranteed that a ttype variable will be bound to a flat tuple, so ensure that this results in a flat tuple
			flatten( toType( *begin ), back_inserter( types ) );
		}
		return std::unique_ptr<Type>( new TupleType( Type::Qualifiers(), types ) );
	}

	template< typename Iterator1, typename Iterator2 >
	bool unifyTypeList( Iterator1 list1Begin, Iterator1 list1End, Iterator2 list2Begin, Iterator2 list2End, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, const OpenVarSet &openVars, const SymTab::Indexer &indexer ) {
		auto get_type = [](DeclarationWithType * dwt){ return dwt->get_type(); };
		for ( ; list1Begin != list1End && list2Begin != list2End; ++list1Begin, ++list2Begin ) {
			Type * t1 = (*list1Begin)->get_type();
			Type * t2 = (*list2Begin)->get_type();
			bool isTtype1 = Tuples::isTtype( t1 );
			bool isTtype2 = Tuples::isTtype( t2 );
			// xxx - assumes ttype must be last parameter
			// xxx - there may be a nice way to refactor this, but be careful because the argument positioning might matter in some cases.
			if ( isTtype1 && ! isTtype2 ) {
				// combine all of the things in list2, then unify
				return unifyExact( t1, combineTypes( list2Begin, list2End, get_type ).get(), env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			} else if ( isTtype2 && ! isTtype1 ) {
				// combine all of the things in list1, then unify
				return unifyExact( combineTypes( list1Begin, list1End, get_type ).get(), t2, env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			} else if ( ! unifyExact( t1, t2, env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer ) ) {
				return false;
			} // if
		} // for
		// may get to the end of one argument list before the end of the other. This is only okay when the other is a ttype
		if ( list1Begin != list1End ) {
			// try unifying empty tuple type with ttype
			Type * t1 = (*list1Begin)->get_type();
			if ( Tuples::isTtype( t1 ) ) {
				return unifyExact( t1, combineTypes( list2Begin, list2End, get_type ).get(), env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			} else return false;
		} else if ( list2Begin != list2End ) {
			// try unifying empty tuple type with ttype
			Type * t2 = (*list2Begin)->get_type();
			if ( Tuples::isTtype( t2 ) ) {
				return unifyExact( combineTypes( list1Begin, list1End, get_type ).get(), t2, env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			} else return false;
		} else {
			return true;
		} // if
	}

	/// Finds ttypes and replaces them with their expansion, if known.
	/// This needs to be done so that satisfying ttype assertions is easier.
	/// If this isn't done then argument lists can have wildly different
	/// size and structure, when they should be compatible.
	struct TtypeExpander_old : public WithShortCircuiting {
		TypeEnvironment & tenv;
		TtypeExpander_old( TypeEnvironment & tenv ) : tenv( tenv ) {}
		void premutate( TypeInstType * ) { visit_children = false; }
		Type * postmutate( TypeInstType * typeInst ) {
			if ( const EqvClass *eqvClass = tenv.lookup( typeInst->get_name() ) ) {
				// expand ttype parameter into its actual type
				if ( eqvClass->data.kind == TypeDecl::Ttype && eqvClass->type ) {
					delete typeInst;
					return eqvClass->type->clone();
				}
			}
			return typeInst;
		}
	};

	/// flattens a list of declarations, so that each tuple type has a single declaration.
	/// makes use of TtypeExpander to ensure ttypes are flat as well.
	void flattenList( std::list< DeclarationWithType * > src, std::list< DeclarationWithType * > & dst, TypeEnvironment & env ) {
		dst.clear();
		for ( DeclarationWithType * dcl : src ) {
			PassVisitor<TtypeExpander_old> expander( env );
			dcl->acceptMutator( expander );
			std::list< Type * > types;
			flatten( dcl->get_type(), back_inserter( types ) );
			for ( Type * t : types ) {
				// outermost const, volatile, _Atomic qualifiers in parameters should not play a role in the unification of function types, since they do not determine whether a function is callable.
				// Note: MUST consider at least mutex qualifier, since functions can be overloaded on outermost mutex and a mutex function has different requirements than a non-mutex function.
				t->get_qualifiers() -= Type::Qualifiers(Type::Const | Type::Volatile | Type::Atomic);

				dst.push_back( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::C, nullptr, t, nullptr ) );
			}
			delete dcl;
		}
	}

	void Unify_old::postvisit(FunctionType *functionType) {
		FunctionType *otherFunction = dynamic_cast< FunctionType* >( type2 );
		if ( otherFunction && functionType->get_isVarArgs() == otherFunction->get_isVarArgs() ) {
			// flatten the parameter lists for both functions so that tuple structure
			// doesn't affect unification. Must be a clone so that the types don't change.
			std::unique_ptr<FunctionType> flatFunc( functionType->clone() );
			std::unique_ptr<FunctionType> flatOther( otherFunction->clone() );
			flattenList( flatFunc->get_parameters(), flatFunc->get_parameters(), env );
			flattenList( flatOther->get_parameters(), flatOther->get_parameters(), env );

			// sizes don't have to match if ttypes are involved; need to be more precise wrt where the ttype is to prevent errors
			if (
					(flatFunc->parameters.size() == flatOther->parameters.size() &&
						flatFunc->returnVals.size() == flatOther->returnVals.size())
					|| flatFunc->isTtype()
					|| flatOther->isTtype()
			) {
				if ( unifyTypeList( flatFunc->parameters.begin(), flatFunc->parameters.end(), flatOther->parameters.begin(), flatOther->parameters.end(), env, needAssertions, haveAssertions, openVars, indexer ) ) {
					if ( unifyTypeList( flatFunc->returnVals.begin(), flatFunc->returnVals.end(), flatOther->returnVals.begin(), flatOther->returnVals.end(), env, needAssertions, haveAssertions, openVars, indexer ) ) {

						// the original types must be used in mark assertions, since pointer comparisons are used
						markAssertions( haveAssertions, needAssertions, functionType );
						markAssertions( haveAssertions, needAssertions, otherFunction );

						result = true;
					} // if
				} // if
			} // if
		} // if
	}

	template< typename RefType >
	void Unify_old::handleRefType( RefType *inst, Type *other ) {
		// check that other type is compatible and named the same
		RefType *otherStruct = dynamic_cast< RefType* >( other );
		result = otherStruct && inst->name == otherStruct->name;
	}

	template< typename RefType >
	void Unify_old::handleGenericRefType( RefType *inst, Type *other ) {
		// Check that other type is compatible and named the same
		handleRefType( inst, other );
		if ( ! result ) return;
		// Check that parameters of types unify, if any
		std::list< Expression* > params = inst->parameters;
		std::list< Expression* > otherParams = ((RefType*)other)->parameters;

		std::list< Expression* >::const_iterator it = params.begin(), jt = otherParams.begin();
		for ( ; it != params.end() && jt != otherParams.end(); ++it, ++jt ) {
			TypeExpr *param = dynamic_cast< TypeExpr* >(*it);
			assertf(param, "Aggregate parameters should be type expressions");
			TypeExpr *otherParam = dynamic_cast< TypeExpr* >(*jt);
			assertf(otherParam, "Aggregate parameters should be type expressions");

			Type* paramTy = param->get_type();
			Type* otherParamTy = otherParam->get_type();

			bool tupleParam = Tuples::isTtype( paramTy );
			bool otherTupleParam = Tuples::isTtype( otherParamTy );

			if ( tupleParam && otherTupleParam ) {
				++it; ++jt;  // skip ttype parameters for break
			} else if ( tupleParam ) {
				// bundle other parameters into tuple to match
				std::list< Type * > binderTypes;

				do {
					binderTypes.push_back( otherParam->get_type()->clone() );
					++jt;

					if ( jt == otherParams.end() ) break;

					otherParam = dynamic_cast< TypeExpr* >(*jt);
					assertf(otherParam, "Aggregate parameters should be type expressions");
				} while (true);

				otherParamTy = new TupleType{ paramTy->get_qualifiers(), binderTypes };
				++it;  // skip ttype parameter for break
			} else if ( otherTupleParam ) {
				// bundle parameters into tuple to match other
				std::list< Type * > binderTypes;

				do {
					binderTypes.push_back( param->get_type()->clone() );
					++it;

					if ( it == params.end() ) break;

					param = dynamic_cast< TypeExpr* >(*it);
					assertf(param, "Aggregate parameters should be type expressions");
				} while (true);

				paramTy = new TupleType{ otherParamTy->get_qualifiers(), binderTypes };
				++jt;  // skip ttype parameter for break
			}

			if ( ! unifyExact( paramTy, otherParamTy, env, needAssertions, haveAssertions, openVars, WidenMode(false, false), indexer ) ) {
				result = false;
				return;
			}

			// ttype parameter should be last
			if ( tupleParam || otherTupleParam ) break;
		}
		result = ( it == params.end() && jt == otherParams.end() );
	}

	void Unify_old::postvisit(StructInstType *structInst) {
		handleGenericRefType( structInst, type2 );
	}

	void Unify_old::postvisit(UnionInstType *unionInst) {
		handleGenericRefType( unionInst, type2 );
	}

	void Unify_old::postvisit(EnumInstType *enumInst) {
		handleRefType( enumInst, type2 );
	}

	void Unify_old::postvisit(TraitInstType *contextInst) {
		handleRefType( contextInst, type2 );
	}

	void Unify_old::postvisit(TypeInstType *typeInst) {
		assert( openVars.find( typeInst->get_name() ) == openVars.end() );
		TypeInstType *otherInst = dynamic_cast< TypeInstType* >( type2 );
		if ( otherInst && typeInst->get_name() == otherInst->get_name() ) {
			result = true;
///   } else {
///     NamedTypeDecl *nt = indexer.lookupType( typeInst->get_name() );
///     if ( nt ) {
///       TypeDecl *type = dynamic_cast< TypeDecl* >( nt );
///       assert( type );
///       if ( type->get_base() ) {
///         result = unifyExact( type->get_base(), typeInst, env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
///       }
///     }
		} // if
	}

	template< typename Iterator1, typename Iterator2 >
	bool unifyList( Iterator1 list1Begin, Iterator1 list1End, Iterator2 list2Begin, Iterator2 list2End, TypeEnvironment &env, AssertionSet &needAssertions, AssertionSet &haveAssertions, const OpenVarSet &openVars, const SymTab::Indexer &indexer ) {
		auto get_type = [](Type * t) { return t; };
		for ( ; list1Begin != list1End && list2Begin != list2End; ++list1Begin, ++list2Begin ) {
			Type * t1 = *list1Begin;
			Type * t2 = *list2Begin;
			bool isTtype1 = Tuples::isTtype( t1 );
			bool isTtype2 = Tuples::isTtype( t2 );
			// xxx - assumes ttype must be last parameter
			// xxx - there may be a nice way to refactor this, but be careful because the argument positioning might matter in some cases.
			if ( isTtype1 && ! isTtype2 ) {
				// combine all of the things in list2, then unify
				return unifyExact( t1, combineTypes( list2Begin, list2End, get_type ).get(), env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			} else if ( isTtype2 && ! isTtype1 ) {
				// combine all of the things in list1, then unify
				return unifyExact( combineTypes( list1Begin, list1End, get_type ).get(), t2, env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			} else if ( ! unifyExact( t1, t2, env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer ) ) {
				return false;
			} // if

		} // for
		if ( list1Begin != list1End ) {
			// try unifying empty tuple type with ttype
			Type * t1 = *list1Begin;
			if ( Tuples::isTtype( t1 ) ) {
				return unifyExact( t1, combineTypes( list2Begin, list2End, get_type ).get(), env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			} else return false;
		} else if ( list2Begin != list2End ) {
			// try unifying empty tuple type with ttype
			Type * t2 = *list2Begin;
			if ( Tuples::isTtype( t2 ) ) {
				return unifyExact( combineTypes( list1Begin, list1End, get_type ).get(), t2, env, needAssertions, haveAssertions, openVars, WidenMode( false, false ), indexer );
			} else return false;
		} else {
			return true;
		} // if
	}

	void Unify_old::postvisit(TupleType *tupleType) {
		if ( TupleType *otherTuple = dynamic_cast< TupleType* >( type2 ) ) {
			std::unique_ptr<TupleType> flat1( tupleType->clone() );
			std::unique_ptr<TupleType> flat2( otherTuple->clone() );
			std::list<Type *> types1, types2;

			PassVisitor<TtypeExpander_old> expander( env );
			flat1->acceptMutator( expander );
			flat2->acceptMutator( expander );

			flatten( flat1.get(), back_inserter( types1 ) );
			flatten( flat2.get(), back_inserter( types2 ) );

			result = unifyList( types1.begin(), types1.end(), types2.begin(), types2.end(), env, needAssertions, haveAssertions, openVars, indexer );
		} // if
	}

	void Unify_old::postvisit( __attribute__((unused)) VarArgsType *varArgsType ) {
		result = dynamic_cast< VarArgsType* >( type2 );
	}

	void Unify_old::postvisit( __attribute__((unused)) ZeroType *zeroType ) {
		result = dynamic_cast< ZeroType* >( type2 );
	}

	void Unify_old::postvisit( __attribute__((unused)) OneType *oneType ) {
		result = dynamic_cast< OneType* >( type2 );
	}

	Type * extractResultType( FunctionType * function ) {
		if ( function->get_returnVals().size() == 0 ) {
			return new VoidType( Type::Qualifiers() );
		} else if ( function->get_returnVals().size() == 1 ) {
			return function->get_returnVals().front()->get_type()->clone();
		} else {
			std::list< Type * > types;
			for ( DeclarationWithType * decl : function->get_returnVals() ) {
				types.push_back( decl->get_type()->clone() );
			} // for
			return new TupleType( Type::Qualifiers(), types );
		}
	}

	namespace {
				/// Replaces ttype variables with their bound types.
		/// If this isn't done when satifying ttype assertions, then argument lists can have
		/// different size and structure when they should be compatible.
		struct TtypeExpander_new : public ast::WithShortCircuiting, public ast::PureVisitor {
			ast::TypeEnvironment & tenv;

			TtypeExpander_new( ast::TypeEnvironment & env ) : tenv( env ) {}

			const ast::Type * postvisit( const ast::TypeInstType * typeInst ) {
				if ( const ast::EqvClass * clz = tenv.lookup( *typeInst ) ) {
					// expand ttype parameter into its actual type
					if ( clz->data.kind == ast::TypeDecl::Ttype && clz->bound ) {
						return clz->bound;
					}
				}
				return typeInst;
			}
		};
	}

	std::vector< ast::ptr< ast::Type > > flattenList(
		const std::vector< ast::ptr< ast::Type > > & src, ast::TypeEnvironment & env
	) {
		std::vector< ast::ptr< ast::Type > > dst;
		dst.reserve( src.size() );
		for ( const auto & d : src ) {
			ast::Pass<TtypeExpander_new> expander{ env };
			// TtypeExpander pass is impure (may mutate nodes in place)
			// need to make nodes shared to prevent accidental mutation
			ast::ptr<ast::Type> dc = d->accept(expander);
			auto types = flatten( dc );
			for ( ast::ptr< ast::Type > & t : types ) {
				// outermost const, volatile, _Atomic qualifiers in parameters should not play
				// a role in the unification of function types, since they do not determine
				// whether a function is callable.
				// NOTE: **must** consider at least mutex qualifier, since functions can be
				// overloaded on outermost mutex and a mutex function has different
				// requirements than a non-mutex function
				remove_qualifiers( t, ast::CV::Const | ast::CV::Volatile | ast::CV::Atomic );
				dst.emplace_back( t );
			}
		}
		return dst;
	}

	class Unify_new final : public ast::WithShortCircuiting {
		const ast::Type * type2;
		ast::TypeEnvironment & tenv;
		ast::AssertionSet & need;
		ast::AssertionSet & have;
		const ast::OpenVarSet & open;
		WidenMode widen;
		const ast::SymbolTable & symtab;
	public:
		static size_t traceId;
		bool result;

		Unify_new(
			const ast::Type * type2, ast::TypeEnvironment & env, ast::AssertionSet & need,
			ast::AssertionSet & have, const ast::OpenVarSet & open, WidenMode widen,
			const ast::SymbolTable & symtab )
		: type2(type2), tenv(env), need(need), have(have), open(open), widen(widen),
		  symtab(symtab), result(false) {}

		void previsit( const ast::Node * ) { visit_children = false; }

		void postvisit( const ast::VoidType * ) {
			result = dynamic_cast< const ast::VoidType * >( type2 );
		}

		void postvisit( const ast::BasicType * basic ) {
			if ( auto basic2 = dynamic_cast< const ast::BasicType * >( type2 ) ) {
				result = basic->kind == basic2->kind;
			}
		}

		void postvisit( const ast::PointerType * pointer ) {
			if ( auto pointer2 = dynamic_cast< const ast::PointerType * >( type2 ) ) {
				result = unifyExact(
					pointer->base, pointer2->base, tenv, need, have, open,
					noWiden(), symtab );
			}
		}

		void postvisit( const ast::ArrayType * array ) {
			auto array2 = dynamic_cast< const ast::ArrayType * >( type2 );
			if ( ! array2 ) return;

			// to unify, array types must both be VLA or both not VLA and both must have a
			// dimension expression or not have a dimension
			if ( array->isVarLen != array2->isVarLen ) return;
			if ( ! array->isVarLen && ! array2->isVarLen
					&& array->dimension && array2->dimension ) {
				auto ce1 = array->dimension.as< ast::ConstantExpr >();
				auto ce2 = array2->dimension.as< ast::ConstantExpr >();

				// see C11 Reference Manual 6.7.6.2.6
				// two array types with size specifiers that are integer constant expressions are
				// compatible if both size specifiers have the same constant value
				if ( ce1 && ce2 && ce1->intValue() != ce2->intValue() ) return;
			}

			result = unifyExact(
				array->base, array2->base, tenv, need, have, open, noWiden(),
				symtab );
		}

		void postvisit( const ast::ReferenceType * ref ) {
			if ( auto ref2 = dynamic_cast< const ast::ReferenceType * >( type2 ) ) {
				result = unifyExact(
					ref->base, ref2->base, tenv, need, have, open, noWiden(),
					symtab );
			}
		}

	private:

		template< typename Iter >
		static bool unifyTypeList(
			Iter crnt1, Iter end1, Iter crnt2, Iter end2, ast::TypeEnvironment & env,
			ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open,
			const ast::SymbolTable & symtab
		) {
			while ( crnt1 != end1 && crnt2 != end2 ) {
				const ast::Type * t1 = *crnt1;
				const ast::Type * t2 = *crnt2;
				bool isTuple1 = Tuples::isTtype( t1 );
				bool isTuple2 = Tuples::isTtype( t2 );

				// assumes here that ttype *must* be last parameter
				if ( isTuple1 && ! isTuple2 ) {
					// combine remainder of list2, then unify
					return unifyExact(
						t1, tupleFromTypes( crnt2, end2 ), env, need, have, open,
						noWiden(), symtab );
				} else if ( ! isTuple1 && isTuple2 ) {
					// combine remainder of list1, then unify
					return unifyExact(
						tupleFromTypes( crnt1, end1 ), t2, env, need, have, open,
						noWiden(), symtab );
				}

				if ( ! unifyExact(
					t1, t2, env, need, have, open, noWiden(), symtab )
				) return false;

				++crnt1; ++crnt2;
			}

			// May get to the end of one argument list before the other. This is only okay if the
			// other is a ttype
			if ( crnt1 != end1 ) {
				// try unifying empty tuple with ttype
				const ast::Type * t1 = *crnt1;
				if ( ! Tuples::isTtype( t1 ) ) return false;
				return unifyExact(
					t1, tupleFromTypes( crnt2, end2 ), env, need, have, open,
					noWiden(), symtab );
			} else if ( crnt2 != end2 ) {
				// try unifying empty tuple with ttype
				const ast::Type * t2 = *crnt2;
				if ( ! Tuples::isTtype( t2 ) ) return false;
				return unifyExact(
					tupleFromTypes( crnt1, end1 ), t2, env, need, have, open,
					noWiden(), symtab );
			}

			return true;
		}

		static bool unifyTypeList(
			const std::vector< ast::ptr< ast::Type > > & list1,
			const std::vector< ast::ptr< ast::Type > > & list2,
			ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
			const ast::OpenVarSet & open, const ast::SymbolTable & symtab
		) {
			return unifyTypeList(
				list1.begin(), list1.end(), list2.begin(), list2.end(), env, need, have, open,
				symtab );
		}

		static void markAssertionSet( ast::AssertionSet & assns, const ast::VariableExpr * assn ) {
			auto i = assns.find( assn );
			if ( i != assns.end() ) {
				i->second.isUsed = true;
			}
		}

		/// mark all assertions in `type` used in both `assn1` and `assn2`
		static void markAssertions(
			ast::AssertionSet & assn1, ast::AssertionSet & assn2,
			const ast::FunctionType * type
		) {
			for ( auto & assert : type->assertions ) {
				markAssertionSet( assn1, assert );
				markAssertionSet( assn2, assert );
			}
		}

	public:
		void postvisit( const ast::FunctionType * func ) {
			auto func2 = dynamic_cast< const ast::FunctionType * >( type2 );
			if ( ! func2 ) return;

			if ( func->isVarArgs != func2->isVarArgs ) return;

			// Flatten the parameter lists for both functions so that tuple structure does not
			// affect unification. Does not actually mutate function parameters.
			auto params = flattenList( func->params, tenv );
			auto params2 = flattenList( func2->params, tenv );

			// sizes don't have to match if ttypes are involved; need to be more precise w.r.t.
			// where the ttype is to prevent errors
			if (
				( params.size() != params2.size() || func->returns.size() != func2->returns.size() )
				&& ! func->isTtype()
				&& ! func2->isTtype()
			) return;

			if ( ! unifyTypeList( params, params2, tenv, need, have, open, symtab ) ) return;
			if ( ! unifyTypeList(
				func->returns, func2->returns, tenv, need, have, open, symtab ) ) return;

			markAssertions( have, need, func );
			markAssertions( have, need, func2 );

			result = true;
		}

	private:
		// Returns: other, cast as XInstType
		// Assigns this->result: whether types are compatible (up to generic parameters)
		template< typename XInstType >
		const XInstType * handleRefType( const XInstType * inst, const ast::Type * other ) {
			// check that the other type is compatible and named the same
			auto otherInst = dynamic_cast< const XInstType * >( other );
			if (otherInst && inst->name == otherInst->name) this->result = otherInst;
			return otherInst;
		}

		/// Creates a tuple type based on a list of TypeExpr
		template< typename Iter >
		static const ast::Type * tupleFromExprs(
			const ast::TypeExpr * param, Iter & crnt, Iter end, ast::CV::Qualifiers qs
		) {
			std::vector< ast::ptr< ast::Type > > types;
			do {
				types.emplace_back( param->type );

				++crnt;
				if ( crnt == end ) break;
				param = strict_dynamic_cast< const ast::TypeExpr * >( crnt->get() );
			} while(true);

			return new ast::TupleType{ std::move(types), qs };
		}

		template< typename XInstType >
		void handleGenericRefType( const XInstType * inst, const ast::Type * other ) {
			// check that other type is compatible and named the same
			const XInstType * otherInst = handleRefType( inst, other );
			if ( ! this->result ) return;

			// check that parameters of types unify, if any
			const std::vector< ast::ptr< ast::Expr > > & params = inst->params;
			const std::vector< ast::ptr< ast::Expr > > & params2 = otherInst->params;

			auto it = params.begin();
			auto jt = params2.begin();
			for ( ; it != params.end() && jt != params2.end(); ++it, ++jt ) {
				auto param = strict_dynamic_cast< const ast::TypeExpr * >( it->get() );
				auto param2 = strict_dynamic_cast< const ast::TypeExpr * >( jt->get() );

				ast::ptr< ast::Type > pty = param->type;
				ast::ptr< ast::Type > pty2 = param2->type;

				bool isTuple = Tuples::isTtype( pty );
				bool isTuple2 = Tuples::isTtype( pty2 );

				if ( isTuple && isTuple2 ) {
					++it; ++jt;  // skip ttype parameters before break
				} else if ( isTuple ) {
					// bundle remaining params into tuple
					pty2 = tupleFromExprs( param2, jt, params2.end(), pty->qualifiers );
					++it;  // skip ttype parameter for break
				} else if ( isTuple2 ) {
					// bundle remaining params into tuple
					pty = tupleFromExprs( param, it, params.end(), pty2->qualifiers );
					++jt;  // skip ttype parameter for break
				}

				if ( ! unifyExact(
						pty, pty2, tenv, need, have, open, noWiden(), symtab ) ) {
					result = false;
					return;
				}

				// ttype parameter should be last
				if ( isTuple || isTuple2 ) break;
			}
			result = it == params.end() && jt == params2.end();
		}

	public:
		void postvisit( const ast::StructInstType * aggrType ) {
			handleGenericRefType( aggrType, type2 );
		}

		void postvisit( const ast::UnionInstType * aggrType ) {
			handleGenericRefType( aggrType, type2 );
		}

		void postvisit( const ast::EnumInstType * aggrType ) {
			handleRefType( aggrType, type2 );
		}

		void postvisit( const ast::TraitInstType * aggrType ) {
			handleRefType( aggrType, type2 );
		}

		void postvisit( const ast::TypeInstType * typeInst ) {
			assert( open.find( *typeInst ) == open.end() );
			handleRefType( typeInst, type2 );
		}

	private:
		/// Creates a tuple type based on a list of Type

		static bool unifyList(
			const std::vector< ast::ptr< ast::Type > > & list1,
			const std::vector< ast::ptr< ast::Type > > & list2, ast::TypeEnvironment & env,
			ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open,
			const ast::SymbolTable & symtab
		) {
			auto crnt1 = list1.begin();
			auto crnt2 = list2.begin();
			while ( crnt1 != list1.end() && crnt2 != list2.end() ) {
				const ast::Type * t1 = *crnt1;
				const ast::Type * t2 = *crnt2;
				bool isTuple1 = Tuples::isTtype( t1 );
				bool isTuple2 = Tuples::isTtype( t2 );

				// assumes ttype must be last parameter
				if ( isTuple1 && ! isTuple2 ) {
					// combine entirety of list2, then unify
					return unifyExact(
						t1, tupleFromTypes( list2 ), env, need, have, open,
						noWiden(), symtab );
				} else if ( ! isTuple1 && isTuple2 ) {
					// combine entirety of list1, then unify
					return unifyExact(
						tupleFromTypes( list1 ), t2, env, need, have, open,
						noWiden(), symtab );
				}

				if ( ! unifyExact(
					t1, t2, env, need, have, open, noWiden(), symtab )
				) return false;

				++crnt1; ++crnt2;
			}

			if ( crnt1 != list1.end() ) {
				// try unifying empty tuple type with ttype
				const ast::Type * t1 = *crnt1;
				if ( ! Tuples::isTtype( t1 ) ) return false;
				// xxx - this doesn't generate an empty tuple, contrary to comment; both ported
				// from Rob's code
				return unifyExact(
						t1, tupleFromTypes( list2 ), env, need, have, open,
						noWiden(), symtab );
			} else if ( crnt2 != list2.end() ) {
				// try unifying empty tuple with ttype
				const ast::Type * t2 = *crnt2;
				if ( ! Tuples::isTtype( t2 ) ) return false;
				// xxx - this doesn't generate an empty tuple, contrary to comment; both ported
				// from Rob's code
				return unifyExact(
						tupleFromTypes( list1 ), t2, env, need, have, open,
						noWiden(), symtab );
			}

			return true;
		}

	public:
		void postvisit( const ast::TupleType * tuple ) {
			auto tuple2 = dynamic_cast< const ast::TupleType * >( type2 );
			if ( ! tuple2 ) return;

			ast::Pass<TtypeExpander_new> expander{ tenv };

			const ast::Type * flat = tuple->accept( expander );
			const ast::Type * flat2 = tuple2->accept( expander );

			auto types = flatten( flat );
			auto types2 = flatten( flat2 );

			result = unifyList( types, types2, tenv, need, have, open, symtab );
		}

		void postvisit( const ast::VarArgsType * ) {
			result = dynamic_cast< const ast::VarArgsType * >( type2 );
		}

		void postvisit( const ast::ZeroType * ) {
			result = dynamic_cast< const ast::ZeroType * >( type2 );
		}

		void postvisit( const ast::OneType * ) {
			result = dynamic_cast< const ast::OneType * >( type2 );
		}

	  private:
		template< typename RefType > void handleRefType( RefType *inst, Type *other );
		template< typename RefType > void handleGenericRefType( RefType *inst, Type *other );
	};

	// size_t Unify_new::traceId = Stats::Heap::new_stacktrace_id("Unify_new");
	bool unify(
			const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2,
			ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
			ast::OpenVarSet & open, const ast::SymbolTable & symtab
	) {
		ast::ptr<ast::Type> common;
		return unify( type1, type2, env, need, have, open, symtab, common );
	}

	bool unify(
			const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2,
			ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
			ast::OpenVarSet & open, const ast::SymbolTable & symtab, ast::ptr<ast::Type> & common
	) {
		ast::OpenVarSet closed;
		findOpenVars( type1, open, closed, need, have, FirstClosed );
		findOpenVars( type2, open, closed, need, have, FirstOpen );
		return unifyInexact(
			type1, type2, env, need, have, open, WidenMode{ true, true }, symtab, common );
	}

	bool unifyExact(
			const ast::Type * type1, const ast::Type * type2, ast::TypeEnvironment & env,
			ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open,
			WidenMode widen, const ast::SymbolTable & symtab
	) {
		if ( type1->qualifiers != type2->qualifiers ) return false;

		auto var1 = dynamic_cast< const ast::TypeInstType * >( type1 );
		auto var2 = dynamic_cast< const ast::TypeInstType * >( type2 );
		ast::OpenVarSet::const_iterator
			entry1 = var1 ? open.find( *var1 ) : open.end(),
			entry2 = var2 ? open.find( *var2 ) : open.end();
		bool isopen1 = entry1 != open.end();
		bool isopen2 = entry2 != open.end();

		if ( isopen1 && isopen2 ) {
			if ( entry1->second.kind != entry2->second.kind ) return false;
			return env.bindVarToVar(
				var1, var2, ast::TypeData{ entry1->second, entry2->second }, need, have,
				open, widen, symtab );
		} else if ( isopen1 ) {
			return env.bindVar( var1, type2, entry1->second, need, have, open, widen, symtab );
		} else if ( isopen2 ) {
			return env.bindVar( var2, type1, entry2->second, need, have, open, widen, symtab );
		} else {
			return ast::Pass<Unify_new>::read(
				type1, type2, env, need, have, open, widen, symtab );
		}
	}

	bool unifyInexact(
			const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2,
			ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
			const ast::OpenVarSet & open, WidenMode widen, const ast::SymbolTable & symtab,
			ast::ptr<ast::Type> & common
	) {
		ast::CV::Qualifiers q1 = type1->qualifiers, q2 = type2->qualifiers;

		// force t1 and t2 to be cloned if their qualifiers must be stripped, so that type1 and
		// type2 are left unchanged; calling convention forces type{1,2}->strong_ref >= 1
		ast::Type * t1 = shallowCopy(type1.get());
		ast::Type * t2 = shallowCopy(type2.get());
		t1->qualifiers = {};
		t2->qualifiers = {};
		ast::ptr< ast::Type > t1_(t1);
		ast::ptr< ast::Type > t2_(t2);

		if ( unifyExact( t1, t2, env, need, have, open, widen, symtab ) ) {
			// if exact unification on unqualified types, try to merge qualifiers
			if ( q1 == q2 || ( ( q1 > q2 || widen.first ) && ( q2 > q1 || widen.second ) ) ) {
				t1->qualifiers = q1 | q2;
				common = t1;
				return true;
			} else {
				return false;
			}

		} else if (( common = commonType( t1, t2, env, need, have, open, widen, symtab ))) {
			// no exact unification, but common type
			auto c = shallowCopy(common.get());
			c->qualifiers = q1 | q2;
			common = c;
			return true;
		} else {
			return false;
		}
	}

	ast::ptr<ast::Type> extractResultType( const ast::FunctionType * func ) {
		if ( func->returns.empty() ) return new ast::VoidType{};
		if ( func->returns.size() == 1 ) return func->returns[0];

		std::vector<ast::ptr<ast::Type>> tys;
		for ( const auto & decl : func->returns ) {
			tys.emplace_back( decl );
		}
		return new ast::TupleType{ std::move(tys) };
	}
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
