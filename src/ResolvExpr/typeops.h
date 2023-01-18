//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// typeops.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 07:28:22 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Oct  1 09:45:00 2019
// Update Count     : 6
//

#pragma once

#include <vector>

#include "Cost.h"
#include "TypeEnvironment.h"
#include "WidenMode.h"
#include "AST/Fwd.hpp"
#include "AST/Node.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"
#include "SynTree/SynTree.h"
#include "SynTree/Type.h"

namespace SymTab {
	class Indexer;
}

namespace ResolvExpr {
	// combos: takes a list of sets and returns a set of lists representing every possible way of forming a list by
	// picking one element out of each set
	template< typename InputIterator, typename OutputIterator >
	void combos( InputIterator begin, InputIterator end, OutputIterator out ) {
		typedef typename InputIterator::value_type SetType;
		typedef typename std::vector< typename SetType::value_type > ListType;

		if ( begin == end )	{
			*out++ = ListType();
			return;
		} // if

		InputIterator current = begin;
		begin++;

		std::vector< ListType > recursiveResult;
		combos( begin, end, back_inserter( recursiveResult ) );

		for ( const auto& i : recursiveResult ) for ( const auto& j : *current ) {
			ListType result;
			std::back_insert_iterator< ListType > inserter = back_inserter( result );
			*inserter++ = j;
			std::copy( i.begin(), i.end(), inserter );
			*out++ = result;
		}
	}

	// in AdjustExprType.cc
	/// Replaces array types with the equivalent pointer, and function types with a pointer-to-function
	void adjustExprType( Type *& type, const TypeEnvironment & env, const SymTab::Indexer & indexer );

	/// Replaces array types with the equivalent pointer, and function types with a pointer-to-function using empty TypeEnvironment and Indexer
	void adjustExprType( Type *& type );

	template< typename ForwardIterator >
	void adjustExprTypeList( ForwardIterator begin, ForwardIterator end, const TypeEnvironment & env, const SymTab::Indexer & indexer ) {
		while ( begin != end ) {
			adjustExprType( *begin++, env, indexer );
		} // while
	}

	/// Replaces array types with equivalent pointer, and function types with a pointer-to-function
	const ast::Type * adjustExprType(
		const ast::Type * type, const ast::TypeEnvironment & env, const ast::SymbolTable & symtab );

	// in CastCost.cc
	Cost castCost( const Type * src, const Type * dest, bool srcIsLvalue,
		const SymTab::Indexer & indexer, const TypeEnvironment & env );
	Cost castCost(
		const ast::Type * src, const ast::Type * dst, bool srcIsLvalue,
		const ast::SymbolTable & symtab, const ast::TypeEnvironment & env );

	// in PtrsAssignable.cc
	int ptrsAssignable( const Type * src, const Type * dest, const TypeEnvironment & env );
	int ptrsAssignable( const ast::Type * src, const ast::Type * dst,
		const ast::TypeEnvironment & env );

	// in PtrsCastable.cc
	int ptrsCastable( const Type * src, const Type * dest, const TypeEnvironment & env, const SymTab::Indexer & indexer );
	int ptrsCastable(
		const ast::Type * src, const ast::Type * dst, const ast::SymbolTable & symtab,
		const ast::TypeEnvironment & env );

	// in CommonType.cc
	Type * commonType( Type * type1, Type * type2, bool widenFirst, bool widenSecond, const SymTab::Indexer & indexer, TypeEnvironment & env, const OpenVarSet & openVars );
	ast::ptr< ast::Type > commonType(
		const ast::ptr< ast::Type > & type1, const ast::ptr< ast::Type > & type2,
			ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
			const ast::OpenVarSet & open, WidenMode widen, const ast::SymbolTable & symtab
	);

	// in PolyCost.cc
	int polyCost( Type * type, const TypeEnvironment & env, const SymTab::Indexer & indexer );
	int polyCost(
		const ast::Type * type, const ast::SymbolTable & symtab, const ast::TypeEnvironment & env );

	// in SpecCost.cc
	int specCost( Type * type );
	int specCost( const ast::Type * type );

	// in Occurs.cc
	bool occurs( const Type * type, const std::string & varName, const TypeEnvironment & env );
	// new AST version in TypeEnvironment.cpp (only place it was used in old AST)

	template<typename Iter>
	bool occursIn( Type* ty, Iter begin, Iter end, const TypeEnvironment & env ) {
		while ( begin != end ) {
			if ( occurs( ty, *begin, env ) ) return true;
			++begin;
		}
		return false;
	}

	/// flatten tuple type into list of types
	template< typename OutputIterator >
	void flatten( Type * type, OutputIterator out ) {
		if ( TupleType * tupleType = dynamic_cast< TupleType * >( type ) ) {
			for ( Type * t : tupleType->get_types() ) {
				flatten( t, out );
			}
		} else {
			*out++ = type->clone();
		}
	}

	/// flatten tuple type into existing list of types
	inline void flatten(
		const ast::Type * type, std::vector< ast::ptr< ast::Type > > & out
	) {
		if ( auto tupleType = dynamic_cast< const ast::TupleType * >( type ) ) {
			for ( const ast::Type * t : tupleType->types ) {
				flatten( t, out );
			}
		} else {
			out.emplace_back( type );
		}
	}

	/// flatten tuple type into list of types
	inline std::vector< ast::ptr< ast::Type > > flatten( const ast::Type * type ) {
		std::vector< ast::ptr< ast::Type > > out;
		out.reserve( type->size() );
		flatten( type, out );
		return out;
	}

	template< typename Iter >
	const ast::Type * tupleFromTypes( Iter crnt, Iter end ) {
		std::vector< ast::ptr< ast::Type > > types;
		while ( crnt != end ) {
			// it is guaranteed that a ttype variable will be bound to a flat tuple, so ensure
			// that this results in a flat tuple
			flatten( *crnt, types );

			++crnt;
		}


		return new ast::TupleType{ std::move(types) };
	}

	inline const ast::Type * tupleFromTypes(
		const std::vector< ast::ptr< ast::Type > > & tys
	) {
		return tupleFromTypes( tys.begin(), tys.end() );
	}

	// in TypeEnvironment.cc
	bool isFtype( const Type * type );
} // namespace ResolvExpr

namespace ast {
	// in TypeEnvironment.cpp
	bool isFtype( const ast::Type * type );
} // namespace ast

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
