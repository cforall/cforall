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
// Last Modified On : Wed Jan 18 11:54:00 2023
// Update Count     : 7
//

#pragma once

#include <vector>

#include "AST/Type.hpp"

namespace SymTab {
	class Indexer;
}

namespace ResolvExpr {
	class TypeEnvironment;

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
