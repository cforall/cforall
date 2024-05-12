//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SpecCost.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Tue Oct 02 15:50:00 2018
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jul  3 11:07:00 2019
// Update Count     : 3
//

#include <cassert>
#include <limits>
#include <type_traits>

#include "AST/Pass.hpp"
#include "AST/Type.hpp"

namespace ResolvExpr {

namespace {

const ast::Type * expr_result( const ast::ptr< ast::Expr > & expr ) {
	return expr->result.get();
}

const ast::Type * type_deref( const ast::ptr< ast::Type > & type ) {
	return type.get();
}

/// The specialization counter inner class.
class SpecCounter : public ast::WithShortCircuiting, public ast::WithVisitorRef<SpecCounter> {
	int count = -1;  ///< specialization count (-1 for none)

	// Converts the max value to -1 (none), otherwise increments the value.
	static int toNoneOrInc( int value ) {
		assert( 0 <= value );
		return value < std::numeric_limits<int>::max() ? value + 1 : -1;
	}

	template<typename T> using MapperT =
		typename std::add_pointer<ast::Type const *(typename T::value_type const &)>::type;

	// Update the minimum to the new lowest non-none value.
	template<typename T>
	void updateMinimumPresent( int & minimum, const T & list, MapperT<T> mapper ) {
		for ( const auto & node : list ) {
			count = -1;

			if ( ast::Type const * type = mapper( node ) ) {
				ast::Type const * newType = type->accept( *visitor );
				assert( newType == nullptr || newType == type );
			}

			if ( count != -1 && count < minimum ) minimum = count;
		}
	}

	// Returns minimum non-negative count + 1 over type parameters (-1 if none such).
	template<typename T>
	int minimumPresent( const T & list, MapperT<T> mapper ) {
		int minCount = std::numeric_limits<int>::max();
		updateMinimumPresent( minCount, list, mapper );
		return toNoneOrInc( minCount );
	}

public:
	int result() const { return 0 <= count ? count : 0; }

	// Mark specialization of base type.
	void postvisit( const ast::PointerType * ) { if ( 0 <= count ) ++count; }
	void postvisit( const ast::ArrayType * ) { if ( 0 <= count ) ++count; }
	void postvisit( const ast::ReferenceType * ) { if ( 0 <= count ) ++count; }

	void postvisit( const ast::StructInstType * ) { if ( 0 <= count ) ++count; }
	void postvisit( const ast::UnionInstType * ) { if ( 0 <= count ) ++count; }

	// Use the minimal specialization value over returns and params.
	void previsit( const ast::FunctionType * fty ) {
		int minCount = std::numeric_limits<int>::max();
		updateMinimumPresent( minCount, fty->params, type_deref );
		updateMinimumPresent( minCount, fty->returns, type_deref );
		// Add another level to minCount if set.
		count = toNoneOrInc( minCount );
		// We have already visited children.
		visit_children = false;
	}

	// Look for polymorphic parameters.
	void previsit( const ast::StructInstType * sty ) {
		count = minimumPresent( sty->params, expr_result );
	}

	// Look for polymorphic parameters.
	void previsit( const ast::UnionInstType * uty ) {
		count = minimumPresent( uty->params, expr_result );
	}

	// Note polymorphic type (which may be specialized).
	// xxx - maybe account for open/closed type variables
	void postvisit( const ast::TypeInstType * ) { count = 0; }

	// Use the minimal specialization over elements.
	// xxx - maybe don't increment, tuple flattening doesn't necessarily specialize
	void previsit( const ast::TupleType * tty ) {
		count = minimumPresent( tty->types, type_deref );
		visit_children = false;
	}
};

} // namespace

int specCost( const ast::Type * type ) {
	return ( nullptr == type ) ? 0 : ast::Pass<SpecCounter>::read( type );
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
