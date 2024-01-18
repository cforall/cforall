//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// parserutility.h -- Collected utilities for the parser.
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 15:31:46 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Apr  4 14:03:00 2023
// Update Count     : 7
//

#pragma once

#include "AST/Copy.hpp"            // for shallowCopy
namespace ast {
	class Expr;
}

ast::Expr * notZeroExpr( ast::Expr *orig );

template< typename T >
static inline auto maybeBuild( T * orig ) -> decltype(orig->build()) {
	return (orig) ? orig->build() : nullptr;
}

template< typename T >
static inline auto maybeMoveBuild( T * orig ) -> decltype(orig->build()) {
	auto ret = maybeBuild<T>(orig);
	delete orig;
	return ret;
}

template<typename node_t>
static inline node_t * maybeCopy( node_t const * node ) {
	return node ? ast::shallowCopy( node ) : nullptr;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
