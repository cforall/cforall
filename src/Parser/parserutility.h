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
// Last Modified On : Thr Mar  9 12:16:00 2023
// Update Count     : 6
//

#pragma once

class Expression;

Expression *notZeroExpr( Expression *orig );

template< typename T >
static inline auto maybeBuild( const T *orig ) -> decltype(orig->build()) {
	return (orig) ? orig->build() : nullptr;
}

template< typename T >
static inline auto maybeMoveBuild( const T *orig ) -> decltype(orig->build()) {
	auto ret = maybeBuild<T>(orig);
	delete orig;
	return ret;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
