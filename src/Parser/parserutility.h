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
// Last Modified On : Thr Feb 16 12:34:00 2023
// Update Count     : 5
//

#pragma once

class Expression;

Expression *notZeroExpr( Expression *orig );

template< typename T, typename U >
struct maybeBuild_t {
	static T * doit( const U *orig ) {
		if ( orig ) {
			return orig->build();
		} else {
			return 0;
		}
	}
};

template< typename T, typename U >
static inline T * maybeBuild( const U *orig ) {
	return maybeBuild_t<T,U>::doit(orig);
}

template< typename T, typename U >
static inline T * maybeMoveBuild( const U *orig ) {
	T* ret = maybeBuild<T>(orig);
	delete orig;
	return ret;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
