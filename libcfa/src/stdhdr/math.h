//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// math.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Mon Jul  4 23:25:26 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Feb  7 19:05:27 2020
// Update Count     : 15
// 

extern "C" {
#if ! defined( exception )								// nesting ?
#define exception ``exception							// make keyword an identifier
#define __CFA_MATH_H__
#endif

#include_next <math.h>									// has internal check for multiple expansion

#if defined( exception ) && defined( __CFA_MATH_H__ )	// reset only if set
#undef exception
#undef __CFA_MATH_H__
#endif
} // extern "C"

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
