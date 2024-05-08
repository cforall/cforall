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
// Last Modified On : Tue May  7 16:41:02 2024
// Update Count     : 22
// 

extern "C" {
#if ! defined( exception )								// nesting ?
#define exception ``exception							// make keyword an identifier
#define __CFA_MATH_H__
#endif

#if __aarch64__ && __GNUC__ == 13						// TEMPORARY: gcc-13 problem on ARM in /usr/include/aarch64-linux-gnu/bits/math-vector.h
typedef double __Float32x4_t;
typedef double __Float64x2_t;
typedef float __SVFloat32_t;
typedef float __SVFloat64_t;
typedef int __SVBool_t;
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
