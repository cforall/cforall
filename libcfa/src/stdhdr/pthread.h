//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// pthread.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Wed Jun 16 13:39:06 2021
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Feb  3 21:53:26 2022
// Update Count     : 13
// 

// pthread.h and setjmp.h cannot agree on the type of __sigsetjmp:
//
//   extern int __sigsetjmp (struct __jmp_buf_tag *__env, int __savemask) __attribute__ ((__nothrow__));
//   extern int __sigsetjmp (struct __jmp_buf_tag __env[1], int __savemask) __attribute__ ((__nothrow__));
//
// With -Wall, gcc-11 warns about the disagreement unless the CPP directive
//
//    # 1 "/usr/include/pthread.h" 1 3 4
//
// appears, which appears to be witchcraft. Unfortunately, this directive is removed by the CFA preprocessor, so the
// batchtest fails because of the spurious warning message. Hence, the warning is elided.

extern "C" {
#if defined(__GNUC__) && __GNUC__ == 11
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Warray-parameter"
#endif // defined(__GNUC__) && __GNUC__ == 11

#include_next <pthread.h>								// has internal check for multiple expansion

#if defined(__GNUC__) && __GNUC__ == 11
	#pragma GCC diagnostic pop
#endif // defined(__GNUC__) && __GNUC__ == 11
} // extern "C"

// Local Variables: //
// mode: c++ //
// End: //
