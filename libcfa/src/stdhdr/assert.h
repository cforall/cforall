//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// assert.h --
//
// Author           : Peter A. Buhr
// Created On       : Mon Jul  4 23:25:26 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb  4 12:58:49 2020
// Update Count     : 15
//

#ifdef __cforall
extern "C" {
#endif //__cforall

#include_next <assert.h>

#ifdef NDEBUG
	#define assertf( expr, fmt, ... ) ((void)0)
#else
	#define __STRINGIFY__(str) #str
	#define __VSTRINGIFY__(str) __STRINGIFY__(str)
	#define assertf( expr, fmt, ... ) ((expr) ? ((void)0) : __assert_fail_f(__VSTRINGIFY__(expr), __FILE__, __LINE__, __PRETTY_FUNCTION__, fmt, ## __VA_ARGS__ ))

	void __assert_warn_f( const char assertion[], const char file[], unsigned int line, const char function[], const char fmt[], ... ) __attribute__((format( printf, 5, 6) ));
	void __assert_fail_f( const char assertion[], const char file[], unsigned int line, const char function[], const char fmt[], ... ) __attribute__((noreturn, format( printf, 5, 6) ));
#endif

#if !defined(NDEBUG) && (defined(__CFA_DEBUG__) || defined(__CFA_VERIFY__))
	#define __CFA_WITH_VERIFY__
	#define verify(x) assert(x)
	#define verifyf(x, ...) assertf(x, __VA_ARGS__)
	#define verifyfail(...)
	#define warnf( expr, fmt, ... ) ({ static bool check_once##__LINE__ = false; if( false == check_once##__LINE__ && false == (expr)) { check_once##__LINE__ = true; __assert_warn_f(__VSTRINGIFY__(expr), __FILE__, __LINE__, __PRETTY_FUNCTION__, fmt, ## __VA_ARGS__ ); } })
#else
	#define verify(x)
	#define verifyf(x, ...)
	#define verifyfail(...)
	#define warnf( expr, fmt, ... )
#endif

#ifdef __cforall
} // extern "C"
#endif //__cforall

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
