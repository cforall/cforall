//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Assert.cc --
//
// Author           : Peter A. Buhr
// Created On       : Thu Aug 18 13:26:59 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Aug 19 17:07:08 2016
// Update Count     : 10
//

#include <cstdarg>  // for va_end, va_list, va_start
#include <cstdio>   // for fprintf, stderr, vfprintf
#include <cstdlib>  // for abort

extern const char * __progname;							// global name of running executable (argv[0])

#define CFA_ASSERT_FMT "*CFA assertion error* \"%s\" from program \"%s\" in \"%s\" at line %d in file \"%s\""

// called by macro assert in assert.h
void __assert_fail( const char *assertion, const char *file, unsigned int line, const char *function ) {
	fprintf( stderr, CFA_ASSERT_FMT ".\n", assertion, __progname, function, line, file );
	abort();
}

// called by macro assertf
void __assert_fail_f( const char *assertion, const char *file, unsigned int line, const char *function, const char *fmt, ... ) {
	fprintf( stderr, CFA_ASSERT_FMT ": ", assertion, __progname, function, line, file );
	va_list args;
	va_start( args, fmt );
	vfprintf( stderr, fmt, args );
	va_end( args );
	fprintf( stderr, "\n" );
	abort();
}

void abort(const char *fmt, ...	) noexcept __attribute__((noreturn, format(printf, 1, 2)));
void abort(const char *fmt, ...	) noexcept {
	va_list args;
	va_start( args, fmt );
	vfprintf( stderr, fmt, args );
	va_end( args );
	fprintf( stderr, "\n" );
	abort();
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End:  //
