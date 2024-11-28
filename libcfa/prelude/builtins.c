//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// builtins.c --
//
// Author           : Peter A. Buhr
// Created On       : Fri Jul 21 16:21:03 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Nov  8 17:07:15 2024
// Update Count     : 144
//

#define __cforall_builtins__

// type that wraps a pointer and a destructor-like function - used in generating implicit destructor calls for struct members in user-defined functions
// Note: needs to occur early, because it is used to generate destructor calls during code generation
forall(T &)
struct __Destructor {
	T * object;
	void (*dtor)(T *);
};

// defined destructor in the case that non-generated code wants to use __Destructor
forall(T &)
static inline void ^?{}(__Destructor(T) & x) {
	if (x.object && x.dtor) {
		x.dtor(x.object);
	}
}

// easy interface into __Destructor's destructor for easy codegen purposes
extern "C" {
	forall(T &)
	static inline void __destroy_Destructor(__Destructor(T) * dtor) {
		^(*dtor){};
	}
}

// exception implementation

typedef unsigned long long __cfaabi_abi_exception_type_t;

#include "../src/virtual.h"
#include "../src/exception.h"

void exit( int status, const char fmt[], ... ) __attribute__ (( format(printf, 2, 3), __nothrow__, __leaf__, __noreturn__ ));
void abort( const char fmt[], ... ) __attribute__ (( format(printf, 1, 2), __nothrow__, __leaf__, __noreturn__ ));

forall(T &)
static inline T & identity(T & i) {
	return i;
}

// generator support
struct generator$ {
	inline int;
};

static inline void  ?{}(generator$ & this) { ((int&)this) = 0; }
static inline void ^?{}(generator$ &) {}

forall( T & )
trait is_generator {
      void main(T & this);
      generator$ * get_generator(T & this);
};

forall(T & | is_generator(T))
static inline T & resume(T & gen) {
	main(gen);
	return gen;
}

// implicit increment, decrement if += defined, and implicit not if != defined

// C11 reference manual Section 6.5.16 (page101): "An assignment expression has the value of the left operand after the
// assignment, but is not an lvalue." Hence, return a value not a reference.
static inline {
	forall( T | { T ?+=?( T &, one_t ); } )
	T ++?( T & x ) { return x += 1; }

	forall( T | { T ?+=?( T &, one_t ); } )
	T ?++( T & x ) { T tmp = x; x += 1; return tmp; }

	forall( T | { T ?-=?( T &, one_t ); } )
	T --?( T & x ) { return x -= 1; }

	forall( T | { T ?-=?( T &, one_t ); } )
	T ?--( T & x ) { T tmp = x; x -= 1; return tmp; }

	forall( T | { int ?!=?( T, zero_t ); } )
	int !?( T & x ) { return !( x != 0 ); }
} // distribution

// universal typed pointer constant
static inline forall( DT & ) DT * intptr( uintptr_t addr ) { return (DT *)addr; }
static inline forall( ftype FT ) FT * intptr( uintptr_t addr ) { return (FT *)addr; }

#if defined(__SIZEOF_INT128__)
// constructor for 128-bit numbers (all constants are unsigned as +/- are operators)
static inline void ?{}( unsigned int128 & this, unsigned long int h, unsigned long int l ) {
	this = (unsigned int128)h << 64 | (unsigned int128)l;
} // ?{}
#endif // __SIZEOF_INT128__

// for-control index constraints
// forall( T | { void ?{}( T &, zero_t ); void ?{}( T &, one_t ); T ?+=?( T &, T ); T ?-=?( T &, T ); int ?<?( T, T ); } )
// static inline T __for_control_index_constraints__( T t ) { return t; }

// exponentiation operator implementation

extern "C" {
	float powf( float x, float y );
	double pow( double x, double y );
	long double powl( long double x, long double y );
	float _Complex cpowf( float _Complex x, _Complex float z );
	double _Complex cpow( double _Complex x, _Complex double z );
	long double _Complex cpowl( long double _Complex x, _Complex long double z );
} // extern "C"

static inline {
	float ?\?( float x, float y ) { return powf( x, y ); }
	double ?\?( double x, double y ) { return pow( x, y ); }
	long double ?\?( long double x, long double y ) { return powl( x, y ); }
	float _Complex ?\?( float _Complex x, _Complex float y ) { return cpowf(x, y ); }
	double _Complex ?\?( double _Complex x, _Complex double y ) { return cpow( x, y ); }
	long double _Complex ?\?( long double _Complex x, _Complex long double y ) { return cpowl( x, y ); }
} // distribution

int ?\?( int x, unsigned int y );
long int ?\?( long int x, unsigned long int y );
long long int ?\?( long long int x, unsigned long long int y );
// unsigned computation may be faster and larger
unsigned int ?\?( unsigned int x, unsigned int y );
unsigned long int ?\?( unsigned long int x, unsigned long int y );
unsigned long long int ?\?( unsigned long long int x, unsigned long long int y );

forall( OT | { void ?{}( OT & this, one_t ); OT ?*?( OT, OT ); } ) {
	OT ?\?( OT x, unsigned int y );
	OT ?\?( OT x, unsigned long int y );
	OT ?\?( OT x, unsigned long long int y );
} // distribution

static inline {
	int ?\=?( int & x, unsigned int y ) { x = x \ y; return x; }
	long int ?\=?( long int & x, unsigned long int y ) { x = x \ y; return x; }
	long long int ?\=?( long long int & x, unsigned long long int y ) { x = x \ y; return x; }
	unsigned int ?\=?( unsigned int & x, unsigned int y ) { x = x \ y; return x; }
	unsigned long int ?\=?( unsigned long int & x, unsigned long int y ) { x = x \ y; return x; }
	unsigned long long int ?\=?( unsigned long long int & x, unsigned long long int y ) { x = x \ y; return x; }
} // distribution

struct quasi_void {};
static inline void ?{}(quasi_void &) {}
static inline void ?{}(quasi_void &, quasi_void) {}
static inline void ^?{}(quasi_void &) {}
static inline quasi_void ?=?(quasi_void &, quasi_void & _src) { return _src; }

// Local Variables: //
// mode: c //
// tab-width: 4 //
// End: //
