//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Cost.h --
//
// Author           : Peter Buhr and Aaron Moss
// Created On       : Sun May 17 09:39:50 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jun 21 11:39:13 2019
// Update Count     : 63
//

#pragma once

#include <iostream>
#include <cassert>
#include <climits>

namespace ResolvExpr {

// To maximize performance and space, the 7 resolution costs are packed into a single 64-bit word. However, the
// specialization cost is a negative value so a correction is needed is a few places.

class Cost {
	union {
		struct {
		#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
			// Little-endian => first value is low priority and last is high priority.
			unsigned char padding;					///< unused
			unsigned char referenceCost;			///< reference conversions
			unsigned char specCost;					///< Polymorphic type specializations (type assertions), negative cost
			unsigned char varCost;					///< Count of polymorphic type variables
			unsigned char signCost;					///< Count of safe sign conversions
			unsigned char safeCost;					///< Safe (widening) conversions
			unsigned char polyCost;					///< Count of parameters and return values bound to some poly type
			unsigned char unsafeCost;				///< Unsafe (narrowing) conversions
		#else
			#error Cost BIG_ENDIAN unsupported
		#endif
		} v;
		uint64_t all;
	};
	static const unsigned char correctb = 0xff;		// byte correction for negative spec cost
	static const uint64_t correctw = 0x00'00'00'00'00'ff'00'00; //' word correction for negative spec cost
  public:
	// Compiler adjusts constants for correct endian.
	enum : uint64_t {
		zero      = 0x00'00'00'00'00'ff'00'00,
		infinity  = 0xff'ff'ff'ff'ff'00'ff'ff,
		unsafe    = 0x01'00'00'00'00'ff'00'00,
		poly      = 0x00'01'00'00'00'ff'00'00,
		safe      = 0x00'00'01'00'00'ff'00'00,
		sign      = 0x00'00'00'01'00'ff'00'00,
		var       = 0x00'00'00'00'01'ff'00'00,
		spec      = 0x00'00'00'00'00'fe'00'00,
		reference = 0x00'00'00'00'00'ff'01'00,
	}; //'

	Cost( uint64_t all ) { Cost::all = all; }
	Cost( int unsafeCost, int polyCost, int safeCost, int signCost, int varCost, int specCost, int referenceCost ) {
		// Assume little-endian => first value is low priority and last is high priority.
		v = {
		#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
			(unsigned char)0,						// padding
			(unsigned char)referenceCost,			// low priority
			(unsigned char)(specCost + correctb),	// correct for signedness
			(unsigned char)varCost,
			(unsigned char)signCost,
			(unsigned char)safeCost,
			(unsigned char)polyCost,
			(unsigned char)unsafeCost, 				// high priority
		#else
			#error Cost BIG_ENDIAN unsupported
		#endif
		};
	}

	int get_unsafeCost() const { return v.unsafeCost; }
	int get_polyCost() const { return v.polyCost; }
	int get_safeCost() const { return v.safeCost; }
	int get_signCost() const { return v.signCost; }
	int get_varCost() const { return v.varCost; }
	int get_specCost() const { return -(correctb - v.specCost); }
	int get_referenceCost() const { return v.referenceCost; }

	friend bool operator==( const Cost, const Cost );
	friend bool operator!=( const Cost lhs, const Cost rhs );
	// returns negative for *this < rhs, 0 for *this == rhs, positive for *this > rhs
	int compare( const Cost rhs ) const {
		if ( all == infinity ) return 1;
		if ( rhs.all == infinity ) return -1;
		return all > rhs.all ? 1 : all == rhs.all ? 0 : -1;
	}
	friend bool operator<( const Cost lhs, const Cost rhs );

	friend Cost operator+( const Cost lhs, const Cost rhs );

	Cost operator+=( const Cost rhs ) {
		if ( all == infinity ) return *this;
		if ( rhs.all == infinity ) {
			all = infinity;
			return *this;
		}
		all += rhs.all - correctw;					// correct for negative spec cost
		return *this;
	}

	Cost incUnsafe( int inc = 1 ) {
		if ( all != infinity ) { assert( v.unsafeCost + inc <= UCHAR_MAX ); v.unsafeCost += inc; }
		return *this;
	}

	Cost incPoly( int inc = 1 ) {
		if ( all != infinity ) { assert( v.polyCost + inc <= UCHAR_MAX ); v.polyCost += inc; }
		return *this;
	}

	Cost incSafe( int inc = 1 ) {
		if ( all != infinity ) { assert( v.safeCost + inc <= UCHAR_MAX ); v.safeCost += inc; }
		return *this;
	}

	Cost incSign( int inc = 1 ) {
		if ( all != infinity ) { assert( v.signCost + inc <= UCHAR_MAX ); v.signCost += inc; }
		return *this;
	}

	Cost incVar( int inc = 1 ) {
		if ( all != infinity ) { assert( v.varCost + inc <= UCHAR_MAX ); v.varCost += inc; }
		return *this;
	}

	Cost decSpec( int dec = 1 ) {
		if ( all != infinity ) { assert( v.specCost - dec >= 0 ); v.specCost -= dec; }
		return *this;
	}

	Cost incReference( int inc = 1 ) {
		if ( all != infinity ) { assert( v.referenceCost + inc <= UCHAR_MAX ); v.referenceCost += inc; }
		return *this;
	}

	friend std::ostream & operator<<( std::ostream & os, const Cost cost );
};

inline bool operator==( const Cost lhs, const Cost rhs ) {
	return lhs.all == rhs.all;
}

inline bool operator!=( const Cost lhs, const Cost rhs ) {
	return !( lhs.all == rhs.all );
}

inline bool operator<( const Cost lhs, const Cost rhs ) {
	if ( lhs.all == Cost::infinity ) return false;
	if ( rhs.all == Cost::infinity ) return true;
	return lhs.all < rhs.all;
}

inline Cost operator+( const Cost lhs, const Cost rhs ) {
	if ( lhs.all == Cost::infinity || rhs.all == Cost::infinity ) return Cost{ Cost::infinity };
	return Cost{ lhs.all + rhs.all - Cost::correctw }; // correct for negative spec cost
}

inline std::ostream & operator<<( std::ostream & os, const Cost cost ) {
	return os << "( " << cost.get_unsafeCost() << ", " << cost.get_polyCost() << ", " << cost.get_safeCost()
			  << ", " << cost.get_signCost() << ", " << cost.get_varCost() << ", " << cost.get_specCost()
			  << ", " << cost.get_referenceCost() << " )";
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
