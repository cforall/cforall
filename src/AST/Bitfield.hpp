//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Bitfield.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Thu May 9 10:00:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jun 5 10:00:00 2019
// Update Count     : 2
//

#pragma once

#include <strings.h>   // for ffs
#include <type_traits> // for is_unsigned

/// Make a type a bitfield.
/// Include in type definition to add operators. Used to simulate inheritance because union
/// does not allow it. Requires type to have `unsigned val` field
/// @param BFType  Name of containing type
template<typename T>
struct bitfield : public T {
	using T::val;
	using val_t = decltype(val);
	static_assert(sizeof(T) == sizeof(unsigned int), "Type has incorrect size");
	static_assert(std::is_unsigned<val_t>::value, "Bitfield val field is not unsigned.");

	constexpr bitfield() : T( 0 ) {}
	constexpr bitfield( val_t v ) : T( v ) {}

	bool operator[]( val_t i ) const { return val & (1 << i); }
	bool any() const { return val != 0; }
	void reset() { val = 0; }
	int ffs() { return ::ffs( val ) - 1; }

	bitfield operator&=( bitfield other ) {
		val &= other.val; return *this;
	}
	bitfield operator&( bitfield other ) const {
		bitfield q = other;
		q &= *this;
		return q;
	}
	bitfield operator|=( bitfield other ) {
		val |= other.val; return *this;
	}
	bitfield operator|( bitfield other ) const {
		bitfield q = other;
		q |= *this;
		return q;
	}
	bitfield operator-=( bitfield other ) {
		val &= ~other.val; return *this;
	}
};

template<typename T>
inline bool operator==( const bitfield<T> & a, const bitfield<T> & b ) {
	return a.val == b.val;
}

template<typename T>
inline bool operator!=( const bitfield<T> & a, const bitfield<T> & b ) {
	return !(a == b);
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
