//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CVQualifiers.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Mon May 13 15:00:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Mon May 13 15:00:00 2019
// Update Count     : 1
//

#pragma once

#include "Bitfield.hpp"

namespace ast {

namespace CV {

	/// Bitflags for qualifiers
	enum {
		Const    = 1 << 0,
		Restrict = 1 << 1,
		Volatile = 1 << 2,
		Mutex    = 1 << 3,
		Atomic   = 1 << 4,
		NumQualifiers = 5
	};

	/// Mask for equivalence-preserving qualfiers
	enum { EquivQualifiers = ~Restrict };

	/// Underlying data for qualifiers
	struct qualifier_flags {
		union {
			unsigned int val;
			struct {
				bool is_const    : 1;
				bool is_restrict : 1;
				bool is_volatile : 1;
				bool is_mutex    : 1;
				bool is_atomic   : 1;
			};
		};

		constexpr qualifier_flags( unsigned int val = 0 ) : val(val) {}
	};

	/// Type qualifiers
	using Qualifiers = bitfield<qualifier_flags>;

	// `restrict` and `lvalue` are ignored by qualifier equivalence.
	// ordering is a subtype relationship over qualifiers, e.g. `int` => `const int` is free

	inline bool operator== ( Qualifiers a, Qualifiers b ) {
		return (a.val & EquivQualifiers) == (b.val & EquivQualifiers);
	}
	inline bool operator!= ( Qualifiers a, Qualifiers b ) {
		return !(a == b);
	}
	inline bool operator<= ( Qualifiers a, Qualifiers b ) {
		return a.is_const    <= b.is_const    // non-const converts to const for free
			&& a.is_volatile <= b.is_volatile // non-volatile converts to volatile for free
			&& a.is_mutex    >= b.is_mutex    // mutex converts to non-mutex for free
			&& a.is_atomic   == b.is_atomic;  // atomicity must be preserved in free conversion
	}
	inline bool operator<  ( Qualifiers a, Qualifiers b ) { return a != b && a <= b; }
	inline bool operator>= ( Qualifiers a, Qualifiers b ) { return b <= a; }
	inline bool operator>  ( Qualifiers a, Qualifiers b ) { return b < a; }

}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
