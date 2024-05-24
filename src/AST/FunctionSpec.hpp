//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FunctionSpec.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Thu May 9 10:00:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Thu May 9 10:00:00 2019
// Update Count     : 1
//

#pragma once

#include "Bitfield.hpp"

namespace ast {

namespace Function {

/// Bitflags for function specifiers
enum {
	Inline   = 1 << 0,
	Noreturn = 1 << 1,
	Fortran  = 1 << 2,
};

/// Bitflag type for storage classes
struct spec_flags {
	union {
		unsigned int val;
		struct {
			bool is_inline   : 1;
			bool is_noreturn : 1;
			bool is_fortran  : 1;
		};
	};

	constexpr spec_flags( unsigned int val = 0 ) : val(val) {}
};

using Specs = bitfield<spec_flags>;

}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
