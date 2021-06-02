//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// StorageClasses.hpp --
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

namespace Storage {

	/// Bitflags for storage classes
	enum {
		Extern      = 1 << 0,
		Static      = 1 << 1,
		Auto        = 1 << 2,
		Register    = 1 << 3,
		ThreadLocal = 1 << 4,
		NumClasses       = 5
	};

	/// Bitflag type for storage classes
	struct class_flags {
		union {
			unsigned int val;
			struct {
				bool is_extern      : 1;
				bool is_static      : 1;
				bool is_auto        : 1;
				bool is_register    : 1;
				bool is_threadlocal : 1;
			};

			// MakeBitfieldPrint( NumClasses )
		};

		constexpr class_flags( unsigned int val = 0 ) : val(val) {}
	};

	using Classes = bitfield<class_flags>;
}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
