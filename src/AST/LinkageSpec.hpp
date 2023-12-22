//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LinkageSpec.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Thu May 9 10:00:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Thu May 9 10:00:00 2019
// Update Count     : 1
//

#pragma once

#include <string>

#include "Bitfield.hpp"
#include "Common/CodeLocation.h"

namespace ast {

namespace Linkage {

/// Bitflags for linkage specifiers
enum {
	Mangle       = 1 << 0,
	Generate     = 1 << 1,
	Overrideable = 1 << 2,
	Builtin      = 1 << 3,
	Overloadable = 1 << 4,
};

/// Bitflag type for storage classes
struct spec_flags {
	union {
		unsigned int val;
		struct {
			bool is_mangled      : 1;
			bool is_generatable  : 1;
			bool is_overrideable : 1;
			bool is_builtin      : 1;
			bool is_overloadable : 1;
		};
	};

	constexpr spec_flags( unsigned int val ) : val(val) {}
};

using Spec = bitfield<spec_flags>;

/// Updates `spec` based on `cmd` (should be "C" or "Cforall").
Spec update( const CodeLocation & loc, Spec spec, const std::string * cmd );

/// A human-readable name for this spec
std::string name( Spec spec );

// Pre-defined flag combinations

/// C built-in defined in prelude
constexpr Spec Intrinsic  = { Mangle | Generate | Overrideable | Builtin | Overloadable };
/// Ordinary Cforall
constexpr Spec Cforall    = { Mangle | Generate | Overloadable };
/// C code: not overloadable, not mangled
constexpr Spec C          = { Generate };
/// Built by translator (e.g. struct assignment)
constexpr Spec AutoGen    = { Mangle | Generate | Overrideable | Overloadable };
/// GCC internal
constexpr Spec Compiler   = { Builtin | Overloadable };
/// Mangled builtins
constexpr Spec BuiltinCFA = { Mangle | Generate | Builtin | Overloadable };
/// Non-mangled builtins
constexpr Spec BuiltinC   = { Generate | Builtin };

}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
