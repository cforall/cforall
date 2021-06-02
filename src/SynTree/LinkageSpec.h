//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LinkageSpec.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 13:24:28 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Mar  2 16:13:00 2020
// Update Count     : 21
//

#pragma once

#include <string>

struct CodeLocation;

namespace LinkageSpec {
	// Bitflags for linkage specifiers
	enum {
		Mangle = 1 << 0,
		Generate = 1 << 1,
		Overrideable = 1 << 2,
		Builtin = 1 << 3,
		GccBuiltin = 1 << 4,
	};

	// Bitflag type for storage classes
	union Spec {
		unsigned int val;
		struct {
			bool is_mangled : 1;
			bool is_generatable : 1;
			bool is_overridable : 1;
			bool is_builtin : 1;
			bool is_gcc_builtin : 1;
		};
		constexpr Spec( unsigned int val ) : val( val ) {}
		constexpr Spec( Spec const & other ) : val( other.val ) {}
		constexpr Spec & operator=( const Spec & ) = default;
		// Operators may go here.
		// Supports == and !=
		constexpr operator unsigned int() const { return val; }
	};


	Spec update( CodeLocation location, Spec spec, const std::string * cmd );
	// If cmd = "C" returns a Spec that is old_spec with is_mangled = false
	// If cmd = "Cforall" returns old_spec Spec with is_mangled = true

	std::string name( Spec );

	// To Update: LinkageSpec::isXyz( cur_spec ) -> cur_spec.is_xyz
	inline bool isMangled( Spec spec ) { return spec.is_mangled; }
	inline bool isGeneratable( Spec spec ) { return spec.is_generatable; }
	inline bool isOverridable( Spec spec ) { return spec.is_overridable; }
	inline bool isBuiltin( Spec spec ) { return spec.is_builtin; }
	inline bool isGccBuiltin( Spec spec ) { return spec.is_gcc_builtin; }

	// Pre-defined flag combinations:
	// C built-in defined in prelude
	constexpr Spec const Intrinsic = { Mangle | Generate | Overrideable | Builtin };
	// ordinary
	constexpr Spec const Cforall = { Mangle | Generate };
	// not overloadable, not mangled
	constexpr Spec const C = { Generate };
	// built by translator (struct assignment)
	constexpr Spec const AutoGen = { Mangle | Generate | Overrideable };
	// gcc internal
	constexpr Spec const Compiler = { Mangle | Builtin | GccBuiltin };
	// mangled builtins
	constexpr Spec const BuiltinCFA = { Mangle | Generate | Builtin };
	// non-mangled builtins
	constexpr Spec const BuiltinC = { Generate | Builtin };
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
