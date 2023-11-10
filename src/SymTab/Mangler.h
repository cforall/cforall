//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Mangler.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 21:44:03 2015
// Last Modified By : Andrew Beach
// Last Modified On : Thu Oct 27 11:58:00 2022
// Update Count     : 16
//

#pragma once

#include <map>                // for map, map<>::value_compare
#include <sstream>            // for ostringstream
#include <string>             // for string
#include <utility>            // for pair

#include "AST/Bitfield.hpp"

// https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling
// The CFA name mangling scheme is based closely on the itanium C++ name mangling scheme, with the following key differences:
// * Variable names are also mangled to include type information, not just functions
// * CFA does not have template expansion, so the rules for function specialization do not apply.
// * CFA instead has to handle type parameters and assertion parameters.
// * Currently name compression is not implemented.

namespace ast {
	class Node;
}

namespace SymTab {
	namespace Mangler {
		namespace Encoding {
			extern const std::string manglePrefix;
			extern const std::string basicTypes[];
			extern const std::map<int, std::string> qualifiers;

			extern const std::string void_t;
			extern const std::string zero;
			extern const std::string one;

			extern const std::string function;
			extern const std::string tuple;
			extern const std::string pointer;
			extern const std::string array;
			extern const std::string qualifiedTypeStart;
			extern const std::string qualifiedTypeEnd;

			extern const std::string forall;
			extern const std::string typeVariables[];

			extern const std::string struct_t;
			extern const std::string union_t;
			extern const std::string enum_t;
			extern const std::string type;

			extern const std::string autogen;
			extern const std::string intrinsic;
		};
	} // Mangler
} // SymTab

namespace Mangle {
	/// Bitflags for mangle modes
	enum {
		NoOverrideable  = 1 << 0,
		Type            = 1 << 1,
		NoGenericParams = 1 << 2
	};

	/// Bitflag type for mangler modes
	struct mangle_flags {
		union {
			unsigned int val;
			struct {
				bool no_overrideable   : 1;
				bool type              : 1;
				bool no_generic_params : 1;
			};
		};

		constexpr mangle_flags( unsigned int val ) : val(val) {}
	};

	using Mode = bitfield<mangle_flags>;

	/// Mangle declaration name.
	std::string mangle( const ast::Node * decl, Mode mode = {} );

	/// Most common mangle configuration for types.
	static inline std::string mangleType( const ast::Node * type ) {
		return mangle( type, { NoOverrideable | Type } );
	}

	namespace Encoding {
		using namespace SymTab::Mangler::Encoding;
	};
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
