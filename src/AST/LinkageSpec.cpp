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

#include "LinkageSpec.hpp"

#include <cassert>
#include <memory>   // for unique_ptr
#include <string>

#include "Common/CodeLocation.h"
#include "Common/SemanticError.h"

namespace ast {

namespace Linkage {

	Spec update( CodeLocation loc, Spec spec, const std::string * cmd ) {
		assert( cmd );
		std::unique_ptr<const std::string> guard( cmd ); // allocated by lexer
		if ( *cmd == "\"Cforall\"" ) {
			spec.is_mangled = true;
			return spec;
		} else if ( *cmd == "\"C\"" ) {
			spec.is_mangled = false;
			return spec;
		} else {
			SemanticError( loc, "Invalid linkage specifier " + *cmd );
		}
	}


	std::string name( Spec spec ) {
		switch ( spec.val ) {
		case Intrinsic.val:  return "intrinsic";
		case C.val:          return "C";
		case Cforall.val:    return "Cforall";
		case AutoGen.val:    return "autogenerated cfa";
		case Compiler.val:   return "compiler built-in";
		case BuiltinCFA.val: return "cfa built-in";
		case BuiltinC.val:   return "c built-in";
		default:         return "<unnamed linkage spec>";
		}
	}

}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
