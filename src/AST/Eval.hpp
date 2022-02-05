//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Eval.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Fri Jun 28 14:00:00 2019
// Last Modified By : Aaron B. Moss
// Created On       : Fri Jun 28 14:00:00 2019
// Update Count     : 1
//

#include <string>
#include <utility>

#include "Expr.hpp"

namespace ast {

/// Create a new UntypedExpr with the given arguments
template< typename... Args >
UntypedExpr * call( const CodeLocation & loc, const std::string & name, Args &&... args ) {
	return new UntypedExpr {
		loc, new NameExpr { loc, name },
		std::vector< ptr< Expr > > { std::forward< Args >( args )... } };
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
