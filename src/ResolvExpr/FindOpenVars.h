//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindOpenVars.h -- 
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 09:46:04 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:35:18 2017
// Update Count     : 3
//

#pragma once

#include "AST/TypeEnvironment.hpp"  // for AssertionSet, OpenVarSet

namespace ast {
	class Type;
}

namespace ResolvExpr {
	enum FirstMode { FirstClosed, FirstOpen };

	// Updates open and closed variables and their associated assertions
	void findOpenVars( 
		const ast::Type * type, ast::OpenVarSet & open, ast::OpenVarSet & closed, 
		ast::AssertionSet & need, ast::AssertionSet & have, ast::TypeEnvironment & env, FirstMode firstIsOpen );
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
