//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CommonType.hpp --
//
// Author           : Andrew Beach
// Created On       : Tue Jan 17 16:52:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jan 17 16:52:00 2023
// Update Count     : 0
//

#pragma once

#include "AST/Fwd.hpp"
#include "AST/TypeEnvironment.hpp"  // for AssertionSet, OpenVarSet
#include "WidenMode.hpp"            // for WidenMode

namespace ResolvExpr {

ast::ptr< ast::Type > commonType(
	const ast::ptr< ast::Type > & type1, const ast::ptr< ast::Type > & type2,
	ast::TypeEnvironment & env,
	ast::AssertionSet & need, ast::AssertionSet & have,
	const ast::OpenVarSet & open, WidenMode widen);

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
