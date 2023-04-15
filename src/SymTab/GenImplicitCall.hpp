//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenImplicitCall.hpp --
//
// Author           : Andrew Beach
// Created On       : Fri Apr 14 14:29:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Fri Apr 14 14:29:00 2023
// Update Count     : 0
//

#pragma once

#include "InitTweak/InitTweak.h"  // for InitExpander
#include "SymTab/Autogen.h"       // for LoopDirection

namespace SymTab {

ast::ptr<ast::Stmt> genImplicitCall(
	InitTweak::InitExpander_new & srcParam, const ast::Expr * dstParam,
	const CodeLocation & loc, const std::string & fname, const ast::ObjectDecl * obj,
	LoopDirection forward = LoopForward
);

} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //

