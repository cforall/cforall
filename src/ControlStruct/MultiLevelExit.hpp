//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// MultiLevelExit.hpp -- Replaces CFA's local control flow with C's versions.
//
// Author           : Andrew Beach
// Created On       : Mon Nov  1 13:49:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Mon Nov  8 10:53:00 2021
// Update Count     : 3
//

#pragma once

#include <map>

namespace ast {
	class CompoundStmt;
	class Label;
	class Stmt;
}

namespace ControlStruct {

using LabelToStmt = std::map<ast::Label, const ast::Stmt *>;

/// Mutate a function body to handle multi-level exits.
const ast::CompoundStmt * multiLevelExitUpdate(
	const ast::CompoundStmt *, const LabelToStmt & );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
