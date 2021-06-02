//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// RenameVars.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:10:28 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:33:54 2017
// Update Count     : 4
//

#pragma once

#include <list>               // for list
#include <map>                // for map
#include <string>             // for string

#include "SynTree/SynTree.h"  // for Visitor Nodes
#include "SynTree/Visitor.h"  // for Visitor

namespace ast {
	class Type;
}

namespace ResolvExpr {
	/// Provides a consistent renaming of forall type names in a hierarchy by prefixing them with a unique "level" ID
	void renameTyVars( Type * );

	enum RenameMode {
		GEN_USAGE, // for type in VariableExpr
		GEN_EXPR_ID // for type in decl
	};
	const ast::Type * renameTyVars( const ast::Type *, RenameMode mode = GEN_USAGE, bool reset = true );
	

	/// resets internal state of renamer to avoid overflow
	void resetTyVarRenaming();

	
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
