//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixFunction.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 17:02:08 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:45:55 2017
// Update Count     : 4
//

#pragma once

#include "Common/PassVisitor.h" // for PassVisitor
#include "SynTree/SynTree.h"    // for Types

namespace ast {
	class DeclWithType;
}

namespace SymTab {
	/// Replaces function and array types by equivalent pointer types. Returns true if type is 
	/// void
	bool fixFunction( DeclarationWithType *& );

	/// Returns declaration with function and array types replaced by equivalent pointer types.
	/// Sets isVoid to true if type is void
	const ast::DeclWithType * fixFunction( const ast::DeclWithType * dwt, bool & isVoid );
} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
