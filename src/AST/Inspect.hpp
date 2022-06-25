//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Node.hpp --
//
// Author           : Thierry Delisle
// Created On       : Fri Jun 24 13:16:31 2022
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#include "AST/Fwd.hpp"

namespace ast {
	bool structHasFlexibleArray( const ast::StructDecl * );
}