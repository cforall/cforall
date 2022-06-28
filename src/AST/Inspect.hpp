//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Inspect.hpp -- Helpers to get information from the AST.
//
// Author           : Thierry Delisle
// Created On       : Fri Jun 24 13:16:31 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jun 27 15:35:00 2022
// Update Count     : 1
//

#include "AST/Fwd.hpp"

namespace ast {

// Does the structure end in a flexable array declaration?
bool structHasFlexibleArray( const ast::StructDecl * );

}
