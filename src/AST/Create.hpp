//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Create.hpp -- Helpers to create pieces of the AST.
//
// Author           : Andrew Beach
// Created On       : Tue Sep 20 13:25:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Sep 20 15:34:00 2022
// Update Count     : 1
//

#include "AST/Fwd.hpp"

namespace ast {

/// Create a forward declaration of the existing declaration.
/// If the argument is already a forward declaration, return nullptr instead.
/// More efficient than the deepCopy and clear pattern.
FunctionDecl * asForward( FunctionDecl const * );
StructDecl * asForward( StructDecl const * );
UnionDecl * asForward( UnionDecl const * );

}
