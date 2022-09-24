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
// Last Modified On : Thr Sep 22 13:44:00 2022
// Update Count     : 2
//

#include "AST/Fwd.hpp"

namespace ast {

/// Returns the base type of an pointer/array/reference type,
/// if the argument is not one of those types, return null.
const Type * getPointerBase( const Type * );

/// Get the declaration of the function called (ApplicationExpr or UntypedExpr).
const DeclWithType * getFunction( const Expr * expr );

/// Get the name of the function being called.
std::string getFunctionName( const Expr * expr );

/// Returns the argument to a call expression in position N, indexed from 0.
const Expr * getCallArg( const Expr * call, unsigned pos );

/// Does the structure end in a flexable array declaration?
bool structHasFlexibleArray( const StructDecl * );

/// If the expression is an application whose target function is an
/// intrinsic, then returns a pointer to that application.
const ApplicationExpr * isIntrinsicCallExpr( const Expr * expr );

}
