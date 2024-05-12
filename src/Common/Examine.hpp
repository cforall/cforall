//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Examine.hpp -- Helpers for examining AST code.
//
// Author           : Andrew Beach
// Created On       : Wed Sept 2 13:57 2020
// Last Modified By : Andrew Beach
// Last Modified On : Fri Dec 10 10:28 2021
// Update Count     : 1
//

#include "AST/Decl.hpp"

/// Check if this is a main function for a type of an aggregate kind.
const ast::DeclWithType * isMainFor(
	const ast::FunctionDecl * func, ast::AggregateDecl::Aggregate kind );
// Returns a pointer to the parameter if true, nullptr otherwise.

/// Check if this function is a destructor for the given structure.
bool isDestructorFor(
	const ast::FunctionDecl * func, const ast::StructDecl * type );
