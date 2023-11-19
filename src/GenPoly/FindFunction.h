//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindFunction.h -- Find function types in a larger type.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri Oct  7 10:30:00 2022
// Update Count     : 3
//

#pragma once

#include "GenPoly.h"            // for TypeVarMap

namespace GenPoly {

typedef bool (*FindFunctionPred)( const ast::FunctionType *, const TypeVarMap & );

/// Recursively walks `type`, placing all functions that match `predicate`
/// under `typeVars` into `functions`.
void findFunction( const ast::Type * type,
		std::vector<ast::ptr<ast::FunctionType>> & functions,
		const TypeVarMap & typeVars, FindFunctionPred predicate );
/// Like findFunction, but also replaces the function type with `void ()(void)`.
const ast::Type * findAndReplaceFunction( const ast::Type * type,
		std::vector<ast::ptr<ast::FunctionType>> & functions,
		const TypeVarMap & typeVars, FindFunctionPred predicate );

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
