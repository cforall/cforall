//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindFunction.h -- 
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:23:36 2017
// Update Count     : 2
//

#pragma once

#include <list>       // for list

#include "GenPoly.h"  // for TyVarMap

class FunctionType;
class Type;

namespace GenPoly {
	typedef bool (*FindFunctionPredicate)( FunctionType*, const TyVarMap& );

	/// recursively walk `type`, placing all functions that match `predicate` under `tyVars` into `functions`
	void findFunction( Type *type, std::list< FunctionType const * > &functions, const TyVarMap &tyVars, FindFunctionPredicate predicate );
	/// like `findFunction`, but also replaces the function type with void ()(void)
	void findAndReplaceFunction( Type *&type, std::list< FunctionType const * > &functions, const TyVarMap &tyVars, FindFunctionPredicate predicate );
} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
