//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Eval.hpp -- Evaluate parts of the ast at compile time.
//
// Author           : Andrew Beach
// Created On       : Fri Feb 17 11:41:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Fri Feb 17 11:41:00 2023
// Update Count     : 0
//

#pragma once

#include <utility>                 // for pair

class Expression;
namespace ast {
	class Expr;
}

struct Evaluation {
	long long int knownValue;
	bool hasKnownValue;
	bool isEvaluableInGCC;
};

/// Evaluates expr as a long long int.
Evaluation eval(const ast::Expr * expr);

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
