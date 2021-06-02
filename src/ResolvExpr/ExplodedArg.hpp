//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExplodedArg.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Tue Jun 11 16:18:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Tue Jun 11 16:18:00 2019
// Update Count     : 1
//

#pragma once

#include <vector>

#include "Candidate.hpp"            // for Candidate, CandidateList
#include "Cost.h"                   // for Cost
#include "AST/Expr.hpp"
#include "AST/Node.hpp"             // for ptr
#include "AST/TypeEnvironment.hpp"  // for TypeEnvironment
#include "AST/SymbolTable.hpp"      // for SymbolTable

namespace ResolvExpr {

/// Pre-exploded argument
struct ExplodedArg {
	ast::TypeEnvironment env;
	Cost cost;
	std::vector< ast::ptr<ast::Expr> > exprs;

	ExplodedArg() : env(), cost( Cost::zero ), exprs() {}
	ExplodedArg( const Candidate & arg, const ast::SymbolTable & symtab );
	
	ExplodedArg( ExplodedArg && ) = default;
	ExplodedArg & operator= ( ExplodedArg && ) = default;
};

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
