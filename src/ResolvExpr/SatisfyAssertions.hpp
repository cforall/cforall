//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SatisfyAssertions.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Mon Jun 10 17:45:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Mon Jun 10 17:45:00 2019
// Update Count     : 1
//

#pragma once

#include <string>
#include <vector>

#include "Candidate.hpp"  // for Candidate, CandidateList

namespace ast {
	class SymbolTable;
}

namespace ResolvExpr {

/// Recursively satisfies all assertions provided in a candidate
/// returns true if it has been run (candidate has any assertions)
void satisfyAssertions(
	CandidateRef & cand, const ast::SymbolTable & symtab, CandidateList & out,
	std::vector<std::string> & errors );

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
