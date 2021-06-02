//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Candidate.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed Jun 5 14:30:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Wed Jun 5 14:30:00 2019
// Update Count     : 1
//

#include "Candidate.hpp"

#include <iostream>

#include "AST/Print.hpp"

namespace ResolvExpr {

void print( std::ostream & os, const Candidate & cand, Indenter indent ) {
	os << "Cost " << cand.cost << ": ";
	if ( cand.expr ) {
		++indent;
		ast::print( os, cand.expr, indent );
		os << std::endl << indent-1 << "(types:" << std::endl;
		os << indent;
		ast::print( os, cand.expr->result, indent );
		--indent;
		os << std::endl << indent << ")" << std::endl;
	} else {
		os << "Null expression!" << std::endl;
	} // if
	os << indent << "Environment:";
	ast::print( os, cand.env, indent+1 );
	os << std::endl;
}

void print( std::ostream & os, const CandidateList & cands, Indenter indent ) {
	std::vector<std::string> sorted;
	sorted.reserve(cands.size());
	for(const auto & c : cands) {
		std::stringstream ss;
		print( ss, *c, indent );
		sorted.push_back(ss.str());
	}

	std::sort(sorted.begin(), sorted.end());

	for ( const auto & s : sorted ) {
		os << s << std::endl;
	}
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //