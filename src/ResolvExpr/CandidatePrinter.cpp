//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CandidatePrinter.cpp -- Print expression canditates.
//
// Author           : Andrew Beach
// Created On       : Tue Nov  9  9:54:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Wed Mar 16 13:56:00 2022
// Update Count     : 1
//

#include "CandidatePrinter.hpp"

#include <iostream>

#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"
#include "ResolvExpr/CandidateFinder.hpp"
#include "ResolvExpr/Resolver.hpp"

namespace ResolvExpr {

namespace {

class CandidatePrintCore : public ast::WithSymbolTable,
		public ast::WithConstTranslationUnit {
	std::ostream & os;
public:
	CandidatePrintCore( std::ostream & os ) : os( os ) {}

	void postvisit( const ast::ExprStmt * stmt ) {
		ast::TypeEnvironment env;
		CandidateFinder finder( { symtab, transUnit().global }, env );
		finder.find( stmt->expr, ResolveMode::withAdjustment() );
		int count = 1;
		os << "There are " << finder.candidates.size() << " candidates\n";
		for ( const std::shared_ptr<Candidate> & cand : finder ) {
			os << "Candidate " << count++ << " ==============\n";
			ast::print( os, cand->expr->result.get() );
			os << std::endl;
		}
	}
};

} // namespace

void printCandidates( ast::TranslationUnit & transUnit ) {
	ast::Pass<CandidatePrintCore>::run( transUnit, std::cout );
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
