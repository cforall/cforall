//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// RunParser.cpp -- External interface to the parser.
//
// Author           : Andrew Beach
// Created On       : Mon Dec 19 11:00:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Mar  6  9:42:00 2023
// Update Count     : 3
//

#include "RunParser.hpp"

#include "AST/Convert.hpp"                  // for convert
#include "AST/TranslationUnit.hpp"          // for TranslationUnit
#include "CodeTools/TrackLoc.h"             // for fillLocations
#include "Common/CodeLocationTools.hpp"     // for forceFillCodeLocations
#include "Parser/DeclarationNode.h"         // for DeclarationNode, buildList
#include "Parser/TypedefTable.h"            // for TypedefTable

// Variables global to the parsing code.
ast::Linkage::Spec linkage = ast::Linkage::Cforall;
TypedefTable typedefTable;
DeclarationNode * parseTree = nullptr;

void parse( FILE * input, ast::Linkage::Spec linkage, bool alwaysExit ) {
	extern int yyparse( void );
	extern FILE * yyin;
	extern int yylineno;

	// Set global information.
	::linkage = linkage;
	yyin = input;
	yylineno = 1;

	int parseStatus = yyparse();
	fclose( input );
	if ( alwaysExit || parseStatus != 0 ) {
		exit( parseStatus );
	} // if
} // parse

ast::TranslationUnit buildUnit(void) {
	std::vector<ast::ptr<ast::Decl>> decls;
	buildList( parseTree, decls );
	delete parseTree;
	parseTree = nullptr;

	ast::TranslationUnit transUnit;
	for ( auto decl : decls ) {
		transUnit.decls.emplace_back( std::move( decl ) );
	}
	return transUnit;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
