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
// Last Modified On : Thr Feb 16 10:08:00 2023
// Update Count     : 2
//

#include "RunParser.hpp"

#include "AST/Convert.hpp"                  // for convert
#include "AST/TranslationUnit.hpp"          // for TranslationUnit
#include "CodeTools/TrackLoc.h"             // for fillLocations
#include "Common/CodeLocationTools.hpp"     // for forceFillCodeLocations
#include "Parser/ParseNode.h"               // for DeclarationNode, buildList
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
	std::list<Declaration *> translationUnit;
	buildList( parseTree, translationUnit );

	delete parseTree;
	parseTree = nullptr;

	// When the parse/buildList code is translated to the new ast, these
	// fill passes (and the one after 'Hoist Type Decls') should be redundent
	// because the code locations should already be filled.
	CodeTools::fillLocations( translationUnit );
	ast::TranslationUnit transUnit = convert( std::move( translationUnit ) );
	forceFillCodeLocations( transUnit );
	return transUnit;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
