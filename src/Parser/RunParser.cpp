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
// Last Modified On : Mon Dec 19 11:15:00 2022
// Update Count     : 0
//

#include "RunParser.hpp"

#include "Parser/ParseNode.h"               // for DeclarationNode, buildList
#include "Parser/TypedefTable.h"            // for TypedefTable

// Variables global to the parsing code.
LinkageSpec::Spec linkage = LinkageSpec::Cforall;
TypedefTable typedefTable;
DeclarationNode * parseTree = nullptr;

void parse( FILE * input, LinkageSpec::Spec linkage, bool alwaysExit ) {
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

void dumpParseTree( std::ostream & out ) {
	parseTree->printList( out );
	delete parseTree;
	parseTree = nullptr;
}

std::list<Declaration *> buildUnit(void) {
	std::list<Declaration *> translationUnit;
	buildList( parseTree, translationUnit );

	delete parseTree;
	parseTree = nullptr;

	return translationUnit;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
