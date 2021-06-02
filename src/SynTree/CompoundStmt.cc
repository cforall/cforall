//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// XXX.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Rob Schluntz
// Last Modified On : Mon May 02 15:19:17 2016
// Update Count     : 3
//

#include <cassert>                    // for assert, strict_dynamic_cast
#include <list>                       // for list, _List_const_iterator, lis...
#include <ostream>                    // for operator<<, ostream, basic_ostream
#include <string>                     // for operator==, string

#include "Common/utility.h"           // for cloneAll, deleteAll, printAll
#include "Declaration.h"              // for DeclarationWithType, Declaration
#include "Statement.h"                // for CompoundStmt, Statement, DeclStmt
#include "SynTree/Label.h"            // for Label
#include "SynTree/DeclReplacer.h"     // for DeclReplacer

using std::string;
using std::endl;

CompoundStmt::CompoundStmt() : Statement() {
}

CompoundStmt::CompoundStmt( std::list<Statement *> stmts ) : Statement(), kids( stmts ) {
}

CompoundStmt::CompoundStmt( const CompoundStmt &other ) : Statement( other ) {
	cloneAll( other.kids, kids );

	// when cloning a compound statement, we may end up cloning declarations which
	// are referred to by VariableExprs throughout the block. Cloning a VariableExpr
	// does a shallow copy, so the VariableExpr will end up pointing to the original
	// declaration. If the original declaration is deleted, e.g. because the original
	// CompoundStmt is deleted, then we have a dangling pointer. To avoid this case,
	// find all DeclarationWithType nodes (since a VariableExpr must point to a
	// DeclarationWithType) in the original CompoundStmt and map them to the cloned
	// node in the new CompoundStmt ('this'), then replace the Declarations referred to
	// by each VariableExpr according to the constructed map. Note that only the declarations
	// in the current level are collected into the map, because child CompoundStmts will
	// recursively execute this routine. There may be more efficient ways of doing
	// this.
	DeclReplacer::DeclMap declMap;
	std::list< Statement * >::const_iterator origit = other.kids.begin();
	for ( Statement * s : kids ) {
		assert( origit != other.kids.end() );
		Statement * origStmt = *origit++;
		if ( DeclStmt * declStmt = dynamic_cast< DeclStmt * >( s ) ) {
			DeclStmt * origDeclStmt = strict_dynamic_cast< DeclStmt * >( origStmt );
			if ( DeclarationWithType * dwt = dynamic_cast< DeclarationWithType * > ( declStmt->get_decl() ) ) {
				DeclarationWithType * origdwt = strict_dynamic_cast< DeclarationWithType * > ( origDeclStmt->get_decl() );
				assert( dwt->get_name() == origdwt->get_name() );
				declMap[ origdwt ] = dwt;
			} else assert( ! dynamic_cast< DeclarationWithType * > ( origDeclStmt->get_decl() ) );
		} else assert( ! dynamic_cast< DeclStmt * > ( s ) );
	}
	if ( ! declMap.empty() ) {
		DeclReplacer::replace( this, declMap );
	}
}

CompoundStmt::~CompoundStmt() {
	deleteAll( kids );
}

void CompoundStmt::print( std::ostream &os, Indenter indent ) const {
	os << "CompoundStmt" << endl;
	printAll( kids, os, indent+1 );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
