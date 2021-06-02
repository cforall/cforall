//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclStmt.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Jun 23 11:38:15 2015
// Update Count     : 4
//

#include <cassert>           // for assert
#include <list>              // for list
#include <ostream>           // for operator<<, ostream

#include "Common/utility.h"  // for maybeClone
#include "Declaration.h"     // for Declaration
#include "Statement.h"       // for DeclStmt, Statement
#include "SynTree/Label.h"   // for Label

DeclStmt::DeclStmt( Declaration *decl ) : Statement(), decl( decl ) {
}

DeclStmt::DeclStmt( const DeclStmt &other ) : Statement( other ), decl( maybeClone( other.decl ) ) {
}

DeclStmt::~DeclStmt() {
	delete decl;
}

void DeclStmt::print( std::ostream &os, Indenter indent ) const {
	assert( decl != 0 );
	os << "Declaration of ";
	decl->print( os, indent );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
