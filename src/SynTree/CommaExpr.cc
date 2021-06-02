//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CommaExpr.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Arg 12 16:11:00 2016
// Update Count     : 2
//

#include <ostream>           // for ostream, endl, operator<<, basic_ostream
#include <string>            // for operator<<, string

#include "Common/utility.h"  // for maybeClone
#include "Expression.h"      // for CommaExpr, Expression
#include "Type.h"            // for Type

CommaExpr::CommaExpr( Expression *arg1, Expression *arg2 )
		: Expression(), arg1( arg1 ), arg2( arg2 ) {
	set_result( maybeClone( arg2->get_result() ) );
}

CommaExpr::CommaExpr( const CommaExpr &other )
		: Expression( other ), arg1( maybeClone( other.arg1 ) ), arg2( maybeClone( other.arg2 ) ) {
}

CommaExpr::~CommaExpr() {
	delete arg1;
	delete arg2;
}

bool CommaExpr::get_lvalue() const {
	// This is wrong by C, but the current implementation uses it.
	// (ex: Specialize, Lvalue and Box)
	return arg2->get_lvalue();
}

void CommaExpr::print( std::ostream &os, Indenter indent ) const {
	os << "Comma Expression:" << std::endl;
	os << (indent+1);
	arg1->print( os, indent+1 );
	os << std::endl;
	os << (indent+1);
	arg2->print( os, indent+1 );
	Expression::print( os, indent );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
