//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeExpr.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon May 18 11:03:15 2015
// Update Count     : 1
//

#include <iosfwd>            // for ostream

#include "Common/utility.h"  // for maybeClone
#include "Expression.h"      // for TypeExpr, Expression
#include "Type.h"            // for Type

TypeExpr::TypeExpr( Type *type ) : type( type ) {
}

TypeExpr::TypeExpr( const TypeExpr &other ) : Expression( other ), type( maybeClone( other.type ) ) {
}

TypeExpr::~TypeExpr() {
	delete type;
}

void TypeExpr::print( std::ostream &os, Indenter indent ) const {
	if ( type ) type->print( os, indent );
	Expression::print( os, indent );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
