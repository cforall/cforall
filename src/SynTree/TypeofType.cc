//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeofType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  1 17:18:29 2017
// Update Count     : 3
//

#include <list>              // for list
#include <ostream>           // for operator<<, ostream

#include "Common/utility.h"  // for maybeClone
#include "Expression.h"      // for Expression
#include "Type.h"            // for TypeofType, Type, Type::Qualifiers

class Attribute;

TypeofType::TypeofType( const Type::Qualifiers &tq, Expression *expr, 
	const std::list< Attribute * > & attributes ) 
: Type( tq, attributes ), expr( expr ), is_basetypeof(false) {}

TypeofType::TypeofType( const Type::Qualifiers &tq, Expression *expr, bool is_basetypeof, 
	const std::list< Attribute * > & attributes ) 
: Type( tq, attributes ), expr( expr ), is_basetypeof( is_basetypeof ) {}

TypeofType::TypeofType( const TypeofType &other )
: Type( other ), expr( maybeClone( other.expr ) ), is_basetypeof( other.is_basetypeof ) {}

TypeofType::~TypeofType() {
	delete expr;
}

void TypeofType::print( std::ostream &os, Indenter indent ) const {
	Type::print( os, indent );
	if ( is_basetypeof ) { os << "base-"; }
	os << "type-of expression ";
	if ( expr ) {
		expr->print( os, indent );
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
