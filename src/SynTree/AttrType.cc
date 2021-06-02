//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AttrType.cc.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  1 17:17:59 2017
// Update Count     : 3
//

#include <list>              // for list
#include <ostream>           // for operator<<, ostream, basic_ostream
#include <string>            // for char_traits, operator<<, string

#include "Common/utility.h"  // for maybeClone
#include "Expression.h"      // for Expression
#include "Type.h"            // for AttrType, Type, Type::Qualifiers

class Attribute;


AttrType::AttrType( const Type::Qualifiers &tq, const std::string &name, Expression *expr, const std::list< Attribute * > & attributes )
	: Type( tq, attributes ), name( name ), expr( expr ), type( 0 ), isType( false ) {
}

AttrType::AttrType( const Type::Qualifiers &tq, const std::string &name, Type *type, const std::list< Attribute * > & attributes )
	: Type( tq, attributes ), name( name ), expr( 0 ), type( type ), isType( true ) {
}

AttrType::AttrType( const AttrType &other )
	: Type( other ), name( other.name ), expr( maybeClone( other.expr ) ), type( maybeClone( other.type ) ), isType( other.isType ) {
}

AttrType::~AttrType() {
	delete expr;
	delete type;
}

void AttrType::print( std::ostream &os, Indenter indent ) const {
	Type::print( os, indent );
	os << "attribute " << name << " applied to ";
	if ( expr ) {
		os << "expression ";
		expr->print( os, indent );
	} // if
	if ( type ) {
		os << "type ";
		type->print( os, indent );
	} // if
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
