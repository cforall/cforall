//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// PointerType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  1 17:17:13 2017
// Update Count     : 6
//

#include <list>              // for list
#include <ostream>           // for operator<<, ostream

#include "Common/utility.h"  // for maybeClone
#include "Expression.h"      // for Expression
#include "Type.h"            // for PointerType, Type, Type::Qualifiers

class Attribute;

PointerType::PointerType( const Type::Qualifiers &tq, Type *base, const std::list< Attribute * > & attributes )
	: Type( tq, attributes ), base( base ), dimension( 0 ), isVarLen( false ), isStatic( false ) {
}

PointerType::PointerType( const Type::Qualifiers &tq, Type *base, Expression *dimension, bool isVarLen, bool isStatic, const std::list< Attribute * > & attributes )
	: Type( tq, attributes ), base( base ), dimension( dimension ), isVarLen( isVarLen ), isStatic( isStatic ) {
}

PointerType::PointerType( const PointerType &other )
	: Type( other ), base( maybeClone( other.base ) ), dimension( maybeClone( other.dimension ) ),
	  isVarLen( other.isVarLen ), isStatic( other.isStatic ) {
}

PointerType::~PointerType() {
	delete base;
	delete dimension;
}

void PointerType::print( std::ostream &os, Indenter indent ) const {
	Type::print( os, indent );
	if ( ! is_array() ) {
		os << "pointer to ";
	} else {
		os << "decayed ";
		if ( isStatic ) {
			os << "static ";
		} // if
		if ( isVarLen ) {
			os << "variable length array of ";
		} else if ( dimension ) {
			os << "array of ";
			dimension->print( os, indent );
			os << " ";
		} // if
	}
	if ( base ) {
		base->print( os, indent );
	} // if
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
