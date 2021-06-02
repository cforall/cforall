//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ArrayType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Mar 17 09:40:30 2017
// Update Count     : 13
//

#include <list>              // for list
#include <ostream>           // for operator<<, ostream

#include "Common/utility.h"  // for maybeClone
#include "Expression.h"      // for Expression
#include "Type.h"            // for ArrayType, Type, Type::Qualifiers

class Attribute;


ArrayType::ArrayType( const Type::Qualifiers &tq, Type *base, Expression *dimension, bool isVarLen, bool isStatic, const std::list< Attribute * > & attributes )
	: Type( tq, attributes ), base( base ), dimension( dimension ), isVarLen( isVarLen ), isStatic( isStatic ) {
}

ArrayType::ArrayType( const ArrayType &other )
		: Type( other ), base( maybeClone( other.base ) ), dimension( maybeClone( other.dimension ) ),
		  isVarLen( other.isVarLen ), isStatic( other.isStatic ) {
}

ArrayType::~ArrayType() {
	delete base;
	delete dimension;
}

void ArrayType::print( std::ostream &os, Indenter indent ) const {
	Type::print( os, indent );
	if ( isStatic ) {
		os << "static ";
	} // if
	if ( isVarLen ) {
		os << "variable length array of ";
	} else if ( dimension ) {
		os << "array of ";
	} else {
		os << "open array of ";
	} // if
	if ( base ) {
		base->print( os, indent );
	} // if
	if ( dimension ) {
		os << " with dimension of ";
		dimension->print( os, indent );
	} // if
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
