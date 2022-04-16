//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// BasicType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Aug  4 21:07:44 2019
// Update Count     : 13
//

#include <cassert>  // for assert
#include <list>     // for list
#include <ostream>  // for operator<<, ostream

#include "Type.h"   // for BasicType, Type, BasicType::Kind, BasicType::Kind...

class Attribute;

BasicType::BasicType( const Type::Qualifiers &tq, Kind bt, const std::list< Attribute * > & attributes ) : Type( tq, attributes ), kind( bt ) {}

void BasicType::print( std::ostream &os, Indenter indent ) const {
	Type::print( os, indent );
	os << BasicType::typeNames[ kind ];
}

bool BasicType::isWholeNumber() const {
	return kind == Bool || 
		kind ==Char ||
		kind == SignedChar ||
		kind == UnsignedChar ||
		kind == ShortSignedInt ||
		kind == ShortUnsignedInt ||
		kind == SignedInt ||
		kind == UnsignedInt ||
		kind == LongSignedInt ||
		kind == LongUnsignedInt ||
		kind == LongLongSignedInt ||
		kind ==LongLongUnsignedInt ||
		kind == SignedInt128 ||
		kind == UnsignedInt128;
}

bool BasicType::isInteger() const {
	return kind <= UnsignedInt128;
}


// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
