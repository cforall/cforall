//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// VarArgsType.cc --
//
// Author           : Aaron B. Moss
// Created On       : Fri Sep 16 14:08:00 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  1 17:15:46 2017
// Update Count     : 4
//

#include <list>     // for list
#include <ostream>  // for operator<<, ostream

#include "Type.h"   // for Type, Type::Qualifiers, OneType, ZeroType

class Attribute;

ZeroType::ZeroType() : Type( Type::Qualifiers(), std::list< Attribute * >() ) {}

ZeroType::ZeroType( Type::Qualifiers tq, const std::list< Attribute * > & attributes ) : Type( tq, attributes ) {}

void ZeroType::print( std::ostream &os, Indenter ) const {
	os << "zero_t";
}

OneType::OneType() : Type( Type::Qualifiers(), std::list< Attribute * >() ) {}

OneType::OneType( Type::Qualifiers tq, const std::list< Attribute * > & attributes ) : Type( tq, attributes ) {}

void OneType::print( std::ostream &os, Indenter ) const {
	os << "one_t";
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
