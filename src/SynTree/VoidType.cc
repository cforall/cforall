//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// VoidType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  1 17:09:26 2017
// Update Count     : 3
//

#include <list>     // for list
#include <ostream>  // for operator<<, ostream

#include "Type.h"   // for VoidType, Type, Type::Qualifiers

class Attribute;

VoidType::VoidType( const Type::Qualifiers &tq, const std::list< Attribute * > & attributes ) : Type( tq, attributes ) {
}

void VoidType::print( std::ostream &os, Indenter indent ) const {
	Type::print( os, indent );
	os << "void ";
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
