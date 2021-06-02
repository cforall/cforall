//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// VarArgsType.cc --
//
// Author           : Aaron B. Moss
// Created On       : Thu Feb 25 16:34:00 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  1 17:14:48 2017
// Update Count     : 4
//

#include <list>     // for list
#include <ostream>  // for operator<<, ostream

#include "Type.h"   // for Type, VarArgsType, Type::Qualifiers

class Attribute;

VarArgsType::VarArgsType() : Type( Type::Qualifiers(), std::list< Attribute * >() ) {}

VarArgsType::VarArgsType( Type::Qualifiers tq, const std::list< Attribute * > & attributes ) : Type( tq, attributes ) {}

void VarArgsType::print( std::ostream &os, Indenter indent ) const {
	Type::print( os, indent );
	os << "builtin var args pack";
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
