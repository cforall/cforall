//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// PointerType.cc --
//
// Author           : Rob Schluntz
// Created On       : Fri May 12 18:12:15 2017
// Last Modified By : Rob Schluntz
// Last Modified On : Fri May 12 18:12:15 2017
// Update Count     : 1
//

#include "Type.h"
#include "Expression.h"
#include "TypeSubstitution.h"
#include "Common/utility.h"

ReferenceType::ReferenceType( const Type::Qualifiers &tq, Type *base, const std::list< Attribute * > & attributes )
	: Type( tq, attributes ), base( base ) {
	assertf( base, "Reference Type with a null base created." );
}

ReferenceType::ReferenceType( const ReferenceType &other )
	: Type( other ), base( maybeClone( other.base ) ) {
}

ReferenceType::~ReferenceType() {
	delete base;
}

int ReferenceType::referenceDepth() const {
	return base->referenceDepth()+1;
}

TypeSubstitution ReferenceType::genericSubstitution() const { return base->genericSubstitution(); }

void ReferenceType::print( std::ostream &os, Indenter indent ) const {
	Type::print( os, indent );
	os << "reference to ";
	if ( base ) {
		base->print( os, indent );
	} // if
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
