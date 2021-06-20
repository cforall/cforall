//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeDecl.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Jan 12 16:07:33 2021
// Update Count     : 26
//

#include <ostream>           // for ostream, operator<<, basic_ostream, basi...
#include <string>            // for string, char_traits, operator+, operator<<

#include "Common/utility.h"  // for maybeClone
#include "Declaration.h"     // for TypeDecl, TypeDecl::Data, TypeDecl::Kind...
#include "Type.h"            // for Type, Type::StorageClasses

TypeDecl::TypeDecl( const std::string & name, Type::StorageClasses scs, Type * type, Kind kind, bool sized, Type * init ) :
	Parent( name, scs, type ), kind( kind ), sized( kind == Ttype || sized ), init( init ) {
}

TypeDecl::TypeDecl( const TypeDecl & other ) : Parent( other ), kind( other.kind ), sized( other.sized ), init( maybeClone( other.init ) ) {
}

TypeDecl::~TypeDecl() {
	delete init;
}

const char * TypeDecl::typeString() const {
	static const char * kindNames[] = { "sized data type", "sized data type", "sized object type", "sized function type", "sized tuple type", "sized length value" };
	static_assert( sizeof(kindNames) / sizeof(kindNames[0]) == TypeDecl::NUMBER_OF_KINDS, "typeString: kindNames is out of sync." );
	assertf( kind < TypeDecl::NUMBER_OF_KINDS, "TypeDecl kind is out of bounds." );
	return isComplete() ? kindNames[ kind ] : &kindNames[ kind ][ sizeof("sized") ]; // sizeof includes '\0'
}

const char * TypeDecl::genTypeString() const {
	static const char * kindNames[] = { "T &", "T *", "T", "(*)", "T ...", "[T]" };
	static_assert( sizeof(kindNames) / sizeof(kindNames[0]) == TypeDecl::NUMBER_OF_KINDS, "genTypeString: kindNames is out of sync." );
	assertf( kind < TypeDecl::NUMBER_OF_KINDS, "TypeDecl kind is out of bounds." );
	return kindNames[ kind ];
}

void TypeDecl::print( std::ostream &os, Indenter indent ) const {
	NamedTypeDecl::print( os, indent );
	if ( init ) {
		os << std::endl << indent << "with type initializer: ";
		init->print( os, indent + 1 );
	} // if
}

std::ostream & operator<<( std::ostream & os, const TypeDecl::Data & data ) {
	return os << data.kind << ", " << data.isComplete;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
