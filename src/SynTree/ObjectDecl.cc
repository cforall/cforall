//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ObjectDecl.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Dec 16 15:12:03 2019
// Update Count     : 61
//

#include <list>                  // for list
#include <ostream>               // for operator<<, ostream, basic_ostream
#include <string>                // for operator<<, string, char_traits, ope...

#include "Attribute.h"           // for Attribute
#include "Common/utility.h"      // for maybeClone, printAll
#include "Declaration.h"         // for ObjectDecl, ObjectDecl::Parent
#include "Expression.h"          // for Expression
#include "Initializer.h"         // for Initializer
#include "LinkageSpec.h"         // for Spec, linkageName, Cforall
#include "Type.h"                // for Type, Type::StorageClasses, Type::Fu...

ObjectDecl::ObjectDecl( const std::string &name, Type::StorageClasses scs, LinkageSpec::Spec linkage, Expression *bitfieldWidth, Type *type, Initializer *init, const std::list< Attribute * > attributes, Type::FuncSpecifiers fs )
	: Parent( name, scs, linkage, attributes, fs ), type( type ), init( init ), bitfieldWidth( bitfieldWidth ) {
}

ObjectDecl::ObjectDecl( const ObjectDecl &other )
	: Parent( other ), type( maybeClone( other.type ) ), init( maybeClone( other.init ) ), bitfieldWidth( maybeClone( other.bitfieldWidth ) ) {
}

ObjectDecl::~ObjectDecl() {
	delete type;
	delete init;
	delete bitfieldWidth;
}

ObjectDecl * ObjectDecl::newObject( const std::string & name, Type * type, Initializer * init ) {
	return new ObjectDecl( name, Type::StorageClasses(), LinkageSpec::C, 0, type, init );
}

void ObjectDecl::print( std::ostream &os, Indenter indent ) const {
	if ( name != "" ) os << name << ": ";

	if ( linkage != LinkageSpec::Cforall ) {
		os << LinkageSpec::name( linkage ) << " ";
	} // if

	get_storageClasses().print( os );

	if ( type ) {
		type->print( os, indent );
	} else {
		os << " untyped entity ";
	} // if

	if ( init ) {
		os << " with initializer (" << (init->get_maybeConstructed() ? "maybe constructed" : "not constructed") << ")" << std::endl << indent+1;
		init->print( os, indent+1 );
		os << std::endl;
	} // if

	if ( ! attributes.empty() ) {
		os << std::endl << indent << "... with attributes:" << std::endl;
		printAll( attributes, os, indent+1 );
	}

	if ( bitfieldWidth ) {
		os << indent << " with bitfield width ";
		bitfieldWidth->print( os );
	} // if
}

void ObjectDecl::printShort( std::ostream &os, Indenter indent ) const {
#if 0
	if ( get_mangleName() != "") {
		os << get_mangleName() << ": ";
	} else
#endif
	if ( name != "" ) os << name << ": ";

	get_storageClasses().print( os );

	if ( type ) {
		type->print( os, indent );
	} else {
		os << "untyped entity ";
	} // if

	if ( bitfieldWidth ) {
		os << "with bitfield width ";
		bitfieldWidth->print( os );
	} // if
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
