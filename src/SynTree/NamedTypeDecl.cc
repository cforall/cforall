//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// NamedTypeDecl.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Dec 16 15:11:40 2019
// Update Count     : 17
//

#include <list>                  // for list
#include <ostream>               // for operator<<, ostream, basic_ostream
#include <string>                // for operator<<, string, char_traits, ope...

#include "Common/utility.h"      // for printAll, cloneAll, deleteAll, maybe...
#include "Declaration.h"         // for NamedTypeDecl, DeclarationWithType
#include "LinkageSpec.h"         // for Spec, Cforall, linkageName
#include "Type.h"                // for Type, Type::StorageClasses
#include "CompilationState.h"

NamedTypeDecl::NamedTypeDecl( const std::string &name, Type::StorageClasses scs, Type *base )
	: Parent( name, scs, LinkageSpec::Cforall ), base( base ) {}

NamedTypeDecl::NamedTypeDecl( const NamedTypeDecl &other )
	: Parent( other ), base( maybeClone( other.base ) ) {
	cloneAll( other.assertions, assertions );
}

NamedTypeDecl::~NamedTypeDecl() {
	delete base;
	deleteAll( assertions );
}

void NamedTypeDecl::print( std::ostream &os, Indenter indent ) const {
	using namespace std;

	if ( ! name.empty() ) {
		if( deterministic_output && isUnboundType(name) ) os << "[unbound]:";
		else os << name << ": ";
	}

	if ( linkage != LinkageSpec::Cforall ) {
		os << LinkageSpec::name( linkage ) << " ";
	} // if
	get_storageClasses().print( os );
	os << typeString();
	if ( base ) {
		os << " for ";
		base->print( os, indent+1 );
	} // if
	if ( ! assertions.empty() ) {
		os << endl << indent << "... with assertions" << endl;
		printAll( assertions, os, indent+1 );
	} // if
}

void NamedTypeDecl::printShort( std::ostream &os, Indenter indent ) const {
	using namespace std;

	if ( name != "" ) os << name << ": ";
	get_storageClasses().print( os );
	os << typeString();
	if ( base ) {
		os << " for ";
		base->print( os, indent+1 );
	} // if
}

const char * TypedefDecl::typeString() const { return "typedef"; }

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
