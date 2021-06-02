//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TupleType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:44:38 2019
// Update Count     : 4
//

#include <list>                  // for list
#include <ostream>               // for operator<<, ostream, basic_ostream

#include "Common/utility.h"      // for cloneAll, deleteAll, printAll
#include "Declaration.h"         // for Declaration, ObjectDecl
#include "Initializer.h"         // for ListInit
#include "LinkageSpec.h"         // for Cforall
#include "Type.h"                // for TupleType, Type, Type::Qualifiers

class Attribute;

TupleType::TupleType( const Type::Qualifiers &tq, const std::list< Type * > & types, const std::list< Attribute * > & attributes ) : Type( tq, attributes ), types( types ) {
	for ( Type * t : *this ) {
		// xxx - this is very awkward. TupleTypes should contain objects so that members can be named, but if they don't have an initializer node then
		// they end up getting constructors, which end up being inserted causing problems. This happens because the object decls have to be visited so that
		// their types are kept in sync with the types list here. Ultimately, the types list here should be eliminated and perhaps replaced with a list-view
		// of the object types list, but I digress. The temporary solution here is to make a ListInit with maybeConstructed = false, that way even when the
		// object is visited, it is never constructed. Ultimately, a better solution might be either:
		// a) to separate TupleType from its declarations, into TupleDecl and Tuple{Inst?}Type, ala StructDecl and StructInstType
		// b) separate initializer nodes better, e.g. add a MaybeConstructed node that is replaced by genInit, rather than what currently exists in a bool
		members.push_back( new ObjectDecl( "" , Type::StorageClasses(), LinkageSpec::Cforall, nullptr, t->clone(), new ListInit( {}, {}, false ) ) );
	}
}

TupleType::TupleType( const TupleType& other ) : Type( other ) {
	cloneAll( other.types, types );
	cloneAll( other.members, members );
}

TupleType::~TupleType() {
	deleteAll( types );
	deleteAll( members );
}

void TupleType::print( std::ostream &os, Indenter indent ) const {
	Type::print( os, indent );
	os << "tuple of types" << std::endl;
	printAll( types, os, indent+1 );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
