//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Initializer.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Aug 21 09:53:15 2017
// Update Count     : 30
//

#include "Initializer.h"

#include <cassert>                   // for assertf
#include <ostream>                   // for ostream, operator<<, basic_ostream
#include <string>                    // for operator<<, string, char_traits

#include "Common/utility.h"          // for maybeClone, cloneAll, deleteAll
#include "Expression.h"              // for Expression
#include "Statement.h"               // for Statement
#include "SynTree/BaseSyntaxNode.h"  // for BaseSyntaxNode

Designation::Designation( const std::list< Expression * > & designators ) : designators( designators ) {}
Designation::Designation( const Designation & other ) : BaseSyntaxNode( other ) {
	// std::cerr << "cloning designation" << std::endl;
	cloneAll( other.designators, designators );
	// std::cerr << "finished cloning designation" << std::endl;
}

Designation::~Designation() {
	// std::cerr << "destroying designation" << std::endl;
	deleteAll( designators );
	// std::cerr << "finished destroying designation" << std::endl;
}

void Designation::print( std::ostream &os, Indenter indent ) const {
	if ( ! designators.empty() ) {
		os << "... designated by: " << std::endl;
		for ( const Expression * d : designators ) {
			os << indent+1;
			d->print(os, indent+1 );
			os << std::endl;
		}
	} // if
}

Initializer::Initializer( bool maybeConstructed ) : maybeConstructed( maybeConstructed ) {}
Initializer::Initializer( const Initializer & other ) : BaseSyntaxNode( other ), maybeConstructed( other.maybeConstructed ) {
}
Initializer::~Initializer() {}

SingleInit::SingleInit( Expression *v, bool maybeConstructed ) : Initializer( maybeConstructed ), value ( v ) {
}

SingleInit::SingleInit( const SingleInit &other ) : Initializer(other), value ( maybeClone( other.value ) ) {
}

SingleInit::~SingleInit() {
	delete value;
}

void SingleInit::print( std::ostream &os, Indenter indent ) const {
	os << "Simple Initializer: ";
	value->print( os, indent );
}


ListInit::ListInit( const std::list<Initializer*> &inits, const std::list<Designation *> &des, bool maybeConstructed )
	: Initializer( maybeConstructed ), initializers( inits ), designations( des ) {
		// handle the common case where a ListInit is created without designations by making a list of empty designations with the same length as the initializer
		if ( designations.empty() ) {
			for ( auto & i : initializers ) {
				(void)i;
				designations.push_back( new Designation( {} ) );
			}
		}
		assertf( initializers.size() == designations.size(), "Created ListInit with mismatching initializers (%zd) and designations (%zd)", initializers.size(), designations.size() );
}

ListInit::ListInit( const ListInit & other ) : Initializer( other ) {
	cloneAll( other.initializers, initializers );
	cloneAll( other.designations, designations );
}

ListInit::~ListInit() {
	deleteAll( initializers );
	deleteAll( designations );
}

void ListInit::print( std::ostream &os, Indenter indent ) const {
	os << "Compound initializer: " << std::endl;
	for ( auto p : group_iterate( designations, initializers ) ) {
		const Designation * d = std::get<0>(p);
		const Initializer * init = std::get<1>(p);
		os << indent+1;
		init->print( os, indent+1 );
		os << std::endl;
		if ( ! d->designators.empty() ) {
			os << indent+1;
			d->print( os, indent+1 );
		}
	}
}


ConstructorInit::ConstructorInit( Statement * ctor, Statement * dtor, Initializer * init ) : Initializer( true ), ctor( ctor ), dtor( dtor ), init( init ) {}
ConstructorInit::ConstructorInit( const ConstructorInit &other ) : Initializer( other ), ctor( maybeClone( other.ctor ) ), dtor( maybeClone( other.dtor ) ), init( maybeClone( other.init ) ) {
}

ConstructorInit::~ConstructorInit() {
	delete ctor;
	delete dtor;
	delete init;
}

void ConstructorInit::print( std::ostream &os, Indenter indent ) const {
	os << "Constructor initializer: " << std::endl;
	if ( ctor ) {
		os << indent << "... initially constructed with ";
		ctor->print( os, indent+1 );
	} // if

	if ( dtor ) {
		os << indent << "... destructed with ";
		dtor->print( os, indent+1 );
	}

	if ( init ) {
		os << indent << "... with fallback C-style initializer: ";
		init->print( os, indent+1 );
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
