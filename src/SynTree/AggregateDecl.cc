//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AggregateDecl.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 23:56:39 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Dec 16 15:07:20 2019
// Update Count     : 31
//

#include <list>                  // for list
#include <ostream>               // for operator<<, basic_ostream, ostream
#include <string>                // for operator<<, string, char_traits

#include "Attribute.h"           // for Attribute
#include "Common/utility.h"      // for printAll, cloneAll, deleteAll
#include "Declaration.h"         // for AggregateDecl, TypeDecl, Declaration
#include "Expression.h"
#include "Initializer.h"
#include "LinkageSpec.h"         // for Spec, linkageName, Cforall
#include "Type.h"                // for Type, Type::StorageClasses


// These must harmonize with the corresponding AggregateDecl::Aggregate enumerations.
static const char * aggregateNames[] = { "struct", "union", "enum", "exception", "trait", "generator", "coroutine", "monitor", "thread", "NoAggregateName" };

const char * AggregateDecl::aggrString( AggregateDecl::Aggregate aggr ) {
	return aggregateNames[aggr];
}

AggregateDecl::AggregateDecl( const std::string &name, const std::list< Attribute * > & attributes, LinkageSpec::Spec linkage ) : Parent( name, Type::StorageClasses(), linkage ), body( false ), attributes( attributes ) {
}

AggregateDecl::AggregateDecl( const AggregateDecl &other ) : Parent( other ) {
	cloneAll( other.members, members );
	cloneAll( other.parameters, parameters );
	cloneAll( other.attributes, attributes );
	body = other.body;
}

AggregateDecl::~AggregateDecl() {
	deleteAll( attributes );
	deleteAll( parameters );
	deleteAll( members );
}

void AggregateDecl::print( std::ostream &os, Indenter indent ) const {
	using std::string;
	using std::endl;

	os << typeString() << " " << name << ":";
	if ( get_linkage() != LinkageSpec::Cforall ) {
		os << " " << LinkageSpec::name( linkage );
	} // if
	os << " with body " << has_body();

	if ( ! parameters.empty() ) {
		os << endl << indent << "... with parameters" << endl;
		printAll( parameters, os, indent+1 );
	} // if
	if ( ! members.empty() ) {
		os << endl << indent << "... with members" << endl;
		printAll( members, os, indent+1 );
	} // if
	if ( ! attributes.empty() ) {
		os << endl << indent << "... with attributes" << endl;
		printAll( attributes, os, indent+1 );
	} // if
	os << endl;
}

void AggregateDecl::printShort( std::ostream &os, Indenter indent ) const {
	using std::string;
	using std::endl;

	os << typeString() << " " << name << " with body " << has_body() << endl;

	if ( ! parameters.empty() ) {
		os << indent << "... with parameters" << endl;
		printAll( parameters, os, indent+1 );
	} // if
}

const char * StructDecl::typeString() const { return aggrString( kind ); }

StructInstType * StructDecl::makeInst( std::list< Expression * > const & new_parameters ) {
	std::list< Expression * > copy_parameters;
	cloneAll( new_parameters, copy_parameters );
	return makeInst( move( copy( copy_parameters ) ) );
}

StructInstType * StructDecl::makeInst( std::list< Expression * > && new_parameters ) {
	assert( parameters.size() == new_parameters.size() );
	StructInstType * type = new StructInstType( noQualifiers, this );
	type->parameters = std::move( new_parameters );
	return type;
}

const char * UnionDecl::typeString() const { return aggrString( Union ); }

const char * EnumDecl::typeString() const { return aggrString( Enum ); }

const char * TraitDecl::typeString() const { return aggrString( Trait ); }

bool EnumDecl::valueOf( Declaration * enumerator, long long int & value ) {
	if ( enumValues.empty() ) {
		long long int currentValue = 0;
		for ( Declaration * member : members ) {
			ObjectDecl * field = strict_dynamic_cast< ObjectDecl * >( member );
			if ( field->init ) {
				SingleInit * init = strict_dynamic_cast< SingleInit * >( field->init );
				auto result = eval( init->value );
				if ( ! result.second ) SemanticError( init->location, toString( "Non-constexpr in initialization of enumerator: ", field ) );
				currentValue = result.first;
			}
			assertf( enumValues.count( field->name ) == 0, "Enum %s has multiple members with the name %s", name.c_str(), field->name.c_str() );
			enumValues[ field->name ] = currentValue;
			++currentValue;
		}
	}
	if ( enumValues.count( enumerator->name ) ) {
		value = enumValues[ enumerator->name ];
		return true;
	}
	return false;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
