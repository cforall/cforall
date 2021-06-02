//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ReferenceToType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Feb 23 16:38:54 2017
// Update Count     : 24
//

#include <cassert>            // for assert
#include <list>               // for list, _List_const_iterator, list<>::cons...
#include <ostream>            // for operator<<, basic_ostream, ostream, endl
#include <string>             // for string, operator<<, char_traits, operator==

#include "Common/utility.h"   // for printAll, cloneAll, deleteAll
#include "Declaration.h"      // for StructDecl, UnionDecl, EnumDecl, Declara...
#include "Expression.h"       // for Expression
#include "Type.h"             // for TypeInstType, StructInstType, UnionInstType
#include "TypeSubstitution.h" // for TypeSubstitution
#include "CompilationState.h"

class Attribute;

ReferenceToType::ReferenceToType( const Type::Qualifiers &tq, const std::string &name, const std::list< Attribute * > & attributes ) : Type( tq, attributes ), name( name ), hoistType( false ) {
}

ReferenceToType::ReferenceToType( const ReferenceToType &other ) : Type( other ), name( other.name ), hoistType( other.hoistType ) {
	cloneAll( other.parameters, parameters );
}

ReferenceToType::~ReferenceToType() {
	deleteAll( parameters );
}

void ReferenceToType::print( std::ostream &os, Indenter indent ) const {
	using std::endl;

	Type::print( os, indent );
	os << "instance of " << typeString() << " " << name << " ";
	if ( ! parameters.empty() ) {
		os << endl << indent << "... with parameters" << endl;
		printAll( parameters, os, indent+1 );
	} // if
}

namespace {
	void doLookup( const std::list< Declaration * > & members, const std::string & name, std::list< Declaration* > & foundDecls ) {
		for ( Declaration * decl : members ) {
			if ( decl->name == name ) {
				foundDecls.push_back( decl );
			} // if
		} // for
	}
} // namespace

StructInstType::StructInstType( const Type::Qualifiers & tq, StructDecl * baseStruct, const std::list< Attribute * > & attributes ) :
		Parent( tq, baseStruct->name, attributes ), baseStruct( baseStruct ) {}

std::string StructInstType::typeString() const { return "struct"; }

const std::list<TypeDecl*>* StructInstType::get_baseParameters() const {
	if ( ! baseStruct ) return nullptr;
	return &baseStruct->get_parameters();
}

std::list<TypeDecl*>* StructInstType::get_baseParameters() {
	if ( ! baseStruct ) return nullptr;
	return &baseStruct->get_parameters();
}

bool StructInstType::isComplete() const { return baseStruct ? baseStruct->has_body() : false; }

AggregateDecl * StructInstType::getAggr() const { return baseStruct; }

TypeSubstitution StructInstType::genericSubstitution() const {
	return TypeSubstitution( get_baseParameters()->begin(), get_baseParameters()->end(), parameters.begin() );
}

void StructInstType::lookup( const std::string &name, std::list< Declaration* > &foundDecls ) const {
	assert( baseStruct );
	doLookup( baseStruct->members, name, foundDecls );
}

void StructInstType::print( std::ostream &os, Indenter indent ) const {
	using std::endl;

	if ( baseStruct == nullptr ) ReferenceToType::print( os, indent );
	else {
		Type::print( os, indent );
		os << "instance of " << typeString() << " " << name << " with body " << baseStruct->has_body();
		if ( ! parameters.empty() ) {
			os << endl << indent << "... with parameters" << endl;
			printAll( parameters, os, indent+1 );
		} // if
	} // if
}


UnionInstType::UnionInstType( const Type::Qualifiers & tq, UnionDecl * baseUnion, const std::list< Attribute * > & attributes ) :
		Parent( tq, baseUnion->name, attributes ), baseUnion( baseUnion ) {}

std::string UnionInstType::typeString() const { return "union"; }

std::list< TypeDecl * > * UnionInstType::get_baseParameters() {
	if ( ! baseUnion ) return nullptr;
	return &baseUnion->get_parameters();
}

const std::list< TypeDecl * > * UnionInstType::get_baseParameters() const {
	if ( ! baseUnion ) return nullptr;
	return &baseUnion->get_parameters();
}

bool UnionInstType::isComplete() const { return baseUnion ? baseUnion->has_body() : false; }

AggregateDecl * UnionInstType::getAggr() const { return baseUnion; }

TypeSubstitution UnionInstType::genericSubstitution() const {
	return TypeSubstitution( get_baseParameters()->begin(), get_baseParameters()->end(), parameters.begin() );
}

void UnionInstType::lookup( const std::string &name, std::list< Declaration* > &foundDecls ) const {
	assert( baseUnion );
	doLookup( baseUnion->members, name, foundDecls );
}

void UnionInstType::print( std::ostream &os, Indenter indent ) const {
	using std::endl;

	if ( baseUnion == nullptr ) ReferenceToType::print( os, indent );
	else {
		Type::print( os, indent );
		os << "instance of " << typeString() << " " << name << " with body " << baseUnion->has_body();
		if ( ! parameters.empty() ) {
			os << endl << indent << "... with parameters" << endl;
			printAll( parameters, os, indent+1 );
		} // if
	} // if
}


EnumInstType::EnumInstType( const Type::Qualifiers & tq, EnumDecl * baseEnum, const std::list< Attribute * > & attributes ) :
		Parent( tq, baseEnum->get_name(), attributes ), baseEnum( baseEnum ) {}

std::string EnumInstType::typeString() const { return "enum"; }

bool EnumInstType::isComplete() const { return baseEnum ? baseEnum->has_body() : false; }

AggregateDecl * EnumInstType::getAggr() const { return baseEnum; }

void EnumInstType::print( std::ostream &os, Indenter indent ) const {
	using std::endl;

	if ( baseEnum == nullptr ) ReferenceToType::print( os, indent );
	else {
		Type::print( os, indent );
		os << "instance of " << typeString() << " " << name << " with body " << baseEnum->has_body();
	} // if
}


std::string TraitInstType::typeString() const { return "trait"; }

TraitInstType::TraitInstType( const Type::Qualifiers & tq, TraitDecl * baseTrait, const std::list< Attribute * > & attributes ) : Parent( tq, baseTrait->name, attributes ), baseTrait( baseTrait ) {}

TraitInstType::TraitInstType( const TraitInstType &other ) : Parent( other ), baseTrait( other.baseTrait ) {
}

TraitInstType::~TraitInstType() {
}

bool TraitInstType::isComplete() const { assert( false ); }

TypeInstType::TypeInstType( const Type::Qualifiers &tq, const std::string &name, TypeDecl *baseType, const std::list< Attribute * > & attributes ) : Parent( tq, name, attributes ) {
	set_baseType( baseType );
}

TypeInstType::TypeInstType( const Type::Qualifiers &tq, const std::string &name, bool isFtype, const std::list< Attribute * > & attributes ) : Parent( tq, name, attributes ), baseType( 0 ), isFtype( isFtype ) {
}

TypeInstType::TypeInstType( const TypeInstType &other ) : Parent( other ), baseType( other.baseType ), isFtype( other.isFtype ) {
}


TypeInstType::~TypeInstType() {
	// delete baseType; //This is shared and should not be deleted
}

void TypeInstType::set_baseType( TypeDecl *newValue ) {
	baseType = newValue;
	isFtype = newValue->get_kind() == TypeDecl::Ftype;
}

std::string TypeInstType::typeString() const { return "type"; }

bool TypeInstType::isComplete() const { return baseType->isComplete(); }

void TypeInstType::print( std::ostream &os, Indenter indent ) const {
	using std::endl;

	Type::print( os, indent );
	os << "instance of " << typeString() << " ";
	const auto & name_ = get_name();
	if( deterministic_output && isUnboundType(name) ) os << "[unbound]";
	else os << name;
	os << " (" << ( isFtype ? "" : "not" ) << " function type)";
	if ( ! parameters.empty() ) {
		os << endl << indent << "... with parameters" << endl;
		printAll( parameters, os, indent+1 );
	} // if
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
