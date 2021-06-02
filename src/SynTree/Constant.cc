//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Constant.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb 13 18:11:22 2019
// Update Count     : 32
//

#include <cassert>   // for strict_dynamic_cast, assertf
#include <iostream>  // for operator<<, ostream, basic_ostream
#include <string>    // for to_string, string, char_traits, operator<<

#include "Constant.h"
#include "Expression.h" // for ConstantExpr
#include "Type.h"    // for BasicType, Type, Type::Qualifiers, PointerType

Constant::Constant( Type * type, std::string rep, std::optional<unsigned long long> ival ) : type( type ), rep( rep ), ival( ival ) {}

Constant::Constant( const Constant &other ) : BaseSyntaxNode( other ), rep( other.rep ), ival( other.ival ) {
	type = other.type->clone();
}

Constant::~Constant() { delete type; }

Constant Constant::from_bool( bool b ) {
	return Constant( new BasicType( Type::Qualifiers(), BasicType::Bool ), b ? "1" : "0" , (unsigned long long int)b );
}

Constant Constant::from_int( int i ) {
	return Constant( new BasicType( Type::Qualifiers(), BasicType::SignedInt ), std::to_string( i ), (unsigned long long int)i );
}

Constant Constant::from_ulong( unsigned long i ) {
	return Constant( new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ), std::to_string( i ), (unsigned long long int)i );
}

Constant Constant::from_string( const std::string & str ) {
	Type * charType = new BasicType( noQualifiers, BasicType::Char );
	// Adjust the length of the string for the terminator.
	Expression * strSize = new ConstantExpr( Constant::from_ulong( str.size() + 1 ) );
	Type * strType = new ArrayType( noQualifiers, charType, strSize, false, false );
	const std::string strValue = "\"" + str + "\"";
	return Constant( strType, strValue, std::nullopt );
}

Constant Constant::null( Type * ptrtype ) {
	if ( nullptr == ptrtype ) {
		ptrtype = new PointerType(
			Type::Qualifiers(),
			new VoidType( Type::Qualifiers() )
			);
	}

	return Constant( ptrtype, "0", (unsigned long long int)0 );
}

unsigned long long Constant::get_ival() const {
	assertf( strict_dynamic_cast<BasicType*>(type)->isInteger(), "Attempt to retrieve ival from non-integer constant." );
	return ival.value();
}

void Constant::print( std::ostream &os, Indenter ) const {
	os << "(" << rep << " " << (ival ? toString(ival.value()) : "") ;
	if ( type ) {
		os << ": ";
		type->print( os );
	} // if
  os << ")";
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
