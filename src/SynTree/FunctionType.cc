//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FunctionType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  1 17:21:00 2017
// Update Count     : 2
//

#include <list>              // for list
#include <ostream>           // for operator<<, basic_ostream, ostream, endl
#include <string>            // for operator<<, char_traits, string

#include "Common/utility.h"  // for cloneAll, deleteAll, printAll
#include "Declaration.h"     // for DeclarationWithType
#include "Tuples/Tuples.h"   // for isTtype
#include "Type.h"            // for FunctionType, Type, Type::Qualifiers

class Attribute;

FunctionType::FunctionType( const Type::Qualifiers &tq, bool isVarArgs, const std::list< Attribute * > & attributes ) : Type( tq, attributes ), isVarArgs( isVarArgs ) {
}

FunctionType::FunctionType( const FunctionType &other ) : Type( other ), isVarArgs( other.isVarArgs ) {
	cloneAll( other.returnVals, returnVals );
	cloneAll( other.parameters, parameters );
}

FunctionType::~FunctionType() {
	deleteAll( returnVals );
	deleteAll( parameters );
}

namespace {
	bool containsTtype( const std::list<DeclarationWithType * > & l ) {
		if ( ! l.empty() ) {
			return Tuples::isTtype( l.back()->get_type() );
		}
		return false;
	}
}

bool FunctionType::isTtype() const {
	return containsTtype( returnVals ) || containsTtype( parameters );
}

void FunctionType::print( std::ostream &os, Indenter indent ) const {
	using std::string;
	using std::endl;

	Type::print( os, indent );
	os << "function" << endl;
	if ( ! parameters.empty() ) {
		os << indent << "... with parameters" << endl;
		printAll( parameters, os, indent+1 );
		if ( isVarArgs ) {
			os << indent+1 << "and a variable number of other arguments" << endl;
		} // if
	} else if ( isVarArgs ) {
		os << indent+1 << "accepting unspecified arguments" << endl;
	} // if
	os << indent << "... returning";
	if ( returnVals.empty() ) {
		os << " nothing" << endl;
	} else {
		os << endl;
		printAll( returnVals, os, indent+1 );
	} // if
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
