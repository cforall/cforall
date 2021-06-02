//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AddressExpr.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 23:54:44 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Feb 28 13:13:38 2019
// Update Count     : 10
//

#include <ostream>           // for ostream, operator<<, basic_ostream, endl
#include <string>            // for operator<<, string

#include "Common/utility.h"  // for maybeClone
#include "Expression.h"      // for AddressExpr, Expression
#include "Type.h"            // for PointerType, Type, Type::Qualifiers

// Address expressions are typed based on the following inference rules:
//    E : lvalue T  &..& (n references)
//   &E :        T *&..& (n references)
//
//    E : T  &..&        (m references)
//   &E : T *&..&        (m-1 references)
//
// That is, lvalues becomes

namespace {
	Type * addrType( Type * type ) {
		if ( ReferenceType * refType = dynamic_cast< ReferenceType * >( type ) ) {
			return new ReferenceType( refType->get_qualifiers(), addrType( refType->base ) );
		} else {
			return new PointerType( Type::Qualifiers(), type->clone() );
		}
	}
}

AddressExpr::AddressExpr( Expression *arg ) : Expression(), arg( arg ) {
	if ( arg->result ) {
		if ( arg->get_lvalue() ) {
			// lvalue, retains all layers of reference and gains a pointer inside the references
			set_result( addrType( arg->result ) );
		} else {
			// taking address of non-lvalue -- must be a reference, loses one layer of reference
			if ( ReferenceType * refType = dynamic_cast< ReferenceType * >( arg->result ) ) {
				set_result( addrType( refType->base ) );
			} else {
				SemanticError( arg->result, "Attempt to take address of non-lvalue expression: " );
			} // if
		}
	}
}

AddressExpr::AddressExpr( const AddressExpr &other ) : Expression( other ), arg( maybeClone( other.arg ) ) {
}

AddressExpr::~AddressExpr() {
	delete arg;
}

void AddressExpr::print( std::ostream &os, Indenter indent ) const {
	os << "Address of:" << std::endl;
	if ( arg ) {
		os << indent+1;
		arg->print( os, indent+1 );
	} // if
}

LabelAddressExpr::LabelAddressExpr( const Label &arg ) : arg( arg ) {
	// label address always has type void *
	result = new PointerType( Type::Qualifiers(), new VoidType( Type::Qualifiers() ) );
}
LabelAddressExpr::LabelAddressExpr( const LabelAddressExpr & other ) : Expression( other ), arg( other.arg ) {}
LabelAddressExpr::~LabelAddressExpr() {}

void LabelAddressExpr::print( std::ostream & os, Indenter ) const {
	os << "Address of label:" << arg;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
