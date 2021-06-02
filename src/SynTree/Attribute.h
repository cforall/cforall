//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Declaration.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Feb 13 21:34:08 2020
// Update Count     : 40
//

#pragma once

#include <iosfwd>  // for ostream
#include <list>    // for list
#include <string>  // for string, operator==

#include "BaseSyntaxNode.h"
#include "Mutator.h"
#include "Visitor.h"

class Expression;

// GCC attribute
// https://gcc.gnu.org/onlinedocs/gcc-6.1.0/gcc/Attribute-Syntax.html#Attribute-Syntax
class Attribute : public BaseSyntaxNode {
  public:
	std::string name;
	// to keep things nice and tight, use NameExpr for special identifier parameters
	std::list< Expression * > parameters;

	Attribute( std::string name = "", const std::list< Expression * > & parameters = std::list< Expression * >() ) : name( name ), parameters( parameters ) {}
	Attribute( const Attribute &other );
	virtual ~Attribute();

	const std::string & get_name() const { return name; }
	void set_name( const std::string & newValue ) { name = newValue; }
	std::list< Expression * > & get_parameters() { return parameters; }
	bool empty() const { return name == ""; }

	std::string normalizedName() const;

	/// true if this attribute is allowed to appear attached to a function parameter
	bool isValidOnFuncParam() const;

	Attribute * clone() const override { return new Attribute( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Attribute * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

const std::list< Attribute * > noAttributes;

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
