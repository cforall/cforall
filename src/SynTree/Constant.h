//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Constant.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Jul 10 15:57:38 2019
// Update Count     : 19
//

#pragma once

#include <iosfwd>     // for ostream
#include <string>     // for string
#include <optional>   // for optional

#include "BaseSyntaxNode.h"
#include "Mutator.h"  // for Mutator
#include "Visitor.h"  // for Visitor

class Type;

class Constant : public BaseSyntaxNode {
  public:
	Constant( Type * type, std::string rep, std::optional<unsigned long long> i );
	Constant( const Constant & other );
	Constant & operator=( const Constant & ) = default;
	virtual ~Constant();

	virtual Constant * clone() const override { return new Constant( *this ); }

	Type * get_type() { return type; }
	void set_type( Type * newValue ) { type = newValue; }
	std::string & get_value() { return rep; }
	void set_value( std::string newValue ) { rep = newValue; }
	unsigned long long get_ival() const;

	/// generates a boolean constant of the given bool
	static Constant from_bool( bool b );
	/// generates an integer constant of the given int
	static Constant from_int( int i );
	/// generates an integer constant of the given unsigned long int
	static Constant from_ulong( unsigned long i );
	/// generates a string constant from the given string (char type, unquoted string)
	static Constant from_string( const std::string & string );

	/// generates a null pointer value for the given type. void * if omitted.
	static Constant null( Type * ptrtype = nullptr );

	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Constant * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = 0 ) const override;

	Type * type;
	std::string rep;
	std::optional<unsigned long long> ival;
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
