//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Initializer.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Aug  9 10:19:00 2017
// Update Count     : 22
//

#pragma once

#include <iosfwd>            // for ostream
#include <list>              // for list, list<>::const_iterator, list<>::it...

#include "BaseSyntaxNode.h"  // for BaseSyntaxNode
#include "Mutator.h"         // for Mutator
#include "Visitor.h"         // for Visitor

class Expression;
class Statement;

// Designation: list of designator (NameExpr, VariableExpr, and ConstantExpr) expressions that specify an object being initialized.
class Designation : public BaseSyntaxNode {
public:
	std::list< Expression * > designators;

	Designation( const std::list< Expression * > & designators );
	Designation( const Designation & other );
	virtual ~Designation();

	std::list< Expression * > & get_designators() { return designators; }

	virtual Designation * clone() const override { return new Designation( *this ); };
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Designation * acceptMutator( Mutator &m ) override { return m.mutate( this ); }
	virtual void print( std::ostream &os, Indenter indent = {} ) const override;
};

const std::list<Designation *> noDesignators;

// Initializer: base class for object initializers (provide default values)
class Initializer : public BaseSyntaxNode {
  public:
	Initializer( bool maybeConstructed );
	Initializer( const Initializer & other );
	virtual ~Initializer();

	bool get_maybeConstructed() const { return maybeConstructed; }

	virtual Initializer *clone() const override = 0;
	virtual void accept( Visitor & v ) override = 0;
	virtual void accept( Visitor & v ) const override = 0;
	virtual Initializer *acceptMutator( Mutator &m ) override = 0;
	virtual void print( std::ostream &os, Indenter indent = {} ) const override = 0;
  private:
	bool maybeConstructed;
};

// SingleInit represents an initializer for a common object (e.g., int x = 4)
class SingleInit : public Initializer {
  public:
	//Constant *value;
	Expression *value;	// has to be a compile-time constant

	SingleInit( Expression *value, bool maybeConstructed = false );
	SingleInit( const SingleInit &other );
	virtual ~SingleInit();

	Expression *get_value() { return value; }
	void set_value( Expression *newValue ) { value = newValue; }

	virtual SingleInit *clone() const override { return new SingleInit( *this); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Initializer *acceptMutator( Mutator &m )  override { return m.mutate( this ); }
	virtual void print( std::ostream &os, Indenter indent = {} ) const override;
};

// ListInit represents an initializer that is composed recursively of a list of initializers; this is used to initialize
// an array or aggregate
class ListInit : public Initializer {
  public:
	std::list<Initializer *> initializers;  // order *is* important
	std::list<Designation *> designations;  // order/length is consistent with initializers

	ListInit( const std::list<Initializer*> &initializers,
			  const std::list<Designation *> &designators = {}, bool maybeConstructed = false );
	ListInit( const ListInit & other );
	virtual ~ListInit();

	std::list<Designation *> & get_designations() { return designations; }
	std::list<Initializer *> & get_initializers() { return initializers; }

	typedef std::list<Initializer*>::iterator iterator;
	typedef std::list<Initializer*>::const_iterator const_iterator;
	iterator begin() { return initializers.begin(); }
	iterator end() { return initializers.end(); }
	const_iterator begin() const { return initializers.begin(); }
	const_iterator end() const { return initializers.end(); }

	virtual ListInit *clone() const override { return new ListInit( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Initializer *acceptMutator( Mutator &m )  override { return m.mutate( this ); }
	virtual void print( std::ostream &os, Indenter indent = {} ) const override;
};

// ConstructorInit represents an initializer that is either a constructor expression or
// a C-style initializer.
// It should not be necessary to create ConstructorInit nodes manually. Instead, set maybeConstructed
// to true on SingleInit or ListInit constructors if object should be constructed.
class ConstructorInit : public Initializer {
  public:
	Statement * ctor;
	Statement * dtor;
	// C-style initializer made up of SingleInit and ListInit nodes to use as a fallback
	// if an appropriate constructor definition is not found by the resolver
	Initializer * init;

	ConstructorInit( Statement * ctor, Statement * dtor, Initializer * init );
	ConstructorInit( const ConstructorInit &other );
	virtual ~ConstructorInit();

	void set_ctor( Statement * newValue ) { ctor = newValue; }
	Statement * get_ctor() const { return ctor; }
	void set_dtor( Statement * newValue ) { dtor = newValue; }
	Statement * get_dtor() const { return dtor; }
	void set_init( Initializer * newValue ) { init = newValue; }
	Initializer * get_init() const { return init; }

	ConstructorInit *clone() const override { return new ConstructorInit( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Initializer *acceptMutator( Mutator &m )  override { return m.mutate( this ); }
	virtual void print( std::ostream &os, Indenter indent = {} ) const override;

  private:
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
