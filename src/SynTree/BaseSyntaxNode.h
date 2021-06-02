//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// BaseSyntaxNode.h --
//
// Author           : Thierry Delisle
// Created On       : Tue Feb 14 07:44:20 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Jul 10 16:13:49 2019
// Update Count     : 4
//

#pragma once

#include "Common/CodeLocation.h"
#include "Common/Indenter.h"
#include "Common/Stats.h"

class Visitor;
class Mutator;

class BaseSyntaxNode {
  public:
	static Stats::Counters::SimpleCounter* new_nodes;

	CodeLocation location;

	BaseSyntaxNode() { ++*new_nodes; }
	BaseSyntaxNode( const BaseSyntaxNode & o ) : location(o.location) { ++*new_nodes; }
	BaseSyntaxNode & operator=( const BaseSyntaxNode & ) = default;

	virtual ~BaseSyntaxNode() {}

	virtual BaseSyntaxNode * clone() const = 0;
	virtual void accept( Visitor & v ) = 0;
	virtual void accept( Visitor & v ) const = 0;
	virtual BaseSyntaxNode * acceptMutator( Mutator & m ) = 0;
	/// Notes:
	/// * each node is responsible for indenting its children.
	/// * Expressions should not finish with a newline, since the expression's parent has better information.
	virtual void print( std::ostream & os, Indenter indent = {} ) const = 0;
};

std::ostream & operator<<( std::ostream & out, const BaseSyntaxNode * node );

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
