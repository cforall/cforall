//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Print.hpp -- Print an AST (or sub-tree) to a stream.
//
// Author           : Thierry Delisle
// Created On       : Tue May 21 16:20:15 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once

#include <iosfwd>

#include "AST/Fwd.hpp"
#include "Common/Indenter.hpp"

/*
A note about Indenter, passed by reference and by value.

There are two Indenter usage styles in the CFA code base:

1.	Functional.  Traversal through the AST is by function calls, each passing an Indenter by value.
	To indent further, a caller passes `indent+1` to a call that works at that new level (and
	below); when the call returns, the caller continues at its original level.  Candidate and
	TypeEnvironment print this way.  Their printing starts from error messages or traces, where they
	are near-top-level objects of discussion.  (At their leaves, they call the `print*` functions
	below, which are implemented in style 2.)

2.	Primarily visitor-based.  Traversal through the AST is by the Pass framework, where each
	(post)visit callback runs on a pass core that holds a single Indenter.  To indent further, the
	core applies paired ++/-- to "the indenter," with style-1 exceptions for one-off uses like
	`out << (indent+1)`.  The `print*` implementations and CodeGenerator print this way.

There is also a third style that resembles, but is not, an Indenter style:

3x.	Parser.  When the parser prints the semantic or pruning errors it detects, it is operating on
	its own ("Rodolfo side") data structures, such as DeclarationNode and ParseNode.  These
	have their own print-with-indentation design.  The parser's style is functional, but its
	parameter, likewise named `indent`, is an integer that counts spaces, not levels.  At its fringe
	are __attribute__((...)) occurrences, which are already built as final `ast::` data structures;
	these lead to further calls to the `print*` functions below.

In Print.hpp, the `print*` functions offer a style-1 interface, which supports styles 1 and 3x.

Style-2 usage lets several visitor types collaborate on one coherently indented print job.  Ideally,
any pass could take part.  Currently, it serves only tactically, in the one case that needs it.

That case is CodeGenerator's error path, where an unhandled node type reaches code generation.
CodeGenerator falls back on Printer for the unhandled node itself, and then Printer taps
CodeGenerator back in for the node's descendents.  The two printers coordinate their indentation in
this case.  Supporting the case in this way matters, for reasons commented in
CodeGenerator::postvisit( ast::Node const * ).

The tactical scheme for coordinating the Indenter for CodeGenerator and Printer has CodeGenerator as
a master and Printer as a slave.  CodeGenerator owns an Indenter and lends it through
`continuePrint`; Printer (the pass, proper) borrows its Indenter.  The `print*` interface provides a
fresh Indenter for the pass proper to borrow.

If more indent coordination is needed in the future, one option is a pass mixin, say
WithAmbientIndent, that makes Indenter sharing implicit through a static (or thread-local) variable
(as an approximation of a call-stack-local one).  Implicit sharing would obviate the current
master-slave asymmetry and allow for coordinating across passes that don't care about printing.
*/

namespace ast {

/// Print a node with the given indenter
void print( std::ostream & os, const ast::Node * node, Indenter indent = {} );

/// Print a declaration in its short form
void printShort( std::ostream & os, const ast::Decl * node, Indenter indent = {} );

/// Print at a single level,
/// with the "cont"inuation getting callbacks for children, and
/// with this level's indentation adjustments applied on the borrowed Indenter.
void continuePrint( std::ostream & os, const ast::Node * node, ast::Visitor & cont, Indenter & indent );

/// Print a collection of items
template< typename Coll >
void printAll( std::ostream & os, const Coll & c, Indenter indent = {} ) {
	for ( const auto & i : c ) {
		if ( ! i ) continue;

		os << indent;
		print( os, i, indent );
		os << std::endl;
	}
}

/// Print each cv-qualifier used in the set, followed by a space.
void print( std::ostream & os, CV::Qualifiers );
/// Print each function specifier used in the set, followed by a space.
void print( std::ostream & os, Function::Specs );
/// Print each storage class used in the set, followed by a space.
void print( std::ostream & os, Storage::Classes );

}
