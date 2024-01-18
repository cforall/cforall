//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ParseNode.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 13:28:16 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Dec  9 17:39:34 2023
// Update Count     : 945
//

#pragma once

#include <algorithm>               // for move
#include <cassert>                 // for assert, assertf
#include <iosfwd>                  // for ostream
#include <iterator>                // for back_insert_iterator
#include <list>                    // for list
#include <memory>                  // for unique_ptr, pointer_traits
#include <string>                  // for string

#include "AST/Expr.hpp"            // for Expr, NameExpr LogicalFlag
#include "AST/Fwd.hpp"             // for ptr, Decl, DeclWithType,
#include "AST/Stmt.hpp"            // for Stmt
#include "Common/CodeLocation.h"   // for CodeLocation
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/UniqueName.h"     // for UniqueName
#include "Parser/parserutility.h"  // for maybeBuild, maybeCopy

struct DeclarationNode;
class InitializerNode;
class ExpressionNode;
struct StatementNode;


//##############################################################################

typedef CodeLocation YYLTYPE;
#define YYLTYPE_IS_DECLARED 1 /* alert the parser that we have our own definition */

extern YYLTYPE yylloc;

class ParseNode {
  public:
	ParseNode() {};
	virtual ~ParseNode() { delete next; delete name; };
	virtual ParseNode * clone() const = 0;

	ParseNode * get_next() const { return next; }
	ParseNode * set_next( ParseNode * newlink ) { next = newlink; return this; }

	ParseNode * get_last() {
		ParseNode * current;
		for ( current = this; current->get_next() != nullptr; current = current->get_next() );
		return current;
	}
	ParseNode * set_last( ParseNode * newlast ) {
		if ( newlast != nullptr ) get_last()->set_next( newlast );
		return this;
	}

	virtual void print( __attribute__((unused)) std::ostream & os, __attribute__((unused)) int indent = 0 ) const {}
	virtual void printList( std::ostream & os, int indent = 0 ) const {
		print( os, indent );
		if ( next ) next->print( os, indent );
	}

	static int indent_by;

	ParseNode * next = nullptr;
	const std::string * name = nullptr;
	CodeLocation location = yylloc;
}; // ParseNode

// Must harmonize with OperName.
enum class OperKinds {
	// diadic
	SizeOf, AlignOf, OffsetOf, Plus, Minus, Exp, Mul, Div, Mod, Or, And,
	BitOr, BitAnd, Xor, Cast, LShift, RShift, LThan, GThan, LEThan, GEThan, Eq, Neq,
	Assign, AtAssn, ExpAssn, MulAssn, DivAssn, ModAssn, PlusAssn, MinusAssn, LSAssn, RSAssn, AndAssn, ERAssn, OrAssn,
	Index, Range,
	// monadic
	UnPlus, UnMinus, AddressOf, PointTo, Neg, BitNeg, Incr, IncrPost, Decr, DecrPost,
	Ctor, Dtor,
}; // OperKinds

enum class EnumHiding { Visible, Hide };

struct LabelNode {
	std::vector<ast::Label> labels;
};

std::ostream & operator<<( std::ostream & out, const ParseNode * node );

__attribute__((noreturn)) static inline void SemanticError( const ParseNode * obj, const std::string & error ) {
	SemanticError( obj->location, toString( error, obj ) );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
