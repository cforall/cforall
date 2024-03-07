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
struct InitializerNode;
struct ExpressionNode;
struct StatementNode;


//##############################################################################

typedef CodeLocation YYLTYPE;
#define YYLTYPE_IS_DECLARED 1 /* alert the parser that we have our own definition */

extern YYLTYPE yylloc;

struct ParseNode {
	ParseNode() {}
	virtual ~ParseNode() {}
	virtual ParseNode * clone() const = 0;

	virtual void print( __attribute__((unused)) std::ostream & os, __attribute__((unused)) int indent = 0 ) const {}

	static int indent_by;

	CodeLocation location = yylloc;
}; // ParseNode

/// Only ever use in the form `struct NAME final : public ParseList<NAME>`!
template<typename Next>
struct ParseList : public ParseNode {
	ParseList() {}
	virtual ~ParseList() { delete next; };
	virtual ParseList<Next> * clone() const = 0;

	Next * get_last() {
		Next * current = static_cast<Next *>( this );
		while ( current->next != nullptr ) current = current->next;
		return current;
	}
	Next * set_last( Next * newlast ) {
		if ( newlast != nullptr ) get_last()->next = newlast;
		return static_cast<Next *>( this );
	}

	virtual void printList( std::ostream & os, int indent = 0 ) const {
		print( os, indent );
		if ( next ) next->print( os, indent );
	}

	Next * next = nullptr;
};

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
