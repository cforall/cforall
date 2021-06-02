//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TupleExpr.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Aug 14 14:34:00 2019
// Update Count     : 5
//

#include <cassert>              // for assert, strict_dynamic_cast, assertf
#include <iterator>             // for next
#include <list>                 // for list, _List_iterator
#include <ostream>              // for ostream, operator<<, basic_ostream, endl
#include <string>               // for operator<<, string, char_traits

#include "Common/utility.h"     // for cloneAll, deleteAll, printAll, toString
#include "Declaration.h"        // for ObjectDecl
#include "Expression.h"         // for Expression, TupleExpr, TupleIndexExpr
#include "SynTree/Label.h"      // for Label
#include "SynTree/Statement.h"  // for CompoundStmt, DeclStmt, ExprStmt, Sta...
#include "Tuples/Tuples.h"      // for makeTupleType
#include "Type.h"               // for TupleType, Type

UntypedTupleExpr::UntypedTupleExpr( const std::list< Expression * > & exprs ) : Expression(), exprs( exprs ) {
}

UntypedTupleExpr::UntypedTupleExpr( const UntypedTupleExpr &other ) : Expression( other ) {
	cloneAll( other.exprs, exprs );
}

UntypedTupleExpr::~UntypedTupleExpr() {
	deleteAll( exprs );
}

void UntypedTupleExpr::print( std::ostream &os, Indenter indent ) const {
	os << "Untyped Tuple:" << std::endl;
	printAll( exprs, os, indent+1 );
	Expression::print( os, indent );
}

TupleExpr::TupleExpr( const std::list< Expression * > & exprs ) : Expression(), exprs( exprs ) {
	set_result( Tuples::makeTupleType( exprs ) );
}

TupleExpr::TupleExpr( const TupleExpr &other ) : Expression( other ) {
	cloneAll( other.exprs, exprs );
}

TupleExpr::~TupleExpr() {
	deleteAll( exprs );
}

bool TupleExpr::get_lvalue() const {
	return false;
}

void TupleExpr::print( std::ostream &os, Indenter indent ) const {
	os << "Tuple:" << std::endl;
	printAll( exprs, os, indent+1 );
	Expression::print( os, indent );
}

TupleIndexExpr::TupleIndexExpr( Expression * tuple, unsigned int index ) : tuple( tuple ), index( index )  {
	TupleType * type = strict_dynamic_cast< TupleType * >( tuple->get_result() );
	assertf( type->size() > index, "TupleIndexExpr index out of bounds: tuple size %d, requested index %d in expr %s", type->size(), index, toString( tuple ).c_str() );
	set_result( (*std::next( type->get_types().begin(), index ))->clone() );
}

TupleIndexExpr::TupleIndexExpr( const TupleIndexExpr &other ) : Expression( other ), tuple( other.tuple->clone() ), index( other.index ) {
}

TupleIndexExpr::~TupleIndexExpr() {
	delete tuple;
}

bool TupleIndexExpr::get_lvalue() const {
	return tuple->get_lvalue();
}

void TupleIndexExpr::print( std::ostream &os, Indenter indent ) const {
	os << "Tuple Index Expression, with tuple:" << std::endl;
	os << indent+1;
	tuple->print( os, indent+1 );
	os << indent+1 << "with index: " << index << std::endl;
	Expression::print( os, indent );
}

TupleAssignExpr::TupleAssignExpr( const std::list< Expression * > & assigns, const std::list< ObjectDecl * > & tempDecls ) : Expression() {
	// convert internally into a StmtExpr which contains the declarations and produces the tuple of the assignments
	set_result( Tuples::makeTupleType( assigns ) );
	CompoundStmt * compoundStmt = new CompoundStmt();
	std::list< Statement * > & stmts = compoundStmt->get_kids();
	for ( ObjectDecl * obj : tempDecls ) {
		stmts.push_back( new DeclStmt( obj ) );
	}
	TupleExpr * tupleExpr = new TupleExpr( assigns );
	assert( tupleExpr->get_result() );
	stmts.push_back( new ExprStmt( tupleExpr ) );
	stmtExpr = new StmtExpr( compoundStmt );
}

TupleAssignExpr::TupleAssignExpr( const TupleAssignExpr &other ) : Expression( other ) {
	assert( other.stmtExpr );
	stmtExpr = other.stmtExpr->clone();
}

TupleAssignExpr::TupleAssignExpr( 
	StmtExpr * s )
: Expression(), stmtExpr(s) {
}


TupleAssignExpr::~TupleAssignExpr() {
	delete stmtExpr;
}

void TupleAssignExpr::print( std::ostream &os, Indenter indent ) const {
	os << "Tuple Assignment Expression, with stmt expr:" << std::endl;
	os << indent+1;
	stmtExpr->print( os, indent+1 );
	Expression::print( os, indent );
}



// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
