//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ApplicationExpr.cc.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Aug 12 14:28:00 2019
// Update Count     : 5
//

#include <cassert>               // for strict_dynamic_cast, assert
#include <list>                  // for list
#include <map>                   // for _Rb_tree_const_iterator, map, map<>:...
#include <memory>                // for unique_ptr
#include <ostream>               // for operator<<, ostream, basic_ostream
#include <string>                // for operator<<, string, char_traits
#include <utility>               // for pair

#include "Common/utility.h"      // for maybeClone, cloneAll, deleteAll, pri...
#include "Declaration.h"         // for Declaration
#include "Expression.h"          // for ParamEntry, ApplicationExpr, Expression
#include "InitTweak/InitTweak.h" // for getFunction
#include "ResolvExpr/typeops.h"  // for extractResultType
#include "Type.h"                // for Type, PointerType, FunctionType

ParamEntry::ParamEntry( UniqueId decl, Declaration * declptr, Type * actualType, Type * formalType, Expression* expr )
		: decl( decl ), declptr( declptr ), actualType( actualType ), formalType( formalType ), expr( expr ) {
	}

ParamEntry::ParamEntry( const ParamEntry &other ) :
		decl( other.decl ), declptr( other.declptr ), actualType( maybeClone( other.actualType ) ), formalType( maybeClone( other.formalType ) ), expr( maybeClone( other.expr ) ) {
}

ParamEntry::~ParamEntry() {
	// delete declptr;
	delete actualType;
	delete formalType;
	delete expr;
}

ParamEntry::ParamEntry( ParamEntry && other ) :
		decl( other.decl ), declptr( other.declptr ), actualType( other.actualType ), formalType( other.formalType ), expr( other.expr ) {
	new (&other) ParamEntry();
}

ParamEntry & ParamEntry::operator=( ParamEntry && other ) {
	if ( &other == this ) return *this;
	this->~ParamEntry();
	new (this) ParamEntry(other.decl, other.declptr, other.actualType, other.formalType, other.expr);
	new (&other) ParamEntry();

	return *this;
}

ApplicationExpr::ApplicationExpr( Expression *funcExpr, const std::list<Expression *> & args ) : function( funcExpr ), args( args ) {
	PointerType *pointer = strict_dynamic_cast< PointerType* >( funcExpr->get_result() );
	FunctionType *function = strict_dynamic_cast< FunctionType* >( pointer->get_base() );

	set_result( ResolvExpr::extractResultType( function ) );

	assert( result );
}

ApplicationExpr::ApplicationExpr( const ApplicationExpr &other ) :
		Expression( other ), function( maybeClone( other.function ) ) {
	cloneAll( other.args, args );
}

ApplicationExpr::~ApplicationExpr() {
	delete function;
	deleteAll( args );
}

bool ApplicationExpr::get_lvalue() const {
	// from src/GenPoly/Lvalue.cc: isIntrinsicReference
	static std::set<std::string> lvalueFunctions = { "*?", "?[?]" };
	if ( const DeclarationWithType * func = InitTweak::getFunction( this ) ) {
		return func->linkage == LinkageSpec::Intrinsic && lvalueFunctions.count(func->name);
	}
	return false;
}

void ApplicationExpr::print( std::ostream &os, Indenter indent ) const {
	os << "Application of" << std::endl << indent+1;
	function->print( os, indent+1 );
	os << std::endl;
	if ( ! args.empty() ) {
		os << indent << "... to arguments" << std::endl;
		printAll( args, os, indent+1 );
	} // if
	Expression::print( os, indent );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
