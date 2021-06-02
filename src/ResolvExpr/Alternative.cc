//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Alternative.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sat May 16 23:44:23 2015
// Last Modified By : Aaron B. Moss
// Last Modified On : Thu Oct 11 10:55:00 2018
// Update Count     : 3
//

#include "Alternative.h"

#include <ostream>                       // for operator<<, ostream, basic_o...
#include <string>                        // for operator<<, char_traits, string
#include <utility>                       // for move

#include "Common/utility.h"              // for cloneAll
#include "ResolvExpr/Cost.h"             // for Cost, Cost::zero, operator<<
#include "ResolvExpr/TypeEnvironment.h"  // for TypeEnvironment
#include "SynTree/Expression.h"          // for Expression
#include "SynTree/Type.h"                // for Type

namespace ResolvExpr {
	Alternative::Alternative() 
	: cost( Cost::zero ), cvtCost( Cost::zero ), expr( nullptr ), env(), openVars(), need() {}

	Alternative::Alternative( Expression *expr, const TypeEnvironment &env )
	: cost( Cost::zero ), cvtCost( Cost::zero ), expr( expr ), env( env ), openVars(), need() {}
	
	Alternative::Alternative( const Alternative &o, Expression *expr, const Cost &cost ) 
	: cost( cost ), cvtCost( Cost::zero ), expr( expr ), env( o.env ), openVars( o.openVars ), 
	  need() { cloneAll( o.need, need ); }

	Alternative::Alternative( Expression *expr, const TypeEnvironment &env, 
		const OpenVarSet& openVars, const AssertionList& oneed, const Cost& cost )
	: cost( cost ), cvtCost( Cost::zero ), expr( expr ), env( env ), openVars( openVars ), 
	  need() { cloneAll( oneed, need ); }

	Alternative::Alternative( Expression *expr, const TypeEnvironment &env, 
		const OpenVarSet& openVars, const AssertionList& oneed, const Cost& cost, 
		const Cost &cvtCost )
	: cost( cost ), cvtCost( cvtCost ), expr( expr ), env( env ), openVars( openVars ), 
	  need() { cloneAll( oneed, need ); }
	
	Alternative::Alternative( Expression *expr, const TypeEnvironment &env, 
		const OpenVarSet &openVars, const AssertionSet &oneed, const Cost &cost)
	: cost( cost ), cvtCost( Cost::zero ), expr( expr ), env( env ), openVars( openVars ), 
	  need() { cloneAll( oneed, need ); }
	
	Alternative::Alternative( Expression *expr, const TypeEnvironment &env, 
		const OpenVarSet &openVars, const AssertionSet &oneed, const Cost &cost, 
		const Cost& cvtCost )
	: cost( cost ), cvtCost( cvtCost ), expr( expr ), env( env ), openVars( openVars ), 
	  need() { cloneAll( oneed, need ); }
	
	Alternative::Alternative( Expression *expr, TypeEnvironment &&env, OpenVarSet &&openVars, 
		AssertionSet &&needSet, const Cost &cost )
	: cost( cost ), cvtCost( Cost::zero ), expr( expr ), env( std::move(env) ), 
	  openVars( std::move(openVars) ), need( needSet.begin(), needSet.end() ) {}
	
	Alternative::Alternative( Expression *expr, TypeEnvironment &&env, OpenVarSet &&openVars, 
		AssertionSet &&needSet, const Cost &cost, const Cost &cvtCost )
	: cost( cost ), cvtCost( cvtCost ), expr( expr ), env( std::move(env) ), 
	  openVars( std::move(openVars) ), need( needSet.begin(), needSet.end() ) {}

	Alternative::Alternative( const Alternative &other ) 
	: cost( other.cost ), cvtCost( other.cvtCost ), expr( maybeClone( other.expr ) ), 
	  env( other.env ), openVars( other.openVars ), need() { cloneAll( other.need, need ); }

	Alternative &Alternative::operator=( const Alternative &other ) {
		if ( &other == this ) return *this;
		delete expr;
		cost = other.cost;
		cvtCost = other.cvtCost;
		expr = maybeClone( other.expr );
		env = other.env;
		openVars = other.openVars;
		need.clear();
		cloneAll( other.need, need );
		return *this;
	}

	Alternative::Alternative( Alternative && other ) 
	: cost( other.cost ), cvtCost( other.cvtCost ), expr( other.expr ), 
	  env( std::move( other.env ) ), openVars( std::move( other.openVars ) ), 
	  need( std::move( other.need ) ) { other.expr = nullptr; }

	Alternative & Alternative::operator=( Alternative && other ) {
		if ( &other == this )  return *this;
		delete expr;
		cost = other.cost;
		cvtCost = other.cvtCost;
		expr = other.expr;
		env = std::move( other.env );
		openVars = std::move( other.openVars );
		need = std::move( other.need );
		other.expr = nullptr;
		return *this;
	}

	Alternative::~Alternative() {
		for ( AssertionItem& n : need ) { delete n.decl; }
		delete expr;
	}

	void Alternative::print( std::ostream &os, Indenter indent ) const {
		os << "Cost " << cost << ": ";
		if ( expr ) {
			expr->print( os, indent+1 );
			os << std::endl << indent << "(types:" << std::endl;
			os << indent+1;
			expr->result->print( os, indent+1 );
			os << std::endl << indent << ")" << std::endl;
		} else {
			os << "Null expression!" << std::endl;
		} // if
		os << indent << "Environment:";
		env.print( os, indent+1 );
		os << std::endl;
	}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
