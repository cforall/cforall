//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// InitializerNode.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 13:20:24 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul 28 23:27:20 2017
// Update Count     : 26
//

#include <iostream>                // for operator<<, ostream, basic_ostream
#include <list>                    // for list
#include <string>                  // for operator<<, string

using namespace std;

#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for maybeBuild
#include "ParseNode.h"             // for InitializerNode, ExpressionNode
#include "SynTree/Expression.h"    // for Expression
#include "SynTree/Initializer.h"   // for Initializer, ListInit, SingleInit

InitializerNode::InitializerNode( ExpressionNode * _expr, bool aggrp, ExpressionNode * des )
		: expr( _expr ), aggregate( aggrp ), designator( des ), kids( nullptr ), maybeConstructed( true ), isDelete( false ) {
	if ( aggrp )
		kids = dynamic_cast< InitializerNode * >( get_next() );

	if ( kids )
		set_last( nullptr );
} // InitializerNode::InitializerNode

InitializerNode::InitializerNode( InitializerNode * init, bool aggrp, ExpressionNode * des )
		: expr( nullptr ), aggregate( aggrp ), designator( des ), kids( nullptr ), maybeConstructed( true ), isDelete( false ) {
	if ( init )
		set_last( init );

	if ( aggrp )
		kids = dynamic_cast< InitializerNode * >( get_next() );

	if ( kids )
		set_next( nullptr );
} // InitializerNode::InitializerNode

InitializerNode::InitializerNode( bool isDelete ) : expr( nullptr ), aggregate( false ), designator( nullptr ), kids( nullptr ), maybeConstructed( false ), isDelete( isDelete ) {}

InitializerNode::~InitializerNode() {
	delete expr;
	delete designator;
	delete kids;
} // InitializerNode::~InitializerNode

void InitializerNode::print( std::ostream &os, int indent ) const {
	os << std::string( indent, ' ' ) << "Initializer expression" << std::endl;
} // InitializerNode::print

void InitializerNode::printOneLine( std::ostream &os ) const {
	if ( ! aggregate ) {
		if ( designator ) {
			os << "designated by: (";
			ExpressionNode *curdes = designator;
			while ( curdes != nullptr) {
				curdes->printOneLine(os);
				curdes = (ExpressionNode *)(curdes->get_next());
				if ( curdes ) os << ", ";
			} // while
			os << ")";
		} // if
		if ( expr ) expr->printOneLine( os );
	} else {  // It's an aggregate
		os << "[--";
		if ( next_init() != nullptr )
			next_init()->printOneLine( os );
		if (aggregate) os << "--]";
	} // if

	InitializerNode *moreInit;
	if ( (moreInit = dynamic_cast< InitializerNode * >( get_next() ) ) ) {
		moreInit->printOneLine( os );
	} // if
} // InitializerNode::printOneLine

Initializer * InitializerNode::build() const {
	assertf( ! isDelete, "Should not build delete stmt InitializerNode" );
	if ( aggregate ) {
		// steal designators from children
		std::list< Designation * > designlist;
		InitializerNode * child = next_init();
		for ( ; child != nullptr; child = dynamic_cast< InitializerNode * >( child->get_next() ) ) {
			std::list< Expression * > desList;
			buildList< Expression, ExpressionNode >( child->designator, desList );
			designlist.push_back( new Designation( desList ) );
		} // for
		std::list< Initializer * > initlist;
		buildList< Initializer, InitializerNode >( next_init(), initlist );
		return new ListInit( initlist, designlist, maybeConstructed );
	} else {
		if ( get_expression() ) {
			assertf( get_expression()->expr, "The expression of initializer must have value" );
			return new SingleInit( maybeBuild< Expression >( get_expression() ), maybeConstructed );
		} // if
	} // if
	return nullptr;
} // InitializerNode::build

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
