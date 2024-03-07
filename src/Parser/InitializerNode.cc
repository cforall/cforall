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
// Last Modified By : Andrew Beach
// Last Modified On : Tue Apr  4 11:18:00 2023
// Update Count     : 27
//

#include "InitializerNode.h"

#include <iostream>                // for operator<<, ostream, basic_ostream
#include <list>                    // for list
#include <string>                  // for operator<<, string

#include "AST/Expr.hpp"            // for Expr
#include "AST/Init.hpp"            // for Designator, Init, ListInit, Sing...
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for maybeBuild
#include "ExpressionNode.h"        // for ExpressionNode
#include "DeclarationNode.h"       // for buildList

using namespace std;

static ast::ConstructFlag toConstructFlag( bool maybeConstructed ) {
	return maybeConstructed ? ast::MaybeConstruct : ast::NoConstruct;
}

InitializerNode::InitializerNode( ExpressionNode * _expr, bool aggrp, ExpressionNode * des )
		: expr( _expr ), aggregate( aggrp ), designator( des ), kids( nullptr ), maybeConstructed( true ), isDelete( false ) {
	if ( aggrp )
		kids = next;

	if ( kids )
		set_last( nullptr );
} // InitializerNode::InitializerNode

InitializerNode::InitializerNode( InitializerNode * init, bool aggrp, ExpressionNode * des )
		: expr( nullptr ), aggregate( aggrp ), designator( des ), kids( nullptr ), maybeConstructed( true ), isDelete( false ) {
	if ( init )
		set_last( init );

	if ( aggrp )
		kids = next;

	if ( kids )
		next = nullptr;
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
				curdes = curdes->next;
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
	if ( ( moreInit = next ) ) {
		moreInit->printOneLine( os );
	} // if
} // InitializerNode::printOneLine

ast::Init * InitializerNode::build() const {
	assertf( ! isDelete, "Should not build delete stmt InitializerNode" );
	if ( aggregate ) {
		// steal designators from children
		std::vector<ast::ptr<ast::Designation>> designlist;
		InitializerNode * child = next_init();
		for ( ; child != nullptr ; child = child->next ) {
			std::deque<ast::ptr<ast::Expr>> desList;
			buildList( child->designator, desList );
			designlist.push_back(
				new ast::Designation( location, std::move( desList ) ) );
		} // for
		std::vector<ast::ptr<ast::Init>> initlist;
		buildList( next_init(), initlist );
		return new ast::ListInit( location,
			std::move( initlist ),
			std::move( designlist ),
			toConstructFlag( maybeConstructed )
		);
	} else if ( get_expression() ) {
		assertf( get_expression()->expr, "The expression of initializer must have value" );
		return new ast::SingleInit( location,
			maybeBuild( get_expression() ),
			toConstructFlag( maybeConstructed )
		);
	} // if
	return nullptr;
} // InitializerNode::build

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
