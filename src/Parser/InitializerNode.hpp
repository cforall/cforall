//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// InitializerNode.hpp --
//
// Author           : Andrew Beach
// Created On       : Wed Apr  5 11:31:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr  5 11:48:00 2023
// Update Count     : 0
//

#pragma once

#include "ParseNode.hpp"

struct InitializerNode final : public ParseList<InitializerNode> {
	InitializerNode( ExpressionNode *, bool aggrp = false, ExpressionNode * des = nullptr );
	InitializerNode( InitializerNode *, bool aggrp = false, ExpressionNode * des = nullptr );
	InitializerNode( bool isDelete );
	~InitializerNode();
	virtual InitializerNode * clone() const { assert( false ); return nullptr; }

	ExpressionNode * get_expression() const { return expr; }

	InitializerNode * set_designators( ExpressionNode * des ) { designator = des; return this; }
	ExpressionNode * get_designators() const { return designator; }

	InitializerNode * set_maybeConstructed( bool value ) { maybeConstructed = value; return this; }
	bool get_maybeConstructed() const { return maybeConstructed; }

	bool get_isDelete() const { return isDelete; }

	InitializerNode * next_init() const { return kids; }

	void print( std::ostream & os, int indent = 0 ) const;
	void printOneLine( std::ostream & ) const;

	virtual ast::Init * build() const;
private:
	ExpressionNode * expr;
	bool aggregate;
	ExpressionNode * designator;                        // may be list
	InitializerNode * kids;
	bool maybeConstructed;
	bool isDelete;
}; // InitializerNode
