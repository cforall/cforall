//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExpressionNode.h --
//
// Author           : Andrew Beach
// Created On       : Wed Apr  5 11:34:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr  5 11:50:00 2023
// Update Count     : 0
//

#pragma once

#include "ParseNode.h"

class InitializerNode;

class ExpressionNode final : public ParseNode {
public:
	ExpressionNode( ast::Expr * expr = nullptr ) : expr( expr ) {}
	virtual ~ExpressionNode() {}
	virtual ExpressionNode * clone() const override {
		if ( nullptr == expr ) return nullptr;
		return static_cast<ExpressionNode*>(
			(new ExpressionNode( ast::shallowCopy( expr.get() ) ))->set_next( maybeCopy( get_next() ) ));
	}

	bool get_extension() const { return extension; }
	ExpressionNode * set_extension( bool exten ) { extension = exten; return this; }

	virtual void print( std::ostream & os, __attribute__((unused)) int indent = 0 ) const override {
		os << expr.get();
	}
	void printOneLine( __attribute__((unused)) std::ostream & os, __attribute__((unused)) int indent = 0 ) const {}

	template<typename T>
	bool isExpressionType() const {  return nullptr != dynamic_cast<T>(expr.get()); }

	ast::Expr * build() {
		ast::Expr * node = expr.release();
		node->set_extension( this->get_extension() );
		node->location = this->location;
		return node;
	}

	// Public because of lifetime implications (what lifetime implications?)
	std::unique_ptr<ast::Expr> expr;
private:
	bool extension = false;
}; // ExpressionNode

// These 4 routines modify the string:
ast::Expr * build_constantInteger( const CodeLocation &, std::string & );
ast::Expr * build_constantFloat( const CodeLocation &, std::string & );
ast::Expr * build_constantChar( const CodeLocation &, std::string & );
ast::Expr * build_constantStr( const CodeLocation &, std::string & );
ast::Expr * build_field_name_FLOATING_FRACTIONconstant( const CodeLocation &, const std::string & str );
ast::Expr * build_field_name_FLOATING_DECIMALconstant( const CodeLocation &, const std::string & str );
ast::Expr * build_field_name_FLOATINGconstant( const CodeLocation &, const std::string & str );
ast::Expr * build_field_name_fraction_constants( const CodeLocation &, ast::Expr * fieldName, ExpressionNode * fracts );

ast::NameExpr * build_varref( const CodeLocation &, const std::string * name );
ast::QualifiedNameExpr * build_qualified_expr( const CodeLocation &, const DeclarationNode * decl_node, const ast::NameExpr * name );
ast::QualifiedNameExpr * build_qualified_expr( const CodeLocation &, const ast::EnumDecl * decl, const ast::NameExpr * name );
ast::DimensionExpr * build_dimensionref( const CodeLocation &, const std::string * name );

ast::Expr * build_cast( const CodeLocation &, DeclarationNode * decl_node, ExpressionNode * expr_node, ast::CastExpr::CastKind kind = ast::CastExpr::Default );
ast::Expr * build_keyword_cast( const CodeLocation &, ast::AggregateDecl::Aggregate target, ExpressionNode * expr_node );
ast::Expr * build_virtual_cast( const CodeLocation &, DeclarationNode * decl_node, ExpressionNode * expr_node );
ast::Expr * build_fieldSel( const CodeLocation &, ExpressionNode * expr_node, ast::Expr * member );
ast::Expr * build_pfieldSel( const CodeLocation &, ExpressionNode * expr_node, ast::Expr * member );
ast::Expr * build_offsetOf( const CodeLocation &, DeclarationNode * decl_node, ast::NameExpr * member );
ast::Expr * build_and( const CodeLocation &, ExpressionNode * expr_node1, ExpressionNode * expr_node2 );
ast::Expr * build_and_or( const CodeLocation &, ExpressionNode * expr_node1, ExpressionNode * expr_node2, ast::LogicalFlag flag );
ast::Expr * build_unary_val( const CodeLocation &, OperKinds op, ExpressionNode * expr_node );
ast::Expr * build_binary_val( const CodeLocation &, OperKinds op, ExpressionNode * expr_node1, ExpressionNode * expr_node2 );
ast::Expr * build_cond( const CodeLocation &, ExpressionNode * expr_node1, ExpressionNode * expr_node2, ExpressionNode * expr_node3 );
ast::Expr * build_tuple( const CodeLocation &, ExpressionNode * expr_node = nullptr );
ast::Expr * build_func( const CodeLocation &, ExpressionNode * function, ExpressionNode * expr_node );
ast::Expr * build_compoundLiteral( const CodeLocation &, DeclarationNode * decl_node, InitializerNode * kids );

ast::Expr * build_enum_pos_expr( const CodeLocation &, ast::Expr * expr_node );