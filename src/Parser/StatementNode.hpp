//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// StatementNode.hpp --
//
// Author           : Andrew Beach
// Created On       : Wed Apr  5 11:42:00 2023
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Sep 23 22:43:05 2024
// Update Count     : 3
//

#pragma once

#include "ParseNode.hpp"

struct StatementNode final : public ParseList<StatementNode> {
	StatementNode() : stmt( nullptr ) {}
	StatementNode( ast::Stmt * stmt ) : stmt( stmt ) {}
	StatementNode( DeclarationNode * decl );
	virtual ~StatementNode() {}

	virtual StatementNode * clone() const final { assert( false ); return nullptr; }
	ast::Stmt * build() { return stmt.release(); }

	StatementNode * add_label(
			const CodeLocation & location,
			const std::string * name,
			DeclarationNode * attr = nullptr );

	virtual void print( std::ostream & os, __attribute__((unused)) int indent = 0 ) const override {
		os << stmt.get() << std::endl;
	}

	std::unique_ptr<ast::Stmt> stmt;
}; // StatementNode

struct ClauseNode final : public ParseList<ClauseNode> {
	ClauseNode( ast::StmtClause * clause ) : clause( clause ) {}
	virtual ~ClauseNode() {}

	virtual ClauseNode * clone() const final { assert( false ); return nullptr; }
	ast::StmtClause * build() { return clause.release(); }

	virtual ClauseNode * append_last_case( StatementNode * );

	std::unique_ptr<ast::StmtClause> clause;
};

ast::Stmt * build_expr( CodeLocation const &, ExpressionNode * ctrl );

struct CondCtrl {
	CondCtrl( DeclarationNode * decl, ExpressionNode * condition ) :
		init( decl ? new StatementNode( decl ) : nullptr ), condition( condition ) {}

	StatementNode * init;
	ExpressionNode * condition;
};

struct ForCtrl {
	ForCtrl( StatementNode * stmt, ExpressionNode * condition, ExpressionNode * change ) :
		init( stmt ), condition( condition ), change( change ), range_over( nullptr ) {}
	ForCtrl( StatementNode * decl, ExpressionNode * range_over, OperKinds kind ) :
		init( decl ), condition( nullptr ), change( nullptr ), range_over( range_over ), kind( kind ) {}

	StatementNode * init;
	ExpressionNode * condition;
	ExpressionNode * change;
	ExpressionNode * range_over;
	OperKinds kind;
};

ast::Stmt * build_if( const CodeLocation &, CondCtrl * ctrl, StatementNode * then, StatementNode * else_ );
ast::Stmt * build_switch( const CodeLocation &, bool isSwitch, ExpressionNode * ctrl, ClauseNode * stmt );
ast::CaseClause * build_case( const CodeLocation &, ExpressionNode * ctrl );
ast::CaseClause * build_default( const CodeLocation & );
ast::Stmt * build_while( const CodeLocation &, CondCtrl * ctrl, StatementNode * stmt, StatementNode * else_ = nullptr );
ast::Stmt * build_do_while( const CodeLocation &, ExpressionNode * ctrl, StatementNode * stmt, StatementNode * else_ = nullptr );
ast::Stmt * build_for( const CodeLocation &, ForCtrl * forctrl, StatementNode * stmt, StatementNode * else_ = nullptr );
ast::Stmt * build_branch( const CodeLocation &, ast::BranchStmt::Kind kind );
ast::Stmt * build_branch( const CodeLocation &, std::string * identifier, ast::BranchStmt::Kind kind );
ast::Stmt * build_computedgoto( ExpressionNode * ctrl );
ast::Stmt * build_return( const CodeLocation &, ExpressionNode * ctrl );
ast::Stmt * build_throw( const CodeLocation &, ExpressionNode * ctrl );
ast::Stmt * build_resume( const CodeLocation &, ExpressionNode * ctrl );
ast::Stmt * build_resume_at( ExpressionNode * ctrl , ExpressionNode * target );
ast::Stmt * build_try( const CodeLocation &, StatementNode * try_, ClauseNode * catch_, ClauseNode * finally_ );
ast::CatchClause * build_catch( const CodeLocation &, ast::ExceptionKind kind, DeclarationNode * decl, ExpressionNode * cond, StatementNode * body );
ast::FinallyClause * build_finally( const CodeLocation &, StatementNode * stmt );
ast::Stmt * build_compound( const CodeLocation &, StatementNode * first );
StatementNode * maybe_build_compound( const CodeLocation &, StatementNode * first );
ast::Stmt * build_asm( const CodeLocation &, bool is_volatile, ExpressionNode * instruction, ExpressionNode * output = nullptr, ExpressionNode * input = nullptr, ExpressionNode * clobber = nullptr, LabelNode * gotolabels = nullptr );
ast::Stmt * build_directive( const CodeLocation &, std::string * directive );
ast::SuspendStmt * build_suspend( const CodeLocation &, StatementNode *, ast::SuspendStmt::Kind );
ast::WaitForStmt * build_waitfor( const CodeLocation &, ast::WaitForStmt * existing, ExpressionNode * when, ExpressionNode * targetExpr, StatementNode * stmt );
ast::WaitForStmt * build_waitfor_else( const CodeLocation &, ast::WaitForStmt * existing, ExpressionNode * when, StatementNode * stmt );
ast::WaitForStmt * build_waitfor_timeout( const CodeLocation &, ast::WaitForStmt * existing, ExpressionNode * when, ExpressionNode * timeout, StatementNode * stmt );
ast::WaitUntilStmt::ClauseNode * build_waituntil_clause( const CodeLocation &, ExpressionNode * when, ExpressionNode * targetExpr, StatementNode * stmt );
ast::WaitUntilStmt::ClauseNode * build_waituntil_else( const CodeLocation &, ExpressionNode * when, StatementNode * stmt );
ast::WaitUntilStmt * build_waituntil_stmt( const CodeLocation &, ast::WaitUntilStmt::ClauseNode * root );
ast::Stmt * build_with( const CodeLocation &, ExpressionNode * exprs, StatementNode * stmt );
ast::Stmt * build_mutex( const CodeLocation &, ExpressionNode * exprs, StatementNode * stmt );
ast::Stmt * build_corun( const CodeLocation &, StatementNode * stmt );
ast::Stmt * build_cofor( const CodeLocation & location, ForCtrl * forctrl, StatementNode * stmt );
