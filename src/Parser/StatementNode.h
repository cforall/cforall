//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// StatementNode.h --
//
// Author           : Andrew Beach
// Created On       : Wed Apr  5 11:42:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Tue Apr 11  9:43:00 2023
// Update Count     : 1
//

#pragma once

#include "ParseNode.h"

struct StatementNode final : public ParseNode {
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

struct ClauseNode final : public ParseNode {
	ClauseNode( ast::StmtClause * clause ) : clause( clause ) {}
	virtual ~ClauseNode() {}

	ClauseNode * set_last( ParseNode * newlast ) {
		ParseNode::set_last( newlast );
        return this;
    }

	virtual ClauseNode * clone() const final { assert( false ); return nullptr; }
	ast::StmtClause * build() { return clause.release(); }

	virtual ClauseNode * append_last_case( StatementNode * );

	std::unique_ptr<ast::StmtClause> clause;
};

ast::Stmt * build_expr( CodeLocation const &, ExpressionNode * ctl );

struct CondCtl {
	CondCtl( DeclarationNode * decl, ExpressionNode * condition ) :
		init( decl ? new StatementNode( decl ) : nullptr ), condition( condition ) {}

	StatementNode * init;
	ExpressionNode * condition;
};

struct ForCtrl {
	ForCtrl( StatementNode * stmt, ExpressionNode * condition, ExpressionNode * change ) :
		init( stmt ), condition( condition ), change( change ) {}

	StatementNode * init;
	ExpressionNode * condition;
	ExpressionNode * change;
};

ast::Stmt * build_if( const CodeLocation &, CondCtl * ctl, StatementNode * then, StatementNode * else_ );
ast::Stmt * build_switch( const CodeLocation &, bool isSwitch, ExpressionNode * ctl, ClauseNode * stmt );
ast::CaseClause * build_case( const CodeLocation &, ExpressionNode * ctl );
ast::CaseClause * build_default( const CodeLocation & );
ast::Stmt * build_while( const CodeLocation &, CondCtl * ctl, StatementNode * stmt, StatementNode * else_ = nullptr );
ast::Stmt * build_do_while( const CodeLocation &, ExpressionNode * ctl, StatementNode * stmt, StatementNode * else_ = nullptr );
ast::Stmt * build_for( const CodeLocation &, ForCtrl * forctl, StatementNode * stmt, StatementNode * else_ = nullptr );
ast::Stmt * build_branch( const CodeLocation &, ast::BranchStmt::Kind kind );
ast::Stmt * build_branch( const CodeLocation &, std::string * identifier, ast::BranchStmt::Kind kind );
ast::Stmt * build_computedgoto( ExpressionNode * ctl );
ast::Stmt * build_return( const CodeLocation &, ExpressionNode * ctl );
ast::Stmt * build_throw( const CodeLocation &, ExpressionNode * ctl );
ast::Stmt * build_resume( const CodeLocation &, ExpressionNode * ctl );
ast::Stmt * build_resume_at( ExpressionNode * ctl , ExpressionNode * target );
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
ast::WaitUntilStmt::ClauseNode * build_waituntil_timeout( const CodeLocation &, ExpressionNode * when, ExpressionNode * timeout, StatementNode * stmt );
ast::WaitUntilStmt * build_waituntil_stmt( const CodeLocation &, ast::WaitUntilStmt::ClauseNode * root );
ast::Stmt * build_with( const CodeLocation &, ExpressionNode * exprs, StatementNode * stmt );
ast::Stmt * build_mutex( const CodeLocation &, ExpressionNode * exprs, StatementNode * stmt );
