//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// StatementNode.cc -- Transform from parse data-structures to AST data-structures, usually deleting the parse
//     data-structure after the transformation.
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 14:59:41 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Apr 11 10:16:00 2023
// Update Count     : 428
//

#include "StatementNode.h"

#include <cassert>                 // for assert, strict_dynamic_cast, assertf
#include <memory>                  // for unique_ptr
#include <string>                  // for string

#include "AST/Label.hpp"           // for Label
#include "AST/Stmt.hpp"            // for Stmt, AsmStmt, BranchStmt, CaseCla...
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for maybeMoveBuild, maybeBuild
#include "DeclarationNode.h"       // for DeclarationNode
#include "ExpressionNode.h"        // for ExpressionNode
#include "parserutility.h"         // for notZeroExpr

class Declaration;

using namespace std;

// Some helpers for cases that really want a single node but check for lists.
static const ast::Stmt * buildMoveSingle( StatementNode * node ) {
	std::vector<ast::ptr<ast::Stmt>> list;
	buildMoveList( node, list );
	assertf( list.size() == 1, "CFA Internal Error: Extra/Missing Nodes" );
	return list.front().release();
}

static const ast::Stmt * buildMoveOptional( StatementNode * node ) {
	std::vector<ast::ptr<ast::Stmt>> list;
	buildMoveList( node, list );
	assertf( list.size() <= 1, "CFA Internal Error: Extra Nodes" );
	return list.empty() ? nullptr : list.front().release();
}

StatementNode::StatementNode( DeclarationNode * decl ) {
	assert( decl );
	DeclarationNode * agg = decl->extractAggregate();
	if ( agg ) {
		StatementNode * nextStmt = new StatementNode(
			new ast::DeclStmt( decl->location, maybeBuild( decl ) ) );
		set_next( nextStmt );
		if ( decl->get_next() ) {
			get_next()->set_next( new StatementNode( dynamic_cast< DeclarationNode * >(decl->get_next()) ) );
			decl->set_next( 0 );
		} // if
	} else {
		if ( decl->get_next() ) {
			set_next( new StatementNode( dynamic_cast< DeclarationNode * >( decl->get_next() ) ) );
			decl->set_next( 0 );
		} // if
		agg = decl;
	} // if
	// Local copy to avoid accessing the pointer after it is moved from.
	CodeLocation declLocation = agg->location;
	stmt.reset( new ast::DeclStmt( declLocation, maybeMoveBuild( agg ) ) );
} // StatementNode::StatementNode

StatementNode * StatementNode::add_label(
		const CodeLocation & location,
		const std::string * name,
		DeclarationNode * attr ) {
	stmt->labels.emplace_back( location,
		*name,
		attr ? std::move( attr->attributes )
			: std::vector<ast::ptr<ast::Attribute>>{} );
	delete attr;
	delete name;
	return this;
}

ClauseNode * ClauseNode::append_last_case( StatementNode * stmt ) {
	ClauseNode * prev = this;
	// find end of list and maintain previous pointer
	for ( ClauseNode * curr = prev; curr != nullptr; curr = (ClauseNode *)curr->get_next() ) {
		ClauseNode * node = strict_dynamic_cast< ClauseNode * >(curr);
		assert( dynamic_cast<ast::CaseClause *>( node->clause.get() ) );
		prev = curr;
	} // for
	ClauseNode * node = dynamic_cast< ClauseNode * >(prev);
	// convert from StatementNode list to Statement list
	std::vector<ast::ptr<ast::Stmt>> stmts;
	buildMoveList( stmt, stmts );
	// splice any new Statements to end of current Statements
	auto caseStmt = strict_dynamic_cast<ast::CaseClause *>( node->clause.get() );
	for ( auto const & newStmt : stmts ) {
		caseStmt->stmts.emplace_back( newStmt );
	}
	stmts.clear();
	return this;
} // ClauseNode::append_last_case

ast::Stmt * build_expr( CodeLocation const & location, ExpressionNode * ctl ) {
	if ( ast::Expr * e = maybeMoveBuild( ctl ) ) {
		return new ast::ExprStmt( location, e );
	} else {
		return new ast::NullStmt( location );
	}
} // build_expr

static ast::Expr * build_if_control( CondCtl * ctl,
		std::vector<ast::ptr<ast::Stmt>> & inits ) {
	assert( inits.empty() );
	if ( nullptr != ctl->init ) {
		buildMoveList( ctl->init, inits );
	} // if

	ast::Expr * cond = nullptr;
	if ( ctl->condition ) {
		// compare the provided condition against 0
		cond = notZeroExpr( maybeMoveBuild( ctl->condition ) );
	} else {
		for ( ast::ptr<ast::Stmt> & stmt : inits ) {
			// build the && of all of the declared variables compared against 0
			auto declStmt = stmt.strict_as<ast::DeclStmt>();
			auto dwt = declStmt->decl.strict_as<ast::DeclWithType>();
			ast::Expr * nze = notZeroExpr( new ast::VariableExpr( dwt->location, dwt ) );
			cond = cond ? new ast::LogicalExpr( dwt->location, cond, nze, ast::AndExpr ) : nze;
		}
	}
	delete ctl;
	return cond;
} // build_if_control

ast::Stmt * build_if( const CodeLocation & location, CondCtl * ctl, StatementNode * then, StatementNode * else_ ) {
	std::vector<ast::ptr<ast::Stmt>> astinit;						// maybe empty
	ast::Expr * astcond = build_if_control( ctl, astinit ); // ctl deleted, cond/init set

	ast::Stmt const * astthen = buildMoveSingle( then );
	ast::Stmt const * astelse = buildMoveOptional( else_ );

	return new ast::IfStmt( location, astcond, astthen, astelse,
		std::move( astinit )
	);
} // build_if

ast::Stmt * build_switch( const CodeLocation & location, bool isSwitch, ExpressionNode * ctl, ClauseNode * stmt ) {
	std::vector<ast::ptr<ast::CaseClause>> aststmt;
	buildMoveList( stmt, aststmt );
	// If it is not a switch it is a choose statement.
	if ( ! isSwitch ) {
		for ( ast::ptr<ast::CaseClause> & stmt : aststmt ) {
			// Code after "case" is the end of case list.
			if ( !stmt->stmts.empty() ) {
				auto mutStmt = ast::mutate( stmt.get() );
				// I believe the stmts are actually always one block.
				auto stmts = mutStmt->stmts.front().get_and_mutate();
				auto block = strict_dynamic_cast<ast::CompoundStmt *>( stmts );
				block->kids.push_back( new ast::BranchStmt( block->location,
					ast::BranchStmt::Break,
					ast::Label( block->location ) ) );
				stmt = mutStmt;
			} // if
		} // for
	} // if
	// aststmt.size() == 0 for switch (...) {}, i.e., no declaration or statements
	return new ast::SwitchStmt( location,
		maybeMoveBuild( ctl ), std::move( aststmt ) );
} // build_switch

ast::CaseClause * build_case( const CodeLocation & location, ExpressionNode * ctl ) {
	// stmt starts empty and then added to
	auto expr = maybeMoveBuild( ctl );
	return new ast::CaseClause( location, expr, {} );
} // build_case

ast::CaseClause * build_default( const CodeLocation & location ) {
	// stmt starts empty and then added to
	return new ast::CaseClause( location, nullptr, {} );
} // build_default

ast::Stmt * build_while( const CodeLocation & location, CondCtl * ctl, StatementNode * stmt, StatementNode * else_ ) {
	std::vector<ast::ptr<ast::Stmt>> astinit;						// maybe empty
	ast::Expr * astcond = build_if_control( ctl, astinit ); // ctl deleted, cond/init set

	return new ast::WhileDoStmt( location,
		astcond,
		buildMoveSingle( stmt ),
		buildMoveOptional( else_ ),
		std::move( astinit ),
		ast::While
	);
} // build_while

ast::Stmt * build_do_while( const CodeLocation & location, ExpressionNode * ctl, StatementNode * stmt, StatementNode * else_ ) {
	// do-while cannot have declarations in the contitional, so init is always empty
	return new ast::WhileDoStmt( location,
		notZeroExpr( maybeMoveBuild( ctl ) ),
		buildMoveSingle( stmt ),
		buildMoveOptional( else_ ),
		{},
		ast::DoWhile
	);
} // build_do_while

ast::Stmt * build_for( const CodeLocation & location, ForCtrl * forctl, StatementNode * stmt, StatementNode * else_ ) {
	std::vector<ast::ptr<ast::Stmt>> astinit;						// maybe empty
	buildMoveList( forctl->init, astinit );

	ast::Expr * astcond = nullptr;						// maybe empty
	astcond = notZeroExpr( maybeMoveBuild( forctl->condition ) );

	ast::Expr * astincr = nullptr;						// maybe empty
	astincr = maybeMoveBuild( forctl->change );
	delete forctl;

	return new ast::ForStmt( location,
		std::move( astinit ),
		astcond,
		astincr,
		buildMoveSingle( stmt ),
		buildMoveOptional( else_ )
	);
} // build_for

ast::Stmt * build_branch( const CodeLocation & location, ast::BranchStmt::Kind kind ) {
	return new ast::BranchStmt( location,
		kind,
		ast::Label( location )
	);
} // build_branch

ast::Stmt * build_branch( const CodeLocation & location, string * identifier, ast::BranchStmt::Kind kind ) {
	ast::Stmt * ret = new ast::BranchStmt( location,
		kind,
		ast::Label( location, *identifier )
	);
	delete identifier; 									// allocated by lexer
	return ret;
} // build_branch

ast::Stmt * build_computedgoto( ExpressionNode * ctl ) {
	ast::Expr * expr = maybeMoveBuild( ctl );
	return new ast::BranchStmt( expr->location, expr );
} // build_computedgoto

ast::Stmt * build_return( const CodeLocation & location, ExpressionNode * ctl ) {
	std::vector<ast::ptr<ast::Expr>> exps;
	buildMoveList( ctl, exps );
	return new ast::ReturnStmt( location,
		exps.size() > 0 ? exps.back().release() : nullptr
	);
} // build_return

static ast::Stmt * build_throw_stmt(
		const CodeLocation & location,
		ExpressionNode * ctl,
		ast::ExceptionKind kind ) {
	std::vector<ast::ptr<ast::Expr>> exps;
	buildMoveList( ctl, exps );
	assertf( exps.size() < 2, "CFA internal error: leaking memory" );
	return new ast::ThrowStmt( location,
		kind,
		!exps.empty() ? exps.back().release() : nullptr,
		(ast::Expr *)nullptr
	);
}

ast::Stmt * build_throw( const CodeLocation & loc, ExpressionNode * ctl ) {
	return build_throw_stmt( loc, ctl, ast::Terminate );
} // build_throw

ast::Stmt * build_resume( const CodeLocation & loc, ExpressionNode * ctl ) {
	return build_throw_stmt( loc, ctl, ast::Resume );
} // build_resume

ast::Stmt * build_resume_at( ExpressionNode * ctl, ExpressionNode * target ) {
	(void)ctl;
	(void)target;
	assertf( false, "resume at (non-local throw) is not yet supported," );
} // build_resume_at

ast::Stmt * build_try( const CodeLocation & location, StatementNode * try_, ClauseNode * catch_, ClauseNode * finally_ ) {
	std::vector<ast::ptr<ast::CatchClause>> aststmt;
	buildMoveList( catch_, aststmt );
	ast::CompoundStmt * tryBlock = strict_dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( try_ ) );
	ast::FinallyClause * finallyBlock = nullptr;
	if ( finally_ ) {
		finallyBlock = dynamic_cast<ast::FinallyClause *>( finally_->clause.release() );
	}
	return new ast::TryStmt( location,
		tryBlock,
		std::move( aststmt ),
		finallyBlock
	);
} // build_try

ast::CatchClause * build_catch( const CodeLocation & location, ast::ExceptionKind kind, DeclarationNode * decl, ExpressionNode * cond, StatementNode * body ) {
	return new ast::CatchClause( location,
		kind,
		maybeMoveBuild( decl ),
		maybeMoveBuild( cond ),
		buildMoveSingle( body )
	);
} // build_catch

ast::FinallyClause * build_finally( const CodeLocation & location, StatementNode * stmt ) {
	return new ast::FinallyClause( location,
		strict_dynamic_cast<const ast::CompoundStmt *>(
			buildMoveSingle( stmt )
		)
	);
} // build_finally

ast::SuspendStmt * build_suspend( const CodeLocation & location, StatementNode * then, ast::SuspendStmt::Kind kind ) {
	return new ast::SuspendStmt( location,
		strict_dynamic_cast<const ast::CompoundStmt *, nullptr>(
			buildMoveOptional( then )
		),
		kind
	);
} // build_suspend

ast::WaitForStmt * build_waitfor( const CodeLocation & location, ast::WaitForStmt * existing, ExpressionNode * when, ExpressionNode * targetExpr, StatementNode * stmt ) {
	auto clause = new ast::WaitForClause( location );
	clause->target = maybeBuild( targetExpr );
	clause->stmt = maybeMoveBuild( stmt );
	clause->when_cond = notZeroExpr( maybeMoveBuild( when ) );

	ExpressionNode * next = dynamic_cast<ExpressionNode *>( targetExpr->get_next() );
	targetExpr->set_next( nullptr );
	buildMoveList( next, clause->target_args );

	delete targetExpr;

	existing->clauses.insert( existing->clauses.begin(), clause );

	return existing;
} // build_waitfor

ast::WaitForStmt * build_waitfor_else( const CodeLocation & location, ast::WaitForStmt * existing, ExpressionNode * when, StatementNode * stmt ) {
	existing->else_stmt = maybeMoveBuild( stmt );
	existing->else_cond = notZeroExpr( maybeMoveBuild( when ) );

	(void)location;
	return existing;
} // build_waitfor_else

ast::WaitForStmt * build_waitfor_timeout( const CodeLocation & location, ast::WaitForStmt * existing, ExpressionNode * when, ExpressionNode * timeout, StatementNode * stmt ) {
	existing->timeout_time = maybeMoveBuild( timeout );
	existing->timeout_stmt = maybeMoveBuild( stmt );
	existing->timeout_cond = notZeroExpr( maybeMoveBuild( when ) );

	(void)location;
	return existing;
} // build_waitfor_timeout

ast::WaitUntilStmt::ClauseNode * build_waituntil_clause( const CodeLocation & loc, ExpressionNode * when, ExpressionNode * targetExpr, StatementNode * stmt ) {
    ast::WhenClause * clause = new ast::WhenClause( loc );
    clause->when_cond = notZeroExpr( maybeMoveBuild( when ) );
    clause->stmt = maybeMoveBuild( stmt );
    clause->target = maybeMoveBuild( targetExpr );
    return new ast::WaitUntilStmt::ClauseNode( clause );
}
ast::WaitUntilStmt::ClauseNode * build_waituntil_else( const CodeLocation & loc, ExpressionNode * when, StatementNode * stmt ) {
    ast::WhenClause * clause = new ast::WhenClause( loc );
    clause->when_cond = notZeroExpr( maybeMoveBuild( when ) );
    clause->stmt = maybeMoveBuild( stmt );
    return new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::ELSE, clause );
}
ast::WaitUntilStmt::ClauseNode * build_waituntil_timeout( const CodeLocation & loc, ExpressionNode * when, ExpressionNode * timeout, StatementNode * stmt ) {
    ast::WhenClause * clause = new ast::WhenClause( loc );
    clause->when_cond = notZeroExpr( maybeMoveBuild( when ) );
    clause->stmt = maybeMoveBuild( stmt );
    clause->target = maybeMoveBuild( timeout );
    return new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::TIMEOUT, clause );
}

ast::WaitUntilStmt * build_waituntil_stmt( const CodeLocation & loc, ast::WaitUntilStmt::ClauseNode * root ) {
    ast::WaitUntilStmt * retStmt = new ast::WaitUntilStmt( loc );
    retStmt->predicateTree = root;
    
    // iterative tree traversal
    std::vector<ast::WaitUntilStmt::ClauseNode *> nodeStack; // stack needed for iterative traversal
    ast::WaitUntilStmt::ClauseNode * currNode = nullptr;
    ast::WaitUntilStmt::ClauseNode * lastInternalNode = nullptr;
    ast::WaitUntilStmt::ClauseNode * cleanup = nullptr; // used to cleanup removed else/timeout
    nodeStack.push_back(root);

    do {
        currNode = nodeStack.back();
        nodeStack.pop_back(); // remove node since it will be processed

        switch (currNode->op) {
            case ast::WaitUntilStmt::ClauseNode::LEAF:
                retStmt->clauses.push_back(currNode->leaf);
                break;
            case ast::WaitUntilStmt::ClauseNode::ELSE:
                retStmt->else_stmt = currNode->leaf->stmt 
                    ? ast::deepCopy( currNode->leaf->stmt )
                    : nullptr;
                
                retStmt->else_cond = currNode->leaf->when_cond
                    ? ast::deepCopy( currNode->leaf->when_cond )
                    : nullptr;

                delete currNode->leaf;
                break;
            case ast::WaitUntilStmt::ClauseNode::TIMEOUT:
                retStmt->timeout_time = currNode->leaf->target 
                    ? ast::deepCopy( currNode->leaf->target )
                    : nullptr;
                retStmt->timeout_stmt = currNode->leaf->stmt
                    ? ast::deepCopy( currNode->leaf->stmt )
                    : nullptr;
                retStmt->timeout_cond = currNode->leaf->when_cond
                    ? ast::deepCopy( currNode->leaf->when_cond )
                    : nullptr;

                delete currNode->leaf;
                break;
            default:
                nodeStack.push_back( currNode->right ); // process right after left
                nodeStack.push_back( currNode->left );

                // Cut else/timeout out of the tree
                if ( currNode->op == ast::WaitUntilStmt::ClauseNode::LEFT_OR ) {
                    if ( lastInternalNode )
                        lastInternalNode->right = currNode->left;
                    else    // if not set then root is LEFT_OR 
                        retStmt->predicateTree = currNode->left;
    
                    currNode->left = nullptr;
                    cleanup = currNode;
                }
                
                lastInternalNode = currNode;
                break;
        }
    } while ( !nodeStack.empty() );

    if ( cleanup ) delete cleanup;

    return retStmt;
}

ast::Stmt * build_with( const CodeLocation & location, ExpressionNode * exprs, StatementNode * stmt ) {
	std::vector<ast::ptr<ast::Expr>> e;
	buildMoveList( exprs, e );
	ast::Stmt * s = maybeMoveBuild( stmt );
	return new ast::DeclStmt( location, new ast::WithStmt( location, std::move( e ), s ) );
} // build_with

ast::Stmt * build_compound( const CodeLocation & location, StatementNode * first ) {
	auto cs = new ast::CompoundStmt( location );
	buildMoveList( first, cs->kids );
	return cs;
} // build_compound

// A single statement in a control structure is always converted to a compound statement so subsequent generated code
// can be placed within this compound statement. Otherwise, code generation has to constantly check for a single
// statement and wrap it into a compound statement to insert additional code. Hence, all control structures have a
// conical form for code generation.
StatementNode * maybe_build_compound( const CodeLocation & location, StatementNode * first ) {
	// Optimization: if the control-structure statement is a compound statement, do not wrap it.
	// e.g., if (...) {...} do not wrap the existing compound statement.
	if ( !dynamic_cast<ast::CompoundStmt *>( first->stmt.get() ) ) { // unique_ptr
		return new StatementNode( build_compound( location, first ) );
	} // if
	return first;
} // maybe_build_compound

// Question
ast::Stmt * build_asm( const CodeLocation & location, bool is_volatile, ExpressionNode * instruction, ExpressionNode * output, ExpressionNode * input, ExpressionNode * clobber, LabelNode * gotolabels ) {
	std::vector<ast::ptr<ast::Expr>> out, in;
	std::vector<ast::ptr<ast::ConstantExpr>> clob;

	buildMoveList( output, out );
	buildMoveList( input, in );
	buildMoveList( clobber, clob );
	return new ast::AsmStmt( location,
		is_volatile,
		maybeMoveBuild( instruction ),
		std::move( out ),
		std::move( in ),
		std::move( clob ),
		gotolabels ? gotolabels->labels : std::vector<ast::Label>()
	);
} // build_asm

ast::Stmt * build_directive( const CodeLocation & location, string * directive ) {
	auto stmt = new ast::DirectiveStmt( location, *directive );
	delete directive;
	return stmt;
} // build_directive

ast::Stmt * build_mutex( const CodeLocation & location, ExpressionNode * exprs, StatementNode * stmt ) {
	std::vector<ast::ptr<ast::Expr>> expList;
	buildMoveList( exprs, expList );
	ast::Stmt * body = maybeMoveBuild( stmt );
	return new ast::MutexStmt( location, body, std::move( expList ) );
} // build_mutex

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
