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
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  2 20:29:30 2022
// Update Count     : 425
//

#include <cassert>                 // for assert, strict_dynamic_cast, assertf
#include <list>                    // for list
#include <memory>                  // for unique_ptr
#include <string>                  // for string

#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for maybeMoveBuild, maybeBuild
#include "ParseNode.h"             // for StatementNode, ExpressionNode, bui...
#include "SynTree/Expression.h"    // for Expression, ConstantExpr
#include "SynTree/Label.h"         // for Label, noLabels
#include "SynTree/Declaration.h"
#include "SynTree/Statement.h"     // for Statement, BranchStmt, CaseStmt
#include "parserutility.h"         // for notZeroExpr

class Declaration;

using namespace std;


StatementNode::StatementNode( DeclarationNode * decl ) {
	assert( decl );
	DeclarationNode * agg = decl->extractAggregate();
	if ( agg ) {
		StatementNode * nextStmt = new StatementNode( new DeclStmt( maybeBuild( decl ) ) );
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
	stmt.reset( new DeclStmt( maybeMoveBuild( agg ) ) );
} // StatementNode::StatementNode

StatementNode * StatementNode::append_last_case( StatementNode * stmt ) {
	StatementNode * prev = this;
	// find end of list and maintain previous pointer
	for ( StatementNode * curr = prev; curr != nullptr; curr = (StatementNode *)curr->get_next() ) {
		StatementNode * node = strict_dynamic_cast< StatementNode * >(curr);
		assert( dynamic_cast< CaseStmt * >(node->stmt.get()) );
		prev = curr;
	} // for
	// convert from StatementNode list to Statement list
	StatementNode * node = dynamic_cast< StatementNode * >(prev);
	list< Statement * > stmts;
	buildMoveList( stmt, stmts );
	// splice any new Statements to end of current Statements
	CaseStmt * caseStmt = dynamic_cast< CaseStmt * >(node->stmt.get());
	caseStmt->get_statements().splice( caseStmt->get_statements().end(), stmts );
	return this;
} // StatementNode::append_last_case

Statement * build_expr( ExpressionNode * ctl ) {
	Expression * e = maybeMoveBuild( ctl );

	if ( e ) return new ExprStmt( e );
	else return new NullStmt();
} // build_expr

Expression * build_if_control( CondCtl * ctl, list< Statement * > & init ) {
	if ( ctl->init != 0 ) {
		buildMoveList( ctl->init, init );
	} // if

	Expression * cond = nullptr;
	if ( ctl->condition ) {
		// compare the provided condition against 0
		cond = notZeroExpr( maybeMoveBuild( ctl->condition ) );
	} else {
		for ( Statement * stmt : init ) {
			// build the && of all of the declared variables compared against 0
			DeclStmt * declStmt = strict_dynamic_cast< DeclStmt * >( stmt );
			DeclarationWithType * dwt = strict_dynamic_cast< DeclarationWithType * >( declStmt->decl );
			Expression * nze = notZeroExpr( new VariableExpr( dwt ) );
			cond = cond ? new LogicalExpr( cond, nze, true ) : nze;
		}
	}
	delete ctl;
	return cond;
} // build_if_control

Statement * build_if( CondCtl * ctl, StatementNode * then, StatementNode * else_ ) {
	list< Statement * > astinit;						// maybe empty
	Expression * astcond = build_if_control( ctl, astinit ); // ctl deleted, cond/init set

	Statement * astthen, * astelse = nullptr;
	list< Statement * > aststmt;
	buildMoveList< Statement, StatementNode >( then, aststmt );
	assert( aststmt.size() == 1 );
	astthen = aststmt.front();

	if ( else_ ) {
		list< Statement * > aststmt;
		buildMoveList< Statement, StatementNode >( else_, aststmt );
		assert( aststmt.size() == 1 );
		astelse = aststmt.front();
	} // if

	return new IfStmt( astcond, astthen, astelse, astinit );
} // build_if

Statement * build_switch( bool isSwitch, ExpressionNode * ctl, StatementNode * stmt ) {
	list< Statement * > aststmt;
	buildMoveList< Statement, StatementNode >( stmt, aststmt );
	if ( ! isSwitch ) {									// choose statement
		for ( Statement * stmt : aststmt ) {
			CaseStmt * caseStmt = strict_dynamic_cast< CaseStmt * >( stmt );
			if ( ! caseStmt->stmts.empty() ) {			// code after "case" => end of case list
				CompoundStmt * block = strict_dynamic_cast< CompoundStmt * >( caseStmt->stmts.front() );
				block->kids.push_back( new BranchStmt( "", BranchStmt::Break ) );
			} // if
		} // for
	} // if
	// aststmt.size() == 0 for switch (...) {}, i.e., no declaration or statements
	return new SwitchStmt( maybeMoveBuild( ctl ), aststmt );
} // build_switch

Statement * build_case( ExpressionNode * ctl ) {
	return new CaseStmt( maybeMoveBuild( ctl ), {} ); // stmt starts empty and then added to
} // build_case

Statement * build_default() {
	return new CaseStmt( nullptr, {}, true );			// stmt starts empty and then added to
} // build_default

Statement * build_while( CondCtl * ctl, StatementNode * stmt, StatementNode * else_ ) {
	list< Statement * > astinit;						// maybe empty
	Expression * astcond = build_if_control( ctl, astinit ); // ctl deleted, cond/init set

	list< Statement * > aststmt;						// loop body, compound created if empty
	buildMoveList< Statement, StatementNode >( stmt, aststmt );
	assert( aststmt.size() == 1 );

	list< Statement * > astelse;						// else clause, maybe empty
	buildMoveList< Statement, StatementNode >( else_, astelse );

	return new WhileDoStmt( astcond, aststmt.front(), astelse.front(), astinit, false );
} // build_while

Statement * build_do_while( ExpressionNode * ctl, StatementNode * stmt, StatementNode * else_ ) {
	list< Statement * > aststmt;						// loop body, compound created if empty
	buildMoveList< Statement, StatementNode >( stmt, aststmt );
	assert( aststmt.size() == 1 );						// compound created if empty

	list< Statement * > astelse;						// else clause, maybe empty
	buildMoveList< Statement, StatementNode >( else_, astelse );

	// do-while cannot have declarations in the contitional, so init is always empty
	return new WhileDoStmt( notZeroExpr( maybeMoveBuild( ctl ) ), aststmt.front(), astelse.front(), {}, true );
} // build_do_while

Statement * build_for( ForCtrl * forctl, StatementNode * stmt, StatementNode * else_ ) {
	list< Statement * > astinit;						// maybe empty
	buildMoveList( forctl->init, astinit );

	Expression * astcond = nullptr;						// maybe empty
	astcond = notZeroExpr( maybeMoveBuild( forctl->condition ) );

	Expression * astincr = nullptr;						// maybe empty
	astincr = maybeMoveBuild( forctl->change );
	delete forctl;

	list< Statement * > aststmt;						// loop body, compound created if empty
	buildMoveList< Statement, StatementNode >( stmt, aststmt );
	assert( aststmt.size() == 1 );

	list< Statement * > astelse;						// else clause, maybe empty
	buildMoveList< Statement, StatementNode >( else_, astelse );

	return new ForStmt( astinit, astcond, astincr, aststmt.front(), astelse.front() );
} // build_for

Statement * build_branch( BranchStmt::Type kind ) {
	Statement * ret = new BranchStmt( "", kind );
	return ret;
} // build_branch

Statement * build_branch( string * identifier, BranchStmt::Type kind ) {
	Statement * ret = new BranchStmt( * identifier, kind );
	delete identifier;									// allocated by lexer
	return ret;
} // build_branch

Statement * build_computedgoto( ExpressionNode * ctl ) {
	return new BranchStmt( maybeMoveBuild( ctl ), BranchStmt::Goto );
} // build_computedgoto

Statement * build_return( ExpressionNode * ctl ) {
	list< Expression * > exps;
	buildMoveList( ctl, exps );
	return new ReturnStmt( exps.size() > 0 ? exps.back() : nullptr );
} // build_return

Statement * build_throw( ExpressionNode * ctl ) {
	list< Expression * > exps;
	buildMoveList( ctl, exps );
	assertf( exps.size() < 2, "CFA internal error: leaking memory" );
	return new ThrowStmt( ThrowStmt::Terminate, !exps.empty() ? exps.back() : nullptr );
} // build_throw

Statement * build_resume( ExpressionNode * ctl ) {
	list< Expression * > exps;
	buildMoveList( ctl, exps );
	assertf( exps.size() < 2, "CFA internal error: leaking memory" );
	return new ThrowStmt( ThrowStmt::Resume, !exps.empty() ? exps.back() : nullptr );
} // build_resume

Statement * build_resume_at( ExpressionNode * ctl, ExpressionNode * target ) {
	(void)ctl;
	(void)target;
	assertf( false, "resume at (non-local throw) is not yet supported," );
} // build_resume_at

Statement * build_try( StatementNode * try_, StatementNode * catch_, StatementNode * finally_ ) {
	list< CatchStmt * > aststmt;
	buildMoveList< CatchStmt, StatementNode >( catch_, aststmt );
	CompoundStmt * tryBlock = strict_dynamic_cast< CompoundStmt * >( maybeMoveBuild( try_ ) );
	FinallyStmt * finallyBlock = dynamic_cast< FinallyStmt * >( maybeMoveBuild( finally_ ) );
	return new TryStmt( tryBlock, aststmt, finallyBlock );
} // build_try

Statement * build_catch( CatchStmt::Kind kind, DeclarationNode * decl, ExpressionNode * cond, StatementNode * body ) {
	list< Statement * > aststmt;
	buildMoveList< Statement, StatementNode >( body, aststmt );
	assert( aststmt.size() == 1 );
	return new CatchStmt( kind, maybeMoveBuild( decl ), maybeMoveBuild( cond ), aststmt.front() );
} // build_catch

Statement * build_finally( StatementNode * stmt ) {
	list< Statement * > aststmt;
	buildMoveList< Statement, StatementNode >( stmt, aststmt );
	assert( aststmt.size() == 1 );
	return new FinallyStmt( dynamic_cast< CompoundStmt * >( aststmt.front() ) );
} // build_finally

SuspendStmt * build_suspend( StatementNode * then, SuspendStmt::Type type ) {
	auto node = new SuspendStmt();

	node->type = type;

	list< Statement * > stmts;
	buildMoveList< Statement, StatementNode >( then, stmts );
	if(!stmts.empty()) {
		assert( stmts.size() == 1 );
		node->then = dynamic_cast< CompoundStmt * >( stmts.front() );
	}

	return node;
}

WaitForStmt * build_waitfor( ExpressionNode * targetExpr, StatementNode * stmt, ExpressionNode * when ) {
	auto node = new WaitForStmt();

	WaitForStmt::Target target;
	target.function = maybeBuild( targetExpr );

	ExpressionNode * next = dynamic_cast<ExpressionNode *>( targetExpr->get_next() );
	targetExpr->set_next( nullptr );
	buildMoveList< Expression >( next, target.arguments );

	delete targetExpr;

	node->clauses.push_back( WaitForStmt::Clause{
		target,
		maybeMoveBuild( stmt ),
		notZeroExpr( maybeMoveBuild( when ) )
	});

	return node;
} // build_waitfor

WaitForStmt * build_waitfor( ExpressionNode * targetExpr, StatementNode * stmt, ExpressionNode * when, WaitForStmt * node ) {
	WaitForStmt::Target target;
	target.function = maybeBuild( targetExpr );

	ExpressionNode * next = dynamic_cast<ExpressionNode *>( targetExpr->get_next() );
	targetExpr->set_next( nullptr );
	buildMoveList< Expression >( next, target.arguments );

	delete targetExpr;

	node->clauses.insert( node->clauses.begin(), WaitForStmt::Clause{
		std::move( target ),
		maybeMoveBuild( stmt ),
		notZeroExpr( maybeMoveBuild( when ) )
	});

	return node;
} // build_waitfor

WaitForStmt * build_waitfor_timeout( ExpressionNode * timeout, StatementNode * stmt, ExpressionNode * when ) {
	auto node = new WaitForStmt();

	if( timeout ) {
		node->timeout.time      = maybeMoveBuild( timeout );
		node->timeout.statement = maybeMoveBuild( stmt    );
		node->timeout.condition = notZeroExpr( maybeMoveBuild( when ) );
	} else {
		node->orelse.statement  = maybeMoveBuild( stmt );
		node->orelse.condition  = notZeroExpr( maybeMoveBuild( when ) );
	} // if

	return node;
} // build_waitfor_timeout

WaitForStmt * build_waitfor_timeout( ExpressionNode * timeout, StatementNode * stmt, ExpressionNode * when,  StatementNode * else_, ExpressionNode * else_when ) {
	auto node = new WaitForStmt();

	node->timeout.time      = maybeMoveBuild( timeout );
	node->timeout.statement = maybeMoveBuild( stmt    );
	node->timeout.condition = notZeroExpr( maybeMoveBuild( when ) );

	node->orelse.statement  = maybeMoveBuild( else_ );
	node->orelse.condition  = notZeroExpr( maybeMoveBuild( else_when ) );

	return node;
} // build_waitfor_timeout

Statement * build_with( ExpressionNode * exprs, StatementNode * stmt ) {
	list< Expression * > e;
	buildMoveList( exprs, e );
	Statement * s = maybeMoveBuild( stmt );
	return new DeclStmt( new WithStmt( e, s ) );
} // build_with

Statement * build_compound( StatementNode * first ) {
	CompoundStmt * cs = new CompoundStmt();
	buildMoveList( first, cs->get_kids() );
	return cs;
} // build_compound

// A single statement in a control structure is always converted to a compound statement so subsequent generated code
// can be placed within this compound statement. Otherwise, code generation has to constantly check for a single
// statement and wrap it into a compound statement to insert additional code. Hence, all control structures have a
// conical form for code generation.
StatementNode * maybe_build_compound( StatementNode * first ) {
	// Optimization: if the control-structure statement is a compound statement, do not wrap it.
	// e.g., if (...) {...} do not wrap the existing compound statement.
	if ( ! dynamic_cast<CompoundStmt *>( first->stmt.get() ) ) { // unique_ptr
		CompoundStmt * cs = new CompoundStmt();
		buildMoveList( first, cs->get_kids() );
		return new StatementNode( cs );
	} // if
	return first;
} // maybe_build_compound

// Question
Statement * build_asm( bool voltile, Expression * instruction, ExpressionNode * output, ExpressionNode * input, ExpressionNode * clobber, LabelNode * gotolabels ) {
	list< Expression * > out, in;
	list< ConstantExpr * > clob;

	buildMoveList( output, out );
	buildMoveList( input, in );
	buildMoveList( clobber, clob );
	return new AsmStmt( voltile, instruction, out, in, clob, gotolabels ? gotolabels->labels : noLabels );
} // build_asm

Statement * build_directive( string * directive ) {
	return new DirectiveStmt( *directive );
} // build_directive

Statement * build_mutex( ExpressionNode * exprs, StatementNode * stmt ) {
	list< Expression * > expList;
	buildMoveList( exprs, expList );
	Statement * body = maybeMoveBuild( stmt );
	return new MutexStmt( body, expList );
} // build_mutex

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
