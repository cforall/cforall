//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Statement.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jan 20 16:03:00 2020
// Update Count     : 71
//

#include "SynTree/Statement.h"

#include <stddef.h>                // for NULL
#include <cassert>                 // for assert, assertf
#include <iostream>                // for operator<<, basic_ostream, endl
#include <list>                    // for list, list<>::const_iterator, _Lis...
#include <string>                  // for operator<<, string, char_traits

#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for maybeClone, cloneAll, deleteAll
#include "Declaration.h"           // for Declaration
#include "Expression.h"            // for Expression, ConstantExpr
#include "Statement.h"             // for Statement, ForStmt, AsmStmt, Catch...
#include "SynTree/Label.h"         // for Label, operator<<

using std::string;
using std::endl;

Statement::Statement( const std::list<Label> & labels ) : labels( labels ) {}

void Statement::print( std::ostream & os, Indenter indent ) const {
	if ( ! labels.empty() ) {
		os << indent << "... Labels: {";
		for ( const Label & l : labels ) {
			os << l << ",";
		}
		os << "}" << endl;
	}
}

Statement::~Statement() {}

ExprStmt::ExprStmt( Expression * expr ) : Statement(), expr( expr ) {}

ExprStmt::ExprStmt( const ExprStmt & other ) : Statement( other ), expr( maybeClone( other.expr ) ) {}

ExprStmt::~ExprStmt() {
	delete expr;
}

void ExprStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Expression Statement:" << endl << indent+1;
	expr->print( os, indent+1 );
}


AsmStmt::AsmStmt( bool voltile, Expression * instruction, std::list<Expression *> output, std::list<Expression *> input, std::list<ConstantExpr *> clobber, std::list<Label> gotolabels ) : Statement(), voltile( voltile ), instruction( instruction ), output( output ), input( input ), clobber( clobber ), gotolabels( gotolabels ) {}

AsmStmt::AsmStmt( const AsmStmt & other ) : Statement( other ), voltile( other.voltile ), instruction( maybeClone( other.instruction ) ), gotolabels( other.gotolabels ) {
  cloneAll( other.output, output );
  cloneAll( other.input, input );
  cloneAll( other.clobber, clobber );
}

AsmStmt::~AsmStmt() {
	delete instruction;
	deleteAll( output );
	deleteAll( input );
	deleteAll( clobber );
}

void AsmStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Assembler Statement:" << endl;
	os << indent+1 << "instruction: " << endl << indent;
	instruction->print( os, indent+1 );
	if ( ! output.empty() ) {
		os << endl << indent+1 << "output: " << endl;
		printAll( output, os, indent+1 );
	} // if
	if ( ! input.empty() ) {
		os << indent+1 << "input: " << endl;
		printAll( input, os, indent+1 );
	} // if
	if ( ! clobber.empty() ) {
		os << indent+1 << "clobber: " << endl;
		printAll( clobber, os, indent+1 );
	} // if
}


DirectiveStmt::DirectiveStmt( const std::string & directive ) : Statement(), directive( directive ) {}

void DirectiveStmt::print( std::ostream & os, Indenter ) const {
	os << "GCC Directive:" << directive << endl;
}


const char * BranchStmt::brType[] = {
	"Goto", "Break", "Continue", "Fall Through", "Fall Through Default",
};

BranchStmt::BranchStmt( Label target, Type type ) throw ( SemanticErrorException ) :
	Statement(), originalTarget( target ), target( target ), computedTarget( nullptr ), type( type ) {
	//actually this is a syntactic error signaled by the parser
	if ( type == BranchStmt::Goto && target.empty() ) {
		SemanticError( target.get_statement()->location, "goto without target");
	}
}

BranchStmt::BranchStmt( Expression * computedTarget, Type type ) throw ( SemanticErrorException ) :
	Statement(), computedTarget( computedTarget ), type( type ) {
	if ( type != BranchStmt::Goto || computedTarget == nullptr ) {
		SemanticError( computedTarget->location, "Computed target not valid in branch statement");
	}
}

void BranchStmt::print( std::ostream & os, Indenter indent ) const {
	assert(type < 5);
	os << "Branch (" << brType[type] << ")" << endl ;
	if ( target != "" ) os << indent+1 << "with target: " << target << endl;
	if ( originalTarget != "" ) os << indent+1 << "with original target: " << originalTarget << endl;
	if ( computedTarget != nullptr ) os << indent+1 << "with computed target: " << computedTarget << endl;
}

ReturnStmt::ReturnStmt( Expression * expr ) : Statement(), expr( expr ) {}

ReturnStmt::ReturnStmt( const ReturnStmt & other ) : Statement( other ), expr( maybeClone( other.expr ) ) {}

ReturnStmt::~ReturnStmt() {
	delete expr;
}

void ReturnStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Return Statement, returning: ";
	if ( expr != nullptr ) {
		os << endl << indent+1;
		expr->print( os, indent+1 );
	}
	os << endl;
}

IfStmt::IfStmt( Expression * condition, Statement * thenPart, Statement * elsePart, std::list<Statement *> initialization ):
	Statement(), condition( condition ), thenPart( thenPart ), elsePart( elsePart ), initialization( initialization ) {}

IfStmt::IfStmt( const IfStmt & other ) :
	Statement( other ), condition( maybeClone( other.condition ) ), thenPart( maybeClone( other.thenPart ) ), elsePart( maybeClone( other.elsePart ) ) {
	cloneAll( other.initialization, initialization );
}

IfStmt::~IfStmt() {
	deleteAll( initialization );
	delete condition;
	delete thenPart;
	delete elsePart;
}

void IfStmt::print( std::ostream & os, Indenter indent ) const {
	os << "If on condition: " << endl;
	os << indent+1;
	condition->print( os, indent+1 );

	if ( !initialization.empty() ) {
		os << indent << "... with initialization: \n";
		for ( const Statement * stmt : initialization ) {
			os << indent+1;
			stmt->print( os, indent+1 );
		}
		os << endl;
	}

	os << indent << "... then: " << endl;

	os << indent+1;
	thenPart->print( os, indent+1 );

	if ( elsePart != nullptr ) {
		os << indent << "... else: " << endl;
		os << indent+1;
		elsePart->print( os, indent+1 );
	} // if
}

SwitchStmt::SwitchStmt( Expression * condition, const std::list<Statement *> & statements ):
	Statement(), condition( condition ), statements( statements ) {
}

SwitchStmt::SwitchStmt( const SwitchStmt & other ):
	Statement( other ), condition( maybeClone( other.condition ) ) {
	cloneAll( other.statements, statements );
}

SwitchStmt::~SwitchStmt() {
	delete condition;
	// destroy statements
	deleteAll( statements );
}

void SwitchStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Switch on condition: ";
	condition->print( os );
	os << endl;

	for ( const Statement * stmt : statements ) {
		stmt->print( os, indent+1 );
	}
}

CaseStmt::CaseStmt( Expression * condition, const std::list<Statement *> & statements, bool deflt ) throw ( SemanticErrorException ) :
	Statement(), condition( condition ), stmts( statements ), _isDefault( deflt ) {
	if ( isDefault() && condition != nullptr ) SemanticError( condition, "default case with condition: " );
}

CaseStmt::CaseStmt( const CaseStmt & other ) :
	Statement( other ), condition( maybeClone(other.condition ) ), _isDefault( other._isDefault ) {
	cloneAll( other.stmts, stmts );
}

CaseStmt::~CaseStmt() {
	delete condition;
	deleteAll( stmts );
}

CaseStmt * CaseStmt::makeDefault( const std::list<Label> & labels, std::list<Statement *> stmts ) {
	CaseStmt * stmt = new CaseStmt( nullptr, stmts, true );
	stmt->labels = labels;
	return stmt;
}

void CaseStmt::print( std::ostream & os, Indenter indent ) const {
	if ( isDefault() ) os << indent << "Default ";
	else {
		os << indent << "Case ";
		condition->print( os, indent );
	} // if
	os << endl;

	for ( Statement * stmt : stmts ) {
		os << indent+1;
		stmt->print( os, indent+1 );
	}
}

WhileStmt::WhileStmt( Expression * condition, Statement * body, std::list< Statement * > & initialization, bool isDoWhile ):
	Statement(), condition( condition), body( body), initialization( initialization ), isDoWhile( isDoWhile) {
}

WhileStmt::WhileStmt( const WhileStmt & other ):
	Statement( other ), condition( maybeClone( other.condition ) ), body( maybeClone( other.body ) ), isDoWhile( other.isDoWhile ) {
}

WhileStmt::~WhileStmt() {
	delete body;
	delete condition;
}

void WhileStmt::print( std::ostream & os, Indenter indent ) const {
	os << "While on condition: " << endl ;
	condition->print( os, indent+1 );

	os << indent << "... with body: " << endl;

	if ( body != nullptr ) body->print( os, indent+1 );
}

ForStmt::ForStmt( std::list<Statement *> initialization, Expression * condition, Expression * increment, Statement * body ):
	Statement(), initialization( initialization ), condition( condition ), increment( increment ), body( body ) {
}

ForStmt::ForStmt( const ForStmt & other ):
	Statement( other ), condition( maybeClone( other.condition ) ), increment( maybeClone( other.increment ) ), body( maybeClone( other.body ) ) {
		cloneAll( other.initialization, initialization );

}

ForStmt::~ForStmt() {
	deleteAll( initialization );
	delete condition;
	delete increment;
	delete body;
}

void ForStmt::print( std::ostream & os, Indenter indent ) const {
	Statement::print( os, indent ); // print labels

	os << "For Statement" << endl;

	if ( ! initialization.empty() ) {
		os << indent << "... initialization: \n";
		for ( Statement * stmt : initialization ) {
			os << indent+1;
			stmt->print( os, indent+1 );
		}
	}

	if ( condition != nullptr ) {
		os << indent << "... condition: \n" << indent+1;
		condition->print( os, indent+1 );
	}

	if ( increment != nullptr ) {
		os << "\n" << indent << "... increment: \n" << indent+1;
		increment->print( os, indent+1 );
	}

	if ( body != nullptr ) {
		os << "\n" << indent << "... with body: \n" << indent+1;
		body->print( os, indent+1 );
	}
	os << endl;
}

ThrowStmt::ThrowStmt( Kind kind, Expression * expr, Expression * target ) :
		Statement(), kind(kind), expr(expr), target(target)	{
	assertf(Resume == kind || nullptr == target, "Non-local termination throw is not accepted." );
}

ThrowStmt::ThrowStmt( const ThrowStmt & other ) :
	Statement ( other ), kind( other.kind ), expr( maybeClone( other.expr ) ), target( maybeClone( other.target ) ) {
}

ThrowStmt::~ThrowStmt() {
	delete expr;
	delete target;
}

void ThrowStmt::print( std::ostream & os, Indenter indent) const {
	if ( target ) os << "Non-Local ";
	os << "Throw Statement, raising: ";
	expr->print(os, indent+1);
	if ( target ) {
		os << "... at: ";
		target->print(os, indent+1);
	}
}

TryStmt::TryStmt( CompoundStmt * tryBlock, std::list<CatchStmt *> & handlers, FinallyStmt * finallyBlock ) :
	Statement(), block( tryBlock ),  handlers( handlers ), finallyBlock( finallyBlock ) {
}

TryStmt::TryStmt( const TryStmt & other ) : Statement( other ), block( maybeClone( other.block ) ), finallyBlock( maybeClone( other.finallyBlock ) ) {
	cloneAll( other.handlers, handlers );
}

TryStmt::~TryStmt() {
	delete block;
	deleteAll( handlers );
	delete finallyBlock;
}

void TryStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Try Statement" << endl;
	os << indent << "... with block:" << endl << indent+1;
	block->print( os, indent+1 );

	// handlers
	os << indent << "... and handlers:" << endl;
	for ( const CatchStmt * stmt : handlers ) {
		os << indent+1;
		stmt->print( os, indent+1 );
	}

	// finally block
	if ( finallyBlock != nullptr ) {
		os << indent << "... and finally:" << endl << indent+1;
		finallyBlock->print( os, indent+1 );
	} // if
}

CatchStmt::CatchStmt( Kind kind, Declaration * decl, Expression * cond, Statement * body ) :
	Statement(), kind ( kind ), decl ( decl ), cond ( cond ), body( body ) {
		assertf( decl, "Catch clause must have a declaration." );
}

CatchStmt::CatchStmt( const CatchStmt & other ) :
	Statement( other ), kind ( other.kind ), decl ( maybeClone( other.decl ) ), cond ( maybeClone( other.cond ) ), body( maybeClone( other.body ) ) {
}

CatchStmt::~CatchStmt() {
	delete decl;
	delete body;
}

void CatchStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Catch " << ((Terminate == kind) ? "Terminate" : "Resume") << " Statement" << endl;

	os << indent << "... catching: ";
	decl->printShort( os, indent+1 );
	os << endl;

	if ( cond ) {
		os << indent << "... with conditional:" << endl << indent+1;
		cond->print( os, indent+1 );
	}

	os << indent << "... with block:" << endl;
	os << indent+1;
	body->print( os, indent+1 );
}


FinallyStmt::FinallyStmt( CompoundStmt * block ) : Statement(), block( block ) {
}

FinallyStmt::FinallyStmt( const FinallyStmt & other ) : Statement( other ), block( maybeClone( other.block ) ) {
}

FinallyStmt::~FinallyStmt() {
	delete block;
}

void FinallyStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Finally Statement" << endl;
	os << indent << "... with block:" << endl << indent+1;
	block->print( os, indent+1 );
}

SuspendStmt::SuspendStmt( const SuspendStmt & other )
	: Statement( other )
	, then( maybeClone(other.then) )
{}

SuspendStmt::~SuspendStmt() {
	delete then;
}

void SuspendStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Suspend Statement";
	switch (type) {
		case None     : os << " with implicit target"; break;
		case Generator: os << " for generator"       ; break;
		case Coroutine: os << " for coroutine"       ; break;
	}
	os << endl;
	indent += 1;

	if(then) {
		os << indent << " with post statement :" << endl;
		then->print( os, indent + 1);
	}
}

WaitForStmt::WaitForStmt() : Statement() {
	timeout.time      = nullptr;
	timeout.statement = nullptr;
	timeout.condition = nullptr;
 	orelse .statement = nullptr;
	orelse .condition = nullptr;
}

WaitForStmt::WaitForStmt( const WaitForStmt & other ) : Statement( other ) {
	clauses.reserve( other.clauses.size() );
	for( auto & ocl : other.clauses ) {
		clauses.emplace_back();
		clauses.back().target.function = ocl.target.function->clone();
		cloneAll( ocl.target.arguments, clauses.back().target.arguments );
		clauses.back().statement = ocl.statement->clone();
		clauses.back().condition = ocl.condition->clone();
	}

	timeout.time      = other.timeout.time     ->clone();
	timeout.statement = other.timeout.statement->clone();
	timeout.condition = other.timeout.condition->clone();
	orelse .statement = other.orelse .statement->clone();
	orelse .condition = other.orelse .condition->clone();
}

WaitForStmt::~WaitForStmt() {
	for( auto & clause : clauses ) {
		delete clause.target.function;
		deleteAll( clause.target.arguments );
		delete clause.statement;
		delete clause.condition;
	}

	delete timeout.time;
	delete timeout.statement;
	delete timeout.condition;

	delete orelse.statement;
	delete orelse.condition;
}

void WaitForStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Waitfor Statement" << endl;
	indent += 1;
	for( auto & clause : clauses ) {
		os << indent << "target function :";
		if(clause.target.function) { clause.target.function->print(os, indent + 1); }
		os << endl << indent << "with arguments :" << endl;
		for( auto & thing : clause.target.arguments) {
			if(thing) { thing->print(os, indent + 1); }
		}
		os << indent << " with statment :" << endl;
		if(clause.statement) { clause.statement->print(os, indent + 1); }

		os << indent << " with condition :" << endl;
		if(clause.condition) { clause.condition->print(os, indent + 1); }
	}

	os << indent << " timeout of :" << endl;
	if(timeout.time) { timeout.time->print(os, indent + 1); }

	os << indent << " with statment :" << endl;
	if(timeout.statement) { timeout.statement->print(os, indent + 1); }

	os << indent << " with condition :" << endl;
	if(timeout.condition) { timeout.condition->print(os, indent + 1); }


	os << indent << " else :" << endl;
	if(orelse.statement) { orelse.statement->print(os, indent + 1); }

	os << indent << " with condition :" << endl;
	if(orelse.condition) { orelse.condition->print(os, indent + 1); }
}


WithStmt::WithStmt( const std::list< Expression * > & exprs, Statement * stmt ) : Declaration("", noStorageClasses, LinkageSpec::Cforall), exprs( exprs ), stmt( stmt ) {}
WithStmt::WithStmt( const WithStmt & other ) : Declaration( other ), stmt( maybeClone( other.stmt ) ) {
	cloneAll( other.exprs, exprs );
}
WithStmt::~WithStmt() {
	deleteAll( exprs );
	delete stmt;
}

void WithStmt::print( std::ostream & os, Indenter indent ) const {
	os << "With statement" << endl;
	os << indent << "... with expressions: " << endl;
	printAll( exprs, os, indent+1 );
	os << indent << "... with statement:" << endl << indent+1;
	stmt->print( os, indent+1 );
}


NullStmt::NullStmt( const std::list<Label> & labels ) : Statement( labels ) {
}

void NullStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Null Statement" << endl;
	Statement::print( os, indent );
}

ImplicitCtorDtorStmt::ImplicitCtorDtorStmt( Statement * callStmt ) : Statement(), callStmt( callStmt ) {
	assert( callStmt );
}

ImplicitCtorDtorStmt::ImplicitCtorDtorStmt( const ImplicitCtorDtorStmt & other ) : Statement( other ), callStmt( maybeClone( other.callStmt ) ) {
}

ImplicitCtorDtorStmt::~ImplicitCtorDtorStmt() {
	delete callStmt;
}

void ImplicitCtorDtorStmt::print( std::ostream & os, Indenter indent ) const {
	os << "Implicit Ctor Dtor Statement" << endl;
	os << indent << "... with Ctor/Dtor: ";
	callStmt->print( os, indent+1);
	os << endl;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
