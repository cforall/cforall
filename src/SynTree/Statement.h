//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Statement.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jan 10 14:13:24 2020
// Update Count     : 85
//

#pragma once

#include <iosfwd>                  // for ostream
#include <list>                    // for list
#include <memory>                  // for allocator
#include <vector>				   // for vector

#include "BaseSyntaxNode.h"        // for BaseSyntaxNode
#include "Common/SemanticError.h"  // for SemanticError
#include "Label.h"                 // for Label
#include "Mutator.h"               // for Mutator
#include "Visitor.h"               // for Visitor

class CatchStmt;
class ConstantExpr;
class Declaration;
class Expression;
class FinallyStmt;

class Statement : public BaseSyntaxNode {
  public:
	std::list<Label> labels;

	Statement( const std::list<Label> & labels = {} );
	virtual ~Statement();

	std::list<Label> & get_labels() { return labels; }
	const std::list<Label> & get_labels() const { return labels; }

	virtual Statement * clone() const override = 0;
	virtual void accept( Visitor & v ) override = 0;
	virtual void accept( Visitor & v ) const override = 0;
	virtual Statement * acceptMutator( Mutator & m ) override = 0;
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class CompoundStmt : public Statement {
  public:
	std::list<Statement*> kids;

	CompoundStmt();
	CompoundStmt( std::list<Statement *> stmts );
	CompoundStmt( const CompoundStmt & other );
	virtual ~CompoundStmt();

	std::list<Statement*>& get_kids() { return kids; }
	void push_back( Statement * stmt ) { kids.push_back( stmt ); }
	void push_front( Statement * stmt ) { kids.push_front( stmt ); }

	virtual CompoundStmt * clone() const override { return new CompoundStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual CompoundStmt * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class NullStmt : public Statement {
  public:
	NullStmt( const std::list<Label> & labels = {} );

	virtual NullStmt * clone() const override { return new NullStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual NullStmt * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class ExprStmt : public Statement {
  public:
	Expression * expr;

	ExprStmt( Expression * expr );
	ExprStmt( const ExprStmt & other );
	virtual ~ExprStmt();

	Expression * get_expr() { return expr; }
	void set_expr( Expression * newValue ) { expr = newValue; }

	virtual ExprStmt * clone() const override { return new ExprStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class AsmStmt : public Statement {
  public:
	bool voltile;
	Expression * instruction;
	std::list<Expression *> output, input;
	std::list<ConstantExpr *> clobber;
	std::list<Label> gotolabels;

	AsmStmt( bool voltile, Expression * instruction, std::list<Expression *> output, std::list<Expression *> input, std::list<ConstantExpr *> clobber, std::list<Label> gotolabels );
	AsmStmt( const AsmStmt & other );
	virtual ~AsmStmt();

	bool get_voltile() { return voltile; }
	void set_voltile( bool newValue ) { voltile = newValue; }
	Expression * get_instruction() { return instruction; }
	void set_instruction( Expression * newValue ) { instruction = newValue; }
	std::list<Expression *> & get_output() { return output; }
	void set_output( const std::list<Expression *> & newValue ) { output = newValue; }
	std::list<Expression *> & get_input() { return input; }
	void set_input( const std::list<Expression *> & newValue ) { input = newValue; }
	std::list<ConstantExpr *> & get_clobber() { return clobber; }
	void set_clobber( const std::list<ConstantExpr *> & newValue ) { clobber = newValue; }
	std::list<Label> & get_gotolabels() { return gotolabels; }
	void set_gotolabels( const std::list<Label> & newValue ) { gotolabels = newValue; }

	virtual AsmStmt * clone() const override { return new AsmStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class DirectiveStmt : public Statement {
	public:
	std::string directive;

	DirectiveStmt( const std::string & );
	virtual ~DirectiveStmt(){}

	virtual DirectiveStmt * clone() const override { return new DirectiveStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class IfStmt : public Statement {
  public:
	Expression * condition;
	Statement * thenPart;
	Statement * elsePart;
	std::list<Statement *> initialization;

	IfStmt( Expression * condition, Statement * thenPart, Statement * elsePart,
			std::list<Statement *> initialization = std::list<Statement *>() );
	IfStmt( const IfStmt & other );
	virtual ~IfStmt();

	std::list<Statement *> & get_initialization() { return initialization; }
	Expression * get_condition() { return condition; }
	void set_condition( Expression * newValue ) { condition = newValue; }
	Statement * get_thenPart() { return thenPart; }
	void set_thenPart( Statement * newValue ) { thenPart = newValue; }
	Statement * get_elsePart() { return elsePart; }
	void set_elsePart( Statement * newValue ) { elsePart = newValue; }

	virtual IfStmt * clone() const override { return new IfStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class SwitchStmt : public Statement {
  public:
	Expression * condition;
	std::list<Statement *> statements;

	SwitchStmt( Expression * condition, const std::list<Statement *> & statements );
	SwitchStmt( const SwitchStmt & other );
	virtual ~SwitchStmt();

	Expression * get_condition() { return condition; }
	void set_condition( Expression * newValue ) { condition = newValue; }

	std::list<Statement *> & get_statements() { return statements; }

	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }

	virtual SwitchStmt * clone() const override { return new SwitchStmt( *this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;

};

class CaseStmt : public Statement {
  public:
	Expression * condition;
	std::list<Statement *> stmts;

	CaseStmt( Expression * conditions, const std::list<Statement *> & stmts, bool isdef = false ) throw (SemanticErrorException);
	CaseStmt( const CaseStmt & other );
	virtual ~CaseStmt();

	static CaseStmt * makeDefault( const std::list<Label> & labels = {}, std::list<Statement *> stmts = std::list<Statement *>() );

	bool isDefault() const { return _isDefault; }
	void set_default(bool b) { _isDefault = b; }

	Expression * & get_condition() { return condition; }
	void set_condition( Expression * newValue ) { condition = newValue; }

	std::list<Statement *> & get_statements() { return stmts; }
	void set_statements( std::list<Statement *> & newValue ) { stmts = newValue; }

	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }

	virtual CaseStmt * clone() const override { return new CaseStmt( *this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
  private:
	bool _isDefault;
};

class WhileStmt : public Statement {
  public:
	Expression * condition;
	Statement * body;
	std::list<Statement *> initialization;
	bool isDoWhile;

	WhileStmt( Expression * condition, Statement * body, std::list<Statement *> & initialization, bool isDoWhile = false );
	WhileStmt( const WhileStmt & other );
	virtual ~WhileStmt();

	Expression * get_condition() { return condition; }
	void set_condition( Expression * newValue ) { condition = newValue; }
	Statement * get_body() { return body; }
	void set_body( Statement * newValue ) { body = newValue; }
	bool get_isDoWhile() { return isDoWhile; }
	void set_isDoWhile( bool newValue ) { isDoWhile = newValue; }

	virtual WhileStmt * clone() const override { return new WhileStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class ForStmt : public Statement {
  public:
	std::list<Statement *> initialization;
	Expression * condition;
	Expression * increment;
	Statement * body;

	ForStmt( std::list<Statement *> initialization, Expression * condition = nullptr, Expression * increment = nullptr, Statement * body = nullptr );
	ForStmt( const ForStmt & other );
	virtual ~ForStmt();

	std::list<Statement *> & get_initialization() { return initialization; }
	Expression * get_condition() { return condition; }
	void set_condition( Expression * newValue ) { condition = newValue; }
	Expression * get_increment() { return increment; }
	void set_increment( Expression * newValue ) { increment = newValue; }
	Statement * get_body() { return body; }
	void set_body( Statement * newValue ) { body = newValue; }

	virtual ForStmt * clone() const override { return new ForStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class BranchStmt : public Statement {
  public:
	enum Type { Goto = 0, Break, Continue, FallThrough, FallThroughDefault };

	// originalTarget kept for error messages.
	const Label originalTarget;
	Label target;
	Expression * computedTarget;
	Type type;

	BranchStmt( Label target, Type ) throw (SemanticErrorException);
	BranchStmt( Expression * computedTarget, Type ) throw (SemanticErrorException);

	Label get_originalTarget() { return originalTarget; }
	Label get_target() { return target; }
	void set_target( Label newValue ) { target = newValue; }

	Expression * get_computedTarget() { return computedTarget; }
	void set_target( Expression * newValue ) { computedTarget = newValue; }

	Type get_type() { return type; }
	const char * get_typename() { return brType[ type ]; }

	virtual BranchStmt * clone() const override { return new BranchStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
  private:
	static const char * brType[];
};

class ReturnStmt : public Statement {
  public:
	Expression * expr;

	ReturnStmt( Expression * expr );
	ReturnStmt( const ReturnStmt & other );
	virtual ~ReturnStmt();

	Expression * get_expr() { return expr; }
	void set_expr( Expression * newValue ) { expr = newValue; }

	virtual ReturnStmt * clone() const override { return new ReturnStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class ThrowStmt : public Statement {
  public:
	enum Kind { Terminate, Resume };

	const Kind kind;
	Expression * expr;
	Expression * target;

	ThrowStmt( Kind kind, Expression * expr, Expression * target = nullptr );
	ThrowStmt( const ThrowStmt & other );
	virtual ~ThrowStmt();

	Kind get_kind() { return kind; }
	Expression * get_expr() { return expr; }
	void set_expr( Expression * newExpr ) { expr = newExpr; }
	Expression * get_target() { return target; }
	void set_target( Expression * newTarget ) { target = newTarget; }

	virtual ThrowStmt * clone() const override { return new ThrowStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class TryStmt : public Statement {
  public:
	CompoundStmt * block;
	std::list<CatchStmt *> handlers;
	FinallyStmt * finallyBlock;

	TryStmt( CompoundStmt * tryBlock, std::list<CatchStmt *> & handlers, FinallyStmt * finallyBlock = nullptr );
	TryStmt( const TryStmt & other );
	virtual ~TryStmt();

	CompoundStmt * get_block() const { return block; }
	void set_block( CompoundStmt * newValue ) { block = newValue; }
	std::list<CatchStmt *>& get_catchers() { return handlers; }

	FinallyStmt * get_finally() const { return finallyBlock; }
	void set_finally( FinallyStmt * newValue ) { finallyBlock = newValue; }

	virtual TryStmt * clone() const override { return new TryStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class CatchStmt : public Statement {
  public:
	enum Kind { Terminate, Resume };

	const Kind kind;
	Declaration * decl;
	Expression * cond;
	Statement * body;

	CatchStmt( Kind kind, Declaration * decl,
	           Expression * cond, Statement * body );
	CatchStmt( const CatchStmt & other );
	virtual ~CatchStmt();

	Kind get_kind() { return kind; }
	Declaration * get_decl() { return decl; }
	void set_decl( Declaration * newValue ) { decl = newValue; }
	Expression * get_cond() { return cond; }
	void set_cond( Expression * newCond ) { cond = newCond; }
	Statement * get_body() { return body; }
	void set_body( Statement * newValue ) { body = newValue; }

	virtual CatchStmt * clone() const override { return new CatchStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class FinallyStmt : public Statement {
  public:
	CompoundStmt * block;

	FinallyStmt( CompoundStmt * block );
	FinallyStmt( const FinallyStmt & other );
	virtual ~FinallyStmt();

	CompoundStmt * get_block() const { return block; }
	void set_block( CompoundStmt * newValue ) { block = newValue; }

	virtual FinallyStmt * clone() const override { return new FinallyStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class SuspendStmt : public Statement {
  public:
	CompoundStmt * then = nullptr;
	enum Type { None, Coroutine, Generator } type = None;

	SuspendStmt() = default;
	SuspendStmt( const SuspendStmt & );
	virtual ~SuspendStmt();

	virtual SuspendStmt * clone() const override { return new SuspendStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class WaitForStmt : public Statement {
  public:

	struct Target {
		Expression * function;
		std::list<Expression * > arguments;
	};

	struct Clause {
		Target       target;
		Statement  * statement;
		Expression * condition;
	};

	WaitForStmt();
	WaitForStmt( const WaitForStmt & );
	virtual ~WaitForStmt();

	std::vector<Clause> clauses;

	struct {
		Expression * time;
		Statement  * statement;
		Expression * condition;
	} timeout;

	struct {
		Statement  * statement;
		Expression * condition;
	} orelse;

	virtual WaitForStmt * clone() const override { return new WaitForStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;

};

// class WithStmt : public Statement {
// public:
// 	std::list< Expression * > exprs;
// 	Statement * stmt;

// 	WithStmt( const std::list< Expression * > & exprs, Statement * stmt );
// 	WithStmt( const WithStmt & other );
// 	virtual ~WithStmt();

// 	virtual WithStmt * clone() const override { return new WithStmt( *this ); }
// 	virtual void accept( Visitor & v ) override { v.visit( this ); }
// 	virtual void accept( Visitor & v ) const override { v.visit( this ); }
// 	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
// 	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
// };


// represents a declaration that occurs as part of a compound statement
class DeclStmt : public Statement {
  public:
	Declaration * decl;

	DeclStmt( Declaration * decl );
	DeclStmt( const DeclStmt & other );
	virtual ~DeclStmt();

	Declaration * get_decl() const { return decl; }
	void set_decl( Declaration * newValue ) { decl = newValue; }

	virtual DeclStmt * clone() const override { return new DeclStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};


/// represents an implicit application of a constructor or destructor. Qualifiers are replaced immediately before and
/// after the call so that qualified objects can be constructed with the same functions as unqualified objects.
class ImplicitCtorDtorStmt : public Statement {
  public:
	// the constructor/destructor call statement; owned here for a while, eventually transferred elsewhere
	Statement * callStmt;

	ImplicitCtorDtorStmt( Statement * callStmt );
	ImplicitCtorDtorStmt( const ImplicitCtorDtorStmt & other );
	virtual ~ImplicitCtorDtorStmt();

	Statement * get_callStmt() const { return callStmt; }
	void set_callStmt( Statement * newValue ) { callStmt = newValue; }

	virtual ImplicitCtorDtorStmt * clone() const override { return new ImplicitCtorDtorStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Statement * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
