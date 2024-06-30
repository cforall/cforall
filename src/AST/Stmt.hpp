//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Stmt.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May  8 13:00:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr  5 10:34:00 2023
// Update Count     : 37
//

#pragma once

#include <list>
#include <utility>										// for move
#include <vector>

#include "Label.hpp"
#include "Node.hpp"										// for node, ptr
#include "ParseNode.hpp"
#include "Visitor.hpp"
#include "Common/CodeLocation.hpp"

// Must be included in *all* AST classes; should be #undef'd at the end of the file
#define MUTATE_FRIEND													\
	template<typename node_t> friend node_t * mutate(const node_t * node); \
	template<typename node_t> friend node_t * shallowCopy(const node_t * node);

namespace ast {
class Expr;

// Base statement node
class Stmt : public ParseNode {
  public:
	std::vector<Label> labels;

	Stmt( const CodeLocation & loc, const std::vector<Label> && labels = {} )
		: ParseNode(loc), labels(std::move(labels)) {}

	Stmt(const Stmt & o) : ParseNode(o), labels(o.labels) {}

	const Stmt * accept( Visitor & v ) const override = 0;
  private:
	Stmt * clone() const override = 0;
	MUTATE_FRIEND
};

// Base statement component node (only serves to group them).
class StmtClause : public ParseNode {
  public:
	// This is for non-statements that still belong with the statements,
	// but are not statements, usually some sort of clause. Often these can
	// (and should) be folded into the approprate parent node, but if they
	// cannot be, they are sub-types of this type, for organization.

	StmtClause( const CodeLocation & loc )
		: ParseNode(loc) {}

  private:
	StmtClause * clone() const override = 0;
	MUTATE_FRIEND
};

// Compound statement: { ... }
class CompoundStmt final : public Stmt {
  public:
	std::list<ptr<Stmt>> kids;

	CompoundStmt(const CodeLocation & loc, const std::list<ptr<Stmt>> && ks = {}, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), kids(std::move(ks)) {}

	CompoundStmt( const CompoundStmt & o );
	CompoundStmt( CompoundStmt && o ) = default;

	void push_back( const Stmt * s ) { kids.emplace_back( s ); }
	void push_front( const Stmt * s ) { kids.emplace_front( s ); }

	const CompoundStmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	CompoundStmt * clone() const override { return new CompoundStmt{ *this }; }
	MUTATE_FRIEND
};

// Empty statment: ;
class NullStmt final : public Stmt {
  public:
	NullStmt( const CodeLocation & loc, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)) {}

	const NullStmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	NullStmt * clone() const override { return new NullStmt{ *this }; }
	MUTATE_FRIEND
};

// Expression wrapped by statement
class ExprStmt final : public Stmt {
  public:
	ptr<Expr> expr;

	ExprStmt( const CodeLocation & loc, const Expr* e, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), expr(e) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	ExprStmt * clone() const override { return new ExprStmt{ *this }; }
	MUTATE_FRIEND
};

// Assembly statement: asm ... ( "..." : ... )
class AsmStmt final : public Stmt {
  public:
	bool isVolatile;
	ptr<Expr> instruction;
	std::vector<ptr<Expr>> output, input;
	std::vector<ptr<ConstantExpr>> clobber;
	std::vector<Label> gotoLabels;

	AsmStmt( const CodeLocation & loc, bool isVolatile, const Expr * instruction,
			 const std::vector<ptr<Expr>> && output, const std::vector<ptr<Expr>> && input,
			 const std::vector<ptr<ConstantExpr>> && clobber, const std::vector<Label> && gotoLabels,
			 const std::vector<Label> && labels = {})
		: Stmt(loc, std::move(labels)), isVolatile(isVolatile), instruction(instruction),
		  output(std::move(output)), input(std::move(input)), clobber(std::move(clobber)),
		  gotoLabels(std::move(gotoLabels)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	AsmStmt * clone() const override { return new AsmStmt{ *this }; }
	MUTATE_FRIEND
};

// C-preprocessor directive: #...
class DirectiveStmt final : public Stmt {
  public:
	std::string directive;

	DirectiveStmt( const CodeLocation & loc, const std::string & directive,
				   std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), directive(directive) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	DirectiveStmt * clone() const override { return new DirectiveStmt{ *this }; }
	MUTATE_FRIEND
};

// If statement: if (...) ... else ...
class IfStmt final : public Stmt {
  public:
	ptr<Expr> cond;
	ptr<Stmt> then;
	ptr<Stmt> else_;
	std::vector<ptr<Stmt>> inits;

	IfStmt( const CodeLocation & loc, const Expr * cond, const Stmt * then,
			const Stmt * else_ = nullptr, const std::vector<ptr<Stmt>> && inits = {},
			const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), cond(cond), then(then), else_(else_),
		  inits(std::move(inits)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	IfStmt * clone() const override { return new IfStmt{ *this }; }
	MUTATE_FRIEND
};

// Switch or choose statement: switch (...) { ... }
class SwitchStmt final : public Stmt {
  public:
	ptr<Expr> cond;
	std::vector<ptr<CaseClause>> cases;

	SwitchStmt( const CodeLocation & loc, const Expr * cond,
				const std::vector<ptr<CaseClause>> && cases,
				const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), cond(cond), cases(std::move(cases)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	SwitchStmt * clone() const override { return new SwitchStmt{ *this }; }
	MUTATE_FRIEND
};

// Case label: case ...: or default:
class CaseClause final : public StmtClause {
  public:
	// Null for the default label.
	ptr<Expr> cond;
	std::vector<ptr<Stmt>> stmts;

	CaseClause( const CodeLocation & loc, const Expr * cond, const std::vector<ptr<Stmt>> && stmts )
		: StmtClause(loc), cond(cond), stmts(std::move(stmts)) {}

	bool isDefault() const { return !cond; }

	const CaseClause * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	CaseClause * clone() const override { return new CaseClause{ *this }; }
	MUTATE_FRIEND
};

// A while loop or a do-while loop:
enum WhileDoKind { While, DoWhile };

// While loop: while (...) ... else ... or do ... while (...) else ...;
class WhileDoStmt final : public Stmt {
  public:
	ptr<Expr> cond;
	ptr<Stmt> body;
	ptr<Stmt> else_;
	std::vector<ptr<Stmt>> inits;
	WhileDoKind isDoWhile;

	WhileDoStmt( const CodeLocation & loc, const Expr * cond, const Stmt * body,
				 const std::vector<ptr<Stmt>> && inits, WhileDoKind isDoWhile = While, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), cond(cond), body(body), else_(nullptr), inits(std::move(inits)), isDoWhile(isDoWhile) {}

	WhileDoStmt( const CodeLocation & loc, const Expr * cond, const Stmt * body, const Stmt * else_,
				 const std::vector<ptr<Stmt>> && inits, WhileDoKind isDoWhile = While, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), cond(cond), body(body), else_(else_), inits(std::move(inits)), isDoWhile(isDoWhile) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	WhileDoStmt * clone() const override { return new WhileDoStmt{ *this }; }
	MUTATE_FRIEND
};

// For loop: for (... ; ... ; ...) ... else ...
class ForStmt final : public Stmt {
  public:
	std::vector<ptr<Stmt>> inits;
	ptr<Expr> cond;
	ptr<Expr> inc;
	ptr<Expr> range_over;
	bool is_inc;
	ptr<Stmt> body;
	ptr<Stmt> else_;

	ForStmt( const CodeLocation & loc, const std::vector<ptr<Stmt>> && inits, const Expr * cond,
			 const Expr * inc, const Stmt * body, const std::vector<Label> && label = {} )
		: Stmt(loc, std::move(label)), inits(std::move(inits)), cond(cond), inc(inc),
			range_over(nullptr), body(body), else_(nullptr) {}

	ForStmt( const CodeLocation & loc, const std::vector<ptr<Stmt>> && inits, const Expr * cond,
			 const Expr * inc, const Stmt * body, const Stmt * else_, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), inits(std::move(inits)), cond(cond), inc(inc),
			range_over(nullptr), body(body), else_(else_) {}

	ForStmt( const CodeLocation & loc, const std::vector<ptr<Stmt>> && inits, const Expr * range_over, bool is_inc,
			 const Stmt * body, const Stmt * else_ )
		: Stmt(loc, std::move(labels)), inits(std::move(inits)), range_over(range_over), is_inc(is_inc), 
		body(body), else_(else_) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	ForStmt * clone() const override { return new ForStmt{ *this }; }
	MUTATE_FRIEND
};

// Branch control flow statement: goto ... or break or continue or fallthru
class BranchStmt final : public Stmt {
  public:
	enum Kind { Goto, Break, Continue, FallThrough, FallThroughDefault };
	static constexpr size_t kindEnd = 1 + (size_t)FallThroughDefault;

	const Label originalTarget;
	Label target;
	ptr<Expr> computedTarget;
	Kind kind;

	BranchStmt( const CodeLocation & loc, Kind kind, Label target, const std::vector<Label> && labels = {} );
	BranchStmt( const CodeLocation & loc, const Expr * computedTarget, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), originalTarget(loc), target(loc), computedTarget(computedTarget), kind(Goto) {}

	const char * kindName() const { return kindNames[kind]; }

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	BranchStmt * clone() const override { return new BranchStmt{ *this }; }
	MUTATE_FRIEND

	static const char * kindNames[kindEnd];
};

// Return statement: return ...
class ReturnStmt final : public Stmt {
  public:
	ptr<Expr> expr;

	ReturnStmt( const CodeLocation & loc, const Expr * expr, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), expr(expr) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	ReturnStmt * clone() const override { return new ReturnStmt{ *this }; }
	MUTATE_FRIEND
};

// Kind of exception
enum ExceptionKind { Terminate, Resume };

// Throw statement: throw ...
class ThrowStmt final : public Stmt {
  public:
	ptr<Expr> expr;
	ptr<Expr> target;
	ExceptionKind kind;

	ThrowStmt( const CodeLocation & loc, ExceptionKind kind, const Expr * expr,
			   const Expr * target, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), expr(expr), target(target), kind(kind) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	ThrowStmt * clone() const override { return new ThrowStmt{ *this }; }
	MUTATE_FRIEND
};

// Try statement: try { ... } ...
class TryStmt final : public Stmt {
  public:
	ptr<CompoundStmt> body;
	std::vector<ptr<CatchClause>> handlers;
	ptr<FinallyClause> finally;

	TryStmt( const CodeLocation & loc, const CompoundStmt * body,
			 const std::vector<ptr<CatchClause>> && handlers, const FinallyClause * finally,
			 const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), body(body), handlers(std::move(handlers)), finally(finally) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	TryStmt * clone() const override { return new TryStmt{ *this }; }
	MUTATE_FRIEND
};

// Catch clause of try statement
class CatchClause final : public StmtClause {
  public:
	ptr<Decl> decl;
	ptr<Expr> cond;
	ptr<Stmt> body;
	ExceptionKind kind;

	CatchClause( const CodeLocation & loc, ExceptionKind kind, const Decl * decl, const Expr * cond,
			const Stmt * body )
		: StmtClause(loc), decl(decl), cond(cond), body(body), kind(kind) {}

	const CatchClause * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	CatchClause * clone() const override { return new CatchClause{ *this }; }
	MUTATE_FRIEND
};

// Finally clause of try statement
class FinallyClause final : public StmtClause {
  public:
	ptr<CompoundStmt> body;

	FinallyClause( const CodeLocation & loc, const CompoundStmt * body )
		: StmtClause(loc), body(body) {}

	const FinallyClause * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	FinallyClause * clone() const override { return new FinallyClause{ *this }; }
	MUTATE_FRIEND
};

// Suspend statement
class SuspendStmt final : public Stmt {
  public:
	ptr<CompoundStmt> then;
	enum Kind { None, Coroutine, Generator } kind = None;

	SuspendStmt( const CodeLocation & loc, const CompoundStmt * then, Kind kind, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), then(then), kind(kind) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	SuspendStmt * clone() const override { return new SuspendStmt{ *this }; }
	MUTATE_FRIEND
};

// Base class of WaitFor/WaitUntil statements
// form: KEYWORD(...) ... timeout(...) ... else ...
class WaitStmt : public Stmt {
  public:
	ptr<Expr> timeout_time;
	ptr<Stmt> timeout_stmt;
	ptr<Expr> timeout_cond;
	ptr<Stmt> else_stmt;
	ptr<Expr> else_cond;

	WaitStmt( const CodeLocation & loc, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)) {}

  private:
	WaitStmt * clone() const override = 0;
	MUTATE_FRIEND
};

// Base class for WaitFor/WaitUntil clauses
// form: when( when_cond ) KEYWORD( target ) stmt
class WhenClause : public StmtClause {
  public:
	ptr<Expr> target;
	ptr<Stmt> stmt;
	ptr<Expr> when_cond;

	WhenClause( const CodeLocation & loc )
		: StmtClause( loc ) {}

	const WhenClause * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	WhenClause * clone() const override { return new WhenClause{ *this }; }
	MUTATE_FRIEND
};

// Waitfor statement: when (...) waitfor (... , ...) ... timeout(...) ... else ...
class WaitForStmt final : public WaitStmt {
  public:
	std::vector<ptr<WaitForClause>> clauses;

	WaitForStmt( const CodeLocation & loc, const std::vector<Label> && labels = {} )
		: WaitStmt(loc, std::move(labels)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	WaitForStmt * clone() const override { return new WaitForStmt{ *this }; }
	MUTATE_FRIEND
};

// Clause in a waitfor statement: waitfor (..., ...) ...
class WaitForClause final : public WhenClause {
  public:
	std::vector<ptr<Expr>> target_args;

	WaitForClause( const CodeLocation & loc )
		: WhenClause( loc ) {}

	const WaitForClause * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	WaitForClause * clone() const override { return new WaitForClause{ *this }; }
	MUTATE_FRIEND
};

// waituntil statement: when (...) waituntil (...) ... timeout(...) ... else ...
class WaitUntilStmt final : public WaitStmt {
  public:
	// Non-ast node used during compilation to store data needed to generate predicates
	//    and set initial status values for clauses
	// Used to create a tree corresponding to the structure of the clauses in a WaitUntil
	struct ClauseNode {
		enum Op { AND, OR, LEFT_OR, LEAF, ELSE, TIMEOUT } op; // operation/type tag
		// LEFT_OR used with TIMEOUT/ELSE to indicate that we ignore right hand side after parsing

		ClauseNode * left;
		ClauseNode * right;
		WhenClause * leaf;  // only set if this node is a leaf (points into vector of clauses)

		bool ambiguousWhen; // used to paint nodes of predicate tree based on when() clauses
		bool whenState;     // used to track if when_cond is toggled on or off for generating init values
		bool childOfAnd;      // true on leaf nodes that are children of AND, false otherwise

		ClauseNode( Op op, ClauseNode * left, ClauseNode * right )
			: op(op), left(left), right(right), leaf(nullptr),
			ambiguousWhen(false), whenState(true), childOfAnd(false) {}
		ClauseNode( Op op, WhenClause * leaf )
			: op(op), left(nullptr), right(nullptr), leaf(leaf),
			ambiguousWhen(false), whenState(true), childOfAnd(false) {}
		ClauseNode( WhenClause * leaf ) : ClauseNode(LEAF, leaf) {}

		~ClauseNode() {
			if ( left ) delete left;
			if ( right ) delete right;
		}
	};

	std::vector<ptr<WhenClause>> clauses;
	ClauseNode * predicateTree;

	WaitUntilStmt( const CodeLocation & loc, const std::vector<Label> && labels = {} )
		: WaitStmt(loc, std::move(labels)) {}

	~WaitUntilStmt() { delete predicateTree; }

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	WaitUntilStmt * clone() const override { return new WaitUntilStmt{ *this }; }
	MUTATE_FRIEND
};

// Any declaration in a (compound) statement.
class DeclStmt final : public Stmt {
  public:
	ptr<Decl> decl;

	DeclStmt( const CodeLocation & loc, const Decl * decl, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), decl(decl) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	DeclStmt * clone() const override { return new DeclStmt{ *this }; }
	MUTATE_FRIEND
};

// Represents an implicit application of a constructor or destructor.
class ImplicitCtorDtorStmt final : public Stmt {
  public:
	ptr<Stmt> callStmt;

	ImplicitCtorDtorStmt( const CodeLocation & loc, const Stmt * callStmt,
						  std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), callStmt(callStmt) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	ImplicitCtorDtorStmt * clone() const override { return new ImplicitCtorDtorStmt{ *this }; }
	MUTATE_FRIEND
};

// Mutex Statement
class MutexStmt final : public Stmt {
  public:
	ptr<Stmt> stmt;
	std::vector<ptr<Expr>> mutexObjs;

	MutexStmt( const CodeLocation & loc, const Stmt * stmt,
			   const std::vector<ptr<Expr>> && mutexes, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), stmt(stmt), mutexObjs(std::move(mutexes)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	MutexStmt * clone() const override { return new MutexStmt{ *this }; }
	MUTATE_FRIEND
};

// Corun Statement
class CorunStmt final : public Stmt {
  public:
	ptr<Stmt> stmt;

	CorunStmt( const CodeLocation & loc, const Stmt * stmt, const std::vector<Label> && labels = {} )
		: Stmt(loc, std::move(labels)), stmt(stmt) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	CorunStmt * clone() const override { return new CorunStmt{ *this }; }
	MUTATE_FRIEND
};

// Corun Statement
class CoforStmt final : public Stmt {
  public:
	std::vector<ptr<Stmt>> inits;
	ptr<Expr> cond;
	ptr<Expr> inc;
	ptr<Stmt> body;

	CoforStmt( const CodeLocation & loc, const std::vector<ptr<Stmt>> && inits, const Expr * cond,
			 const Expr * inc, const Stmt * body, const std::vector<Label> && label = {} )
		: Stmt(loc, std::move(label)), inits(std::move(inits)), cond(cond), inc(inc), body(body) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
  private:
	CoforStmt * clone() const override { return new CoforStmt{ *this }; }
	MUTATE_FRIEND
};

} // namespace ast

#undef MUTATE_FRIEND

// Local Variables: //
// mode: c++ //
// End: //
