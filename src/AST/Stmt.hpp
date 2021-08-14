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
// Last Modified On : Fri May 17 12:45:00 2019
// Update Count     : 5
//

#pragma once

#include <list>
#include <utility>                // for move
#include <vector>

#include "Label.hpp"
#include "Node.hpp"               // for node, ptr
#include "ParseNode.hpp"
#include "Visitor.hpp"
#include "Common/CodeLocation.h"

// Must be included in *all* AST classes; should be #undef'd at the end of the file
#define MUTATE_FRIEND \
    template<typename node_t> friend node_t * mutate(const node_t * node); \
	template<typename node_t> friend node_t * shallowCopy(const node_t * node);

namespace ast {

class Expr;

/// Base statement node
class Stmt : public ParseNode {
public:
	std::vector<Label> labels;

	Stmt( const CodeLocation & loc, std::vector<Label> && labels = {} )
	: ParseNode(loc), labels(std::move(labels)) {}

	Stmt(const Stmt& o) : ParseNode(o), labels(o.labels) {}

	const Stmt * accept( Visitor & v ) const override = 0;
private:
	Stmt * clone() const override = 0;
	MUTATE_FRIEND
};

/// Compound statement `{ ... }`
class CompoundStmt final : public Stmt {
public:
	std::list<ptr<Stmt>> kids;

	CompoundStmt(const CodeLocation & loc, std::list<ptr<Stmt>> && ks = {},
		std::vector<Label>&& labels = {} )
	: Stmt(loc, std::move(labels)), kids(std::move(ks)) {}

	CompoundStmt( const CompoundStmt& o );
	CompoundStmt( CompoundStmt&& o ) = default;

	void push_back( const Stmt * s ) { kids.emplace_back( s ); }
	void push_front( const Stmt * s ) { kids.emplace_front( s ); }

	const CompoundStmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	CompoundStmt * clone() const override { return new CompoundStmt{ *this }; }
	MUTATE_FRIEND
};

/// Empty statment `;`
class NullStmt final : public Stmt {
public:
	NullStmt( const CodeLocation & loc, std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)) {}

	const NullStmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	NullStmt * clone() const override { return new NullStmt{ *this }; }
	MUTATE_FRIEND
};

/// Expression wrapped by statement
class ExprStmt final : public Stmt {
public:
	ptr<Expr> expr;

	ExprStmt( const CodeLocation& loc, const Expr* e, std::vector<Label>&& labels = {} )
	: Stmt(loc, std::move(labels)), expr(e) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ExprStmt * clone() const override { return new ExprStmt{ *this }; }
	MUTATE_FRIEND
};

/// Assembly statement `asm ... ( "..." : ... )`
class AsmStmt final : public Stmt {
public:
	bool isVolatile;
	ptr<Expr> instruction;
	std::vector<ptr<Expr>> output, input;
	std::vector<ptr<ConstantExpr>> clobber;
	std::vector<Label> gotoLabels;

	AsmStmt( const CodeLocation & loc, bool isVolatile, const Expr * instruction,
		std::vector<ptr<Expr>> && output, std::vector<ptr<Expr>> && input,
		std::vector<ptr<ConstantExpr>> && clobber, std::vector<Label> && gotoLabels,
		std::vector<Label> && labels = {})
	: Stmt(loc, std::move(labels)), isVolatile(isVolatile), instruction(instruction),
	  output(std::move(output)), input(std::move(input)), clobber(std::move(clobber)),
	  gotoLabels(std::move(gotoLabels)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	AsmStmt * clone() const override { return new AsmStmt{ *this }; }
	MUTATE_FRIEND
};

/// C-preprocessor directive `#...`
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

/// If conditional statement `if (...) ... else ...`
class IfStmt final : public Stmt {
public:
	ptr<Expr> cond;
	ptr<Stmt> thenPart;
	ptr<Stmt> elsePart;
	std::vector<ptr<Stmt>> inits;

	IfStmt( const CodeLocation & loc, const Expr * cond, const Stmt * thenPart,
		const Stmt * elsePart = nullptr, std::vector<ptr<Stmt>> && inits = {},
		std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), cond(cond), thenPart(thenPart), elsePart(elsePart),
	  inits(std::move(inits)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	IfStmt * clone() const override { return new IfStmt{ *this }; }
	MUTATE_FRIEND
};

/// Switch or choose conditional statement `switch (...) { ... }`
class SwitchStmt final : public Stmt {
public:
	ptr<Expr> cond;
	std::vector<ptr<Stmt>> stmts;

	SwitchStmt( const CodeLocation & loc, const Expr * cond, std::vector<ptr<Stmt>> && stmts,
		std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), cond(cond), stmts(std::move(stmts)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	SwitchStmt * clone() const override { return new SwitchStmt{ *this }; }
	MUTATE_FRIEND
};

/// Case label `case ...:` `default:`
class CaseStmt final : public Stmt {
public:
	ptr<Expr> cond;
	std::vector<ptr<Stmt>> stmts;

	CaseStmt( const CodeLocation & loc, const Expr * cond, std::vector<ptr<Stmt>> && stmts,
		std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), cond(cond), stmts(std::move(stmts)) {}

	bool isDefault() const { return !cond; }

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	CaseStmt * clone() const override { return new CaseStmt{ *this }; }
	MUTATE_FRIEND
};

/// While loop `while (...) ...` `do ... while (...);
class WhileStmt final : public Stmt {
public:
	ptr<Expr> cond;
	ptr<Stmt> body;
	std::vector<ptr<Stmt>> inits;
	bool isDoWhile;

	WhileStmt( const CodeLocation & loc, const Expr * cond, const Stmt * body,
		std::vector<ptr<Stmt>> && inits, bool isDoWhile = false, std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), cond(cond), body(body), inits(std::move(inits)),
	  isDoWhile(isDoWhile) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	WhileStmt * clone() const override { return new WhileStmt{ *this }; }
	MUTATE_FRIEND
};

/// For loop `for (... ; ... ; ...) ...`
class ForStmt final : public Stmt {
public:
	std::vector<ptr<Stmt>> inits;
	ptr<Expr> cond;
	ptr<Expr> inc;
	ptr<Stmt> body;

	ForStmt( const CodeLocation & loc, std::vector<ptr<Stmt>> && inits, const Expr * cond,
		const Expr * inc, const Stmt * body, std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), inits(std::move(inits)), cond(cond), inc(inc),
	  body(body) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ForStmt * clone() const override { return new ForStmt{ *this }; }
	MUTATE_FRIEND
};

/// Branch control flow statement `goto ...` `break` `continue` `fallthru`
class BranchStmt final : public Stmt {
public:
	enum Kind { Goto, Break, Continue, FallThrough, FallThroughDefault };
	static constexpr size_t kindEnd = 1 + (size_t)FallThroughDefault;

	const Label originalTarget;
	Label target;
	ptr<Expr> computedTarget;
	Kind kind;

	BranchStmt( const CodeLocation & loc, Kind kind, Label target,
		std::vector<Label> && labels = {} );
	BranchStmt( const CodeLocation & loc, const Expr * computedTarget,
		std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), originalTarget(loc), target(loc),
	  computedTarget(computedTarget), kind(Goto) {}

	const char * kindName() const { return kindNames[kind]; }

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	BranchStmt * clone() const override { return new BranchStmt{ *this }; }
	MUTATE_FRIEND

	static const char * kindNames[kindEnd];
};

/// Return statement `return ...`
class ReturnStmt final : public Stmt {
public:
	ptr<Expr> expr;

	ReturnStmt( const CodeLocation & loc, const Expr * expr, std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), expr(expr) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ReturnStmt * clone() const override { return new ReturnStmt{ *this }; }
	MUTATE_FRIEND
};

/// Kind of exception
enum ExceptionKind { Terminate, Resume };

/// Throw statement `throw ...`
class ThrowStmt final : public Stmt {
public:
	ptr<Expr> expr;
	ptr<Expr> target;
	ExceptionKind kind;

	ThrowStmt(
		const CodeLocation & loc, ExceptionKind kind, const Expr * expr, const Expr * target,
		std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), expr(expr), target(target), kind(kind) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ThrowStmt * clone() const override { return new ThrowStmt{ *this }; }
	MUTATE_FRIEND
};

/// Try statement `try { ... } ...`
class TryStmt final : public Stmt {
public:
	ptr<CompoundStmt> body;
	std::vector<ptr<CatchStmt>> handlers;
	ptr<FinallyStmt> finally;

	TryStmt(
		const CodeLocation & loc, const CompoundStmt * body,
		std::vector<ptr<CatchStmt>> && handlers, const FinallyStmt * finally,
		std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), body(body), handlers(std::move(handlers)), finally(finally) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	TryStmt * clone() const override { return new TryStmt{ *this }; }
	MUTATE_FRIEND
};

/// Catch clause of try statement
class CatchStmt final : public Stmt {
public:
	ptr<Decl> decl;
	ptr<Expr> cond;
	ptr<Stmt> body;
	ExceptionKind kind;

	CatchStmt(
		const CodeLocation & loc, ExceptionKind kind, const Decl * decl, const Expr * cond,
		const Stmt * body, std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), decl(decl), cond(cond), body(body), kind(kind) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	CatchStmt * clone() const override { return new CatchStmt{ *this }; }
	MUTATE_FRIEND
};

/// Finally clause of try statement
class FinallyStmt final : public Stmt {
public:
	ptr<CompoundStmt> body;

	FinallyStmt( const CodeLocation & loc, const CompoundStmt * body,
		std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), body(body) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	FinallyStmt * clone() const override { return new FinallyStmt{ *this }; }
	MUTATE_FRIEND
};

/// Suspend statement
class SuspendStmt final : public Stmt {
public:
	ptr<CompoundStmt> then;
	enum Type { None, Coroutine, Generator } type = None;

	SuspendStmt( const CodeLocation & loc, const CompoundStmt * then, Type type, std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), then(then), type(type) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	SuspendStmt * clone() const override { return new SuspendStmt{ *this }; }
	MUTATE_FRIEND
};

/// Wait for concurrency statement `when (...) waitfor (... , ...) ... timeout(...) ... else ...`
class WaitForStmt final : public Stmt {
public:
	struct Target {
		ptr<Expr> func;
		std::vector<ptr<Expr>> args;
	};

	struct Clause {
		Target target;
		ptr<Stmt> stmt;
		ptr<Expr> cond;
	};

	struct Timeout {
		ptr<Expr> time;
		ptr<Stmt> stmt;
		ptr<Expr> cond;
	};

	struct OrElse {
		ptr<Stmt> stmt;
		ptr<Expr> cond;
	};

	std::vector<Clause> clauses;
	Timeout timeout;
	OrElse orElse;

	WaitForStmt( const CodeLocation & loc, std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	WaitForStmt * clone() const override { return new WaitForStmt{ *this }; }
	MUTATE_FRIEND
};

/// Any declaration in a (compound) statement.
class DeclStmt final : public Stmt {
public:
	ptr<Decl> decl;

	DeclStmt( const CodeLocation & loc, const Decl * decl, std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), decl(decl) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	DeclStmt * clone() const override { return new DeclStmt{ *this }; }
	MUTATE_FRIEND
};

/// Represents an implicit application of a constructor or destructor.
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

/// Mutex Statement
class MutexStmt final : public Stmt {
public:
	ptr<Stmt> stmt;
	std::vector<ptr<Expr>> mutexObjs;

	MutexStmt( const CodeLocation & loc, const Stmt * stmt, 
		std::vector<ptr<Expr>> && mutexes, std::vector<Label> && labels = {} )
	: Stmt(loc, std::move(labels)), stmt(stmt), mutexObjs(std::move(mutexes)) {}

	const Stmt * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	MutexStmt * clone() const override { return new MutexStmt{ *this }; }
	MUTATE_FRIEND
};

}

#undef MUTATE_FRIEND

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
