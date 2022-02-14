//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Resolver.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:18:34 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Feb 18 20:40:38 2019
// Update Count     : 4
//

#pragma once

#include <list>          // for list

#include "AST/Node.hpp"  // for ptr

class ConstructorInit;
class Declaration;
class Expression;
class StmtExpr;
namespace SymTab {
	class Indexer;
} // namespace SymTab

namespace ast {
	class ConstructorInit;
	class Decl;
	class DeletedExpr;
	class Init;
	class StmtExpr;
	class SymbolTable;
	class TranslationUnit;
	class Type;
	class TypeEnvironment;
} // namespace ast

namespace ResolvExpr {
	/// Checks types and binds syntactic constructs to typed representations
	void resolve( std::list< Declaration * > translationUnit );
	void resolveDecl( Declaration *, const SymTab::Indexer & indexer );
	Expression *resolveInVoidContext( Expression * expr, const SymTab::Indexer & indexer );
	void findVoidExpression( Expression *& untyped, const SymTab::Indexer & indexer );
	void findSingleExpression( Expression *& untyped, const SymTab::Indexer & indexer );
	void findSingleExpression( Expression *& untyped, Type * type, const SymTab::Indexer & indexer );
	void resolveCtorInit( ConstructorInit * ctorInit, const SymTab::Indexer & indexer );
	void resolveStmtExpr( StmtExpr * stmtExpr, const SymTab::Indexer & indexer );
	/// Searches expr and returns the first DeletedExpr found, otherwise nullptr
	DeletedExpr * findDeletedExpr( Expression * expr );
	/// Resolves with-stmts and with-clauses on functions
	void resolveWithExprs( std::list< Declaration * > & translationUnit );

	/// Checks types and binds syntactic constructs to typed representations
	void resolve( ast::TranslationUnit& translationUnit );
	/// Searches expr and returns the first DeletedExpr found, otherwise nullptr
	const ast::DeletedExpr * findDeletedExpr( const ast::Expr * expr );
	/// Find the expression candidate that is the unique best match for `untyped` in a `void`
	/// context.
	ast::ptr< ast::Expr > resolveInVoidContext(
		const ast::Expr * expr, const ast::SymbolTable & symtab, ast::TypeEnvironment & env );
	/// Resolve `untyped` to the single expression whose candidate is the best match for the
	/// given type.
	ast::ptr< ast::Expr > findSingleExpression(
		const ast::Expr * untyped, const ast::Type * type, const ast::SymbolTable & symtab );
	ast::ptr< ast::Expr > findVoidExpression(
		const ast::Expr * untyped, const ast::SymbolTable & symtab);
	/// Resolves a constructor init expression
	ast::ptr< ast::Init > resolveCtorInit(
		const ast::ConstructorInit * ctorInit, const ast::SymbolTable & symtab );
	/// Resolves a statement expression
	const ast::Expr * resolveStmtExpr(
		const ast::StmtExpr * stmtExpr, const ast::SymbolTable & symtab );
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
