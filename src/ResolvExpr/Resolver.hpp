//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Resolver.hpp --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:18:34 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Mar 16 11:32:00 2022
// Update Count     : 5
//

#pragma once

#include "AST/Node.hpp"  // for ptr

namespace ast {
	class ConstructorInit;
	class Decl;
	class DeletedExpr;
	class Expr;
	class Init;
	class StmtExpr;
	class SymbolTable;
	class TranslationGlobal;
	class TranslationUnit;
	class Type;
	class TypeEnvironment;
} // namespace ast

namespace ResolvExpr {

/// Helper Type: Passes around information between various sub-calls.
struct ResolveContext {
	const ast::SymbolTable & symtab;
	const ast::TranslationGlobal & global;
};

/// Checks types and binds syntactic constructs to typed representations
void resolve( ast::TranslationUnit& translationUnit );
/// Searches expr and returns the first DeletedExpr found, otherwise nullptr
const ast::DeletedExpr * findDeletedExpr( const ast::Expr * expr );
/// Find the expression candidate that is the unique
/// best match for `untyped` in a `void` context.
ast::ptr< ast::Expr > resolveInVoidContext(
	const ast::Expr * expr, const ResolveContext &, ast::TypeEnvironment & env );
/// Resolve `untyped` to the single expression whose
/// candidate is the best match for the given type.
ast::ptr< ast::Expr > findSingleExpression(
	const ast::Expr * untyped, const ast::Type * type, const ResolveContext & );
ast::ptr< ast::Expr > findVoidExpression(
	const ast::Expr * untyped, const ResolveContext & );
/// Resolves a constructor init expression
ast::ptr< ast::Init > resolveCtorInit(
	const ast::ConstructorInit * ctorInit, const ResolveContext & context );
/// Resolves a statement expression
const ast::Expr * resolveStmtExpr(
	const ast::StmtExpr * stmtExpr, const ResolveContext & context );

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
