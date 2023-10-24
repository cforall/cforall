//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// EraseWith.cpp -- After resolution, erase all with constructs.
//
// Author           : Andrew Beach
// Created On       : Sun Oct  8  9:42:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Sun Oct  8 10:03:00 2023
// Update Count     : 0
//

#include "EraseWith.hpp"

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"

namespace ResolvExpr {

namespace {

struct WithEraser {
	ast::FunctionDecl const * postvisit( ast::FunctionDecl const * decl ) {
		if ( decl->withExprs.empty() ) return decl;
		auto mutDecl = mutate( decl );
		mutDecl->withExprs.clear();
		return mutDecl;
	}

	ast::Stmt const * postvisit( ast::DeclStmt const * stmt ) {
		if ( auto decl = stmt->decl.as<ast::WithStmt>() ) return decl->stmt;
		return stmt;
	}
};

} // namespace

void eraseWith( ast::TranslationUnit & translationUnit ) {
	ast::Pass<WithEraser>::run( translationUnit );
}

} // namespace ResolvExpr
