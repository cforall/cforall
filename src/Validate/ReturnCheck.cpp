//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ReturnCheck.cpp -- Run simple (non-typed) checks on return statements.
//
// Author           : Andrew Beach
// Created On       : Fri Mar 18 14:07:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri Mar 18 14:26:00 2022
// Update Count     : 0
//

#include "ReturnCheck.hpp"

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"

namespace Validate {

namespace {

struct ReturnCore : public ast::WithGuards {
	bool inVoidFunction = false;

	void previsit( const ast::FunctionDecl * decl ) {
		GuardValue( inVoidFunction ) = (0 == decl->returns.size());
	}

	void previsit( const ast::ReturnStmt * stmt ) {
		// The only definite error case is an empty return on a non-void
		// functions. Everything else requires resolution. (You can have
		// a void expression in a return statement.
		if ( !stmt->expr && !inVoidFunction ) {
			SemanticError( stmt, "Non-void function returns no values: " );
		}
	}
};

} // namespace

void checkReturnStatements( ast::TranslationUnit & translationUnit ) {
	ast::Pass<ReturnCore>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
