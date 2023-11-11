//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixNames.cc -- Adjustments to typed declarations.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jul 20 11:49:00 2022
// Update Count     : 24
//

#include "FixNames.h"

#include <memory>                  // for unique_ptr
#include <string>                  // for string, operator!=, operator==

#include "AST/Chain.hpp"
#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "Common/SemanticError.h"  // for SemanticError
#include "FixMain.h"               // for FixMain
#include "SymTab/Mangler.h"        // for Mangler
#include "CompilationState.h"

namespace CodeGen {

namespace {

/// Does work with the main function and scopeLevels.
class FixNames final {
	int scopeLevel = 1;

	bool shouldSetScopeLevel( const ast::DeclWithType * dwt ) {
		return !dwt->name.empty() && dwt->linkage.is_mangled
			&& dwt->scopeLevel != scopeLevel;
	}
public:
	const ast::ObjectDecl *postvisit( const ast::ObjectDecl *objectDecl ) {
		if ( shouldSetScopeLevel( objectDecl ) ) {
			return ast::mutate_field( objectDecl, &ast::ObjectDecl::scopeLevel, scopeLevel );
		}
		return objectDecl;
	}

	const ast::FunctionDecl *postvisit( const ast::FunctionDecl *functionDecl ) {
		if ( isMain( functionDecl ) ) {
			auto mutDecl = ast::mutate( functionDecl );

			if ( shouldSetScopeLevel( mutDecl ) ) {
				mutDecl->scopeLevel = scopeLevel;
			}

			int nargs = mutDecl->params.size();
			if ( 0 != nargs && 2 != nargs && 3 != nargs ) {
				SemanticError( functionDecl, "Main expected to have 0, 2 or 3 arguments\n" );
			}
			ast::chain_mutate( mutDecl->stmts )->kids.push_back(
				new ast::ReturnStmt(
					mutDecl->location,
					ast::ConstantExpr::from_int( mutDecl->location, 0 )
				)
			);

			return mutDecl;
		} else if ( shouldSetScopeLevel( functionDecl ) ) {
			return ast::mutate_field( functionDecl, &ast::FunctionDecl::scopeLevel, scopeLevel );
		} else {
			return functionDecl;
		}
	}

	void previsit( const ast::CompoundStmt * ) {
		scopeLevel += 1;
	}

	void postvisit( const ast::CompoundStmt * ) {
		scopeLevel -= 1;
	}
};

} // namespace

void fixNames( ast::TranslationUnit & translationUnit ) {
	ast::Pass<FixNames>::run( translationUnit );
}

} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
