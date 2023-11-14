//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Generate.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 16 03:01:51 2020
// Update Count     : 9
//
#include "Generate.h"

#include <iostream>                  // for ostream, endl, operator<<
#include <list>                      // for list
#include <string>                    // for operator<<

#include "CodeGeneratorNew.hpp"      // for CodeGenerator, doSemicolon, ...
#include "GenType.h"                 // for genPrettyType

using namespace std;

namespace CodeGen {

namespace {
	bool shouldClean( ast::Decl const * decl ) {
		return dynamic_cast<ast::TraitDecl const *>( decl );
	}

	/// Removes various nodes that should not exist in CodeGen.
	struct TreeCleaner final {
		ast::CompoundStmt const * previsit( ast::CompoundStmt const * stmt ) {
			auto mutStmt = ast::mutate( stmt );
			erase_if( mutStmt->kids, []( ast::Stmt const * stmt ){
				auto declStmt = dynamic_cast<ast::DeclStmt const *>( stmt );
				return ( declStmt ) ? shouldClean( declStmt->decl ) : false;
			} );
			return mutStmt;
		}

		ast::Stmt const * postvisit( ast::ImplicitCtorDtorStmt const * stmt ) {
			return stmt->callStmt;
		}
	};
} // namespace

void generate( ast::TranslationUnit & translationUnit, std::ostream & os, bool doIntrinsics,
		bool pretty, bool generateC, bool lineMarks, bool printExprTypes ) {
	erase_if( translationUnit.decls, shouldClean );
	ast::Pass<TreeCleaner>::run( translationUnit );

	ast::Pass<CodeGenerator> cgv( os,
			Options( pretty, generateC, lineMarks, printExprTypes ) );
	for ( auto & decl : translationUnit.decls ) {
		if ( decl->linkage.is_generatable && (doIntrinsics || !decl->linkage.is_builtin ) ) {
			cgv.core.updateLocation( decl );
			decl->accept( cgv );
			if ( doSemicolon( decl ) ) {
				os << ";";
			}
			os << cgv.core.endl;
		}
	}
}

} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
