//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CompoundLiteral.cpp -- Use variables to implement compound literals.
//
// Author           : Andrew Beach
// Created On       : Mon Nov 15 16:33:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Mon Nov 16  9:47:00 2021
// Update Count     : 0
//

#include "CompoundLiteral.hpp"

#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "Common/UniqueName.hpp"

namespace Validate {

namespace {

struct CompoundLiteral final :
		public ast::WithDeclsToAdd<> {
	ast::Storage::Classes storageClasses;

	void previsit( const ast::ObjectDecl * decl );
	const ast::Expr * postvisit( const ast::CompoundLiteralExpr * expr );
};

void CompoundLiteral::previsit( const ast::ObjectDecl * decl ) {
	storageClasses = decl->storage;
}

const ast::Expr * CompoundLiteral::postvisit(
		const ast::CompoundLiteralExpr * expr ) {
	static UniqueName litName( "_compLit" );

	// Transform: [storageClasses] ... (struct S){...} ...
	// Into:      [storageClasses] struct S _compLit = {...};
	//                             ... _compLit ...
	ast::ObjectDecl * temp = new ast::ObjectDecl(
		expr->location,
		litName.newName(),
		expr->result,
		expr->init,
		storageClasses
		);
	declsToAddBefore.push_back( temp );
	return new ast::VariableExpr( expr->location, temp );
}

} // namespace

void handleCompoundLiterals( ast::TranslationUnit & translationUnit ) {
	ast::Pass<CompoundLiteral>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
