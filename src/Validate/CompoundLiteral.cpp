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
		public ast::WithDeclsToAdd, public ast::WithGuards,
		public ast::WithVisitorRef<CompoundLiteral> {

	// Consider a declaration's storage when handling its initializer's immediate expression
	ast::Storage::Classes declSC = 0;
	void previsit( const ast::ObjectDecl * decl ) {     // capture
		GuardValue(declSC) = decl->storage;
	}
	void previsit( const ast::Init * ) {}               // preserve
	void previsit( const ast::Expr * ) {}               // preserve
	void previsit( const ast::Node * ) {                // reset
		if (declSC.any()) {
			GuardValue(declSC) = 0;
		}
	}
	const ast::Expr * postvisit( const ast::CompoundLiteralExpr * expr );
};

const ast::Expr * CompoundLiteral::postvisit(
		const ast::CompoundLiteralExpr * expr ) {
	static UniqueName litName( "_compLit" );

	// Transform: [   declSC ] ... (struct S){...} ...
	// Into:      [tl(declSC)] struct S _compLit = {...};
	//            [   declSC ] ... _compLit ...
	// where tl(-) means, "add static for top-level"
	ast::Storage::Classes topLevelAdj = isInFunction() ? 0 : ast::Storage::Static;
	ast::ObjectDecl * temp = new ast::ObjectDecl(
		expr->location,
		litName.newName(),
		expr->result,
		expr->init,
		declSC | topLevelAdj
		);
	// FIXME: A resolution of #280 could make the unused attribute unnecessary here
	//     (let test nowarn/unused decide)
	temp->attributes.push_back( new ast::Attribute( "unused" ) );
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
