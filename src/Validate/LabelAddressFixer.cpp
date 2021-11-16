//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LabelAddressFixer.cpp --
//
// Author           : Andrew Beach
// Created On       : Fri Nov 12 16:30:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Fri Nov 12 16:30:00 2021
// Update Count     : 0
//

#include "Validate/LabelAddressFixer.hpp"

#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"

#include <set>

namespace Validate {

namespace {

struct LabelFinder {
	std::set<ast::Label> & labels;
	LabelFinder( std::set<ast::Label> & labels ) : labels( labels ) {}
	void previsit( const ast::Stmt * stmt ) {
		for ( const ast::Label & label : stmt->labels ) {
			labels.insert( label );
		}
	}
};

struct LabelAddressFixer : public ast::WithGuards {
	std::set<ast::Label> labels;
	void previsit( const ast::FunctionDecl * decl );
	const ast::Expr * postvisit( const ast::AddressExpr * expr );
};

void LabelAddressFixer::previsit( const ast::FunctionDecl * decl ) {
	GuardValue( labels );
	ast::Pass<LabelFinder>::read( decl, labels );
}

const ast::Expr * LabelAddressFixer::postvisit( const ast::AddressExpr * expr ) {
	if ( auto inner = expr->arg.as<ast::AddressExpr>() ) {
		if ( auto nameExpr = inner->arg.as<ast::NameExpr>() ) {
			ast::Label label( nameExpr->location, nameExpr->name );
			if ( labels.count( label ) ) {
				return new ast::LabelAddressExpr( nameExpr->location, std::move( label ) );
			}
		}
	}
	return expr;
}

} // namespace

void fixLabelAddresses( ast::TranslationUnit & translationUnit ) {
	ast::Pass<LabelAddressFixer>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
