//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// HoistTypeDecls.cpp -- Hoists declarations of implicitly declared types.
//
// Author           : Andrew Beach
// Created On       : Mon Jul  4  9:52:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jul 12 14:07:00 2022
// Update Count     : 0
//

#include "HoistTypeDecls.hpp"

#include "AST/Pass.hpp"

namespace Validate {

namespace {

struct HoistTypeDecls final : public ast::WithDeclsToAdd<> {
	void previsit( ast::SizeofExpr const * );
	void previsit( ast::AlignofExpr const * );
	void previsit( ast::UntypedOffsetofExpr const * );
	void previsit( ast::CompoundLiteralExpr const * );
	void handleType( ast::Type const * );
};

void HoistTypeDecls::previsit( ast::SizeofExpr const * expr ) {
	handleType( expr->type );
}

void HoistTypeDecls::previsit( ast::AlignofExpr const * expr ) {
	handleType( expr->type );
}

void HoistTypeDecls::previsit( ast::UntypedOffsetofExpr const * expr ) {
	handleType( expr->type );
}

void HoistTypeDecls::previsit( ast::CompoundLiteralExpr const * expr ) {
	handleType( expr->result );
}

void HoistTypeDecls::handleType( ast::Type const * type ) {
	// Some type declarations are buried in expressions and not easy to
	// hoist during parsing; so they are hoisted here instead.
	// This also where some missing code locations come in.
	ast::AggregateDecl const * aggr = nullptr;
	if ( auto inst = dynamic_cast<ast::StructInstType const *>( type ) ) {
		aggr = inst->base;
	} else if ( auto inst = dynamic_cast<ast::UnionInstType const *>( type ) ) {
		aggr = inst->base;
	} else if ( auto inst = dynamic_cast<ast::EnumInstType const *>( type ) ) {
		aggr = inst->base;
	}
	if ( aggr && aggr->body ) {
		declsToAddBefore.push_front( aggr );
	}
}

} // namespace

void hoistTypeDecls( ast::TranslationUnit & translationUnit ) {
	ast::Pass<HoistTypeDecls>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
