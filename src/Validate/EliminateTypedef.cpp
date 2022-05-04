//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// EliminateTypedef.cpp -- Removes TypedefDecl nodes from the AST.
//
// Author           : Andrew Beach
// Created On       : Wed Apr 20 16:37:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Apr 25 14:26:00 2022
// Update Count     : 0
//

#include "Validate/EliminateTypedef.hpp"

#include <algorithm>

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "Common/utility.h"

namespace Validate {

namespace {

struct EliminateTypedefCore {
	ast::StructDecl const * previsit( ast::StructDecl const * decl );
	ast::UnionDecl const * previsit( ast::UnionDecl const * decl );
	ast::CompoundStmt const * previsit( ast::CompoundStmt const * stmt );
};

bool isTypedef( ast::ptr<ast::Decl> const & decl ) {
	return (nullptr != decl.as<ast::TypedefDecl>());
}

bool isTypedefStmt( ast::ptr<ast::Stmt> const & stmt ) {
	if ( auto declStmt = stmt.as<ast::DeclStmt>() ) {
		return isTypedef( declStmt->decl );
	}
	return false;
}

template<typename node_t, typename super_t, typename field_t, typename pred_t>
node_t const * field_erase_if( node_t const * node, field_t super_t::*field, pred_t && pred) {
	node_t * mut = ast::mutate( node );
	erase_if( mut->*field, pred );
	return mut;
}

ast::StructDecl const * EliminateTypedefCore::previsit( ast::StructDecl const * decl ) {
	return field_erase_if( decl, &ast::StructDecl::members, isTypedef );
}

ast::UnionDecl const * EliminateTypedefCore::previsit( ast::UnionDecl const * decl ) {
	return field_erase_if( decl, &ast::UnionDecl::members, isTypedef );
}

ast::CompoundStmt const * EliminateTypedefCore::previsit( ast::CompoundStmt const * stmt ) {
	return field_erase_if( stmt, &ast::CompoundStmt::kids, isTypedefStmt );
}

} // namespace

/// Removes TypedefDecl nodes from the AST.
void eliminateTypedef( ast::TranslationUnit & translationUnit ) {
	ast::Pass<EliminateTypedefCore>::run( translationUnit );
	erase_if( translationUnit.decls, isTypedef );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
