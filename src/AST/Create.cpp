//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Create.cpp -- Helpers to create pieces of the AST.
//
// Author           : Andrew Beach
// Created On       : Tue Sep 20 13:28:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Sep 21  9:29:00 2022
// Update Count     : 1
//

#include "AST/Create.hpp"

#include "AST/Attribute.hpp"
#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/Type.hpp"
#include "Common/Iterate.hpp"

namespace ast {

namespace {

	template<typename node_t>
	std::vector<ast::ptr<node_t>> vectorCopy(
			std::vector<ast::ptr<node_t>> const & nodes ) {
		return map_range<std::vector<ast::ptr<node_t>>>( nodes,
			[]( ptr<node_t> const & node ){
				return deepCopy<node_t>( node );
			}
		);
	}

} // namespace

FunctionDecl * asForward( FunctionDecl const * decl ) {
	if ( nullptr == decl->stmts ) {
		return nullptr;
	}
	// The cast and changing the original should be safe as long as the
	// change is reverted before anything else sees it. It's also faster.
	FunctionDecl * mutDecl = const_cast<FunctionDecl *>( decl );
	CompoundStmt const * stmts = mutDecl->stmts.release();
	FunctionDecl * copy = deepCopy( mutDecl );
	mutDecl->stmts = stmts;
	return copy;
}

StructDecl * asForward( StructDecl const * decl ) {
	if ( !decl->body ) {
		return nullptr;
	}
	StructDecl * fwd = new StructDecl( decl->location,
		decl->name,
		decl->kind,
		vectorCopy<ast::Attribute>( decl->attributes ),
		decl->linkage );
	fwd->params = vectorCopy( decl->params );
	return fwd;
}

UnionDecl * asForward( UnionDecl const * decl ) {
	if ( !decl->body ) {
		return nullptr;
	}
	UnionDecl * fwd = new UnionDecl( decl->location,
		decl->name,
		vectorCopy( decl->attributes ),
		decl->linkage );
	fwd->params = vectorCopy( decl->params );
	return fwd;
}

}
