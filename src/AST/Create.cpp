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
// Last Modified On : Tue Sep 20 14:55:00 2022
// Update Count     : 0
//

#include "AST/Create.hpp"

#include "AST/Attribute.hpp"
#include "AST/Copy.hpp"
#include "AST/Decl.hpp"

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
