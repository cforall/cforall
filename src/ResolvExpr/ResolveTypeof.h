//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolveTypeof.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:14:53 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Mar 16 11:33:00 2022
// Update Count     : 4
//

#pragma once

class Type;
namespace SymTab {
class Indexer;
}  // namespace SymTab
namespace ast {
	class Type;
	class ObjectDecl;
}

namespace ResolvExpr {
	struct ResolveContext;

	Type *resolveTypeof( Type*, const SymTab::Indexer &indexer );
	const ast::Type * resolveTypeof( const ast::Type *, const ResolveContext & );
	const ast::Type * fixArrayType( const ast::Type *, const ResolveContext & );
	const ast::ObjectDecl * fixObjectType( const ast::ObjectDecl * decl , const ResolveContext & );
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
