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
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:38:35 2017
// Update Count     : 3
//

#pragma once

class Type;
namespace SymTab {
class Indexer;
}  // namespace SymTab
namespace ast {
	class Type;
	class SymbolTable;
	class ObjectDecl;
}

namespace ResolvExpr {
	Type *resolveTypeof( Type*, const SymTab::Indexer &indexer );
	const ast::Type * resolveTypeof( const ast::Type *, const ast::SymbolTable & );
	const ast::ObjectDecl * fixObjectType( const ast::ObjectDecl * decl , const ast::SymbolTable & symtab );
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
