//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenInit.h -- Generate initializers, and other stuff.
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri Mar 18 14:22:00 2022
// Update Count     : 7
//

#pragma once

#include <list>                // for list
#include <string>              // for string

#include "AST/Fwd.hpp"
#include "Common/CodeLocation.h"
#include "GenPoly/ScopedSet.h" // for ScopedSet

namespace InitTweak {

/// Adds return value temporaries and wraps Initializers in ConstructorInit nodes
void genInit( ast::TranslationUnit & translationUnit );

/// Converts return statements into copy constructor calls on the hidden return variable.
/// This pass must happen before auto-gen.
void fixReturnStatements( ast::TranslationUnit & translationUnit );

/// generates a single ctor/dtor statement using objDecl as the 'this' parameter and arg as the optional argument
ast::ptr<ast::Stmt> genCtorDtor (const CodeLocation & loc, const std::string & fname, const ast::ObjectDecl * objDecl, const ast::Expr * arg = nullptr);

/// creates an appropriate ConstructorInit node which contains a constructor, destructor, and C-initializer
ast::ConstructorInit * genCtorInit( const CodeLocation & loc, const ast::ObjectDecl * objDecl );

class ManagedTypes final {
public:
	bool isManaged( const ast::ObjectDecl * objDecl ) const ; // determine if object is managed
	bool isManaged( const ast::Type * type ) const; // determine if type is managed

	void handleDWT( const ast::DeclWithType * dwt ); // add type to managed if ctor/dtor
	void handleStruct( const ast::StructDecl * aggregateDecl ); // add type to managed if child is managed

	void beginScope();
	void endScope();
private:
	GenPoly::ScopedSet< std::string > managedTypes;
};

} // namespace

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
