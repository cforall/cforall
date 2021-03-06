//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenInit.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:31:19 2017
// Update Count     : 4
//

#pragma once

#include <list>                // for list
#include <string>              // for string

#include "AST/Fwd.hpp"
#include "Common/CodeLocation.h"
#include "GenPoly/ScopedSet.h" // for ScopedSet
#include "SynTree/SynTree.h"   // for Visitor Nodes

namespace InitTweak {
	/// Adds return value temporaries and wraps Initializers in ConstructorInit nodes
	void genInit( std::list< Declaration * > & translationUnit );

	/// Converts return statements into copy constructor calls on the hidden return variable
	void fixReturnStatements( std::list< Declaration * > & translationUnit );

	/// generates a single ctor/dtor statement using objDecl as the 'this' parameter and arg as the optional argument
	ImplicitCtorDtorStmt * genCtorDtor( const std::string & fname, ObjectDecl * objDecl, Expression * arg = nullptr );
	ast::ptr<ast::Stmt> genCtorDtor (const CodeLocation & loc, const std::string & fname, const ast::ObjectDecl * objDecl, const ast::Expr * arg = nullptr);

	/// creates an appropriate ConstructorInit node which contains a constructor, destructor, and C-initializer
	ConstructorInit * genCtorInit( ObjectDecl * objDecl );
	ast::ConstructorInit * genCtorInit( const CodeLocation & loc, const ast::ObjectDecl * objDecl );

	class ManagedTypes {
	public:
		bool isManaged( ObjectDecl * objDecl ) const ; // determine if object is managed
		bool isManaged( Type * type ) const; // determine if type is managed

		void handleDWT( DeclarationWithType * dwt ); // add type to managed if ctor/dtor
		void handleStruct( StructDecl * aggregateDecl ); // add type to managed if child is managed

		void beginScope();
		void endScope();
	private:
		GenPoly::ScopedSet< std::string > managedTypes;
	};

	class ManagedTypes_new {
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
