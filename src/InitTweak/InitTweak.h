//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// InitTweak.h --
//
// Author           : Rob Schluntz
// Created On       : Fri May 13 11:26:36 2016
// Last Modified By : Andrew Beach
// Last Modified On : Wed Sep 22  9:21:00 2022
// Update Count     : 9
//

#pragma once

#include <list>               // for list
#include <memory>             // for shared_ptr
#include <string>             // for string, allocator
#include <vector>

#include "AST/Fwd.hpp"        // for AST nodes

// helper functions for initialization
namespace InitTweak {
	bool isAssignment( const ast::FunctionDecl * decl );
	bool isDestructor( const ast::FunctionDecl * decl );
	bool isDefaultConstructor( const ast::FunctionDecl * decl );
	bool isCopyConstructor( const ast::FunctionDecl * decl );
	bool isCopyFunction( const ast::FunctionDecl * decl );

	/// returns the base type of the first parameter to a constructor/destructor/assignment function
	const ast::Type * getTypeofThis( const ast::FunctionType * ftype );

	/// returns the first parameter of a constructor/destructor/assignment function
	const ast::ObjectDecl * getParamThis(const ast::FunctionDecl * func);

	/// generate a bitwise assignment operation.
	ast::Expr * createBitwiseAssignment( const ast::Expr * dst, const ast::Expr * src);

	/// transform Initializer into an argument list that can be passed to a call expression
	std::vector< ast::ptr< ast::Expr > > makeInitList( const ast::Init * init );

	/// True if the resolver should try to construct dwt
	bool tryConstruct( const ast::DeclWithType * dwt );

	/// True if the type can have a user-defined constructor
	bool isConstructable( const ast::Type * t );

	/// True if the Initializer contains designations
	bool isDesignated( const ast::Init * init );

	/// True if the ObjectDecl's Initializer nesting level is not deeper than the depth of its
	/// type, where the depth of its type is the number of nested ArrayTypes + 1
	bool checkInitDepth( const ast::ObjectDecl * objDecl );

	/// True if stmt is a call statement where the function called is intrinsic and takes one parameter.
	/// Intended to be used for default ctor/dtor calls, but might have use elsewhere.
	/// Currently has assertions that make it less than fully general.
	bool isIntrinsicSingleArgCallStmt( const ast::Stmt * stmt );

	/// get all Ctor/Dtor call expressions from a Statement
	std::vector< const ast::Expr * > collectCtorDtorCalls( const ast::Stmt * stmt );

	/// returns true if expr is trivially a compile-time constant
	bool isConstExpr( const ast::Expr * expr );
	bool isConstExpr( const ast::Init * init );

	/// Modifies objDecl to have:
	///    __attribute__((section (".data#")))
	/// which makes gcc put the declared variable in the data section,
	/// which is helpful for global constants on newer gcc versions,
	/// so that CFA's generated initialization won't segfault when writing it via a const cast.
	/// The trailing # is an injected assembly comment, to suppress the "a" in
	///    .section .data,"a"
	///    .section .data#,"a"
	/// to avoid assembler warning "ignoring changed section attributes for .data"
	void addDataSectionAttribute( ast::ObjectDecl * objDecl );

	class InitExpander final {
	public:
		using IndexList = std::vector< ast::ptr< ast::Expr > >;
		class ExpanderImpl;

	private:
		std::shared_ptr< ExpanderImpl > expander;
		std::vector< ast::ptr< ast::Expr > > crnt;
		// invariant: list of size 2N (elements come in pairs [index, dimension])
		IndexList indices;

	public:
		/// Expand by stepping through init to get each list of arguments
		InitExpander( const ast::Init * init );

		/// Always expand to expression
		InitExpander( const ast::Expr * expr );

		std::vector< ast::ptr< ast::Expr > > operator* ();
		InitExpander & operator++ ();

		/// builds statement which has the same semantics as a C-style list initializer (for array
		/// initializers) using callExpr as the base expression to perform initialization.
		/// Mutates callExpr
		ast::ptr< ast::Stmt > buildListInit( ast::UntypedExpr * callExpr );

		void addArrayIndex( const ast::Expr * index, const ast::Expr * dimension );

		void clearArrayIndices();

		bool addReference();
	};
} // namespace InitTweak

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
