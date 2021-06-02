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
// Last Modified On : Fri Jul 19 14:18:00 2019
// Update Count     : 6
//

#pragma once

#include <list>               // for list
#include <memory>             // for shared_ptr
#include <string>             // for string, allocator
#include <vector>

#include "AST/Fwd.hpp"        // for AST nodes
#include "SynTree/SynTree.h"  // for Visitor Nodes

// helper functions for initialization
namespace InitTweak {
	const FunctionDecl * isAssignment( const Declaration * decl );
	const FunctionDecl * isDestructor( const Declaration * decl );
	const FunctionDecl * isDefaultConstructor( const Declaration * decl );
	const FunctionDecl * isCopyConstructor( const Declaration * decl );
	const FunctionDecl * isCopyFunction( const Declaration * decl, const std::string & fname );
	bool isCopyFunction( const ast::FunctionDecl * decl );

	/// returns the base type of the first parameter to a constructor/destructor/assignment function
	Type * getTypeofThis( FunctionType * ftype );

	/// returns the first parameter of a constructor/destructor/assignment function
	ObjectDecl * getParamThis( FunctionType * ftype );
	const ast::ObjectDecl * getParamThis(const ast::FunctionDecl * func);

	/// generate a bitwise assignment operation.
	ApplicationExpr * createBitwiseAssignment( Expression * dst, Expression * src );

	ast::Expr * createBitwiseAssignment( const ast::Expr * dst, const ast::Expr * src);

	/// transform Initializer into an argument list that can be passed to a call expression
	std::list< Expression * > makeInitList( Initializer * init );
	std::vector< ast::ptr< ast::Expr > > makeInitList( const ast::Init * init );

	/// True if the resolver should try to construct dwt
	bool tryConstruct( DeclarationWithType * dwt );
	bool tryConstruct( const ast::DeclWithType * dwt );

	/// True if the type can have a user-defined constructor
	bool isConstructable( Type * t );
	bool isConstructable( const ast::Type * t );

	/// True if the Initializer contains designations
	bool isDesignated( Initializer * init );
	bool isDesignated( const ast::Init * init );

	/// True if the ObjectDecl's Initializer nesting level is not deeper than the depth of its
	/// type, where the depth of its type is the number of nested ArrayTypes + 1
	bool checkInitDepth( ObjectDecl * objDecl );
	bool checkInitDepth( const ast::ObjectDecl * objDecl );

	/// returns the declaration of the function called by the expr (must be ApplicationExpr or UntypedExpr)
	DeclarationWithType * getFunction( Expression * expr );
	const DeclarationWithType * getFunction( const Expression * expr );
	const ast::DeclWithType * getFunction( const ast::Expr * expr );

	/// Non-Null if expr is a call expression whose target function is intrinsic
	ApplicationExpr * isIntrinsicCallExpr( Expression * expr );
	const ast::ApplicationExpr * isIntrinsicCallExpr( const ast::Expr * expr);

	/// True if stmt is a call statement where the function called is intrinsic and takes one parameter.
	/// Intended to be used for default ctor/dtor calls, but might have use elsewhere.
	/// Currently has assertions that make it less than fully general.
	bool isIntrinsicSingleArgCallStmt( Statement * stmt );
	bool isIntrinsicSingleArgCallStmt( const ast::Stmt * stmt );

	/// True if stmt is a call statement where the function called is intrinsic.
	bool isIntrinsicCallStmt( Statement * stmt );

	/// get all Ctor/Dtor call expressions from a Statement
	void collectCtorDtorCalls( Statement * stmt, std::list< Expression * > & matches );
	std::vector< const ast::Expr * > collectCtorDtorCalls( const ast::Stmt * stmt );

	/// get the Ctor/Dtor call expression from a Statement that looks like a generated ctor/dtor call
	Expression * getCtorDtorCall( Statement * stmt );

	/// returns the name of the function being called
	std::string getFunctionName( Expression * expr );
	std::string getFunctionName( const ast::Expr * expr );

	/// returns the argument to a call expression in position N indexed from 0
	Expression *& getCallArg( Expression * callExpr, unsigned int pos );
	const ast::Expr * getCallArg( const ast::Expr * call, unsigned pos );

	/// returns the base type of a PointerType or ArrayType, else returns NULL
	Type * getPointerBase( Type * );
	const ast::Type* getPointerBase( const ast::Type* );

	/// returns the argument if it is a PointerType or ArrayType, else returns NULL
	Type * isPointerType( Type * );

	/// returns true if expr is trivially a compile-time constant
	bool isConstExpr( Expression * expr );
	bool isConstExpr( Initializer * init );

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
	void addDataSectonAttribute( ObjectDecl * objDecl );

	void addDataSectionAttribute( ast::ObjectDecl * objDecl );

	class InitExpander_old {
	public:
		// expand by stepping through init to get each list of arguments
		InitExpander_old( Initializer * init );

		// always expand to expr
		InitExpander_old( Expression * expr );

		// iterator-like interface
		std::list< Expression * > operator*();
		InitExpander_old & operator++();

		// builds statement which has the same semantics as a C-style list initializer
		// (for array initializers) using callExpr as the base expression to perform initialization
		Statement * buildListInit( UntypedExpr * callExpr );
		void addArrayIndex( Expression * index, Expression * dimension );
		void clearArrayIndices();
		bool addReference();

		class ExpanderImpl;

		typedef std::list< Expression * > IndexList;
	private:
		std::shared_ptr< ExpanderImpl > expander;
		std::list< Expression * > cur;

		// invariant: list of size 2N (elements come in pairs [index, dimension])
		IndexList indices;
	};

	class InitExpander_new {
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
		InitExpander_new( const ast::Init * init );

		/// Always expand to expression
		InitExpander_new( const ast::Expr * expr );

		std::vector< ast::ptr< ast::Expr > > operator* ();
		InitExpander_new & operator++ ();

		/// builds statement which has the same semantics as a C-style list initializer (for array
		/// initializers) using callExpr as the base expression to perform initialization.
		/// Mutates callExpr
		ast::ptr< ast::Stmt > buildListInit( ast::UntypedExpr * callExpr );

		void addArrayIndex( const ast::Expr * index, const ast::Expr * dimension );

		void clearArrayIndices();

		bool addReference();
	};
} // namespace

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
