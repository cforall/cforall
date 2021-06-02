//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeData.h --
//
// Author           : Peter A. Buhr
// Created On       : Sat May 16 15:18:36 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Mar 27 09:05:35 2021
// Update Count     : 200
//

#pragma once

#include <iosfwd>										// for ostream
#include <list>											// for list
#include <string>										// for string

#include "ParseNode.h"									// for DeclarationNode, DeclarationNode::Ag...
#include "SynTree/LinkageSpec.h"						// for Spec
#include "SynTree/Type.h"								// for Type, ReferenceToType (ptr only)
#include "SynTree/SynTree.h"							// for Visitor Nodes

struct TypeData {
	enum Kind { Basic, Pointer, Reference, Array, Function, Aggregate, AggregateInst, Enum, EnumConstant, Symbolic,
				SymbolicInst, Tuple, Typeof, Basetypeof, Builtin, GlobalScope, Qualified, Unknown };

	struct Aggregate_t {
		AggregateDecl::Aggregate kind;
		const std::string * name = nullptr;
		DeclarationNode * params = nullptr;
		ExpressionNode * actuals = nullptr;				// holds actual parameters later applied to AggInst
		DeclarationNode * fields = nullptr;
		bool body;
		bool anon;

		bool tagged;
		const std::string * parent = nullptr;
	};

	struct AggInst_t {
		TypeData * aggregate = nullptr;
		ExpressionNode * params = nullptr;
		bool hoistType;
	};

	struct Array_t {
		ExpressionNode * dimension = nullptr;
		bool isVarLen;
		bool isStatic;
	};

	struct Enumeration_t {
		const std::string * name = nullptr;
		DeclarationNode * constants = nullptr;
		bool body;
		bool anon;
	};

	struct Function_t {
		mutable DeclarationNode * params = nullptr;		// mutables modified in buildKRFunction
		mutable DeclarationNode * idList = nullptr;		// old-style
		mutable DeclarationNode * oldDeclList = nullptr;
		StatementNode * body = nullptr;
		ExpressionNode * withExprs = nullptr;			// expressions from function's with_clause
	};

	struct Symbolic_t {
		const std::string * name = nullptr;
		bool isTypedef;									// false => TYPEGENname, true => TYPEDEFname
		DeclarationNode * params = nullptr;
		ExpressionNode * actuals = nullptr;
		DeclarationNode * assertions = nullptr;
	};

	struct Qualified_t {								// qualified type S.T
		TypeData * parent = nullptr;
		TypeData * child = nullptr;
	};

	CodeLocation location;

	Kind kind;
	TypeData * base;
	DeclarationNode::BasicType basictype = DeclarationNode::NoBasicType;
	DeclarationNode::ComplexType complextype = DeclarationNode::NoComplexType;
	DeclarationNode::Signedness signedness = DeclarationNode::NoSignedness;
	DeclarationNode::Length length = DeclarationNode::NoLength;
	DeclarationNode::BuiltinType builtintype = DeclarationNode::NoBuiltinType;

	Type::Qualifiers qualifiers;
	DeclarationNode * forall = nullptr;

	Aggregate_t aggregate;
	AggInst_t aggInst;
	Array_t array;
	Enumeration_t enumeration;
	Function_t function;
	Symbolic_t symbolic;
	Qualified_t qualified;
	DeclarationNode * tuple = nullptr;
	ExpressionNode * typeexpr = nullptr;

	TypeData( Kind k = Unknown );
	~TypeData();
	void print( std::ostream &, int indent = 0 ) const;
	TypeData * clone() const;

	const std::string * leafName() const;
};

Type * typebuild( const TypeData * );
TypeData * typeextractAggregate( const TypeData * td, bool toplevel = true );
Type::Qualifiers buildQualifiers( const TypeData * td );
Type * buildBasicType( const TypeData * );
PointerType * buildPointer( const TypeData * );
ArrayType * buildArray( const TypeData * );
ReferenceType * buildReference( const TypeData * );
AggregateDecl * buildAggregate( const TypeData *, std::list< Attribute * > );
ReferenceToType * buildComAggInst( const TypeData *, std::list< Attribute * > attributes, LinkageSpec::Spec linkage );
ReferenceToType * buildAggInst( const TypeData * );
TypeDecl * buildVariable( const TypeData * );
EnumDecl * buildEnum( const TypeData *, std::list< Attribute * >, LinkageSpec::Spec );
TypeInstType * buildSymbolicInst( const TypeData * );
TupleType * buildTuple( const TypeData * );
TypeofType * buildTypeof( const TypeData * );
Declaration * buildDecl( const TypeData *, const std::string &, Type::StorageClasses, Expression *, Type::FuncSpecifiers funcSpec, LinkageSpec::Spec, Expression * asmName,
						 Initializer * init = nullptr, std::list< class Attribute * > attributes = std::list< class Attribute * >() );
FunctionType * buildFunction( const TypeData * );
void buildKRFunction( const TypeData::Function_t & function );

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
