//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeData.hpp --
//
// Author           : Peter A. Buhr
// Created On       : Sat May 16 15:18:36 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Feb 22 16:30:31 2024
// Update Count     : 210
//

#pragma once

#include <iosfwd>                                   // for ostream
#include <list>                                     // for list
#include <string>                                   // for string

#include "AST/CVQualifiers.hpp"                     // for CV
#include "AST/Fwd.hpp"                              // for Type
#include "DeclarationNode.hpp"                      // for DeclarationNode

struct TypeData {
	// Type flags used in this type, and there names (harmonize with implementation).
	enum BasicType {
		Void, Bool, Char, Int, Int128,
		Float, Double, LongDouble, uuFloat80, uuFloat128,
		uFloat16, uFloat32, uFloat32x, uFloat64, uFloat64x, uFloat128, uFloat128x,
		NoBasicType
	};
	static const char * basicTypeNames[];
	enum ComplexType { Complex, NoComplexType, Imaginary };
	// Imaginary unsupported => parse, but make invisible and print error message
	static const char * complexTypeNames[];
	enum Signedness { Signed, Unsigned, NoSignedness };
	static const char * signednessNames[];
	enum Length { Short, Long, LongLong, NoLength };
	static const char * lengthNames[];
	enum BuiltinType { Valist, AutoType, Zero, One, NoBuiltinType };
	static const char * builtinTypeNames[];

	enum Kind { Basic, Pointer, Reference, Array, Function, Aggregate, AggregateInst, EnumConstant, Symbolic,
				SymbolicInst, Tuple, Basetypeof, Typeof, Vtable, Builtin, GlobalScope, Qualified, Unknown };

	struct Aggregate_t {
		ast::AggregateDecl::Aggregate kind;
		const std::string * name = nullptr;
		// Polymorphics parameters. (Polymorphic types only.)
		DeclarationNode * params = nullptr;
		// Arguments later applied to AggInst. (Polymorphic types only.)
		ExpressionNode * actuals = nullptr;
		// Only set if body is true. (Constants for enumerations.)
		DeclarationNode * fields = nullptr;
		std::vector<ast::ptr<ast::Attribute>> attributes;
		// Is this a declaration with a body (may have fields)?
		bool body;
		// Is this type anonymous? (Name can still be set to generated name.)
		bool anon;
		// Is this a cfa enumeration? Type stored in base.
		bool isCfa;
		EnumHiding hiding;
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
	BasicType basictype = NoBasicType;
	ComplexType complextype = NoComplexType;
	Signedness signedness = NoSignedness;
	Length length = NoLength;
	BuiltinType builtintype = NoBuiltinType;

	ast::CV::Qualifiers qualifiers;
	DeclarationNode * forall = nullptr;

	Aggregate_t aggregate;
	AggInst_t aggInst;
	Array_t array;
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

	TypeData * getLastBase();
	void setLastBase( TypeData * );
};


TypeData * build_type_qualifier( ast::CV::Qualifiers );
TypeData * build_basic_type( TypeData::BasicType );
TypeData * build_complex_type( TypeData::ComplexType );
TypeData * build_signedness( TypeData::Signedness );
TypeData * build_builtin_type( TypeData::BuiltinType );
TypeData * build_length( TypeData::Length );
TypeData * build_forall( DeclarationNode * );
TypeData * build_global_scope();
TypeData * build_qualified_type( TypeData *, TypeData * );
TypeData * build_typedef( const std::string * name );
TypeData * build_type_gen( const std::string * name, ExpressionNode * params );
TypeData * build_vtable_type( TypeData * );

TypeData * addQualifiers( TypeData * ltype, TypeData * rtype );
TypeData * addType( TypeData * ltype, TypeData * rtype, std::vector<ast::ptr<ast::Attribute>> & );
TypeData * addType( TypeData * ltype, TypeData * rtype );
TypeData * cloneBaseType( TypeData * type, TypeData * other );
TypeData * makeNewBase( TypeData * type );


ast::Type * typebuild( const TypeData * );
TypeData * typeextractAggregate( const TypeData * td, bool toplevel = true );
ast::CV::Qualifiers buildQualifiers( const TypeData * td );
ast::Type * buildBasicType( const TypeData * );
ast::PointerType * buildPointer( const TypeData * );
ast::ArrayType * buildArray( const TypeData * );
ast::ReferenceType * buildReference( const TypeData * );
ast::AggregateDecl * buildAggregate( const TypeData *, std::vector<ast::ptr<ast::Attribute>> );
ast::BaseInstType * buildComAggInst( const TypeData *, std::vector<ast::ptr<ast::Attribute>> && attributes, ast::Linkage::Spec linkage );
ast::BaseInstType * buildAggInst( const TypeData * );
ast::TypeDecl * buildVariable( const TypeData * );
ast::EnumDecl * buildEnum( const TypeData *, std::vector<ast::ptr<ast::Attribute>> &&, ast::Linkage::Spec );
ast::TypeInstType * buildSymbolicInst( const TypeData * );
ast::TupleType * buildTuple( const TypeData * );
ast::TypeofType * buildTypeof( const TypeData * );
ast::VTableType * buildVtable( const TypeData * );
ast::Decl * buildDecl(
	const TypeData *, const std::string &, ast::Storage::Classes, ast::Expr *,
	ast::Function::Specs funcSpec, ast::Linkage::Spec, ast::Expr * asmName,
	ast::Init * init = nullptr, std::vector<ast::ptr<ast::Attribute>> && attributes = std::vector<ast::ptr<ast::Attribute>>() );
ast::FunctionType * buildFunctionType( const TypeData * );
void buildKRFunction( const TypeData::Function_t & function );

static inline ast::Type * maybeMoveBuildType( TypeData * type ) {
	ast::Type * ret = type ? typebuild( type ) : nullptr;
	delete type;
	return ret;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
