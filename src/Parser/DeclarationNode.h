//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclarationNode.h --
//
// Author           : Andrew Beach
// Created On       : Wed Apr  5 11:38:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr  5 11:55:00 2023
// Update Count     : 0
//

#pragma once

#include "ParseNode.h"

struct TypeData;
class InitializerNode;

struct DeclarationNode : public ParseNode {
	// These enumerations must harmonize with their names in DeclarationNode.cc.
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

	static DeclarationNode * newStorageClass( ast::Storage::Classes );
	static DeclarationNode * newFuncSpecifier( ast::Function::Specs );
	static DeclarationNode * newTypeQualifier( ast::CV::Qualifiers );
	static DeclarationNode * newBasicType( BasicType );
	static DeclarationNode * newComplexType( ComplexType );
	static DeclarationNode * newSignedNess( Signedness );
	static DeclarationNode * newLength( Length );
	static DeclarationNode * newBuiltinType( BuiltinType );
	static DeclarationNode * newForall( DeclarationNode * );
	static DeclarationNode * newFromTypedef( const std::string * );
	static DeclarationNode * newFromGlobalScope();
	static DeclarationNode * newQualifiedType( DeclarationNode *, DeclarationNode * );
	static DeclarationNode * newFunction( const std::string * name, DeclarationNode * ret, DeclarationNode * param, StatementNode * body );
	static DeclarationNode * newAggregate( ast::AggregateDecl::Aggregate kind, const std::string * name, ExpressionNode * actuals, DeclarationNode * fields, bool body );
	static DeclarationNode * newEnum( const std::string * name, DeclarationNode * constants, bool body, bool typed, DeclarationNode * base = nullptr, EnumHiding hiding = EnumHiding::Visible );
	static DeclarationNode * newEnumConstant( const std::string * name, ExpressionNode * constant );
	static DeclarationNode * newEnumValueGeneric( const std::string * name, InitializerNode * init );
	static DeclarationNode * newEnumInLine( const std::string name );
	static DeclarationNode * newName( const std::string * );
	static DeclarationNode * newFromTypeGen( const std::string *, ExpressionNode * params );
	static DeclarationNode * newTypeParam( ast::TypeDecl::Kind, const std::string * );
	static DeclarationNode * newTrait( const std::string * name, DeclarationNode * params, DeclarationNode * asserts );
	static DeclarationNode * newTraitUse( const std::string * name, ExpressionNode * params );
	static DeclarationNode * newTypeDecl( const std::string * name, DeclarationNode * typeParams );
	static DeclarationNode * newPointer( DeclarationNode * qualifiers, OperKinds kind );
	static DeclarationNode * newArray( ExpressionNode * size, DeclarationNode * qualifiers, bool isStatic );
	static DeclarationNode * newVarArray( DeclarationNode * qualifiers );
	static DeclarationNode * newBitfield( ExpressionNode * size );
	static DeclarationNode * newTuple( DeclarationNode * members );
	static DeclarationNode * newTypeof( ExpressionNode * expr, bool basetypeof = false );
	static DeclarationNode * newVtableType( DeclarationNode * expr );
	static DeclarationNode * newAttribute( const std::string *, ExpressionNode * expr = nullptr ); // gcc attributes
	static DeclarationNode * newDirectiveStmt( StatementNode * stmt ); // gcc external directive statement
	static DeclarationNode * newAsmStmt( StatementNode * stmt ); // gcc external asm statement
	static DeclarationNode * newStaticAssert( ExpressionNode * condition, ast::Expr * message );

	DeclarationNode();
	~DeclarationNode();
	DeclarationNode * clone() const override;

	DeclarationNode * addQualifiers( DeclarationNode * );
	void checkQualifiers( const TypeData *, const TypeData * );
	void checkSpecifiers( DeclarationNode * );
	DeclarationNode * copySpecifiers( DeclarationNode * );
	DeclarationNode * addType( DeclarationNode * );
	DeclarationNode * addTypedef();
	DeclarationNode * addEnumBase( DeclarationNode * );
	DeclarationNode * addAssertions( DeclarationNode * );
	DeclarationNode * addName( std::string * );
	DeclarationNode * addAsmName( DeclarationNode * );
	DeclarationNode * addBitfield( ExpressionNode * size );
	DeclarationNode * addVarArgs();
	DeclarationNode * addFunctionBody( StatementNode * body, ExpressionNode * with = nullptr );
	DeclarationNode * addOldDeclList( DeclarationNode * list );
	DeclarationNode * setBase( TypeData * newType );
	DeclarationNode * copyAttribute( DeclarationNode * attr );
	DeclarationNode * addPointer( DeclarationNode * qualifiers );
	DeclarationNode * addArray( DeclarationNode * array );
	DeclarationNode * addNewPointer( DeclarationNode * pointer );
	DeclarationNode * addNewArray( DeclarationNode * array );
	DeclarationNode * addParamList( DeclarationNode * list );
	DeclarationNode * addIdList( DeclarationNode * list ); // old-style functions
	DeclarationNode * addInitializer( InitializerNode * init );
	DeclarationNode * addTypeInitializer( DeclarationNode * init );

	DeclarationNode * cloneType( std::string * newName );
	DeclarationNode * cloneBaseType( DeclarationNode * newdecl );

	DeclarationNode * appendList( DeclarationNode * node ) {
		return (DeclarationNode *)set_last( node );
	}

	virtual void print( __attribute__((unused)) std::ostream & os, __attribute__((unused)) int indent = 0 ) const override;
	virtual void printList( __attribute__((unused)) std::ostream & os, __attribute__((unused)) int indent = 0 ) const override;

	ast::Decl * build() const;
	ast::Type * buildType() const;

	ast::Linkage::Spec get_linkage() const { return linkage; }
	DeclarationNode * extractAggregate() const;
	bool has_enumeratorValue() const { return (bool)enumeratorValue; }
	ExpressionNode * consume_enumeratorValue() const { return const_cast<DeclarationNode *>(this)->enumeratorValue.release(); }

	bool get_extension() const { return extension; }
	DeclarationNode * set_extension( bool exten ) { extension = exten; return this; }

	bool get_inLine() const { return inLine; }
	DeclarationNode * set_inLine( bool inL ) { inLine = inL; return this; }

	DeclarationNode * get_last() { return (DeclarationNode *)ParseNode::get_last(); }

	struct Variable_t {
//		const std::string * name;
		ast::TypeDecl::Kind tyClass;
		DeclarationNode * assertions;
		DeclarationNode * initializer;
	};
	Variable_t variable;

	struct StaticAssert_t {
		ExpressionNode * condition;
		ast::Expr * message;
	};
	StaticAssert_t assert;

	BuiltinType builtin = NoBuiltinType;

	TypeData * type = nullptr;

	bool inLine = false;
	bool enumInLine = false;
	ast::Function::Specs funcSpecs;
	ast::Storage::Classes storageClasses;

	ExpressionNode * bitfieldWidth = nullptr;
	std::unique_ptr<ExpressionNode> enumeratorValue;
	bool hasEllipsis = false;
	ast::Linkage::Spec linkage;
	ast::Expr * asmName = nullptr;
	std::vector<ast::ptr<ast::Attribute>> attributes;
	InitializerNode * initializer = nullptr;
	bool extension = false;
	std::string error;
	StatementNode * asmStmt = nullptr;
	StatementNode * directiveStmt = nullptr;

	static UniqueName anonymous;
}; // DeclarationNode

ast::Type * buildType( TypeData * type );

static inline ast::Type * maybeMoveBuildType( const DeclarationNode * orig ) {
	ast::Type * ret = orig ? orig->buildType() : nullptr;
	delete orig;
	return ret;
}

// This generic buildList is here along side its overloads.
template<typename AstType, typename NodeType,
    template<typename, typename...> class Container, typename... Args>
void buildList( NodeType * firstNode,
        Container<ast::ptr<AstType>, Args...> & output ) {
    SemanticErrorException errors;
    std::back_insert_iterator<Container<ast::ptr<AstType>, Args...>> out( output );
    NodeType * cur = firstNode;

    while ( cur ) {
        try {
            if ( auto result = dynamic_cast<AstType *>( maybeBuild( cur ) ) ) {
                *out++ = result;
            } else {
                assertf(false, __PRETTY_FUNCTION__ );
                SemanticError( cur->location, "type specifier declaration in forall clause is currently unimplemented." );
            } // if
        } catch( SemanticErrorException & e ) {
            errors.append( e );
        } // try
        ParseNode * temp = cur->get_next();
        // Should not return nullptr, then it is non-homogeneous:
        cur = dynamic_cast<NodeType *>( temp );
        if ( !cur && temp ) {
            SemanticError( temp->location, "internal error, non-homogeneous nodes founds in buildList processing." );
        } // if
    } // while
    if ( ! errors.isEmpty() ) {
        throw errors;
    } // if
}

void buildList( DeclarationNode * firstNode, std::vector<ast::ptr<ast::Decl>> & outputList );
void buildList( DeclarationNode * firstNode, std::vector<ast::ptr<ast::DeclWithType>> & outputList );
void buildTypeList( const DeclarationNode * firstNode, std::vector<ast::ptr<ast::Type>> & outputList );

template<typename AstType, typename NodeType,
template<typename, typename...> class Container, typename... Args>
void buildMoveList( NodeType * firstNode,
		Container<ast::ptr<AstType>, Args...> & output ) {
	buildList<AstType, NodeType, Container, Args...>( firstNode, output );
	delete firstNode;
}
