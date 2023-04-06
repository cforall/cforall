//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ParseNode.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 13:28:16 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Apr  3 17:55:00 2023
// Update Count     : 942
//

#pragma once

#include <algorithm>               // for move
#include <cassert>                 // for assert, assertf
#include <iosfwd>                  // for ostream
#include <iterator>                // for back_insert_iterator
#include <list>                    // for list
#include <memory>                  // for unique_ptr, pointer_traits
#include <string>                  // for string

#include "AST/Expr.hpp"            // for Expr, NameExpr LogicalFlag
#include "AST/Fwd.hpp"             // for ptr, Decl, DeclWithType,
#include "AST/Stmt.hpp"            // for Stmt
#include "Common/CodeLocation.h"   // for CodeLocation
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/UniqueName.h"     // for UniqueName
#include "Common/utility.h"        // for maybeClone
#include "Parser/parserutility.h"  // for maybeBuild, maybeCopy

class Attribute;
class Declaration;
struct DeclarationNode;
class DeclarationWithType;
class Initializer;
class ExpressionNode;
struct StatementNode;

//##############################################################################

typedef CodeLocation YYLTYPE;
#define YYLTYPE_IS_DECLARED 1 /* alert the parser that we have our own definition */

extern YYLTYPE yylloc;

class ParseNode {
  public:
	ParseNode() {};
	virtual ~ParseNode() { delete next; delete name; };
	virtual ParseNode * clone() const = 0;

	ParseNode * get_next() const { return next; }
	ParseNode * set_next( ParseNode * newlink ) { next = newlink; return this; }

	ParseNode * get_last() {
		ParseNode * current;
		for ( current = this; current->get_next() != nullptr; current = current->get_next() );
		return current;
	}
	ParseNode * set_last( ParseNode * newlast ) {
		if ( newlast != nullptr ) get_last()->set_next( newlast );
		return this;
	}

	virtual void print( __attribute__((unused)) std::ostream & os, __attribute__((unused)) int indent = 0 ) const {}
	virtual void printList( std::ostream & os, int indent = 0 ) const {
		print( os, indent );
		if ( next ) next->print( os, indent );
	}

	static int indent_by;

	ParseNode * next = nullptr;
	const std::string * name = nullptr;
	CodeLocation location = yylloc;
}; // ParseNode

//##############################################################################

class InitializerNode : public ParseNode {
  public:
	InitializerNode( ExpressionNode *, bool aggrp = false, ExpressionNode * des = nullptr );
	InitializerNode( InitializerNode *, bool aggrp = false, ExpressionNode * des = nullptr );
	InitializerNode( bool isDelete );
	~InitializerNode();
	virtual InitializerNode * clone() const { assert( false ); return nullptr; }

	ExpressionNode * get_expression() const { return expr; }

	InitializerNode * set_designators( ExpressionNode * des ) { designator = des; return this; }
	ExpressionNode * get_designators() const { return designator; }

	InitializerNode * set_maybeConstructed( bool value ) { maybeConstructed = value; return this; }
	bool get_maybeConstructed() const { return maybeConstructed; }

	bool get_isDelete() const { return isDelete; }

	InitializerNode * next_init() const { return kids; }

	void print( std::ostream & os, int indent = 0 ) const;
	void printOneLine( std::ostream & ) const;

	virtual ast::Init * build() const;
  private:
	ExpressionNode * expr;
	bool aggregate;
	ExpressionNode * designator;						// may be list
	InitializerNode * kids;
	bool maybeConstructed;
	bool isDelete;
}; // InitializerNode

//##############################################################################

class ExpressionNode final : public ParseNode {
  public:
	ExpressionNode( ast::Expr * expr = nullptr ) : expr( expr ) {}
	virtual ~ExpressionNode() {}
	virtual ExpressionNode * clone() const override {
		if ( nullptr == expr ) return nullptr;
		return static_cast<ExpressionNode*>(
			(new ExpressionNode( ast::shallowCopy( expr.get() ) ))->set_next( maybeCopy( get_next() ) ));
	}

	bool get_extension() const { return extension; }
	ExpressionNode * set_extension( bool exten ) { extension = exten; return this; }

	virtual void print( std::ostream & os, __attribute__((unused)) int indent = 0 ) const override {
		os << expr.get();
	}
	void printOneLine( __attribute__((unused)) std::ostream & os, __attribute__((unused)) int indent = 0 ) const {}

	template<typename T>
	bool isExpressionType() const {	return nullptr != dynamic_cast<T>(expr.get()); }

	ast::Expr * build() const {
		ast::Expr * node = const_cast<ExpressionNode *>(this)->expr.release();
		node->set_extension( this->get_extension() );
		node->location = this->location;
		return node;
	}

	// Public because of lifetime implications (what lifetime implications?)
	std::unique_ptr<ast::Expr> expr;
  private:
	bool extension = false;
}; // ExpressionNode

// Must harmonize with OperName.
enum class OperKinds {
	// diadic
	SizeOf, AlignOf, OffsetOf, Plus, Minus, Exp, Mul, Div, Mod, Or, And,
	BitOr, BitAnd, Xor, Cast, LShift, RShift, LThan, GThan, LEThan, GEThan, Eq, Neq,
	Assign, AtAssn, ExpAssn, MulAssn, DivAssn, ModAssn, PlusAssn, MinusAssn, LSAssn, RSAssn, AndAssn, ERAssn, OrAssn,
	Index, Range,
	// monadic
	UnPlus, UnMinus, AddressOf, PointTo, Neg, BitNeg, Incr, IncrPost, Decr, DecrPost,
	Ctor, Dtor,
}; // OperKinds

enum class EnumHiding { Visible, Hide };

struct LabelNode {
	std::vector<ast::Label> labels;
};

// These 4 routines modify the string:
ast::Expr * build_constantInteger( const CodeLocation &, std::string & );
ast::Expr * build_constantFloat( const CodeLocation &, std::string & );
ast::Expr * build_constantChar( const CodeLocation &, std::string & );
ast::Expr * build_constantStr( const CodeLocation &, std::string & );
ast::Expr * build_field_name_FLOATING_FRACTIONconstant( const CodeLocation &, const std::string & str );
ast::Expr * build_field_name_FLOATING_DECIMALconstant( const CodeLocation &, const std::string & str );
ast::Expr * build_field_name_FLOATINGconstant( const CodeLocation &, const std::string & str );
ast::Expr * build_field_name_fraction_constants( const CodeLocation &, ast::Expr * fieldName, ExpressionNode * fracts );

ast::NameExpr * build_varref( const CodeLocation &, const std::string * name );
ast::QualifiedNameExpr * build_qualified_expr( const CodeLocation &, const DeclarationNode * decl_node, const ast::NameExpr * name );
ast::QualifiedNameExpr * build_qualified_expr( const CodeLocation &, const ast::EnumDecl * decl, const ast::NameExpr * name );
ast::DimensionExpr * build_dimensionref( const CodeLocation &, const std::string * name );

ast::Expr * build_cast( const CodeLocation &, DeclarationNode * decl_node, ExpressionNode * expr_node );
ast::Expr * build_keyword_cast( const CodeLocation &, ast::AggregateDecl::Aggregate target, ExpressionNode * expr_node );
ast::Expr * build_virtual_cast( const CodeLocation &, DeclarationNode * decl_node, ExpressionNode * expr_node );
ast::Expr * build_fieldSel( const CodeLocation &, ExpressionNode * expr_node, ast::Expr * member );
ast::Expr * build_pfieldSel( const CodeLocation &, ExpressionNode * expr_node, ast::Expr * member );
ast::Expr * build_offsetOf( const CodeLocation &, DeclarationNode * decl_node, ast::NameExpr * member );
ast::Expr * build_and( const CodeLocation &, ExpressionNode * expr_node1, ExpressionNode * expr_node2 );
ast::Expr * build_and_or( const CodeLocation &, ExpressionNode * expr_node1, ExpressionNode * expr_node2, ast::LogicalFlag flag );
ast::Expr * build_unary_val( const CodeLocation &, OperKinds op, ExpressionNode * expr_node );
ast::Expr * build_binary_val( const CodeLocation &, OperKinds op, ExpressionNode * expr_node1, ExpressionNode * expr_node2 );
ast::Expr * build_binary_ptr( const CodeLocation &, OperKinds op, ExpressionNode * expr_node1, ExpressionNode * expr_node2 );
ast::Expr * build_cond( const CodeLocation &, ExpressionNode * expr_node1, ExpressionNode * expr_node2, ExpressionNode * expr_node3 );
ast::Expr * build_tuple( const CodeLocation &, ExpressionNode * expr_node = nullptr );
ast::Expr * build_func( const CodeLocation &, ExpressionNode * function, ExpressionNode * expr_node );
ast::Expr * build_compoundLiteral( const CodeLocation &, DeclarationNode * decl_node, InitializerNode * kids );

//##############################################################################

struct TypeData;

struct DeclarationNode : public ParseNode {
	// These enumerations must harmonize with their names in DeclarationNode.cc.
	enum BasicType {
		Void, Bool, Char, Int, Int128,
		Float, Double, LongDouble, uuFloat80, uuFloat128,
		uFloat16, uFloat32, uFloat32x, uFloat64, uFloat64x, uFloat128, uFloat128x,
		NoBasicType
	};
	static const char * basicTypeNames[];
	enum ComplexType { Complex, NoComplexType, Imaginary };	// Imaginary unsupported => parse, but make invisible and print error message
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

//##############################################################################

struct StatementNode final : public ParseNode {
	StatementNode() :
		stmt( nullptr ), clause( nullptr ) {}
	StatementNode( ast::Stmt * stmt ) :
		stmt( stmt ), clause( nullptr ) {}
	StatementNode( ast::StmtClause * clause ) :
		stmt( nullptr ), clause( clause ) {}
	StatementNode( DeclarationNode * decl );
	virtual ~StatementNode() {}

	virtual StatementNode * clone() const final { assert( false ); return nullptr; }
	ast::Stmt * build() const { return const_cast<StatementNode *>(this)->stmt.release(); }

	virtual StatementNode * add_label(
			const CodeLocation & location,
			const std::string * name,
			DeclarationNode * attr = nullptr ) {
		stmt->labels.emplace_back( location,
			*name,
			attr ? std::move( attr->attributes )
				: std::vector<ast::ptr<ast::Attribute>>{} );
		delete attr;
		delete name;
		return this;
	}

	virtual StatementNode * append_last_case( StatementNode * );

	virtual void print( std::ostream & os, __attribute__((unused)) int indent = 0 ) const override {
		os << stmt.get() << std::endl;
	}

	std::unique_ptr<ast::Stmt> stmt;
	std::unique_ptr<ast::StmtClause> clause;
}; // StatementNode

ast::Stmt * build_expr( CodeLocation const &, ExpressionNode * ctl );

struct CondCtl {
	CondCtl( DeclarationNode * decl, ExpressionNode * condition ) :
		init( decl ? new StatementNode( decl ) : nullptr ), condition( condition ) {}

	StatementNode * init;
	ExpressionNode * condition;
};

struct ForCtrl {
	ForCtrl( StatementNode * stmt, ExpressionNode * condition, ExpressionNode * change ) :
		init( stmt ), condition( condition ), change( change ) {}

	StatementNode * init;
	ExpressionNode * condition;
	ExpressionNode * change;
};

ast::Stmt * build_if( const CodeLocation &, CondCtl * ctl, StatementNode * then, StatementNode * else_ );
ast::Stmt * build_switch( const CodeLocation &, bool isSwitch, ExpressionNode * ctl, StatementNode * stmt );
ast::CaseClause * build_case( ExpressionNode * ctl );
ast::CaseClause * build_default( const CodeLocation & );
ast::Stmt * build_while( const CodeLocation &, CondCtl * ctl, StatementNode * stmt, StatementNode * else_ = nullptr );
ast::Stmt * build_do_while( const CodeLocation &, ExpressionNode * ctl, StatementNode * stmt, StatementNode * else_ = nullptr );
ast::Stmt * build_for( const CodeLocation &, ForCtrl * forctl, StatementNode * stmt, StatementNode * else_ = nullptr );
ast::Stmt * build_branch( const CodeLocation &, ast::BranchStmt::Kind kind );
ast::Stmt * build_branch( const CodeLocation &, std::string * identifier, ast::BranchStmt::Kind kind );
ast::Stmt * build_computedgoto( ExpressionNode * ctl );
ast::Stmt * build_return( const CodeLocation &, ExpressionNode * ctl );
ast::Stmt * build_throw( const CodeLocation &, ExpressionNode * ctl );
ast::Stmt * build_resume( const CodeLocation &, ExpressionNode * ctl );
ast::Stmt * build_resume_at( ExpressionNode * ctl , ExpressionNode * target );
ast::Stmt * build_try( const CodeLocation &, StatementNode * try_, StatementNode * catch_, StatementNode * finally_ );
ast::CatchClause * build_catch( const CodeLocation &, ast::ExceptionKind kind, DeclarationNode * decl, ExpressionNode * cond, StatementNode * body );
ast::FinallyClause * build_finally( const CodeLocation &, StatementNode * stmt );
ast::Stmt * build_compound( const CodeLocation &, StatementNode * first );
StatementNode * maybe_build_compound( const CodeLocation &, StatementNode * first );
ast::Stmt * build_asm( const CodeLocation &, bool voltile, ast::Expr * instruction, ExpressionNode * output = nullptr, ExpressionNode * input = nullptr, ExpressionNode * clobber = nullptr, LabelNode * gotolabels = nullptr );
ast::Stmt * build_directive( const CodeLocation &, std::string * directive );
ast::SuspendStmt * build_suspend( const CodeLocation &, StatementNode *, ast::SuspendStmt::Kind );
ast::WaitForStmt * build_waitfor( const CodeLocation &, ast::WaitForStmt * existing, ExpressionNode * when, ExpressionNode * targetExpr, StatementNode * stmt );
ast::WaitForStmt * build_waitfor_else( const CodeLocation &, ast::WaitForStmt * existing, ExpressionNode * when, StatementNode * stmt );
ast::WaitForStmt * build_waitfor_timeout( const CodeLocation &, ast::WaitForStmt * existing, ExpressionNode * when, ExpressionNode * timeout, StatementNode * stmt );
ast::Stmt * build_with( const CodeLocation &, ExpressionNode * exprs, StatementNode * stmt );
ast::Stmt * build_mutex( const CodeLocation &, ExpressionNode * exprs, StatementNode * stmt );

//##############################################################################

template<typename AstType, typename NodeType,
	template<typename, typename...> class Container, typename... Args>
void buildList( const NodeType * firstNode,
		Container<ast::ptr<AstType>, Args...> & output ) {
	SemanticErrorException errors;
	std::back_insert_iterator<Container<ast::ptr<AstType>, Args...>> out( output );
	const NodeType * cur = firstNode;

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
		const ParseNode * temp = cur->get_next();
		// Should not return nullptr, then it is non-homogeneous:
		cur = dynamic_cast<const NodeType *>( temp );
		if ( !cur && temp ) {
			SemanticError( temp->location, "internal error, non-homogeneous nodes founds in buildList processing." );
		} // if
	} // while
	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
}

// in DeclarationNode.cc
void buildList( const DeclarationNode * firstNode, std::vector<ast::ptr<ast::Decl>> & outputList );
void buildList( const DeclarationNode * firstNode, std::vector<ast::ptr<ast::DeclWithType>> & outputList );
void buildTypeList( const DeclarationNode * firstNode, std::vector<ast::ptr<ast::Type>> & outputList );

template<typename AstType, typename NodeType,
	template<typename, typename...> class Container, typename... Args>
void buildMoveList( const NodeType * firstNode,
		Container<ast::ptr<AstType>, Args...> & output ) {
	buildList<AstType, NodeType, Container, Args...>( firstNode, output );
	delete firstNode;
}

// in ParseNode.cc
std::ostream & operator<<( std::ostream & out, const ParseNode * node );

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
