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
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Mar 12 15:19:04 2021
// Update Count     : 897
//

#pragma once

#include <algorithm>               // for move
#include <cassert>                 // for assert, assertf
#include <iosfwd>                  // for ostream
#include <iterator>                // for back_insert_iterator
#include <list>                    // for list
#include <memory>                  // for unique_ptr, pointer_traits
#include <string>                  // for string

#include "Common/CodeLocation.h"   // for CodeLocation
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/UniqueName.h"     // for UniqueName
#include "Common/utility.h"        // for maybeClone, maybeBuild
#include "SynTree/LinkageSpec.h"   // for Spec
#include "SynTree/Declaration.h"   // for Aggregate
#include "SynTree/Expression.h"    // for Expression, ConstantExpr (ptr only)
#include "SynTree/Label.h"         // for Label
#include "SynTree/Statement.h"     // for Statement, BranchStmt, BranchStmt:...
#include "SynTree/Type.h"          // for Type, Type::FuncSpecifiers, Type::...

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

	virtual Initializer * build() const;
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
	ExpressionNode( Expression * expr = nullptr ) : expr( expr ) {}
	virtual ~ExpressionNode() {}
	virtual ExpressionNode * clone() const override { return expr ? static_cast<ExpressionNode*>((new ExpressionNode( expr->clone() ))->set_next( maybeClone( get_next() ) )) : nullptr; }

	bool get_extension() const { return extension; }
	ExpressionNode * set_extension( bool exten ) { extension = exten; return this; }

	virtual void print( std::ostream & os, __attribute__((unused)) int indent = 0 ) const override {
		os << expr.get();
	}
	void printOneLine( __attribute__((unused)) std::ostream & os, __attribute__((unused)) int indent = 0 ) const {}

	template<typename T>
	bool isExpressionType() const {	return nullptr != dynamic_cast<T>(expr.get()); }

	Expression * build() const { return const_cast<ExpressionNode *>(this)->expr.release(); }

	std::unique_ptr<Expression> expr;					// public because of lifetime implications
  private:
	bool extension = false;
}; // ExpressionNode

template< typename T >
struct maybeBuild_t< Expression, T > {
	static inline Expression * doit( const T * orig ) {
		if ( orig ) {
			Expression * p = orig->build();
			p->set_extension( orig->get_extension() );
			p->location = orig->location;
			return p;
		} else {
			return nullptr;
		} // if
	}
};

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

struct LabelNode {
	std::list< Label > labels;
};

Expression * build_constantInteger( std::string & str ); // these 4 routines modify the string
Expression * build_constantFloat( std::string & str );
Expression * build_constantChar( std::string & str );
Expression * build_constantStr( std::string & str );
Expression * build_field_name_FLOATING_FRACTIONconstant( const std::string & str );
Expression * build_field_name_FLOATING_DECIMALconstant( const std::string & str );
Expression * build_field_name_FLOATINGconstant( const std::string & str );
Expression * build_field_name_fraction_constants( Expression * fieldName, ExpressionNode * fracts );

NameExpr * build_varref( const std::string * name );

Expression * build_cast( DeclarationNode * decl_node, ExpressionNode * expr_node );
Expression * build_keyword_cast( AggregateDecl::Aggregate target, ExpressionNode * expr_node );
Expression * build_virtual_cast( DeclarationNode * decl_node, ExpressionNode * expr_node );
Expression * build_fieldSel( ExpressionNode * expr_node, Expression * member );
Expression * build_pfieldSel( ExpressionNode * expr_node, Expression * member );
Expression * build_offsetOf( DeclarationNode * decl_node, NameExpr * member );
Expression * build_and( ExpressionNode * expr_node1, ExpressionNode * expr_node2 );
Expression * build_and_or( ExpressionNode * expr_node1, ExpressionNode * expr_node2, bool kind );
Expression * build_unary_val( OperKinds op, ExpressionNode * expr_node );
Expression * build_unary_ptr( OperKinds op, ExpressionNode * expr_node );
Expression * build_binary_val( OperKinds op, ExpressionNode * expr_node1, ExpressionNode * expr_node2 );
Expression * build_binary_ptr( OperKinds op, ExpressionNode * expr_node1, ExpressionNode * expr_node2 );
Expression * build_cond( ExpressionNode * expr_node1, ExpressionNode * expr_node2, ExpressionNode * expr_node3 );
Expression * build_tuple( ExpressionNode * expr_node = nullptr );
Expression * build_func( ExpressionNode * function, ExpressionNode * expr_node );
Expression * build_compoundLiteral( DeclarationNode * decl_node, InitializerNode * kids );

//##############################################################################

struct TypeData;

struct DeclarationNode : public ParseNode {
	// These enumerations must harmonize with their names in DeclarationNode.cc.
	enum BasicType { Void, Bool, Char, Int, Int128,
					 Float, Double, LongDouble, uuFloat80, uuFloat128,
					 uFloat16, uFloat32, uFloat32x, uFloat64, uFloat64x, uFloat128, uFloat128x, NoBasicType };
	static const char * basicTypeNames[];
	enum ComplexType { Complex, NoComplexType, Imaginary };	// Imaginary unsupported => parse, but make invisible and print error message
	static const char * complexTypeNames[];
	enum Signedness { Signed, Unsigned, NoSignedness };
	static const char * signednessNames[];
	enum Length { Short, Long, LongLong, NoLength };
	static const char * lengthNames[];
	enum BuiltinType { Valist, AutoType, Zero, One, NoBuiltinType };
	static const char * builtinTypeNames[];

	static DeclarationNode * newStorageClass( Type::StorageClasses );
	static DeclarationNode * newFuncSpecifier( Type::FuncSpecifiers );
	static DeclarationNode * newTypeQualifier( Type::Qualifiers );
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
	static DeclarationNode * newAggregate( AggregateDecl::Aggregate kind, const std::string * name, ExpressionNode * actuals, DeclarationNode * fields, bool body );
	static DeclarationNode * newEnum( const std::string * name, DeclarationNode * constants, bool body );
	static DeclarationNode * newEnumConstant( const std::string * name, ExpressionNode * constant );
	static DeclarationNode * newName( const std::string * );
	static DeclarationNode * newFromTypeGen( const std::string *, ExpressionNode * params );
	static DeclarationNode * newTypeParam( TypeDecl::Kind, const std::string * );
	static DeclarationNode * newTrait( const std::string * name, DeclarationNode * params, DeclarationNode * asserts );
	static DeclarationNode * newTraitUse( const std::string * name, ExpressionNode * params );
	static DeclarationNode * newTypeDecl( const std::string * name, DeclarationNode * typeParams );
	static DeclarationNode * newPointer( DeclarationNode * qualifiers, OperKinds kind );
	static DeclarationNode * newArray( ExpressionNode * size, DeclarationNode * qualifiers, bool isStatic );
	static DeclarationNode * newVarArray( DeclarationNode * qualifiers );
	static DeclarationNode * newBitfield( ExpressionNode * size );
	static DeclarationNode * newTuple( DeclarationNode * members );
	static DeclarationNode * newTypeof( ExpressionNode * expr, bool basetypeof = false );
	static DeclarationNode * newAttribute( const std::string *, ExpressionNode * expr = nullptr ); // gcc attributes
	static DeclarationNode * newDirectiveStmt( StatementNode * stmt ); // gcc external directive statement
	static DeclarationNode * newAsmStmt( StatementNode * stmt ); // gcc external asm statement
	static DeclarationNode * newStaticAssert( ExpressionNode * condition, Expression * message );

	DeclarationNode();
	~DeclarationNode();
	DeclarationNode * clone() const override;

	DeclarationNode * addQualifiers( DeclarationNode * );
	void checkQualifiers( const TypeData *, const TypeData * );
	void checkSpecifiers( DeclarationNode * );
	DeclarationNode * copySpecifiers( DeclarationNode * );
	DeclarationNode * addType( DeclarationNode * );
	DeclarationNode * addTypedef();
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

	Declaration * build() const;
	Type * buildType() const;

	LinkageSpec::Spec get_linkage() const { return linkage; }
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
		TypeDecl::Kind tyClass;
		DeclarationNode * assertions;
		DeclarationNode * initializer;
	};
	Variable_t variable;

	struct Attr_t {
//		const std::string * name;
		ExpressionNode * expr;
		DeclarationNode * type;
	};
	Attr_t attr;

	struct StaticAssert_t {
		ExpressionNode * condition;
		Expression * message;
	};
	StaticAssert_t assert;

	BuiltinType builtin = NoBuiltinType;

	TypeData * type = nullptr;

	bool inLine = false;
	Type::FuncSpecifiers funcSpecs;
	Type::StorageClasses storageClasses;

	ExpressionNode * bitfieldWidth = nullptr;
	std::unique_ptr<ExpressionNode> enumeratorValue;
	bool hasEllipsis = false;
	LinkageSpec::Spec linkage;
	Expression * asmName = nullptr;
	std::list< Attribute * > attributes;
	InitializerNode * initializer = nullptr;
	bool extension = false;
	std::string error;
	StatementNode * asmStmt = nullptr;
	StatementNode * directiveStmt = nullptr;

	static UniqueName anonymous;
}; // DeclarationNode

Type * buildType( TypeData * type );

static inline Type * maybeMoveBuildType( const DeclarationNode * orig ) {
	Type * ret = orig ? orig->buildType() : nullptr;
	delete orig;
	return ret;
}

//##############################################################################

struct StatementNode final : public ParseNode {
	StatementNode() { stmt = nullptr; }
	StatementNode( Statement * stmt ) : stmt( stmt ) {}
	StatementNode( DeclarationNode * decl );
	virtual ~StatementNode() {}

	virtual StatementNode * clone() const final { assert( false ); return nullptr; }
	Statement * build() const { return const_cast<StatementNode *>(this)->stmt.release(); }

	virtual StatementNode * add_label( const std::string * name, DeclarationNode * attr = nullptr ) {
		stmt->get_labels().emplace_back( * name, nullptr, attr ? std::move( attr->attributes ) : std::list< Attribute * > {} );
		delete attr;
		delete name;
		return this;
	}

	virtual StatementNode * append_last_case( StatementNode * );

	virtual void print( std::ostream & os, __attribute__((unused)) int indent = 0 ) const override {
		os << stmt.get() << std::endl;
	}

	std::unique_ptr<Statement> stmt;
}; // StatementNode

Statement * build_expr( ExpressionNode * ctl );

struct IfCtrl {
	IfCtrl( DeclarationNode * decl, ExpressionNode * condition ) :
		init( decl ? new StatementNode( decl ) : nullptr ), condition( condition ) {}

	StatementNode * init;
	ExpressionNode * condition;
};

struct ForCtrl {
	ForCtrl( ExpressionNode * expr, ExpressionNode * condition, ExpressionNode * change ) :
		init( new StatementNode( build_expr( expr ) ) ), condition( condition ), change( change ) {}
	ForCtrl( DeclarationNode * decl, ExpressionNode * condition, ExpressionNode * change ) :
		init( new StatementNode( decl ) ), condition( condition ), change( change ) {}

	StatementNode * init;
	ExpressionNode * condition;
	ExpressionNode * change;
};

Expression * build_if_control( IfCtrl * ctl, std::list< Statement * > & init );
Statement * build_if( IfCtrl * ctl, StatementNode * then_stmt, StatementNode * else_stmt );
Statement * build_switch( bool isSwitch, ExpressionNode * ctl, StatementNode * stmt );
Statement * build_case( ExpressionNode * ctl );
Statement * build_default();
Statement * build_while( IfCtrl * ctl, StatementNode * stmt );
Statement * build_do_while( ExpressionNode * ctl, StatementNode * stmt );
Statement * build_for( ForCtrl * forctl, StatementNode * stmt );
Statement * build_branch( BranchStmt::Type kind );
Statement * build_branch( std::string * identifier, BranchStmt::Type kind );
Statement * build_computedgoto( ExpressionNode * ctl );
Statement * build_return( ExpressionNode * ctl );
Statement * build_throw( ExpressionNode * ctl );
Statement * build_resume( ExpressionNode * ctl );
Statement * build_resume_at( ExpressionNode * ctl , ExpressionNode * target );
Statement * build_try( StatementNode * try_stmt, StatementNode * catch_stmt, StatementNode * finally_stmt );
Statement * build_catch( CatchStmt::Kind kind, DeclarationNode *decl, ExpressionNode *cond, StatementNode *body );
Statement * build_finally( StatementNode * stmt );
Statement * build_compound( StatementNode * first );
StatementNode * maybe_build_compound( StatementNode * first );
Statement * build_asm( bool voltile, Expression * instruction, ExpressionNode * output = nullptr, ExpressionNode * input = nullptr, ExpressionNode * clobber = nullptr, LabelNode * gotolabels = nullptr );
Statement * build_directive( std::string * directive );
SuspendStmt * build_suspend( StatementNode *, SuspendStmt::Type = SuspendStmt::None);
WaitForStmt * build_waitfor( ExpressionNode * target, StatementNode * stmt, ExpressionNode * when );
WaitForStmt * build_waitfor( ExpressionNode * target, StatementNode * stmt, ExpressionNode * when, WaitForStmt * existing );
WaitForStmt * build_waitfor_timeout( ExpressionNode * timeout, StatementNode * stmt, ExpressionNode * when );
WaitForStmt * build_waitfor_timeout( ExpressionNode * timeout, StatementNode * stmt, ExpressionNode * when, StatementNode * else_stmt, ExpressionNode * else_when );
Statement * build_with( ExpressionNode * exprs, StatementNode * stmt );

//##############################################################################

template< typename SynTreeType, typename NodeType, template< typename, typename...> class Container, typename... Args >
void buildList( const NodeType * firstNode, Container< SynTreeType *, Args... > & outputList ) {
	SemanticErrorException errors;
	std::back_insert_iterator< Container< SynTreeType *, Args... > > out( outputList );
	const NodeType * cur = firstNode;

	while ( cur ) {
		try {
			SynTreeType * result = dynamic_cast< SynTreeType * >( maybeBuild< typename std::pointer_traits< decltype(cur->build())>::element_type >( cur ) );
			if ( result ) {
				result->location = cur->location;
				* out++ = result;
			} else {
				SemanticError( cur->location, "type specifier declaration in forall clause is currently unimplemented." );
			} // if
		} catch( SemanticErrorException & e ) {
			errors.append( e );
		} // try
		cur = dynamic_cast< NodeType * >( cur->get_next() );
	} // while
	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
}

// in DeclarationNode.cc
void buildList( const DeclarationNode * firstNode, std::list< Declaration * > & outputList );
void buildList( const DeclarationNode * firstNode, std::list< DeclarationWithType * > & outputList );
void buildTypeList( const DeclarationNode * firstNode, std::list< Type * > & outputList );

template< typename SynTreeType, typename NodeType >
void buildMoveList( const NodeType * firstNode, std::list< SynTreeType * > & outputList ) {
	buildList( firstNode, outputList );
	delete firstNode;
}

// in ParseNode.cc
std::ostream & operator<<( std::ostream & out, const ParseNode * node );

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
