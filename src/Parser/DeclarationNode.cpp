//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclarationNode.cpp --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 12:34:05 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Feb 23 18:25:57 2024
// Update Count     : 1533
//

#include "DeclarationNode.hpp"

#include <cassert>                     // for assert, assertf, strict_dynami...
#include <iterator>                    // for back_insert_iterator
#include <list>                        // for list
#include <memory>                      // for unique_ptr
#include <ostream>                     // for operator<<, ostream, basic_ost...
#include <string>                      // for string, operator+, allocator, ...

#include "AST/Attribute.hpp"           // for Attribute
#include "AST/Copy.hpp"                // for shallowCopy
#include "AST/Decl.hpp"                // for Decl
#include "AST/Expr.hpp"                // for Expr
#include "AST/Print.hpp"               // for print
#include "AST/Stmt.hpp"                // for AsmStmt, DirectiveStmt
#include "AST/StorageClasses.hpp"      // for Storage::Class
#include "AST/Type.hpp"                // for Type
#include "Common/CodeLocation.hpp"     // for CodeLocation
#include "Common/Iterate.hpp"          // for reverseIterate
#include "Common/SemanticError.hpp"    // for SemanticError
#include "Common/UniqueName.hpp"       // for UniqueName
#include "Common/Utility.hpp"          // for copy, spliceBegin
#include "Parser/ExpressionNode.hpp"   // for ExpressionNode
#include "Parser/InitializerNode.hpp"  // for InitializerNode
#include "Parser/StatementNode.hpp"    // for StatementNode
#include "TypeData.hpp"                // for TypeData, TypeData::Aggregate_t
#include "TypedefTable.hpp"            // for TypedefTable

extern TypedefTable typedefTable;

using namespace std;

UniqueName DeclarationNode::anonymous( "__anonymous" );

extern ast::Linkage::Spec linkage;						// defined in parser.yy

DeclarationNode::DeclarationNode() :
	linkage( ::linkage ) {

	variable.tyClass = ast::TypeDecl::NUMBER_OF_KINDS;
	variable.assertions = nullptr;
	variable.initializer = nullptr;

	assert.condition = nullptr;
	assert.message = nullptr;
}

DeclarationNode::~DeclarationNode() {
	delete name;

	delete variable.assertions;
	delete variable.initializer;

	delete type;
	delete bitfieldWidth;

	delete asmStmt;
	// asmName, no delete, passed to next stage
	delete initializer;

	delete assert.condition;
	delete assert.message;
}

DeclarationNode * DeclarationNode::clone() const {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->next = maybeCopy( next );
	newnode->name = name ? new string( *name ) : nullptr;

	newnode->type = maybeCopy( type );
	newnode->inLine = inLine;
	newnode->storageClasses = storageClasses;
	newnode->funcSpecs = funcSpecs;
	newnode->bitfieldWidth = maybeCopy( bitfieldWidth );
	newnode->enumeratorValue.reset( maybeCopy( enumeratorValue.get() ) );
	newnode->hasEllipsis = hasEllipsis;
	newnode->linkage = linkage;
	newnode->asmName = maybeCopy( asmName );
	newnode->attributes = attributes;
	newnode->initializer = maybeCopy( initializer );
	newnode->extension = extension;
	newnode->asmStmt = maybeCopy( asmStmt );
	newnode->error = error;

	newnode->variable.tyClass = variable.tyClass;
	newnode->variable.assertions = maybeCopy( variable.assertions );
	newnode->variable.initializer = maybeCopy( variable.initializer );

	newnode->assert.condition = maybeCopy( assert.condition );
	newnode->assert.message = maybeCopy( assert.message );
	return newnode;
} // DeclarationNode::clone

void DeclarationNode::print( std::ostream & os, int indent ) const {
	os << string( indent, ' ' );
	if ( name ) {
		os << *name << ": ";
	} // if

	if ( linkage != ast::Linkage::Cforall ) {
		os << ast::Linkage::name( linkage ) << " ";
	} // if

	ast::print( os, storageClasses );
	ast::print( os, funcSpecs );

	if ( type ) {
		type->print( os, indent );
	} else {
		os << "untyped entity ";
	} // if

	if ( bitfieldWidth ) {
		os << endl << string( indent + 2, ' ' ) << "with bitfield width ";
		bitfieldWidth->printOneLine( os );
	} // if

	if ( initializer ) {
		os << endl << string( indent + 2, ' ' ) << "with initializer ";
		initializer->printOneLine( os );
		os << " maybe constructed? " << initializer->get_maybeConstructed();
	} // if

	if ( ! attributes.empty() ) {
		os << string( indent + 2, ' ' ) << "with attributes" << endl;
		for ( ast::ptr<ast::Attribute> const & attr : reverseIterate( attributes ) ) {
			os << string( indent + 4, ' ' );
			ast::print( os, attr, indent + 2 );
		} // for
	} // if

	os << endl;
}

void DeclarationNode::printList( std::ostream & os, int indent ) const {
	ParseList::printList( os, indent );
	if ( hasEllipsis ) {
		os << string( indent, ' ' )  << "and a variable number of other arguments" << endl;
	} // if
}

DeclarationNode * DeclarationNode::newFromTypeData( TypeData * type ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = type;
	return newnode;
} // DeclarationNode::newFromTypeData

DeclarationNode * DeclarationNode::newStorageClass( ast::Storage::Classes sc ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->storageClasses = sc;
	return newnode;
} // DeclarationNode::newStorageClass

DeclarationNode * DeclarationNode::newFuncSpecifier( ast::Function::Specs fs ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->funcSpecs = fs;
	return newnode;
} // DeclarationNode::newFuncSpecifier

DeclarationNode * DeclarationNode::newAggregate( ast::AggregateDecl::Aggregate kind, const string * name, ExpressionNode * actuals, DeclarationNode * fields, bool body ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Aggregate );
	newnode->type->aggregate.kind = kind;
	newnode->type->aggregate.anon = name == nullptr;
	newnode->type->aggregate.name = newnode->type->aggregate.anon ? new string( DeclarationNode::anonymous.newName() ) : name;
	newnode->type->aggregate.actuals = actuals;
	newnode->type->aggregate.fields = fields;
	newnode->type->aggregate.body = body;
	return newnode;
} // DeclarationNode::newAggregate

DeclarationNode * DeclarationNode::newEnum( const string * name, DeclarationNode * constants, bool body, bool isCfa, DeclarationNode * base, EnumHiding hiding ) {
	DeclarationNode * newnode = newAggregate( ast::AggregateDecl::Enum, name, nullptr, constants, body );
	newnode->type->aggregate.isCfa = isCfa;
	newnode->type->aggregate.hiding = hiding;
	if ( base ) {
		assert( isCfa );
		assert( base->type );
		newnode->type->base = base->type;
		base->type = nullptr;
		delete base;
	} // if

	return newnode;
} // DeclarationNode::newEnum

DeclarationNode * DeclarationNode::newName( const string * name ) {
	DeclarationNode * newnode = new DeclarationNode;
	assert( ! newnode->name );
	newnode->name = name;
	return newnode;
} // DeclarationNode::newName

DeclarationNode * DeclarationNode::newEnumConstant( const string * name, ExpressionNode * constant ) {
	DeclarationNode * newnode = newName( name );
	newnode->enumeratorValue.reset( constant );
	return newnode;
} // DeclarationNode::newEnumConstant

DeclarationNode * DeclarationNode::newEnumValueGeneric( const string * name, InitializerNode * init ) {
	if ( nullptr == init ) {
		return newName( name );
	} else if ( init->get_expression() ) {
		return newEnumConstant( name, init->get_expression() );
	} else {
		DeclarationNode * newnode = newName( name );
		newnode->initializer = init;
		return newnode;
	} // if
} // DeclarationNode::newEnumValueGeneric

DeclarationNode * DeclarationNode::newEnumInLine( const std::string * name ) {
	DeclarationNode * newnode = newName( name );
	newnode->enumInLine = true;
	return newnode;
}

DeclarationNode * DeclarationNode::newTypeParam( ast::TypeDecl::Kind tc, const string * name ) {
	DeclarationNode * newnode = newName( name );
	newnode->type = nullptr;
	newnode->variable.tyClass = tc;
	newnode->variable.assertions = nullptr;
	return newnode;
} // DeclarationNode::newTypeParam

DeclarationNode * DeclarationNode::newTrait( const string * name, DeclarationNode * params, DeclarationNode * asserts ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Aggregate );
	newnode->type->aggregate.name = name;
	newnode->type->aggregate.kind = ast::AggregateDecl::Trait;
	newnode->type->aggregate.params = params;
	newnode->type->aggregate.fields = asserts;
	return newnode;
} // DeclarationNode::newTrait

DeclarationNode * DeclarationNode::newTraitUse( const string * name, ExpressionNode * params ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::AggregateInst );
	newnode->type->aggInst.aggregate = new TypeData( TypeData::Aggregate );
	newnode->type->aggInst.aggregate->aggregate.kind = ast::AggregateDecl::Trait;
	newnode->type->aggInst.aggregate->aggregate.name = name;
	newnode->type->aggInst.params = params;
	return newnode;
} // DeclarationNode::newTraitUse

DeclarationNode * DeclarationNode::newTypeDecl( const string * name, DeclarationNode * typeParams ) {
	DeclarationNode * newnode = newName( name );
	newnode->type = new TypeData( TypeData::Symbolic );
	newnode->type->symbolic.isTypedef = false;
	newnode->type->symbolic.params = typeParams;
	return newnode;
} // DeclarationNode::newTypeDecl

DeclarationNode * DeclarationNode::newPointer( DeclarationNode * qualifiers, OperKinds kind ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( kind == OperKinds::PointTo ? TypeData::Pointer : TypeData::Reference );
	if ( kind == OperKinds::And ) {
		// T && is parsed as 'And' operator rather than two references => add a second reference type
		TypeData * td = new TypeData( TypeData::Reference );
		td->base = newnode->type;
		newnode->type = td;
	}
	if ( qualifiers ) {
		return newnode->addQualifiers( qualifiers );
	} else {
		return newnode;
	} // if
} // DeclarationNode::newPointer

DeclarationNode * DeclarationNode::newArray( ExpressionNode * size, DeclarationNode * qualifiers, bool isStatic ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Array );
	newnode->type->array.dimension = size;
	newnode->type->array.isStatic = isStatic;
	newnode->type->array.isVarLen = size && !size->isExpressionType<ast::ConstantExpr *>();
	return newnode->addQualifiers( qualifiers );
} // DeclarationNode::newArray

DeclarationNode * DeclarationNode::newVarArray( DeclarationNode * qualifiers ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Array );
	newnode->type->array.dimension = nullptr;
	newnode->type->array.isStatic = false;
	newnode->type->array.isVarLen = true;
	return newnode->addQualifiers( qualifiers );
}

DeclarationNode * DeclarationNode::newBitfield( ExpressionNode * size ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->bitfieldWidth = size;
	return newnode;
}

DeclarationNode * DeclarationNode::newTuple( DeclarationNode * members ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Tuple );
	newnode->type->tuple = members;
	return newnode;
}

DeclarationNode * DeclarationNode::newTypeof( ExpressionNode * expr, bool basetypeof ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( basetypeof ? TypeData::Basetypeof : TypeData::Typeof );
	newnode->type->typeexpr = expr;
	return newnode;
}

DeclarationNode * DeclarationNode::newFunction( const string * name, DeclarationNode * ret, DeclarationNode * param, StatementNode * body ) {
	DeclarationNode * newnode = newName( name );
	newnode->type = new TypeData( TypeData::Function );
	newnode->type->function.params = param;
	newnode->type->function.body = body;

	if ( ret ) {
		newnode->type->base = ret->type;
		ret->type = nullptr;
		delete ret;
	} // if

	return newnode;
} // DeclarationNode::newFunction

DeclarationNode * DeclarationNode::newAttribute( const string * name, ExpressionNode * expr ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = nullptr;
	std::vector<ast::ptr<ast::Expr>> exprs;
	buildList( expr, exprs );
	newnode->attributes.push_back( new ast::Attribute( *name, std::move( exprs ) ) );
	delete name;
	return newnode;
}

DeclarationNode * DeclarationNode::newDirectiveStmt( StatementNode * stmt ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->directiveStmt = stmt;
	return newnode;
}

DeclarationNode * DeclarationNode::newAsmStmt( StatementNode * stmt ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->asmStmt = stmt;
	return newnode;
}

DeclarationNode * DeclarationNode::newStaticAssert( ExpressionNode * condition, ast::Expr * message ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->assert.condition = condition;
	newnode->assert.message = message;
	return newnode;
}

static void appendError( string & dst, const string & src ) {
	if ( src.empty() ) return;
	if ( dst.empty() ) { dst = src; return; }
	dst += ", " + src;
} // appendError

void DeclarationNode::checkQualifiers( const TypeData * src, const TypeData * dst ) {
	const ast::CV::Qualifiers qsrc = src->qualifiers, qdst = dst->qualifiers; // optimization
	const ast::CV::Qualifiers duplicates = qsrc & qdst;

	if ( duplicates.any() ) {
		std::stringstream str;
		str << "duplicate ";
		ast::print( str, duplicates );
		str << "qualifier(s)";
		appendError( error, str.str() );
	} // for
} // DeclarationNode::checkQualifiers

void DeclarationNode::checkSpecifiers( DeclarationNode * src ) {
	ast::Function::Specs fsDups = funcSpecs & src->funcSpecs;
	if ( fsDups.any() ) {
		std::stringstream str;
		str << "duplicate ";
		ast::print( str, fsDups );
		str << "function specifier(s)";
		appendError( error, str.str() );
	} // if

	// Skip if everything is unset.
	if ( storageClasses.any() && src->storageClasses.any() ) {
		ast::Storage::Classes dups = storageClasses & src->storageClasses;
		// Check for duplicates.
		if ( dups.any() ) {
			std::stringstream str;
			str << "duplicate ";
			ast::print( str, dups );
			str << "storage class(es)";
			appendError( error, str.str() );
		// Check for conflicts.
		} else if ( !src->storageClasses.is_threadlocal_any() ) {
			std::stringstream str;
			str << "conflicting ";
			ast::print( str, ast::Storage::Classes( 1 << storageClasses.ffs() ) );
			str << "& ";
			ast::print( str, ast::Storage::Classes( 1 << src->storageClasses.ffs() ) );
			str << "storage classes";
			appendError( error, str.str() );
			// FIX to preserve invariant of one basic storage specifier
			src->storageClasses.reset();
		}
	} // if

	appendError( error, src->error );
} // DeclarationNode::checkSpecifiers

DeclarationNode * DeclarationNode::copySpecifiers( DeclarationNode * q, bool copyattr ) {
	funcSpecs |= q->funcSpecs;
	storageClasses |= q->storageClasses;

	if ( copyattr ) {
		std::vector<ast::ptr<ast::Attribute>> tmp;
		tmp.reserve( q->attributes.size() );
		for ( auto const & attr : q->attributes ) {
			tmp.emplace_back( ast::shallowCopy( attr.get() ) );
		} // for
		spliceBegin( attributes, tmp );
	} // if

	return this;
} // DeclarationNode::copySpecifiers

DeclarationNode * DeclarationNode::addQualifiers( DeclarationNode * q ) {
	if ( ! q ) { return this; }							// empty qualifier

	checkSpecifiers( q );
	copySpecifiers( q );

	if ( ! q->type ) { delete q; return this; }

	if ( ! type ) {
		type = q->type;									// reuse structure
		q->type = nullptr;
		delete q;
		return this;
	} // if

	checkQualifiers( type, q->type );
	TypeData::BuiltinType const builtin = type->builtintype;
	if ( (builtin == TypeData::Zero || builtin == TypeData::One) && q->type->qualifiers.any() && error.length() == 0 ) {
		SemanticWarning( yylloc, Warning::BadQualifiersZeroOne, TypeData::builtinTypeNames[builtin] );
	} // if
	type = ::addQualifiers( type, q->type );
	q->type = nullptr;

	delete q;
	return this;
} // addQualifiers

DeclarationNode * DeclarationNode::addType( DeclarationNode * o, bool copyattr ) {
	if ( !o ) return this;

	checkSpecifiers( o );
	copySpecifiers( o, copyattr );
	if ( o->type ) {
		type = ::addType( type, o->type, o->attributes );
		o->type = nullptr;
	} // if
	if ( o->bitfieldWidth ) {
		bitfieldWidth = o->bitfieldWidth;
	} // if

	// there may be typedefs chained onto the type
	if ( o->next ) {
		set_last( o->next->clone() );
	} // if

	delete o;
	return this;
}

DeclarationNode * DeclarationNode::addEnumBase( DeclarationNode * o ) {
	if ( o && o->type ) {
		type->base = o->type;
	} // if
	delete o;
	return this;
}

// This code handles a special issue with the attribute transparent_union.
//
//    typedef union U { int i; } typedef_name __attribute__(( aligned(16) )) __attribute__(( transparent_union ))
//
// Here the attribute aligned goes with the typedef_name, so variables declared of this type are
// aligned.  However, the attribute transparent_union must be moved from the typedef_name to
// alias union U.  Currently, this is the only know attribute that must be moved from typedef to
// alias.
static void moveUnionAttribute( DeclarationNode * decl, DeclarationNode * unionDecl ) {
	assert( decl->type->kind == TypeData::Symbolic );
	assert( decl->type->symbolic.isTypedef );
	assert( unionDecl->type->kind == TypeData::Aggregate );

	if ( unionDecl->type->aggregate.kind != ast::AggregateDecl::Union ) return;

	// Ignore the Aggregate_t::attributes. Why did we add that before the rework?
	for ( auto attr = decl->attributes.begin() ; attr != decl->attributes.end() ; ) {
		if ( (*attr)->name == "transparent_union" || (*attr)->name == "__transparent_union__" ) {
			unionDecl->attributes.emplace_back( attr->release() );
			attr = decl->attributes.erase( attr );
		} else {
			++attr;
		}
	}
}

// Helper for addTypedef, handles the case where the typedef wraps an
// aggregate declaration (not a type), returns a chain of nodes.
static DeclarationNode * addTypedefAggr(
		DeclarationNode * olddecl, TypeData * newtype ) {
	TypeData *& oldaggr = olddecl->type->aggInst.aggregate;

	// Handle anonymous aggregates: typedef struct { int i; } foo
	// Give the typedefed type a consistent name across translation units.
	if ( oldaggr->aggregate.anon ) {
		delete oldaggr->aggregate.name;
		oldaggr->aggregate.name = new string( "__anonymous_" + *olddecl->name );
		oldaggr->aggregate.anon = false;
		oldaggr->qualifiers.reset();
	}

	// Replace the wrapped TypeData with a forward declaration.
	TypeData * newaggr = new TypeData( TypeData::Aggregate );
	newaggr->aggregate.kind = oldaggr->aggregate.kind;
	newaggr->aggregate.name = oldaggr->aggregate.name ? new string( *oldaggr->aggregate.name ) : nullptr;
	newaggr->aggregate.body = false;
	newaggr->aggregate.anon = oldaggr->aggregate.anon;
	newaggr->aggregate.isCfa = oldaggr->aggregate.isCfa;
	newaggr->aggregate.hiding = oldaggr->aggregate.hiding;
	swap( newaggr, oldaggr );

	newtype->base = olddecl->type;
	olddecl->type = newtype;
	DeclarationNode * newdecl = new DeclarationNode;
	newdecl->type = newaggr;
	newdecl->next = olddecl;

	moveUnionAttribute( olddecl, newdecl );

	return newdecl;
}

// Wrap the declaration in a typedef. It actually does that by modifying the
// existing declaration, and may split it into two declarations.
// This only happens if the wrapped type is actually a declaration of a SUE
// type. If it does, the DeclarationNode for the SUE declaration is the node
// returned, make sure later transformations are applied to the right node.
DeclarationNode * DeclarationNode::addTypedef() {
	TypeData * newtype = new TypeData( TypeData::Symbolic );
	newtype->symbolic.params = nullptr;
	newtype->symbolic.isTypedef = true;
	newtype->symbolic.name = name ? new string( *name ) : nullptr;
	// If this typedef is wrapping an aggregate, separate them out.
	if ( TypeData::AggregateInst == type->kind
			&& TypeData::Aggregate == type->aggInst.aggregate->kind
			&& type->aggInst.aggregate->aggregate.body ) {
		return addTypedefAggr( this, newtype );
	// There is no internal declaration, just a type.
	} else {
		newtype->base = type;
		type = newtype;
		return this;
	}
}

DeclarationNode * DeclarationNode::addAssertions( DeclarationNode * assertions ) {
	if ( variable.tyClass != ast::TypeDecl::NUMBER_OF_KINDS ) {
		extend( variable.assertions, assertions );
		return this;
	} // if

	assert( type );
	assert( TypeData::Symbolic == type->kind );
	extend( type->symbolic.assertions, assertions );

	return this;
}

DeclarationNode * DeclarationNode::addName( string * newname ) {
	assert( ! name );
	name = newname;
	return this;
}

DeclarationNode * DeclarationNode::addAsmName( DeclarationNode * newname ) {
	assert( ! asmName );
	asmName = newname ? newname->asmName : nullptr;
	return this->addQualifiers( newname );
}

DeclarationNode * DeclarationNode::addBitfield( ExpressionNode * size ) {
	bitfieldWidth = size;
	return this;
}

DeclarationNode * DeclarationNode::addVarArgs() {
	assert( type );
	hasEllipsis = true;
	return this;
}

DeclarationNode * DeclarationNode::addFunctionBody( StatementNode * body, ExpressionNode * withExprs ) {
	assert( type );
	assert( type->kind == TypeData::Function );
	assert( ! type->function.body );
	type->function.body = body;
	type->function.withExprs = withExprs;
	return this;
}

DeclarationNode * DeclarationNode::addOldDeclList( DeclarationNode * list ) {
	assert( type );
	assert( type->kind == TypeData::Function );
	assert( ! type->function.oldDeclList );
	type->function.oldDeclList = list;
	return this;
}

DeclarationNode * DeclarationNode::setBase( TypeData * newType ) {
	if ( type ) {
		type->setLastBase( newType );
	} else {
		type = newType;
	} // if
	return this;
}

DeclarationNode * DeclarationNode::copyAttribute( DeclarationNode * a ) {
	if ( a ) {
		spliceBegin( attributes, a->attributes );
		a->attributes.clear();
	} // if
	return this;
} // copyAttribute

DeclarationNode * DeclarationNode::addPointer( DeclarationNode * p ) {
	if ( p ) {
		assert( p->type->kind == TypeData::Pointer || p->type->kind == TypeData::Reference );
		setBase( p->type );
		p->type = nullptr;
		copyAttribute( p );
		delete p;
	} // if
	return this;
}

DeclarationNode * DeclarationNode::addArray( DeclarationNode * a ) {
	if ( a ) {
		assert( a->type->kind == TypeData::Array );
		setBase( a->type );
		a->type = nullptr;
		copyAttribute( a );
		delete a;
	} // if
	return this;
}

DeclarationNode * DeclarationNode::addNewPointer( DeclarationNode * p ) {
	if ( !p ) return this;
	assert( p->type->kind == TypeData::Pointer || p->type->kind == TypeData::Reference );
	if ( type ) {
		p->type->base = makeNewBase( type );
		type = nullptr;
	} // if
	delete this;
	return p;
}

DeclarationNode * DeclarationNode::addNewArray( DeclarationNode * a ) {
	if ( !a ) return this;
	assert( a->type->kind == TypeData::Array );
	if ( type ) {
		a->type->setLastBase( makeNewBase( type ) );
		type = nullptr;
	} // if
	delete this;
	return a;
}

DeclarationNode * DeclarationNode::addParamList( DeclarationNode * params ) {
	TypeData * ftype = new TypeData( TypeData::Function );
	ftype->function.params = params;
	setBase( ftype );
	return this;
}

static TypeData * addIdListToType( TypeData * type, DeclarationNode * ids ) {
	if ( type ) {
		if ( type->kind != TypeData::Function ) {
			type->base = addIdListToType( type->base, ids );
		} else {
			type->function.idList = ids;
		} // if
		return type;
	} else {
		TypeData * newtype = new TypeData( TypeData::Function );
		newtype->function.idList = ids;
		return newtype;
	} // if
} // addIdListToType

DeclarationNode * DeclarationNode::addIdList( DeclarationNode * ids ) {
	type = addIdListToType( type, ids );
	return this;
}

DeclarationNode * DeclarationNode::addInitializer( InitializerNode * init ) {
	initializer = init;
	return this;
}

DeclarationNode * DeclarationNode::addTypeInitializer( DeclarationNode * init ) {
	assertf( variable.tyClass != ast::TypeDecl::NUMBER_OF_KINDS, "Called addTypeInitializer on something that isn't a type variable." );
	variable.initializer = init;
	return this;
}

DeclarationNode * DeclarationNode::cloneType( string * name ) {
	DeclarationNode * newnode = newName( name );
	newnode->type = maybeCopy( type );
	newnode->copySpecifiers( this );
	return newnode;
}

DeclarationNode * DeclarationNode::cloneBaseType( DeclarationNode * o, bool copyattr ) {
	if ( ! o ) return nullptr;
	o->copySpecifiers( this, copyattr );
	if ( type ) {
		o->type = ::cloneBaseType( type, o->type );
	} // if
	return o;
}

DeclarationNode * DeclarationNode::extractAggregate() const {
	if ( type ) {
		TypeData * ret = typeextractAggregate( type );
		if ( ret ) {
			DeclarationNode * newnode = new DeclarationNode;
			newnode->type = ret;
			if ( ret->kind == TypeData::Aggregate ) {
				newnode->attributes.swap( ret->aggregate.attributes );
			} // if
			return newnode;
		} // if
	} // if
	return nullptr;
}

// Get the non-anonymous name of the instance type of the declaration,
// if one exists.
static const std::string * getInstTypeOfName( ast::Decl * decl ) {
	if ( auto dwt = dynamic_cast<ast::DeclWithType *>( decl ) ) {
		if ( auto aggr = dynamic_cast<ast::BaseInstType const *>( dwt->get_type() ) ) {
			if ( aggr->name.find("anonymous") == std::string::npos ) {
				return &aggr->name;
			}
		}
	}
	return nullptr;
}

void buildList( DeclarationNode * firstNode, std::vector<ast::ptr<ast::Decl>> & outputList ) {
	SemanticErrorException errors;
	std::back_insert_iterator<std::vector<ast::ptr<ast::Decl>>> out( outputList );

	for ( const DeclarationNode * cur = firstNode ; cur ; cur = cur->next ) {
		try {
			bool extracted_named = false;

			if ( DeclarationNode * extr = cur->extractAggregate() ) {
				assert( cur->type );

				if ( ast::Decl * decl = extr->build() ) {
					*out++ = decl;

					// need to remember the cases where a declaration contains an anonymous aggregate definition
					assert( extr->type );
					if ( extr->type->kind == TypeData::Aggregate ) {
						// typedef struct { int A } B is the only case?
						extracted_named = ! extr->type->aggregate.anon;
					} else {
						extracted_named = true;
					}
				} // if
				delete extr;
			} // if

			if ( ast::Decl * decl = cur->build() ) {
				if ( "" == decl->name && !cur->get_inLine() ) {
					// Don't include anonymous declaration for named aggregates,
					// but do include them for anonymous aggregates, e.g.:
					// struct S {
					//   struct T { int x; }; // no anonymous member
					//   struct { int y; };   // anonymous member
					//   struct T;            // anonymous member
					// };
					if ( extracted_named ) {
						continue;
					}

					if ( auto name = getInstTypeOfName( decl ) ) {
						// Temporary: warn about anonymous member declarations of named types, since
						// this conflicts with the syntax for the forward declaration of an anonymous type.
						SemanticWarning( cur->location, Warning::AggrForwardDecl, name->c_str() );
					}
				} // if
				*out++ = decl;
			} // if
		} catch ( SemanticErrorException & e ) {
			errors.append( e );
		} // try
	} // for

	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
} // buildList

// currently only builds assertions, function parameters, and return values
void buildList( DeclarationNode * firstNode, std::vector<ast::ptr<ast::DeclWithType>> & outputList ) {
	SemanticErrorException errors;
	std::back_insert_iterator<std::vector<ast::ptr<ast::DeclWithType>>> out( outputList );

	for ( const DeclarationNode * cur = firstNode; cur; cur = cur->next ) {
		try {
			ast::Decl * decl = cur->build();
			assertf( decl, "buildList: build for ast::DeclWithType." );
			if ( ast::DeclWithType * dwt = dynamic_cast<ast::DeclWithType *>( decl ) ) {
				dwt->location = cur->location;
				*out++ = dwt;
			} else if ( ast::StructDecl * agg = dynamic_cast<ast::StructDecl *>( decl ) ) {
				// e.g., int foo(struct S) {}
				auto inst = new ast::StructInstType( agg->name );
				auto obj = new ast::ObjectDecl( cur->location, "", inst );
				obj->linkage = linkage;
				*out++ = obj;
				delete agg;
			} else if ( ast::UnionDecl * agg = dynamic_cast<ast::UnionDecl *>( decl ) ) {
				// e.g., int foo(union U) {}
				auto inst = new ast::UnionInstType( agg->name );
				auto obj = new ast::ObjectDecl( cur->location,
					"", inst, nullptr, ast::Storage::Classes(),
					linkage );
				*out++ = obj;
			} else if ( ast::EnumDecl * agg = dynamic_cast<ast::EnumDecl *>( decl ) ) {
				// e.g., int foo(enum E) {}
				auto inst = new ast::EnumInstType( agg->name );
				auto obj = new ast::ObjectDecl( cur->location,
					"",
					inst,
					nullptr,
					ast::Storage::Classes(),
					linkage
				);
				*out++ = obj;
			} else {
				assertf( false, "buildList: Could not convert to ast::DeclWithType." );
			} // if
		} catch ( SemanticErrorException & e ) {
			errors.append( e );
		} // try
	} // for

	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
} // buildList

void buildTypeList( const DeclarationNode * firstNode,
		std::vector<ast::ptr<ast::Type>> & outputList ) {
	SemanticErrorException errors;
	std::back_insert_iterator<std::vector<ast::ptr<ast::Type>>> out( outputList );

	for ( const DeclarationNode * cur = firstNode ; cur ; cur = cur->next ) {
		try {
			* out++ = cur->buildType();
		} catch ( SemanticErrorException & e ) {
			errors.append( e );
		} // try
	} // for

	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
} // buildTypeList

ast::Decl * DeclarationNode::build() const {
	if ( ! error.empty() ) SemanticError( this, error + " in declaration of " );

	if ( asmStmt ) {
		auto stmt = strict_dynamic_cast<ast::AsmStmt *>( asmStmt->build() );
		return new ast::AsmDecl( stmt->location, stmt );
	} // if
	if ( directiveStmt ) {
		auto stmt = strict_dynamic_cast<ast::DirectiveStmt *>( directiveStmt->build() );
		return new ast::DirectiveDecl( stmt->location, stmt );
	} // if

	if ( variable.tyClass != ast::TypeDecl::NUMBER_OF_KINDS ) {
		// otype is internally converted to dtype + otype parameters
		static const ast::TypeDecl::Kind kindMap[] = { ast::TypeDecl::Dtype, ast::TypeDecl::Dtype, ast::TypeDecl::Dtype, ast::TypeDecl::Ftype, ast::TypeDecl::Ttype, ast::TypeDecl::Dimension };
		static_assert( sizeof(kindMap) / sizeof(kindMap[0]) == ast::TypeDecl::NUMBER_OF_KINDS, "DeclarationNode::build: kindMap is out of sync." );
		assertf( variable.tyClass < sizeof(kindMap)/sizeof(kindMap[0]), "Variable's tyClass is out of bounds." );
		ast::TypeDecl * ret = new ast::TypeDecl( location,
			*name,
			ast::Storage::Classes(),
			(ast::Type *)nullptr,
			kindMap[ variable.tyClass ],
			variable.tyClass == ast::TypeDecl::Otype || variable.tyClass == ast::TypeDecl::DStype,
			variable.initializer ? variable.initializer->buildType() : nullptr
		);
		buildList( variable.assertions, ret->assertions );
		return ret;
	} // if

	if ( type ) {
		// Function specifiers can only appear on a function definition/declaration.
		//
		//    inline _Noreturn int f();			// allowed
		//    inline _Noreturn int g( int i );	// allowed
		//    inline _Noreturn int i;			// disallowed
		if ( type->kind != TypeData::Function && funcSpecs.any() ) {
			SemanticError( this, "invalid function specifier for " );
		} // if
		// Forall qualifier can only appear on a function/aggregate definition/declaration.
		//
		//    forall int f();					// allowed
		//    forall int g( int i );			// allowed
		//    forall int i;						// disallowed
		if ( type->kind != TypeData::Function && type->forall ) {
			SemanticError( this, "invalid type qualifier for " );
		} // if
		bool isDelete = initializer && initializer->get_isDelete();
		ast::Decl * decl = buildDecl(
			type,
			name ? *name : string( "" ),
			storageClasses,
			maybeBuild( bitfieldWidth ),
			funcSpecs,
			linkage,
			asmName,
			isDelete ? nullptr : maybeBuild( initializer ),
			copy( attributes )
		)->set_extension( extension );
		if ( isDelete ) {
			auto dwt = strict_dynamic_cast<ast::DeclWithType *>( decl );
			dwt->isDeleted = true;
		}
		return decl;
	} // if

	if ( assert.condition ) {
		auto cond = maybeBuild( assert.condition );
		auto msg = strict_dynamic_cast<ast::ConstantExpr *>( maybeCopy( assert.message ) );
		return new ast::StaticAssertDecl( location, cond, msg );
	}

	// SUE's cannot have function specifiers, either
	//
	//    inline _Noreturn struct S { ... };		// disallowed
	//    inline _Noreturn enum   E { ... };		// disallowed
	if ( funcSpecs.any() ) {
		SemanticError( this, "invalid function specifier for " );
	} // if
	if ( enumInLine ) {
		return new ast::InlineMemberDecl( location,
			*name, (ast::Type*)nullptr, storageClasses, linkage );
	} // if
	assertf( name, "ObjectDecl must a have name\n" );
	auto ret = new ast::ObjectDecl( location,
		*name,
		(ast::Type*)nullptr,
		maybeBuild( initializer ),
		storageClasses,
		linkage,
		maybeBuild( bitfieldWidth )
	);
	ret->asmName = asmName;
	ret->extension = extension;
	return ret;
}

ast::Type * DeclarationNode::buildType() const {
	assert( type );

	// Some types are parsed as declarations and, syntactically, can have
	// initializers. However, semantically, this is meaningless.
	if ( initializer ) {
		SemanticError( this, "Initializer on type declaration " );
	}

	switch ( type->kind ) {
	case TypeData::Aggregate: {
		ast::BaseInstType * ret =
			buildComAggInst( type, copy( attributes ), linkage );
		buildList( type->aggregate.actuals, ret->params );
		return ret;
	}
	case TypeData::Symbolic: {
		ast::TypeInstType * ret = new ast::TypeInstType(
			*type->symbolic.name,
			// This is just a default, the true value is not known yet.
			ast::TypeDecl::Dtype,
			buildQualifiers( type ),
			copy( attributes ) );
		buildList( type->symbolic.actuals, ret->params );
		return ret;
	}
	default:
		ast::Type * simpletypes = typebuild( type );
		// copy because member is const
		simpletypes->attributes = attributes;
		return simpletypes;
	} // switch
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
