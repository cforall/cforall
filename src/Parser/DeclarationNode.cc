//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclarationNode.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 12:34:05 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Aug  8 17:07:00 2022
// Update Count     : 1185
//

#include <cassert>                 // for assert, assertf, strict_dynamic_cast
#include <iterator>                // for back_insert_iterator
#include <list>                    // for list
#include <memory>                  // for unique_ptr
#include <ostream>                 // for operator<<, ostream, basic_ostream
#include <string>                  // for string, operator+, allocator, char...

#include "Common/SemanticError.h"  // for SemanticError
#include "Common/UniqueName.h"     // for UniqueName
#include "Common/utility.h"        // for maybeClone, maybeBuild, CodeLocation
#include "Parser/ParseNode.h"      // for DeclarationNode, ExpressionNode
#include "SynTree/LinkageSpec.h"   // for Spec, linkageName, Cforall
#include "SynTree/Attribute.h"     // for Attribute
#include "SynTree/Declaration.h"   // for TypeDecl, ObjectDecl, Declaration
#include "SynTree/Expression.h"    // for Expression, ConstantExpr
#include "SynTree/Statement.h"     // for AsmStmt
#include "SynTree/Type.h"          // for Type, Type::StorageClasses, Type::...
#include "TypeData.h"              // for TypeData, TypeData::Aggregate_t
#include "TypedefTable.h"          // for TypedefTable

class Initializer;

extern TypedefTable typedefTable;

using namespace std;

// These must harmonize with the corresponding DeclarationNode enumerations.
const char * DeclarationNode::basicTypeNames[] = { "void", "_Bool", "char", "int", "int128",
												   "float", "double", "long double", "float80", "float128",
												   "_float16", "_float32", "_float32x", "_float64", "_float64x", "_float128", "_float128x", "NoBasicTypeNames" };
const char * DeclarationNode::complexTypeNames[] = { "_Complex", "NoComplexTypeNames", "_Imaginary" }; // Imaginary unsupported => parse, but make invisible and print error message
const char * DeclarationNode::signednessNames[] = { "signed", "unsigned", "NoSignednessNames" };
const char * DeclarationNode::lengthNames[] = { "short", "long", "long long", "NoLengthNames" };
const char * DeclarationNode::builtinTypeNames[] = { "__builtin_va_list", "__auto_type", "zero_t", "one_t", "NoBuiltinTypeNames" };

UniqueName DeclarationNode::anonymous( "__anonymous" );

extern LinkageSpec::Spec linkage;						// defined in parser.yy

DeclarationNode::DeclarationNode() :
	linkage( ::linkage ) {

//	variable.name = nullptr;
	variable.tyClass = TypeDecl::NUMBER_OF_KINDS;
	variable.assertions = nullptr;
	variable.initializer = nullptr;

//	attr.name = nullptr;
	attr.expr = nullptr;
	attr.type = nullptr;

	assert.condition = nullptr;
	assert.message = nullptr;
}

DeclarationNode::~DeclarationNode() {
//	delete attr.name;
	delete attr.expr;
	delete attr.type;

//	delete variable.name;
	delete variable.assertions;
	delete variable.initializer;

// 	delete type;
	delete bitfieldWidth;

	delete asmStmt;
	// asmName, no delete, passed to next stage
	delete initializer;

	delete assert.condition;
	delete assert.message;
}

DeclarationNode * DeclarationNode::clone() const {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->set_next( maybeClone( get_next() ) );
	newnode->name = name ? new string( *name ) : nullptr;

	newnode->builtin = NoBuiltinType;
	newnode->type = maybeClone( type );
	newnode->inLine = inLine;
	newnode->storageClasses = storageClasses;
	newnode->funcSpecs = funcSpecs;
	newnode->bitfieldWidth = maybeClone( bitfieldWidth );
	newnode->enumeratorValue.reset( maybeClone( enumeratorValue.get() ) );
	newnode->hasEllipsis = hasEllipsis;
	newnode->linkage = linkage;
	newnode->asmName = maybeClone( asmName );
	cloneAll( attributes, newnode->attributes );
	newnode->initializer = maybeClone( initializer );
	newnode->extension = extension;
	newnode->asmStmt = maybeClone( asmStmt );
	newnode->error = error;

//	newnode->variable.name = variable.name ? new string( *variable.name ) : nullptr;
	newnode->variable.tyClass = variable.tyClass;
	newnode->variable.assertions = maybeClone( variable.assertions );
	newnode->variable.initializer = maybeClone( variable.initializer );

//	newnode->attr.name = attr.name ? new string( *attr.name ) : nullptr;
	newnode->attr.expr = maybeClone( attr.expr );
	newnode->attr.type = maybeClone( attr.type );

	newnode->assert.condition = maybeClone( assert.condition );
	newnode->assert.message = maybeClone( assert.message );
	return newnode;
} // DeclarationNode::clone

void DeclarationNode::print( std::ostream & os, int indent ) const {
	os << string( indent, ' ' );
	if ( name ) {
		os << *name << ": ";
	} // if

	if ( linkage != LinkageSpec::Cforall ) {
		os << LinkageSpec::name( linkage ) << " ";
	} // if

	storageClasses.print( os );
	funcSpecs.print( os );

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

	for ( Attribute * attr: reverseIterate( attributes ) ) {
		os << string( indent + 2, ' ' ) << "attr " << attr->name.c_str();
	} // for

	os << endl;
}

void DeclarationNode::printList( std::ostream & os, int indent ) const {
	ParseNode::printList( os, indent );
	if ( hasEllipsis ) {
		os << string( indent, ' ' )  << "and a variable number of other arguments" << endl;
	} // if
}

DeclarationNode * DeclarationNode::newStorageClass( Type::StorageClasses sc ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->storageClasses = sc;
	return newnode;
} // DeclarationNode::newStorageClass

DeclarationNode * DeclarationNode::newFuncSpecifier( Type::FuncSpecifiers fs ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->funcSpecs = fs;
	return newnode;
} // DeclarationNode::newFuncSpecifier

DeclarationNode * DeclarationNode::newTypeQualifier( Type::Qualifiers tq ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData();
	newnode->type->qualifiers = tq;
	return newnode;
} // DeclarationNode::newQualifier

DeclarationNode * DeclarationNode::newBasicType( BasicType bt ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Basic );
	newnode->type->basictype = bt;
	return newnode;
} // DeclarationNode::newBasicType

DeclarationNode * DeclarationNode::newComplexType( ComplexType ct ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Basic );
	newnode->type->complextype = ct;
	return newnode;
} // DeclarationNode::newComplexType

DeclarationNode * DeclarationNode::newSignedNess( Signedness sn ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Basic );
	newnode->type->signedness = sn;
	return newnode;
} // DeclarationNode::newSignedNess

DeclarationNode * DeclarationNode::newLength( Length lnth ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Basic );
	newnode->type->length = lnth;
	return newnode;
} // DeclarationNode::newLength

DeclarationNode * DeclarationNode::newForall( DeclarationNode * forall ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Unknown );
	newnode->type->forall = forall;
	return newnode;
} // DeclarationNode::newForall

DeclarationNode * DeclarationNode::newFromGlobalScope() {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::GlobalScope );
	return newnode;
}

DeclarationNode * DeclarationNode::newQualifiedType( DeclarationNode * parent, DeclarationNode * child) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Qualified );
	newnode->type->qualified.parent = parent->type;
	newnode->type->qualified.child = child->type;
	parent->type = nullptr;
	child->type = nullptr;
	delete parent;
	delete child;
	return newnode;
}

DeclarationNode * DeclarationNode::newAggregate( AggregateDecl::Aggregate kind, const string * name, ExpressionNode * actuals, DeclarationNode * fields, bool body ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Aggregate );
	newnode->type->aggregate.kind = kind;
	newnode->type->aggregate.name = name == nullptr ? new string( DeclarationNode::anonymous.newName() ) : name;
	newnode->type->aggregate.actuals = actuals;
	newnode->type->aggregate.fields = fields;
	newnode->type->aggregate.body = body;
	newnode->type->aggregate.tagged = false;
	newnode->type->aggregate.parent = nullptr;
	newnode->type->aggregate.anon = name == nullptr;
	return newnode;
} // DeclarationNode::newAggregate

DeclarationNode * DeclarationNode::newEnum( const string * name, DeclarationNode * constants, bool body, DeclarationNode * base) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Enum );
	newnode->type->enumeration.name = name == nullptr ? new string( DeclarationNode::anonymous.newName() ) : name;
	newnode->type->enumeration.constants = constants;
	newnode->type->enumeration.body = body;
	newnode->type->enumeration.anon = name == nullptr;
	if ( base && base->type)  {
		newnode->type->base = base->type;
	} // if

	// Check: if base has TypeData
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
	if ( init ) { // list init {} or a singleInit
		if ( init->get_expression() ) { // singleInit
			return newEnumConstant( name, init->get_expression() );
		} else { // TODO: listInit
			DeclarationNode * newnode = newName( name );
			newnode->initializer = init;
			return newnode;
		} // if
	} else {
		return newName( name ); // Not explicitly inited enum value;
	} // if
} // DeclarationNode::newEnumValueGeneric

DeclarationNode * DeclarationNode::newFromTypedef( const string * name ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::SymbolicInst );
	newnode->type->symbolic.name = name;
	newnode->type->symbolic.isTypedef = true;
	newnode->type->symbolic.params = nullptr;
	return newnode;
} // DeclarationNode::newFromTypedef

DeclarationNode * DeclarationNode::newFromTypeGen( const string * name, ExpressionNode * params ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::SymbolicInst );
	newnode->type->symbolic.name = name;
	newnode->type->symbolic.isTypedef = false;
	newnode->type->symbolic.actuals = params;
	return newnode;
} // DeclarationNode::newFromTypeGen

DeclarationNode * DeclarationNode::newTypeParam( TypeDecl::Kind tc, const string * name ) {
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
	newnode->type->aggregate.kind = AggregateDecl::Trait;
	newnode->type->aggregate.params = params;
	newnode->type->aggregate.fields = asserts;
	return newnode;
} // DeclarationNode::newTrait

DeclarationNode * DeclarationNode::newTraitUse( const string * name, ExpressionNode * params ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::AggregateInst );
	newnode->type->aggInst.aggregate = new TypeData( TypeData::Aggregate );
	newnode->type->aggInst.aggregate->aggregate.kind = AggregateDecl::Trait;
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
	if ( newnode->type->array.dimension == nullptr || newnode->type->array.dimension->isExpressionType<ConstantExpr * >() ) {
		newnode->type->array.isVarLen = false;
	} else {
		newnode->type->array.isVarLen = true;
	} // if
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

DeclarationNode * DeclarationNode::newVtableType( DeclarationNode * decl ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Vtable );
	newnode->setBase( decl->type );
	return newnode;
}

DeclarationNode * DeclarationNode::newBuiltinType( BuiltinType bt ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Builtin );
	newnode->builtin = bt;
	newnode->type->builtintype = newnode->builtin;
	return newnode;
} // DeclarationNode::newBuiltinType

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
	std::list< Expression * > exprs;
	buildList( expr, exprs );
	newnode->attributes.push_back( new Attribute( *name, exprs ) );
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

DeclarationNode * DeclarationNode::newStaticAssert( ExpressionNode * condition, Expression * message ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->assert.condition = condition;
	newnode->assert.message = message;
	return newnode;
}


void appendError( string & dst, const string & src ) {
	if ( src.empty() ) return;
	if ( dst.empty() ) { dst = src; return; }
	dst += ", " + src;
} // appendError

void DeclarationNode::checkQualifiers( const TypeData * src, const TypeData * dst ) {
	const Type::Qualifiers qsrc = src->qualifiers, qdst = dst->qualifiers; // optimization

	if ( (qsrc & qdst).any() ) {						// duplicates ?
		for ( unsigned int i = 0; i < Type::NumTypeQualifier; i += 1 ) { // find duplicates
			if ( qsrc[i] && qdst[i] ) {
				appendError( error, string( "duplicate " ) + Type::QualifiersNames[i] );
			} // if
		} // for
	} // for
} // DeclarationNode::checkQualifiers

void DeclarationNode::checkSpecifiers( DeclarationNode * src ) {
	if ( (funcSpecs & src->funcSpecs).any() ) {			// duplicates ?
		for ( unsigned int i = 0; i < Type::NumFuncSpecifier; i += 1 ) { // find duplicates
			if ( funcSpecs[i] && src->funcSpecs[i] ) {
				appendError( error, string( "duplicate " ) + Type::FuncSpecifiersNames[i] );
			} // if
		} // for
	} // if

	if ( storageClasses.any() && src->storageClasses.any() ) { // any reason to check ?
		if ( (storageClasses & src->storageClasses ).any() ) { // duplicates ?
			for ( unsigned int i = 0; i < Type::NumStorageClass; i += 1 ) { // find duplicates
				if ( storageClasses[i] && src->storageClasses[i] ) {
					appendError( error, string( "duplicate " ) + Type::StorageClassesNames[i] );
				} // if
			} // for
			// src is the new item being added and has a single bit
		} else if ( ! src->storageClasses.is_threadlocal_any() ) { // conflict ?
			appendError( error, string( "conflicting " ) + Type::StorageClassesNames[storageClasses.ffs()] +
						 " & " + Type::StorageClassesNames[src->storageClasses.ffs()] );
			src->storageClasses.reset();				// FIX to preserve invariant of one basic storage specifier
		} // if
	} // if

	appendError( error, src->error );
} // DeclarationNode::checkSpecifiers

DeclarationNode * DeclarationNode::copySpecifiers( DeclarationNode * q ) {
	funcSpecs |= q->funcSpecs;
	storageClasses |= q->storageClasses;

	for ( Attribute * attr: reverseIterate( q->attributes ) ) {
		attributes.push_front( attr->clone() );
	} // for
	return this;
} // DeclarationNode::copySpecifiers

static void addQualifiersToType( TypeData *& src, TypeData * dst ) {
	if ( dst->base ) {
		addQualifiersToType( src, dst->base );
	} else if ( dst->kind == TypeData::Function ) {
		dst->base = src;
		src = nullptr;
	} else {
		dst->qualifiers |= src->qualifiers;
	} // if
} // addQualifiersToType

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

	if ( q->type->forall ) {							// forall qualifier ?
		if ( type->forall ) {							// polymorphic routine ?
			type->forall->appendList( q->type->forall ); // augment forall qualifier
		} else {
			if ( type->kind == TypeData::Aggregate ) {	// struct/union ?
				if ( type->aggregate.params ) {			// polymorphic ?
					type->aggregate.params->appendList( q->type->forall ); // augment forall qualifier
				} else {								// not polymorphic
					type->aggregate.params = q->type->forall; // set forall qualifier
				} // if
			} else {									// not polymorphic
				type->forall = q->type->forall;			// make polymorphic routine
			} // if
		} // if
		q->type->forall = nullptr;						// forall qualifier moved
	} // if

	checkQualifiers( type, q->type );
	if ( (builtin == Zero || builtin == One) && q->type->qualifiers.val != 0 && error.length() == 0 ) {
		SemanticWarning( yylloc, Warning::BadQualifiersZeroOne, Type::QualifiersNames[ilog2( q->type->qualifiers.val )], builtinTypeNames[builtin] );
	} // if
	addQualifiersToType( q->type, type );

	delete q;
	return this;
} // addQualifiers

static void addTypeToType( TypeData *& src, TypeData *& dst ) {
	if ( src->forall && dst->kind == TypeData::Function ) {
		if ( dst->forall ) {
			dst->forall->appendList( src->forall );
		} else {
			dst->forall = src->forall;
		} // if
		src->forall = nullptr;
	} // if
	if ( dst->base ) {
		addTypeToType( src, dst->base );
	} else {
		switch ( dst->kind ) {
		  case TypeData::Unknown:
			src->qualifiers |= dst->qualifiers;
			dst = src;
			src = nullptr;
			break;
		  case TypeData::Basic:
			dst->qualifiers |= src->qualifiers;
			if ( src->kind != TypeData::Unknown ) {
				assert( src->kind == TypeData::Basic );

				if ( dst->basictype == DeclarationNode::NoBasicType ) {
					dst->basictype = src->basictype;
				} else if ( src->basictype != DeclarationNode::NoBasicType )
					SemanticError( yylloc, src, string( "conflicting type specifier " ) + DeclarationNode::basicTypeNames[ src->basictype ] + " in type: " );

				if ( dst->complextype == DeclarationNode::NoComplexType ) {
					dst->complextype = src->complextype;
				} else if ( src->complextype != DeclarationNode::NoComplexType )
					SemanticError( yylloc, src, string( "conflicting type specifier " ) + DeclarationNode::complexTypeNames[ src->complextype ] + " in type: " );

				if ( dst->signedness == DeclarationNode::NoSignedness ) {
					dst->signedness = src->signedness;
				} else if ( src->signedness != DeclarationNode::NoSignedness )
					SemanticError( yylloc, src, string( "conflicting type specifier " ) + DeclarationNode::signednessNames[ src->signedness ] + " in type: " );

				if ( dst->length == DeclarationNode::NoLength ) {
					dst->length = src->length;
				} else if ( dst->length == DeclarationNode::Long && src->length == DeclarationNode::Long ) {
					dst->length = DeclarationNode::LongLong;
				} else if ( src->length != DeclarationNode::NoLength )
					SemanticError( yylloc, src, string( "conflicting type specifier " ) + DeclarationNode::lengthNames[ src->length ] + " in type: " );
			} // if
			break;
		  default:
			switch ( src->kind ) {
			  case TypeData::Aggregate:
			  case TypeData::Enum:
				dst->base = new TypeData( TypeData::AggregateInst );
				dst->base->aggInst.aggregate = src;
				if ( src->kind == TypeData::Aggregate ) {
					dst->base->aggInst.params = maybeClone( src->aggregate.actuals );
				} // if
				dst->base->qualifiers |= src->qualifiers;
				src = nullptr;
				break;
			  default:
				if ( dst->forall ) {
					dst->forall->appendList( src->forall );
				} else {
					dst->forall = src->forall;
				} // if
				src->forall = nullptr;
				dst->base = src;
				src = nullptr;
			} // switch
		} // switch
	} // if
}

DeclarationNode * DeclarationNode::addType( DeclarationNode * o ) {
	if ( o ) {
		checkSpecifiers( o );
		copySpecifiers( o );
		if ( o->type ) {
			if ( ! type ) {
				if ( o->type->kind == TypeData::Aggregate || o->type->kind == TypeData::Enum ) {
					type = new TypeData( TypeData::AggregateInst );
					type->aggInst.aggregate = o->type;
					if ( o->type->kind == TypeData::Aggregate ) {
						type->aggInst.hoistType = o->type->aggregate.body;
						type->aggInst.params = maybeClone( o->type->aggregate.actuals );
					} else {
						type->aggInst.hoistType = o->type->enumeration.body;
					} // if
					type->qualifiers |= o->type->qualifiers;
				} else {
					type = o->type;
				} // if
				o->type = nullptr;
			} else {
				addTypeToType( o->type, type );
			} // if
		} // if
		if ( o->bitfieldWidth ) {
			bitfieldWidth = o->bitfieldWidth;
		} // if

		// there may be typedefs chained onto the type
		if ( o->get_next() ) {
			set_last( o->get_next()->clone() );
		} // if
	} // if
	delete o;

	return this;
}

DeclarationNode * DeclarationNode::addEnumBase( DeclarationNode * o ) {
	if ( o && o -> type)  {
		type->base= o->type;
	}
	delete o;
	return this;
}

DeclarationNode * DeclarationNode::addTypedef() {
	TypeData * newtype = new TypeData( TypeData::Symbolic );
	newtype->symbolic.params = nullptr;
	newtype->symbolic.isTypedef = true;
	newtype->symbolic.name = name ? new string( *name ) : nullptr;
	newtype->base = type;
	type = newtype;
	return this;
}

DeclarationNode * DeclarationNode::addAssertions( DeclarationNode * assertions ) {
	if ( variable.tyClass != TypeDecl::NUMBER_OF_KINDS ) {
	  	if ( variable.assertions ) {
	  		variable.assertions->appendList( assertions );
	  	} else {
	  		variable.assertions = assertions;
	  	} // if
	  	return this;
	} // if

	assert( type );
	switch ( type->kind ) {
	  case TypeData::Symbolic:
		if ( type->symbolic.assertions ) {
			type->symbolic.assertions->appendList( assertions );
		} else {
			type->symbolic.assertions = assertions;
		} // if
		break;
	  default:
		assert( false );
	} // switch

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
		TypeData * prevBase = type;
		TypeData * curBase = type->base;
		while ( curBase != nullptr ) {
			prevBase = curBase;
			curBase = curBase->base;
		} // while
		prevBase->base = newType;
	} else {
		type = newType;
	} // if
	return this;
}

DeclarationNode * DeclarationNode::copyAttribute( DeclarationNode * a ) {
	if ( a ) {
		for ( Attribute *attr: reverseIterate( a->attributes ) ) {
			attributes.push_front( attr );
		} // for
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
	if ( p ) {
		assert( p->type->kind == TypeData::Pointer || p->type->kind == TypeData::Reference );
		if ( type ) {
			switch ( type->kind ) {
			  case TypeData::Aggregate:
			  case TypeData::Enum:
				p->type->base = new TypeData( TypeData::AggregateInst );
				p->type->base->aggInst.aggregate = type;
				if ( type->kind == TypeData::Aggregate ) {
					p->type->base->aggInst.params = maybeClone( type->aggregate.actuals );
				} // if
				p->type->base->qualifiers |= type->qualifiers;
				break;

			  default:
				p->type->base = type;
			} // switch
			type = nullptr;
		} // if
		delete this;
		return p;
	} else {
		return this;
	} // if
}

static TypeData * findLast( TypeData * a ) {
	assert( a );
	TypeData * cur = a;
	while ( cur->base ) {
		cur = cur->base;
	} // while
	return cur;
}

DeclarationNode * DeclarationNode::addNewArray( DeclarationNode * a ) {
  if ( ! a ) return this;
	assert( a->type->kind == TypeData::Array );
	TypeData * lastArray = findLast( a->type );
	if ( type ) {
		switch ( type->kind ) {
		  case TypeData::Aggregate:
		  case TypeData::Enum:
			lastArray->base = new TypeData( TypeData::AggregateInst );
			lastArray->base->aggInst.aggregate = type;
			if ( type->kind == TypeData::Aggregate ) {
				lastArray->base->aggInst.params = maybeClone( type->aggregate.actuals );
			} // if
			lastArray->base->qualifiers |= type->qualifiers;
			break;
		  default:
			lastArray->base = type;
		} // switch
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
	assertf( variable.tyClass != TypeDecl::NUMBER_OF_KINDS, "Called addTypeInitializer on something that isn't a type variable." );
	variable.initializer = init;
	return this;
}

DeclarationNode * DeclarationNode::cloneType( string * name ) {
	DeclarationNode * newnode = newName( name );
	newnode->type = maybeClone( type );
	newnode->copySpecifiers( this );
	return newnode;
}

DeclarationNode * DeclarationNode::cloneBaseType( DeclarationNode * o ) {
	if ( ! o ) return nullptr;

	o->copySpecifiers( this );
	if ( type ) {
		TypeData * srcType = type;

		// search for the base type by scanning off pointers and array designators
		while ( srcType->base ) {
			srcType = srcType->base;
		} // while

		TypeData * newType = srcType->clone();
		if ( newType->kind == TypeData::AggregateInst ) {
			// don't duplicate members
			if ( newType->aggInst.aggregate->kind == TypeData::Enum ) {
				delete newType->aggInst.aggregate->enumeration.constants;
				newType->aggInst.aggregate->enumeration.constants = nullptr;
				newType->aggInst.aggregate->enumeration.body = false;
			} else {
				assert( newType->aggInst.aggregate->kind == TypeData::Aggregate );
				delete newType->aggInst.aggregate->aggregate.fields;
				newType->aggInst.aggregate->aggregate.fields = nullptr;
				newType->aggInst.aggregate->aggregate.body = false;
			} // if
			// don't hoist twice
			newType->aggInst.hoistType = false;
		} // if

		newType->forall = maybeClone( type->forall );
		if ( ! o->type ) {
			o->type = newType;
		} else {
			addTypeToType( newType, o->type );
			delete newType;
		} // if
	} // if
	return o;
}

DeclarationNode * DeclarationNode::extractAggregate() const {
	if ( type ) {
		TypeData * ret = typeextractAggregate( type );
		if ( ret ) {
			DeclarationNode * newnode = new DeclarationNode;
			newnode->type = ret;
			return newnode;
		} // if
	} // if
	return nullptr;
}

void buildList( const DeclarationNode * firstNode, std::list< Declaration * > & outputList ) {
	SemanticErrorException errors;
	std::back_insert_iterator< std::list< Declaration * > > out( outputList );

	for ( const DeclarationNode * cur = firstNode; cur; cur = dynamic_cast< DeclarationNode * >( cur->get_next() ) ) {
		try {
			bool extracted = false;
			bool anon = false;
			if ( DeclarationNode * extr = cur->extractAggregate() ) {
				// handle the case where a structure declaration is contained within an object or type declaration
				Declaration * decl = extr->build();
				if ( decl ) {
					// hoist the structure declaration
					decl->location = cur->location;
					* out++ = decl;

					// need to remember the cases where a declaration contains an anonymous aggregate definition
					extracted = true;
					assert( extr->type );
					if ( extr->type->kind == TypeData::Aggregate ) {
						anon = extr->type->aggregate.anon;
					} else if ( extr->type->kind == TypeData::Enum ) {
						// xxx - is it useful to have an implicit anonymous enum member?
						anon = extr->type->enumeration.anon;
					}
				} // if
				delete extr;
			} // if

			Declaration * decl = cur->build();
			if ( decl ) {
				// don't include anonymous declaration for named aggregates, but do include them for anonymous aggregates, e.g.:
				// struct S {
				//   struct T { int x; }; // no anonymous member
				//   struct { int y; };   // anonymous member
				//   struct T;            // anonymous member
				// };
				if ( ! (extracted && decl->name == "" && ! anon && ! cur->get_inLine()) ) {
					if ( decl->name == "" ) {
						if ( DeclarationWithType * dwt = dynamic_cast<DeclarationWithType *>( decl ) ) {
							if ( ReferenceToType * aggr = dynamic_cast<ReferenceToType *>( dwt->get_type() ) ) {
								if ( aggr->name.find("anonymous") == std::string::npos ) {
									if ( ! cur->get_inLine() ) {
										// temporary: warn about anonymous member declarations of named types, since
										// this conflicts with the syntax for the forward declaration of an anonymous type
										SemanticWarning( cur->location, Warning::AggrForwardDecl, aggr->name.c_str() );
									} // if
								} // if
							} // if
						} // if
					} // if
					decl->location = cur->location;
					*out++ = decl;
				} // if
			} // if
		} catch( SemanticErrorException & e ) {
			errors.append( e );
		} // try
	} // for

	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
} // buildList

// currently only builds assertions, function parameters, and return values
void buildList( const DeclarationNode * firstNode, std::list< DeclarationWithType * > & outputList ) {
	SemanticErrorException errors;
	std::back_insert_iterator< std::list< DeclarationWithType * > > out( outputList );

	for ( const DeclarationNode * cur = firstNode; cur; cur = dynamic_cast< DeclarationNode * >( cur->get_next() ) ) {
		try {
			Declaration * decl = cur->build();
			assert( decl );
			if ( DeclarationWithType * dwt = dynamic_cast< DeclarationWithType * >( decl ) ) {
				dwt->location = cur->location;
				*out++ = dwt;
			} else if ( StructDecl * agg = dynamic_cast< StructDecl * >( decl ) ) {
				// e.g., int foo(struct S) {}
				StructInstType * inst = new StructInstType( Type::Qualifiers(), agg->name );
				auto obj = new ObjectDecl( "", Type::StorageClasses(), linkage, nullptr, inst, nullptr );
				obj->location = cur->location;
				*out++ = obj;
				delete agg;
			} else if ( UnionDecl * agg = dynamic_cast< UnionDecl * >( decl ) ) {
				// e.g., int foo(union U) {}
				UnionInstType * inst = new UnionInstType( Type::Qualifiers(), agg->name );
				auto obj = new ObjectDecl( "", Type::StorageClasses(), linkage, nullptr, inst, nullptr );
				obj->location = cur->location;
				*out++ = obj;
			} else if ( EnumDecl * agg = dynamic_cast< EnumDecl * >( decl ) ) {
				// e.g., int foo(enum E) {}
				EnumInstType * inst = new EnumInstType( Type::Qualifiers(), agg->name );
				auto obj = new ObjectDecl( "", Type::StorageClasses(), linkage, nullptr, inst, nullptr );
				obj->location = cur->location;
				*out++ = obj;
			} // if
		} catch( SemanticErrorException & e ) {
			errors.append( e );
		} // try
	} // for

	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
} // buildList

void buildTypeList( const DeclarationNode * firstNode, std::list< Type * > & outputList ) {
	SemanticErrorException errors;
	std::back_insert_iterator< std::list< Type * > > out( outputList );
	const DeclarationNode * cur = firstNode;

	while ( cur ) {
		try {
			* out++ = cur->buildType();
		} catch( SemanticErrorException & e ) {
			errors.append( e );
		} // try
		cur = dynamic_cast< DeclarationNode * >( cur->get_next() );
	} // while

	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
} // buildTypeList

Declaration * DeclarationNode::build() const {
	if ( ! error.empty() ) SemanticError( this, error + " in declaration of " );

	if ( asmStmt ) {
		return new AsmDecl( strict_dynamic_cast<AsmStmt *>( asmStmt->build() ) );
	} // if
	if ( directiveStmt ) {
		return new DirectiveDecl( strict_dynamic_cast<DirectiveStmt *>( directiveStmt->build() ) );
	} // if

	if ( variable.tyClass != TypeDecl::NUMBER_OF_KINDS ) {
		// otype is internally converted to dtype + otype parameters
		static const TypeDecl::Kind kindMap[] = { TypeDecl::Dtype, TypeDecl::Dtype, TypeDecl::Dtype, TypeDecl::Ftype, TypeDecl::Ttype, TypeDecl::Dimension };
		static_assert( sizeof(kindMap) / sizeof(kindMap[0]) == TypeDecl::NUMBER_OF_KINDS, "DeclarationNode::build: kindMap is out of sync." );
		assertf( variable.tyClass < sizeof(kindMap)/sizeof(kindMap[0]), "Variable's tyClass is out of bounds." );
		TypeDecl * ret = new TypeDecl( *name, Type::StorageClasses(), nullptr, kindMap[ variable.tyClass ], variable.tyClass == TypeDecl::Otype || variable.tyClass == TypeDecl::DStype, variable.initializer ? variable.initializer->buildType() : nullptr );
		buildList( variable.assertions, ret->get_assertions() );
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
		Declaration * decl = buildDecl( type, name ? *name : string( "" ), storageClasses, maybeBuild< Expression >( bitfieldWidth ), funcSpecs, linkage, asmName, isDelete ? nullptr : maybeBuild< Initializer >(initializer), attributes )->set_extension( extension );
		if ( isDelete ) {
			DeclarationWithType * dwt = strict_dynamic_cast<DeclarationWithType *>( decl );
			dwt->isDeleted = true;
		}
		return decl;
	} // if

	if ( assert.condition ) {
		return new StaticAssertDecl( maybeBuild< Expression >( assert.condition ), strict_dynamic_cast< ConstantExpr * >( maybeClone( assert.message ) ) );
	}

	// SUE's cannot have function specifiers, either
	//
	//    inline _Noreturn struct S { ... };		// disallowed
	//    inline _Noreturn enum   E { ... };		// disallowed
	if ( funcSpecs.any() ) {
		SemanticError( this, "invalid function specifier for " );
	} // if
	assertf( name, "ObjectDecl must a have name\n" );
	return (new ObjectDecl( *name, storageClasses, linkage, maybeBuild< Expression >( bitfieldWidth ), nullptr, maybeBuild< Initializer >( initializer ) ))->set_asmName( asmName )->set_extension( extension );
}

Type * DeclarationNode::buildType() const {
	assert( type );

	if ( attr.expr ) {
		return new AttrType( buildQualifiers( type ), *name, attr.expr->build(), attributes );
	} else if ( attr.type ) {
		return new AttrType( buildQualifiers( type ), *name, attr.type->buildType(), attributes );
	} // if

	switch ( type->kind ) {
	  case TypeData::Enum:
	  case TypeData::Aggregate: {
		  ReferenceToType * ret = buildComAggInst( type, attributes, linkage );
		  buildList( type->aggregate.actuals, ret->get_parameters() );
		  return ret;
	  }
	  case TypeData::Symbolic: {
		  TypeInstType * ret = new TypeInstType( buildQualifiers( type ), *type->symbolic.name, false, attributes );
		  buildList( type->symbolic.actuals, ret->get_parameters() );
		  return ret;
	  }
	  default:
		Type * simpletypes = typebuild( type );
		simpletypes->get_attributes() = attributes;		// copy because member is const
		return simpletypes;
	} // switch
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
