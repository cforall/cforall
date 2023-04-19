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
// Last Modified By : Andrew Beach
// Last Modified On : Tue Apr  4 10:28:00 2023
// Update Count     : 1392
//

#include "DeclarationNode.h"

#include <cassert>                 // for assert, assertf, strict_dynamic_cast
#include <iterator>                // for back_insert_iterator
#include <list>                    // for list
#include <memory>                  // for unique_ptr
#include <ostream>                 // for operator<<, ostream, basic_ostream
#include <string>                  // for string, operator+, allocator, char...

#include "AST/Attribute.hpp"       // for Attribute
#include "AST/Copy.hpp"            // for shallowCopy
#include "AST/Decl.hpp"            // for Decl
#include "AST/Expr.hpp"            // for Expr
#include "AST/Print.hpp"           // for print
#include "AST/Stmt.hpp"            // for AsmStmt, DirectiveStmt
#include "AST/StorageClasses.hpp"  // for Storage::Class
#include "AST/Type.hpp"            // for Type
#include "Common/CodeLocation.h"   // for CodeLocation
#include "Common/Iterate.hpp"      // for reverseIterate
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/UniqueName.h"     // for UniqueName
#include "Common/utility.h"        // for maybeClone
#include "Parser/ExpressionNode.h" // for ExpressionNode
#include "Parser/InitializerNode.h"// for InitializerNode
#include "Parser/StatementNode.h"  // for StatementNode
#include "TypeData.h"              // for TypeData, TypeData::Aggregate_t
#include "TypedefTable.h"          // for TypedefTable

class Initializer;

extern TypedefTable typedefTable;

using namespace std;

// These must harmonize with the corresponding DeclarationNode enumerations.
const char * DeclarationNode::basicTypeNames[] = {
	"void", "_Bool", "char", "int", "int128",
	"float", "double", "long double", "float80", "float128",
	"_float16", "_float32", "_float32x", "_float64", "_float64x", "_float128", "_float128x", "NoBasicTypeNames"
};
const char * DeclarationNode::complexTypeNames[] = {
	"_Complex", "NoComplexTypeNames", "_Imaginary"
}; // Imaginary unsupported => parse, but make invisible and print error message
const char * DeclarationNode::signednessNames[] = {
	"signed", "unsigned", "NoSignednessNames"
};
const char * DeclarationNode::lengthNames[] = {
	"short", "long", "long long", "NoLengthNames"
};
const char * DeclarationNode::builtinTypeNames[] = {
	"__builtin_va_list", "__auto_type", "zero_t", "one_t", "NoBuiltinTypeNames"
};

UniqueName DeclarationNode::anonymous( "__anonymous" );

extern ast::Linkage::Spec linkage;						// defined in parser.yy

DeclarationNode::DeclarationNode() :
	linkage( ::linkage ) {

//	variable.name = nullptr;
	variable.tyClass = ast::TypeDecl::NUMBER_OF_KINDS;
	variable.assertions = nullptr;
	variable.initializer = nullptr;

	assert.condition = nullptr;
	assert.message = nullptr;
}

DeclarationNode::~DeclarationNode() {
//	delete variable.name;
	delete variable.assertions;
	delete variable.initializer;

//	delete type;
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
	newnode->asmName = maybeCopy( asmName );
	newnode->attributes = attributes;
	newnode->initializer = maybeClone( initializer );
	newnode->extension = extension;
	newnode->asmStmt = maybeClone( asmStmt );
	newnode->error = error;

//	newnode->variable.name = variable.name ? new string( *variable.name ) : nullptr;
	newnode->variable.tyClass = variable.tyClass;
	newnode->variable.assertions = maybeClone( variable.assertions );
	newnode->variable.initializer = maybeClone( variable.initializer );

	newnode->assert.condition = maybeClone( assert.condition );
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
		os << string( indent + 2, ' ' ) << "with attributes " << endl;
		for ( ast::ptr<ast::Attribute> const & attr : reverseIterate( attributes ) ) {
			os << string( indent + 4, ' ' ) << attr->name.c_str() << endl;
		} // for
	} // if

	os << endl;
}

void DeclarationNode::printList( std::ostream & os, int indent ) const {
	ParseNode::printList( os, indent );
	if ( hasEllipsis ) {
		os << string( indent, ' ' )  << "and a variable number of other arguments" << endl;
	} // if
}

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

DeclarationNode * DeclarationNode::newTypeQualifier( ast::CV::Qualifiers tq ) {
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

DeclarationNode * DeclarationNode::newAggregate( ast::AggregateDecl::Aggregate kind, const string * name, ExpressionNode * actuals, DeclarationNode * fields, bool body ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Aggregate );
	newnode->type->aggregate.kind = kind;
	newnode->type->aggregate.anon = name == nullptr;
	newnode->type->aggregate.name = newnode->type->aggregate.anon ? new string( DeclarationNode::anonymous.newName() ) : name;
	newnode->type->aggregate.actuals = actuals;
	newnode->type->aggregate.fields = fields;
	newnode->type->aggregate.body = body;
	newnode->type->aggregate.tagged = false;
	newnode->type->aggregate.parent = nullptr;
	return newnode;
} // DeclarationNode::newAggregate

DeclarationNode * DeclarationNode::newEnum( const string * name, DeclarationNode * constants, bool body, bool typed, DeclarationNode * base, EnumHiding hiding ) {
	DeclarationNode * newnode = new DeclarationNode;
	newnode->type = new TypeData( TypeData::Enum );
	newnode->type->enumeration.anon = name == nullptr;
	newnode->type->enumeration.name = newnode->type->enumeration.anon ? new string( DeclarationNode::anonymous.newName() ) : name;
	newnode->type->enumeration.constants = constants;
	newnode->type->enumeration.body = body;
	newnode->type->enumeration.typed = typed;
	newnode->type->enumeration.hiding = hiding;
	if ( base && base->type )  {
		newnode->type->base = base->type;
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
	if ( init ) {
		if ( init->get_expression() ) {
			return newEnumConstant( name, init->get_expression() );
		} else {
			DeclarationNode * newnode = newName( name );
			newnode->initializer = init;
			return newnode;
		} // if
	} else {
		return newName( name );
	} // if
} // DeclarationNode::newEnumValueGeneric

DeclarationNode * DeclarationNode::newEnumInLine( const string name ) {
	DeclarationNode * newnode = newName( new std::string(name) );
	newnode->enumInLine = true;
	return newnode;
}

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
	if ( newnode->type->array.dimension == nullptr || newnode->type->array.dimension->isExpressionType<ast::ConstantExpr *>() ) {
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
	std::vector<ast::ptr<ast::Expr>> exprs;
	buildList( expr, exprs );
	newnode->attributes.push_back(
		new ast::Attribute( *name, std::move( exprs ) ) );
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

DeclarationNode * DeclarationNode::copySpecifiers( DeclarationNode * q ) {
	funcSpecs |= q->funcSpecs;
	storageClasses |= q->storageClasses;

	std::vector<ast::ptr<ast::Attribute>> tmp;
	tmp.reserve( q->attributes.size() );
	for ( auto const & attr : q->attributes ) {
		tmp.emplace_back( ast::shallowCopy( attr.get() ) );
	}
	spliceBegin( attributes, tmp );

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
	if ( (builtin == Zero || builtin == One) && q->type->qualifiers.any() && error.length() == 0 ) {
		SemanticWarning( yylloc, Warning::BadQualifiersZeroOne, builtinTypeNames[builtin] );
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
	if ( variable.tyClass != ast::TypeDecl::NUMBER_OF_KINDS ) {
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
	assertf( variable.tyClass != ast::TypeDecl::NUMBER_OF_KINDS, "Called addTypeInitializer on something that isn't a type variable." );
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

void buildList( DeclarationNode * firstNode,
		std::vector<ast::ptr<ast::Decl>> & outputList ) {
	SemanticErrorException errors;
	std::back_insert_iterator<std::vector<ast::ptr<ast::Decl>>> out( outputList );

	for ( const DeclarationNode * cur = firstNode; cur; cur = dynamic_cast< DeclarationNode * >( cur->get_next() ) ) {
		try {
			bool extracted = false, anon = false;
			ast::AggregateDecl * unionDecl = nullptr;

			if ( DeclarationNode * extr = cur->extractAggregate() ) {
				// Handle the case where a SUE declaration is contained within an object or type declaration.

				assert( cur->type );
				// Replace anonymous SUE name with typedef name to prevent anonymous naming problems across translation units.
				if ( cur->type->kind == TypeData::Symbolic && cur->type->symbolic.isTypedef ) {
					assert( extr->type );
					// Handle anonymous aggregates: typedef struct { int i; } foo
					extr->type->qualifiers.reset();		// clear any CVs associated with the aggregate
					if ( extr->type->kind == TypeData::Aggregate && extr->type->aggregate.anon ) {
						delete extr->type->aggregate.name;
						extr->type->aggregate.name = new string( "__anonymous_" + *cur->name );
						extr->type->aggregate.anon = false;
						assert( cur->type->base );
						if ( cur->type->base ) {
							delete cur->type->base->aggInst.aggregate->aggregate.name;
							cur->type->base->aggInst.aggregate->aggregate.name = new string( "__anonymous_" + *cur->name );
							cur->type->base->aggInst.aggregate->aggregate.anon = false;
							cur->type->base->aggInst.aggregate->qualifiers.reset();
						} // if
					} // if
					// Handle anonymous enumeration: typedef enum { A, B, C } foo
					if ( extr->type->kind == TypeData::Enum && extr->type->enumeration.anon ) {
						delete extr->type->enumeration.name;
						extr->type->enumeration.name = new string( "__anonymous_" + *cur->name );
						extr->type->enumeration.anon = false;
						assert( cur->type->base );
						if ( cur->type->base ) {
							delete cur->type->base->aggInst.aggregate->enumeration.name;
							cur->type->base->aggInst.aggregate->enumeration.name = new string( "__anonymous_" + *cur->name );
							cur->type->base->aggInst.aggregate->enumeration.anon = false;
						} // if
					} // if
				} // if

				ast::Decl * decl = extr->build();
				if ( decl ) {
					// Remember the declaration if it is a union aggregate ?
					unionDecl = dynamic_cast<ast::UnionDecl *>( decl );

					*out++ = decl;

					// need to remember the cases where a declaration contains an anonymous aggregate definition
					extracted = true;
					assert( extr->type );
					if ( extr->type->kind == TypeData::Aggregate ) {
						// typedef struct { int A } B is the only case?
						anon = extr->type->aggregate.anon;
					} else if ( extr->type->kind == TypeData::Enum ) {
						// typedef enum { A } B is the only case?
						anon = extr->type->enumeration.anon;
					}
				} // if
				delete extr;
			} // if

			ast::Decl * decl = cur->build();
			if ( decl ) {
				if ( auto typedefDecl = dynamic_cast<ast::TypedefDecl *>( decl ) ) {
					if ( unionDecl ) {					// is the typedef alias a union aggregate ?
						// This code handles a special issue with the attribute transparent_union.
						//
						//    typedef union U { int i; } typedef_name __attribute__(( aligned(16) )) __attribute__(( transparent_union ))
						//
						// Here the attribute aligned goes with the typedef_name, so variables declared of this type are
						// aligned.  However, the attribute transparent_union must be moved from the typedef_name to
						// alias union U.  Currently, this is the only know attribute that must be moved from typedef to
						// alias.

						// If typedef is an alias for a union, then its alias type was hoisted above and remembered.
						if ( auto unionInstType = typedefDecl->base.as<ast::UnionInstType>() ) {
							auto instType = ast::mutate( unionInstType );
							// Remove all transparent_union attributes from typedef and move to alias union.
							for ( auto attr = instType->attributes.begin() ; attr != instType->attributes.end() ; ) { // forward order
								assert( *attr );
								if ( (*attr)->name == "transparent_union" || (*attr)->name == "__transparent_union__" ) {
									unionDecl->attributes.emplace_back( attr->release() );
									attr = instType->attributes.erase( attr );
								} else {
									attr++;
								} // if
							} // for
							typedefDecl->base = instType;
						} // if
					} // if
				} // if

				// don't include anonymous declaration for named aggregates, but do include them for anonymous aggregates, e.g.:
				// struct S {
				//   struct T { int x; }; // no anonymous member
				//   struct { int y; };   // anonymous member
				//   struct T;            // anonymous member
				// };
				if ( ! (extracted && decl->name == "" && ! anon && ! cur->get_inLine()) ) {
					if ( decl->name == "" ) {
						if ( auto dwt = dynamic_cast<ast::DeclWithType *>( decl ) ) {
							if ( auto aggr = dynamic_cast<ast::BaseInstType const *>( dwt->get_type() ) ) {
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
void buildList( DeclarationNode * firstNode, std::vector<ast::ptr<ast::DeclWithType>> & outputList ) {
	SemanticErrorException errors;
	std::back_insert_iterator<std::vector<ast::ptr<ast::DeclWithType>>> out( outputList );

	for ( const DeclarationNode * cur = firstNode; cur; cur = dynamic_cast< DeclarationNode * >( cur->get_next() ) ) {
		try {
			ast::Decl * decl = cur->build();
			assert( decl );
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
			} // if
		} catch( SemanticErrorException & e ) {
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

	switch ( type->kind ) {
	case TypeData::Enum:
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
