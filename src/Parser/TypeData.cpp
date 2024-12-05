//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeData.cpp --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 15:12:51 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Sep 12 22:43:59 2024
// Update Count     : 735
//

#include "TypeData.hpp"

#include <cassert>                   // for assert
#include <ostream>                   // for operator<<, ostream, basic_ostream

#include "AST/Attribute.hpp"         // for Attribute
#include "AST/Decl.hpp"              // for AggregateDecl, ObjectDecl, Type...
#include "AST/Init.hpp"              // for SingleInit, ListInit
#include "AST/Print.hpp"             // for print
#include "AST/Type.hpp"              // for Type
#include "Common/SemanticError.hpp"  // for SemanticError
#include "Common/Utility.hpp"        // for splice, spliceBegin
#include "Common/Iterate.hpp"        // for reverseIterate
#include "Parser/ExpressionNode.hpp" // for ExpressionNode
#include "Parser/StatementNode.hpp"  // for StatementNode

class Attribute;

using namespace std;

// These must harmonize with the corresponding enumerations in the header.
const char * TypeData::basicTypeNames[] = {
	"void", "_Bool", "char", "int", "int128",
	"float", "double", "long double", "float80", "float128",
	"_float16", "_float32", "_float32x", "_float64", "_float64x", "_float128", "_float128x",
	"NoBasicTypeNames"
};
const char * TypeData::complexTypeNames[] = {
	"_Complex", "NoComplexTypeNames", "_Imaginary"
}; // Imaginary unsupported => parse, but make invisible and print error message
const char * TypeData::signednessNames[] = {
	"signed", "unsigned", "NoSignednessNames"
};
const char * TypeData::lengthNames[] = {
	"short", "long", "long long", "NoLengthNames"
};
const char * TypeData::builtinTypeNames[] = {
	"__builtin_va_list", "__auto_type", "zero_t", "one_t", "NoBuiltinTypeNames"
};

TypeData::TypeData( Kind k ) : location( yylloc ), kind( k ), base( nullptr ), forall( nullptr ) /*, PTR1( (void*)(0xdeadbeefdeadbeef)), PTR2( (void*)(0xdeadbeefdeadbeef) ) */ {
	switch ( kind ) {
	case Unknown:
	case Pointer:
	case Reference:
	case EnumConstant:
	case GlobalScope:
	case Basic:
		// No unique data to initialize.
		break;
	case Array:
		array.dimension = nullptr;
		array.isVarLen = false;
		array.isStatic = false;
		break;
	case Function:
		function.params = nullptr;
		function.idList = nullptr;
		function.oldDeclList = nullptr;
		function.body = nullptr;
		function.withExprs = nullptr;
		break;
	case Aggregate:
		aggregate.kind = ast::AggregateDecl::NoAggregate;
		aggregate.name = nullptr;
		aggregate.params = nullptr;
		aggregate.actuals = nullptr;
		aggregate.fields = nullptr;
		aggregate.body = false;
		aggregate.anon = false;
		aggregate.isCfa = false;
		aggregate.hiding = EnumHiding::Visible;
		break;
	case AggregateInst:
		aggInst.aggregate = nullptr;
		aggInst.params = nullptr;
		aggInst.hoistType = false;
		break;
	case Symbolic:
	case SymbolicInst:
		symbolic.name = nullptr;
		symbolic.params = nullptr;
		symbolic.actuals = nullptr;
		symbolic.assertions = nullptr;
		break;
	case Tuple:
		tuple = nullptr;
		break;
	case Typeof:
	case Basetypeof:
		typeexpr = nullptr;
		break;
	case Vtable:
	case Builtin:
		// No unique data to initialize.
		break;
	case Qualified:
		qualified.parent = nullptr;
		qualified.child = nullptr;
		break;
	} // switch
} // TypeData::TypeData


TypeData::~TypeData() {
	delete base;
	delete forall;

	switch ( kind ) {
	case Unknown:
	case Pointer:
	case Reference:
	case EnumConstant:
	case GlobalScope:
	case Basic:
		// No unique data to deconstruct.
		break;
	case Array:
		delete array.dimension;
		break;
	case Function:
		delete function.params;
		delete function.idList;
		delete function.oldDeclList;
		delete function.body;
		delete function.withExprs;
		break;
	case Aggregate:
		delete aggregate.name;
		delete aggregate.params;
		delete aggregate.actuals;
		delete aggregate.fields;
		break;
	case AggregateInst:
		delete aggInst.aggregate;
		delete aggInst.params;
		break;
	case Symbolic:
	case SymbolicInst:
		delete symbolic.name;
		delete symbolic.params;
		delete symbolic.actuals;
		delete symbolic.assertions;
		break;
	case Tuple:
		delete tuple;
		break;
	case Typeof:
	case Basetypeof:
		delete typeexpr;
		break;
	case Vtable:
	case Builtin:
		// No unique data to deconstruct.
		break;
	case Qualified:
		delete qualified.parent;
		delete qualified.child;
		break;
	} // switch
} // TypeData::~TypeData


TypeData * TypeData::clone() const {
	TypeData * newtype = new TypeData( kind );
	newtype->qualifiers = qualifiers;
	newtype->base = maybeCopy( base );
	newtype->forall = maybeCopy( forall );

	switch ( kind ) {
	case Unknown:
	case EnumConstant:
	case Pointer:
	case Reference:
	case GlobalScope:
		// nothing else to copy
		break;
	case Basic:
		newtype->basictype = basictype;
		newtype->complextype = complextype;
		newtype->signedness = signedness;
		newtype->length = length;
		break;
	case Array:
		newtype->array.dimension = maybeCopy( array.dimension );
		newtype->array.isVarLen = array.isVarLen;
		newtype->array.isStatic = array.isStatic;
		break;
	case Function:
		newtype->function.params = maybeCopy( function.params );
		newtype->function.idList = maybeCopy( function.idList );
		newtype->function.oldDeclList = maybeCopy( function.oldDeclList );
		newtype->function.body = maybeCopy( function.body );
		newtype->function.withExprs = maybeCopy( function.withExprs );
		break;
	case Aggregate:
		newtype->aggregate.kind = aggregate.kind;
		newtype->aggregate.name = aggregate.name ? new string( *aggregate.name ) : nullptr;
		newtype->aggregate.params = maybeCopy( aggregate.params );
		newtype->aggregate.actuals = maybeCopy( aggregate.actuals );
		newtype->aggregate.fields = maybeCopy( aggregate.fields );
		newtype->aggregate.attributes = aggregate.attributes;
		newtype->aggregate.body = aggregate.body;
		newtype->aggregate.anon = aggregate.anon;
		newtype->aggregate.isCfa = aggregate.isCfa;
		newtype->aggregate.hiding = aggregate.hiding;
		break;
	case AggregateInst:
		newtype->aggInst.aggregate = maybeCopy( aggInst.aggregate );
		newtype->aggInst.params = maybeCopy( aggInst.params );
		newtype->aggInst.hoistType = aggInst.hoistType;
		break;
	case Symbolic:
	case SymbolicInst:
		newtype->symbolic.name = symbolic.name ? new string( *symbolic.name ) : nullptr;
		newtype->symbolic.params = maybeCopy( symbolic.params );
		newtype->symbolic.actuals = maybeCopy( symbolic.actuals );
		newtype->symbolic.assertions = maybeCopy( symbolic.assertions );
		newtype->symbolic.isTypedef = symbolic.isTypedef;
		break;
	case Tuple:
		newtype->tuple = maybeCopy( tuple );
		break;
	case Typeof:
	case Basetypeof:
		newtype->typeexpr = maybeCopy( typeexpr );
		break;
	case Vtable:
		break;
	case Builtin:
		assert( builtintype == Zero || builtintype == One );
		newtype->builtintype = builtintype;
		break;
	case Qualified:
		newtype->qualified.parent = maybeCopy( qualified.parent );
		newtype->qualified.child = maybeCopy( qualified.child );
		break;
	} // switch
	return newtype;
} // TypeData::clone


void TypeData::print( ostream &os, int indent ) const {
	ast::print( os, qualifiers );

	if ( forall ) {
		os << "forall " << endl;
		forall->printList( os, indent + 4 );
	} // if

	switch ( kind ) {
	case Basic:
		if ( signedness != NoSignedness ) os << signednessNames[ signedness ] << " ";
		if ( length != NoLength ) os << lengthNames[ length ] << " ";
		if ( complextype != NoComplexType ) os << complexTypeNames[ complextype ] << " ";
		if ( basictype != NoBasicType ) os << basicTypeNames[ basictype ] << " ";
		break;
	case Pointer:
		os << "pointer ";
		if ( base ) {
			os << "to ";
			base->print( os, indent );
		} // if
		break;
	case Reference:
		os << "reference ";
		if ( base ) {
			os << "to ";
			base->print( os, indent );
		} // if
		break;
	case Array:
		if ( array.isStatic ) {
			os << "static ";
		} // if
		if ( array.dimension ) {
			os << "array of ";
			array.dimension->printOneLine( os, indent );
		} else if ( array.isVarLen ) {
			os << "variable-length array of ";
		} else {
			os << "open array of ";
		} // if
		if ( base ) {
			base->print( os, indent );
		} // if
		break;
	case Function:
		os << "function" << endl;
		if ( function.params ) {
			os << string( indent + 2, ' ' ) << "with parameters " << endl;
			function.params->printList( os, indent + 4 );
		} else {
			os << string( indent + 2, ' ' ) << "with no parameters" << endl;
		} // if
		if ( function.idList ) {
			os << string( indent + 2, ' ' ) << "with old-style identifier list " << endl;
			function.idList->printList( os, indent + 4 );
		} // if
		if ( function.oldDeclList ) {
			os << string( indent + 2, ' ' ) << "with old-style declaration list " << endl;
			function.oldDeclList->printList( os, indent + 4 );
		} // if
		os << string( indent + 2, ' ' ) << "returning ";
		if ( base ) {
			base->print( os, indent + 4 );
		} else {
			os << "nothing ";
		} // if
		os << endl;
		if ( function.body ) {
			os << string( indent + 2, ' ' ) << "with body " << endl;
			function.body->printList( os, indent + 2 );
		} // if
		break;
	case Aggregate:
		os << ast::AggregateDecl::aggrString( aggregate.kind ) << ' ' << *aggregate.name << endl;
		if ( aggregate.params ) {
			os << string( indent + 2, ' ' ) << "with type parameters" << endl;
			aggregate.params->printList( os, indent + 4 );
		} // if
		if ( aggregate.actuals ) {
			os << string( indent + 2, ' ' ) << "instantiated with actual parameters" << endl;
			aggregate.actuals->printList( os, indent + 4 );
		} // if
		if ( aggregate.fields ) {
			os << string( indent + 2, ' ' ) << "with members" << endl;
			aggregate.fields->printList( os, indent + 4 );
		} // if
		if ( aggregate.body ) {
			os << string( indent + 2, ' ' ) << "with body" << endl;
		} // if
		if ( ! aggregate.attributes.empty() ) {
			os << string( indent + 2, ' ' ) << "with attributes" << endl;
			for ( ast::ptr<ast::Attribute> const & attr : reverseIterate( aggregate.attributes ) ) {
				os << string( indent + 4, ' ' );
				ast::print( os, attr, indent + 2 );
			} // for
		} // if
		break;
	case AggregateInst:
		if ( aggInst.aggregate ) {
			os << "instance of " ;
			aggInst.aggregate->print( os, indent );
		} else {
			os << "instance of an unspecified aggregate ";
		} // if
		if ( aggInst.params ) {
			os << string( indent + 2, ' ' ) << "with parameters" << endl;
			aggInst.params->printList( os, indent + 2 );
		} // if
		break;
	case EnumConstant:
		os << "enumeration constant ";
		break;
	case Symbolic:
		if ( symbolic.isTypedef ) {
			os << "typedef definition ";
		} else {
			os << "type definition ";
		} // if
		if ( symbolic.params ) {
			os << endl << string( indent + 2, ' ' ) << "with parameters" << endl;
			symbolic.params->printList( os, indent + 2 );
		} // if
		if ( symbolic.assertions ) {
			os << endl << string( indent + 2, ' ' ) << "with assertions" << endl;
			symbolic.assertions->printList( os, indent + 4 );
			os << string( indent + 2, ' ' );
		} // if
		if ( base ) {
			os << "for ";
			base->print( os, indent + 2 );
		} // if
		break;
	case SymbolicInst:
		os << *symbolic.name;
		if ( symbolic.actuals ) {
			os << "(";
			symbolic.actuals->printList( os, indent + 2 );
			os << ")";
		} // if
		break;
	case Tuple:
		os << "tuple ";
		if ( tuple ) {
			os << "with members" << endl;
			tuple->printList( os, indent + 2 );
		} // if
		break;
	case Basetypeof:
		os << "base-";
		#if defined(__GNUC__) && __GNUC__ >= 7
			__attribute__((fallthrough));
		#endif
		// FALL THROUGH
	case Typeof:
		os << "type-of expression ";
		if ( typeexpr ) {
			typeexpr->print( os, indent + 2 );
		} // if
		break;
	case Vtable:
		os << "vtable";
		break;
	case Builtin:
		os << builtinTypeNames[builtintype];
		break;
	case GlobalScope:
		break;
	case Qualified:
		qualified.parent->print( os );
		os << ".";
		qualified.child->print( os );
		break;
	case Unknown:
		os << "entity of unknown type ";
		break;
	default:
		os << "internal error: TypeData::print " << kind << endl;
		assert( false );
	} // switch
} // TypeData::print

const std::string * TypeData::leafName() const {
	switch ( kind ) {
	case Unknown:
	case Pointer:
	case Reference:
	case EnumConstant:
	case GlobalScope:
	case Array:
	case Basic:
	case Function:
	case AggregateInst:
	case Tuple:
	case Typeof:
	case Basetypeof:
	case Builtin:
	case Vtable:
		assertf(false, "Tried to get leaf name from kind without a name: %d", kind);
		break;
	case Aggregate:
		return aggregate.name;
	case Symbolic:
	case SymbolicInst:
		return symbolic.name;
	case Qualified:
		return qualified.child->leafName();
	} // switch
	assert(false);
}

TypeData * TypeData::getLastBase() {
	TypeData * cur = this;
	while ( cur->base ) cur = cur->base;
	return cur;
}

void TypeData::setLastBase( TypeData * newBase ) {
	getLastBase()->base = newBase;
}


// Wrap an aggregate up in an instance. Takes and gives ownership.
static TypeData * makeInstance( TypeData * type ) {
	assert( TypeData::Aggregate == type->kind );
	TypeData * out = new TypeData( TypeData::AggregateInst );
	out->aggInst.aggregate = type;
	out->aggInst.params = maybeCopy( type->aggregate.actuals );
	out->aggInst.hoistType = type->aggregate.body;
	out->qualifiers |= type->qualifiers;
	return out;
}


TypeData * build_type_qualifier( ast::CV::Qualifiers tq ) {
	TypeData * type = new TypeData;
	type->qualifiers = tq;
	return type;
}

TypeData * build_basic_type( TypeData::BasicType basic ) {
	TypeData * type = new TypeData( TypeData::Basic );
	type->basictype = basic;
	return type;
}

TypeData * build_complex_type( TypeData::ComplexType complex ) {
	TypeData * type = new TypeData( TypeData::Basic );
	type->complextype = complex;
	return type;
}

TypeData * build_signedness( TypeData::Signedness signedness ) {
	TypeData * type = new TypeData( TypeData::Basic );
	type->signedness = signedness;
	return type;
}

TypeData * build_builtin_type( TypeData::BuiltinType bit ) {
	TypeData * type = new TypeData( TypeData::Builtin );
	type->builtintype = bit;
	return type;
}

TypeData * build_length( TypeData::Length length ) {
	TypeData * type = new TypeData( TypeData::Basic );
	type->length = length;
	return type;
}

TypeData * build_forall( DeclarationNode * forall ) {
	TypeData * type = new TypeData( TypeData::Unknown );
	type->forall = forall;
	return type;
}

TypeData * build_global_scope() {
	return new TypeData( TypeData::GlobalScope );
}

TypeData * build_qualified_type( TypeData * parent, TypeData * child ) {
	TypeData * type = new TypeData( TypeData::Qualified );
	type->qualified.parent = parent;
	type->qualified.child = child;
	return type;
}

TypeData * build_typedef( const std::string * name ) {
	TypeData * type = new TypeData( TypeData::SymbolicInst );
	type->symbolic.name = name;
	type->symbolic.isTypedef = true;
	type->symbolic.actuals = nullptr;
	return type;
}

TypeData * build_type_gen( const std::string * name, ExpressionNode * params ) {
	TypeData * type = new TypeData( TypeData::SymbolicInst );
	type->symbolic.name = name;
	type->symbolic.isTypedef = false;
	type->symbolic.actuals = params;
	return type;
}

TypeData * build_vtable_type( TypeData * base ) {
	TypeData * type = new TypeData( TypeData::Vtable );
	type->base = base;
	return type;
}

// Takes ownership of src.
static void addQualifiersToType( TypeData * dst, TypeData * src ) {
	if ( dst->base ) {
		addQualifiersToType( dst->base, src );
	} else if ( dst->kind == TypeData::Function ) {
		dst->base = src;
		src = nullptr;
    } else {
		dst->qualifiers |= src->qualifiers;
		delete src;
	} // if
}

// Takes ownership of all arguments, gives ownership of return value.
TypeData * addQualifiers( TypeData * dst, TypeData * src ) {
	if ( src->forall ) {
		if ( dst->forall || TypeData::Aggregate != dst->kind ) {
			extend( dst->forall, src->forall );
		} else {
			extend( dst->aggregate.params, src->forall );
		}
		src->forall = nullptr;
	}

	addQualifiersToType( dst, src );
	return dst;
}

// Helper for addType and cloneBaseType.
static void addTypeToType( TypeData *& dst, TypeData *& src ) {
	if ( src->forall && dst->kind == TypeData::Function ) {
		extend( dst->forall, src->forall );
		src->forall = nullptr;
	} // if
	if ( dst->base ) {
		addTypeToType( dst->base, src );
		return;
	}
	switch ( dst->kind ) {
	case TypeData::Unknown:
		src->qualifiers |= dst->qualifiers;
		// LEAKS dst?
		dst = src;
		src = nullptr;
		break;
	case TypeData::Basic:
		dst->qualifiers |= src->qualifiers;
		if ( src->kind != TypeData::Unknown ) {
			assert( src->kind == TypeData::Basic );

			if ( dst->basictype == TypeData::NoBasicType ) {
				dst->basictype = src->basictype;
			} else if ( src->basictype != TypeData::NoBasicType ) {
				SemanticError( yylloc, "multiple declaration types \"%s\" and \"%s\".",
					TypeData::basicTypeNames[ dst->basictype ],
					TypeData::basicTypeNames[ src->basictype ] );
			}
			if ( dst->complextype == TypeData::NoComplexType ) {
				dst->complextype = src->complextype;
			} else if ( src->complextype != TypeData::NoComplexType ) {
				SemanticError( yylloc, "multiple declaration types \"%s\" and \"%s\".",
					TypeData::complexTypeNames[ src->complextype ],
					TypeData::complexTypeNames[ src->complextype ] );
			}
			if ( dst->signedness == TypeData::NoSignedness ) {
				dst->signedness = src->signedness;
			} else if ( src->signedness != TypeData::NoSignedness ) {
				SemanticError( yylloc, "conflicting type specifier \"%s\" and \"%s\".",
					TypeData::signednessNames[ dst->signedness ],
					TypeData::signednessNames[ src->signedness ] );
			}
			if ( dst->length == TypeData::NoLength ) {
				dst->length = src->length;
			} else if ( dst->length == TypeData::Long && src->length == TypeData::Long ) {
				dst->length = TypeData::LongLong;
			} else if ( src->length != TypeData::NoLength ) {
				SemanticError( yylloc, "conflicting type specifier \"%s\" and \"%s\".",
					TypeData::lengthNames[ dst->length ],
					TypeData::lengthNames[ src->length ] );
			}
		} // if
		break;
	default:
		if ( TypeData::Aggregate == src->kind ) {
			dst->base = makeInstance( src );
		} else {
			extend( dst->forall, src->forall );
			src->forall = nullptr;
			dst->base = src;
		}
		src = nullptr;
	} // switch
}

// Takes ownership of all arguments, gives ownership of return value.
TypeData * addType( TypeData * dst, TypeData * src, std::vector<ast::ptr<ast::Attribute>> & attributes ) {
	if ( dst ) {
		addTypeToType( dst, src );
	} else if ( src->kind == TypeData::Aggregate ) {
		// Hide type information aggregate instances.
		dst = makeInstance( src );
		dst->aggInst.aggregate->aggregate.attributes.swap( attributes );
	} else {
		dst = src;
	} // if
	return dst;
}

TypeData * addType( TypeData * dst, TypeData * src ) {
	std::vector<ast::ptr<ast::Attribute>> attributes;
	return addType( dst, src, attributes );
}

// Takes ownership of both arguments, gives ownership of return value.
TypeData * cloneBaseType( TypeData * type, TypeData * other ) {
	TypeData * newType = type->getLastBase()->clone();
	if ( newType->kind == TypeData::AggregateInst ) {
		// don't duplicate members
		assert( newType->aggInst.aggregate->kind == TypeData::Aggregate );
		delete newType->aggInst.aggregate->aggregate.fields;
		newType->aggInst.aggregate->aggregate.fields = nullptr;
		newType->aggInst.aggregate->aggregate.body = false;
		// don't hoist twice
		newType->aggInst.hoistType = false;
	} // if
	newType->forall = maybeCopy( type->forall );

	if ( other ) {
		addTypeToType( other, newType );
		delete newType;
		return other;
	} // if
	return newType;
}

TypeData * makeNewBase( TypeData * type ) {
	return ( TypeData::Aggregate == type->kind ) ? makeInstance( type ) : type;
}


void buildForall(
		const DeclarationNode * firstNode,
		std::vector<ast::ptr<ast::TypeInstType>> &outputList ) {
	{
		std::vector<ast::ptr<ast::Type>> tmpList;
		buildTypeList( firstNode, tmpList );
		for ( auto tmp : tmpList ) {
			outputList.emplace_back(
				strict_dynamic_cast<const ast::TypeInstType *>(
					tmp.release() ) );
		}
	}
	auto n = firstNode;
	for ( auto i = outputList.begin() ;
			i != outputList.end() ;
			++i, n = n->next ) {
		// Only the object type class adds additional assertions.
		if ( n->variable.tyClass != ast::TypeDecl::Otype ) {
			continue;
		}

		ast::TypeDecl const * td = i->strict_as<ast::TypeDecl>();
		std::vector<ast::ptr<ast::DeclWithType>> newAssertions;
		auto mutTypeDecl = ast::mutate( td );
		const CodeLocation & location = mutTypeDecl->location;
		*i = mutTypeDecl;

		// add assertion parameters to `type' tyvars in reverse order
		// add assignment operator:  T * ?=?(T *, T)
		newAssertions.push_back( new ast::FunctionDecl(
			location,
			"?=?",
			{}, // forall
			{}, // assertions
			{
				new ast::ObjectDecl(
					location,
					"",
					new ast::ReferenceType( i->get() ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
				new ast::ObjectDecl(
					location,
					"",
					i->get(),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // params
			{
				new ast::ObjectDecl(
					location,
					"",
					i->get(),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // returns
			(ast::CompoundStmt *)nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		) );

		// add default ctor:  void ?{}(T *)
		newAssertions.push_back( new ast::FunctionDecl(
			location,
			"?{}",
			{}, // forall
			{}, // assertions
			{
				new ast::ObjectDecl(
					location,
					"",
					new ast::ReferenceType( i->get() ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // params
			{}, // returns
			(ast::CompoundStmt *)nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		) );

		// add copy ctor:  void ?{}(T *, T)
		newAssertions.push_back( new ast::FunctionDecl(
			location,
			"?{}",
			{}, // forall
			{}, // assertions
			{
				new ast::ObjectDecl(
					location,
					"",
					new ast::ReferenceType( i->get() ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
				new ast::ObjectDecl(
					location,
					"",
					i->get(),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // params
			{}, // returns
			(ast::CompoundStmt *)nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		) );

		// add dtor:  void ^?{}(T *)
		newAssertions.push_back( new ast::FunctionDecl(
			location,
			"^?{}",
			{}, // forall
			{}, // assertions
			{
				new ast::ObjectDecl(
					location,
					"",
					new ast::ReferenceType( i->get() ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // params
			{}, // returns
			(ast::CompoundStmt *)nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		) );

		spliceBegin( mutTypeDecl->assertions, newAssertions );
	} // for
}


void buildForall(
		const DeclarationNode * firstNode,
		std::vector<ast::ptr<ast::TypeDecl>> &outputForall ) {
	buildList( firstNode, outputForall );
	auto n = firstNode;
	for ( auto i = outputForall.begin() ;
			i != outputForall.end() ;
			++i, n = n->next ) {
		// Only the object type class adds additional assertions.
		if ( n->variable.tyClass != ast::TypeDecl::Otype ) {
			continue;
		}

		ast::TypeDecl const * td = i->strict_as<ast::TypeDecl>();
		std::vector<ast::ptr<ast::DeclWithType>> newAssertions;
		auto mutTypeDecl = ast::mutate( td );
		const CodeLocation & location = mutTypeDecl->location;
		*i = mutTypeDecl;

		// add assertion parameters to `type' tyvars in reverse order
		// add assignment operator:  T * ?=?(T *, T)
		newAssertions.push_back( new ast::FunctionDecl(
			location,
			"?=?",
			{}, // forall
			{}, // assertions
			{
				new ast::ObjectDecl(
					location,
					"",
					new ast::ReferenceType( new ast::TypeInstType( td->name, *i ) ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
				new ast::ObjectDecl(
					location,
					"",
					new ast::TypeInstType( td->name, *i ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // params
			{
				new ast::ObjectDecl(
					location,
					"",
					new ast::TypeInstType( td->name, *i ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // returns
			(ast::CompoundStmt *)nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		) );

		// add default ctor:  void ?{}(T *)
		newAssertions.push_back( new ast::FunctionDecl(
			location,
			"?{}",
			{}, // forall
			{}, // assertions
			{
				new ast::ObjectDecl(
					location,
					"",
					new ast::ReferenceType(
						new ast::TypeInstType( td->name, i->get() ) ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // params
			{}, // returns
			(ast::CompoundStmt *)nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		) );

		// add copy ctor:  void ?{}(T *, T)
		newAssertions.push_back( new ast::FunctionDecl(
			location,
			"?{}",
			{}, // forall
			{}, // assertions
			{
				new ast::ObjectDecl(
					location,
					"",
					new ast::ReferenceType(
						new ast::TypeInstType( td->name, *i ) ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
				new ast::ObjectDecl(
					location,
					"",
					new ast::TypeInstType( td->name, *i ),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // params
			{}, // returns
			(ast::CompoundStmt *)nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		) );

		// add dtor:  void ^?{}(T *)
		newAssertions.push_back( new ast::FunctionDecl(
			location,
			"^?{}",
			{}, // forall
			{}, // assertions
			{
				new ast::ObjectDecl(
					location,
					"",
					new ast::ReferenceType(
						new ast::TypeInstType( i->get() )
					),
					(ast::Init *)nullptr,
					ast::Storage::Classes(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr
				),
			}, // params
			{}, // returns
			(ast::CompoundStmt *)nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		) );

		spliceBegin( mutTypeDecl->assertions, newAssertions );
	} // for
} // buildForall


ast::Type * typebuild( const TypeData * td ) {
	assert( td );
	switch ( td->kind ) {
	case TypeData::Unknown:
		// fill in implicit int
		return new ast::BasicType(
			ast::BasicKind::SignedInt,
			buildQualifiers( td )
		);
	case TypeData::Basic:
		return buildBasicType( td );
	case TypeData::Pointer:
		return buildPointer( td );
	case TypeData::Array:
		return buildArray( td );
	case TypeData::Reference:
		return buildReference( td );
	case TypeData::Function:
		return buildFunctionType( td );
	case TypeData::AggregateInst:
		return buildAggInst( td );
	case TypeData::EnumConstant:
		return new ast::EnumInstType( "", buildQualifiers( td ) );
	case TypeData::SymbolicInst:
		return buildSymbolicInst( td );
	case TypeData::Tuple:
		return buildTuple( td );
	case TypeData::Typeof:
	case TypeData::Basetypeof:
		return buildTypeof( td );
	case TypeData::Vtable:
		return buildVtable( td );
	case TypeData::Builtin:
		switch ( td->builtintype ) {
		case TypeData::Zero:
			return new ast::ZeroType();
		case TypeData::One:
			return new ast::OneType();
		default:
			return new ast::VarArgsType( buildQualifiers( td ) );
		} // switch
	case TypeData::GlobalScope:
		return new ast::GlobalScopeType();
	case TypeData::Qualified:
		return new ast::QualifiedType(
			typebuild( td->qualified.parent ),
			typebuild( td->qualified.child ),
			buildQualifiers( td )
		);
	case TypeData::Symbolic:
	case TypeData::Aggregate:
		assert( false );
	} // switch

	return nullptr;
} // typebuild


TypeData * typeextractAggregate( const TypeData * td, bool toplevel ) {
	TypeData * ret = nullptr;

	switch ( td->kind ) {
	case TypeData::Aggregate:
		if ( ! toplevel && td->aggregate.body ) {
			ret = td->clone();
		} // if
		break;
	case TypeData::AggregateInst:
		if ( td->aggInst.aggregate ) {
			ret = typeextractAggregate( td->aggInst.aggregate, false );
		} // if
		break;
	default:
		if ( td->base ) {
			ret = typeextractAggregate( td->base, false );
		} // if
	} // switch
	return ret;
} // typeextractAggregate


ast::CV::Qualifiers buildQualifiers( const TypeData * td ) {
	return td->qualifiers;
} // buildQualifiers


static string genTSError( string msg, TypeData::BasicType basictype ) {
	SemanticError( yylloc, "invalid type specifier \"%s\" for type \"%s\".", msg.c_str(), TypeData::basicTypeNames[basictype] );
} // genTSError

ast::Type * buildBasicType( const TypeData * td ) {
	ast::BasicKind ret;

	switch ( td->basictype ) {
	case TypeData::Void:
		if ( td->signedness != TypeData::NoSignedness ) {
			genTSError( TypeData::signednessNames[ td->signedness ], td->basictype );
		} // if
		if ( td->length != TypeData::NoLength ) {
			genTSError( TypeData::lengthNames[ td->length ], td->basictype );
		} // if
		return new ast::VoidType( buildQualifiers( td ) );
		break;

	case TypeData::Bool:
		if ( td->signedness != TypeData::NoSignedness ) {
			genTSError( TypeData::signednessNames[ td->signedness ], td->basictype );
		} // if
		if ( td->length != TypeData::NoLength ) {
			genTSError( TypeData::lengthNames[ td->length ], td->basictype );
		} // if

		ret = ast::BasicKind::Bool;
		break;

	case TypeData::Char:
		// C11 Standard 6.2.5.15: The three types char, signed char, and unsigned char are collectively called the
		// character types. The implementation shall define char to have the same range, representation, and behavior as
		// either signed char or unsigned char.
		static ast::BasicKind chartype[] = { ast::BasicKind::SignedChar, ast::BasicKind::UnsignedChar, ast::BasicKind::Char };

		if ( td->length != TypeData::NoLength ) {
			genTSError( TypeData::lengthNames[ td->length ], td->basictype );
		} // if

		ret = chartype[ td->signedness ];
		break;

	case TypeData::Int:
		static ast::BasicKind inttype[2][4] = {
			{ ast::BasicKind::ShortSignedInt, ast::BasicKind::LongSignedInt, ast::BasicKind::LongLongSignedInt, ast::BasicKind::SignedInt },
			{ ast::BasicKind::ShortUnsignedInt, ast::BasicKind::LongUnsignedInt, ast::BasicKind::LongLongUnsignedInt, ast::BasicKind::UnsignedInt },
		};

	Integral: ;
		if ( td->signedness == TypeData::NoSignedness ) {
			const_cast<TypeData *>(td)->signedness = TypeData::Signed;
		} // if
		ret = inttype[ td->signedness ][ td->length ];
		break;

	case TypeData::Int128:
		ret = td->signedness == TypeData::Unsigned ? ast::BasicKind::UnsignedInt128 : ast::BasicKind::SignedInt128;
		if ( td->length != TypeData::NoLength ) {
			genTSError( TypeData::lengthNames[ td->length ], td->basictype );
		} // if
		break;

	case TypeData::Float:
	case TypeData::Double:
	case TypeData::LongDouble:					// not set until below
	case TypeData::Float80:
	case TypeData::uuFloat128:
	case TypeData::Float16:
	case TypeData::Float32:
	case TypeData::Float32x:
	case TypeData::Float64:
	case TypeData::Float64x:
	case TypeData::Float128:
	case TypeData::Float128x:
		static ast::BasicKind floattype[2][12] = {
			{ ast::BasicKind::FloatComplex, ast::BasicKind::DoubleComplex, ast::BasicKind::LongDoubleComplex, (ast::BasicKind)-1, (ast::BasicKind)-1, ast::BasicKind::Float16Complex, ast::BasicKind::Float32Complex, ast::BasicKind::Float32xComplex, ast::BasicKind::Float64Complex, ast::BasicKind::Float64xComplex, ast::BasicKind::Float128Complex, ast::BasicKind::Float128xComplex, },
			{ ast::BasicKind::Float, ast::BasicKind::Double, ast::BasicKind::LongDouble, ast::BasicKind::Float80, ast::BasicKind::uuFloat128, ast::BasicKind::Float16, ast::BasicKind::Float32, ast::BasicKind::Float32x, ast::BasicKind::Float64, ast::BasicKind::Float64x, ast::BasicKind::Float128, ast::BasicKind::Float128x, },
		};

	FloatingPoint: ;
		if ( td->signedness != TypeData::NoSignedness ) {
			genTSError( TypeData::signednessNames[ td->signedness ], td->basictype );
		} // if
		if ( td->length == TypeData::Short || td->length == TypeData::LongLong ) {
			genTSError( TypeData::lengthNames[ td->length ], td->basictype );
		} // if
		if ( td->basictype != TypeData::Double && td->length == TypeData::Long ) {
			genTSError( TypeData::lengthNames[ td->length ], td->basictype );
		} // if
		if ( td->complextype == TypeData::Imaginary ) {
			genTSError( TypeData::complexTypeNames[ td->complextype ], td->basictype );
		} // if
		if ( (td->basictype == TypeData::Float80 || td->basictype == TypeData::uuFloat128) && td->complextype == TypeData::Complex ) { // gcc unsupported
			genTSError( TypeData::complexTypeNames[ td->complextype ], td->basictype );
		} // if
		if ( td->length == TypeData::Long ) {
			const_cast<TypeData *>(td)->basictype = TypeData::LongDouble;
		} // if

		ret = floattype[ td->complextype ][ td->basictype - TypeData::Float ];
		//printf( "XXXX %d %d %d %d\n", td->complextype, td->basictype, TypeData::Float, ret );
		break;

	case TypeData::NoBasicType:
		// No basic type in declaration => default double for Complex/Imaginary and int type for integral types
		if ( td->complextype == TypeData::Complex || td->complextype == TypeData::Imaginary ) {
			const_cast<TypeData *>(td)->basictype = TypeData::Double;
			goto FloatingPoint;
		} // if

		const_cast<TypeData *>(td)->basictype = TypeData::Int;
		goto Integral;

	  case TypeData::Float32x4: case TypeData::Float64x2: case TypeData::Svfloat32: case TypeData:: Svfloat64: case TypeData::Svbool:
		return nullptr;
	default:
		assertf( false, "unknown basic type" );
		return nullptr;
	} // switch

	ast::BasicType * bt = new ast::BasicType( ret, buildQualifiers( td ) );
	return bt;
} // buildBasicType


static ast::Type * buildDefaultType( const TypeData * td ) {
	return ( td ) ? typebuild( td ) : new ast::BasicType( ast::BasicKind::SignedInt );
} // buildDefaultType


ast::PointerType * buildPointer( const TypeData * td ) {
	return new ast::PointerType(
		buildDefaultType( td->base ),
		buildQualifiers( td )
	);
} // buildPointer


ast::ArrayType * buildArray( const TypeData * td ) {
	return new ast::ArrayType(
		buildDefaultType( td->base ),
		maybeBuild( td->array.dimension ),
		td->array.isVarLen ? ast::VariableLen : ast::FixedLen,
		td->array.isStatic ? ast::StaticDim : ast::DynamicDim,
		buildQualifiers( td )
	);
} // buildArray


ast::ReferenceType * buildReference( const TypeData * td ) {
	return new ast::ReferenceType(
		buildDefaultType( td->base ),
		buildQualifiers( td )
	);
} // buildReference


ast::AggregateDecl * buildAggregate( const TypeData * td, std::vector<ast::ptr<ast::Attribute>> attributes, ast::Linkage::Spec linkage ) {
	assert( td->kind == TypeData::Aggregate );
	ast::AggregateDecl * at;
	switch ( td->aggregate.kind ) {
	case ast::AggregateDecl::Struct:
	case ast::AggregateDecl::Coroutine:
	case ast::AggregateDecl::Exception:
	case ast::AggregateDecl::Generator:
	case ast::AggregateDecl::Monitor:
	case ast::AggregateDecl::Thread:
		at = new ast::StructDecl( td->location,
			*td->aggregate.name,
			td->aggregate.kind,
			std::move( attributes ),
			linkage
		);
		buildForall( td->aggregate.params, at->params );
		break;
	case ast::AggregateDecl::Union:
		at = new ast::UnionDecl( td->location,
			*td->aggregate.name,
			std::move( attributes ),
			linkage
		);
		buildForall( td->aggregate.params, at->params );
		break;
	case ast::AggregateDecl::Enum:
		return buildEnum( td, std::move( attributes ), linkage );
	case ast::AggregateDecl::Trait:
		at = new ast::TraitDecl( td->location,
			*td->aggregate.name,
			std::move( attributes ),
			linkage
		);
		buildList( td->aggregate.params, at->params );
		break;
	default:
		assert( false );
	} // switch

	buildList( td->aggregate.fields, at->members );
	at->set_body( td->aggregate.body );

	return at;
} // buildAggregate


ast::BaseInstType * buildComAggInst(
		const TypeData * td,
		std::vector<ast::ptr<ast::Attribute>> && attributes,
		ast::Linkage::Spec linkage ) {
	switch ( td->kind ) {
	case TypeData::Aggregate:
		if ( td->aggregate.body ) {
			ast::AggregateDecl * typedecl =
				buildAggregate( td, std::move( attributes ), linkage );
			switch ( td->aggregate.kind ) {
			case ast::AggregateDecl::Struct:
			case ast::AggregateDecl::Coroutine:
			case ast::AggregateDecl::Monitor:
			case ast::AggregateDecl::Thread:
				return new ast::StructInstType(
					strict_dynamic_cast<ast::StructDecl *>( typedecl ),
					buildQualifiers( td )
				);
			case ast::AggregateDecl::Union:
				return new ast::UnionInstType(
					strict_dynamic_cast<ast::UnionDecl *>( typedecl ),
					buildQualifiers( td )
				);
			case ast::AggregateDecl::Enum:
				return new ast::EnumInstType(
					strict_dynamic_cast<ast::EnumDecl *>( typedecl ),
					buildQualifiers( td )
				);
			case ast::AggregateDecl::Trait:
				assert( false );
				break;
			default:
				assert( false );
			} // switch
		} else {
			switch ( td->aggregate.kind ) {
			case ast::AggregateDecl::Struct:
			case ast::AggregateDecl::Coroutine:
			case ast::AggregateDecl::Monitor:
			case ast::AggregateDecl::Thread:
				return new ast::StructInstType(
					*td->aggregate.name,
					buildQualifiers( td )
				);
			case ast::AggregateDecl::Union:
				return new ast::UnionInstType(
					*td->aggregate.name,
					buildQualifiers( td )
				);
			case ast::AggregateDecl::Enum:
				return new ast::EnumInstType(
					*td->aggregate.name,
					buildQualifiers( td )
				);
			case ast::AggregateDecl::Trait:
				return new ast::TraitInstType(
					*td->aggregate.name,
					buildQualifiers( td )
				);
			default:
				assert( false );
			} // switch
			break;
		} // if
		break;
	default:
		assert( false );
	} // switch
	assert( false );
} // buildAggInst


ast::BaseInstType * buildAggInst( const TypeData * td ) {
	assert( td->kind == TypeData::AggregateInst );

	ast::BaseInstType * ret = nullptr;
	TypeData * type = td->aggInst.aggregate;
	switch ( type->kind ) {
	case TypeData::Aggregate:
		switch ( type->aggregate.kind ) {
		case ast::AggregateDecl::Struct:
		case ast::AggregateDecl::Coroutine:
		case ast::AggregateDecl::Monitor:
		case ast::AggregateDecl::Thread:
			ret = new ast::StructInstType(
				*type->aggregate.name,
				buildQualifiers( type )
			);
			break;
		case ast::AggregateDecl::Union:
			ret = new ast::UnionInstType(
				*type->aggregate.name,
				buildQualifiers( type )
			);
			break;
		case ast::AggregateDecl::Enum:
			ret = new ast::EnumInstType(
				*type->aggregate.name,
				buildQualifiers( type )
			);
			break;
		case ast::AggregateDecl::Trait:
			ret = new ast::TraitInstType(
				*type->aggregate.name,
				buildQualifiers( type )
			);
			break;
		default:
			assert( false );
		} // switch
		break;
	default:
		assert( false );
	} // switch

	ret->hoistType = td->aggInst.hoistType;
	buildList( td->aggInst.params, ret->params );
	return ret;
} // buildAggInst


ast::NamedTypeDecl * buildSymbolic(
		const TypeData * td,
		std::vector<ast::ptr<ast::Attribute>> attributes,
		const std::string & name,
		ast::Storage::Classes scs,
		ast::Linkage::Spec linkage ) {
	assert( td->kind == TypeData::Symbolic );
	ast::NamedTypeDecl * ret;
	assert( td->base );
	if ( td->symbolic.isTypedef ) {
		ret = new ast::TypedefDecl(
			td->location,
			name,
			scs,
			typebuild( td->base ),
			linkage
		);
	} else {
		ret = new ast::TypeDecl(
			td->location,
			name,
			scs,
			typebuild( td->base ),
			ast::TypeDecl::Dtype,
			true
		);
	} // if
	buildList( td->symbolic.assertions, ret->assertions );
	splice( ret->base.get_and_mutate()->attributes, attributes );
	return ret;
} // buildSymbolic


ast::EnumDecl * buildEnum(
		const TypeData * td,
		std::vector<ast::ptr<ast::Attribute>> && attributes,
		ast::Linkage::Spec linkage ) {
	assert( td->kind == TypeData::Aggregate );
	assert( td->aggregate.kind == ast::AggregateDecl::Enum );
	ast::ptr<ast::Type> baseType;
	if ( td->base ) {
		if ( td->base->kind == TypeData::Aggregate ) {
			baseType = buildComAggInst( td->base, copy(attributes), linkage );
		} else {
			baseType = typebuild( td->base );
		}
	}
	ast::EnumDecl * ret = new ast::EnumDecl(
		td->location,
		*td->aggregate.name,
		td->aggregate.isCfa,
		std::move( attributes ),
		linkage,
		baseType
	);
	buildList( td->aggregate.fields, ret->members );
	auto members = ret->members.begin();
	ret->hide = td->aggregate.hiding == EnumHiding::Hide ? ast::EnumDecl::EnumHiding::Hide : ast::EnumDecl::EnumHiding::Visible;
	for ( const DeclarationNode * cur = td->aggregate.fields ; cur != nullptr ; cur = cur->next, ++members ) {
		if (cur->enumInLine) continue;
		ast::Decl * member = members->get_and_mutate();
		ast::ObjectDecl * object = strict_dynamic_cast<ast::ObjectDecl *>( member );
		object->isHidden = ast::EnumDecl::EnumHiding::Hide == ret->hide;
		object->isMember = true;
		if ( ret->isOpaque() && cur->has_enumeratorValue() ) {
			SemanticError( td->location, "Opague cannot have an explicit initializer value." );
		} else if ( cur->has_enumeratorValue() ) {
			ast::Expr * initValue;
			if ( ret->isCfa && ret->base ) {
				CodeLocation location = cur->enumeratorValue->location;
				initValue = new ast::CastExpr( location, maybeMoveBuild( cur->consume_enumeratorValue() ), ret->base );
			} else {
				initValue = maybeMoveBuild( cur->consume_enumeratorValue() );
			}
			object->init = new ast::SingleInit(
				td->location,
				initValue,
				ast::NoConstruct
			);
		}
		// else cur is a List Initializer and has been set as init in buildList()
		// if
	} // for
	ret->body = td->aggregate.body;
	return ret;
} // buildEnum


ast::TypeInstType * buildSymbolicInst( const TypeData * td ) {
	assert( td->kind == TypeData::SymbolicInst );
	ast::TypeInstType * ret = new ast::TypeInstType(
		*td->symbolic.name,
		ast::TypeDecl::Dtype,
		buildQualifiers( td )
	);
	buildList( td->symbolic.actuals, ret->params );
	return ret;
} // buildSymbolicInst


ast::TupleType * buildTuple( const TypeData * td ) {
	assert( td->kind == TypeData::Tuple );
	std::vector<ast::ptr<ast::Type>> types;
	buildTypeList( td->tuple, types );
	ast::TupleType * ret = new ast::TupleType(
		std::move( types ),
		buildQualifiers( td )
	);
	return ret;
} // buildTuple


ast::TypeofType * buildTypeof( const TypeData * td ) {
	assert( td->kind == TypeData::Typeof || td->kind == TypeData::Basetypeof );
	assert( td->typeexpr );
	return new ast::TypeofType(
		td->typeexpr->build(),
		td->kind == TypeData::Typeof
			? ast::TypeofType::Typeof : ast::TypeofType::Basetypeof,
		buildQualifiers( td )
	);
} // buildTypeof


ast::VTableType * buildVtable( const TypeData * td ) {
	assert( td->base );
	return new ast::VTableType(
		typebuild( td->base ),
		buildQualifiers( td )
	);
} // buildVtable


// The argument flag (is/is not var-args) of a computed property.
static ast::ArgumentFlag buildArgumentFlag( const TypeData * td ) {
	assert( td->kind == TypeData::Function );
	bool isVarArgs = !td->function.params || td->function.params->hasEllipsis;
	return (isVarArgs) ? ast::VariableArgs : ast::FixedArgs;
}


// Wrapper to convert the void parameter into the empty explicit list.
static void buildParamList( DeclarationNode * decl,
		std::vector<ast::ptr<ast::DeclWithType>> & params ) {
	buildList( decl, params );
	if ( 1 == params.size() && params[0]->get_type()->isVoid() ) {
		params.pop_back();
	}
}


ast::FunctionDecl * buildFunctionDecl(
		const TypeData * td,
		const string &name,
		ast::Storage::Classes scs,
		ast::Function::Specs funcSpec,
		ast::Linkage::Spec linkage,
		ast::Expr * asmName,
		std::vector<ast::ptr<ast::Attribute>> && attributes ) {
	assert( td->kind == TypeData::Function );
	ast::CV::Qualifiers cvq = buildQualifiers( td );
	std::vector<ast::ptr<ast::TypeDecl>> forall;
	std::vector<ast::ptr<ast::DeclWithType>> assertions;
	std::vector<ast::ptr<ast::DeclWithType>> params;
	std::vector<ast::ptr<ast::DeclWithType>> returns;
	buildParamList( td->function.params, params );
	buildForall( td->forall, forall );
	// Functions do not store their assertions there anymore.
	for ( ast::ptr<ast::TypeDecl> & type_param : forall ) {
		auto mut = type_param.get_and_mutate();
		splice( assertions, mut->assertions );
	}
	if ( td->base ) {
		switch ( td->base->kind ) {
		case TypeData::Tuple:
			buildList( td->base->tuple, returns );
			break;
		default:
			returns.push_back( dynamic_cast<ast::DeclWithType *>(
				buildDecl(
					td->base,
					"",
					ast::Storage::Classes(),
					(ast::Expr *)nullptr, // bitfieldWidth
					ast::Function::Specs(),
					ast::Linkage::Cforall,
					(ast::Expr *)nullptr // asmName
				)
			) );
		} // switch
	} else {
		returns.push_back( new ast::ObjectDecl(
			td->location,
			"",
			new ast::BasicType( ast::BasicKind::SignedInt ),
			(ast::Init *)nullptr,
			ast::Storage::Classes(),
			ast::Linkage::Cforall
		) );
	} // if
	ast::Stmt * stmt = maybeBuild( td->function.body );
	ast::CompoundStmt * body = dynamic_cast<ast::CompoundStmt *>( stmt );
	ast::FunctionDecl * decl = new ast::FunctionDecl( td->location,
		name,
		std::move( forall ),
		std::move( assertions ),
		std::move( params ),
		std::move( returns ),
		body,
		scs,
		linkage,
		std::move( attributes ),
		funcSpec,
		buildArgumentFlag( td )
	);
	buildList( td->function.withExprs, decl->withExprs );
	decl->asmName = asmName;
	// This may be redundant on a declaration.
	decl->type.get_and_mutate()->qualifiers = cvq;
	return decl;
} // buildFunctionDecl


ast::Decl * buildDecl(
		const TypeData * td,
		const string &name,
		ast::Storage::Classes scs,
		ast::Expr * bitfieldWidth,
		ast::Function::Specs funcSpec,
		ast::Linkage::Spec linkage,
		ast::Expr * asmName,
		ast::Init * init,
		std::vector<ast::ptr<ast::Attribute>> && attributes ) {
	if ( td->kind == TypeData::Function ) {
		if ( td->function.idList ) {					// KR function ?
			buildKRFunction( td->function );			// transform into C11 function
		} // if

		return buildFunctionDecl(
			td, name, scs, funcSpec, linkage,
			asmName, std::move( attributes ) );
	} else if ( td->kind == TypeData::Aggregate ) {
		return buildAggregate( td, std::move( attributes ), linkage );
	} else if ( td->kind == TypeData::Symbolic ) {
		return buildSymbolic( td, std::move( attributes ), name, scs, linkage );
	} else {
		auto ret = new ast::ObjectDecl( td->location,
			name,
			typebuild( td ),
			init,
			scs,
			linkage,
			bitfieldWidth,
			std::move( attributes )
		);
		ret->asmName = asmName;
		return ret;
	} // if
	return nullptr;
} // buildDecl


ast::FunctionType * buildFunctionType( const TypeData * td ) {
	assert( td->kind == TypeData::Function );
	ast::FunctionType * ft = new ast::FunctionType(
		buildArgumentFlag( td ),
		buildQualifiers( td )
	);
	buildTypeList( td->function.params, ft->params );
	buildForall( td->forall, ft->forall );
	if ( td->base ) {
		switch ( td->base->kind ) {
		case TypeData::Tuple:
			buildTypeList( td->base->tuple, ft->returns );
			break;
		default:
			ft->returns.push_back( typebuild( td->base ) );
			break;
		} // switch
	} else {
		ft->returns.push_back(
			new ast::BasicType( ast::BasicKind::SignedInt ) );
	} // if
	return ft;
} // buildFunctionType


// Transform KR routine declarations into C99 routine declarations:
//
//    rtn( a, b, c ) int a, c; double b {}  =>  int rtn( int a, double c, int b ) {}
//
// The type information for each post-declaration is moved to the corresponding pre-parameter and the post-declaration
// is deleted. Note, the order of the parameter names may not be the same as the declaration names. Duplicate names and
// extra names are disallowed.
//
// Note, there is no KR routine-prototype syntax:
//
//    rtn( a, b, c ) int a, c; double b; // invalid KR prototype
//    rtn(); // valid KR prototype

void buildKRFunction( const TypeData::Function_t & function ) {
	assert( ! function.params );
	// loop over declaration first as it is easier to spot errors
	for ( DeclarationNode * decl = function.oldDeclList; decl != nullptr; decl = decl->next ) {
		// scan ALL parameter names for each declaration name to check for duplicates
		for ( DeclarationNode * param = function.idList; param != nullptr; param = param->next ) {
			if ( *decl->name == *param->name ) {
				// type set => parameter name already transformed by a declaration names so there is a duplicate
				// declaration name attempting a second transformation
				if ( param->type ) SemanticError( param->location, "duplicate declaration name \"%s\".", param->name->c_str() );
				// declaration type reset => declaration already transformed by a parameter name so there is a duplicate
				// parameter name attempting a second transformation
				if ( ! decl->type ) SemanticError( param->location, "duplicate parameter name \"%s\".", param->name->c_str() );
				param->type = decl->type;				// set copy declaration type to parameter type
				decl->type = nullptr;					// reset declaration type
				// Copy and reset attributes from declaration to parameter:
				splice( param->attributes, decl->attributes );
			} // if
		} // for
		// declaration type still set => type not moved to a matching parameter so there is a missing parameter name
		if ( decl->type ) SemanticError( decl->location, "missing name in parameter list %s", decl->name->c_str() );
	} // for

	// Parameter names without a declaration default to type int:
	//
	//    rtb( a, b, c ) const char * b; {} => int rtn( int a, const char * b, int c ) {}

	for ( DeclarationNode * param = function.idList; param != nullptr; param = param->next ) {
		if ( ! param->type ) {							// generate type int for empty parameter type
			param->type = new TypeData( TypeData::Basic );
			param->type->basictype = TypeData::Int;
		} // if
	} // for

	function.params = function.idList;					// newly modified idList becomes parameters
	function.idList = nullptr;							// idList now empty
	delete function.oldDeclList;						// deletes entire list
	function.oldDeclList = nullptr;						// reset
} // buildKRFunction

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
