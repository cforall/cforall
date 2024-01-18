//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypeData.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 15:12:51 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Dec 14 18:59:12 2023
// Update Count     : 684
//

#include "TypeData.h"

#include <cassert>                 // for assert
#include <ostream>                 // for operator<<, ostream, basic_ostream

#include "AST/Decl.hpp"            // for AggregateDecl, ObjectDecl, TypeDe...
#include "AST/Init.hpp"            // for SingleInit, ListInit
#include "AST/Print.hpp"           // for print
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for splice, spliceBegin
#include "Parser/ExpressionNode.h" // for ExpressionNode
#include "Parser/StatementNode.h"  // for StatementNode

class Attribute;

using namespace std;

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
	case Enum:
		enumeration.name = nullptr;
		enumeration.constants = nullptr;
		enumeration.body = false;
		enumeration.anon = false;
		break;
	case Aggregate:
		aggregate.kind = ast::AggregateDecl::NoAggregate;
		aggregate.name = nullptr;
		aggregate.params = nullptr;
		aggregate.actuals = nullptr;
		aggregate.fields = nullptr;
		aggregate.body = false;
		aggregate.tagged = false;
		aggregate.parent = nullptr;
		aggregate.anon = false;
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
	case Enum:
		delete enumeration.name;
		delete enumeration.constants;
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
		newtype->aggregate.body = aggregate.body;
		newtype->aggregate.anon = aggregate.anon;
		newtype->aggregate.tagged = aggregate.tagged;
		newtype->aggregate.parent = aggregate.parent ? new string( *aggregate.parent ) : nullptr;
		break;
	case AggregateInst:
		newtype->aggInst.aggregate = maybeCopy( aggInst.aggregate );
		newtype->aggInst.params = maybeCopy( aggInst.params );
		newtype->aggInst.hoistType = aggInst.hoistType;
		break;
	case Enum:
		newtype->enumeration.name = enumeration.name ? new string( *enumeration.name ) : nullptr;
		newtype->enumeration.constants = maybeCopy( enumeration.constants );
		newtype->enumeration.body = enumeration.body;
		newtype->enumeration.anon = enumeration.anon;
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
		assert( builtintype == DeclarationNode::Zero || builtintype == DeclarationNode::One );
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
		if ( signedness != DeclarationNode::NoSignedness ) os << DeclarationNode::signednessNames[ signedness ] << " ";
		if ( length != DeclarationNode::NoLength ) os << DeclarationNode::lengthNames[ length ] << " ";
		if ( complextype != DeclarationNode::NoComplexType ) os << DeclarationNode::complexTypeNames[ complextype ] << " ";
		if ( basictype != DeclarationNode::NoBasicType ) os << DeclarationNode::basicTypeNames[ basictype ] << " ";
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
			os << string( indent + 2, ' ' ) << " with body" << endl;
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
	case Enum:
		os << "enumeration " << *enumeration.name << endl;;
		if ( enumeration.constants ) {
			os << "with constants" << endl;
			enumeration.constants->printList( os, indent + 2 );
		} // if
		if ( enumeration.body ) {
			os << string( indent + 2, ' ' ) << " with body" << endl;
		} // if
		if ( base ) {
			os << "for ";
			base->print( os, indent + 2 );
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
		os << DeclarationNode::builtinTypeNames[builtintype];
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
	case Enum:
		return enumeration.name;
	case Symbolic:
	case SymbolicInst:
		return symbolic.name;
	case Qualified:
		return qualified.child->leafName();
	} // switch
	assert(false);
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
			++i, n = (DeclarationNode*)n->get_next() ) {
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
			++i, n = (DeclarationNode*)n->get_next() ) {
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
			ast::BasicType::SignedInt,
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
		case DeclarationNode::Zero:
			return new ast::ZeroType();
		case DeclarationNode::One:
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
	case TypeData::Enum:
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
	case TypeData::Enum:
		if ( ! toplevel && td->enumeration.body ) {
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


static string genTSError( string msg, DeclarationNode::BasicType basictype ) {
	SemanticError( yylloc, "invalid type specifier \"%s\" for type \"%s\".", msg.c_str(), DeclarationNode::basicTypeNames[basictype] );
} // genTSError

ast::Type * buildBasicType( const TypeData * td ) {
	ast::BasicType::Kind ret;

	switch ( td->basictype ) {
	case DeclarationNode::Void:
		if ( td->signedness != DeclarationNode::NoSignedness ) {
			genTSError( DeclarationNode::signednessNames[ td->signedness ], td->basictype );
		} // if
		if ( td->length != DeclarationNode::NoLength ) {
			genTSError( DeclarationNode::lengthNames[ td->length ], td->basictype );
		} // if
		return new ast::VoidType( buildQualifiers( td ) );
		break;

	case DeclarationNode::Bool:
		if ( td->signedness != DeclarationNode::NoSignedness ) {
			genTSError( DeclarationNode::signednessNames[ td->signedness ], td->basictype );
		} // if
		if ( td->length != DeclarationNode::NoLength ) {
			genTSError( DeclarationNode::lengthNames[ td->length ], td->basictype );
		} // if

		ret = ast::BasicType::Bool;
		break;

	case DeclarationNode::Char:
		// C11 Standard 6.2.5.15: The three types char, signed char, and unsigned char are collectively called the
		// character types. The implementation shall define char to have the same range, representation, and behavior as
		// either signed char or unsigned char.
		static ast::BasicType::Kind chartype[] = { ast::BasicType::SignedChar, ast::BasicType::UnsignedChar, ast::BasicType::Char };

		if ( td->length != DeclarationNode::NoLength ) {
			genTSError( DeclarationNode::lengthNames[ td->length ], td->basictype );
		} // if

		ret = chartype[ td->signedness ];
		break;

	case DeclarationNode::Int:
		static ast::BasicType::Kind inttype[2][4] = {
			{ ast::BasicType::ShortSignedInt, ast::BasicType::LongSignedInt, ast::BasicType::LongLongSignedInt, ast::BasicType::SignedInt },
			{ ast::BasicType::ShortUnsignedInt, ast::BasicType::LongUnsignedInt, ast::BasicType::LongLongUnsignedInt, ast::BasicType::UnsignedInt },
		};

	Integral: ;
		if ( td->signedness == DeclarationNode::NoSignedness ) {
			const_cast<TypeData *>(td)->signedness = DeclarationNode::Signed;
		} // if
		ret = inttype[ td->signedness ][ td->length ];
		break;

	case DeclarationNode::Int128:
		ret = td->signedness == DeclarationNode::Unsigned ? ast::BasicType::UnsignedInt128 : ast::BasicType::SignedInt128;
		if ( td->length != DeclarationNode::NoLength ) {
			genTSError( DeclarationNode::lengthNames[ td->length ], td->basictype );
		} // if
		break;

	case DeclarationNode::Float:
	case DeclarationNode::Double:
	case DeclarationNode::LongDouble:					// not set until below
	case DeclarationNode::uuFloat80:
	case DeclarationNode::uuFloat128:
	case DeclarationNode::uFloat16:
	case DeclarationNode::uFloat32:
	case DeclarationNode::uFloat32x:
	case DeclarationNode::uFloat64:
	case DeclarationNode::uFloat64x:
	case DeclarationNode::uFloat128:
	case DeclarationNode::uFloat128x:
		static ast::BasicType::Kind floattype[2][12] = {
			{ ast::BasicType::FloatComplex, ast::BasicType::DoubleComplex, ast::BasicType::LongDoubleComplex, (ast::BasicType::Kind)-1, (ast::BasicType::Kind)-1, ast::BasicType::uFloat16Complex, ast::BasicType::uFloat32Complex, ast::BasicType::uFloat32xComplex, ast::BasicType::uFloat64Complex, ast::BasicType::uFloat64xComplex, ast::BasicType::uFloat128Complex, ast::BasicType::uFloat128xComplex, },
			{ ast::BasicType::Float, ast::BasicType::Double, ast::BasicType::LongDouble, ast::BasicType::uuFloat80, ast::BasicType::uuFloat128, ast::BasicType::uFloat16, ast::BasicType::uFloat32, ast::BasicType::uFloat32x, ast::BasicType::uFloat64, ast::BasicType::uFloat64x, ast::BasicType::uFloat128, ast::BasicType::uFloat128x, },
		};

	FloatingPoint: ;
		if ( td->signedness != DeclarationNode::NoSignedness ) {
			genTSError( DeclarationNode::signednessNames[ td->signedness ], td->basictype );
		} // if
		if ( td->length == DeclarationNode::Short || td->length == DeclarationNode::LongLong ) {
			genTSError( DeclarationNode::lengthNames[ td->length ], td->basictype );
		} // if
		if ( td->basictype != DeclarationNode::Double && td->length == DeclarationNode::Long ) {
			genTSError( DeclarationNode::lengthNames[ td->length ], td->basictype );
		} // if
		if ( td->complextype == DeclarationNode::Imaginary ) {
			genTSError( DeclarationNode::complexTypeNames[ td->complextype ], td->basictype );
		} // if
		if ( (td->basictype == DeclarationNode::uuFloat80 || td->basictype == DeclarationNode::uuFloat128) && td->complextype == DeclarationNode::Complex ) { // gcc unsupported
			genTSError( DeclarationNode::complexTypeNames[ td->complextype ], td->basictype );
		} // if
		if ( td->length == DeclarationNode::Long ) {
			const_cast<TypeData *>(td)->basictype = DeclarationNode::LongDouble;
		} // if

		ret = floattype[ td->complextype ][ td->basictype - DeclarationNode::Float ];
		//printf( "XXXX %d %d %d %d\n", td->complextype, td->basictype, DeclarationNode::Float, ret );
		break;

	case DeclarationNode::NoBasicType:
		// No basic type in declaration => default double for Complex/Imaginary and int type for integral types
		if ( td->complextype == DeclarationNode::Complex || td->complextype == DeclarationNode::Imaginary ) {
			const_cast<TypeData *>(td)->basictype = DeclarationNode::Double;
			goto FloatingPoint;
		} // if

		const_cast<TypeData *>(td)->basictype = DeclarationNode::Int;
		goto Integral;
	default:
		assertf( false, "unknown basic type" );
		return nullptr;
	} // switch

	ast::BasicType * bt = new ast::BasicType( ret, buildQualifiers( td ) );
	return bt;
} // buildBasicType


ast::PointerType * buildPointer( const TypeData * td ) {
	ast::PointerType * pt;
	if ( td->base ) {
		pt = new ast::PointerType(
			typebuild( td->base ),
			buildQualifiers( td )
		);
	} else {
		pt = new ast::PointerType(
			new ast::BasicType( ast::BasicType::SignedInt ),
			buildQualifiers( td )
		);
	} // if
	return pt;
} // buildPointer


ast::ArrayType * buildArray( const TypeData * td ) {
	ast::ArrayType * at;
	if ( td->base ) {
		at = new ast::ArrayType(
			typebuild( td->base ),
			maybeBuild( td->array.dimension ),
			td->array.isVarLen ? ast::VariableLen : ast::FixedLen,
			td->array.isStatic ? ast::StaticDim : ast::DynamicDim,
			buildQualifiers( td )
		);
	} else {
		at = new ast::ArrayType(
			new ast::BasicType( ast::BasicType::SignedInt ),
			maybeBuild( td->array.dimension ),
			td->array.isVarLen ? ast::VariableLen : ast::FixedLen,
			td->array.isStatic ? ast::StaticDim : ast::DynamicDim,
			buildQualifiers( td )
		);
	} // if
	return at;
} // buildArray


ast::ReferenceType * buildReference( const TypeData * td ) {
	ast::ReferenceType * rt;
	if ( td->base ) {
		rt = new ast::ReferenceType(
			typebuild( td->base ),
			buildQualifiers( td )
		);
	} else {
		rt = new ast::ReferenceType(
			new ast::BasicType( ast::BasicType::SignedInt ),
			buildQualifiers( td )
		);
	} // if
	return rt;
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
		const TypeData * type,
		std::vector<ast::ptr<ast::Attribute>> && attributes,
		ast::Linkage::Spec linkage ) {
	switch ( type->kind ) {
	case TypeData::Enum:
		if ( type->enumeration.body ) {
			ast::EnumDecl * typedecl =
				buildEnum( type, std::move( attributes ), linkage );
			return new ast::EnumInstType(
				typedecl,
				buildQualifiers( type )
			);
		} else {
			return new ast::EnumInstType(
				*type->enumeration.name,
				buildQualifiers( type )
			);
		} // if
		break;
	case TypeData::Aggregate:
		if ( type->aggregate.body ) {
			ast::AggregateDecl * typedecl =
				buildAggregate( type, std::move( attributes ), linkage );
			switch ( type->aggregate.kind ) {
			case ast::AggregateDecl::Struct:
			case ast::AggregateDecl::Coroutine:
			case ast::AggregateDecl::Monitor:
			case ast::AggregateDecl::Thread:
				return new ast::StructInstType(
					strict_dynamic_cast<ast::StructDecl *>( typedecl ),
					buildQualifiers( type )
				);
			case ast::AggregateDecl::Union:
				return new ast::UnionInstType(
					strict_dynamic_cast<ast::UnionDecl *>( typedecl ),
					buildQualifiers( type )
				);
			case ast::AggregateDecl::Trait:
				assert( false );
				break;
			default:
				assert( false );
			} // switch
		} else {
			switch ( type->aggregate.kind ) {
			case ast::AggregateDecl::Struct:
			case ast::AggregateDecl::Coroutine:
			case ast::AggregateDecl::Monitor:
			case ast::AggregateDecl::Thread:
				return new ast::StructInstType(
					*type->aggregate.name,
					buildQualifiers( type )
				);
			case ast::AggregateDecl::Union:
				return new ast::UnionInstType(
					*type->aggregate.name,
					buildQualifiers( type )
				);
			case ast::AggregateDecl::Trait:
				return new ast::TraitInstType(
					*type->aggregate.name,
					buildQualifiers( type )
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
	case TypeData::Enum:
		return new ast::EnumInstType(
			*type->enumeration.name,
			buildQualifiers( type )
		);
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
	assert( td->kind == TypeData::Enum );
	ast::Type * baseType = td->base ? typebuild(td->base) : nullptr;
	ast::EnumDecl * ret = new ast::EnumDecl(
		td->location,
		*td->enumeration.name,
		td->enumeration.typed,
		std::move( attributes ),
		linkage,
		baseType
	);
	buildList( td->enumeration.constants, ret->members );
	auto members = ret->members.begin();
	ret->hide = td->enumeration.hiding == EnumHiding::Hide ? ast::EnumDecl::EnumHiding::Hide : ast::EnumDecl::EnumHiding::Visible;
	for ( const DeclarationNode * cur = td->enumeration.constants; cur != nullptr; cur = dynamic_cast< DeclarationNode * >( cur->get_next() ), ++members ) {
		if ( cur->enumInLine ) {
			// Do Nothing
		} else if ( ret->isTyped && !ret->base && cur->has_enumeratorValue() ) {
			SemanticError( td->location, "Enumerator of enum(void) cannot have an explicit initializer value." );
		} else if ( cur->has_enumeratorValue() ) {
			ast::Decl * member = members->get_and_mutate();
			ast::ObjectDecl * object = strict_dynamic_cast<ast::ObjectDecl *>( member );
			object->init = new ast::SingleInit(
				td->location,
				maybeMoveBuild( cur->consume_enumeratorValue() ),
				ast::NoConstruct
			);
		} else if ( !cur->initializer ) {
			if ( baseType && (!dynamic_cast<ast::BasicType *>(baseType) || !dynamic_cast<ast::BasicType *>(baseType)->isInteger())) {
				SemanticError( td->location, "Enumerators of an non-integer typed enum must be explicitly initialized." );
			}
		}
		// else cur is a List Initializer and has been set as init in buildList()
		// if
	} // for
	ret->body = td->enumeration.body;
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


ast::FunctionDecl * buildFunctionDecl(
		const TypeData * td,
		const string &name,
		ast::Storage::Classes scs,
		ast::Function::Specs funcSpec,
		ast::Linkage::Spec linkage,
		ast::Expr * asmName,
		std::vector<ast::ptr<ast::Attribute>> && attributes ) {
	assert( td->kind == TypeData::Function );
	// For some reason FunctionDecl takes a bool instead of an ArgumentFlag.
	bool isVarArgs = !td->function.params || td->function.params->hasEllipsis;
	ast::CV::Qualifiers cvq = buildQualifiers( td );
	std::vector<ast::ptr<ast::TypeDecl>> forall;
	std::vector<ast::ptr<ast::DeclWithType>> assertions;
	std::vector<ast::ptr<ast::DeclWithType>> params;
	std::vector<ast::ptr<ast::DeclWithType>> returns;
	buildList( td->function.params, params );
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
			new ast::BasicType( ast::BasicType::SignedInt ),
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
		(isVarArgs) ? ast::VariableArgs : ast::FixedArgs
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
	} else if ( td->kind == TypeData::Enum ) {
		return buildEnum( td, std::move( attributes ), linkage );
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
		( !td->function.params || td->function.params->hasEllipsis )
			? ast::VariableArgs : ast::FixedArgs,
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
			new ast::BasicType( ast::BasicType::SignedInt ) );
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
	for ( DeclarationNode * decl = function.oldDeclList; decl != nullptr; decl = dynamic_cast< DeclarationNode * >( decl->get_next() ) ) {
		// scan ALL parameter names for each declaration name to check for duplicates
		for ( DeclarationNode * param = function.idList; param != nullptr; param = dynamic_cast< DeclarationNode * >( param->get_next() ) ) {
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

	for ( DeclarationNode * param = function.idList; param != nullptr; param = dynamic_cast< DeclarationNode * >( param->get_next() ) ) {
		if ( ! param->type ) {							// generate type int for empty parameter type
			param->type = new TypeData( TypeData::Basic );
			param->type->basictype = DeclarationNode::Int;
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
