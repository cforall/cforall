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
// Last Modified By : Henry Xue
// Last Modified On : Tue Jul 20 04:10:50 2021
// Update Count     : 673
//

#include <cassert>                 // for assert
#include <ostream>                 // for operator<<, ostream, basic_ostream

#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for maybeClone, maybeBuild, maybeMoveB...
#include "Parser/ParseNode.h"      // for DeclarationNode, ExpressionNode
#include "SynTree/Declaration.h"   // for TypeDecl, ObjectDecl, FunctionDecl
#include "SynTree/Expression.h"    // for Expression, ConstantExpr (ptr only)
#include "SynTree/Initializer.h"   // for SingleInit, Initializer (ptr only)
#include "SynTree/Statement.h"     // for CompoundStmt, Statement
#include "SynTree/Type.h"          // for BasicType, Type, Type::ForallList
#include "TypeData.h"

class Attribute;

using namespace std;

TypeData::TypeData( Kind k ) : location( yylloc ), kind( k ), base( nullptr ), forall( nullptr ) /*, PTR1( (void*)(0xdeadbeefdeadbeef)), PTR2( (void*)(0xdeadbeefdeadbeef) ) */ {
	switch ( kind ) {
	  case Unknown:
	  case Pointer:
	  case Reference:
	  case EnumConstant:
	  case GlobalScope:
		// nothing else to initialize
		break;
	  case Basic:
		// basic = new Basic_t;
		break;
	  case Array:
		// array = new Array_t;
		array.dimension = nullptr;
		array.isVarLen = false;
		array.isStatic = false;
		break;
	  case Function:
		// function = new Function_t;
		function.params = nullptr;
		function.idList = nullptr;
		function.oldDeclList = nullptr;
		function.body = nullptr;
		function.withExprs = nullptr;
		break;
		// Enum is an Aggregate, so both structures are initialized together.
	  case Enum:
		// enumeration = new Enumeration_t;
		enumeration.name = nullptr;
		enumeration.constants = nullptr;
		enumeration.body = false;
		enumeration.anon = false;
		break;
	  case Aggregate:
		// aggregate = new Aggregate_t;
		aggregate.kind = AggregateDecl::NoAggregate;
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
		// aggInst = new AggInst_t;
		aggInst.aggregate = nullptr;
		aggInst.params = nullptr;
		aggInst.hoistType = false;
		break;
	  case Symbolic:
	  case SymbolicInst:
		// symbolic = new Symbolic_t;
		symbolic.name = nullptr;
		symbolic.params = nullptr;
		symbolic.actuals = nullptr;
		symbolic.assertions = nullptr;
		break;
	  case Tuple:
		// tuple = new Tuple_t;
		tuple = nullptr;
		break;
	  case Typeof:
	  case Basetypeof:
		// typeexpr = new Typeof_t;
		typeexpr = nullptr;
		break;
	  case Vtable:
		break;
	  case Builtin:
		// builtin = new Builtin_t;
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
		// nothing to destroy
		break;
	  case Basic:
		// delete basic;
		break;
	  case Array:
		delete array.dimension;
		// delete array;
		break;
	  case Function:
		delete function.params;
		delete function.idList;
		delete function.oldDeclList;
		delete function.body;
		delete function.withExprs;
		// delete function;
		break;
	  case Aggregate:
		delete aggregate.name;
		delete aggregate.params;
		delete aggregate.actuals;
		delete aggregate.fields;
		// delete aggregate;
		break;
	  case AggregateInst:
		delete aggInst.aggregate;
		delete aggInst.params;
		// delete aggInst;
		break;
	  case Enum:
		delete enumeration.name;
		delete enumeration.constants;
		// delete enumeration;
		break;
	  case Symbolic:
	  case SymbolicInst:
		delete symbolic.name;
		delete symbolic.params;
		delete symbolic.actuals;
		delete symbolic.assertions;
		// delete symbolic;
		break;
	  case Tuple:
		// delete tuple->members;
		delete tuple;
		break;
	  case Typeof:
	  case Basetypeof:
		// delete typeexpr->expr;
		delete typeexpr;
		break;
	  case Vtable:
		break;
	  case Builtin:
		// delete builtin;
		break;
	  case Qualified:
		delete qualified.parent;
		delete qualified.child;
	} // switch
} // TypeData::~TypeData


TypeData * TypeData::clone() const {
	TypeData * newtype = new TypeData( kind );
	newtype->qualifiers = qualifiers;
	newtype->base = maybeClone( base );
	newtype->forall = maybeClone( forall );

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
		newtype->array.dimension = maybeClone( array.dimension );
		newtype->array.isVarLen = array.isVarLen;
		newtype->array.isStatic = array.isStatic;
		break;
	  case Function:
		newtype->function.params = maybeClone( function.params );
		newtype->function.idList = maybeClone( function.idList );
		newtype->function.oldDeclList = maybeClone( function.oldDeclList );
		newtype->function.body = maybeClone( function.body );
		newtype->function.withExprs = maybeClone( function.withExprs );
		break;
	  case Aggregate:
		newtype->aggregate.kind = aggregate.kind;
		newtype->aggregate.name = aggregate.name ? new string( *aggregate.name ) : nullptr;
		newtype->aggregate.params = maybeClone( aggregate.params );
		newtype->aggregate.actuals = maybeClone( aggregate.actuals );
		newtype->aggregate.fields = maybeClone( aggregate.fields );
		newtype->aggregate.body = aggregate.body;
		newtype->aggregate.anon = aggregate.anon;
		newtype->aggregate.tagged = aggregate.tagged;
		newtype->aggregate.parent = aggregate.parent ? new string( *aggregate.parent ) : nullptr;
		break;
	  case AggregateInst:
		newtype->aggInst.aggregate = maybeClone( aggInst.aggregate );
		newtype->aggInst.params = maybeClone( aggInst.params );
		newtype->aggInst.hoistType = aggInst.hoistType;
		break;
	  case Enum:
		newtype->enumeration.name = enumeration.name ? new string( *enumeration.name ) : nullptr;
		newtype->enumeration.constants = maybeClone( enumeration.constants );
		newtype->enumeration.body = enumeration.body;
		newtype->enumeration.anon = enumeration.anon;
		break;
	  case Symbolic:
	  case SymbolicInst:
		newtype->symbolic.name = symbolic.name ? new string( *symbolic.name ) : nullptr;
		newtype->symbolic.params = maybeClone( symbolic.params );
		newtype->symbolic.actuals = maybeClone( symbolic.actuals );
		newtype->symbolic.assertions = maybeClone( symbolic.assertions );
		newtype->symbolic.isTypedef = symbolic.isTypedef;
		break;
	  case Tuple:
		newtype->tuple = maybeClone( tuple );
		break;
	  case Typeof:
	  case Basetypeof:
		newtype->typeexpr = maybeClone( typeexpr );
		break;
	  case Vtable:
		break;
	  case Builtin:
		assert( builtintype == DeclarationNode::Zero || builtintype == DeclarationNode::One );
		newtype->builtintype = builtintype;
		break;
		case Qualified:
		newtype->qualified.parent = maybeClone( qualified.parent );
		newtype->qualified.child = maybeClone( qualified.child );
		break;
	} // switch
	return newtype;
} // TypeData::clone


void TypeData::print( ostream &os, int indent ) const {
	for ( int i = 0; i < Type::NumTypeQualifier; i += 1 ) {
		if ( qualifiers[i] ) os << Type::QualifiersNames[ i ] << ' ';
	} // for

	if ( forall ) {
		os << "forall " << endl;
		forall->printList( os, indent + 4 );
	} // if

	switch ( kind ) {
	  case Basic:
		if ( signedness != DeclarationNode::NoSignedness ) os << DeclarationNode::signednessNames[ signedness ] << " ";
		if ( length != DeclarationNode::NoLength ) os << DeclarationNode::lengthNames[ length ] << " ";
		if ( complextype == DeclarationNode::NoComplexType ) { // basic type
			assert( basictype != DeclarationNode::NoBasicType );
			os << DeclarationNode::basicTypeNames[ basictype ] << " ";
		} else {										// complex type
			// handle double _Complex
			if ( basictype != DeclarationNode::NoBasicType ) os << DeclarationNode::basicTypeNames[ basictype ] << " ";
			os << DeclarationNode::complexTypeNames[ complextype ] << " ";
		} // if
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
		os << AggregateDecl::aggrString( aggregate.kind ) << ' ' << *aggregate.name << endl;
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
		os << "enumeration ";
		if ( enumeration.constants ) {
			os << "with constants" << endl;
			enumeration.constants->printList( os, indent + 2 );
		} // if
		if ( enumeration.body ) {
			os << string( indent + 2, ' ' ) << " with body" << endl;
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
	  case Typeof:
		os << "type-of expression ";
		if ( typeexpr ) {
			typeexpr->print( os, indent + 2 );
		} // if
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


template< typename ForallList >
void buildForall( const DeclarationNode * firstNode, ForallList &outputList ) {
	buildList( firstNode, outputList );
	auto n = firstNode;
	for ( typename ForallList::iterator i = outputList.begin(); i != outputList.end(); ++i, n = (DeclarationNode*)n->get_next() ) {
		TypeDecl * td = static_cast<TypeDecl *>(*i);
		if ( n->variable.tyClass == TypeDecl::Otype ) {
			// add assertion parameters to `type' tyvars in reverse order
			// add dtor:  void ^?{}(T *)
			FunctionType * dtorType = new FunctionType( Type::Qualifiers(), false );
			dtorType->get_parameters().push_back( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new ReferenceType( Type::Qualifiers(), new TypeInstType( Type::Qualifiers(), td->get_name(), *i ) ), nullptr ) );
			td->get_assertions().push_front( new FunctionDecl( "^?{}", Type::StorageClasses(), LinkageSpec::Cforall, dtorType, nullptr ) );

			// add copy ctor:  void ?{}(T *, T)
			FunctionType * copyCtorType = new FunctionType( Type::Qualifiers(), false );
			copyCtorType->get_parameters().push_back( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new ReferenceType( Type::Qualifiers(), new TypeInstType( Type::Qualifiers(), td->get_name(), *i ) ), nullptr ) );
			copyCtorType->get_parameters().push_back( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new TypeInstType( Type::Qualifiers(), td->get_name(), *i ), nullptr ) );
			td->get_assertions().push_front( new FunctionDecl( "?{}", Type::StorageClasses(), LinkageSpec::Cforall, copyCtorType, nullptr ) );

			// add default ctor:  void ?{}(T *)
			FunctionType * ctorType = new FunctionType( Type::Qualifiers(), false );
			ctorType->get_parameters().push_back( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new ReferenceType( Type::Qualifiers(), new TypeInstType( Type::Qualifiers(), td->get_name(), *i ) ), nullptr ) );
			td->get_assertions().push_front( new FunctionDecl( "?{}", Type::StorageClasses(), LinkageSpec::Cforall, ctorType, nullptr ) );

			// add assignment operator:  T * ?=?(T *, T)
			FunctionType * assignType = new FunctionType( Type::Qualifiers(), false );
			assignType->get_parameters().push_back( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new ReferenceType( Type::Qualifiers(), new TypeInstType( Type::Qualifiers(), td->get_name(), *i ) ), nullptr ) );
			assignType->get_parameters().push_back( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new TypeInstType( Type::Qualifiers(), td->get_name(), *i ), nullptr ) );
			assignType->get_returnVals().push_back( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new TypeInstType( Type::Qualifiers(), td->get_name(), *i ), nullptr ) );
			td->get_assertions().push_front( new FunctionDecl( "?=?", Type::StorageClasses(), LinkageSpec::Cforall, assignType, nullptr ) );
		} // if
	} // for
} // buildForall


Type * typebuild( const TypeData * td ) {
	assert( td );
	switch ( td->kind ) {
	  case TypeData::Unknown:
		// fill in implicit int
		return new BasicType( buildQualifiers( td ), BasicType::SignedInt );
	  case TypeData::Basic:
		return buildBasicType( td );
	  case TypeData::Pointer:
		return buildPointer( td );
	  case TypeData::Array:
		return buildArray( td );
	  case TypeData::Reference:
		return buildReference( td );
	  case TypeData::Function:
		return buildFunction( td );
	  case TypeData::AggregateInst:
		return buildAggInst( td );
	  case TypeData::EnumConstant:
		// the name gets filled in later -- by SymTab::Validate
		return new EnumInstType( buildQualifiers( td ), "" );
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
			return new ZeroType( noQualifiers );
		  case DeclarationNode::One:
			return new OneType( noQualifiers );
		  default:
			return new VarArgsType( buildQualifiers( td ) );
		} // switch
	  case TypeData::GlobalScope:
		return new GlobalScopeType();
	  case TypeData::Qualified:
		return new QualifiedType( buildQualifiers( td ), typebuild( td->qualified.parent ), typebuild( td->qualified.child ) );
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


Type::Qualifiers buildQualifiers( const TypeData * td ) {
	return td->qualifiers;
} // buildQualifiers


static string genTSError( string msg, DeclarationNode::BasicType basictype ) {
	SemanticError( yylloc, string( "invalid type specifier \"" ) + msg + "\" for type \"" + DeclarationNode::basicTypeNames[basictype] + "\"." );
} // genTSError

Type * buildBasicType( const TypeData * td ) {
	BasicType::Kind ret;

	switch ( td->basictype ) {
	  case DeclarationNode::Void:
		if ( td->signedness != DeclarationNode::NoSignedness ) {
			genTSError( DeclarationNode::signednessNames[ td->signedness ], td->basictype );
		} // if
		if ( td->length != DeclarationNode::NoLength ) {
			genTSError( DeclarationNode::lengthNames[ td->length ], td->basictype );
		} // if
		return new VoidType( buildQualifiers( td ) );
		break;

	  case DeclarationNode::Bool:
		if ( td->signedness != DeclarationNode::NoSignedness ) {
			genTSError( DeclarationNode::signednessNames[ td->signedness ], td->basictype );
		} // if
		if ( td->length != DeclarationNode::NoLength ) {
			genTSError( DeclarationNode::lengthNames[ td->length ], td->basictype );
		} // if

		ret = BasicType::Bool;
		break;

	  case DeclarationNode::Char:
		// C11 Standard 6.2.5.15: The three types char, signed char, and unsigned char are collectively called the
		// character types. The implementation shall define char to have the same range, representation, and behavior as
		// either signed char or unsigned char.
		static BasicType::Kind chartype[] = { BasicType::SignedChar, BasicType::UnsignedChar, BasicType::Char };

		if ( td->length != DeclarationNode::NoLength ) {
			genTSError( DeclarationNode::lengthNames[ td->length ], td->basictype );
		} // if

		ret = chartype[ td->signedness ];
		break;

	  case DeclarationNode::Int:
		static BasicType::Kind inttype[2][4] = {
			{ BasicType::ShortSignedInt, BasicType::LongSignedInt, BasicType::LongLongSignedInt, BasicType::SignedInt },
			{ BasicType::ShortUnsignedInt, BasicType::LongUnsignedInt, BasicType::LongLongUnsignedInt, BasicType::UnsignedInt },
		};

	  Integral: ;
		if ( td->signedness == DeclarationNode::NoSignedness ) {
			const_cast<TypeData *>(td)->signedness = DeclarationNode::Signed;
		} // if
		ret = inttype[ td->signedness ][ td->length ];
		break;

	  case DeclarationNode::Int128:
		ret = td->signedness == DeclarationNode::Unsigned ? BasicType::UnsignedInt128 : BasicType::SignedInt128;
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
		static BasicType::Kind floattype[2][12] = {
			{ BasicType::FloatComplex, BasicType::DoubleComplex, BasicType::LongDoubleComplex, (BasicType::Kind)-1, (BasicType::Kind)-1, BasicType::uFloat16Complex, BasicType::uFloat32Complex, BasicType::uFloat32xComplex, BasicType::uFloat64Complex, BasicType::uFloat64xComplex, BasicType::uFloat128Complex, BasicType::uFloat128xComplex, },
			{ BasicType::Float, BasicType::Double, BasicType::LongDouble, BasicType::uuFloat80, BasicType::uuFloat128, BasicType::uFloat16, BasicType::uFloat32, BasicType::uFloat32x, BasicType::uFloat64, BasicType::uFloat64x, BasicType::uFloat128, BasicType::uFloat128x, },
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

	BasicType * bt = new BasicType( buildQualifiers( td ), ret );
	buildForall( td->forall, bt->get_forall() );
	return bt;
} // buildBasicType


PointerType * buildPointer( const TypeData * td ) {
	PointerType * pt;
	if ( td->base ) {
		pt = new PointerType( buildQualifiers( td ), typebuild( td->base ) );
	} else {
		pt = new PointerType( buildQualifiers( td ), new BasicType( Type::Qualifiers(), BasicType::SignedInt ) );
	} // if
	buildForall( td->forall, pt->get_forall() );
	return pt;
} // buildPointer


ArrayType * buildArray( const TypeData * td ) {
	ArrayType * at;
	if ( td->base ) {
		at = new ArrayType( buildQualifiers( td ), typebuild( td->base ), maybeBuild< Expression >( td->array.dimension ),
							td->array.isVarLen, td->array.isStatic );
	} else {
		at = new ArrayType( buildQualifiers( td ), new BasicType( Type::Qualifiers(), BasicType::SignedInt ),
							maybeBuild< Expression >( td->array.dimension ), td->array.isVarLen, td->array.isStatic );
	} // if
	buildForall( td->forall, at->get_forall() );
	return at;
} // buildArray


ReferenceType * buildReference( const TypeData * td ) {
	ReferenceType * rt;
	if ( td->base ) {
		rt = new ReferenceType( buildQualifiers( td ), typebuild( td->base ) );
	} else {
		rt = new ReferenceType( buildQualifiers( td ), new BasicType( Type::Qualifiers(), BasicType::SignedInt ) );
	} // if
	buildForall( td->forall, rt->get_forall() );
	return rt;
} // buildReference


AggregateDecl * buildAggregate( const TypeData * td, std::list< Attribute * > attributes, LinkageSpec::Spec linkage ) {
	assert( td->kind == TypeData::Aggregate );
	AggregateDecl * at;
	switch ( td->aggregate.kind ) {
	  case AggregateDecl::Struct:
	  case AggregateDecl::Coroutine:
	  case AggregateDecl::Exception:
	  case AggregateDecl::Generator:
	  case AggregateDecl::Monitor:
	  case AggregateDecl::Thread:
		at = new StructDecl( *td->aggregate.name, td->aggregate.kind, attributes, linkage );
		buildForall( td->aggregate.params, at->get_parameters() );
		break;
	  case AggregateDecl::Union:
		at = new UnionDecl( *td->aggregate.name, attributes, linkage );
		buildForall( td->aggregate.params, at->get_parameters() );
		break;
	  case AggregateDecl::Trait:
		at = new TraitDecl( *td->aggregate.name, attributes, linkage );
		buildList( td->aggregate.params, at->get_parameters() );
		break;
	  default:
		assert( false );
	} // switch

	buildList( td->aggregate.fields, at->get_members() );
	at->set_body( td->aggregate.body );

	return at;
} // buildAggregate


ReferenceToType * buildComAggInst( const TypeData * type, std::list< Attribute * > attributes, LinkageSpec::Spec linkage ) {
	switch ( type->kind ) {
	  case TypeData::Enum: {
		  if ( type->enumeration.body ) {
			  EnumDecl * typedecl = buildEnum( type, attributes, linkage );
			  return new EnumInstType( buildQualifiers( type ), typedecl );
		  } else {
			  return new EnumInstType( buildQualifiers( type ), *type->enumeration.name );
		  } // if
	  }
	  case TypeData::Aggregate: {
		  ReferenceToType * ret;
		  if ( type->aggregate.body ) {
			  AggregateDecl * typedecl = buildAggregate( type, attributes, linkage );
			  switch ( type->aggregate.kind ) {
				case AggregateDecl::Struct:
				case AggregateDecl::Coroutine:
				case AggregateDecl::Monitor:
				case AggregateDecl::Thread:
				  ret = new StructInstType( buildQualifiers( type ), (StructDecl *)typedecl );
				  break;
				case AggregateDecl::Union:
				  ret = new UnionInstType( buildQualifiers( type ), (UnionDecl *)typedecl );
				  break;
				case AggregateDecl::Trait:
				  assert( false );
				  //ret = new TraitInstType( buildQualifiers( type ), (TraitDecl *)typedecl );
				  break;
				default:
				  assert( false );
			  } // switch
		  } else {
			  switch ( type->aggregate.kind ) {
				case AggregateDecl::Struct:
				case AggregateDecl::Coroutine:
				case AggregateDecl::Monitor:
				case AggregateDecl::Thread:
				  ret = new StructInstType( buildQualifiers( type ), *type->aggregate.name );
				  break;
				case AggregateDecl::Union:
				  ret = new UnionInstType( buildQualifiers( type ), *type->aggregate.name );
				  break;
				case AggregateDecl::Trait:
				  ret = new TraitInstType( buildQualifiers( type ), *type->aggregate.name );
				  break;
				default:
				  assert( false );
			  } // switch
		  } // if
		  return ret;
	  }
	  default:
		assert( false );
	} // switch
} // buildAggInst


ReferenceToType * buildAggInst( const TypeData * td ) {
	assert( td->kind == TypeData::AggregateInst );

	// ReferenceToType * ret = buildComAggInst( td->aggInst.aggregate, std::list< Attribute * >() );
	ReferenceToType * ret = nullptr;
	TypeData * type = td->aggInst.aggregate;
	switch ( type->kind ) {
	  case TypeData::Enum: {
		  return new EnumInstType( buildQualifiers( type ), *type->enumeration.name );
	  }
	  case TypeData::Aggregate: {
		  switch ( type->aggregate.kind ) {
			case AggregateDecl::Struct:
			case AggregateDecl::Coroutine:
			case AggregateDecl::Monitor:
			case AggregateDecl::Thread:
			  ret = new StructInstType( buildQualifiers( type ), *type->aggregate.name );
			  break;
			case AggregateDecl::Union:
			  ret = new UnionInstType( buildQualifiers( type ), *type->aggregate.name );
			  break;
			case AggregateDecl::Trait:
			  ret = new TraitInstType( buildQualifiers( type ), *type->aggregate.name );
			  break;
			default:
			  assert( false );
		  } // switch
	  }
	  break;
	  default:
		assert( false );
	} // switch

	ret->set_hoistType( td->aggInst.hoistType );
	buildList( td->aggInst.params, ret->get_parameters() );
	buildForall( td->forall, ret->get_forall() );
	return ret;
} // buildAggInst


NamedTypeDecl * buildSymbolic( const TypeData * td, std::list< Attribute * > attributes, const string & name, Type::StorageClasses scs, LinkageSpec::Spec linkage ) {
	assert( td->kind == TypeData::Symbolic );
	NamedTypeDecl * ret;
	assert( td->base );
	if ( td->symbolic.isTypedef ) {
		ret = new TypedefDecl( name, td->location, scs, typebuild( td->base ), linkage );
	} else {
		ret = new TypeDecl( name, scs, typebuild( td->base ), TypeDecl::Dtype, true );
	} // if
	buildList( td->symbolic.assertions, ret->get_assertions() );
	ret->base->attributes.splice( ret->base->attributes.end(), attributes );
	return ret;
} // buildSymbolic


EnumDecl * buildEnum( const TypeData * td, std::list< Attribute * > attributes, LinkageSpec::Spec linkage ) {
	assert( td->kind == TypeData::Enum );
	Type * baseType = td->base ? typebuild(td->base) : nullptr;
	EnumDecl * ret = new EnumDecl( *td->enumeration.name, attributes, linkage, baseType );
	buildList( td->enumeration.constants, ret->get_members() );
	list< Declaration * >::iterator members = ret->get_members().begin();
	for ( const DeclarationNode * cur = td->enumeration.constants; cur != nullptr; cur = dynamic_cast< DeclarationNode * >( cur->get_next() ), ++members ) {
		if ( cur->has_enumeratorValue() ) {
			ObjectDecl * member = dynamic_cast< ObjectDecl * >(* members);
			member->set_init( new SingleInit( maybeMoveBuild< Expression >( cur->consume_enumeratorValue() ) ) );
		} else {
			if ( baseType && (!dynamic_cast<BasicType *>(baseType) || !dynamic_cast<BasicType *>(baseType)->isWholeNumber())) {
				SemanticError( td->location, "A non whole number enum value decl must be explicitly initialized." );
			}
		} // if
	} // for
	ret->set_body( td->enumeration.body ); // Boolean; if it has body
	return ret;
} // buildEnum


TypeInstType * buildSymbolicInst( const TypeData * td ) {
	assert( td->kind == TypeData::SymbolicInst );
	TypeInstType * ret = new TypeInstType( buildQualifiers( td ), *td->symbolic.name, false );
	buildList( td->symbolic.actuals, ret->get_parameters() );
	buildForall( td->forall, ret->get_forall() );
	return ret;
} // buildSymbolicInst


TupleType * buildTuple( const TypeData * td ) {
	assert( td->kind == TypeData::Tuple );
	std::list< Type * > types;
	buildTypeList( td->tuple, types );
	TupleType * ret = new TupleType( buildQualifiers( td ), types );
	buildForall( td->forall, ret->get_forall() );
	return ret;
} // buildTuple


TypeofType * buildTypeof( const TypeData * td ) {
	assert( td->kind == TypeData::Typeof || td->kind == TypeData::Basetypeof );
	assert( td->typeexpr );
	// assert( td->typeexpr->expr );
	return new TypeofType{ buildQualifiers( td ), td->typeexpr->build(), td->kind == TypeData::Basetypeof };
} // buildTypeof


VTableType * buildVtable( const TypeData * td ) {
	assert( td->base );
	return new VTableType{ buildQualifiers( td ), typebuild( td->base ) };
} // buildVtable


Declaration * buildDecl( const TypeData * td, const string &name, Type::StorageClasses scs, Expression * bitfieldWidth, Type::FuncSpecifiers funcSpec, LinkageSpec::Spec linkage, Expression *asmName, Initializer * init, std::list< Attribute * > attributes ) {
	if ( td->kind == TypeData::Function ) {
		if ( td->function.idList ) {					// KR function ?
			buildKRFunction( td->function );			// transform into C11 function
		} // if

		FunctionDecl * decl;
		Statement * stmt = maybeBuild<Statement>( td->function.body );
		CompoundStmt * body = dynamic_cast< CompoundStmt * >( stmt );
		decl = new FunctionDecl( name, scs, linkage, buildFunction( td ), body, attributes, funcSpec );
		buildList( td->function.withExprs, decl->withExprs );
		return decl->set_asmName( asmName );
	} else if ( td->kind == TypeData::Aggregate ) {
		return buildAggregate( td, attributes, linkage );
	} else if ( td->kind == TypeData::Enum ) {
		return buildEnum( td, attributes, linkage );
	} else if ( td->kind == TypeData::Symbolic ) {
		return buildSymbolic( td, attributes, name, scs, linkage );
	} else {
		return (new ObjectDecl( name, scs, linkage, bitfieldWidth, typebuild( td ), init, attributes ))->set_asmName( asmName );
	} // if
	return nullptr;
} // buildDecl


FunctionType * buildFunction( const TypeData * td ) {
	assert( td->kind == TypeData::Function );
	FunctionType * ft = new FunctionType( buildQualifiers( td ), ! td->function.params || td->function.params->hasEllipsis );
	buildList( td->function.params, ft->parameters );
	buildForall( td->forall, ft->forall );
	if ( td->base ) {
		switch ( td->base->kind ) {
		  case TypeData::Tuple:
			buildList( td->base->tuple, ft->returnVals );
			break;
		  default:
			ft->get_returnVals().push_back( dynamic_cast< DeclarationWithType * >( buildDecl( td->base, "", Type::StorageClasses(), nullptr, Type::FuncSpecifiers(), LinkageSpec::Cforall, nullptr ) ) );
		} // switch
	} else {
		ft->get_returnVals().push_back( new ObjectDecl( "", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new BasicType( Type::Qualifiers(), BasicType::SignedInt ), nullptr ) );
	} // if
	return ft;
} // buildFunction


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
				if ( param->type ) SemanticError( param->location, string( "duplicate declaration name " ) + *param->name );
				// declaration type reset => declaration already transformed by a parameter name so there is a duplicate
				// parameter name attempting a second transformation
				if ( ! decl->type ) SemanticError( param->location, string( "duplicate parameter name " ) + *param->name );
				param->type = decl->type;				// set copy declaration type to parameter type
				decl->type = nullptr;					// reset declaration type
				param->attributes.splice( param->attributes.end(), decl->attributes ); // copy and reset attributes from declaration to parameter
			} // if
		} // for
		// declaration type still set => type not moved to a matching parameter so there is a missing parameter name
		if ( decl->type ) SemanticError( decl->location, string( "missing name in parameter list " ) + *decl->name );
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
