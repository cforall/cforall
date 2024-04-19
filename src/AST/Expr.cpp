//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Expr.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 15 17:00:00 2019
// Last Modified By : Peter A. Buhr
// Created On       : Wed May 18 13:56:00 2022
// Update Count     : 12
//

#include "Expr.hpp"

#include <cassert>                 // for strict_dynamic_cast
#include <string>                  // for to_string
#include <vector>

#include "Copy.hpp"                // for shallowCopy
#include "GenericSubstitution.hpp"
#include "Inspect.hpp"
#include "LinkageSpec.hpp"
#include "Stmt.hpp"
#include "Type.hpp"
#include "TypeSubstitution.hpp"
#include "Common/utility.h"
#include "Common/SemanticError.h"
#include "GenPoly/Lvalue.h"        // for referencesPermissable
#include "ResolvExpr/Unify.h"      // for extractResultType
#include "Tuples/Tuples.h"         // for makeTupleType

namespace ast {

namespace {
	std::set<std::string> const lvalueFunctionNames = {"*?", "?[?]"};
}

// --- Expr
bool Expr::get_lvalue() const {
	return false;
}

// --- ApplicationExpr

ApplicationExpr::ApplicationExpr( const CodeLocation & loc, const Expr * f,
	std::vector<ptr<Expr>> && as )
: Expr( loc ), func( f ), args( std::move(as) ) {
	// ensure that `ApplicationExpr` result type is `FuncExpr`
	const PointerType * pt = strict_dynamic_cast< const PointerType * >( f->result.get() );
	const FunctionType * fn = strict_dynamic_cast< const FunctionType * >( pt->base.get() );

	result = ResolvExpr::extractResultType( fn );
	assert( result );
}

bool ApplicationExpr::get_lvalue() const {
	if ( const DeclWithType * func = getFunction( this ) ) {
		return func->linkage == Linkage::Intrinsic && lvalueFunctionNames.count( func->name );
	}
	return false;
}

// --- UntypedExpr

bool UntypedExpr::get_lvalue() const {
	std::string fname = getFunctionName( this );
	return lvalueFunctionNames.count( fname );
}

UntypedExpr * UntypedExpr::createDeref( const CodeLocation & loc, const Expr * arg ) {
	assert( arg );

	UntypedExpr * ret = createCall( loc, "*?", { arg } );
	if ( const Type * ty = arg->result ) {
		const Type * base = getPointerBase( ty );
		assertf( base, "expected pointer type in dereference (type was %s)", toString( ty ).c_str() );

		if ( GenPoly::referencesPermissable() ) {
			// if references are still allowed in the AST, dereference returns a reference
			ret->result = new ReferenceType{ base };
		} else {
			// references have been removed, in which case dereference returns an lvalue of the
			// base type
			ret->result = base;
		}
	}
	return ret;
}

UntypedExpr * UntypedExpr::createAssign( const CodeLocation & loc, const Expr * lhs, const Expr * rhs ) {
	assert( lhs && rhs );

	UntypedExpr * ret = createCall( loc, "?=?", { lhs, rhs } );
	if ( lhs->result && rhs->result ) {
		// if both expressions are typed, assumes that this assignment is a C bitwise assignment,
		// so the result is the type of the RHS
		ret->result = rhs->result;
	}
	return ret;
}

UntypedExpr * UntypedExpr::createCall( const CodeLocation & loc,
		const std::string & name, std::vector<ptr<Expr>> && args ) {
	return new UntypedExpr( loc,
			new NameExpr( loc, name ), std::move( args ) );
}

// --- VariableExpr

VariableExpr::VariableExpr( const CodeLocation & loc )
: Expr( loc ), var( nullptr ) {}

VariableExpr::VariableExpr( const CodeLocation & loc, const DeclWithType * v )
: Expr( loc ), var( v ) {
	assert( var );
	assert( var->get_type() );
	result = shallowCopy( var->get_type() );
}

bool VariableExpr::get_lvalue() const {
	// It isn't always an lvalue, but it is never an rvalue.
	return true;
}

VariableExpr * VariableExpr::functionPointer(
		const CodeLocation & loc, const FunctionDecl * decl ) {
	// wrap usually-determined result type in a pointer
	VariableExpr * funcExpr = new VariableExpr{ loc, decl };
	funcExpr->result = new PointerType{ funcExpr->result };
	return funcExpr;
}

// --- AddressExpr

// Address expressions are typed based on the following inference rules:
//    E : lvalue T  &..& (n references)
//   &E :        T *&..& (n references)
//
//    E : T  &..&        (m references)
//   &E : T *&..&        (m-1 references)

namespace {
	/// The type of the address of a type.
	/// Caller is responsible for managing returned memory
	Type * addrType( const ptr<Type> & type ) {
		if ( auto refType = type.as< ReferenceType >() ) {
			return new ReferenceType( addrType( refType->base ), refType->qualifiers );
		} else {
			return new PointerType( type );
		}
	}

	/// The type of the address of an expression.
	/// Caller is responsible for managing returned memory
	Type * addrExprType( const CodeLocation & loc, const Expr * arg ) {
		assert( arg );
		// If the expression's type is unknown, the address type is unknown.
		if ( nullptr == arg->result ) {
			return nullptr;
		// An lvalue is transformed directly.
		} else if ( arg->get_lvalue() ) {
			return addrType( arg->result );
		// Strip a layer of reference to "create" an lvalue expression.
		} else if ( auto refType = arg->result.as< ReferenceType >() ) {
			return addrType( refType->base );
		} else {
			SemanticError( loc, "Attempt to take address of non-lvalue expression %s",
						   toString( arg->result.get() ).c_str() );
		}
	}
}

AddressExpr::AddressExpr( const CodeLocation & loc, const Expr * a ) :
	Expr( loc, addrExprType( loc, a ) ), arg( a )
{}

// --- LabelAddressExpr

// label address always has type `void*`
LabelAddressExpr::LabelAddressExpr( const CodeLocation & loc, Label && a )
: Expr( loc, new PointerType{ new VoidType{} } ), arg( a ) {}

// --- CastExpr

CastExpr::CastExpr( const CodeLocation & loc, const Expr * a, GeneratedFlag g, CastKind kind )
: Expr( loc, new VoidType{} ), arg( a ), isGenerated( g ), kind( kind ) {}

bool CastExpr::get_lvalue() const {
	// This is actually wrong by C, but it works with our current set-up.
	return arg->get_lvalue();
}

// --- KeywordCastExpr

const char * KeywordCastExpr::targetString() const {
	return AggregateDecl::aggrString( target );
}

// --- UntypedMemberExpr

bool UntypedMemberExpr::get_lvalue() const {
	return aggregate->get_lvalue();
}

// --- MemberExpr

MemberExpr::MemberExpr( const CodeLocation & loc, const DeclWithType * mem, const Expr * agg )
: Expr( loc ), member( mem ), aggregate( agg ) {
	assert( member );
	assert( aggregate );
	assert( aggregate->result );

	result = mem->get_type();

	// substitute aggregate generic parameters into member type
	genericSubstitution( aggregate->result ).apply( result );
	// ensure appropriate restrictions from aggregate type
	add_qualifiers( result, aggregate->result->qualifiers );
}

bool MemberExpr::get_lvalue() const {
	// This is actually wrong by C, but it works with our current set-up.
	return true;
}

// --- ConstantExpr

long long int ConstantExpr::intValue() const {
	if ( const BasicType * bty = result.as< BasicType >() ) {
		if ( bty->isInteger() ) {
			assert(ival);
			return ival.value();
		}
	} else if ( result.as< ZeroType >() ) {
		return 0;
	} else if ( result.as< OneType >() ) {
		return 1;
	}
	SemanticError( this->location, "Constant expression of non-integral type %s",
				   toString( this ).c_str() );
}

ConstantExpr * ConstantExpr::from_bool( const CodeLocation & loc, bool b ) {
	return new ConstantExpr{
		loc, new BasicType{ BasicKind::Bool }, b ? "1" : "0", (unsigned long long)b };
}

ConstantExpr * ConstantExpr::from_int( const CodeLocation & loc, int i ) {
	return new ConstantExpr{
		loc, new BasicType{ BasicKind::SignedInt }, std::to_string( i ), (unsigned long long)i };
}

ConstantExpr * ConstantExpr::from_ulong( const CodeLocation & loc, unsigned long i ) {
	return new ConstantExpr{
		loc, new BasicType{ BasicKind::LongUnsignedInt }, std::to_string( i ),
		(unsigned long long)i };
}

ConstantExpr * ConstantExpr::from_string( const CodeLocation & loc, const std::string & str ) {
	const Type * charType = new BasicType( BasicKind::Char );
	// Adjust the length of the string for the terminator.
	const Expr * strSize = from_ulong( loc, str.size() + 1 );
	const Type * strType = new ArrayType( charType, strSize, FixedLen, DynamicDim );
	const std::string strValue = "\"" + str + "\"";
	return new ConstantExpr( loc, strType, strValue, std::nullopt );
}

ConstantExpr * ConstantExpr::null( const CodeLocation & loc, const Type * ptrType ) {
	return new ConstantExpr{
		loc, ptrType ? ptrType : new PointerType{ new VoidType{} }, "0", (unsigned long long)0 };
}

// --- SizeofExpr

SizeofExpr::SizeofExpr( const CodeLocation & loc, const Expr * e )
: Expr( loc, new BasicType{ BasicKind::LongUnsignedInt } ), expr( e ), type( nullptr ) {}

SizeofExpr::SizeofExpr( const CodeLocation & loc, const Type * t )
: Expr( loc, new BasicType{ BasicKind::LongUnsignedInt } ), expr( nullptr ), type( t ) {}

// --- AlignofExpr

AlignofExpr::AlignofExpr( const CodeLocation & loc, const Expr * e )
: Expr( loc, new BasicType{ BasicKind::LongUnsignedInt } ), expr( e ), type( nullptr ) {}

AlignofExpr::AlignofExpr( const CodeLocation & loc, const Type * t )
: Expr( loc, new BasicType{ BasicKind::LongUnsignedInt } ), expr( nullptr ), type( t ) {}

// --- OffsetofExpr

OffsetofExpr::OffsetofExpr( const CodeLocation & loc, const Type * ty, const DeclWithType * mem )
: Expr( loc, new BasicType{ BasicKind::LongUnsignedInt } ), type( ty ), member( mem ) {
	assert( type );
	assert( member );
}

// --- OffsetPackExpr

OffsetPackExpr::OffsetPackExpr( const CodeLocation & loc, const StructInstType * ty )
: Expr( loc, new ArrayType{
	new BasicType{ BasicKind::LongUnsignedInt }, nullptr, FixedLen, DynamicDim }
), type( ty ) {
	assert( type );
}

// --- LogicalExpr

LogicalExpr::LogicalExpr(
	const CodeLocation & loc, const Expr * a1, const Expr * a2, LogicalFlag ia )
: Expr( loc, new BasicType{ BasicKind::SignedInt } ), arg1( a1 ), arg2( a2 ), isAnd( ia ) {}

// --- CommaExpr
bool CommaExpr::get_lvalue() const {
	// This is wrong by C, but the current implementation uses it.
	// (ex: Specialize, Lvalue and Box)
	return arg2->get_lvalue();
}

// --- ConstructorExpr

ConstructorExpr::ConstructorExpr( const CodeLocation & loc, const Expr * call )
: Expr( loc ), callExpr( call ) {
	// allow resolver to type a constructor used as an expression if it has the same type as its
	// first argument
	assert( callExpr );
	const Expr * arg = getCallArg( callExpr, 0 );
	assert( arg );
	result = arg->result;
}

// --- CompoundLiteralExpr

CompoundLiteralExpr::CompoundLiteralExpr( const CodeLocation & loc, const Type * t, const Init * i )
: Expr( loc ), init( i ) {
	assert( t && i );
	result = t;
}

bool CompoundLiteralExpr::get_lvalue() const {
	return true;
}

// --- TupleExpr

TupleExpr::TupleExpr( const CodeLocation & loc, std::vector<ptr<Expr>> && xs )
: Expr( loc, Tuples::makeTupleType( xs ) ), exprs( xs ) {}

// --- TupleIndexExpr

TupleIndexExpr::TupleIndexExpr( const CodeLocation & loc, const Expr * t, unsigned i )
: Expr( loc ), tuple( t ), index( i ) {
	const TupleType * type = strict_dynamic_cast< const TupleType * >( tuple->result.get() );
	assertf( type->size() > index, "TupleIndexExpr index out of bounds: tuple size %d, requested "
		"index %d in expr %s", type->size(), index, toString( tuple ).c_str() );
	// like MemberExpr, TupleIndexExpr is always an lvalue
	result = type->types[ index ];
}

bool TupleIndexExpr::get_lvalue() const {
	return tuple->get_lvalue();
}

// --- TupleAssignExpr

TupleAssignExpr::TupleAssignExpr(
	const CodeLocation & loc, std::vector<ptr<Expr>> && assigns,
	std::vector<ptr<ObjectDecl>> && tempDecls )
: Expr( loc, Tuples::makeTupleType( assigns ) ), stmtExpr() {
	// convert internally into a StmtExpr which contains the declarations and produces the tuple of
	// the assignments
	std::list<ptr<Stmt>> stmts;
	for ( const ObjectDecl * obj : tempDecls ) {
		stmts.emplace_back( new DeclStmt{ loc, obj } );
	}
	TupleExpr * tupleExpr = new TupleExpr{ loc, std::move(assigns) };
	assert( tupleExpr->result );
	stmts.emplace_back( new ExprStmt{ loc, tupleExpr } );
	stmtExpr = new StmtExpr{ loc, new CompoundStmt{ loc, std::move(stmts) } };
}

// --- StmtExpr

StmtExpr::StmtExpr( const CodeLocation & loc, const CompoundStmt * ss )
: Expr( loc ), stmts( ss ), returnDecls(), dtors() { computeResult(); }

void StmtExpr::computeResult() {
	assert( stmts );
	const std::list<ptr<Stmt>> & body = stmts->kids;
	if ( ! returnDecls.empty() ) {
		// prioritize return decl for result type, since if a return decl exists, then the StmtExpr
		// is currently in an intermediate state where the body will always give a void result type
		result = returnDecls.front()->get_type();
	} else if ( ! body.empty() ) {
		if ( const ExprStmt * exprStmt = body.back().as< ExprStmt >() ) {
			result = exprStmt->expr->result;
		}
	}
	// ensure a result type exists
	if ( ! result ) { result = new VoidType{}; }
}

// --- UniqueExpr

unsigned long long UniqueExpr::nextId = 0;

UniqueExpr::UniqueExpr( const CodeLocation & loc, const Expr * e, unsigned long long i )
: Expr( loc, e->result ), expr( e ), id( i ) {
	assert( expr );
	if ( id == -1ull ) {
		assert( nextId != -1ull );
		id = nextId++;
	}
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
