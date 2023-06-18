//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Expression.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Dec 11 07:55:15 2019
// Update Count     : 70
//

#include "SynTree/Expression.h"

#include <cassert>                   // for assert, assertf
#include <iostream>                  // for ostream, operator<<, basic_ostream
#include <list>                      // for list, _List_iterator, list<>::co...
#include <set>                       // for set

#include "Common/utility.h"          // for maybeClone, cloneAll, deleteAll
#include "Expression.h"              // for Expression, ImplicitCopyCtorExpr
#include "InitTweak/InitTweak.h"     // for getCallArg, getPointerBase
#include "Initializer.h"             // for Designation, Initializer
#include "Statement.h"               // for CompoundStmt, ExprStmt, Statement
#include "SynTree/BaseSyntaxNode.h"  // for BaseSyntaxNode
#include "SynTree/Constant.h"        // for Constant
#include "Type.h"                    // for Type, BasicType, Type::Qualifiers
#include "TypeSubstitution.h"        // for TypeSubstitution
#include "CompilationState.h"        // for deterministic_output

#include "GenPoly/Lvalue.h"

void printInferParams( const InferredParams & inferParams, std::ostream & os, Indenter indent, int level ) {
	if ( ! inferParams.empty() ) {
		os << indent << "with inferred parameters " << level << ":" << std::endl;
		for ( InferredParams::const_iterator i = inferParams.begin(); i != inferParams.end(); ++i ) {
			os << indent+1;
			assert(i->second.declptr);
			i->second.declptr->printShort( os, indent+1 );
			os << std::endl;
			printInferParams( i->second.expr->inferParams, os, indent+1, level+1 );
		} // for
	} // if
}

Expression::Expression() : result( 0 ), env( 0 ) {}

Expression::Expression( const Expression & other ) : BaseSyntaxNode( other ), result( maybeClone( other.result ) ), env( maybeClone( other.env ) ), extension( other.extension ), inferParams( other.inferParams ), resnSlots( other.resnSlots ) {}

void Expression::spliceInferParams( Expression * other ) {
	if ( ! other ) return;
	for ( auto p : other->inferParams ) {
		inferParams[p.first] = std::move( p.second );
	}
	resnSlots.insert( resnSlots.end(), other->resnSlots.begin(), other->resnSlots.end() );
}

Expression::~Expression() {
	delete env;
	delete result;
}

bool Expression::get_lvalue() const {
	return false;
}

void Expression::print( std::ostream & os, Indenter indent ) const {
	printInferParams( inferParams, os, indent+1, 0 );

	if ( result ) {
		os << std::endl << indent << "with resolved type:" << std::endl;
		os << (indent+1);
		result->print( os, indent+1 );
	}

	if ( env ) {
		os << std::endl << indent << "... with environment:" << std::endl;
		env->print( os, indent+1 );
	} // if

	if ( extension ) {
		os << std::endl << indent << "... with extension:";
	} // if
}

ConstantExpr::ConstantExpr( Constant _c ) : Expression(), constant( _c ) {
	set_result( constant.get_type()->clone() );
}

ConstantExpr::ConstantExpr( const ConstantExpr & other) : Expression( other ), constant( other.constant ) {
}

ConstantExpr::~ConstantExpr() {}

void ConstantExpr::print( std::ostream & os, Indenter indent ) const {
	os << "constant expression " ;
	constant.print( os );
	Expression::print( os, indent );
}

long long int ConstantExpr::intValue() const {
	if ( BasicType * basicType = dynamic_cast< BasicType * >( result ) ) {
		if ( basicType->isInteger() ) {
			return get_constant()->get_ival();
		}
	} else if ( dynamic_cast< OneType * >( result ) ) {
		return 1;
	} else if ( dynamic_cast< ZeroType * >( result ) ) {
		return 0;
	}
	SemanticError( this, "Constant expression of non-integral type " );
}

VariableExpr::VariableExpr() : Expression(), var( nullptr ) {}

VariableExpr::VariableExpr( DeclarationWithType *_var ) : Expression(), var( _var ) {
	assert( var );
	assert( var->get_type() );
	Type * type = var->get_type()->clone();

	// xxx - doesn't quite work yet - get different alternatives with the same cost

	// // enumerators are not lvalues
	// if ( EnumInstType * inst = dynamic_cast< EnumInstType * >( var->get_type() ) ) {
	// 	assert( inst->baseEnum );
	// 	EnumDecl * decl = inst->baseEnum;
	// 	long long int value;
	// 	if ( decl->valueOf( var, value ) ) {
	// 		type->set_lvalue( false ); // Would have to move to get_lvalue.
	// 	}
	// }

	set_result( type );
}

VariableExpr::VariableExpr( const VariableExpr & other ) : Expression( other ), var( other.var ) {
}

VariableExpr::~VariableExpr() {
	// don't delete the declaration, since it points somewhere else in the tree
}

bool VariableExpr::get_lvalue() const {
	// It isn't always an lvalue, but it is never an rvalue.
	return true;
}

VariableExpr * VariableExpr::functionPointer( FunctionDecl * func ) {
	VariableExpr * funcExpr = new VariableExpr( func );
	funcExpr->set_result( new PointerType( Type::Qualifiers(), funcExpr->get_result() ) );
	return funcExpr;
}

void VariableExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Variable Expression: ";
	var->printShort(os, indent);
	Expression::print( os, indent );
}

SizeofExpr::SizeofExpr( Expression * expr_ ) :
		Expression(), expr(expr_), type(0), isType(false) {
	set_result( new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ) );
}

SizeofExpr::SizeofExpr( Type * type_ ) :
		Expression(), expr(0), type(type_), isType(true) {
	set_result( new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ) );
}

SizeofExpr::SizeofExpr( const SizeofExpr & other ) :
	Expression( other ), expr( maybeClone( other.expr ) ), type( maybeClone( other.type ) ), isType( other.isType ) {
}

SizeofExpr::~SizeofExpr() {
	delete expr;
	delete type;
}

void SizeofExpr::print( std::ostream & os, Indenter indent) const {
	os << "Sizeof Expression on: ";
	if (isType) type->print(os, indent+1);
	else expr->print(os, indent+1);
	Expression::print( os, indent );
}

AlignofExpr::AlignofExpr( Expression * expr_ ) :
		Expression(), expr(expr_), type(0), isType(false) {
	set_result( new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ) );
}

AlignofExpr::AlignofExpr( Type * type_ ) :
		Expression(), expr(0), type(type_), isType(true) {
	set_result( new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ) );
}

AlignofExpr::AlignofExpr( const AlignofExpr & other ) :
	Expression( other ), expr( maybeClone( other.expr ) ), type( maybeClone( other.type ) ), isType( other.isType ) {
}

AlignofExpr::~AlignofExpr() {
	delete expr;
	delete type;
}

void AlignofExpr::print( std::ostream & os, Indenter indent) const {
	os << "Alignof Expression on: ";
	if (isType) type->print(os, indent+1);
	else expr->print(os, indent+1);
	Expression::print( os, indent );
}

UntypedOffsetofExpr::UntypedOffsetofExpr( Type * type, const std::string & member ) :
		Expression(), type(type), member(member) {
	assert( type );
	set_result( new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ) );
}

UntypedOffsetofExpr::UntypedOffsetofExpr( const UntypedOffsetofExpr & other ) :
	Expression( other ), type( maybeClone( other.type ) ), member( other.member ) {}

UntypedOffsetofExpr::~UntypedOffsetofExpr() {
	delete type;
}

void UntypedOffsetofExpr::print( std::ostream & os, Indenter indent) const {
	os << "Untyped Offsetof Expression on member " << member << " of ";
	type->print(os, indent+1);
	Expression::print( os, indent );
}

OffsetofExpr::OffsetofExpr( Type * type, DeclarationWithType * member ) :
		Expression(), type(type), member(member) {
	assert( member );
	assert( type );
	set_result( new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ) );
}

OffsetofExpr::OffsetofExpr( const OffsetofExpr & other ) :
	Expression( other ), type( maybeClone( other.type ) ), member( other.member ) {}

OffsetofExpr::~OffsetofExpr() {
	delete type;
}

void OffsetofExpr::print( std::ostream & os, Indenter indent) const {
	os << "Offsetof Expression on member " << member->name << " of ";
	type->print(os, indent+1);
	Expression::print( os, indent );
}

OffsetPackExpr::OffsetPackExpr( StructInstType * type ) : Expression(), type( type ) {
	assert( type );
	set_result( new ArrayType( Type::Qualifiers(), new BasicType( Type::Qualifiers(), BasicType::LongUnsignedInt ), 0, false, false ) );
}

OffsetPackExpr::OffsetPackExpr( const OffsetPackExpr & other ) : Expression( other ), type( maybeClone( other.type ) ) {}

OffsetPackExpr::~OffsetPackExpr() { delete type; }

void OffsetPackExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Offset pack expression on ";
	type->print(os, indent+1);
	Expression::print( os, indent );
}

CastExpr::CastExpr( Expression * arg, Type * toType, bool isGenerated, CastKind kind ) : arg(arg), isGenerated( isGenerated ), kind( kind ) {
	set_result(toType);
}

CastExpr::CastExpr( Expression * arg, bool isGenerated, CastKind kind ) : arg(arg), isGenerated( isGenerated ), kind( kind ) {
	set_result( new VoidType( Type::Qualifiers() ) );
}

CastExpr::CastExpr( const CastExpr & other ) : Expression( other ), arg( maybeClone( other.arg ) ), isGenerated( other.isGenerated ), kind( other.kind ) {
}

CastExpr::~CastExpr() {
	delete arg;
}

bool CastExpr::get_lvalue() const {
	// This is actually wrong by C, but it works with our current set-up.
	return arg->get_lvalue();
}

void CastExpr::print( std::ostream & os, Indenter indent ) const {
	os << (isGenerated ? "Generated " : "Explicit ") << "Cast of:" << std::endl << indent+1;
	arg->print(os, indent+1);
	os << std::endl << indent << "... to:";
	if ( result->isVoid() ) {
		os << " nothing";
	} else {
		os << std::endl << indent+1;
		result->print( os, indent+1 );
	} // if
	Expression::print( os, indent );
}

KeywordCastExpr::KeywordCastExpr( Expression * arg, AggregateDecl::Aggregate target ) : Expression(), arg(arg), target( target ) {}
KeywordCastExpr::KeywordCastExpr( Expression * arg, AggregateDecl::Aggregate target, const KeywordCastExpr::Concrete & concrete_target ) : Expression(), arg(arg), target( target ), concrete_target(concrete_target) {}

KeywordCastExpr::KeywordCastExpr( const KeywordCastExpr & other ) : Expression( other ), arg( maybeClone( other.arg ) ), target( other.target ) {}

KeywordCastExpr::~KeywordCastExpr() {
	delete arg;
}

const char * KeywordCastExpr::targetString() const {
	return AggregateDecl::aggrString( target );
}

void KeywordCastExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Keyword Cast of:" << std::endl << indent+1;
	arg->print(os, indent+1);
	os << std::endl << indent << "... to: ";
	os << targetString();
	Expression::print( os, indent );
}

VirtualCastExpr::VirtualCastExpr( Expression * arg_, Type * toType ) : Expression(), arg(arg_) {
	set_result(toType);
}

VirtualCastExpr::VirtualCastExpr( const VirtualCastExpr & other ) : Expression( other ), arg( maybeClone( other.arg ) ) {
}

VirtualCastExpr::~VirtualCastExpr() {
	delete arg;
}

void VirtualCastExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Virtual Cast of:" << std::endl << indent+1;
	arg->print(os, indent+1);
	os << std::endl << indent << "... to:";
	if ( ! result ) {
		os << " unknown";
	} else {
		os << std::endl << indent+1;
		result->print( os, indent+1 );
	} // if
	Expression::print( os, indent );
}

UntypedMemberExpr::UntypedMemberExpr( Expression * member, Expression * aggregate ) :
		Expression(), member(member), aggregate(aggregate) {
	assert( aggregate );
}

UntypedMemberExpr::UntypedMemberExpr( const UntypedMemberExpr & other ) :
		Expression( other ), member( maybeClone( other.member ) ), aggregate( maybeClone( other.aggregate ) ) {
}

UntypedMemberExpr::~UntypedMemberExpr() {
	delete aggregate;
	delete member;
}

bool UntypedMemberExpr::get_lvalue() const {
	return aggregate->get_lvalue();
}

void UntypedMemberExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Untyped Member Expression, with field: " << std::endl << indent+1;
	member->print(os, indent+1 );
	os << indent << "... from aggregate:" << std::endl << indent+1;
	aggregate->print(os, indent+1);
	Expression::print( os, indent );
}

MemberExpr::MemberExpr( DeclarationWithType * member, Expression * aggregate ) :
		Expression(), member(member), aggregate(aggregate) {
	assert( member );
	assert( aggregate );
	assert( aggregate->result );

	TypeSubstitution sub = aggregate->result->genericSubstitution();
	Type * res = member->get_type()->clone();
	sub.apply( res );
	result = res;
	result->get_qualifiers() |= aggregate->result->get_qualifiers();
}

MemberExpr::MemberExpr( const MemberExpr & other ) :
		Expression( other ), member( other.member ), aggregate( maybeClone( other.aggregate ) ) {
}

MemberExpr::~MemberExpr() {
	// don't delete the member declaration, since it points somewhere else in the tree
	delete aggregate;
}

bool MemberExpr::get_lvalue() const {
	// This is actually wrong by C, but it works with our current set-up.
	return true;
}

void MemberExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Member Expression, with field:" << std::endl;
	os << indent+1;
	member->print( os, indent+1 );
	os << std::endl << indent << "... from aggregate:" << std::endl << indent+1;
	aggregate->print(os, indent + 1);
	Expression::print( os, indent );
}

UntypedExpr::UntypedExpr( Expression * function, const std::list<Expression *> & args ) :
		Expression(), function(function), args(args) {}

UntypedExpr::UntypedExpr( const UntypedExpr & other ) :
		Expression( other ), function( maybeClone( other.function ) ) {
	cloneAll( other.args, args );
}

UntypedExpr::~UntypedExpr() {
	delete function;
	deleteAll( args );
}

UntypedExpr * UntypedExpr::createDeref( Expression * expr ) {
	UntypedExpr * ret = new UntypedExpr( new NameExpr("*?"), std::list< Expression * >{ expr } );
	if ( Type * type = expr->get_result() ) {
		Type * base = InitTweak::getPointerBase( type );
		assertf( base, "expected pointer type in dereference (type was %s)", toString( type ).c_str() );
		ret->set_result( base->clone() );
		if ( GenPoly::referencesPermissable() ) {
			// if references are still allowed in the AST, dereference returns a reference
			ret->set_result( new ReferenceType( Type::Qualifiers(), ret->get_result() ) );
		}
	}
	return ret;
}

UntypedExpr * UntypedExpr::createAssign( Expression * arg1, Expression * arg2 ) {
	assert( arg1 && arg2 );
	UntypedExpr * ret = new UntypedExpr( new NameExpr( "?=?" ), std::list< Expression * >{ arg1, arg2 } );
	if ( arg1->get_result() && arg2->get_result() ) {
		// if both expressions are typed, assumes that this assignment is a C bitwise assignment,
		// so the result is the type of the RHS
		ret->set_result( arg2->get_result()->clone() );
	}
	return ret;
}

bool UntypedExpr::get_lvalue() const {
	// from src/GenPoly/Lvalue.cc: isIntrinsicReference
	static std::set<std::string> lvalueFunctions = { "*?", "?[?]" };
	std::string fname = InitTweak::getFunctionName( const_cast< UntypedExpr * >( this ) );
	return lvalueFunctions.count(fname);
}

void UntypedExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Applying untyped:" << std::endl;
	os << indent+1;
	function->print(os, indent+1);
	os << std::endl << indent << "...to:" << std::endl;
	printAll(args, os, indent+1);
	Expression::print( os, indent );
}

NameExpr::NameExpr( std::string name ) : Expression(), name(name) {
	assertf(name != "0", "Zero is not a valid name");
	assertf(name != "1", "One is not a valid name");
}

NameExpr::NameExpr( const NameExpr & other ) : Expression( other ), name( other.name ) {
}

NameExpr::~NameExpr() {}

void NameExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Name: " << get_name();
	Expression::print( os, indent );
}

LogicalExpr::LogicalExpr( Expression * arg1_, Expression * arg2_, bool andp ) :
		Expression(), arg1(arg1_), arg2(arg2_), isAnd(andp) {
	set_result( new BasicType( Type::Qualifiers(), BasicType::SignedInt ) );
}

LogicalExpr::LogicalExpr( const LogicalExpr & other ) :
		Expression( other ), arg1( maybeClone( other.arg1 ) ), arg2( maybeClone( other.arg2 ) ), isAnd( other.isAnd ) {
}

LogicalExpr::~LogicalExpr() {
	delete arg1;
	delete arg2;
}

void LogicalExpr::print( std::ostream & os, Indenter indent )const {
	os << "Short-circuited operation (" << (isAnd ? "and" : "or") << ") on: ";
	arg1->print(os);
	os << " and ";
	arg2->print(os);
	Expression::print( os, indent );
}

ConditionalExpr::ConditionalExpr( Expression * arg1, Expression * arg2, Expression * arg3 ) :
		Expression(), arg1(arg1), arg2(arg2), arg3(arg3) {}

ConditionalExpr::ConditionalExpr( const ConditionalExpr & other ) :
		Expression( other ), arg1( maybeClone( other.arg1 ) ), arg2( maybeClone( other.arg2 ) ), arg3( maybeClone( other.arg3 ) ) {
}

ConditionalExpr::~ConditionalExpr() {
	delete arg1;
	delete arg2;
	delete arg3;
}

bool ConditionalExpr::get_lvalue() const {
	return false;
}

void ConditionalExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Conditional expression on: " << std::endl << indent+1;
	arg1->print( os, indent+1 );
	os << indent << "First alternative:" << std::endl << indent+1;
	arg2->print( os, indent+1 );
	os << indent << "Second alternative:" << std::endl << indent+1;
	arg3->print( os, indent+1 );
	Expression::print( os, indent );
}

AsmExpr::AsmExpr( const AsmExpr & other ) : Expression( other ), inout( other.inout ), constraint( maybeClone( other.constraint ) ), operand( maybeClone( other.operand ) ) {}


void AsmExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Asm Expression: " << std::endl;
	if ( !inout.empty() ) os <<  "[" << inout << "] ";
	if ( constraint ) constraint->print( os, indent+1 );
	if ( operand ) operand->print( os, indent+1 );
}


ImplicitCopyCtorExpr::ImplicitCopyCtorExpr( ApplicationExpr * callExpr ) : callExpr( callExpr ) {
	assert( callExpr );
	assert( callExpr->result );
	set_result( callExpr->result->clone() );
}

ImplicitCopyCtorExpr::ImplicitCopyCtorExpr( const ImplicitCopyCtorExpr & other ) : Expression( other ), callExpr( maybeClone( other.callExpr ) ) {
}

ImplicitCopyCtorExpr::~ImplicitCopyCtorExpr() {
	set_env( nullptr ); // ImplicitCopyCtorExpr does not take ownership of an environment
	delete callExpr;
}

void ImplicitCopyCtorExpr::print( std::ostream & os, Indenter indent ) const {
	os <<  "Implicit Copy Constructor Expression: " << std::endl << indent+1;
	callExpr->print( os, indent+1 );
}


ConstructorExpr::ConstructorExpr( Expression * callExpr ) : callExpr( callExpr ) {
	// allow resolver to type a constructor used as an expression as if it has the same type as its first argument
	assert( callExpr );
	Expression * arg = InitTweak::getCallArg( callExpr, 0 );
	assert( arg );
	set_result( maybeClone( arg->result ) );
}

ConstructorExpr::ConstructorExpr( const ConstructorExpr & other ) : Expression( other ), callExpr( maybeClone( other.callExpr ) ) {
}

ConstructorExpr::~ConstructorExpr() {
	delete callExpr;
}

bool ConstructorExpr::get_lvalue() const {
	return false;
}

void ConstructorExpr::print( std::ostream & os, Indenter indent ) const {
	os <<  "Constructor Expression: " << std::endl << indent+1;
	callExpr->print( os, indent + 2 );
	Expression::print( os, indent );
}


CompoundLiteralExpr::CompoundLiteralExpr( Type * type, Initializer * initializer ) : initializer( initializer ) {
	assert( type && initializer );
	set_result( type );
}

CompoundLiteralExpr::CompoundLiteralExpr( const CompoundLiteralExpr & other ) : Expression( other ), initializer( other.initializer->clone() ) {}

CompoundLiteralExpr::~CompoundLiteralExpr() {
	delete initializer;
}

bool CompoundLiteralExpr::get_lvalue() const {
	return true;
}

void CompoundLiteralExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Compound Literal Expression: " << std::endl << indent+1;
	result->print( os, indent+1 );
	os << indent+1;
	initializer->print( os, indent+1 );
	Expression::print( os, indent );
}

RangeExpr::RangeExpr( Expression * low, Expression * high ) : low( low ), high( high ) {}
RangeExpr::RangeExpr( const RangeExpr & other ) : Expression( other ), low( other.low->clone() ), high( other.high->clone() ) {}
void RangeExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Range Expression: ";
	low->print( os, indent );
	os << " ... ";
	high->print( os, indent );
	Expression::print( os, indent );
}

StmtExpr::StmtExpr( CompoundStmt * statements ) : statements( statements ) {
	computeResult();
}
StmtExpr::StmtExpr( const StmtExpr & other ) : Expression( other ), statements( other.statements->clone() ), resultExpr( other.resultExpr ) {
	cloneAll( other.returnDecls, returnDecls );
	cloneAll( other.dtors, dtors );
}
StmtExpr::~StmtExpr() {
	delete statements;
	deleteAll( dtors );
	deleteAll( returnDecls );
}
void StmtExpr::computeResult() {
	assert( statements );
	std::list< Statement * > & body = statements->kids;
	delete result;
	result = nullptr;
	if ( ! returnDecls.empty() ) {
		// prioritize return decl for result type, since if a return decl exists, then
		// the StmtExpr is currently in an intermediate state where the body will always
		// give a void result type.
		result = returnDecls.front()->get_type()->clone();
	} else if ( ! body.empty() ) {
		if ( ExprStmt * exprStmt = dynamic_cast< ExprStmt * >( body.back() ) ) {
			result = maybeClone( exprStmt->expr->result );
		}
	}
	// ensure that StmtExpr has a result type
	if ( ! result ) {
		result = new VoidType( Type::Qualifiers() );
	}
}
bool StmtExpr::get_lvalue() const {
	return false;
}
void StmtExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Statement Expression: " << std::endl << indent+1;
	statements->print( os, indent+1 );
	if ( ! returnDecls.empty() ) {
		os << indent+1 << "... with returnDecls: ";
		printAll( returnDecls, os, indent+1 );
	}
	if ( ! dtors.empty() ) {
		os << indent+1 << "... with dtors: ";
		printAll( dtors, os, indent+1 );
	}
	Expression::print( os, indent );
}


long long UniqueExpr::count = 0;
UniqueExpr::UniqueExpr( Expression * expr, long long idVal ) : expr( expr ), object( nullptr ), var( nullptr ), id( idVal ) {
	assert( expr );
	assert( count != -1 );
	if ( id == -1 ) id = count++;
	if ( expr->get_result() ) {
		set_result( expr->get_result()->clone() );
	}
}
UniqueExpr::UniqueExpr( const UniqueExpr & other ) : Expression( other ), expr( maybeClone( other.expr ) ), object( maybeClone( other.object ) ), var( maybeClone( other.var ) ), id( other.id ) {
}
UniqueExpr::~UniqueExpr() {
	delete expr;
	delete object;
	delete var;
}
void UniqueExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Unique Expression with id:" << id << std::endl << indent+1;
	expr->print( os, indent+1 );
	if ( object ) {
		os << indent << "... with decl: ";
		get_object()->printShort( os, indent+1 );
	}
	Expression::print( os, indent );
}

InitAlternative::InitAlternative( Type * type, Designation * designation ) : type( type ), designation( designation ) {}
InitAlternative::InitAlternative( const InitAlternative & other ) : type( maybeClone( other.type ) ), designation( maybeClone( other.designation ) ) {}
InitAlternative::~InitAlternative() {
	delete type;
	delete designation;
}

UntypedInitExpr::UntypedInitExpr( Expression * expr, const std::list<InitAlternative> & initAlts ) : expr( expr ), initAlts( initAlts ) {}
UntypedInitExpr::UntypedInitExpr( const UntypedInitExpr & other ) : Expression( other ), expr( maybeClone( other.expr ) ), initAlts( other.initAlts ) {}
UntypedInitExpr::~UntypedInitExpr() {
	delete expr;
}

void UntypedInitExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Untyped Init Expression" << std::endl << indent+1;
	expr->print( os, indent+1 );
	if ( ! initAlts.empty() ) {
		for ( const InitAlternative & alt : initAlts ) {
			os << indent+1 <<  "InitAlternative: ";
			alt.type->print( os, indent+1 );
			alt.designation->print( os, indent+1 );
		}
	}
}

InitExpr::InitExpr( Expression * expr, Designation * designation ) : expr( expr ), designation( designation ) {
	set_result( expr->get_result()->clone() );
}
InitExpr::InitExpr( const InitExpr & other ) : Expression( other ), expr( maybeClone( other.expr ) ), designation( maybeClone( other.designation) ) {}
InitExpr::~InitExpr() {
	delete expr;
	delete designation;
}

void InitExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Init Expression" << std::endl << indent+1;
	expr->print( os, indent+1 );
	os << indent+1 << "... with designation: ";
	designation->print( os, indent+1 );
}

DeletedExpr::DeletedExpr( Expression * expr, Declaration * deleteStmt ) : expr( expr ), deleteStmt( deleteStmt ) {
	assert( expr->result );
	result = expr->result->clone();
}
DeletedExpr::DeletedExpr( const DeletedExpr & other ) : Expression( other ), expr( maybeClone( other.expr ) ), deleteStmt( other.deleteStmt ) {}
DeletedExpr::~DeletedExpr() {
	delete expr;
}

void DeletedExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Deleted Expression" << std::endl << indent+1;
	expr->print( os, indent+1 );
	os << std::endl << indent+1 << "... deleted by: ";
	deleteStmt->print( os, indent+1 );
}


DefaultArgExpr::DefaultArgExpr( Expression * expr ) : expr( expr ) {
	assert( expr->result );
	result = expr->result->clone();
}
DefaultArgExpr::DefaultArgExpr( const DefaultArgExpr & other ) : Expression( other ), expr( maybeClone( other.expr ) ) {}
DefaultArgExpr::~DefaultArgExpr() {
	delete expr;
}

void DefaultArgExpr::print( std::ostream & os, Indenter indent ) const {
	os << "Default Argument Expression" << std::endl << indent+1;
	expr->print( os, indent+1 );
}

GenericExpr::Association::Association( Type * type, Expression * expr ) : type( type ), expr( expr ), isDefault( false ) {}
GenericExpr::Association::Association( Expression * expr ) : type( nullptr ), expr( expr ), isDefault( true ) {}
GenericExpr::Association::Association( const Association & other ) : type( maybeClone( other.type ) ), expr( maybeClone( other.expr ) ), isDefault( other.isDefault ) {}
GenericExpr::Association::~Association() {
	delete type;
	delete expr;
}

GenericExpr::GenericExpr( Expression * control, const std::list<Association> & assoc ) : Expression(), control( control ), associations( assoc ) {}
GenericExpr::GenericExpr( const GenericExpr & other ) : Expression(other), control( maybeClone( other.control ) ), associations( other.associations ) {
}
GenericExpr::~GenericExpr() {
	delete control;
}

void GenericExpr::print( std::ostream & os, Indenter indent ) const {
	os << "C11 _Generic Expression" << std::endl << indent+1;
	control->print( os, indent+1 );
	os << std::endl << indent+1 << "... with associations: " << std::endl;
	for ( const Association & assoc : associations ) {
		os << indent+1;
		if (assoc.isDefault) {
			os << "... default: ";
			assoc.expr->print( os, indent+1 );
		} else {
			os << "... type: ";
			assoc.type->print( os, indent+1 );
			os << std::endl << indent+1 << "... expression: ";
			assoc.expr->print( os, indent+1 );
			os << std::endl;
		}
		os << std::endl;
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
