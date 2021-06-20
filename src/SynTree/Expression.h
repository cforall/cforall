//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Expression.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Dec 11 16:50:19 2019
// Update Count     : 60
//

#pragma once

#include <iosfwd>                 // for ostream
#include <list>                   // for list, list<>::iterator
#include <map>                    // for map, map<>::value_compare
#include <memory>                 // for allocator, unique_ptr
#include <string>                 // for string
#include <vector>                 // for vector

#include "BaseSyntaxNode.h"       // for BaseSyntaxNode
#include "Constant.h"             // for Constant
#include "Initializer.h"          // for Designation (ptr only), Initializer
#include "Label.h"                // for Label
#include "Mutator.h"              // for Mutator
#include "Declaration.h"          // for Aggregate
#include "SynTree.h"              // for UniqueId
#include "Visitor.h"              // for Visitor


struct ParamEntry;

typedef std::map< UniqueId, ParamEntry > InferredParams;

/// ParamEntry contains the i.d. of a declaration and a type that is derived from that declaration,
/// but subject to decay-to-pointer and type parameter renaming
struct ParamEntry {
	ParamEntry(): decl( 0 ), declptr( nullptr ), actualType( nullptr ), formalType( nullptr ), expr( nullptr ) {}
	ParamEntry( UniqueId decl, Declaration * declptr, Type * actualType, Type * formalType, Expression* expr );
	ParamEntry( const ParamEntry & other );
	ParamEntry( ParamEntry && other );
	~ParamEntry();
	ParamEntry & operator=( ParamEntry && other );

	UniqueId const decl;
	Declaration * const declptr;
	Type * const actualType;
	Type * const formalType;
	Expression * expr;
};

/// Expression is the root type for all expressions
class Expression : public BaseSyntaxNode {
  public:
	Type * result;
	TypeSubstitution * env;
	bool extension = false;
	InferredParams inferParams;       ///< Post-resolution inferred parameter slots
	std::vector<UniqueId> resnSlots;  ///< Pre-resolution inferred parameter slots

	// xxx - should turn inferParams+resnSlots into a union to save some memory

	Expression();
	Expression( const Expression & other );
	virtual ~Expression();

	Type *& get_result() { return result; }
	const Type * get_result() const { return result; }
	void set_result( Type * newValue ) { result = newValue; }
	virtual bool get_lvalue() const;

	TypeSubstitution * get_env() const { return env; }
	void set_env( TypeSubstitution * newValue ) { env = newValue; }
	bool get_extension() const { return extension; }
	Expression * set_extension( bool exten ) { extension = exten; return this; }

	// move other's inferParams to this
	void spliceInferParams( Expression * other );

	virtual Expression * clone() const override = 0;
	virtual void accept( Visitor & v ) override = 0;
	virtual void accept( Visitor & v ) const override = 0;
	virtual Expression * acceptMutator( Mutator & m ) override = 0;
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// ApplicationExpr represents the application of a function to a set of parameters.  This is the result of running an
/// UntypedExpr through the expression analyzer.
class ApplicationExpr : public Expression {
  public:
	Expression * function;
	std::list<Expression *> args;

	ApplicationExpr( Expression * function, const std::list<Expression *> & args = std::list< Expression * >() );
	ApplicationExpr( const ApplicationExpr & other );
	virtual ~ApplicationExpr();

	bool get_lvalue() const final;

	Expression * get_function() const { return function; }
	void set_function( Expression * newValue ) { function = newValue; }
	std::list<Expression *>& get_args() { return args; }

	virtual ApplicationExpr * clone() const override { return new ApplicationExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// UntypedExpr represents the application of a function to a set of parameters, but where the particular overload for
/// the function name has not yet been determined.  Most operators are converted into functional form automatically, to
/// permit operator overloading.
class UntypedExpr : public Expression {
  public:
	Expression * function;
	std::list<Expression*> args;

	UntypedExpr( Expression * function, const std::list<Expression *> & args = std::list< Expression * >() );
	UntypedExpr( const UntypedExpr & other );
	virtual ~UntypedExpr();

	bool get_lvalue() const final;

	Expression * get_function() const { return function; }
	void set_function( Expression * newValue ) { function = newValue; }

	std::list<Expression*>::iterator begin_args() { return args.begin(); }
	std::list<Expression*>::iterator end_args() { return args.end(); }
	std::list<Expression*>& get_args() { return args; }

	static UntypedExpr * createDeref( Expression * arg );
	static UntypedExpr * createAssign( Expression * arg1, Expression * arg2 );

	virtual UntypedExpr * clone() const override { return new UntypedExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// NameExpr contains a name whose meaning is still not determined
class NameExpr : public Expression {
  public:
	std::string name;

	NameExpr( std::string name );
	NameExpr( const NameExpr & other );
	virtual ~NameExpr();

	const std::string & get_name() const { return name; }
	void set_name( std::string newValue ) { name = newValue; }

	virtual NameExpr * clone() const override { return new NameExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// VariableExpr represents an expression that simply refers to the value of a named variable.
/// Does not take ownership of var.
class VariableExpr : public Expression {
  public:
	DeclarationWithType * var;

	VariableExpr();
	VariableExpr( DeclarationWithType * var );
	VariableExpr( const VariableExpr & other );
	virtual ~VariableExpr();

	bool get_lvalue() const final;

	DeclarationWithType * get_var() const { return var; }
	void set_var( DeclarationWithType * newValue ) { var = newValue; }

	static VariableExpr * functionPointer( FunctionDecl * decl );

	virtual VariableExpr * clone() const override { return new VariableExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

// The following classes are used to represent expression types that cannot be converted into
// function-call format.

/// AddressExpr represents a address-of expression, e.g. & e
class AddressExpr : public Expression {
  public:
	Expression * arg;

	AddressExpr( Expression * arg );
	AddressExpr( const AddressExpr & other );
	virtual ~AddressExpr();

	Expression * get_arg() const { return arg; }
	void set_arg(Expression * newValue ) { arg = newValue; }

	virtual AddressExpr * clone() const override { return new AddressExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

// GCC &&label
// https://gcc.gnu.org/onlinedocs/gcc-3.4.2/gcc/Labels-as-Values.html
class LabelAddressExpr : public Expression {
  public:
	Label arg;

	LabelAddressExpr( const Label &arg );
	LabelAddressExpr( const LabelAddressExpr & other );
	virtual ~LabelAddressExpr();

	virtual LabelAddressExpr * clone() const override { return new LabelAddressExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// CastExpr represents a type cast expression, e.g. (int)e
class CastExpr : public Expression {
  public:
	Expression * arg;

	// Inidicates cast is introduced by the CFA type system.
	// true for casts that the resolver introduces to force a return type
	// false for casts from user code
	// false for casts from desugaring advanced CFA features into simpler CFA
	// example
	//   int * p;     // declaration
	//   (float *) p; // use, with subject cast
	// subject cast isGenerated means we are considering an interpretation with a type mismatch
	// subject cast not isGenerated means someone in charge wants it that way
	bool isGenerated = true;

	CastExpr( Expression * arg, bool isGenerated = true );
	CastExpr( Expression * arg, Type * toType, bool isGenerated = true );
	CastExpr( Expression * arg, void * ) = delete; // prevent accidentally passing pointers for isGenerated in the first constructor
	CastExpr( const CastExpr & other );
	virtual ~CastExpr();

	bool get_lvalue() const final;

	Expression * get_arg() const { return arg; }
	void set_arg( Expression * newValue ) { arg = newValue; }

	virtual CastExpr * clone() const override { return new CastExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// KeywordCastExpr represents a cast to 'keyword types', e.g. (thread &)t
class KeywordCastExpr : public Expression {
public:
	Expression * arg;
	struct Concrete {
		std::string field;
		std::string getter;
	};
	AggregateDecl::Aggregate target;
	Concrete concrete_target;

	KeywordCastExpr( Expression * arg, AggregateDecl::Aggregate target );
	KeywordCastExpr( Expression * arg, AggregateDecl::Aggregate target, const Concrete & concrete_target );
	KeywordCastExpr( const KeywordCastExpr & other );
	virtual ~KeywordCastExpr();

	const char * targetString() const;

	virtual KeywordCastExpr * clone() const override { return new KeywordCastExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// VirtualCastExpr repersents a virtual dynamic cast, e.g. (virtual exception)e
class VirtualCastExpr : public Expression {
  public:
	Expression * arg;

	VirtualCastExpr( Expression * arg, Type * toType );
	VirtualCastExpr( const VirtualCastExpr & other );
	virtual ~VirtualCastExpr();

	Expression * get_arg() const { return arg; }
	void set_arg( Expression * newValue ) { arg = newValue; }

	virtual VirtualCastExpr * clone() const override { return new VirtualCastExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// UntypedMemberExpr represents a member selection operation, e.g. q.p before processing by the expression analyzer
class UntypedMemberExpr : public Expression {
  public:
	Expression * member;
	Expression * aggregate;

	UntypedMemberExpr( Expression * member, Expression * aggregate );
	UntypedMemberExpr( const UntypedMemberExpr & other );
	virtual ~UntypedMemberExpr();

	bool get_lvalue() const final;

	Expression * get_member() const { return member; }
	void set_member( Expression * newValue ) { member = newValue; }
	Expression * get_aggregate() const { return aggregate; }
	void set_aggregate( Expression * newValue ) { aggregate = newValue; }

	virtual UntypedMemberExpr * clone() const override { return new UntypedMemberExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// MemberExpr represents a member selection operation, e.g. q.p after processing by the expression analyzer.
/// Does not take ownership of member.
class MemberExpr : public Expression {
  public:
	DeclarationWithType * member;
	Expression * aggregate;

	MemberExpr( DeclarationWithType * member, Expression * aggregate );
	MemberExpr( const MemberExpr & other );
	virtual ~MemberExpr();

	bool get_lvalue() const final;

	DeclarationWithType * get_member() const { return member; }
	void set_member( DeclarationWithType * newValue ) { member = newValue; }
	Expression * get_aggregate() const { return aggregate; }
	void set_aggregate( Expression * newValue ) { aggregate = newValue; }

	virtual MemberExpr * clone() const override { return new MemberExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// ConstantExpr represents an expression that simply refers to the value of a constant
class ConstantExpr : public Expression {
  public:
	Constant constant;

	ConstantExpr( Constant constant );
	ConstantExpr( const ConstantExpr & other );
	virtual ~ConstantExpr();

	Constant * get_constant() { return & constant; }
	const Constant * get_constant() const { return & constant; }
	void set_constant( const Constant & newValue ) { constant = newValue; }

	long long int intValue() const;

	virtual ConstantExpr * clone() const override { return new ConstantExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// SizeofExpr represents a sizeof expression (could be sizeof(int) or sizeof 3+4)
class SizeofExpr : public Expression {
  public:
	Expression * expr;
	Type * type;
	bool isType;

	SizeofExpr( Expression * expr );
	SizeofExpr( const SizeofExpr & other );
	SizeofExpr( Type * type );
	virtual ~SizeofExpr();

	Expression * get_expr() const { return expr; }
	void set_expr( Expression * newValue ) { expr = newValue; }
	Type * get_type() const { return type; }
	void set_type( Type * newValue ) { type = newValue; }
	bool get_isType() const { return isType; }
	void set_isType( bool newValue ) { isType = newValue; }

	virtual SizeofExpr * clone() const override { return new SizeofExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// AlignofExpr represents an alignof expression
class AlignofExpr : public Expression {
  public:
	Expression * expr;
	Type * type;
	bool isType;

	AlignofExpr( Expression * expr );
	AlignofExpr( const AlignofExpr & other );
	AlignofExpr( Type * type );
	virtual ~AlignofExpr();

	Expression * get_expr() const { return expr; }
	void set_expr( Expression * newValue ) { expr = newValue; }
	Type * get_type() const { return type; }
	void set_type( Type * newValue ) { type = newValue; }
	bool get_isType() const { return isType; }
	void set_isType( bool newValue ) { isType = newValue; }

	virtual AlignofExpr * clone() const override { return new AlignofExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// UntypedOffsetofExpr represents an offsetof expression before resolution
class UntypedOffsetofExpr : public Expression {
  public:
	Type * type;
	std::string member;

	UntypedOffsetofExpr( Type * type, const std::string & member );
	UntypedOffsetofExpr( const UntypedOffsetofExpr & other );
	virtual ~UntypedOffsetofExpr();

	std::string get_member() const { return member; }
	void set_member( const std::string & newValue ) { member = newValue; }
	Type * get_type() const { return type; }
	void set_type( Type * newValue ) { type = newValue; }

	virtual UntypedOffsetofExpr * clone() const override { return new UntypedOffsetofExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// OffsetofExpr represents an offsetof expression
class OffsetofExpr : public Expression {
  public:
	Type * type;
	DeclarationWithType * member;

	OffsetofExpr( Type * type, DeclarationWithType * member );
	OffsetofExpr( const OffsetofExpr & other );
	virtual ~OffsetofExpr();

	Type * get_type() const { return type; }
	void set_type( Type * newValue ) { type = newValue; }
	DeclarationWithType * get_member() const { return member; }
	void set_member( DeclarationWithType * newValue ) { member = newValue; }

	virtual OffsetofExpr * clone() const override { return new OffsetofExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// Expression representing a pack of field-offsets for a generic type
class OffsetPackExpr : public Expression {
public:
	StructInstType * type;

	OffsetPackExpr( StructInstType * type );
	OffsetPackExpr( const OffsetPackExpr & other );
	virtual ~OffsetPackExpr();

	StructInstType * get_type() const { return type; }
	void set_type( StructInstType * newValue ) { type = newValue; }

	virtual OffsetPackExpr * clone() const override { return new OffsetPackExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// LogicalExpr represents a short-circuit boolean expression (&& or ||)
class LogicalExpr : public Expression {
  public:
	Expression * arg1;
	Expression * arg2;

	LogicalExpr( Expression * arg1, Expression * arg2, bool andp = true );
	LogicalExpr( const LogicalExpr & other );
	virtual ~LogicalExpr();

	bool get_isAnd() const { return isAnd; }
	Expression * get_arg1() { return arg1; }
	void set_arg1( Expression * newValue ) { arg1 = newValue; }
	Expression * get_arg2() const { return arg2; }
	void set_arg2( Expression * newValue ) { arg2 = newValue; }

	virtual LogicalExpr * clone() const override { return new LogicalExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;

  private:
	bool isAnd;
};

/// ConditionalExpr represents the three-argument conditional ( p ? a : b )
class ConditionalExpr : public Expression {
  public:
	Expression * arg1;
	Expression * arg2;
	Expression * arg3;

	ConditionalExpr( Expression * arg1, Expression * arg2, Expression * arg3 );
	ConditionalExpr( const ConditionalExpr & other );
	virtual ~ConditionalExpr();

	bool get_lvalue() const final;

	Expression * get_arg1() const { return arg1; }
	void set_arg1( Expression * newValue ) { arg1 = newValue; }
	Expression * get_arg2() const { return arg2; }
	void set_arg2( Expression * newValue ) { arg2 = newValue; }
	Expression * get_arg3() const { return arg3; }
	void set_arg3( Expression * newValue ) { arg3 = newValue; }

	virtual ConditionalExpr * clone() const override { return new ConditionalExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// CommaExpr represents the sequence operator ( a, b )
class CommaExpr : public Expression {
  public:
	Expression * arg1;
	Expression * arg2;

	CommaExpr( Expression * arg1, Expression * arg2 );
	CommaExpr( const CommaExpr & other );
	virtual ~CommaExpr();

	bool get_lvalue() const final;

	Expression * get_arg1() const { return arg1; }
	void set_arg1( Expression * newValue ) { arg1 = newValue; }
	Expression * get_arg2() const { return arg2; }
	void set_arg2( Expression * newValue ) { arg2 = newValue; }

	virtual CommaExpr * clone() const override { return new CommaExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// TypeExpr represents a type used in an expression (e.g. as a type generator parameter)
class TypeExpr : public Expression {
  public:
	Type * type;

	TypeExpr( Type * type );
	TypeExpr( const TypeExpr & other );
	virtual ~TypeExpr();

	Type * get_type() const { return type; }
	void set_type( Type * newValue ) { type = newValue; }

	virtual TypeExpr * clone() const override { return new TypeExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// DimensionExpr represents a type-system provided value used in an expression ( forrall([N]) ... N + 1 )
class DimensionExpr : public Expression {
  public:
	std::string name;

	DimensionExpr( std::string name );
	DimensionExpr( const DimensionExpr & other );
	virtual ~DimensionExpr();

	const std::string & get_name() const { return name; }
	void set_name( std::string newValue ) { name = newValue; }

	virtual DimensionExpr * clone() const override { return new DimensionExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// AsmExpr represents a GCC 'asm constraint operand' used in an asm statement: [output] "=f" (result)
class AsmExpr : public Expression {
  public:
	std::string inout;
	Expression * constraint;
	Expression * operand;

	AsmExpr( const std::string * _inout, Expression * constraint, Expression * operand ) : inout( _inout ? *_inout : "" ), constraint( constraint ), operand( operand ) { delete _inout; }
	AsmExpr( const AsmExpr & other );
	virtual ~AsmExpr() { delete constraint; delete operand; };

	virtual AsmExpr * clone() const override { return new AsmExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;

	// https://gcc.gnu.org/onlinedocs/gcc-4.7.1/gcc/Machine-Constraints.html#Machine-Constraints
};

/// ImplicitCopyCtorExpr represents the application of a function to a set of parameters,
/// along with a set of copy constructor calls, one for each argument.
class ImplicitCopyCtorExpr : public Expression {
public:
	ApplicationExpr * callExpr = nullptr;

	ImplicitCopyCtorExpr( ApplicationExpr * callExpr );
	ImplicitCopyCtorExpr( const ImplicitCopyCtorExpr & other );
	virtual ~ImplicitCopyCtorExpr();

	virtual ImplicitCopyCtorExpr * clone() const override { return new ImplicitCopyCtorExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// ConstructorExpr represents the use of a constructor in an expression context, e.g. int * x = malloc() { 5 };
class ConstructorExpr : public Expression {
public:
	Expression * callExpr;

	ConstructorExpr( Expression * callExpr );
	ConstructorExpr( const ConstructorExpr & other );
	~ConstructorExpr();

	bool get_lvalue() const final;

	Expression * get_callExpr() const { return callExpr; }
	void set_callExpr( Expression * newValue ) { callExpr = newValue; }

	virtual ConstructorExpr * clone() const override { return new ConstructorExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// CompoundLiteralExpr represents a C99 'compound literal'
class CompoundLiteralExpr : public Expression {
  public:
	Initializer * initializer;

	CompoundLiteralExpr( Type * type, Initializer * initializer );
	CompoundLiteralExpr( const CompoundLiteralExpr & other );
	virtual ~CompoundLiteralExpr();

	bool get_lvalue() const final;

	Initializer * get_initializer() const { return initializer; }
	void set_initializer( Initializer * i ) { initializer = i; }

	virtual CompoundLiteralExpr * clone() const override { return new CompoundLiteralExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// RangeExpr represents a range e.g. '3 ... 5' or '1~10'
class RangeExpr : public Expression {
  public:
	Expression * low, * high;

	RangeExpr( Expression * low, Expression * high );
	RangeExpr( const RangeExpr & other );

	Expression * get_low() const { return low; }
	Expression * get_high() const { return high; }
	RangeExpr * set_low( Expression * low ) { RangeExpr::low = low; return this; }
	RangeExpr * set_high( Expression * high ) { RangeExpr::high = high; return this; }

	virtual RangeExpr * clone() const override { return new RangeExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// UntypedTupleExpr represents a tuple expression ( [a, b, c] ) before resolution
class UntypedTupleExpr : public Expression {
  public:
	std::list<Expression*> exprs;

	UntypedTupleExpr( const std::list< Expression * > & exprs );
	UntypedTupleExpr( const UntypedTupleExpr & other );
	virtual ~UntypedTupleExpr();

	std::list<Expression*>& get_exprs() { return exprs; }

	virtual UntypedTupleExpr * clone() const override { return new UntypedTupleExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// TupleExpr represents a tuple expression ( [a, b, c] )
class TupleExpr : public Expression {
  public:
	std::list<Expression*> exprs;

	TupleExpr( const std::list< Expression * > & exprs );
	TupleExpr( const TupleExpr & other );
	virtual ~TupleExpr();

	bool get_lvalue() const final;

	std::list<Expression*>& get_exprs() { return exprs; }

	virtual TupleExpr * clone() const override { return new TupleExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// TupleIndexExpr represents an element selection operation on a tuple value, e.g. t.3 after processing by the expression analyzer
class TupleIndexExpr : public Expression {
  public:
	Expression * tuple;
	unsigned int index;

	TupleIndexExpr( Expression * tuple, unsigned int index );
	TupleIndexExpr( const TupleIndexExpr & other );
	virtual ~TupleIndexExpr();

	bool get_lvalue() const final;

	Expression * get_tuple() const { return tuple; }
	int get_index() const { return index; }
	TupleIndexExpr * set_tuple( Expression * newValue ) { tuple = newValue; return this; }
	TupleIndexExpr * set_index( unsigned int newValue ) { index = newValue; return this; }

	virtual TupleIndexExpr * clone() const override { return new TupleIndexExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// TupleAssignExpr represents a multiple assignment operation, where both sides of the assignment have tuple type, e.g. [a, b, c] = [d, e, f];, a mass assignment operation, where the left hand side has tuple type and the right hand side does not, e.g. [a, b, c] = 5.0;, or a tuple ctor/dtor expression
class TupleAssignExpr : public Expression {
  public:
	StmtExpr * stmtExpr = nullptr;

	TupleAssignExpr( const std::list< Expression * > & assigns, const std::list< ObjectDecl * > & tempDecls );
	TupleAssignExpr( const TupleAssignExpr & other );
	virtual ~TupleAssignExpr();

	TupleAssignExpr * set_stmtExpr( StmtExpr * newValue ) { stmtExpr = newValue; return this; }
	StmtExpr * get_stmtExpr() const { return stmtExpr; }

	virtual TupleAssignExpr * clone() const override { return new TupleAssignExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;

	friend class ConverterNewToOld;
  private:
    TupleAssignExpr( StmtExpr * stmts );
};

/// StmtExpr represents a GCC 'statement expression', e.g. ({ int x = 5; x; })
class StmtExpr : public Expression {
public:
	CompoundStmt * statements;
	std::list< ObjectDecl * > returnDecls; // return variable(s) for stmt expression
	std::list< Expression * > dtors; // destructor(s) for return variable(s)

	// readonly
	ExprStmt * resultExpr = nullptr;

	StmtExpr( CompoundStmt * statements );
	StmtExpr( const StmtExpr & other );
	virtual ~StmtExpr();

	bool get_lvalue() const final;

	CompoundStmt * get_statements() const { return statements; }
	StmtExpr * set_statements( CompoundStmt * newValue ) { statements = newValue; return this; }

	// call to set the result type of this StmtExpr based on its body
	void computeResult();

	std::list< ObjectDecl * > & get_returnDecls() { return returnDecls; }
	std::list< Expression * > & get_dtors() { return dtors; }

	virtual StmtExpr * clone() const override { return new StmtExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class UniqueExpr : public Expression {
public:
	Expression * expr;
	ObjectDecl * object;
	VariableExpr * var;

	UniqueExpr( Expression * expr, long long idVal = -1 );
	UniqueExpr( const UniqueExpr & other );
	~UniqueExpr();

	Expression * get_expr() const { return expr; }
	UniqueExpr * set_expr( Expression * newValue ) { expr = newValue; return this; }

	ObjectDecl * get_object() const { return object; }
	UniqueExpr * set_object( ObjectDecl * newValue ) { object = newValue; return this; }

	VariableExpr * get_var() const { return var; }
	UniqueExpr * set_var( VariableExpr * newValue ) { var = newValue; return this; }

	int get_id() const { return id; }

	virtual UniqueExpr * clone() const override { return new UniqueExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;

private:
	int id;
	static long long count;
};

struct InitAlternative {
public:
	Type * type = nullptr;
	Designation * designation = nullptr;
	InitAlternative( Type * type, Designation * designation );
	InitAlternative( const InitAlternative & other );
	InitAlternative & operator=( const Initializer & other ) = delete; // at the moment this isn't used, and I don't want to implement it
	~InitAlternative();
};

class UntypedInitExpr : public Expression {
public:
	Expression * expr;
	std::list<InitAlternative> initAlts;

	UntypedInitExpr( Expression * expr, const std::list<InitAlternative> & initAlts );
	UntypedInitExpr( const UntypedInitExpr & other );
	~UntypedInitExpr();

	Expression * get_expr() const { return expr; }
	UntypedInitExpr * set_expr( Expression * newValue ) { expr = newValue; return this; }

	std::list<InitAlternative> & get_initAlts() { return initAlts; }

	virtual UntypedInitExpr * clone() const override { return new UntypedInitExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class InitExpr : public Expression {
public:
	Expression * expr;
	Designation * designation;

	InitExpr( Expression * expr, Designation * designation );
	InitExpr( const InitExpr & other );
	~InitExpr();

	Expression * get_expr() const { return expr; }
	InitExpr * set_expr( Expression * newValue ) { expr = newValue; return this; }

	Designation * get_designation() const { return designation; }
	InitExpr * set_designation( Designation * newValue ) { designation = newValue; return this; }

	virtual InitExpr * clone() const override { return new InitExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// expression that contains a deleted identifier - should never make it past the resolver.
class DeletedExpr : public Expression {
public:
	Expression * expr;
	Declaration * deleteStmt;

	DeletedExpr( Expression * expr, Declaration * deleteStmt );
	DeletedExpr( const DeletedExpr & other );
	~DeletedExpr();

	virtual DeletedExpr * clone() const override { return new DeletedExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// expression wrapping the use of a default argument - should never make it past the resolver.
class DefaultArgExpr : public Expression {
public:
	Expression * expr;

	DefaultArgExpr( Expression * expr );
	DefaultArgExpr( const DefaultArgExpr & other );
	~DefaultArgExpr();

	virtual DefaultArgExpr * clone() const override { return new DefaultArgExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// C11 _Generic expression
class GenericExpr : public Expression {
public:
	struct Association {
		Type * type = nullptr;
		Expression * expr = nullptr;
		bool isDefault = false;

		Association( Type * type, Expression * expr );
		Association( Expression * expr );
		Association( const Association & other );
		Association & operator=( const Association & other ) = delete; // at the moment this isn't used, and I don't want to implement it
		~Association();
	};

	Expression * control;
	std::list<Association> associations;

	GenericExpr( Expression * control, const std::list<Association> & assoc );
	GenericExpr( const GenericExpr & other );
	virtual ~GenericExpr();

	virtual GenericExpr * clone() const override { return new GenericExpr( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Expression * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
