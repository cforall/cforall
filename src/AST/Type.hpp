//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Type.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Thu May 9 10:00:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Thu Apr  6 15:58:00 2023
// Update Count     : 9
//

#pragma once

#include <cassert>
#include <cstddef>           // for nullptr_t
#include <cstdint>           // for uintptr_t
#include <utility>           // for move
#include <vector>

#include "BasicKind.hpp"     // for BasicKind
#include "CVQualifiers.hpp"
#include "Decl.hpp"          // for AggregateDecl subclasses
#include "Fwd.hpp"
#include "Node.hpp"          // for Node, ptr, ptr_base
#include "Visitor.hpp"

// Must be included in *all* AST classes; should be #undef'd at the end of the file
#define MUTATE_FRIEND \
	template<typename node_t> friend node_t * mutate(const node_t * node); \
	template<typename node_t> friend node_t * shallowCopy(const node_t * node);

namespace ast {

class Type : public Node {
public:
	CV::Qualifiers qualifiers;
	std::vector<ptr<Attribute>> attributes;

	Type( CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} )
	: qualifiers(q), attributes(std::move(as)) {}

	bool is_const() const { return qualifiers.is_const; }
	bool is_volatile() const { return qualifiers.is_volatile; }
	bool is_restrict() const { return qualifiers.is_restrict; }
	bool is_mutex() const { return qualifiers.is_mutex; }
	bool is_atomic() const { return qualifiers.is_atomic; }

	Type * set_const( bool v ) { qualifiers.is_const = v; return this; }
	Type * set_volatile( bool v ) { qualifiers.is_volatile = v; return this; }
	Type * set_restrict( bool v ) { qualifiers.is_restrict = v; return this; }
	Type * set_mutex( bool v ) { qualifiers.is_mutex = v; return this; }
	Type * set_atomic( bool v ) { qualifiers.is_atomic = v; return this; }

	/// How many elemental types are represented by this type
	virtual unsigned size() const { return 1; }
	/// Is this a void type?
	virtual bool isVoid() const { return size() == 0; }
	/// Get the i'th component of this type
	virtual const Type * getComponent( unsigned i ) const;

	/// type without outer pointers and arrays
	const Type * stripDeclarator() const;
	/// type without outer references
	const Type * stripReferences() const;
	/// number of reference occuring consecutively on the outermost layer of this type
	/// (i.e. do not count references nested within other types)
	virtual unsigned referenceDepth() const { return 0; }
	/// true iff type is complete type (i.e. compiler knows the size, alignment, and layout)
	virtual bool isComplete() const { return true; }

	virtual const Type * accept( Visitor & v ) const override = 0;
private:
	virtual Type * clone() const override = 0;
	MUTATE_FRIEND
};

/// Clear/reset the qualifiers on this type, cloning only if necessary
template< enum Node::ref_type ref_t >
void reset_qualifiers( ptr_base< Type, ref_t > & p, CV::Qualifiers q = {} ) {
	if ( p->qualifiers != q ) p.get_and_mutate()->qualifiers = q;
}

/// Add the specified qualifiers to this type, cloning only if necessary
template< enum Node::ref_type ref_t >
void add_qualifiers( ptr_base< Type, ref_t > & p, CV::Qualifiers q ) {
	if ( ( p->qualifiers & q ) != q ) p.get_and_mutate()->qualifiers |= q;
}

/// Remove the specified qualifiers from this type, cloning only if necessary
template< enum Node::ref_type ref_t >
void remove_qualifiers( ptr_base< Type, ref_t > & p, CV::Qualifiers q ) {
	if ( ( p->qualifiers & q ) != 0 ) p.get_and_mutate()->qualifiers -= q;
}

/// `void`
class VoidType final : public Type {
public:
	VoidType( CV::Qualifiers q = {} ) : Type( q ) {}

	unsigned size() const override { return 0; }
	bool isVoid() const override { return true; }
	bool isComplete() const override { return false; }

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	VoidType * clone() const override { return new VoidType{ *this }; }
	MUTATE_FRIEND
};

/// Built-in arithmetic type
class BasicType final : public Type {
public:
	BasicKind kind;

	/// string names of basic types; generated to match with Kind
	static const char *typeNames[];

	BasicType( BasicKind k, CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} )
	: Type(q, std::move(as)), kind(k) {}

	/// Check if this type represents an integer type
	bool isInteger() const { return kind <= MAX_INTEGER_TYPE; }

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	BasicType * clone() const override { return new BasicType{ *this }; }
	MUTATE_FRIEND
};

/// Pointer/array variable length?
enum LengthFlag { FixedLen, VariableLen };

/// Pointer/array static dimension?
enum DimensionFlag { DynamicDim, StaticDim };

/// Pointer type `T*`
class PointerType final : public Type {
public:
	ptr<Type> base;

	// In C99, pointer types can be qualified in many ways, e.g. `int a[ static 3 ]`
	ptr<Expr> dimension;
	LengthFlag isVarLen = FixedLen;
	DimensionFlag isStatic = DynamicDim;

	PointerType( const Type * b, CV::Qualifiers q = {} ) : Type(q), base(b), dimension() {}
	PointerType( const Type * b, const Expr * d, LengthFlag vl, DimensionFlag s,
		CV::Qualifiers q = {} ) : Type(q), base(b), dimension(d), isVarLen(vl), isStatic(s) {}

	// true if this pointer is actually an array
	bool isArray() const { return isVarLen || isStatic || dimension; }

	bool isComplete() const override { return ! isVarLen; }

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	PointerType * clone() const override { return new PointerType{ *this }; }
	MUTATE_FRIEND
};

/// Array type `T[]`
class ArrayType final : public Type {
public:
	ptr<Type> base;
	ptr<Expr> dimension;
	LengthFlag isVarLen;
	DimensionFlag isStatic;

	ArrayType( const Type * b, const Expr * d, LengthFlag vl, DimensionFlag s,
		CV::Qualifiers q = {} ) : Type(q), base(b), dimension(d), isVarLen(vl), isStatic(s) {}

	// array types are complete if they have a dimension expression or are
	// VLAs ('*' in parameter declaration), and incomplete otherwise.
	// See 6.7.6.2
	bool isComplete() const override { return dimension || isVarLen; }

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ArrayType * clone() const override { return new ArrayType{ *this }; }
	MUTATE_FRIEND
};

/// Reference type `T&`
class ReferenceType final : public Type {
public:
	ptr<Type> base;

	ReferenceType( const Type * b, CV::Qualifiers q = {} ) : Type(q), base(b) {}

	unsigned referenceDepth() const override { return base->referenceDepth() + 1; }

	// Since reference types act like value types, their size is the size of the base.
	// This makes it simple to cast the empty tuple to a reference type, since casts that increase
	// the number of values are disallowed.
	unsigned size() const override { return base->size(); }

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ReferenceType * clone() const override { return new ReferenceType{ *this }; }
	MUTATE_FRIEND
};

/// Qualified type `P.C`
class QualifiedType final : public Type {
public:
	ptr<Type> parent;
	ptr<Type> child;

	QualifiedType( const Type * p, const Type * c, CV::Qualifiers q = {} )
	: Type(q), parent(p), child(c) {}

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	QualifiedType * clone() const override { return new QualifiedType{ *this }; }
	MUTATE_FRIEND
};

/// Type of a function `[R1, R2](*)(P1, P2, P3)`
class FunctionType final : public Type {
public:
	using ForallList = std::vector<ptr<TypeInstType>>;
	using AssertionList = std::vector<ptr<VariableExpr>>;
	ForallList forall;
	AssertionList assertions;

	std::vector<ptr<Type>> returns;
	std::vector<ptr<Type>> params;

	/// Does the function accept a variable number of arguments following the arguments specified
	/// in the parameters list.
	/// This could be because of
	/// - an ellipsis in a prototype declaration
	/// - an unprototyped declaration
	ArgumentFlag isVarArgs;

	FunctionType( ArgumentFlag va = FixedArgs, CV::Qualifiers q = {} )
	: Type(q), returns(), params(), isVarArgs(va) {}

	FunctionType( const FunctionType & o ) = default;

	/// true if either the parameters or return values contain a tttype
	bool isTtype() const;
	/// true if function parameters are unconstrained by prototype
	bool isUnprototyped() const { return isVarArgs && params.size() == 0; }

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	FunctionType * clone() const override { return new FunctionType{ *this }; }
	MUTATE_FRIEND
};

/// base class for types that refer to types declared elsewhere (aggregates and typedefs)
class BaseInstType : public Type {
public:
	std::vector<ptr<Expr>> params;
	std::string name;
	bool hoistType = false;

	BaseInstType(
		const std::string& n, CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} )
	: Type(q, std::move(as)), params(), name(n) {}

	BaseInstType(
		const std::string& n, std::vector<ptr<Expr>> && params,
		CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} )
	: Type(q, std::move(as)), params(std::move(params)), name(n) {}

	BaseInstType( const BaseInstType & o ) = default;

	/// Gets aggregate declaration this type refers to
	virtual const AggregateDecl * aggr() const = 0;
	/// Looks up member declarations with given name
	std::vector<readonly<Decl>> lookup( const std::string & name ) const;

private:
	virtual BaseInstType * clone() const override = 0;
	MUTATE_FRIEND
};

// Common implementation for the SUE instance types. Not to be used directly.
template<typename decl_t>
class SueInstType final : public BaseInstType {
public:
	using base_type = decl_t;
	readonly<decl_t> base;

	SueInstType(
		const std::string& n, CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} )
	: BaseInstType( n, q, std::move(as) ), base() {}

	SueInstType(
		const base_type * b, CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} );

	SueInstType(
		const base_type * b, std::vector<ptr<Expr>> && params,
		CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} );

	bool isComplete() const override;

	const decl_t * aggr() const override { return base; }

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	SueInstType<decl_t> * clone() const override { return new SueInstType<decl_t>{ *this }; }
	MUTATE_FRIEND
};

/// An instance of a struct type.
using StructInstType = SueInstType<StructDecl>;

/// An instance of a union type.
using UnionInstType = SueInstType<UnionDecl>;

/// An instance of an enum type.
using EnumInstType = SueInstType<EnumDecl>;

/// An instance of a trait type.
class TraitInstType final : public BaseInstType {
public:
	readonly<TraitDecl> base;

	TraitInstType(
		const std::string& n, CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} )
	: BaseInstType( n, q, std::move(as) ), base() {}

	TraitInstType(
		const TraitDecl * b, CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} );

	// not meaningful for TraitInstType
	bool isComplete() const override { assert(false); }

	const TraitDecl * aggr() const override { return base; }

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	TraitInstType * clone() const override { return new TraitInstType{ *this }; }
	MUTATE_FRIEND
};

struct TypeEnvKey;

/// instance of named type alias (typedef, variable, or even, just after parsing, the name of a struct)
class TypeInstType final : public BaseInstType {
public:
	readonly<TypeDecl> base;
	// previously from renameTyVars; now directly use integer fields instead of synthesized strings
	// a nonzero value of formal_usage indicates a formal type (only used in function type)
	// a zero value of formal_usage indicates an actual type (referenced inside body of parametric structs and functions)
	TypeDecl::Kind kind;
	int formal_usage = 0;
	int expr_id = 0;

	bool operator==(const TypeInstType & other) const;

	TypeInstType(
		const std::string& n, const TypeDecl * b, CV::Qualifiers q = {},
		std::vector<ptr<Attribute>> && as = {} )
	: BaseInstType( n, q, std::move(as) ), base( b ), kind( b->kind ) {}

	TypeInstType( const TypeDecl * b,
		CV::Qualifiers q = {}, std::vector<ptr<Attribute>> && as = {} );

	TypeInstType( const std::string& n, TypeDecl::Kind k, CV::Qualifiers q = {},
		std::vector<ptr<Attribute>> && as = {} )
	: BaseInstType( n, q, std::move(as) ), base(), kind( k ) {}

	TypeInstType( const TypeInstType & o ) = default;

	explicit TypeInstType( const TypeEnvKey & key );

	/// sets `base`, updating `kind` correctly
	void set_base( const TypeDecl * );

	bool isComplete() const override;

	// not meaningful for TypeInstType
	const AggregateDecl * aggr() const override { assert(false); }

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }

	std::string typeString() const;
private:
	TypeInstType * clone() const override { return new TypeInstType{ *this }; }
	MUTATE_FRIEND
};

/// Compact representation of TypeInstType used for map lookups.
struct TypeEnvKey {
	const TypeDecl * base = nullptr;
	int formal_usage = 0;
	int expr_id = 0;

	TypeEnvKey() = default;
	TypeEnvKey(const TypeDecl * base, int formal_usage = 0, int expr_id = 0)
	: base(base), formal_usage(formal_usage), expr_id(expr_id) {}
	TypeEnvKey(const TypeInstType & inst)
	: base(inst.base), formal_usage(inst.formal_usage), expr_id(inst.expr_id) {}
	std::string typeString() const;
	bool operator==(const TypeEnvKey & other) const;
	bool operator<(const TypeEnvKey & other) const;
	operator bool() {return base;}
};


/// tuple type e.g. `[int, char]`
class TupleType final : public Type {
public:
	std::vector<ptr<Type>> types;

	TupleType( std::vector<ptr<Type>> && ts, CV::Qualifiers q = {} );

	// collection simulation
	using iterator = std::vector<ptr<Type>>::const_iterator;
	iterator begin() const { return types.begin(); }
	iterator end() const { return types.end(); }

	unsigned size() const override { return types.size(); }

	const Type * getComponent( unsigned i ) const override {
		assertf( i < size(), "TupleType::getComponent: index %d must be less than size %d",
			i, size() );
		return *(begin()+i);
	}

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	TupleType * clone() const override { return new TupleType{ *this }; }
	MUTATE_FRIEND
};

/// Type of unresolved `typeof()` expression
class TypeofType : public Type {
public:
	ptr<Expr> expr;
	enum Kind { Typeof, Basetypeof } kind;

	TypeofType( const Expr * e, Kind k = Typeof, CV::Qualifiers q = {} )
	: Type(q), expr(e), kind(k) {}

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	TypeofType * clone() const override { return new TypeofType{ *this }; }
	MUTATE_FRIEND
};

/// Virtual Table Type `vtable(T)`
class VTableType final : public Type {
public:
	ptr<Type> base;

	VTableType( const Type * b, CV::Qualifiers q = {} ) : Type(q), base(b) {}

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	VTableType * clone() const override { return new VTableType{ *this }; }
	MUTATE_FRIEND
};

/// GCC built-in varargs type
class VarArgsType final : public Type {
public:
	VarArgsType( CV::Qualifiers q = {} ) : Type( q ) {}

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	VarArgsType * clone() const override { return new VarArgsType{ *this }; }
	MUTATE_FRIEND
};

/// Type of zero constant `0`
class ZeroType final : public Type {
public:
	ZeroType( CV::Qualifiers q = {} ) : Type( q ) {}

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ZeroType * clone() const override { return new ZeroType{ *this }; }
	MUTATE_FRIEND
};

/// Type of one constant `1`
class OneType final : public Type {
public:
	OneType( CV::Qualifiers q = {} ) : Type( q ) {}

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	OneType * clone() const override { return new OneType{ *this }; }
	MUTATE_FRIEND
};

/// Parent type for scope-qualified types at global scope
class GlobalScopeType final : public Type {
public:
	GlobalScopeType() : Type() {}

	const Type * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	GlobalScopeType * clone() const override { return new GlobalScopeType{ *this }; }
	MUTATE_FRIEND
};

bool isUnboundType(const Type * type);

}

namespace std {
	template<>
	struct hash<typename ast::TypeEnvKey> {
		size_t operator() (const ast::TypeEnvKey & x) const {
			const size_t p = 1000007;
			size_t res = reinterpret_cast<size_t>(x.base);
			res = p * res + x.formal_usage;
			res = p * res + x.expr_id;
			return res;
		}
	};
}

#undef MUTATE_FRIEND

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
