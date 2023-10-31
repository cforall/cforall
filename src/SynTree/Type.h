//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Type.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 19 22:37:10 2023
// Update Count     : 176
//

#pragma once

#include <strings.h>         // for ffs
#include <cassert>           // for assert, assertf
#include <list>              // for list, _List_iterator
#include <ostream>           // for ostream, operator<<, basic_ostream
#include <string>            // for string

#include "BaseSyntaxNode.h"  // for BaseSyntaxNode
#include "Common/Iterate.hpp"// for operator+
#include "Mutator.h"         // for Mutator
#include "SynTree.h"         // for AST nodes
#include "Visitor.h"         // for Visitor

class Type : public BaseSyntaxNode {
  public:
	// Simulate inheritance because union does not allow it.
	// Bug in g++-4.9 prevents static field in union
	//static const char * Names[];
	#define BFCommon( BFType, N ) \
		bool operator[]( unsigned int i ) const { return val & (1 << i); } \
		bool any() const { return val != 0; } \
		void reset() { val = 0; } \
		int ffs() { return ::ffs( val ) - 1; } \
		BFType operator&=( BFType other ) { \
			val &= other.val; return *this; \
		} \
	 	BFType operator&( BFType other ) const { \
			BFType q = other; \
			q &= *this; \
			return q; \
		} \
	 	BFType operator|=( BFType other ) { \
			val |= other.val; return *this; \
		} \
	 	BFType operator|( BFType other ) const { \
			BFType q = other; \
			q |= *this; \
			return q; \
		} \
	 	BFType operator-=( BFType other ) { \
			val &= ~other.val; return *this; \
		} \
		void print( std::ostream & os ) const { \
			if ( (*this).any() ) { \
				for ( unsigned int i = 0; i < N; i += 1 ) { \
					if ( (*this)[i] ) { \
						os << BFType##Names[i] << ' '; \
					} \
				} \
			} \
		}

	// enum must remain in the same order as the corresponding bit fields.

	enum { Inline = 1 << 0, Noreturn = 1 << 1, Fortran = 1 << 2, NumFuncSpecifier = 3 };
	static const char * FuncSpecifiersNames[];
	union FuncSpecifiers {
		unsigned int val;
		struct {
			bool is_inline : 1;
			bool is_noreturn : 1;
			bool is_fortran : 1;
		};
		FuncSpecifiers() : val( 0 ) {}
		FuncSpecifiers( unsigned int val ) : val( val ) {}
		// equality (==, !=) works implicitly on first field "val", relational operations are undefined.
		BFCommon( FuncSpecifiers, NumFuncSpecifier )
	}; // FuncSpecifiers

	enum { Extern = 1 << 0, Static = 1 << 1, Auto = 1 << 2, Register = 1 << 3, ThreadlocalGcc = 1 << 4, ThreadlocalC11 = 1 << 5, NumStorageClass = 6 };
	static const char * StorageClassesNames[];
	union StorageClasses {
		unsigned int val;
		struct {
			bool is_extern : 1;
			bool is_static : 1;
			bool is_auto : 1;
			bool is_register : 1;
			bool is_threadlocalGcc : 1;
			bool is_threadlocalC11 : 1;
		};

		StorageClasses() : val( 0 ) {}
		StorageClasses( unsigned int val ) : val( val ) {}
		// equality (==, !=) works implicitly on first field "val", relational operations are undefined.
		BFCommon( StorageClasses, NumStorageClass )

		bool is_threadlocal_any() { return this->is_threadlocalC11 || this->is_threadlocalGcc; }
	}; // StorageClasses

	enum { Const = 1 << 0, Restrict = 1 << 1, Volatile = 1 << 2, Mutex = 1 << 3, Atomic = 1 << 4, NumTypeQualifier = 5 };
	static const char * QualifiersNames[];
	union Qualifiers {
		enum { Mask = ~Restrict };
		unsigned int val;
		struct {
			bool is_const : 1;
			bool is_restrict : 1;
			bool is_volatile : 1;
			bool is_mutex : 1;
			bool is_atomic : 1;
		};

		Qualifiers() : val( 0 ) {}
		Qualifiers( unsigned int val ) : val( val ) {}
		// Complex comparisons provide implicit qualifier downcasting, e.g., T downcast to const T.
		bool operator==( Qualifiers other ) const { return (val & Mask) == (other.val & Mask); }
		bool operator!=( Qualifiers other ) const { return (val & Mask) != (other.val & Mask); }
		bool operator<=( Qualifiers other ) const {
			return is_const    <= other.is_const        // Any non-const converts to const without cost
				&& is_volatile <= other.is_volatile		// Any non-volatile converts to volatile without cost
				&& is_mutex    >= other.is_mutex		// Any mutex converts to non-mutex without cost
				&& is_atomic   == other.is_atomic;		// No conversion from atomic to non atomic is free
		}
		bool operator<( Qualifiers other ) const { return *this != other && *this <= other; }
	 	bool operator>=( Qualifiers other ) const { return ! (*this < other); }
	 	bool operator>( Qualifiers other ) const { return *this != other && *this >= other; }
		BFCommon( Qualifiers, NumTypeQualifier )

		Qualifiers unify( Qualifiers const & other ) const {
			int or_flags = Mask & (val | other.val);
			int and_flags = val & other.val;
			return Qualifiers( or_flags | and_flags );
		}
	}; // Qualifiers

	typedef std::list<TypeDecl *> ForallList;

	Qualifiers tq;
	ForallList forall;
	std::list< Attribute * > attributes;

	Type( const Qualifiers & tq, const std::list< Attribute * > & attributes );
	Type( const Type & other );
	virtual ~Type();

	Qualifiers & get_qualifiers() { return tq; }
	bool get_const() const { return tq.is_const; }
	bool get_volatile() const { return tq.is_volatile; }
	bool get_restrict() const { return tq.is_restrict; }
	bool get_mutex() const { return tq.is_mutex; }
	bool get_atomic() const { return tq.is_atomic; }
	void set_const( bool newValue ) { tq.is_const = newValue; }
	void set_volatile( bool newValue ) { tq.is_volatile = newValue; }
	void set_restrict( bool newValue ) { tq.is_restrict = newValue; }
	void set_mutex( bool newValue ) { tq.is_mutex = newValue; }
	void set_atomic( bool newValue ) { tq.is_atomic = newValue; }

	ForallList& get_forall() { return forall; }

	std::list< Attribute * >& get_attributes() { return attributes; }
	const std::list< Attribute * >& get_attributes() const { return attributes; }

	/// How many elemental types are represented by this type
	virtual unsigned size() const { return 1; };
	virtual bool isVoid() const { return size() == 0; }
	virtual Type * getComponent( unsigned i ) { assertf( size() == 1 && i == 0, "Type::getComponent was called with size %d and index %d\n", size(), i ); return this; }

	/// return type without outer pointers and arrays
	Type * stripDeclarator();

	/// return type without outer references
	Type * stripReferences();
	const Type * stripReferences() const;

	/// return the number of references occuring consecutively on the outermost layer of this type (i.e. do not count references nested within other types)
	virtual int referenceDepth() const;

	virtual bool isComplete() const { return true; }

	virtual AggregateDecl * getAggr() const;

	virtual TypeSubstitution genericSubstitution() const;

	virtual Type * clone() const = 0;
	virtual void accept( Visitor & v ) = 0;
	virtual void accept( Visitor & v ) const = 0;
	virtual Type * acceptMutator( Mutator & m ) = 0;
	virtual void print( std::ostream & os, Indenter indent = {} ) const;
};

extern const Type::FuncSpecifiers noFuncSpecifiers;
extern const Type::StorageClasses noStorageClasses;
extern const Type::Qualifiers noQualifiers;			// no qualifiers on constants

class VoidType : public Type {
  public:
	VoidType( const Type::Qualifiers & tq, const std::list< Attribute * > & attributes = std::list< Attribute * >() );

	virtual unsigned size() const override { return 0; };
	virtual bool isComplete() const override { return false; }

	virtual VoidType * clone() const override { return new VoidType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class BasicType : public Type {
  public:
	// GENERATED START, DO NOT EDIT
	// GENERATED BY c/BasicTypes-gen.cc
	enum Kind {
		Bool,
		Char,
		SignedChar,
		UnsignedChar,
		ShortSignedInt,
		ShortUnsignedInt,
		SignedInt,
		UnsignedInt,
		LongSignedInt,
		LongUnsignedInt,
		LongLongSignedInt,
		LongLongUnsignedInt,
		SignedInt128,
		UnsignedInt128,
		uFloat16,
		uFloat16Complex,
		uFloat32,
		uFloat32Complex,
		Float,
		FloatComplex,
		uFloat32x,
		uFloat32xComplex,
		uFloat64,
		uFloat64Complex,
		Double,
		DoubleComplex,
		uFloat64x,
		uFloat64xComplex,
		uuFloat80,
		uFloat128,
		uFloat128Complex,
		uuFloat128,
		LongDouble,
		LongDoubleComplex,
		uFloat128x,
		uFloat128xComplex,
		NUMBER_OF_BASIC_TYPES
	} kind;
	// GENERATED END

	static const char * typeNames[];					// string names for basic types, MUST MATCH with Kind

	BasicType( const Type::Qualifiers & tq, Kind bt, const std::list< Attribute * > & attributes = std::list< Attribute * >() );

	Kind get_kind() const { return kind; }
	void set_kind( Kind newValue ) { kind = newValue; }

	virtual BasicType * clone() const override { return new BasicType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
	bool isInteger() const;
};

class PointerType : public Type {
  public:
	Type * base;

	// In C99, pointer types can be qualified in many ways e.g., int f( int a[ static 3 ] )
	Expression * dimension;
	bool isVarLen;
	bool isStatic;

	PointerType( const Type::Qualifiers & tq, Type * base, const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	PointerType( const Type::Qualifiers & tq, Type * base, Expression * dimension, bool isVarLen, bool isStatic, const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	PointerType( const PointerType& );
	virtual ~PointerType();

	Type * get_base() { return base; }
	void set_base( Type * newValue ) { base = newValue; }
	Expression * get_dimension() { return dimension; }
	void set_dimension( Expression * newValue ) { dimension = newValue; }
	bool get_isVarLen() { return isVarLen; }
	void set_isVarLen( bool newValue ) { isVarLen = newValue; }
	bool get_isStatic() { return isStatic; }
	void set_isStatic( bool newValue ) { isStatic = newValue; }

	bool is_array() const { return isStatic || isVarLen || dimension; }

	virtual bool isComplete() const override { return ! isVarLen; }

	virtual PointerType * clone() const override { return new PointerType( * this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class ArrayType : public Type {
  public:
	Type * base;
	Expression * dimension;
	bool isVarLen;
	bool isStatic;

	ArrayType( const Type::Qualifiers & tq, Type * base, Expression * dimension, bool isVarLen, bool isStatic, const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	ArrayType( const ArrayType& );
	virtual ~ArrayType();

	Type * get_base() { return base; }
	void set_base( Type * newValue ) { base = newValue; }
	Expression * get_dimension() { return dimension; }
	void set_dimension( Expression * newValue ) { dimension = newValue; }
	bool get_isVarLen() { return isVarLen; }
	void set_isVarLen( bool newValue ) { isVarLen = newValue; }
	bool get_isStatic() { return isStatic; }
	void set_isStatic( bool newValue ) { isStatic = newValue; }

	// array types are complete if they have a dimension expression or are
	// VLAs ('*' in parameter declaration), and incomplete otherwise.
	// See 6.7.6.2
	virtual bool isComplete() const override { return dimension || isVarLen; }

	virtual ArrayType * clone() const override { return new ArrayType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class QualifiedType : public Type {
public:
	Type * parent;
	Type * child;
	QualifiedType( const Type::Qualifiers & tq, Type * parent, Type * child );
	QualifiedType( const QualifiedType & tq );
	virtual ~QualifiedType();

	virtual QualifiedType * clone() const override { return new QualifiedType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class ReferenceType : public Type {
public:
	Type * base;

	ReferenceType( const Type::Qualifiers & tq, Type * base, const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	ReferenceType( const ReferenceType & );
	virtual ~ReferenceType();

	Type * get_base() { return base; }
	void set_base( Type * newValue ) { base = newValue; }

	virtual int referenceDepth() const override;

	// Since reference types act like value types, their size is the size of the base.
	// This makes it simple to cast the empty tuple to a reference type, since casts that increase
	// the number of values are disallowed.
	virtual unsigned size() const override { return base->size(); }

	virtual TypeSubstitution genericSubstitution() const override;

	virtual ReferenceType * clone() const override { return new ReferenceType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class FunctionType : public Type {
  public:
	std::list<DeclarationWithType*> returnVals;
	std::list<DeclarationWithType*> parameters;

	// Does the function accept a variable number of arguments following the arguments specified in the parameters list.
	// This could be because of
	// - an ellipsis in a prototype declaration
	// - an unprototyped declaration
	bool isVarArgs;

	FunctionType( const Type::Qualifiers & tq, bool isVarArgs, const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	FunctionType( const FunctionType& );
	virtual ~FunctionType();

	std::list<DeclarationWithType*> & get_returnVals() { return returnVals; }
	std::list<DeclarationWithType*> & get_parameters() { return parameters; }
	bool get_isVarArgs() const { return isVarArgs; }
	void set_isVarArgs( bool newValue ) { isVarArgs = newValue; }
	bool isTtype() const;

	bool isUnprototyped() const { return isVarArgs && parameters.size() == 0; }

	virtual FunctionType * clone() const override { return new FunctionType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class ReferenceToType : public Type {
  public:
	std::list< Expression * > parameters;
	std::string name;
	bool hoistType;

	ReferenceToType( const Type::Qualifiers & tq, const std::string & name, const std::list< Attribute * > & attributes );
	ReferenceToType( const ReferenceToType & other );
	virtual ~ReferenceToType();

	const std::string & get_name() const { return name; }
	void set_name( std::string newValue ) { name = newValue; }
	std::list< Expression* >& get_parameters() { return parameters; }
	bool get_hoistType() const { return hoistType; }
	void set_hoistType( bool newValue ) { hoistType = newValue; }

	virtual ReferenceToType * clone() const override = 0;
	virtual void accept( Visitor & v ) override = 0;
	virtual Type * acceptMutator( Mutator & m ) override = 0;
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;

	virtual void lookup( __attribute__((unused)) const std::string & name, __attribute__((unused)) std::list< Declaration* > & foundDecls ) const {}
  protected:
	virtual std::string typeString() const = 0;
};

class StructInstType : public ReferenceToType {
	typedef ReferenceToType Parent;
  public:
	// this decl is not "owned" by the struct inst; it is merely a pointer to elsewhere in the tree,
	// where the structure used in this type is actually defined
	StructDecl * baseStruct;

	StructInstType( const Type::Qualifiers & tq, const std::string & name, const std::list< Attribute * > & attributes = std::list< Attribute * >()  ) : Parent( tq, name, attributes ), baseStruct( 0 ) {}
	StructInstType( const Type::Qualifiers & tq, StructDecl * baseStruct, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );
	StructInstType( const StructInstType & other ) : Parent( other ), baseStruct( other.baseStruct ) {}

	StructDecl * get_baseStruct() const { return baseStruct; }
	void set_baseStruct( StructDecl * newValue ) { baseStruct = newValue; }

	/// Accesses generic parameters of base struct (NULL if none such)
	std::list<TypeDecl*> * get_baseParameters();
	const std::list<TypeDecl*> * get_baseParameters() const;

	virtual bool isComplete() const override;

	virtual AggregateDecl * getAggr() const override;

	virtual TypeSubstitution genericSubstitution() const override;

	/// Looks up the members of this struct named "name" and places them into "foundDecls".
	/// Clones declarations into "foundDecls", caller responsible for freeing
	void lookup( const std::string & name, std::list< Declaration* > & foundDecls ) const override;

	virtual StructInstType * clone() const override { return new StructInstType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }

	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
  private:
	virtual std::string typeString() const override;
};

class UnionInstType : public ReferenceToType {
	typedef ReferenceToType Parent;
  public:
	// this decl is not "owned" by the union inst; it is merely a pointer to elsewhere in the tree,
	// where the union used in this type is actually defined
	UnionDecl * baseUnion;

	UnionInstType( const Type::Qualifiers & tq, const std::string & name, const std::list< Attribute * > & attributes = std::list< Attribute * >()  ) : Parent( tq, name, attributes ), baseUnion( 0 ) {}
	UnionInstType( const Type::Qualifiers & tq, UnionDecl * baseUnion, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );
	UnionInstType( const UnionInstType & other ) : Parent( other ), baseUnion( other.baseUnion ) {}

	UnionDecl * get_baseUnion() const { return baseUnion; }
	void set_baseUnion( UnionDecl * newValue ) { baseUnion = newValue; }

	/// Accesses generic parameters of base union (NULL if none such)
	std::list<TypeDecl*> * get_baseParameters();
	const std::list<TypeDecl*> * get_baseParameters() const;

	virtual bool isComplete() const override;

	virtual AggregateDecl * getAggr() const override;

	virtual TypeSubstitution genericSubstitution() const override;

	/// looks up the members of this union named "name" and places them into "foundDecls"
	/// Clones declarations into "foundDecls", caller responsible for freeing
	void lookup( const std::string & name, std::list< Declaration* > & foundDecls ) const override;

	virtual UnionInstType * clone() const override { return new UnionInstType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }

	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
  private:
	virtual std::string typeString() const override;
};

class EnumInstType : public ReferenceToType {
	typedef ReferenceToType Parent;
  public:
	// this decl is not "owned" by the enum inst; it is merely a pointer to elsewhere in the tree,
	// where the enum used in this type is actually defined
	EnumDecl * baseEnum = nullptr;

	EnumInstType( const Type::Qualifiers & tq, const std::string & name, const std::list< Attribute * > & attributes = std::list< Attribute * >()  ) : Parent( tq, name, attributes ) {}
	EnumInstType( const Type::Qualifiers & tq, EnumDecl * baseEnum, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );
	EnumInstType( const EnumInstType & other ) : Parent( other ), baseEnum( other.baseEnum ) {}

	EnumDecl * get_baseEnum() const { return baseEnum; }
	void set_baseEnum( EnumDecl * newValue ) { baseEnum = newValue; }

	virtual bool isComplete() const override;

	virtual AggregateDecl * getAggr() const override;

	virtual EnumInstType * clone() const override { return new EnumInstType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }

	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
  private:
	virtual std::string typeString() const override;
};

class TraitInstType : public ReferenceToType {
	typedef ReferenceToType Parent;
  public:
	// this decl is not "owned" by the trait inst; it is merely a pointer to elsewhere in the tree,
	// where the trait used in this type is actually defined
	TraitDecl * baseTrait = nullptr;

	TraitInstType( const Type::Qualifiers & tq, const std::string & name, const std::list< Attribute * > & attributes = std::list< Attribute * >() ) : Parent( tq, name, attributes ) {}
	TraitInstType( const Type::Qualifiers & tq, TraitDecl * baseTrait, const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	TraitInstType( const TraitInstType & other );
	~TraitInstType();

	virtual bool isComplete() const override;

	virtual TraitInstType * clone() const override { return new TraitInstType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
  private:
	virtual std::string typeString() const override;
};

class TypeInstType : public ReferenceToType {
	typedef ReferenceToType Parent;
  public:
	// this decl is not "owned" by the type inst; it is merely a pointer to elsewhere in the tree,
	// where the type used here is actually defined
	TypeDecl * baseType;
	bool isFtype;

	TypeInstType( const Type::Qualifiers & tq, const std::string & name, TypeDecl * baseType, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );
	TypeInstType( const Type::Qualifiers & tq, const std::string & name, bool isFtype, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );
	TypeInstType( const TypeInstType & other );
	~TypeInstType();

	TypeDecl * get_baseType() const { return baseType; }
	void set_baseType( TypeDecl * newValue );
	bool get_isFtype() const { return isFtype; }
	void set_isFtype( bool newValue ) { isFtype = newValue; }

	virtual bool isComplete() const override;

	virtual TypeInstType * clone() const override { return new TypeInstType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
  private:
	virtual std::string typeString() const override;
};

class TupleType : public Type {
  public:
	std::list<Type *> types;
	std::list<Declaration *> members;

	TupleType( const Type::Qualifiers & tq, const std::list< Type * > & types, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );
	TupleType( const TupleType& );
	virtual ~TupleType();

	typedef std::list<Type*> value_type;
	typedef value_type::iterator iterator;

	std::list<Type *> & get_types() { return types; }
	virtual unsigned size() const override { return types.size(); };

	// For now, this is entirely synthetic -- tuple types always have unnamed members.
	// Eventually, we may allow named tuples, in which case members should subsume types
	std::list<Declaration *> & get_members() { return members; }

	iterator begin() { return types.begin(); }
	iterator end() { return types.end(); }

	virtual Type * getComponent( unsigned i ) override {
		assertf( i < size(), "TupleType::getComponent: index %d must be less than size %d", i, size() );
		return *(begin()+i);
	}

	// virtual bool isComplete() const override { return true; } // xxx - not sure if this is right, might need to recursively check complete-ness

	virtual TupleType * clone() const override { return new TupleType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class TypeofType : public Type {
  public:
	Expression * expr;		///< expression to take the type of
	bool is_basetypeof;		///< true iff is basetypeof type

	TypeofType( const Type::Qualifiers & tq, Expression * expr, const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	TypeofType( const Type::Qualifiers & tq, Expression * expr, bool is_basetypeof,
		const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	TypeofType( const TypeofType& );
	virtual ~TypeofType();

	Expression * get_expr() const { return expr; }
	void set_expr( Expression * newValue ) { expr = newValue; }

	virtual bool isComplete() const override { assert( false ); return false; }

	virtual TypeofType * clone() const override { return new TypeofType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class VTableType : public Type {
public:
	Type * base;

	VTableType( const Type::Qualifiers & tq, Type * base,
		const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	VTableType( const VTableType & );
	virtual ~VTableType();

	Type * get_base() { return base; }
	void set_base( Type * newValue ) { base = newValue; }

	virtual VTableType * clone() const override { return new VTableType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class AttrType : public Type {
  public:
	std::string name;
	Expression * expr;
	Type * type;
	bool isType;

	AttrType( const Type::Qualifiers & tq, const std::string & name, Expression * expr, const std::list< Attribute * > & attributes = std::list< Attribute * >() );
	AttrType( const Type::Qualifiers & tq, const std::string & name, Type * type, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );
	AttrType( const AttrType& );
	virtual ~AttrType();

	const std::string & get_name() const { return name; }
	void set_name( const std::string & newValue ) { name = newValue; }
	Expression * get_expr() const { return expr; }
	void set_expr( Expression * newValue ) { expr = newValue; }
	Type * get_type() const { return type; }
	void set_type( Type * newValue ) { type = newValue; }
	bool get_isType() const { return isType; }
	void set_isType( bool newValue ) { isType = newValue; }

	virtual bool isComplete() const override { assert( false ); } // xxx - not sure what to do here

	virtual AttrType * clone() const override { return new AttrType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// Represents the GCC built-in varargs type
class VarArgsType : public Type {
  public:
	VarArgsType();
	VarArgsType( Type::Qualifiers tq, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );

	virtual bool isComplete() const override{ return true; } // xxx - is this right?

	virtual VarArgsType * clone() const override { return new VarArgsType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// Represents a zero constant
class ZeroType : public Type {
  public:
	ZeroType();
	ZeroType( Type::Qualifiers tq, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );

	virtual ZeroType * clone() const override { return new ZeroType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

/// Represents a one constant
class OneType : public Type {
  public:
	OneType();
	OneType( Type::Qualifiers tq, const std::list< Attribute * > & attributes = std::list< Attribute * >()  );

	virtual OneType * clone() const override { return new OneType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class GlobalScopeType : public Type {
  public:
	GlobalScopeType();

	virtual GlobalScopeType * clone() const override { return new GlobalScopeType( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Type * acceptMutator( Mutator & m ) override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};


bool isUnboundType(const Type * type);
bool isUnboundType(const std::string & tname);

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
