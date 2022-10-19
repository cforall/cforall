//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Declaration.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Henry Xue
// Last Modified On : Tue Jul 20 04:10:50 2021
// Update Count     : 160
//

#pragma once

#include <cassert>               // for assertf
#include <iosfwd>                // for ostream
#include <list>                  // for list
#include <unordered_map>         // for unordered_map
#include <string>                // for string, operator+, allocator, to_string

#include "BaseSyntaxNode.h"      // for BaseSyntaxNode
#include "Mutator.h"             // for Mutator
#include "LinkageSpec.h"         // for Spec, Cforall
#include "SynTree.h"             // for UniqueId
#include "SynTree/Type.h"        // for Type, Type::StorageClasses, Type::Fu...
#include "Visitor.h"             // for Visitor

class AsmStmt;
class Attribute;
class CompoundStmt;
class ConstantExpr;
class Expression;
class Initializer;
class TypeDecl;

class Declaration : public BaseSyntaxNode {
  public:
	std::string name;
	LinkageSpec::Spec linkage;
	bool extension = false;

	Declaration( const std::string & name, Type::StorageClasses scs, LinkageSpec::Spec linkage );
	Declaration( const Declaration & other );
	virtual ~Declaration();

	const std::string & get_name() const { return name; }
	void set_name( std::string newValue ) { name = newValue; }

	Type::StorageClasses get_storageClasses() const { return storageClasses; }

	LinkageSpec::Spec get_linkage() const { return linkage; }
	void set_linkage( LinkageSpec::Spec newValue ) { linkage = newValue; }

	UniqueId get_uniqueId() const { return uniqueId; }

	bool get_extension() const { return extension; }
	Declaration * set_extension( bool exten ) { extension = exten; return this; }

	void fixUniqueId( void );
	virtual Declaration * clone() const override = 0;
	virtual void accept( Visitor & v ) override = 0;
	virtual void accept( Visitor & v ) const override = 0;
	virtual Declaration * acceptMutator( Mutator & m ) override = 0;
	virtual void print( std::ostream & os, Indenter indent = {} ) const override = 0;
	virtual void printShort( std::ostream & os, Indenter indent = {} ) const = 0;

	UniqueId uniqueId;
	Type::StorageClasses storageClasses;
  private:
};

class DeclarationWithType : public Declaration {
  public:
	// this represents the type with all types and typedefs expanded it is generated by SymTab::Validate::Pass2
	std::string mangleName;
	// need to remember the scope level at which the variable was declared, so that shadowed identifiers can be accessed
	int scopeLevel = 0;

	Expression * asmName;
	std::list< Attribute * > attributes;
	bool isDeleted = false;

	DeclarationWithType( const std::string & name, Type::StorageClasses scs, LinkageSpec::Spec linkage, const std::list< Attribute * > & attributes, Type::FuncSpecifiers fs );
	DeclarationWithType( const DeclarationWithType & other );
	virtual ~DeclarationWithType();

	std::string get_mangleName() const { return mangleName; }
	DeclarationWithType * set_mangleName( std::string newValue ) { mangleName = newValue; return this; }

	std::string get_scopedMangleName() const { return mangleName + "_" + std::to_string(scopeLevel); }

	int get_scopeLevel() const { return scopeLevel; }
	DeclarationWithType * set_scopeLevel( int newValue ) { scopeLevel = newValue; return this; }

	Expression * get_asmName() const { return asmName; }
	DeclarationWithType * set_asmName( Expression * newValue ) { asmName = newValue; return this; }

	std::list< Attribute * >& get_attributes() { return attributes; }
	const std::list< Attribute * >& get_attributes() const { return attributes; }

	Type::FuncSpecifiers get_funcSpec() const { return fs; }
	//void set_functionSpecifiers( Type::FuncSpecifiers newValue ) { fs = newValue; }

	virtual DeclarationWithType * clone() const override = 0;
	virtual DeclarationWithType * acceptMutator( Mutator & m )  override = 0;

	virtual Type * get_type() const = 0;
	virtual void set_type(Type *) = 0;

  private:
	Type::FuncSpecifiers fs;
};

class ObjectDecl : public DeclarationWithType {
	typedef DeclarationWithType Parent;
  public:
	Type * type;
	Initializer * init;
	Expression * bitfieldWidth;
	bool enumInLine = false;

	ObjectDecl( const std::string & name, Type::StorageClasses scs, LinkageSpec::Spec linkage, Expression * bitfieldWidth, Type * type, Initializer * init,
				const std::list< Attribute * > attributes = std::list< Attribute * >(), Type::FuncSpecifiers fs = Type::FuncSpecifiers() );
	ObjectDecl( const ObjectDecl & other );
	virtual ~ObjectDecl();

	virtual Type * get_type() const override { return type; }
	virtual void set_type(Type * newType) override { type = newType; }

	Initializer * get_init() const { return init; }
	void set_init( Initializer * newValue ) { init = newValue; }

	Expression * get_bitfieldWidth() const { return bitfieldWidth; }
	void set_bitfieldWidth( Expression * newValue ) { bitfieldWidth = newValue; }

	static ObjectDecl * newObject( const std::string & name, Type * type, Initializer * init );

	virtual ObjectDecl * clone() const override { return new ObjectDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual DeclarationWithType * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
	virtual void printShort( std::ostream & os, Indenter indent = {} ) const override;

	void checkAssignedValue() const;
};

class FunctionDecl : public DeclarationWithType {
	typedef DeclarationWithType Parent;
  public:
	FunctionType * type;
	CompoundStmt * statements;
	std::list< Expression * > withExprs;

	FunctionDecl( const std::string & name, Type::StorageClasses scs, LinkageSpec::Spec linkage, FunctionType * type, CompoundStmt * statements,
				  const std::list< Attribute * > attributes = std::list< Attribute * >(), Type::FuncSpecifiers fs = Type::FuncSpecifiers() );
	FunctionDecl( const FunctionDecl & other );
	virtual ~FunctionDecl();

	virtual Type * get_type() const override { return type; }
	virtual void set_type(Type * t) override { type = strict_dynamic_cast< FunctionType* >( t ); }

	FunctionType * get_functionType() const { return type; }
	void set_functionType( FunctionType * newValue ) { type = newValue; }
	CompoundStmt * get_statements() const { return statements; }
	void set_statements( CompoundStmt * newValue ) { statements = newValue; }
	bool has_body() const { return NULL != statements; }

	static FunctionDecl * newFunction( const std::string & name, FunctionType * type, CompoundStmt * statements );

	virtual FunctionDecl * clone() const override { return new FunctionDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual DeclarationWithType * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
	virtual void printShort( std::ostream & os, Indenter indent = {} ) const override;
};

class NamedTypeDecl : public Declaration {
	typedef Declaration Parent;
  public:
	Type * base;
	std::list< DeclarationWithType * > assertions;

	NamedTypeDecl( const std::string & name, Type::StorageClasses scs, Type * type );
	NamedTypeDecl( const NamedTypeDecl & other );
	virtual ~NamedTypeDecl();

	Type * get_base() const { return base; }
	void set_base( Type * newValue ) { base = newValue; }
	std::list< DeclarationWithType * >& get_assertions() { return assertions; }

	virtual const char * typeString() const = 0;

	virtual NamedTypeDecl * clone() const override = 0;
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
	virtual void printShort( std::ostream & os, Indenter indent = {} ) const override;
};

class TypeDecl : public NamedTypeDecl {
	typedef NamedTypeDecl Parent;
  public:
	enum Kind { Dtype, DStype, Otype, Ftype, Ttype, Dimension, NUMBER_OF_KINDS };

	Kind kind;
	bool sized;
	Type * init;

	/// Data extracted from a type decl
	struct Data {
		Kind kind;
		bool isComplete;

		Data() : kind( NUMBER_OF_KINDS ), isComplete( false ) {}
		Data( const TypeDecl * typeDecl ) : Data( typeDecl->get_kind(), typeDecl->isComplete() ) {}
		Data( Kind kind, bool isComplete ) : kind( kind ), isComplete( isComplete ) {}
		Data( const Data & d1, const Data & d2 )
			: kind( d1.kind ), isComplete( d1.isComplete || d2.isComplete ) {}

		bool operator==( const Data & other ) const { return kind == other.kind && isComplete == other.isComplete; }
		bool operator!=( const Data & other ) const { return !(*this == other);}
	};

	TypeDecl( const std::string & name, Type::StorageClasses scs, Type * type, Kind kind, bool sized, Type * init = nullptr );
	TypeDecl( const TypeDecl & other );
	virtual ~TypeDecl();

	Kind get_kind() const { return kind; }

	Type * get_init() const { return init; }
	TypeDecl * set_init( Type * newValue ) { init = newValue; return this; }

	bool isComplete() const { return sized; }
	bool get_sized() const { return sized; }
	TypeDecl * set_sized( bool newValue ) { sized = newValue; return this; }

	virtual const char * typeString() const override;
	virtual const char * genTypeString() const;

	virtual TypeDecl * clone() const override { return new TypeDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Declaration * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
};

class TypedefDecl : public NamedTypeDecl {
	typedef NamedTypeDecl Parent;
  public:
	TypedefDecl( const std::string & name, CodeLocation location, Type::StorageClasses scs, Type * type, LinkageSpec::Spec spec = LinkageSpec::Cforall )
		: Parent( name, scs, type ) { set_linkage( spec ); this->location = location; }

	TypedefDecl( const TypedefDecl & other ) : Parent( other ) {}

	virtual const char * typeString() const override;

	virtual TypedefDecl * clone() const override { return new TypedefDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Declaration * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
  private:
};

class AggregateDecl : public Declaration {
	typedef Declaration Parent;
  public:
	enum Aggregate { Struct, Union, Enum, Exception, Trait, Generator, Coroutine, Monitor, Thread, NoAggregate };
	static const char * aggrString( Aggregate aggr );

	std::list<Declaration*> members;
	std::list<TypeDecl*> parameters;
	bool body;
	std::list< Attribute * > attributes;
	AggregateDecl * parent = nullptr;

	AggregateDecl( const std::string & name, const std::list< Attribute * > & attributes = std::list< class Attribute * >(), LinkageSpec::Spec linkage = LinkageSpec::Cforall );
	AggregateDecl( const AggregateDecl & other );
	virtual ~AggregateDecl();

	std::list<Declaration*>& get_members() { return members; }
	std::list<TypeDecl*>& get_parameters() { return parameters; }

	std::list< Attribute * >& get_attributes() { return attributes; }
	const std::list< Attribute * >& get_attributes() const { return attributes; }

	bool has_body() const { return body; }
	AggregateDecl * set_body( bool body ) { AggregateDecl::body = body; return this; }

	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
	virtual void printShort( std::ostream & os, Indenter indent = {} ) const override;
  protected:
	virtual const char * typeString() const = 0;
};

class StructDecl : public AggregateDecl {
	typedef AggregateDecl Parent;
  public:
	StructDecl( const std::string & name, Aggregate kind = Struct, const std::list< Attribute * > & attributes = std::list< class Attribute * >(), LinkageSpec::Spec linkage = LinkageSpec::Cforall ) : Parent( name, attributes, linkage ), kind( kind ) {}
	StructDecl( const StructDecl & other ) : Parent( other ), kind( other.kind ) {}

	bool is_coroutine() { return kind == Coroutine; }
	bool is_exception() { return kind == Exception; }
	bool is_generator() { return kind == Generator; }
	bool is_monitor  () { return kind == Monitor  ; }
	bool is_thread   () { return kind == Thread   ; }

	// Make a type instance of this declaration.
	StructInstType * makeInst( std::list< Expression * > const & parameters );
	StructInstType * makeInst( std::list< Expression * > && parameters );

	virtual StructDecl * clone() const override { return new StructDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Declaration * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	Aggregate kind;
  private:
	virtual const char * typeString() const override;
};

class UnionDecl : public AggregateDecl {
	typedef AggregateDecl Parent;
  public:
	UnionDecl( const std::string & name, const std::list< Attribute * > & attributes = std::list< class Attribute * >(), LinkageSpec::Spec linkage = LinkageSpec::Cforall ) : Parent( name, attributes, linkage ) {}
	UnionDecl( const UnionDecl & other ) : Parent( other ) {}

	virtual UnionDecl * clone() const override { return new UnionDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Declaration * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
  private:
	virtual const char * typeString() const override;
};

class EnumDecl : public AggregateDecl {
	typedef AggregateDecl Parent;
  public:
  	bool isTyped;
	Type * base;

	EnumDecl( const std::string & name,
	 const std::list< Attribute * > & attributes = std::list< class Attribute * >(),
	  bool isTyped = false, LinkageSpec::Spec linkage = LinkageSpec::Cforall,
	  Type * baseType = nullptr ) 
	  : Parent( name, attributes, linkage ),isTyped(isTyped), base( baseType ) {}
	EnumDecl( const EnumDecl & other ) 
	  : Parent( other ), isTyped( other.isTyped), base( other.base ) {}
	bool valueOf( Declaration * enumerator, long long int & value );
	virtual EnumDecl * clone() const override { return new EnumDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Declaration * acceptMutator( Mutator & m )  override { return m.mutate( this ); }

	std::unordered_map< std::string, long long int > enumValues; // This attribute is unused
	virtual void print( std::ostream & os, Indenter indent = {} ) const override final;
  private:
	// std::unordered_map< std::string, long long int > enumValues;
	virtual const char * typeString() const override;
};

class TraitDecl : public AggregateDecl {
	typedef AggregateDecl Parent;
  public:
	TraitDecl( const std::string & name, const std::list< Attribute * > & attributes, LinkageSpec::Spec linkage ) : Parent( name, attributes, linkage ) {
		assertf( attributes.empty(), "attribute unsupported for traits" );
	}
	TraitDecl( const TraitDecl & other ) : Parent( other ) {}

	virtual TraitDecl * clone() const override { return new TraitDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Declaration * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
  private:
	virtual const char * typeString() const override;
};

class WithStmt : public Declaration {
public:
	std::list< Expression * > exprs;
	Statement * stmt;

	WithStmt( const std::list< Expression * > & exprs, Statement * stmt );
	WithStmt( const WithStmt & other );
	virtual ~WithStmt();

	virtual WithStmt * clone() const override { return new WithStmt( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual Declaration * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
	virtual void printShort( std::ostream & os, Indenter indent = {} ) const override { print(os, indent); }
};

class AsmDecl : public Declaration {
  public:
	AsmStmt * stmt;

	AsmDecl( AsmStmt * stmt );
	AsmDecl( const AsmDecl & other );
	virtual ~AsmDecl();

	AsmStmt * get_stmt() { return stmt; }
	void set_stmt( AsmStmt * newValue ) { stmt = newValue; }

	virtual AsmDecl * clone() const override { return new AsmDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual AsmDecl * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
	virtual void printShort( std::ostream & os, Indenter indent = {} ) const override;
};

class DirectiveDecl : public Declaration {
  public:
	DirectiveStmt * stmt;

	DirectiveDecl( DirectiveStmt * stmt );
	DirectiveDecl( const DirectiveDecl & other );
	virtual ~DirectiveDecl();

	DirectiveStmt * get_stmt() { return stmt; }
	void set_stmt( DirectiveStmt * newValue ) { stmt = newValue; }

	virtual DirectiveDecl * clone() const override { return new DirectiveDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual DirectiveDecl * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
	virtual void printShort( std::ostream & os, Indenter indent = {} ) const override;
};

class StaticAssertDecl : public Declaration {
public:
	Expression * condition;
	ConstantExpr * message;   // string literal

	StaticAssertDecl( Expression * condition, ConstantExpr * message );
	StaticAssertDecl( const StaticAssertDecl & other );
	virtual ~StaticAssertDecl();

	virtual StaticAssertDecl * clone() const override { return new StaticAssertDecl( *this ); }
	virtual void accept( Visitor & v ) override { v.visit( this ); }
	virtual void accept( Visitor & v ) const override { v.visit( this ); }
	virtual StaticAssertDecl * acceptMutator( Mutator & m )  override { return m.mutate( this ); }
	virtual void print( std::ostream & os, Indenter indent = {} ) const override;
	virtual void printShort( std::ostream & os, Indenter indent = {} ) const override;
};

std::ostream & operator<<( std::ostream & os, const TypeDecl::Data & data );

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
