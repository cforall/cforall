//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SymbolTable.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 29 11:00:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Wed May 29 11:00:00 2019
// Update Count     : 1
//

#pragma once

#include <memory>                  // for shared_ptr, enable_shared_from_this
#include <vector>

#include "Fwd.hpp"
#include "Node.hpp"                // for ptr, readonly
#include "Common/CodeLocation.h"
#include "Common/PersistentMap.h"

namespace ResolvExpr {
	class Cost;
}

namespace ast {

/// Builds and stores the symbol table, mapping identifiers to declarations.
class SymbolTable final : public std::enable_shared_from_this<ast::SymbolTable> {
public:
	/// special functions stored in dedicated tables, with different lookup keys
	enum SpecialFunctionKind {CTOR, DTOR, ASSIGN, NUMBER_OF_KINDS};
	static SpecialFunctionKind getSpecialFunctionKind(const std::string & name);

	/// Stored information about a declaration
	struct IdData {
		readonly<DeclWithType> id = nullptr;  ///< Identifier of declaration
		readonly<Expr> baseExpr = nullptr;    ///< Implied containing aggregate (from WithExpr)
		readonly<Decl> deleter = nullptr;     ///< Node deleting this declaration (if non-null)
		unsigned long scope = 0;              ///< Scope of identifier

		IdData() = default;
		IdData( const DeclWithType * i, const Expr * base, const Decl * del, unsigned long s )
		: id( i ), baseExpr( base ), deleter( del ), scope( s ) {}

		/// Modify an existing node with a new deleter
		IdData( const IdData & o, const Decl * del )
		: id( o.id ), baseExpr( o.baseExpr ), deleter( del ), scope( o.scope ) {}

		/// Constructs an expression referring to this identifier.
		/// Increments `cost` by cost of reference conversion
		Expr * combine( const CodeLocation & loc, ResolvExpr::Cost & cost ) const;
	};

private:
	/// wraps a reference to D with a scope
	template<typename D>
	struct scoped {
		readonly<D> decl;     ///< wrapped declaration
		unsigned long scope;  ///< scope of this declaration

		scoped(const D * d, unsigned long s) : decl(d), scope(s) {}
	};

	using MangleTable = PersistentMap< std::string, IdData >;
	using IdTable = PersistentMap< std::string, MangleTable::Ptr >;
	using TypeTable = PersistentMap< std::string, scoped<NamedTypeDecl> >;
	using StructTable = PersistentMap< std::string, scoped<StructDecl> >;
	using EnumTable = PersistentMap< std::string, scoped<EnumDecl> >;
	using UnionTable = PersistentMap< std::string, scoped<UnionDecl> >;
	using TraitTable = PersistentMap< std::string, scoped<TraitDecl> >;

	IdTable::Ptr idTable;          ///< identifier namespace
	TypeTable::Ptr typeTable;      ///< type namespace
	StructTable::Ptr structTable;  ///< struct namespace
	EnumTable::Ptr enumTable;      ///< enum namespace
	UnionTable::Ptr unionTable;    ///< union namespace
	TraitTable::Ptr traitTable;    ///< trait namespace
	IdTable::Ptr specialFunctionTable[NUMBER_OF_KINDS];

	// using SpecialFuncTable = PersistentMap< std::string, IdTable::Ptr >; // fname (ctor/dtor/assign) - otypekey
	// SpecialFuncTable::Ptr specialFuncTable;

	using Ptr = std::shared_ptr<const SymbolTable>;

	Ptr prevScope;                 ///< Indexer for parent scope
	unsigned long scope;           ///< Scope index of this indexer
	unsigned long repScope;        ///< Scope index of currently represented scope

public:
	SymbolTable();
	~SymbolTable();

	// when using an indexer manually (e.g., within a mutator traversal), it is necessary to
	// tell the indexer explicitly when scopes begin and end
	void enterScope();
	void leaveScope();

	/// Gets all declarations with the given ID
	std::vector<IdData> lookupId( const std::string &id ) const;
	/// Gets special functions associated with a type; if no key is given, returns everything
	std::vector<IdData> specialLookupId( SpecialFunctionKind kind, const std::string & otypeKey = "" ) const;
	/// Gets the top-most type declaration with the given ID
	const NamedTypeDecl * lookupType( const std::string &id ) const;
	/// Gets the top-most struct declaration with the given ID
	const StructDecl * lookupStruct( const std::string &id ) const;
	/// Gets the top-most enum declaration with the given ID
	const EnumDecl * lookupEnum( const std::string &id ) const;
	/// Gets the top-most union declaration with the given ID
	const UnionDecl * lookupUnion( const std::string &id ) const;
	/// Gets the top-most trait declaration with the given ID
	const TraitDecl * lookupTrait( const std::string &id ) const;

	/// Gets the type declaration with the given ID at global scope
	const NamedTypeDecl * globalLookupType( const std::string &id ) const;
	/// Gets the struct declaration with the given ID at global scope
	const StructDecl * globalLookupStruct( const std::string &id ) const;
	/// Gets the union declaration with the given ID at global scope
	const UnionDecl * globalLookupUnion( const std::string &id ) const;
	/// Gets the enum declaration with the given ID at global scope
	const EnumDecl * globalLookupEnum( const std::string &id ) const;

	/// Adds an identifier declaration to the symbol table
	void addId( const DeclWithType * decl, const Expr * baseExpr = nullptr );
	/// Adds a deleted identifier declaration to the symbol table
	void addDeletedId( const DeclWithType * decl, const Decl * deleter );

	/// Adds a type to the symbol table
	void addType( const NamedTypeDecl * decl );
	/// Adds a struct declaration to the symbol table by name
	void addStruct( const std::string & id );
	/// Adds a struct declaration to the symbol table
	void addStruct( const StructDecl * decl );
	/// Adds an enum declaration to the symbol table
	void addEnum( const EnumDecl * decl );
	/// Adds a union declaration to the symbol table by name
	void addUnion( const std::string & id );
	/// Adds a union declaration to the symbol table
	void addUnion( const UnionDecl * decl );
	/// Adds a trait declaration to the symbol table
	void addTrait( const TraitDecl * decl );

	/// adds all of the IDs from WithStmt exprs
	void addWith( const std::vector< ptr<Expr> > & withExprs, const Decl * withStmt );

	/// convenience function for adding a list of Ids to the indexer
	void addIds( const std::vector< ptr<DeclWithType> > & decls );

	/// convenience function for adding a list of forall parameters to the indexer
	void addTypes( const std::vector< ptr<TypeDecl> > & tds );

	/// convenience function for adding all of the declarations in a function type to the indexer
	void addFunction( const FunctionDecl * );

private:
	/// Ensures that a proper backtracking scope exists before a mutation
	void lazyInitScope();

	/// Gets the symbol table at a given scope
	const SymbolTable * atScope( unsigned long i ) const;

	/// Removes matching autogenerated constructors and destructors so that they will not be
	/// selected. If returns false, passed decl should not be added.
	bool removeSpecialOverrides( IdData & decl, MangleTable::Ptr & mangleTable );

	/// Options for handling identifier conflicts
	struct OnConflict {
		enum {
			Error,  ///< Throw a semantic error
			Delete  ///< Delete the earlier version with the delete statement
		} mode;
		const Decl * deleter;  ///< Statement that deletes this expression

	private:
		OnConflict() : mode(Error), deleter(nullptr) {}
		OnConflict( const Decl * d ) : mode(Delete), deleter(d) {}
	public:
		OnConflict( const OnConflict& ) = default;

		static OnConflict error() { return {}; }
		static OnConflict deleteWith( const Decl * d ) { return { d }; }
	};

	/// true if the existing identifier conflicts with the added identifier
	bool addedIdConflicts(
		const IdData & existing, const DeclWithType * added, OnConflict handleConflicts,
		const Decl * deleter );

	/// common code for addId, addDeletedId, etc.
	void addIdCommon(
		const DeclWithType * decl, OnConflict handleConflicts,
		const Expr * baseExpr = nullptr, const Decl * deleter = nullptr );

	/// common code for addId when special decls are placed into separate tables
	void addIdToTable(
		const DeclWithType * decl, const std::string & lookupKey,
		IdTable::Ptr & idTable, OnConflict handleConflicts,
		const Expr * baseExpr = nullptr, const Decl * deleter = nullptr);

	/// adds all of the members of the Aggregate (addWith helper)
	void addMembers( const AggregateDecl * aggr, const Expr * expr, OnConflict handleConflicts );

	/// returns true if there exists a declaration with C linkage and the given name with the same mangled name
	bool hasCompatibleCDecl( const std::string &id, const std::string &mangleName ) const;
	/// returns true if there exists a declaration with C linkage and the given name with a different mangled name
	bool hasIncompatibleCDecl( const std::string &id, const std::string &mangleName ) const;
};

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
