//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Indexer.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 21:38:55 2015
// Last Modified By : Aaron B. Moss
// Last Modified On : Fri Mar  8 13:55:00 2019
// Update Count     : 9
//

#pragma once

#include <functional>              // for function
#include <list>                    // for list
#include <memory>                  // for shared_ptr, enable_shared_from_this
#include <string>                  // for string

#include "Common/PersistentMap.h"  // for PersistentMap
#include "SynTree/SynTree.h"       // for AST nodes

namespace ResolvExpr {
	class Cost;
}

namespace SymTab {
	class Indexer : public std::enable_shared_from_this<SymTab::Indexer> {
	public:
		explicit Indexer( bool trackIdentifiers = true );
		virtual ~Indexer();

		// when using an indexer manually (e.g., within a mutator traversal), it is necessary to
		// tell the indexer explicitly when scopes begin and end
		void enterScope();
		void leaveScope();

		struct IdData {
			const DeclarationWithType * id = nullptr;
			const Expression * baseExpr = nullptr; // WithExpr

			/// non-null if this declaration is deleted
			const Declaration * deleteStmt = nullptr;
			/// scope of identifier
			unsigned long scope = 0;

			// NOTE: shouldn't need either of these constructors, but gcc-4 does not properly support initializer lists with default members.
			IdData() = default;
			IdData(
				const DeclarationWithType * id, const Expression * baseExpr, const Declaration * deleteStmt,
				unsigned long scope )
				: id( id ), baseExpr( baseExpr ), deleteStmt( deleteStmt ), scope( scope ) {}
			IdData( const IdData& o, const Declaration * deleteStmt )
				: id( o.id ), baseExpr( o.baseExpr ), deleteStmt( deleteStmt ), scope( o.scope ) {}

			Expression * combine( ResolvExpr::Cost & cost ) const;
		};

		/// Gets all declarations with the given ID
		void lookupId( const std::string & id, std::list< IdData > &out ) const;
		/// Gets the top-most type declaration with the given ID
		const NamedTypeDecl * lookupType( const std::string & id ) const;
		/// Gets the top-most struct declaration with the given ID
		const StructDecl * lookupStruct( const std::string & id ) const;
		/// Gets the top-most enum declaration with the given ID
		const EnumDecl * lookupEnum( const std::string & id ) const;
		/// Gets the top-most union declaration with the given ID
		const UnionDecl * lookupUnion( const std::string & id ) const;
		/// Gets the top-most trait declaration with the given ID
		const TraitDecl * lookupTrait( const std::string & id ) const;

		/// Gets the type declaration with the given ID at global scope
		const NamedTypeDecl * globalLookupType( const std::string & id ) const;
		/// Gets the struct declaration with the given ID at global scope
		const StructDecl * globalLookupStruct( const std::string & id ) const;
		/// Gets the union declaration with the given ID at global scope
		const UnionDecl * globalLookupUnion( const std::string & id ) const;
		/// Gets the enum declaration with the given ID at global scope
		const EnumDecl * globalLookupEnum( const std::string & id ) const;

		void addId( const DeclarationWithType * decl, const Expression * baseExpr = nullptr );
		void addDeletedId( const DeclarationWithType * decl, const Declaration * deleteStmt );

		void addType( const NamedTypeDecl * decl );
		void addStruct( const std::string & id );
		void addStruct( const StructDecl * decl );
		void addEnum( const EnumDecl * decl );
		void addUnion( const std::string & id );
		void addUnion( const UnionDecl * decl );
		void addTrait( const TraitDecl * decl );

		/// adds all of the IDs from WithStmt exprs
		void addWith( const std::list< Expression * > & withExprs, const Declaration * withStmt );

		/// convenience function for adding a list of Ids to the indexer
		void addIds( const std::list< DeclarationWithType * > & decls );

		/// convenience function for adding a list of forall parameters to the indexer
		void addTypes( const std::list< TypeDecl * > & tds );

		/// convenience function for adding all of the declarations in a function type to the indexer
		void addFunctionType( const FunctionType * ftype );

	  private:
	  	/// Wraps a Decl * with a scope
	  	template<typename Decl>
		struct Scoped {
			const Decl * decl;           ///< declaration
			unsigned long scope;  ///< scope of this declaration

			Scoped(const Decl * d, unsigned long s) : decl(d), scope(s) {}
		};

		using Ptr = std::shared_ptr<const Indexer>;

		using MangleTable = PersistentMap< std::string, IdData >;
		using IdTable = PersistentMap< std::string, MangleTable::Ptr >;
		using TypeTable = PersistentMap< std::string, Scoped<NamedTypeDecl> >;
		using StructTable = PersistentMap< std::string, Scoped<StructDecl> >;
		using EnumTable = PersistentMap< std::string, Scoped<EnumDecl> >;
		using UnionTable = PersistentMap< std::string, Scoped<UnionDecl> >;
		using TraitTable = PersistentMap< std::string, Scoped<TraitDecl> >;

		IdTable::Ptr idTable;          ///< identifier namespace
		TypeTable::Ptr typeTable;      ///< type namespace
		StructTable::Ptr structTable;  ///< struct namespace
		EnumTable::Ptr enumTable;      ///< enum namespace
		UnionTable::Ptr unionTable;    ///< union namespace
		TraitTable::Ptr traitTable;    ///< trait namespace

		Ptr prevScope;                 ///< reference to indexer for parent scope
		unsigned long scope;           ///< Scope index of this indexer
		unsigned long repScope;        ///< Scope index of currently represented scope

		/// Ensures that a proper backtracking scope exists before a mutation
		void lazyInitScope();

		/// Gets the indexer at the given scope
		const Indexer * atScope( unsigned long scope ) const;

		/// Removes matching autogenerated constructors and destructors so that they will not be
		/// selected. If returns false, passed decl should not be added.
		bool removeSpecialOverrides( IdData & decl, MangleTable::Ptr & mangleTable );

		/// Options for handling identifier conflicts
		struct OnConflict {
			enum {
				Error,  ///< Throw a semantic error
				Delete  ///< Delete the earlier version with the delete statement
			} mode;
			const Declaration * deleteStmt;  ///< Statement that deletes this expression

		private:
			OnConflict() : mode(Error), deleteStmt(nullptr) {}
			OnConflict( const Declaration * d ) : mode(Delete), deleteStmt(d) {}
		public:
			OnConflict( const OnConflict& ) = default;

			static OnConflict error() { return {}; }
			static OnConflict deleteWith( const Declaration * d ) { return { d }; }
		};

		/// true if the existing identifier conflicts with the added identifier
		bool addedIdConflicts(
			const IdData & existing, const DeclarationWithType * added, OnConflict handleConflicts,
			const Declaration * deleteStmt );

		/// common code for addId, addDeletedId, etc.
		void addId(const DeclarationWithType * decl, OnConflict handleConflicts,
			const Expression * baseExpr = nullptr, const Declaration * deleteStmt = nullptr );

		/// adds all of the members of the Aggregate (addWith helper)
		void addMembers( const AggregateDecl * aggr, const Expression * expr, OnConflict handleConflicts );

		/// returns true if there exists a declaration with C linkage and the given name with the same mangled name
		bool hasCompatibleCDecl( const std::string & id, const std::string & mangleName ) const;
		/// returns true if there exists a declaration with C linkage and the given name with a different mangled name
		bool hasIncompatibleCDecl( const std::string & id, const std::string & mangleName ) const;

	    bool trackIdentifiers;
	};
} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
