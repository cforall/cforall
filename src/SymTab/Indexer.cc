//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Indexer.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 21:37:33 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:43:19 2019
// Update Count     : 22
//

#include "Indexer.h"

#include <cassert>                 // for assert, strict_dynamic_cast
#include <string>                  // for string, operator<<, operator!=
#include <memory>                  // for shared_ptr, make_shared
#include <unordered_map>           // for operator!=, unordered_map<>::const...
#include <unordered_set>           // for unordered_set
#include <utility>                 // for pair, make_pair, move
#include <vector>                  // for vector

#include "CodeGen/OperatorTable.h" // for isCtorDtor, isCtorDtorAssign
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for cloneAll
#include "Common/Stats/Counter.h"  // for counters
#include "GenPoly/GenPoly.h"       // for getFunctionType
#include "InitTweak/InitTweak.h"   // for isConstructor, isCopyFunction, isC...
#include "Mangler.h"               // for Mangler
#include "ResolvExpr/AlternativeFinder.h"  // for referenceToRvalueConversion
#include "ResolvExpr/Unify.h"      // for typesCompatible
#include "SynTree/LinkageSpec.h"   // for isMangled, isOverridable, Spec
#include "SynTree/Constant.h"      // for Constant
#include "SynTree/Declaration.h"   // for DeclarationWithType, FunctionDecl
#include "SynTree/Expression.h"    // for Expression, ImplicitCopyCtorExpr
#include "SynTree/Initializer.h"   // for Initializer
#include "SynTree/Statement.h"     // for CompoundStmt, Statement, ForStmt (...
#include "SynTree/Type.h"          // for Type, StructInstType, UnionInstType

namespace SymTab {

	// Statistics block
	namespace {
		static inline auto stats() {
			using namespace Stats::Counters;
			static auto group   = build<CounterGroup>("Indexers");
			static struct {
				SimpleCounter * count;
				AverageCounter<double> * size;
				SimpleCounter * new_scopes;
				SimpleCounter * lazy_scopes;
				AverageCounter<double> * avg_scope_depth;
				MaxCounter<size_t> * max_scope_depth;
				SimpleCounter * add_calls;
				SimpleCounter * lookup_calls;
				SimpleCounter * map_lookups;
				SimpleCounter * map_mutations;
			} ret = {
				.count   = build<SimpleCounter>("Count", group),
				.size    = build<AverageCounter<double>>("Average Size", group),
				.new_scopes = build<SimpleCounter>("Scopes", group),
				.lazy_scopes = build<SimpleCounter>("Lazy Scopes", group),
				.avg_scope_depth = build<AverageCounter<double>>("Average Scope", group),
				.max_scope_depth = build<MaxCounter<size_t>>("Max Scope", group),
				.add_calls = build<SimpleCounter>("Add Calls", group),
				.lookup_calls = build<SimpleCounter>("Lookup Calls", group),
				.map_lookups = build<SimpleCounter>("Map Lookups", group),
				.map_mutations = build<SimpleCounter>("Map Mutations", group)
			};
			return ret;
		}
	}

	Indexer::Indexer( bool trackIdentifiers )
	: idTable(), typeTable(), structTable(), enumTable(), unionTable(), traitTable(),
	  prevScope(), scope( 0 ), repScope( 0 ), trackIdentifiers( trackIdentifiers ) { ++* stats().count; }

	Indexer::~Indexer() {
		stats().size->push( idTable ? idTable->size() : 0 );
	}

	void Indexer::lazyInitScope() {
		if ( repScope < scope ) {
			++* stats().lazy_scopes;
			// create rollback
			prevScope = std::make_shared<Indexer>( * this );
			// update repScope
			repScope = scope;
		}
	}

	void Indexer::enterScope() {
		++scope;

		++* stats().new_scopes;
		stats().avg_scope_depth->push( scope );
		stats().max_scope_depth->push( scope );
	}

	void Indexer::leaveScope() {
		if ( repScope == scope ) {
			Ptr prev = prevScope;           // make sure prevScope stays live
			* this = std::move(* prevScope);  // replace with previous scope
		}

		--scope;
	}

	void Indexer::lookupId( const std::string & id, std::list< IdData > &out ) const {
		assert( trackIdentifiers );

		++* stats().lookup_calls;
		if ( ! idTable ) return;

		++* stats().map_lookups;
		auto decls = idTable->find( id );
		if ( decls == idTable->end() ) return;

		for ( auto decl : *(decls->second) ) {
			out.push_back( decl.second );
		}
	}

	const NamedTypeDecl * Indexer::lookupType( const std::string & id ) const {
		++* stats().lookup_calls;
		if ( ! typeTable ) return nullptr;
		++* stats().map_lookups;
		auto it = typeTable->find( id );
		return it == typeTable->end() ? nullptr : it->second.decl;
	}

	const StructDecl * Indexer::lookupStruct( const std::string & id ) const {
		++* stats().lookup_calls;
		if ( ! structTable ) return nullptr;
		++* stats().map_lookups;
		auto it = structTable->find( id );
		return it == structTable->end() ? nullptr : it->second.decl;
	}

	const EnumDecl * Indexer::lookupEnum( const std::string & id ) const {
		++* stats().lookup_calls;
		if ( ! enumTable ) return nullptr;
		++* stats().map_lookups;
		auto it = enumTable->find( id );
		return it == enumTable->end() ? nullptr : it->second.decl;
	}

	const UnionDecl * Indexer::lookupUnion( const std::string & id ) const {
		++* stats().lookup_calls;
		if ( ! unionTable ) return nullptr;
		++* stats().map_lookups;
		auto it = unionTable->find( id );
		return it == unionTable->end() ? nullptr : it->second.decl;
	}

	const TraitDecl * Indexer::lookupTrait( const std::string & id ) const {
		++* stats().lookup_calls;
		if ( ! traitTable ) return nullptr;
		++* stats().map_lookups;
		auto it = traitTable->find( id );
		return it == traitTable->end() ? nullptr : it->second.decl;
	}

	const Indexer * Indexer::atScope( unsigned long target ) const {
		// by lazy construction, final indexer in list has repScope 0, cannot be > target
		// otherwise, will find first scope representing the target
		const Indexer * indexer = this;
		while ( indexer->repScope > target ) {
			indexer = indexer->prevScope.get();
		}
		return indexer;
	}

	const NamedTypeDecl * Indexer::globalLookupType( const std::string & id ) const {
		return atScope( 0 )->lookupType( id );
	}

	const StructDecl * Indexer::globalLookupStruct( const std::string & id ) const {
		return atScope( 0 )->lookupStruct( id );
	}

	const UnionDecl * Indexer::globalLookupUnion( const std::string & id ) const {
		return atScope( 0 )->lookupUnion( id );
	}

	const EnumDecl * Indexer::globalLookupEnum( const std::string & id ) const {
		return atScope( 0 )->lookupEnum( id );
	}

	bool isFunction( const DeclarationWithType * decl ) {
		return GenPoly::getFunctionType( decl->get_type() );
	}

	bool isObject( const DeclarationWithType * decl ) {
		return ! isFunction( decl );
	}

	bool isDefinition( const DeclarationWithType * decl ) {
		if ( const FunctionDecl * func = dynamic_cast< const FunctionDecl * >( decl ) ) {
			// a function is a definition if it has a body
			return func->statements;
		} else {
			// an object is a definition if it is not marked extern.
			// both objects must be marked extern
			return ! decl->get_storageClasses().is_extern;
		}
	}


	bool Indexer::addedIdConflicts(
			const Indexer::IdData & existing, const DeclarationWithType * added,
			Indexer::OnConflict handleConflicts, const Declaration * deleteStmt ) {
		// if we're giving the same name mangling to things of different types then there is
		// something wrong
		assert( (isObject( added ) && isObject( existing.id ) )
			|| ( isFunction( added ) && isFunction( existing.id ) ) );

		if ( LinkageSpec::isOverridable( existing.id->linkage ) ) {
			// new definition shadows the autogenerated one, even at the same scope
			return false;
		} else if ( LinkageSpec::isMangled( added->linkage )
				|| ResolvExpr::typesCompatible(
					added->get_type(), existing.id->get_type(), Indexer() ) ) {

			// it is a conflict if one declaration is deleted and the other is not
			if ( deleteStmt && ! existing.deleteStmt ) {
				if ( handleConflicts.mode == OnConflict::Error ) {
					SemanticError( added, "deletion of defined identifier " );
				}
				return true;
			} else if ( ! deleteStmt && existing.deleteStmt ) {
				if ( handleConflicts.mode == OnConflict::Error ) {
					SemanticError( added, "definition of deleted identifier " );
				}
				return true;
			}

			if ( isDefinition( added ) && isDefinition( existing.id ) ) {
				if ( handleConflicts.mode == OnConflict::Error ) {
					SemanticError( added,
						isFunction( added ) ?
							"duplicate function definition for " :
							"duplicate object definition for " );
				}
				return true;
			} // if
		} else {
			if ( handleConflicts.mode == OnConflict::Error ) {
				SemanticError( added, "duplicate definition for " );
			}
			return true;
		} // if

		return true;
	}

	bool Indexer::hasCompatibleCDecl( const std::string & id, const std::string &mangleName ) const {
		if ( ! idTable ) return false;

		++* stats().map_lookups;
		auto decls = idTable->find( id );
		if ( decls == idTable->end() ) return false;

		for ( auto decl : *(decls->second) ) {
			// skip other scopes (hidden by this decl)
			if ( decl.second.scope != scope ) continue;
			// check for C decl with compatible type (by mangleName)
			if ( ! LinkageSpec::isMangled( decl.second.id->linkage ) && decl.first == mangleName ) {
				return true;
			}
		}

		return false;
	}

	bool Indexer::hasIncompatibleCDecl(const std::string & id, const std::string &mangleName ) const {
		if ( ! idTable ) return false;

		++* stats().map_lookups;
		auto decls = idTable->find( id );
		if ( decls == idTable->end() ) return false;

		for ( auto decl : *(decls->second) ) {
			// skip other scopes (hidden by this decl)
			if ( decl.second.scope != scope ) continue;
			// check for C decl with incompatible type (by manglename)
			if ( ! LinkageSpec::isMangled( decl.second.id->linkage ) && decl.first != mangleName ) {
				return true;
			}
		}

		return false;
	}

	/// gets the base type of the first parameter; decl must be a ctor/dtor/assignment function
	std::string getOtypeKey( const FunctionDecl * function ) {
		auto& params = function->type->parameters;
		assert( ! params.empty() );
		// use base type of pointer, so that qualifiers on the pointer type aren't considered.
		Type * base = InitTweak::getPointerBase( params.front()->get_type() );
		assert( base );
		return Mangler::mangle( base );
	}

	/// gets the declaration for the function acting on a type specified by otype key,
	/// nullptr if none such
	const FunctionDecl * getFunctionForOtype( const DeclarationWithType * decl, const std::string& otypeKey ) {
		const FunctionDecl * func = dynamic_cast< const FunctionDecl * >( decl );
		if ( ! func || otypeKey != getOtypeKey( func ) ) return nullptr;
		return func;
	}

	bool Indexer::removeSpecialOverrides(Indexer::IdData& data, Indexer::MangleTable::Ptr& mangleTable ) {
		// if a type contains user defined ctor/dtor/assign, then special rules trigger, which
		// determinethe set of ctor/dtor/assign that can be used  by the requester. In particular,
		// if the user defines a default ctor, then the generated default ctor is unavailable,
		// likewise for copy ctor and dtor. If the user defines any ctor/dtor, then no generated
		// field ctors are available. If the user defines any ctor then the generated default ctor
		// is unavailable (intrinsic default ctor must be overridden exactly). If the user defines
		// anything that looks like a copy constructor, then the generated copy constructor is
		// unavailable, and likewise for the assignment operator.

		// only relevant on function declarations
		const FunctionDecl * function = dynamic_cast< const FunctionDecl * >( data.id );
		if ( ! function ) return true;
		// only need to perform this check for constructors, destructors, and assignment functions
		if ( ! CodeGen::isCtorDtorAssign( data.id->name ) ) return true;

		// set up information for this type
		bool dataIsUserDefinedFunc = ! LinkageSpec::isOverridable( function->linkage );
		bool dataIsCopyFunc = InitTweak::isCopyFunction( function, function->name );
		std::string dataOtypeKey = getOtypeKey( function );

		if ( dataIsUserDefinedFunc && dataIsCopyFunc ) {
			// this is a user-defined copy function
			// if this is the first such, delete/remove non-user-defined overloads as needed
			std::vector< std::string > removed;
			std::vector< MangleTable::value_type > deleted;
			bool alreadyUserDefinedFunc = false;

			for ( const auto& entry : * mangleTable ) {
				// skip decls that aren't functions or are for the wrong type
				const FunctionDecl * decl = getFunctionForOtype( entry.second.id, dataOtypeKey );
				if ( ! decl ) continue;

				bool isCopyFunc = InitTweak::isCopyFunction( decl, decl->name );
				if ( ! LinkageSpec::isOverridable( decl->linkage ) ) {
					// matching user-defined function
					if ( isCopyFunc ) {
						// mutation already performed, return early
						return true;
					} else {
						// note that non-copy deletions already performed
						alreadyUserDefinedFunc = true;
					}
				} else {
					// non-user-defined function; mark for deletion/removal as appropriate
					if ( isCopyFunc ) {
						removed.push_back( entry.first );
					} else if ( ! alreadyUserDefinedFunc ) {
						deleted.push_back( entry );
					}
				}
			}

			// perform removals from mangle table, and deletions if necessary
			for ( const auto& key : removed ) {
				++* stats().map_mutations;
				mangleTable = mangleTable->erase( key );
			}
			if ( ! alreadyUserDefinedFunc ) for ( const auto& entry : deleted ) {
				++* stats().map_mutations;
				mangleTable = mangleTable->set( entry.first, IdData{ entry.second, function } );
			}
		} else if ( dataIsUserDefinedFunc ) {
			// this is a user-defined non-copy function
			// if this is the first user-defined function, delete non-user-defined overloads
			std::vector< MangleTable::value_type > deleted;

			for ( const auto& entry : * mangleTable ) {
				// skip decls that aren't functions or are for the wrong type
				const FunctionDecl * decl = getFunctionForOtype( entry.second.id, dataOtypeKey );
				if ( ! decl ) continue;

				// exit early if already a matching user-defined function;
				// earlier function will have mutated table
				if ( ! LinkageSpec::isOverridable( decl->linkage ) ) return true;

				// skip mutating intrinsic functions
				if ( decl->linkage == LinkageSpec::Intrinsic ) continue;

				// user-defined non-copy functions do not override copy functions
				if ( InitTweak::isCopyFunction( decl, decl->name ) ) continue;

				// this function to be deleted after mangleTable iteration is complete
				deleted.push_back( entry );
			}

			// mark deletions to update mangle table
			// this needs to be a separate loop because of iterator invalidation
			for ( const auto& entry : deleted ) {
				++* stats().map_mutations;
				mangleTable = mangleTable->set( entry.first, IdData{ entry.second, function } );
			}
		} else if ( function->linkage != LinkageSpec::Intrinsic ) {
			// this is an overridable generated function
			// if there already exists a matching user-defined function, delete this appropriately
			for ( const auto& entry : * mangleTable ) {
				// skip decls that aren't functions or are for the wrong type
				const FunctionDecl * decl = getFunctionForOtype( entry.second.id, dataOtypeKey );
				if ( ! decl ) continue;

				// skip non-user-defined functions
				if ( LinkageSpec::isOverridable( decl->linkage ) ) continue;

				if ( dataIsCopyFunc ) {
					// remove current function if exists a user-defined copy function
					// since the signatures for copy functions don't need to match exactly, using
					// a delete statement is the wrong approach
					if ( InitTweak::isCopyFunction( decl, decl->name ) ) return false;
				} else {
					// mark current function deleted by first user-defined function found
					data.deleteStmt = decl;
					return true;
				}
			}
		}

		// nothing (more) to fix, return true
		return true;
	}

	void Indexer::addId(const DeclarationWithType * decl, OnConflict handleConflicts, const Expression * baseExpr,
			const Declaration * deleteStmt ) {
		++* stats().add_calls;
		if ( ! trackIdentifiers ) return;
		const std::string &name = decl->name;
		if ( name == "" ) return;

		std::string mangleName;
		if ( LinkageSpec::isOverridable( decl->linkage ) ) {
			// mangle the name without including the appropriate suffix, so overridable routines
			// are placed into the same "bucket" as their user defined versions.
			mangleName = Mangler::mangle( decl, false );
		} else {
			mangleName = Mangler::mangle( decl );
		} // if

		// this ensures that no two declarations with the same unmangled name at the same scope
		// both have C linkage
		if ( LinkageSpec::isMangled( decl->linkage ) ) {
			// Check that a Cforall declaration doesn't override any C declaration
			if ( hasCompatibleCDecl( name, mangleName ) ) {
				SemanticError( decl, "Cforall declaration hides C function " );
			}
		} else {
			// NOTE: only correct if name mangling is completely isomorphic to C
			// type-compatibility, which it may not be.
			if ( hasIncompatibleCDecl( name, mangleName ) ) {
				SemanticError( decl, "conflicting overload of C function " );
			}
		}

		// ensure tables exist and add identifier
		MangleTable::Ptr mangleTable;
		if ( ! idTable ) {
			idTable = IdTable::new_ptr();
			mangleTable = MangleTable::new_ptr();
		} else {
			++* stats().map_lookups;
			auto decls = idTable->find( name );
			if ( decls == idTable->end() ) {
				mangleTable = MangleTable::new_ptr();
			} else {
				mangleTable = decls->second;
				// skip in-scope repeat declarations of same identifier
				++* stats().map_lookups;
				auto existing = mangleTable->find( mangleName );
				if ( existing != mangleTable->end()
						&& existing->second.scope == scope
						&& existing->second.id ) {
					if ( addedIdConflicts( existing->second, decl, handleConflicts, deleteStmt ) ) {
						if ( handleConflicts.mode == OnConflict::Delete ) {
							// set delete expression for conflicting identifier
							lazyInitScope();
							* stats().map_mutations += 2;
							idTable = idTable->set(
								name,
								mangleTable->set(
									mangleName,
									IdData{ existing->second, handleConflicts.deleteStmt } ) );
						}
						return;
					}
				}
			}
		}

		// add/overwrite with new identifier
		lazyInitScope();
		IdData data{ decl, baseExpr, deleteStmt, scope };
		// Ensure that auto-generated ctor/dtor/assignment are deleted if necessary
		if ( ! removeSpecialOverrides( data, mangleTable ) ) return;
		* stats().map_mutations += 2;
		idTable = idTable->set( name, mangleTable->set( mangleName, std::move(data) ) );
	}

	void Indexer::addId( const DeclarationWithType * decl, const Expression * baseExpr ) {
		// default handling of conflicts is to raise an error
		addId( decl, OnConflict::error(), baseExpr, decl->isDeleted ? decl : nullptr );
	}

	void Indexer::addDeletedId( const DeclarationWithType * decl, const Declaration * deleteStmt ) {
		// default handling of conflicts is to raise an error
		addId( decl, OnConflict::error(), nullptr, deleteStmt );
	}

	bool addedTypeConflicts( const NamedTypeDecl * existing, const NamedTypeDecl * added ) {
		if ( existing->base == nullptr ) {
			return false;
		} else if ( added->base == nullptr ) {
			return true;
		} else {
			assert( existing->base && added->base );
			// typedef redeclarations are errors only if types are different
			if ( ! ResolvExpr::typesCompatible( existing->base, added->base, Indexer() ) ) {
				SemanticError( added->location, "redeclaration of " + added->name );
			}
		}
		// does not need to be added to the table if both existing and added have a base that are
		// the same
		return true;
	}

	void Indexer::addType( const NamedTypeDecl * decl ) {
		++* stats().add_calls;
		const std::string & id = decl->name;

		if ( ! typeTable ) {
			typeTable = TypeTable::new_ptr();
		} else {
			++* stats().map_lookups;
			auto existing = typeTable->find( id );
			if ( existing != typeTable->end()
				&& existing->second.scope == scope
				&& addedTypeConflicts( existing->second.decl, decl ) ) return;
		}

		lazyInitScope();
		++* stats().map_mutations;
		typeTable = typeTable->set( id, Scoped<NamedTypeDecl>{ decl, scope } );
	}

	bool addedDeclConflicts( const AggregateDecl * existing, const AggregateDecl * added ) {
		if ( ! existing->body ) {
			return false;
		} else if ( added->body ) {
			SemanticError( added, "redeclaration of " );
		} // if
		return true;
	}

	void Indexer::addStruct( const std::string & id ) {
		addStruct( new StructDecl( id ) );
	}

	void Indexer::addStruct( const StructDecl * decl ) {
		++* stats().add_calls;
		const std::string & id = decl->name;

		if ( ! structTable ) {
			structTable = StructTable::new_ptr();
		} else {
			++* stats().map_lookups;
			auto existing = structTable->find( id );
			if ( existing != structTable->end()
				&& existing->second.scope == scope
				&& addedDeclConflicts( existing->second.decl, decl ) ) return;
		}

		lazyInitScope();
		++* stats().map_mutations;
		structTable = structTable->set( id, Scoped<StructDecl>{ decl, scope } );
	}

	void Indexer::addEnum( const EnumDecl * decl ) {
		++* stats().add_calls;
		const std::string & id = decl->name;

		if ( ! enumTable ) {
			enumTable = EnumTable::new_ptr();
		} else {
			++* stats().map_lookups;
			auto existing = enumTable->find( id );
			if ( existing != enumTable->end()
				&& existing->second.scope == scope
				&& addedDeclConflicts( existing->second.decl, decl ) ) return;
		}

		lazyInitScope();
		++* stats().map_mutations;
		enumTable = enumTable->set( id, Scoped<EnumDecl>{ decl, scope } );
	}

	void Indexer::addUnion( const std::string & id ) {
		addUnion( new UnionDecl( id ) );
	}

	void Indexer::addUnion( const UnionDecl * decl ) {
		++* stats().add_calls;
		const std::string & id = decl->name;

		if ( ! unionTable ) {
			unionTable = UnionTable::new_ptr();
		} else {
			++* stats().map_lookups;
			auto existing = unionTable->find( id );
			if ( existing != unionTable->end()
				&& existing->second.scope == scope
				&& addedDeclConflicts( existing->second.decl, decl ) ) return;
		}

		lazyInitScope();
		++* stats().map_mutations;
		unionTable = unionTable->set( id, Scoped<UnionDecl>{ decl, scope } );
	}

	void Indexer::addTrait( const TraitDecl * decl ) {
		++* stats().add_calls;
		const std::string & id = decl->name;

		if ( ! traitTable ) {
			traitTable = TraitTable::new_ptr();
		} else {
			++* stats().map_lookups;
			auto existing = traitTable->find( id );
			if ( existing != traitTable->end()
				&& existing->second.scope == scope
				&& addedDeclConflicts( existing->second.decl, decl ) ) return;
		}

		lazyInitScope();
		++* stats().map_mutations;
		traitTable = traitTable->set( id, Scoped<TraitDecl>{ decl, scope } );
	}

	void Indexer::addMembers( const AggregateDecl * aggr, const Expression * expr, OnConflict handleConflicts ) {
		for ( Declaration * decl : aggr->members ) {
			if ( DeclarationWithType * dwt = dynamic_cast< DeclarationWithType * >( decl ) ) {
				addId( dwt, handleConflicts, expr );
				if ( dwt->name == "" ) {
					const Type * t = dwt->get_type()->stripReferences();
					if ( dynamic_cast<const StructInstType *>( t ) || dynamic_cast<const UnionInstType *>( t ) ) {
						Expression * base = expr->clone();
						ResolvExpr::Cost cost = ResolvExpr::Cost::zero; // xxx - carry this cost into the indexer as a base cost?
						ResolvExpr::referenceToRvalueConversion( base, cost );
						addMembers( t->getAggr(), new MemberExpr( dwt, base ), handleConflicts );
					}
				}
			}
		}
	}

	void Indexer::addWith( const std::list< Expression * > & withExprs, const Declaration * withStmt ) {
		for ( const Expression * expr : withExprs ) {
			if ( expr->result ) {
				AggregateDecl * aggr = expr->result->stripReferences()->getAggr();
				assertf( aggr, "WithStmt expr has non-aggregate type: %s", toString( expr->result ).c_str() );

				addMembers( aggr, expr, OnConflict::deleteWith( withStmt ) );
			}
		}
	}

	void Indexer::addIds( const std::list< DeclarationWithType * > & decls ) {
		for ( auto d : decls ) {
			addId( d );
		}
	}

	void Indexer::addTypes( const std::list< TypeDecl * > & tds ) {
		for ( auto td : tds ) {
			addType( td );
			addIds( td->assertions );
		}
	}

	void Indexer::addFunctionType( const FunctionType * ftype ) {
		addTypes( ftype->forall );
		addIds( ftype->returnVals );
		addIds( ftype->parameters );
	}

	Expression * Indexer::IdData::combine( ResolvExpr::Cost & cost ) const {
		Expression * ret = nullptr;
		if ( baseExpr ) {
			Expression * base = baseExpr->clone();
			ResolvExpr::referenceToRvalueConversion( base, cost );
			ret = new MemberExpr( const_cast<DeclarationWithType *>(id), base );
			// xxx - this introduces hidden environments, for now remove them.
			// std::swap( base->env, ret->env );
			delete base->env;
			base->env = nullptr;
		} else {
			ret = new VariableExpr( const_cast<DeclarationWithType *>(id) );
		}
		if ( deleteStmt ) ret = new DeletedExpr( ret, const_cast<Declaration *>(deleteStmt) );
		return ret;
	}
} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
