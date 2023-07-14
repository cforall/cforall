//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SymbolTable.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 29 11:00:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Wed May 29 11:00:00 2019
// Update Count     : 1
//

#include "SymbolTable.hpp"

#include <cassert>

#include "Copy.hpp"
#include "Decl.hpp"
#include "Expr.hpp"
#include "Inspect.hpp"
#include "Type.hpp"
#include "CodeGen/OperatorTable.h"         // for isCtorDtorAssign
#include "Common/SemanticError.h"
#include "Common/Stats/Counter.h"
#include "GenPoly/GenPoly.h"
#include "InitTweak/InitTweak.h"
#include "ResolvExpr/Cost.h"
#include "ResolvExpr/CandidateFinder.hpp"  // for referenceToRvalueConversion
#include "ResolvExpr/Unify.h"
#include "SymTab/Mangler.h"

namespace ast {

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

Expr * SymbolTable::IdData::combine( const CodeLocation & loc, ResolvExpr::Cost & cost ) const {
	Expr * ret;
	if ( baseExpr ) {
		if (baseExpr->env) {
			Expr * base = deepCopy(baseExpr);
			const TypeSubstitution * subs = baseExpr->env;
			base->env = nullptr;
			ret = new MemberExpr{loc, id, referenceToRvalueConversion( base, cost )};
			ret->env = subs;
		}
		else {
			ret = new MemberExpr{ loc, id, referenceToRvalueConversion( baseExpr, cost ) };
		}
	}
	else {
		ret = new VariableExpr{ loc, id };
	}
	if ( deleter ) { ret = new DeletedExpr{ loc, ret, deleter }; }
	return ret;
}

SymbolTable::SymbolTable( ErrorDetection errorMode )
: idTable(), typeTable(), structTable(), enumTable(), unionTable(), traitTable(),
  prevScope(), scope( 0 ), repScope( 0 ), errorMode(errorMode) { ++*stats().count; }

SymbolTable::~SymbolTable() { stats().size->push( idTable ? idTable->size() : 0 ); }

void SymbolTable::OnFindError( CodeLocation location, std::string error ) const {
	assertf( errorMode != AssertClean, "Name collision/redefinition, found during a compilation phase where none should be possible.  Detail: %s", error.c_str() );
	if (errorMode == ValidateOnAdd) {
		SemanticError(location, error);
	}
	assertf( errorMode == IgnoreErrors, "Unrecognized symbol-table error mode %d", errorMode );
}

void SymbolTable::enterScope() {
	++scope;

	++*stats().new_scopes;
	stats().avg_scope_depth->push( scope );
	stats().max_scope_depth->push( scope );
}

void SymbolTable::leaveScope() {
	if ( repScope == scope ) {
		Ptr prev = prevScope;           // make sure prevScope stays live
		*this = std::move(*prevScope);  // replace with previous scope
	}

	--scope;
}

SymbolTable::SpecialFunctionKind SymbolTable::getSpecialFunctionKind(const std::string & name) {
	if (name == "?{}") return CTOR;
	if (name == "^?{}") return DTOR;
	if (name == "?=?") return ASSIGN;
	return NUMBER_OF_KINDS;
}

std::vector<SymbolTable::IdData> SymbolTable::lookupId( const std::string &id ) const {
	static Stats::Counters::CounterGroup * name_lookup_stats = Stats::Counters::build<Stats::Counters::CounterGroup>("Name Lookup Stats");
	static std::map<std::string, Stats::Counters::SimpleCounter *> lookups_by_name;
	static std::map<std::string, Stats::Counters::SimpleCounter *> candidates_by_name;

	SpecialFunctionKind kind = getSpecialFunctionKind(id);
	if (kind != NUMBER_OF_KINDS) return specialLookupId(kind);

	++*stats().lookup_calls;
	if ( ! idTable ) return {};

	++*stats().map_lookups;
	auto decls = idTable->find( id );
	if ( decls == idTable->end() ) return {};

	std::vector<IdData> out;
	for ( auto decl : *(decls->second) ) {
		out.push_back( decl.second );
	}

	if (Stats::Counters::enabled) {
		if (! lookups_by_name.count(id)) {
			// leaks some strings, but it is because Counters do not hold them
			auto lookupCounterName = new std::string(id + "%count");
			auto candidatesCounterName = new std::string(id + "%candidate");
			lookups_by_name.emplace(id, new Stats::Counters::SimpleCounter(lookupCounterName->c_str(), name_lookup_stats));
			candidates_by_name.emplace(id, new Stats::Counters::SimpleCounter(candidatesCounterName->c_str(), name_lookup_stats));
		}
		(*lookups_by_name[id]) ++;
		*candidates_by_name[id] += out.size();
	}

	return out;
}

std::vector<SymbolTable::IdData> SymbolTable::specialLookupId( SymbolTable::SpecialFunctionKind kind, const std::string & otypeKey ) const {
	static Stats::Counters::CounterGroup * special_stats = Stats::Counters::build<Stats::Counters::CounterGroup>("Special Lookups");
	static Stats::Counters::SimpleCounter * stat_counts[3] = {
		Stats::Counters::build<Stats::Counters::SimpleCounter>("constructor - count", special_stats),
		Stats::Counters::build<Stats::Counters::SimpleCounter>("destructor - count", special_stats),
		Stats::Counters::build<Stats::Counters::SimpleCounter>("assignment - count", special_stats)
	};

	static Stats::Counters::SimpleCounter * stat_candidates[3] = {
		Stats::Counters::build<Stats::Counters::SimpleCounter>("constructor - candidates", special_stats),
		Stats::Counters::build<Stats::Counters::SimpleCounter>("destructor - candidates", special_stats),
		Stats::Counters::build<Stats::Counters::SimpleCounter>("assignment - candidates", special_stats)
	};

	static Stats::Counters::SimpleCounter * num_lookup_with_key 
		= Stats::Counters::build<Stats::Counters::SimpleCounter>("keyed lookups", special_stats);
	static Stats::Counters::SimpleCounter * num_lookup_without_key 
		= Stats::Counters::build<Stats::Counters::SimpleCounter>("unkeyed lookups", special_stats);

	assert (kind != NUMBER_OF_KINDS);
	++*stats().lookup_calls;
	if ( ! specialFunctionTable[kind] ) return {};

	std::vector<IdData> out;

	if (otypeKey.empty()) { // returns everything
		++*num_lookup_without_key;
		for (auto & table : *specialFunctionTable[kind]) {
			for (auto & decl : *table.second) {
				out.push_back(decl.second);
			}
		}
	}
	else {
		++*num_lookup_with_key;
		++*stats().map_lookups;
		auto decls = specialFunctionTable[kind]->find(otypeKey);
		if (decls == specialFunctionTable[kind]->end()) return {};

		for (auto decl : *(decls->second)) {
			out.push_back(decl.second);
		}
	}

	++*stat_counts[kind];
	*stat_candidates[kind] += out.size();

	return out;
}

const NamedTypeDecl * SymbolTable::lookupType( const std::string &id ) const {
	++*stats().lookup_calls;
	if ( ! typeTable ) return nullptr;
	++*stats().map_lookups;
	auto it = typeTable->find( id );
	return it == typeTable->end() ? nullptr : it->second.decl;
}

const StructDecl * SymbolTable::lookupStruct( const std::string &id ) const {
	++*stats().lookup_calls;
	if ( ! structTable ) return nullptr;
	++*stats().map_lookups;
	auto it = structTable->find( id );
	return it == structTable->end() ? nullptr : it->second.decl;
}

const EnumDecl * SymbolTable::lookupEnum( const std::string &id ) const {
	++*stats().lookup_calls;
	if ( ! enumTable ) return nullptr;
	++*stats().map_lookups;
	auto it = enumTable->find( id );
	return it == enumTable->end() ? nullptr : it->second.decl;
}

const UnionDecl * SymbolTable::lookupUnion( const std::string &id ) const {
	++*stats().lookup_calls;
	if ( ! unionTable ) return nullptr;
	++*stats().map_lookups;
	auto it = unionTable->find( id );
	return it == unionTable->end() ? nullptr : it->second.decl;
}

const TraitDecl * SymbolTable::lookupTrait( const std::string &id ) const {
	++*stats().lookup_calls;
	if ( ! traitTable ) return nullptr;
	++*stats().map_lookups;
	auto it = traitTable->find( id );
	return it == traitTable->end() ? nullptr : it->second.decl;
}

const NamedTypeDecl * SymbolTable::globalLookupType( const std::string &id ) const {
	return atScope( 0 )->lookupType( id );
}

const StructDecl * SymbolTable::globalLookupStruct( const std::string &id ) const {
	return atScope( 0 )->lookupStruct( id );
}

const UnionDecl * SymbolTable::globalLookupUnion( const std::string &id ) const {
	return atScope( 0 )->lookupUnion( id );
}

const EnumDecl * SymbolTable::globalLookupEnum( const std::string &id ) const {
	return atScope( 0 )->lookupEnum( id );
}

void SymbolTable::addId( const DeclWithType * decl, const Expr * baseExpr ) {
	// default handling of conflicts is to raise an error
	addIdCommon( decl, OnConflict::error(), baseExpr, decl->isDeleted ? decl : nullptr );
}

void SymbolTable::addDeletedId( const DeclWithType * decl, const Decl * deleter ) {
	// default handling of conflicts is to raise an error
	addIdCommon( decl, OnConflict::error(), nullptr, deleter );
}

bool SymbolTable::addedTypeConflicts(
		const NamedTypeDecl * existing, const NamedTypeDecl * added ) const {
	if ( existing->base == nullptr ) {
		return false;
	} else if ( added->base == nullptr ) {
		return true;
	} else {
		// typedef redeclarations are errors only if types are different
		if ( ! ResolvExpr::typesCompatible( existing->base, added->base ) ) {
			OnFindError( added->location, "redeclaration of " + added->name );
		}
	}
	// does not need to be added to the table if both existing and added have a base that are
	// the same
	return true;
}

bool SymbolTable::addedDeclConflicts( 
		const AggregateDecl * existing, const AggregateDecl * added ) const {
	if ( ! existing->body ) {
		return false;
	} else if ( added->body ) {
		OnFindError( added, "redeclaration of " );
	}
	return true;
}

void SymbolTable::addType( const NamedTypeDecl * decl ) {
	++*stats().add_calls;
	const std::string &id = decl->name;

	if ( ! typeTable ) {
		typeTable = TypeTable::new_ptr();
	} else {
		++*stats().map_lookups;
		auto existing = typeTable->find( id );
		if ( existing != typeTable->end()
			&& existing->second.scope == scope
			&& addedTypeConflicts( existing->second.decl, decl ) ) return;
	}

	lazyInitScope();
	++*stats().map_mutations;
	typeTable = typeTable->set( id, scoped<NamedTypeDecl>{ decl, scope } );
}

void SymbolTable::addStructId( const std::string &id ) {
	addStruct( new StructDecl( CodeLocation(), id ) );
}

void SymbolTable::addStruct( const StructDecl * decl ) {
	++*stats().add_calls;
	const std::string &id = decl->name;

	if ( ! structTable ) {
		structTable = StructTable::new_ptr();
	} else {
		++*stats().map_lookups;
		auto existing = structTable->find( id );
		if ( existing != structTable->end()
			&& existing->second.scope == scope
			&& addedDeclConflicts( existing->second.decl, decl ) ) return;
	}

	lazyInitScope();
	++*stats().map_mutations;
	structTable = structTable->set( id, scoped<StructDecl>{ decl, scope } );
}

void SymbolTable::addEnum( const EnumDecl *decl ) {
	++*stats().add_calls;
	const std::string &id = decl->name;

	if ( ! enumTable ) {
		enumTable = EnumTable::new_ptr();
	} else {
		++*stats().map_lookups;
		auto existing = enumTable->find( id );
		if ( existing != enumTable->end()
			&& existing->second.scope == scope
			&& addedDeclConflicts( existing->second.decl, decl ) ) return;
	}

	lazyInitScope();
	++*stats().map_mutations;
	enumTable = enumTable->set( id, scoped<EnumDecl>{ decl, scope } );
}

void SymbolTable::addUnionId( const std::string &id ) {
	addUnion( new UnionDecl( CodeLocation(), id ) );
}

void SymbolTable::addUnion( const UnionDecl * decl ) {
	++*stats().add_calls;
	const std::string &id = decl->name;

	if ( ! unionTable ) {
		unionTable = UnionTable::new_ptr();
	} else {
		++*stats().map_lookups;
		auto existing = unionTable->find( id );
		if ( existing != unionTable->end()
			&& existing->second.scope == scope
			&& addedDeclConflicts( existing->second.decl, decl ) ) return;
	}

	lazyInitScope();
	++*stats().map_mutations;
	unionTable = unionTable->set( id, scoped<UnionDecl>{ decl, scope } );
}

void SymbolTable::addTrait( const TraitDecl * decl ) {
	++*stats().add_calls;
	const std::string &id = decl->name;

	if ( ! traitTable ) {
		traitTable = TraitTable::new_ptr();
	} else {
		++*stats().map_lookups;
		auto existing = traitTable->find( id );
		if ( existing != traitTable->end()
			&& existing->second.scope == scope
			&& addedDeclConflicts( existing->second.decl, decl ) ) return;
	}

	lazyInitScope();
	++*stats().map_mutations;
	traitTable = traitTable->set( id, scoped<TraitDecl>{ decl, scope } );
}


void SymbolTable::addWith( const std::vector< ptr<Expr> > & withExprs, const Decl * withStmt ) {
	for ( const Expr * expr : withExprs ) {
		if ( ! expr->result ) continue;
		const Type * resTy = expr->result->stripReferences();
		auto aggrType = dynamic_cast< const BaseInstType * >( resTy );
		assertf( aggrType, "WithStmt expr has non-aggregate type: %s",
			toString( expr->result ).c_str() );
		const AggregateDecl * aggr = aggrType->aggr();
		assertf( aggr, "WithStmt has null aggregate from type: %s",
			toString( expr->result ).c_str() );

		addMembers( aggr, expr, OnConflict::deleteWith( withStmt ) );
	}
}

void SymbolTable::addIds( const std::vector< ptr<DeclWithType> > & decls ) {
	for ( const DeclWithType * decl : decls ) { addId( decl ); }
}

void SymbolTable::addTypes( const std::vector< ptr<TypeDecl> > & tds ) {
	for ( const TypeDecl * td : tds ) {
		addType( td );
		addIds( td->assertions );
	}
}


void SymbolTable::addFunction( const FunctionDecl * func ) {
	for (auto & td : func->type_params) {
		addType(td);
	}
	for (auto & asst : func->assertions) {
		addId(asst);
	}
	// addTypes( func->type->forall );
	addIds( func->returns );
	addIds( func->params );
}


void SymbolTable::lazyInitScope() {
	// do nothing if already in represented scope
	if ( repScope == scope ) return;

	++*stats().lazy_scopes;
	// create rollback
	prevScope = std::make_shared<SymbolTable>( *this );
	// update repScope
	repScope = scope;
}

const ast::SymbolTable * SymbolTable::atScope( unsigned long target ) const {
	// by lazy construction, final symtab in list has repScope 0, cannot be > target
	// otherwise, will find first scope representing the target
	const SymbolTable * symtab = this;
	while ( symtab->repScope > target ) {
		symtab = symtab->prevScope.get();
	}
	return symtab;
}

namespace {
	/// gets the base type of the first parameter; decl must be a ctor/dtor/assignment function
	std::string getOtypeKey( const FunctionType * ftype, bool stripParams = true ) {
		const auto & params = ftype->params;
		assert( ! params.empty() );
		// use base type of pointer, so that qualifiers on the pointer type aren't considered.
		const Type * base = ast::getPointerBase( params.front() );
		assert( base );
		if (stripParams) {
			if (dynamic_cast<const PointerType *>(base)) return Mangle::Encoding::pointer;
			return Mangle::mangle( base, Mangle::Type | Mangle::NoGenericParams );
		}
		else
			return Mangle::mangle( base );	
	}

	/// gets the declaration for the function acting on a type specified by otype key,
	/// nullptr if none such
	const FunctionDecl * getFunctionForOtype(
			const DeclWithType * decl, const std::string & otypeKey ) {
		auto func = dynamic_cast< const FunctionDecl * >( decl );
		if ( ! func || otypeKey != getOtypeKey( func->type, false ) ) return nullptr;
		return func;
	}
}

bool SymbolTable::removeSpecialOverrides(
		SymbolTable::IdData & data, SymbolTable::MangleTable::Ptr & mangleTable ) {
	// if a type contains user defined ctor/dtor/assign, then special rules trigger, which
	// determine the set of ctor/dtor/assign that can be used  by the requester. In particular,
	// if the user defines a default ctor, then the generated default ctor is unavailable,
	// likewise for copy ctor and dtor. If the user defines any ctor/dtor, then no generated
	// field ctors are available. If the user defines any ctor then the generated default ctor
	// is unavailable (intrinsic default ctor must be overridden exactly). If the user defines
	// anything that looks like a copy constructor, then the generated copy constructor is
	// unavailable, and likewise for the assignment operator.

	// only relevant on function declarations
	const FunctionDecl * function = data.id.as< FunctionDecl >();
	if ( ! function ) return true;
	// only need to perform this check for constructors, destructors, and assignment functions
	if ( ! CodeGen::isCtorDtorAssign( data.id->name ) ) return true;

	// set up information for this type
	bool dataIsUserDefinedFunc = ! function->linkage.is_overrideable;
	bool dataIsCopyFunc = InitTweak::isCopyFunction( function );
	std::string dataOtypeKey = getOtypeKey( function->type, false ); // requires exact match to override autogen

	if ( dataIsUserDefinedFunc && dataIsCopyFunc ) {
		// this is a user-defined copy function
		// if this is the first such, delete/remove non-user-defined overloads as needed
		std::vector< std::string > removed;
		std::vector< MangleTable::value_type > deleted;
		bool alreadyUserDefinedFunc = false;

		for ( const auto& entry : *mangleTable ) {
			// skip decls that aren't functions or are for the wrong type
			const FunctionDecl * decl = getFunctionForOtype( entry.second.id, dataOtypeKey );
			if ( ! decl ) continue;

			bool isCopyFunc = InitTweak::isCopyFunction( decl );
			if ( ! decl->linkage.is_overrideable ) {
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
			++*stats().map_mutations;
			mangleTable = mangleTable->erase( key );
		}
		if ( ! alreadyUserDefinedFunc ) for ( const auto& entry : deleted ) {
			++*stats().map_mutations;
			mangleTable = mangleTable->set( entry.first, IdData{ entry.second, function } );
		}
	} else if ( dataIsUserDefinedFunc ) {
		// this is a user-defined non-copy function
		// if this is the first user-defined function, delete non-user-defined overloads
		std::vector< MangleTable::value_type > deleted;

		for ( const auto& entry : *mangleTable ) {
			// skip decls that aren't functions or are for the wrong type
			const FunctionDecl * decl = getFunctionForOtype( entry.second.id, dataOtypeKey );
			if ( ! decl ) continue;

			// exit early if already a matching user-defined function;
			// earlier function will have mutated table
			if ( ! decl->linkage.is_overrideable ) return true;

			// skip mutating intrinsic functions
			if ( decl->linkage == Linkage::Intrinsic ) continue;

			// user-defined non-copy functions do not override copy functions
			if ( InitTweak::isCopyFunction( decl ) ) continue;

			// this function to be deleted after mangleTable iteration is complete
			deleted.push_back( entry );
		}

		// mark deletions to update mangle table
		// this needs to be a separate loop because of iterator invalidation
		for ( const auto& entry : deleted ) {
			++*stats().map_mutations;
			mangleTable = mangleTable->set( entry.first, IdData{ entry.second, function } );
		}
	} else if ( function->linkage != Linkage::Intrinsic ) {
		// this is an overridable generated function
		// if there already exists a matching user-defined function, delete this appropriately
		for ( const auto& entry : *mangleTable ) {
			// skip decls that aren't functions or are for the wrong type
			const FunctionDecl * decl = getFunctionForOtype( entry.second.id, dataOtypeKey );
			if ( ! decl ) continue;

			// skip non-user-defined functions
			if ( decl->linkage.is_overrideable ) continue;

			if ( dataIsCopyFunc ) {
				// remove current function if exists a user-defined copy function
				// since the signatures for copy functions don't need to match exactly, using
				// a delete statement is the wrong approach
				if ( InitTweak::isCopyFunction( decl ) ) return false;
			} else {
				// mark current function deleted by first user-defined function found
				data.deleter = decl;
				return true;
			}
		}
	}

	// nothing (more) to fix, return true
	return true;
}

namespace {
	/// true iff the declaration represents a function
	bool isFunction( const DeclWithType * decl ) {
		return GenPoly::getFunctionType( decl->get_type() );
	}

	bool isObject( const DeclWithType * decl ) { return ! isFunction( decl ); }

	/// true if the declaration represents a definition instead of a forward decl
	bool isDefinition( const DeclWithType * decl ) {
		if ( auto func = dynamic_cast< const FunctionDecl * >( decl ) ) {
			// a function is a definition if it has a body
			return func->stmts;
		} else {
			// an object is a definition if it is not marked extern
			return ! decl->storage.is_extern;
		}
	}
}

bool SymbolTable::addedIdConflicts(
		const SymbolTable::IdData & existing, const DeclWithType * added,
		SymbolTable::OnConflict handleConflicts, const Decl * deleter ) {
	// if we're giving the same name mangling to things of different types then there is something
	// wrong
	assert( (isObject( added ) && isObject( existing.id ) )
		|| ( isFunction( added ) && isFunction( existing.id ) ) );

	if ( existing.id->linkage.is_overrideable ) {
		// new definition shadows the autogenerated one, even at the same scope
		return false;
	} else if ( existing.id->linkage.is_mangled
			|| ResolvExpr::typesCompatible(
				added->get_type(), existing.id->get_type() ) ) {

		// it is a conflict if one declaration is deleted and the other is not
		if ( deleter && ! existing.deleter ) {
			if ( handleConflicts.mode == OnConflict::Error ) {
				OnFindError( added, "deletion of defined identifier " );
			}
			return true;
		} else if ( ! deleter && existing.deleter ) {
			if ( handleConflicts.mode == OnConflict::Error ) {
				OnFindError( added, "definition of deleted identifier " );
			}
			return true;
		}

		// it is a conflict if both declarations are definitions
		if ( isDefinition( added ) && isDefinition( existing.id ) ) {
			if ( handleConflicts.mode == OnConflict::Error ) {
				OnFindError( added,
					isFunction( added ) ?
						"duplicate function definition for " :
						"duplicate object definition for " );
			}
			return true;
		}
	} else {
		if ( handleConflicts.mode == OnConflict::Error ) {
			OnFindError( added, "duplicate definition for " );
		}
		return true;
	}

	return true;
}

void SymbolTable::addIdCommon(
		const DeclWithType * decl, SymbolTable::OnConflict handleConflicts,
		const Expr * baseExpr, const Decl * deleter ) {
	SpecialFunctionKind kind = getSpecialFunctionKind(decl->name);
	if (kind == NUMBER_OF_KINDS) { // not a special decl
		addIdToTable(decl, decl->name, idTable, handleConflicts, baseExpr, deleter);
	}
	else {
		std::string key;
		if (auto func = dynamic_cast<const FunctionDecl *>(decl)) {
			key = getOtypeKey(func->type);
		}
		else if (auto obj = dynamic_cast<const ObjectDecl *>(decl)) {
			key = getOtypeKey(obj->type.strict_as<PointerType>()->base.strict_as<FunctionType>());
		}
		else {
			assertf(false, "special decl with non-function type");
		}
		addIdToTable(decl, key, specialFunctionTable[kind], handleConflicts, baseExpr, deleter);
	}
}

void SymbolTable::addIdToTable(
		const DeclWithType * decl, const std::string & lookupKey,
		IdTable::Ptr & table, SymbolTable::OnConflict handleConflicts,
		const Expr * baseExpr, const Decl * deleter ) {
	++*stats().add_calls;
	const std::string &name = decl->name;
	if ( name == "" ) return;

	std::string mangleName;
	if ( decl->linkage.is_overrideable ) {
		// mangle the name without including the appropriate suffix, so overridable routines
		// are placed into the same "bucket" as their user defined versions.
		mangleName = Mangle::mangle( decl, Mangle::Mode{ Mangle::NoOverrideable } );
	} else {
		mangleName = Mangle::mangle( decl );
	}

	// this ensures that no two declarations with the same unmangled name at the same scope
	// both have C linkage
	if ( decl->linkage.is_mangled ) {
		// Check that a Cforall declaration doesn't override any C declaration
		if ( hasCompatibleCDecl( name, mangleName ) ) {
			OnFindError( decl, "Cforall declaration hides C function " );
		}
	} else {
		// NOTE: only correct if name mangling is completely isomorphic to C
		// type-compatibility, which it may not be.
		if ( hasIncompatibleCDecl( name, mangleName ) ) {
			OnFindError( decl, "conflicting overload of C function " );
		}
	}

	// ensure tables exist and add identifier
	MangleTable::Ptr mangleTable;
	if ( ! table ) {
		table = IdTable::new_ptr();
		mangleTable = MangleTable::new_ptr();
	} else {
		++*stats().map_lookups;
		auto decls = table->find( lookupKey );
		if ( decls == table->end() ) {
			mangleTable = MangleTable::new_ptr();
		} else {
			mangleTable = decls->second;
			// skip in-scope repeat declarations of same identifier
			++*stats().map_lookups;
			auto existing = mangleTable->find( mangleName );
			if ( existing != mangleTable->end()
					&& existing->second.scope == scope
					&& existing->second.id ) {
				if ( addedIdConflicts( existing->second, decl, handleConflicts, deleter ) ) {
					if ( handleConflicts.mode == OnConflict::Delete ) {
						// set delete expression for conflicting identifier
						lazyInitScope();
						*stats().map_mutations += 2;
						table = table->set(
							lookupKey,
							mangleTable->set(
								mangleName,
								IdData{ existing->second, handleConflicts.deleter } ) );
					}
					return;
				}
			}
		}
	}

	// add/overwrite with new identifier
	lazyInitScope();
	IdData data{ decl, baseExpr, deleter, scope };
	// Ensure that auto-generated ctor/dtor/assignment are deleted if necessary
	if (table != idTable) { // adding to special table
		if ( ! removeSpecialOverrides( data, mangleTable ) ) return;
	}
	*stats().map_mutations += 2;
	table = table->set( lookupKey, mangleTable->set( mangleName, std::move(data) ) );
}

void SymbolTable::addMembers(
		const AggregateDecl * aggr, const Expr * expr, SymbolTable::OnConflict handleConflicts ) {
	for ( const ptr<Decl> & decl : aggr->members ) {
		auto dwt = decl.as<DeclWithType>();
		if ( nullptr == dwt ) continue;
		addIdCommon( dwt, handleConflicts, expr );
		// Inline through unnamed struct/union members.
		if ( "" != dwt->name ) continue;
		const Type * t = dwt->get_type()->stripReferences();
		if ( auto rty = dynamic_cast<const BaseInstType *>( t ) ) {
			if ( ! dynamic_cast<const StructInstType *>(rty)
				&& ! dynamic_cast<const UnionInstType *>(rty) ) continue;
			ResolvExpr::Cost cost = ResolvExpr::Cost::zero;
			ast::ptr<ast::TypeSubstitution> tmp = expr->env;
			expr = mutate_field(expr, &Expr::env, nullptr);
			const Expr * base = ResolvExpr::referenceToRvalueConversion( expr, cost );
			base = mutate_field(base, &Expr::env, tmp);

			addMembers(
				rty->aggr(), new MemberExpr{ base->location, dwt, base }, handleConflicts );
		}
	}
}

bool SymbolTable::hasCompatibleCDecl( const std::string &id, const std::string &mangleName ) const {
	if ( ! idTable ) return false;

	++*stats().map_lookups;
	auto decls = idTable->find( id );
	if ( decls == idTable->end() ) return false;

	for ( auto decl : *(decls->second) ) {
		// skip other scopes (hidden by this decl)
		if ( decl.second.scope != scope ) continue;
		// check for C decl with compatible type (by mangleName)
		if ( ! decl.second.id->linkage.is_mangled && decl.first == mangleName ) return true;
	}

	return false;
}

bool SymbolTable::hasIncompatibleCDecl( const std::string &id, const std::string &mangleName ) const {
	if ( ! idTable ) return false;

	++*stats().map_lookups;
	auto decls = idTable->find( id );
	if ( decls == idTable->end() ) return false;

	for ( auto decl : *(decls->second) ) {
		// skip other scopes (hidden by this decl)
		if ( decl.second.scope != scope ) continue;
		// check for C decl with incompatible type (by manglename)
		if ( ! decl.second.id->linkage.is_mangled && decl.first != mangleName ) return true;
	}

	return false;
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
