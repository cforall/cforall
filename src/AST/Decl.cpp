//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Decl.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Thu May 9 10:00:00 2019
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Jan 12 16:54:55 2021
// Update Count     : 23
//

#include "Decl.hpp"

#include <cassert>             // for assert, strict_dynamic_cast
#include <iostream>
#include <unordered_map>

#include "Common/utility.h"

#include "Fwd.hpp"             // for UniqueId
#include "Init.hpp"
#include "Node.hpp"            // for readonly
#include "Type.hpp"            // for readonly
#include "Expr.hpp"

namespace ast {

// To canonicalize declarations
static UniqueId lastUniqueId = 0;

using IdMapType = std::unordered_map< UniqueId, readonly<Decl> >;
static IdMapType idMap;

void Decl::fixUniqueId() {
	if ( uniqueId ) return;  // ensure only set once
	uniqueId = ++lastUniqueId;
	idMap[ uniqueId ] = this;
}

readonly<Decl> Decl::fromId( UniqueId id ) {
	IdMapType::const_iterator i = idMap.find( id );
	if ( i != idMap.end() ) return i->second;
	return {};
}

// --- FunctionDecl

FunctionDecl::FunctionDecl( const CodeLocation & loc, const std::string & name, 
	std::vector<ptr<TypeDecl>>&& forall,
	std::vector<ptr<DeclWithType>>&& params, std::vector<ptr<DeclWithType>>&& returns,
	CompoundStmt * stmts, Storage::Classes storage, Linkage::Spec linkage,
	std::vector<ptr<Attribute>>&& attrs, Function::Specs fs, bool isVarArgs)
: DeclWithType( loc, name, storage, linkage, std::move(attrs), fs ), params(std::move(params)), returns(std::move(returns)),
	type_params(std::move(forall)), stmts( stmts ) {
	FunctionType * ftype = new FunctionType(static_cast<ArgumentFlag>(isVarArgs));
	for (auto & param : this->params) {
		ftype->params.emplace_back(param->get_type());
	}
	for (auto & ret : this->returns) {
		ftype->returns.emplace_back(ret->get_type());
	}
	for (auto & tp : this->type_params) {
		ftype->forall.emplace_back(new TypeInstType(tp->name, tp));
		for (auto & ap: tp->assertions) {
			ftype->assertions.emplace_back(new VariableExpr(loc, ap));
		}
	}
	this->type = ftype;
}


const Type * FunctionDecl::get_type() const { return type.get(); }
void FunctionDecl::set_type( const Type * t ) {
	type = strict_dynamic_cast< const FunctionType * >( t );
}

// --- TypeDecl

const char * TypeDecl::typeString() const {
	static const char * kindNames[] = { "sized data type", "sized data type", "sized object type", "sized function type", "sized tuple type", "sized length value" };
	static_assert( sizeof(kindNames) / sizeof(kindNames[0]) == TypeDecl::NUMBER_OF_KINDS, "typeString: kindNames is out of sync." );
	assertf( kind < TypeDecl::NUMBER_OF_KINDS, "TypeDecl kind is out of bounds." );
	return sized ? kindNames[ kind ] : &kindNames[ kind ][ sizeof("sized") ]; // sizeof includes '\0'
}

const char * TypeDecl::genTypeString() const {
	static const char * kindNames[] = { "T &", "T *", "T", "(*)", "T ...", "[T]" };
	static_assert( sizeof(kindNames) / sizeof(kindNames[0]) == TypeDecl::NUMBER_OF_KINDS, "genTypeString: kindNames is out of sync." );
	assertf( kind < TypeDecl::NUMBER_OF_KINDS, "TypeDecl kind is out of bounds." );
	return kindNames[ kind ];
}

std::ostream & operator<< ( std::ostream & out, const TypeDecl::Data & data ) {
	return out << data.kind << ", " << data.isComplete;
}

// --- AggregateDecl

// These must harmonize with the corresponding AggregateDecl::Aggregate enumerations.
static const char * aggregateNames[] = { "struct", "union", "enum", "exception", "trait", "generator", "coroutine", "monitor", "thread", "NoAggregateName" };

const char * AggregateDecl::aggrString( AggregateDecl::Aggregate aggr ) {
	return aggregateNames[aggr];
}

// --- EnumDecl

bool EnumDecl::valueOf( const Decl * enumerator, long long& value ) const {
	if ( enumValues.empty() ) {
		long long crntVal = 0;
		for ( const Decl * member : members ) {
			const ObjectDecl* field = strict_dynamic_cast< const ObjectDecl* >( member );
			if ( field->init ) {
				const SingleInit * init = strict_dynamic_cast< const SingleInit* >( field->init.get() );
				auto result = eval( init->value );
				if ( ! result.second ) {
					SemanticError( init->location, ::toString( "Non-constexpr in initialization of "
						"enumerator: ", field ) );
				}
				crntVal = result.first;
			}
			if ( enumValues.count( field->name ) != 0 ) {
				SemanticError( location, ::toString( "Enum ", name, " has multiple members with the " 	"name ", field->name ) );
			}
			enumValues[ field->name ] = crntVal;
			++crntVal;
		}
	}

	auto it = enumValues.find( enumerator->name );
	if ( it != enumValues.end() ) {
		value = it->second;
		return true;
	}
	return false;
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
