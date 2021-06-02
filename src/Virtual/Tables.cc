//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Tables.cc --
//
// Author           : Andrew Beach
// Created On       : Mon Aug 31 11:11:00 2020
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr 21 15:36:00 2021
// Update Count     : 2
//

#include <SynTree/Attribute.h>
#include <SynTree/Declaration.h>
#include <SynTree/Expression.h>
#include <SynTree/Statement.h>
#include <SynTree/Type.h>

namespace Virtual {

std::string typeIdType( std::string const & type_name ) {
	return "__cfatid_struct_" + type_name;
}

std::string typeIdName( std::string const & type_name ) {
	return "__cfatid_" + type_name;
}

static std::string typeIdTypeToInstance( std::string const & type_name ) {
	return typeIdName(type_name.substr(16));
}

std::string vtableTypeName( std::string const & name ) {
	return name + "_vtable";
}

std::string baseTypeName( std::string const & vtable_type_name ) {
	return vtable_type_name.substr(0, vtable_type_name.size() - 7);
}

std::string instanceName( std::string const & name ) {
	return std::string("_") + name + "_instance";
}

std::string vtableInstanceName( std::string const & name ) {
	return instanceName( vtableTypeName( name ) );
}

std::string concurrentDefaultVTableName() {
	return "_default_vtable";
}

bool isVTableInstanceName( std::string const & name ) {
	// There are some delicate length calculations here.
	return 17 < name.size() && '_' == name[0] &&
		std::string("_vtable_instance") == name.substr(1, name.size() - 17);
}

static ObjectDecl * makeVtableDeclaration(
		std::string const & name,
		StructInstType * type, Initializer * init ) {
	Type::StorageClasses storage = noStorageClasses;
	if ( nullptr == init ) {
		storage.is_extern = true;
	}
	return new ObjectDecl(
		name,
		storage,
		LinkageSpec::Cforall,
		nullptr,
		type,
		init
	);
}

ObjectDecl * makeVtableForward( std::string const & name, StructInstType * type ) {
	assert( type );
	return makeVtableDeclaration( name, type, nullptr );
}

ObjectDecl * makeVtableInstance(
		std::string const & name, StructInstType * vtableType,
		Type * objectType, Initializer * init ) {
	assert( vtableType );
	assert( objectType );
	StructDecl * vtableStruct = vtableType->baseStruct;
	// Build the initialization
	if ( nullptr == init ) {
		std::list< Initializer * > inits;

		// This is going to have to be run before the resolver to connect expressions.
		for ( auto field : vtableStruct->members ) {
			if ( std::string( "parent" ) == field->name ) {
				// This will not work with polymorphic state.
				auto oField = strict_dynamic_cast< ObjectDecl * >( field );
				auto fieldType = strict_dynamic_cast< PointerType * >( oField->type );
				auto parentType = strict_dynamic_cast< StructInstType * >( fieldType->base );
				std::string const & parentInstance = instanceName( parentType->name );
				inits.push_back(
						new SingleInit( new AddressExpr( new NameExpr( parentInstance ) ) ) );
			} else if ( std::string( "__cfavir_typeid" ) == field->name ) {
				std::string const & baseType = baseTypeName( vtableType->name );
				std::string const & typeId = typeIdName( baseType );
				inits.push_back( new SingleInit( new AddressExpr( new NameExpr( typeId ) ) ) );
			} else if ( std::string( "size" ) == field->name ) {
				inits.push_back( new SingleInit( new SizeofExpr( objectType->clone() ) ) );
			} else if ( std::string( "align" ) == field->name ) {
				inits.push_back( new SingleInit( new AlignofExpr( objectType->clone() ) ) );
			} else {
				inits.push_back( new SingleInit( new NameExpr( field->name ) ) );
			}
		}
		init = new ListInit( inits );
	// This should initialize everything except the parent pointer, the
	// size-of and align-of fields. These should be inserted.
	} else {
		assert(false);
	}
	return makeVtableDeclaration( name, vtableType, init );
}

namespace {
	std::string const functionName = "get_exception_vtable";
}

FunctionDecl * makeGetExceptionForward(
		Type * vtableType, Type * exceptType ) {
	assert( vtableType );
	assert( exceptType );
	FunctionType * type = new FunctionType( noQualifiers, false );
	vtableType->tq.is_const = true;
	type->returnVals.push_back( new ObjectDecl(
		"_retvalue",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new ReferenceType( noQualifiers, vtableType ),
		nullptr,
        { new Attribute("unused") }
	) );
	type->parameters.push_back( new ObjectDecl(
		"__unused",
		noStorageClasses,
		LinkageSpec::Cforall,
		nullptr,
		new PointerType( noQualifiers, exceptType ),
		nullptr,
		{ new Attribute("unused") }
	) );
	return new FunctionDecl(
		functionName,
		noStorageClasses,
		LinkageSpec::Cforall,
		type,
		nullptr
	);
}

FunctionDecl * makeGetExceptionFunction(
		ObjectDecl * vtableInstance, Type * exceptType ) {
	assert( vtableInstance );
	assert( exceptType );
	FunctionDecl * func = makeGetExceptionForward(
		vtableInstance->type->clone(), exceptType );
	func->statements = new CompoundStmt( {
		new ReturnStmt( new VariableExpr( vtableInstance ) ),
	} );
	return func;
}

ObjectDecl * makeTypeIdInstance( StructInstType const * typeIdType ) {
	assert( typeIdType );
	StructInstType * type = typeIdType->clone();
	type->tq.is_const = true;
	std::string const & typeid_name = typeIdTypeToInstance( typeIdType->name );
	return new ObjectDecl(
		typeid_name,
		noStorageClasses,
		LinkageSpec::Cforall,
		/* bitfieldWidth */ nullptr,
		type,
		new ListInit( { new SingleInit(
			new AddressExpr( new NameExpr( "__cfatid_exception_t" ) )
			) } ),
		{ new Attribute( "cfa_linkonce", {} ) },
		noFuncSpecifiers
	);
}

}
