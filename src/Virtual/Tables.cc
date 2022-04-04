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
// Last Modified On : Fri Mar 11 10:40:00 2022
// Update Count     : 3
//

#include "AST/Attribute.hpp"
#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Init.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"
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

static ast::ObjectDecl * makeVtableDeclaration(
		CodeLocation const & location, std::string const & name,
		ast::StructInstType const * type, ast::Init const * init ) {
	ast::Storage::Classes storage;
	if ( nullptr == init ) {
		storage.is_extern = true;
	}
	return new ast::ObjectDecl(
		location,
		name,
		type,
		init,
		storage,
		ast::Linkage::Cforall
	);
}

ObjectDecl * makeVtableForward( std::string const & name, StructInstType * type ) {
	assert( type );
	return makeVtableDeclaration( name, type, nullptr );
}

ast::ObjectDecl * makeVtableForward(
		CodeLocation const & location, std::string const & name,
		ast::StructInstType const * vtableType ) {
	assert( vtableType );
	return makeVtableDeclaration( location, name, vtableType, nullptr );
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

static std::vector<ast::ptr<ast::Init>> buildInits(
		CodeLocation const & location,
		//std::string const & name,
		ast::StructInstType const * vtableType,
		ast::Type const * objectType ) {
	ast::StructDecl const * vtableStruct = vtableType->base;

	std::vector<ast::ptr<ast::Init>> inits;
	inits.reserve( vtableStruct->members.size() );

	// This is designed to run before the resolver.
	for ( auto field : vtableStruct->members ) {
		if ( std::string( "parent" ) == field->name ) {
			// This will not work with polymorphic state.
			auto oField = field.strict_as<ast::ObjectDecl>();
			auto fieldType = oField->type.strict_as<ast::PointerType>();
			auto parentType = fieldType->base.strict_as<ast::StructInstType>();
			std::string const & parentInstance = instanceName( parentType->name );
			inits.push_back(
					new ast::SingleInit( location, new ast::AddressExpr( new ast::NameExpr( location, parentInstance ) ) ) );
		} else if ( std::string( "__cfavir_typeid" ) == field->name ) {
			std::string const & baseType = baseTypeName( vtableType->name );
			std::string const & typeId = typeIdName( baseType );
			inits.push_back( new ast::SingleInit( location, new ast::AddressExpr( new ast::NameExpr( location, typeId ) ) ) );
		} else if ( std::string( "size" ) == field->name ) {
			inits.push_back( new ast::SingleInit( location, new ast::SizeofExpr( location, objectType )
			) );
		} else if ( std::string( "align" ) == field->name ) {
			inits.push_back( new ast::SingleInit( location,
				new ast::AlignofExpr( location, objectType )
			) );
		} else {
			inits.push_back( new ast::SingleInit( location,
				new ast::NameExpr( location, field->name )
			) );
		}
		//ast::Expr * expr = buildInitExpr(...);
		//inits.push_back( new ast::SingleInit( location, expr ) )
	}

	return inits;
}

ast::ObjectDecl * makeVtableInstance(
		CodeLocation const & location,
		std::string const & name,
		ast::StructInstType const * vtableType,
		ast::Type const * objectType,
		ast::Init const * init ) {
	assert( vtableType );
	assert( objectType );

	// Build the initialization.
	if ( nullptr == init ) {
		init = new ast::ListInit( location,
			buildInits( location, vtableType, objectType ) );

	// The provided init should initialize everything except the parent
	// pointer, the size-of and align-of fields. These should be inserted.
	} else {
		// Except this is not yet supported.
		assert(false);
	}
	return makeVtableDeclaration( location, name, vtableType, init );
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

ast::FunctionDecl * makeGetExceptionForward(
		CodeLocation const & location,
		ast::Type const * vtableType,
		ast::Type const * exceptType ) {
	assert( vtableType );
	assert( exceptType );
	return new ast::FunctionDecl(
		location,
		functionName,
		{ /* forall */ },
		{ new ast::ObjectDecl(
			location,
			"__unused",
			new ast::PointerType( exceptType )
		) },
		{ new ast::ObjectDecl(
			location,
			"_retvalue",
			new ast::ReferenceType( vtableType )
		) },
		nullptr,
		ast::Storage::Classes(),
		ast::Linkage::Cforall,
		{ new ast::Attribute( "unused" ) }
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

ast::FunctionDecl * makeGetExceptionFunction(
		CodeLocation const & location,
		ast::ObjectDecl const * vtableInstance, ast::Type const * exceptType ) {
	assert( vtableInstance );
	assert( exceptType );
	ast::FunctionDecl * func = makeGetExceptionForward(
			location, ast::deepCopy( vtableInstance->type ), exceptType );
	func->stmts = new ast::CompoundStmt( location, {
		new ast::ReturnStmt( location, new ast::VariableExpr( location, vtableInstance ) )
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

ast::ObjectDecl * makeTypeIdInstance(
		CodeLocation const & location,
		ast::StructInstType const * typeIdType ) {
	assert( typeIdType );
	ast::StructInstType * type = ast::mutate( typeIdType );
	type->set_const( true );
	std::string const & typeid_name = typeIdTypeToInstance( typeIdType->name );
	return new ast::ObjectDecl(
		location,
		typeid_name,
		type,
		new ast::ListInit( location, {
			new ast::SingleInit( location,
				new ast::AddressExpr( location,
					new ast::NameExpr( location, "__cfatid_exception_t" ) ) )
		} ),
		ast::Storage::Classes(),
		ast::Linkage::Cforall,
		nullptr,
		{ new ast::Attribute( "cfa_linkonce" ) }
	);
}

}
