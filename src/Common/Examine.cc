//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Examine.cc -- Helpers for examining AST code.
//
// Author           : Andrew Beach
// Created On       : Wed Sept 2 14:02 2020
// Last Modified By : Andrew Beach
// Last Modified On : Fri Dec 10 10:27 2021
// Update Count     : 1
//

#include "Common/Examine.h"

#include "AST/Type.hpp"
#include "CodeGen/OperatorTable.h"
#include "InitTweak/InitTweak.h"

DeclarationWithType * isMainFor( FunctionDecl * func, AggregateDecl::Aggregate kind ) {
	if (func->name != "main") return nullptr;
	if (func->type->parameters.size() != 1) return nullptr;

	auto param = func->type->parameters.front();

	auto type = dynamic_cast<ReferenceType * >(param->get_type());
	if (!type) return nullptr;

	auto obj = dynamic_cast<StructInstType *>(type->base);
	if (!obj) return nullptr;

	if (kind != obj->baseStruct->kind) return nullptr;

	return param;
}

namespace {

// getTypeofThis but does some extra checks used in this module.
const ast::Type * getTypeofThisSolo( const ast::FunctionDecl * func ) {
	if ( 1 != func->params.size() ) {
		return nullptr;
	}
	auto ref = func->type->params.front().as<ast::ReferenceType>();
	return (ref) ? ref->base : nullptr;
}

}

const ast::DeclWithType * isMainFor(
		const ast::FunctionDecl * func, ast::AggregateDecl::Aggregate kind ) {
	if ( "main" != func->name ) return nullptr;
	if ( 1 != func->params.size() ) return nullptr;

	auto param = func->params.front();

	auto type = dynamic_cast<const ast::ReferenceType *>( param->get_type() );
	if ( !type ) return nullptr;

	auto obj = type->base.as<ast::StructInstType>();
	if ( !obj ) return nullptr;

	if ( kind != obj->base->kind ) return nullptr;

	return param;
}

namespace {
	Type * getDestructorParam( FunctionDecl * func ) {
		if ( !CodeGen::isDestructor( func->name ) ) return nullptr;

		auto params = func->type->parameters;
		if ( 1 != params.size() ) return nullptr;

		auto ref = dynamic_cast<ReferenceType *>( params.front()->get_type() );
		if ( ref ) {
			return ref->base;
		}
		return nullptr;
	}

const ast::Type * getDestructorParam( const ast::FunctionDecl * func ) {
	if ( !CodeGen::isDestructor( func->name ) ) return nullptr;
	//return InitTweak::getParamThis( func )->type;
	return getTypeofThisSolo( func );
}

}

bool isDestructorFor( FunctionDecl * func, StructDecl * type_decl ) {
	if ( Type * type = getDestructorParam( func ) ) {
		auto stype = dynamic_cast<StructInstType *>( type );
		return stype && stype->baseStruct == type_decl;
	}
	return false;
}

bool isDestructorFor(
		const ast::FunctionDecl * func, const ast::StructDecl * type_decl ) {
	if ( const ast::Type * type = getDestructorParam( func ) ) {
		auto stype = dynamic_cast<const ast::StructInstType *>( type );
		return stype && stype->base.get() == type_decl;
	}
	return false;
}
