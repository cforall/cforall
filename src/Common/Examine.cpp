//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Examine.cpp -- Helpers for examining AST code.
//
// Author           : Andrew Beach
// Created On       : Wed Sept 2 14:02 2020
// Last Modified By : Andrew Beach
// Last Modified On : Fri Dec 10 10:27 2021
// Update Count     : 1
//

#include "Common/Examine.hpp"

#include "AST/Type.hpp"
#include "CodeGen/OperatorTable.hpp"
#include "InitTweak/InitTweak.hpp"

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

const ast::Type * getDestructorParam( const ast::FunctionDecl * func ) {
	if ( !CodeGen::isDestructor( func->name ) ) return nullptr;
	return getTypeofThisSolo( func );
}

}

bool isDestructorFor(
		const ast::FunctionDecl * func, const ast::StructDecl * type_decl ) {
	if ( const ast::Type * type = getDestructorParam( func ) ) {
		auto stype = dynamic_cast<const ast::StructInstType *>( type );
		return stype && stype->base.get() == type_decl;
	}
	return false;
}
