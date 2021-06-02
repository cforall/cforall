//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Examine.h --
//
// Author           : Andrew Beach
// Created On       : Wed Sept 2 14:02 2020
// Last Modified By : Andrew Beach
// Last Modified On : Wed Sep  8 12:15 2020
// Update Count     : 0
//

#include "Common/Examine.h"

#include "CodeGen/OperatorTable.h"

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
}

bool isDestructorFor( FunctionDecl * func, StructDecl * type_decl ) {
	if ( Type * type = getDestructorParam( func ) ) {
		auto stype = dynamic_cast<StructInstType *>( type );
		return stype && stype->baseStruct == type_decl;
	}
	return false;
}
