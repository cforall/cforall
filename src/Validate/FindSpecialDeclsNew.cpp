//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindSpecialDeclsNew.cpp -- Find special declarations used in the compiler.
//
// Author           : Andrew Beach
// Created On       : Wed Nov 10 13:51:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Wed Nov 10 15:22:00 2021
// Update Count     : 0
//

#include "Validate/FindSpecialDecls.h"

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"

// NOTE: currently, it is assumed that every special declaration occurs at the
// top-level, so function bodies, aggregate bodies, object initializers, etc.
// are not visited. If this assumption changes, e.g., with the introduction
// of namespaces, remove the visit_children assignments.

namespace Validate {

namespace {

struct FindDeclsCore : public ast::WithShortCircuiting {
	ast::TranslationGlobal & global;
	FindDeclsCore( ast::TranslationGlobal & g ) : global( g ) {}

	void previsit( const ast::Decl * decl );
	void previsit( const ast::FunctionDecl * decl );
	void previsit( const ast::StructDecl * decl );
};

void FindDeclsCore::previsit( const ast::Decl * ) {
	visit_children = false;
}

void FindDeclsCore::previsit( const ast::FunctionDecl * decl ) {
	visit_children = false;
	if ( !global.dereference && decl->name == "*?" ) {
		const ast::FunctionType * type = decl->type.get();
		if ( decl->linkage == ast::Linkage::Intrinsic && type->params.size() == 1 ) {
			const ast::PointerType * ptrType = type->params.front().strict_as<ast::PointerType>();
			ast::ptr<ast::Type> baseType = ptrType->base;
			if ( baseType->qualifiers == ast::CV::Qualifiers() ) {
				const ast::TypeInstType * inst = baseType.as<ast::TypeInstType>();
				if ( inst || inst->kind != ast::TypeDecl::Ftype ) {
					global.dereference = decl;
				}
			}
		}
	} else if ( !global.dtorDestroy && decl->name == "__destroy_Destructor" ) {
		global.dtorDestroy = decl;
	}
}

void FindDeclsCore::previsit( const ast::StructDecl * decl ) {
	visit_children = false;
	if ( !global.dtorStruct && decl->name == "__Destructor" ) {
		global.dtorStruct = decl;
	}
}

} // namespace

// Fill the TranslationUnit's dereference, dtorStruct and dtorDestroy fields.
void findGlobalDecls( ast::TranslationUnit & translationUnit ) {
	ast::Pass<FindDeclsCore>::run( translationUnit, translationUnit.global );

	// TODO: conditionally generate 'fake' declarations for missing features,
	// so that translation can proceed in the event that builtins, prelude,
	// etc. are missing.
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
