//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// VerifyCtorDtorAssign.cpp -- Check the form of operators.
//
// Author           : Andrew Beach
// Created On       : Mon Jul  4 10:26:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jul 12 11:26:00 2022
// Update Count     : 0
//

#include "VerifyCtorDtorAssign.hpp"

#include "AST/Pass.hpp"
#include "CodeGen/OperatorTable.hpp"

namespace Validate {

namespace {

struct VerifyCore {
	void previsit( ast::FunctionDecl const * decl );
};

void VerifyCore::previsit( ast::FunctionDecl const * decl ) {
	// Skip any of the functions we are not checking.
	// Should get contructors, destructors and all forms of assignment.
	if ( !CodeGen::isCtorDtorAssign( decl->name ) ) {
		return;
	}

	if ( 0 == decl->params.size() ) {
		SemanticError( decl->location, "Constructors, destructors, and assignment functions require at least one parameter." );
	}
	auto refType = decl->type->params.front().as<ast::ReferenceType>();
	if ( !refType ) {
		SemanticError( decl->location, "First parameter of a constructor, destructor, or assignment function must be a reference." );
	}
	if ( CodeGen::isCtorDtor( decl->name ) && 0 != decl->returns.size()
			&& !decl->returns.front()->get_type()->isVoid() ) {
		SemanticError( decl->location, "Constructors and destructors cannot have explicit return values." );
	}
}

} // namespace

void verifyCtorDtorAssign( ast::TranslationUnit & translationUnit ) {
	ast::Pass<VerifyCore>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
