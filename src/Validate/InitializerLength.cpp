//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// InitializerLength.cpp -- Calculate the length of arrays from initializers.
//
// Author           : Andrew Beach
// Created On       : Fri Nov 12 11:46:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Fri Nov 12 13:35:00 2021
// Update Count     : 0
//

//#include "InitializerLength.hpp"

#include "AST/Expr.hpp"
#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"

namespace Validate {

namespace {

/// for array types without an explicit length, compute the length and store it so that it
/// is known to the rest of the phases. For example,
///   int x[] = { 1, 2, 3 };
///   int y[][2] = { { 1, 2, 3 }, { 1, 2, 3 } };
/// here x and y are known at compile-time to have length 3, so change this into
///   int x[3] = { 1, 2, 3 };
///   int y[3][2] = { { 1, 2, 3 }, { 1, 2, 3 } };
struct InitializerLength {
	const ast::ObjectDecl * previsit( const ast::ObjectDecl * decl );
};

const ast::ObjectDecl * InitializerLength::previsit( const ast::ObjectDecl * decl ) {
	if ( auto type = decl->type.as<ast::ArrayType>() ) {
		if ( type->dimension ) return decl;
		if ( auto init = decl->init.as<ast::ListInit>() ) {
			ast::ObjectDecl * mutDecl = ast::mutate( decl );
			ast::ArrayType * mutType = ast::mutate( type );
			mutType->dimension = ast::ConstantExpr::from_ulong(
				mutDecl->location, init->size() );
			mutDecl->type = mutType;
			return mutDecl;
		}
	}
	return decl;
}

} // namespace

void setLengthFromInitializer( ast::TranslationUnit & translationUnit ) {
	ast::Pass<InitializerLength>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
