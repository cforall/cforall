//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixReturnTypes.cpp --
//
// Author           : Andrew Beach
// Created On       : Tue Jun 29 11:06:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jul 12 14:04:00 2022
// Update Count     : 0
//

#include "FixReturnTypes.hpp"

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "CodeGen/CodeGenerator.h"
#include "ResolvExpr/typeops.h"

namespace ast {
    class TranslationUnit;
}

namespace Validate {

namespace {

struct ReturnTypeFixer final {
	ast::FunctionDecl const * postvisit( ast::FunctionDecl const * decl );
	ast::FunctionType const * postvisit( ast::FunctionType const * type );
};

ast::FunctionDecl const * ReturnTypeFixer::postvisit(
		ast::FunctionDecl const * decl ) {
	// TODO: It does not handle return values. This information needs to be
	// saved if resolution is to use it. (But does it currently?)
	if ( 1 < decl->returns.size() ) {
		auto mut = ast::mutate( decl );
		// Generate a single return value which is the tuple of all return values.
		auto resultType = ResolvExpr::extractResultType( mut->type );
		ast::TupleType const * tupleType = resultType.strict_as<ast::TupleType>();
		// Ensure return values are not destructed by explicitly creating
		// an empty ListInit node wherein the ConstructFlag is NoConstruct.
		ast::ObjectDecl * newRet = new ast::ObjectDecl(
			decl->location, "", tupleType,
			new ast::ListInit( decl->location, {}, {}, ast::NoConstruct ),
			ast::Storage::Classes(), ast::Linkage::Cforall );
		mut->returns.clear();
		mut->returns.push_back( newRet );
		decl = mut;
	}

	assertf( decl->returns.size() < 2,
		"Function %s has too many return values: %zu",
		decl->name.c_str(), decl->returns.size() );
	if ( 0 == decl->returns.size() ) {
		return decl;
	}
	// Ensure that all function return values have a name.
	// The function name is used to disambiguate the name (and provide
	// debugging information) from other return values.
	auto mut = ast::mutate( decl );
	ast::ptr<ast::DeclWithType> & ret = mut->returns.front();
	if ( "" == ret->name ) {
		ret.get_and_mutate()->name = "_retval_" + CodeGen::genName( decl );
	}
	ret.get_and_mutate()->attributes.push_back( new ast::Attribute( "unused" ) );
	return mut;
}

ast::FunctionType const * ReturnTypeFixer::postvisit(
		ast::FunctionType const * type ) {
	if ( 1 < type->returns.size() ) {
		auto mut = ast::mutate( type );
		// Generate a single return type which is the tuple of all return types.
		auto resultType = ResolvExpr::extractResultType( mut );
		ast::TupleType const * tupleType = resultType.strict_as<ast::TupleType>();
		mut->returns.clear();
		mut->returns.push_back( tupleType );
		return mut;
	}
	return type;
}

} // namespace

void fixReturnTypes( ast::TranslationUnit & translationUnit ) {
	ast::Pass<ReturnTypeFixer>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
