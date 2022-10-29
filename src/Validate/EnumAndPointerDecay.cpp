//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// EnumAndPointerDecay.cpp -- Normalizes enumerations and types in functions.
//
// Author           : Andrew Beach
// Created On       : Tue Jun 28 15:50:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Sep 20 16:14:00 2022
// Update Count     : 1
//

#include "EnumAndPointerDecay.hpp"

#include "AST/CVQualifiers.hpp"
#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "SymTab/FixFunction.h"
#include "Validate/NoIdSymbolTable.hpp"

namespace Validate {

namespace {

struct EnumAndPointerDecayCore final : public WithNoIdSymbolTable, public ast::WithCodeLocation {
	ast::EnumDecl const * previsit( ast::EnumDecl const * decl );
	ast::FunctionDecl const * previsit( ast::FunctionDecl const * decl );
	ast::FunctionType const * previsit( ast::FunctionType const * type );
};

ast::EnumDecl const * EnumAndPointerDecayCore::previsit(
		ast::EnumDecl const * decl ) {
	if ( decl->members.empty() ) {
		return decl;
	}
	// Set the type of each member of the enumeration to be EnumContant.
	auto mut = ast::mutate( decl );
	std::vector<ast::ptr<ast::Decl>> buffer;
	for ( auto it = decl->members.begin(); it != decl->members.end(); ++it ) {
		if ( ast::ObjectDecl const * object = (*it).as<ast::ObjectDecl>() ) {
			buffer.push_back( ast::mutate_field( object, &ast::ObjectDecl::type, new ast::EnumInstType( decl, ast::CV::Const ) ) );
		} else if ( ast::InlineValueDecl const * value = (*it).as<ast::InlineValueDecl>() ) {
			if ( auto targetEnum = symtab.lookupEnum( value->name ) ) {
				for ( auto singleMember : targetEnum->members ) {
					auto copyingMember = singleMember.as<ast::ObjectDecl>();
					buffer.push_back( new ast::ObjectDecl(
						value->location, // use the "inline" location
						copyingMember->name,
						new ast::EnumInstType( decl, ast::CV::Const ),
						// Construct a new EnumInstType as the type
						copyingMember->init,
						copyingMember->storage,
						copyingMember->linkage,
						copyingMember->bitfieldWidth,
						{},
						copyingMember->funcSpec
					) );
				}
			}
		}
	}
	mut->members = buffer;
	return mut;
}

template<typename Member>
void fixFunctionList( CodeLocation const & location, bool isVarArgs,
		std::vector<ast::ptr<Member>> & list ) {
	bool hasVoid = false;
	for ( ast::ptr<Member> & member : list ) {
		member = SymTab::fixFunction( member, hasVoid );
	}

	// The remaining code only applies if void is present.
	if ( !hasVoid ) {
		return;
	}

	// So there is a void, which is only valid if it is the only argument.
	if ( 1 < list.size() || isVarArgs ) {
		SemanticError( location, "invalid type void in function type " );
	}

	// If a single "void" thing in the list to remove it.
	list.clear();
}

ast::FunctionDecl const * EnumAndPointerDecayCore::previsit(
		ast::FunctionDecl const * decl ) {
	auto mut = ast::mutate( decl );
	ast::ArgumentFlag isVarArgs = mut->type->isVarArgs;
	// It seems fixFunction (via fixFunctionList) does the pointer decay part.
	fixFunctionList( mut->location, isVarArgs, mut->params );
	fixFunctionList( mut->location, false, mut->returns );
	return mut;
}

ast::FunctionType const * EnumAndPointerDecayCore::previsit(
		ast::FunctionType const * type ) {
	assert( location );
	auto mut = ast::mutate( type );
	ast::ArgumentFlag isVarArgs = mut->isVarArgs;
	// It seems fixFunction (via fixFunctionList) does the pointer decay part.
	fixFunctionList( *location, isVarArgs, mut->params );
	fixFunctionList( *location, false, mut->returns );
	return mut;
}

} // namespace

void decayEnumsAndPointers( ast::TranslationUnit & translationUnit ) {
	ast::Pass<EnumAndPointerDecayCore>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
