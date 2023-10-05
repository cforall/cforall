//
// Cforall Version 1.0.0 Copyright (C) 2021 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LinkOnce.cc -- Translate the cfa_linkonce attribute.
//
// Author           : Andrew Beach
// Created On       : Thur May 13 10:10:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Wed Oct  4 10:52:00 2023
// Update Count     : 1
//

#include "LinkOnce.h"

#include <algorithm>

#include "AST/Attribute.hpp"
#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "Common/PassVisitor.h"       // for PassVisitor, WithShortCircuiting

namespace CodeGen {

namespace {

bool is_cfa_linkonce_old( Attribute const * attr ) {
	return std::string("cfa_linkonce") == attr->name;
}

bool is_section_attribute_old( Attribute const * attr ) {
	return std::string("section") == attr->name;
}

class LinkOnceVisitorCore : public WithShortCircuiting {
public:
	void previsit( Declaration * ) {
		visit_children = false;
	}

	void previsit( DeclarationWithType * decl ) {
		std::list< Attribute * > & attributes = decl->attributes;
		// See if we can find the element:
		auto found = std::find_if(attributes.begin(), attributes.end(), is_cfa_linkonce_old );
		if ( attributes.end() != found ) {
			// Remove any other sections:
			attributes.remove_if( is_section_attribute_old );
			// Iterator to the cfa_linkonce attribute should still be valid.
			Attribute * attribute = *found;
			assert( attribute->parameters.empty() );
			assert( !decl->mangleName.empty() );
			// Overwrite the attribute in place.
			const std::string section_name = ".gnu.linkonce." + decl->mangleName;
			attribute->name = "section";
			attribute->parameters.push_back(
				new ConstantExpr( Constant::from_string( section_name ) )
			);

			// Unconditionnaly add "visibility(default)" to anything with gnu.linkonce
			// visibility is a mess otherwise
			attributes.push_back(new Attribute("visibility", {new ConstantExpr( Constant::from_string( "default" ) )}));

		}
		visit_children = false;
	}
};

bool is_cfa_linkonce( ast::Attribute const * attr ) {
	return "cfa_linkonce" == attr->name;
}

bool is_section_attribute( ast::Attribute const * attr ) {
	return "section" == attr->name;
}

struct LinkOnceCore : public ast::WithShortCircuiting {
	void previsit( ast::Decl const * ) {
		visit_children = false;
	}

	ast::DeclWithType const * postvisit( ast::DeclWithType const * decl ) {
		// Check to see if we have to mutate, because should be uncommon.
		{
			auto & attributes = decl->attributes;
			auto found = std::find_if( attributes.begin(), attributes.end(),
					is_cfa_linkonce );
			if ( attributes.end() == found ) return decl;
		}
		auto mutDecl = mutate( decl );
		auto & attributes = mutDecl->attributes;

		// Remove all conflicting section attributes.
		erase_if( attributes, is_section_attribute );

		// Get the attribute, and overwrite it as a section attribute.
		auto found = std::find_if( attributes.begin(), attributes.end(),
				is_cfa_linkonce );
		assert( attributes.end() != found );
		ast::Attribute * attribute = found->get_and_mutate();
		assert( attribute->params.empty() );
		assert( !decl->mangleName.empty() );

		attribute->name = "section";
		attribute->params.push_back(
			ast::ConstantExpr::from_string( mutDecl->location,
				".gnu.linkonce." + decl->mangleName
			)
		);

		// Unconditionnaly add "visibility(default)" to anything with
		// .gnu.linkonce visibility is a mess otherwise.
		attributes.push_back( new ast::Attribute( "visibility", {
			ast::ConstantExpr::from_string( mutDecl->location, "default" )
		} ) );
		return mutDecl;
	}
};

} // namespace

void translateLinkOnce( std::list< Declaration *> & translationUnit ) {
	PassVisitor<LinkOnceVisitorCore> translator;
	acceptAll( translationUnit, translator );
}

void translateLinkOnce( ast::TranslationUnit & translationUnit ) {
	ast::Pass<LinkOnceCore>::run( translationUnit );
}

} // namespace CodeGen
