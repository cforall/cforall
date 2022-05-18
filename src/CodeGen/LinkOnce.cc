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
// Last Modified On : Thur May 13 14:39:00 2021
// Update Count     : 0
//

#include "LinkOnce.h"

#include <algorithm>

#include "Common/PassVisitor.h"       // for PassVisitor, WithShortCircuiting

namespace CodeGen {

static bool is_cfa_linkonce( Attribute const * attr ) {
	return std::string("cfa_linkonce") == attr->name;
}

static bool is_section_attribute( Attribute const * attr ) {
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
		auto found = std::find_if(attributes.begin(), attributes.end(), is_cfa_linkonce );
		if ( attributes.end() != found ) {
			// Remove any other sections:
			attributes.remove_if( is_section_attribute );
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

void translateLinkOnce( std::list< Declaration *> & translationUnit ) {
	PassVisitor<LinkOnceVisitorCore> translator;
	acceptAll( translationUnit, translator );
}

}
