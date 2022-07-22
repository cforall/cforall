//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixQualifiedTypes.cpp -- Replace the qualified type with a direct type.
//
// Author           : Andrew Beach
// Created On       : Thr Apr 21 11:13:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri Apr 22 11:36:00 2022
// Update Count     : 0
//

#include "Validate/FixQualifiedTypes.hpp"

#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "Validate/NoIdSymbolTable.hpp"

namespace Validate {

namespace {

struct FixQualifiedTypesCore :
		public WithNoIdSymbolTable, public ast::WithGuards {
	CodeLocation const * location = nullptr;

	void previsit( ast::ParseNode const * node ) {
		GuardValue( location ) = &node->location;
	}

	ast::Type const * postvisit( ast::QualifiedType const * type ) {
		assert( location );

		ast::ptr<ast::Type> const & parent = type->parent;
		ast::ptr<ast::Type> const & child = type->child;
		if ( parent.as<ast::GlobalScopeType>() ) {
			// .T => lookup T at global scope.
			if ( auto inst = child.as<ast::TypeInstType>() ) {
				auto td = symtab.globalLookupType( inst->name );
				if ( !td ) {
					SemanticError( *location, toString("Use of undefined global type ", inst->name) );
				}
				auto base = td->base;
				assert( base );
				ast::Type * ret = ast::deepCopy( base );
				ret->qualifiers = type->qualifiers;
				return ret;
			} else {
				// .T => T is not a type name.
				assertf( false, "unhandled global qualified child type: %s", toCString(child) );
			}
		} else {
			// S.T => S must be an aggregate type, find the declaration for T in S.
			ast::AggregateDecl const * aggr = nullptr;
			ast::BaseInstType const * instp = nullptr;
			if ( auto inst = parent.as<ast::StructInstType>() ) {
				aggr = inst->base;
				instp = inst;
			} else if ( auto inst = parent.as<ast::UnionInstType>() ) {
				aggr = inst->base;
				instp = inst;
			} else {
				SemanticError( *location, toString("Qualified type requires an aggregate on the left, but has: ", parent) );
			}
			// TODO: Need to handle forward declarations.
			assert( aggr );
			for ( ast::ptr<ast::Decl> const & member : aggr->members ) {
				if ( auto inst = child.as<ast::TypeInstType>() ) {
					if ( auto decl = member.as<ast::NamedTypeDecl>() ) {
						if ( decl->name == inst->name ) {
							assert( decl->base );
							ast::Type * ret = ast::deepCopy( decl->base );
							ret->qualifiers = type->qualifiers;
							ast::TypeSubstitution sub( aggr->params, instp->params );
							auto result = sub.apply(ret);
							return result.node.release();
						}
					}
				} else {
					// S.T - S is not an aggregate => error.
					assertf( false, "unhandled qualified child type: %s", toCString(type) );
				}
			}
			// failed to find a satisfying definition of type
			SemanticError( *location, toString("Undefined type in qualified type: ", type) );
		}
	}
};

} // namespace

void fixQualifiedTypes( ast::TranslationUnit & translationUnit ) {
	ast::Pass<FixQualifiedTypesCore>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
