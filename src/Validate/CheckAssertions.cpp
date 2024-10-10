//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CheckAssertions.cpp --
//
// Author           : Andrew Beach
// Created On       : Thu Sep  5 14:43:00 2024
// Last Modified By : Andrew Beach
// Last Modified On : Thu Sep  5 14:43:00 2024
// Update Count     : 0
//

#include "CheckAssertions.hpp"

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "Common/SemanticError.hpp"
#include "GenPoly/GenPoly.hpp"

namespace Validate {

namespace {

/// Check for dynamic (sized polymorphic) variadic assertions.
struct VariadicAssertionCore final {
	void checkAssertion( GenPoly::TypeVarMap const & typeVars,
			ast::FunctionDecl const * assert ) {
		if ( ast::FixedArgs != assert->type->isVarArgs
				&& needsAdapter( assert->type, typeVars ) ) {
			SemanticError( assert,
				"Cannot assert a function that both has by-value polymorphic "
				"parameters or return values, and also takes variadic (...) "
				"parameters. Consider using a va_list parameter instead.\n" );
		}
	}

	template<typename T>
	void checkList(
			GenPoly::TypeVarMap const & typeVars,
			SemanticErrorException & errors,
			std::vector<ast::ptr<T>> const & members ) {
		for ( const ast::ptr<T> & member : members ) {
			try {
				if ( auto func = member.template as<ast::FunctionDecl>() ) {
					checkAssertion( typeVars, func );
				}
			} catch ( SemanticErrorException & error ) {
				errors.append( error );
			}
		}
	}

	void postvisit( ast::FunctionDecl const * decl ) {
		GenPoly::TypeVarMap typeVars;
		SemanticErrorException errors;
		makeTypeVarMap( decl, typeVars );
		checkList( typeVars, errors, decl->assertions );
		errors.throwIfNonEmpty();
	}

	void checkAggr( bool checkMembers,
			ast::AggregateDecl const * decl ) {
		SemanticErrorException errors;
		GenPoly::TypeVarMap typeVars;
		for ( auto & param : decl->params ) {
			addToTypeVarMap( param, typeVars );
			checkList( typeVars, errors, param->assertions );
		}
		if ( checkMembers ) checkList( typeVars, errors, decl->members );
		errors.throwIfNonEmpty();
	}

	void postvisit( ast::StructDecl const * decl ) {
	checkAggr( false, decl );
	}

	void postvisit( ast::UnionDecl const * decl ) {
		checkAggr( false, decl );
	}

	void postvisit( ast::TraitDecl const * decl ) {
		checkAggr( true, decl );
	}
};

} // namespace

void checkAssertions( ast::TranslationUnit & transUnit ) {
	ast::Pass<VariadicAssertionCore>::run( transUnit );
}

} // namespace Validate
