//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ForallPointerDecay.cpp --
//
// Author           : Andrew Beach
// Created On       : Tue Dec  7 16:15:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Fri Feb 11 10:59:00 2022
// Update Count     : 0
//

#include "ForallPointerDecay.hpp"

#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/DeclReplacer.hpp"
#include "AST/Pass.hpp"
#include "CodeGen/OperatorTable.h"
#include "Common/CodeLocation.h"
#include "SymTab/FixFunction.h"

#include "AST/Print.hpp"

namespace Validate {

namespace {

// Create a function type only using information on the FunctionDecl.
ast::FunctionType * makeFuncType( const ast::FunctionDecl * decl ) {
	auto type = new ast::FunctionType( decl->type->isVarArgs );
	for ( auto & param : decl->params ) {
		type->params.emplace_back( param->get_type() );
	}
	for ( auto & ret : decl->returns ) {
		type->returns.emplace_back( ret->get_type() );
	}
	for ( auto & type_param : decl->type_params ) {
		type->forall.emplace_back(
			new ast::TypeInstType( type_param->name, type_param ) );
	}
	for ( auto & assertion : decl->assertions ) {
		type->assertions.emplace_back( new ast::VariableExpr(
			assertion->location, assertion ) );
	}
	return type;
}

template<typename T>
void append( std::vector<T> & dst, std::vector<T> & src ) {
	dst.reserve( dst.size() + src.size() );
	for ( auto el : src ) {
		dst.emplace_back( std::move( el ) );
	}
	src.clear();
}

// Component Passes:
/// Expand assertions from a trait instance,
/// performing appropriate type variable substitutions.
struct TraitExpander final {
	using AssertionList = std::vector<ast::ptr<ast::DeclWithType>>;

	static AssertionList expandTrait( const ast::TraitInstType * inst ) {
		assertf( inst->base, "Trait instance not linked to base trait: %s",
			toCString( inst ) );
		AssertionList assertions;
		// Substitute trait decl parameters for instance parameters.
		ast::TypeSubstitution sub(
			inst->base->params.begin(),
			inst->base->params.end(),
			inst->params.begin()
		);
		for ( const ast::ptr<ast::Decl> & decl : inst->base->members ) {
			ast::ptr<ast::DeclWithType> copy =
				ast::deepCopy( decl.strict_as<ast::DeclWithType>() );

			int count = sub.apply( copy );
			(void)count;

			// Update the type (type substution does not seem to cover it).
			if ( auto func = copy.as<ast::FunctionDecl>() ) {
				auto mut = ast::mutate( func );
				mut->type = makeFuncType( func );
				copy = mut;
			}
			assertions.push_back( copy );
		}
		return assertions;
	}

	static AssertionList expandAssertions( const AssertionList & old ) {
		AssertionList assertions;
		for ( const ast::ptr<ast::DeclWithType> & decl : old ) {
			if ( auto traitInst = dynamic_cast<const ast::TraitInstType *>(
					decl->get_type() ) ) {
				auto moreAsserts = expandTrait( traitInst );
				append( assertions, moreAsserts );
			} else {
				assertions.push_back( decl );
			}
		}
		return assertions;
	}

	using TypeDeclVec = std::vector<ast::ptr<ast::TypeDecl>>;

	static TypeDeclVec expandTypeDecls( const TypeDeclVec & old ) {
		TypeDeclVec typeDecls;
		for ( const ast::TypeDecl * typeDecl : old ) {
			typeDecls.push_back( ast::mutate_field( typeDecl,
				&ast::TypeDecl::assertions,
				expandAssertions( typeDecl->assertions ) ) );
		}
		return typeDecls;
	}

	const ast::FunctionDecl * postvisit( const ast::FunctionDecl * decl ) {
		if ( decl->assertions.empty() ) {
			return decl;
		}
		auto mut = ast::mutate( decl );
		mut->assertions = expandAssertions( decl->assertions );
		// Update the assertion list on the type as well.
		auto mutType = ast::mutate( mut->type.get() );
		mutType->assertions.clear();
		for ( auto & assertion : mut->assertions ) {
			mutType->assertions.emplace_back(
				new ast::VariableExpr( mut->location, assertion ) );
		}
		mut->type = mutType;
		return mut;
	}

	const ast::StructDecl * previsit( const ast::StructDecl * decl ) {
		if ( decl->params.empty() ) {
			return decl;
		}
		return ast::mutate_field( decl, &ast::StructDecl::params,
			expandTypeDecls( decl->params ) );
	}

	const ast::UnionDecl * previsit( const ast::UnionDecl * decl ) {
		if ( decl->params.empty() ) {
			return decl;
		}
		return ast::mutate_field( decl, &ast::UnionDecl::params,
			expandTypeDecls( decl->params ) );
	}
};

std::vector<ast::ptr<ast::DeclWithType>> fixAssertionList(
		const ast::ParseNode * node,
		const std::vector<ast::ptr<ast::DeclWithType>> & assertions ) {
	std::vector<ast::ptr<ast::DeclWithType>> ret;
	for ( const auto & assn : assertions ) {
		bool isVoid = false;
		ret.push_back( SymTab::fixFunction( assn, isVoid ) );
		if ( isVoid ) {
			SemanticError( node->location, node,
				"invalid type void in assertion of function " );
		}
	}
	return ret;
}

std::vector<ast::ptr<ast::TypeDecl>> fixTypeDeclList(
		const ast::ParseNode * node,
		const std::vector<ast::ptr<ast::TypeDecl>> & type_params ) {
	std::vector<ast::ptr<ast::TypeDecl>> ret;
	ret.reserve( type_params.size() );
	for ( const ast::TypeDecl * type_param : type_params ) {
		auto mutParam = ast::mutate( type_param );
		mutParam->assertions = fixAssertionList( node, mutParam->assertions );
		ret.push_back( mutParam );
	}
	return ret;
}

struct AssertionFunctionFixer final {
	const ast::FunctionDecl * previsit( const ast::FunctionDecl * decl ) {
		if ( decl->assertions.empty() ) {
			return decl;
		}
		return ast::mutate_field( decl, &ast::FunctionDecl::assertions,
			fixAssertionList( decl, decl->assertions ) );
	}

	const ast::StructDecl * previsit( const ast::StructDecl * decl ) {
		if ( decl->params.empty() ) {
			return decl;
		}
		return ast::mutate_field( decl, &ast::StructDecl::params,
			fixTypeDeclList( decl, decl->params ) );
	}

	const ast::UnionDecl * previsit( const ast::UnionDecl * decl ) {
		if ( decl->params.empty() ) {
			return decl;
		}
		return ast::mutate_field( decl, &ast::UnionDecl::params,
			fixTypeDeclList( decl, decl->params ) );
	}
};

struct OberatorChecker final {
	void previsit( const ast::ObjectDecl * obj ) {
		if ( CodeGen::isOperator( obj->name ) ) {
			auto type = obj->type->stripDeclarator();
			if ( ! dynamic_cast< const ast::FunctionType * >( type ) ) {
				SemanticError( obj->location,
					toCString( "operator ", obj->name.c_str(), " is not "
					"a function or function pointer." ) );
			}
		}
	}
};

struct UniqueFixCore final {
	const ast::DeclWithType * postvisit( const ast::DeclWithType * decl ) {
		if ( decl->uniqueId ) {
			return decl;
		} else {
			auto mut = ast::mutate( decl );
			mut->fixUniqueId();
			return mut;
		}
	}
};

} // namespace

void decayForallPointers( ast::TranslationUnit & transUnit ) {
	ast::Pass<TraitExpander>::run( transUnit );
	ast::Pass<AssertionFunctionFixer>::run( transUnit );
	ast::Pass<OberatorChecker>::run( transUnit );
	ast::Pass<UniqueFixCore>::run( transUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
