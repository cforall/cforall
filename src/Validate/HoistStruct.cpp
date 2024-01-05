//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// HoistStruct.cpp -- Flattens nested type declarations.
//
// Author           : Andrew Beach
// Created On       : Thr Apr 21 10:34:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Thr Apr 21 10:34:00 2022
// Update Count     : 0
//

#include "Validate/HoistStruct.hpp"

#include <sstream>

#include "AST/DeclReplacer.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "AST/Vector.hpp"

namespace Validate {

namespace {

bool shouldHoist( ast::Decl const * decl ) {
	return dynamic_cast< ast::StructDecl const * >( decl )
		|| dynamic_cast< ast::UnionDecl const * >( decl )
		|| dynamic_cast< ast::EnumDecl const * >( decl )
		|| dynamic_cast< ast::StaticAssertDecl const * >( decl );
}

/* This pass also does some renaming and internal field alteration, but the
 * complex part is the actual hoisting. Hoisted declarations should always
 * appear before the declaration they are hoisted out of and if two types are
 * nested in the same declaration their order should not change.
 */
struct HoistStructCore final :
		public ast::WithDeclsToAdd<>, public ast::WithGuards {
	ast::StructDecl const * previsit( ast::StructDecl const * decl );
	ast::StructDecl const * postvisit( ast::StructDecl const * decl );
	ast::UnionDecl const * previsit( ast::UnionDecl const * decl );
	ast::UnionDecl const * postvisit( ast::UnionDecl const * decl );
	ast::StructInstType const * previsit( ast::StructInstType const * type );
	ast::UnionInstType const * previsit( ast::UnionInstType const * type );
	ast::EnumInstType const * previsit( ast::EnumInstType const * type );

private:
	template<typename AggrDecl>
	AggrDecl const * preAggregate( AggrDecl const * );
	template<typename AggrDecl>
	AggrDecl const * postAggregate( AggrDecl const * );
	template<typename InstType>
	InstType const * preCollectionInstType( InstType const * type );

	ast::AggregateDecl const * parent = nullptr;
};

void qualifiedName( ast::AggregateDecl const * decl, std::ostringstream & ss ) {
	if ( decl->parent ) {
		qualifiedName( decl->parent, ss );
	}
	ss << "__" << decl->name;
}

std::string qualifiedName( ast::AggregateDecl const * decl ) {
	std::ostringstream ss;
	qualifiedName( decl, ss );
	return ss.str();
}

void extendParams( ast::vector<ast::TypeDecl> & dstParams,
		ast::vector<ast::TypeDecl> const & srcParams ) {
	if ( srcParams.empty() ) return;

	ast::DeclReplacer::TypeMap newToOld;
	ast::vector<ast::TypeDecl> params;
	for ( ast::ptr<ast::TypeDecl> const & srcParam : srcParams ) {
		ast::TypeDecl * dstParam = ast::deepCopy( srcParam.get() );
		dstParam->init = nullptr;
		newToOld.emplace( srcParam, dstParam );
		for ( auto assertion : dstParam->assertions ) {
			assertion = ast::DeclReplacer::replace( assertion, newToOld );
		}
		params.emplace_back( dstParam );
	}
	spliceBegin( dstParams, params );
}

template<typename AggrDecl>
AggrDecl const * HoistStructCore::preAggregate( AggrDecl const * decl ) {
	if ( parent ) {
		auto mut = ast::mutate( decl );
		mut->parent = parent;
		mut->name = qualifiedName( mut );
		extendParams( mut->params, parent->params );
		decl = mut;
	}
	GuardValue( parent ) = decl;
	return decl;
}

template<typename AggrDecl>
AggrDecl const * HoistStructCore::postAggregate( AggrDecl const * decl ) {
	auto mut = ast::mutate( decl );
	for ( auto it = mut->members.begin() ; it != mut->members.end() ; ) {
		if ( shouldHoist( *it ) ) {
			// This is the place where the actual hoisting happens.
			declsToAddBefore.push_back( it->get() );
			it = mut->members.erase( it );
		} else {
			++it;
		}
	}
	return mut;
}

ast::StructDecl const * HoistStructCore::previsit( ast::StructDecl const * decl ) {
	return preAggregate( decl );
}

ast::StructDecl const * HoistStructCore::postvisit( ast::StructDecl const * decl ) {
	return postAggregate( decl );
}

ast::UnionDecl const * HoistStructCore::previsit( ast::UnionDecl const * decl ) {
	return preAggregate( decl );
}

ast::UnionDecl const * HoistStructCore::postvisit( ast::UnionDecl const * decl ) {
	return postAggregate( decl );
}

ast::AggregateDecl const * commonParent(
		ast::AggregateDecl const * lhs, ast::AggregateDecl const * rhs ) {
	for ( auto outer = lhs ; outer ; outer = outer->parent ) {
		for ( auto inner = rhs ; inner ; inner = inner->parent ) {
			if ( outer == inner ) {
				return outer;
			}
		}
	}
	return nullptr;
}

template<typename InstType>
InstType const * HoistStructCore::preCollectionInstType( InstType const * type ) {
    if ( !type->base->parent ) return type;
    if ( type->base->params.empty() ) return type;

    InstType * mut = ast::mutate( type );
    ast::AggregateDecl const * parent =
        commonParent( this->parent, mut->base->parent );
    assert( parent );

    std::vector<ast::ptr<ast::Expr>> args;
    for ( const ast::ptr<ast::TypeDecl> & param : parent->params ) {
        args.emplace_back( new ast::TypeExpr( param->location,
            new ast::TypeInstType( param )
        ) );
    }
    spliceBegin( mut->params, args );
    return mut;
}

template<typename InstType>
InstType const * preInstType( InstType const * type ) {
	assert( type->base );
	auto mut = ast::mutate( type );
	mut->name = mut->base->name;
	return mut;
}

ast::StructInstType const * HoistStructCore::previsit( ast::StructInstType const * type ) {
	return preInstType( preCollectionInstType( type ) );
}

ast::UnionInstType const * HoistStructCore::previsit( ast::UnionInstType const * type ) {
	return preInstType( preCollectionInstType( type ) );
}

ast::EnumInstType const * HoistStructCore::previsit( ast::EnumInstType const * type ) {
	return preInstType( type );
}

} // namespace

void hoistStruct( ast::TranslationUnit & translationUnit ) {
	ast::Pass<HoistStructCore>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
