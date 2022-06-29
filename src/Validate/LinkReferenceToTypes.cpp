//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LinkReferenceToTypes.cpp -- Connect instance types to declarations.
//
// Author           : Andrew Beach
// Created On       : Thr Apr 21 11:41:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jun 28 14:58:00 2022
// Update Count     : 1
//

#include "Validate/LinkReferenceToTypes.hpp"

#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "Validate/ForallPointerDecay.hpp"
#include "Validate/NoIdSymbolTable.hpp"

namespace Validate {

namespace {

struct LinkTypesCore : public WithNoIdSymbolTable,
		public ast::WithGuards,
		public ast::WithVisitorRef<LinkTypesCore>,
		public ast::WithShortCircuiting {

	ast::TypeInstType const * postvisit( ast::TypeInstType const * type );
	ast::EnumInstType const * postvisit( ast::EnumInstType const * type );
	ast::StructInstType const * postvisit( ast::StructInstType const * type );
	ast::UnionInstType const * postvisit( ast::UnionInstType const * type );
	ast::TraitInstType const * postvisit( ast::TraitInstType const * type );
	void previsit( ast::QualifiedType const * type );
	void postvisit( ast::QualifiedType const * type );

	void previsit( ast::ParseNode const * node );

	ast::EnumDecl const * postvisit( ast::EnumDecl const * decl );
	ast::StructDecl const * previsit( ast::StructDecl const * decl );
	void postvisit( ast::StructDecl const * decl );
	ast::UnionDecl const * previsit( ast::UnionDecl const * decl );
	void postvisit( ast::UnionDecl const * decl );
	ast::TraitDecl const * postvisit( ast::TraitDecl const * decl );

private:
	using ForwardStructsType =
		std::map< std::string, std::list< ast::StructInstType * > >;
	using ForwardUnionsType =
		std::map< std::string, std::list< ast::UnionInstType * > >;
	using ForwardEnumsType =
		std::map< std::string, std::list< ast::EnumInstType * > >;

	ForwardStructsType forwardStructs;
	ForwardUnionsType forwardUnions;
	ForwardEnumsType forwardEnums;

	const CodeLocation * location = nullptr;
	/// true if currently in a generic type body,
	/// so that type parameter instances can be renamed appropriately
	bool inGeneric = false;

	template<typename AggrDecl>
	AggrDecl const * renameGenericParams( AggrDecl const * decl );
};

ast::TypeInstType const * LinkTypesCore::postvisit( ast::TypeInstType const * type ) {
	auto mut = ast::mutate( type );
	if ( inGeneric && mut->base ) {
		mut->name = mut->base->name;
	}
	if ( auto namedTypeDecl = symtab.lookupType( mut->name ) ) {
		if ( auto typeDecl = dynamic_cast<ast::TypeDecl const *>( namedTypeDecl ) ) {
			mut->kind = typeDecl->kind;
		}
	}
	return mut;
}

ast::EnumInstType const * LinkTypesCore::postvisit( ast::EnumInstType const * type ) {
	ast::EnumDecl const * decl = symtab.lookupEnum( type->name );
	// It's not a semantic error if the enum is not found, just an implicit forward declaration.
	if ( decl ) {
		// Just linking in the node.
		auto mut = ast::mutate( type );
		mut->base = const_cast<ast::EnumDecl *>( decl );
		type = mut;
	}
	if ( !decl || !decl->body ) {
		auto mut = ast::mutate( type );
		forwardEnums[ mut->name ].push_back( mut );
		type = mut;
	}
	return type;
}

ast::StructInstType const * LinkTypesCore::postvisit( ast::StructInstType const * type ) {
	ast::StructDecl const * decl = symtab.lookupStruct( type->name );
	// It's not a semantic error if the struct is not found, just an implicit forward declaration.
	if ( decl ) {
		// Just linking in the node.
		auto mut = ast::mutate( type );
		mut->base = const_cast<ast::StructDecl *>( decl );
		type = mut;
	}
	if ( !decl || !decl->body ) {
		auto mut = ast::mutate( type );
		forwardStructs[ mut->name ].push_back( mut );
		type = mut;
	}
	return type;
}

ast::UnionInstType const * LinkTypesCore::postvisit( ast::UnionInstType const * type ) {
	ast::UnionDecl const * decl = symtab.lookupUnion( type->name );
	// It's not a semantic error if the union is not found, just an implicit forward declaration.
	if ( decl ) {
		// Just linking in the node.
		auto mut = ast::mutate( type );
		mut->base = const_cast<ast::UnionDecl *>( decl );
		type = mut;
	}
	if ( !decl || !decl->body ) {
		auto mut = ast::mutate( type );
		forwardUnions[ mut->name ].push_back( mut );
		type = mut;
	}
	return type;
}

ast::TraitInstType const * LinkTypesCore::postvisit( ast::TraitInstType const * type ) {
	assert( location );

	ast::TraitDecl const * decl = symtab.lookupTrait( type->name );
	if ( !decl ) {
		SemanticError( *location, "use of undeclared trait " + type->name );
	} else if ( decl->params.size() != type->params.size() ) {
		SemanticError( *location, "incorrect number of trait parameters: " );
	}
	auto mut = ast::mutate( type );

	// Just linking in the node.
	mut->base = const_cast<ast::TraitDecl *>( decl );

	// Need to carry over the 'sized' status of each decl in the instance.
	for ( auto p : group_iterate( decl->params, type->params ) ) {
		ast::TypeExpr const * expr = std::get<1>(p).as<ast::TypeExpr>();
		if ( !expr ) {
			SemanticError( std::get<1>(p).get(), "Expression parameters for trait instances are currently unsupported: " );
		}
		if ( auto inst = expr->type.as<ast::TypeInstType>() ) {
			ast::ptr<ast::TypeDecl> const & formalDecl = std::get<0>(p);
			if ( !formalDecl->sized ) {
				continue;
			}
			// To do this modification we need to reach through a readonly
			// pointer. The Pass doesn't quite work in that way, so we just
			// ensure it mutates in-place so it should work out.
			ast::TypeDecl const * base = inst->base.get();
			assert( base->unique() );
			ast::TypeDecl * mutBase = ast::mutate( base );
			mutBase->sized = true;
		}
	}
	return mut;
}

void LinkTypesCore::previsit( ast::QualifiedType const * ) {
	visit_children = false;
}

void LinkTypesCore::postvisit( ast::QualifiedType const * type ) {
	// Linking only makes sense for the 'oldest ancestor' of the qualified type.
	type->parent->accept( *visitor );
}

void LinkTypesCore::previsit( ast::ParseNode const * node ) {
	GuardValue( location ) = &node->location;
}

ast::EnumDecl const * LinkTypesCore::postvisit( ast::EnumDecl const * decl ) {
	// After visiting enum members for self-referencing members,
	// we replace the enum base. Right now it only works for StructDecl.
	if ( decl->base ) {
		if ( auto base = decl->base.as<ast::TypeInstType>() ) {
			if ( auto structDecl = symtab.lookupStruct( base->name ) ) {
				auto mut = ast::mutate( decl );
				mut->base = new ast::StructInstType( structDecl );
				decl = mut;
			}
		} else if ( auto ptr = decl->base.as<ast::PointerType>() ) {
			if ( auto base = ptr->base.as<ast::TypeInstType>() ) {
				if ( auto structDecl = symtab.lookupStruct( base->name ) ) {
					auto mut = ast::mutate( decl );
					mut->base = new ast::PointerType(
						new ast::StructInstType( structDecl ) );
					decl = mut;
				}
			}
		}
	}

	// This section is common with struct/union, except for the return value.
	if ( !decl->body ) {
		return decl;
	}

	ForwardEnumsType::iterator fwds = forwardEnums.find( decl->name );
	if ( fwds != forwardEnums.end() ) {
		for ( auto inst : fwds->second ) {
			inst->base = decl;
		}
		forwardEnums.erase( fwds );
	}

	return decl;
}

template<typename AggrDecl>
AggrDecl const * LinkTypesCore::renameGenericParams( AggrDecl const * decl ) {
	GuardValue( inGeneric ) = !decl->params.empty();
	if ( !inGeneric ) {
		GuardValue( location ) = &decl->location;
		return decl;
	}
	auto mut = ast::mutate( decl );
	GuardValue( location ) = &mut->location;
	for ( ast::ptr<ast::TypeDecl> & typeDecl : mut->params ) {
		typeDecl.get_and_mutate()->name = "__" + typeDecl->name + "_generic_";
	}
	return mut;
}

ast::StructDecl const * LinkTypesCore::previsit( ast::StructDecl const * decl ) {
	return renameGenericParams( decl );
}

void LinkTypesCore::postvisit( ast::StructDecl const * decl ) {
	if ( !decl->body ) {
		return;
	}

	ForwardStructsType::iterator fwds = forwardStructs.find( decl->name );
	if ( fwds != forwardStructs.end() ) {
		for ( auto inst : fwds->second ) {
			inst->base = decl;
		}
		forwardStructs.erase( fwds );
	}
}

ast::UnionDecl const * LinkTypesCore::previsit( ast::UnionDecl const * decl ) {
	return renameGenericParams( decl );
}

void LinkTypesCore::postvisit( ast::UnionDecl const * decl ) {
	if ( !decl->body ) {
		return;
	}

	ForwardUnionsType::iterator fwds = forwardUnions.find( decl->name );
	if ( fwds != forwardUnions.end() ) {
		for ( auto inst : fwds->second ) {
			inst->base = decl;
		}
		forwardUnions.erase( fwds );
	}
}

ast::TraitDecl const * LinkTypesCore::postvisit( ast::TraitDecl const * decl ) {
	auto mut = ast::mutate( decl );
	if ( mut->name == "sized" ) {
		// "sized" is a special trait - flick the sized status on for the type variable.
		assertf( mut->params.size() == 1, "Built-in trait 'sized' has incorrect number of parameters: %zd", decl->params.size() );
		ast::TypeDecl * td = mut->params.front().get_and_mutate();
		td->sized = true;
	}

	// There is some overlap with code from decayForallPointers,
	// perhaps reorganization or shared helper functions are called for.
	// Move assertions from type parameters into the body of the trait.
	for ( ast::ptr<ast::TypeDecl> const & td : decl->params ) {
		auto expanded = expandAssertions( td->assertions );
		for ( auto declWithType : expanded ) {
			mut->members.emplace_back( declWithType.release() );
		}
	}
	return mut;
}

} // namespace

void linkReferenceToTypes( ast::TranslationUnit & translationUnit ) {
	ast::Pass<LinkTypesCore>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
