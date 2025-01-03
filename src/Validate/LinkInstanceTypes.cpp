//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LinkInstanceTypes.cpp -- Connect instance types to declarations.
//
// Author           : Andrew Beach
// Created On       : Thr Apr 21 11:41:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri Jul 14  9:19:00 2023
// Update Count     : 3
//

#include "Validate/LinkInstanceTypes.hpp"

#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "Validate/ForallPointerDecay.hpp"
#include "Validate/NoIdSymbolTable.hpp"

namespace Validate {

namespace {

struct LinkTypesCore : public WithNoIdSymbolTable,
		public ast::WithCodeLocation,
		public ast::WithDeclsToAdd,
		public ast::WithGuards,
		public ast::WithShortCircuiting,
		public ast::WithVisitorRef<LinkTypesCore> {
	ast::TypeInstType const * postvisit( ast::TypeInstType const * type );
	ast::EnumInstType const * postvisit( ast::EnumInstType const * type );
	ast::StructInstType const * postvisit( ast::StructInstType const * type );
	ast::UnionInstType const * postvisit( ast::UnionInstType const * type );
	ast::TraitInstType const * postvisit( ast::TraitInstType const * type );
	void previsit( ast::QualifiedType const * type );
	void postvisit( ast::QualifiedType const * type );

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

	/// true if currently in a generic type body,
	/// so that type parameter instances can be renamed appropriately
	bool inGeneric = false;

	template<typename AggrDecl>
	AggrDecl const * renameGenericParams( AggrDecl const * decl );

	// This cluster is used to add declarations (before) but outside of
	// any "namespaces" which would qualify the names.
	bool inNamespace = false;
	std::list<ast::ptr<ast::Decl>> declsToAddOutside;
	/// The "leaveNamespace" is handled by guard.
	void enterNamespace();
	/// Puts the decl on the back of declsToAddAfter once traversal is
	/// outside of any namespaces.
	void addDeclAfterOutside( ast::Decl const * );
};

void LinkTypesCore::enterNamespace() {
	if ( inNamespace ) return;
	inNamespace = true;
	GuardAction( [this](){
		inNamespace = false;
		declsToAddAfter.splice( declsToAddAfter.begin(), declsToAddOutside );
	} );
}

void LinkTypesCore::addDeclAfterOutside( ast::Decl const * decl ) {
	if ( inNamespace ) {
		declsToAddOutside.emplace_back( decl );
	} else {
		declsToAddAfter.emplace_back( decl );
	}
}

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
	// The unset code location is used to detect imaginary declarations.
	// (They may never be used for enumerations.)
	if ( !decl || decl->location.isUnset() ) {
		assert( location );
		ast::EnumDecl * mut = new ast::EnumDecl( *location, type->name );
		mut->linkage = ast::Linkage::Compiler;
		decl = mut;
		symtab.addEnum( decl );
		addDeclAfterOutside( decl );
	}

	ast::EnumInstType * mut = ast::mutate( type );

	// Just linking in the node.
	mut->base = decl;

	if ( !decl->body ) {
		forwardEnums[ mut->name ].push_back( mut );
	}
	return mut;
}

ast::StructInstType const * LinkTypesCore::postvisit( ast::StructInstType const * type ) {
	ast::StructDecl const * decl = symtab.lookupStruct( type->name );
	// It's not a semantic error if the struct is not found, just an implicit forward declaration.
	// The unset code location is used to detect imaginary declarations.
	if ( !decl || decl->location.isUnset() ) {
		assert( location );
		ast::StructDecl * mut = new ast::StructDecl( *location, type->name );
		mut->linkage = ast::Linkage::Compiler;
		decl = mut;
		symtab.addStruct( decl );
		addDeclAfterOutside( decl );
	}

	ast::StructInstType * mut = ast::mutate( type );

	// Just linking in the node.
	mut->base = decl;

	if ( !decl->body ) {
		forwardStructs[ mut->name ].push_back( mut );
	}
	return mut;
}

ast::UnionInstType const * LinkTypesCore::postvisit( ast::UnionInstType const * type ) {
	ast::UnionDecl const * decl = symtab.lookupUnion( type->name );
	// It's not a semantic error if the union is not found, just an implicit forward declaration.
	// The unset code location is used to detect imaginary declarations.
	if ( !decl || decl->location.isUnset() ) {
		assert( location );
		ast::UnionDecl * mut = new ast::UnionDecl( *location, type->name );
		mut->linkage = ast::Linkage::Compiler;
		decl = mut;
		symtab.addUnion( decl );
		addDeclAfterOutside( decl );
	}

	ast::UnionInstType * mut = ast::mutate( type );

	// Just linking in the node.
	mut->base = decl;

	if ( !decl->body ) {
		forwardUnions[ mut->name ].push_back( mut );
	}
	return mut;
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
	mut->base = decl;

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
			// visit the base
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
		return decl;
	}
	auto mut = ast::mutate( decl );
	for ( ast::ptr<ast::TypeDecl> & typeDecl : mut->params ) {
		typeDecl.get_and_mutate()->name = "__" + typeDecl->name + "_generic_";
	}
	return mut;
}

ast::StructDecl const * LinkTypesCore::previsit( ast::StructDecl const * decl ) {
	enterNamespace();
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
	enterNamespace();
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
	// There is some overlap with code from decayForallPointers,
	// perhaps reorganization or shared helper functions are called for.
	// Move assertions from type parameters into the body of the trait.
	auto mut = ast::mutate( decl );
	for ( ast::ptr<ast::TypeDecl> const & td : decl->params ) {
		auto expanded = expandAssertions( td->assertions );
		for ( auto declWithType : expanded ) {
			mut->members.emplace_back( declWithType.release() );
		}
	}
	return mut;
}

} // namespace

void linkInstanceTypes( ast::TranslationUnit & translationUnit ) {
	ast::Pass<LinkTypesCore>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
