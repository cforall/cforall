//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ReplaceTypedef.cpp -- Fill in all typedefs with the underlying type.
//
// Author           : Andrew Beach
// Created On       : Tue Jun 29 14:59:00 2022
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Dec 16 20:50:42 2024
// Update Count     : 5
//

#include "ReplaceTypedef.hpp"

#include "AST/Copy.hpp"
#include "AST/Pass.hpp"
#include "Common/ScopedMap.hpp"
#include "Common/UniqueName.hpp"
#include "ResolvExpr/Unify.hpp"

namespace Validate {

namespace {

struct ReplaceTypedefCore final :
		public ast::WithCodeLocation,
		public ast::WithDeclsToAdd,
		public ast::WithGuards,
		public ast::WithShortCircuiting,
		public ast::WithVisitorRef<ReplaceTypedefCore> {

	void previsit( ast::QualifiedType const * );
	ast::Type const * postvisit( ast::QualifiedType const * );
	ast::Type const * postvisit( ast::TypeInstType const * );
	ast::Decl const * postvisit( ast::TypedefDecl const * );
	void previsit( ast::TypeDecl const * );
	void previsit( ast::FunctionDecl const * );
	void previsit( ast::ObjectDecl const * );
	ast::DeclWithType const * postvisit( ast::ObjectDecl const * );

	void previsit( ast::CastExpr const * );
	void previsit( ast::CompoundStmt const * );
	void postvisit( ast::CompoundStmt const * );

	ast::StructDecl const * previsit( ast::StructDecl const * );
	ast::UnionDecl const * previsit( ast::UnionDecl const * );
	void previsit( ast::EnumDecl const * );
	void previsit( ast::TraitDecl const * );

	template<typename AggrDecl>
	void addImplicitTypedef( AggrDecl * aggDecl );
	template<typename AggrDecl>
	AggrDecl const * handleAggregate( AggrDecl const * aggDecl );

	using TypedefDeclPtr = ast::ptr<ast::TypedefDecl>;
	using TypedefMap = ScopedMap<std::string, std::pair<TypedefDeclPtr, int>>;
	using TypeDeclMap = ScopedMap<std::string, ast::TypeDecl const *>;

	TypedefMap typedefNames;
	TypeDeclMap typedeclNames;
	int scopeLevel;
	bool isAtFunctionTop = false;
};

void ReplaceTypedefCore::previsit( ast::QualifiedType const * ) {
	visit_children = false;
}

ast::Type const * ReplaceTypedefCore::postvisit(
		ast::QualifiedType const * type ) {
	// Replacing typedefs only makes sense for the 'oldest ancestor'
	// of the qualified type.
	return ast::mutate_field( type, &ast::QualifiedType::parent,
		type->parent->accept( *visitor ) );
}

ast::Type const * ReplaceTypedefCore::postvisit(
		ast::TypeInstType const * type ) {
	// Instances of typedef types will come here. If it is an instance
	// of a typedef type, link the instance to its actual type.
	TypedefMap::const_iterator def = typedefNames.find( type->name );
	if ( def != typedefNames.end() ) {
		ast::Type * ret = ast::deepCopy( def->second.first->base );
		ret->qualifiers |= type->qualifiers;
		// We ignore certain attributes on function parameters if they arrive
		// by typedef. GCC appears to do the same thing.
		if ( isAtFunctionTop ) {
			erase_if( ret->attributes, []( ast::Attribute const * attr ){
				return !attr->isValidOnFuncParam();
			} );
		}
		for ( const auto & attribute : type->attributes ) {
			ret->attributes.push_back( attribute );
		}
		// Place instance parameters on the typedef'd type.
		if ( !type->params.empty() ) {
			auto rtt = dynamic_cast<ast::BaseInstType *>( ret );
			if ( !rtt ) {
				assert( location );
				SemanticError( *location, "Cannot apply type parameters to base type of %s.", type->name.c_str() );
			}
			rtt->params.clear();
			for ( auto it : type->params ) {
				rtt->params.push_back( ast::deepCopy( it ) );
			}
			// Recursively fix typedefs on parameters.
			ast::mutate_each( rtt, &ast::BaseInstType::params, *visitor );
		}
		return ret;
	} else {
		TypeDeclMap::const_iterator base = typedeclNames.find( type->name );
		if ( base == typedeclNames.end() ) {
			assert( location );
			SemanticError( *location, "Use of undefined type %s.", type->name.c_str() );
		}
		return ast::mutate_field( type, &ast::TypeInstType::base, base->second );
	}
}
struct VarLenChecker : public ast::WithShortCircuiting {
	bool result = false;
	void previsit( ast::FunctionType const * ) { visit_children = false; }
	void previsit( ast::ArrayType const * at ) { result |= at->isVarLen; }
};
static bool hasVarLen( const ast::Type * t ) {
	return ast::Pass<VarLenChecker>::read( t );
}
struct ArrayTypeExtractor {
	std::vector<const ast::ArrayType *> result;
	void postvisit( const ast::ArrayType * at ) {
		result.push_back( at );
	}
};
static bool dimensionPresenceMismatched( const ast::Type * t0, const ast::Type * t1) {
	std::vector<const ast::ArrayType *> at0s = ast::Pass<ArrayTypeExtractor>::read( t0 );
	std::vector<const ast::ArrayType *> at1s = ast::Pass<ArrayTypeExtractor>::read( t1 );
	assert( at0s.size() == at1s.size() );
	for (size_t i = 0; i < at0s.size(); i++) {
		const ast::ArrayType * at0 = at0s[i];
		const ast::ArrayType * at1 = at1s[i];
		assert( ResolvExpr::typesCompatible( at0, at1 ) );
		if ( (at0->dimension != nullptr) != (at1->dimension != nullptr) ) return true;
	}
	return false;
}
ast::Decl const * ReplaceTypedefCore::postvisit(
		ast::TypedefDecl const * decl ) {
	if ( 1 == typedefNames.count( decl->name ) &&
			typedefNames[ decl->name ].second == scopeLevel ) {
		ast::Type const * t0 = decl->base;
		ast::Type const * t1 = typedefNames[ decl->name ].first->base;
		// [hasVarLen]
		// Cannot redefine VLA typedefs. Note: this is slightly incorrect,
		// because our notion of VLAs at this point in the translator is
		// imprecise. In particular, this will disallow redefining typedefs
		// with arrays whose dimension is an enumerator or a cast of a
		// constant/enumerator. The effort required to fix this corner case
		// likely outweighs the utility of allowing it.
		// [dimensionPresenceMismatched]
		// Core typesCompatible logic interprets absent dimensions as wildcards,
		// i.e. float[][*] matches float[][42].
		// For detecting incompatible typedefs, we have to interpret them verbatim,
		// i.e. float[] is different than float[42].
		// But typesCompatible does assure that the pair of types is structurally
		// consistent, outside of the dimension expressions.  This assurance guards
		// the dimension-presence traversal.  So this traversal logic can (and does)
		// assume that ArrayTypes will be encountered in analogous places.
		if ( !ResolvExpr::typesCompatible( t0, t1 )
				|| hasVarLen( t0 )
				|| hasVarLen( t1 )
				|| dimensionPresenceMismatched( t0, t1 ) ) {
			SemanticError( decl->location, "Cannot redefine typedef %s", decl->name.c_str() );
		}
	} else {
		typedefNames[ decl->name ] =
			std::make_pair( TypedefDeclPtr( decl ), scopeLevel );
	}

	// When a typedef is a forward declaration:
	// >	typedef struct screen SCREEN;
	// the declaration portion must be retained:
	// >	struct screen;
	// because the expansion of the typedef is:
	// >	void func( SCREEN * p ) -> void func( struct screen * p );
	// hence type name "screen" must be defined.
	// Note: qualifiers on the typedef are not used for the forward declaration.

	ast::Type const * designatorType = decl->base->stripDeclarator();
	if ( auto structType = dynamic_cast<ast::StructInstType const *>( designatorType ) ) {
		declsToAddBefore.push_back( new ast::StructDecl(
			decl->location, structType->name, ast::AggregateDecl::Struct, {},
			decl->linkage ) );
	} else if ( auto unionType = dynamic_cast<ast::UnionInstType const *>( designatorType ) ) {
		declsToAddBefore.push_back( new ast::UnionDecl(
			decl->location, unionType->name, {}, decl->linkage ) );
	} else if ( auto enumType = dynamic_cast<ast::EnumInstType const *>( designatorType ) ) {
		declsToAddBefore.push_back( new ast::EnumDecl(
			decl->location, enumType->name, false, {}, decl->linkage,
			( (enumType->base) ? enumType->base->base : nullptr )
			) );
	}
	return ast::deepCopy( decl );
}

void ReplaceTypedefCore::previsit( ast::TypeDecl const * decl ) {
	typedefNames.erase( decl->name );
	typedeclNames.insert( decl->name, decl );
}

void ReplaceTypedefCore::previsit( ast::FunctionDecl const * ) {
	GuardScope( typedefNames );
	GuardScope( typedeclNames );
	GuardValue( isAtFunctionTop ) = true;
}

void ReplaceTypedefCore::previsit( ast::ObjectDecl const * ) {
	GuardScope( typedefNames );
	GuardScope( typedeclNames );
}

ast::DeclWithType const * ReplaceTypedefCore::postvisit(
		ast::ObjectDecl const * decl ) {
	if ( ast::FunctionType const * type = decl->type.as<ast::FunctionType>() ) {
		using DWTVector = std::vector<ast::ptr<ast::DeclWithType>>;
		using DeclVector = std::vector<ast::ptr<ast::TypeDecl>>;
		CodeLocation const & declLocation = decl->location;
		UniqueName paramNamer( decl->name + "Param" );

		// Replace the current object declaration with a function declaration.
		ast::FunctionDecl const * newDecl = new ast::FunctionDecl(
			declLocation,
			decl->name,
			map_range<DeclVector>( type->forall, []( const ast::TypeInstType * inst ) {
				return ast::deepCopy( inst->base );
			} ),
			map_range<DWTVector>( type->assertions, []( const ast::VariableExpr * expr ) {
				return ast::deepCopy( expr->var );
			} ),
			map_range<DWTVector>( type->params, [&declLocation, &paramNamer]( const ast::Type * type ) {
				assert( type );
				return new ast::ObjectDecl( declLocation, paramNamer.newName(), ast::deepCopy( type ) );
			} ),
			map_range<DWTVector>( type->returns, [&declLocation, &paramNamer]( const ast::Type * type ) {
				assert( type );
				return new ast::ObjectDecl( declLocation, paramNamer.newName(), ast::deepCopy( type ) );
			} ),
			nullptr,
			decl->storage,
			decl->linkage,
			{/* attributes */},
			decl->funcSpec
		);
		return newDecl;
	}
	return decl;
}

void ReplaceTypedefCore::previsit( ast::CastExpr const * ) {
	GuardScope( typedefNames );
	GuardScope( typedeclNames );
}

void ReplaceTypedefCore::previsit( ast::CompoundStmt const * ) {
	GuardScope( typedefNames );
	GuardScope( typedeclNames );
	GuardValue( isAtFunctionTop ) = false;
	scopeLevel += 1;
}

void ReplaceTypedefCore::postvisit( ast::CompoundStmt const * ) {
	scopeLevel -= 1;
}

ast::StructDecl const * ReplaceTypedefCore::previsit( ast::StructDecl const * decl ) {
	visit_children = false;
	addImplicitTypedef( decl );
	return handleAggregate( decl );
}

ast::UnionDecl const * ReplaceTypedefCore::previsit( ast::UnionDecl const * decl ) {
	visit_children = false;
	addImplicitTypedef( decl );
	return handleAggregate( decl );
}

void ReplaceTypedefCore::previsit( ast::EnumDecl const * decl ) {
	addImplicitTypedef( decl );
}

void ReplaceTypedefCore::previsit( ast::TraitDecl const * ) {
	GuardScope( typedefNames );
	GuardScope( typedeclNames );
}

template<typename AggrDecl>
void ReplaceTypedefCore::addImplicitTypedef( AggrDecl * aggrDecl ) {
	if ( 0 != typedefNames.count( aggrDecl->name ) ) {
		return;
	}
	ast::Type * type = nullptr;
	if ( auto structDecl = dynamic_cast<const ast::StructDecl *>( aggrDecl ) ) {
		type = new ast::StructInstType( structDecl->name );
	} else if ( auto unionDecl = dynamic_cast<const ast::UnionDecl *>( aggrDecl ) ) {
		type = new ast::UnionInstType( unionDecl->name );
	} else if ( auto enumDecl = dynamic_cast<const ast::EnumDecl *>( aggrDecl ) ) {
		type = new ast::EnumInstType( enumDecl->name );
	}
	assert( type );

	TypedefDeclPtr typeDecl = new ast::TypedefDecl( aggrDecl->location,
		aggrDecl->name, ast::Storage::Classes(), type, aggrDecl->linkage );
	// Add the implicit typedef to the AST.
	declsToAddAfter.push_back( ast::deepCopy( typeDecl.get() ) );
	// Shore the name in the map of names.
	typedefNames[ aggrDecl->name ] =
		std::make_pair( std::move( typeDecl ), scopeLevel );
}

template<typename AggrDecl>
AggrDecl const * ReplaceTypedefCore::handleAggregate( AggrDecl const * decl ) {
	SemanticErrorException errors;

	ValueGuard<decltype(declsToAddBefore)> oldBeforeDecls( declsToAddBefore );
	ValueGuard<decltype(declsToAddAfter )> oldAfterDecls(  declsToAddAfter );
	declsToAddBefore.clear();
	declsToAddAfter.clear();

	GuardScope( typedefNames );
	GuardScope( typedeclNames );
	decl = mutate_each( decl, &ast::AggregateDecl::params, *visitor );
	decl = mutate_each( decl, &ast::AggregateDecl::attributes, *visitor );

	auto mut = ast::mutate( decl );

	std::list<ast::ptr<ast::Decl>> members;
	// Unroll accept_all for decl->members so that implicit typedefs for
	// nested types are added to the aggregate body.
	for ( ast::ptr<ast::Decl> const & member : mut->members ) {
		assert( declsToAddBefore.empty() );
		assert( declsToAddAfter.empty() );
		ast::Decl const * newMember = nullptr;
		try {
			newMember = member->accept( *visitor );
		} catch ( SemanticErrorException & e ) {
			errors.append( e );
		}
		if ( !declsToAddBefore.empty() ) {
			members.splice( members.end(), declsToAddBefore );
		}
		members.push_back( newMember );
		if ( !declsToAddAfter.empty() ) {
			members.splice( members.end(), declsToAddAfter );
		}
	}
	assert( declsToAddBefore.empty() );
	assert( declsToAddAfter.empty() );
	errors.throwIfNonEmpty();

	mut->members.clear();
	for ( auto member : members ) {
		mut->members.push_back( member );
	}

	return mut;
}

} // namespace

void replaceTypedef( ast::TranslationUnit & translationUnit ) {
	ast::Pass<ReplaceTypedefCore> pass;
	ast::accept_all( translationUnit, pass );
	if ( pass.core.typedefNames.count( "size_t" ) ) {
		translationUnit.global.sizeType =
			ast::deepCopy( pass.core.typedefNames["size_t"].first->base );
	} else {
		// Missing the global definition, default to long unsigned int.
		// Perhaps this should be a warning instead.
		translationUnit.global.sizeType =
			new ast::BasicType( ast::BasicKind::LongUnsignedInt );
	}
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
