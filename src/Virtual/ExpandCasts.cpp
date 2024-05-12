//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExpandCasts.cpp --
//
// Author           : Andrew Beach
// Created On       : Mon Jul 24 13:59:00 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Nov 27 09:28:20 2023
// Update Count     : 10
//

#include "ExpandCasts.hpp"

#include <cassert>                   // for assert, assertf
#include <iterator>                  // for back_inserter, inserter
#include <string>                    // for string, allocator, operator==, o...

#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Pass.hpp"
#include "Common/ScopedMap.hpp"      // for ScopedMap
#include "Common/SemanticError.hpp"  // for SemanticError
#include "SymTab/Mangler.hpp"        // for mangleType

namespace Virtual {

namespace {

bool is_prefix( const std::string & prefix, const std::string& entire ) {
	size_t const p_size = prefix.size();
	return (p_size < entire.size() && prefix == entire.substr(0, p_size));
}

bool is_type_id_object( const ast::ObjectDecl * decl ) {
	return is_prefix( "__cfatid_", decl->name );
}

	// Indented until the new ast code gets added.

	/// Maps virtual table types the instance for that type.

/// Better error locations for generated casts.
// TODO: Does the improved distribution of code locations make this unneeded?
CodeLocation castLocation( const ast::VirtualCastExpr * castExpr ) {
	if ( castExpr->location.isSet() ) {
		return castExpr->location;
	} else if ( castExpr->arg->location.isSet() ) {
		return castExpr->arg->location;
	} else {
		return CodeLocation();
	}
}

[[noreturn]] void castError( ast::VirtualCastExpr const * castExpr, std::string const & message ) {
	SemanticError( castLocation( castExpr ), message );
}

class TypeIdTable final {
	ScopedMap<std::string, ast::ObjectDecl const *> instances;
public:
	void enterScope() { instances.beginScope(); }
	void leaveScope() { instances.endScope(); }

	// Attempt to insert an instance into the map. If there is a conflict,
	// returns the previous declaration for error messages.
	ast::ObjectDecl const * insert( ast::ObjectDecl const * typeIdDecl ) {
		std::string mangledName = Mangle::mangleType( typeIdDecl->type );
		ast::ObjectDecl const *& value = instances[ mangledName ];
		if ( value ) {
			if ( typeIdDecl->storage.is_extern ) {
				return nullptr;
			} else if ( !value->storage.is_extern ) {
				return value;
			}
		}
		value = typeIdDecl;
		return nullptr;
	}

	ast::ObjectDecl const * lookup( ast::Type const * typeIdType ) {
		std::string mangledName = Mangle::mangleType( typeIdType );
		auto const it = instances.find( mangledName );
		return ( instances.end() == it ) ? nullptr : it->second;
	}
};

struct ExpandCastsCore final {
	void previsit( ast::FunctionDecl const * decl );
	void previsit( ast::StructDecl const * decl );
	void previsit( ast::ObjectDecl const * decl );
	ast::Expr const * postvisit( ast::VirtualCastExpr const * expr );

	ast::CastExpr const * cast_to_type_id(
		ast::Expr const * expr, unsigned int level_of_indirection );

	ast::FunctionDecl const * vcast_decl = nullptr;
	ast::StructDecl const * info_decl = nullptr;

	TypeIdTable symtab;
};

void ExpandCastsCore::previsit( ast::FunctionDecl const * decl ) {
	if ( !vcast_decl && "__cfavir_virtual_cast" == decl->name ) {
		vcast_decl = decl;
	}
}

void ExpandCastsCore::previsit( ast::StructDecl const * decl ) {
	if ( !info_decl && decl->body && "__cfavir_type_info" == decl->name ) {
		info_decl = decl;
	}
}

void ExpandCastsCore::previsit( ast::ObjectDecl const * decl ) {
	if ( is_type_id_object( decl ) ) {
		// Multiple definitions should be fine because of linkonce.
		symtab.insert( decl );
	}
}

/// Get the base type from a pointer or reference.
ast::Type const * getBaseType( ast::ptr<ast::Type> const & type ) {
	if ( auto target = type.as<ast::PointerType>() ) {
		return target->base.get();
	} else if ( auto target = type.as<ast::ReferenceType>() ) {
		return target->base.get();
	} else {
		return nullptr;
	}
}

/// Copy newType, but give the copy the params of the oldType.
ast::StructInstType * polyCopy(
		ast::StructInstType const * oldType,
		ast::StructInstType const * newType ) {
	assert( oldType->params.size() == newType->params.size() );
	ast::StructInstType * retType = ast::deepCopy( newType );
	if ( ! oldType->params.empty() ) {
		retType->params.clear();
		for ( auto oldParams : oldType->params ) {
			retType->params.push_back( ast::deepCopy( oldParams ) );
		}
	}
	return retType;
}

/// Follow the "head" field of the structure to get the type that is pointed
/// to by that field.
ast::StructInstType const * followHeadPointerType(
		CodeLocation const & errorLocation,
		ast::StructInstType const * oldType,
		std::string const & fieldName ) {
	ast::StructDecl const * oldDecl = oldType->base;
	assert( oldDecl );

	// Helper function for throwing semantic errors.
	auto throwError = [&fieldName, &errorLocation, &oldDecl]( std::string const & message ) {
		SemanticError( errorLocation, "While following head pointer of %s named \"%s\": %s",
					   oldDecl->name.c_str(), fieldName.c_str(), message.c_str() );
	};

	if ( oldDecl->members.empty() ) {
		throwError( "Type has no fields." );
	}
	ast::ptr<ast::Decl> const & memberDecl = oldDecl->members.front();
	assert( memberDecl );
	ast::ObjectDecl const * fieldDecl = memberDecl.as<ast::ObjectDecl>();
	assert( fieldDecl );
	if ( fieldName != fieldDecl->name ) {
		throwError( "Head field did not have expected name." );
	}

	ast::ptr<ast::Type> const & fieldType = fieldDecl->type;
	if ( nullptr == fieldType ) {
		throwError( "Could not get head field." );
	}
	auto ptrType = fieldType.as<ast::PointerType>();
	if ( nullptr == ptrType ) {
		throwError( "First field is not a pointer type." );
	}
	assert( ptrType->base );
	auto newType = ptrType->base.as<ast::StructInstType>();
	if ( nullptr == newType ) {
		throwError( "First field does not point to a structure type." );
	}

	return polyCopy( oldType, newType );
}

/// Get the type-id type from a virtual type.
ast::StructInstType const * getTypeIdType(
		CodeLocation const & errorLocation,
		ast::Type const * type ) {
	auto typeInst = dynamic_cast<ast::StructInstType const *>( type );
	if ( nullptr == typeInst ) {
		return nullptr;
	}
	ast::ptr<ast::StructInstType> tableInst =
		followHeadPointerType( errorLocation, typeInst, "virtual_table" );
	if ( nullptr == tableInst ) {
		return nullptr;
	}
	ast::StructInstType const * typeIdInst =
		followHeadPointerType( errorLocation, tableInst, "__cfavir_typeid" );
	return typeIdInst;
}

ast::Expr const * ExpandCastsCore::postvisit(
		ast::VirtualCastExpr const * expr ) {
	assertf( expr->result, "Virtual cast target not found before expansion." );

	assert( vcast_decl );
	assert( info_decl );

	ast::Type const * base_type = getBaseType( expr->result );
	if ( nullptr == base_type ) {
		castError( expr, "Virtual cast target must be a pointer or reference type." );
	}
	ast::StructInstType const * type_id_type =
			getTypeIdType( castLocation( expr ), base_type );
	if ( nullptr == type_id_type ) {
		castError( expr, "Ill formed virtual cast target type." );
	}
	ast::ObjectDecl const * type_id = symtab.lookup( type_id_type );
	if ( nullptr == type_id ) {
		// I'm trying to give a different error for polymorpic types as
		// different things can go wrong there.
		if ( type_id_type->params.empty() ) {
			castError( expr, "Virtual cast does not target a virtual type." );
		} else {
			castError( expr, "Virtual cast does not target a type with a "
				"type id (possible missing virtual table)." );
		}
	}

	return new ast::CastExpr( expr->location,
		new ast::ApplicationExpr( expr->location,
			ast::VariableExpr::functionPointer( expr->location, vcast_decl ),
			{
				cast_to_type_id(
					new ast::AddressExpr( expr->location,
						new ast::VariableExpr( expr->location, type_id ) ),
					1 ),
				cast_to_type_id( expr->arg, 2 ),
			}
		),
		ast::deepCopy( expr->result )
	);
}

ast::CastExpr const * ExpandCastsCore::cast_to_type_id(
		ast::Expr const * expr, unsigned int level_of_indirection ) {
	assert( info_decl );
	ast::Type * type = new ast::StructInstType( info_decl, ast::CV::Const );
	for ( unsigned int i = 0 ; i < level_of_indirection ; ++i ) {
		type = new ast::PointerType( type );
	}
	return new ast::CastExpr( expr->location, expr, type );
}

} // namespace

void expandCasts( ast::TranslationUnit & translationUnit ) {
	ast::Pass<ExpandCastsCore>::run( translationUnit );
}

} // namespace Virtual
