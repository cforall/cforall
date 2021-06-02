//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExpandCasts.cc --
//
// Author           : Andrew Beach
// Created On       : Mon Jul 24 13:59:00 2017
// Last Modified By : Andrew Beach
// Last Modified On : Fri Jul 31 10:29:00 2020
// Update Count     : 4
//

#include "ExpandCasts.h"

#include <cassert>                 // for assert, assertf
#include <iterator>                // for back_inserter, inserter
#include <string>                  // for string, allocator, operator==, ope...

#include "Common/PassVisitor.h"    // for PassVisitor
#include "Common/ScopedMap.h"      // for ScopedMap
#include "Common/SemanticError.h"  // for SemanticError
#include "SymTab/Mangler.h"        // for mangleType
#include "SynTree/Declaration.h"   // for ObjectDecl, StructDecl, FunctionDecl
#include "SynTree/Expression.h"    // for VirtualCastExpr, CastExpr, Address...
#include "SynTree/Mutator.h"       // for mutateAll
#include "SynTree/Type.h"          // for Type, PointerType, StructInstType
#include "SynTree/Visitor.h"       // for acceptAll

namespace Virtual {

static bool is_prefix( const std::string & prefix, const std::string& entire ) {
	size_t const p_size = prefix.size();
	return (p_size < entire.size() && prefix == entire.substr(0, p_size));
}

static bool is_type_id_object( const ObjectDecl * objectDecl ) {
	const std::string & objectName = objectDecl->name;
	return is_prefix( "__cfatid_", objectName );
}

	// Indented until the new ast code gets added.

	/// Maps virtual table types the instance for that type.
	class VirtualTableMap final {
		ScopedMap<std::string, ObjectDecl *> vtable_instances;
	public:
		void enterScope() {
			vtable_instances.beginScope();
		}
		void leaveScope() {
			vtable_instances.endScope();
		}

		ObjectDecl * insert( ObjectDecl * vtableDecl ) {
			std::string const & mangledName = SymTab::Mangler::mangleType( vtableDecl->type );
			ObjectDecl *& value = vtable_instances[ mangledName ];
			if ( value ) {
				if ( vtableDecl->storageClasses.is_extern ) {
					return nullptr;
				} else if ( ! value->storageClasses.is_extern ) {
					return value;
				}
			}
			value = vtableDecl;
			return nullptr;
		}

		ObjectDecl * lookup( const Type * vtableType ) {
			std::string const & mangledName = SymTab::Mangler::mangleType( vtableType );
			const auto it = vtable_instances.find( mangledName );
			return ( vtable_instances.end() == it ) ? nullptr : it->second;
		}
	};

	class VirtualCastCore {
		CastExpr * cast_to_type_id( Expression * expr, int level_of_indirection ) {
			Type * type = new StructInstType(
				Type::Qualifiers( Type::Const ), pvt_decl );
			for (int i = 0 ; i < level_of_indirection ; ++i) {
				type = new PointerType( noQualifiers, type );
			}
			return new CastExpr( expr, type );
		}

	public:
		VirtualCastCore() :
			indexer(), vcast_decl( nullptr ), pvt_decl( nullptr )
		{}

		void premutate( FunctionDecl * functionDecl );
		void premutate( StructDecl * structDecl );
		void premutate( ObjectDecl * objectDecl );

		Expression * postmutate( VirtualCastExpr * castExpr );

		VirtualTableMap indexer;
	private:
		FunctionDecl *vcast_decl;
		StructDecl *pvt_decl;
	};

	void VirtualCastCore::premutate( FunctionDecl * functionDecl ) {
		if ( (! vcast_decl) &&
		     functionDecl->get_name() == "__cfavir_virtual_cast" ) {
			vcast_decl = functionDecl;
		}
	}

	void VirtualCastCore::premutate( StructDecl * structDecl ) {
		if ( pvt_decl || ! structDecl->has_body() ) {
			return;
		} else if ( structDecl->get_name() == "__cfavir_type_info" ) {
			pvt_decl = structDecl;
		}
	}

	void VirtualCastCore::premutate( ObjectDecl * objectDecl ) {
		if ( is_type_id_object( objectDecl ) ) {
			// Multiple definitions should be fine because of linkonce.
			indexer.insert( objectDecl );
		}
	}

	namespace {

	/// Better error locations for generated casts.
	CodeLocation castLocation( const VirtualCastExpr * castExpr ) {
		if ( castExpr->location.isSet() ) {
			return castExpr->location;
		} else if ( castExpr->arg->location.isSet() ) {
			return castExpr->arg->location;
		} else if ( castExpr->result->location.isSet() ) {
			return castExpr->result->location;
		} else {
			return CodeLocation();
		}
	}

	[[noreturn]] void castError( const VirtualCastExpr * castExpr, std::string const & message ) {
		SemanticError( castLocation( castExpr ), message );
	}

	/// Get the base type from a pointer or reference.
	const Type * getBaseType( const Type * type ) {
		if ( auto target = dynamic_cast<const PointerType *>( type ) ) {
			return target->base;
		} else if ( auto target = dynamic_cast<const ReferenceType *>( type ) ) {
			return target->base;
		} else {
			return nullptr;
		}
	}

	/* Attempt to follow the "head" field of the structure to get the...
	 * Returns nullptr on error, otherwise owner must free returned node.
	 */
	StructInstType * followHeadPointerType(
			const StructInstType * oldType,
			const std::string& fieldName,
			const CodeLocation& errorLocation ) {

		// First section of the function is all about trying to fill this variable in.
		StructInstType * newType = nullptr;
		{
			const StructDecl * oldDecl = oldType->baseStruct;
			assert( oldDecl );

			// Helper function for throwing semantic errors.
			auto throwError = [&fieldName, &errorLocation, &oldDecl](const std::string& message) {
				const std::string& context = "While following head pointer of " +
					oldDecl->name + " named '" + fieldName + "': ";
				SemanticError( errorLocation, context + message );
			};

			if ( oldDecl->members.empty() ) {
				throwError( "Type has no fields." );
			}
			const Declaration * memberDecl = oldDecl->members.front();
			assert( memberDecl );
			const ObjectDecl * fieldDecl = dynamic_cast<const ObjectDecl *>( memberDecl );
			assert( fieldDecl );
			if ( fieldName != fieldDecl->name ) {
				throwError( "Head field did not have expected name." );
			}

			const Type * fieldType = fieldDecl->type;
			if ( nullptr == fieldType ) {
				throwError( "Could not get head field." );
			}
			const PointerType * ptrType = dynamic_cast<const PointerType *>( fieldType );
			if ( nullptr == ptrType ) {
				throwError( "First field is not a pointer type." );
			}
			assert( ptrType->base );
			newType = dynamic_cast<StructInstType *>( ptrType->base );
			if ( nullptr == newType ) {
				throwError( "First field does not point to a structure type." );
			}
		}

		// Now we can look into copying it.
		newType = newType->clone();
		if ( ! oldType->parameters.empty() ) {
			deleteAll( newType->parameters );
			newType->parameters.clear();
			cloneAll( oldType->parameters, newType->parameters );
		}
		return newType;
	}

	/// Get the type-id type from a virtual type.
	StructInstType * getTypeIdType( const Type * type, const CodeLocation& errorLocation ) {
		const StructInstType * typeInst = dynamic_cast<const StructInstType *>( type );
		if ( nullptr == typeInst ) {
			return nullptr;
		}
		StructInstType * tableInst =
			followHeadPointerType( typeInst, "virtual_table", errorLocation );
		if ( nullptr == tableInst ) {
			return nullptr;
		}
		StructInstType * typeIdInst =
			followHeadPointerType( tableInst, "__cfavir_typeid", errorLocation );
		delete tableInst;
		return typeIdInst;
	}

	} // namespace

	Expression * VirtualCastCore::postmutate( VirtualCastExpr * castExpr ) {
		assertf( castExpr->result, "Virtual Cast target not found before expansion." );

		assert( vcast_decl );
		assert( pvt_decl );

		const Type * base_type = getBaseType( castExpr->result );
		if ( nullptr == base_type ) {
			castError( castExpr, "Virtual cast target must be a pointer or reference type." );
		}
		const Type * type_id_type = getTypeIdType( base_type, castLocation( castExpr ) );
		if ( nullptr == type_id_type ) {
			castError( castExpr, "Ill formed virtual cast target type." );
		}
		ObjectDecl * type_id = indexer.lookup( type_id_type );
		delete type_id_type;
		if ( nullptr == type_id ) {
			castError( castExpr, "Virtual cast does not target a virtual type." );
		}

		Expression * result = new CastExpr(
			new ApplicationExpr( VariableExpr::functionPointer( vcast_decl ), {
				cast_to_type_id( new AddressExpr( new VariableExpr( type_id ) ), 1 ),
				cast_to_type_id( castExpr->get_arg(), 2 ),
			} ),
			castExpr->get_result()->clone()
		);

		castExpr->set_arg( nullptr );
		castExpr->set_result( nullptr );
		delete castExpr;
		return result;
	}

	void expandCasts( std::list< Declaration * > & translationUnit ) {
		PassVisitor<VirtualCastCore> translator;
		mutateAll( translationUnit, translator );
	}
}
