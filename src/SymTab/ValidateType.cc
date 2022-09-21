//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ValidateType.cc -- Validate and normalize types.
//
// Author           : Andrew Beach
// Created On       : Mon May 16 16:21:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue May 17 14:06:00 2022
// Update Count     : 0
//

#include "ValidateType.h"

#include "CodeGen/OperatorTable.h"
#include "Common/PassVisitor.h"
#include "SymTab/FixFunction.h"
#include "SynTree/Declaration.h"
#include "SynTree/Type.h"

namespace SymTab {

namespace {

/// Replaces enum types by int, and function or array types in function
/// parameter and return lists by appropriate pointers.
struct EnumAndPointerDecay_old {
	void previsit( EnumDecl * aggregateDecl );
	void previsit( FunctionType * func );
};

void EnumAndPointerDecay_old::previsit( EnumDecl * enumDecl ) {
	// Set the type of each member of the enumeration to be EnumConstant
	for ( std::list< Declaration * >::iterator i = enumDecl->members.begin(); i != enumDecl->members.end(); ++i ) {
		ObjectDecl * obj = dynamic_cast< ObjectDecl * >( * i );
		assert( obj );
		obj->set_type( new EnumInstType( Type::Qualifiers( Type::Const ), enumDecl->name ) );
	} // for
}

template< typename DWTList >
void fixFunctionList( DWTList & dwts, bool isVarArgs, FunctionType * func ) {
	auto nvals = dwts.size();
	bool containsVoid = false;
	for ( auto & dwt : dwts ) {
		// fix each DWT and record whether a void was found
		containsVoid |= fixFunction( dwt );
	}

	// the only case in which "void" is valid is where it is the only one in the list
	if ( containsVoid && ( nvals > 1 || isVarArgs ) ) {
		SemanticError( func, "invalid type void in function type " );
	}

	// one void is the only thing in the list; remove it.
	if ( containsVoid ) {
		delete dwts.front();
		dwts.clear();
	}
}

void EnumAndPointerDecay_old::previsit( FunctionType * func ) {
	// Fix up parameters and return types
	fixFunctionList( func->parameters, func->isVarArgs, func );
	fixFunctionList( func->returnVals, false, func );
}

/// Associates forward declarations of aggregates with their definitions
struct LinkReferenceToTypes_old final : public WithIndexer, public WithGuards, public WithVisitorRef<LinkReferenceToTypes_old>, public WithShortCircuiting {
	LinkReferenceToTypes_old( const Indexer * indexer );

	void postvisit( TypeInstType * typeInst );

	void postvisit( EnumInstType * enumInst );
	void postvisit( StructInstType * structInst );
	void postvisit( UnionInstType * unionInst );
	void postvisit( TraitInstType * traitInst );
	void previsit( QualifiedType * qualType );
	void postvisit( QualifiedType * qualType );

	void postvisit( QualifiedNameExpr * qualExpr );

	void postvisit( EnumDecl * enumDecl );
	void postvisit( StructDecl * structDecl );
	void postvisit( UnionDecl * unionDecl );
	void postvisit( TraitDecl * traitDecl );

	void previsit( StructDecl * structDecl );
	void previsit( UnionDecl * unionDecl );

	void renameGenericParams( std::list< TypeDecl * > & params );

private:
	const Indexer * local_indexer;

	typedef std::map< std::string, std::list< EnumInstType * > > ForwardEnumsType;
	typedef std::map< std::string, std::list< StructInstType * > > ForwardStructsType;
	typedef std::map< std::string, std::list< UnionInstType * > > ForwardUnionsType;
	ForwardEnumsType forwardEnums;
	ForwardStructsType forwardStructs;
	ForwardUnionsType forwardUnions;
	/// true if currently in a generic type body, so that type parameter instances can be renamed appropriately
	bool inGeneric = false;
};


LinkReferenceToTypes_old::LinkReferenceToTypes_old( const Indexer * other_indexer ) : WithIndexer( false ) {
	if ( other_indexer ) {
		local_indexer = other_indexer;
	} else {
		local_indexer = &indexer;
	} // if
}

void LinkReferenceToTypes_old::postvisit( EnumInstType * enumInst ) {
	const EnumDecl * st = local_indexer->lookupEnum( enumInst->name );
	// it's not a semantic error if the enum is not found, just an implicit forward declaration
	if ( st ) {
		enumInst->baseEnum = const_cast<EnumDecl *>(st); // Just linking in the node
	} // if
	if ( ! st || ! st->body ) {
		// use of forward declaration
		forwardEnums[ enumInst->name ].push_back( enumInst );
	} // if
}

void LinkReferenceToTypes_old::postvisit( StructInstType * structInst ) {
	const StructDecl * st = local_indexer->lookupStruct( structInst->name );
	// it's not a semantic error if the struct is not found, just an implicit forward declaration
	if ( st ) {
		structInst->baseStruct = const_cast<StructDecl *>(st); // Just linking in the node
	} // if
	if ( ! st || ! st->body ) {
		// use of forward declaration
		forwardStructs[ structInst->name ].push_back( structInst );
	} // if
}

void LinkReferenceToTypes_old::postvisit( UnionInstType * unionInst ) {
	const UnionDecl * un = local_indexer->lookupUnion( unionInst->name );
	// it's not a semantic error if the union is not found, just an implicit forward declaration
	if ( un ) {
		unionInst->baseUnion = const_cast<UnionDecl *>(un); // Just linking in the node
	} // if
	if ( ! un || ! un->body ) {
		// use of forward declaration
		forwardUnions[ unionInst->name ].push_back( unionInst );
	} // if
}

void LinkReferenceToTypes_old::previsit( QualifiedType * ) {
	visit_children = false;
}

void LinkReferenceToTypes_old::postvisit( QualifiedType * qualType ) {
	// linking only makes sense for the 'oldest ancestor' of the qualified type
	qualType->parent->accept( * visitor );
}

void LinkReferenceToTypes_old::postvisit( QualifiedNameExpr * qualExpr ) {
	const EnumDecl * st = local_indexer->lookupEnum( qualExpr->type_decl->name );
	qualExpr->type_decl = const_cast<EnumDecl *>(st);
}

// expand assertions from trait instance, performing the appropriate type variable substitutions
template< typename Iterator >
void expandAssertions( TraitInstType * inst, Iterator out ) {
	assertf( inst->baseTrait, "Trait instance not linked to base trait: %s", toCString( inst ) );
	std::list< DeclarationWithType * > asserts;
	for ( Declaration * decl : inst->baseTrait->members ) {
		asserts.push_back( strict_dynamic_cast<DeclarationWithType *>( decl->clone() ) );
	}
	// substitute trait decl parameters for instance parameters
	applySubstitution( inst->baseTrait->parameters.begin(), inst->baseTrait->parameters.end(), inst->parameters.begin(), asserts.begin(), asserts.end(), out );
}

void LinkReferenceToTypes_old::postvisit( TraitDecl * traitDecl ) {
	if ( traitDecl->name == "sized" ) {
		// "sized" is a special trait - flick the sized status on for the type variable
		assertf( traitDecl->parameters.size() == 1, "Built-in trait 'sized' has incorrect number of parameters: %zd", traitDecl->parameters.size() );
		TypeDecl * td = traitDecl->parameters.front();
		td->set_sized( true );
	}

	// move assertions from type parameters into the body of the trait
	for ( TypeDecl * td : traitDecl->parameters ) {
		for ( DeclarationWithType * assert : td->assertions ) {
			if ( TraitInstType * inst = dynamic_cast< TraitInstType * >( assert->get_type() ) ) {
				expandAssertions( inst, back_inserter( traitDecl->members ) );
			} else {
				traitDecl->members.push_back( assert->clone() );
			}
		}
		deleteAll( td->assertions );
		td->assertions.clear();
	} // for
}

void LinkReferenceToTypes_old::postvisit( TraitInstType * traitInst ) {
	// handle other traits
	const TraitDecl * traitDecl = local_indexer->lookupTrait( traitInst->name );
	if ( ! traitDecl ) {
		SemanticError( traitInst->location, "use of undeclared trait " + traitInst->name );
	} // if
	if ( traitDecl->parameters.size() != traitInst->parameters.size() ) {
		SemanticError( traitInst, "incorrect number of trait parameters: " );
	} // if
	traitInst->baseTrait = const_cast<TraitDecl *>(traitDecl); // Just linking in the node

	// need to carry over the 'sized' status of each decl in the instance
	for ( auto p : group_iterate( traitDecl->parameters, traitInst->parameters ) ) {
		TypeExpr * expr = dynamic_cast< TypeExpr * >( std::get<1>(p) );
		if ( ! expr ) {
			SemanticError( std::get<1>(p), "Expression parameters for trait instances are currently unsupported: " );
		}
		if ( TypeInstType * inst = dynamic_cast< TypeInstType * >( expr->get_type() ) ) {
			TypeDecl * formalDecl = std::get<0>(p);
			TypeDecl * instDecl = inst->baseType;
			if ( formalDecl->get_sized() ) instDecl->set_sized( true );
		}
	}
	// normalizeAssertions( traitInst->members );
}

void LinkReferenceToTypes_old::postvisit( EnumDecl * enumDecl ) {
	// visit enum members first so that the types of self-referencing members are updated properly
	// Replace the enum base; right now it works only for StructEnum
	if ( enumDecl->base ) {
		if ( const TypeInstType * base = dynamic_cast< TypeInstType * >(enumDecl->base) ) {
			if ( const StructDecl * decl = local_indexer->lookupStruct( base->name ) ) {
				enumDecl->base = new StructInstType( Type::Qualifiers(), const_cast< StructDecl * >( decl ) ); // Just linking in the node
			}
		} else if ( const PointerType * ptr = dynamic_cast< PointerType * >(enumDecl->base) ) {
			if ( const TypeInstType * ptrBase = dynamic_cast< TypeInstType * >( ptr->base ) ) {
				if ( const StructDecl * decl = local_indexer->lookupStruct( ptrBase->name ) ) {
					enumDecl->base = new PointerType( Type::Qualifiers(),
						new StructInstType( Type::Qualifiers(), const_cast< StructDecl * >( decl ) ) );
				}
			}
		}
	}
	
	if ( enumDecl->body ) {
		ForwardEnumsType::iterator fwds = forwardEnums.find( enumDecl->name );
		if ( fwds != forwardEnums.end() ) {
			for ( std::list< EnumInstType * >::iterator inst = fwds->second.begin(); inst != fwds->second.end(); ++inst ) {
				(* inst)->baseEnum = enumDecl;
			} // for
			forwardEnums.erase( fwds );
		} // if
	} // if
}

void LinkReferenceToTypes_old::renameGenericParams( std::list< TypeDecl * > & params ) {
	// rename generic type parameters uniquely so that they do not conflict with user-defined function forall parameters, e.g.
	//   forall(otype T)
	//   struct Box {
	//     T x;
	//   };
	//   forall(otype T)
	//   void f(Box(T) b) {
	//     ...
	//   }
	// The T in Box and the T in f are different, so internally the naming must reflect that.
	GuardValue( inGeneric );
	inGeneric = ! params.empty();
	for ( TypeDecl * td : params ) {
		td->name = "__" + td->name + "_generic_";
	}
}

void LinkReferenceToTypes_old::previsit( StructDecl * structDecl ) {
	renameGenericParams( structDecl->parameters );
}

void LinkReferenceToTypes_old::previsit( UnionDecl * unionDecl ) {
	renameGenericParams( unionDecl->parameters );
}

void LinkReferenceToTypes_old::postvisit( StructDecl * structDecl ) {
	// visit struct members first so that the types of self-referencing members are updated properly
	// xxx - need to ensure that type parameters match up between forward declarations and definition (most importantly, number of type parameters and their def>
	if ( structDecl->body ) {
		ForwardStructsType::iterator fwds = forwardStructs.find( structDecl->name );
		if ( fwds != forwardStructs.end() ) {
			for ( std::list< StructInstType * >::iterator inst = fwds->second.begin(); inst != fwds->second.end(); ++inst ) {
				(* inst)->baseStruct = structDecl;
			} // for
			forwardStructs.erase( fwds );
		} // if
	} // if
}

void LinkReferenceToTypes_old::postvisit( UnionDecl * unionDecl ) {
	if ( unionDecl->body ) {
	ForwardUnionsType::iterator fwds = forwardUnions.find( unionDecl->name );
		if ( fwds != forwardUnions.end() ) {
			for ( std::list< UnionInstType * >::iterator inst = fwds->second.begin(); inst != fwds->second.end(); ++inst ) {
				(* inst)->baseUnion = unionDecl;
			} // for
			forwardUnions.erase( fwds );
		} // if
	} // if
}

void LinkReferenceToTypes_old::postvisit( TypeInstType * typeInst ) {
	// ensure generic parameter instances are renamed like the base type
	if ( inGeneric && typeInst->baseType ) typeInst->name = typeInst->baseType->name;
	if ( const NamedTypeDecl * namedTypeDecl = local_indexer->lookupType( typeInst->name ) ) {
		if ( const TypeDecl * typeDecl = dynamic_cast< const TypeDecl * >( namedTypeDecl ) ) {
			typeInst->set_isFtype( typeDecl->kind == TypeDecl::Ftype );
		} // if
	} // if
}

/* // expand assertions from trait instance, performing the appropriate type variable substitutions
template< typename Iterator >
void expandAssertions( TraitInstType * inst, Iterator out ) {
	assertf( inst->baseTrait, "Trait instance not linked to base trait: %s", toCString( inst ) );
	std::list< DeclarationWithType * > asserts;
	for ( Declaration * decl : inst->baseTrait->members ) {
		asserts.push_back( strict_dynamic_cast<DeclarationWithType *>( decl->clone() ) );
	}
	// substitute trait decl parameters for instance parameters
	applySubstitution( inst->baseTrait->parameters.begin(), inst->baseTrait->parameters.end(), inst->parameters.begin(), asserts.begin(), asserts.end(), out );
}*/

/// Replace all traits in assertion lists with their assertions.
void expandTraits( std::list< TypeDecl * > & forall ) {
	for ( TypeDecl * type : forall ) {
		std::list< DeclarationWithType * > asserts;
		asserts.splice( asserts.end(), type->assertions );
		// expand trait instances into their members
		for ( DeclarationWithType * assertion : asserts ) {
			if ( TraitInstType * traitInst = dynamic_cast< TraitInstType * >( assertion->get_type() ) ) {
				// expand trait instance into all of its members
				expandAssertions( traitInst, back_inserter( type->assertions ) );
				delete traitInst;
			} else {
				// pass other assertions through
				type->assertions.push_back( assertion );
			} // if
		} // for
	} // for
}

struct TraitExpander_old final {
	void previsit( FunctionType * type ) {
		expandTraits( type->forall );
	}
	void previsit( StructDecl * decl ) {
		expandTraits( decl->parameters );
	}
	void previsit( UnionDecl * decl ) {
		expandTraits( decl->parameters );
	}
};

/*struct TraitExpander_old final {
	void previsit( FunctionType * );
	void previsit( StructDecl * );
	void previsit( UnionDecl * );
};

void TraitExpander_old::previsit( FunctionType * ftype ) {
	expandTraits( ftype->forall );
}

void TraitExpander_old::previsit( StructDecl * aggrDecl ) {
	expandTraits( aggrDecl->parameters );
}

void TraitExpander_old::previsit( UnionDecl * aggrDecl ) {
	expandTraits( aggrDecl->parameters );
}*/

/// Fix each function in the assertion list and check for invalid void type.
void fixAssertions(
		std::list< TypeDecl * > & forall, BaseSyntaxNode * node ) {
	for ( TypeDecl * type : forall ) {
		for ( DeclarationWithType *& assertion : type->assertions ) {
			bool isVoid = fixFunction( assertion );
			if ( isVoid ) {
				SemanticError( node, "invalid type void in assertion of function " );
			} // if
		} // for
	}
}

struct AssertionFixer_old final {
	void previsit( FunctionType * type ) {
		fixAssertions( type->forall, type );
	}
	void previsit( StructDecl * decl ) {
		fixAssertions( decl->parameters, decl );
	}
	void previsit( UnionDecl * decl ) {
		fixAssertions( decl->parameters, decl );
	}
};

/*
struct AssertionFixer_old final {
	void previsit( FunctionType * );
	void previsit( StructDecl * );
	void previsit( UnionDecl * );
};

void AssertionFixer_old::previsit( FunctionType * ftype ) {
	fixAssertions( ftype->forall, ftype );
}

void AssertionFixer_old::previsit( StructDecl * aggrDecl ) {
	fixAssertions( aggrDecl->parameters, aggrDecl );
}

void AssertionFixer_old::previsit( UnionDecl * aggrDecl ) {
	fixAssertions( aggrDecl->parameters, aggrDecl );
}*/

struct CheckOperatorTypes_old final {
	void previsit( ObjectDecl * );
};

void CheckOperatorTypes_old::previsit( ObjectDecl * object ) {
	// ensure that operator names only apply to functions or function pointers
	if ( CodeGen::isOperator( object->name ) && ! dynamic_cast< FunctionType * >( object->type->stripDeclarator() ) ) {
		SemanticError( object->location, toCString( "operator ", object->name.c_str(), " is not a function or function pointer." )  );
	}
}

struct FixUniqueIds_old final {
	void previsit( DeclarationWithType * decl ) {
		decl->fixUniqueId();
	}
};

//void FixUniqueIds_old::previsit( DeclarationWithType * decl ) {
//	decl->fixUniqueId();
//}


} // namespace

void validateType( Type *type, const Indexer *indexer ) {
	PassVisitor<EnumAndPointerDecay_old> epc;
	PassVisitor<LinkReferenceToTypes_old> lrt( indexer );
	PassVisitor<TraitExpander_old> te;
	PassVisitor<AssertionFixer_old> af;
	PassVisitor<CheckOperatorTypes_old> cot;
	PassVisitor<FixUniqueIds_old> fui;
	type->accept( epc );
	type->accept( lrt );
	type->accept( te );
	type->accept( af );
	type->accept( cot );
	type->accept( fui );
}

void decayEnumsAndPointers( std::list< Declaration * > & translationUnit ) {
	PassVisitor<EnumAndPointerDecay_old> epc;
	acceptAll( translationUnit, epc );
}

void linkReferenceToTypes( std::list< Declaration * > & translationUnit ) {
	PassVisitor<LinkReferenceToTypes_old> lrt( nullptr );
	acceptAll( translationUnit, lrt ); // must happen before autogen, because sized flag needs to propagate to generated functions
}

void decayForallPointers( std::list< Declaration * > & translationUnit ) {
	PassVisitor<TraitExpander_old> te;
	acceptAll( translationUnit, te );
	PassVisitor<AssertionFixer_old> af;
	acceptAll( translationUnit, af );
	PassVisitor<CheckOperatorTypes_old> cot;
	acceptAll( translationUnit, cot );
	PassVisitor<FixUniqueIds_old> fui;
	acceptAll( translationUnit, fui );
}


} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
