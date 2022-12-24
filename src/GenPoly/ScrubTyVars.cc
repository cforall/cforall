//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ScrubTyVars.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Dec  7 17:01:00 2022
// Update Count     : 6
//

#include <utility>                      // for pair

#include "AST/Pass.hpp"
#include "GenPoly.h"                    // for mangleType, TyVarMap, alignof...
#include "GenPoly/ErasableScopedMap.h"  // for ErasableScopedMap<>::const_it...
#include "ScrubTyVars.h"
#include "SymTab/Mangler.h"             // for mangleType
#include "SynTree/Declaration.h"        // for TypeDecl, TypeDecl::Data, Typ...
#include "SynTree/Expression.h"         // for Expression (ptr only), NameExpr
#include "SynTree/Mutator.h"            // for Mutator
#include "SynTree/Type.h"               // for PointerType, TypeInstType, Type

namespace GenPoly {
	Type * ScrubTyVars::postmutate( TypeInstType * typeInst ) {
		if ( ! tyVars ) {
			if ( typeInst->get_isFtype() ) {
				delete typeInst;
				return new PointerType( Type::Qualifiers(), new FunctionType( Type::Qualifiers(), true ) );
			} else {
				PointerType * ret = new PointerType( Type::Qualifiers(), new VoidType( typeInst->get_qualifiers() ) );
				delete typeInst;
				return ret;
			}
		}

		TyVarMap::const_iterator tyVar = tyVars->find( typeInst->name );
		if ( tyVar != tyVars->end() ) {
			switch ( tyVar->second.kind ) {
			  case TypeDecl::Dtype:
			  case TypeDecl::Ttype:
				{
					PointerType * ret = new PointerType( Type::Qualifiers(), new VoidType( typeInst->get_qualifiers() ) );
					delete typeInst;
					return ret;
				}
			  case TypeDecl::Ftype:
				delete typeInst;
				return new PointerType( Type::Qualifiers(), new FunctionType( Type::Qualifiers(), true ) );
			  default:
				assertf(false, "Unhandled tyvar kind: %d", tyVar->second.kind);
			} // switch
		} // if
		return typeInst;
	}

	Type * ScrubTyVars::mutateAggregateType( Type * ty ) {
		if ( shouldScrub( ty ) ) {
			PointerType * ret = new PointerType( Type::Qualifiers(), new VoidType( ty->get_qualifiers() ) );
			delete ty;
			return ret;
		}
		return ty;
	}

	Type * ScrubTyVars::postmutate( StructInstType * structInst ) {
		return mutateAggregateType( structInst );
	}

	Type * ScrubTyVars::postmutate( UnionInstType * unionInst ) {
		return mutateAggregateType( unionInst );
	}

	void ScrubTyVars::primeBaseScrub( Type * type ) {
		// need to determine whether type needs to be scrubbed to determine whether
		// automatic recursion is necessary
		if ( Type * t = shouldScrub( type ) ) {
			visit_children = false;
			GuardValue( dynType );
			dynType = t;
		}
	}

	Expression * ScrubTyVars::postmutate( SizeofExpr * szeof ) {
		// sizeof( T ) => _sizeof_T parameter, which is the size of T
		if ( dynType ) {
			Expression *expr = new NameExpr( sizeofName( mangleType( dynType ) ) );
			return expr;
		} // if
		return szeof;
	}

	Expression * ScrubTyVars::postmutate( AlignofExpr * algnof ) {
		// alignof( T ) => _alignof_T parameter, which is the alignment of T
		if ( dynType ) {
			Expression *expr = new NameExpr( alignofName( mangleType( dynType ) ) );
			return expr;
		} // if
		return algnof;
	}

	Type * ScrubTyVars::postmutate( PointerType * pointer ) {
		if ( dynType ) {
			Type * ret = dynType->acceptMutator( *visitor );
			ret->get_qualifiers() |= pointer->get_qualifiers();
			pointer->base = nullptr;
			delete pointer;
			return ret;
		}
		return pointer;
	}

namespace {

struct ScrubTypeVars :
	public ast::WithGuards,
	public ast::WithShortCircuiting,
	public ast::WithVisitorRef<ScrubTypeVars> {

	ScrubTypeVars( ScrubMode m, TypeVarMap const * tv ) :
			mode ( m ), typeVars( tv ) {}

	void previsit( ast::TypeInstType const * ) { visit_children = false; }
	void previsit( ast::StructInstType const * ) { visit_children = false; }
	void previsit( ast::UnionInstType const * ) { visit_children = false; }
	void previsit( ast::SizeofExpr const * expr ) { primeBaseScrub( expr->type ); }
	void previsit( ast::AlignofExpr const * expr ) { primeBaseScrub( expr->type ); }
	void previsit( ast::PointerType const * type ) { primeBaseScrub( type->base ); }

	ast::Type const * postvisit( ast::TypeInstType const * type );
	ast::Type const * postvisit( ast::StructInstType const * type );
	ast::Type const * postvisit( ast::UnionInstType const * type );
	ast::Expr const * postvisit( ast::SizeofExpr const * expr );
	ast::Expr const * postvisit( ast::AlignofExpr const * expr );
	ast::Type const * postvisit( ast::PointerType const * type );

private:
	ScrubMode const mode;
	/// Type varriables to scrub.
	TypeVarMap const * const typeVars;
	/// Value cached by primeBaseScrub.
	ast::Type const * dynType = nullptr;

	/// Returns the type if it should be scrubbed, nullptr otherwise.
	ast::Type const * shouldScrub( ast::Type const * type ) {
		switch ( mode ) {
		case ScrubMode::FromMap:
			return isPolyType( type, *typeVars );
		case ScrubMode::DynamicFromMap:
			return isDynType( type, *typeVars );
		case ScrubMode::All:
			return isPolyType( type );
		default:
			assertf( false, "Invalid ScrubMode in shouldScrub." );
			throw;
		}
	}

	void primeBaseScrub( ast::Type const * type ) {
		// Need to determine whether type needs to be scrubbed to
		// determine whether automatic recursion is necessary.
		if ( ast::Type const * t = shouldScrub( type ) ) {
			visit_children = false;
			GuardValue( dynType ) = t;
		}
	}

	ast::Type const * postvisitAggregateType(
			ast::BaseInstType const * type ) {
		if ( !shouldScrub( type ) ) return type;
		return new ast::PointerType( new ast::VoidType( type->qualifiers ) );
	}
};

ast::Type const * ScrubTypeVars::postvisit( ast::TypeInstType const * type ) {
	// This implies that mode == ScrubMode::All.
	if ( !typeVars ) {
		if ( ast::TypeDecl::Ftype == type->kind ) {
			return new ast::PointerType(
				new ast::FunctionType( ast::FixedArgs ) );
		} else {
			return new ast::PointerType(
				new ast::VoidType( type->qualifiers ) );
		}
	}

	auto typeVar = typeVars->find( *type );
	if ( typeVar == typeVars->end() ) {
		return type;
	}

	switch ( typeVar->second.kind ) {
	case ::TypeDecl::Dtype:
	case ::TypeDecl::Ttype:
		return new ast::PointerType(
			new ast::VoidType( type->qualifiers ) );
	case ::TypeDecl::Ftype:
		return new ast::PointerType(
			new ast::FunctionType( ast::VariableArgs ) );
	default:
		assertf( false,
			"Unhandled type variable kind: %d", typeVar->second.kind );
		throw; // Just in case the assert is removed, stop here.
	}
}

ast::Type const * ScrubTypeVars::postvisit( ast::StructInstType const * type ) {
	return postvisitAggregateType( type );
}

ast::Type const * ScrubTypeVars::postvisit( ast::UnionInstType const * type ) {
	return postvisitAggregateType( type );
}

ast::Expr const * ScrubTypeVars::postvisit( ast::SizeofExpr const * expr ) {
	// sizeof( T ) becomes the _sizeof_T parameter.
	if ( dynType ) {
		return new ast::NameExpr( expr->location,
			sizeofName( Mangle::mangleType( dynType ) ) );
	} else {
		return expr;
	}
}

ast::Expr const * ScrubTypeVars::postvisit( ast::AlignofExpr const * expr ) {
	// alignof( T ) becomes the _alignof_T parameter.
	if ( dynType ) {
		return new ast::NameExpr( expr->location,
			alignofName( Mangle::mangleType( dynType ) ) );
	} else {
		return expr;
	}
}

ast::Type const * ScrubTypeVars::postvisit( ast::PointerType const * type ) {
	if ( dynType ) {
		ast::Type * ret = ast::mutate( dynType->accept( *visitor ) );
		ret->qualifiers |= type->qualifiers;
		return ret;
	} else {
		return type;
	}
}

} // namespace

const ast::Node * scrubTypeVarsBase(
		const ast::Node * node, const TypeVarMap * typeVars, ScrubMode mode ) {
	if ( ScrubMode::All == mode ) {
		assert( nullptr == typeVars );
	} else {
		assert( nullptr != typeVars );
	}
	ast::Pass<ScrubTypeVars> visitor( mode, typeVars );
	return node->accept( visitor );
}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
