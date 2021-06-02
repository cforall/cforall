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
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Mar 16 15:44:27 2017
// Update Count     : 3
//

#include <utility>                      // for pair

#include "GenPoly.h"                    // for mangleType, TyVarMap, alignof...
#include "GenPoly/ErasableScopedMap.h"  // for ErasableScopedMap<>::const_it...
#include "ScrubTyVars.h"
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
} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
