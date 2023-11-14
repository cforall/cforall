//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CastCost.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 06:57:43 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Oct  4 15:00:00 2019
// Update Count     : 9
//

#include "CastCost.hpp"

#include <cassert>                       // for assert

#include "AST/Print.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"
#include "ConversionCost.h"              // for ConversionCost
#include "Cost.h"                        // for Cost, Cost::infinity
#include "ResolvExpr/ConversionCost.h"   // for conversionCost
#include "ResolvExpr/PtrsCastable.hpp"   // for ptrsCastable
#include "ResolvExpr/Unify.h"            // for typesCompatibleIgnoreQualifiers

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace ResolvExpr {

namespace {
	struct CastCost : public ConversionCost {
		using ConversionCost::previsit;
		using ConversionCost::postvisit;

		CastCost(
			const ast::Type * dst, bool srcIsLvalue, const ast::SymbolTable & symtab,
			const ast::TypeEnvironment & env, CostCalculation costFunc )
		: ConversionCost( dst, srcIsLvalue, symtab, env, costFunc ) {}

		void postvisit( const ast::BasicType * basicType ) {
			auto ptr = dynamic_cast< const ast::PointerType * >( dst );
			if ( ptr && basicType->isInteger() ) {
				// needed for, e.g. unsigned long => void *
				cost = Cost::unsafe;
			} else {
				cost = conversionCost( basicType, dst, srcIsLvalue, symtab, env );
			}
		}

		void postvisit( const ast::PointerType * pointerType ) {
			if ( auto ptr = dynamic_cast< const ast::PointerType * >( dst ) ) {
				if (
					pointerType->qualifiers <= ptr->qualifiers
					&& typesCompatibleIgnoreQualifiers( pointerType->base, ptr->base, env )
				) {
					cost = Cost::safe;
				} else {
					ast::TypeEnvironment newEnv{ env };
					if ( auto wParams = pointerType->base.as< ast::FunctionType >() ) {
						newEnv.add( wParams->forall );
					}
					int castResult = ptrsCastable( pointerType->base, ptr->base, symtab, newEnv );
					if ( castResult > 0 ) {
						cost = Cost::safe;
					} else if ( castResult < 0 ) {
						cost = Cost::infinity;
					}
				}
			} else if ( auto basic = dynamic_cast< const ast::BasicType * >( dst ) ) {
				if ( basic->isInteger() ) {
					// necessary for, e.g. void * => unsigned long
					cost = Cost::unsafe;
				}
			}
		}
	};

} // anonymous namespace

Cost castCost(
	const ast::Type * src, const ast::Type * dst, bool srcIsLvalue,
	const ast::SymbolTable & symtab, const ast::TypeEnvironment & env
) {
	if ( auto typeInst = dynamic_cast< const ast::TypeInstType * >( dst ) ) {
		if ( const ast::EqvClass * eqvClass = env.lookup( *typeInst ) ) {
			// check cast cost against bound type, if present
			if ( eqvClass->bound ) {
				return castCost( src, eqvClass->bound, srcIsLvalue, symtab, env );
			} else {
				return Cost::infinity;
			}
		} else if ( const ast::NamedTypeDecl * named = symtab.lookupType( typeInst->name ) ) {
			// all typedefs should be gone by now
			auto type = strict_dynamic_cast< const ast::TypeDecl * >( named );
			if ( type->base ) {
				return castCost( src, type->base, srcIsLvalue, symtab, env ) + Cost::safe;
			}
		}
	}

	PRINT(
		std::cerr << "castCost ::: src is ";
		ast::print( std::cerr, src );
		std::cerr << std::endl << "dest is ";
		ast::print( std::cerr, dst );
		std::cerr << std::endl << "env is" << std::endl;
		ast::print( std::cerr, env, 2 );
	)

	if ( typesCompatibleIgnoreQualifiers( src, dst, env ) ) {
		PRINT( std::cerr << "compatible!" << std::endl; )
		if (dynamic_cast<const ast::ZeroType *>(dst) || dynamic_cast<const ast::OneType *>(dst)) {
			return Cost::spec;
		}
		return Cost::zero;
	} else if ( dynamic_cast< const ast::VoidType * >( dst ) ) {
		return Cost::safe;
	} else if ( auto refType = dynamic_cast< const ast::ReferenceType * >( dst ) ) {
		PRINT( std::cerr << "conversionCost: dest is reference" << std::endl; )
		return convertToReferenceCost(
			src, refType, srcIsLvalue, symtab, env, ptrsCastable );
	} else {
		ast::Pass< CastCost > converter(
			dst, srcIsLvalue, symtab, env, castCost );
		src->accept( converter );
		return converter.core.cost;
	}
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
