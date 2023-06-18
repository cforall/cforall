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
#include "ResolvExpr/TypeEnvironment.h"  // for TypeEnvironment, EqvClass
#include "ResolvExpr/typeops.h"          // for ptrsCastable
#include "ResolvExpr/Unify.h"            // for typesCompatibleIgnoreQualifiers
#include "SymTab/Indexer.h"              // for Indexer
#include "SynTree/Declaration.h"         // for TypeDecl, NamedTypeDecl
#include "SynTree/Type.h"                // for PointerType, Type, TypeInstType

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace ResolvExpr {
	struct CastCost_old : public ConversionCost {
	  public:
		CastCost_old( const Type * dest, bool srcIsLvalue,
			const SymTab::Indexer &indexer, const TypeEnvironment &env, CostFunction costFunc );

		using ConversionCost::previsit;
		using ConversionCost::postvisit;
		void postvisit( const BasicType * basicType );
		void postvisit( const PointerType * pointerType );
	};

	Cost castCost( const Type * src, const Type * dest, bool srcIsLvalue,
			const SymTab::Indexer &indexer, const TypeEnvironment &env ) {
		if ( const TypeInstType * destAsTypeInst = dynamic_cast< const TypeInstType * >( dest ) ) {
			if ( const EqvClass * eqvClass = env.lookup( destAsTypeInst->name ) ) {
				if ( eqvClass->type ) {
					return castCost( src, eqvClass->type, srcIsLvalue, indexer, env );
				} else {
					return Cost::infinity;
				}
			} else if ( const NamedTypeDecl * namedType = indexer.lookupType( destAsTypeInst->name ) ) {
				// all typedefs should be gone by this point
				const TypeDecl * type = strict_dynamic_cast< const TypeDecl * >( namedType );
				if ( type->base ) {
					return castCost( src, type->base, srcIsLvalue, indexer, env ) + Cost::safe;
				} // if
			} // if
		} // if

		PRINT(
			std::cerr << "castCost ::: src is ";
			src->print( std::cerr );
			std::cerr << std::endl << "dest is ";
			dest->print( std::cerr );
			std::cerr << std::endl << "env is" << std::endl;
			env.print( std::cerr, 8 );
		)

		if ( typesCompatibleIgnoreQualifiers( src, dest, indexer, env ) ) {
			PRINT( std::cerr << "compatible!" << std::endl; )
			return Cost::zero;
		} else if ( dynamic_cast< const VoidType * >( dest ) ) {
			return Cost::safe;
		} else if ( const ReferenceType * refType = dynamic_cast< const ReferenceType * > ( dest ) ) {
			PRINT( std::cerr << "conversionCost: dest is reference" << std::endl; )
			return convertToReferenceCost( src, refType, srcIsLvalue, indexer, env, [](const Type * t1, const Type * t2, const SymTab::Indexer & indexer, const TypeEnvironment & env ) {
				return ptrsCastable( t1, t2, env, indexer );
			});
		} else {
			PassVisitor<CastCost_old> converter(
				dest, srcIsLvalue, indexer, env,
				(Cost (*)( const Type *, const Type *, bool, const SymTab::Indexer &, const TypeEnvironment & ))
					castCost );
			src->accept( converter );
			if ( converter.pass.get_cost() == Cost::infinity ) {
				return Cost::infinity;
			} else {
				// xxx - why are we adding cost 0 here?
				return converter.pass.get_cost() + Cost::zero;
			} // if
		} // if
	}

	CastCost_old::CastCost_old( const Type * dest, bool srcIsLvalue,
			const SymTab::Indexer &indexer, const TypeEnvironment &env, CostFunction costFunc )
		: ConversionCost( dest, srcIsLvalue, indexer, env, costFunc ) {
	}

	void CastCost_old::postvisit( const BasicType * basicType ) {
		const PointerType * destAsPointer = dynamic_cast< const PointerType * >( dest );
		if ( destAsPointer && basicType->isInteger() ) {
			// necessary for, e.g. unsigned long => void *
			cost = Cost::unsafe;
		} else {
			cost = conversionCost( basicType, dest, srcIsLvalue, indexer, env );
		} // if
	}

	void CastCost_old::postvisit( const PointerType * pointerType ) {
		if ( const PointerType * destAsPtr = dynamic_cast< const PointerType * >( dest ) ) {
			if ( pointerType->tq <= destAsPtr->tq && typesCompatibleIgnoreQualifiers( pointerType->base, destAsPtr->base, indexer, env ) ) {
				cost = Cost::safe;
			} else {
				TypeEnvironment newEnv( env );
				newEnv.add( pointerType->forall );
				newEnv.add( pointerType->base->forall );
				int castResult = ptrsCastable( pointerType->base, destAsPtr->base, newEnv, indexer );
				if ( castResult > 0 ) {
					cost = Cost::safe;
				} else if ( castResult < 0 ) {
					cost = Cost::infinity;
				} // if
			} // if
		} else if ( const BasicType * destAsBasic = dynamic_cast< const BasicType * >( dest ) ) {
			if ( destAsBasic->isInteger() ) {
				// necessary for, e.g. void * => unsigned long
				cost = Cost::unsafe;
			} // if
		}
	}

namespace {
	struct CastCost_new : public ConversionCost_new {
		using ConversionCost_new::previsit;
		using ConversionCost_new::postvisit;

		CastCost_new(
			const ast::Type * dst, bool srcIsLvalue, const ast::SymbolTable & symtab,
			const ast::TypeEnvironment & env, CostCalculation costFunc )
		: ConversionCost_new( dst, srcIsLvalue, symtab, env, costFunc ) {}

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

	#warning For overload resolution between the two versions.
	int localPtrsCastable(const ast::Type * t1, const ast::Type * t2,
			const ast::SymbolTable & symtab, const ast::TypeEnvironment & env ) {
		return ptrsCastable( t1, t2, symtab, env );
	}
	Cost localCastCost(
		const ast::Type * src, const ast::Type * dst, bool srcIsLvalue,
		const ast::SymbolTable & symtab, const ast::TypeEnvironment & env
	) { return castCost( src, dst, srcIsLvalue, symtab, env ); }
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
		#warning cast on ptrsCastable artifact of having two functions, remove when port done
		return convertToReferenceCost(
			src, refType, srcIsLvalue, symtab, env, localPtrsCastable );
	} else {
		#warning cast on castCost artifact of having two functions, remove when port done
		ast::Pass< CastCost_new > converter(
			dst, srcIsLvalue, symtab, env, localCastCost );
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
