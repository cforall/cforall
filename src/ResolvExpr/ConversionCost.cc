//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ConversionCost.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 07:06:19 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jul 29 16:11:00 2020
// Update Count     : 28
//

#include "ConversionCost.h"

#include <cassert>                       // for assert
#include <list>                          // for list, list<>::const_iterator
#include <string>                        // for operator==, string

#include "ResolvExpr/Cost.h"             // for Cost
#include "ResolvExpr/TypeEnvironment.h"  // for EqvClass, TypeEnvironment
#include "ResolvExpr/Unify.h"
#include "SymTab/Indexer.h"              // for Indexer
#include "SynTree/Declaration.h"         // for TypeDecl, NamedTypeDecl
#include "SynTree/Type.h"                // for Type, BasicType, TypeInstType
#include "typeops.h"                     // for typesCompatibleIgnoreQualifiers


namespace ResolvExpr {
#if 0
	const Cost Cost::zero =      Cost{  0,  0,  0,  0,  0,  0,  0 };
	const Cost Cost::infinity =  Cost{ -1, -1, -1, -1, -1,  1, -1 };
	const Cost Cost::unsafe =    Cost{  1,  0,  0,  0,  0,  0,  0 };
	const Cost Cost::poly =      Cost{  0,  1,  0,  0,  0,  0,  0 };
	const Cost Cost::safe =      Cost{  0,  0,  1,  0,  0,  0,  0 };
	const Cost Cost::sign =      Cost{  0,  0,  0,  1,  0,  0,  0 };
	const Cost Cost::var =       Cost{  0,  0,  0,  0,  1,  0,  0 };
	const Cost Cost::spec =      Cost{  0,  0,  0,  0,  0, -1,  0 };
	const Cost Cost::reference = Cost{  0,  0,  0,  0,  0,  0,  1 };
#endif

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

	Cost conversionCost( const Type * src, const Type * dest, bool srcIsLvalue,
			const SymTab::Indexer &indexer, const TypeEnvironment &env ) {
		if ( const TypeInstType * destAsTypeInst = dynamic_cast< const TypeInstType * >( dest ) ) {
			PRINT( std::cerr << "type inst " << destAsTypeInst->name; )
			if ( const EqvClass * eqvClass = env.lookup( destAsTypeInst->name ) ) {
				if ( eqvClass->type ) {
					return conversionCost( src, eqvClass->type, srcIsLvalue, indexer, env );
				} else {
					return Cost::infinity;
				}
			} else if ( const NamedTypeDecl * namedType = indexer.lookupType( destAsTypeInst->name ) ) {
				PRINT( std::cerr << " found" << std::endl; )
				const TypeDecl * type = dynamic_cast< const TypeDecl * >( namedType );
				// all typedefs should be gone by this point
				assert( type );
				if ( type->base ) {
					return conversionCost( src, type->base, srcIsLvalue, indexer, env )
						+ Cost::safe;
				} // if
			} // if
			PRINT( std::cerr << " not found" << std::endl; )
		} // if
		PRINT(
			std::cerr << "src is ";
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
			return convertToReferenceCost( src, refType, srcIsLvalue, indexer, env, [](const Type * const t1, const Type * t2, const SymTab::Indexer &, const TypeEnvironment & env ){
				return ptrsAssignable( t1, t2, env );
			});
		} else {
			PassVisitor<ConversionCost> converter(
				dest, srcIsLvalue, indexer, env,
				(Cost (*)(const Type *, const Type *, bool, const SymTab::Indexer&, const TypeEnvironment&))
					conversionCost );
			src->accept( converter );
			if ( converter.pass.get_cost() == Cost::infinity ) {
				return Cost::infinity;
			} else {
				return converter.pass.get_cost() + Cost::zero;
			} // if
		} // if
	}

	static Cost convertToReferenceCost( const Type * src, const Type * dest, bool srcIsLvalue,
			int diff, const SymTab::Indexer & indexer, const TypeEnvironment & env, PtrsFunction func ) {
		PRINT( std::cerr << "convert to reference cost... diff " << diff << " " << src << " / " << dest << std::endl; )
		if ( diff > 0 ) {
			// TODO: document this
			Cost cost = convertToReferenceCost(
				strict_dynamic_cast< const ReferenceType * >( src )->base, dest, srcIsLvalue,
				diff-1, indexer, env, func );
			cost.incReference();
			return cost;
		} else if ( diff < -1 ) {
			// TODO: document this
			Cost cost = convertToReferenceCost(
				src, strict_dynamic_cast< const ReferenceType * >( dest )->base, srcIsLvalue,
				diff+1, indexer, env, func );
			cost.incReference();
			return cost;
		} else if ( diff == 0 ) {
			const ReferenceType * srcAsRef = dynamic_cast< const ReferenceType * >( src );
			const ReferenceType * destAsRef = dynamic_cast< const ReferenceType * >( dest );
			if ( srcAsRef && destAsRef ) { // pointer-like conversions between references
				PRINT( std::cerr << "converting between references" << std::endl; )
				Type::Qualifiers tq1 = srcAsRef->base->tq;
				Type::Qualifiers tq2 = destAsRef->base->tq;
				if ( tq1 <= tq2 && typesCompatibleIgnoreQualifiers( srcAsRef->base, destAsRef->base, indexer, env ) ) {
					PRINT( std::cerr << " :: compatible and good qualifiers" << std::endl; )
					if ( tq1 == tq2 ) {
						// types are the same
						return Cost::zero;
					} else {
						// types are the same, except otherPointer has more qualifiers
						return Cost::safe;
					}
				} else {  // xxx - this discards reference qualifiers from consideration -- reducing qualifiers is a safe conversion; is this right?
					int assignResult = func( srcAsRef->base, destAsRef->base, indexer, env );
					PRINT( std::cerr << "comparing references: " << assignResult << " " << srcAsRef << " " << destAsRef << std::endl; )
					if ( assignResult > 0 ) {
						return Cost::safe;
					} else if ( assignResult < 0 ) {
						return Cost::unsafe;
					} // if
				} // if
			} else {
				PRINT( std::cerr << "reference to rvalue conversion" << std::endl; )
				PassVisitor<ConversionCost> converter(
					dest, srcIsLvalue, indexer, env,
					(Cost (*)(const Type *, const Type *, bool, const SymTab::Indexer&, const TypeEnvironment&))
						conversionCost );
				src->accept( converter );
				return converter.pass.get_cost();
			} // if
		} else {
			const ReferenceType * destAsRef = dynamic_cast< const ReferenceType * >( dest );
			assert( diff == -1 && destAsRef );
			PRINT( std::cerr << "dest is: " << dest << " / src is: " << src << std::endl; )
			if ( typesCompatibleIgnoreQualifiers( src, destAsRef->base, indexer, env ) ) {
				PRINT( std::cerr << "converting compatible base type" << std::endl; )
				if ( srcIsLvalue ) {
					PRINT(
						std::cerr << "lvalue to reference conversion" << std::endl;
						std::cerr << src << " => " << destAsRef << std::endl;
					)
					// lvalue-to-reference conversion:  cv lvalue T => cv T &
					if ( src->tq == destAsRef->base->tq ) {
						return Cost::reference; // cost needs to be non-zero to add cast
					} if ( src->tq < destAsRef->base->tq ) {
						return Cost::safe; // cost needs to be higher than previous cast to differentiate adding qualifiers vs. keeping same
					} else {
						return Cost::unsafe;
					} // if
				} else if ( destAsRef->base->get_const() ) {
					PRINT( std::cerr << "rvalue to const ref conversion" << std::endl; )
					// rvalue-to-const-reference conversion: T => const T &
					return Cost::safe;
				} else {
					PRINT( std::cerr << "rvalue to non-const reference conversion" << std::endl; )
					// rvalue-to-reference conversion: T => T &
					return Cost::unsafe;
				} // if
			} // if
			PRINT( std::cerr << "attempting to convert from incompatible base type -- fail" << std::endl; )
		}
		return Cost::infinity;
	}

	Cost convertToReferenceCost( const Type * src, const ReferenceType * dest, bool srcIsLvalue,
			const SymTab::Indexer & indexer, const TypeEnvironment & env, PtrsFunction func ) {
		int sdepth = src->referenceDepth(), ddepth = dest->referenceDepth();
		Cost cost = convertToReferenceCost( src, dest, srcIsLvalue, sdepth-ddepth, indexer, env, func );
		PRINT( std::cerr << "convertToReferenceCost result: " << cost << std::endl; )
		return cost;
	}

	ConversionCost::ConversionCost( const Type * dest, bool srcIsLvalue, const SymTab::Indexer &indexer, const TypeEnvironment &env, CostFunction costFunc )
		: dest( dest ), srcIsLvalue( srcIsLvalue ), indexer( indexer ), cost( Cost::infinity ), env( env ), costFunc( costFunc ) {
	}

	// GENERATED START, DO NOT EDIT
	// GENERATED BY BasicTypes-gen.cc
	/* EXTENDED INTEGRAL RANK HIERARCHY (root to leaves)
	                         _Bool
	char                signed char         unsigned char
	          signed short int         unsigned short int
	          signed int               unsigned int
	          signed long int          unsigned long int
	          signed long long int     unsigned long long int
	          __int128                 unsigned __int128
	          _Float16                 _Float16 _Complex
	          _Float32                 _Float32 _Complex
	          float                    float _Complex
	          _Float32x                _Float32x _Complex
	          _Float64                 _Float64 _Complex
	          double                   double _Complex
	          _Float64x                _Float64x _Complex
	                     __float80
	          _Float128                _Float128 _Complex
	                    __float128
	          long double              long double _Complex
	          _Float128x               _Float128x _Complex
	*/
	// GENERATED END

	// GENERATED START, DO NOT EDIT
	// GENERATED BY BasicTypes-gen.cc
	static const int costMatrix[BasicType::NUMBER_OF_BASIC_TYPES][BasicType::NUMBER_OF_BASIC_TYPES] = { // path length from root to node
		/*               B    C   SC   UC   SI  SUI    I   UI   LI  LUI  LLI LLUI   IB  UIB  _FH  _FH   _F  _FC    F   FC  _FX _FXC   FD _FDC    D   DC F80X_FDXC  F80  _FB_FLDC   FB   LD  LDC _FBX_FLDXC */
		/*      B */ {   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  14,  15,  15,  16,  17,  16,  18,  17, },
		/*      C */ {  -1,   0,   1,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  14,  15,  16,  15,  17,  16, },
		/*     SC */ {  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  14,  15,  16,  15,  17,  16, },
		/*     UC */ {  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  14,  15,  16,  15,  17,  16, },
		/*     SI */ {  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  15,  14,  16,  15, },
		/*    SUI */ {  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  15,  14,  16,  15, },
		/*      I */ {  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  14,  13,  15,  14, },
		/*     UI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  14,  13,  15,  14, },
		/*     LI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  13,  12,  14,  13, },
		/*    LUI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  13,  12,  14,  13, },
		/*    LLI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  12,  11,  13,  12, },
		/*   LLUI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  12,  11,  13,  12, },
		/*     IB */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  11,  10,  12,  11, },
		/*    UIB */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  11,  10,  12,  11, },
		/*    _FH */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,  10,   9,  11,  10, },
		/*    _FH */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,  -1,   4,  -1,   5,  -1,   6,  -1,  -1,   7,  -1,  -1,   8,  -1,   9, },
		/*     _F */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   9,   8,  10,   9, },
		/*    _FC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,  -1,   4,  -1,   5,  -1,  -1,   6,  -1,  -1,   7,  -1,   8, },
		/*      F */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   8,   7,   9,   8, },
		/*     FC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,  -1,   4,  -1,  -1,   5,  -1,  -1,   6,  -1,   7, },
		/*    _FX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   7,   6,   8,   7, },
		/*   _FXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,  -1,  -1,   4,  -1,  -1,   5,  -1,   6, },
		/*     FD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   6,   5,   7,   6, },
		/*   _FDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,  -1,   3,  -1,  -1,   4,  -1,   5, },
		/*      D */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   5,   4,   6,   5, },
		/*     DC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,  -1,   2,  -1,  -1,   3,  -1,   4, },
		/*   F80X */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   4,   3,   5,   4, },
		/*  _FDXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,   1,  -1,  -1,   2,  -1,   3, },
		/*    F80 */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   1,   0,   1,   2,   2,   3,   3,   4,   4, },
		/*    _FB */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3, },
		/*  _FLDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,   1,  -1,   2, },
		/*     FB */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   1,   0,   1,   2,   2,   3, },
		/*     LD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2, },
		/*    LDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1, },
		/*   _FBX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1, },
		/* _FLDXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0, },
	}; // costMatrix
	static const int maxIntCost = 15;
	// GENERATED END
	static_assert(
		sizeof(costMatrix)/sizeof(costMatrix[0][0]) == BasicType::NUMBER_OF_BASIC_TYPES * BasicType::NUMBER_OF_BASIC_TYPES,
		"Missing row in the cost matrix"
	);

	// GENERATED START, DO NOT EDIT
	// GENERATED BY BasicTypes-gen.cc
	static const int signMatrix[BasicType::NUMBER_OF_BASIC_TYPES][BasicType::NUMBER_OF_BASIC_TYPES] = { // number of sign changes in safe conversion
		/*               B    C   SC   UC   SI  SUI    I   UI   LI  LUI  LLI LLUI   IB  UIB  _FH  _FH   _F  _FC    F   FC  _FX _FXC   FD _FDC    D   DC F80X_FDXC  F80  _FB_FLDC   FB   LD  LDC _FBX_FLDXC */
		/*      B */ {   0,   0,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*      C */ {  -1,   0,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     SC */ {  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     UC */ {  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     SI */ {  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    SUI */ {  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*      I */ {  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     UI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     LI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    LUI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    LLI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*   LLUI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     IB */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    UIB */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    _FH */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    _FH */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*     _F */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    _FC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*      F */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     FC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*    _FX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*   _FXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*     FD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*   _FDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*      D */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     DC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*   F80X */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*  _FDXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*    F80 */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    _FB */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0, },
		/*  _FLDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*     FB */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0, },
		/*     LD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0, },
		/*    LDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0, },
		/*   _FBX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0, },
		/* _FLDXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0, },
	}; // signMatrix
	// GENERATED END
	static_assert(
		sizeof(signMatrix)/sizeof(signMatrix[0][0]) == BasicType::NUMBER_OF_BASIC_TYPES * BasicType::NUMBER_OF_BASIC_TYPES,
		"Missing row in the sign matrix"
	);

	void ConversionCost::postvisit( const VoidType * ) {
		cost = Cost::infinity;
	}

	// refactor for code resue
	void ConversionCost::conversionCostFromBasicToBasic(const BasicType * src, const BasicType * dest) {
		int tableResult = costMatrix[ src->kind ][ dest->kind ];
		if ( tableResult == -1 ) {
			cost = Cost::unsafe;
		} else {
			cost = Cost::zero;
			cost.incSafe( tableResult );
			cost.incSign( signMatrix[ src->kind ][ dest->kind ] );
		} // if
	} // ConversionCost::conversionCostFromBasicToBasic

	void ConversionCost::postvisit(const BasicType * basicType) {
		if ( const BasicType * destAsBasic = dynamic_cast< const BasicType * >( dest ) ) {
			conversionCostFromBasicToBasic(basicType, destAsBasic);
		} else if ( const EnumInstType * enumInst = dynamic_cast< const EnumInstType * >( dest ) ) {
			const EnumDecl * base_enum = enumInst->baseEnum;
			if ( const Type * base = base_enum->base ) {
				if ( const BasicType * enumBaseAstBasic = dynamic_cast< const BasicType *> (base) ) {
					conversionCostFromBasicToBasic(basicType, enumBaseAstBasic);
				} else {
					cost = Cost::infinity;
				} // if
			} else {
				cost = Cost::unsafe;
			} // if
		} // if
		// no cases for zero_t/one_t because it should not be possible to convert int, etc. to zero_t/one_t.
	}

	void ConversionCost::postvisit( const PointerType * pointerType ) {
		if ( const PointerType * destAsPtr = dynamic_cast< const PointerType * >( dest ) ) {
			PRINT( std::cerr << pointerType << " ===> " << destAsPtr << std::endl; )
			Type::Qualifiers tq1 = pointerType->base->tq;
			Type::Qualifiers tq2 = destAsPtr->base->tq;
			if ( tq1 <= tq2 && typesCompatibleIgnoreQualifiers( pointerType->base, destAsPtr->base, indexer, env ) ) {
				PRINT( std::cerr << " :: compatible and good qualifiers" << std::endl; )
				if ( tq1 == tq2 ) {
					// types are the same
					cost = Cost::zero;
				} else {
					// types are the same, except otherPointer has more qualifiers
					cost = Cost::safe;
				} // if
			} else {
				int assignResult = ptrsAssignable( pointerType->base, destAsPtr->base, env );
				PRINT( std::cerr << " :: " << assignResult << std::endl; )
				if ( assignResult > 0 && tq1 <= tq2 ) {
					// xxx - want the case where qualifiers are added to be more expensive than the case where qualifiers are the same. Is 1 safe vs. 2 safe correct?
					if ( tq1 == tq2 ) {
						cost = Cost::safe;
					} else if ( tq1 < tq2 ) {
						cost = Cost::safe+Cost::safe;
					}
				} else if ( assignResult < 0 ) {
					cost = Cost::unsafe;
				} // if
				// assignResult == 0 means Cost::Infinity
			} // if
			// case case for zero_t because it should not be possible to convert pointers to zero_t.
		} // if
	}

	void ConversionCost::postvisit( const ArrayType * ) {}

	void ConversionCost::postvisit( const ReferenceType * refType ) {
		// Note: dest can never be a reference, since it would have been caught in an earlier check
		assert( ! dynamic_cast< const ReferenceType * >( dest ) );
		// convert reference to rvalue: cv T1 & => T2
		// recursively compute conversion cost from T1 to T2.
		// cv can be safely dropped because of 'implicit dereference' behavior.
		cost = costFunc( refType->base, dest, srcIsLvalue, indexer, env );
		if ( refType->base->tq == dest->tq ) {
			cost.incReference();  // prefer exact qualifiers
		} else if ( refType->base->tq < dest->tq ) {
			cost.incSafe(); // then gaining qualifiers
		} else {
			cost.incUnsafe(); // lose qualifiers as last resort
		}
		PRINT( std::cerr << refType << " ==> " << dest << " " << cost << std::endl; )
	}

	void ConversionCost::postvisit( const FunctionType * ) {}

	void ConversionCost::postvisit( const EnumInstType * enumInst) {
		const EnumDecl * enumDecl = enumInst -> baseEnum;
		if ( const Type * enumType = enumDecl -> base ) { // if it is a typed enum
			cost = costFunc( enumType, dest, srcIsLvalue, indexer, env );
		} else {
			static Type::Qualifiers q;
			static BasicType integer( q, BasicType::SignedInt );
			cost = costFunc( &integer, dest, srcIsLvalue, indexer, env );  // safe if dest >= int
		} // if
		if ( cost < Cost::unsafe ) {
				cost.incSafe();
		} // if
	}

	void ConversionCost::postvisit( const TraitInstType * ) {}

	void ConversionCost::postvisit( const TypeInstType * inst ) {
		if ( const EqvClass * eqvClass = env.lookup( inst->name ) ) {
			cost = costFunc( eqvClass->type, dest, srcIsLvalue, indexer, env );
		} else if ( const TypeInstType * destAsInst = dynamic_cast< const TypeInstType * >( dest ) ) {
			if ( inst->name == destAsInst->name ) {
				cost = Cost::zero;
			}
		} else if ( const NamedTypeDecl * namedType = indexer.lookupType( inst->name ) ) {
			const TypeDecl * type = dynamic_cast< const TypeDecl * >( namedType );
			// all typedefs should be gone by this point
			assert( type );
			if ( type->base ) {
				cost = costFunc( type->base, dest, srcIsLvalue, indexer, env ) + Cost::safe;
			} // if
		} // if
	}

	void ConversionCost::postvisit( const TupleType * tupleType ) {
		Cost c = Cost::zero;
		if ( const TupleType * destAsTuple = dynamic_cast< const TupleType * >( dest ) ) {
			std::list< Type * >::const_iterator srcIt = tupleType->types.begin();
			std::list< Type * >::const_iterator destIt = destAsTuple->types.begin();
			while ( srcIt != tupleType->types.end() && destIt != destAsTuple->types.end() ) {
				Cost newCost = costFunc( * srcIt++, * destIt++, srcIsLvalue, indexer, env );
				if ( newCost == Cost::infinity ) {
					return;
				} // if
				c += newCost;
			} // while
			if ( destIt != destAsTuple->types.end() ) {
				cost = Cost::infinity;
			} else {
				cost = c;
			} // if
		} // if
	}

	void ConversionCost::postvisit( const VarArgsType * ) {
		if ( dynamic_cast< const VarArgsType * >( dest ) ) {
			cost = Cost::zero;
		}
	}

	void ConversionCost::postvisit( const ZeroType * ) {
		if ( dynamic_cast< const ZeroType * >( dest ) ) {
			cost = Cost::zero;
		} else if ( const BasicType * destAsBasic = dynamic_cast< const BasicType * >( dest ) ) {
			// copied from visit(BasicType *) for signed int, but +1 for safe conversions
			int tableResult = costMatrix[ BasicType::SignedInt ][ destAsBasic->kind ];
			if ( tableResult == -1 ) {
				cost = Cost::unsafe;
			} else {
				cost = Cost::zero;
				cost.incSafe( tableResult + 1 );
				cost.incSign( signMatrix[ BasicType::SignedInt ][ destAsBasic->kind ] );
			} // if
		} else if ( dynamic_cast< const PointerType * >( dest ) ) {
			cost = Cost::zero;
			cost.incSafe( maxIntCost + 2 ); // +1 for zero_t -> int, +1 for disambiguation
		} // if
	}

	void ConversionCost::postvisit( const OneType * ) {
		if ( dynamic_cast< const OneType * >( dest ) ) {
			cost = Cost::zero;
		} else if ( const BasicType * destAsBasic = dynamic_cast< const BasicType * >( dest ) ) {
			// copied from visit(BasicType *) for signed int, but +1 for safe conversions
			int tableResult = costMatrix[ BasicType::SignedInt ][ destAsBasic->kind ];
			if ( tableResult == -1 ) {
				cost = Cost::unsafe;
			} else {
				cost = Cost::zero;
				cost.incSafe( tableResult + 1 );
				cost.incSign( signMatrix[ BasicType::SignedInt ][ destAsBasic->kind ] );
			} // if
		} // if
	}

namespace {
	# warning For overload resolution between the two versions.
	int localPtrsAssignable(const ast::Type * t1, const ast::Type * t2,
			const ast::SymbolTable &, const ast::TypeEnvironment & env ) {
		return ptrsAssignable( t1, t2, env );
	}
	Cost localConversionCost(
		const ast::Type * src, const ast::Type * dst, bool srcIsLvalue,
		const ast::SymbolTable & symtab, const ast::TypeEnvironment & env
	) { return conversionCost( src, dst, srcIsLvalue, symtab, env ); }
}

Cost conversionCost(
	const ast::Type * src, const ast::Type * dst, bool srcIsLvalue,
	const ast::SymbolTable & symtab, const ast::TypeEnvironment & env
) {
	if ( const ast::TypeInstType * inst = dynamic_cast< const ast::TypeInstType * >( dst ) ) {
		if ( const ast::EqvClass * eqv = env.lookup( *inst ) ) {
			if ( eqv->bound ) {
				return conversionCost(src, eqv->bound, srcIsLvalue, symtab, env );
			} else {
				return Cost::infinity;
			}
		} else if ( const ast::NamedTypeDecl * named = symtab.lookupType( inst->name ) ) {
			const ast::TypeDecl * type = dynamic_cast< const ast::TypeDecl * >( named );
			assertf( type, "Unexpected typedef." );
			if ( type->base ) {
				return conversionCost( src, type->base, srcIsLvalue, symtab, env ) + Cost::safe;
			}
		}
	}
	if ( typesCompatibleIgnoreQualifiers( src, dst, symtab, env ) ) {
		return Cost::zero;
	} else if ( dynamic_cast< const ast::VoidType * >( dst ) ) {
		return Cost::safe;
	} else if ( const ast::ReferenceType * refType =
			 dynamic_cast< const ast::ReferenceType * >( dst ) ) {
		return convertToReferenceCost( src, refType, srcIsLvalue, symtab, env, localPtrsAssignable );
	} else {
		return ast::Pass<ConversionCost_new>::read( src, dst, srcIsLvalue, symtab, env, localConversionCost );
	}
}

static Cost convertToReferenceCost( const ast::Type * src, const ast::Type * dst, bool srcIsLvalue,
		int diff, const ast::SymbolTable & symtab, const ast::TypeEnvironment & env,
		PtrsCalculation func ) {
	if ( 0 < diff ) {
		Cost cost = convertToReferenceCost(
			strict_dynamic_cast< const ast::ReferenceType * >( src )->base, dst,
			srcIsLvalue, (diff - 1), symtab, env, func );
		cost.incReference();
		return cost;
	} else if ( diff < -1 ) {
		Cost cost = convertToReferenceCost(
			src, strict_dynamic_cast< const ast::ReferenceType * >( dst )->base,
			srcIsLvalue, (diff + 1), symtab, env, func );
		cost.incReference();
		return cost;
	} else if ( 0 == diff ) {
		const ast::ReferenceType * srcAsRef = dynamic_cast< const ast::ReferenceType * >( src );
		const ast::ReferenceType * dstAsRef = dynamic_cast< const ast::ReferenceType * >( dst );
		if ( srcAsRef && dstAsRef ) {
			ast::CV::Qualifiers tq1 = srcAsRef->base->qualifiers;
			ast::CV::Qualifiers tq2 = dstAsRef->base->qualifiers;
			if ( tq1 <= tq2 && typesCompatibleIgnoreQualifiers(
					srcAsRef->base, dstAsRef->base, symtab, env ) ) {
				if ( tq1 == tq2 ) {
					return Cost::zero;
				} else {
					return Cost::safe;
				}
			} else {
				int assignResult = func( srcAsRef->base, dstAsRef->base, symtab, env );
				if ( 0 < assignResult ) {
					return Cost::safe;
				} else if ( assignResult < 0 ) {
					return Cost::unsafe;
				}
			}
		} else {
			return ast::Pass<ConversionCost_new>::read( src, dst, srcIsLvalue, symtab, env, localConversionCost );
		}
	} else {
		assert( -1 == diff );
		const ast::ReferenceType * dstAsRef = dynamic_cast< const ast::ReferenceType * >( dst );
		assert( dstAsRef );
		if ( typesCompatibleIgnoreQualifiers( src, dstAsRef->base, symtab, env ) ) {
			if ( srcIsLvalue ) {
				if ( src->qualifiers == dstAsRef->base->qualifiers ) {
					return Cost::reference;
				} else if ( src->qualifiers < dstAsRef->base->qualifiers ) {
					return Cost::safe;
				} else {
					return Cost::unsafe;
				}
			} else if ( dstAsRef->base->is_const() ) {
				return Cost::safe;
			} else {
				return Cost::unsafe;
			}
		}
	}
	return Cost::infinity;
}

Cost convertToReferenceCost( const ast::Type * src, const ast::ReferenceType * dst,
		bool srcIsLvalue, const ast::SymbolTable & symtab, const ast::TypeEnvironment & env,
		PtrsCalculation func ) {
	int sdepth = src->referenceDepth(), ddepth = dst->referenceDepth();
	return convertToReferenceCost( src, dst, srcIsLvalue, sdepth - ddepth, symtab, env, func );
}

void ConversionCost_new::postvisit( const ast::VoidType * voidType ) {
	(void)voidType;
	cost = Cost::infinity;
}

void ConversionCost_new::conversionCostFromBasicToBasic( const ast::BasicType * src, const ast::BasicType* dest ) {
	int tableResult = costMatrix[ src->kind ][ dest->kind ];
	if ( tableResult == -1 ) {
		cost = Cost::unsafe;
	} else {
		cost = Cost::zero;
		cost.incSafe( tableResult );
		cost.incSign( signMatrix[ src->kind ][ dest->kind ] );
	}
}

void ConversionCost_new::postvisit( const ast::BasicType * basicType ) {
	if ( const ast::BasicType * dstAsBasic = dynamic_cast< const ast::BasicType * >( dst ) ) {
		conversionCostFromBasicToBasic( basicType, dstAsBasic );
	} else if ( const ast::EnumInstType * enumInst = dynamic_cast< const ast::EnumInstType * >( dst ) ) {
		const ast::EnumDecl * enumDecl = enumInst->base.get();
		if ( enumDecl->isTyped && !enumDecl->base.get() ) {
			cost = Cost::infinity; 
		} else if ( const ast::Type * enumType = enumDecl->base.get() ) {
			if ( const ast::BasicType * enumTypeAsBasic = dynamic_cast<const ast::BasicType *>(enumType) ) {
				conversionCostFromBasicToBasic( basicType, enumTypeAsBasic );
			} else {
				cost = Cost::infinity;
			}
		} else {
            cost = Cost::unsafe;
		}
	}
}

void ConversionCost_new::postvisit( const ast::PointerType * pointerType ) {
	if ( const ast::PointerType * dstAsPtr = dynamic_cast< const ast::PointerType * >( dst ) ) {
		ast::CV::Qualifiers tq1 = pointerType->base->qualifiers;
		ast::CV::Qualifiers tq2 = dstAsPtr->base->qualifiers;
		if ( tq1 <= tq2 && typesCompatibleIgnoreQualifiers(
				pointerType->base, dstAsPtr->base, symtab, env ) ) {
			if ( tq1 == tq2 ) {
				cost = Cost::zero;
			} else {
				cost = Cost::safe;
			}
		}
		/*
		else if ( const ast::FunctionType * dstFunc = dstAsPtr->base.as<ast::FunctionType>()) {
			if (const ast::FunctionType * srcFunc = pointerType->base.as<ast::FunctionType>()) {
				if (dstFunc->params.empty() && dstFunc->isVarArgs ) {
					cost = Cost::unsafe; // assign any function to variadic fptr
				}
			}
			else {
				ast::AssertionSet need, have; // unused
				ast::OpenVarSet open;
				env.extractOpenVars(open);
				ast::TypeEnvironment tenv = env;
				if ( unify(dstAsPtr->base, pointerType->base, tenv, need, have, open, symtab) ) {
					cost = Cost::safe;
				}
			}
			// else infinity
		}
		*/
		else {
			int assignResult = ptrsAssignable( pointerType->base, dstAsPtr->base, env );
			if ( 0 < assignResult && tq1 <= tq2 ) {
				if ( tq1 == tq2 ) {
					cost = Cost::safe;
				} else {
					cost = Cost::safe + Cost::safe;
				}
			} else if ( assignResult < 0 ) {
				cost = Cost::unsafe;
			} // else Cost::infinity
		}
	}
}

void ConversionCost_new::postvisit( const ast::ArrayType * arrayType ) {
	(void)arrayType;
}

void ConversionCost_new::postvisit( const ast::ReferenceType * refType ) {
	assert( nullptr == dynamic_cast< const ast::ReferenceType * >( dst ) );

	cost = costCalc( refType->base, dst, srcIsLvalue, symtab, env );
	if ( refType->base->qualifiers == dst->qualifiers ) {
		cost.incReference();
	} else if ( refType->base->qualifiers < dst->qualifiers ) {
		cost.incSafe();
	} else {
		cost.incUnsafe();
	}
}

void ConversionCost_new::postvisit( const ast::FunctionType * functionType ) {
	(void)functionType;
}

void ConversionCost_new::postvisit( const ast::EnumInstType * enumInstType ) {
	const ast::EnumDecl * baseEnum = enumInstType->base;
	if ( const ast::Type * baseType = baseEnum->base ) {
		costCalc( baseType, dst, srcIsLvalue, symtab, env );
	} else {
		static ast::ptr<ast::BasicType> integer = { new ast::BasicType( ast::BasicType::SignedInt ) };
		cost = costCalc( integer, dst, srcIsLvalue, symtab, env );
	}
	if ( cost < Cost::unsafe ) {
		cost.incSafe();
	}
}

void ConversionCost_new::postvisit( const ast::TraitInstType * traitInstType ) {
	(void)traitInstType;
}

void ConversionCost_new::postvisit( const ast::TypeInstType * typeInstType ) {
	if ( const ast::EqvClass * eqv = env.lookup( *typeInstType ) ) {
		cost = costCalc( eqv->bound, dst, srcIsLvalue, symtab, env );
	} else if ( const ast::TypeInstType * dstAsInst =
			dynamic_cast< const ast::TypeInstType * >( dst ) ) {
		if ( *typeInstType == *dstAsInst ) {
			cost = Cost::zero;
		}
	} else if ( const ast::NamedTypeDecl * namedType = symtab.lookupType( typeInstType->name ) ) {
		const ast::TypeDecl * type = dynamic_cast< const ast::TypeDecl * >( namedType );
		assertf( type, "Unexpected typedef.");
		if ( type->base ) {
			cost = costCalc( type->base, dst, srcIsLvalue, symtab, env ) + Cost::safe;
		}
	}
}

void ConversionCost_new::postvisit( const ast::TupleType * tupleType ) {
	Cost c = Cost::zero;
	if ( const ast::TupleType * dstAsTuple = dynamic_cast< const ast::TupleType * >( dst ) ) {
		auto srcIt = tupleType->types.begin();
		auto dstIt = dstAsTuple->types.begin();
		auto srcEnd = tupleType->types.end();
		auto dstEnd = dstAsTuple->types.end();
		while ( srcIt != srcEnd && dstIt != dstEnd ) {
			Cost newCost = costCalc( * srcIt++, * dstIt++, srcIsLvalue, symtab, env );
			if ( newCost == Cost::infinity ) {
				return;
			}
			c += newCost;
		}
		if ( dstIt != dstEnd ) {
			cost = Cost::infinity;
		} else {
			cost = c;
		}
	}
}

void ConversionCost_new::postvisit( const ast::VarArgsType * varArgsType ) {
	(void)varArgsType;
	if ( dynamic_cast< const ast::VarArgsType * >( dst ) ) {
		cost = Cost::zero;
	}
}

void ConversionCost_new::postvisit( const ast::ZeroType * zeroType ) {
	(void)zeroType;
	if ( dynamic_cast< const ast::ZeroType * >( dst ) ) {
		cost = Cost::zero;
	} else if ( const ast::BasicType * dstAsBasic =
			dynamic_cast< const ast::BasicType * >( dst ) ) {
		int tableResult = costMatrix[ ast::BasicType::SignedInt ][ dstAsBasic->kind ];
		if ( -1 == tableResult ) {
			cost = Cost::unsafe;
		} else {
			cost = Cost::zero;
			cost.incSafe( tableResult + 1 );
			cost.incSign( signMatrix[ ast::BasicType::SignedInt ][ dstAsBasic->kind ] );
		}
	} else if ( dynamic_cast< const ast::PointerType * >( dst ) ) {
		cost = Cost::zero;
		// +1 for zero_t ->, +1 for disambiguation
		cost.incSafe( maxIntCost + 2 );
	}
}

void ConversionCost_new::postvisit( const ast::OneType * oneType ) {
	(void)oneType;
	if ( dynamic_cast< const ast::OneType * >( dst ) ) {
		cost = Cost::zero;
	} else if ( const ast::BasicType * dstAsBasic =
			dynamic_cast< const ast::BasicType * >( dst ) ) {
		int tableResult = costMatrix[ ast::BasicType::SignedInt ][ dstAsBasic->kind ];
		if ( -1 == tableResult ) {
			cost = Cost::unsafe;
		} else {
			cost = Cost::zero;
			cost.incSafe( tableResult + 1 );
			cost.incSign( signMatrix[ ast::BasicType::SignedInt ][ dstAsBasic->kind ] );
		}
	}
}
// size_t ConversionCost_new::traceId = Stats::Heap::new_stacktrace_id("ConversionCost");

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
