//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ConversionCost.cpp --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 07:06:19 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jul 29 16:11:00 2020
// Update Count     : 28
//

#include "ConversionCost.hpp"

#include <cassert>                       // for assert
#include <list>                          // for list, list<>::const_iterator
#include <string>                        // for operator==, string

#include "ResolvExpr/Cost.hpp"           // for Cost
#include "ResolvExpr/Unify.hpp"          // for typesCompatibleIgnoreQualifiers
#include "ResolvExpr/PtrsAssignable.hpp" // for ptrsAssignable

namespace ResolvExpr {

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace {

	// GENERATED START, DO NOT EDIT
	// GENERATED BY c/BasicTypes-gen.cpp
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
	// GENERATED BY c/BasicTypes-gen.cpp
	static const int costMatrix[ast::BasicKind::NUMBER_OF_BASIC_TYPES][ast::BasicKind::NUMBER_OF_BASIC_TYPES] = { // path length from root to node
		/*               B    C   SC   UC   SI  USI    I   UI   LI  ULI  LLI ULLI __ID__UID  _FH _FHC   _F  _FC    F   FC  _FX _FXC  _FD _FDC    D   DC _FDX_FDXC _F80 _FLD_FLDC__FLD   LD  LDC_FLDX_FLDXC */
		/*      B */ {   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  14,  15,  16,  16,  17,  18,  18,  19, },
		/*      C */ {  -1,   0,   1,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  15,  15,  16,  17,  17,  18, },
		/*     SC */ {  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  15,  15,  16,  17,  17,  18, },
		/*     UC */ {  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  13,  14,  15,  15,  16,  17,  17,  18, },
		/*     SI */ {  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  14,  14,  15,  16,  16,  17, },
		/*    USI */ {  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  12,  13,  14,  14,  15,  16,  16,  17, },
		/*      I */ {  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  13,  13,  14,  15,  15,  16, },
		/*     UI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  11,  12,  13,  13,  14,  15,  15,  16, },
		/*     LI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  12,  12,  13,  14,  14,  15, },
		/*    ULI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  11,  12,  12,  13,  14,  14,  15, },
		/*    LLI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  11,  11,  12,  13,  13,  14, },
		/*   ULLI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  11,  11,  12,  13,  13,  14, },
		/*   __ID */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,  10,  10,  11,  12,  12,  13, },
		/*  __UID */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   8,   9,  10,  10,  11,  12,  12,  13, },
		/*    _FH */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   7,   8,   9,   9,  10,  11,  11,  12, },
		/*   _FHC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,  -1,   4,  -1,   5,  -1,   6,  -1,  -1,   8,  -1,  -1,  10,  -1,  11, },
		/*     _F */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   6,   7,   8,   8,   9,  10,  10,  11, },
		/*    _FC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,  -1,   4,  -1,   5,  -1,  -1,   7,  -1,  -1,   9,  -1,  10, },
		/*      F */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   5,   6,   7,   7,   8,   9,   9,  10, },
		/*     FC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,  -1,   4,  -1,  -1,   6,  -1,  -1,   8,  -1,   9, },
		/*    _FX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   4,   5,   6,   6,   7,   8,   8,   9, },
		/*   _FXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,   3,  -1,  -1,   5,  -1,  -1,   7,  -1,   8, },
		/*    _FD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   3,   4,   5,   5,   6,   7,   7,   8, },
		/*   _FDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,   2,  -1,  -1,   4,  -1,  -1,   6,  -1,   7, },
		/*      D */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   2,   3,   4,   4,   5,   6,   6,   7, },
		/*     DC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1,  -1,  -1,   3,  -1,  -1,   5,  -1,   6, },
		/*   _FDX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   3,   3,   4,   5,   5,   6, },
		/*  _FDXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,   2,  -1,  -1,   4,  -1,   5, },
		/*   _F80 */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   1,   0,   1,   2,   2,   3,   4,   4,   5, },
		/*   _FLD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2,   3,   3,   4, },
		/*  _FLDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,   2,  -1,   3, },
		/*  __FLD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   1,   0,   1,   2,   2,   3, },
		/*     LD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   2, },
		/*    LDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   1, },
		/*  _FLDX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1, },
		/* _FLDXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0, },
	}; // costMatrix
	static const int maxIntCost = 16;
	// GENERATED END
	static_assert(
		sizeof(costMatrix)/sizeof(costMatrix[0][0]) == ast::BasicKind::NUMBER_OF_BASIC_TYPES * ast::BasicKind::NUMBER_OF_BASIC_TYPES,
		"Missing row in the cost matrix"
	);

	// GENERATED START, DO NOT EDIT
	// GENERATED BY c/BasicTypes-gen.cpp
	static const int signMatrix[ast::BasicKind::NUMBER_OF_BASIC_TYPES][ast::BasicKind::NUMBER_OF_BASIC_TYPES] = { // number of sign changes in safe conversion
		/*               B    C   SC   UC   SI  USI    I   UI   LI  ULI  LLI ULLI __ID__UID  _FH _FHC   _F  _FC    F   FC  _FX _FXC  _FD _FDC    D   DC _FDX_FDXC _F80 _FLD_FLDC__FLD   LD  LDC_FLDX_FLDXC */
		/*      B */ {   0,   0,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*      C */ {  -1,   0,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     SC */ {  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     UC */ {  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     SI */ {  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    USI */ {  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*      I */ {  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     UI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     LI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    ULI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    LLI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*   ULLI */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*   __ID */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*  __UID */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    _FH */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*   _FHC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*     _F */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*    _FC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*      F */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     FC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*    _FX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*   _FXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*    _FD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*   _FDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*      D */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*     DC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*   _FDX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*  _FDXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*   _F80 */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0,   0,   0, },
		/*   _FLD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0,   0, },
		/*  _FLDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,  -1,   0,  -1,   0, },
		/*  __FLD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0,   0,   0, },
		/*     LD */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0,   0,   0, },
		/*    LDC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,  -1,   0, },
		/*  _FLDX */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   0, },
		/* _FLDXC */ {  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0, },
	}; // signMatrix
	// GENERATED END
	static_assert(
		sizeof(signMatrix)/sizeof(signMatrix[0][0]) == ast::BasicKind::NUMBER_OF_BASIC_TYPES * ast::BasicKind::NUMBER_OF_BASIC_TYPES,
		"Missing row in the sign matrix"
	);

	int localPtrsAssignable(const ast::Type * t1, const ast::Type * t2,
			const ast::SymbolTable &, const ast::TypeEnvironment & env ) {
		return ptrsAssignable( t1, t2, env );
	}
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
	if ( typesCompatibleIgnoreQualifiers( src, dst, env ) ) {
		return Cost::zero;
	} else if ( dynamic_cast< const ast::VoidType * >( dst ) ) {
		return Cost::safe;
	} else if ( const ast::ReferenceType * refType =
			 dynamic_cast< const ast::ReferenceType * >( dst ) ) {
		return convertToReferenceCost( src, refType, srcIsLvalue, symtab, env, localPtrsAssignable );
	} else {
		return ast::Pass<ConversionCost>::read( src, dst, srcIsLvalue, symtab, env, conversionCost );
	}
}

Cost enumCastCost (
	const ast::EnumInstType * src, const ast::EnumInstType * dst,
	const ast::SymbolTable & symtab, const ast::TypeEnvironment & env
);

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
					srcAsRef->base, dstAsRef->base, env ) ) {
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
			return ast::Pass<ConversionCost>::read( src, dst, srcIsLvalue, symtab, env, conversionCost );
		}
		if (const ast::EnumInstType * srcAsInst = dynamic_cast< const ast::EnumInstType * >( src )) {
			if (srcAsInst->base && !srcAsInst->base->isCfa) {
				static const ast::BasicType* integer = new ast::BasicType( ast::BasicKind::UnsignedInt );
				return ast::Pass<ConversionCost>::read( integer, dst, srcIsLvalue, symtab, env, conversionCost );
			}
		}
	} else {
		assert( -1 == diff );
		const ast::ReferenceType * dstAsRef = dynamic_cast< const ast::ReferenceType * >( dst );
		assert( dstAsRef );
		auto dstBaseType = dstAsRef->base;
		const ast::Type * newSrc = src;
		if ( dynamic_cast< const ast::EnumInstType * >( src ) && dstBaseType.as<ast::BasicType>() ) {
			newSrc = new ast::BasicType( ast::BasicKind::UnsignedInt );
		}
		if (dstAsRef->base->is_const() ) {
			auto cvtCost = conversionCost(newSrc, dstAsRef->base, srcIsLvalue, symtab, env) ;
			if (cvtCost == Cost::zero) { // exact match, may use a lvalue src
				if ( srcIsLvalue ) {
					if ( src->qualifiers == dstAsRef->base->qualifiers ) {
						return Cost::reference;
					} else if ( src->qualifiers < dstAsRef->base->qualifiers ) {
						return Cost::safe;
					} else {
						return Cost::unsafe;
					}
				}
				else {
					return Cost::reference;
				}
			}
			else { // not exact match, conversion is needed so lvalueness of src does not matter
				return cvtCost + Cost::reference;
			}
		}
		if ( typesCompatibleIgnoreQualifiers( newSrc, dstAsRef->base, env ) ) {
			if ( srcIsLvalue ) {
				if ( src->qualifiers == dstAsRef->base->qualifiers ) {
					return Cost::reference;
				} else if ( src->qualifiers < dstAsRef->base->qualifiers ) {
					return Cost::safe;
				} else {
					return Cost::unsafe;
				}
			} else { // rvalue-to-NC-ref conversion
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

void ConversionCost::postvisit( const ast::VoidType * voidType ) {
	(void)voidType;
	cost = Cost::infinity;
}

void ConversionCost::conversionCostFromBasicToBasic( const ast::BasicType * src, const ast::BasicType* dest ) {
	int tableResult = costMatrix[ src->kind ][ dest->kind ];
	if ( tableResult == -1 ) {
		cost = Cost::unsafe;
	} else {
		cost = Cost::zero;
		cost.incSafe( tableResult );
		cost.incSign( signMatrix[ src->kind ][ dest->kind ] );
	}
}

void ConversionCost::postvisit( const ast::BasicType * basicType ) {
	if ( const ast::BasicType * dstAsBasic = dynamic_cast< const ast::BasicType * >( dst ) ) {
		conversionCostFromBasicToBasic( basicType, dstAsBasic );
	} else if ( auto dstAsEnumInst = dynamic_cast< const ast::EnumInstType * >( dst ) ) {
		if ( dstAsEnumInst->base && !dstAsEnumInst->base->isCfa ) {
			cost = Cost::safe;
		}
	}
}

void ConversionCost::postvisit( const ast::PointerType * pointerType ) {
	if ( const ast::PointerType * dstAsPtr = dynamic_cast< const ast::PointerType * >( dst ) ) {
		ast::CV::Qualifiers tq1 = pointerType->base->qualifiers;
		ast::CV::Qualifiers tq2 = dstAsPtr->base->qualifiers;
		if ( tq1 <= tq2 && typesCompatibleIgnoreQualifiers(
				pointerType->base, dstAsPtr->base, env ) ) {
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

void ConversionCost::postvisit( const ast::ArrayType * arrayType ) {
	(void)arrayType;
}

void ConversionCost::postvisit( const ast::ReferenceType * refType ) {
	assert( nullptr == dynamic_cast< const ast::ReferenceType * >( dst ) );

	cost = costCalc( refType->base, dst, srcIsLvalue, symtab, env );

	// xxx - should qualifiers be considered in pass-by-value?
	/*
	if ( refType->base->qualifiers == dst->qualifiers ) {
		cost.incReference();
	} else if ( refType->base->qualifiers < dst->qualifiers ) {
		cost.incSafe();
	} else {
		cost.incUnsafe();
	}
	*/
	cost.incReference();
}

void ConversionCost::postvisit( const ast::FunctionType * functionType ) {
	(void)functionType;
}

void ConversionCost::postvisit( const ast::EnumInstType * inst ) {
	if ( auto dstInst = dynamic_cast<const ast::EnumInstType *>( dst ) ) {
		cost = enumCastCost(inst, dstInst, symtab, env);
	} else if ( !inst->base->isCfa ) {
		static ast::ptr<ast::BasicType> integer = { new ast::BasicType( ast::BasicKind::SignedInt ) };
		cost = costCalc( integer, dst, srcIsLvalue, symtab, env );
	}
	// cost.incUnsafe();
}

void ConversionCost::postvisit( const ast::TraitInstType * traitInstType ) {
	(void)traitInstType;
}

void ConversionCost::postvisit( const ast::TypeInstType * typeInstType ) {
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

void ConversionCost::postvisit( const ast::TupleType * tupleType ) {
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

void ConversionCost::postvisit( const ast::VarArgsType * varArgsType ) {
	(void)varArgsType;
	if ( dynamic_cast< const ast::VarArgsType * >( dst ) ) {
		cost = Cost::zero;
	}
}

void ConversionCost::postvisit( const ast::ZeroType * zeroType ) {
	(void)zeroType;
	if ( dynamic_cast< const ast::ZeroType * >( dst ) ) {
		cost = Cost::zero;
	} else if ( const ast::BasicType * dstAsBasic =
			dynamic_cast< const ast::BasicType * >( dst ) ) {
		int tableResult = costMatrix[ ast::BasicKind::SignedInt ][ dstAsBasic->kind ];
		if ( -1 == tableResult ) {
			cost = Cost::unsafe;
		} else {
			cost = Cost::zero;
			cost.incSafe( tableResult + 1 );
			cost.incSign( signMatrix[ ast::BasicKind::SignedInt ][ dstAsBasic->kind ] );
		}
		// this has the effect of letting any expr such as x+0, x+1 to be typed
		// the same as x, instead of at least int. are we willing to sacrifice this little
		// bit of coherence with C?
		// TODO: currently this does not work when no zero/one overloads exist. Find a fix for it.
		// cost = Cost::zero;
	} else if ( dynamic_cast< const ast::PointerType * >( dst ) ) {
		cost = Cost::zero;
		// +1 for zero_t ->, +1 for disambiguation
		cost.incSafe( maxIntCost + 2 );
		// assuming 0p is supposed to be used for pointers?
	} else if ( auto dstAsEnumInst = dynamic_cast< const ast::EnumInstType * >( dst ) ) {
		if ( dstAsEnumInst->base && !dstAsEnumInst->base->isCfa ) {
			cost = Cost::safe;
		}
	}
}

void ConversionCost::postvisit( const ast::OneType * oneType ) {
	(void)oneType;
	if ( dynamic_cast< const ast::OneType * >( dst ) ) {
		cost = Cost::zero;
	} else if ( const ast::BasicType * dstAsBasic =
			dynamic_cast< const ast::BasicType * >( dst ) ) {
		int tableResult = costMatrix[ ast::BasicKind::SignedInt ][ dstAsBasic->kind ];
		if ( -1 == tableResult ) {
			cost = Cost::unsafe;
		} else {
			cost = Cost::zero;
			cost.incSafe( tableResult + 1 );
			cost.incSign( signMatrix[ ast::BasicKind::SignedInt ][ dstAsBasic->kind ] );
		}
	} else if ( auto dstAsEnumInst = dynamic_cast< const ast::EnumInstType * >( dst ) ) {
		if ( dstAsEnumInst->base && !dstAsEnumInst->base->isCfa ) {
			cost = Cost::safe;
		}
	}
}

// (dst) src is safe is src is a subtype of dst, or dst {inline src, ...}
Cost enumCastCost (
	const ast::EnumInstType * src, const ast::EnumInstType * dst,
	const ast::SymbolTable & symtab, const ast::TypeEnvironment & env
) {
	auto srcDecl = src->base;
	auto dstDecl = dst->base;
	if (srcDecl->name == dstDecl->name) return Cost::safe;
	Cost minCost = Cost::infinity;
	for (auto child: dstDecl->inlinedDecl) {
		Cost c = enumCastCost(src, child, symtab, env) + Cost::safe;
		if (c<minCost) minCost = c;
	}
	return minCost;
}


// size_t ConversionCost::traceId = Stats::Heap::new_stacktrace_id("ConversionCost");

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
