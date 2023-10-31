//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CommonType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 06:59:27 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Feb 14 17:10:10 2019
// Update Count     : 24
//

#include "CommonType.hpp"

#include <cassert>                       // for strict_dynamic_cast
#include <map>                           // for _Rb_tree_const_iterator
#include <utility>                       // for pair

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "Common/PassVisitor.h"
#include "ResolvExpr/TypeEnvironment.h"  // for OpenVarSet, AssertionSet
#include "SymTab/Indexer.h"              // for Indexer
#include "SynTree/Declaration.h"         // for TypeDecl, NamedTypeDecl (ptr...
#include "SynTree/Type.h"                // for BasicType, BasicType::Kind::...
#include "SynTree/Visitor.h"             // for Visitor
#include "Unify.h"                       // for unifyExact, WidenMode
#include "typeops.h"                     // for isFtype
#include "Tuples/Tuples.h"

// #define DEBUG
#ifdef DEBUG
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace ResolvExpr {
	struct CommonType_old : public WithShortCircuiting {
		CommonType_old( Type * type2, bool widenFirst, bool widenSecond, const SymTab::Indexer &indexer, TypeEnvironment &env, const OpenVarSet &openVars );
		Type * get_result() const { return result; }

		void previsit( BaseSyntaxNode * ) { visit_children = false; }

		void postvisit( VoidType * voidType );
		void postvisit( BasicType * basicType );
		void postvisit( PointerType * pointerType );
		void postvisit( ArrayType * arrayType );
		void postvisit( ReferenceType * refType );
		void postvisit( FunctionType * functionType );
		void postvisit( StructInstType * aggregateUseType );
		void postvisit( UnionInstType * aggregateUseType );
		void postvisit( EnumInstType * aggregateUseType );
		void postvisit( TraitInstType * aggregateUseType );
		void postvisit( TypeInstType * aggregateUseType );
		void postvisit( TupleType * tupleType );
		void postvisit( VarArgsType * varArgsType );
		void postvisit( ZeroType * zeroType );
		void postvisit( OneType * oneType );

	  private:
		template< typename Pointer > void getCommonWithVoidPointer( Pointer * voidPointer, Pointer * otherPointer );
		template< typename RefType > void handleRefType( RefType * inst, Type * other );

		Type * result;
		Type * type2;				// inherited
		bool widenFirst, widenSecond;
		const SymTab::Indexer &indexer;
		TypeEnvironment &env;
		const OpenVarSet &openVars;
	};

	Type * handleReference( Type * t1, Type * t2, bool widenFirst, bool widenSecond, const SymTab::Indexer &indexer, TypeEnvironment & env, const OpenVarSet &openVars ) {
		Type * common = nullptr;
		AssertionSet have, need;
		OpenVarSet newOpen( openVars );
		// need unify to bind type variables
		if ( unify( t1, t2, env, have, need, newOpen, indexer, common ) ) {
			PRINT(
				std::cerr << "unify success: " << widenFirst << " " << widenSecond << std::endl;
			)
			if ( (widenFirst || t2->tq <= t1->tq) && (widenSecond || t1->tq <= t2->tq) ) {
				PRINT(
					std::cerr << "widen okay" << std::endl;
				)
				common->tq |= t1->tq;
				common->tq |= t2->tq;
				return common;
			}
		}
		PRINT(
			std::cerr << "exact unify failed: " << t1 << " " << t2 << std::endl;
		)
		return nullptr;
	}

	Type * commonType( Type * type1, Type * type2, bool widenFirst, bool widenSecond, const SymTab::Indexer &indexer, TypeEnvironment &env, const OpenVarSet &openVars ) {
		PassVisitor<CommonType_old> visitor( type2, widenFirst, widenSecond, indexer, env, openVars );

		int depth1 = type1->referenceDepth();
		int depth2 = type2->referenceDepth();
		if ( depth1 > 0 || depth2 > 0 ) {
			int diff = depth1-depth2;
			// TODO: should it be possible for commonType to generate complicated conversions? I would argue no, only conversions that involve types of the same reference level or a difference of 1 should be allowed.
			// if ( diff > 1 || diff < -1 ) return nullptr;

			// special case where one type has a reference depth of 1 larger than the other
			if ( diff > 0 || diff < 0 ) {
				PRINT(
					std::cerr << "reference depth diff: " << diff << std::endl;
				)
				Type * result = nullptr;
				ReferenceType * ref1 = dynamic_cast< ReferenceType * >( type1 );
				ReferenceType * ref2 = dynamic_cast< ReferenceType * >( type2 );
				if ( diff > 0 ) {
					// deeper on the left
					assert( ref1 );
					result = handleReference( ref1->base, type2, widenFirst, widenSecond, indexer, env, openVars );
				} else {
					// deeper on the right
					assert( ref2 );
					result = handleReference( type1, ref2->base, widenFirst, widenSecond, indexer, env, openVars );
				}
				if ( result && ref1 ) {
					// formal is reference, so result should be reference
					PRINT(
						std::cerr << "formal is reference; result should be reference" << std::endl;
					)
					result = new ReferenceType( ref1->tq, result );
				}
				PRINT(
					std::cerr << "common type of reference [" << type1 << "] and [" << type2 << "] is [" << result << "]" << std::endl;
				)
				return result;
			}
			// otherwise, both are reference types of the same depth and this is handled by the CommonType visitor.
		}

		type1->accept( visitor );
		Type * result = visitor.pass.get_result();
		if ( ! result ) {
			// this appears to be handling for opaque type declarations
			if ( widenSecond ) {
				if ( const TypeInstType * inst = dynamic_cast< const TypeInstType * >( type2 ) ) {
					if ( const NamedTypeDecl * nt = indexer.lookupType( inst->get_name() ) ) {
						const TypeDecl * type = strict_dynamic_cast< const TypeDecl * >( nt );
						if ( type->get_base() ) {
							Type::Qualifiers tq1 = type1->tq, tq2 = type2->tq;
							AssertionSet have, need;
							OpenVarSet newOpen( openVars );
							type1->tq = Type::Qualifiers();
							type->get_base()->tq = tq1;
							if ( unifyExact( type1, type->get_base(), env, have, need, newOpen, indexer ) ) {
								result = type1->clone();
								result->tq = tq1 | tq2;
							} // if
							type1->tq = tq1;
							type->get_base()->tq = Type::Qualifiers();
						} // if
					} // if
				} // if
			} // if
		} // if
#ifdef DEBUG
		std::cerr << "============= commonType" << std::endl << "type1 is ";
		type1->print( std::cerr );
		std::cerr << " type2 is ";
		type2->print( std::cerr );
		if ( result ) {
			std::cerr << " common type is ";
			result->print( std::cerr );
		} else {
			std::cerr << " no common type";
		} // if
		std::cerr << std::endl;
#endif
		return result;
	}

	// GENERATED START, DO NOT EDIT
	// GENERATED BY c/BasicTypes-gen.cc
	#define BT ast::BasicType::
	static const BT Kind commonTypes[BT NUMBER_OF_BASIC_TYPES][BT NUMBER_OF_BASIC_TYPES] = { // nearest common ancestor
		/*		                        B                       C                      SC                      UC                      SI                     SUI
				                        I                      UI                      LI                     LUI                     LLI                    LLUI
				                       IB                     UIB                     _FH                     _FH                      _F                     _FC
				                        F                      FC                     _FX                    _FXC                      FD                    _FDC
				                        D                      DC                    F80X                   _FDXC                     F80                     _FB
				                    _FLDC                      FB                      LD                     LDC                    _FBX                  _FLDXC
				 */
				  {
		/*      B */                BT Bool,                BT Char,          BT SignedChar,        BT UnsignedChar,      BT ShortSignedInt,    BT ShortUnsignedInt,
				             BT SignedInt,         BT UnsignedInt,       BT LongSignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*      C */                BT Char,                BT Char,          BT SignedChar,        BT UnsignedChar,      BT ShortSignedInt,    BT ShortUnsignedInt,
				             BT SignedInt,         BT UnsignedInt,       BT LongSignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*     SC */          BT SignedChar,          BT SignedChar,          BT SignedChar,        BT UnsignedChar,      BT ShortSignedInt,    BT ShortUnsignedInt,
				             BT SignedInt,         BT UnsignedInt,       BT LongSignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*     UC */        BT UnsignedChar,        BT UnsignedChar,        BT UnsignedChar,        BT UnsignedChar,      BT ShortSignedInt,    BT ShortUnsignedInt,
				             BT SignedInt,         BT UnsignedInt,       BT LongSignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*     SI */      BT ShortSignedInt,      BT ShortSignedInt,      BT ShortSignedInt,      BT ShortSignedInt,      BT ShortSignedInt,    BT ShortUnsignedInt,
				             BT SignedInt,         BT UnsignedInt,       BT LongSignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*    SUI */    BT ShortUnsignedInt,    BT ShortUnsignedInt,    BT ShortUnsignedInt,    BT ShortUnsignedInt,    BT ShortUnsignedInt,    BT ShortUnsignedInt,
				             BT SignedInt,         BT UnsignedInt,       BT LongSignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*      I */           BT SignedInt,           BT SignedInt,           BT SignedInt,           BT SignedInt,           BT SignedInt,           BT SignedInt,
				             BT SignedInt,         BT UnsignedInt,       BT LongSignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*     UI */         BT UnsignedInt,         BT UnsignedInt,         BT UnsignedInt,         BT UnsignedInt,         BT UnsignedInt,         BT UnsignedInt,
				           BT UnsignedInt,         BT UnsignedInt,       BT LongSignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*     LI */       BT LongSignedInt,       BT LongSignedInt,       BT LongSignedInt,       BT LongSignedInt,       BT LongSignedInt,       BT LongSignedInt,
				         BT LongSignedInt,       BT LongSignedInt,       BT LongSignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*    LUI */     BT LongUnsignedInt,     BT LongUnsignedInt,     BT LongUnsignedInt,     BT LongUnsignedInt,     BT LongUnsignedInt,     BT LongUnsignedInt,
				       BT LongUnsignedInt,     BT LongUnsignedInt,     BT LongUnsignedInt,     BT LongUnsignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*    LLI */   BT LongLongSignedInt,   BT LongLongSignedInt,   BT LongLongSignedInt,   BT LongLongSignedInt,   BT LongLongSignedInt,   BT LongLongSignedInt,
				     BT LongLongSignedInt,   BT LongLongSignedInt,   BT LongLongSignedInt,   BT LongLongSignedInt,   BT LongLongSignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*   LLUI */ BT LongLongUnsignedInt, BT LongLongUnsignedInt, BT LongLongUnsignedInt, BT LongLongUnsignedInt, BT LongLongUnsignedInt, BT LongLongUnsignedInt,
				   BT LongLongUnsignedInt, BT LongLongUnsignedInt, BT LongLongUnsignedInt, BT LongLongUnsignedInt, BT LongLongUnsignedInt, BT LongLongUnsignedInt,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*     IB */        BT SignedInt128,        BT SignedInt128,        BT SignedInt128,        BT SignedInt128,        BT SignedInt128,        BT SignedInt128,
				          BT SignedInt128,        BT SignedInt128,        BT SignedInt128,        BT SignedInt128,        BT SignedInt128,        BT SignedInt128,
				          BT SignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*    UIB */      BT UnsignedInt128,      BT UnsignedInt128,      BT UnsignedInt128,      BT UnsignedInt128,      BT UnsignedInt128,      BT UnsignedInt128,
				        BT UnsignedInt128,      BT UnsignedInt128,      BT UnsignedInt128,      BT UnsignedInt128,      BT UnsignedInt128,      BT UnsignedInt128,
				        BT UnsignedInt128,      BT UnsignedInt128,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*    _FH */            BT uFloat16,            BT uFloat16,            BT uFloat16,            BT uFloat16,            BT uFloat16,            BT uFloat16,
				              BT uFloat16,            BT uFloat16,            BT uFloat16,            BT uFloat16,            BT uFloat16,            BT uFloat16,
				              BT uFloat16,            BT uFloat16,            BT uFloat16,     BT uFloat16Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*    _FH */     BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,
				       BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,
				       BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat16Complex,     BT uFloat32Complex,     BT uFloat32Complex,
				          BT FloatComplex,        BT FloatComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,     BT uFloat64Complex,     BT uFloat64Complex,
				         BT DoubleComplex,       BT DoubleComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
				  {
		/*     _F */            BT uFloat32,            BT uFloat32,            BT uFloat32,            BT uFloat32,            BT uFloat32,            BT uFloat32,
				              BT uFloat32,            BT uFloat32,            BT uFloat32,            BT uFloat32,            BT uFloat32,            BT uFloat32,
				              BT uFloat32,            BT uFloat32,            BT uFloat32,     BT uFloat32Complex,            BT uFloat32,     BT uFloat32Complex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*    _FC */     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,
				       BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,
				       BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,     BT uFloat32Complex,
				          BT FloatComplex,        BT FloatComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,     BT uFloat64Complex,     BT uFloat64Complex,
				         BT DoubleComplex,       BT DoubleComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
				  {
		/*      F */               BT Float,               BT Float,               BT Float,               BT Float,               BT Float,               BT Float,
				                 BT Float,               BT Float,               BT Float,               BT Float,               BT Float,               BT Float,
				                 BT Float,               BT Float,               BT Float,        BT FloatComplex,               BT Float,        BT FloatComplex,
				                 BT Float,        BT FloatComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*     FC */        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,
				          BT FloatComplex,        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,
				          BT FloatComplex,        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,        BT FloatComplex,
				          BT FloatComplex,        BT FloatComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,     BT uFloat64Complex,     BT uFloat64Complex,
				         BT DoubleComplex,       BT DoubleComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
				  {
		/*    _FX */           BT uFloat32x,           BT uFloat32x,           BT uFloat32x,           BT uFloat32x,           BT uFloat32x,           BT uFloat32x,
				             BT uFloat32x,           BT uFloat32x,           BT uFloat32x,           BT uFloat32x,           BT uFloat32x,           BT uFloat32x,
				             BT uFloat32x,           BT uFloat32x,           BT uFloat32x,    BT uFloat32xComplex,           BT uFloat32x,    BT uFloat32xComplex,
				             BT uFloat32x,    BT uFloat32xComplex,           BT uFloat32x,    BT uFloat32xComplex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*   _FXC */    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,
				      BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,
				      BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,
				      BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,    BT uFloat32xComplex,     BT uFloat64Complex,     BT uFloat64Complex,
				         BT DoubleComplex,       BT DoubleComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
				  {
		/*     FD */            BT uFloat64,            BT uFloat64,            BT uFloat64,            BT uFloat64,            BT uFloat64,            BT uFloat64,
				              BT uFloat64,            BT uFloat64,            BT uFloat64,            BT uFloat64,            BT uFloat64,            BT uFloat64,
				              BT uFloat64,            BT uFloat64,            BT uFloat64,     BT uFloat64Complex,            BT uFloat64,     BT uFloat64Complex,
				              BT uFloat64,     BT uFloat64Complex,            BT uFloat64,     BT uFloat64Complex,            BT uFloat64,     BT uFloat64Complex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*   _FDC */     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,
				       BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,
				       BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,
				       BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,     BT uFloat64Complex,
				         BT DoubleComplex,       BT DoubleComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
				  {
		/*      D */              BT Double,              BT Double,              BT Double,              BT Double,              BT Double,              BT Double,
				                BT Double,              BT Double,              BT Double,              BT Double,              BT Double,              BT Double,
				                BT Double,              BT Double,              BT Double,       BT DoubleComplex,              BT Double,       BT DoubleComplex,
				                BT Double,       BT DoubleComplex,              BT Double,       BT DoubleComplex,              BT Double,       BT DoubleComplex,
				                BT Double,       BT DoubleComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*     DC */       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,
				         BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,
				         BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,
				         BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,       BT DoubleComplex,
				         BT DoubleComplex,       BT DoubleComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
				  {
		/*   F80X */           BT uFloat64x,           BT uFloat64x,           BT uFloat64x,           BT uFloat64x,           BT uFloat64x,           BT uFloat64x,
				             BT uFloat64x,           BT uFloat64x,           BT uFloat64x,           BT uFloat64x,           BT uFloat64x,           BT uFloat64x,
				             BT uFloat64x,           BT uFloat64x,           BT uFloat64x,    BT uFloat64xComplex,           BT uFloat64x,    BT uFloat64xComplex,
				             BT uFloat64x,    BT uFloat64xComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uFloat64x,    BT uFloat64xComplex,
				             BT uFloat64x,    BT uFloat64xComplex,           BT uFloat64x,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*  _FDXC */    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,
				      BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,
				      BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,
				      BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,
				      BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat64xComplex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
				  {
		/*    F80 */           BT uuFloat80,           BT uuFloat80,           BT uuFloat80,           BT uuFloat80,           BT uuFloat80,           BT uuFloat80,
				             BT uuFloat80,           BT uuFloat80,           BT uuFloat80,           BT uuFloat80,           BT uuFloat80,           BT uuFloat80,
				             BT uuFloat80,           BT uuFloat80,           BT uuFloat80,    BT uFloat64xComplex,           BT uuFloat80,    BT uFloat64xComplex,
				             BT uuFloat80,    BT uFloat64xComplex,           BT uuFloat80,    BT uFloat64xComplex,           BT uuFloat80,    BT uFloat64xComplex,
				             BT uuFloat80,    BT uFloat64xComplex,           BT uuFloat80,    BT uFloat64xComplex,           BT uuFloat80,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*    _FB */           BT uFloat128,           BT uFloat128,           BT uFloat128,           BT uFloat128,           BT uFloat128,           BT uFloat128,
				             BT uFloat128,           BT uFloat128,           BT uFloat128,           BT uFloat128,           BT uFloat128,           BT uFloat128,
				             BT uFloat128,           BT uFloat128,           BT uFloat128,    BT uFloat128Complex,           BT uFloat128,    BT uFloat128Complex,
				             BT uFloat128,    BT uFloat128Complex,           BT uFloat128,    BT uFloat128Complex,           BT uFloat128,    BT uFloat128Complex,
				             BT uFloat128,    BT uFloat128Complex,           BT uFloat128,    BT uFloat128Complex,           BT uFloat128,           BT uFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*  _FLDC */    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,    BT uFloat128Complex,
				      BT uFloat128Complex,    BT uFloat128Complex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
				  {
		/*     FB */          BT uuFloat128,          BT uuFloat128,          BT uuFloat128,          BT uuFloat128,          BT uuFloat128,          BT uuFloat128,
				            BT uuFloat128,          BT uuFloat128,          BT uuFloat128,          BT uuFloat128,          BT uuFloat128,          BT uuFloat128,
				            BT uuFloat128,          BT uuFloat128,          BT uuFloat128,    BT uFloat128Complex,          BT uuFloat128,    BT uFloat128Complex,
				            BT uuFloat128,    BT uFloat128Complex,          BT uuFloat128,    BT uFloat128Complex,          BT uuFloat128,    BT uFloat128Complex,
				            BT uuFloat128,    BT uFloat128Complex,          BT uuFloat128,    BT uFloat128Complex,          BT uuFloat128,          BT uuFloat128,
				      BT uFloat128Complex,          BT uuFloat128,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*     LD */          BT LongDouble,          BT LongDouble,          BT LongDouble,          BT LongDouble,          BT LongDouble,          BT LongDouble,
				            BT LongDouble,          BT LongDouble,          BT LongDouble,          BT LongDouble,          BT LongDouble,          BT LongDouble,
				            BT LongDouble,          BT LongDouble,          BT LongDouble,   BT LongDoubleComplex,          BT LongDouble,   BT LongDoubleComplex,
				            BT LongDouble,   BT LongDoubleComplex,          BT LongDouble,   BT LongDoubleComplex,          BT LongDouble,   BT LongDoubleComplex,
				            BT LongDouble,   BT LongDoubleComplex,          BT LongDouble,   BT LongDoubleComplex,          BT LongDouble,          BT LongDouble,
				     BT LongDoubleComplex,          BT LongDouble,          BT LongDouble,   BT LongDoubleComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/*    LDC */   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,
				     BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,
				     BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,
				     BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,
				     BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,
				     BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT LongDoubleComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
				  {
		/*   _FBX */          BT uFloat128x,          BT uFloat128x,          BT uFloat128x,          BT uFloat128x,          BT uFloat128x,          BT uFloat128x,
				            BT uFloat128x,          BT uFloat128x,          BT uFloat128x,          BT uFloat128x,          BT uFloat128x,          BT uFloat128x,
				            BT uFloat128x,          BT uFloat128x,          BT uFloat128x,   BT uFloat128xComplex,          BT uFloat128x,   BT uFloat128xComplex,
				            BT uFloat128x,   BT uFloat128xComplex,          BT uFloat128x,   BT uFloat128xComplex,          BT uFloat128x,   BT uFloat128xComplex,
				            BT uFloat128x,   BT uFloat128xComplex,          BT uFloat128x,   BT uFloat128xComplex,          BT uFloat128x,          BT uFloat128x,
				     BT uFloat128xComplex,          BT uFloat128x,          BT uFloat128x,   BT uFloat128xComplex,          BT uFloat128x,   BT uFloat128xComplex,
				  },
				  {
		/* _FLDXC */   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				     BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				     BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				     BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				     BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				     BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,   BT uFloat128xComplex,
				  },
	}; // commonTypes
	#undef BT
	// GENERATED END
	static_assert(
		sizeof(commonTypes)/sizeof(commonTypes[0][0]) == ast::BasicType::NUMBER_OF_BASIC_TYPES * ast::BasicType::NUMBER_OF_BASIC_TYPES,
		"Each basic type kind should have a corresponding row in the combined type matrix"
	);

	CommonType_old::CommonType_old( Type * type2, bool widenFirst, bool widenSecond, const SymTab::Indexer &indexer, TypeEnvironment &env, const OpenVarSet &openVars )
		: result( 0 ), type2( type2 ), widenFirst( widenFirst ), widenSecond( widenSecond ), indexer( indexer ), env( env ), openVars( openVars ) {
	}

	void CommonType_old::postvisit( VoidType * ) {}

	void CommonType_old::postvisit( BasicType * basicType ) {
		if ( BasicType * otherBasic = dynamic_cast< BasicType * >( type2 ) ) {
			BasicType::Kind newType = (BasicType::Kind)(int)commonTypes[ (ast::BasicType::Kind)(int)basicType->get_kind() ][ (ast::BasicType::Kind)(int)otherBasic->get_kind() ];
			if ( ( ( newType == basicType->get_kind() && basicType->tq >= otherBasic->tq ) || widenFirst ) && ( ( newType == otherBasic->get_kind() && basicType->tq <= otherBasic->tq ) || widenSecond ) ) {
				result = new BasicType( basicType->tq | otherBasic->tq, newType );
			} // if
		} else if (  dynamic_cast< ZeroType * >( type2 ) || dynamic_cast< OneType * >( type2 ) ) {
			// use signed int in lieu of the enum/zero/one type
			BasicType::Kind newType = (BasicType::Kind)(int)commonTypes[ (ast::BasicType::Kind)(int)basicType->get_kind() ][ ast::BasicType::SignedInt ];
			if ( ( ( newType == basicType->get_kind() && basicType->tq >= type2->tq ) || widenFirst ) && ( ( newType != basicType->get_kind() && basicType->tq <= type2->tq ) || widenSecond ) ) {
				result = new BasicType( basicType->tq | type2->tq, newType );
			} // if
		} else if ( const EnumInstType * enumInst = dynamic_cast< const EnumInstType * > ( type2 ) ) {
			const EnumDecl* enumDecl = enumInst->baseEnum;
			if ( const Type* baseType = enumDecl->base ) {
				result = baseType->clone();
			} else {
				BasicType::Kind newType = (BasicType::Kind)(int)commonTypes[ (ast::BasicType::Kind)(int)basicType->get_kind() ][ ast::BasicType::SignedInt ];
				if ( ( ( newType == basicType->get_kind() && basicType->tq >= type2->tq ) || widenFirst ) && ( ( newType != basicType->get_kind() && basicType->tq <= type2->tq ) || widenSecond ) ) {
					result = new BasicType( basicType->tq | type2->tq, newType );
				} // if
			}
		}
	}

	template< typename Pointer >
	void CommonType_old::getCommonWithVoidPointer( Pointer * voidPointer, Pointer * otherPointer ) {
		if ( TypeInstType * var = dynamic_cast< TypeInstType * >( otherPointer->get_base() ) ) {
			OpenVarSet::const_iterator entry = openVars.find( var->get_name() );
			if ( entry != openVars.end() ) {
				AssertionSet need, have;
				WidenMode widen( widenFirst, widenSecond );
				if ( entry != openVars.end() && ! env.bindVar(var, voidPointer->get_base(), entry->second, need, have, openVars, widen, indexer ) ) return;
			}
		}
		result = voidPointer->clone();
		result->tq |= otherPointer->tq;
	}

	void CommonType_old::postvisit( PointerType * pointerType ) {
		if ( PointerType * otherPointer = dynamic_cast< PointerType * >( type2 ) ) {
			// std::cerr << "commonType: two pointers: " << pointerType << " / " << otherPointer << std::endl;
			if ( widenFirst && dynamic_cast< VoidType * >( otherPointer->get_base() ) && ! isFtype(pointerType->get_base()) ) {
				getCommonWithVoidPointer( otherPointer, pointerType );
			} else if ( widenSecond && dynamic_cast< VoidType * >( pointerType->get_base() ) && ! isFtype(otherPointer->get_base()) ) {
				getCommonWithVoidPointer( pointerType, otherPointer );
			} else if ( ( pointerType->get_base()->tq >= otherPointer->get_base()->tq || widenFirst )
					   && ( pointerType->get_base()->tq <= otherPointer->get_base()->tq || widenSecond ) ) {
				// std::cerr << "middle case" << std::endl;
				Type::Qualifiers tq1 = pointerType->get_base()->tq, tq2 = otherPointer->get_base()->tq;
				pointerType->get_base()->tq = Type::Qualifiers();
				otherPointer->get_base()->tq = Type::Qualifiers();
				AssertionSet have, need;
				OpenVarSet newOpen( openVars );
				if ( unifyExact( pointerType->get_base(), otherPointer->get_base(), env, have, need, newOpen, indexer ) ) {
					// std::cerr << "unifyExact success" << std::endl;
					if ( tq1 < tq2 ) {
						result = pointerType->clone();
					} else {
						result = otherPointer->clone();
					} // if
					strict_dynamic_cast<PointerType *>(result)->base->tq = tq1 | tq2;
				} else {
					/// std::cerr << "place for ptr-to-type" << std::endl;
				} // if
				pointerType->get_base()->tq = tq1;
				otherPointer->get_base()->tq = tq2;
			} // if
		} else if ( widenSecond && dynamic_cast< ZeroType * >( type2 ) ) {
			result = pointerType->clone();
			result->tq |= type2->tq;
		} // if
	}

	void CommonType_old::postvisit( ArrayType * ) {}

	void CommonType_old::postvisit( ReferenceType * refType ) {
		if ( ReferenceType * otherRef = dynamic_cast< ReferenceType * >( type2 ) ) {
			// std::cerr << "commonType: both references: " << refType << " / " << otherRef << std::endl;
			// std::cerr << ( refType->get_base()->tq >= otherRef->get_base()->tq || widenFirst ) << (refType->get_base()->tq <= otherRef->get_base()->tq || widenSecond) << std::endl;
			if ( widenFirst && dynamic_cast< VoidType * >( otherRef->get_base() ) && ! isFtype(refType->get_base()) ) {
				getCommonWithVoidPointer( otherRef, refType );
			} else if ( widenSecond && dynamic_cast< VoidType * >( refType->get_base() ) && ! isFtype(otherRef->get_base()) ) {
				getCommonWithVoidPointer( refType, otherRef );
			} else if ( ( refType->get_base()->tq >= otherRef->get_base()->tq || widenFirst )
					   && ( refType->get_base()->tq <= otherRef->get_base()->tq || widenSecond ) ) {
				// std::cerr << "middle case" << std::endl;
				Type::Qualifiers tq1 = refType->get_base()->tq, tq2 = otherRef->get_base()->tq;
				refType->get_base()->tq = Type::Qualifiers();
				otherRef->get_base()->tq = Type::Qualifiers();
				AssertionSet have, need;
				OpenVarSet newOpen( openVars );
				if ( unifyExact( refType->get_base(), otherRef->get_base(), env, have, need, newOpen, indexer ) ) {
					if ( tq1 < tq2 ) {
						result = refType->clone();
					} else {
						result = otherRef->clone();
					} // if
					strict_dynamic_cast<ReferenceType *>(result)->base->tq = tq1 | tq2;
				} else {
					/// std::cerr << "place for ptr-to-type" << std::endl;
				} // if
				refType->get_base()->tq = tq1;
				otherRef->get_base()->tq = tq2;
			} // if
		} else if ( widenSecond && dynamic_cast< ZeroType * >( type2 ) ) {
			result = refType->clone();
			result->tq |= type2->tq;
		} // if
	}

	void CommonType_old::postvisit( FunctionType * ) {}
	void CommonType_old::postvisit( StructInstType * ) {}
	void CommonType_old::postvisit( UnionInstType * ) {}

	void CommonType_old::postvisit( EnumInstType * enumInstType ) {
		if ( dynamic_cast< BasicType * >( type2 ) || dynamic_cast< ZeroType * >( type2 ) || dynamic_cast< OneType * >( type2 ) ) {
			// reuse BasicType, EnumInstType code by swapping type2 with enumInstType
			result = commonType( type2, enumInstType, widenSecond, widenFirst, indexer, env, openVars );
		} // if
	}

	void CommonType_old::postvisit( TraitInstType * ) {
	}

	void CommonType_old::postvisit( TypeInstType * inst ) {
		if ( widenFirst ) {
			const NamedTypeDecl * nt = indexer.lookupType( inst->get_name() );
			if ( nt ) {
				const TypeDecl * type = strict_dynamic_cast< const TypeDecl * >( nt );
				if ( type->get_base() ) {
					Type::Qualifiers tq1 = inst->tq, tq2 = type2->tq;
					AssertionSet have, need;
					OpenVarSet newOpen( openVars );
					type2->tq = Type::Qualifiers();
					type->get_base()->tq = tq1;
					if ( unifyExact( type->get_base(), type2, env, have, need, newOpen, indexer ) ) {
						result = type2->clone();
						result->tq = tq1 | tq2;
					} // if
					type2->tq = tq2;
					type->get_base()->tq = Type::Qualifiers();
				} // if
			} // if
		} // if
	}

	void CommonType_old::postvisit( TupleType * ) {}
	void CommonType_old::postvisit( VarArgsType * ) {}

	void CommonType_old::postvisit( ZeroType * zeroType ) {
		if ( widenFirst ) {
			if ( dynamic_cast< BasicType * >( type2 ) || dynamic_cast< PointerType * >( type2 ) || dynamic_cast< EnumInstType * >( type2 ) ) {
				if ( widenSecond || zeroType->tq <= type2->tq ) {
					result = type2->clone();
					result->tq |= zeroType->tq;
				}
			} else if ( widenSecond && dynamic_cast< OneType * >( type2 ) ) {
				result = new BasicType( zeroType->tq, BasicType::SignedInt );
				result->tq |= type2->tq;
			}
		}
	}

	void CommonType_old::postvisit( OneType * oneType ) {
		if ( widenFirst ) {
			if ( dynamic_cast< BasicType * >( type2 ) || dynamic_cast< EnumInstType * >( type2 ) ) {
				if ( widenSecond || oneType->tq <= type2->tq ) {
					result = type2->clone();
					result->tq |= oneType->tq;
				}
			} else if ( widenSecond && dynamic_cast< ZeroType * >( type2 ) ) {
				result = new BasicType( oneType->tq, BasicType::SignedInt );
				result->tq |= type2->tq;
			}
		}
	}

	class CommonType_new final : public ast::WithShortCircuiting {
		const ast::Type * type2;
		WidenMode widen;
		ast::TypeEnvironment & tenv;
		const ast::OpenVarSet & open;
		ast::AssertionSet & need;
		ast::AssertionSet & have;
	public:
		static size_t traceId;
		ast::ptr< ast::Type > result;

		CommonType_new(
			const ast::Type * t2, WidenMode w,
			ast::TypeEnvironment & env, const ast::OpenVarSet & o,
			ast::AssertionSet & need, ast::AssertionSet & have )
		: type2( t2 ), widen( w ), tenv( env ), open( o ), need (need), have (have) ,result() {}

		void previsit( const ast::Node * ) { visit_children = false; }

		void postvisit( const ast::VoidType * ) {}

		void postvisit( const ast::BasicType * basic ) {
			if ( auto basic2 = dynamic_cast< const ast::BasicType * >( type2 ) ) {
				ast::BasicType::Kind kind;
				if (basic->kind != basic2->kind && !widen.first && !widen.second) return;
				else if (!widen.first) kind = basic->kind; // widen.second
				else if (!widen.second) kind = basic2->kind;
				else kind = commonTypes[ basic->kind ][ basic2->kind ];
				// xxx - what does qualifiers even do here??
				if ( (basic->qualifiers >= basic2->qualifiers || widen.first)
					&& (basic->qualifiers <= basic2->qualifiers || widen.second) ) {
					result = new ast::BasicType{ kind, basic->qualifiers | basic2->qualifiers };
				}
			} else if (
				dynamic_cast< const ast::ZeroType * >( type2 )
				|| dynamic_cast< const ast::OneType * >( type2 )
			) {
				if (widen.second) {
					result = new ast::BasicType{ basic->kind, basic->qualifiers | type2->qualifiers };
				}
			} else if ( const ast::EnumInstType * enumInst = dynamic_cast< const ast::EnumInstType * >( type2 ) ) {
				const ast::EnumDecl* enumDecl = enumInst->base;
				if ( enumDecl->base ) {
					result = enumDecl->base.get();
				} else {
					#warning remove casts when `commonTypes` moved to new AST
					ast::BasicType::Kind kind = commonTypes[ basic->kind ][ ast::BasicType::SignedInt ];
					if (
						( ( kind == basic->kind && basic->qualifiers >= type2->qualifiers )
							|| widen.first )
						&& ( ( kind != basic->kind && basic->qualifiers <= type2->qualifiers )
							|| widen.second )
					) {
						result = new ast::BasicType{ kind, basic->qualifiers | type2->qualifiers };
					}
				}
			}
		}

	private:
		template< typename Pointer >
		void getCommonWithVoidPointer( const Pointer * voidPtr, const Pointer * oPtr ) {
			const ast::Type * base = oPtr->base;
			if ( auto var = dynamic_cast< const ast::TypeInstType * >( base ) ) {
				auto entry = open.find( *var );
				if ( entry != open.end() ) {
					ast::AssertionSet need, have;
					if ( ! tenv.bindVar(
						var, voidPtr->base, entry->second, need, have, open, widen )
					) return;
				}
			}
			result = voidPtr;
			add_qualifiers( result, oPtr->qualifiers );
		}

		// For a typed enum, we want to unify type1 with the base type of the enum
		bool tryResolveWithTypedEnum( const ast::Type * type1 ) {
			if (auto enumInst = dynamic_cast<const ast::EnumInstType *> (type2) ) {
				ast::OpenVarSet newOpen{ open };
				if (enumInst->base->base 
				&& unifyExact(type1, enumInst->base->base, tenv, need, have, newOpen, widen)) {
					result = type1;
					return true;
				}
			}
			return false;
		}

	public:
		void postvisit( const ast::PointerType * pointer ) {
			if ( auto pointer2 = dynamic_cast< const ast::PointerType * >( type2 ) ) {
				if (
					widen.first
					&& pointer2->base.as< ast::VoidType >()
					&& ! ast::isFtype( pointer->base )
				) {
					getCommonWithVoidPointer( pointer2, pointer );
				} else if (
					widen.second
					&& pointer->base.as< ast::VoidType >()
					&& ! ast::isFtype( pointer2->base )
				) {
					getCommonWithVoidPointer( pointer, pointer2 );
				} else if (
					( pointer->base->qualifiers >= pointer2->base->qualifiers || widen.first )
					&& ( pointer->base->qualifiers <= pointer2->base->qualifiers || widen.second )
				) {
					ast::CV::Qualifiers q1 = pointer->base->qualifiers;
					ast::CV::Qualifiers q2 = pointer2->base->qualifiers;

					// force t{1,2} to be cloned if their qualifiers must be stripped, so that
					// pointer{,2}->base are unchanged
					ast::ptr< ast::Type > t1{ pointer->base }, t2{ pointer2->base };
					reset_qualifiers( t1 );
					reset_qualifiers( t2 );

					ast::OpenVarSet newOpen{ open };
					if ( unifyExact( t1, t2, tenv, have, need, newOpen, noWiden() ) ) {
						result = pointer;
						if ( q1.val != q2.val ) {
							// reset result->base->qualifiers to be union of two base qualifiers
							strict_dynamic_cast< ast::PointerType * >(
								result.get_and_mutate()
							)->base.get_and_mutate()->qualifiers = q1 | q2;
						}
					}
					else if ( isFtype (t1) && isFtype (t2) ) {
						auto f1 = t1.as<ast::FunctionType>();
						if (!f1) return;
						auto f2 = t2.strict_as<ast::FunctionType>();

						assertf(f1->returns.size() <= 1, "Function return should not be a list");
						assertf(f2->returns.size() <= 1, "Function return should not be a list");

						if (
							( f1->params.size() != f2->params.size() || f1->returns.size() != f2->returns.size() )
							&& ! f1->isTtype()
							&& ! f2->isTtype()
						) return;

						auto params1 = flattenList( f1->params, tenv );
						auto params2 = flattenList( f2->params, tenv );

						auto crnt1 = params1.begin();
						auto crnt2 = params2.begin();
						auto end1 = params1.end();
						auto end2 = params2.end();

						while (crnt1 != end1 && crnt2 != end2 ) {
							const ast::Type * arg1 = *crnt1;
							const ast::Type * arg2 = *crnt2;

							bool isTuple1 = Tuples::isTtype( t1 );
							bool isTuple2 = Tuples::isTtype( t2 );

							// assumes here that ttype *must* be last parameter
							if ( isTuple1 && ! isTuple2 ) {
								// combine remainder of list2, then unify
								if (unifyExact(
									arg1, tupleFromTypes( crnt2, end2 ), tenv, need, have, open,
									noWiden() )) {
										break;

								}
								else return;
							} else if ( ! isTuple1 && isTuple2 ) {
								// combine remainder of list1, then unify
								if (unifyExact(
									tupleFromTypes( crnt1, end1 ), arg2, tenv, need, have, open,
									noWiden() )) {
										break;

								}
								else return;
							}

							// allow qualifiers of pointer and reference base to become more specific
							if (auto ref1 = dynamic_cast<const ast::ReferenceType *> (arg1)) {
								if (auto ref2 = dynamic_cast<const ast::ReferenceType *> (arg2)) {
									ast::ptr<ast::Type> base1 = ref1->base;
									ast::ptr<ast::Type> base2 = ref2->base;

									// xxx - assume LHS is always the target type

									if ( ! ((widen.second && ref2->qualifiers.is_mutex) 
									|| (ref1->qualifiers.is_mutex == ref2->qualifiers.is_mutex ))) return;

									if ( (widen.second && base1->qualifiers <= base2->qualifiers ) || (base2->qualifiers == base1->qualifiers) ) {

										reset_qualifiers(base1);
										reset_qualifiers(base2);

										if ( ! unifyExact(
											base1, base2, tenv, need, have, open, noWiden() )
										) return;
									}	
								}
								else return;
							}
							else if (auto ptr1 = dynamic_cast<const ast::PointerType *> (arg1)) {
								if (auto ptr2 = dynamic_cast<const ast::PointerType *> (arg2)) {
									ast::ptr<ast::Type> base1 = ptr1->base;
									ast::ptr<ast::Type> base2 = ptr2->base;

									// xxx - assume LHS is always the target type
									// a function accepting const can always be called by non-const arg

									if ( (widen.second && base1->qualifiers <= base2->qualifiers ) || (base2->qualifiers == base1->qualifiers) ) {

										reset_qualifiers(base1);
										reset_qualifiers(base2);

										if ( ! unifyExact(
											base1, base2, tenv, need, have, open, noWiden() )
										) return;
									}	
								}
								else return;

							}
							else if (! unifyExact(
								arg1, arg2, tenv, need, have, open, noWiden() )) return;

							++crnt1; ++crnt2;
						}
						if ( crnt1 != end1 ) {
							// try unifying empty tuple with ttype
							const ast::Type * t1 = *crnt1;
							if (! Tuples::isTtype( t1 ) ) return;
							if (! unifyExact(
								t1, tupleFromTypes( crnt2, end2 ), tenv, need, have, open,
								noWiden() )) return;
						} else if ( crnt2 != end2 ) {
							// try unifying empty tuple with ttype
							const ast::Type * t2 = *crnt2;
							if (! Tuples::isTtype( t2 ) ) return;
							if (! unifyExact(
								tupleFromTypes( crnt1, end1 ), t2, tenv, need, have, open,
								noWiden() )) return;
						}
						if ((f1->returns.size() == 0 && f2->returns.size() == 0)
						  || (f1->returns.size() == 1 && f2->returns.size() == 1 && unifyExact(f1->returns[0], f2->returns[0], tenv, need, have, open, noWiden()))) {
							result = pointer;

							for (auto & assn : f1->assertions) {
								auto i = need.find(assn);
								if (i != need.end()) i->second.isUsed = true;
								auto j = have.find(assn);
								if (j != have.end()) j->second.isUsed = true;
							}

							for (auto & assn : f2->assertions) {
								auto i = need.find(assn);
								if (i != need.end()) i->second.isUsed = true;
								auto j = have.find(assn);
								if (j != have.end()) j->second.isUsed = true;
							}

						}
					} // if ftype
					
				}
			} else if ( widen.second && dynamic_cast< const ast::ZeroType * >( type2 ) ) {
				result = pointer;
				add_qualifiers( result, type2->qualifiers );
			} else {
				tryResolveWithTypedEnum( pointer );
			}
		}

		void postvisit( const ast::ArrayType * arr ) {
			// xxx - does it make sense? 
			tryResolveWithTypedEnum( arr );
		}

		void postvisit( const ast::ReferenceType * ref ) {
			if ( auto ref2 = dynamic_cast< const ast::ReferenceType * >( type2 ) ) {
				if (
					widen.first && ref2->base.as< ast::VoidType >() && ! ast::isFtype( ref->base )
				) {
					getCommonWithVoidPointer( ref2, ref );
				} else if (
					widen.second && ref->base.as< ast::VoidType>() && ! ast::isFtype( ref2->base )
				) {
					getCommonWithVoidPointer( ref, ref2 );
				} else if (
					( ref->base->qualifiers >= ref2->base->qualifiers || widen.first )
					&& ( ref->base->qualifiers <= ref2->base->qualifiers || widen.second )
				) {
					ast::CV::Qualifiers q1 = ref->base->qualifiers, q2 = ref2->base->qualifiers;

					// force t{1,2} to be cloned if their qualifiers must be stripped, so that
					// ref{,2}->base are unchanged
					ast::ptr< ast::Type > t1{ ref->base }, t2{ ref2->base };
					reset_qualifiers( t1 );
					reset_qualifiers( t2 );

					ast::OpenVarSet newOpen{ open };
					if ( unifyExact( t1, t2, tenv, have, need, newOpen, noWiden() ) ) {
						result = ref;
						if ( q1.val != q2.val ) {
							// reset result->base->qualifiers to be union of two base qualifiers
							strict_dynamic_cast< ast::ReferenceType * >(
								result.get_and_mutate()
							)->base.get_and_mutate()->qualifiers = q1 | q2;
						}
					}
				}
			} else if ( widen.second && dynamic_cast< const ast::ZeroType * >( type2 ) ) {
				result = ref;
				add_qualifiers( result, type2->qualifiers );
			} else {
				if (!dynamic_cast<const ast::EnumInstType *>(type2))
					result = commonType( type2, ref, tenv, need, have, open, widen );
			}
		}

		void postvisit( const ast::FunctionType * func) {
			tryResolveWithTypedEnum( func ); 
		}

		void postvisit( const ast::StructInstType * inst ) {
			tryResolveWithTypedEnum( inst );
		}

		void postvisit( const ast::UnionInstType * inst ) {
			tryResolveWithTypedEnum( inst );
		}

		void postvisit( const ast::EnumInstType * enumInst ) {
			if (!dynamic_cast<const ast::EnumInstType *>(type2))
				result = commonType( type2, enumInst, tenv, need, have, open, widen);
		}

		void postvisit( const ast::TraitInstType * ) {}

		void postvisit( const ast::TypeInstType * ) {}

		void postvisit( const ast::TupleType * tuple ) {
			tryResolveWithTypedEnum( tuple );
		}

		void postvisit( const ast::VarArgsType * ) {}

		void postvisit( const ast::ZeroType * zero ) {
			if ( ! widen.first ) return;
			if ( dynamic_cast< const ast::BasicType * >( type2 )
				|| dynamic_cast< const ast::PointerType * >( type2 ) ) {
				if ( widen.second || zero->qualifiers <= type2->qualifiers ) {
					result = type2;
					add_qualifiers( result, zero->qualifiers );
				}
			} else if ( widen.second && dynamic_cast< const ast::OneType * >( type2 ) ) {
				result = new ast::BasicType{
					ast::BasicType::SignedInt, zero->qualifiers | type2->qualifiers };
			} else if ( const ast::EnumInstType * enumInst = dynamic_cast< const ast::EnumInstType * >( type2 ) ) {
				const ast::EnumDecl * enumDecl = enumInst->base;
				if ( enumDecl->base ) {
					if ( tryResolveWithTypedEnum( zero ) ) 
						add_qualifiers( result, zero->qualifiers );
				} else {
					if ( widen.second || zero->qualifiers <= type2->qualifiers ) {
						result = type2;
						add_qualifiers( result, zero->qualifiers );
					}
				}
			}
		}

		void postvisit( const ast::OneType * one ) {
			if ( ! widen.first ) return;
			if ( dynamic_cast< const ast::BasicType * >( type2 ) ) {
				if ( widen.second || one->qualifiers <= type2->qualifiers ) {
					result = type2;
					add_qualifiers( result, one->qualifiers );
				}
			} else if ( widen.second && dynamic_cast< const ast::ZeroType * >( type2 ) ) {
				result = new ast::BasicType{
					ast::BasicType::SignedInt, one->qualifiers | type2->qualifiers };
			} else if ( const ast::EnumInstType * enumInst = dynamic_cast< const ast::EnumInstType * >( type2 ) ) {
				const ast::EnumDecl * enumBase = enumInst->base;
				if ( enumBase->base ) {
					if ( tryResolveWithTypedEnum( one ))
						add_qualifiers( result, one->qualifiers );
				} else {
					if ( widen.second || one->qualifiers <= type2->qualifiers ) {
						result = type2;
						add_qualifiers( result, one->qualifiers );
					}
				}
			}
		}

	};

	// size_t CommonType_new::traceId = Stats::Heap::new_stacktrace_id("CommonType_new");
	namespace {
		ast::ptr< ast::Type > handleReference(
			const ast::ptr< ast::Type > & t1, const ast::ptr< ast::Type > & t2, WidenMode widen,
			ast::TypeEnvironment & env,
			const ast::OpenVarSet & open
		) {
			ast::ptr<ast::Type> common;
			ast::AssertionSet have, need;
			ast::OpenVarSet newOpen{ open };

			// need unify to bind type variables
			if ( unify( t1, t2, env, have, need, newOpen, common ) ) {
				ast::CV::Qualifiers q1 = t1->qualifiers, q2 = t2->qualifiers;
				PRINT(
					std::cerr << "unify success: " << widenFirst << " " << widenSecond << std::endl;
				)
				if ( ( widen.first || q2 <= q1 ) && ( widen.second || q1 <= q2 ) ) {
					PRINT(
						std::cerr << "widen okay" << std::endl;
					)
					add_qualifiers( common, q1 | q2 );
					return common;
				}
			}

			PRINT(
				std::cerr << "exact unify failed: " << t1 << " " << t2 << std::endl;
			)
			return { nullptr };
		}
	}

	ast::ptr< ast::Type > commonType(
			const ast::ptr< ast::Type > & type1, const ast::ptr< ast::Type > & type2,
			ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
			const ast::OpenVarSet & open, WidenMode widen
	) {
		unsigned depth1 = type1->referenceDepth();
		unsigned depth2 = type2->referenceDepth();

		if ( depth1 != depth2 ) {  // implies depth1 > 0 || depth2 > 0
			PRINT(
				std::cerr << "reference depth diff: " << (depth1-depth2) << std::endl;
			)
			ast::ptr< ast::Type > result;
			const ast::ReferenceType * ref1 = type1.as< ast::ReferenceType >();
			const ast::ReferenceType * ref2 = type2.as< ast::ReferenceType >();

			if ( depth1 > depth2 ) {
				assert( ref1 );
				result = handleReference( ref1->base, type2, widen, env, open );
			} else {  // implies depth1 < depth2
				assert( ref2 );
				result = handleReference( type1, ref2->base, widen, env, open );
			}

			if ( result && ref1 ) {
				// formal is reference, so result should be reference
				PRINT(
					std::cerr << "formal is reference; result should be reference" << std::endl;
				)
				result = new ast::ReferenceType{ result, ref1->qualifiers };
			}

			PRINT(
				std::cerr << "common type of reference [" << type1 << "] and [" << type2 << "] is "
				"[" << result << "]" << std::endl;
			)
			return result;
		}
		// otherwise both are reference types of the same depth and this is handled by the visitor
		ast::Pass<CommonType_new> visitor{ type2, widen, env, open, need, have };
		type1->accept( visitor );
		// ast::ptr< ast::Type > result = visitor.core.result;

		return visitor.core.result;
	}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
