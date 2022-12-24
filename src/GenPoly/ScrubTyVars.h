//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ScrubTyVars.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Dec  7 16:57:00 2022
// Update Count     : 5
//

#pragma once

#include <cassert>            // for assert

#include "AST/Fwd.hpp"        // for Node
#include "Common/PassVisitor.h"
#include "GenPoly.h"          // for TyVarMap, isPolyType, isDynType
#include "SynTree/Mutator.h"  // for Mutator
#include "SynTree/Type.h"     // for Type (ptr only), PointerType (ptr only)

class AlignofExpr;
class Expression;
class SizeofExpr;

namespace GenPoly {
	struct ScrubTyVars : public WithVisitorRef<ScrubTyVars>, public WithShortCircuiting, public WithGuards {
		/// Whether to scrub all type variables from the provided map, dynamic type variables from the provided map, or all type variables
		enum ScrubMode { FromMap, DynamicFromMap, All };

		ScrubTyVars() : tyVars(nullptr), mode( All ) {}

		ScrubTyVars( const TyVarMap &tyVars, ScrubMode mode = FromMap ): tyVars( &tyVars ), mode( mode ) {}

	public:
		/// For all polymorphic types with type variables in `tyVars`, replaces generic types, dtypes, and ftypes with the appropriate void type,
		/// and sizeof/alignof expressions with the proper variable
		template< typename SynTreeClass >
		static SynTreeClass *scrub( SynTreeClass *target, const TyVarMap &tyVars );

		/// For all dynamic-layout types with type variables in `tyVars`, replaces generic types, dtypes, and ftypes with the appropriate void type,
		/// and sizeof/alignof expressions with the proper variable
		template< typename SynTreeClass >
		static SynTreeClass *scrubDynamic( SynTreeClass *target, const TyVarMap &tyVars );

		/// For all polymorphic types, replaces generic types, dtypes, and ftypes with the appropriate void type,
		/// and sizeof/alignof expressions with the proper variable
		template< typename SynTreeClass >
		static SynTreeClass *scrubAll( SynTreeClass *target );

		/// determine if children should be visited based on whether base type should be scrubbed.
		void primeBaseScrub( Type * );

		void premutate( TypeInstType * ) { visit_children = false; }
		void premutate( StructInstType * ) { visit_children = false; }
		void premutate( UnionInstType * ) { visit_children = false; }
		void premutate( SizeofExpr * szeof ) { primeBaseScrub( szeof->type ); }
		void premutate( AlignofExpr * algnof ) { primeBaseScrub( algnof->type ); }
		void premutate( PointerType * pointer ) { primeBaseScrub( pointer->base ); }

		Type * postmutate( TypeInstType * typeInst );
		Type * postmutate( StructInstType * structInst );
		Type * postmutate( UnionInstType * unionInst );
		Expression * postmutate( SizeofExpr * szeof );
		Expression * postmutate( AlignofExpr * algnof );
		Type * postmutate( PointerType * pointer );

	  private:
		/// Returns the type if it should be scrubbed, NULL otherwise.
		Type* shouldScrub( Type *ty ) {
			switch ( mode ) {
			case FromMap: return isPolyType( ty, *tyVars );
			case DynamicFromMap: return isDynType( ty, *tyVars );
			case All: return isPolyType( ty );
			}
			assert(false); return nullptr; // unreachable
			// return dynamicOnly ? isDynType( ty, tyVars ) : isPolyType( ty, tyVars );
		}

		/// Mutates (possibly generic) aggregate types appropriately
		Type* mutateAggregateType( Type *ty );

		const TyVarMap *tyVars;  ///< Type variables to scrub
		ScrubMode mode;          ///< which type variables to scrub? [FromMap]

		Type * dynType = nullptr; ///< result of shouldScrub
	};

	template< typename SynTreeClass >
	SynTreeClass * ScrubTyVars::scrub( SynTreeClass *target, const TyVarMap &tyVars ) {
		PassVisitor<ScrubTyVars> scrubber( tyVars );
		return static_cast< SynTreeClass * >( target->acceptMutator( scrubber ) );
	}

	template< typename SynTreeClass >
	SynTreeClass * ScrubTyVars::scrubDynamic( SynTreeClass *target, const TyVarMap &tyVars ) {
		PassVisitor<ScrubTyVars> scrubber( tyVars, ScrubTyVars::DynamicFromMap );
		return static_cast< SynTreeClass * >( target->acceptMutator( scrubber ) );
	}

	template< typename SynTreeClass >
	SynTreeClass * ScrubTyVars::scrubAll( SynTreeClass *target ) {
		PassVisitor<ScrubTyVars> scrubber;
		return static_cast< SynTreeClass * >( target->acceptMutator( scrubber ) );
	}

// ScrubMode and scrubTypeVarsBase are internal.
enum class ScrubMode { FromMap, DynamicFromMap, All };

const ast::Node * scrubTypeVarsBase(
	const ast::Node * target, const TypeVarMap * typeVars, ScrubMode mode );


/// For all polymorphic types with type variables in `typeVars`,
/// replaces generic types, dtypes, and ftypes with the appropriate void type,
/// and sizeof/alignof expressions with the proper variable.
template<typename node_t>
node_t const * scrubTypeVars(
		node_t const * target, const TypeVarMap & typeVars ) {
	return strict_dynamic_cast<node_t const *>(
			scrubTypeVarsBase( target, &typeVars, ScrubMode::FromMap ) );
}

/// For all dynamic-layout types with type variables in `typeVars`,
/// replaces generic types, dtypes, and ftypes with the appropriate void type,
/// and sizeof/alignof expressions with the proper variable.
template<typename node_t>
node_t const * scrubTypeVarsDynamic(
		node_t const * target, const TypeVarMap & typeVars ) {
	return strict_dynamic_cast<node_t const *>(
			scrubTypeVarsBase( target, &typeVars, ScrubMode::DynamicFromMap ) );
}

/// For all polymorphic types, replaces generic types, with the appropriate
/// void type, and sizeof/alignof expressions with the proper variable.
template<typename node_t>
node_t const * scrubAllTypeVars( node_t const * target ) {
	return strict_dynamic_cast<node_t const *>(
			scrubTypeVarsBase( target, nullptr, ScrubMode::All ) );
}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
