//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ScrubTypeVars.hpp -- Remove polymorphic types.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Dec  7 16:57:00 2022
// Update Count     : 5
//

#pragma once

#include <cassert>            // for strict_dynamic_cast

#include "AST/Fwd.hpp"        // for Node
#include "GenPoly.h"          // for TypeVarMap

namespace GenPoly {

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
