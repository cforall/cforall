//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExplodedActual.h --
//
// Author           : Aaron B. Moss
// Created On       : Tue Nov 22 17:06:00 2017
// Last Modified By : Aaron B. Moss
// Last Modified On : Tue Nov 22 17:06:00 2017
// Update Count     : 1
//

#pragma once

#include <memory>
#include <vector>

#include "Alternative.h"      // for Alternative, AltList
#include "Cost.h"             // for Cost
#include "TypeEnvironment.h"  // for TypeEnvironment
#include "SymTab/Indexer.h"   // for Indexer

namespace ResolvExpr {
	/// Pre-exploded actual
	struct ExplodedActual {
		TypeEnvironment env;
		Cost cost;
		std::vector< std::unique_ptr<Expression> > exprs;

		ExplodedActual() : env(), cost(Cost::zero), exprs() {}
		ExplodedActual( const Alternative& actual, const SymTab::Indexer& indexer );
		ExplodedActual(ExplodedActual&&) = default;
		ExplodedActual& operator= (ExplodedActual&&) = default;
	};
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
