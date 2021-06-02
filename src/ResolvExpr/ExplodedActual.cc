//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExplodedActual.cc --
//
// Author           : Aaron B. Moss
// Created On       : Tue Nov 22 17:06:00 2017
// Last Modified By : Aaron B. Moss
// Last Modified On : Tue Nov 22 17:06:00 2017
// Update Count     : 1
//

#include "ExplodedActual.h"

#include "Tuples/Explode.h"   // for Tuples::explode

namespace ResolvExpr {
	ExplodedActual::ExplodedActual( const Alternative& actual, const SymTab::Indexer& indexer ) 
			: env(actual.env), cost(actual.cost), exprs() {
		Tuples::explode( actual, indexer, *this );
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
