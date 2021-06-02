//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolveAssertions.h --
//
// Author           : Aaron B. Moss
// Created On       : Fri Oct 05 13:46:00 2018
// Last Modified By : Aaron B. Moss
// Last Modified On : Fri Oct 05 13:46:00 2018
// Update Count     : 1
//

#pragma once

#include "Alternative.h"  // for Alternative, AltList

namespace SymTab {
	class Indexer;
}

namespace ResolvExpr {
	/// Recursively resolves all assertions provided in an alternative; returns true iff succeeds
	void resolveAssertions( Alternative& alt, const SymTab::Indexer& indexer, AltList& out, std::list<std::string>& errors );
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
