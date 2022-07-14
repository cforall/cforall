//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Validate.h -- This class is intended to perform pre-processing of declarations, validating their correctness and
//               computing some auxilliary data that is necessary for the indexer.
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 21:53:34 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jul 12 15:30:00 2022
// Update Count     : 6
//

#pragma once

#include <list>  // for list

class Declaration;

namespace SymTab {
	/// Normalizes struct and function declarations
	void validate( std::list< Declaration * > &translationUnit, bool doDebug = false );
} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
