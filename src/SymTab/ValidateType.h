//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ValidateType.h -- Validate and normalize types.
//
// Author           : Andrew Beach
// Created On       : Mon May 16 16:18:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon May 16 16:18:00 2022
// Update Count     : 0
//

#pragma once

#include <list>

class Declaration;
class Type;

namespace SymTab {
	class Indexer;

	void validateType( Type *type, const Indexer *indexer );

	// Sub-passes that are also used by the larger validate pass.
	void decayEnumsAndPointers( std::list< Declaration * > & translationUnit );
	void linkReferenceToTypes( std::list< Declaration * > & translationUnit );
	void decayForallPointers( std::list< Declaration * > & translationUnit );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
