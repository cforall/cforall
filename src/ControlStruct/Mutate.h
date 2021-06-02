//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Mutate.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:17:59 2017
// Update Count     : 3
//

#pragma once

#include <list>  // for list

class Declaration;

/// Desugars Cforall control structures
namespace ControlStruct {
	/// normalizes label definitions and generates multi-level exit labels
	void fixLabels( std::list< Declaration * > & translationUnit );

	/// hoist initialization out of for statements
	void hoistControlDecls( std::list< Declaration * > & translationUnit );
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
