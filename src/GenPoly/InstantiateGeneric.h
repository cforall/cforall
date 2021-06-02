//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// InstantiateGeneric.h --
//
// Author           : Aaron B. Moss
// Created On       : Thu Aug 04 18:33:00 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:22:42 2017
// Update Count     : 2
//

#pragma once

#include <list>  // for list

class Declaration;

namespace GenPoly {
	/// Replaces all generic types that have static layout with concrete instantiations.
	/// Types with concrete values for otype parameters will be template-expanded, while
	/// dtype and ftype parameters will be replaced by the appropriate void type.
	void instantiateGeneric( std::list< Declaration* > &translationUnit );
} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
