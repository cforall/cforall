//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// HandleAttributes.h --
//
// Author           : Rob Schluntz
// Created On       : Fri Jul 27 10:10:10 2018
// Last Modified By : Rob Schluntz
// Last Modified On : Fri Jul 27 10:12:53 2018
// Update Count     : 2
//

#pragma once

#include <list>  // for list

class Declaration;

namespace Validate {
	void handleAttributes( std::list< Declaration * > &translationUnit );
} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
