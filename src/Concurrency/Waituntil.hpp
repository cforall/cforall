//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Waituntil.hpp --
//
// Author           : Thierry Delisle
// Created On       : Mon Aug 28 11:03:31 2017
// Last Modified By :
// Last Modified On :
// Update Count     : 1
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Concurrency {

void generateWaitUntil( ast::TranslationUnit & translationUnit );

};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
