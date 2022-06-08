//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Waitfor.h --
//
// Author           : Thierry Delisle
// Created On       : Mon Aug 28 11:03:31 2017
// Last Modified By :
// Last Modified On :
// Update Count     : 1
//

#pragma once

#include <list>  // for list

class Declaration;
namespace ast {
	class TranslationUnit;
}

namespace Concurrency {
	void generateWaitFor( std::list< Declaration * > & translationUnit );

void generateWaitFor( ast::TranslationUnit & translationUnit );
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
