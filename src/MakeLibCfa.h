//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// MakeLibCfa.h -- 
//
// Author           : Richard C. Bilson
// Created On       : Sat May 16 10:42:14 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:31:35 2017
// Update Count     : 2
//

#pragma once

#include <list>  // for list

class Declaration;
namespace ast {
	struct TranslationUnit;
}

namespace LibCfa {
	void makeLibCfa( std::list< Declaration* > &prelude );
	void makeLibCfa( ast::TranslationUnit & translationUnit );
} // namespace LibCfa

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
