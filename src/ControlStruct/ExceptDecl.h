//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExceptDecl.h --
//
// Author           : Henry Xue
// Created On       : Tue Jul 20 04:10:50 2021
// Last Modified By : Henry Xue
// Last Modified On : Tue Jul 20 04:10:50 2021
// Update Count     : 1
//

#pragma once

#include <list>  // for list

class Declaration;

namespace ControlStruct {
	void translateExcept( std::list< Declaration *> & translationUnit );
}
