//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Keywords.h --
//
// Author           : Thierry Delisle
// Created On       : Fri Mar 10 15:16:42 2017
// Last Modified By :
// Last Modified On :
// Update Count     : 1
//

#pragma once

#include <list>  // for list

class Declaration;

namespace Concurrency {
	void applyKeywords( std::list< Declaration * > & translationUnit );
	void implementMutexFuncs( std::list< Declaration * > & translationUnit );
	void implementThreadStarter( std::list< Declaration * > & translationUnit );
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
