//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// UniqueName.h -- 
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul 21 22:18:45 2017
// Update Count     : 2
//

#pragma once

#include <string>

class UniqueName {
  public:
	UniqueName( const std::string &base = "" );
	std::string newName( const std::string &additional = "" );
  private:
	std::string base;
	int count;
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
