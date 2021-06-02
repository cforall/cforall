//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CompilerError.h -- 
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul 21 22:18:07 2017
// Update Count     : 4
//

#pragma once

#include <string>

class CompilerError : public std::exception {
  public:
	CompilerError();
	CompilerError( std::string what ) : what( what ) {}
	~CompilerError() throw () {}

	std::string get_what() const { return what; }
	void set_what( std::string newValue ) { what = newValue; }
  private:
	std::string what;
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
