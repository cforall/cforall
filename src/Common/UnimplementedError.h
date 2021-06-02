//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// UnimplementedError.h -- 
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul 21 22:18:35 2017
// Update Count     : 2
//

#pragma once

#include <string>

class UnimplementedError : public std::exception {
  public:
	UnimplementedError();
	UnimplementedError( std::string what ) : what( what ) {}
	~UnimplementedError() throw () {}
  
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
