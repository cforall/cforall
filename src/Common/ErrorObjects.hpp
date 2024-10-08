//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ErrorObjects.hpp --
//
// Author           : Thierry Delisle
// Created On       : Wed Feb 28 15:16:47 2018
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once


#include <exception>	// for exception
#include <iostream>	// for ostream
#include <list>		// for list
#include <string>		// for string
#include <unistd.h>	// for isatty

#include "CodeLocation.hpp"								// for CodeLocation, toString

struct error {
	CodeLocation location;
	std::string description;

	error() = default;
	error( CodeLocation loc, const std::string & str ) : location( loc ), description( str ) {}
};

class SemanticErrorException : public std::exception {
  public:
	SemanticErrorException() = default;
	SemanticErrorException( CodeLocation location, std::string error );
	~SemanticErrorException() throw() {}

	void append( SemanticErrorException & other );
	void append( CodeLocation location, const std::string & );
	void throwIfNonEmpty();
	void print();
  private:
	std::list< error > errors;
};
