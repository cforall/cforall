//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ToString.hpp -- Tools to convert to string.
//
// Author           : Andrew Beach
// Created On       : Tue Mar 28  9:24:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Tue Mar 28  9:24:00 2023
// Update Count     : 0
//

#pragma once

#include <string>
#include <sstream>

/// Convert all arguments to strings and concatenate them.
template<typename... Params>
std::string toString( const Params &... params ) {
	std::ostringstream buffer;
	(buffer << ... << params);
	return buffer.str();
}

/// Convert all arguments to a C-string.
/// It is a macro so that a underlying std::string manages the memory.
#define toCString( ... ) toString( __VA_ARGS__ ).c_str()
