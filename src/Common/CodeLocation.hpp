//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CodeLocation.hpp --
//
// Author           : Andrew Beach
// Created On       : Thr Aug 17 11:23:00 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Aug 28 12:46:01 2017
// Update Count     : 2
//

#pragma once

#include <iostream>
#include <string>

struct CodeLocation {
	int first_line = -1, first_column = -1, last_line = -1, last_column = -1;
	std::string filename = "";

	/// Create a new unset CodeLocation.
	CodeLocation() = default;

	/// Create a new CodeLocation with the given values.
	CodeLocation( const char* filename, int lineno )
		: first_line( lineno )
		, filename(filename ? filename : "")
	{}

	CodeLocation( const CodeLocation& rhs ) = default;
	CodeLocation( CodeLocation&& rhs ) = default;
	CodeLocation& operator=( const CodeLocation & ) = default;
	CodeLocation& operator=( CodeLocation && ) = default;

	bool isSet () const {
		return -1 != first_line;
	}

	bool isUnset () const {
		return !isSet();
	}

	bool startsBefore( CodeLocation const & other ) const {
		if( filename < other.filename ) return true;
		if( filename > other.filename ) return false;

		if( first_line < other.first_line ) return true;
		if( first_line > other.first_line ) return false;

		if( last_line < other.last_line ) return true;
		return false;
	}

	bool followedBy( CodeLocation const & other, int seperation ) const {
		return (first_line + seperation == other.first_line &&
		        filename == other.filename);
	}

	bool operator==( CodeLocation const & other ) const {
		return followedBy( other, 0 );
	}

	bool operator!=( CodeLocation const & other ) const {
		return !(*this == other);
	}
};

inline std::ostream & operator<<( std::ostream & out, const CodeLocation & location ) {
	// Column number ":1" allows IDEs to parse the error message and position the cursor in the source text.
	return location.isSet() ? out << location.filename << ":" << location.first_line << ":1 " : out;
}
