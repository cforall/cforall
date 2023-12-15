//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SemanticError.cc --
//
// Author           : Thierry Delisle
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Dec 14 13:45:28 2023
// Update Count     : 34
//

#include <cstdarg>
#include <cstdio>										// for fileno, stderr
#include <cstring>
#include <unistd.h>										// for isatty
#include <iostream>										// for basic_ostream, operator<<, ostream
#include <list>											// for list, _List_iterator
#include <string>										// for string, operator<<, operator+, to_string
#include <vector>

using namespace std;

#include "Common/utility.h"								// for to_string, CodeLocation (ptr only)
#include "SemanticError.h"

//-----------------------------------------------------------------------------
// Severity Handling
vector<Severity> & get_severities() {
	static vector<Severity> severities;
	if(severities.empty()) {
		severities.reserve((size_t)Warning::NUMBER_OF_WARNINGS);
		for ( const auto w : WarningFormats ) {
			severities.push_back( w.default_severity );
		} // for
	}
	return severities;
}

void SemanticWarning_SuppressAll() {
	for( auto & s : get_severities() ) {
		s = Severity::Suppress;
	}
}

void SemanticWarning_EnableAll() {
	for( auto & s : get_severities() ) {
		s = Severity::Warn;
	}
}

void SemanticWarning_WarningAsError() {
	for( auto & s : get_severities() ) {
		if(s == Severity::Warn) s = Severity::Error;
	}
}

void SemanticWarning_Set(const char * const name, Severity s) {
	size_t idx = 0;
	for ( const auto & w : WarningFormats ) {
		if ( strcmp( name, w.name ) == 0 ) {
			get_severities()[idx] = s;
			break;
		}
		idx++;
	}
}

//-----------------------------------------------------------------------------
// Semantic Error

bool SemanticErrorThrow = false;

SemanticErrorException::SemanticErrorException( CodeLocation location, string error ) {
	append( location, error );
}

void SemanticErrorException::append( SemanticErrorException &other ) {
	errors.splice( errors.end(), other.errors );
}

void SemanticErrorException::append( CodeLocation location, const string & msg ) {
	errors.emplace_back( location, msg );
}

bool SemanticErrorException::isEmpty() const {
	return errors.empty();
}

void SemanticErrorException::print() {
//	using to_string;

	errors.sort([](const error & lhs, const error & rhs) -> bool {
		if(lhs.location.startsBefore(rhs.location)) return true;
		if(rhs.location.startsBefore(lhs.location)) return false;

		return lhs.description < rhs.description;
	});

	for( auto err : errors ) {
		cerr << ErrorHelpers::bold() << err.location << ErrorHelpers::error_str() << ErrorHelpers::reset_font() << err.description << endl;
	}
}

void SemanticError( CodeLocation location, const char * fmt, ... ) {
	char msg[2048];										// worst-case error-message buffer
	va_list args;
	va_start( args, fmt );
	vsnprintf( msg, sizeof(msg), fmt, args );			// always null terminated, but may be truncated
	va_end( args );

	SemanticErrorThrow = true;
	throw SemanticErrorException( location, msg );		// convert msg to string
}

void SemanticWarning( CodeLocation location, Warning warning, ... ) {
	Severity severity = get_severities()[(int)warning];

	switch ( severity ) {
	case Severity::Suppress :
		break;
	case Severity::Warn :
	case Severity::Error :
		{
			char msg[2048];								// worst-case error-message buffer
			va_list args;
			va_start( args, warning );
			vsnprintf( msg, sizeof(msg), WarningFormats[(int)warning].message, args ); // always null terminated, but may be truncated
			va_end( args );

			if ( severity == Severity::Warn ) {
				cerr << ErrorHelpers::bold() << location << ErrorHelpers::warning_str() << ErrorHelpers::reset_font() << msg << endl;
			} else {
				SemanticError( location, string( msg ) );
			}
		}
		break;
	case Severity::Critical :
		assertf(false, "Critical errors not implemented yet");
		break;
	}
}

//-----------------------------------------------------------------------------
// Helpers
namespace ErrorHelpers {
	Colors colors = Colors::Auto;

	static inline bool with_colors() {
		return colors == Colors::Auto ? isatty( STDERR_FILENO ) : bool(colors);
	}

	const string & error_str() {
		static string str = with_colors() ? "\e[31merror:\e[39m " : "error: ";
		return str;
	}

	const string & warning_str() {
		static string str = with_colors() ? "\e[95mwarning:\e[39m " : "warning: ";
		return str;
	}

	const string & bold_ttycode() {
		static string str = with_colors() ? "\e[1m" : "";
		return str;
	}

	const string & reset_font_ttycode() {
		static string str = with_colors() ? "\e[0m" : "";
		return str;
	}

	string make_bold( const string & str ) {
		return bold_ttycode() + str + reset_font_ttycode();
	}

	ostream & operator<<(ostream & os, bold) {
		os << bold_ttycode();
		return os;
	}

	ostream & operator<<(ostream & os, reset_font) {
		os << reset_font_ttycode();
		return os;
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
