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
// Last Modified On : Thu Jun  7 08:05:26 2018
// Update Count     : 10
//

#include <cstdarg>
#include <cstdio>										// for fileno, stderr
#include <cstring>
#include <unistd.h>										// for isatty
#include <iostream>										// for basic_ostream, operator<<, ostream
#include <list>											// for list, _List_iterator
#include <string>										// for string, operator<<, operator+, to_string
#include <vector>

#include "Common/utility.h"								// for to_string, CodeLocation (ptr only)
#include "SemanticError.h"

//-----------------------------------------------------------------------------
// Severity Handling
std::vector<Severity> & get_severities() {
	static std::vector<Severity> severities;
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
		if ( std::strcmp( name, w.name ) == 0 ) {
			get_severities()[idx] = s;
			break;
		}
		idx++;
	}
}

//-----------------------------------------------------------------------------
// Semantic Error
bool SemanticErrorThrow = false;

SemanticErrorException::SemanticErrorException( CodeLocation location, std::string error ) {
	append( location, error );
}

void SemanticErrorException::append( SemanticErrorException &other ) {
	errors.splice( errors.end(), other.errors );
}

void SemanticErrorException::append( CodeLocation location, const std::string & msg ) {
	errors.emplace_back( location, msg );
}

bool SemanticErrorException::isEmpty() const {
	return errors.empty();
}

void SemanticErrorException::print() {
	using std::to_string;

	errors.sort([](const error & lhs, const error & rhs) -> bool {
		if(lhs.location.startsBefore(rhs.location)) return true;
		if(rhs.location.startsBefore(lhs.location)) return false;

		return lhs.description < rhs.description;
	});

	for( auto err : errors ) {
		std::cerr << ErrorHelpers::bold() << err.location << ErrorHelpers::error_str() << ErrorHelpers::reset_font() << err.description << std::endl;
	}
}

void SemanticError( CodeLocation location, std::string error ) {
	SemanticErrorThrow = true;
	throw SemanticErrorException( location, error );
}

namespace {
	// convert format string and arguments into a single string
	std::string fmtToString(const char * fmt, va_list ap) {
		int size = 128;
		while ( true ) {
			char buf[size];
			va_list args;
			va_copy( args, ap );
			int n = vsnprintf(&buf[0], size, fmt, args);
			va_end( args );
			if ( n < size && n >= 0 ) return buf;
			size *= 2;
		}
		assert( false );
	}
}

void SemanticWarningImpl( CodeLocation location, Warning warning, const char * const fmt, ... ) {
	Severity severity = get_severities()[(int)warning];
	switch(severity) {
	case Severity::Suppress :
		break;
	case Severity::Warn :
		{
			va_list args;
			va_start(args, fmt);
			std::string msg = fmtToString( fmt, args );
			va_end(args);
			std::cerr << ErrorHelpers::bold() << location << ErrorHelpers::warning_str() << ErrorHelpers::reset_font() << msg << std::endl;
		}
		break;
	case Severity::Error :
		{
			va_list args;
			va_start(args, fmt);
			std::string msg = fmtToString( fmt, args );
			va_end(args);
			SemanticError(location, msg);
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

	const std::string & error_str() {
		static std::string str = with_colors() ? "\e[31merror:\e[39m " : "error: ";
		return str;
	}

	const std::string & warning_str() {
		static std::string str = with_colors() ? "\e[95mwarning:\e[39m " : "warning: ";
		return str;
	}

	const std::string & bold_ttycode() {
		static std::string str = with_colors() ? "\e[1m" : "";
		return str;
	}

	const std::string & reset_font_ttycode() {
		static std::string str = with_colors() ? "\e[0m" : "";
		return str;
	}

	std::string make_bold( const std::string & str ) {
		return bold_ttycode() + str + reset_font_ttycode();
	}

	std::ostream & operator<<(std::ostream & os, bold) {
		os << bold_ttycode();
		return os;
	}

	std::ostream & operator<<(std::ostream & os, reset_font) {
		os << reset_font_ttycode();
		return os;
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
