//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LabelGenerator.cpp --
//
// Author           : Peter A. Buhr
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Mar 28 10:03:00 2022
// Update Count     : 73
//

#include "LabelGenerator.hpp"

#include "AST/Attribute.hpp"
#include "AST/Label.hpp"
#include "AST/Stmt.hpp"

using namespace std;
using namespace ast;

namespace ControlStruct {

enum { size = 128 };

static int newLabelPre( char buf[size], const string & suffix ) {
	static int current = 0;

	int len = snprintf( buf, size, "__L%d__%s", current++, suffix.c_str() );
	assertf( len < size, "CFA Internal error: buffer overflow creating label" );
	return len;
}

static Label newLabelPost( char buf[size], const CodeLocation & location ) {
	Label ret_label( location, buf );
	ret_label.attributes.push_back( new Attribute( "unused" ) );
	return ret_label;
}

Label newLabel( const string & suffix, const Stmt * stmt ) {
	// Buffer for string manipulation.
	char buf[size];

	assertf( stmt, "CFA internal error: parameter statement cannot be null pointer" );
	int len = newLabelPre( buf, suffix );

	// What does this do?
	if ( ! stmt->labels.empty() ) {
		len = snprintf( buf + len, size - len, "_%s__", stmt->labels.front().name.c_str() );
		assertf( len < size - len, "CFA Internal error: buffer overflow creating label" );
	} // if

	return newLabelPost( buf, stmt->location );
}

Label newLabel( const string & suffix, const CodeLocation & location ) {
	// Buffer for string manipulation.
	char buf[size];

	newLabelPre( buf, suffix );
	return newLabelPost( buf, location );
}

} // namespace ControlStruct

// Local Variables: //
// mode: c++ //
// End: //
