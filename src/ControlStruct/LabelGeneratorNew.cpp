//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LabelGenerator.cc --
//
// Author           : Peter A. Buhr
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  2 09:11:17 2022
// Update Count     : 72
//

using namespace std;

#include "LabelGeneratorNew.hpp"

#include "AST/Attribute.hpp"
#include "AST/Label.hpp"
#include "AST/Stmt.hpp"
using namespace ast;

namespace ControlStruct {

Label newLabel( const string & suffix, const Stmt * stmt ) {
	static int current = 0;

	assertf( stmt, "CFA internal error: parameter statement cannot be null pointer" );

	enum { size = 128 };
	char buf[size];										// space to build label
	int len = snprintf( buf, size, "__L%d__%s", current++, suffix.c_str() );
	assertf( len < size, "CFA Internal error: buffer overflow creating label" );

	// What does this do?
	if ( ! stmt->labels.empty() ) {
		len = snprintf( buf + len, size - len, "_%s__", stmt->labels.front().name.c_str() );
		assertf( len < size - len, "CFA Internal error: buffer overflow creating label" );
	} // if

	Label ret_label( stmt->location, buf );
	ret_label.attributes.push_back( new Attribute( "unused" ) );
	return ret_label;
}

} // namespace ControlStruct

// Local Variables: //
// mode: c++ //
// End: //
