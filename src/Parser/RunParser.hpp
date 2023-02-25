//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// RunParser.hpp -- External interface to the parser.
//
// Author           : Andrew Beach
// Created On       : Mon Dec 19 10:42:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Thr Feb 16 10:08:00 2023
// Update Count     : 2
//

#pragma once

#include <iosfwd>                           // for ostream

#include "AST/LinkageSpec.hpp"              // for Spec
namespace ast {
	class TranslationUnit;
}

// The Parser does not have an enclosing namespace.

/// Parse the contents of the input file, setting the initial linkage to the
/// value provided. Results are saved to the internal accumulator.
/// The input file is closed when complete. Exits instead of returning on
/// error or if alwaysExit is true.
void parse( FILE * input, ast::Linkage::Spec linkage, bool alwaysExit = false );

/// Drain the internal accumulator of parsed code and build a translation
/// unit from it.
ast::TranslationUnit buildUnit(void);

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
