//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Generate.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul 21 22:16:35 2017
// Update Count     : 2
//

#pragma once

#include <iostream>  // for ostream
#include <list>      // for list

class BaseSyntaxNode;
class Declaration;

namespace ast {
	class TranslationUnit;
}

namespace CodeGen {
	/// Generates code. doIntrinsics determines if intrinsic functions are printed, pretty formats output nicely (e.g., uses unmangled names, etc.), generateC is true when the output must consist only of C code (allows some assertions, etc.)
	void generate( std::list< Declaration* > translationUnit, std::ostream &os, bool doIntrinsics, bool pretty, bool generateC = false , bool lineMarks = false, bool printTypeExpr = false );

	/// Generate code for a single node -- helpful for debugging in gdb
	void generate( BaseSyntaxNode * node, std::ostream & os );

/// Generates all code in transUnit and writing it to the os.
/// doIntrinsics: Should intrinsic functions be printed?
/// pretty: Format output nicely (e.g., uses unmangled names, etc.).
/// generateC: Make sure the output only consists of C code (allows some assertions, etc.)
/// lineMarks: Output line marks (processed line directives) in the output.
/// printExprTypes: Print the types of expressions in comments.
void generate( ast::TranslationUnit & transUnit, std::ostream &os, bool doIntrinsics,
		bool pretty, bool generateC, bool lineMarks, bool printExprTypes );

} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
