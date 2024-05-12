//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixMain.hpp -- Tools to change a Cforall main into a C main.
//
// Author           : Thierry Delisle
// Created On       : Thr Jan 12 14:11:09 2017
// Last Modified By : Andrew Beach
// Last Modified On : Fri Oct 29 16:20:00 2021
// Update Count     : 8
//

#pragma once

#include <iosfwd>

namespace ast {
	class FunctionDecl;
	class TranslationUnit;
}

namespace CodeGen {

/// Is this function a program main function?
bool isMain( const ast::FunctionDecl * decl );

/// Adjust the linkage of main functions.
void fixMainLinkage( ast::TranslationUnit & transUnit, bool replaceMain );

/// Add a wrapper around to run the Cforall main.
void fixMainInvoke( ast::TranslationUnit & transUnit,
		std::ostream & os, const char * bootloaderFilename );

} // namespace CodeGen
