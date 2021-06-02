//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindSpecialDeclarations.h --
//
// Author           : Rob Schluntz
// Created On       : Thu Aug 30 09:49:02 2018
// Last Modified By : Rob Schluntz
// Last Modified On : Thu Aug 30 09:51:12 2018
// Update Count     : 2
//

#pragma once

#include <list>  // for list

class Declaration;
class FunctionDecl;
class StructDecl;
class Type;

namespace Validate {
	/// size_t type - set when size_t typedef is seen. Useful in a few places,
	/// such as in determining array dimension type
	extern Type * SizeType;

	/// intrinsic dereference operator for unqualified types - set when *? function is seen in FindSpecialDeclarations.
	/// Useful for creating dereference ApplicationExprs without a full resolver pass.
	extern FunctionDecl * dereferenceOperator;

	/// special built-in functions and data structures necessary for destructor generation
	extern StructDecl * dtorStruct;
	extern FunctionDecl * dtorStructDestroy;

	/// find and remember some of the special declarations that are useful for generating code, so that they do not have to be discovered multiple times.
	void findSpecialDecls( std::list< Declaration * > & translationUnit );
} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
