//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExceptTranslate.h -- Conversion of exception control flow structures.
//
// Author           : Andrew Beach
// Created On       : Tus Jun 06 10:13:00 2017
// Last Modified By : Andrew Beach
// Last Modified On : Mon Nov  8 11:43:00 2020
// Update Count     : 6
//

#pragma once

#include <list>  // for list

class Declaration;
namespace ast {
	class TranslationUnit;
}

namespace ControlStruct {
	void translateThrows( std::list< Declaration *> & translationUnit );
	void translateThrows( ast::TranslationUnit & transUnit );
	/* Replaces all throw & throwResume statements with function calls.
	 * These still need to be resolved, so call this before the reslover.
	 */

	void translateTries( std::list< Declaration *> & translationUnit );
	/* Replaces all try blocks (and their many clauses) with function definitions and calls.
	 * This uses the exception built-ins to produce typed output and should take place after
	 * the resolver. It also produces virtual casts and should happen before they are expanded.
	 */
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
