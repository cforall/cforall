//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Mutate.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 16 03:22:07 2020
// Update Count     : 10
//

#include <iterator>                // for back_inserter, inserter
#include <list>                    // for list

#include "Common/PassVisitor.h"    // for mutateAll
#include "ForExprMutator.h"        // for ForExprMutator
#include "LabelFixer.h"            // for LabelFixer
#include "Mutate.h"
#include "SynTree/Declaration.h"   // for Declaration
#include "SynTree/Mutator.h"       // for mutateAll

#include "Common/PassVisitor.h"    // for PassVisitor
#include "SynTree/Visitor.h"       // for acceptAll

namespace ControlStruct {
	void fixLabels( std::list< Declaration * > & translationUnit ) {
		PassVisitor<LabelFixer> lfix;
		acceptAll( translationUnit, lfix );
	}

	void hoistControlDecls( std::list< Declaration * > & translationUnit ) {
		PassVisitor<ForExprMutator> formut;
		mutateAll( translationUnit, formut );
	}
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
