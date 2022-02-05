//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ForExprMutator.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb  1 09:26:12 2022
// Update Count     : 16
//

#include <list>                 // for list, _List_iterator, list<>::iterator

#include "ForExprMutator.h"
#include "SynTree/Label.h"      // for Label
#include "SynTree/Statement.h"  // for Statement (ptr only), ForStmt, Compou...

namespace ControlStruct {
	Statement * hoist( Statement * originalStmt, std::list<Statement *> & init ) {
		// If no hoisting is needed, skip:
		if ( 0 == init.size() ) {
			return originalStmt;
		}

		// Create compound statement, move initializers outside,
		// the resut of the original stays as is.
		CompoundStmt * block = new CompoundStmt();
		std::list<Statement *> & stmts = block->get_kids();
		stmts.splice( stmts.end(), init );

		// Add for to the new block.
		stmts.push_back( originalStmt );
		return block;
	}

	Statement * ForExprMutator::postmutate( IfStmt * ifStmt ) {
		return hoist( ifStmt, ifStmt->initialization );
	}
	Statement * ForExprMutator::postmutate( ForStmt * forStmt ) {
		// hoist any initializer declarations to make them C89 (rather than C99)
		return hoist( forStmt, forStmt->initialization );
	}
	Statement * ForExprMutator::postmutate( WhileDoStmt * whileDoStmt ) {
		return hoist( whileDoStmt, whileDoStmt->initialization );
	}
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
