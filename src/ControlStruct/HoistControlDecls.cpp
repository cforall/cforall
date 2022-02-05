//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// HoistControlDecls.cpp -- Desugar Cforall control structures.
//
// Author           : Andrew Beach
// Created On       : Fri Dec  3 15:34:00 2021
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb  1 18:59:47 2022
// Update Count     : 25
//

#include "HoistControlDecls.hpp"

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
using namespace ast;

namespace ControlStruct {

template<typename StmtT>
const Stmt * hoist( const StmtT * stmt ) {
	// If no hoisting is needed, then make no changes.

	if ( stmt->inits.size() == 0 ) {					// no declarations ?
		// if ( /* no conditional declarations */ ...  ) ...
		return stmt;
	} // if

	// Put hoist declarations and modified statement in a compound statement.

	CompoundStmt * block = new CompoundStmt( stmt->location ); // create empty compound statement
	//    {}

	for ( const Stmt * next : stmt->inits ) {			// link conditional declarations into compound
		block->kids.push_back( next );
	}
	//    if ( int x = f(), y = g(); ... ) ...
	// link declarations into compound statement
	//    {
	//         int x = f();
	//         int y = g();
	//    }

	StmtT * mutStmt = mutate( stmt );					// create mutate handle to change statement
	mutStmt->inits.clear();								// remove declarations
	//    if ( ... ) ...

	block->kids.push_back( mutStmt );					// link modified statement into compound
	//    {
	//        int x = f();
	//        int y = g();
	//        if ( ... ) ...
	//    }
	return block;
}

struct hoistControlDeclsCore {
	// Statements with declarations in conditional.
	const Stmt * postvisit( const IfStmt * stmt ) {
		return hoist<IfStmt>( stmt );
	}
	const Stmt * postvisit( const ForStmt * stmt ) {
		return hoist<ForStmt>( stmt );
	}
	const Stmt * postvisit( const WhileDoStmt * stmt ) {
		return hoist<WhileDoStmt>( stmt );
	}
};

// Hoist initialization out of for statements.
void hoistControlDecls( TranslationUnit & translationUnit ) {
	Pass<hoistControlDeclsCore>::run( translationUnit );
}

} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
