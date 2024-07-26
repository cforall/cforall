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
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jul 24 12:00:00 2024
// Update Count     : 3
//

#include "HoistControlDecls.hpp"

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
using namespace ast;

namespace ControlStruct {

namespace {

template<typename StmtT>
const Stmt * hoist( const StmtT * stmt ) {
	// If no hoisting is needed (no declaration), then make no changes.
	if ( stmt->inits.size() == 0 ) {
		// if ( /* no conditional declarations */ ...  ) ...
		return stmt;
	}

	StmtT * mutStmt = mutate( stmt );
	CompoundStmt * block = new CompoundStmt( stmt->location );
	//    {}

	//    Label: if ( int x = f(), y = g(); ... ) ...
	// link declarations into compound statement
	for ( const Stmt * next : mutStmt->inits ) {
		block->kids.push_back( next );
	}
	//    {
	//         int x = f();
	//         int y = g();
	//    }
	mutStmt->inits.clear();
	//    Label: if ( ... ) ...

	block->labels.swap( mutStmt->labels );
	//    Label: {
	//        int x = f();
	//        int y = g();
	//    }
	//    if ( ... ) ...

	block->kids.push_back( mutStmt );
	//    Label: {
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

} // namespace

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
