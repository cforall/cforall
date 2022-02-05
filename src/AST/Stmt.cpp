//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Stmt.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May  8 13:00:00 2019
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Feb  2 19:01:20 2022
// Update Count     : 3
//

#include "Stmt.hpp"


#include "DeclReplacer.hpp"
#include "Type.hpp"

namespace ast {

// --- CompoundStmt
CompoundStmt::CompoundStmt( const CompoundStmt& other ) : Stmt(other), kids(other.kids) {
	// when cloning a compound statement, we may end up cloning declarations which
	// are referred to by VariableExprs throughout the block. Cloning a VariableExpr
	// does a shallow copy, so the VariableExpr will end up pointing to the original
	// declaration. If the original declaration is deleted, e.g. because the original
	// CompoundStmt is deleted, then we have a dangling pointer. To avoid this case,
	// find all DeclarationWithType nodes (since a VariableExpr must point to a
	// DeclarationWithType) in the original CompoundStmt and map them to the cloned
	// node in the new CompoundStmt ('this'), then replace the Declarations referred to
	// by each VariableExpr according to the constructed map. Note that only the declarations
	// in the current level are collected into the map, because child CompoundStmts will
	// recursively execute this routine. There may be more efficient ways of doing
	// this.
	DeclReplacer::DeclMap declMap;
	auto origit = other.kids.begin();
	for ( const Stmt * s : kids ) {
		assert( origit != other.kids.end() );
		const Stmt * origStmt = *origit++;
		if ( const DeclStmt * declStmt = dynamic_cast< const DeclStmt * >( s ) ) {
			const DeclStmt * origDeclStmt = strict_dynamic_cast< const DeclStmt * >( origStmt );
			if ( const DeclWithType * dwt = dynamic_cast< const DeclWithType * > ( declStmt->decl.get() ) ) {
				const DeclWithType * origdwt = strict_dynamic_cast< const DeclWithType * > ( origDeclStmt->decl.get() );
				assert( dwt->name == origdwt->name );
				declMap[ origdwt ] = dwt;
			} else assert( ! dynamic_cast< const DeclWithType * > ( origDeclStmt->decl.get() ) );
		} else assert( ! dynamic_cast< const DeclStmt * > ( s ) );
	}
	if ( ! declMap.empty() ) {
		DeclReplacer::replace( this, declMap );
	}
}

// --- BranchStmt
BranchStmt::BranchStmt( const CodeLocation& loc, Kind kind, Label target, const std::vector<Label>&& labels )
		: Stmt(loc, std::move(labels)), originalTarget(target), target(target), kind(kind) {
	// Make sure a syntax error hasn't slipped through.
	assert( Goto != kind || !target.empty() );
}

const char * BranchStmt::kindNames[] = {
    "Goto", "Break", "Continue", "FallThrough", "FallThroughDefault"
};

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
