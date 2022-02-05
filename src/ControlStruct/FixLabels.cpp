//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixLabels.cpp -- Normalizes labels and handles multi-level exit labels.
//
// Author           : Andrew Beach
// Created On       : Mon Nov  1 09:39:00 2021
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Jan 31 22:19:17 2022
// Update Count     : 9
//

#include "FixLabels.hpp"

#include "AST/Label.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "ControlStruct/MultiLevelExit.hpp"
using namespace ast;

namespace ControlStruct {
class FixLabelsCore final : public WithGuards {
	LabelToStmt labelTable;
  public:
	FixLabelsCore() : labelTable() {}

	void previsit( const FunctionDecl * );
	const FunctionDecl * postvisit( const FunctionDecl * );
	void previsit( const Stmt * );
	void previsit( const BranchStmt * );
	void previsit( const LabelAddressExpr * );

	void setLabelsDef( const std::vector<Label> &, const Stmt * );
	void setLabelsUsage( const Label & );
};

void FixLabelsCore::previsit( const FunctionDecl * ) {
	GuardValue( labelTable ).clear();
}

const FunctionDecl * FixLabelsCore::postvisit(
	const FunctionDecl * decl ) {
	if ( nullptr == decl->stmts ) return decl;
	for ( auto kvp : labelTable ) {
		if ( nullptr == kvp.second ) {
			SemanticError( kvp.first.location,
						   "Use of undefined label: " + kvp.first.name );
		}
	}
	return mutate_field( decl, &FunctionDecl::stmts,
						 multiLevelExitUpdate( decl->stmts.get(), labelTable ) );
}

void FixLabelsCore::previsit( const Stmt * stmt ) {
	if ( !stmt->labels.empty() ) {
		setLabelsDef( stmt->labels, stmt );
	}
}

void FixLabelsCore::previsit( const BranchStmt * stmt ) {
	if ( !stmt->labels.empty() ) {
		setLabelsDef( stmt->labels, stmt );
	}
	if ( !stmt->target.empty() ) {
		setLabelsUsage( stmt->target );
	}
}

void FixLabelsCore::previsit( const LabelAddressExpr * expr ) {
	assert( !expr->arg.empty() );
	setLabelsUsage( expr->arg );
}

void FixLabelsCore::setLabelsDef(
	const std::vector<Label> & labels, const Stmt * stmt ) {
	assert( !labels.empty() );
	assert( stmt );

	for ( auto label : labels ) {
		if ( labelTable.find( label ) == labelTable.end() ) {
			// Make sure to only create the entry once.
			labelTable[ label ] = stmt;
		} else if ( nullptr != labelTable[ label ] ) {
			// Duplicate definition, this is an error.
			SemanticError( label.location,
						   "Duplicate definition of label: " + label.name );
		} else {
			// Perviously used, but not defined until now.
			labelTable[ label ] = stmt;
		}
	}
}

// Label was used, if it is new add it to the table.
void FixLabelsCore::setLabelsUsage( const Label & label ) {
	if ( labelTable.find( label ) == labelTable.end() ) {
		labelTable[ label ] = nullptr;
	}
}

void fixLabels( TranslationUnit & translationUnit ) {
	Pass<FixLabelsCore>::run( translationUnit );
}
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
