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
// Last Modified By : Andrew Beach
// Last Modified On : Mon Nov  8 10:53:00 2021
// Update Count     : 3
//

#include "FixLabels.hpp"

#include "AST/Label.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "ControlStruct/MultiLevelExit.hpp"

namespace ControlStruct {

namespace {

class FixLabelsCore final : public ast::WithGuards {
	LabelToStmt labelTable;
public:
	FixLabelsCore() : labelTable() {}

	void previsit( const ast::FunctionDecl * );
	const ast::FunctionDecl * postvisit( const ast::FunctionDecl * );
	void previsit( const ast::Stmt * );
	void previsit( const ast::BranchStmt * );
	void previsit( const ast::LabelAddressExpr * );

	void setLabelsDef( const std::vector<ast::Label> &, const ast::Stmt * );
	void setLabelsUsage( const ast::Label & );
};

void FixLabelsCore::previsit( const ast::FunctionDecl * ) {
	GuardValue( labelTable ).clear();
}

const ast::FunctionDecl * FixLabelsCore::postvisit(
		const ast::FunctionDecl * decl ) {
	if ( nullptr == decl->stmts ) return decl;
	for ( auto kvp : labelTable ) {
		if ( nullptr == kvp.second ) {
			SemanticError( kvp.first.location,
				"Use of undefined label: " + kvp.first.name );
		}
	}
	return ast::mutate_field( decl, &ast::FunctionDecl::stmts,
		multiLevelExitUpdate( decl->stmts.get(), labelTable ) );
}

void FixLabelsCore::previsit( const ast::Stmt * stmt ) {
	if ( !stmt->labels.empty() ) {
		setLabelsDef( stmt->labels, stmt );
	}
}

void FixLabelsCore::previsit( const ast::BranchStmt * stmt ) {
	if ( !stmt->labels.empty() ) {
		setLabelsDef( stmt->labels, stmt );
	}
	if ( !stmt->target.empty() ) {
		setLabelsUsage( stmt->target );
	}
}

void FixLabelsCore::previsit( const ast::LabelAddressExpr * expr ) {
	assert( !expr->arg.empty() );
	setLabelsUsage( expr->arg );
}

void FixLabelsCore::setLabelsDef(
		const std::vector<ast::Label> & labels, const ast::Stmt * stmt ) {
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
void FixLabelsCore::setLabelsUsage( const ast::Label & label ) {
	if ( labelTable.find( label ) == labelTable.end() ) {
		labelTable[ label ] = nullptr;
	}
}

} // namespace

void fixLabels( ast::TranslationUnit & translationUnit ) {
	ast::Pass<FixLabelsCore>::run( translationUnit );
}

} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
