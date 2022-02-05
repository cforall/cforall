//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LabelFixer.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb  1 09:12:09 2022
// Update Count     : 162
//

#include <cassert>                         // for assert
#include <list>                            // for list, _List_iterator, list...
#include <string>                          // for operator+, string, operator==
#include <utility>                         // for pair

#include "ControlStruct/LabelGenerator.h"  // for LabelGenerator
#include "LabelFixer.h"
#include "MLEMutator.h"                    // for MultiLevelExitMutator
#include "SynTree/Declaration.h"           // for FunctionDecl
#include "SynTree/Expression.h"            // for NameExpr, Expression, Unty...
#include "SynTree/Statement.h"             // for Statement, BranchStmt, Com...

namespace ControlStruct {
bool LabelFixer::Entry::insideLoop() {
	return ( dynamic_cast< ForStmt * > ( definition ) ||
		dynamic_cast< WhileDoStmt * > ( definition )  );
}

LabelFixer::LabelFixer( LabelGenerator * gen ) : generator ( gen ) {
	if ( generator == 0 )
		generator = LabelGenerator::getGenerator();
}

void LabelFixer::previsit( FunctionDecl * ) {
	// need to go into a nested function in a fresh state
	GuardValue( labelTable );
	labelTable.clear();
}

void LabelFixer::postvisit( FunctionDecl * functionDecl ) {
	PassVisitor<MultiLevelExitMutator> mlem( resolveJumps(), generator );
	// We start in the body so we can stop when we hit another FunctionDecl.
	maybeMutate( functionDecl->statements, mlem );
}

// prune to at most one label definition for each statement
void LabelFixer::previsit( Statement * stmt ) {
	std::list< Label > &labels = stmt->get_labels();

	if ( ! labels.empty() ) {
		// only remember one label for each statement
		Label current = setLabelsDef( labels, stmt );
	} // if
}

void LabelFixer::previsit( BranchStmt * branchStmt ) {
	previsit( ( Statement *)branchStmt );

	// for labeled branches, add an entry to the label table
	Label target = branchStmt->get_target();
	if ( target != "" ) {
		setLabelsUsg( target, branchStmt );
	}
}

void LabelFixer::previsit( LabelAddressExpr * addrExpr ) {
	Label & target = addrExpr->arg;
	assert( target != "" );
	setLabelsUsg( target, addrExpr );
}


// Sets the definition of the labelTable entry to be the provided statement for every label in
// the list parameter. Happens for every kind of statement.
Label LabelFixer::setLabelsDef( std::list< Label > & llabel, Statement * definition ) {
	assert( definition != 0 );
	assert( llabel.size() > 0 );

	for ( std::list< Label >::iterator i = llabel.begin(); i != llabel.end(); i++ ) {
		Label & l = *i;
		l.set_statement( definition ); // attach statement to the label to be used later
		if ( labelTable.find( l ) == labelTable.end() ) {
			// All labels on this statement need to use the same entry,
			// so this should only be created once.
			// undefined and unused until now, add an entry
			labelTable[ l ] = new Entry( definition );
		} else if ( labelTable[ l ]->defined() ) {
			// defined twice, error
			SemanticError( l.get_statement()->location,
				"Duplicate definition of label: " + l.get_name() );
		} else {
			// used previously, but undefined until now -> link with this entry
			// Question: Is changing objects important?
			delete labelTable[ l ];
			labelTable[ l ] = new Entry( definition );
		} // if
	} // for

	// Produce one of the labels attached to this statement to be temporarily used as the
	// canonical label.
	return labelTable[ llabel.front() ]->get_label();
}

// A label was used, add it to the table if it isn't already there
template< typename UsageNode >
void LabelFixer::setLabelsUsg( Label orgValue, UsageNode *use ) {
	assert( use != 0 );

	// add label with an unknown origin
	if ( labelTable.find( orgValue ) == labelTable.end() ) {
		labelTable[ orgValue ] = new Entry( 0 );
	}
}

// Builds a table that maps a label to its defining statement.
std::map<Label, Statement * > * LabelFixer::resolveJumps() throw ( SemanticErrorException ) {
	std::map< Label, Statement * > *ret = new std::map< Label, Statement * >();
	for ( std::map< Label, Entry * >::iterator i = labelTable.begin(); i != labelTable.end(); ++i ) {
		if ( ! i->second->defined() ) {
			SemanticError( i->first.get_statement()->location, "Use of undefined label: " + i->first.get_name() );
		}
		(*ret)[ i->first ] = i->second->get_definition();
	}

	return ret;
}
}  // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
