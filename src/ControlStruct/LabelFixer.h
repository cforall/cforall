//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LabelFixer.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Jan 31 22:28:04 2022
// Update Count     : 35
//

#pragma once

#include <list>                    // for list
#include <map>                     // for map

#include "Common/PassVisitor.h"
#include "Common/SemanticError.h"  // for SemanticError
#include "SynTree/Label.h"         // for Label
#include "SynTree/Visitor.h"       // for Visitor
#include "SynTree/SynTree.h"       // for Visitor Nodes

namespace ControlStruct {
// normalizes label definitions and generates multi-level exit labels
class LabelGenerator;

class LabelFixer final : public WithGuards {
  public:
	LabelFixer( LabelGenerator *gen = 0 );

	std::map < Label, Statement * > *resolveJumps() throw ( SemanticErrorException );

	// Declarations
	void previsit( FunctionDecl *functionDecl );
	void postvisit( FunctionDecl *functionDecl );

	// Statements
	void previsit( Statement *stmt );
	void previsit( BranchStmt *branchStmt );

	// Expressions
	void previsit( LabelAddressExpr *addrExpr );

	Label setLabelsDef( std::list< Label > &, Statement *definition );
	template< typename UsageNode >
	void setLabelsUsg( Label, UsageNode *usage = 0 );

  private:
	class Entry {
		public:
		Entry( Statement *to ) : definition( to ) {}
		bool defined() { return ( definition != 0 ); }
		bool insideLoop();

		Label get_label() const { return label; }
		void set_label( Label lab ) { label = lab; }

		Statement *get_definition() const { return definition; }
		void set_definition( Statement *def ) { definition = def; }

	  private:
		Label label;
		Statement *definition;
	};

	std::map < Label, Entry *> labelTable;
	LabelGenerator *generator;
};
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
