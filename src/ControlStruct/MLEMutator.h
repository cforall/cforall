//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// MLEMutator.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb  1 09:27:24 2022
// Update Count     : 50
//

#pragma once

#include <list>                    // for list
#include <map>                     // for map
#include <string>                  // for string
#include <set>                     // for unordered_set

#include "Common/PassVisitor.h"
#include "Common/SemanticError.h"  // for SemanticError
#include "SynTree/Label.h"         // for Label
#include "SynTree/Mutator.h"       // for Mutator
#include "SynTree/SynTree.h"       // for Visitor Nodes

namespace ControlStruct {
	class LabelGenerator;

	class MultiLevelExitMutator : public WithVisitorRef<MultiLevelExitMutator>,
			public WithShortCircuiting, public WithGuards {
	  public:
		class Entry;
		MultiLevelExitMutator( std::map<Label, Statement *> *t, LabelGenerator *gen = 0 ) :
			targetTable( t ), breakLabel(std::string("")), generator( gen ) {}
		~MultiLevelExitMutator();

		void premutate( FunctionDecl * );

		void premutate( CompoundStmt *cmpndStmt );
		Statement * postmutate( BranchStmt *branchStmt );
		void premutate( WhileDoStmt *whileDoStmt );
		Statement * postmutate( WhileDoStmt *whileDoStmt );
		void premutate( ForStmt *forStmt );
		Statement * postmutate( ForStmt *forStmt );
		void premutate( CaseStmt *caseStmt );
		void premutate( IfStmt *ifStmt );
		Statement * postmutate( IfStmt *ifStmt );
		void premutate( SwitchStmt *switchStmt );
		Statement * postmutate( SwitchStmt *switchStmt );
		void premutate( ReturnStmt *returnStmt );
		void premutate( TryStmt *tryStmt );
		Statement * postmutate( TryStmt *tryStmt );
		void premutate( FinallyStmt *finallyStmt );

		Statement *mutateLoop( Statement *bodyLoop, Entry &e );

		Label &get_breakLabel() { return breakLabel; }
		void set_breakLabel( Label newValue ) { breakLabel = newValue; }

		class Entry {
		  public:
			// specialized constructors for each combination of statement with labelled break/continue/fallthrough that is valid to cleanup the use cases
			explicit Entry( ForStmt *stmt, Label breakExit, Label contExit ) :
				stmt( stmt ), breakExit( breakExit ), contExit( contExit ) {}

			explicit Entry( WhileDoStmt *stmt, Label breakExit, Label contExit ) :
				stmt( stmt ), breakExit( breakExit ), contExit( contExit ) {}

			explicit Entry( CompoundStmt *stmt, Label breakExit ) :
				stmt( stmt ), breakExit( breakExit ) {}

			explicit Entry( IfStmt *stmt, Label breakExit ) :
				stmt( stmt ), breakExit( breakExit ) {}

			explicit Entry( CaseStmt *stmt, Label fallExit ) :
				stmt( stmt ), fallExit( fallExit ) {}

			explicit Entry( SwitchStmt *stmt, Label breakExit, Label fallDefaultExit ) :
				stmt( stmt ), breakExit( breakExit ), fallDefaultExit( fallDefaultExit ) {}

			explicit Entry( TryStmt *stmt, Label breakExit ) :
				stmt( stmt ), breakExit( breakExit ) {}

			bool operator==( const Statement *other ) { return stmt == other; }
			bool operator!=( const Statement *other ) { return stmt != other; }

			bool operator==( const Entry &other ) { return stmt == other.get_controlStructure(); }

			Statement *get_controlStructure() const { return stmt; }

			Label useContExit() { contUsed = true; return contExit; }
			Label useBreakExit() { breakUsed = true; return breakExit; }
			Label useFallExit() { fallUsed = true; return fallExit; }
			Label useFallDefaultExit() { fallDefaultUsed = true; return fallDefaultExit; }

			bool isContUsed() const { return contUsed; }
			bool isBreakUsed() const { return breakUsed; }
			bool isFallUsed() const { return fallUsed; }
			bool isFallDefaultUsed() const { return fallDefaultUsed; }
			void seenDefault() { fallDefaultValid = false; }
			bool isFallDefaultValid() const { return fallDefaultValid; }
		  private:
			Statement *stmt;
			Label breakExit, contExit, fallExit, fallDefaultExit;
			bool breakUsed = false, contUsed = false, fallUsed = false, fallDefaultUsed = false;
			bool fallDefaultValid = true;
		};

	  private:
		std::map< Label, Statement * > *targetTable;
		std::set< Label > fallthroughLabels;
		std::list< Entry > enclosingControlStructures;
		Label breakLabel;
		LabelGenerator *generator;
		bool inFinally = false;

		template< typename LoopClass >
		void prehandleLoopStmt( LoopClass * loopStmt );

		template< typename LoopClass >
		Statement * posthandleLoopStmt( LoopClass * loopStmt );

		void fixBlock( std::list< Statement * > &kids, bool caseClause = false );
	};
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
