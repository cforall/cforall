//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Generate.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 16 03:01:51 2020
// Update Count     : 9
//
#include "Generate.h"

#include <iostream>                  // for ostream, endl, operator<<
#include <list>                      // for list
#include <string>                    // for operator<<

#include "CodeGenerator.h"           // for CodeGenerator, doSemicolon, oper...
#include "GenType.h"                 // for genPrettyType
#include "Common/PassVisitor.h"      // for PassVisitor
#include "SynTree/LinkageSpec.h"     // for isBuiltin, isGeneratable
#include "SynTree/BaseSyntaxNode.h"  // for BaseSyntaxNode
#include "SynTree/Declaration.h"     // for Declaration
#include "SynTree/Type.h"            // for Type

using namespace std;

namespace CodeGen {
	namespace {
		/// Removes misc. nodes that should not exist in CodeGen
		struct TreeCleaner {
			void premutate( CompoundStmt * stmt );
			Statement * postmutate( ImplicitCtorDtorStmt * stmt );

			static bool shouldClean( Declaration * );
		};

		void cleanTree( std::list< Declaration * > & translationUnit ) {
			PassVisitor<TreeCleaner> cleaner;
			filter( translationUnit, [](Declaration * decl) { return TreeCleaner::shouldClean(decl); }, false );
			mutateAll( translationUnit, cleaner );
		} // cleanTree
	} // namespace

	void generate( std::list< Declaration* > translationUnit, std::ostream &os, bool doIntrinsics, bool pretty, bool generateC, bool lineMarks, bool printExprTypes ) {
		cleanTree( translationUnit );

		PassVisitor<CodeGenerator> cgv( os, pretty, generateC, lineMarks, printExprTypes );
		for ( auto & dcl : translationUnit ) {
			if ( LinkageSpec::isGeneratable( dcl->get_linkage() ) && (doIntrinsics || ! LinkageSpec::isBuiltin( dcl->get_linkage() ) ) ) {
				cgv.pass.updateLocation( dcl );
				dcl->accept(cgv);
				if ( doSemicolon( dcl ) ) {
					os << ";";
				} // if
				os << cgv.pass.endl;
			} // if
		} // for
	}

	void generate( BaseSyntaxNode * node, std::ostream & os ) {
		if ( Type * type = dynamic_cast< Type * >( node ) ) {
			os << genPrettyType( type, "" );
		} else {
			PassVisitor<CodeGenerator> cgv( os, true, false, false, false );
			node->accept( cgv );
		}
		os << std::endl;
	}

	namespace {
		void TreeCleaner::premutate( CompoundStmt * cstmt ) {
			filter( cstmt->kids, [](Statement * stmt) {
				if ( DeclStmt * declStmt = dynamic_cast< DeclStmt * >( stmt ) ) {
					return shouldClean( declStmt->decl );
				}
				return false;
			}, false );
		}

		Statement * TreeCleaner::postmutate( ImplicitCtorDtorStmt * stmt ) {
			Statement * callStmt = nullptr;
			std::swap( stmt->callStmt, callStmt );
			delete stmt;
			return callStmt;
		}

		bool TreeCleaner::shouldClean( Declaration * decl ) {
			return dynamic_cast< TraitDecl * >( decl );
		}
	} // namespace
} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
