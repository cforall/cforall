//
// Cforall Version 1.0.0 Copyright (C) 2017 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Debug.h --
//
// Author           : Rob Schluntz
// Created On       : Fri Sep 1 11:09:14 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:39:42 2019
// Update Count     : 3
//

#pragma once

#include <string>
#include <list>
#include <iostream>

#include "CodeGen/Generate.h"
#include "SynTree/LinkageSpec.h"
#include "SynTree/Declaration.h"

#define DEBUG

namespace Debug {
	/// debug codegen a translation unit
	static inline void codeGen( __attribute__((unused)) const std::list< Declaration * > & translationUnit, __attribute__((unused)) const std::string & label, __attribute__((unused)) LinkageSpec::Spec linkageFilter = LinkageSpec::Builtin ) {
	#ifdef DEBUG
		std::list< Declaration * > decls;

		filter( translationUnit.begin(), translationUnit.end(), back_inserter( decls ), [linkageFilter]( Declaration * decl ) {
			return ! (decl->linkage & linkageFilter);
		});

		std::cerr << "======" << label << "======" << std::endl;
		CodeGen::generate(
			decls,
			std::cerr,
			true /* doIntrinsics */,
			true /* pretty */,
			false /* generateC */,
			false /* lineMarks */,
			true /* printTypeExpr */
		);
	#endif
	} // dump

	static inline void treeDump( __attribute__((unused)) const std::list< Declaration * > & translationUnit, __attribute__((unused)) const std::string & label, __attribute__((unused)) LinkageSpec::Spec linkageFilter = LinkageSpec::Compiler ) {
	#ifdef DEBUG
		std::list< Declaration * > decls;

		filter( translationUnit.begin(), translationUnit.end(), back_inserter( decls ), [linkageFilter]( Declaration * decl ) {
			return ! (decl->linkage & linkageFilter);
		});

		std::cerr << "======" << label << "======" << std::endl;
		printAll( decls, std::cerr );
	#endif
	} // dump
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
