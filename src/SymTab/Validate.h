//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Validate.h -- This class is intended to perform pre-processing of declarations, validating their correctness and
//               computing some auxilliary data that is necessary for the indexer.
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 21:53:34 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:46:07 2017
// Update Count     : 4
//

#pragma once

#include <list>  // for list

struct CodeLocation;
class  Declaration;
class  Type;

namespace ast {
	class Type;
	class SymbolTable;
}

namespace SymTab {
	class Indexer;

	/// Normalizes struct and function declarations
	void validate( std::list< Declaration * > &translationUnit, bool doDebug = false );
	void validateType( Type *type, const Indexer *indexer );

	// Sub-passes of validate.
	void validate_A( std::list< Declaration * > &translationUnit );
	void validate_B( std::list< Declaration * > &translationUnit );
	void validate_C( std::list< Declaration * > &translationUnit );
	void validate_D( std::list< Declaration * > &translationUnit );
	void validate_E( std::list< Declaration * > &translationUnit );
	void validate_F( std::list< Declaration * > &translationUnit );

	const ast::Type * validateType(
		const CodeLocation & loc, const ast::Type * type, const ast::SymbolTable & symtab );
} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
