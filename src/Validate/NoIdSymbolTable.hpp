//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// NoIdSymbolTable.hpp -- Special WithSymbolTable variant ast::Pass helper.
//
// Author           : Andrew Beach
// Created On       : Thr Apr 21 11:57:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Thr Apr 21 11:57:00 2022
// Update Count     : 0
//

#pragma once

#include "AST/SymbolTable.hpp"

namespace Validate {

// A SymbolTable that only tracks names relevant to Validate passes.
// It tracks type names but not identifier names.
// Some of the canonicalization that occurs before the resolver
// affects how identifier name errors get reported to the user.
// The Validate phase needs to chase type names,
// but it is too early to try tracking identifier names.
// Identifier skipping is acheived by omitting methods that should not be
// called by the Pass template (lookupId and addId).
class NoIdSymbolTable {
	ast::SymbolTable base;
public:
	// All names that are tracked (now) are eligible for collision validation (now).
	// (Names that are only tracked later get their collision validation then.)
	NoIdSymbolTable() : base(ast::SymbolTable::ValidateOnAdd) {}

#	define FORWARD_X( func, types_and_names, just_names ) \
	inline auto func types_and_names -> decltype( base.func just_names ) { \
		return base.func just_names ; \
	}
#	define FORWARD_0( func )         FORWARD_X( func, (),             () )
#	define FORWARD_1( func, type )   FORWARD_X( func, (type arg),     (arg) )
#	define FORWARD_2( func, t0, t1 ) FORWARD_X( func, (t0 a0, t1 a1), (a0, a1) )

	FORWARD_0( enterScope )
	FORWARD_0( leaveScope )
	FORWARD_1( lookupType  , const std::string & )
	FORWARD_1( lookupStruct, const std::string & )
	FORWARD_1( lookupEnum  , const std::string & )
	FORWARD_1( lookupUnion , const std::string & )
	FORWARD_1( lookupTrait , const std::string & )
	FORWARD_1( addType  , const ast::NamedTypeDecl * )
	FORWARD_1( addStruct, const ast::StructDecl *    )
	FORWARD_1( addEnum  , const ast::EnumDecl *      )
	FORWARD_1( addUnion , const ast::UnionDecl *     )
	FORWARD_1( addTrait , const ast::TraitDecl *     )
	FORWARD_2( addWith  , const std::vector< ast::ptr<ast::Expr> > &, const ast::Decl * )
	FORWARD_1( addStructId, const std::string & )
	FORWARD_1( addUnionId , const std::string & )

	FORWARD_1( globalLookupType, const std::string & )

#	undef FORWARD_2
#	undef FORWARD_1
#	undef FORWARD_0
#	undef FORWARD_X
};

struct WithNoIdSymbolTable {
	NoIdSymbolTable symtab;
};

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
