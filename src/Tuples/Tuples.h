//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Tuples.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jun 18 09:36:00 2019
// Update Count     : 18
//

#pragma once

#include <string>
#include <vector>

#include "AST/Fwd.hpp"
#include "AST/Node.hpp"
#include "SynTree/Expression.h"
#include "SynTree/Declaration.h"
#include "SynTree/Type.h"

#include "ResolvExpr/AlternativeFinder.h"
#include "ResolvExpr/CandidateFinder.hpp"

namespace Tuples {
	// TupleAssignment.cc
	void handleTupleAssignment( ResolvExpr::AlternativeFinder & currentFinder, UntypedExpr * assign,
		std::vector< ResolvExpr::AlternativeFinder >& args );
	void handleTupleAssignment( 
		ResolvExpr::CandidateFinder & finder, const ast::UntypedExpr * assign, 
		std::vector< ResolvExpr::CandidateFinder > & args );

	// TupleExpansion.cc
	/// expands z.[a, b.[x, y], c] into [z.a, z.b.x, z.b.y, z.c], inserting UniqueExprs as appropriate
	void expandMemberTuples( std::list< Declaration * > & translationUnit );

	/// replaces tuple-related elements, such as TupleType, TupleExpr, TupleAssignExpr, etc.
	void expandTuples( std::list< Declaration * > & translationUnit );

	/// replaces UniqueExprs with a temporary variable and one call
	void expandUniqueExpr( std::list< Declaration * > & translationUnit );

	/// returns VoidType if any of the expressions have Voidtype, otherwise TupleType of the Expression result types
	Type * makeTupleType( const std::list< Expression * > & exprs );
	const ast::Type * makeTupleType( const std::vector<ast::ptr<ast::Expr>> & exprs );

	/// returns a TypeInstType if `type` is a ttype, nullptr otherwise
	TypeInstType * isTtype( Type * type );
	const TypeInstType * isTtype( const Type * type );
	const ast::TypeInstType * isTtype( const ast::Type * type );

	/// returns true if the expression may contain side-effects.
	bool maybeImpure( const Expression * expr );
	bool maybeImpure( const ast::Expr * expr );

	/// Returns true if the expression may contain side-effect,
	/// ignoring the presence of unique expressions.
	bool maybeImpureIgnoreUnique( const Expression * expr );
	bool maybeImpureIgnoreUnique( const ast::Expr * expr );
} // namespace Tuples

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
