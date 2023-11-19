//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Tuples.cc -- A collection of tuple operations.
//
// Author           : Andrew Beach
// Created On       : Mon Jun 17 14:41:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Mon May 16 16:15:00 2022
// Update Count     : 2
//

#include "Tuples.h"

#include "AST/Pass.hpp"
#include "AST/Inspect.hpp"
#include "AST/LinkageSpec.hpp"
#include "InitTweak/InitTweak.h"

namespace Tuples {

namespace {

/// Determines if impurity (read: side-effects) may exist in a piece of code.
/// Currently gives a very crude approximation, wherein almost any function
/// call expression means the code may be impure.
struct ImpurityDetector : public ast::WithShortCircuiting {
	bool result = false;

	void previsit( ast::ApplicationExpr const * appExpr ) {
		if ( ast::DeclWithType const * function = ast::getFunction( appExpr ) ) {
			if ( function->linkage == ast::Linkage::Intrinsic
					&& ( function->name == "*?" || function->name == "?[?]" ) ) {
				return;
			}
		}
		result = true; visit_children = false;
	}
	void previsit( ast::UntypedExpr const * ) {
		result = true; visit_children = false;
	}
};

struct ImpurityDetectorIgnoreUnique : public ImpurityDetector {
	using ImpurityDetector::previsit;
	void previsit( ast::UniqueExpr const * ) {
		visit_children = false;
	}
};

} // namespace

bool maybeImpure( const ast::Expr * expr ) {
	return ast::Pass<ImpurityDetector>::read( expr );
}

bool maybeImpureIgnoreUnique( const ast::Expr * expr ) {
	return ast::Pass<ImpurityDetectorIgnoreUnique>::read( expr );
}

} // namespace Tuples

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
