//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Tuples.h --
//
// Author           : Andrew Beach
// Created On       : Mon Jun 17 14:41:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jun 18  9:31:00 2019
// Update Count     : 1
//

#include "Tuples.h"

#include "AST/Pass.hpp"
#include "AST/LinkageSpec.hpp"
#include "InitTweak/InitTweak.h"

namespace Tuples {

namespace {
	/// Determines if impurity (read: side-effects) may exist in a piece of code. Currently gives
	/// a very crude approximation, wherein any function call expression means the code may be
	/// impure.
    struct ImpurityDetector : public ast::WithShortCircuiting {
		bool maybeImpure = false;

		void previsit( ast::ApplicationExpr const * appExpr ) {
			if ( ast::DeclWithType const * function = InitTweak::getFunction( appExpr ) ) {
				if ( function->linkage == ast::Linkage::Intrinsic
						&& ( function->name == "*?" || function->name == "?[?]" ) ) {
					return;
				}
			}
			maybeImpure = true; visit_children = false;
		}
		void previsit( ast::UntypedExpr const * ) {
			maybeImpure = true; visit_children = false;
		}
	};
	struct ImpurityDetectorIgnoreUnique : public ImpurityDetector {
		using ImpurityDetector::previsit;
		void previsit( ast::UniqueExpr const * ) {
			visit_children = false;
		}
	};

	template<typename Detector>
	bool detectImpurity( const ast::Expr * expr ) {
		ast::Pass<Detector> detector;
		expr->accept( detector );
		return detector.core.maybeImpure;
	}
} // namespace

bool maybeImpure( const ast::Expr * expr ) {
	return detectImpurity<ImpurityDetector>( expr );
}

bool maybeImpureIgnoreUnique( const ast::Expr * expr ) {
	return detectImpurity<ImpurityDetectorIgnoreUnique>( expr );
}

} // namespace Tuples

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
