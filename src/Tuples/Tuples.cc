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
// Last Modified On : Mon May 16 16:15:00 2022
// Update Count     : 2
//

#include "Tuples.h"

#include "AST/Pass.hpp"
#include "AST/LinkageSpec.hpp"
#include "Common/PassVisitor.h"
#include "InitTweak/InitTweak.h"

namespace Tuples {

namespace {
	/// Checks if impurity (read: side-effects) may exist in a piece of code.
	/// Currently gives a very crude approximation, wherein any function
	/// call expression means the code may be impure.
	struct ImpurityDetector_old : public WithShortCircuiting {
		bool const ignoreUnique;
		bool maybeImpure;

		ImpurityDetector_old( bool ignoreUnique ) :
			ignoreUnique( ignoreUnique ), maybeImpure( false )
		{}

		void previsit( const ApplicationExpr * appExpr ) {
			visit_children = false;
			if ( const DeclarationWithType * function =
					InitTweak::getFunction( appExpr ) ) {
				if ( function->linkage == LinkageSpec::Intrinsic ) {
					if ( function->name == "*?" || function->name == "?[?]" ) {
						// intrinsic dereference, subscript are pure,
						// but need to recursively look for impurity
						visit_children = true;
						return;
					}
				}
			}
			maybeImpure = true;
		}

		void previsit( const UntypedExpr * ) {
			maybeImpure = true;
			visit_children = false;
		}

		void previsit( const UniqueExpr * ) {
			if ( ignoreUnique ) {
				// bottom out at unique expression.
				// The existence of a unique expression doesn't change the purity of an expression.
				// That is, even if the wrapped expression is impure, the wrapper protects the rest of the expression.
				visit_children = false;
				return;
			}
		}
	};

	bool detectImpurity( const Expression * expr, bool ignoreUnique ) {
		PassVisitor<ImpurityDetector_old> detector( ignoreUnique );
		expr->accept( detector );
		return detector.pass.maybeImpure;
	}

	/// Determines if impurity (read: side-effects) may exist in a piece of code. Currently gives
	/// a very crude approximation, wherein any function call expression means the code may be
	/// impure.
    struct ImpurityDetector : public ast::WithShortCircuiting {
		bool result = false;

		void previsit( ast::ApplicationExpr const * appExpr ) {
			if ( ast::DeclWithType const * function = InitTweak::getFunction( appExpr ) ) {
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

bool maybeImpure( const Expression * expr ) {
	return detectImpurity( expr, false );
}

bool maybeImpureIgnoreUnique( const Expression * expr ) {
	return detectImpurity( expr, true );
}

} // namespace Tuples

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
