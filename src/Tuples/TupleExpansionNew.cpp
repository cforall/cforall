//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TupleExpansionNew.cpp --
//
// Author           : Henry Xue
// Created On       : Mon Aug 23 15:36:09 2021
// Last Modified By : Henry Xue
// Last Modified On : Mon Aug 23 15:36:09 2021
// Update Count     : 1
//

#include "Tuples.h"

namespace Tuples {
namespace {
	struct MemberTupleExpander final : public ast::WithShortCircuiting, public ast::WithVisitorRef< MemberTupleExpander > {
		void previsit( const ast::UntypedMemberExpr * ) { visit_children = false; }
        const ast::Expr * postvisit( const ast::UntypedMemberExpr * memberExpr );
	};
} // namespace

void expandMemberTuples( ast::TranslationUnit & translationUnit ) {
	ast::Pass< MemberTupleExpander >::run( translationUnit );
}

namespace {
	namespace {
		/// given a expression representing the member and an expression representing the aggregate,
		/// reconstructs a flattened UntypedMemberExpr with the right precedence
		const ast::Expr * reconstructMemberExpr( const ast::Expr * member, const ast::Expr * aggr, const CodeLocation & loc ) {
			if ( auto memberExpr = dynamic_cast< const ast::UntypedMemberExpr * >( member ) ) {
				// construct a new UntypedMemberExpr with the correct structure , and recursively
				// expand that member expression.
				ast::Pass< MemberTupleExpander > expander;
				auto inner = new ast::UntypedMemberExpr( loc, memberExpr->aggregate, aggr );
				auto newMemberExpr = new ast::UntypedMemberExpr( loc, memberExpr->member, inner );
				//delete memberExpr;
				return newMemberExpr->accept( expander );
			} else {
				// not a member expression, so there is nothing to do but attach and return
				return new ast::UntypedMemberExpr( loc, member, aggr );
			}
		}
	}

	const ast::Expr * MemberTupleExpander::postvisit( const ast::UntypedMemberExpr * memberExpr ) {
		const CodeLocation loc = memberExpr->location;
        if ( auto tupleExpr = memberExpr->member.as< ast::UntypedTupleExpr >() ) {
			auto mutExpr = mutate( tupleExpr );
			ast::ptr< ast::Expr > aggr = memberExpr->aggregate->accept( *visitor );
			// aggregate expressions which might be impure must be wrapped in unique expressions
			if ( Tuples::maybeImpureIgnoreUnique( memberExpr->aggregate ) ) aggr = new ast::UniqueExpr( loc, aggr );
			for ( auto & expr : mutExpr->exprs ) {
				expr = reconstructMemberExpr( expr, aggr, loc );
			}
			//delete aggr;
			return mutExpr;
		} else {
			// there may be a tuple expr buried in the aggregate
			return new ast::UntypedMemberExpr( loc, memberExpr->member, memberExpr->aggregate->accept( *visitor ) );
		}
	}
} // namespace
} // namespace Tuples
