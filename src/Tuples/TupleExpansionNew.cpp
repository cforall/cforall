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
	struct UniqueExprExpander final : public ast::WithDeclsToAdd<> {
		const ast::Expr * postvisit( const ast::UniqueExpr * unqExpr );
		std::map< int, ast::ptr<ast::Expr> > decls; // not vector, because order added may not be increasing order
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

void expandUniqueExpr( ast::TranslationUnit & translationUnit ) {
	ast::Pass< UniqueExprExpander >::run( translationUnit );
}

namespace {
	const ast::Expr * UniqueExprExpander::postvisit( const ast::UniqueExpr * unqExpr ) {
		const CodeLocation loc = unqExpr->location;
		const int id = unqExpr->id;

		// on first time visiting a unique expr with a particular ID, generate the expression that replaces all UniqueExprs with that ID,
		// and lookup on subsequent hits. This ensures that all unique exprs with the same ID reference the same variable.
		if ( ! decls.count( id ) ) {
			ast::ptr< ast::Expr > assignUnq;
			const ast::VariableExpr * var = unqExpr->var;
			if ( unqExpr->object ) {
				// an object was generated to represent this unique expression -- it should be added to the list of declarations now
				declsToAddBefore.push_back( unqExpr->object.as< ast::Decl >() );
				// deep copy required due to unresolved issues with UniqueExpr
				assignUnq = ast::UntypedExpr::createAssign( loc, var, unqExpr->expr );
			} else {
				const auto commaExpr = unqExpr->expr.strict_as< ast::CommaExpr >();
				assignUnq = commaExpr->arg1;
			}
			auto finished = new ast::ObjectDecl( loc, toString( "_unq", id, "_finished_" ), new ast::BasicType( ast::BasicType::Kind::Bool ),
				new ast::SingleInit( loc, ast::ConstantExpr::from_int( loc, 0 ) ), {}, ast::Linkage::Cforall );
			declsToAddBefore.push_back( finished );
			// (finished ? _unq_expr_N : (_unq_expr_N = <unqExpr->get_expr()>, finished = 1, _unq_expr_N))
			// This pattern ensures that each unique expression is evaluated once, regardless of evaluation order of the generated C code.
			auto assignFinished = ast::UntypedExpr::createAssign( loc, new ast::VariableExpr( loc, finished ),
				ast::ConstantExpr::from_int( loc, 1 ) );
			auto condExpr = new ast::ConditionalExpr( loc, new ast::VariableExpr( loc, finished ), var,
				new ast::CommaExpr( loc, new ast::CommaExpr( loc, assignUnq, assignFinished ), var ) );
			condExpr->result = var->result;
			condExpr->env = unqExpr->env;
			decls[id] = condExpr;
		}
		//delete unqExpr;
		return ast::deepCopy(decls[id].get());
	}
} // namespace
} // namespace Tuples
