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
// Last Modified By : Andrew Beach
// Last Modified On : Tue Sep 20 16:17:00 2022
// Update Count     : 4
//

#include "Tuples.h"

#include "AST/Pass.hpp"
#include "Common/ScopedMap.h"

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

/// given a expression representing the member and an expression representing the aggregate,
/// reconstructs a flattened UntypedMemberExpr with the right precedence
const ast::Expr * reconstructMemberExpr( const ast::Expr * member, const ast::Expr * aggr, const CodeLocation & loc ) {
	if ( auto memberExpr = dynamic_cast< const ast::UntypedMemberExpr * >( member ) ) {
		// construct a new UntypedMemberExpr with the correct structure , and recursively
		// expand that member expression.
		ast::Pass< MemberTupleExpander > expander;
		auto inner = new ast::UntypedMemberExpr( loc, memberExpr->aggregate, aggr );
		auto newMemberExpr = new ast::UntypedMemberExpr( loc, memberExpr->member, inner );
		return newMemberExpr->accept( expander );
	} else {
		// not a member expression, so there is nothing to do but attach and return
		return new ast::UntypedMemberExpr( loc, member, aggr );
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
		return mutExpr;
	} else {
		// there may be a tuple expr buried in the aggregate
		return new ast::UntypedMemberExpr( loc, memberExpr->member, memberExpr->aggregate->accept( *visitor ) );
	}
}

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
	return ast::deepCopy(decls[id].get());
}

/// Replaces Tuple Assign & Index Expressions, and Tuple Types.
struct TupleMainExpander final :
		public ast::WithCodeLocation,
		public ast::WithDeclsToAdd<>,
		public ast::WithGuards,
		public ast::WithVisitorRef<TupleMainExpander> {
	ast::Expr const * postvisit( ast::TupleAssignExpr const * expr ) {
		// Just move the env on the new top level expression.
		return ast::mutate_field( expr->stmtExpr.get(),
			&ast::TupleAssignExpr::env, expr->env.get() );
	}

	void previsit( ast::CompoundStmt const * ) {
		GuardScope( typeMap );
	}

	ast::Expr const * postvisit( ast::Expr const * expr ) {
		if ( nullptr == expr->env ) {
			return expr;
		}
		// TypeSubstitutions are never visited in the new Pass template,
		// so it is done manually here, where all types have to be replaced.
		return ast::mutate_field( expr, &ast::Expr::env,
			expr->env->accept( *visitor ) );
	}

	ast::Type const * postvisit( ast::TupleType const * type ) {
		assert( location );
		unsigned tupleSize = type->size();
		if ( !typeMap.count( tupleSize ) ) {
			ast::StructDecl * decl = new ast::StructDecl( *location,
				toString( "_tuple", tupleSize, "_" )
			);
			decl->body = true;

			for ( size_t i = 0 ; i < tupleSize ; ++i ) {
				ast::TypeDecl * typeParam = new ast::TypeDecl( *location,
					toString( "tuple_param_", tupleSize, "_", i ),
					ast::Storage::Classes(),
					nullptr,
					ast::TypeDecl::Dtype,
					true
					);
				ast::ObjectDecl * member = new ast::ObjectDecl( *location,
					toString( "field_", i ),
					new ast::TypeInstType( typeParam )
					);
				decl->params.push_back( typeParam );
				decl->members.push_back( member );
			}

			// Empty structures are not standard C. Add a dummy field to
			// empty tuples to silence warnings when a compound literal
			// `_tuple0_` is created.
			if ( tupleSize == 0 ) {
				decl->members.push_back(
					new ast::ObjectDecl( *location,
						"dummy",
						new ast::BasicType( ast::BasicType::SignedInt ),
						nullptr,
						ast::Storage::Classes(),
						// Does this have to be a C linkage?
						ast::Linkage::C
					)
				);
			}
			typeMap[tupleSize] = decl;
			declsToAddBefore.push_back( decl );
		}

		ast::StructDecl const * decl = typeMap[ tupleSize ];
		ast::StructInstType * newType =
			new ast::StructInstType( decl, type->qualifiers );
		for ( auto pair : group_iterate( type->types, decl->params ) ) {
			ast::Type const * t = std::get<0>( pair );
			newType->params.push_back(
				new ast::TypeExpr( *location, ast::deepCopy( t ) ) );
		}
		return newType;
	}

	ast::Expr const * postvisit( ast::TupleIndexExpr const * expr ) {
		CodeLocation const & location = expr->location;
		ast::Expr const * tuple = expr->tuple.get();
		assert( tuple );
		unsigned int index = expr->index;
		ast::TypeSubstitution const * env = expr->env.get();

		if ( auto tupleExpr = dynamic_cast<ast::TupleExpr const *>( tuple ) ) {
			// Optimization: If it is a definitely pure tuple expr,
			// then it can reduce to the only relevant component.
			if ( !maybeImpureIgnoreUnique( tupleExpr ) ) {
				assert( index < tupleExpr->exprs.size() );
				ast::ptr<ast::Expr> const & expr =
					*std::next( tupleExpr->exprs.begin(), index );
				ast::Expr * ret = ast::mutate( expr.get() );
				ret->env = env;
				return ret;
			}
		}

		auto type = tuple->result.strict_as<ast::StructInstType>();
		ast::StructDecl const * structDecl = type->base;
		assert( index < structDecl->members.size() );
		ast::ptr<ast::Decl> const & member =
			*std::next( structDecl->members.begin(), index );
		ast::MemberExpr * memberExpr = new ast::MemberExpr( location,
			member.strict_as<ast::DeclWithType>(), tuple );
		memberExpr->env = env;
		return memberExpr;
	}
private:
	ScopedMap< int, ast::StructDecl const * > typeMap;
};

ast::Expr const * replaceTupleExpr(
		CodeLocation const & location,
		ast::Type const * result,
		std::vector<ast::ptr<ast::Expr>> const & exprs,
		ast::TypeSubstitution const * env ) {
	assert( result );
	// A void result: It doesn't need to produce a value for cascading,
	// just output a chain of comma exprs.
	if ( result->isVoid() ) {
		assert( !exprs.empty() );
		std::vector<ast::ptr<ast::Expr>>::const_iterator iter = exprs.begin();
		ast::Expr * expr = new ast::CastExpr( *iter++ );
		for ( ; iter != exprs.end() ; ++iter ) {
			expr = new ast::CommaExpr( location,
				expr, new ast::CastExpr( *iter ) );
		}
		expr->env = env;
		return expr;
	// Typed tuple expression: Produce a compound literal which performs
	// each of the expressions as a distinct part of its initializer. The
	// produced compound literal may be used as part of another expression.
	} else {
		auto inits = map_range<std::vector<ast::ptr<ast::Init>>>( exprs,
			[]( ast::Expr const * expr ) {
				return new ast::SingleInit( expr->location, expr );
			}
		);
		ast::Expr * expr = new ast::CompoundLiteralExpr( location,
			result, new ast::ListInit( location, std::move( inits ) ) );
		expr->env = env;
		return expr;
	}
}

struct TupleExprExpander final {
	ast::Expr const * postvisit( ast::TupleExpr const * expr ) {
		return replaceTupleExpr( expr->location,
			expr->result, expr->exprs, expr->env );
	}
};

} // namespace

void expandMemberTuples( ast::TranslationUnit & translationUnit ) {
	ast::Pass< MemberTupleExpander >::run( translationUnit );
}

void expandUniqueExpr( ast::TranslationUnit & translationUnit ) {
	ast::Pass< UniqueExprExpander >::run( translationUnit );
}

void expandTuples( ast::TranslationUnit & translationUnit ) {
	// These can't just be combined simply (there might be a way with work).
	ast::Pass<TupleMainExpander>::run( translationUnit );
	ast::Pass<TupleExprExpander>::run( translationUnit );
}

} // namespace Tuples
