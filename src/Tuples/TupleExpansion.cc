//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TupleAssignment.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue May 17 15:02:00 2022
// Update Count     : 25
//

#include <stddef.h>               // for size_t
#include <cassert>                // for assert
#include <list>                   // for list
#include <vector>

#include "AST/CVQualifiers.hpp"
#include "AST/Expr.hpp"
#include "AST/Node.hpp"
#include "AST/Type.hpp"
#include "Common/PassVisitor.h"   // for PassVisitor, WithDeclsToAdd, WithGu...
#include "Common/ScopedMap.h"     // for ScopedMap
#include "Common/utility.h"       // for CodeLocation
#include "InitTweak/InitTweak.h"  // for getFunction
#include "SynTree/LinkageSpec.h"  // for Spec, C, Intrinsic
#include "SynTree/Constant.h"     // for Constant
#include "SynTree/Declaration.h"  // for StructDecl, DeclarationWithType
#include "SynTree/Expression.h"   // for UntypedMemberExpr, Expression, Uniq...
#include "SynTree/Label.h"        // for operator==, Label
#include "SynTree/Mutator.h"      // for Mutator
#include "SynTree/Type.h"         // for Type, Type::Qualifiers, TupleType
#include "SynTree/Visitor.h"      // for Visitor
#include "Tuples.h"

class CompoundStmt;
class TypeSubstitution;

namespace Tuples {
	namespace {
		struct MemberTupleExpander final : public WithShortCircuiting, public WithVisitorRef<MemberTupleExpander> {
			void premutate( UntypedMemberExpr * ) { visit_children = false; }
			Expression * postmutate( UntypedMemberExpr * memberExpr );
		};

		struct UniqueExprExpander final : public WithDeclsToAdd {
			Expression * postmutate( UniqueExpr * unqExpr );

			std::map< int, Expression * > decls; // not vector, because order added may not be increasing order

			~UniqueExprExpander() {
				for ( std::pair<const int, Expression *> & p : decls ) {
					delete p.second;
				}
			}
		};

		struct TupleAssignExpander {
			Expression * postmutate( TupleAssignExpr * tupleExpr );
		};

		struct TupleTypeReplacer : public WithDeclsToAdd, public WithGuards, public WithConstTypeSubstitution {
			Type * postmutate( TupleType * tupleType );

			void premutate( CompoundStmt * ) {
				GuardScope( typeMap );
			}
		  private:
			ScopedMap< int, StructDecl * > typeMap;
		};

		struct TupleIndexExpander {
			Expression * postmutate( TupleIndexExpr * tupleExpr );
		};

		struct TupleExprExpander final {
			Expression * postmutate( TupleExpr * tupleExpr );
		};
	}

	void expandMemberTuples( std::list< Declaration * > & translationUnit ) {
		PassVisitor<MemberTupleExpander> expander;
		mutateAll( translationUnit, expander );
	}

	void expandUniqueExpr( std::list< Declaration * > & translationUnit ) {
		PassVisitor<UniqueExprExpander> unqExpander;
		mutateAll( translationUnit, unqExpander );
	}

	void expandTuples( std::list< Declaration * > & translationUnit ) {
		PassVisitor<TupleAssignExpander> assnExpander;
		mutateAll( translationUnit, assnExpander );

		PassVisitor<TupleTypeReplacer> replacer;
		mutateAll( translationUnit, replacer );

		PassVisitor<TupleIndexExpander> idxExpander;
		mutateAll( translationUnit, idxExpander );

		PassVisitor<TupleExprExpander> exprExpander;
		mutateAll( translationUnit, exprExpander );
	}

	namespace {
		/// given a expression representing the member and an expression representing the aggregate,
		/// reconstructs a flattened UntypedMemberExpr with the right precedence
		Expression * reconstructMemberExpr( Expression * member, Expression * aggr, CodeLocation & loc ) {
			if ( UntypedMemberExpr * memberExpr = dynamic_cast< UntypedMemberExpr * >( member ) ) {
				// construct a new UntypedMemberExpr with the correct structure , and recursively
				// expand that member expression.
				PassVisitor<MemberTupleExpander> expander;
				UntypedMemberExpr * inner = new UntypedMemberExpr( memberExpr->aggregate, aggr->clone() );
				UntypedMemberExpr * newMemberExpr = new UntypedMemberExpr( memberExpr->member, inner );
				inner->location = newMemberExpr->location = loc;
				memberExpr->member = nullptr;
				memberExpr->aggregate = nullptr;
				delete memberExpr;
				return newMemberExpr->acceptMutator( expander );
			} else {
				// not a member expression, so there is nothing to do but attach and return
				UntypedMemberExpr * newMemberExpr = new UntypedMemberExpr( member, aggr->clone() );
				newMemberExpr->location = loc;
				return newMemberExpr;
			}
		}
	}

	Expression * MemberTupleExpander::postmutate( UntypedMemberExpr * memberExpr ) {
		if ( UntypedTupleExpr * tupleExpr = dynamic_cast< UntypedTupleExpr * > ( memberExpr->member ) ) {
			Expression * aggr = memberExpr->aggregate->clone()->acceptMutator( *visitor );
			// aggregate expressions which might be impure must be wrapped in unique expressions
			if ( Tuples::maybeImpureIgnoreUnique( memberExpr->aggregate ) ) aggr = new UniqueExpr( aggr );
			for ( Expression *& expr : tupleExpr->exprs ) {
				expr = reconstructMemberExpr( expr, aggr, memberExpr->location );
				expr->location = memberExpr->location;
			}
			delete aggr;
			tupleExpr->location = memberExpr->location;
			return tupleExpr;
		} else {
			// there may be a tuple expr buried in the aggregate
			// xxx - this is a memory leak
			UntypedMemberExpr * newMemberExpr = new UntypedMemberExpr( memberExpr->member->clone(), memberExpr->aggregate->acceptMutator( *visitor ) );
			newMemberExpr->location = memberExpr->location;
			return newMemberExpr;
		}
	}

	Expression * UniqueExprExpander::postmutate( UniqueExpr * unqExpr ) {
		const int id = unqExpr->get_id();

		// on first time visiting a unique expr with a particular ID, generate the expression that replaces all UniqueExprs with that ID,
		// and lookup on subsequent hits. This ensures that all unique exprs with the same ID reference the same variable.
		if ( ! decls.count( id ) ) {
			Expression * assignUnq;
			Expression * var = unqExpr->get_var();
			if ( unqExpr->get_object() ) {
				// an object was generated to represent this unique expression -- it should be added to the list of declarations now
				declsToAddBefore.push_back( unqExpr->get_object() );
				unqExpr->set_object( nullptr );
				// steal the expr from the unqExpr
				assignUnq = UntypedExpr::createAssign( unqExpr->get_var()->clone(), unqExpr->get_expr() );
				unqExpr->set_expr( nullptr );
			} else {
				// steal the already generated assignment to var from the unqExpr - this has been generated by FixInit
				Expression * expr = unqExpr->get_expr();
				CommaExpr * commaExpr = strict_dynamic_cast< CommaExpr * >( expr );
				assignUnq = commaExpr->get_arg1();
				commaExpr->set_arg1( nullptr );
			}
			ObjectDecl * finished = new ObjectDecl( toString( "_unq", id, "_finished_" ), Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new BasicType( Type::Qualifiers(), BasicType::Bool ),
													new SingleInit( new ConstantExpr( Constant::from_int( 0 ) ) ) );
			declsToAddBefore.push_back( finished );
			// (finished ? _unq_expr_N : (_unq_expr_N = <unqExpr->get_expr()>, finished = 1, _unq_expr_N))
			// This pattern ensures that each unique expression is evaluated once, regardless of evaluation order of the generated C code.
			Expression * assignFinished = UntypedExpr::createAssign( new VariableExpr(finished), new ConstantExpr( Constant::from_int( 1 ) ) );
			ConditionalExpr * condExpr = new ConditionalExpr( new VariableExpr( finished ), var->clone(),
				new CommaExpr( new CommaExpr( assignUnq, assignFinished ), var->clone() ) );
			condExpr->set_result( var->get_result()->clone() );
			condExpr->set_env( maybeClone( unqExpr->get_env() ) );
			decls[id] = condExpr;
		}
		delete unqExpr;
		return decls[id]->clone();
	}

	Expression * TupleAssignExpander::postmutate( TupleAssignExpr * assnExpr ) {
		StmtExpr * ret = assnExpr->get_stmtExpr();
		assnExpr->set_stmtExpr( nullptr );
		// move env to StmtExpr
		ret->set_env( assnExpr->get_env() );
		assnExpr->set_env( nullptr );
		delete assnExpr;
		return ret;
	}

	Type * TupleTypeReplacer::postmutate( TupleType * tupleType ) {
		unsigned tupleSize = tupleType->size();
		if ( ! typeMap.count( tupleSize ) ) {
			// generate struct type to replace tuple type based on the number of components in the tuple
			StructDecl * decl = new StructDecl( toString( "_tuple", tupleSize, "_" ) );
			decl->location = tupleType->location;
			decl->set_body( true );
			for ( size_t i = 0; i < tupleSize; ++i ) {
				TypeDecl * tyParam = new TypeDecl( toString( "tuple_param_", tupleSize, "_", i ), Type::StorageClasses(), nullptr, TypeDecl::Dtype, true );
				decl->get_members().push_back( new ObjectDecl( toString("field_", i ), Type::StorageClasses(), LinkageSpec::C, nullptr, new TypeInstType( Type::Qualifiers(), tyParam->get_name(), tyParam ), nullptr ) );
				decl->get_parameters().push_back( tyParam );
			}
			if ( tupleSize == 0 ) {
				// empty structs are not standard C. Add a dummy field to empty tuples to silence warnings when a compound literal Tuple0 is created.
				decl->get_members().push_back( new ObjectDecl( "dummy", Type::StorageClasses(), LinkageSpec::C, nullptr, new BasicType( Type::Qualifiers(), BasicType::SignedInt ), nullptr ) );
			}
			typeMap[tupleSize] = decl;
			declsToAddBefore.push_back( decl );
		}
		Type::Qualifiers qualifiers = tupleType->get_qualifiers();

		StructDecl * decl = typeMap[tupleSize];
		StructInstType * newType = new StructInstType( qualifiers, decl );
		for ( auto p : group_iterate( tupleType->get_types(), decl->get_parameters() ) ) {
			Type * t = std::get<0>(p);
			newType->get_parameters().push_back( new TypeExpr( t->clone() ) );
		}
		delete tupleType;
		return newType;
	}

	Expression * TupleIndexExpander::postmutate( TupleIndexExpr * tupleExpr ) {
		Expression * tuple = tupleExpr->tuple;
		assert( tuple );
		tupleExpr->tuple = nullptr;
		unsigned int idx = tupleExpr->index;
		TypeSubstitution * env = tupleExpr->env;
		tupleExpr->env = nullptr;
		delete tupleExpr;

		if ( TupleExpr * tupleExpr = dynamic_cast< TupleExpr * > ( tuple ) ) {
			if ( ! maybeImpureIgnoreUnique( tupleExpr ) ) {
				// optimization: definitely pure tuple expr => can reduce to the only relevant component.
				assert( tupleExpr->exprs.size() > idx );
				Expression *& expr = *std::next(tupleExpr->exprs.begin(), idx);
				Expression * ret = expr;
				ret->env = env;
				expr = nullptr; // remove from list so it can safely be deleted
				delete tupleExpr;
				return ret;
			}
		}

		StructInstType * type = strict_dynamic_cast< StructInstType * >( tuple->result );
		StructDecl * structDecl = type->baseStruct;
		assert( structDecl->members.size() > idx );
		Declaration * member = *std::next(structDecl->members.begin(), idx);
		MemberExpr * memExpr = new MemberExpr( strict_dynamic_cast< DeclarationWithType * >( member ), tuple );
		memExpr->env = env;
		return memExpr;
	}

	Expression * replaceTupleExpr( Type * result, const std::list< Expression * > & exprs, TypeSubstitution * env ) {
		if ( result->isVoid() ) {
			// void result - don't need to produce a value for cascading - just output a chain of comma exprs
			assert( ! exprs.empty() );
			std::list< Expression * >::const_iterator iter = exprs.begin();
			Expression * expr = new CastExpr( *iter++ );
			for ( ; iter != exprs.end(); ++iter ) {
				expr = new CommaExpr( expr, new CastExpr( *iter ) );
			}
			expr->set_env( env );
			return expr;
		} else {
			// typed tuple expression - produce a compound literal which performs each of the expressions
			// as a distinct part of its initializer - the produced compound literal may be used as part of
			// another expression
			std::list< Initializer * > inits;
			for ( Expression * expr : exprs ) {
				inits.push_back( new SingleInit( expr ) );
			}
			Expression * expr = new CompoundLiteralExpr( result, new ListInit( inits ) );
			expr->set_env( env );
			return expr;
		}
	}

	Expression * TupleExprExpander::postmutate( TupleExpr * tupleExpr ) {
		Type * result = tupleExpr->get_result();
		std::list< Expression * > exprs = tupleExpr->get_exprs();
		assert( result );
		TypeSubstitution * env = tupleExpr->get_env();

		// remove data from shell and delete it
		tupleExpr->set_result( nullptr );
		tupleExpr->get_exprs().clear();
		tupleExpr->set_env( nullptr );
		delete tupleExpr;

		return replaceTupleExpr( result, exprs, env );
	}

	Type * makeTupleType( const std::list< Expression * > & exprs ) {
		// produce the TupleType which aggregates the types of the exprs
		std::list< Type * > types;
		Type::Qualifiers qualifiers( Type::Const | Type::Volatile | Type::Restrict | Type::Atomic | Type::Mutex );
		for ( Expression * expr : exprs ) {
			assert( expr->get_result() );
			if ( expr->get_result()->isVoid() ) {
				// if the type of any expr is void, the type of the entire tuple is void
				return new VoidType( Type::Qualifiers() );
			}
			Type * type = expr->get_result()->clone();
			types.push_back( type );
			// the qualifiers on the tuple type are the qualifiers that exist on all component types
			qualifiers &= type->get_qualifiers();
		} // for
		if ( exprs.empty() ) qualifiers = Type::Qualifiers();
		return new TupleType( qualifiers, types );
	}
	const ast::Type * makeTupleType( const std::vector<ast::ptr<ast::Expr>> & exprs ) {
		// produce the TupleType which aggregates the types of the exprs
		std::vector<ast::ptr<ast::Type>> types;
		ast::CV::Qualifiers quals{
			ast::CV::Const | ast::CV::Volatile | ast::CV::Restrict |
			ast::CV::Atomic | ast::CV::Mutex };

		for ( const ast::Expr * expr : exprs ) {
			assert( expr->result );
			// if the type of any expr is void, the type of the entire tuple is void
			if ( expr->result->isVoid() ) return new ast::VoidType{};

			// qualifiers on the tuple type are the qualifiers that exist on all components
			quals &= expr->result->qualifiers;

			types.emplace_back( expr->result );
		}

		if ( exprs.empty() ) { quals = ast::CV::Qualifiers{}; }
		return new ast::TupleType{ std::move(types), quals };
	}

	TypeInstType * isTtype( Type * type ) {
		if ( TypeInstType * inst = dynamic_cast< TypeInstType * >( type ) ) {
			if ( inst->get_baseType() && inst->get_baseType()->get_kind() == TypeDecl::Ttype ) {
				return inst;
			}
		}
		return nullptr;
	}

	const TypeInstType * isTtype( const Type * type ) {
		if ( const TypeInstType * inst = dynamic_cast< const TypeInstType * >( type ) ) {
			if ( inst->baseType && inst->baseType->kind == TypeDecl::Ttype ) {
				return inst;
			}
		}
		return nullptr;
	}

	const ast::TypeInstType * isTtype( const ast::Type * type ) {
		if ( const ast::TypeInstType * inst = dynamic_cast< const ast::TypeInstType * >( type ) ) {
			if ( inst->base && inst->base->kind == ast::TypeDecl::Ttype ) {
				return inst;
			}
		}
		return nullptr;
	}
} // namespace Tuples

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
