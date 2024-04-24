//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Unify.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:27:10 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:43:05 2019
// Update Count     : 46
//

#include "Unify.h"

#include <cassert>                  // for assertf, assert
#include <iterator>                 // for back_insert_iterator, back_inserter
#include <map>                      // for _Rb_tree_const_iterator, _Rb_tree_i...
#include <memory>                   // for unique_ptr
#include <set>                      // for set
#include <string>                   // for string, operator==, operator!=, bas...
#include <utility>                  // for pair, move
#include <vector>

#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"
#include "Common/Eval.h"            // for eval
#include "CommonType.hpp"           // for commonType
#include "FindOpenVars.h"           // for findOpenVars
#include "SpecCost.hpp"             // for SpecCost
#include "Tuples/Tuples.h"          // for isTtype
#include "typeops.h"                // for flatten, occurs

namespace ast {
	class SymbolTable;
}

// #define DEBUG

namespace ResolvExpr {

bool typesCompatible(
		const ast::Type * first, const ast::Type * second,
		const ast::TypeEnvironment & env ) {
	ast::TypeEnvironment newEnv;
	ast::OpenVarSet open, closed;
	ast::AssertionSet need, have;

	ast::ptr<ast::Type> newFirst( first ), newSecond( second );
	env.apply( newFirst );
	env.apply( newSecond );

	// findOpenVars( newFirst, open, closed, need, have, FirstClosed );
	findOpenVars( newSecond, open, closed, need, have, newEnv, FirstOpen );

	return unifyExact(newFirst, newSecond, newEnv, need, have, open, noWiden() );
}

bool typesCompatibleIgnoreQualifiers(
		const ast::Type * first, const ast::Type * second,
		const ast::TypeEnvironment & env ) {
	ast::TypeEnvironment newEnv;
	ast::OpenVarSet open;
	ast::AssertionSet need, have;

	ast::Type * newFirst  = shallowCopy( first  );
	ast::Type * newSecond = shallowCopy( second );

	newFirst ->qualifiers = {};
	newSecond->qualifiers = {};
	ast::ptr< ast::Type > t1_(newFirst );
	ast::ptr< ast::Type > t2_(newSecond);

	ast::ptr< ast::Type > subFirst = env.apply(newFirst).node;
	ast::ptr< ast::Type > subSecond = env.apply(newSecond).node;

	return unifyExact(
		subFirst,
		subSecond,
		newEnv, need, have, open, noWiden() );
}

namespace {
	/// Replaces ttype variables with their bound types.
	/// If this isn't done when satifying ttype assertions, then argument lists can have
	/// different size and structure when they should be compatible.
	struct TtypeExpander : public ast::WithShortCircuiting, public ast::PureVisitor {
		ast::TypeEnvironment & tenv;

		TtypeExpander( ast::TypeEnvironment & env ) : tenv( env ) {}

		const ast::Type * postvisit( const ast::TypeInstType * typeInst ) {
			if ( const ast::EqvClass * clz = tenv.lookup( *typeInst ) ) {
				// expand ttype parameter into its actual type
				if ( clz->data.kind == ast::TypeDecl::Ttype && clz->bound ) {
					return clz->bound;
				}
			}
			return typeInst;
		}
	};
}

std::vector< ast::ptr< ast::Type > > flattenList(
	const std::vector< ast::ptr< ast::Type > > & src, ast::TypeEnvironment & env
) {
	std::vector< ast::ptr< ast::Type > > dst;
	dst.reserve( src.size() );
	for ( const auto & d : src ) {
		ast::Pass<TtypeExpander> expander( env );
		// TtypeExpander pass is impure (may mutate nodes in place)
		// need to make nodes shared to prevent accidental mutation
		ast::ptr<ast::Type> dc = d->accept(expander);
		auto types = flatten( dc );
		for ( ast::ptr< ast::Type > & t : types ) {
			// outermost const, volatile, _Atomic qualifiers in parameters should not play
			// a role in the unification of function types, since they do not determine
			// whether a function is callable.
			// NOTE: **must** consider at least mutex qualifier, since functions can be
			// overloaded on outermost mutex and a mutex function has different
			// requirements than a non-mutex function
			remove_qualifiers( t, ast::CV::Const | ast::CV::Volatile | ast::CV::Atomic );
			dst.emplace_back( t );
		}
	}
	return dst;
}

// Unification of Expressions
//
// Boolean outcome (obvious):  Are they basically spelled the same?
// Side effect of binding variables (subtle):  if `sizeof(int)` ===_expr `sizeof(T)` then `int` ===_ty `T`
//
// Context:  if `float[VAREXPR1]` ===_ty `float[VAREXPR2]` then `VAREXPR1` ===_expr `VAREXPR2`
// where the VAREXPR are meant as notational metavariables representing the fact that unification always
// sees distinct ast::VariableExpr objects at these positions

static bool unify( const ast::Expr * e1, const ast::Expr * e2, ast::TypeEnvironment & env,
	ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open,
	WidenMode widen );

class UnifyExpr final : public ast::WithShortCircuiting {
	const ast::Expr * e2;
	ast::TypeEnvironment & tenv;
	ast::AssertionSet & need;
	ast::AssertionSet & have;
	const ast::OpenVarSet & open;
	WidenMode widen;
public:
	bool result;

private:

	void tryMatchOnStaticValue( const ast::Expr * e1 ) {
		Evaluation r1 = eval(e1);
		Evaluation r2 = eval(e2);

		if ( !r1.hasKnownValue ) return;
		if ( !r2.hasKnownValue ) return;

		if ( r1.knownValue != r2.knownValue ) return;

		visit_children = false;
		result = true;
	}

public:

	void previsit( const ast::Node * ) { assert(false); }

	void previsit( const ast::Expr * e1 ) {
		tryMatchOnStaticValue( e1 );
		visit_children = false;
	}

	void previsit( const ast::CastExpr * e1 ) {
		tryMatchOnStaticValue( e1 );

		if ( result ) {
			assert( visit_children == false );
		} else {
			assert( visit_children == true );
			visit_children = false;

			auto e2c = dynamic_cast< const ast::CastExpr * >( e2 );
			if ( !e2c ) return;

			// inspect casts' target types
			if ( !unifyExact(
				e1->result, e2c->result, tenv, need, have, open, widen ) ) return;

			// inspect casts' inner expressions
			result = unify( e1->arg, e2c->arg, tenv, need, have, open, widen );
		}
	}

	void previsit( const ast::VariableExpr * e1 ) {
		tryMatchOnStaticValue( e1 );

		if ( result ) {
			assert( visit_children == false );
		} else {
			assert( visit_children == true );
			visit_children = false;

			auto e2v = dynamic_cast< const ast::VariableExpr * >( e2 );
			if ( !e2v ) return;

			assert(e1->var);
			assert(e2v->var);

			// conservative: variable exprs match if their declarations are represented by the same C++ AST object
			result = (e1->var == e2v->var);
		}
	}

	void previsit( const ast::SizeofExpr * e1 ) {
		tryMatchOnStaticValue( e1 );

		if ( result ) {
			assert( visit_children == false );
		} else {
			assert( visit_children == true );
			visit_children = false;

			auto e2so = dynamic_cast< const ast::SizeofExpr * >( e2 );
			if ( !e2so ) return;

			assert((e1->type != nullptr) ^ (e1->expr != nullptr));
			assert((e2so->type != nullptr) ^ (e2so->expr != nullptr));
			if ( !(e1->type && e2so->type) ) return;

			// expression unification calls type unification (mutual recursion)
			result = unifyExact( e1->type, e2so->type, tenv, need, have, open, widen );
		}
	}

	UnifyExpr( const ast::Expr * e2, ast::TypeEnvironment & env, ast::AssertionSet & need,
		ast::AssertionSet & have, const ast::OpenVarSet & open, WidenMode widen )
	: e2( e2 ), tenv(env), need(need), have(have), open(open), widen(widen), result(false) {}
};

static bool unify( const ast::Expr * e1, const ast::Expr * e2, ast::TypeEnvironment & env,
	ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open,
	WidenMode widen ) {
	assert( e1 && e2 );
	return ast::Pass<UnifyExpr>::read( e1, e2, env, need, have, open, widen );
}

class Unify final : public ast::WithShortCircuiting {
	const ast::Type * type2;
	ast::TypeEnvironment & tenv;
	ast::AssertionSet & need;
	ast::AssertionSet & have;
	const ast::OpenVarSet & open;
	WidenMode widen;
public:
	static size_t traceId;
	bool result;

	Unify(
		const ast::Type * type2, ast::TypeEnvironment & env, ast::AssertionSet & need,
		ast::AssertionSet & have, const ast::OpenVarSet & open, WidenMode widen )
	: type2(type2), tenv(env), need(need), have(have), open(open), widen(widen),
	result(false) {}

	void previsit( const ast::Node * ) { visit_children = false; }

	void postvisit( const ast::VoidType * vt) {
		result = dynamic_cast< const ast::VoidType * >( type2 )
			|| tryToUnifyWithEnumValue(vt, type2, tenv, need, have, open, noWiden());
		;
	}

	void postvisit( const ast::BasicType * basic ) {
		if ( auto basic2 = dynamic_cast< const ast::BasicType * >( type2 ) ) {
			result = basic->kind == basic2->kind;
		}
		result = result || tryToUnifyWithEnumValue(basic, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::PointerType * pointer ) {
		if ( auto pointer2 = dynamic_cast< const ast::PointerType * >( type2 ) ) {
			result = unifyExact(
				pointer->base, pointer2->base, tenv, need, have, open,
				noWiden());
		}
		result = result || tryToUnifyWithEnumValue(pointer, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::ArrayType * array ) {
		auto array2 = dynamic_cast< const ast::ArrayType * >( type2 );
		if ( !array2 ) return;

		if ( array->isVarLen != array2->isVarLen ) return;
		if ( (array->dimension != nullptr) != (array2->dimension != nullptr) ) return;

		if ( array->dimension ) {
			assert( array2->dimension );
			// type unification calls expression unification (mutual recursion)
			if ( !unify(array->dimension, array2->dimension,
				tenv, need, have, open, widen) ) return;
		}

		result = unifyExact(
			array->base, array2->base, tenv, need, have, open, noWiden())
			|| tryToUnifyWithEnumValue(array, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::ReferenceType * ref ) {
		if ( auto ref2 = dynamic_cast< const ast::ReferenceType * >( type2 ) ) {
			result = unifyExact(
				ref->base, ref2->base, tenv, need, have, open, noWiden());
		}
	}

private:

	template< typename Iter >
	static bool unifyTypeList(
		Iter crnt1, Iter end1, Iter crnt2, Iter end2, ast::TypeEnvironment & env,
		ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open
	) {
		while ( crnt1 != end1 && crnt2 != end2 ) {
			const ast::Type * t1 = *crnt1;
			const ast::Type * t2 = *crnt2;
			bool isTuple1 = Tuples::isTtype( t1 );
			bool isTuple2 = Tuples::isTtype( t2 );

			// assumes here that ttype *must* be last parameter
			if ( isTuple1 && !isTuple2 ) {
				// combine remainder of list2, then unify
				return unifyExact(
					t1, tupleFromTypes( crnt2, end2 ), env, need, have, open,
					noWiden() );
			} else if ( !isTuple1 && isTuple2 ) {
				// combine remainder of list1, then unify
				return unifyExact(
					tupleFromTypes( crnt1, end1 ), t2, env, need, have, open,
					noWiden() );
			}

			if ( !unifyExact(
				t1, t2, env, need, have, open, noWiden() )
			) return false;

			++crnt1; ++crnt2;
		}

		// May get to the end of one argument list before the other. This is only okay if the
		// other is a ttype
		if ( crnt1 != end1 ) {
			// try unifying empty tuple with ttype
			const ast::Type * t1 = *crnt1;
			if ( !Tuples::isTtype( t1 ) ) return false;
			return unifyExact(
				t1, tupleFromTypes( crnt2, end2 ), env, need, have, open,
				noWiden() );
		} else if ( crnt2 != end2 ) {
			// try unifying empty tuple with ttype
			const ast::Type * t2 = *crnt2;
			if ( !Tuples::isTtype( t2 ) ) return false;
			return unifyExact(
				tupleFromTypes( crnt1, end1 ), t2, env, need, have, open,
				noWiden() );
		}

		return true;
	}

	static bool unifyTypeList(
		const std::vector< ast::ptr< ast::Type > > & list1,
		const std::vector< ast::ptr< ast::Type > > & list2,
		ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
		const ast::OpenVarSet & open
	) {
		return unifyTypeList(
			list1.begin(), list1.end(), list2.begin(), list2.end(), env, need, have, open);
	}

	static void markAssertionSet( ast::AssertionSet & assns, const ast::VariableExpr * assn ) {
		auto i = assns.find( assn );
		if ( i != assns.end() ) {
			i->second.isUsed = true;
		}
	}

	/// mark all assertions in `type` used in both `assn1` and `assn2`
	static void markAssertions(
		ast::AssertionSet & assn1, ast::AssertionSet & assn2,
		const ast::FunctionType * type
	) {
		for ( auto & assert : type->assertions ) {
			markAssertionSet( assn1, assert );
			markAssertionSet( assn2, assert );
		}
	}

	bool tryToUnifyWithEnumValue( const ast::Type * type1, const ast::Type * type2, ast::TypeEnvironment & env,
		ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open,
		WidenMode widen) {
		if ( auto attrType2 = dynamic_cast<const ast::EnumAttrType *>(type2)) {
			if (attrType2->attr == ast::EnumAttribute::Value) {
				return unifyExact( type1, attrType2->instance->base->base, env, need, have, open,
					widen);
			} else if (attrType2->attr == ast::EnumAttribute::Posn) {
				return unifyExact( type1, attrType2->instance, env, need, have, open, widen );
			}
		}
		return false;
	}

public:
	void postvisit( const ast::FunctionType * func ) {
		auto func2 = dynamic_cast< const ast::FunctionType * >( type2 );
		if ( !func2 ) return;

		if ( func->isVarArgs != func2->isVarArgs ) return;

		// Flatten the parameter lists for both functions so that tuple structure does not
		// affect unification. Does not actually mutate function parameters.
		auto params = flattenList( func->params, tenv );
		auto params2 = flattenList( func2->params, tenv );

		// sizes don't have to match if ttypes are involved; need to be more precise w.r.t.
		// where the ttype is to prevent errors
		if (
			( params.size() != params2.size() || func->returns.size() != func2->returns.size() )
			&& !func->isTtype()
			&& !func2->isTtype()
		) return;

		if ( !unifyTypeList( params, params2, tenv, need, have, open ) ) return;
		if ( !unifyTypeList(
			func->returns, func2->returns, tenv, need, have, open ) ) return;

		markAssertions( have, need, func );
		markAssertions( have, need, func2 );

		result = true;
	}

private:
	// Returns: other, cast as XInstType
	// Assigns this->result: whether types are compatible (up to generic parameters)
	template< typename XInstType >
	const XInstType * handleRefType( const XInstType * inst, const ast::Type * other ) {
		// check that the other type is compatible and named the same
		auto otherInst = dynamic_cast< const XInstType * >( other );
		if ( otherInst && inst->name == otherInst->name ) {
			this->result = otherInst;
		}
		return otherInst;
	}

	/// Creates a tuple type based on a list of TypeExpr
	template< typename Iter >
	static const ast::Type * tupleFromExprs(
		const ast::TypeExpr * param, Iter & crnt, Iter end, ast::CV::Qualifiers qs
	) {
		std::vector< ast::ptr< ast::Type > > types;
		do {
			types.emplace_back( param->type );

			++crnt;
			if ( crnt == end ) break;
			param = strict_dynamic_cast< const ast::TypeExpr * >( crnt->get() );
		} while(true);

		return new ast::TupleType( std::move(types), qs );
	}

	template< typename XInstType >
	void handleGenericRefType( const XInstType * inst, const ast::Type * other ) {
		// check that other type is compatible and named the same
		const XInstType * otherInst = handleRefType( inst, other );
		if ( !this->result ) return;

		// check that parameters of types unify, if any
		const std::vector< ast::ptr< ast::Expr > > & params = inst->params;
		const std::vector< ast::ptr< ast::Expr > > & params2 = otherInst->params;

		auto it = params.begin();
		auto jt = params2.begin();
		for ( ; it != params.end() && jt != params2.end(); ++it, ++jt ) {
			auto param = strict_dynamic_cast< const ast::TypeExpr * >( it->get() );
			auto param2 = strict_dynamic_cast< const ast::TypeExpr * >( jt->get() );

			ast::ptr< ast::Type > pty = param->type;
			ast::ptr< ast::Type > pty2 = param2->type;

			bool isTuple = Tuples::isTtype( pty );
			bool isTuple2 = Tuples::isTtype( pty2 );

			if ( isTuple && isTuple2 ) {
				++it; ++jt;  // skip ttype parameters before break
			} else if ( isTuple ) {
				// bundle remaining params into tuple
				pty2 = tupleFromExprs( param2, jt, params2.end(), pty->qualifiers );
				++it;  // skip ttype parameter for break
			} else if ( isTuple2 ) {
				// bundle remaining params into tuple
				pty = tupleFromExprs( param, it, params.end(), pty2->qualifiers );
				++jt;  // skip ttype parameter for break
			}

			if ( !unifyExact(
					pty, pty2, tenv, need, have, open, noWiden() ) ) {
				result = false;
				return;
			}

			// ttype parameter should be last
			if ( isTuple || isTuple2 ) break;
		}
		result = it == params.end() && jt == params2.end();
	}

public:
	void postvisit( const ast::StructInstType * aggrType ) {
		handleGenericRefType( aggrType, type2 );
		result = result || tryToUnifyWithEnumValue(aggrType, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::UnionInstType * aggrType ) {
		handleGenericRefType( aggrType, type2 );
		result = result || tryToUnifyWithEnumValue(aggrType, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::EnumInstType * aggrType ) {
		handleRefType( aggrType, type2 );
		result = result || tryToUnifyWithEnumValue(aggrType, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::EnumAttrType * enumAttr ) {
		// Lazy approach for now
		if ( auto otherPos = dynamic_cast< const ast::EnumAttrType *>( type2 ) ) {
			if ( enumAttr->match(otherPos) ) {
				result = otherPos;
			}
		}
	}

	void postvisit( const ast::TraitInstType * aggrType ) {
		handleRefType( aggrType, type2 );
		result = result || tryToUnifyWithEnumValue(aggrType, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::TypeInstType * typeInst ) {
		// assert( open.find( *typeInst ) == open.end() );
		auto otherInst = dynamic_cast< const ast::TypeInstType * >( type2 );
		if ( otherInst && typeInst->name == otherInst->name ) {
			this->result = otherInst;
		}
		result = result || tryToUnifyWithEnumValue(typeInst, type2, tenv, need, have, open, noWiden());
	}

private:
	/// Creates a tuple type based on a list of Type
	static bool unifyList(
		const std::vector< ast::ptr< ast::Type > > & list1,
		const std::vector< ast::ptr< ast::Type > > & list2, ast::TypeEnvironment & env,
		ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open
	) {
		auto crnt1 = list1.begin();
		auto crnt2 = list2.begin();
		while ( crnt1 != list1.end() && crnt2 != list2.end() ) {
			const ast::Type * t1 = *crnt1;
			const ast::Type * t2 = *crnt2;
			bool isTuple1 = Tuples::isTtype( t1 );
			bool isTuple2 = Tuples::isTtype( t2 );

			// assumes ttype must be last parameter
			if ( isTuple1 && !isTuple2 ) {
				// combine entirety of list2, then unify
				return unifyExact(
					t1, tupleFromTypes( list2 ), env, need, have, open,
					noWiden() );
			} else if ( !isTuple1 && isTuple2 ) {
				// combine entirety of list1, then unify
				return unifyExact(
					tupleFromTypes( list1 ), t2, env, need, have, open,
					noWiden() );
			}

			if ( !unifyExact(
				t1, t2, env, need, have, open, noWiden() )
			) return false;

			++crnt1; ++crnt2;
		}

		if ( crnt1 != list1.end() ) {
			// try unifying empty tuple type with ttype
			const ast::Type * t1 = *crnt1;
			if ( !Tuples::isTtype( t1 ) ) return false;
			// xxx - this doesn't generate an empty tuple, contrary to comment; both ported
			// from Rob's code
			return unifyExact(
					t1, tupleFromTypes( list2 ), env, need, have, open,
					noWiden() );
		} else if ( crnt2 != list2.end() ) {
			// try unifying empty tuple with ttype
			const ast::Type * t2 = *crnt2;
			if ( !Tuples::isTtype( t2 ) ) return false;
			// xxx - this doesn't generate an empty tuple, contrary to comment; both ported
			// from Rob's code
			return unifyExact(
					tupleFromTypes( list1 ), t2, env, need, have, open,
					noWiden() );
		}

		return true;
	}

public:
	void postvisit( const ast::TupleType * tuple ) {
		auto tuple2 = dynamic_cast< const ast::TupleType * >( type2 );
		if ( ! tuple2 ) return;

		ast::Pass<TtypeExpander> expander{ tenv };

		const ast::Type * flat = tuple->accept( expander );
		const ast::Type * flat2 = tuple2->accept( expander );

		auto types = flatten( flat );
		auto types2 = flatten( flat2 );

		result = unifyList( types, types2, tenv, need, have, open )
			|| tryToUnifyWithEnumValue(tuple, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::VarArgsType * vat) {
		result = dynamic_cast< const ast::VarArgsType * >( type2 )
			|| tryToUnifyWithEnumValue(vat, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::ZeroType * zt) {
		result = dynamic_cast< const ast::ZeroType * >( type2 )
			|| tryToUnifyWithEnumValue(zt, type2, tenv, need, have, open, noWiden());
	}

	void postvisit( const ast::OneType * ot) {
		result = dynamic_cast< const ast::OneType * >( type2 )
			|| tryToUnifyWithEnumValue(ot, type2, tenv, need, have, open, noWiden());
	}
};

// size_t Unify::traceId = Stats::Heap::new_stacktrace_id("Unify");

bool unify(
		const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2,
		ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
		ast::OpenVarSet & open
) {
	ast::ptr<ast::Type> common;
	return unify( type1, type2, env, need, have, open, common );
}

bool unify(
		const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2,
		ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
		ast::OpenVarSet & open, ast::ptr<ast::Type> & common
) {
	ast::OpenVarSet closed;
	// findOpenVars( type1, open, closed, need, have, FirstClosed );
	findOpenVars( type2, open, closed, need, have, env, FirstOpen );
	return unifyInexact(
		type1, type2, env, need, have, open, WidenMode{ true, true }, common );
}

bool unifyExact(
		const ast::Type * type1, const ast::Type * type2, ast::TypeEnvironment & env,
		ast::AssertionSet & need, ast::AssertionSet & have, const ast::OpenVarSet & open,
		WidenMode widen
) {
	if ( type1->qualifiers != type2->qualifiers ) return false;

	auto var1 = dynamic_cast< const ast::TypeInstType * >( type1 );
	auto var2 = dynamic_cast< const ast::TypeInstType * >( type2 );
	bool isopen1 = var1 && env.lookup(*var1);
	bool isopen2 = var2 && env.lookup(*var2);

	if ( isopen1 && isopen2 ) {
		if ( var1->base->kind != var2->base->kind ) return false;
		return env.bindVarToVar(
			var1, var2, ast::TypeData{ var1->base->kind, var1->base->sized||var2->base->sized }, need, have,
			open, widen );
	} else if ( isopen1 ) {
		return env.bindVar( var1, type2, ast::TypeData{var1->base}, need, have, open, widen );
	} else if ( isopen2 ) {
		return env.bindVar( var2, type1, ast::TypeData{var2->base}, need, have, open, widen );
	} else {
		return ast::Pass<Unify>::read(
			type1, type2, env, need, have, open, widen );
	}
}

bool unifyInexact(
		const ast::ptr<ast::Type> & type1, const ast::ptr<ast::Type> & type2,
		ast::TypeEnvironment & env, ast::AssertionSet & need, ast::AssertionSet & have,
		const ast::OpenVarSet & open, WidenMode widen,
		ast::ptr<ast::Type> & common
) {
	ast::CV::Qualifiers q1 = type1->qualifiers, q2 = type2->qualifiers;

	// force t1 and t2 to be cloned if their qualifiers must be stripped, so that type1 and
	// type2 are left unchanged; calling convention forces type{1,2}->strong_ref >= 1
	ast::Type * t1 = shallowCopy(type1.get());
	ast::Type * t2 = shallowCopy(type2.get());
	t1->qualifiers = {};
	t2->qualifiers = {};
	ast::ptr< ast::Type > t1_(t1);
	ast::ptr< ast::Type > t2_(t2);

	if ( unifyExact( t1, t2, env, need, have, open, widen ) ) {
		// if exact unification on unqualified types, try to merge qualifiers
		if ( q1 == q2 || ( ( q1 > q2 || widen.first ) && ( q2 > q1 || widen.second ) ) ) {
			t1->qualifiers = q1 | q2;
			common = t1;
			return true;
		} else {
			return false;
		}
	} else if (( common = commonType( t1, t2, env, need, have, open, widen ))) {
		// no exact unification, but common type
		auto c = shallowCopy(common.get());
		c->qualifiers = q1 | q2;
		common = c;
		return true;
	} else {
		return false;
	}
}

ast::ptr<ast::Type> extractResultType( const ast::FunctionType * func ) {
	if ( func->returns.empty() ) return new ast::VoidType();
	if ( func->returns.size() == 1 ) return func->returns[0];

	std::vector<ast::ptr<ast::Type>> tys;
	for ( const auto & decl : func->returns ) {
		tys.emplace_back( decl );
	}
	return new ast::TupleType( std::move(tys) );
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
