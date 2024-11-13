//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Lvalue.cpp -- Clean up lvalues and remove references.
//
// Author           : Andrew Beach
// Created On       : Thu Sep 15 14:08:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Aug 12 18:07:00 2024
// Update Count     : 1
//

#include "Lvalue.hpp"

#include <set>
#include <iostream>

#include "AST/Copy.hpp"                // for deepCopy
#include "AST/Expr.hpp"
#include "AST/Inspect.hpp"
#include "AST/LinkageSpec.hpp"         // for Linkage
#include "AST/Pass.hpp"
#include "Common/SemanticError.hpp"    // for SemanticWarning
#include "Common/ToString.hpp"         // for toCString
#include "Common/UniqueName.hpp"       // for UniqueName
#include "GenPoly/GenPoly.hpp"         // for genFunctionType
#include "ResolvExpr/Typeops.hpp"      // for typesCompatible
#include "ResolvExpr/Unify.hpp"        // for unify

#if 0
#define PRINT(x) x
#else
#define PRINT(x)
#endif

namespace GenPoly {

namespace {

/// Intrinsic functions that return references now instead return lvalues.
struct FixIntrinsicResults final : public ast::WithGuards {
	enum {
		NoSkip,
		Skip,
		SkipInProgress,
	} skip = NoSkip;

	void previsit( ast::AsmExpr const * ) {
		GuardValue( skip ) = Skip;
	}
	void previsit( ast::ApplicationExpr const * ) {
		GuardValue( skip ) = (skip == Skip) ? SkipInProgress : NoSkip;
	}

	ast::Expr const * postvisit( ast::ApplicationExpr const * expr );
	void previsit( ast::FunctionDecl const * decl );
	bool inIntrinsic = false;
};

/// Add de-references around address-of operations on reference types.
struct AddressRef final :
		public ast::WithConstTranslationUnit,
		public ast::WithGuards,
		public ast::WithShortCircuiting,
		public ast::WithVisitorRef<AddressRef> {
	void previsit( ast::AddressExpr const * expr );
	ast::Expr const * postvisit( ast::AddressExpr const * expr );
	void previsit( ast::Expr const * expr );
	ast::ApplicationExpr const * previsit( ast::ApplicationExpr const * expr );
	void previsit( ast::SingleInit const * init );

	void handleNonAddr( ast::Expr const * expr );

	bool first = true;
	bool current = false;
	bool addCast = false;
	int refDepth = 0;
};

/// Handles casts between references and pointers,
/// creating temporaries for the conversion.
struct ReferenceConversions final :
		public ast::WithConstTranslationUnit,
		public ast::WithGuards, public ast::WithStmtsToAdd<> {
	ast::Expr const * postvisit( ast::CastExpr const * expr );
	ast::Expr const * postvisit( ast::AddressExpr const * expr );
};

/// Intrinsic functions that take reference parameters don't actually do.
/// Their reference arguments must be implicity dereferenced.
/// TODO Also appears to contain redundent code with AddressRef
struct FixIntrinsicArgs final :
		public ast::WithConstTranslationUnit {
	ast::Expr const * postvisit( ast::ApplicationExpr const * expr );
};

/// Removes redundant &* / *& patterns that may be generated.
struct CollapseAddressDeref final {
	ast::Expr const * postvisit( ast::AddressExpr const * expr );
	ast::Expr const * postvisit( ast::ApplicationExpr const * expr );
};

/// GCC-like Generalized Lvalues (which have since been removed from GCC).
/// https://gcc.gnu.org/onlinedocs/gcc-3.4.6/gcc/Lvalues.html#Lvalues
/// Replaces &(a,b) with (a, &b), &(a ? b : c) with (a ? &b : &c)
struct GeneralizedLvalue final :
		public ast::WithVisitorRef<GeneralizedLvalue> {
	ast::Expr const * postvisit( ast::AddressExpr const * expr );
	ast::Expr const * postvisit( ast::MemberExpr const * expr );

	template<typename Node, typename Func>
	ast::Expr const * applyTransformation(
	      Node const * expr, ast::ptr<ast::Expr> Node::*field, Func mkExpr );
};

/// Replace all reference types with pointer types.
struct ReferenceTypeElimination final {
	ast::SizeofExpr const * previsit( ast::SizeofExpr const * expr );
	ast::AlignofExpr const * previsit( ast::AlignofExpr const * expr );
	ast::Type const * postvisit( ast::ReferenceType const * type );
};

/// True for intrinsic function calls that return an lvalue in C.
bool isIntrinsicReference( ast::Expr const * expr ) {
	// The known intrinsic-reference prelude functions.
	static std::set<std::string> const lvalueFunctions = { "*?", "?[?]" };
	if ( auto untyped = dynamic_cast<ast::UntypedExpr const *>( expr ) ) {
		std::string fname = ast::getFunctionName( untyped );
		return lvalueFunctions.count( fname );
	} else if ( auto app = dynamic_cast<ast::ApplicationExpr const *>( expr ) ) {
		if ( auto func = ast::getFunction( app ) ) {
			return func->linkage == ast::Linkage::Intrinsic
				&& lvalueFunctions.count( func->name );
		}
	}
	return false;
}

// A maybe typed variant of the createDeref function (only UntypedExpr).
ast::Expr * mkDeref(
		ast::TranslationGlobal const & global, ast::Expr const * arg ) {
	if ( global.dereference ) {
		// Note: Reference depth can be arbitrarily deep here,
		// so peel off the outermost pointer/reference, not just
		// pointer because they are effecitvely equivalent in this pass
		ast::VariableExpr * deref = new ast::VariableExpr(
			arg->location, global.dereference );
		deref->result = new ast::PointerType( deref->result );
		ast::Type const * base = ast::getPointerBase( arg->result );
		assertf( base, "expected pointer type in dereference (type was %s)", toString( arg->result ).c_str() );
		ast::ApplicationExpr * ret =
			new ast::ApplicationExpr( arg->location, deref, { arg } );
		ret->result = ast::deepCopy( base );
		return ret;
	} else {
		return ast::UntypedExpr::createDeref( arg->location, arg );
	}
}

ast::Expr const * FixIntrinsicResults::postvisit(
		ast::ApplicationExpr const * expr ) {

	if ( skip == SkipInProgress || !isIntrinsicReference( expr ) ) {
		return expr;
	}
	// Eliminate reference types from intrinsic applications
	// now they return lvalues.
	ast::ptr<ast::ReferenceType> result =
			expr->result.strict_as<ast::ReferenceType>();
	expr = ast::mutate_field( expr, &ast::ApplicationExpr::result,
			ast::deepCopy( result->base ) );
	if ( inIntrinsic ) {
		return expr;
	}
	// When not in an intrinsic function, add a cast to don't add cast when
	// in an intrinsic function, since they already have the cast.
	auto * ret = new ast::CastExpr( expr->location, expr, result.get() );
	ret->env = expr->env;
	return ret;
}

void FixIntrinsicResults::previsit( ast::FunctionDecl const * decl ) {
	GuardValue( inIntrinsic ) = decl->linkage == ast::Linkage::Intrinsic;
}

void AddressRef::previsit( ast::AddressExpr const * ) {
	// Is this the first address-of in the chain?
	GuardValue( current ) = first;
	// Later references will not be for next address-of to be first in chain.
	GuardValue( first ) = false;
	// If is the outermost address-of in a chain:
	if ( current ) {
		// Set depth to 0 so that postvisit can
		// find the innermost address-of easily.
		GuardValue( refDepth ) = 0;
	}
}

ast::Expr const * AddressRef::postvisit( ast::AddressExpr const * expr ) {
	PRINT( std::cerr << "addr ref at " << expr << std::endl; )
	if ( 0 == refDepth ) {
		PRINT( std::cerr << "depth 0, get new depth..." << std::endl; )
		// Is this the innermost address-of in a chain? record depth D.
		if ( isIntrinsicReference( expr->arg ) ) {
			assertf( false, "AddrRef : address-of should not have intrinsic reference argument: %s", toCString( expr->arg )  );
		} else {
			// try to avoid ?[?]
			// TODO is this condition still necessary? intrinsicReferences
			// should have a cast around them at this point, so I don't think
			// this condition ever fires.
			refDepth = expr->arg->result->referenceDepth();
			PRINT( std::cerr << "arg not intrinsic reference, new depth is: " << refDepth << std::endl; )
		}
	}
	if ( current ) {
		PRINT( std::cerr << "current, depth is: " << refDepth << std::endl; )
		ast::Expr const * ret = expr;
		while ( refDepth ) {
			// Add one dereference for each address-of in the chain.
			ret = mkDeref( transUnit().global, ret );
			--refDepth;
		}

		// if addrExpr depth is 0, then the result is a pointer because the
		// arg was depth 1 and not lvalue. This means the dereference result
		// is not a reference, is lvalue, and one less pointer depth than the
		// addrExpr. Thus the cast is meaningless.
		// TODO: One thing to double check is whether it is possible for the
		// types to differ outside of the single pointer level (i.e. can the
		// base type of addrExpr differ from the type of addrExpr-arg?). If
		// so then the cast might need to be added, conditional on a more
		// sophisticated check.
		if ( addCast && 0 != expr->result->referenceDepth() ) {
			PRINT( std::cerr << "adding cast to " << expr->result << std::endl; )
			return new ast::CastExpr( expr->location,
				ret, ast::deepCopy( expr->result ) );
		}
		return ret;
	}
	PRINT( std::cerr << "not current..." << std::endl; )
	return expr;
}

void AddressRef::previsit( ast::Expr const * expr ) {
	handleNonAddr( expr );
	GuardValue( addCast ) = false;
}

// So we want to skip traversing to the head?
ast::ApplicationExpr const * AddressRef::previsit(
		ast::ApplicationExpr const * expr ) {
	visit_children = false;
	GuardValue( addCast );
	handleNonAddr( expr );
	auto mutExpr = ast::mutate( expr );
	for ( ast::ptr<ast::Expr> & arg : mutExpr->args ) {
		addCast = true;
		arg = arg->accept( *visitor );
	}
	return mutExpr;
}

void AddressRef::previsit( ast::SingleInit const * ) {
	// Each initialization context with address-of requires a cast.
	GuardValue( addCast ) = true;
}

// idea: &&&E: get outer &, inner &
// at inner &, record depth D of reference type of argument of &.
// at auter &, add D derefs.
void AddressRef::handleNonAddr( ast::Expr const * ) {
	// non-address-of: reset status variables:
	// * current expr is NOT the first address-of expr in an address-of chain.
	// * next seen address-of expr IS the first in the chain.
	GuardValue( current ) = false;
	GuardValue( first ) = true;
}

ast::Expr const * ReferenceConversions::postvisit(
		ast::CastExpr const * expr ) {
	// TODO: Is it possible to convert directly between reference types with
	// a different base. e.g.
	//   int x;
	//   (double&)x;
	// At the moment, I (who?) am working off of the assumption that this is
	// illegal, thus the cast becomes redundant after this pass, so trash the
	// cast altogether. If that changes, care must be taken to insert the
	// correct pointer casts in the right places.

	// Note: reference depth difference is the determining factor in what
	// code is run, rather than whether something is reference type or not,
	// since conversion still needs to occur when both types are references
	// that differ in depth.
	ast::Type const * dstType = expr->result.get();
	ast::Type const * srcType = expr->arg->result.get();
	assertf( dstType, "Cast to no type in: %s", toCString( expr ) );
	assertf( srcType, "Cast from no type in: %s", toCString( expr ) );
	int dstDepth = dstType->referenceDepth();
	int srcDepth = srcType->referenceDepth();
	int diff = dstDepth - srcDepth;

	if ( 0 < diff && !expr->arg->get_lvalue() ) {
		// rvalue to reference conversion -- introduce temporary
		// know that reference depth of cast argument is 0
		//   (int &&&)3;
		// becomes
		//   int __ref_tmp_0 = 3;
		//   int & __ref_tmp_1 = &__ref_tmp_0;
		//   int && __ref_tmp_2 = &__ref_tmp_1;
		//   &__ref_tmp_2;
		// The last & comes from the remaining reference conversion code.
		SemanticWarning( expr->arg->location,
			Warning::RvalueToReferenceConversion, toCString( expr->arg ) );


		// allowing conversion in the rvalue to const ref case
		// use the referenced-to type to create temp variables
		ast::Type const * targetType = dstType;
		for (int i = 0; i < diff; ++i) targetType = (strict_dynamic_cast<ast::ReferenceType const *>(targetType))->base;

		static UniqueName tmpNamer( "__ref_tmp_" );
		ast::ObjectDecl * tmp = new ast::ObjectDecl( expr->arg->location,
			tmpNamer.newName(),
			// ast::deepCopy( expr->arg->result ),
			ast::deepCopy (targetType),
			new ast::SingleInit( expr->arg->location, expr->arg ) );
		PRINT( std::cerr << "make tmp: " << tmp << std::endl; )
		stmtsToAddBefore.push_back( new ast::DeclStmt( tmp->location, tmp ) );
		for ( int i = 0 ; i < dstDepth - 1 ; ++i ) {
			ast::ObjectDecl * newTmp = new ast::ObjectDecl( tmp->location,
				tmpNamer.newName(),
				new ast::ReferenceType( ast::deepCopy( tmp->type ) ),
				new ast::SingleInit( tmp->location,
					new ast::AddressExpr( tmp->location,
						new ast::VariableExpr( tmp->location, tmp ) ) ) );
			PRINT( std::cerr << "make tmp: " << i << ": " << newTmp << std::endl; )
			stmtsToAddBefore.push_back(
				new ast::DeclStmt( newTmp->location, newTmp ) );
			tmp = newTmp;
		}
		// Update diff so that remaining code works out correctly.
		expr = ast::mutate_field( expr, &ast::CastExpr::arg,
			new ast::VariableExpr( tmp->location, tmp ) );
		PRINT( std::cerr << "update cast to: " << expr << std::endl; )
		srcType = expr->arg->result;
		srcDepth = srcType->referenceDepth();
		diff = dstDepth - srcDepth;
		assert( 1 == diff );
	}

	// Handle conversion between different depths.
	PRINT(
		if ( dstDepth || srcDepth ) {
			std::cerr << "dstType: " << dstType << " / srcType: " << srcType << '\n';
			std::cerr << "depth: " << dstDepth << " / " << srcDepth << std::endl;
		}
	)
	// Conversion to type with more depth/more references.
	// Add address-of for each level of difference.
	if ( 0 < diff ) {
		ast::Expr * ret = ast::mutate( expr->arg.get() );
		for ( int i = 0 ; i < diff ; ++i ) {
			ret = new ast::AddressExpr( ret->location, ret );
		}
		if ( expr->arg->get_lvalue() &&
				!ResolvExpr::typesCompatible(
					srcType,
					strict_dynamic_cast<ast::ReferenceType const *>( dstType )->base ) ) {
			// Must keep cast if cast-to type is different from the actual type.
			return ast::mutate_field( expr, &ast::CastExpr::arg, ret );
		}
		ret->env = expr->env;
		ret->result = expr->result;
		return ret;
	// Conversion to type with less depth/fewer references.
	// Add dereferences for each level of difference.
	} else if ( diff < 0 ) {
		ast::Expr * ret = ast::mutate( expr->arg.get() );
		for ( int i = 0 ; i < -diff ; ++i ) {
			ret = mkDeref( transUnit().global, ret );
		}
		// Must keep cast if types are different.
		if ( !ResolvExpr::typesCompatibleIgnoreQualifiers(
				dstType->stripReferences(),
				srcType->stripReferences() ) ) {
			return ast::mutate_field( expr, &ast::CastExpr::arg, ret );
		}
		ret->env = expr->env;
		ret->result = expr->result;
		// The result must be an lvalue.
		assert( ret->get_lvalue() );
		return ret;
	// Conversion with the same depth.
	} else {
		assert( 0 == diff );
		// Remove useless generated casts.
		if ( expr->isGenerated == ast::GeneratedFlag::GeneratedCast &&
				ResolvExpr::typesCompatible(
					expr->result,
					expr->arg->result ) ) {
			PRINT(
				std::cerr << "types are compatible, removing cast: " << expr << '\n';
				std::cerr << "-- " << expr->result << '\n';
				std::cerr << "-- " << expr->arg->result << std::endl;
			)
			auto argAsEnum = expr->arg.as<ast::EnumInstType>();
			auto resultAsEnum = expr->result.as<ast::EnumInstType>();
			if (argAsEnum && resultAsEnum) {
				if (argAsEnum->base->name != resultAsEnum->base->name) {
					return expr;
				}
			}
			return ast::mutate_field( expr->arg.get(),
					&ast::Expr::env, expr->env.get() );
		}
		return expr;
	}
}

ast::Expr const * ReferenceConversions::postvisit(
		ast::AddressExpr const * expr ) {
	// Inner expression may have been lvalue to reference conversion, which
	// becomes an address expression. In this case, remove the outer address
	// expression and return the argument.
	// TODO: It's possible that this might catch too much and require a more
	// sophisticated check. TODO What check are we talking about here?
	return expr;
}

ast::Expr const * FixIntrinsicArgs::postvisit(
		ast::ApplicationExpr const * expr ) {
	// Intrinsic functions don't really take reference-typed parameters,
	// so they require an implicit dereference on their arguments.
	auto function = ast::getFunction( expr );
	if ( function == nullptr ) {
		return expr;
	}

	ast::FunctionType const * ftype = GenPoly::getFunctionType( function->get_type() );
	assertf( ftype, "Function declaration does not have function type." );
	// Can be of different lengths only when function is variadic.
	assertf( ftype->params.size() == expr->args.size() || ftype->isVarArgs,
		"ApplicationExpr args do not match formal parameter type." );
	assertf( ftype->params.size() <= expr->args.size(),
		"Cannot have more parameters than arguments." );

	unsigned int i = 0;
	unsigned int const end = ftype->params.size();

	// This is used to make sure we get a zip on shortests.
	if ( end == i ) return expr;

	// This mutate could be redundent, but it is simpler this way.
	auto mutExpr = ast::mutate( expr );

	for ( auto pair : unsafe_group_iterate( mutExpr->args, ftype->params ) ) {
		ast::ptr<ast::Expr> & arg = std::get<0>( pair );
		ast::ptr<ast::Type> const & formal = std::get<1>( pair );
		PRINT(
			std::cerr << "pair<0>: " << arg.get() << std::endl;
			std::cerr << " -- " << arg->result << std::endl;
			std::cerr << "pair<1>: " << formal << std::endl;
		)
		//if ( dynamic_cast<ast::ReferenceType const *>( formal.get() ) ) {
		if ( formal.as<ast::ReferenceType>() ) {
			PRINT( std::cerr << "===formal is reference" << std::endl; )
			// TODO: It's likely that the second condition should be
			// `... && ! isIntrinsicReference( arg )`, but this requires
			// investigation.

			if ( ast::Linkage::Intrinsic != function->linkage
					&& isIntrinsicReference( arg ) ) {
				// Needed for definition of prelude functions, etc.
				// If argument is dereference or array subscript, the result
				// isn't REALLY a reference, but non-intrinsic functions
				// expect a reference: take address

				// TODO: OK, so this should be cut?!
				// NODE: Previously, this condition fixed
				//   void f(int *&);
				//   int & x = ...;
				//   f(&x);
				// But now this is taken care of by a reference cast added by
				// AddressRef. Need to find a new example or remove this
				// branch.
				PRINT(
					std::cerr << "===is intrinsic arg in non-intrinsic call - adding address" << std::endl;
				)
				arg = new ast::AddressExpr( arg->location, arg );
			} else if ( ast::Linkage::Intrinsic == function->linkage
					&& arg->result->referenceDepth() != 0 ) {
				// Argument is a 'real' reference, but function expects a C
				// lvalue: Add a dereference to the reference-typed argument.
				PRINT(
					std::cerr << "===is non-intrinsic arg in intrinsic call - adding deref to arg" << std::endl;
				)
				ast::Type const * base = ast::getPointerBase( arg->result );
				assertf( base, "parameter is reference, arg must be pointer or reference: %s", toString( arg->result ).c_str() );
				ast::PointerType * ptr = new ast::PointerType( ast::deepCopy( base ) );
				arg = ast::mutate_field( arg.get(),
						&ast::ApplicationExpr::result, ptr );
				arg = mkDeref( transUnit().global, arg );
			}
		}
		++i;
		if ( end == i ) break;
	}
	return mutExpr;
}

ast::Expr const * CollapseAddressDeref::postvisit(
		ast::AddressExpr const * expr ) {
	ast::Expr const * arg = expr->arg;
	if ( isIntrinsicReference( arg ) ) {
		std::string fname = ast::getFunctionName( arg );
		if ( fname == "*?" ) {
			ast::Expr const * arg0 = ast::getCallArg( arg, 0 );
			ast::Expr * ret = ast::mutate( arg0 );
			ret->env = expr->env;
			return ret;
		}
	} else if ( auto cast = dynamic_cast<ast::CastExpr const *>( arg ) ) {
		// Need to move cast to pointer type out a level since address of
		// pointer is not valid C code (can be introduced in prior passes,
		// e.g., InstantiateGeneric)
		if ( ast::getPointerBase( cast->result ) ) {
			auto mutExpr = ast::mutate( expr );
			auto mutCast = strict_dynamic_cast<ast::CastExpr *>(
					ast::mutate( mutExpr->arg.release() ) );
			mutExpr->arg = mutCast->arg;
			mutCast->arg = mutExpr;
			mutCast->result = new ast::PointerType( mutCast->result );
			return mutCast;
		}
	}
	return expr;
}

ast::Expr const * CollapseAddressDeref::postvisit(
		ast::ApplicationExpr const * expr ) {
	if ( isIntrinsicReference( expr ) ) {
		std::string fname = ast::getFunctionName( expr );
		if ( fname == "*?" ) {
			assert( 1 == expr->args.size() );
			ast::Expr const * arg = ast::getCallArg( expr, 0 );
			// xxx - this isn't right, because it can remove casts that
			// should be there...
			//	while ( auto cast = dynamic_cast< ast::CastExpr const * >( arg ) ) {
			//		arg = cast->arg;
			//	}
			if ( auto addr = dynamic_cast<ast::AddressExpr const *>( arg ) ) {
				return ast::mutate_field( addr->arg.get(),
						&ast::Expr::env, expr->env.get() );
			}
		}
	}
	return expr;
}

ast::Expr const * GeneralizedLvalue::postvisit(
		ast::AddressExpr const * expr ) {
	return applyTransformation( expr, &ast::AddressExpr::arg,
		[]( ast::Expr const * arg ) {
			return new ast::AddressExpr( arg->location, arg );
		}
	);
}

ast::Expr const * GeneralizedLvalue::postvisit(
		ast::MemberExpr const * expr ) {
	return applyTransformation( expr, &ast::MemberExpr::aggregate,
		[expr]( ast::Expr const * aggr ) {
			return new ast::MemberExpr( aggr->location, expr->member, aggr );
		}
	);
}

template<typename Node, typename Func>
ast::Expr const * GeneralizedLvalue::applyTransformation(
		Node const * expr, ast::ptr<ast::Expr> Node::*field, Func mkExpr ) {
	ast::ptr<ast::Expr> const & arg = expr->*field;
	if ( auto commaArg = arg.as<ast::CommaExpr>() ) {
		ast::Expr const * arg1 = ast::deepCopy( commaArg->arg1 );
		ast::Expr const * arg2 = ast::deepCopy( commaArg->arg2 );
		ast::Expr const * ret = new ast::CommaExpr(
			commaArg->location, arg1, mkExpr( arg2 )->accept( *visitor ) );
		return ret;
	} else if ( auto condArg = arg.as<ast::ConditionalExpr>() ) {
		ast::Expr const * arg1 = ast::deepCopy( condArg->arg1 );
		ast::Expr const * arg2 = ast::deepCopy( condArg->arg2 );
		ast::Expr const * arg3 = ast::deepCopy( condArg->arg3 );
		ast::ConditionalExpr * ret = new ast::ConditionalExpr(
			condArg->location, arg1, mkExpr( arg2 )->accept( *visitor ),
			mkExpr( arg3 )->accept( *visitor ) );

		// Conditional expr type may not be either of the arguments,
		// so unify to get the result.
		// TODO: Maybe I could create a wrapper for this.
		ast::ptr<ast::Type> common = nullptr;
		ast::TypeEnvironment newEnv;
		ast::AssertionSet needAssertions, haveAssertions;
		ast::OpenVarSet openVars;
		ResolvExpr::unify( ret->arg2->result, ret->arg3->result, newEnv,
			needAssertions, haveAssertions, openVars, common );
		ret->result = common ? common : ast::deepCopy( ret->arg2->result );
		return ret;
	}
	return expr;
}

ast::SizeofExpr const * ReferenceTypeElimination::previsit(
		ast::SizeofExpr const * expr ) {
	return ast::mutate_field( expr, &ast::SizeofExpr::type,
		expr->type->stripReferences() );
}

ast::AlignofExpr const * ReferenceTypeElimination::previsit(
		ast::AlignofExpr const * expr ) {
	return ast::mutate_field( expr, &ast::AlignofExpr::type,
		expr->type->stripReferences() );
}

ast::Type const * ReferenceTypeElimination::postvisit(
		ast::ReferenceType const * type ) {
	return new ast::PointerType( type->base, type->qualifiers );
}

} // namespace

// Stored elsewhere (Lvalue2, initially false):
extern bool referencesEliminated;

void convertLvalue( ast::TranslationUnit & translationUnit ) {
	ast::Pass<FixIntrinsicResults>::run( translationUnit );
	ast::Pass<AddressRef>::run( translationUnit );
	ast::Pass<ReferenceConversions>::run( translationUnit );
	ast::Pass<FixIntrinsicArgs>::run( translationUnit );
	ast::Pass<CollapseAddressDeref>::run( translationUnit );
	ast::Pass<GeneralizedLvalue>::run( translationUnit );
	// Last because other passes need reference types to work.
	ast::Pass<ReferenceTypeElimination>::run( translationUnit );
	// From this point forward, nothing should create reference types.
	referencesEliminated = true;
}

ast::Expr const * generalizedLvalue( ast::Expr const * expr ) {
	ast::Pass<GeneralizedLvalue> visitor;
	return expr->accept( visitor );
}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
