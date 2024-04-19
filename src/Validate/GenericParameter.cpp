//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenericParameter.cpp -- Generic parameter related passes.
//
// Author           : Andrew Beach
// Created On       : Fri Mar 21 10:02:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue Sep 20 16:28:00 2022
// Update Count     : 2
//

#include "GenericParameter.hpp"

#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/ParseNode.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "AST/Type.hpp"
#include "Validate/NoIdSymbolTable.hpp"

namespace Validate {

namespace {

// Test for special name on a generic parameter.  Special treatment for the
// special name is a bootstrapping hack.  In most cases, the worlds of T's
// and of N's don't overlap (normal treamtemt).  The foundations in
// array.hfa use tagging for both types and dimensions.  Tagging treats
// its subject parameter even more opaquely than T&, which assumes it is
// possible to have a pointer/reference to such an object.  Tagging only
// seeks to identify the type-system resident at compile time.  Both N's
// and T's can make tags.  The tag definition uses the special name, which
// is treated as "an N or a T."  This feature is not inteded to be used
// outside of the definition and immediate uses of a tag.
inline bool isReservedTysysIdOnlyName( const std::string & name ) {
	// The name might be wrapped in __..._generic so check for that as well.
	int foundAt = name.find("__CFA_tysys_id_only");
	if (foundAt == 0) return true;
	if (foundAt == 2 && name[0] == '_' && name[1] == '_') return true;
	return false;
}

template< typename InstType >
const InstType * validateGeneric(
		const CodeLocation & location, const InstType * type ) {
	const typename InstType::base_type * base = type->base.get();
	if ( nullptr == base ) {
		return type;
	}

	const std::vector<ast::ptr<ast::TypeDecl>> & params = base->params;
	if ( params.empty() ) {
		return type;
	}

	// I think I can move this check up, or it should check the result of
	// the substuition.

	auto mutType = ast::mutate( type );
	std::vector<ast::ptr<ast::Expr>> & args = mutType->params;

	// Quick check before we get into the real work.
	if ( params.size() < args.size() ) {
		SemanticError( location, type, "Too many type arguments in generic type " );
	}

	// Insert defaults arguments when a type argument is missing (currently
	// only supports missing arguments at the end of the list).
	// A substitution is used to ensure that defaults are replaced correctly:
	//   forall(otype T, otype alloc = heap_allocator(T)) struct vector;
	//   vector(int) v;
	// After insertion of default values becomes:
	//   vector(int, heap_allocator(T))
	// The substitution is built with T=int so the result is:
	//   vector(int, heap_allocator(int))

	ast::TypeSubstitution sub;
	// Build the substution:
	auto paramIter = params.begin();
	auto argIter = args.begin();
	for ( ; paramIter != params.end() ; ++paramIter, ++argIter ) {
		if ( argIter != args.end() ) {
			if ( auto expr = argIter->as<ast::TypeExpr>() ) {
				sub.add( paramIter->get(), ast::deepCopy( expr->type ) );
			}
		} else if ( const ast::Type * defaultType = (*paramIter)->init ) {
			args.push_back( new ast::TypeExpr(
				location, ast::deepCopy( defaultType ) ) );
			sub.add( paramIter->get(), ast::deepCopy( defaultType ) );
			argIter = std::prev( args.end() );
		} else {
			SemanticError( location, type, "Too few type arguments in generic type " );
		}
		assert( argIter != args.end() );
		bool typeParamDeclared = (*paramIter)->kind != ast::TypeDecl::Dimension;
		bool typeArgGiven;
		if ( isReservedTysysIdOnlyName( (*paramIter)->name ) ) {
			// Always match when declaration is reserved name, means "either".
			typeArgGiven = typeParamDeclared;
		} else {
			typeArgGiven = argIter->as<ast::TypeExpr>();
		}
		if ( !typeParamDeclared && typeArgGiven ) {
			SemanticError( location, type, "Type argument given for value parameter: " );
		}
		if ( typeParamDeclared && !typeArgGiven ) {
			SemanticError( location, type, "Expression argument given for type parameter: " );
		}
	}

	// Actually do the application:
	auto result = sub.apply( mutType );
	return result.node.release();
}

bool isSizedPolymorphic( const ast::AggregateDecl * decl ) {
	for ( const auto & param : decl->params ) {
		if ( param->sized ) return true;
	}
	return false;
}

struct ValidateGenericParamsCore :
		public ast::WithCodeLocation, public ast::WithGuards {
	// Generic parameter filling and checks:
	const ast::StructInstType * previsit( const ast::StructInstType * type ) {
		assert( location );
		return validateGeneric( *location, type );
	}

	const ast::UnionInstType * previsit( const ast::UnionInstType * type ) {
		assert( location );
		return validateGeneric( *location, type );
	}

	// Check parameter and bitfield combinations:
	bool insideSized = false;
	void previsit( const ast::StructDecl * decl ) {
		if ( isSizedPolymorphic( decl ) && !insideSized ) {
			GuardValue( insideSized ) = true;
		}
	}

	void previsit( const ast::UnionDecl * decl ) {
		if ( isSizedPolymorphic( decl ) && !insideSized ) {
			GuardValue( insideSized ) = true;
		}
	}

	void previsit( const ast::ObjectDecl * decl ) {
		if ( insideSized && decl->bitfieldWidth ) {
			SemanticError( decl->location, decl,
				"Cannot have bitfields inside a sized polymorphic structure." );
		}
	}
};

// --------------------------------------------------------------------------

struct TranslateDimensionCore :
		public WithNoIdSymbolTable, public ast::WithGuards,
		public ast::WithVisitorRef<TranslateDimensionCore> {

	// SUIT: Struct- or Union- InstType
	// Situational awareness:
	// array( float, [[currentExpr]]     )  has  visitingChildOfSUIT == true
	// array( float, [[currentExpr]] - 1 )  has  visitingChildOfSUIT == false
	// size_t x =    [[currentExpr]]        has  visitingChildOfSUIT == false
	bool nextVisitedNodeIsChildOfSUIT = false;
	bool visitingChildOfSUIT = false;
	void changeState_ChildOfSUIT( bool newValue ) {
		GuardValue( visitingChildOfSUIT ) = nextVisitedNodeIsChildOfSUIT;
		GuardValue( nextVisitedNodeIsChildOfSUIT ) = newValue;
	}

	void previsit( const ast::StructInstType * ) {
		changeState_ChildOfSUIT( true );
	}
	void previsit( const ast::UnionInstType * ) {
		changeState_ChildOfSUIT( true );
	}
	void previsit( const ast::Node * ) {
		changeState_ChildOfSUIT( false );
	}

	const ast::TypeDecl * postvisit( const ast::TypeDecl * decl );
	const ast::Type * postvisit( const ast::FunctionType * type );
	const ast::Type * postvisit( const ast::TypeInstType * type );

	const ast::Expr * postvisit( const ast::DimensionExpr * expr );
	const ast::Expr * postvisit( const ast::Expr * expr );
	const ast::Expr * postvisit( const ast::TypeExpr * expr );
};

// Declaration of type variable: forall( [N] )  ->  forall( N & | sized( N ) )
const ast::TypeDecl * TranslateDimensionCore::postvisit(
		const ast::TypeDecl * decl ) {
	if ( decl->kind == ast::TypeDecl::Dimension ) {
		auto mutDecl = ast::mutate( decl );
		mutDecl->kind = ast::TypeDecl::Dtype;
		if ( !isReservedTysysIdOnlyName( mutDecl->name ) ) {
			mutDecl->sized = true;
		}
		return mutDecl;
	}
	return decl;
}

// Makes postvisit( TypeInstType ) get called on the entries of the function declaration's type's forall list.
// Pass.impl.hpp's visit( FunctionType ) does not consider the forall entries to be child nodes.
// Workaround is: during the current TranslateDimension pass, manually visit down there.
const ast::Type * TranslateDimensionCore::postvisit(
		const ast::FunctionType * type ) {
	visitor->maybe_accept( type, &ast::FunctionType::forall );
	return type;
}

// Use of type variable, assuming `forall( [N] )` in scope:  void (*)( foo( /*dimension*/ N ) & )  ->  void (*)( foo( /*dtype*/ N ) & )
const ast::Type * TranslateDimensionCore::postvisit(
		const ast::TypeInstType * type ) {
	if ( type->kind == ast::TypeDecl::Dimension ) {
		auto mutType = ast::mutate( type );
		mutType->kind = ast::TypeDecl::Dtype;
		return mutType;
	}
	return type;
}

// Passing values as dimension arguments:  array( float,     7 )  -> array( float, char[             7 ] )
// Consuming dimension parameters:         size_t x =    N - 1 ;  -> size_t x =          sizeof(N) - 1   ;
// Intertwined reality:                    array( float, N     )  -> array( float,              N        )
//                                         array( float, N - 1 )  -> array( float, char[ sizeof(N) - 1 ] )
// Intertwined case 1 is not just an optimization.
// Avoiding char[sizeof(-)] is necessary to enable the call of f to bind the value of N, in:
//   forall([N]) void f( array(float, N) & );
//   array(float, 7) a;
//   f(a);
const ast::Expr * TranslateDimensionCore::postvisit(
		const ast::DimensionExpr * expr ) {
	// Expression `expr` is an occurrence of N in LHS of above examples.
	// Look up the name that `expr` references.
	// If we are in a struct body, then this reference can be to an entry of
	// the stuct's forall list.
	// Whether or not we are in a struct body, this reference can be to an
	// entry of a containing function's forall list.
	// If we are in a struct body, then the stuct's forall declarations are
	// innermost (functions don't occur in structs).
	// Thus, a potential struct's declaration is highest priority.
	// A struct's forall declarations are already renamed with _generic_ suffix.
	// Try that name variant first.

	std::string useName = "__" + expr->name + "_generic_";
	ast::TypeDecl * namedParamDecl = const_cast<ast::TypeDecl *>(
		strict_dynamic_cast<const ast::TypeDecl *, nullptr >(
			symtab.lookupType( useName ) ) );

	if ( !namedParamDecl ) {
		useName = expr->name;
		namedParamDecl = const_cast<ast::TypeDecl *>( strict_dynamic_cast<const ast::TypeDecl *, nullptr >( symtab.lookupType( useName ) ) );
	}

	// Expect to find it always.
	// A misspelled name would have been parsed as an identifier.
	assertf( namedParamDecl, "Type-system-managed value name not found in symbol table" );

	auto * refToDecl = new ast::TypeInstType( useName, namedParamDecl );

	if ( visitingChildOfSUIT ) {
		// As in postvisit( Expr * ), topmost expression needs a TypeExpr
		// wrapper. But avoid ArrayType-Sizeof.
		return new ast::TypeExpr( expr->location, refToDecl );
	} else {
		// the N occurrence is being used directly as a runtime value,
		// if we are in a type instantiation, then the N is within a bigger value computation
		return new ast::SizeofExpr( expr->location, refToDecl );
	}
}

const ast::Expr * TranslateDimensionCore::postvisit(
		const ast::Expr * expr ) {
	// This expression is used as an argument to instantiate a type.
	if ( visitingChildOfSUIT ) {
		// DimensionExpr and TypeExpr should not reach here.
		return new ast::TypeExpr( expr->location,
			new ast::ArrayType(
				new ast::BasicType( ast::BasicKind::Char ),
				expr,
				ast::VariableLen,
				ast::DynamicDim
			)
		);
	}
	return expr;
}

const ast::Expr * TranslateDimensionCore::postvisit(
		const ast::TypeExpr * expr ) {
	if ( auto instType = dynamic_cast<const ast::EnumInstType *>( expr->type.get() ) ) {
		const ast::EnumDecl * baseEnum = instType->base.get();
		return ast::ConstantExpr::from_int( expr->location, baseEnum->members.size() );
	}
	return expr;
}

} // namespace

void fillGenericParameters( ast::TranslationUnit & translationUnit ) {
	ast::Pass<ValidateGenericParamsCore>::run( translationUnit );
}

void translateDimensionParameters( ast::TranslationUnit & translationUnit ) {
	ast::Pass<TranslateDimensionCore>::run( translationUnit );
}

} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
