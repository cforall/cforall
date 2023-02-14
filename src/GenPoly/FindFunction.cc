//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindFunction.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri Oct  7 17:05:20 2022
// Update Count     : 7
//

#include "FindFunction.h"

#include <utility>                      // for pair

#include "AST/Pass.hpp"                 // for Pass
#include "AST/Type.hpp"
#include "Common/PassVisitor.h"         // for PassVisitor
#include "GenPoly/ErasableScopedMap.h"  // for ErasableScopedMap<>::iterator
#include "GenPoly/GenPoly.h"            // for TyVarMap
#include "ScrubTyVars.h"                // for ScrubTyVars
#include "SynTree/Declaration.h"        // for DeclarationWithType, TypeDecl
#include "SynTree/Mutator.h"            // for Mutator, mutateAll
#include "SynTree/Type.h"               // for FunctionType, Type, Type::For...

namespace GenPoly {
	class FindFunction : public WithGuards, public WithVisitorRef<FindFunction>, public WithShortCircuiting {
	  public:
		FindFunction( std::list< FunctionType const* > &functions, const TyVarMap &tyVars, bool replaceMode, FindFunctionPredicate predicate );

		void premutate( FunctionType * functionType );
		Type * postmutate( FunctionType * functionType );
		void premutate( PointerType * pointerType );
	  private:
		void handleForall( const Type::ForallList &forall );

		std::list< FunctionType const * > & functions;
		TyVarMap tyVars;
		bool replaceMode;
		FindFunctionPredicate predicate;
	};

	void findFunction( Type *type, std::list< FunctionType const * > &functions, const TyVarMap &tyVars, FindFunctionPredicate predicate ) {
		PassVisitor<FindFunction> finder( functions, tyVars, false, predicate );
		type->acceptMutator( finder );
	}

	void findAndReplaceFunction( Type *&type, std::list< FunctionType const * > &functions, const TyVarMap &tyVars, FindFunctionPredicate predicate ) {
		PassVisitor<FindFunction> finder( functions, tyVars, true, predicate );
		type = type->acceptMutator( finder );
	}

	FindFunction::FindFunction( std::list< FunctionType const * > &functions, const TyVarMap &tyVars, bool replaceMode, FindFunctionPredicate predicate )
		: functions( functions ), tyVars( tyVars ), replaceMode( replaceMode ), predicate( predicate ) {
	}

	void FindFunction::handleForall( const Type::ForallList &forall ) {
		for ( const Declaration * td : forall ) {
			TyVarMap::iterator var = tyVars.find( td->name );
			if ( var != tyVars.end() ) {
				tyVars.erase( var->first );
			} // if
		} // for
	}

	void FindFunction::premutate( FunctionType * functionType ) {
		visit_children = false;
		GuardScope( tyVars );
		handleForall( functionType->get_forall() );
		mutateAll( functionType->get_returnVals(), *visitor );
	}

	Type * FindFunction::postmutate( FunctionType * functionType ) {
		Type *ret = functionType;
		if ( predicate( functionType, tyVars ) ) {
			functions.push_back( functionType );
			if ( replaceMode ) {
				// replace type parameters in function type with void*
				ret = ScrubTyVars::scrub( functionType->clone(), tyVars );
			} // if
		} // if
		return ret;
	}

	void FindFunction::premutate( PointerType * pointerType ) {
		GuardScope( tyVars );
		handleForall( pointerType->get_forall() );
	}

namespace {

struct FindFunctionCore :
		public ast::WithGuards,
		public ast::WithShortCircuiting,
		public ast::WithVisitorRef<FindFunctionCore> {
	FindFunctionCore(
		std::vector<ast::ptr<ast::FunctionType>> & functions,
		const TypeVarMap & typeVars, FindFunctionPred predicate,
		bool replaceMode );

	void previsit( ast::FunctionType const * type );
	ast::Type const * postvisit( ast::FunctionType const * type );
	void previsit( ast::PointerType const * type );
private:
	void handleForall( const ast::FunctionType::ForallList & forall );

	std::vector<ast::ptr<ast::FunctionType>> &functions;
	TypeVarMap typeVars;
	FindFunctionPred predicate;
	bool replaceMode;
};

FindFunctionCore::FindFunctionCore(
		std::vector<ast::ptr<ast::FunctionType>> & functions,
		const TypeVarMap &typeVars, FindFunctionPred predicate,
		bool replaceMode ) :
	functions( functions ), typeVars( typeVars ),
	predicate( predicate ), replaceMode( replaceMode ) {}

void FindFunctionCore::handleForall( const ast::FunctionType::ForallList & forall ) {
	for ( const ast::ptr<ast::TypeInstType> & td : forall ) {
		TypeVarMap::iterator var = typeVars.find( *td );
		if ( var != typeVars.end() ) {
			typeVars.erase( var->first );
		} // if
	} // for
}

void FindFunctionCore::previsit( ast::FunctionType const * type ) {
	visit_children = false;
	GuardScope( typeVars );
	handleForall( type->forall );
	//ast::accept_all( type->returns, *visitor );
	// This might have to become ast::mutate_each with return.
	ast::accept_each( type->returns, *visitor );
}

ast::Type const * FindFunctionCore::postvisit( ast::FunctionType const * type ) {
	ast::Type const * ret = type;
	if ( predicate( type, typeVars ) ) {
		functions.push_back( type );
		if ( replaceMode ) {
			// replace type parameters in function type with void*
			ret = scrubTypeVars( ast::deepCopy( type ), typeVars );
		} // if
	} // if
	return ret;
}

void FindFunctionCore::previsit( ast::PointerType const * /*type*/ ) {
	GuardScope( typeVars );
	//handleForall( type->forall );
}

} // namespace

void findFunction( const ast::Type * type,
		std::vector<ast::ptr<ast::FunctionType>> & functions,
		const TypeVarMap & typeVars, FindFunctionPred predicate ) {
	ast::Pass<FindFunctionCore> pass( functions, typeVars, predicate, false );
	type->accept( pass );
	//(void)type;
	//(void)functions;
	//(void)typeVars;
	//(void)predicate;
}

const ast::Type * findAndReplaceFunction( const ast::Type * type,
		std::vector<ast::ptr<ast::FunctionType>> & functions,
		const TypeVarMap & typeVars, FindFunctionPred predicate ) {
	ast::Pass<FindFunctionCore> pass( functions, typeVars, predicate, true );
	return type->accept( pass );
	//(void)functions;
	//(void)typeVars;
	//(void)predicate;
	//return type;
}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
