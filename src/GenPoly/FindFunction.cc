//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindFunction.cc -- Find function types in a larger type.
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
#include "GenPoly/ErasableScopedMap.h"  // for ErasableScopedMap<>::iterator
#include "GenPoly/GenPoly.h"            // for TyVarMap
#include "ScrubTypeVars.hpp"            // for scrubTypeVars

namespace GenPoly {

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
	ast::accept_each( type->returns, *visitor );
}

ast::Type const * FindFunctionCore::postvisit( ast::FunctionType const * type ) {
	ast::Type const * ret = type;
	if ( predicate( type, typeVars ) ) {
		functions.push_back( type );
		if ( replaceMode ) {
			// Replace type parameters in function type with void *.
			ret = scrubTypeVars( ast::deepCopy( type ), typeVars );
		} // if
	} // if
	return ret;
}

void FindFunctionCore::previsit( ast::PointerType const * /*type*/ ) {
	GuardScope( typeVars );
}

} // namespace

void findFunction( const ast::Type * type,
		std::vector<ast::ptr<ast::FunctionType>> & functions,
		const TypeVarMap & typeVars, FindFunctionPred predicate ) {
	ast::Pass<FindFunctionCore> pass( functions, typeVars, predicate, false );
	type->accept( pass );
}

const ast::Type * findAndReplaceFunction( const ast::Type * type,
		std::vector<ast::ptr<ast::FunctionType>> & functions,
		const TypeVarMap & typeVars, FindFunctionPred predicate ) {
	ast::Pass<FindFunctionCore> pass( functions, typeVars, predicate, true );
	return type->accept( pass );
}

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
