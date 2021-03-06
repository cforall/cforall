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
// Last Modified By : Rob Schluntz
// Last Modified On : Fri Feb 05 12:22:20 2016
// Update Count     : 6
//

#include "FindFunction.h"

#include <utility>                      // for pair

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
		FindFunction( std::list< FunctionType* > &functions, const TyVarMap &tyVars, bool replaceMode, FindFunctionPredicate predicate );

		void premutate( FunctionType * functionType );
		Type * postmutate( FunctionType * functionType );
		void premutate( PointerType * pointerType );
	  private:
		void handleForall( const Type::ForallList &forall );

		std::list< FunctionType* > &functions;
		TyVarMap tyVars;
		bool replaceMode;
		FindFunctionPredicate predicate;
	};

	void findFunction( Type *type, std::list< FunctionType* > &functions, const TyVarMap &tyVars, FindFunctionPredicate predicate ) {
		PassVisitor<FindFunction> finder( functions, tyVars, false, predicate );
		type->acceptMutator( finder );
	}

	void findAndReplaceFunction( Type *&type, std::list< FunctionType* > &functions, const TyVarMap &tyVars, FindFunctionPredicate predicate ) {
		PassVisitor<FindFunction> finder( functions, tyVars, true, predicate );
		type = type->acceptMutator( finder );
	}

	FindFunction::FindFunction( std::list< FunctionType* > &functions, const TyVarMap &tyVars, bool replaceMode, FindFunctionPredicate predicate )
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
} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
