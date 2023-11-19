//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// PolyCost.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 09:50:12 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jun 19 10:45:00 2019
// Update Count     : 4
//

#include "AST/SymbolTable.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"

namespace ResolvExpr {

namespace {

class PolyCost {
	const ast::SymbolTable &symtab;
public:
	int result;
	const ast::TypeEnvironment &env_;

	PolyCost( const ast::SymbolTable & symtab, const ast::TypeEnvironment & env ) 
	: symtab( symtab ), result( 0 ), env_( env ) {}

	void previsit( const ast::TypeInstType * type ) {
		if ( const ast::EqvClass * eqv = env_.lookup( *type ) ) /* && */ if ( eqv->bound ) {
			if ( const ast::TypeInstType * otherType = eqv->bound.as< ast::TypeInstType >() ) {
				if ( symtab.lookupType( otherType->name ) ) {
					// Bound to opaque type.
					result += 1;
				}
			} else {
				// Bound to concrete type.
				result += 1;
			}
		}
	}
};

} // namespace

int polyCost(
	const ast::Type * type, const ast::SymbolTable & symtab, const ast::TypeEnvironment & env
) {
	ast::Pass<PolyCost> costing( symtab, env );
	type->accept( costing );
	return (costing.core.result > 0) ? 1 : 0;
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
