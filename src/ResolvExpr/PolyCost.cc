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
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"
#include "Common/PassVisitor.h"
#include "SymTab/Indexer.h"   // for Indexer
#include "SynTree/Type.h"     // for TypeInstType, Type
#include "TypeEnvironment.h"  // for EqvClass, TypeEnvironment

namespace ResolvExpr {
	struct PolyCost {
		PolyCost( const TypeEnvironment &env, const SymTab::Indexer &indexer );

		void previsit( TypeInstType * aggregateUseType );
		int result;
		const TypeEnvironment &tenv;
		const SymTab::Indexer &indexer;
	};

	int polyCost( Type *type, const TypeEnvironment & env, const SymTab::Indexer &indexer ) {
		PassVisitor<PolyCost> coster( env, indexer );
		type->accept( coster );
		return (coster.pass.result > 0) ? 1 : 0;
	}

	PolyCost::PolyCost( const TypeEnvironment & env, const SymTab::Indexer & indexer ) : result( 0 ), tenv( env ), indexer( indexer ) {
	}

	void PolyCost::previsit(TypeInstType * typeInst) {
		if ( const EqvClass *eqvClass = tenv.lookup( typeInst->name ) ) {
			if ( eqvClass->type ) {
				if ( TypeInstType * otherTypeInst = dynamic_cast< TypeInstType* >( eqvClass->type ) ) {
					if ( indexer.lookupType( otherTypeInst->name ) ) {
						// bound to opaque type
						result += 1;
					} // if
				} else {
					// bound to concrete type
					result += 1;
				} // if
			} // if
		} // if
	}

// TODO: When the old PolyCost is torn out get rid of the _new suffix.
class PolyCost_new {
	const ast::SymbolTable &symtab;
public:
	int result;
	const ast::TypeEnvironment &env_;

	PolyCost_new( const ast::SymbolTable & symtab, const ast::TypeEnvironment & env ) 
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

int polyCost(
	const ast::Type * type, const ast::SymbolTable & symtab, const ast::TypeEnvironment & env
) {
	ast::Pass<PolyCost_new> costing( symtab, env );
	type->accept( costing );
	return (costing.core.result > 0) ? 1 : 0;
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
