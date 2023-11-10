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
#include "Common/ScopedMap.h"     // for ScopedMap
#include "Common/utility.h"       // for CodeLocation
#include "InitTweak/InitTweak.h"  // for getFunction
#include "Tuples.h"

namespace Tuples {

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
