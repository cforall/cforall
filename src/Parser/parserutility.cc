//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// parserutility.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Sat May 16 15:30:39 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Mar  1 10:42:00 2023
// Update Count     : 9
//

#include "parserutility.h"

#include <list>                  // for list
#include <string>                // for string

#include "AST/Expr.hpp"          // for UntypedExpr, CastExpr, ConstantExpr
#include "AST/Type.hpp"          // for BasicType, ZeroType, BasicType::Kind...

// rewrite
//    if ( x ) ...
// as
//    if ( (int)(x != 0) ) ...

ast::Expr * notZeroExpr( const ast::Expr * orig ) {
	return ( !orig ) ? nullptr : new ast::CastExpr( orig->location,
		ast::UntypedExpr::createCall( orig->location,
			"?!=?",
			{
				orig,
				new ast::ConstantExpr( orig->location,
					new ast::ZeroType(),
					"0",
					std::optional<unsigned long long>( 0 )
				),
			}
		),
		new ast::BasicType( ast::BasicType::SignedInt )
	);
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
