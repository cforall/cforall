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
// Last Modified On : Tus Jul 18 10:12:00 2017
// Update Count     : 8
//

#include "parserutility.h"

#include <list>                  // for list
#include <string>                // for string

#include "SynTree/Constant.h"    // for Constant
#include "SynTree/Expression.h"  // for UntypedExpr, CastExpr, ConstantExpr
#include "SynTree/Type.h"        // for BasicType, ZeroType, BasicType::Kind...

// rewrite
//    if ( x ) ...
// as
//    if ( (int)(x != 0) ) ...

Expression *notZeroExpr( Expression *orig ) {
	if( !orig ) return nullptr;
	UntypedExpr *comparison = new UntypedExpr( new NameExpr( "?!=?" ) );
	comparison->get_args().push_back( orig );
	comparison->get_args().push_back( new ConstantExpr( Constant( new ZeroType( noQualifiers ), "0", (unsigned long long int)0 ) ) );
	return new CastExpr( comparison, new BasicType( Type::Qualifiers(), BasicType::SignedInt ) );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
