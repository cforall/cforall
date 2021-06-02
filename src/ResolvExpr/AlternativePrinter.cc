//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AlternativePrinter.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 06:53:19 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun May 17 06:55:05 2015
// Update Count     : 2
//

#include "AlternativePrinter.h"

#include <list>                          // for _List_const_iterator, list<>...

#include "Alternative.h"                 // for AltList, Alternative
#include "AlternativeFinder.h"           // for AlternativeFinder
#include "ResolvExpr/TypeEnvironment.h"  // for TypeEnvironment
#include "SynTree/Expression.h"          // for Expression
#include "SynTree/Statement.h"           // for ExprStmt
#include "SynTree/Type.h"                // for Type

namespace ResolvExpr {
	AlternativePrinter::AlternativePrinter( std::ostream &os ) : os( os ) {}

	void AlternativePrinter::postvisit( ExprStmt *exprStmt ) {
		TypeEnvironment env;
		AlternativeFinder finder( indexer, env );
		finder.findWithAdjustment( exprStmt->get_expr() );
		int count = 1;
		os << "There are " << finder.get_alternatives().size() << " alternatives" << std::endl;
		for ( AltList::const_iterator i = finder.get_alternatives().begin(); i != finder.get_alternatives().end(); ++i ) {
			os << "Alternative " << count++ << " ==============" << std::endl;
			i->expr->get_result()->print( os );
			//    i->print( os );
			os << std::endl;
		} // for
	} // AlternativePrinter::visit
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
