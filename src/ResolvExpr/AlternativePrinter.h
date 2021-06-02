//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AlternativePrinter.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 06:55:43 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:37:09 2017
// Update Count     : 4
//

#pragma once

#include <iostream>          // for ostream

#include "Common/PassVisitor.h"

class ExprStmt;

namespace ResolvExpr {
	class AlternativePrinter final : public WithIndexer {
	  public:
		AlternativePrinter( std::ostream &os );

		void postvisit( ExprStmt *exprStmt );
	  private:
		std::ostream &os;
	};
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
