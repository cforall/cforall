//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExplodedArg.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Tue Jun 11 16:18:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Tue Jun 11 16:18:00 2019
// Update Count     : 1
//

#include "ExplodedArg.hpp"

#include "Tuples/Explode.hpp"   // for Tuples::explode

namespace ResolvExpr {

ExplodedArg::ExplodedArg( const Candidate & arg, const ast::SymbolTable & symtab )
: env( arg.env ), cost( arg.cost ), exprs() { Tuples::explode( arg, symtab, *this ); }

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
