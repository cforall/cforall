//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CandidatePrinter.hpp -- Print expression canditates.
//
// Author           : Andrew Beach
// Created On       : Tue Nov  9  9:49:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Tue Nov  9 15:33:00 2021
// Update Count     : 0
//

#pragma once

namespace ast {
    class TranslationUnit;
}

namespace ResolvExpr {

void printCandidates( ast::TranslationUnit & transUnit );
/* Traverse over the entire translation unit, printing candidates for each
 * top level expression. See CandidateFinder.
 */

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
