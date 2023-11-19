//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SpecCost.hpp --
//
// Author           : Andrew Beach
// Created On       : Tue Jan 17 16:49:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jan 17 16:49:00 2023
// Update Count     : 0
//

#pragma once

namespace ast {
    class Type;
}

namespace ResolvExpr {

int specCost( const ast::Type * type );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
