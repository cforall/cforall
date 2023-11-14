//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// PolyCost.hpp --
//
// Author           : Andrew Beach
// Created On       : Tue Jan 17 16:45:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jan 17 16:45:00 2023
// Update Count     : 0
//

#pragma once

namespace ast {
    class SymbolTable;
    class Type;
    class TypeEnvironment;
}

namespace ResolvExpr {

int polyCost( const ast::Type * type,
	const ast::SymbolTable & symtab, const ast::TypeEnvironment & env );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
