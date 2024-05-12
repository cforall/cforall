//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CastCost.hpp -- Cost of a cast.
//
// Author           : Andrew Beach
// Created On       : Fri Dec  9 11:28:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri Dec  9 11:28:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
	class SymbolTable;
	class Type;
	class TypeEnvironment;
}

namespace ResolvExpr {

class Cost;

Cost castCost(
	const ast::Type * src, const ast::Type * dst, bool srcIsLvalue,
	const ast::SymbolTable & symtab, const ast::TypeEnvironment & env );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
