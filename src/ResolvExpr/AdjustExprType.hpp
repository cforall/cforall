//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AdjustExprType.hpp --
//
// Author           : Andrew Beach
// Created On       : Wed Jan 18  9:56:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jan 18  9:56:00 2023
// Update Count     : 0
//

#pragma once

namespace ast {
	class SymbolTable;
	class Type;
	class TypeEnvironment;
}

namespace ResolvExpr {

/// Replaces array types with equivalent pointer,
/// and function types with a pointer-to-function.
const ast::Type * adjustExprType( const ast::Type * type,
	const ast::TypeEnvironment & env, const ast::SymbolTable & symtab );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
