//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixFunction.hpp --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 17:02:08 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Jul 12 14:19:00 2022
// Update Count     : 5
//

#pragma once

namespace ast {
	class DeclWithType;
	class Type;
}

namespace SymTab {

/// Returns declaration with function and array types replaced by equivalent pointer types.
/// Sets isVoid to true if type is void.
const ast::DeclWithType * fixFunction( const ast::DeclWithType * dwt, bool & isVoid );
const ast::Type * fixFunction( const ast::Type * type, bool & isVoid );

} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
