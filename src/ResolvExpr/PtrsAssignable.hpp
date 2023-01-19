//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// PtrsAssignable.hpp --
//
// Author           : Andrew Beach
// Created On       : Fri Dec  9 11:40:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri Dec  9 11:40:00 2022
// Update Count     : 0
//

#pragma once

class Type;
namespace ast {
	class Type;
	class TypeEnvironment;
}

namespace ResolvExpr {

class TypeEnvironment;

int ptrsAssignable( const Type * src, const Type * dest,
	const TypeEnvironment & env );
int ptrsAssignable( const ast::Type * src, const ast::Type * dst,
	const ast::TypeEnvironment & env );

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
