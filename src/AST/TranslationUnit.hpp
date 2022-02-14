//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TranslationUnit.hpp --
//
// Author           : Andrew Beach
// Created On       : Tue Jun 11 15:30:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jun 11 15:42:00 2019
// Update Count     : 0
//

#pragma once

#include <map>
#include <vector>

#include "Fwd.hpp"

namespace ast {

class TranslationUnit {
public:
	std::list< ptr< Decl > > decls;

	struct Global {
		std::map< UniqueId, Decl * > idMap;

		ptr<Type> sizeType;
		const FunctionDecl * dereference;
		const StructDecl * dtorStruct;
		const FunctionDecl * dtorDestroy;
	} global;
};

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
