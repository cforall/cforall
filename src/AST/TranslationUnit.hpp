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
// Last Modified On : Thr Mar  9 16:41:00 2023
// Update Count     : 2
//

#pragma once

#include <map>
#include <list>

#include "Fwd.hpp"

namespace ast {

class TranslationGlobal {
public:
	std::map< UniqueId, Decl * > idMap;

	ptr<Type> sizeType;
	const FunctionDecl * dereference = nullptr;
	const StructDecl * dtorStruct = nullptr;
	const FunctionDecl * dtorDestroy = nullptr;
};

class TranslationUnit {
public:
	std::list< ptr< Decl > > decls;
	TranslationGlobal global;
};

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
