//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// HoistTypeDecls.hpp -- Hoists declarations of implicitly declared types.
//
// Author           : Andrew Beach
// Created On       : Mon Jul  4  9:51:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Mon Jul  4  9:51:00 2022
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Validate {

/// There are some places where a type can be declared but are usually only
/// referenced (with an *InstType). This inserts the declarations before
/// they are referenced.
void hoistTypeDecls( ast::TranslationUnit & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
