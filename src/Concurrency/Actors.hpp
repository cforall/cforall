//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Actors.hpp -- Implement concurrency constructs from their keywords.
//
// Author           : Colby Parsons
// Created On       : Thurs Jan 19 15:16:42 2023
// Last Modified By :
// Last Modified On :
// Update Count     : 1
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Concurrency {

void implementActors( ast::TranslationUnit & translationUnit );

};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
