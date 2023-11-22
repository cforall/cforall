//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Keywords.h -- Implement concurrency constructs from their keywords.
//
// Author           : Thierry Delisle
// Created On       : Fri Mar 10 15:16:42 2017
// Last Modified By :
// Last Modified On :
// Update Count     : 1
//

#pragma once

namespace ast {
	class TranslationUnit;
}

namespace Concurrency {

/// Implement the sue-like keywords and the suspend keyword. Pre-Autogen
void implementKeywords( ast::TranslationUnit & translationUnit );
/// Implement the mutex parameters and mutex statement. Post-Autogen
void implementMutex( ast::TranslationUnit & translationUnit );
/// Add the thread starter code to constructors. Post-Autogen
void implementThreadStarter( ast::TranslationUnit & translationUnit );

};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
