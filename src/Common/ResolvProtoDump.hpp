//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolvProtoDump.hpp -- Prints AST as instances for resolv-proto.
//
// Author           : Andrew Beach
// Created On       : Wed Oct  6 14:05:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Wed Oct  6 14:29:00 2021
// Update Count     : 0
//

#pragma once

namespace ast {
	class TranslationUnit;
}

/// Prints AST as instances for resolv-proto.
void dumpAsResolverProto( ast::TranslationUnit & transUnit );

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
