//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AssertAcyclic.hpp -- Check that ast::ptr does not form a cycle.
//
// Author           : Andrew Beach
// Created On       : Thr Jun  6 15:00:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Fri Jun  7 14:32:00 2019
// Update Count     : 1
//

#pragma once

#include <list>

#include "Node.hpp"
namespace ast {
    class Decl;
};

namespace ast {

void assertAcyclic( const std::list< ast::ptr< ast::Decl > > & translationUnit );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
