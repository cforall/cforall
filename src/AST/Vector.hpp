//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Vector.hpp -- Short hand for vector of ast pointers.
//
// Author           : Andrew Beach
// Created On       : Thu Oct 20  9:46:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Thu Oct 20 10:16:00 2022
// Update Count     : 0
//

#pragma once

#include <vector>

#include "AST/Node.hpp"

namespace ast {

/// Short hand for a vector of ast::ptr types.
template<typename T, typename Alloc = std::allocator<ptr<T>> >
using vector = std::vector<ptr<T>, Alloc>;

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
