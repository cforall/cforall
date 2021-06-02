//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenericSubstitution.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 22 14:15:00 2019
// Last Modified By : Aaron B. Moss
// Created On       : Wed May 22 14:15:00 2019
// Update Count     : 1
//

#pragma once

#include "TypeSubstitution.hpp"

namespace ast {

class Type;

TypeSubstitution genericSubstitution( const Type * );

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
