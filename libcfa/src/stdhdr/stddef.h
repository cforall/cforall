//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// stddef.h --
//
// Author           : Peter A. Buhr
// Created On       : Mon Jul  4 23:25:26 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Jul  5 20:40:01 2016
// Update Count     : 12
//

extern "C" {
#include_next <stddef.h>                // has internal check for multiple expansion
#undef NULL
#define NULL 0                          // define NULL as 0 rather than (void*)0 to take advantage of zero_t
} // extern "C"

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
