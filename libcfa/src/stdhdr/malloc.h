// 
// Cforall Version 1.0.0 Copyright (C) 2017 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// malloc.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Thu Jul 20 15:58:16 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed May 27 14:13:14 2020
// Update Count     : 18
// 

extern "C" {
#include_next <malloc.h>								// has internal check for multiple expansion
} // extern "C"

#include <heap.hfa>

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
