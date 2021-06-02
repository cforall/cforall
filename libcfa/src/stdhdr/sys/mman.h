// 
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
// 
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// mman.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Tue May 26 21:23:45 2020
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed May 27 14:23:41 2020
// Update Count     : 3
// 

extern "C" {
#include_next <sys/mman.h>									// has internal check for multiple expansion
} // extern "C"

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
