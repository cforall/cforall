// 
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
// 
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// time.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Tue Jun 16 22:19:03 2020
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Jun 16 22:19:56 2020
// Update Count     : 1
// 

extern "C" {
#include_next <sys/time.h>								// has internal check for multiple expansion
} // extern "C"

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
