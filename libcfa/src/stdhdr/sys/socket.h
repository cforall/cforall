// 
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
// 
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// socket.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Sun Mar 14 10:58:36 2021
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Mar 14 10:59:41 2021
// Update Count     : 1
// 

extern "C" {
#include_next <sys/socket.h>								// has internal check for multiple expansion
} // extern "C"

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
