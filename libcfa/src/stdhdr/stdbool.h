//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// stdbool.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Mon Jul  4 23:25:26 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Mar 25 08:00:08 2019
// Update Count     : 15
// 

extern "C" {
#include_next <stdbool.h>								// has internal check for multiple expansion

// allows printing as true/false
#if defined( true )
#undef true
#define true ((_Bool)1)
#endif // true

#if defined( false )
#undef false
#define false ((_Bool)0)
#endif // false
} // extern "C"

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
