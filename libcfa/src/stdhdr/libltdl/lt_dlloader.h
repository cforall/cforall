// 
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
// 
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// lt_dlloader.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Thu Apr  1 13:49:40 2021
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Apr  1 14:00:19 2021
// Update Count     : 4
// 

extern "C" {
#if ! defined( vtable )									// nesting ?
#define vtable ``vtable									// make keyword an identifier
#define __CFA_LT_DLLOADER_H__
#endif

#include_next <libltdl/lt_dlloader.h>					// has internal check for multiple expansion

#if defined( vtable ) && defined( __CFA_LT_DLLOADER_H__ ) // reset only if set
#undef vtable
#undef __CFA_LT_DLLOADER_H__
#endif
} // extern "C"

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
