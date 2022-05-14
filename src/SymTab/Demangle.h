//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Demangle.h -- Convert a mangled name into a human readable name.
//
// Author           : Andrew Beach
// Created On       : Fri May 13 10:11:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Fri May 13 10:30:00 2022
// Update Count     : 0
//

#pragma once

extern "C" {
	char * cforall_demangle(const char *, int);
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
