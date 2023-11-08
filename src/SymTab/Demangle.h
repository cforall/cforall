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
// Last Modified On : Mon Nov  6 15:48:00 2023
// Update Count     : 1
//

#pragma once

extern "C" {
	/// Main interface to the demangler as a utility.
	/// Caller must free the returned string.
	char * cforall_demangle(const char *, int);
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
