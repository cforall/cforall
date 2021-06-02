//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// unwind.h -- Safely include unwind.h from Cforall.
//
// Author           : Andrew Beach
// Created On       : Wed Oct 28 11:25:00 2020
// Last Modified By : Andrew Beach
// Last Modified On : Wed Oct 28 14:11:00 2020
// Update Count     : 0
//

#pragma once

extern "C" {
// This prevents some GCC pragmas that CFA can't handle from being generated.
#define HIDE_EXPORTS

// Always include the header and use its header guard.
#include_next <unwind.h>

#undef HIDE_EXPORTS
}

// Local Variables: //
// mode: c //
// tab-width: 4 //
// End: //
