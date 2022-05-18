//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixMain2.cc -- A side file used to seperate the compiler and demangler.
//
// Author           : Andrew Beach
// Created On       : Tue May 17 10:05:00 2022
// Last Modified By : Andrew Beach
// Last Modified On : Tue May 17 10:08:00 2022
// Update Count     : 0
//

#include "FixMain.h"

namespace CodeGen {

bool FixMain::replace_main = false;

} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
