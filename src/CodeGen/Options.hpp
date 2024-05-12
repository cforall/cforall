//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Options.h --
//
// Author           : Andrew Beach
// Created On       : Tue Apr 30 11:36:00 2019
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Feb 15 18:37:06 2020
// Update Count     : 3
//

#pragma once

struct Options {
	// External Options: Same thoughout a pass.
	bool pretty;
	bool genC;
	bool lineMarks;
	bool printExprTypes;

	// Internal Options: Changed on some recurisive calls.
	bool anonymousUnused = false;

	Options(bool pretty, bool genC, bool lineMarks, bool printExprTypes) :
		pretty(pretty), genC(genC), lineMarks(lineMarks), printExprTypes(printExprTypes)
		{}
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
