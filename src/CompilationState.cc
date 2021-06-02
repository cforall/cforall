//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CompilationState.cc --
//
// Author           : Rob Schluntz
// Created On       : Mon Ju1 30 10:47:01 2018
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri May  3 13:45:23 2019
// Update Count     : 4
//

#include "config.h"

int
	astp = false,
	bresolvep = false,
	bboxp = false,
	bcodegenp = false,
	ctorinitp = false,
	declstatsp = false,
	exprp = false,
	expraltp = false,
	genericsp = false,
	libcfap = false,
	nopreludep = false,
	genproto = false,
	deterministic_output = false,
	useNewAST = CFA_USE_NEW_AST,
	nomainp = false,
	parsep = false,
	resolvep = false,
	resolvprotop = false,
	symtabp = false,
	treep = false,
	tuplep = false,
	validp = false,
	errorp = false,
	codegenp = false,
	prettycodegenp = false,
	linemarks = false;

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End:  //
