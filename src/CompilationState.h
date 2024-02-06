//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CompilationState.h --
//
// Author           : Rob Schluntz
// Created On       : Mon Ju1 30 10:47:01 2018
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Apr 10 19:12:53 2023
// Update Count     : 6
//

extern int yydebug;                   // set for -g flag (Grammar)
extern int
	astp,
	bresolvep,
	bboxp,
	bcodegenp,
	ctorinitp,
	declstatsp,
	exdeclp,
	exprp,
	expraltp,
	genericsp,
	invariant,
	libcfap,
	nopreludep,
	genproto,
	deterministic_output,
	useNewAST,
	nomainp,
	resolvep,
	resolvprotop,
	symtabp,
	treep,
	tuplep,
	validp,
	errorp,
	codegenp,
	prettycodegenp,
	linemarks,
	reppseu;

// is the compiler building prelude or libcfa?
inline bool buildingLibrary() {
	return libcfap | treep;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End:  //
