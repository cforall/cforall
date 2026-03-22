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
// Last Modified On : Sat Mar  7 15:39:59 2026
// Update Count     : 14
//

extern int yydebug;                   // set for -g flag (Grammar)
extern int
	astp,
	bresolverp,
	bboxp,
	bcodegenp,
	ctordtorp,
	declstatsp,
	excpdeclp,
	expraltp,
	expranlp,
	instgenp,
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
	valideclp,
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
