// 
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// filter.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Tue Apr  9 22:31:18 2002
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 10:12:55 2017
// Update Count     : 11
// 

#pragma once

#include "ParserTypes.h"

extern void (* filter)( Token * tree );					// pointer to filter for parse tree
void freeTree( Token * tree );							// free storage for parse tree

void Identity( Token * tree );							// parse-tree filters
void Parse_Tree( Token * tree );
void Nocode( Token * tree );
void LaTeX( Token * tree );
void HTML( Token * tree );

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
