//                              -*- Mode: C++ -*- 
// 
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// parse.h - Various declarations that are needed so that the generated parser
//           and lexer compile with C++, and to share information between the
//           parser, lexer, and driver program
// 
// Author           : Richard C. Bilson and Rodolfo G. Esteves
// Created On       : Sun Dec 16 15:00:49 2001
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Jan 26 23:05:34 2021
// Update Count     : 176
// 

#pragma once

extern "C" int yylex();

#include <string>
#include <list>
#include "token.h"

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
