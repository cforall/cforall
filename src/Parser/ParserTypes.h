//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// parser.hh --
//
// Author           : Peter A. Buhr
// Created On       : Sat Sep 22 08:58:10 2001
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Feb 15 11:04:40 2020
// Update Count     : 351
//

#pragma once

int yylex();
void yyerror( const char * );

#include <string>
#include "ParseNode.h"
// External declarations for information sharing between lexer and scanner
class TypedefTable;
extern TypedefTable typedefTable;

// current location in the input
extern int yylineno;
extern char * yyfilename;

struct Location {
    char * file;
    int line;
}; // Location

struct Token {
    std::string * str;									// must be pointer as used in union
    Location loc;

    operator std::string *() { return str; }
}; // Token

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
