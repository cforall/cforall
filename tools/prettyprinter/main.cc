// 
// Cforall Version 1.0.0 Copyright (C) 2017 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// main.cc -- 
// 
// Author           : Peter A. Buhr
// Created On       : Wed Jun 28 22:57:26 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Jun 29 13:09:32 2017
// Update Count     : 58
// 

#include <iostream>
#include <fstream>
#include <string>
using namespace std;
#include <unistd.h>										// close
#include <getopt.h>										// getopt
#include "filter.h"

extern FILE * yyin;
extern int yydebug;
extern int yyparse( void );

bool parse_cmdline( int argc, char * argv[] ) {
	enum { Html, Identity, Latex, Nocode, ParseTree, };

	static struct option long_opts[] = {
		{ "html", no_argument, nullptr, Html },
		{ "identity", no_argument, nullptr, Identity },
		{ "latex", no_argument, nullptr, Latex },
		{ "nocode", no_argument, nullptr, Nocode },
		{ "parse-tree", no_argument, nullptr, ParseTree },
		{ nullptr, 0, nullptr, 0 }
	}; // long_opts
	int long_index;

	opterr = 0;											// (global) prevent getopt from printing error messages

	int c;
	while ( (c = getopt_long( argc, argv, "hilnp", long_opts, &long_index )) != -1 ) {
		switch ( c ) {
		  case Html:
		  case 'h':
			filter = HTML;
			break;
		  case Identity:
		  case 'i':
			filter = ::Identity;
			break;
		  case Latex:
		  case 'l':
			filter = LaTeX;
			break;
		  case Nocode:
		  case 'n':
			filter = ::Nocode;
			break;
		  case ParseTree:
		  case 'p':
			filter = Parse_Tree;
		  case '?':
			if ( optopt ) {								// short option ?
				cerr << "Unknown option: -" << (char)optopt << endl;
			} else {									// long option
				cerr << "Unknown option: " << argv[optind - 1] << endl;
			} // if
			return false;
		  default:
			abort();
		} // switch
	} // while

	if ( optind != argc ) {								// input files ?
		if ( optind == argc - 1 ) {						// any commands after the flags ? => input file name
			yyin = fopen( argv[ optind ], "r" );
			if ( yyin == nullptr ) {
				cerr << "Open failure for input file \"" << argv[ optind ] << "\"" << endl;
				return false;
			} // if
		} else {
			cerr << "Too many input files " << argv[ optind + 1 ] << endl;
			return false;
		} // if
	} // if
	return true;
} // parse_cmdline

int main( int argc, char *argv[] ) {
	yyin = stdin;										// defaults
	filter = Nocode;

	if ( ! parse_cmdline( argc, argv ) ) {
		cerr << "Usage: " << argv[0]
			 << " ["
			 << "-h/--html | "
			 << "-i/--identity | "
			 << "-l/--latex | "
			 << "-n/--nocode | "
			 << "-p/--parse-tree"
			 << "] [input-file]"
			 << endl;
		exit( EXIT_FAILURE );							// TERMINATE
	} // if

	//yydebug = 1;
	yyparse();

	if ( yyin != stdin ) fclose( yyin );				// close file, do not delete cin!
} // main

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
