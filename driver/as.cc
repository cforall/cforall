//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// as.c -- map assembler file, scan for debug information. If found, expand file by one character and insert Cforall
//         language code on the N line from the start of the debug information.
//
// Author           : Peter A. Buhr
// Created On       : Wed Aug  1 10:49:42 2018
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Sep  8 08:40:16 2018
// Update Count     : 97
//

#include <cstdio>										// perror
#include <cstdlib>										// exit
#include <fcntl.h>										// open
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>									// mmap
#include <string.h>

//#define __DEBUG_H__

#ifdef __DEBUG_H__
#include <iostream>
using namespace std;
#endif // __DEBUG_H__

int main( const int argc, const char * argv[] ) {
	#ifdef __DEBUG_H__
	for ( int i = 0; i < argc; i += 1 ) {
		cerr << argv[i] << endl;
	} // for
	#endif // __DEBUG_H__

	int fd = open( argv[argc - 1], O_RDWR );
	if ( fd < 0 ) { perror( "open" ); exit( EXIT_FAILURE ); };

	struct stat mystat = {};
	if ( fstat( fd, &mystat ) ) { perror( "fstat" ); exit( EXIT_FAILURE ); };
	off_t size = mystat.st_size;

	if ( size ) {										// cannot map 0 sized file
		char * start = (char *)mmap( NULL, size + 2, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0 );
		if ( start == (void *)-1 ) { perror( "mmap" ); exit( EXIT_FAILURE ); };

		if ( char * cursor = strstr( start, ".Ldebug_info0:" ) ) { // debug information ?
			// Expand file by one byte to hold 2 character Cforall language code.
			if ( ftruncate( fd, size + 1 ) ) { perror( "ftruncate" ); exit( EXIT_FAILURE ); };

			for ( int i = 0; i < 8; i += 1 ) {			// move N (magic) lines forward
				cursor = strstr( cursor, "\n" ) + 1;
			} // for

			cursor -= 2;								// backup over "c\n" language value
			if ( *(cursor - 1) != 'x' ) { fprintf( stderr, "invalid C language code\n" ); exit( EXIT_FAILURE ); };

			memmove( cursor + 2, cursor + 1, start + size - cursor - 1 ); // move remaining text 1 character right

			*(cursor) = '2';							// replace C language value with CFA
			*(cursor + 1) = '5';
		} // if

		if ( munmap( start, size + 2 ) ) { perror( "munmap" ); exit( EXIT_FAILURE ); }; // update on disk
	} // if

	argv[0] = "as";
	execvp( argv[0], (char * const *)argv );			// should not return
	perror( "CFA Translator error: cpp level, execvp" );
	exit( EXIT_FAILURE );								// tell gcc not to go any further
} // main

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "g++ -Wall -Wextra as.c -o as" //
// End: //
