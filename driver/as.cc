//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// as.c -- map assembler file, scan for debug information, then language code, and skip N lines forward to code. If
//         code is C dialect, possibly expand file by one character, and replace with Cforall language code.
//
// Author           : Peter A. Buhr
// Created On       : Wed Aug  1 10:49:42 2018
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Dec  8 07:56:12 2021
// Update Count     : 136
//

#include <cstdio>										// perror
#include <cstdlib>										// exit
#include <fcntl.h>										// open
#include <cstring>										// strstr, memmove
#include <unistd.h>										// ftruncate,execvp
#include <sys/stat.h>									// fstat
#include <sys/mman.h>									// mmap

//#define __DEBUG_H__

int main( const int argc, const char * argv[] ) {
	#ifdef __DEBUG_H__
	for ( int i = 0; i < argc; i += 1 ) {
		fprintf( stderr, "%s\n", argv[i] );
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

		char * dcursor;
		if ( (dcursor = strstr( start, ".Ldebug_info0:" ) ) ) { // debug information ?

			if ( char * cursor = strstr( dcursor, ".long\t.LASF" ) ) { // language code ?
				for ( int i = 0; i < 2; i += 1 ) {		// move N (magic) lines forward
					cursor = strstr( cursor, "\n" ) + 1;
				} // for
				cursor -= 2;							// backup over "d\n", where d is a hex digit
				// From elfcpp/dwarf.h in the binutils source tree.
				// DW_LANG_C89 = 0x1, DW_LANG_C = 0x2, DW_LANG_C99 = 0xc, DW_LANG_C11 = 0x1d
				if ( *(cursor - 2) == '0' && *(cursor - 1) == 'x' &&
					 (*cursor == 'c' || *cursor == '1' || *cursor == '2') ) { // C99/C89/C
					// Expand file by one byte to hold 2 character Cforall language code.
					if ( ftruncate( fd, size + 1 ) ) { perror( "ftruncate" ); exit( EXIT_FAILURE ); };
					memmove( cursor + 2, cursor + 1, start + size - cursor - 1 ); // move remaining text 1 character right
				} else if ( *(cursor - 3) == '0' && *(cursor - 2) == 'x' && *(cursor - 1) == '1' && *cursor == 'd' ) { // C11
				} else {
					for ( int i = 0; i < 6; i += 1 ) {	// move N (magic) lines forward
						cursor = strstr( cursor, "\n" ) + 1;
					} // for
					fprintf( stderr, "*** ERROR *** Invalid C language code found in assembler file: %s\n"
							 "Assembler debug information:\n%.*s",
							 argv[argc - 1], (int)(cursor - dcursor), dcursor );
					exit( EXIT_FAILURE );
				} // if

				*(cursor - 1) = '2';					// replace C89/C/C99/C11 language code with CFA code
				*cursor = '5';
			} // if
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
