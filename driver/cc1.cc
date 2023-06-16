//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// cc1.cc --
//
// Author           : Peter A. Buhr
// Created On       : Fri Aug 26 14:23:51 2005
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jun  9 11:36:44 2023
// Update Count     : 423
//

#include <iostream>
using std::cerr;
using std::endl;
#include <string>
using std::string;
#include <algorithm>									// find
#include <cstdio>										// stderr, stdout, perror, fprintf
#include <cstdlib>										// getenv, exit, mkstemp
#include <unistd.h>										// execvp, fork, unlink
#include <sys/wait.h>									// wait
#include <fcntl.h>										// creat


#include "config.h"										// configure info


//#define __DEBUG_H__


static string compiler_path( CFA_BACKEND_CC );			// C compiler path/name
static bool CFA_flag = false;							// -CFA flag
static bool save_temps = false;							// -save-temps flag
static string o_file;
static string bprefix;
static string lang;										// -x flag


static bool prefix( const string & arg, const string & pre ) {
	return arg.substr( 0, pre.size() ) == pre;
} // prefix

static void suffix( const string & arg, const char * args[], int & nargs ) {
	enum { NumSuffixes = 3 };
	static const string suffixes[NumSuffixes] = { "cfa", "hfa", "ifa" };

	size_t dot = arg.find_last_of( "." );
	if ( dot == string::npos ) return;
	const string * end = suffixes + NumSuffixes;
	if ( std::find( suffixes, end, arg.substr( dot + 1 ) ) != end ) {
		args[nargs++] = "-x";
		args[nargs++] = "c";
	} // if
} // suffix


static string __CFA_FLAGPREFIX__( "__CFA_FLAG" );		// "__CFA_FLAG__=" suffix

static void checkEnv1() {								// stage 1
	extern char ** environ;

	for ( int i = 0; environ[i]; i += 1 ) {
		string arg( environ[i] );
		#ifdef __DEBUG_H__
		cerr << "env arg:\"" << arg << "\"" << endl;
		#endif // __DEBUG_H__

		if ( prefix( arg, __CFA_FLAGPREFIX__ ) ) {
			string val( arg.substr( arg.find_first_of( "=" ) + 1 ) );
			if ( prefix( val, "-compiler=" ) ) {
				compiler_path = val.substr( 10 );
			} else if ( prefix( val, "-x=" ) ) {
				lang = val.substr( 3 );
			} // if
		} // if
	} // for
} // checkEnv1


static void checkEnv2( const char * args[], int & nargs ) { // stage 2
	extern char ** environ;

	for ( int i = 0; environ[i]; i += 1 ) {
		string arg( environ[i] );
		#ifdef __DEBUG_H__
		cerr << "env arg:\"" << arg << "\"" << endl;
		#endif // __DEBUG_H__

		if ( prefix( arg, __CFA_FLAGPREFIX__ ) ) {
			string val( arg.substr( arg.find_first_of( "=" ) + 1 ) );
			if ( prefix( val, "-compiler=" ) ) {
				compiler_path = val.substr( 10 );
			} else if ( val == "-CFA" ) {
				CFA_flag = true;
			} else if ( val == "-save-temps" || val == "--save-temps" ) {
				save_temps = true;
			} else if ( prefix( val, "-o=" ) ) {		// output file for -CFA
				o_file = val.substr( 3 );
			} else if ( prefix( val, "-B=" ) ) {		// location of cfa-cpp
				bprefix = val.substr( 3 );
			} else if ( prefix( val, "-x=" ) ) {		// ignore
			} else {									// normal flag for cfa-cpp
				args[nargs++] = ( *new string( arg.substr( arg.find_first_of( "=" ) + 1 ) ) ).c_str();
			} // if
		} // if
	} // for
} // checkEnv2

#define CFA_SUFFIX ".ifa"

static char tmpname[] = P_tmpdir "/CFAXXXXXX" CFA_SUFFIX;
static int tmpfilefd = -1;
static bool startrm = false;

static void rmtmpfile() {
	if ( tmpfilefd == -1 ) return;						// RACE, file created ?

	startrm = true;										// RACE with C-c C-c
	if ( unlink( tmpname ) == -1 ) {					// remove tmpname
		perror ( "CC1 Translator error: failed, unlink" );
		exit( EXIT_FAILURE );
	} // if
	tmpfilefd = -1;										// mark removed
} // rmtmpfile


static void sigTermHandler( int ) {						// C-c C-c
	if ( startrm ) return;								// return and let rmtmpfile finish, and then program finishes

	if ( tmpfilefd != -1 ) {							// RACE, file created ?
		rmtmpfile();									// remove tmpname
	} // if
	exit( EXIT_FAILURE );								// terminate
} // sigTermHandler


static void Stage1( const int argc, const char * const argv[] ) {
	int code;
	string arg;

	const char * cpp_in = nullptr;
	const char * cpp_out = nullptr;

	bool cpp_flag = false;
	bool o_flag = false;

	const char * args[argc + 100];						// leave space for 100 additional cpp command line values
	int nargs = 1;										// number of arguments in args list; 0 => command name

	#ifdef __DEBUG_H__
	cerr << "Stage1" << endl;
	#endif // __DEBUG_H__
	checkEnv1();										// arguments passed via environment variables
	#ifdef __DEBUG_H__
	for ( int i = 1; i < argc; i += 1 ) {
		cerr << "argv[" << i << "]:\"" << argv[i] << "\"" << endl;
	} // for
	#endif // __DEBUG_H__

	// process all the arguments

	for ( int i = 1; i < argc; i += 1 ) {
		arg = argv[i];
		if ( prefix( arg, "-" ) ) {
			// strip g++ flags that are inappropriate or cause duplicates in subsequent passes

			if ( arg == "-quiet" ) {
			} else if ( arg == "-imultilib" || arg == "-imultiarch" ) {
				i += 1;									// and argument
			} else if ( prefix( arg, "-A" ) ) {
			} else if ( prefix( arg, "-D__GNU" ) ) {
				//********
				// GCC 5.6.0 SEPARATED THE -D FROM THE ARGUMENT!
				//********
			} else if ( arg == "-D" && prefix( argv[i + 1], "__GNU" ) ) {
				i += 1;									// and argument

				// strip flags controlling cpp step

			} else if ( arg == "-D__CPP__" ) {
				cpp_flag = true;
			} else if ( arg == "-D" && string( argv[i + 1] ) == "__CPP__" ) {
				i += 1;									// and argument
				cpp_flag = true;

				// all other flags

			} else if ( arg == "-o" ) {
				i += 1;
				o_flag = true;
				cpp_out = argv[i];
			} else {
				args[nargs++] = argv[i];				// pass flag along
				// CPP flags with an argument
				if ( arg == "-D" || arg == "-U" || arg == "-I" || arg == "-MF" || arg == "-MT" || arg == "-MQ" ||
					 arg == "-include" || arg == "-imacros" || arg == "-idirafter" || arg == "-iprefix" ||
					 arg == "-iwithprefix" || arg == "-iwithprefixbefore" || arg == "-isystem" || arg == "-isysroot" ||
					 arg == "-dumpbase-ext" || arg == "-dumpbase"
					) {
					i += 1;
					args[nargs++] = argv[i];			// pass argument along
					#ifdef __DEBUG_H__
					cerr << "argv[" << i << "]:\"" << argv[i] << "\"" << endl;
					#endif // __DEBUG_H__
				} else if ( arg == "-MD" || arg == "-MMD" ) {
					// gcc frontend generates the dependency file-name after the -MD/-MMD flag, but it is necessary to
					// prefix that file name with -MF.
					args[nargs++] = "-MF";				// insert before file
					i += 1;
					args[nargs++] = argv[i];			// pass argument along
					#ifdef __DEBUG_H__
					cerr << "argv[" << i << "]:\"" << argv[i] << "\"" << endl;
					#endif // __DEBUG_H__
				} // if
			} // if
		} else {										// obtain input and possibly output files
			if ( cpp_in == nullptr ) {
				cpp_in = argv[i];
				#ifdef __DEBUG_H__
				cerr << "cpp_in:\"" << cpp_in << "\"" << endl;
				#endif // __DEBUG_H__
			} else if ( cpp_out == nullptr ) {
				cpp_out = argv[i];
				#ifdef __DEBUG_H__
				cerr << "cpp_out:\"" << cpp_out << "\""<< endl;
				#endif // __DEBUG_H__
			} else {
				cerr << "Usage: " << argv[0] << " input-file [output-file] [options]" << endl;
				exit( EXIT_FAILURE );
			} // if
		} // if
	} // for

	#ifdef __DEBUG_H__
	cerr << "args:";
	for ( int i = 1; i < nargs; i += 1 ) {
		cerr << " " << args[i];
	} // for
	if ( cpp_in != nullptr ) cerr << " " << cpp_in;
	if ( cpp_out != nullptr ) cerr << " " << cpp_out;
	cerr << endl;
	#endif // __DEBUG_H__

	if ( cpp_in == nullptr ) {
		cerr << "Usage: " << argv[0] << " input-file [output-file] [options]" << endl;
		exit( EXIT_FAILURE );
	} // if

	if ( cpp_flag ) {
		// The -E flag is specified on the cfa command so only run the preprocessor and output is written to standard
		// output or -o. The call to cfa has a -E so it does not have to be added to the argument list.

		args[0] = compiler_path.c_str();
		if ( lang.size() == 0 ) {
			suffix( cpp_in, args, nargs );				// check suffix
		} else {
			args[nargs++] = "-x";
			args[nargs++] = ( *new string( lang.c_str() ) ).c_str();
		} // if
		args[nargs++] = cpp_in;
		if ( o_flag ) {									// location for output
			args[nargs++] = "-o";
		} // if
		args[nargs++] = cpp_out;
		args[nargs] = nullptr;							// terminate argument list

		#ifdef __DEBUG_H__
		cerr << "nargs: " << nargs << endl;
		for ( int i = 0; args[i] != nullptr; i += 1 ) {
			cerr << args[i] << " ";
		} // for
		cerr << endl;
		#endif // __DEBUG_H__

		execvp( args[0], (char * const *)args );		// should not return
		perror( "CC1 Translator error: stage 1, execvp" );
		exit( EXIT_FAILURE );
	} // if

	// Run the C preprocessor and save the output in the given file.

	if ( fork() == 0 ) {								// child process ?
		// -o xxx.ii cannot be used to write the output file from cpp because no output file is created if cpp detects
		// an error (e.g., cannot find include file). Whereas, output is always generated, even when there is an error,
		// when cpp writes to stdout. Hence, stdout is redirected into the temporary file.
		if ( freopen( cpp_out, "w", stdout ) == nullptr ) { // redirect stdout to output file
			perror( "CC1 Translator error: stage 1, freopen" );
			exit( EXIT_FAILURE );
		} // if

		args[0] = compiler_path.c_str();
		if ( lang.size() == 0 ) {
			suffix( cpp_in, args, nargs );				// check suffix
		} else {
			args[nargs++] = "-x";
			args[nargs++] = ( *new string( lang.c_str() ) ).c_str();
		} // if
		args[nargs++] = cpp_in;							// input to cpp
		args[nargs] = nullptr;							// terminate argument list

		#ifdef __DEBUG_H__
		cerr << "cpp nargs: " << nargs << endl;
		for ( int i = 0; args[i] != nullptr; i += 1 ) {
			cerr << args[i] << " ";
		} // for
		cerr << endl;
		#endif // __DEBUG_H__

		execvp( args[0], (char * const *)args );		// should not return
		perror( "CC1 Translator error: stage 1 cpp, execvp" );
		cerr << " invoked " << args[0] << endl;
		exit( EXIT_FAILURE );
	} // if

	wait( &code );										// wait for child to finish

	#ifdef __DEBUG_H__
	cerr << "return code from cpp:" << WEXITSTATUS(code) << endl;
	#endif // __DEBUG_H__

	if ( WIFSIGNALED(code) ) {							// child failed ?
		rmtmpfile();									// remove tmpname
		cerr << "CC1 Translator error: stage 1, child failed " << WTERMSIG(code) << endl;
		exit( EXIT_FAILURE );
	} // if

	exit( WEXITSTATUS( code ) );						// bad cpp result stops top-level gcc
} // Stage1


static void Stage2( const int argc, const char * const * argv ) {
	int code;
	string arg;

	const char * cpp_in = nullptr;
	const char * cpp_out = nullptr;

	const char * args[argc + 100];						// leave space for 100 additional cfa command line values
	int nargs = 1;										// number of arguments in args list; 0 => command name
	const char * cargs[20];								// leave space for 20 additional cfa-cpp command line values
	int ncargs = 1;										// 0 => command name

	#ifdef __DEBUG_H__
	cerr << "Stage2" << endl;
	#endif // __DEBUG_H__
	checkEnv2( cargs, ncargs );							// arguments passed via environment variables
	#ifdef __DEBUG_H__
	for ( int i = 1; i < argc; i += 1 ) {
		cerr << "argv[" << i << "]:\"" << argv[i] << "\"" << endl;
	} // for
	#endif // __DEBUG_H__

	enum {
		Color_Auto   = 0,
		Color_Always = 1,
		Color_Never  = 2,
	} color_arg = Color_Auto;

	const char * color_names[3] = { "--colors=auto", "--colors=always", "--colors=never" };

	// process all the arguments

	for ( int i = 1; i < argc; i += 1 ) {
		arg = argv[i];
		if ( prefix( arg, "-" ) ) {
			// strip inappropriate flags

			if ( prefix( arg, "-fdiagnostics-color=" ) ) {
				string choice = arg.substr(20);
				if ( choice == "always" ) color_arg = Color_Always;
				else if ( choice == "never" ) color_arg = Color_Never;
				else if ( choice == "auto" ) color_arg = Color_Auto;
			} else if ( arg == "-fno-diagnostics-color" ) {
				color_arg = Color_Auto;
			} // if

			if ( arg == "-quiet" || arg == "-version" || arg == "-fpreprocessed" ||
				 // Currently CFA does not suppose precompiled .h files.
				 prefix( arg, "--output-pch" ) ) {

				// strip inappropriate flags with an argument

			} else if ( arg == "-auxbase" || arg == "-auxbase-strip" ||
						arg == "-dumpbase" || arg == "-dumpbase-ext" || arg == "-dumpdir" ) {
				i += 1;
				#ifdef __DEBUG_H__
				cerr << "arg:\"" << argv[i] << "\"" << endl;
				#endif // __DEBUG_H__

				// all other flags

			} else {
				args[nargs++] = argv[i];				// pass flag along
				if ( arg == "-o" ) {
					i += 1;
					cpp_out = argv[i];
					args[nargs++] = argv[i];			// pass argument along
					#ifdef __DEBUG_H__
					cerr << "arg:\"" << argv[i] << "\"" << endl;
					#endif // __DEBUG_H__
				} // if
			} // if
		} else {										// obtain input and possibly output files
			if ( cpp_in == nullptr ) {
				cpp_in = argv[i];
				#ifdef __DEBUG_H__
				cerr << "cpp_in:\"" << cpp_in << "\"" << endl;
				#endif // __DEBUG_H__
			} else if ( cpp_out == nullptr ) {
				cpp_out = argv[i];
				#ifdef __DEBUG_H__
				cerr << "cpp_out:\"" << cpp_out << "\""<< endl;
				#endif // __DEBUG_H__
			} else {
				cerr << "Usage: " << argv[0] << " more than two files specified" << endl;
				exit( EXIT_FAILURE );
			} // if
		} // if
	} // for

	if ( cpp_in == nullptr ) {
		cerr << "Usage: " << argv[0] << " missing input file" << endl;
		exit( EXIT_FAILURE );
	} // if
	if ( cpp_out == nullptr ) {
		cerr << "Usage: " << argv[0] << " missing output file" << endl;
		exit( EXIT_FAILURE );
	} // if

	// Create a temporary file, if needed, to store output of the cfa-cpp preprocessor. Cannot be created in forked
	// process because variables tmpname and tmpfilefd are cloned.

	string cfa_cpp_out;

	if ( ! CFA_flag ) {									// run compiler ?
		if ( save_temps ) {
			cfa_cpp_out = cpp_in;
			size_t dot = cfa_cpp_out.find_last_of( "." );
			if ( dot == string::npos ) {
				cerr << "CC1 Translator error: stage 2, bad file name " << endl;
				exit( EXIT_FAILURE );
			} // if

			cfa_cpp_out = cfa_cpp_out.substr( 0, dot ) + CFA_SUFFIX;
			if ( creat( cfa_cpp_out.c_str(), 0666 ) == -1 ) {
				perror( "CC1 Translator error: stage 2, creat" );
				exit( EXIT_FAILURE );
			} // if
		} else {
			tmpfilefd = mkstemps( tmpname, 4 );
			if ( tmpfilefd == -1 ) {
				perror( "CC1 Translator error: stage 2, mkstemp" );
				exit( EXIT_FAILURE );
			} // if
			cfa_cpp_out = tmpname;
		} // if
		#ifdef __DEBUG_H__
		cerr << "cfa_cpp_out: " << cfa_cpp_out << endl;
		#endif // __DEBUG_H__
	} // if

	// If -CFA flag specified, run the cfa-cpp preprocessor on the temporary file, and output is written to standard
	// output.  Otherwise, run the cfa-cpp preprocessor on the temporary file and save the result into the output file.

	if ( fork() == 0 ) {								// child runs CFA preprocessor
		cargs[0] = ( *new string( bprefix + "cfa-cpp" ) ).c_str();
		cargs[ncargs++] = cpp_in;

		if ( CFA_flag ) {								// run cfa-cpp ?
			if ( o_file.size() != 0 ) {					// location for output
				cargs[ncargs++] = ( *new string( o_file.c_str() ) ).c_str();
			} // if
		} else {
			cargs[ncargs++] = cfa_cpp_out.c_str();
		} // if

		cargs[ncargs++] = color_names[color_arg];

		cargs[ncargs] = nullptr;						// terminate argument list

		#ifdef __DEBUG_H__
		for ( int i = 0; cargs[i] != nullptr; i += 1 ) {
			cerr << cargs[i] << " ";
		} // for
		cerr << endl;
		#endif // __DEBUG_H__

		execvp( cargs[0], (char * const *)cargs );		// should not return
		perror( "CC1 Translator error: stage 2 cfa-cpp, execvp" );
		cerr << " invoked " << cargs[0] << endl;
		exit( EXIT_FAILURE );
	} // if

	wait( &code );										// wait for child to finish

	if ( WIFSIGNALED(code) ) {							// child failed ?
		rmtmpfile();									// remove tmpname
		cerr << "CC1 Translator error: stage 2, child failed " << WTERMSIG(code) << endl;
		exit( EXIT_FAILURE );
	} // if

	if ( CFA_flag ) {									// no tmpfile created
		exit( WEXITSTATUS( code ) );					// stop regardless of success or failure
	} // if

	#ifdef __DEBUG_H__
	cerr << "return code from cfa-cpp:" << WEXITSTATUS(code) << endl;
	#endif // __DEBUG_H__

	if ( WEXITSTATUS(code) ) {							// child error ?
		rmtmpfile();									// remove tmpname
		exit( WEXITSTATUS( code ) );					// do not continue
	} // if

	#ifdef __DEBUG_H__
	cerr << "args:";
	for ( int i = 1; i < nargs; i += 1 ) {
		cerr << " " << args[i];
	} // for
	cerr << " " << cpp_in << endl;
	#endif // __DEBUG_H__

	if ( fork() == 0 ) {								// child runs gcc
		args[0] = compiler_path.c_str();
		args[nargs++] = "-S";							// only compile and put assembler output in specified file
		args[nargs++] = "-x";
		args[nargs++] = "cpp-output";

		args[nargs++] = cfa_cpp_out.c_str();
		args[nargs] = nullptr;							// terminate argument list

		#ifdef __DEBUG_H__
		cerr << "stage2 nargs: " << nargs << endl;
		for ( int i = 0; args[i] != nullptr; i += 1 ) {
			cerr << args[i] << " ";
		} // for
		cerr << endl;
		#endif // __DEBUG_H__

		execvp( args[0], (char * const *)args );		// should not return
		perror( "CC1 Translator error: stage 2 cc1, execvp" );
		cerr << " invoked " << args[0] << endl;
		exit( EXIT_FAILURE );							// tell gcc not to go any further
	} // if

	wait( &code );										// wait for child to finish
	rmtmpfile();										// remove tmpname

	if ( WIFSIGNALED(code) ) {							// child failed ?
		cerr << "CC1 Translator error: stage 2, child failed " << WTERMSIG(code) << endl;
		exit( EXIT_FAILURE );
	} // if

	#ifdef __DEBUG_H__
	cerr << "return code from gcc cc1:" << WEXITSTATUS(code) << endl;
	#endif // __DEBUG_H__

	exit( WEXITSTATUS( code ) );						// stop regardless of success or failure
} // Stage2


// This program is called twice because of the -no-integrated-cpp. The calls are differentiated by the first
// command-line argument. The first call replaces the traditional cpp pass to preprocess the C program. The second call
// is to the compiler, which is broken into two steps: preprocess again with cfa-cpp and then call gcc to compile the
// doubly preprocessed program.

int main( const int argc, const char * const argv[], __attribute__((unused)) const char * const env[] ) {
	#ifdef __DEBUG_H__
	for ( int i = 0; env[i] != nullptr; i += 1 ) {
		cerr << env[i] << endl;
	} // for
	#endif // __DEBUG_H__

	signal( SIGINT,  sigTermHandler );
	signal( SIGTERM, sigTermHandler );

	string arg( argv[1] );

	// Currently, stage 1 starts with flag -E and stage 2 with flag -fpreprocessed.

	if ( arg == "-E" ) {
		Stage1( argc, argv );
	} else if ( arg == "-fpreprocessed" ) {
		Stage2( argc, argv );
	} else {
		cerr << "Usage: " << argv[0] << " [-E input-file [output-file] ] | [-fpreprocessed input-file output-file] [options]" << endl;
		exit( EXIT_FAILURE );
	} // if
} // main

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
