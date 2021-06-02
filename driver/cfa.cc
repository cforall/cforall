//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// cfa.cc --
//
// Author           : Peter A. Buhr
// Created On       : Tue Aug 20 13:44:49 2002
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jan 16 07:30:19 2021
// Update Count     : 442
//

#include <iostream>
#include <cstdio>										// perror
#include <cstdlib>										// putenv, exit
#include <climits>										// PATH_MAX
#include <string>										// STL version
#include <algorithm>									// find

#include <unistd.h>										// execvp
#include <sys/types.h>
#include <sys/stat.h>

#include "Common/SemanticError.h"
#include "config.h"										// configure info

using std::cerr;
using std::endl;
using std::string;
using std::to_string;

//#define __DEBUG_H__

#define xstr(s) str(s)
#define str(s) #s

static string __CFA_FLAGPREFIX__( "__CFA_FLAG" );		// "__CFA_FLAG__=" suffix

static void Putenv( char * argv[], string arg ) {
	// environment variables must have unique names
	static int flags = 0;

	if ( putenv( (char *)( *new string( string( __CFA_FLAGPREFIX__ + to_string( flags++ ) + "__=" ) + arg ) ).c_str() ) ) {
		cerr << argv[0] << " error, cannot set environment variable." << endl;
		exit( EXIT_FAILURE );
	} // if
} // Putenv

static bool prefix( const string & arg, const string & pre ) { // check if string has prefix
	return arg.substr( 0, pre.size() ) == pre;
} // prefix

static inline bool ends_with(const string & str, const string & sfix) {
	if (sfix.size() > str.size()) return false;
	return std::equal(str.rbegin(), str.rbegin() + sfix.size(), sfix.rbegin(), sfix.rend());
}

// check if string has suffix
static bool suffix( const string & arg ) {
	enum { NumSuffixes = 3 };
	static const string suffixes[NumSuffixes] = { "cfa", "hfa", "ifa" };

	size_t dot = arg.find_last_of( "." );
	if ( dot == string::npos ) return false;
	const string * end = suffixes + NumSuffixes;
	return std::find( suffixes, end, arg.substr( dot + 1 ) ) != end;
} // suffix

static inline bool dirExists( const string & path ) {	// check if directory exists
    struct stat info;
    if ( stat( path.c_str(), &info ) != 0 ) return false;
	return (info.st_mode & S_IFDIR) != 0;
} // dirExists

static inline string dir(const string & path) {
	return path.substr(0, path.find_last_of('/'));
} // dir

// Different path modes
enum PathMode {
	Installed,     // cfa is installed, use prefix
	BuildTree,     // cfa is in the tree, use source and build tree
	Distributed    // cfa is distributed, use build tree for includes and executable directory for .cfs
};

// Get path mode from /proc
PathMode FromProc() {
	std::string abspath;
	abspath.resize(PATH_MAX);

	// get executable path from /proc/self/exe
	ssize_t size = readlink("/proc/self/exe", const_cast<char*>(abspath.c_str()), abspath.size());
	if(size <= 0) {
		std::cerr << "Error could not evaluate absolute path from /proc/self/exe" << std::endl;
		std::cerr << "Failed with " << std::strerror(errno) << std::endl;
		std::exit(1);
	}

	// Trim extra characters
	abspath.resize(size);

	// Are we installed
	if(abspath.rfind(CFA_BINDIR  , 0) == 0) { return Installed; }

	// Is this the build tree
	if(abspath.rfind(TOP_BUILDDIR, 0) == 0) { return BuildTree; }

	// Does this look like distcc
	if(abspath.find("/.cfadistcc/") != std::string::npos) { return Distributed; }

	// None of the above? Give up since we don't know where the prelude or include directories are
	std::cerr << "Cannot find required files from excutable path " << abspath << std::endl;
	std::exit(1);
}


int main( int argc, char * argv[] ) {
	string Version( CFA_VERSION_LONG );					// current version number from CONFIG
	string Major( xstr( CFA_VERSION_MAJOR ) ), Minor( xstr( CFA_VERSION_MINOR ) ), Patch( xstr( CFA_VERSION_PATCH ) );

	string installincdir( CFA_INCDIR );					// fixed location of include files
	string installlibdir( CFA_LIBDIR );					// fixed location of cc1 and cfa-cpp commands when installed
	string srcdriverdir ( TOP_BUILDDIR "driver");		// fixed location of cc1 and cfa-cpp commands when in tree

	string heading;										// banner printed at start of cfa compilation
	string arg;											// current command-line argument during command-line parsing
	string bprefix;										// path where gcc looks for compiler steps
	string langstd;										// language standard

	string compiler_path( CFA_BACKEND_CC );				// path/name of C compiler
	string compiler_name;								// name of C compiler

	bool x_flag = false;								// -x flag
	bool nonoptarg = false;								// no non-option arguments specified, i.e., no file names
	bool link = true;									// link stage occurring
	bool verbose = false;								// -v flag
	bool quiet = false;									// -quiet flag
	bool debug = true;									// -debug flag
	bool nolib = false;									// -nolib flag
	bool help = false;									// -help flag
	bool CFA_flag = false;								// -CFA flag
	bool cpp_flag = false;								// -E or -M flag, preprocessor only
	bool std_flag = false;								// -std= flag
	bool noincstd_flag = false;							// -no-include-stdhdr= flag
	bool debugging __attribute(( unused )) = false;		// -g flag
	bool m32 = false;									// -m32 flag
	bool m64 = false;									// -m64 flag
	bool compiling_libs = false;
	int o_file = 0;										// -o filename position

	PathMode path = FromProc();

	const char * args[argc + 100];						// cfa command line values, plus some space for additional flags
	int sargs = 1;										// starting location for arguments in args list
	int nargs = sargs;									// number of arguments in args list; 0 => command name

	const char * libs[argc + 20];						// non-user libraries must come separately, plus some added libraries and flags
	int nlibs = 0;

	#ifdef __DEBUG_H__
	cerr << "CFA:" << endl;
	for ( int i = 1; i < argc; i += 1 ) {
	    cerr << "argv[" << i << "]:\"" << argv[i] << "\"" << endl;
	} // for
	#endif // __DEBUG_H__

	// process command-line arguments

	for ( int i = 1; i < argc; i += 1 ) {
		arg = argv[i];									// convert to string value
		if ( prefix( arg, "-" ) ) {
			// pass through arguments

			if ( arg == "-Xlinker" || arg == "-o" ) {
				args[nargs++] = argv[i];				// pass flag along
				i += 1;
				if ( i == argc ) continue;				// next argument available ?
				args[nargs++] = argv[i];				// pass argument along
				if ( arg == "-o" ) o_file = i;			// remember file

				// CFA specific arguments

			} else if ( strncmp(arg.c_str(), "-XCFA", 5) == 0 ) { // CFA pass through
				if ( arg.size() == 5 ) {
					i += 1;
					if ( i == argc ) continue;			// next argument available ?
					Putenv( argv, argv[i] );
				} else if ( arg[5] == ',' ) {			// CFA specific arguments
					Putenv( argv, argv[i] + 6 );
				} else {								// CFA specific arguments
					args[nargs++] = argv[i];
				} // if
			} else if ( arg == "-CFA" ) {
				CFA_flag = true;						// strip the -CFA flag
				link = false;
				args[nargs++] = "-fsyntax-only";		// stop after stage 2
			} else if ( arg == "-debug" ) {
				debug = true;							// strip the debug flag
			} else if ( arg == "-nodebug" ) {
				debug = false;							// strip the nodebug flag
			} else if ( arg == "-quiet" ) {
				quiet = true;							// strip the quiet flag
			} else if ( arg == "-noquiet" ) {
				quiet = false;							// strip the noquiet flag
			} else if ( arg == "-no-include-stdhdr" ) {
				noincstd_flag = true;					// strip the no-include-stdhdr flag
			} else if ( arg == "-nolib" ) {
				nolib = true;							// strip the nolib flag
			} else if ( arg == "-help" ) {
				help = true;							// strip the help flag
			} else if ( arg == "-nohelp" ) {
				help = false;							// strip the nohelp flag
			} else if ( arg == "-cfalib") {
				compiling_libs = true;
			} else if ( arg == "-compiler" ) {
				// use the user specified compiler
				i += 1;
				if ( i == argc ) continue;				// next argument available ?
				compiler_path = argv[i];
				Putenv( argv, arg + "=" + argv[i] );

				// C specific arguments

			} else if ( arg == "-v" ) {
				verbose = true;							// verbosity required
				args[nargs++] = argv[i];				// pass flag along
			} else if ( arg == "-g" ) {
				debugging = true;						// symbolic debugging required
				args[nargs++] = argv[i];				// pass flag along
			} else if ( arg == "-save-temps" || arg == "--save-temps" ) {
				args[nargs++] = argv[i];				// pass flag along
				Putenv( argv, arg );					// save cfa-cpp output
			} else if ( prefix( arg, "-x" ) ) {			// file suffix ?
				string lang;
				args[nargs++] = argv[i];				// pass flag along
				if ( arg.length() == 2 ) {				// separate argument ?
					i += 1;
					if ( i == argc ) continue;			// next argument available ?
					lang = argv[i];
					args[nargs++] = argv[i];			// pass argument along
				} else {
					lang = arg.substr( 2 );
				} // if
				if ( x_flag ) {
					cerr << argv[0] << " warning, only one -x flag per compile, ignoring subsequent flag." << endl;
				} else {
					x_flag = true;
					Putenv( argv, string( "-x=" ) + lang );
				} // if
			} else if ( prefix( arg, "-std=" ) || prefix( arg, "--std=" ) ) {
				std_flag = true;						// -std=XX provided
				args[nargs++] = argv[i];				// pass flag along
			} else if ( arg == "-w" ) {
				args[nargs++] = argv[i];				// pass flag along
				Putenv( argv, arg );
			} else if ( prefix( arg, "-W" ) ) {			// check before next tests
				if ( arg == "-Werror" || arg == "-Wall" ) {
					args[nargs++] = argv[i];			// pass flag along
					Putenv( argv, argv[i] );
				} else {
					unsigned int adv = prefix( arg, "-Wno-" ) ? 5 : 2;
					args[nargs] = argv[i];				// conditionally pass argument along
					const char * warning = argv[i] + adv; // extract warning
					if ( SemanticWarning_Exist( warning ) ) { // replace the argument for cfa-cpp
						Putenv( argv, arg );
					} // if
					nargs += 1;
				} // if
			} else if ( prefix( arg, "-B" ) ) {
				bprefix = arg.substr(2);				// strip the -B flag
			} else if ( arg == "-c" || arg == "-S" || arg == "-E" || arg == "-M" || arg == "-MM" ) {
				args[nargs++] = argv[i];				// pass flag along
				if ( arg == "-E" || arg == "-M" || arg == "-MM" ) {
					cpp_flag = true;					// cpp only
				} // if
				link = false;                           // no linkage required
			} else if ( arg == "-D" || arg == "-U" || arg == "-I" || arg == "-MF" || arg == "-MT" || arg == "-MQ" ||
						arg == "-include" || arg == "-imacros" || arg == "-idirafter" || arg == "-iprefix" ||
						arg == "-iwithprefix" || arg == "-iwithprefixbefore" || arg == "-isystem" || arg == "-isysroot" ) {
				args[nargs++] = argv[i];				// pass flag along
				i += 1;
				args[nargs++] = argv[i];				// pass argument along
			} else if ( arg[1] == 'l' ) {
				// if the user specifies a library, load it after user code
				libs[nlibs++] = argv[i];
			} else if ( arg == "-m32" ) {
				m32 = true;
				m64 = false;
				args[nargs++] = argv[i];
			} else if ( arg == "-m64" ) {
				m64 = true;
				m32 = false;
				args[nargs++] = argv[i];
			} else {
				// concatenate any other arguments
				args[nargs++] = argv[i];
			} // if
		} else {
			bool cfa = suffix( arg );					// check suffix
			if ( ! x_flag && cfa ) {					// no explicit suffix and cfa suffix ?
				args[nargs++] = "-x";
				args[nargs++] = "c";
			} // if
			args[nargs++] = argv[i];					// concatenate files
			if ( ! x_flag && cfa ) {					// no explicit suffix and cfa suffix ?
				args[nargs++] = "-x";
				args[nargs++] = "none";
			} // if
			nonoptarg = true;
		} // if
	} // for

	#ifdef __x86_64__
	args[nargs++] = "-mcx16";							// allow double-wide CAS
	#endif // __x86_64__

	#ifdef __DEBUG_H__
	cerr << "args:";
	for ( int i = 1; i < nargs; i += 1 ) {
		cerr << " " << args[i];
	} // for
	cerr << endl;
	#endif // __DEBUG_H__

	// -E flag stops at cc1 stage 1, so cfa-cpp in cc1 stage 2 is never executed.
	if ( cpp_flag && CFA_flag ) {
		CFA_flag = false;
		cerr << argv[0] << " warning, both -E and -CFA flags specified, using -E and ignoring -CFA." << endl;
	} // if

	// add the CFA include-library paths, which allow direct access to header files without directory qualification
	string libbase;
	switch(path) {
	  case Installed:
		args[nargs++] = "-I" CFA_INCDIR;
		// do not use during build
		if ( ! noincstd_flag ) {
			args[nargs++] = "-I" CFA_INCDIR "stdhdr";
		} // if
		args[nargs++] = "-I" CFA_INCDIR "concurrency";
		args[nargs++] = "-I" CFA_INCDIR "containers";
		libbase = CFA_LIBDIR;
		break;
	  case BuildTree:
	  case Distributed:
		args[nargs++] = "-I" TOP_SRCDIR "libcfa/src";
		// do not use during build
		if ( ! noincstd_flag ) {
			args[nargs++] = "-I" TOP_SRCDIR "libcfa/src" "/stdhdr";
		} // if
		args[nargs++] = "-I" TOP_SRCDIR "libcfa/src" "/concurrency";
		args[nargs++] = "-I" TOP_SRCDIR "libcfa/src" "/containers";

		libbase = TOP_BUILDDIR "libcfa/";

		break;
	} // if

	// add stdbool to get defines for bool/true/false
	args[nargs++] = "-imacros";
	args[nargs++] = "stdbool.h";

	if( compiling_libs ) {
		Putenv( argv, "-t" );
	} // if

	string arch( m32 ? CFA_32_CPU : (m64 ? CFA_64_CPU : CFA_DEFAULT_CPU) );
	if ( ! m32 && ! m64 ) {
		if ( arch == "x86" ) {
			args[nargs++] = "-m32";
		} else if ( arch == "x64" ) {
			args[nargs++] = "-m64";
		}  // if
	} // if

	const char * config = nolib ? "nolib" : (debug ? "debug": "nodebug");
	string libdir = libbase + arch + "-" + config;

	if ( path != Distributed ) {
		if ( ! nolib && ! dirExists( libdir ) ) {
			cerr << argv[0] << " internal error, configuration " << config << " not installed." << endl;
			cerr << "Was looking for " << libdir << endl;
			for(int i = 1; i < argc; i++) {
				cerr << argv[i] << " ";
			}
			cerr << endl;
			libdir = libbase + arch + "-" + "nolib";
		} // if

		if ( ! dirExists( libdir ) ) {
			cerr << argv[0] << " internal error, cannot find prelude directory." << endl;
			cerr << "Was looking for " << libdir << endl;
			exit( EXIT_FAILURE );
		} // if
	} // if

	string preludedir;
	switch(path) {
	  case Installed   : preludedir = libdir; break;
	  case BuildTree   : preludedir = libdir + "/prelude"; break;
	  case Distributed : preludedir = dir(argv[0]); break;
	} // switch

	Putenv( argv, "--prelude-dir=" + preludedir );
	args[nargs++] = "-include";
	args[nargs++] = (*new string(preludedir + "/defines.hfa")).c_str();

	for ( int i = 0; i < nlibs; i += 1 ) {				// copy non-user libraries after all user libraries
		args[nargs++] = libs[i];
	} // for

	if ( link ) {
		args[nargs++] = "-Xlinker";
		args[nargs++] = "--undefined=__cfaabi_dbg_bits_write";
		args[nargs++] = "-Xlinker";
		args[nargs++] = "--undefined=__cfaabi_interpose_startup";
		args[nargs++] = "-Xlinker";
		args[nargs++] = "--undefined=__cfaabi_appready_startup";
		args[nargs++] = "-z";
		args[nargs++] = "execstack";

		// include the cfa library in case it is needed
		args[nargs++] = ( *new string( string("-L" ) + libdir + (path != Installed ? "/src/.libs" : "")) ).c_str();
		args[nargs++] = ( *new string( string("-Wl,-rpath," ) + libdir + (path != Installed ? "/src/.libs" : "")) ).c_str();
		args[nargs++] = "-Wl,--push-state,--as-needed";
		args[nargs++] = "-lcfathread";
		args[nargs++] = "-Wl,--pop-state";
		args[nargs++] = "-Wl,--push-state,--no-as-needed";
		args[nargs++] = "-lcfa";
		args[nargs++] = "-Wl,--pop-state";
		args[nargs++] = "-pthread";
		#if defined(  __x86_64__ ) || defined( __ARM_ARCH )
		args[nargs++] = "-latomic";						// allow double-wide CAS
		#endif // __x86_64__
		args[nargs++] = "-ldl";
		args[nargs++] = "-lm";
	} // if

	args[nargs++] = "-fexceptions";						// add exception flags (unconditionally)

	// add flags based on the type of compile

	args[nargs++] = ( *new string( string("-D__CFA_MAJOR__=") + Major ) ).c_str();
	args[nargs++] = ( *new string( string("-D__CFA_MINOR__=") + Minor ) ).c_str();
	args[nargs++] = ( *new string( string("-D__CFA_PATCH__=") + Patch ) ).c_str();
	args[nargs++] = "-D__CFA__";
	args[nargs++] = "-D__CFORALL__";
	args[nargs++] = "-D__cforall";

	if ( cpp_flag ) {
		args[nargs++] = "-D__CPP__";
	} // if

	if ( CFA_flag ) {
		Putenv( argv, "-N" );
		Putenv( argv, "-CFA" );
		// -CFA implies cc1 stage 2, but gcc does not pass the -o file to this stage because it believe the file is for
		// the linker. Hence, the -o file is explicit passed to cc1 stage 2 and used as cfa-cpp's output file.
		if ( o_file ) Putenv( argv, string( "-o=" ) + argv[o_file] );
	} else {
		Putenv( argv, "-L" );
	} // if

	if ( debug ) {
		heading += " (debug)";
		args[nargs++] = "-D__CFA_DEBUG__";
	} else {
		heading += " (no debug)";
	} // if

	if ( bprefix.length() == 0 ) {
		switch(path) {
		  case Installed   : bprefix = installlibdir; break;
		  case BuildTree   : bprefix = srcdriverdir ; break;
		  case Distributed : bprefix = dir(argv[0]) ; break;
		} // switch
	} // if
	if ( bprefix[bprefix.length() - 1] != '/' ) bprefix += '/';
	Putenv( argv, string("-B=") + bprefix );

	args[nargs++] = "-Xlinker";							// used by backtrace
	args[nargs++] = "-export-dynamic";

	// execute the compilation command

	args[0] = compiler_path.c_str();					// set compiler command for exec
	// find actual name of the compiler independent of the path to it
	int p = compiler_path.find_last_of( '/' );			// scan r -> l for first '/'
	if ( p == -1 ) {
		compiler_name = compiler_path;
	} else {
		compiler_name = *new string( compiler_path.substr( p + 1 ) );
	} // if

	if ( prefix( compiler_name, "gcc" ) ) {				// allow suffix on gcc name
		args[nargs++] = "-no-integrated-cpp";
		args[nargs++] = "-Wno-deprecated";
		args[nargs++] = "-Wno-strict-aliasing";			// casting from one type to another
		#ifdef HAVE_CAST_FUNCTION_TYPE
		args[nargs++] = "-Wno-cast-function-type";
		#endif // HAVE_CAST_FUNCTION_TYPE
		if ( ! std_flag && ! x_flag ) {
			args[nargs++] = "-std=gnu11";				// default c11, if none specified
		} // if
		args[nargs++] = "-fgnu89-inline";
		args[nargs++] = "-D__int8_t_defined";			// prevent gcc type-size attributes
		args[nargs++] = ( *new string( string("-B") + bprefix ) ).c_str();
	} else {
		cerr << argv[0] << " error, compiler \"" << compiler_name << "\" unsupported." << endl;
		exit( EXIT_FAILURE );
	} // if

	args[nargs] = nullptr;								// terminate

	#ifdef __DEBUG_H__
	cerr << "nargs: " << nargs << endl;
	cerr << "args:" << endl;
	for ( int i = 0; args[i] != nullptr; i += 1 ) {
		cerr << " \"" << args[i] << "\"" << endl;
	} // for
	cerr << endl;
	#endif // __DEBUG_H__

	if ( ! quiet ) {
		cerr << "CFA " << "Version " << Version << heading << endl;
		if ( help ) {
			cerr <<
				"-debug\t\t\t: use cfa runtime with debug checking" << endl <<
				"-help\t\t\t: print this help message" << endl <<
				"-quiet\t\t\t: print no messages from the cfa command" << endl <<
				"-CFA\t\t\t: run the cpp preprocessor and the cfa-cpp translator" << endl <<
				"-XCFA -cfa-cpp-flag\t: pass next flag as-is to the cfa-cpp translator" << endl <<
				"...\t\t\t: any other " << compiler_name << " flags" << endl;
		} // if
	} // if

	if ( verbose ) {
		if ( argc == 2 ) exit( EXIT_SUCCESS );			// if only the -v flag is specified, do not invoke gcc

		for ( int i = 0; args[i] != nullptr; i += 1 ) {
			cerr << args[i] << " ";
		} // for
		cerr << endl;
	} // if

	if ( ! nonoptarg ) {
		cerr << argv[0] << " error, no input files" << endl;
		exit( EXIT_FAILURE );
	} // if

	// execute the command and return the result

	execvp( args[0], (char * const *)args );			// should not return
	perror( "CFA Translator error: execvp" );
	exit( EXIT_FAILURE );
} // main

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
