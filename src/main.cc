//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// main.cc --
//
// Author           : Peter Buhr and Rob Schluntz
// Created On       : Fri May 15 23:12:02 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Mar  6 15:49:00 2021
// Update Count     : 656
//

#include <cxxabi.h>                         // for __cxa_demangle
#include <execinfo.h>                       // for backtrace, backtrace_symbols
#include <getopt.h>                         // for no_argument, optind, geto...
#include <cassert>                          // for assertf
#include <cstdio>                           // for fopen, FILE, fclose, stdin
#include <cstdlib>                          // for exit, free, abort, EXIT_F...
#include <csignal>                          // for signal, SIGABRT, SIGSEGV
#include <cstring>                          // for index
#include <fstream>                          // for ofstream
#include <iostream>                         // for operator<<, basic_ostream
#include <iomanip>
#include <iterator>                         // for back_inserter
#include <list>                             // for list
#include <string>                           // for char_traits, operator<<

using namespace std;

#include "AST/Convert.hpp"
#include "CompilationState.h"
#include "../config.h"                      // for CFA_LIBDIR
#include "CodeGen/FixMain.h"                // for FixMain
#include "CodeGen/FixNames.h"               // for fixNames
#include "CodeGen/Generate.h"               // for generate
#include "CodeGen/LinkOnce.h"               // for translateLinkOnce
#include "CodeTools/DeclStats.h"            // for printDeclStats
#include "CodeTools/ResolvProtoDump.h"      // for dumpAsResolvProto
#include "CodeTools/TrackLoc.h"             // for fillLocations
#include "Common/CodeLocationTools.hpp"     // for forceFillCodeLocations
#include "Common/CompilerError.h"           // for CompilerError
#include "Common/Stats.h"
#include "Common/PassVisitor.h"
#include "Common/SemanticError.h"           // for SemanticError
#include "Common/UnimplementedError.h"      // for UnimplementedError
#include "Common/utility.h"                 // for deleteAll, filter, printAll
#include "Concurrency/Waitfor.h"            // for generateWaitfor
#include "ControlStruct/ExceptTranslate.h"  // for translateEHM
#include "ControlStruct/Mutate.h"           // for mutate
#include "GenPoly/Box.h"                    // for box
#include "GenPoly/InstantiateGeneric.h"     // for instantiateGeneric
#include "GenPoly/Lvalue.h"                 // for convertLvalue
#include "GenPoly/Specialize.h"             // for convertSpecializations
#include "InitTweak/FixInit.h"              // for fix
#include "InitTweak/GenInit.h"              // for genInit
#include "MakeLibCfa.h"                     // for makeLibCfa
#include "Parser/ParseNode.h"               // for DeclarationNode, buildList
#include "Parser/TypedefTable.h"            // for TypedefTable
#include "ResolvExpr/AlternativePrinter.h"  // for AlternativePrinter
#include "ResolvExpr/Resolver.h"            // for resolve
#include "SymTab/Validate.h"                // for validate
#include "SynTree/LinkageSpec.h"            // for Spec, Cforall, Intrinsic
#include "SynTree/Declaration.h"            // for Declaration
#include "SynTree/Visitor.h"                // for acceptAll
#include "Tuples/Tuples.h"                  // for expandMemberTuples, expan...
#include "Virtual/ExpandCasts.h"            // for expandCasts


static void NewPass( const char * const name ) {
	Stats::Heap::newPass( name );
	using namespace Stats::Counters;
	{
		static auto group = build<CounterGroup>( "Pass Visitor" );
		auto pass = build<CounterGroup>( name, group );
		pass_visitor_stats.depth = 0;
		pass_visitor_stats.avg = build<AverageCounter<double>>( "Average Depth", pass );
		pass_visitor_stats.max = build<MaxCounter<double>>( "Max Depth", pass );
	}
	{
		static auto group = build<CounterGroup>( "Syntax Node" );
		auto pass = build<CounterGroup>( name, group );
		BaseSyntaxNode::new_nodes = build<SimpleCounter>( "Allocs", pass );
	}
}

#define PASS( name, pass )                  \
	if ( errorp ) { cerr << name << endl; } \
	NewPass(name);                          \
	Stats::Time::StartBlock(name);          \
	pass;                                   \
	Stats::Time::StopBlock();

LinkageSpec::Spec linkage = LinkageSpec::Cforall;
TypedefTable typedefTable;
DeclarationNode * parseTree = nullptr;					// program parse tree

static bool waiting_for_gdb = false;					// flag to set cfa-cpp to wait for gdb on start

static string PreludeDirector = "";

static void parse_cmdline( int argc, char * argv[] );
static void parse( FILE * input, LinkageSpec::Spec linkage, bool shouldExit = false );
static void dump( list< Declaration * > & translationUnit, ostream & out = cout );
static void dump( ast::TranslationUnit && transUnit, ostream & out = cout );

static void backtrace( int start ) {					// skip first N stack frames
	enum { Frames = 50, };								// maximum number of stack frames
	void * array[Frames];
	size_t size = ::backtrace( array, Frames );
	char ** messages = ::backtrace_symbols( array, size ); // does not demangle names

	*index( messages[0], '(' ) = '\0';					// find executable name
	cerr << "Stack back trace for: " << messages[0] << endl;

	// skip last 2 stack frames after main
	for ( unsigned int i = start; i < size - 2 && messages != nullptr; i += 1 ) {
		char * mangled_name = nullptr, * offset_begin = nullptr, * offset_end = nullptr;

		for ( char * p = messages[i]; *p; p += 1 ) {	// find parantheses and +offset
			if ( *p == '(' ) {
				mangled_name = p;
			} else if ( *p == '+' ) {
				offset_begin = p;
			} else if ( *p == ')' ) {
				offset_end = p;
				break;
			} // if
		} // for

		// if line contains symbol, attempt to demangle
		int frameNo = i - start;
		if ( mangled_name && offset_begin && offset_end && mangled_name < offset_begin ) {
			*mangled_name++ = '\0';						// delimit strings
			*offset_begin++ = '\0';
			*offset_end++ = '\0';

			int status;
			char * real_name = __cxxabiv1::__cxa_demangle( mangled_name, 0, 0, &status );
			// bug in __cxa_demangle for single-character lower-case non-mangled names
			if ( status == 0 ) {						// demangling successful ?
				cerr << "(" << frameNo << ") " << messages[i] << " : "
					 << real_name << "+" << offset_begin << offset_end << endl;
			} else {									// otherwise, output mangled name
				cerr << "(" << frameNo << ") " << messages[i] << " : "
					 << mangled_name << "(/*unknown*/)+" << offset_begin << offset_end << endl;
			} // if

			free( real_name );
		} else {										// otherwise, print the whole line
			cerr << "(" << frameNo << ") " << messages[i] << endl;
		} // if
	} // for

	free( messages );
} // backtrace

#define SIGPARMS int sig __attribute__(( unused )), siginfo_t * sfp __attribute__(( unused )), ucontext_t * cxt __attribute__(( unused ))

static void _Signal(struct sigaction & act, int sig, int flags ) {
	act.sa_flags = flags;

	if ( sigaction( sig, &act, nullptr ) == -1 ) {
	    cerr << "*cfa-cpp compilation error* problem installing signal handler, error(" << errno << ") " << strerror( errno ) << endl;
	    _exit( EXIT_FAILURE );
	} // if
}

static void Signal( int sig, void (* handler)(SIGPARMS), int flags ) {
	struct sigaction act;
	act.sa_sigaction = (void (*)(int, siginfo_t *, void *))handler;
	_Signal(act, sig, flags);
} // Signal

static void Signal( int sig, void (* handler)(int), int flags ) {
	struct sigaction act;
	act.sa_handler = handler;
	_Signal(act, sig, flags);
} // Signal

static void sigSegvBusHandler( SIGPARMS ) {
	if ( sfp->si_addr == nullptr ) {
		cerr << "Null pointer (nullptr) dereference." << endl;
	} else {
		cerr << (sig == SIGSEGV ? "Segment fault" : "Bus error") << " at memory location " << sfp->si_addr << "." << endl
			 << "Possible cause is reading outside the address space or writing to a protected area within the address space with an invalid pointer or subscript." << endl;
	} // if
	backtrace( 2 );										// skip first 2 stack frames
	abort();											// cause core dump for debugging
} // sigSegvBusHandler

static void sigFpeHandler( SIGPARMS ) {
	const char * msg;

	switch ( sfp->si_code ) {
	  case FPE_INTDIV: case FPE_FLTDIV: msg = "divide by zero"; break;
	  case FPE_FLTOVF: msg = "overflow"; break;
	  case FPE_FLTUND: msg = "underflow"; break;
	  case FPE_FLTRES: msg = "inexact result"; break;
	  case FPE_FLTINV: msg = "invalid operation"; break;
	  default: msg = "unknown";
	} // choose
	cerr << "Computation error " << msg << " at location " << sfp->si_addr << endl
		 << "Possible cause is constant-expression evaluation invalid." << endl;
	backtrace( 2 );										// skip first 2 stack frames
	abort();											// cause core dump for debugging
} // sigFpeHandler

static void sigAbortHandler( SIGPARMS ) {
	backtrace( 6 );										// skip first 6 stack frames
	Signal( SIGABRT, SIG_DFL, SA_SIGINFO );	// reset default signal handler
	raise( SIGABRT );									// reraise SIGABRT
} // sigAbortHandler

int main( int argc, char * argv[] ) {
	FILE * input;										// use FILE rather than istream because yyin is FILE
	ostream * output = & cout;
	list< Declaration * > translationUnit;

	Signal( SIGSEGV, sigSegvBusHandler, SA_SIGINFO );
	Signal( SIGBUS, sigSegvBusHandler, SA_SIGINFO );
	Signal( SIGFPE, sigFpeHandler, SA_SIGINFO );
	Signal( SIGABRT, sigAbortHandler, SA_SIGINFO );

	// cout << "main" << endl;
	// for ( int i = 0; i < argc; i += 1 ) {
	// 	cout << '\t' << argv[i] << endl;
	// } // for

	parse_cmdline( argc, argv );						// process command-line arguments
	CodeGen::FixMain::setReplaceMain( !nomainp );

	if ( waiting_for_gdb ) {
		cerr << "Waiting for gdb" << endl;
		cerr << "run :" << endl;
		cerr << "  gdb attach " << getpid() << endl;
		raise(SIGSTOP);
	} // if

	try {
		// choose to read the program from a file or stdin
		if ( optind < argc ) {							// any commands after the flags ? => input file name
			input = fopen( argv[ optind ], "r" );
			assertf( input, "cannot open %s because %s\n", argv[ optind ], strerror( errno ) );
			optind += 1;
		} else {										// no input file name
			input = stdin;
		} // if

		Stats::Time::StartGlobal();
		NewPass("Parse");
		Stats::Time::StartBlock("Parse");

		// read in the builtins, extras, and the prelude
		if ( ! nopreludep ) {							// include gcc builtins
			// -l is for initial build ONLY and builtins.cf is not in the lib directory so access it here.

			assertf( !PreludeDirector.empty(), "Can't find prelude without option --prelude-dir must be used." );

			// Read to gcc builtins, if not generating the cfa library
			FILE * gcc_builtins = fopen( (PreludeDirector + "/gcc-builtins.cf").c_str(), "r" );
			assertf( gcc_builtins, "cannot open gcc-builtins.cf\n" );
			parse( gcc_builtins, LinkageSpec::Compiler );

			// read the extra prelude in, if not generating the cfa library
			FILE * extras = fopen( (PreludeDirector + "/extras.cf").c_str(), "r" );
			assertf( extras, "cannot open extras.cf\n" );
			parse( extras, LinkageSpec::BuiltinC );

			if ( ! libcfap ) {
				// read the prelude in, if not generating the cfa library
				FILE * prelude = fopen( (PreludeDirector + "/prelude.cfa").c_str(), "r" );
				assertf( prelude, "cannot open prelude.cfa\n" );
				parse( prelude, LinkageSpec::Intrinsic );

				// Read to cfa builtins, if not generating the cfa library
				FILE * builtins = fopen( (PreludeDirector + "/builtins.cf").c_str(), "r" );
				assertf( builtins, "cannot open builtins.cf\n" );
				parse( builtins, LinkageSpec::BuiltinCFA );
			} // if
		} // if

		parse( input, libcfap ? LinkageSpec::Intrinsic : LinkageSpec::Cforall, yydebug );

		if ( parsep ) {
			parseTree->printList( cout );
			delete parseTree;
			return EXIT_SUCCESS;
		} // if

		buildList( parseTree, translationUnit );
		delete parseTree;
		parseTree = nullptr;

		if ( astp ) {
			dump( translationUnit );
			return EXIT_SUCCESS;
		} // if

		// Temporary: fill locations after parsing so that every node has a location, for early error messages.
		// Eventually we should pass the locations from the parser to every node, but this quick and dirty solution
		// works okay for now.
		CodeTools::fillLocations( translationUnit );
		Stats::Time::StopBlock();

		// add the assignment statement after the initialization of a type parameter
		PASS( "Validate", SymTab::validate( translationUnit, symtabp ) );
		if ( symtabp ) {
			deleteAll( translationUnit );
			return EXIT_SUCCESS;
		} // if

		if ( expraltp ) {
			PassVisitor<ResolvExpr::AlternativePrinter> printer( cout );
			acceptAll( translationUnit, printer );
			return EXIT_SUCCESS;
		} // if

		if ( validp ) {
			dump( translationUnit );
			return EXIT_SUCCESS;
		} // if

		PASS( "Translate Throws", ControlStruct::translateThrows( translationUnit ) );
		PASS( "Fix Labels", ControlStruct::fixLabels( translationUnit ) );
		PASS( "Fix Names", CodeGen::fixNames( translationUnit ) );
		PASS( "Gen Init", InitTweak::genInit( translationUnit ) );
		PASS( "Expand Member Tuples" , Tuples::expandMemberTuples( translationUnit ) );
		if ( libcfap ) {
			// generate the bodies of cfa library functions
			LibCfa::makeLibCfa( translationUnit );
		} // if

		if ( declstatsp ) {
			CodeTools::printDeclStats( translationUnit );
			deleteAll( translationUnit );
			return EXIT_SUCCESS;
		} // if

		if ( bresolvep ) {
			dump( translationUnit );
			return EXIT_SUCCESS;
		} // if

		CodeTools::fillLocations( translationUnit );

		if ( resolvprotop ) {
			CodeTools::dumpAsResolvProto( translationUnit );
			return EXIT_SUCCESS;
		} // if

		if( useNewAST ) {
			if (Stats::Counters::enabled) {
				ast::pass_visitor_stats.avg = Stats::Counters::build<Stats::Counters::AverageCounter<double>>("Average Depth - New");
				ast::pass_visitor_stats.max = Stats::Counters::build<Stats::Counters::MaxCounter<double>>("Max depth - New");
			}
			auto transUnit = convert( move( translationUnit ) );
			PASS( "Resolve", ResolvExpr::resolve( transUnit ) );
			if ( exprp ) {
				dump( move( transUnit ) );
				return EXIT_SUCCESS;
			} // if

			forceFillCodeLocations( transUnit );

			PASS( "Fix Init", InitTweak::fix(transUnit, buildingLibrary()));
			translationUnit = convert( move( transUnit ) );
		} else {
			PASS( "Resolve", ResolvExpr::resolve( translationUnit ) );
			if ( exprp ) {
				dump( translationUnit );
				return EXIT_SUCCESS;
			}

			PASS( "Fix Init", InitTweak::fix( translationUnit, buildingLibrary() ) );
		}

		// fix ObjectDecl - replaces ConstructorInit nodes
		if ( ctorinitp ) {
			dump ( translationUnit );
			return EXIT_SUCCESS;
		} // if

		PASS( "Expand Unique Expr", Tuples::expandUniqueExpr( translationUnit ) ); // xxx - is this the right place for this? want to expand ASAP so tha, sequent passes don't need to worry about double-visiting a unique expr - needs to go after InitTweak::fix so that copy constructed return declarations are reused

		PASS( "Translate Tries" , ControlStruct::translateTries( translationUnit ) );

		PASS( "Gen Waitfor" , Concurrency::generateWaitFor( translationUnit ) );

		PASS( "Convert Specializations",  GenPoly::convertSpecializations( translationUnit ) ); // needs to happen before tuple types are expanded

		PASS( "Expand Tuples", Tuples::expandTuples( translationUnit ) ); // xxx - is this the right place for this?

		if ( tuplep ) {
			dump( translationUnit );
			return EXIT_SUCCESS;
		} // if

		PASS( "Virtual Expand Casts", Virtual::expandCasts( translationUnit ) ); // Must come after translateEHM

		PASS( "Instantiate Generics", GenPoly::instantiateGeneric( translationUnit ) );
		if ( genericsp ) {
			dump( translationUnit );
			return EXIT_SUCCESS;
		} // if

		PASS( "Convert L-Value", GenPoly::convertLvalue( translationUnit ) );

		if ( bboxp ) {
			dump( translationUnit );
			return EXIT_SUCCESS;
		} // if
		PASS( "Box", GenPoly::box( translationUnit ) );

		PASS( "Link-Once", CodeGen::translateLinkOnce( translationUnit ) );

		// Code has been lowered to C, now we can start generation.

		if ( bcodegenp ) {
			dump( translationUnit );
			return EXIT_SUCCESS;
		} // if

		if ( optind < argc ) {							// any commands after the flags and input file ? => output file name
			output = new ofstream( argv[ optind ] );
		} // if

		CodeTools::fillLocations( translationUnit );
		PASS( "Code Gen", CodeGen::generate( translationUnit, *output, ! genproto, prettycodegenp, true, linemarks ) );

		CodeGen::FixMain::fix( *output, (PreludeDirector + "/bootloader.c").c_str() );
		if ( output != &cout ) {
			delete output;
		} // if
	} catch ( SemanticErrorException & e ) {
		if ( errorp ) {
			cerr << "---AST at error:---" << endl;
			dump( translationUnit, cerr );
			cerr << endl << "---End of AST, begin error message:---\n" << endl;
		} // if
		e.print();
		if ( output != &cout ) {
			delete output;
		} // if
		return EXIT_FAILURE;
	} catch ( UnimplementedError & e ) {
		cout << "Sorry, " << e.get_what() << " is not currently implemented" << endl;
		if ( output != &cout ) {
			delete output;
		} // if
		return EXIT_FAILURE;
	} catch ( CompilerError & e ) {
		cerr << "Compiler Error: " << e.get_what() << endl;
		cerr << "(please report bugs to [REDACTED])" << endl;
		if ( output != &cout ) {
			delete output;
		} // if
		return EXIT_FAILURE;
	} catch ( std::bad_alloc & ) {
		cerr << "*cfa-cpp compilation error* std::bad_alloc" << endl;
		backtrace( 1 );
		abort();
	} catch ( ... ) {
		exception_ptr eptr = current_exception();
		try {
			if (eptr) {
				rethrow_exception(eptr);
			} else {
				cerr << "*cfa-cpp compilation error* exception uncaught and unknown" << endl;
			} // if
		} catch( const exception & e ) {
			cerr << "*cfa-cpp compilation error* uncaught exception \"" << e.what() << "\"\n";
		} // try
		return EXIT_FAILURE;
	} // try

	deleteAll( translationUnit );
	Stats::print();
	return EXIT_SUCCESS;
} // main


static const char optstring[] = ":c:ghlLmNnpdOAP:S:twW:D:";

enum { PreludeDir = 128 };
static struct option long_opts[] = {
	{ "colors", required_argument, nullptr, 'c' },
	{ "gdb", no_argument, nullptr, 'g' },
	{ "help", no_argument, nullptr, 'h' },
	{ "libcfa", no_argument, nullptr, 'l' },
	{ "linemarks", no_argument, nullptr, 'L' },
	{ "no-main", no_argument, 0, 'm' },
	{ "no-linemarks", no_argument, nullptr, 'N' },
	{ "no-prelude", no_argument, nullptr, 'n' },
	{ "prototypes", no_argument, nullptr, 'p' },
	{ "deterministic-out", no_argument, nullptr, 'd' },
	{ "old-ast", no_argument, nullptr, 'O'},
	{ "new-ast", no_argument, nullptr, 'A'},
	{ "print", required_argument, nullptr, 'P' },
	{ "prelude-dir", required_argument, nullptr, PreludeDir },
	{ "statistics", required_argument, nullptr, 'S' },
	{ "tree", no_argument, nullptr, 't' },
	{ "", no_argument, nullptr, 0 },					// -w
	{ "", no_argument, nullptr, 0 },					// -W
	{ "", no_argument, nullptr, 0 },					// -D
	{ nullptr, 0, nullptr, 0 }
}; // long_opts

static const char * description[] = {
	"diagnostic color: never, always, auto",			// -c
	"wait for gdb to attach",							// -g
	"print translator help message",					// -h
	"generate libcfa.c",								// -l
	"generate line marks",								// -L
	"do not replace main",								// -m
	"do not generate line marks",						// -N
	"do not read prelude",								// -n
	"do not generate prelude prototypes => prelude not printed", // -p
	"only print deterministic output",                  // -d
	"Use the old-ast",									// -O
	"Use the new-ast",									// -A
	"print",											// -P
	"<directory> prelude directory for debug/nodebug",	// no flag
	"<option-list> enable profiling information: counters, heap, time, all, none", // -S
	"building cfa standard lib",						// -t
	"",													// -w
	"",													// -W
	"",													// -D
}; // description

static_assert( sizeof( long_opts ) / sizeof( long_opts[0] ) - 1 == sizeof( description ) / sizeof( description[0] ), "Long opts and description must match" );

static struct Printopts {
	const char * name;
	int & flag;
	int val;
	const char * descript;
} printopts[] = {
	{ "ascodegen", codegenp, true, "print AST as codegen rather than AST" },
	{ "asterr", errorp, true, "print AST on error" },
	{ "declstats", declstatsp, true, "code property statistics" },
	{ "parse", yydebug, true, "yacc (parsing) debug information" },
	{ "pretty", prettycodegenp, true, "prettyprint for ascodegen flag" },
	{ "rproto", resolvprotop, true, "resolver-proto instance" },
	{ "rsteps", resolvep, true, "print resolver steps" },
	{ "tree", parsep, true, "print parse tree" },
	// code dumps
	{ "ast", astp, true, "print AST after parsing" },
	{ "symevt", symtabp, true, "print AST after symbol table events" },
	{ "altexpr", expraltp, true, "print alternatives for expressions" },
	{ "astdecl", validp, true, "print AST after declaration validation pass" },
	{ "resolver", bresolvep, true, "print AST before resolver step" },
	{ "astexpr", exprp, true, "print AST after expression analysis" },
	{ "ctordtor", ctorinitp, true, "print AST after ctor/dtor are replaced" },
	{ "tuple", tuplep, true, "print AST after tuple expansion" },
	{ "astgen", genericsp, true, "print AST after instantiate generics" },
	{ "box", bboxp, true, "print AST before box step" },
	{ "codegen", bcodegenp, true, "print AST before code generation" },
};
enum { printoptsSize = sizeof( printopts ) / sizeof( printopts[0] ) };

static void usage( char * argv[] ) {
    cout << "Usage: " << argv[0] << " [options] [input-file (default stdin)] [output-file (default stdout)], where options are:" << endl;
	int i = 0, j = 1;									// j skips starting colon
	for ( ; long_opts[i].name != 0 && optstring[j] != '\0'; i += 1, j += 1 ) {
		if ( long_opts[i].name[0] != '\0' ) {			// hidden option, internal usage only
			if ( strcmp( long_opts[i].name, "prelude-dir" ) != 0 ) { // flag
				cout << "  -" << optstring[j] << ",";
			} else {									// no flag
				j -= 1;									// compensate
				cout << "     ";
			} // if
			cout << " --" << left << setw(12) << long_opts[i].name << "  ";
			if ( strcmp( long_opts[i].name, "print" ) == 0 ) {
				cout << "one of: " << endl;
				for ( int i = 0; i < printoptsSize; i += 1 ) {
					cout << setw(10) << " " << left << setw(10) << printopts[i].name << "  " << printopts[i].descript << endl;
				} // for
			} else {
				cout << description[i] << endl;
			} // if
		} // if
		if ( optstring[j + 1] == ':' ) j += 1;
	} // for
	if ( long_opts[i].name != 0 || optstring[j] != '\0' ) assertf( false, "internal error, mismatch of option flags and names\n" );
    exit( EXIT_FAILURE );
} // usage

static void parse_cmdline( int argc, char * argv[] ) {
	opterr = 0;											// (global) prevent getopt from printing error messages

	bool Wsuppress = false, Werror = false;
	int c;
	while ( (c = getopt_long( argc, argv, optstring, long_opts, nullptr )) != -1 ) {
		switch ( c ) {
		  case 'c':										// diagnostic colors
			if ( strcmp( optarg, "always" ) == 0 ) {
				ErrorHelpers::colors = ErrorHelpers::Colors::Always;
			} else if ( strcmp( optarg, "never" ) == 0 ) {
				ErrorHelpers::colors = ErrorHelpers::Colors::Never;
			} else if ( strcmp( optarg, "auto" ) == 0 ) {
				ErrorHelpers::colors = ErrorHelpers::Colors::Auto;
			} // if
			break;
		  case 'h':										// help message
			usage( argv );								// no return
			break;
		  case 'l':										// generate libcfa.c
			libcfap = true;
			break;
		  case 'L':										// generate line marks
			linemarks = true;
			break;
		  case 'm':										// do not replace main
			nomainp = true;
			break;
		  case 'N':										// do not generate line marks
			linemarks = false;
			break;
		  case 'n':										// do not read prelude
			nopreludep = true;
			break;
		  case 'p':										// generate prototypes for prelude functions
			genproto = true;
			break;
		  case 'd':                                     // don't print non-deterministic output
			deterministic_output = true;
			break;
		  case 'O':                                     // don't print non-deterministic output
			useNewAST = false;
			break;
		  case 'A':                                     // don't print non-deterministic output
			useNewAST = true;
			break;
		  case 'P':										// print options
			for ( int i = 0;; i += 1 ) {
				if ( i == printoptsSize ) {
					cout << "Unknown --print option " << optarg << endl;
					goto Default;
				} // if
				if ( strcmp( optarg, printopts[i].name ) == 0 ) {
					printopts[i].flag = printopts[i].val;
					break;
				} // if
			} // for
			break;
		  case PreludeDir:								// prelude directory for debug/nodebug, hidden
			PreludeDirector = optarg;
			break;
		  case 'S':										// enable profiling information, argument comma separated list of names
			Stats::parse_params( optarg );
			break;
		  case 't':										// building cfa stdlib
			treep = true;
			break;
		  case 'g':										// wait for gdb
			waiting_for_gdb = true;
			break;
		  case 'w':										// suppress all warnings, hidden
			Wsuppress = true;
			break;
		  case 'W':										// coordinate gcc -W with CFA, hidden
			if ( strcmp( optarg, "all" ) == 0 ) {
				SemanticWarning_EnableAll();
			} else if ( strcmp( optarg, "error" ) == 0 ) {
				Werror = true;
			} else {
				char * warning = optarg;
				Severity s;
				if ( strncmp( optarg, "no-", 3 ) == 0 ) {
					warning += 3;
					s = Severity::Suppress;
				} else {
					s = Severity::Warn;
				} // if
				SemanticWarning_Set( warning, s );
			} // if
			break;
		  case 'D':										// ignore -Dxxx, forwarded by cpp, hidden
			break;
		  case '?':										// unknown option
			if ( optopt ) {								// short option ?
				cout << "Unknown option -" << (char)optopt << endl;
			} else {
				cout << "Unknown option " << argv[optind - 1] << endl;
			} // if
			goto Default;
		  case ':':										// missing option
			if ( optopt ) {								// short option ?
				cout << "Missing option for -" << (char)optopt << endl;
			} else {
				cout << "Missing option for " << argv[optind - 1] << endl;
			} // if
			goto Default;
		  Default:
		  default:
			usage( argv );								// no return
		} // switch
	} // while

	if ( Werror ) {
		SemanticWarning_WarningAsError();
	} // if
	if ( Wsuppress ) {
		SemanticWarning_SuppressAll();
	} // if
	// for ( const auto w : WarningFormats ) {
	// 	cout << w.name << ' ' << (int)w.severity << endl;
	// } // for
} // parse_cmdline

static void parse( FILE * input, LinkageSpec::Spec linkage, bool shouldExit ) {
	extern int yyparse( void );
	extern FILE * yyin;
	extern int yylineno;

	::linkage = linkage;								// set globals
	yyin = input;
	yylineno = 1;
	int parseStatus = yyparse();

	fclose( input );
	if ( shouldExit || parseStatus != 0 ) {
		exit( parseStatus );
	} // if
} // parse

static bool notPrelude( Declaration * decl ) {
	return ! LinkageSpec::isBuiltin( decl->get_linkage() );
} // notPrelude

static void dump( list< Declaration * > & translationUnit, ostream & out ) {
	list< Declaration * > decls;

	if ( genproto ) {
		filter( translationUnit.begin(), translationUnit.end(), back_inserter( decls ), notPrelude );
	} else {
		decls = translationUnit;
	} // if

	// depending on commandline options, either generate code or dump the AST
	if ( codegenp ) {
		CodeGen::generate( decls, out, ! genproto, prettycodegenp );
	} else {
		printAll( decls, out );
	} // if
	deleteAll( translationUnit );
} // dump

static void dump( ast::TranslationUnit && transUnit, ostream & out ) {
	std::list< Declaration * > translationUnit = convert( move( transUnit ) );
	dump( translationUnit, out );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End:  //
