//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// main.cpp --
//
// Author           : Peter Buhr and Rob Schluntz
// Created On       : Fri May 15 23:12:02 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Jun 23 16:38:09 2024
// Update Count     : 691
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

#include "AST/Pass.hpp"                     // for pass_visitor_stats
#include "AST/Print.hpp"                    // for printAll
#include "AST/TranslationUnit.hpp"          // for TranslationUnit
#include "AST/Util.hpp"                     // for checkInvariants
#include "CompilationState.hpp"
#include "../config.h"                      // for CFA_LIBDIR
#include "CodeGen/FixMain.hpp"              // for FixMain
#include "CodeGen/FixNames.hpp"             // for fixNames
#include "CodeGen/Generate.hpp"             // for generate
#include "CodeGen/LinkOnce.hpp"             // for translateLinkOnce
#include "Common/CodeLocationTools.hpp"     // for forceFillCodeLocations
#include "Common/DeclStats.hpp"             // for printDeclStats
#include "Common/ResolvProtoDump.hpp"       // for dumpAsResolverProto
#include "Common/Stats.hpp"                 // for Stats
#include "Common/Utility.hpp"               // for deleteAll, filter, printAll
#include "Concurrency/Actors.hpp"           // for implementActors
#include "Concurrency/Corun.hpp"            // for implementCorun
#include "Concurrency/Keywords.hpp"         // for implementMutex, implement...
#include "Concurrency/Waitfor.hpp"          // for generateWaitfor
#include "Concurrency/Waituntil.hpp"        // for generateWaitUntil
#include "ControlStruct/ExceptDecl.hpp"     // for translateExcept
#include "ControlStruct/ExceptTranslate.hpp"// for translateThrows, translat...
#include "ControlStruct/FixLabels.hpp"      // for fixLabels
#include "ControlStruct/HoistControlDecls.hpp" //  hoistControlDecls
#include "ControlStruct/TranslateEnumRange.hpp" // translateEnumRange
#include "GenPoly/Box.hpp"                  // for box
#include "GenPoly/InstantiateGeneric.hpp"   // for instantiateGeneric
#include "GenPoly/Lvalue.hpp"               // for convertLvalue
#include "GenPoly/Specialize.hpp"           // for convertSpecializations
#include "InitTweak/FixInit.hpp"            // for fix
#include "InitTweak/GenInit.hpp"            // for genInit
#include "MakeLibCfa.hpp"                   // for makeLibCfa
#include "Parser/RunParser.hpp"             // for buildList, dumpParseTree,...
#include "ResolvExpr/CandidatePrinter.hpp"  // for printCandidates
#include "ResolvExpr/EraseWith.hpp"         // for eraseWith
#include "ResolvExpr/Resolver.hpp"          // for resolve
#include "Tuples/Tuples.hpp"                // for expandMemberTuples, expan...
#include "Validate/Autogen.hpp"             // for autogenerateRoutines
#include "Validate/CompoundLiteral.hpp"     // for handleCompoundLiterals
#include "Validate/EliminateTypedef.hpp"    // for eliminateTypedef
#include "Validate/EnumAndPointerDecay.hpp" // for decayEnumsAndPointers
#include "Validate/FindSpecialDecls.hpp"    // for findGlobalDecls
#include "Validate/FixQualifiedTypes.hpp"   // for fixQualifiedTypes
#include "Validate/FixReturnTypes.hpp"      // for fixReturnTypes
#include "Validate/ForallPointerDecay.hpp"  // for decayForallPointers
#include "Validate/GenericParameter.hpp"    // for fillGenericParameters, tr...
#include "Validate/HoistStruct.hpp"         // for hoistStruct
#include "Validate/HoistTypeDecls.hpp"      // for hoistTypeDecls
#include "Validate/ImplementEnumFunc.hpp"   // for implementEnumFunc
#include "Validate/InitializerLength.hpp"   // for setLengthFromInitializer
#include "Validate/LabelAddressFixer.hpp"   // for fixLabelAddresses
#include "Validate/LinkInstanceTypes.hpp"   // for linkInstanceTypes
#include "Validate/ReplaceTypedef.hpp"      // for replaceTypedef
#include "Validate/ReturnCheck.hpp"         // for checkReturnStatements
#include "Validate/VerifyCtorDtorAssign.hpp" // for verifyCtorDtorAssign
#include "Virtual/ExpandCasts.hpp"          // for expandCasts
#include "Virtual/VirtualDtor.hpp"          // for implementVirtDtors

using namespace std;

static void NewPass( const char * const name ) {
	Stats::Heap::newPass( name );
	using namespace Stats::Counters;
	{
		static auto group = build<CounterGroup>( "Pass Visitor Template" );
		auto pass = build<CounterGroup>( name, group );
		ast::pass_visitor_stats.depth = 0;
		ast::pass_visitor_stats.avg = build<AverageCounter<double>>( "Average Depth", pass );
		ast::pass_visitor_stats.max = build<MaxCounter<double>>( "Max Depth", pass );
	}
}

#define PASS( name, pass, unit, ... )       \
	if ( errorp ) { cerr << name << endl; } \
	NewPass(name);                          \
	Stats::Time::StartBlock(name);          \
	pass(unit,##__VA_ARGS__);               \
	Stats::Time::StopBlock();               \
	if ( invariant ) {                      \
		ast::checkInvariants(unit);         \
	}

#define DUMP( cond, unit )                  \
	if ( cond ) {                           \
		dump( std::move( unit ) );          \
		return EXIT_SUCCESS;                \
	}

static bool waiting_for_gdb = false;					// flag to set cfa-cpp to wait for gdb on start

static string PreludeDirector = "";

static void parse_cmdline( int argc, char * argv[] );
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
	sigemptyset( &act.sa_mask );
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
	ast::TranslationUnit transUnit;

	Signal( SIGSEGV, sigSegvBusHandler, SA_SIGINFO );
	Signal( SIGBUS, sigSegvBusHandler, SA_SIGINFO );
	Signal( SIGFPE, sigFpeHandler, SA_SIGINFO );
	Signal( SIGABRT, sigAbortHandler, SA_SIGINFO );

	// cout << "main" << endl;
	// for ( int i = 0; i < argc; i += 1 ) {
	// 	cout << '\t' << argv[i] << endl;
	// } // for

	parse_cmdline( argc, argv );						// process command-line arguments

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
			FILE * gcc_builtins = fopen( (PreludeDirector + "/gcc-builtins.cfa").c_str(), "r" );
			assertf( gcc_builtins, "cannot open gcc-builtins.cfa\n" );
			parse( gcc_builtins, ast::Linkage::Compiler );

			// read the extra prelude in, if not generating the cfa library
			FILE * extras = fopen( (PreludeDirector + "/extras.cfa").c_str(), "r" );
			assertf( extras, "cannot open extras.cfa\n" );
			parse( extras, ast::Linkage::BuiltinC );
			if ( ! libcfap ) {
				// read the prelude in, if not generating the cfa library
				FILE * prelude = fopen( (PreludeDirector + "/prelude.cfa").c_str(), "r" );
				assertf( prelude, "cannot open prelude.cfa\n" );
				parse( prelude, ast::Linkage::Intrinsic );

				// Read to cfa builtins, if not generating the cfa library
				FILE * builtins = fopen( (PreludeDirector + "/builtins.cfa").c_str(), "r" );
				assertf( builtins, "cannot open builtins.cfa\n" );
				parse( builtins, ast::Linkage::BuiltinCFA );
			} // if
		} // if

		parse( input, libcfap ? ast::Linkage::Intrinsic : ast::Linkage::Cforall, yydebug );

		transUnit = buildUnit();

		DUMP( astp, transUnit );

		Stats::Time::StopBlock();

		PASS( "Hoist Type Decls", Validate::hoistTypeDecls, transUnit );

		PASS( "Translate Exception Declarations", ControlStruct::translateExcept, transUnit );
		DUMP( exdeclp, transUnit );
		PASS( "Verify Ctor, Dtor & Assign", Validate::verifyCtorDtorAssign, transUnit );
		PASS( "Replace Typedefs", Validate::replaceTypedef, transUnit );
		PASS( "Fix Return Types", Validate::fixReturnTypes, transUnit );
		PASS( "Enum and Pointer Decay", Validate::decayEnumsAndPointers, transUnit );

		PASS( "Link Instance Types", Validate::linkInstanceTypes, transUnit );

		PASS( "Forall Pointer Decay", Validate::decayForallPointers, transUnit );
		PASS( "Fix Qualified Types", Validate::fixQualifiedTypes, transUnit );
		PASS( "Eliminate Typedef", Validate::eliminateTypedef, transUnit );
		PASS( "Hoist Struct", Validate::hoistStruct, transUnit );
		PASS( "Validate Generic Parameters", Validate::fillGenericParameters, transUnit );
		PASS( "Translate Enum Range Expression", ControlStruct::translateEnumRange, transUnit );
		PASS( "Translate Dimensions", Validate::translateDimensionParameters, transUnit );
		PASS( "Generate Enum Attributes Functions", Validate::implementEnumFunc, transUnit );
		PASS( "Check Function Returns", Validate::checkReturnStatements, transUnit );
		PASS( "Fix Return Statements", InitTweak::fixReturnStatements, transUnit );
		PASS( "Implement Concurrent Keywords", Concurrency::implementKeywords, transUnit );
		PASS( "Fix Unique Ids", Validate::fixUniqueIds, transUnit );
		PASS( "Implement Corun", Concurrency::implementCorun, transUnit );
		PASS( "Fix Label Address", Validate::fixLabelAddresses, transUnit );
		PASS( "Fix Labels", ControlStruct::fixLabels, transUnit );
		PASS( "Hoist Control Declarations", ControlStruct::hoistControlDecls, transUnit );

		PASS( "Generate Autogen Routines", Validate::autogenerateRoutines, transUnit );

		PASS( "Implement Actors", Concurrency::implementActors, transUnit );
		PASS( "Implement Virtual Destructors", Virtual::implementVirtDtors, transUnit );
		PASS( "Implement Mutex", Concurrency::implementMutex, transUnit );
		PASS( "Implement Thread Start", Concurrency::implementThreadStarter, transUnit );
		PASS( "Compound Literal", Validate::handleCompoundLiterals, transUnit );
		PASS( "Set Length From Initializer", Validate::setLengthFromInitializer, transUnit );
		PASS( "Find Global Decls", Validate::findGlobalDecls, transUnit );

		if ( symtabp ) {
			return EXIT_SUCCESS;
		} // if

		if ( expraltp ) {
			ResolvExpr::printCandidates( transUnit );
			return EXIT_SUCCESS;
		} // if

		DUMP( validp, transUnit );

		PASS( "Translate Throws", ControlStruct::translateThrows, transUnit );
		PASS( "Implement Waituntil", Concurrency::generateWaitUntil, transUnit  );
		PASS( "Fix Names", CodeGen::fixNames, transUnit );
		PASS( "Gen Init", InitTweak::genInit, transUnit );
		PASS( "Expand Member Tuples" , Tuples::expandMemberTuples, transUnit );

		if ( libcfap ) {
			// Generate the bodies of cfa library functions.
			LibCfa::makeLibCfa( transUnit );
		} // if

		if ( declstatsp ) {
			printDeclStats( transUnit );
			return EXIT_SUCCESS;
		} // if

		DUMP( bresolvep, transUnit );

		if ( resolvprotop ) {
			dumpAsResolverProto( transUnit );
			return EXIT_SUCCESS;
		} // if

		PASS( "Resolve", ResolvExpr::resolve, transUnit );
		DUMP( exprp, transUnit );
		PASS( "Fix Init", InitTweak::fix, transUnit, buildingLibrary() ); // Here
		PASS( "Erase With", ResolvExpr::eraseWith, transUnit );

		// fix ObjectDecl - replaces ConstructorInit nodes
		DUMP( ctorinitp, transUnit );

		// Currently not working due to unresolved issues with UniqueExpr
		PASS( "Expand Unique Expr", Tuples::expandUniqueExpr, transUnit ); // xxx - is this the right place for this? want to expand ASAP so tha, sequent passes don't need to worry about double-visiting a unique expr - needs to go after InitTweak::fix so that copy constructed return declarations are reused

		PASS( "Translate Tries", ControlStruct::translateTries, transUnit );
		PASS( "Gen Waitfor", Concurrency::generateWaitFor, transUnit );
		PASS( "Fix Main Linkage", CodeGen::fixMainLinkage, transUnit, !nomainp );

		// Needs to happen before tuple types are expanded.
		PASS( "Convert Specializations",  GenPoly::convertSpecializations, transUnit );

		PASS( "Expand Tuples", Tuples::expandTuples, transUnit );
		DUMP( tuplep, transUnit );

		// Must come after Translate Tries.
		PASS( "Virtual Expand Casts", Virtual::expandCasts, transUnit );

		PASS( "Instantiate Generics", GenPoly::instantiateGeneric, transUnit );
		DUMP( genericsp, transUnit );

		PASS( "Convert L-Value", GenPoly::convertLvalue, transUnit );
		DUMP( bboxp, transUnit );
		PASS( "Box", GenPoly::box, transUnit );
		PASS( "Link-Once", CodeGen::translateLinkOnce, transUnit );

		// Code has been lowered to C, now we can start generation.

		DUMP( bcodegenp, transUnit );

		if ( optind < argc ) {							// any commands after the flags and input file ? => output file name
			output = new ofstream( argv[ optind ] );
		} // if

		PASS( "Code Gen", CodeGen::generate, transUnit, *output, !genproto, prettycodegenp, true, linemarks, false );
		CodeGen::fixMainInvoke( transUnit, *output, (PreludeDirector + "/bootloader.c").c_str() );

		if ( output != &cout ) {
			delete output;
		} // if
	} catch ( SemanticErrorException & e ) {
		if ( errorp ) {
			cerr << "---AST at error:---" << endl;
			dump( std::move( transUnit ), cerr );
			cerr << endl << "---End of AST, begin error message:---\n" << endl;
		} // if
		e.print();
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

	// This pseudo-pass is used to get more accurate heap statistics.
	NewPass("Clean-up");
	Stats::Time::StartBlock("Clean-Up");
	transUnit.global = ast::TranslationGlobal();
	transUnit.decls.clear();
	Stats::Time::StopBlock();

	Stats::print();
	return EXIT_SUCCESS;
} // main


static const char optstring[] = ":c:ghilLmNnpdP:S:twW:D:";

enum { PreludeDir = 128 };
static struct option long_opts[] = {
	{ "colors", required_argument, nullptr, 'c' },
	{ "gdb", no_argument, nullptr, 'g' },
	{ "help", no_argument, nullptr, 'h' },
	{ "invariant", no_argument, nullptr, 'i' },
	{ "libcfa", no_argument, nullptr, 'l' },
	{ "linemarks", no_argument, nullptr, 'L' },
	{ "no-main", no_argument, nullptr, 'm' },
	{ "no-linemarks", no_argument, nullptr, 'N' },
	{ "no-prelude", no_argument, nullptr, 'n' },
	{ "prototypes", no_argument, nullptr, 'p' },
	{ "deterministic-out", no_argument, nullptr, 'd' },
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
	"invariant checking during AST passes",				// -i
	"generate libcfa.c",								// -l
	"generate line marks",								// -L
	"do not replace main",								// -m
	"do not generate line marks",						// -N
	"do not read prelude",								// -n
	"do not generate prelude prototypes => prelude not printed", // -p
	"only print deterministic output",                  // -d
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
	{ "declstats", declstatsp, true, "print code property statistics" },
	{ "parse", yydebug, true, "print yacc (parsing) debug information" },
	{ "pretty", prettycodegenp, true, "prettyprint for ascodegen flag" },
	{ "rproto", resolvprotop, true, "resolver-proto instance" },
	{ "rsteps", resolvep, true, "print resolver steps" },
	// AST dumps
	{ "ast", astp, true, "print AST after parsing" },
	{ "excpdecl", exdeclp, true, "print AST after translating exception decls" },
	{ "symevt", symtabp, true, "print AST after symbol table events" },
	{ "expralt", expraltp, true, "print AST after expressions alternatives" },
	{ "valdecl", validp, true, "print AST after declaration validation pass" },
	{ "bresolver", bresolvep, true, "print AST before resolver step" },
	{ "expranly", exprp, true, "print AST after expression analysis" },
	{ "ctordtor", ctorinitp, true, "print AST after ctor/dtor are replaced" },
	{ "tuple", tuplep, true, "print AST after tuple expansion" },
	{ "instgen", genericsp, true, "print AST after instantiate generics" },
	{ "bbox", bboxp, true, "print AST before box pass" },
	{ "bcodegen", bcodegenp, true, "print AST before code generation" }
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
		  case 'i':										// invariant checking
			invariant = true;
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

static bool notPrelude( ast::ptr<ast::Decl> & decl ) {
	return !decl->linkage.is_builtin;
}

static void dump( ast::TranslationUnit && unit, std::ostream & out ) {
	// May filter out all prelude declarations.
	if ( genproto ) {
		std::list<ast::ptr<ast::Decl>> decls;
		std::copy_if( unit.decls.begin(), unit.decls.end(),
			std::back_inserter( decls ), notPrelude );
		decls.swap( unit.decls );
	}

	// May print as full dump or as code generation.
	if ( codegenp ) {
		CodeGen::generate( unit, out, !genproto, prettycodegenp, false, false, false );
	} else {
		ast::printAll( out, unit.decls );
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End:  //
