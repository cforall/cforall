#include <algorithm>
#include <queue>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <utility>
#include <string>
using namespace std;
#include <assert.h>
#include <string.h>										// strlen
#include "config.h"										// configure info

enum Kind {
	Bool,
	Char,
	SignedChar,
	UnsignedChar,
	ShortSignedInt,
	ShortUnsignedInt,
	SignedInt,
	UnsignedInt,
	LongSignedInt,
	LongUnsignedInt,
	LongLongSignedInt,
	LongLongUnsignedInt,
	SignedInt128,
	UnsignedInt128,
	Float16,
	Float16Complex,
	Float32,
	Float32Complex,
	Float,
	FloatComplex,
	// FloatImaginary,
	Float32x,
	Float32xComplex,
	Float64,
	Float64Complex,
	Double,
	DoubleComplex,
	// DoubleImaginary,
	Float64x,
	Float64xComplex,
	Float80,
	Float128,
	Float128Complex,
	uuFloat128,
	LongDouble,
	LongDoubleComplex,
	// LongDoubleImaginary,
	Float128x,
	Float128xComplex,
	NUMBER_OF_BASIC_TYPES,

	Float32x4,											// ARM, gcc-14
	Float64x2,
	Svfloat32,
	Svfloat64,
	Svbool,
};

enum NumSort {											// floating point types act as both signed and unsigned
	Signed = 0x1,
	Unsigned = 0x2,
	Floating = 0x3
};

struct Node {
	Kind basicType;										// basic type
	const char * name;									// basic-type name
	const char * abbrev;								// internal abbreviation (documentation only)
	const char * type;									// actual type name
	const char * mangled;								// mangled abbreviation
	NumSort sign;										// is this a signed integral type?
	int left, middle, right;							// 3-ary tree, -1 => nullptr
	int rank;											// integral rank (C standard 6.3.1.1.1, extended)
} graph[NUMBER_OF_BASIC_TYPES] = {
	{ Bool, "Bool", "B", "_Bool", "b", Signed, Char, SignedChar, -1, 0 }, // root

	{ Char, "Char", "C", "char", "c", Signed, SignedChar, UnsignedChar, ShortSignedInt, 1 },
	{ SignedChar, "SignedChar", "SC", "signed char", "a", Signed, UnsignedChar, ShortSignedInt, -1, 1 },
	{ UnsignedChar, "UnsignedChar", "UC", "unsigned char", "h", Unsigned, ShortUnsignedInt, ShortSignedInt, -1, 1 },

	{ ShortSignedInt, "ShortSignedInt", "SI", "signed short int", "s", Signed, ShortUnsignedInt, SignedInt, -1, 2 },
	{ ShortUnsignedInt, "ShortUnsignedInt", "SUI", "unsigned short int", "t", Unsigned, UnsignedInt, SignedInt, -1, 2 },

	{ SignedInt, "SignedInt", "I", "signed int", "i", Signed, UnsignedInt, LongSignedInt, -1, 3 },
	{ UnsignedInt, "UnsignedInt", "UI", "unsigned int", "j", Unsigned, LongUnsignedInt, LongSignedInt, -1, 3 },

	{ LongSignedInt, "LongSignedInt", "LI", "signed long int", "l", Signed, LongUnsignedInt, LongLongSignedInt, -1, 4 },
	{ LongUnsignedInt, "LongUnsignedInt", "LUI", "unsigned long int", "m", Unsigned, LongLongSignedInt, LongLongUnsignedInt, -1, 4 },

	{ LongLongSignedInt, "LongLongSignedInt", "LLI", "signed long long int", "x", Signed, LongLongUnsignedInt, SignedInt128, -1, 5 },
	{ LongLongUnsignedInt, "LongLongUnsignedInt", "LLUI", "unsigned long long int", "y", Unsigned, SignedInt128, UnsignedInt128, -1, 5 },

	{ SignedInt128, "SignedInt128", "IB", "__int128", "n", Signed, UnsignedInt128, Float16, -1, 6 },
	{ UnsignedInt128, "UnsignedInt128", "UIB", "unsigned __int128", "o", Unsigned, Float16, -1, -1, 6 },

	{ Float16, "Float16", "_FH", "_Float16", "DF16_", Floating, Float32, Float16Complex, -1, 7 },
	{ Float16Complex, "Float16Complex", "_FH", "_Float16 _Complex", "CDF16_", Floating, Float32Complex, -1, -1, 7 },
	{ Float32, "Float32", "_F", "_Float32", "DF32_", Floating, Float, Float32Complex, -1, 8 },
	{ Float32Complex, "Float32Complex", "_FC", "_Float32 _Complex", "CDF32_", Floating, FloatComplex, -1, -1, 8 },
	{ Float, "Float", "F", "float", "f", Floating, Float32x, FloatComplex, -1, 9 },
	{ FloatComplex, "FloatComplex", "FC", "float _Complex", "Cf", Floating, Float32xComplex, -1, -1, 9 },
	// { FloatImaginary, "FloatImaginary", "FI", "float _Imaginary", "If", false, DoubleImaginary, FloatComplex, -1, 9 },

	{ Float32x, "Float32x", "_FX", "_Float32x", "DF32x_", Floating, Float64, Float32xComplex, -1, 10 },
	{ Float32xComplex, "Float32xComplex", "_FXC", "_Float32x _Complex", "CDF32x_", Floating, Float64Complex, -1, -1, 10 },
	{ Float64, "Float64", "FD", "_Float64", "DF64_", Floating, Double, Float64Complex, -1, 11 },
	{ Float64Complex, "Float64Complex", "_FDC", "_Float64 _Complex", "CDF64_", Floating, DoubleComplex, -1, -1, 11 },
	{ Double, "Double", "D", "double", "d", Floating, Float64x, DoubleComplex, -1, 12 },
	{ DoubleComplex, "DoubleComplex", "DC", "double _Complex", "Cd", Floating, Float64xComplex, -1, -1, 12 },
	// { DoubleImaginary, "DoubleImaginary", "DI", "double _Imaginary", "Id", false, LongDoubleImaginary, DoubleComplex, -1, 12 },

	{ Float64x, "Float64x", "F80X", "_Float64x", "DF64x_", Floating, Float80, Float64xComplex, -1, 13 },
	{ Float64xComplex, "Float64xComplex", "_FDXC", "_Float64x _Complex", "CDF64x_", Floating, Float128Complex, -1, -1, 13 },
	{ Float80, "Float80", "F80", "__float80", "Dq", Floating, Float128, Float64xComplex, -1, 14 },
	{ Float128, "Float128", "_FB", "_Float128", "DF128_", Floating, uuFloat128, Float128Complex, -1, 15 },
	{ Float128Complex, "Float128Complex", "_FLDC", "_Float128 _Complex", "CDF128_", Floating, LongDoubleComplex, -1, -1, 15 },
	{ uuFloat128, "uuFloat128", "FB", "__float128", "g", Floating, LongDouble, Float128Complex, -1, 16 },
	{ LongDouble, "LongDouble", "LD", "long double", "e", Floating, Float128x, LongDoubleComplex, -1, 17 },
	{ LongDoubleComplex, "LongDoubleComplex", "LDC", "long double _Complex", "Ce", Floating, Float128xComplex, -1, -1, 17 },
	// { LongDoubleImaginary, "LongDoubleImaginary", "LDI", "long double _Imaginary", "Ie", false, LongDoubleComplex, -1, -1, 17 },

	{ Float128x, "Float128x", "_FBX", "_Float128x", "DF128x_", Floating, Float128xComplex, -1, -1, 18 },
	{ Float128xComplex, "Float128xComplex", "_FLDXC", "_Float128x _Complex", "CDF128x_", Floating, -1, -1, -1, 18 }
}; // graph

static int costMatrix[NUMBER_OF_BASIC_TYPES][NUMBER_OF_BASIC_TYPES];
static int signMatrix[NUMBER_OF_BASIC_TYPES][NUMBER_OF_BASIC_TYPES];
static Kind commonTypeMatrix[NUMBER_OF_BASIC_TYPES][NUMBER_OF_BASIC_TYPES];

void generateCosts( int row ) {
	bool seen[NUMBER_OF_BASIC_TYPES] = { false /*, ... */ };

	struct el_cost {
		int i;
		int path;
		int sign;

		el_cost( int i = 0, int p = 0, int s = 0 ) : i(i), path(p), sign(s) {}

		// reverse the sense for use in largest-on-top priority queue
		bool operator< (const el_cost& o) const {
			return path > o.path || (path == o.path && sign > o.sign);
		}
	};

	// initialize BFS queue with root of traversal
	priority_queue< el_cost > q;
	q.emplace( row, 0, 0 );

	// BFS costs
	do {
		// visit cost element
		int col = q.top().i;
		// skip if already set
		if ( seen[col] ) {
			q.pop();
			continue;
		} else {
			seen[col] = true;
		} // if

		// otherwise set min-costs into matrix
		int cost = q.top().path;
		int scost = q.top().sign;
		costMatrix[row][col] = cost;
		signMatrix[row][col] = scost;
		q.pop();

		// traverse children
		int i = graph[col].left;
		if ( i == -1 ) continue;
		q.emplace( i, cost + 1, scost + ! (graph[col].sign & graph[i].sign) );

		i = graph[col].middle;
		if ( i == -1 ) continue;
		q.emplace( i, cost + 1, scost + !(graph[col].sign & graph[i].sign) );

		i = graph[col].right;
		if ( i == -1 ) continue;
		q.emplace( i, cost + 1, scost + !(graph[col].sign & graph[i].sign) );
	} while ( ! q.empty() );
} // generateCosts

void generateCommonType( int row, int col ) {			// row <= col
	if ( costMatrix[row][col] >= 0 ) {
		// safe conversion from row => col
		commonTypeMatrix[row][col] = commonTypeMatrix[col][row] = graph[col].basicType;
	} else if ( costMatrix[col][row] >= 0 ) {
		// safe conversion from col => row
		commonTypeMatrix[row][col] = commonTypeMatrix[col][row] = graph[row].basicType;
	} else {
		// need to find least common ancestor
		// can cheat a bit here, in that there is always a direct ancestor of the later (col) element
		int i = graph[col].left;
		if ( i == -1 ) assert("invalid ancestor assumption");
		if ( costMatrix[row][i] >= 0 ) {
			commonTypeMatrix[row][col] = commonTypeMatrix[col][row] = graph[i].basicType;
			return;
		} // if

		i = graph[col].middle;
		if ( i == -1 ) assert("invalid ancestor assumption");
		if ( costMatrix[row][i] >= 0 ) {
			commonTypeMatrix[row][col] = commonTypeMatrix[col][row] = graph[i].basicType;
			return;
		} // if

		i = graph[col].right;
		if ( i == -1 ) assert("invalid ancestor assumption");
		if ( costMatrix[row][i] >= 0 ) {
			commonTypeMatrix[row][col] = commonTypeMatrix[col][row] = graph[i].basicType;
			return;
		} // if

		assert("invalid ancestor assumption");
	} // if
} // generateCommonType

void resetInput( fstream & file, const char * filename, stringstream & buffer, stringstream & code, string & str ) {
	file.close();
	buffer.str( "" );
	code.str( "" );
	file.open( filename, fstream::in );
	if ( file.fail() ) {
		cout << "Internal error, could not open " << filename << " for input." << endl;
		abort();
	} // if
	buffer << file.rdbuf();
	str = buffer.str();
} // resetInput

void output( fstream & file, const char * filename, stringstream & code ) {
	file.close();
	file.open( filename, fstream::out );
	if ( file.fail() ) {
		cout << "Internal error, could not open " << filename << " for output." << endl;
		abort();
	} // if
	file << code.rdbuf();								// overwrite file
} // output

void Abort( const char * kind, const char * file ) {
	cerr << "Internal error, could not find " << kind << " of generated code for " << file << endl;
} // Abort

int main() {
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) { // initialization
		for ( int c = 0; c < NUMBER_OF_BASIC_TYPES; c += 1 ) {
			costMatrix[r][c] = -1;
			signMatrix[r][c] = -1;
			commonTypeMatrix[r][c] = NUMBER_OF_BASIC_TYPES;
		} // for
	} // for
	int lastInteger = NUMBER_OF_BASIC_TYPES;

	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) { // perform breath-first traversal to generate cost graph
		generateCosts(r);
	} // for

	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) { // use cost graph to find nearest-common-ancestor
		for (int c = r; c < NUMBER_OF_BASIC_TYPES; c += 1 ) {
			generateCommonType(r, c);
		} // for
	} // for

	// Find the last integer type.
	// Assumes at least 1, and all come before the floating types.
	for ( int i = 1 ; i < NUMBER_OF_BASIC_TYPES ; i += 1 ) {
		if ( Floating == graph[i].sign ) {
			lastInteger = (i - 1);
			break;
		}
	}

	#define STARTMK "// GENERATED START, DO NOT EDIT"
	#define ENDMK "// GENERATED END"
	string BYMK( __FILE__ );
	string::size_type posn = BYMK.find_last_of( "/" );
	if ( posn != string::npos ) BYMK.erase( 0, posn - 1); // remove directories
	BYMK = "// GENERATED BY " + BYMK;

	fstream file;
	stringstream buffer, code;
	string str;
	size_t start, end;

	#define TypeH_AST TOP_SRCDIR "src/AST/BasicKind.hpp"
	resetInput( file, TypeH_AST, buffer, code, str );

	if ( (start = str.find( STARTMK )) == string::npos ) Abort( "start", TypeH_AST );
	start += sizeof( STARTMK );							// includes newline
	code << str.substr( 0, start );

	code << BYMK << endl;
	code << "enum BasicKind {" << endl;
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) {
		code << "\t" << graph[r].name << "," << endl;
	} // for
	code << "\tNUMBER_OF_BASIC_TYPES," << endl;
	code << "\tMAX_INTEGER_TYPE = " << graph[lastInteger].name << "," << endl;
	code << "};" << endl;

	if ( (start = str.find( ENDMK, start + 1 )) == string::npos ) Abort( "end", TypeH_AST );
	code << str.substr( start );

	output( file, TypeH_AST, code );
	// cout << code.str();


	#define TypeC_AST TOP_SRCDIR "src/AST/Type.cpp"
	resetInput( file, TypeC_AST, buffer, code, str );

	if ( (start = str.find( STARTMK )) == string::npos ) Abort( "start", TypeC_AST );
	start += sizeof( STARTMK );							// includes newline
	code << str.substr( 0, start );

	code << BYMK << endl;
	code << "const char * BasicType::typeNames[] = {" << endl;
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) {
		code << "\t\"" << graph[r].type << "\"," << endl;
	} // for
	code << "};" << endl;

	if ( (start = str.find( ENDMK, start + 1 )) == string::npos ) Abort( "end", TypeC_AST );
	code << str.substr( start );

	output( file, TypeC_AST, code );
	// cout << code.str();


	#define ConversionCost TOP_SRCDIR "src/ResolvExpr/ConversionCost.cpp"
	resetInput( file, ConversionCost, buffer, code, str );

	if ( (start = str.find( STARTMK )) == string::npos ) Abort( "start", ConversionCost );
	start += sizeof( STARTMK );							// includes newline
	code << str.substr( 0, start );

	code << "\t" << BYMK << endl;
	code << "\t/* EXTENDED INTEGRAL RANK HIERARCHY (root to leaves)" << endl;
	for ( int c = 0; c < NUMBER_OF_BASIC_TYPES; c += 1 ) {
		code << '\t' << left;
		if ( graph[c].rank != graph[c + 1].rank ) {
			code << right << setw(30) << graph[c].type << left;
		} else if ( graph[c].rank != graph[c + 2].rank ) {
			code << string( 10, ' ' ) << setw(25) << graph[c].type << graph[c + 1].type;
			c += 1;
		} else {
			code << setw(20) << graph[c].type << setw(20) << graph[c + 1].type << graph[c + 2].type;
			c += 2;
		} // if
		code << endl;
	} // for
	code << right << "\t*/" << endl;
	code << "\t";										// indentation for end marker

	if ( (start = str.find( ENDMK, start + 1 )) == string::npos ) Abort( "end", ConversionCost );
	if ( (end = str.find( STARTMK, start + 1 )) == string::npos ) Abort( "start", ConversionCost );
	end += sizeof( STARTMK );
	code << str.substr( start, end - start );

	code << "\t" << BYMK << endl;
	code << "\tstatic const int costMatrix[ast::BasicKind::NUMBER_OF_BASIC_TYPES][ast::BasicKind::NUMBER_OF_BASIC_TYPES] = { // path length from root to node" << endl
		 << "\t\t/*           ";
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) { // titles
		code << setw(5) << graph[r].abbrev;
	} // for
	code << " */" << endl;
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) { // costs
		code << "\t\t/* " << setw(6) << graph[r].abbrev << " */ {";
		for ( int c = 0; c < NUMBER_OF_BASIC_TYPES; c += 1 ) {
			code << setw(4) << costMatrix[r][c] << ",";
		} // for
		code << " }," << endl;
	} // for
	code << "\t}; // costMatrix" << endl;

	// maximum conversion cost from int
	code << "\tstatic const int maxIntCost = " << *max_element(costMatrix[SignedInt], costMatrix[SignedInt] + NUMBER_OF_BASIC_TYPES) << ";" << endl;
	code << "\t";										// indentation for end marker

	if ( (start = str.find( ENDMK, start + 1 )) == string::npos ) Abort( "end", ConversionCost );
	if ( (end = str.find( STARTMK, start + 1 )) == string::npos ) Abort( "start", ConversionCost );
	end += sizeof( STARTMK );
	code << str.substr( start, end - start );

	code << "\t" << BYMK << endl;
	code << "\tstatic const int signMatrix[ast::BasicKind::NUMBER_OF_BASIC_TYPES][ast::BasicKind::NUMBER_OF_BASIC_TYPES] = { // number of sign changes in safe conversion" << endl
		 << "\t\t/*           ";
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) { // titles
		code << setw(5) << graph[r].abbrev;
	} // for
	code << " */" << endl;
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) { // costs
		code << "\t\t/* " << setw(6) << graph[r].abbrev << " */ {";
		for ( int c = 0; c < NUMBER_OF_BASIC_TYPES; c += 1 ) {
			code << setw(4) << signMatrix[r][c] << ",";
		} // for
		code << " }," << endl;
	} // for
	code << "\t}; // signMatrix" << endl;
	code << "\t";										// indentation for end marker

	if ( (start = str.find( ENDMK, start + 1 )) == string::npos ) Abort( "end", ConversionCost );
	code << str.substr( start );

	output( file, ConversionCost, code );
	// cout << code.str();


	#define CommonType TOP_SRCDIR "src/ResolvExpr/CommonType.cpp"
	resetInput( file, CommonType, buffer, code, str );

	if ( (start = str.find( STARTMK )) == string::npos ) Abort( "start", CommonType );
	start += sizeof( STARTMK );							// includes newline
	code << str.substr( 0, start );

	enum { PER_ROW = 6 };
	code << "\t" << BYMK << endl;
	code << "\t#define BT ast::BasicKind::" << endl;
	code << "\tstatic const ast::BasicKind commonTypes[BT NUMBER_OF_BASIC_TYPES][BT NUMBER_OF_BASIC_TYPES] = { // nearest common ancestor" << endl
	     << "\t\t/*\t\t ";
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) { // titles
		code << setw(24) << graph[r].abbrev;
		if ( (r+1) % PER_ROW == 0 ) {
			code << endl << "\t\t\t\t ";
		} // if
	} // for
	code << "*/" << endl;
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) { // costs
		code << "\t\t\t\t  {\n\t\t/* " << setw(6) << graph[r].abbrev << " */";
		for ( int c = 0; c < NUMBER_OF_BASIC_TYPES; c += 1 ) {
			string s = string{"BT "} + graph[commonTypeMatrix[r][c]].name;
			code << setw(23) << s << ",";
			if ( (c+1) % PER_ROW == 0 ) {
				code << endl << "\t\t\t\t  ";
			} // if
		} // for
		code << "}," << endl;
	} // for
	code << "\t}; // commonTypes" << endl;
	code << "\t#undef BT" << endl;
	code << "\t";										// indentation for end marker

	if ( (start = str.find( ENDMK, start + 1 )) == string::npos ) Abort( "end", CommonType );
	code << str.substr( start );

	output( file, CommonType, code );
	// cout << code.str();


	#define ManglerCommon TOP_SRCDIR "src/SymTab/ManglerCommon.cpp"
	resetInput( file, ManglerCommon, buffer, code, str );

	if ( (start = str.find( STARTMK )) == string::npos ) Abort( "start", ManglerCommon );
	start += sizeof( STARTMK );							// includes newline
	code << str.substr( 0, start );

	code <<
		"// GENERATED BY " __FILE__ "\n"
		"// NOTES ON MANGLING:\n"
		"// * Itanium spec says that Float80 encodes to \"e\" (like LongDouble), but the distinct lengths cause resolution problems.\n"
		"// * Float128 is supposed to encode to \"g\", but I wanted it to mangle equal to LongDouble.\n"
		"// * Mangling for non-standard complex types is by best guess\n"
		"// * _FloatN is supposed to encode as \"DF\"N\"_\"; modified for same reason as above.\n"
		"// * unused mangling identifiers:\n"
		"//   - \"z\" ellipsis\n"
		"//   - \"Dd\" IEEE 754r 64-bit decimal floating point (borrowed for _Float32x)\n"
		"//   - \"De\" IEEE 754r 128-bit decimal floating point\n"
		"//   - \"Df\" IEEE 754r 32-bit decimal floating point\n"
		"//   - \"Dh\" IEEE 754r 16-bit decimal floating point (borrowed for _Float16)\n"
		"//   - \"DF\"N\"_\" ISO/IEC TS 18661 N-bit binary floating point (_FloatN)\n"
		"//   - \"Di\" char32_t\n"
		"//   - \"Ds\" char16_t\n";

	code << "const std::string basicTypes[ast::BasicKind::NUMBER_OF_BASIC_TYPES] = {" << endl;
	for ( int r = 0; r < NUMBER_OF_BASIC_TYPES; r += 1 ) {
		code << "\t\"" << graph[r].mangled << "\"," << setw(9 - strlen(graph[r].mangled)) << ' ' << "// " << graph[r].type << endl;
	} // for
	code << "}; // basicTypes" << endl;

	if ( (start = str.find( ENDMK, start + 1 )) == string::npos ) Abort( "end", ManglerCommon );
	code << str.substr( start );

	output( file, ManglerCommon, code );
	// cout << code.str();
} // main

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "g++-8 -Wall -Wextra BasicTypes-gen.cpp" //
// End: //
