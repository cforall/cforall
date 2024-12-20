//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExpressionNode.cpp --
//
// Author           : Peter A. Buhr
// Created On       : Sat May 16 13:17:07 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Dec 16 20:50:27 2024
// Update Count     : 1110
//

#include "ExpressionNode.hpp"

#include <cassert>                 // for assert
#include <stdio.h>                 // for sscanf, size_t
#include <climits>                 // for LLONG_MAX, LONG_MAX, INT_MAX, UINT...
#include <list>                    // for list
#include <sstream>                 // for basic_istream::operator>>, basic_i...
#include <string>                  // for string, operator+, operator==

#include "AST/BasicKind.hpp"       // for BasicKind
#include "AST/Expr.hpp"            // for NameExpr
#include "AST/Type.hpp"            // for Type, LengthFlag, DimentionFlag
#include "Common/SemanticError.hpp"// for SemanticError
#include "Common/Utility.hpp"      // for maybeMoveBuild, maybeBuild, CodeLo...
#include "DeclarationNode.hpp"     // for DeclarationNode
#include "InitializerNode.hpp"     // for InitializerNode
#include "TypeData.hpp"            // for addType, build_basic_type, build_c...
#include "ParserUtility.hpp"       // for notZeroExpr

using namespace std;

//##############################################################################

// Difficult to separate extra parts of constants during lexing because actions are not allow in the middle of patterns:
//
//		prefix action constant action suffix
//
// Alternatively, breaking a pattern using BEGIN does not work if the following pattern can be empty:
//
//		constant BEGIN CONT ...
//		<CONT>(...)? BEGIN 0 ... // possible empty suffix
//
// because the CONT rule is NOT triggered if the pattern is empty. Hence, constants are reparsed here to determine their
// type.

// static inline bool checkH( char c ) { return c == 'h' || c == 'H'; }
// static inline bool checkZ( char c ) { return c == 'z' || c == 'Z'; }
// static inline bool checkU( char c ) { return c == 'u' || c == 'U'; }
static inline bool checkF( char c ) { return c == 'f' || c == 'F'; }
static inline bool checkD( char c ) { return c == 'd' || c == 'D'; }
static inline bool checkF80( char c ) { return c == 'w' || c == 'W'; }
static inline bool checkF128( char c ) { return c == 'q' || c == 'Q'; }
static inline bool checkL( char c ) { return c == 'l' || c == 'L'; }
static inline bool checkI( char c ) { return c == 'i' || c == 'I'; }
static inline bool checkB( char c ) { return c == 'b' || c == 'B'; }
static inline bool checkX( char c ) { return c == 'x' || c == 'X'; }
// static inline bool checkN( char c ) { return c == 'n' || c == 'N'; }

void lnthSuffix( string & str, int & type, int & ltype ) {
	// 'u' can appear before or after length suffix
	string::size_type posn = str.find_last_of( "lL" );

	if ( posn == string::npos ) return;					// no suffix
	size_t end = str.length() - 1;
	if ( posn == end ) { type = 3; return; }			// no length after 'l' => long

	string::size_type next = posn + 1;					// advance to length
	if ( str[next] == '3' ) {							// 32
		type = ltype = 2;
	} else if ( str[next] == '6' ) {					// 64
		type = ltype = 3;
	} else if ( str[next] == '8' ) {					// 8
		type = ltype = 1;
	} else if ( str[next] == '1' ) {
		if ( str[next + 1] == '6' ) {					// 16
			type = ltype = 0;
		} else {										// 128
			type = 5; ltype = 6;
		} // if
	} // if

	char fix = '\0';
	if ( str[end] == 'u' || str[end] == 'U' ) fix = str[end]; // ends with 'uU' ?
	str.erase( posn );									// remove length suffix and possibly uU
	if ( type == 5 ) {									// L128 does not need uU
		end = str.length() - 1;
		if ( str[end] == 'u' || str[end] == 'U' ) str.erase( end ); // ends with 'uU' ? remove
	} else if ( fix != '\0' ) str += fix;				// put 'uU' back if removed
} // lnthSuffix

void valueToType( unsigned long long int & v, bool dec, int & type, bool & Unsigned ) {
	// use value to determine type
	if ( v <= INT_MAX ) {								// signed int
		type = 2;
	} else if ( v <= UINT_MAX && ! dec ) {				// unsigned int
		type = 2;
		Unsigned = true;								// unsigned
	} else if ( v <= LONG_MAX ) {						// signed long int
		type = 3;
	} else if ( v <= ULONG_MAX && ( ! dec || LONG_MAX == LLONG_MAX ) ) { // signed long int
		type = 3;
		Unsigned = true;								// unsigned long int
	} else if ( v <= LLONG_MAX ) {						// signed long long int
		type = 4;
	} else {											// unsigned long long int
		type = 4;
		Unsigned = true;								// unsigned long long int
	} // if
} // valueToType

static void scanbin( string & str, unsigned long long int & v ) {
	v = 0;
	size_t last = str.length() - 1;						// last subscript of constant
	for ( unsigned int i = 2;; ) {						// ignore prefix
		if ( str[i] == '1' ) v |= 1;
		i += 1;
		if ( i == last - 1 || (str[i] != '0' && str[i] != '1') ) break;
		v <<= 1;
	} // for
} // scanbin

ast::Expr * build_constantInteger(
		const CodeLocation & location, string & str ) {
	static const ast::BasicKind kind[2][6] = {
		// short (h) must be before char (hh) because shorter type has the longer suffix
		{ ast::BasicKind::ShortSignedInt, ast::BasicKind::SignedChar, ast::BasicKind::SignedInt, ast::BasicKind::LongSignedInt, ast::BasicKind::LongLongSignedInt, /* BasicKind::SignedInt128 */ ast::BasicKind::LongLongSignedInt, },
		{ ast::BasicKind::ShortUnsignedInt, ast::BasicKind::UnsignedChar, ast::BasicKind::UnsignedInt, ast::BasicKind::LongUnsignedInt, ast::BasicKind::LongLongUnsignedInt, /* BasicKind::UnsignedInt128 */ ast::BasicKind::LongLongUnsignedInt, },
	};

	static const char * lnthsInt[2][6] = {
		{ "int16_t",  "int8_t",  "int32_t",  "int64_t",  "size_t",  "uintptr_t", },
		{ "uint16_t", "uint8_t", "uint32_t", "uint64_t", "size_t",  "uintptr_t", },
	}; // lnthsInt

	string str2( "0x0" );
	unsigned long long int v, v2 = 0;					// converted integral value
	ast::Expr * ret, * ret2;

	int type = -1;										// 0 => short, 1 => char, 2 => int, 3 => long int, 4 => long long int, 5 => int128
	int ltype = -1;										// 0 => 16 bits, 1 => 8 bits, 2 => 32 bits, 3 => 64 bits, 4 => size_t, 5 => intptr, 6 => pointer
	bool dec = true, Unsigned = false;					// decimal, unsigned constant

	// special constants
	if ( str == "0" ) {
		ret = new ast::ConstantExpr( location, new ast::ZeroType(), str, 0 );
		goto CLEANUP;
	} // if
	if ( str == "1" ) {
		ret = new ast::ConstantExpr( location, new ast::OneType(), str, 1 );
		goto CLEANUP;
	} // if

	// 'u' can appear before or after length suffix
	if ( str.find_last_of( "uU" ) != string::npos ) Unsigned = true;

	if ( isdigit( str[str.length() - 1] ) ) {			// no suffix ?
		lnthSuffix( str, type, ltype );					// could have length suffix
	} else {
		// At least one digit in integer constant, so safe to backup while looking for suffix.  This
		// declaration and the comma expressions in the conditions mimic the declare and check
		// pattern allowed in later compiler versions.  (Only some early compilers/C++ standards do
		// not support it.)
		string::size_type posn;
		// pointer value
		if ( posn = str.find_last_of( "pP" ), posn != string::npos ) {
			ltype = 5; str.erase( posn, 1 );
		// size_t
		} else if ( posn = str.find_last_of( "zZ" ), posn != string::npos ) {
			Unsigned = true; type = 2; ltype = 4; str.erase( posn, 1 );
		// signed char
		} else if ( posn = str.rfind( "hh" ), posn != string::npos ) {
			type = 1; str.erase( posn, 2 );
		// signed char
		} else if ( posn = str.rfind( "HH" ), posn != string::npos ) {
			type = 1; str.erase( posn, 2 );
		// short
		} else if ( posn = str.find_last_of( "hH" ), posn != string::npos ) {
			type = 0; str.erase( posn, 1 );
		// int (natural number)
		} else if ( posn = str.find_last_of( "nN" ), posn != string::npos ) {
			type = 2; str.erase( posn, 1 );
		} else if ( str.rfind( "ll" ) != string::npos || str.rfind( "LL" ) != string::npos ) {
			type = 4;
		} else {
			lnthSuffix( str, type, ltype );
		} // if
	} // if

	// Cannot be just "0"/"1"; sscanf stops at the suffix, if any; value goes over the wall => always generate

#if ! defined(__SIZEOF_INT128__)
	if ( type == 5 ) SemanticError( yylloc, "int128 constant is not supported on this target \"%s\"", str.c_str() );
#endif // ! __SIZEOF_INT128__

	if ( str[0] == '0' ) {								// radix character ?
		dec = false;
		if ( checkX( str[1] ) ) {						// hex constant ?
			if ( type < 5 ) {							// not L128 ?
				sscanf( (char *)str.c_str(), "%llx", &v );
#if defined(__SIZEOF_INT128__)
			} else {									// hex int128 constant
				unsigned int len = str.length();
				if ( len > (2 + 16 + 16) ) SemanticError( yylloc, "128-bit hexadecimal constant to large \"%s\"", str.c_str() );
				// hex digits < 2^64
				if ( len > (2 + 16) ) {
					str2 = "0x" + str.substr( len - 16 );
					sscanf( (char *)str2.c_str(), "%llx", &v2 );
					str = str.substr( 0, len - 16 );
				} // if
				sscanf( (char *)str.c_str(), "%llx", &v );
#endif // __SIZEOF_INT128__
			} // if
			//printf( "%llx %llu\n", v, v );
		} else if ( checkB( str[1] ) ) {				// binary constant ?
#if defined(__SIZEOF_INT128__)
			unsigned int len = str.length();
			if ( type == 5 && len > 2 + 64 ) {
				if ( len > 2 + 64 + 64 ) SemanticError( yylloc, "128-bit binary constant to large \"%s\".", str.c_str() );
				str2 = "0b" + str.substr( len - 64 );
				str = str.substr( 0, len - 64 );
				scanbin( str2, v2 );
			} // if
#endif // __SIZEOF_INT128__
			scanbin( str, v );
			//printf( "%#llx %llu\n", v, v );
		} else {										// octal constant
			if ( type < 5 ) {							// not L128 ?
				sscanf( (char *)str.c_str(), "%llo", &v );
#if defined(__SIZEOF_INT128__)
			} else {									// octal int128 constant
				unsigned int len = str.length();
				if ( len > 1 + 43 || (len == 1 + 43 && str[0] > '3') ) SemanticError( yylloc, "128-bit octal constant to large \"%s\"", str.c_str() );
				char buf[32];
				if ( len <= 1 + 21 ) {					// value < 21 octal digitis
					sscanf( (char *)str.c_str(), "%llo", &v );
				} else {
					sscanf( &str[len - 21], "%llo", &v );
					__int128 val = v;					// accumulate bits
					str[len - 21] ='\0';				// shorten string
					sscanf( &str[len == 43 ? 1 : 0], "%llo", &v );
					val |= (__int128)v << 63;			// store bits
					if ( len == 1 + 43 ) {				// most significant 2 bits ?
						str[2] = '\0';					// shorten string
						sscanf( &str[1], "%llo", &v );	// process most significant 2 bits
						val |= (__int128)v << 126;		// store bits
					} // if
					v = val >> 64; v2 = (uint64_t)val;	// replace octal constant with 2 hex constants
					sprintf( buf, "%#llx", v2 );
					str2 = buf;
				} // if
				sprintf( buf, "%#llx", v );
				str = buf;
#endif // __SIZEOF_INT128__
			} // if
			//printf( "%#llo %llu\n", v, v );
		} // if
	} else {											// decimal constant ?
		if ( type < 5 ) {								// not L128 ?
			sscanf( (char *)str.c_str(), "%llu", &v );
#if defined(__SIZEOF_INT128__)
		} else {										// decimal int128 constant
			#define P10_UINT64 10'000'000'000'000'000'000ULL // 19 zeroes
			unsigned int len = str.length();
			if ( str.length() == 39 && str > (Unsigned ? "340282366920938463463374607431768211455" : "170141183460469231731687303715884105727") )
				SemanticError( yylloc, "128-bit decimal constant to large \"%s\".", str.c_str() );
			char buf[32];
			if ( len <= 19 ) {							// value < 19 decimal digitis
				sscanf( (char *)str.c_str(), "%llu", &v );
			} else {
				sscanf( &str[len - 19], "%llu", &v );
				__int128 val = v;						// accumulate bits
				str[len - 19] ='\0';					// shorten string
				sscanf( &str[len == 39 ? 1 : 0], "%llu", &v );
				val += (__int128)v * (__int128)P10_UINT64; // store bits
				if ( len == 39 ) {						// most significant 2 bits ?
					str[1] = '\0';						// shorten string
					sscanf( &str[0], "%llu", &v );		// process most significant 2 bits
					val += (__int128)v * (__int128)P10_UINT64 * (__int128)P10_UINT64; // store bits
				} // if
				v = val >> 64; v2 = (uint64_t)val;		// replace decimal constant with 2 hex constants
				sprintf( buf, "%#llx", v2 );
				str2 = buf;
			} // if
			sprintf( buf, "%#llx", v );
			str = buf;
#endif // __SIZEOF_INT128__
		} // if
		//printf( "%llu\n", v );
	} // if

	if ( type == -1 ) {									// no suffix => determine type from value size
		valueToType( v, dec, type, Unsigned );
	} // if
	/* printf( "%s %llo %s %llo\n", str.c_str(), v, str2.c_str(), v2 ); */

	//if ( !( 0 <= type && type <= 6 ) ) { printf( "%s %lu %d %s\n", fred.c_str(), fred.length(), type, str.c_str() ); }
	assert( 0 <= type && type <= 6 );

	// Constant type is correct for overload resolving.
	ret = new ast::ConstantExpr( location,
		new ast::BasicType( kind[Unsigned][type] ), str, v );
	if ( Unsigned && type < 2 ) {						// hh or h, less than int ?
		// int i = -1uh => 65535 not -1, so cast is necessary for unsigned, which unfortunately eliminates warnings for large values.
		ret = new ast::CastExpr( location,
			ret,
			new ast::BasicType( kind[Unsigned][type] ),
			ast::ExplicitCast );
	} else if ( ltype != -1 ) {							// explicit length ?
		if ( ltype == 6 ) {								// int128, (int128)constant
			ret2 = new ast::ConstantExpr( location,
				new ast::BasicType( ast::BasicKind::LongLongSignedInt ),
				str2,
				v2 );
			ret = build_compoundLiteral( location,
				DeclarationNode::newFromTypeData(
					addType(
						build_basic_type( TypeData::Int128 ),
						build_signedness( TypeData::Unsigned ) ) ),
				new InitializerNode(
					(new InitializerNode( new ExpressionNode( v2 == 0 ? ret2 : ret ) ))->set_last( new InitializerNode( new ExpressionNode( v2 == 0 ? ret : ret2 ) ) ), true )
			);
		} else {										// explicit length, (length_type)constant
			ret = new ast::CastExpr( location,
				ret,
				new ast::TypeInstType( lnthsInt[Unsigned][ltype], ast::TypeDecl::Dtype ),
				ast::ExplicitCast );
			if ( ltype == 5 ) {							// pointer, intptr( (uintptr_t)constant )
				ret = build_func( location,
					new ExpressionNode(
						build_varref( location, new string( "intptr" ) ) ),
					new ExpressionNode( ret ) );
			} // if
		} // if
	} // if

  CLEANUP: ;
	delete &str;										// created by lex
	return ret;
} // build_constantInteger


static inline void checkFnxFloat( string & str, size_t last, bool & explnth, int & type ) {
	string::size_type posn;
	// floating-point constant has minimum of 2 characters, 1. or .1, so safe to look ahead
	if ( str[1] == 'x' ) {								// hex ?
		posn = str.find_last_of( "pP" );				// back for exponent (must have)
		posn = str.find_first_of( "fF", posn + 1 );		// forward for size (fF allowed in hex constant)
	} else {
		posn = str.find_last_of( "fF" );				// back for size (fF not allowed)
	} // if
  if ( posn == string::npos ) return;
	explnth = true;
	posn += 1;											// advance to size
	if ( str[posn] == '3' ) {							// 32
		if ( str[last] != 'x' ) type = 6;
		else type = 7;
	} else if ( str[posn] == '6' ) {					// 64
		if ( str[last] != 'x' ) type = 8;
		else type = 9;
	} else if ( str[posn] == '8' ) {					// 80
		type = 3;
	} else if ( str[posn] == '1' ) {					// 16/128
		if ( str[posn + 1] == '6' ) {					// 16
			type = 5;
		} else {										// 128
			if ( str[last] != 'x' ) type = 10;
			else type = 11;
		} // if
	} else {
		assertf( false, "internal error, bad floating point length %s", str.c_str() );
	} // if
} // checkFnxFloat


ast::Expr * build_constantFloat(
		const CodeLocation & location, string & str ) {
	static const ast::BasicKind kind[2][12] = {
		{ ast::BasicKind::Float, ast::BasicKind::Double, ast::BasicKind::LongDouble, ast::BasicKind::Float80, ast::BasicKind::uuFloat128, ast::BasicKind::Float16, ast::BasicKind::Float32, ast::BasicKind::Float32x, ast::BasicKind::Float64, ast::BasicKind::Float64x, ast::BasicKind::Float128, ast::BasicKind::Float128x },
		{ ast::BasicKind::FloatComplex, ast::BasicKind::DoubleComplex, ast::BasicKind::LongDoubleComplex, ast::BasicKind::NUMBER_OF_BASIC_TYPES, ast::BasicKind::NUMBER_OF_BASIC_TYPES, ast::BasicKind::Float16Complex, ast::BasicKind::Float32Complex, ast::BasicKind::Float32xComplex, ast::BasicKind::Float64Complex, ast::BasicKind::Float64xComplex, ast::BasicKind::Float128Complex, ast::BasicKind::Float128xComplex },
	};

	// floating-point constant has minimum of 2 characters 1. or .1
	size_t last = str.length() - 1;
	double v;
	int type;											// 0 => float, 1 => double, 3 => long double, ...
	bool complx = false;								// real, complex
	bool explnth = false;								// explicit literal length

	sscanf( str.c_str(), "%lg", &v );

	if ( checkI( str[last] ) ) {						// imaginary ?
		complx = true;
		last -= 1;										// backup one character
	} // if

	if ( checkF( str[last] ) ) {						// float ?
		type = 0;
	} else if ( checkD( str[last] ) ) {					// double ?
		type = 1;
	} else if ( checkL( str[last] ) ) {					// long double ?
		type = 2;
	} else if ( checkF80( str[last] ) ) {				// __float80 ?
		type = 3;
	} else if ( checkF128( str[last] ) ) {				// __float128 ?
		type = 4;
	} else {
		type = 1;										// double (default if no suffix)
		checkFnxFloat( str, last, explnth, type );
	} // if

	if ( ! complx && checkI( str[last - 1] ) ) {		// imaginary ?
		complx = true;
	} // if

	assert( 0 <= type && type < 12 );
	ast::Expr * ret = new ast::ConstantExpr( location,
		new ast::BasicType( kind[complx][type] ),
		str,
		v );
	// explicit length ?
	if ( explnth ) {
		ret = new ast::CastExpr( location,
			ret,
			new ast::BasicType( kind[complx][type] ),
			ast::ExplicitCast );
	} // if

	delete &str;										// created by lex
	return ret;
} // build_constantFloat

static void sepString( string & str, string & units, char delimit ) {
	string::size_type posn = str.find_last_of( delimit ) + 1;
	if ( posn != str.length() ) {
		units = "?" + str.substr( posn );				// extract units
		str.erase( posn );								// remove units
	} // if
} // sepString

ast::Expr * build_constantChar( const CodeLocation & location, string & str ) {
	string units;										// units
	sepString( str, units, '\'' );						// separate constant from units

	ast::Expr * ret = new ast::ConstantExpr( location,
		new ast::BasicType( ast::BasicKind::Char ),
		str,
		(unsigned long long int)(unsigned char)str[1] );
	if ( units.length() != 0 ) {
		ret = new ast::UntypedExpr( location,
			new ast::NameExpr( location, units ),
			{ ret } );
	} // if

	delete &str;										// created by lex
	return ret;
} // build_constantChar

static bool isoctal( char ch ) {
	return ('0' <= ch && ch <= '7');
}

// A "sequence" is the series of characters in a character/string literal that becomes a single
// character value in the runtime value.
static size_t sequenceLength( const std::string & str, size_t pos ) {
	// Most "sequences" are just a single character, filter those out:
	if ( '\\' != str[pos] ) return 1;
	switch ( str[pos + 1] ) {
		// Simple Escape Sequence (\_ where _ is one of the following):
	  case '\'': case '\"': case '?': case '\\':
	  case 'a': case 'b': case 'f': case 'n': case 'r': case 't': case 'v':
		// GCC Escape Sequence (as simple, just some different letters):
	  case 'e':
		return 2;
		// Numeric Escape Sequence (\___ where _ is 1-3 octal digits):
	  case '0': case '1': case '2': case '3':
	  case '4': case '5': case '6': case '7':
		return ( !isoctal( str[pos + 2] ) ) ? 2 :
		  ( !isoctal( str[pos + 3] ) ) ? 3 : 4;
		  // Numeric Escape Sequence (\x_ where _ is 1 or more hexadecimal digits):
	  case 'x': {
		  size_t length = 2;
		  while ( isxdigit( str[pos + length] ) ) ++length;
		  return length;
	  }
		// Universal Character Name (\u____ where _ is 4 decimal digits):
	  case 'u':
		return 6;
		// Universal Character Name (\U________ where _ is 8 decimal digits):
	  case 'U':
		return 10;
	  default:
		assertf( false, "Unknown escape sequence (start %c).", str[pos] );
		return 1;
	} // switch
}

ast::Expr * build_constantStr( const CodeLocation & location, string & str ) {
	assert( str.length() > 0 );
	string units;										// units
	sepString( str, units, '"' );						// separate constant from units

	ast::Type * strtype;
	switch ( str[0] ) {									// str has >= 2 characters, i.e, null string "" => safe to look at subscripts 0/1
	case 'u':
		if ( str[1] == '8' ) goto Default;				// utf-8 characters => array of char
		// lookup type of associated typedef
		strtype = new ast::TypeInstType( "char16_t", ast::TypeDecl::Dtype );
		break;
	case 'U':
		strtype = new ast::TypeInstType( "char32_t", ast::TypeDecl::Dtype );
		break;
	case 'L':
		strtype = new ast::TypeInstType( "wchar_t", ast::TypeDecl::Dtype );
		break;
	Default:											// char default string type
	default:
		strtype = new ast::BasicType( ast::BasicKind::Char );
	} // switch

	// The length value of the type is equal to the number of "sequences" not including the openning
	// and closing quotes in the literal plus 1 for the implicit null terminator.
	size_t length = 1;
	for ( size_t pos = 1 ; pos < str.size() - 1 ; pos += sequenceLength( str, pos ) ) {
		if ( '"' == str[pos] ) {						// concatenated strings ? "ABC" "DEF"
			int cnt = 1;								// skip outside quotes and space between
			for ( unsigned int i = pos + 1; str[i] != '"'; i += 1, cnt += 1 );
			pos += cnt;
			continue;									// not part of length
		} // if
		length += 1;
	} // for

	ast::ArrayType * at = new ast::ArrayType(
		strtype,
		ast::ConstantExpr::from_ulong( location, length ),
		ast::FixedLen,
		ast::DynamicDim );
	ast::Expr * ret = new ast::ConstantExpr( location, at, str, std::nullopt );
	if ( units.length() != 0 ) {
		ret = new ast::UntypedExpr( location, new ast::NameExpr( location, units ),	{ ret } );
	} // if

	delete &str;										// created by lex
	return ret;
} // build_constantStr

ast::Expr * build_field_name_FLOATING_FRACTIONconstant(
		const CodeLocation & location, const string & str ) {
	if ( str.find_first_not_of( "0123456789", 1 ) != string::npos ) SemanticError( yylloc, "invalid tuple index \"%s\".", str.c_str() );
	ast::Expr * ret = build_constantInteger( location,
		*new string( str.substr(1) ) );
	delete &str;
	return ret;
} // build_field_name_FLOATING_FRACTIONconstant

ast::Expr * build_field_name_FLOATING_DECIMALconstant(
		const CodeLocation & location, const string & str ) {
	if ( str[str.size() - 1] != '.' ) SemanticError( yylloc, "invalid tuple index \"%s\".", str.c_str() );
	ast::Expr * ret = build_constantInteger(
		location, *new string( str.substr( 0, str.size()-1 ) ) );
	delete &str;
	return ret;
} // build_field_name_FLOATING_DECIMALconstant

ast::Expr * build_field_name_FLOATINGconstant( const CodeLocation & location,
		const string & str ) {
	// str is of the form A.B -> separate at the . and return member expression
	int a, b;
	char dot;
	stringstream ss( str );
	ss >> a >> dot >> b;
	auto ret = new ast::UntypedMemberExpr( location,
		ast::ConstantExpr::from_int( location, b ),
		ast::ConstantExpr::from_int( location, a )
	);
	delete &str;
	return ret;
} // build_field_name_FLOATINGconstant

ast::Expr * make_field_name_fraction_constants( const CodeLocation & location,
		ast::Expr * fieldName,
		ast::Expr * fracts ) {
	if ( nullptr == fracts ) {
		return fieldName;
	} else if ( auto memberExpr = dynamic_cast<ast::UntypedMemberExpr *>( fracts ) ) {
		memberExpr->member = make_field_name_fraction_constants( location,
			fieldName,
			ast::mutate( memberExpr->aggregate.get() ) );
		return memberExpr;
	} else {
		return new ast::UntypedMemberExpr( location, fracts, fieldName );
	} // if
} // make_field_name_fraction_constants

ast::Expr * build_field_name_fraction_constants( const CodeLocation & location,
		ast::Expr * fieldName,
		ExpressionNode * fracts ) {
	return make_field_name_fraction_constants( location, fieldName, maybeMoveBuild( fracts ) );
} // build_field_name_fraction_constants

ast::NameExpr * build_varref( const CodeLocation & location,
		const string * name ) {
	ast::NameExpr * expr = new ast::NameExpr( location, *name );
	delete name;
	return expr;
} // build_varref

ast::QualifiedNameExpr * build_qualified_expr( const CodeLocation & location,
		const DeclarationNode * decl_node,
		const ast::NameExpr * name ) {
	ast::Decl * newDecl = maybeBuild( decl_node );
	if ( ast::DeclWithType * newDeclWithType = dynamic_cast<ast::DeclWithType *>( newDecl ) ) {
		if ( const ast::Type * t = newDeclWithType->get_type() ) {
			if ( auto typeInst = dynamic_cast<const ast::TypeInstType *>( t ) ) {
				newDecl = new ast::EnumDecl( location, typeInst->name );
			}
		}
	}
	return new ast::QualifiedNameExpr( location, newDecl, name->name );
}

ast::QualifiedNameExpr * build_qualified_expr( const CodeLocation & location,
		const ast::EnumDecl * decl,
		const ast::NameExpr * name ) {
	return new ast::QualifiedNameExpr( location, decl, name->name );
}

ast::DimensionExpr * build_dimensionref( const CodeLocation & location,
		const string * name ) {
	ast::DimensionExpr * expr = new ast::DimensionExpr( location, *name );
	delete name;
	return expr;
} // build_varref

// TODO: get rid of this and OperKinds and reuse code from OperatorTable
static const char * OperName[] = {						// must harmonize with OperKinds
	// diadic
	"SizeOf", "AlignOf", "OffsetOf", "?+?", "?-?", "?\\?", "?*?", "?/?", "?%?", "||", "&&",
	"?|?", "?&?", "?^?", "Cast", "?<<?", "?>>?", "?<?", "?>?", "?<=?", "?>=?", "?==?", "?!=?",
	"?=?", "?@=?", "?\\=?", "?*=?", "?/=?", "?%=?", "?+=?", "?-=?", "?<<=?", "?>>=?", "?&=?", "?^=?", "?|=?",
	"?[?]", "...",
	// monadic
	"+?", "-?", "AddressOf", "*?", "!?", "~?", "++?", "?++", "--?", "?--",
}; // OperName

ast::Expr * build_cast( const CodeLocation & location,
		DeclarationNode * decl_node,
		ExpressionNode * expr_node,
		ast::CastKind kind ) {
	ast::Type * targetType = maybeMoveBuildType( decl_node );
	if ( dynamic_cast<ast::VoidType *>( targetType ) ) {
		delete targetType;
		return new ast::CastExpr( location,
			maybeMoveBuild( expr_node ),
			ast::ExplicitCast, kind );
	} else {
		return new ast::CastExpr( location,
			maybeMoveBuild( expr_node ),
			targetType,
			ast::ExplicitCast, kind );
	} // if
} // build_cast

ast::Expr * build_keyword_cast( const CodeLocation & location,
		ast::AggregateDecl::Aggregate target,
		ExpressionNode * expr_node ) {
	return new ast::KeywordCastExpr( location,
		maybeMoveBuild( expr_node ),
		target
	);
}

ast::Expr * build_virtual_cast( const CodeLocation & location,
		DeclarationNode * decl_node,
		ExpressionNode * expr_node ) {
	return new ast::VirtualCastExpr( location,
		maybeMoveBuild( expr_node ),
		maybeMoveBuildType( decl_node )
	);
} // build_virtual_cast

ast::Expr * build_fieldSel( const CodeLocation & location,
		ExpressionNode * expr_node,
		ast::Expr * member ) {
	return new ast::UntypedMemberExpr( location,
		member,
		maybeMoveBuild( expr_node )
	);
} // build_fieldSel

ast::Expr * build_pfieldSel( const CodeLocation & location,
		ExpressionNode * expr_node,
		ast::Expr * member ) {
	auto deref = new ast::UntypedExpr( location,
		new ast::NameExpr( location, "*?" )
	);
	deref->location = expr_node->location;
	deref->args.push_back( maybeMoveBuild( expr_node ) );
	auto ret = new ast::UntypedMemberExpr( location, member, deref );
	return ret;
} // build_pfieldSel

ast::Expr * build_offsetOf( const CodeLocation & location,
		DeclarationNode * decl_node,
		ast::NameExpr * member ) {
	ast::Expr * ret = new ast::UntypedOffsetofExpr( location,
		maybeMoveBuildType( decl_node ),
		member->name
	);
	ret->result = new ast::BasicType( ast::BasicKind::LongUnsignedInt );
	delete member;
	return ret;
} // build_offsetOf

ast::Expr * build_and_or( const CodeLocation & location,
		ExpressionNode * expr_node1,
		ExpressionNode * expr_node2,
		ast::LogicalFlag flag ) {
	return new ast::LogicalExpr( location,
		maybeMoveBuild( expr_node1 ),
		maybeMoveBuild( expr_node2 ),
		flag
	);
} // build_and_or

ast::Expr * build_unary_val( const CodeLocation & location,
		OperKinds op,
		ExpressionNode * expr_node ) {
	std::vector<ast::ptr<ast::Expr>> args;
	args.push_back( maybeMoveBuild( expr_node ) );
	return new ast::UntypedExpr( location,
		new ast::NameExpr( location, OperName[ (int)op ] ),
		std::move( args )
	);
} // build_unary_val

ast::Expr * build_binary_val( const CodeLocation & location,
		OperKinds op,
		ExpressionNode * expr_node1,
		ExpressionNode * expr_node2 ) {
	std::vector<ast::ptr<ast::Expr>> args;
	args.push_back( maybeMoveBuild( expr_node1 ) );
	args.push_back( maybeMoveBuild( expr_node2 ) );
	return new ast::UntypedExpr( location,
		new ast::NameExpr( location, OperName[ (int)op ] ),
		std::move( args )
	);
} // build_binary_val

ast::Expr * build_cond( const CodeLocation & location,
		ExpressionNode * expr_node1,
		ExpressionNode * expr_node2,
		ExpressionNode * expr_node3 ) {
	return new ast::ConditionalExpr( location,
		maybeMoveBuild( expr_node1 ),
		maybeMoveBuild( expr_node2 ),
		maybeMoveBuild( expr_node3 )
	);
} // build_cond

ast::Expr * build_tuple( const CodeLocation & location,
		ExpressionNode * expr_node ) {
	std::vector<ast::ptr<ast::Expr>> exprs;
	buildMoveList( expr_node, exprs );
	return new ast::UntypedTupleExpr( location, std::move( exprs ) );
} // build_tuple

ast::Expr * build_func( const CodeLocation & location,
		ExpressionNode * function,
		ExpressionNode * expr_node ) {
	std::vector<ast::ptr<ast::Expr>> args;
	buildMoveList( expr_node, args );
	return new ast::UntypedExpr( location,
		maybeMoveBuild( function ),
		std::move( args )
	);
} // build_func

ast::Expr * build_compoundLiteral( const CodeLocation & location,
		DeclarationNode * decl_node,
		InitializerNode * kids ) {
	// compound literal type
	ast::Decl * newDecl = maybeBuild( decl_node );
	// non-sue compound-literal type
	if ( ast::DeclWithType * newDeclWithType = dynamic_cast<ast::DeclWithType *>( newDecl ) ) {
		return new ast::CompoundLiteralExpr( location,
			newDeclWithType->get_type(),
			maybeMoveBuild( kids ) );
	// these types do not have associated type information
	} else if ( auto newDeclStructDecl = dynamic_cast<ast::StructDecl *>( newDecl ) ) {
		if ( newDeclStructDecl->body ) {
			return new ast::CompoundLiteralExpr( location,
				new ast::StructInstType( newDeclStructDecl ),
				maybeMoveBuild( kids ) );
		} else {
			return new ast::CompoundLiteralExpr( location,
				new ast::StructInstType( newDeclStructDecl->name ),
				maybeMoveBuild( kids ) );
		} // if
	} else if ( auto newDeclUnionDecl = dynamic_cast<ast::UnionDecl *>( newDecl )  ) {
		if ( newDeclUnionDecl->body ) {
			return new ast::CompoundLiteralExpr( location,
				new ast::UnionInstType( newDeclUnionDecl ),
				maybeMoveBuild( kids ) );
		} else {
			return new ast::CompoundLiteralExpr( location,
				new ast::UnionInstType( newDeclUnionDecl->name ),
				maybeMoveBuild( kids ) );
		} // if
	} else if ( auto newDeclEnumDecl = dynamic_cast<ast::EnumDecl *>( newDecl )  ) {
		if ( newDeclEnumDecl->body ) {
			return new ast::CompoundLiteralExpr( location,
				new ast::EnumInstType( newDeclEnumDecl ),
				maybeMoveBuild( kids ) );
		} else {
			return new ast::CompoundLiteralExpr( location,
				new ast::EnumInstType( newDeclEnumDecl->name ),
				maybeMoveBuild( kids ) );
		} // if
	} else {
		assert( false );
	} // if
} // build_compoundLiteral

ast::Expr * build_va_arg( const CodeLocation & location,
		ExpressionNode * function, DeclarationNode * declaration ) {
	return build_func( location,
		new ExpressionNode(
			build_varref( location, new std::string( "__builtin_va_arg" ) ) ),
		function->set_last( new ExpressionNode( new ast::TypeExpr( location,
			maybeMoveBuildType( declaration ) ) ) )
	);
}

// Local Variables: //
// tab-width: 4 //
// End: //
