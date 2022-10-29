//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ExpressionNode.cc --
//
// Author           : Peter A. Buhr
// Created On       : Sat May 16 13:17:07 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Aug  7 09:18:56 2021
// Update Count     : 1077
//

#include <cassert>                 // for assert
#include <stdio.h>                 // for sscanf, size_t
#include <climits>                 // for LLONG_MAX, LONG_MAX, INT_MAX, UINT...
#include <list>                    // for list
#include <sstream>                 // for basic_istream::operator>>, basic_i...
#include <string>                  // for string, operator+, operator==

#include "Common/SemanticError.h"  // for SemanticError
#include "Common/utility.h"        // for maybeMoveBuild, maybeBuild, CodeLo...
#include "ParseNode.h"             // for ExpressionNode, maybeMoveBuildType
#include "SynTree/Constant.h"      // for Constant
#include "SynTree/Declaration.h"   // for EnumDecl, StructDecl, UnionDecl
#include "SynTree/Expression.h"    // for Expression, ConstantExpr, NameExpr
#include "SynTree/Statement.h"     // for CompoundStmt, Statement
#include "SynTree/Type.h"          // for BasicType, Type, Type::Qualifiers
#include "parserutility.h"         // for notZeroExpr

class Initializer;

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

extern const Type::Qualifiers noQualifiers;				// no qualifiers on constants

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

Expression * build_constantInteger( string & str ) {
	static const BasicType::Kind kind[2][6] = {
		// short (h) must be before char (hh) because shorter type has the longer suffix
		{ BasicType::ShortSignedInt, BasicType::SignedChar, BasicType::SignedInt, BasicType::LongSignedInt, BasicType::LongLongSignedInt, /* BasicType::SignedInt128 */ BasicType::LongLongSignedInt, },
		{ BasicType::ShortUnsignedInt, BasicType::UnsignedChar, BasicType::UnsignedInt, BasicType::LongUnsignedInt, BasicType::LongLongUnsignedInt, /* BasicType::UnsignedInt128 */ BasicType::LongLongUnsignedInt, },
	};

	static const char * lnthsInt[2][6] = {
		{ "int16_t",  "int8_t",  "int32_t",  "int64_t",  "size_t",  "uintptr_t", },
		{ "uint16_t", "uint8_t", "uint32_t", "uint64_t", "size_t",  "uintptr_t", },
	}; // lnthsInt

	string str2( "0x0" );
	unsigned long long int v, v2 = 0;					// converted integral value
	Expression * ret, * ret2;

	int type = -1;										// 0 => short, 1 => char, 2 => int, 3 => long int, 4 => long long int, 5 => int128
	int ltype = -1;										// 0 => 16 bits, 1 => 8 bits, 2 => 32 bits, 3 => 64 bits, 4 => size_t, 5 => intptr, 6 => pointer
	bool dec = true, Unsigned = false;					// decimal, unsigned constant

	// special constants
	if ( str == "0" ) {
		ret = new ConstantExpr( Constant( (Type *)new ZeroType( noQualifiers ), str, (unsigned long long int)0 ) );
		goto CLEANUP;
	} // if
	if ( str == "1" ) {
		ret = new ConstantExpr( Constant( (Type *)new OneType( noQualifiers ), str, (unsigned long long int)1 ) );
		goto CLEANUP;
	} // if

	string::size_type posn;

	// 'u' can appear before or after length suffix
	if ( str.find_last_of( "uU" ) != string::npos ) Unsigned = true;

	if ( isdigit( str[str.length() - 1] ) ) {			// no suffix ?
		lnthSuffix( str, type, ltype );					// could have length suffix
	} else {
		// At least one digit in integer constant, so safe to backup while looking for suffix.

		posn = str.find_last_of( "pP" );				// pointer value
		if ( posn != string::npos ) { ltype = 5; str.erase( posn, 1 ); goto FINI; }

		posn = str.find_last_of( "zZ" );				// size_t
		if ( posn != string::npos ) { Unsigned = true; type = 2; ltype = 4; str.erase( posn, 1 ); goto FINI; }

		posn = str.rfind( "hh" );						// char
		if ( posn != string::npos ) { type = 1; str.erase( posn, 2 ); goto FINI; }

		posn = str.rfind( "HH" );						// char
		if ( posn != string::npos ) { type = 1; str.erase( posn, 2 ); goto FINI; }

		posn = str.find_last_of( "hH" );				// short
		if ( posn != string::npos ) { type = 0; str.erase( posn, 1 ); goto FINI; }

		posn = str.find_last_of( "nN" );				// int (natural number)
		if ( posn != string::npos ) { type = 2; str.erase( posn, 1 ); goto FINI; }

		if ( str.rfind( "ll" ) != string::npos || str.rfind( "LL" ) != string::npos ) { type = 4; goto FINI; }

		lnthSuffix( str, type, ltype );					// must be after check for "ll"
	  FINI: ;
	} // if

	// Cannot be just "0"/"1"; sscanf stops at the suffix, if any; value goes over the wall => always generate

#if ! defined(__SIZEOF_INT128__)
	if ( type == 5 ) SemanticError( yylloc, "int128 constant is not supported on this target " + str );
#endif // ! __SIZEOF_INT128__
	
	if ( str[0] == '0' ) {								// radix character ?
		dec = false;
		if ( checkX( str[1] ) ) {						// hex constant ?
			if ( type < 5 ) {							// not L128 ?
				sscanf( (char *)str.c_str(), "%llx", &v );
#if defined(__SIZEOF_INT128__)
			} else {									// hex int128 constant
				unsigned int len = str.length();
				if ( len > (2 + 16 + 16) ) SemanticError( yylloc, "128-bit hexadecimal constant to large " + str );
			  if ( len <= (2 + 16) ) goto FHEX1;		// hex digits < 2^64
				str2 = "0x" + str.substr( len - 16 );
				sscanf( (char *)str2.c_str(), "%llx", &v2 );
				str = str.substr( 0, len - 16 );
			  FHEX1: ;
				sscanf( (char *)str.c_str(), "%llx", &v );
#endif // __SIZEOF_INT128__
			} // if
			//printf( "%llx %llu\n", v, v );
		} else if ( checkB( str[1] ) ) {				// binary constant ?
#if defined(__SIZEOF_INT128__)
			unsigned int len = str.length();
			if ( type == 5 && len > 2 + 64 ) {
				if ( len > 2 + 64 + 64 ) SemanticError( yylloc, "128-bit binary constant to large " + str );
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
				if ( len > 1 + 43 || (len == 1 + 43 && str[0] > '3') ) SemanticError( yylloc, "128-bit octal constant to large " + str );
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
				SemanticError( yylloc, "128-bit decimal constant to large " + str );
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
	ret = new ConstantExpr( Constant( new BasicType( noQualifiers, kind[Unsigned][type] ), str, v ) );
	if ( Unsigned && type < 2 ) {						// hh or h, less than int ?
		// int i = -1uh => 65535 not -1, so cast is necessary for unsigned, which unfortunately eliminates warnings for large values.
		ret = new CastExpr( ret, new BasicType( Type::Qualifiers(), kind[Unsigned][type] ), false );
	} else if ( ltype != -1 ) {							// explicit length ?
		if ( ltype == 6 ) {								// int128, (int128)constant
//			ret = new CastExpr( ret, new BasicType( Type::Qualifiers(), kind[Unsigned][type] ), false );
			ret2 = new ConstantExpr( Constant( new BasicType( noQualifiers, BasicType::LongLongSignedInt ), str2, v2 ) );
			ret = build_compoundLiteral( DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ),
										 new InitializerNode( (InitializerNode *)(new InitializerNode( new ExpressionNode( v2 == 0 ? ret2 : ret ) ))->set_last( new InitializerNode( new ExpressionNode( v2 == 0 ? ret : ret2 ) ) ), true ) );
		} else {										// explicit length, (length_type)constant
			ret = new CastExpr( ret, new TypeInstType( Type::Qualifiers(), lnthsInt[Unsigned][ltype], false ), false );
			if ( ltype == 5 ) {							// pointer, intptr( (uintptr_t)constant )
				ret = build_func( new ExpressionNode( build_varref( new string( "intptr" ) ) ), new ExpressionNode( ret ) );
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


Expression * build_constantFloat( string & str ) {
	static const BasicType::Kind kind[2][12] = {
		{ BasicType::Float, BasicType::Double, BasicType::LongDouble, BasicType::uuFloat80, BasicType::uuFloat128, BasicType::uFloat16, BasicType::uFloat32, BasicType::uFloat32x, BasicType::uFloat64, BasicType::uFloat64x, BasicType::uFloat128, BasicType::uFloat128x },
		{ BasicType::FloatComplex, BasicType::DoubleComplex, BasicType::LongDoubleComplex, BasicType::NUMBER_OF_BASIC_TYPES, BasicType::NUMBER_OF_BASIC_TYPES, BasicType::uFloat16Complex, BasicType::uFloat32Complex, BasicType::uFloat32xComplex, BasicType::uFloat64Complex, BasicType::uFloat64xComplex, BasicType::uFloat128Complex, BasicType::uFloat128xComplex },
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
	Expression * ret = new ConstantExpr( Constant( new BasicType( noQualifiers, kind[complx][type] ), str, v ) );
	if ( explnth ) {									// explicit length ?
		ret = new CastExpr( ret, new BasicType( Type::Qualifiers(), kind[complx][type] ), false );
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

Expression * build_constantChar( string & str ) {
	string units;										// units
	sepString( str, units, '\'' );						// separate constant from units

	Expression * ret = new ConstantExpr( Constant( new BasicType( noQualifiers, BasicType::Char ), str, (unsigned long long int)(unsigned char)str[1] ) );
	if ( units.length() != 0 ) {
		ret = new UntypedExpr( new NameExpr( units ), { ret } );
	} // if

	delete &str;										// created by lex
	return ret;
} // build_constantChar

Expression * build_constantStr( string & str ) {
	assert( str.length() > 0 );
	string units;										// units
	sepString( str, units, '"' );						// separate constant from units

	Type * strtype;
	switch ( str[0] ) {									// str has >= 2 characters, i.e, null string "" => safe to look at subscripts 0/1
	  case 'u':
		if ( str[1] == '8' ) goto Default;				// utf-8 characters => array of char
		// lookup type of associated typedef
		strtype = new TypeInstType( Type::Qualifiers( ), "char16_t", false );
		break;
	  case 'U':
		strtype = new TypeInstType( Type::Qualifiers( ), "char32_t", false );
		break;
	  case 'L':
		strtype = new TypeInstType( Type::Qualifiers( ), "wchar_t", false );
		break;
	  Default:											// char default string type
	  default:
		strtype = new BasicType( Type::Qualifiers( ), BasicType::Char );
	} // switch
	ArrayType * at = new ArrayType( noQualifiers, strtype,
									new ConstantExpr( Constant::from_ulong( str.size() + 1 - 2 ) ), // +1 for '\0' and -2 for '"'
									false, false );
	Expression * ret = new ConstantExpr( Constant( at, str, std::nullopt ) );
	if ( units.length() != 0 ) {
		ret = new UntypedExpr( new NameExpr( units ), { ret } );
	} // if

	delete &str;										// created by lex
	return ret;
} // build_constantStr

Expression * build_field_name_FLOATING_FRACTIONconstant( const string & str ) {
	if ( str.find_first_not_of( "0123456789", 1 ) != string::npos ) SemanticError( yylloc, "invalid tuple index " + str );
	Expression * ret = build_constantInteger( *new string( str.substr(1) ) );
	delete &str;
	return ret;
} // build_field_name_FLOATING_FRACTIONconstant

Expression * build_field_name_FLOATING_DECIMALconstant( const string & str ) {
	if ( str[str.size() - 1] != '.' ) SemanticError( yylloc, "invalid tuple index " + str );
	Expression * ret = build_constantInteger( *new string( str.substr( 0, str.size()-1 ) ) );
	delete &str;
	return ret;
} // build_field_name_FLOATING_DECIMALconstant

Expression * build_field_name_FLOATINGconstant( const string & str ) {
	// str is of the form A.B -> separate at the . and return member expression
	int a, b;
	char dot;
	stringstream ss( str );
	ss >> a >> dot >> b;
	UntypedMemberExpr * ret = new UntypedMemberExpr( new ConstantExpr( Constant::from_int( b ) ), new ConstantExpr( Constant::from_int( a ) ) );
	delete &str;
	return ret;
} // build_field_name_FLOATINGconstant

Expression * make_field_name_fraction_constants( Expression * fieldName, Expression * fracts ) {
	if ( fracts ) {
		if ( UntypedMemberExpr * memberExpr = dynamic_cast< UntypedMemberExpr * >( fracts ) ) {
			memberExpr->set_member( make_field_name_fraction_constants( fieldName, memberExpr->get_aggregate() ) );
			return memberExpr;
		} else {
			return new UntypedMemberExpr( fracts, fieldName );
		} // if
	} // if
	return fieldName;
} // make_field_name_fraction_constants

Expression * build_field_name_fraction_constants( Expression * fieldName, ExpressionNode * fracts ) {
	return make_field_name_fraction_constants( fieldName, maybeMoveBuild< Expression >( fracts ) );
} // build_field_name_fraction_constants

NameExpr * build_varref( const string * name ) {
	NameExpr * expr = new NameExpr( *name );
	delete name;
	return expr;
} // build_varref

QualifiedNameExpr * build_qualified_expr( const DeclarationNode * decl_node, const NameExpr * name ) {
	Declaration * newDecl = maybeBuild< Declaration >(decl_node);
	if ( DeclarationWithType * newDeclWithType = dynamic_cast< DeclarationWithType * >( newDecl ) ) {
		const Type * t = newDeclWithType->get_type();
		if ( t ) {
			if ( const TypeInstType * typeInst = dynamic_cast<const TypeInstType *>( t ) ) {
				newDecl= new EnumDecl( typeInst->name );
			}
		}
	}
	return new QualifiedNameExpr( newDecl, name->name );
}

QualifiedNameExpr * build_qualified_expr( const EnumDecl * decl_node, const NameExpr * name ) {
	EnumDecl * newDecl = const_cast< EnumDecl * >( decl_node );
	return new QualifiedNameExpr( newDecl, name->name );
}

DimensionExpr * build_dimensionref( const string * name ) {
	DimensionExpr * expr = new DimensionExpr( *name );
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

Expression * build_cast( DeclarationNode * decl_node, ExpressionNode * expr_node ) {
	Type * targetType = maybeMoveBuildType( decl_node );
	if ( dynamic_cast< VoidType * >( targetType ) ) {
		delete targetType;
		return new CastExpr( maybeMoveBuild< Expression >(expr_node), false );
	} else {
		return new CastExpr( maybeMoveBuild< Expression >(expr_node), targetType, false );
	} // if
} // build_cast

Expression * build_keyword_cast( AggregateDecl::Aggregate target, ExpressionNode * expr_node ) {
	return new KeywordCastExpr( maybeMoveBuild< Expression >(expr_node), target );
}

Expression * build_virtual_cast( DeclarationNode * decl_node, ExpressionNode * expr_node ) {
	return new VirtualCastExpr( maybeMoveBuild< Expression >( expr_node ), maybeMoveBuildType( decl_node ) );
} // build_virtual_cast

Expression * build_fieldSel( ExpressionNode * expr_node, Expression * member ) {
	return new UntypedMemberExpr( member, maybeMoveBuild< Expression >(expr_node) );
} // build_fieldSel

Expression * build_pfieldSel( ExpressionNode * expr_node, Expression * member ) {
	UntypedExpr * deref = new UntypedExpr( new NameExpr( "*?" ) );
	deref->location = expr_node->location;
	deref->get_args().push_back( maybeMoveBuild< Expression >(expr_node) );
	UntypedMemberExpr * ret = new UntypedMemberExpr( member, deref );
	return ret;
} // build_pfieldSel

Expression * build_offsetOf( DeclarationNode * decl_node, NameExpr * member ) {
	Expression * ret = new UntypedOffsetofExpr( maybeMoveBuildType( decl_node ), member->get_name() );
	delete member;
	return ret;
} // build_offsetOf

Expression * build_and_or( ExpressionNode * expr_node1, ExpressionNode * expr_node2, bool kind ) {
	return new LogicalExpr( notZeroExpr( maybeMoveBuild< Expression >(expr_node1) ), notZeroExpr( maybeMoveBuild< Expression >(expr_node2) ), kind );
} // build_and_or

Expression * build_unary_val( OperKinds op, ExpressionNode * expr_node ) {
	list< Expression * > args;
	args.push_back( maybeMoveBuild< Expression >(expr_node) );
	return new UntypedExpr( new NameExpr( OperName[ (int)op ] ), args );
} // build_unary_val

Expression * build_unary_ptr( OperKinds op, ExpressionNode * expr_node ) {
	list< Expression * > args;
	args.push_back(  maybeMoveBuild< Expression >(expr_node) ); // xxx -- this is exactly the same as the val case now, refactor this code.
	return new UntypedExpr( new NameExpr( OperName[ (int)op ] ), args );
} // build_unary_ptr

Expression * build_binary_val( OperKinds op, ExpressionNode * expr_node1, ExpressionNode * expr_node2 ) {
	list< Expression * > args;
	args.push_back( maybeMoveBuild< Expression >(expr_node1) );
	args.push_back( maybeMoveBuild< Expression >(expr_node2) );
	return new UntypedExpr( new NameExpr( OperName[ (int)op ] ), args );
} // build_binary_val

Expression * build_binary_ptr( OperKinds op, ExpressionNode * expr_node1, ExpressionNode * expr_node2 ) {
	list< Expression * > args;
	args.push_back( maybeMoveBuild< Expression >(expr_node1) );
	args.push_back( maybeMoveBuild< Expression >(expr_node2) );
	return new UntypedExpr( new NameExpr( OperName[ (int)op ] ), args );
} // build_binary_ptr

Expression * build_cond( ExpressionNode * expr_node1, ExpressionNode * expr_node2, ExpressionNode * expr_node3 ) {
	return new ConditionalExpr( notZeroExpr( maybeMoveBuild< Expression >(expr_node1) ), maybeMoveBuild< Expression >(expr_node2), maybeMoveBuild< Expression >(expr_node3) );
} // build_cond

Expression * build_tuple( ExpressionNode * expr_node ) {
	list< Expression * > exprs;
	buildMoveList( expr_node, exprs );
	return new UntypedTupleExpr( exprs );;
} // build_tuple

Expression * build_func( ExpressionNode * function, ExpressionNode * expr_node ) {
	list< Expression * > args;
	buildMoveList( expr_node, args );
	return new UntypedExpr( maybeMoveBuild< Expression >(function), args );
} // build_func

Expression * build_compoundLiteral( DeclarationNode * decl_node, InitializerNode * kids ) {
	Declaration * newDecl = maybeBuild< Declaration >(decl_node); // compound literal type
	if ( DeclarationWithType * newDeclWithType = dynamic_cast< DeclarationWithType * >( newDecl ) ) { // non-sue compound-literal type
		return new CompoundLiteralExpr( newDeclWithType->get_type(), maybeMoveBuild< Initializer >(kids) );
	// these types do not have associated type information
	} else if ( StructDecl * newDeclStructDecl = dynamic_cast< StructDecl * >( newDecl )  ) {
		if ( newDeclStructDecl->has_body() ) {
			return new CompoundLiteralExpr( new StructInstType( Type::Qualifiers(), newDeclStructDecl ), maybeMoveBuild< Initializer >(kids) );
		} else {
			return new CompoundLiteralExpr( new StructInstType( Type::Qualifiers(), newDeclStructDecl->get_name() ), maybeMoveBuild< Initializer >(kids) );
		} // if
	} else if ( UnionDecl * newDeclUnionDecl = dynamic_cast< UnionDecl * >( newDecl )  ) {
		if ( newDeclUnionDecl->has_body() ) {
			return new CompoundLiteralExpr( new UnionInstType( Type::Qualifiers(), newDeclUnionDecl ), maybeMoveBuild< Initializer >(kids) );
		} else {
			return new CompoundLiteralExpr( new UnionInstType( Type::Qualifiers(), newDeclUnionDecl->get_name() ), maybeMoveBuild< Initializer >(kids) );
		} // if
	} else if ( EnumDecl * newDeclEnumDecl = dynamic_cast< EnumDecl * >( newDecl )  ) {
		if ( newDeclEnumDecl->has_body() ) {
			return new CompoundLiteralExpr( new EnumInstType( Type::Qualifiers(), newDeclEnumDecl ), maybeMoveBuild< Initializer >(kids) );
		} else {
			return new CompoundLiteralExpr( new EnumInstType( Type::Qualifiers(), newDeclEnumDecl->get_name() ), maybeMoveBuild< Initializer >(kids) );
		} // if
	} else {
		assert( false );
	} // if
} // build_compoundLiteral

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
