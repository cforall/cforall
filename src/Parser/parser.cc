/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 38 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

#define YYDEBUG_LEXER_TEXT( yylval )					// lexer loads this up each time
#define YYDEBUG 1										// get the pretty debugging code to compile
#define YYERROR_VERBOSE									// more information in syntax errors

#undef __GNUC_MINOR__

#include <cstdio>
#include <sstream>
#include <stack>
using namespace std;

#include "DeclarationNode.hpp"                          // for DeclarationNode, ...
#include "ExpressionNode.hpp"                           // for ExpressionNode, ...
#include "InitializerNode.hpp"                          // for InitializerNode, ...
#include "ParserTypes.hpp"
#include "StatementNode.hpp"                            // for build_...
#include "TypedefTable.hpp"
#include "TypeData.hpp"
#include "AST/Type.hpp"                                 // for BasicType, BasicKind
#include "Common/SemanticError.hpp"                     // error_str
#include "Common/Utility.hpp"                           // for maybeMoveBuild, maybeBuild, CodeLo...

// lex uses __null in a boolean context, it's fine.
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wparentheses-equality"
#endif

extern DeclarationNode * parseTree;
extern ast::Linkage::Spec linkage;
extern TypedefTable typedefTable;

stack<ast::Linkage::Spec> linkageStack;

bool appendStr( string & to, string & from ) {
	// 1. Multiple strings are concatenated into a single string but not combined internally. The reason is that
	//    "\x12" "3" is treated as 2 characters versus 1 because "escape sequences are converted into single members of
	//    the execution character set just prior to adjacent string literal concatenation" (C11, Section 6.4.5-8). It is
	//    easier to let the C compiler handle this case.
	//
	// 2. String encodings are transformed into canonical form (one encoding at start) so the encoding can be found
	//    without searching the string, e.g.: "abc" L"def" L"ghi" => L"abc" "def" "ghi". Multiple encodings must match,
	//    e.g., u"a" U"b" L"c" is disallowed.

	if ( from[0] != '"' ) {								// encoding ?
		if ( to[0] != '"' ) {							// encoding ?
			if ( to[0] != from[0] || to[1] != from[1] ) { // different encodings ?
				yyerror( "non-matching string encodings for string-literal concatenation" );
				return false;							// parse error, must call YYERROR in action
			} else if ( from[1] == '8' ) {
				from.erase( 0, 1 );						// remove 2nd encoding
			} // if
		} else {
			if ( from[1] == '8' ) {						// move encoding to start
				to = "u8" + to;
				from.erase( 0, 1 );						// remove 2nd encoding
			} else {
				to = from[0] + to;
			} // if
		} // if
		from.erase( 0, 1 );								// remove 2nd encoding
	} // if
	to += " " + from;									// concatenated into single string
	return true;
} // appendStr

DeclarationNode * distAttr( DeclarationNode * typeSpec, DeclarationNode * declList ) {
	// Distribute type specifier across all declared variables, e.g., static, const, __attribute__.
	assert( declList );

	// Do not distribute attributes for aggregates because the attributes surrounding the aggregate belong it not the
	// variables in the declaration list, e.g.,
	//
	//   struct __attribute__(( aligned(128) )) S { ...
	//   } v1 __attribute__(( aligned(64) )), v2 __attribute__(( aligned(32) )), v3;
	//   struct S v4;
	//
	// v1 => 64, v2 =>32, v3 => 128, v2 => 128
	//
	// Anonymous aggregates are a special case because there is no aggregate to bind the attribute to; hence it floats
	// to the declaration list.
	//
	//   struct __attribute__(( aligned(128) )) /*anonymous */ { ... } v1;
	//
	// v1 => 128

	bool copyattr = ! (typeSpec->type && typeSpec->type->kind == TypeData::Aggregate && ! typeSpec->type->aggregate.anon );

	// addType copies the type information for the aggregate instances from typeSpec into cl's aggInst.aggregate.
	DeclarationNode * cl = (new DeclarationNode)->addType( typeSpec ); // typeSpec IS DELETED!!!

	// Start at second variable in declaration list and clone the type specifiers for each variable.
	for ( DeclarationNode * cur = declList->next ; cur != nullptr; cur = cur->next ) {
		cl->cloneBaseType( cur, copyattr );				// cur is modified
	} // for

	// Add first variable in declaration list with hidden type information in aggInst.aggregate, which is used by
	// extractType to recover the type for the aggregate instances.
	declList->addType( cl, copyattr );					// cl IS DELETED!!!
	return declList;
} // distAttr

void distExt( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
	for ( DeclarationNode *iter = declaration ; iter != nullptr ; iter = iter->next ) {
		iter->set_extension( true );
	} // for
} // distExt

void distInl( DeclarationNode * declaration ) {
	// distribute INLINE across all declarations
	for ( DeclarationNode *iter = declaration ; iter != nullptr ; iter = iter->next ) {
		iter->set_inLine( true );
	} // for
} // distInl

void distQual( DeclarationNode * declaration, DeclarationNode * qualifiers ) {
	// distribute qualifiers across all non-variable declarations in a distribution statemement
	for ( DeclarationNode * iter = declaration ; iter != nullptr ; iter = iter->next ) {
		// SKULLDUGGERY: Distributions are parsed inside out, so qualifiers are added to declarations inside out. Since
		// addQualifiers appends to the back of the list, the forall clauses are in the wrong order (right to left). To
		// get the qualifiers in the correct order and still use addQualifiers (otherwise, 90% of addQualifiers has to
		// be copied to add to front), the appropriate forall pointers are interchanged before calling addQualifiers.
		DeclarationNode * clone = qualifiers->clone();
		if ( qualifiers->type ) {						// forall clause ? (handles SC)
			if ( iter->type->kind == TypeData::Aggregate ) { // struct/union ?
				swap( clone->type->forall, iter->type->aggregate.params );
				iter->addQualifiers( clone );
			} else if ( iter->type->kind == TypeData::AggregateInst && iter->type->aggInst.aggregate->aggregate.body ) { // struct/union ?
				// Create temporary node to hold aggregate, call addQualifiers as above, then put nodes back together.
				DeclarationNode newnode;
				swap( newnode.type, iter->type->aggInst.aggregate );
				swap( clone->type->forall, newnode.type->aggregate.params );
				newnode.addQualifiers( clone );
				swap( newnode.type, iter->type->aggInst.aggregate );
			} else if ( iter->type->kind == TypeData::Function ) { // routines ?
				swap( clone->type->forall, iter->type->forall );
				iter->addQualifiers( clone );
			} // if
		} else {										// just SC qualifiers
			iter->addQualifiers( clone );
		} // if
	} // for
	delete qualifiers;
} // distQual

// There is an ambiguity for inline generic-routine return-types and generic routines.
//   forall( otype T ) struct S { int i; } bar( T ) {}
// Does the forall bind to the struct or the routine, and how would it be possible to explicitly specify the binding.
//   forall( otype T ) struct S { int T; } forall( otype W ) bar( W ) {}
// Currently, the forall is associated with the routine, and the generic type has to be separately defined:
//   forall( otype T ) struct S { int T; };
//   forall( otype W ) bar( W ) {}

void rebindForall( DeclarationNode * declSpec, DeclarationNode * funcDecl ) {
	if ( declSpec->type->kind == TypeData::Aggregate ) { // ignore aggregate definition
		funcDecl->type->forall = declSpec->type->aggregate.params; // move forall from aggregate to function type
		declSpec->type->aggregate.params = nullptr;
	} // if
} // rebindForall

string * build_postfix_name( string * name ) {
	*name = string("__postfix_func_") + *name;
	return name;
} // build_postfix_name

DeclarationNode * fieldDecl( DeclarationNode * typeSpec, DeclarationNode * fieldList ) {
	if ( nullptr == fieldList ) {
		if ( !( typeSpec->type && typeSpec->type->kind == TypeData::Aggregate ) ) {
			stringstream ss;
			// printf( "fieldDecl1 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
			SemanticWarning( yylloc, Warning::SuperfluousDecl, ss.str().c_str() );
			return nullptr;
		} // if
		// printf( "fieldDecl2 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
		fieldList = DeclarationNode::newName( nullptr );
	} // if

	// printf( "fieldDecl3 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout, 0 );
	DeclarationNode * temp = distAttr( typeSpec, fieldList ); // mark all fields in list
	// printf( "fieldDecl4 temp %p\n", temp ); temp->print( std::cout, 0 );
	return temp;
} // fieldDecl

#define NEW_ZERO new ExpressionNode( build_constantInteger( yylloc, *new string( "0" ) ) )
#define NEW_ONE  new ExpressionNode( build_constantInteger( yylloc, *new string( "1" ) ) )
#define UPDOWN( compop, left, right ) (compop == OperKinds::LThan || compop == OperKinds::LEThan ? left : right)
#define MISSING_ANON_FIELD "illegal syntax, missing loop fields with an anonymous loop index is meaningless as loop index is unavailable in loop body."
#define MISSING_LOW "illegal syntax, missing low value for ascanding range so index is uninitialized."
#define MISSING_HIGH "illegal syntax, missing high value for descending range so index is uninitialized."

static ForCtrl * makeForCtrl( const CodeLocation & location, DeclarationNode * init, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	// Wrap both comp/inc if they are non-null.
	if ( comp ) comp = new ExpressionNode( build_binary_val( location,
		compop,
		new ExpressionNode( build_varref( location, new string( *init->name ) ) ),
		comp ) );
	if ( inc ) inc = new ExpressionNode( build_binary_val( location,
		// choose += or -= for upto/downto
		compop == OperKinds::LThan || compop == OperKinds::LEThan ? OperKinds::PlusAssn : OperKinds::MinusAssn,
		new ExpressionNode( build_varref( location, new string( *init->name ) ) ),
		inc ) );
	// The StatementNode call frees init->name, it must happen later.
	return new ForCtrl( new StatementNode( init ), comp, inc );
}

ForCtrl * forCtrl( const CodeLocation & location, DeclarationNode * index, ExpressionNode * start, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( index->initializer ) {
		SemanticError( yylloc, "illegal syntax, direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "illegal syntax, multiple loop indexes disallowed in for-loop declaration." );
	} // if
	DeclarationNode * initDecl = index->addInitializer( new InitializerNode( start ) );
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, string * index, ExpressionNode * start, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ast::ConstantExpr * constant = dynamic_cast<ast::ConstantExpr *>(type->expr.get());
	if ( constant && (constant->rep == "0" || constant->rep == "1") ) {
		type = new ExpressionNode( new ast::CastExpr( location, maybeMoveBuild(type), new ast::BasicType( ast::BasicKind::SignedInt ) ) );
	} // if
	DeclarationNode * initDecl = distAttr(
		DeclarationNode::newTypeof( type, true ),
		DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) )
	);
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

#define MISSING_LOOP_INDEX "illegal syntax, only a single identifier or declaration allowed in initialization, e.g., for ( i; ... ) or for ( int i; ... ). Expression disallowed."

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, ExpressionNode * index, ExpressionNode * start, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index->expr.get()) ) {
		return forCtrl( location, type, new string( identifier->name ), start, compop, comp, inc );
	} else {
		SemanticError( yylloc, MISSING_LOOP_INDEX ); return nullptr;
	} // if
} // forCtrl

ForCtrl * enumRangeCtrl( ExpressionNode * index_expr, OperKinds compop, ExpressionNode * range_over_expr, DeclarationNode * type ) {
	assert( compop == OperKinds::LEThan || compop == OperKinds::GEThan );
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index_expr->expr.get()) ) {
		DeclarationNode * indexDecl =
			DeclarationNode::newName( new std::string(identifier->name) )->addType( type );
		return new ForCtrl( new StatementNode( indexDecl ), range_over_expr, compop );
	} else {
		SemanticError( yylloc, MISSING_LOOP_INDEX ); return nullptr;
	} // if
} // enumRangeCtrl

static void IdentifierBeforeIdentifier( string & identifier1, string & identifier2, const char * kind ) {
	SemanticError( yylloc, "illegal syntax, adjacent identifiers \"%s\" and \"%s\" are not meaningful in an %s.\n"
				   "Possible cause is misspelled type name or missing generic parameter.",
				   identifier1.c_str(), identifier2.c_str(), kind );
} // IdentifierBeforeIdentifier

static void IdentifierBeforeType( string & identifier, const char * kind ) {
	SemanticError( yylloc, "illegal syntax, identifier \"%s\" cannot appear before a %s.\n"
				   "Possible cause is misspelled storage/CV qualifier, misspelled typename, or missing generic parameter.",
				   identifier.c_str(), kind );
} // IdentifierBeforeType

bool forall = false;									// aggregate have one or more forall qualifiers ?

// https://www.gnu.org/software/bison/manual/bison.html#Location-Type
#define YYLLOC_DEFAULT(Cur, Rhs, N)												\
if ( N ) {																		\
	(Cur).first_line   = YYRHSLOC( Rhs, 1 ).first_line;							\
	(Cur).first_column = YYRHSLOC( Rhs, 1 ).first_column;						\
	(Cur).last_line    = YYRHSLOC( Rhs, N ).last_line;							\
	(Cur).last_column  = YYRHSLOC( Rhs, N ).last_column;						\
	(Cur).filename     = YYRHSLOC( Rhs, 1 ).filename;							\
} else {																		\
	(Cur).first_line   = (Cur).last_line = YYRHSLOC( Rhs, 0 ).last_line;		\
	(Cur).first_column = (Cur).last_column = YYRHSLOC( Rhs, 0 ).last_column;	\
	(Cur).filename     = YYRHSLOC( Rhs, 0 ).filename;							\
}

#line 349 "Parser/parser.cc"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_PARSER_PARSER_HH_INCLUDED
# define YY_YY_PARSER_PARSER_HH_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TYPEDEF = 258,
    EXTERN = 259,
    STATIC = 260,
    AUTO = 261,
    REGISTER = 262,
    THREADLOCALGCC = 263,
    THREADLOCALC11 = 264,
    INLINE = 265,
    FORTRAN = 266,
    NORETURN = 267,
    CONST = 268,
    VOLATILE = 269,
    RESTRICT = 270,
    ATOMIC = 271,
    FORALL = 272,
    MUTEX = 273,
    VIRTUAL = 274,
    VTABLE = 275,
    COERCE = 276,
    VOID = 277,
    CHAR = 278,
    SHORT = 279,
    INT = 280,
    LONG = 281,
    FLOAT = 282,
    DOUBLE = 283,
    SIGNED = 284,
    UNSIGNED = 285,
    BOOL = 286,
    COMPLEX = 287,
    IMAGINARY = 288,
    INT128 = 289,
    UINT128 = 290,
    FLOAT80 = 291,
    uuFLOAT128 = 292,
    FLOAT16 = 293,
    FLOAT32 = 294,
    FLOAT32X = 295,
    FLOAT64 = 296,
    FLOAT64X = 297,
    FLOAT128 = 298,
    FLOAT128X = 299,
    FLOAT32X4 = 300,
    FLOAT64X2 = 301,
    SVFLOAT32 = 302,
    SVFLOAT64 = 303,
    SVBOOL = 304,
    DECIMAL32 = 305,
    DECIMAL64 = 306,
    DECIMAL128 = 307,
    ZERO_T = 308,
    ONE_T = 309,
    SIZEOF = 310,
    TYPEOF = 311,
    VA_LIST = 312,
    VA_ARG = 313,
    AUTO_TYPE = 314,
    COUNTOF = 315,
    OFFSETOF = 316,
    BASETYPEOF = 317,
    TYPEID = 318,
    ENUM = 319,
    STRUCT = 320,
    UNION = 321,
    EXCEPTION = 322,
    GENERATOR = 323,
    COROUTINE = 324,
    MONITOR = 325,
    THREAD = 326,
    OTYPE = 327,
    FTYPE = 328,
    DTYPE = 329,
    TTYPE = 330,
    TRAIT = 331,
    LABEL = 332,
    SUSPEND = 333,
    ATTRIBUTE = 334,
    EXTENSION = 335,
    IF = 336,
    ELSE = 337,
    SWITCH = 338,
    CASE = 339,
    DEFAULT = 340,
    DO = 341,
    WHILE = 342,
    FOR = 343,
    BREAK = 344,
    CONTINUE = 345,
    GOTO = 346,
    RETURN = 347,
    CHOOSE = 348,
    FALLTHRU = 349,
    FALLTHROUGH = 350,
    WITH = 351,
    WHEN = 352,
    WAITFOR = 353,
    WAITUNTIL = 354,
    CORUN = 355,
    COFOR = 356,
    DISABLE = 357,
    ENABLE = 358,
    TRY = 359,
    THROW = 360,
    THROWRESUME = 361,
    AT = 362,
    ASM = 363,
    ALIGNAS = 364,
    ALIGNOF = 365,
    GENERIC = 366,
    STATICASSERT = 367,
    IDENTIFIER = 368,
    TYPEDIMname = 369,
    TYPEDEFname = 370,
    TYPEGENname = 371,
    TIMEOUT = 372,
    WAND = 373,
    WOR = 374,
    CATCH = 375,
    RECOVER = 376,
    CATCHRESUME = 377,
    FIXUP = 378,
    FINALLY = 379,
    INTEGERconstant = 380,
    CHARACTERconstant = 381,
    STRINGliteral = 382,
    DIRECTIVE = 383,
    C23_ATTRIBUTE = 384,
    FLOATING_DECIMALconstant = 385,
    FLOATING_FRACTIONconstant = 386,
    FLOATINGconstant = 387,
    ARROW = 388,
    ICR = 389,
    DECR = 390,
    LS = 391,
    RS = 392,
    LE = 393,
    GE = 394,
    EQ = 395,
    NE = 396,
    ANDAND = 397,
    OROR = 398,
    ATTR = 399,
    ELLIPSIS = 400,
    EXPassign = 401,
    MULTassign = 402,
    DIVassign = 403,
    MODassign = 404,
    PLUSassign = 405,
    MINUSassign = 406,
    LSassign = 407,
    RSassign = 408,
    ANDassign = 409,
    ERassign = 410,
    ORassign = 411,
    ErangeUp = 412,
    ErangeUpEq = 413,
    ErangeDown = 414,
    ErangeDownEq = 415,
    ATassign = 416,
    THEN = 417
  };
#endif
/* Tokens.  */
#define TYPEDEF 258
#define EXTERN 259
#define STATIC 260
#define AUTO 261
#define REGISTER 262
#define THREADLOCALGCC 263
#define THREADLOCALC11 264
#define INLINE 265
#define FORTRAN 266
#define NORETURN 267
#define CONST 268
#define VOLATILE 269
#define RESTRICT 270
#define ATOMIC 271
#define FORALL 272
#define MUTEX 273
#define VIRTUAL 274
#define VTABLE 275
#define COERCE 276
#define VOID 277
#define CHAR 278
#define SHORT 279
#define INT 280
#define LONG 281
#define FLOAT 282
#define DOUBLE 283
#define SIGNED 284
#define UNSIGNED 285
#define BOOL 286
#define COMPLEX 287
#define IMAGINARY 288
#define INT128 289
#define UINT128 290
#define FLOAT80 291
#define uuFLOAT128 292
#define FLOAT16 293
#define FLOAT32 294
#define FLOAT32X 295
#define FLOAT64 296
#define FLOAT64X 297
#define FLOAT128 298
#define FLOAT128X 299
#define FLOAT32X4 300
#define FLOAT64X2 301
#define SVFLOAT32 302
#define SVFLOAT64 303
#define SVBOOL 304
#define DECIMAL32 305
#define DECIMAL64 306
#define DECIMAL128 307
#define ZERO_T 308
#define ONE_T 309
#define SIZEOF 310
#define TYPEOF 311
#define VA_LIST 312
#define VA_ARG 313
#define AUTO_TYPE 314
#define COUNTOF 315
#define OFFSETOF 316
#define BASETYPEOF 317
#define TYPEID 318
#define ENUM 319
#define STRUCT 320
#define UNION 321
#define EXCEPTION 322
#define GENERATOR 323
#define COROUTINE 324
#define MONITOR 325
#define THREAD 326
#define OTYPE 327
#define FTYPE 328
#define DTYPE 329
#define TTYPE 330
#define TRAIT 331
#define LABEL 332
#define SUSPEND 333
#define ATTRIBUTE 334
#define EXTENSION 335
#define IF 336
#define ELSE 337
#define SWITCH 338
#define CASE 339
#define DEFAULT 340
#define DO 341
#define WHILE 342
#define FOR 343
#define BREAK 344
#define CONTINUE 345
#define GOTO 346
#define RETURN 347
#define CHOOSE 348
#define FALLTHRU 349
#define FALLTHROUGH 350
#define WITH 351
#define WHEN 352
#define WAITFOR 353
#define WAITUNTIL 354
#define CORUN 355
#define COFOR 356
#define DISABLE 357
#define ENABLE 358
#define TRY 359
#define THROW 360
#define THROWRESUME 361
#define AT 362
#define ASM 363
#define ALIGNAS 364
#define ALIGNOF 365
#define GENERIC 366
#define STATICASSERT 367
#define IDENTIFIER 368
#define TYPEDIMname 369
#define TYPEDEFname 370
#define TYPEGENname 371
#define TIMEOUT 372
#define WAND 373
#define WOR 374
#define CATCH 375
#define RECOVER 376
#define CATCHRESUME 377
#define FIXUP 378
#define FINALLY 379
#define INTEGERconstant 380
#define CHARACTERconstant 381
#define STRINGliteral 382
#define DIRECTIVE 383
#define C23_ATTRIBUTE 384
#define FLOATING_DECIMALconstant 385
#define FLOATING_FRACTIONconstant 386
#define FLOATINGconstant 387
#define ARROW 388
#define ICR 389
#define DECR 390
#define LS 391
#define RS 392
#define LE 393
#define GE 394
#define EQ 395
#define NE 396
#define ANDAND 397
#define OROR 398
#define ATTR 399
#define ELLIPSIS 400
#define EXPassign 401
#define MULTassign 402
#define DIVassign 403
#define MODassign 404
#define PLUSassign 405
#define MINUSassign 406
#define LSassign 407
#define RSassign 408
#define ANDassign 409
#define ERassign 410
#define ORassign 411
#define ErangeUp 412
#define ErangeUpEq 413
#define ErangeDown 414
#define ErangeDownEq 415
#define ATassign 416
#define THEN 417

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

	// A raw token can be used.
	Token tok;

	// The general node types hold some generic node or list of nodes.
	DeclarationNode * decl;
	InitializerNode * init;
	ExpressionNode * expr;
	StatementNode * stmt;
	ClauseNode * clause;
	TypeData * type;

	// Special "nodes" containing compound information.
	CondCtrl * ifctrl;
	ForCtrl * forctrl;
	LabelNode * labels;

	// Various flags and single values that become fields later.
	ast::AggregateDecl::Aggregate aggKey;
	ast::TypeDecl::Kind tclass;
	OperKinds oper;
	bool is_volatile;
	EnumHiding enum_hiding;
	ast::ExceptionKind except_kind;
	// String passes ownership with it.
	std::string * str;

	// Narrower node types are used to avoid constant unwrapping.
	ast::WaitForStmt * wfs;
	ast::WaitUntilStmt::ClauseNode * wucn;
	ast::GenericExpr * genexpr;

#line 758 "Parser/parser.cc"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;
int yyparse (void);

#endif /* !YY_YY_PARSER_PARSER_HH_INCLUDED  */



#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  157
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   31999

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  190
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1133
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2172

#define YYUNDEFTOK  2
#define YYMAXUTOK   417


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   179,     2,     2,     2,   183,   176,     2,
     164,   166,   175,   177,   170,   178,   167,   182,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   171,   189,
     184,   188,   185,   187,   165,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   168,   181,   169,   174,     2,   173,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   172,   186,   163,   180,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   645,   645,   649,   656,   657,   658,   659,   660,   664,
     665,   666,   667,   668,   669,   670,   671,   675,   676,   680,
     681,   686,   687,   688,   692,   696,   697,   708,   710,   712,
     714,   715,   717,   719,   721,   723,   733,   735,   737,   739,
     741,   743,   748,   749,   760,   765,   770,   771,   776,   782,
     784,   786,   792,   794,   796,   798,   800,   820,   823,   825,
     827,   829,   831,   833,   835,   837,   839,   841,   843,   845,
     854,   855,   859,   860,   862,   864,   866,   868,   870,   875,
     877,   879,   887,   888,   896,   899,   900,   902,   907,   923,
     925,   927,   929,   931,   933,   935,   940,   942,   945,   947,
     952,   954,   959,   960,   962,   966,   967,   968,   969,   973,
     974,   976,   978,   980,   982,   984,   986,   988,   995,   996,
     997,   998,  1002,  1003,  1007,  1008,  1013,  1014,  1016,  1018,
    1023,  1024,  1026,  1031,  1032,  1034,  1039,  1040,  1042,  1044,
    1046,  1051,  1052,  1054,  1059,  1060,  1065,  1066,  1071,  1072,
    1077,  1078,  1083,  1084,  1089,  1090,  1092,  1097,  1102,  1103,
    1107,  1109,  1114,  1117,  1120,  1125,  1126,  1134,  1140,  1141,
    1145,  1146,  1150,  1151,  1155,  1156,  1157,  1158,  1159,  1160,
    1161,  1162,  1163,  1164,  1165,  1171,  1173,  1175,  1177,  1179,
    1184,  1185,  1187,  1189,  1194,  1195,  1201,  1202,  1208,  1209,
    1210,  1211,  1212,  1213,  1214,  1215,  1216,  1217,  1218,  1219,
    1220,  1221,  1223,  1224,  1230,  1232,  1242,  1244,  1252,  1253,
    1258,  1260,  1262,  1264,  1266,  1270,  1271,  1273,  1279,  1308,
    1311,  1313,  1315,  1325,  1327,  1329,  1334,  1339,  1341,  1343,
    1345,  1353,  1354,  1356,  1360,  1362,  1366,  1368,  1369,  1371,
    1373,  1378,  1379,  1383,  1388,  1389,  1393,  1395,  1400,  1402,
    1407,  1409,  1411,  1413,  1418,  1420,  1422,  1424,  1429,  1431,
    1436,  1437,  1459,  1461,  1465,  1468,  1470,  1473,  1475,  1478,
    1480,  1485,  1490,  1492,  1497,  1502,  1504,  1506,  1508,  1510,
    1515,  1517,  1520,  1522,  1527,  1533,  1536,  1538,  1543,  1549,
    1551,  1556,  1562,  1566,  1568,  1571,  1573,  1578,  1585,  1587,
    1592,  1598,  1600,  1605,  1611,  1614,  1618,  1629,  1634,  1639,
    1650,  1652,  1654,  1656,  1661,  1663,  1665,  1670,  1671,  1673,
    1678,  1680,  1685,  1687,  1689,  1691,  1694,  1698,  1701,  1705,
    1707,  1709,  1711,  1713,  1715,  1717,  1719,  1721,  1723,  1725,
    1730,  1731,  1735,  1741,  1749,  1754,  1755,  1759,  1760,  1765,
    1769,  1770,  1773,  1775,  1780,  1783,  1785,  1787,  1790,  1792,
    1797,  1802,  1803,  1807,  1812,  1814,  1819,  1821,  1826,  1828,
    1830,  1835,  1840,  1845,  1850,  1852,  1854,  1859,  1861,  1867,
    1868,  1872,  1873,  1874,  1875,  1879,  1884,  1885,  1887,  1889,
    1891,  1895,  1899,  1900,  1904,  1906,  1908,  1910,  1912,  1918,
    1919,  1925,  1926,  1930,  1931,  1936,  1938,  1947,  1948,  1950,
    1955,  1960,  1971,  1972,  1976,  1977,  1983,  1984,  1988,  1990,
    1994,  1996,  2000,  2001,  2005,  2006,  2010,  2011,  2012,  2016,
    2018,  2033,  2034,  2035,  2036,  2038,  2042,  2044,  2048,  2055,
    2057,  2059,  2067,  2069,  2074,  2075,  2077,  2079,  2081,  2091,
    2093,  2105,  2108,  2113,  2115,  2121,  2126,  2131,  2142,  2149,
    2154,  2156,  2158,  2164,  2168,  2175,  2177,  2178,  2179,  2195,
    2197,  2200,  2202,  2205,  2210,  2211,  2215,  2216,  2217,  2218,
    2227,  2228,  2229,  2238,  2239,  2240,  2244,  2245,  2246,  2255,
    2256,  2257,  2262,  2263,  2272,  2273,  2278,  2280,  2284,  2286,
    2288,  2290,  2297,  2302,  2307,  2308,  2310,  2320,  2321,  2326,
    2328,  2330,  2332,  2334,  2336,  2339,  2341,  2343,  2348,  2354,
    2356,  2358,  2360,  2362,  2364,  2366,  2368,  2370,  2372,  2374,
    2376,  2378,  2380,  2382,  2384,  2387,  2389,  2391,  2393,  2395,
    2397,  2399,  2401,  2403,  2405,  2407,  2409,  2411,  2413,  2415,
    2417,  2419,  2421,  2426,  2427,  2431,  2437,  2438,  2444,  2445,
    2447,  2449,  2451,  2456,  2458,  2463,  2464,  2466,  2468,  2473,
    2475,  2477,  2479,  2481,  2483,  2488,  2489,  2491,  2493,  2498,
    2500,  2499,  2503,  2511,  2512,  2514,  2516,  2521,  2522,  2524,
    2529,  2530,  2532,  2534,  2539,  2541,  2543,  2548,  2550,  2552,
    2554,  2555,  2557,  2562,  2564,  2566,  2571,  2572,  2576,  2577,
    2584,  2583,  2588,  2587,  2597,  2596,  2607,  2606,  2616,  2621,
    2622,  2627,  2633,  2651,  2652,  2656,  2658,  2660,  2665,  2667,
    2669,  2671,  2676,  2678,  2683,  2685,  2694,  2695,  2700,  2709,
    2714,  2716,  2718,  2727,  2729,  2730,  2731,  2733,  2735,  2736,
    2741,  2742,  2743,  2748,  2750,  2753,  2756,  2763,  2764,  2765,
    2771,  2776,  2778,  2784,  2785,  2791,  2792,  2796,  2804,  2811,
    2824,  2823,  2827,  2830,  2829,  2838,  2842,  2846,  2848,  2854,
    2855,  2860,  2865,  2874,  2875,  2877,  2883,  2885,  2890,  2891,
    2897,  2898,  2899,  2908,  2909,  2911,  2912,  2917,  2918,  2919,
    2921,  2927,  2928,  2930,  2931,  2932,  2934,  2936,  2943,  2944,
    2946,  2948,  2953,  2954,  2963,  2965,  2970,  2972,  2977,  2978,
    2980,  2983,  2985,  2989,  2990,  2991,  2993,  2995,  3003,  3005,
    3010,  3011,  3012,  3017,  3018,  3023,  3024,  3025,  3026,  3030,
    3031,  3036,  3037,  3038,  3039,  3040,  3054,  3055,  3060,  3061,
    3066,  3068,  3070,  3072,  3074,  3097,  3098,  3104,  3105,  3111,
    3110,  3120,  3119,  3123,  3129,  3131,  3141,  3142,  3144,  3148,
    3153,  3155,  3157,  3159,  3165,  3166,  3170,  3171,  3176,  3178,
    3185,  3187,  3188,  3190,  3195,  3197,  3199,  3204,  3206,  3211,
    3216,  3224,  3229,  3231,  3236,  3241,  3242,  3247,  3248,  3252,
    3253,  3254,  3260,  3262,  3264,  3270,  3272,  3277,  3279,  3285,
    3286,  3290,  3294,  3298,  3300,  3312,  3314,  3316,  3318,  3320,
    3322,  3324,  3325,  3330,  3333,  3332,  3344,  3343,  3356,  3355,
    3369,  3368,  3382,  3381,  3394,  3399,  3405,  3407,  3413,  3414,
    3425,  3432,  3437,  3443,  3446,  3449,  3453,  3459,  3462,  3465,
    3470,  3471,  3472,  3473,  3477,  3485,  3486,  3498,  3499,  3503,
    3504,  3509,  3511,  3513,  3515,  3520,  3521,  3527,  3528,  3530,
    3535,  3536,  3538,  3573,  3575,  3578,  3583,  3585,  3586,  3588,
    3593,  3595,  3597,  3599,  3604,  3606,  3608,  3610,  3612,  3614,
    3616,  3621,  3623,  3625,  3627,  3636,  3638,  3639,  3644,  3646,
    3648,  3650,  3652,  3657,  3659,  3661,  3663,  3668,  3670,  3672,
    3674,  3676,  3678,  3690,  3691,  3692,  3696,  3698,  3700,  3702,
    3704,  3709,  3711,  3713,  3715,  3720,  3722,  3724,  3726,  3728,
    3730,  3742,  3747,  3752,  3754,  3755,  3757,  3762,  3764,  3766,
    3768,  3773,  3775,  3777,  3779,  3781,  3783,  3785,  3790,  3792,
    3794,  3796,  3805,  3807,  3808,  3813,  3815,  3817,  3819,  3821,
    3826,  3828,  3830,  3832,  3837,  3839,  3841,  3843,  3845,  3847,
    3857,  3859,  3862,  3863,  3865,  3870,  3872,  3874,  3879,  3881,
    3883,  3885,  3890,  3892,  3894,  3908,  3910,  3913,  3914,  3916,
    3921,  3923,  3928,  3930,  3932,  3937,  3939,  3944,  3946,  3963,
    3964,  3966,  3971,  3973,  3975,  3977,  3979,  3984,  3985,  3987,
    3989,  3994,  3996,  3998,  4004,  4006,  4009,  4016,  4018,  4027,
    4029,  4031,  4032,  4034,  4036,  4040,  4042,  4047,  4049,  4051,
    4053,  4088,  4089,  4093,  4094,  4097,  4099,  4104,  4106,  4108,
    4110,  4112,  4117,  4118,  4120,  4122,  4127,  4129,  4131,  4137,
    4138,  4140,  4149,  4152,  4154,  4157,  4159,  4161,  4175,  4176,
    4178,  4183,  4185,  4187,  4189,  4191,  4196,  4197,  4199,  4201,
    4206,  4208,  4216,  4217,  4218,  4223,  4224,  4229,  4231,  4233,
    4235,  4237,  4239,  4246,  4248,  4250,  4252,  4254,  4257,  4259,
    4261,  4263,  4265,  4270,  4272,  4274,  4279,  4305,  4306,  4308,
    4312,  4313,  4317,  4319,  4321,  4323,  4325,  4327,  4334,  4336,
    4338,  4340,  4342,  4344,  4349,  4351,  4353,  4358,  4360,  4362,
    4380,  4382,  4387,  4388
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TYPEDEF", "EXTERN", "STATIC", "AUTO",
  "REGISTER", "THREADLOCALGCC", "THREADLOCALC11", "INLINE", "FORTRAN",
  "NORETURN", "CONST", "VOLATILE", "RESTRICT", "ATOMIC", "FORALL", "MUTEX",
  "VIRTUAL", "VTABLE", "COERCE", "VOID", "CHAR", "SHORT", "INT", "LONG",
  "FLOAT", "DOUBLE", "SIGNED", "UNSIGNED", "BOOL", "COMPLEX", "IMAGINARY",
  "INT128", "UINT128", "FLOAT80", "uuFLOAT128", "FLOAT16", "FLOAT32",
  "FLOAT32X", "FLOAT64", "FLOAT64X", "FLOAT128", "FLOAT128X", "FLOAT32X4",
  "FLOAT64X2", "SVFLOAT32", "SVFLOAT64", "SVBOOL", "DECIMAL32",
  "DECIMAL64", "DECIMAL128", "ZERO_T", "ONE_T", "SIZEOF", "TYPEOF",
  "VA_LIST", "VA_ARG", "AUTO_TYPE", "COUNTOF", "OFFSETOF", "BASETYPEOF",
  "TYPEID", "ENUM", "STRUCT", "UNION", "EXCEPTION", "GENERATOR",
  "COROUTINE", "MONITOR", "THREAD", "OTYPE", "FTYPE", "DTYPE", "TTYPE",
  "TRAIT", "LABEL", "SUSPEND", "ATTRIBUTE", "EXTENSION", "IF", "ELSE",
  "SWITCH", "CASE", "DEFAULT", "DO", "WHILE", "FOR", "BREAK", "CONTINUE",
  "GOTO", "RETURN", "CHOOSE", "FALLTHRU", "FALLTHROUGH", "WITH", "WHEN",
  "WAITFOR", "WAITUNTIL", "CORUN", "COFOR", "DISABLE", "ENABLE", "TRY",
  "THROW", "THROWRESUME", "AT", "ASM", "ALIGNAS", "ALIGNOF", "GENERIC",
  "STATICASSERT", "IDENTIFIER", "TYPEDIMname", "TYPEDEFname",
  "TYPEGENname", "TIMEOUT", "WAND", "WOR", "CATCH", "RECOVER",
  "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
  "CHARACTERconstant", "STRINGliteral", "DIRECTIVE", "C23_ATTRIBUTE",
  "FLOATING_DECIMALconstant", "FLOATING_FRACTIONconstant",
  "FLOATINGconstant", "ARROW", "ICR", "DECR", "LS", "RS", "LE", "GE", "EQ",
  "NE", "ANDAND", "OROR", "ATTR", "ELLIPSIS", "EXPassign", "MULTassign",
  "DIVassign", "MODassign", "PLUSassign", "MINUSassign", "LSassign",
  "RSassign", "ANDassign", "ERassign", "ORassign", "ErangeUp",
  "ErangeUpEq", "ErangeDown", "ErangeDownEq", "ATassign", "THEN", "'}'",
  "'('", "'@'", "')'", "'.'", "'['", "']'", "','", "':'", "'{'", "'`'",
  "'^'", "'*'", "'&'", "'+'", "'-'", "'!'", "'~'", "'\\\\'", "'/'", "'%'",
  "'<'", "'>'", "'|'", "'?'", "'='", "';'", "$accept", "push", "pop",
  "constant", "quasi_keyword", "identifier", "identifier_at",
  "identifier_or_type_name", "string_literal", "string_literal_list",
  "primary_expression", "generic_assoc_list", "generic_association",
  "postfix_expression", "field_name_list", "field", "field_name",
  "fraction_constants_opt", "unary_expression", "ptrref_operator",
  "unary_operator", "cast_expression", "qualifier_cast_list",
  "cast_modifier", "exponential_expression", "multiplicative_expression",
  "additive_expression", "shift_expression", "relational_expression",
  "equality_expression", "AND_expression", "exclusive_OR_expression",
  "inclusive_OR_expression", "logical_AND_expression",
  "logical_OR_expression", "conditional_expression", "constant_expression",
  "argument_expression_list_opt", "argument_expression_list",
  "argument_expression", "assignment_expression",
  "assignment_expression_opt", "assignment_operator",
  "simple_assignment_operator", "compound_assignment_operator", "tuple",
  "tuple_expression_list", "comma_expression", "comma_expression_opt",
  "statement", "labeled_statement", "compound_statement",
  "statement_decl_list", "statement_decl", "statement_list_nodecl",
  "expression_statement", "selection_statement", "conditional_declaration",
  "case_value", "case_value_list", "case_label", "case_label_list",
  "case_clause", "switch_clause_list_opt", "switch_clause_list",
  "iteration_statement", "for_control_expression_list",
  "for_control_expression", "enum_key", "downupdowneq", "updown",
  "updowneq", "jump_statement", "fall_through_name", "with_statement",
  "mutex_statement", "when_clause", "when_clause_opt",
  "cast_expression_list", "timeout", "wor", "waitfor",
  "wor_waitfor_clause", "waitfor_statement", "wand", "waituntil",
  "waituntil_clause", "wand_waituntil_clause", "wor_waituntil_clause",
  "waituntil_statement", "corun_statement", "cofor_statement",
  "exception_statement", "handler_clause", "handler_predicate_opt",
  "handler_key", "finally_clause", "exception_declaration",
  "enable_disable_statement", "enable_disable_key", "asm_statement",
  "asm_volatile_opt", "asm_operands_opt", "asm_operands_list",
  "asm_operand", "asm_clobbers_list_opt", "label_list",
  "declaration_list_opt", "declaration_list", "KR_parameter_list_opt",
  "KR_parameter_list", "local_label_declaration_opt",
  "local_label_declaration_list", "local_label_list", "declaration",
  "static_assert", "cfa_declaration", "cfa_variable_declaration",
  "cfa_variable_specifier", "cfa_function_declaration",
  "cfa_function_specifier", "cfa_function_return",
  "cfa_typedef_declaration", "typedef_declaration", "typedef_expression",
  "c_declaration", "declaring_list", "general_function_declarator",
  "declaration_specifier", "invalid_types", "declaration_specifier_nobody",
  "type_specifier", "type_specifier_nobody", "type_qualifier_list_opt",
  "type_qualifier_list", "type_qualifier", "type_qualifier_name", "forall",
  "declaration_qualifier_list", "storage_class_list", "storage_class",
  "basic_type_name", "basic_type_name_type", "vtable_opt", "vtable",
  "default_opt", "basic_declaration_specifier", "basic_type_specifier",
  "direct_type", "indirect_type", "sue_declaration_specifier",
  "sue_type_specifier", "$@1", "sue_declaration_specifier_nobody",
  "sue_type_specifier_nobody", "type_declaration_specifier",
  "type_type_specifier", "type_name", "typegen_name", "elaborated_type",
  "elaborated_type_nobody", "aggregate_type", "$@2", "$@3", "$@4", "$@5",
  "type_parameters_opt", "aggregate_type_nobody", "aggregate_key",
  "aggregate_data", "aggregate_control", "field_declaration_list_opt",
  "field_declaration", "field_declaring_list_opt", "field_declarator",
  "field_abstract_list_opt", "field_abstract", "cfa_field_declaring_list",
  "cfa_field_abstract_list", "bit_subrange_size_opt", "bit_subrange_size",
  "enum_type", "$@6", "$@7", "enumerator_type", "hide_opt",
  "enum_type_nobody", "enumerator_list", "visible_hide_opt",
  "enumerator_value_opt", "parameter_list_ellipsis_opt", "parameter_list",
  "cfa_parameter_list_ellipsis_opt", "cfa_parameter_list",
  "cfa_abstract_parameter_list", "parameter_declaration",
  "abstract_parameter_declaration", "cfa_parameter_declaration",
  "cfa_abstract_parameter_declaration", "identifier_list",
  "type_no_function", "type", "initializer_opt", "initializer",
  "initializer_list_opt", "designation", "designator_list", "designator",
  "type_parameter_list", "type_initializer_opt", "type_parameter", "$@8",
  "$@9", "new_type_class", "type_class", "assertion_list_opt",
  "assertion_list", "assertion", "type_list", "type_declaring_list",
  "type_declarator", "type_declarator_name", "trait_specifier",
  "trait_declaration_list", "trait_declaration",
  "cfa_trait_declaring_list", "trait_declaring_list", "translation_unit",
  "external_definition_list", "external_definition_list_opt", "up", "down",
  "external_definition", "$@10", "$@11", "$@12", "$@13", "$@14",
  "external_function_definition", "with_clause_opt", "function_definition",
  "declarator", "subrange", "asm_name_opt", "attribute_list_opt",
  "attribute_list", "attribute", "attribute_name_list", "attribute_name",
  "attr_name", "paren_identifier", "variable_declarator", "variable_ptr",
  "variable_array", "variable_function", "function_declarator",
  "function_no_ptr", "function_ptr", "function_array",
  "KR_function_declarator", "KR_function_no_ptr", "KR_function_ptr",
  "KR_function_array", "paren_type", "variable_type_redeclarator",
  "variable_type_ptr", "variable_type_array", "variable_type_function",
  "function_type_redeclarator", "function_type_no_ptr",
  "function_type_ptr", "function_type_array",
  "identifier_parameter_declarator", "identifier_parameter_ptr",
  "identifier_parameter_array", "identifier_parameter_function",
  "type_parameter_redeclarator", "typedef_name", "type_parameter_ptr",
  "type_parameter_array", "type_parameter_function", "abstract_declarator",
  "abstract_ptr", "abstract_array", "abstract_function", "array_dimension",
  "array_type_list", "upupeq", "multi_array_dimension",
  "abstract_parameter_declarator_opt", "abstract_parameter_declarator",
  "abstract_parameter_ptr", "abstract_parameter_array",
  "abstract_parameter_function", "array_parameter_dimension",
  "array_parameter_1st_dimension", "variable_abstract_declarator",
  "variable_abstract_ptr", "variable_abstract_array",
  "variable_abstract_function",
  "cfa_identifier_parameter_declarator_tuple",
  "cfa_identifier_parameter_declarator_no_tuple",
  "cfa_identifier_parameter_ptr", "cfa_identifier_parameter_array",
  "cfa_array_parameter_1st_dimension", "cfa_abstract_declarator_tuple",
  "cfa_abstract_declarator_no_tuple", "cfa_abstract_ptr",
  "cfa_abstract_array", "cfa_abstract_tuple", "cfa_abstract_function",
  "comma_opt", "default_initializer_opt", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   125,    40,    64,    41,    46,    91,    93,
      44,    58,   123,    96,    94,    42,    38,    43,    45,    33,
     126,    92,    47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1824)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1132)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     127, 12231,   150,   224, 23799,   135, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,   136,   825,
     191, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824,   528,   393, -1824, -1824, -1824, -1824,
   -1824, -1824,  5432,  5432,   285, 12231,   343,   361, 28662, -1824,
     417, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824,   428,  2257, -1824,   284, 14757, -1824, -1824,  2271,
   -1824, -1824, -1824, -1824, 17325, -1824,   283,   431,   505,   255,
      83, -1824,  4957,   513,   516,   530,   454,  3928,   630,   938,
   12418, -1824, -1824,   650, 17159,  1890, -1824, -1824, -1824, -1824,
    3366,   750,  8184, 11223,   886,  3366,  1185,   609, -1824, -1824,
   -1824, -1824,    63, -1824, -1824, -1824, -1824,   621, -1824, -1824,
   -1824, -1824, -1824,   644,   634,    63, -1824,    63, 21671, -1824,
   -1824, -1824, 25248,  5432, -1824, -1824,  5432, -1824, 12231, -1824,
     626, 25412, -1824, -1824,  4780, 27364, -1824, -1824,  1490,  1490,
     648,  2859, -1824, -1824, -1824, -1824,   421, 19707,    63,  2919,
      63, -1824, -1824, -1824, -1824, -1824, -1824,   659, -1824,   640,
     683,  2188, -1824,   726, 31351, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, 22167,  3527,  5004,  2257,    44,   704,   713,   719,
     731,   740,   755, -1824, -1824, 31429,   699, 31507,   752,   760,
      63, 31351, 31585,   763, 28788, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, 31663, 31663, 21999, 13695, 23963,  4508,   764, -1824,
   -1824, -1824, -1824,   310, -1824,   444,   809, -1824,  2406,  5333,
   22671, 31351, -1824,   757,   709,  1066,   642,   517,  1151,   786,
     821,   781,   864,   -14, -1824,   850, -1824, -1824,  5127,  3660,
     882, 20399, 28310,  3366,  3366,   895,  3366,  1454,  3366,  1526,
     876, -1824, -1824,    63, -1824,  1146,  1161, -1824, -1824, -1824,
   -1824, 25576,  5432, -1824, -1824, 25740,  5353, -1824, -1824, 14934,
     881, -1824, 17657, -1824, -1824, -1824, -1824,   888, -1824, -1824,
   -1824,   884, -1824,  9990,  1037, 29318, -1824,   914,  5432,   634,
     921,   940, -1824,  2271,  4780,  2271, -1824, -1824, -1824,  4218,
    5034,   932,  1002,   416,  1002, -1824,    63,    63,    53, 21331,
     693,  1002, -1824,    63,    63,    53,    63, -1824,    63, -1824,
    5762, -1824, -1824,   951,   963,  1490, 28456, 19880, 17325, -1824,
    4957, -1824,  3366, -1824,   877,   609,   961,  1039, 21331,  5432,
    5432,   255, -1824, 12971, -1824,  1490,  1490,   986,  1039, 21331,
    5432, -1824, 28911, -1824, -1824, -1824,  1490, -1824, -1824, -1824,
   -1824,  1490, -1824,  1226,  4747,  5432, -1824, 23480,   985, -1824,
   -1824, -1824, 28164,   634, 21501,   990,  4780, 23334, 28456, 31741,
   -1824, 27678, -1824,  1002,    27, -1824, 31351, 27521,  4469,  5762,
   -1824,   793, -1824, -1824, -1824, -1824, 20572, 25412,  1002,  5432,
   -1824,  1015,  1010, -1824, -1824, -1824, -1824,  5432,  4024,   440,
     684, -1824,  5432,   640, -1824,  1178, -1824, 15111, 25904,  1077,
   20399, -1824,  1490,  1490,  1046, -1824,   888,  3580,   -25,   583,
   -1824,   473,   609,  1033,  1049, -1824,  2859,  1045,   640,  2859,
   -1824, -1824,  3527, -1824,   703, -1824,  1082,  1083, 29084, -1824,
   31351, -1824,   783,   876, -1824, 13872, 22839, -1824,   463, -1824,
   -1824,   784, -1824, -1824,   803,  5004,  1090,  1097,  1103,  1105,
    1118,  1123, -1824, -1824,   892,  1109, -1824,   811,  1109, 22335,
   -1824,  4508, 22503, -1824, 26068, 25412,  5139, -1824, 22335, -1824,
   31351, -1824, -1824, -1824, -1824, -1824, -1824, 22503, -1824, -1824,
   24592, 26068, 26068, 14049,  1841,  2259,   705,  2438, -1824,   835,
    1128,  1197,  1132, -1824,  1130, 23635,  1156,  1207, 15642, 23007,
    1172, 31819,  1175, -1824, 25248, -1824, 29396,  1179, -1824, 31351,
    2271, 31351,  2271, -1824, -1824,  2592, -1824, -1824,  5853,  3378,
   31351,  5853,  2271, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824, -1824,  1181, 31351, -1824, -1824, 14226, -1824,
   -1824, 26232, -1824,  1490,  1490, -1824, -1824,   888, -1824, -1824,
   31351, 31351, 31351, 31351, 31351, 31351, 31351, 31351, 31351, 31351,
   31351, 31351, 31351, 31351, 31351, 31351, 31351, 31351, 31351, 29474,
   -1824, -1824, 12787, 29552,  1339, 31351,  5487,   995,  1159, -1824,
      63,    63,  1159,  1008, -1824,    63,    63,  1191,  1159, -1824,
      63,    63, -1824,  1109, -1824, 29630, 20745, 25904, -1824,  5432,
   24446,  1490,  1490, -1824,  4716,  5139, -1824, 20572, -1824, 20572,
   -1824, 27207, -1824,  1159, -1824, 25576, -1824, -1824,    29, 24756,
   -1824, -1824, -1824, -1824, 23184,  4233, 29084,  9990,  1196,  1209,
   -1824, -1824,  1213, 29318,   308, -1824, -1824, -1824, 22839,  1222,
   -1824,   726, -1824, -1824, -1824,  1206,  4218,   867,  1232,  1235,
    1239,   913,  1243,  1245,  1247,  1252,  1260,  1266,  5034, -1824,
   -1824, -1824,    63,  1251, 27835, -1824, -1824,  1191,   255, -1824,
   -1824,   634,  1039, 24136, -1824, -1824,   255, -1824, -1824,   634,
   -1824, -1824,  5762, -1824, 22839, 22839, -1824,  1490,  4780, 28602,
   15288,  3170, -1824, -1824, -1824, -1824, -1824, -1824,   634,  1039,
      27,  1286, -1824, -1824,  3366,  1294,  1039, 21331, -1824,   634,
    1039, -1824, 28969, -1824,  1490,  1490, -1824, -1824,  1312,   406,
    1314,   609,  1319, -1824, -1824, -1824, 24446,  1278,  1292, -1824,
   -1824,   848, -1824,  1431, -1824,  1323, -1824, -1824, -1824, 26405,
    1347,  1363, -1824, -1824, -1824, -1824, -1824,  4469,   920,  5762,
   24136,  1002, 12231, -1824,  5432,  1367, 16995,  1374, -1824, -1824,
   -1824, -1824, -1824,  2859, -1824, -1824,  1470, 24920, 19188,  1543,
     897, -1824, -1824,    63,  1394,    63,  1049,   686,  1398,   862,
   25412,   878,   885, -1824,  3527,  5853,  1402,  1409, -1824,   726,
   19361,  2329, -1824, -1824,    63,    63, -1824, -1824, 22839, -1824,
   -1824,  1052,  1109, -1824,   941,  1109, 24136, -1824, -1824,  1191,
   24136, -1824,  1191,  1427,  1430,  1434,  1447, 15465,  1444,  1455,
   -1824,  1456,  1459,  1450,  1462, 31351,  1464,  1465,  1466, 26551,
    1259, 31351, -1824, -1824,  2446, -1824, -1824, -1824, 31351, -1824,
    1471,  1472, 29162, 29708,  1474, 19534, -1824, 25576, -1824, -1824,
   29240, 14580,  1483, 22671,  1484,   876,  1485, 15819, -1824, -1824,
   -1824, -1824,  5853,  1487, -1824,  1488, -1824, -1824,  3033, -1824,
    2271,  1473,  1491, -1824, -1824, -1824,  3033, -1824, -1824,  1272,
    1437, -1824,  9990, -1824, -1824, -1824,   757,   757,   757,   709,
     709,  1066,  1066,   642,   642,   642,   642,   517,   517,  1151,
     786,   821,   781,   864, 31351,  1283, 19534, 12787,  1475,   962,
    1494,  1497,  1498,  1499,  1500,  1502,  1506, -1824,  1290,  1955,
   -1824,  5487, -1824, -1824, -1824, 24136, -1824, -1824, -1824, -1824,
   -1824, -1824, 24136, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
    1191, -1824,  1514, -1824, -1824, -1824,  1159,  1490,  3033, -1824,
   -1824,  1291, -1824, -1824, -1824, -1824, -1824, -1824, 19534, -1824,
    5432,  3033, -1824,  1519,   270,  1515,  1213, -1824,  9990,  1528,
   -1824,  3057, 31351, -1824, -1824,   950, -1824,  1529, 19534, 31351,
     974,  1533,  1534,  1537,   977,  1542,  1545,  1549,  1554,  1560,
    1561,  1111,  1109, -1824, -1824,  1144,  1109, -1824, -1824,  1188,
    1109, -1824, -1824, -1824,  4780,  1672,  1109,   697, -1824,   876,
    1303, -1824, -1824,   634,  1562, -1824, -1824, -1824,   981,  1563,
     983,  1567, -1824,  1172,  1541, -1824,   634, -1824, 11285, -1824,
     634,  1039,  1541, -1824,   634,  1566,  1576,  1579, -1824, -1824,
   24300, -1824,  2271,  5432, 11670,  1637, -1824, -1824, -1824, 19534,
     992, -1824,  1541,  1568, -1824, -1824, -1824, -1824,  4780, 26715,
   16339, -1824,   173,   316, 22839,  1547, -1824,  1547, -1824, -1824,
      63,   897, -1824,   686,  1049,  1581,   421, -1824, -1824,  1569,
    5432,   686, -1824, -1824,  1582,  1590, -1824,  1591, -1824,  1595,
    1598,  1600,  1601,  1604,  2329, -1824, -1824, -1824, -1824, -1824,
   24136, -1824, -1824,  1191, 24136, -1824,  1191,  1609,  1611,   539,
   -1824, 24446,   539,  2271, -1824,   539, -1824, 25084,   539, -1824,
   31351, 31351, 31351, 20745, -1824, -1824, -1824, -1824, 31351, 31351,
    1608,  9990, -1824, -1824, -1824,  1612, -1824, -1824,  1617,  1619,
    1620, -1824, -1824, -1824,  1612, -1824, -1824, -1824,  1622, 19534,
   19534,  1640, -1824, -1824, -1824,  3110, -1824, -1824,  1305, -1824,
     110,  1616, -1824,  5853,  1308, -1824, 29708, -1824,  1213, -1824,
   31351, -1824,  1634, -1824,  1358,  1109, -1824,  1380,  1538,  1109,
   -1824,  1490,  9851,  2093, -1824,    63,    63, -1824, -1824, -1824,
    1641,  1642, -1824, -1824,  1311, -1824, 20572, -1824,   255,  1313,
   31351, -1824, 31351, -1824,  1648, -1824, 29318, -1824,    63, 19534,
      63, -1824, -1824,  1548,  1109, -1824,  1557,  1109, -1824, -1824,
    1652,  1109, 24136, -1824, -1824,  1191, 24136, -1824, -1824,  1191,
   24136, -1824, -1824,  1191,  1002, -1824,  1191, -1824, 31351, -1824,
   31351, -1824, 27996, -1824, -1824, -1824, -1824, -1824, -1824,  1650,
   -1824, -1824, -1824, 16503,  1541, -1824,   634, -1824, -1824, -1824,
   -1824, -1824, 18183, -1824, -1824, -1824, -1824, -1824,   354,    69,
      34, 13518,  1651,  1653, 21144,  1654,  1657,  2486,  3215,  3675,
   29786,  1658, -1824, -1824,  1659,  1660, 21144,  1661, -1824, -1824,
     634, 31351, 31351,  1800,  1656,   575, -1824, 21831, 14403,  1663,
    1667,  1655, -1824, -1824, -1824, 11477, -1824, -1824, -1824, -1824,
   -1824,  2109, -1824, -1824, -1824,  1390,   237, -1824,   266, -1824,
     237, -1824, -1824, -1824, -1824, -1824,  2271, -1824, -1824, 12603,
   17491, -1824,  5432, -1824, -1824, -1824,  5432, -1824, -1824, -1824,
    5432, -1824,  4780, -1824,   998, 25412,   640,   640,  1049,  1569,
    1670,   686,   609,   353,  1683,  1662,  1569, 16667, -1824, -1824,
   -1824, -1824,  1665,  1109, -1824, -1824,  1664,  1682, -1824, -1824,
     700,  1685,  1690,  1003, -1824,  1692, -1824, -1824, -1824, -1824,
   -1824,  9990,  1213, 29864,  1695, -1824, 20053, 20226,  1697, -1824,
   -1824, -1824, -1824,  1738,  3033, -1824,  1738,  1738, -1824,  3033,
    3771,  5109, 31351, -1824, -1824,  1326,  1708, -1824,  1710,    63,
   24136, -1824, -1824,  1191, 24136, -1824, -1824, 24136, -1824, -1824,
    1191, 31351, 31351,  1705,  1709, -1824,  1711, -1824, -1824, -1824,
   -1824, -1824, -1824,  1715, -1824, -1824,  1713, -1824, -1824, -1824,
   -1824, -1824, -1824,  1717, 24136, -1824, -1824,  1191, 24136, -1824,
   -1824,  1191, 24136, -1824, -1824,  1191,  1718,  1726,  1737,   255,
    1329, -1824,   245, -1824,   876,  1743, -1824, -1824, -1824,  1745,
   18350, 18517, 18684, 26879, 28456, 26068, 26068,  1747,  1723,   383,
     434,  2826, 19015, -1824,   479,  5432,  5432, -1824,  5853,    67,
     181, -1824, -1824, -1824, -1824, 13518, 31351,  1750,  1829, 13340,
   11857, -1824,  1730, -1824,  1731, 31351,  1734,  9990,  1735, 31351,
   22839, 31351, -1824, 12044,  1241, -1824,  1741,    25, -1824,    21,
    1827,   164,    63, -1824,  1764, -1824,  1742, -1824,  1744,  1768,
    1772, 21144, 21144, -1824, -1824,  1840, -1824, -1824,   102,   102,
     386, 13155,   488,  1773,  1777,   440, -1824, -1824, -1824, -1824,
   -1824, -1824,  1769,  1781,   686,  1569,   421,  5432, -1824, 29942,
   -1824,  1782, -1824, 16831, 24136, -1824, -1824,  1191, -1824, -1824,
    1783, -1824, -1824, 31351, -1824, 25084, 31351,  1213,  1785, -1824,
   -1824, -1824, -1824,  1788, -1824, -1824,  1786,  1787, -1824,  1331,
   -1824,  3033, -1824,  3033, -1824, -1824, -1824, 29864, -1824, -1824,
    1789,  1797,  1799, -1824, -1824,  1798, -1824,  1803, -1824, -1824,
    1810,    63,  1811,  1813,  1822, -1824, -1824, -1824, -1824, -1824,
   31351, -1824,  1815, -1824,  1747,  1747,  1747,  4090,   874,  1762,
     509, -1824,  4090,   521, 22839, -1824, -1824, -1824, -1824,  4632,
   31351,  6161,   519, -1824, -1824,    54,  1795,  1795,  1795,  5432,
   -1824, -1824, -1824,  1823, -1824, -1824, -1824, -1824,  1667,  1825,
   31351,   431,  1824,   454, 18858, 26879,  1005,  1834, 21144,  1837,
   -1824, -1824, -1824, -1824,  1026, 21144, 31351,   910,   495, -1824,
   31351, 29001, -1824, -1824,   527, -1824,  1213, -1824,  1041,  1047,
    1056,   547, -1824, -1824, -1824, -1824,   634,  1241,  1842, -1824,
   -1824, 31351, -1824,  1843,   726, -1824, 10726, -1824, -1824, -1824,
   31351, 31351, -1824, -1824,    84,   102, -1824,   453, -1824, -1824,
   -1824,    63, -1824,  1547,   686, -1824,  1569,  1846,   609,  1662,
    9990, -1824, -1824, -1824,  1839, -1824, -1824, -1824, -1824,  1850,
   -1824,    63,    63, -1824,  1345,  1349, -1824, -1824, -1824,  1847,
    1848, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824, -1824,
   -1824, -1824, -1824,   536,   874,  2742,   563, -1824, -1824, -1824,
   -1824,    63,    63, -1824, -1824, -1824,   568, -1824,  1081,  4632,
     593, -1824,  6161, -1824,    63, -1824, -1824, -1824, -1824, -1824,
   -1824, 21144, 21144,  1667, 20918,   147, 30020,  1933, 21144, -1824,
   -1824, -1824, -1824, -1824, 31351, -1824, 30098,  1937,  1831, 11008,
   30176, 21144, 12044,  1667,  1034,  1932,  1833, 31351, -1824,  1865,
     165, 21144, -1824, 21144, -1824,  1861, -1824, 27043,  1844,   726,
     604, -1824, -1824,  1869,  1350,  1088, 21144,  1867, 21144, 21144,
   21144, -1824,   640,  1569,  1875, -1824, -1824,  1213, -1824, -1824,
   -1824, -1824, -1824, -1824, -1824, -1824, -1824,  1873,  1876,  1877,
    2742, -1824,    63, -1824, -1824, -1824, -1824, -1824,  1878,  4090,
   -1824,  1965,  5212,    65, 15999, -1824, 21017, -1824,    38,  1091,
   21144,  1966,   601,  1870,    94, 21144, 31351,  1034,  1932,  1864,
   30259,   967,  1490,  1871,   470,  1978, -1824, 30337, 30415, 31351,
    1667,  1872, 16175, -1824, -1824, -1824, 27043,  1874,  5947, 26232,
    2271, -1824,  1891,  1892,    12, -1824, 31351,  5853, -1824, -1824,
   31351,   237, -1824, -1824, -1824,  1901, -1824,  1903,  1668,  1109,
   -1824, -1824,   874, -1824, 21144, -1824,    68, -1824,    89, -1824,
   -1824, -1824,  1904, 17842, -1824, -1824, 21144, -1824,    39, -1824,
   21144, 31351,  1915, 30493, -1824, -1824, 30571, 30649, 31351,  5139,
    1667, -1824,   876, 30727, 30805, 21144,  1902,   597,  1905,   624,
   -1824, -1824,  1925, 17842,  1874, 31351,  1927,  4378,  5623, -1824,
   -1824, -1824,  1924, -1824,  1984,  1936,   738,  1935, -1824, -1824,
    1940,  1096,   349, -1824, -1824, 24136, -1824, -1824,  1191, -1824,
   -1824, 31351, -1824, 31351, -1824, -1824,  1443, 18016, -1824, -1824,
   21144, -1824, -1824,  1667, -1824, -1824,  1667,  1923,   668,  1928,
     682, -1824, -1824,   609, -1824,  1667, -1824,  1667, -1824,  1946,
   30883, 30961, 31039, -1824,  1443,  1947, -1824,   634,  5623,    12,
    1956, 31351,  1926,    12,    12, -1824, -1824, 21144,  2042,  1960,
   -1824, -1824, 21017, -1824,  1443, -1824, -1824,  1963, 31117, 31195,
   31273, -1824, -1824,  1667, -1824,  1667, -1824,  1667, -1824,   634,
   -1824,  1964,   726,  1971, -1824,   749, -1824, -1824, 21144, -1824,
   -1824,  9587,  1968, 21017, -1824, -1824,  1667, -1824,  1667, -1824,
    1667,  1975, -1824,   726,  1976, -1824,  1950,   726, -1824, -1824,
   -1824, -1824, 10511, -1824, -1824,  1387, 31351, -1824,  1102,   726,
    2271,  1977,  1952, -1824, -1824,  1115, -1824, -1824,  1957,  2271,
   -1824, -1824
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   502,     0,     2,   502,   519,   520,   521,   522,   523,
     524,   525,   526,   527,   508,   510,   509,   511,     0,     0,
       0,   529,   531,   558,   532,   559,   535,   536,   556,   557,
     530,   554,   555,   533,   534,   537,   538,   539,   540,   541,
     542,   543,   544,   545,   546,   547,   548,   549,   550,   551,
     552,   553,   560,   561,   867,   563,   636,   637,   640,   642,
     638,   644,     0,     0,     0,   502,     0,     0,    17,   607,
     613,     9,    10,    11,    12,    13,    14,    15,    16,   823,
     874,   104,     0,     0,    20,     0,   502,   102,   103,     0,
     844,    18,    19,   883,   502,   824,     0,     0,   441,   745,
     443,   454,   865,   442,   476,   477,     0,     0,     0,     0,
     590,   504,   506,   512,   502,   514,   517,   575,   528,   562,
     486,   568,   573,   488,   585,   487,   600,   604,   610,   589,
     616,   628,   867,   633,   634,   617,   686,   444,   445,     3,
     831,   845,   507,     0,     0,   867,   906,   867,   502,   923,
     924,   925,   502,     0,  1110,  1111,     0,     1,   502,    17,
       0,   502,   465,   466,     0,   590,   512,   496,   497,   498,
     834,     0,   639,   641,   643,   645,     0,   502,   867,   689,
     868,   869,   635,   564,    22,    23,    21,   799,   794,   784,
       0,   877,   832,     0,     0,   519,   825,   829,   830,   826,
     827,   828,   502,   877,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   608,   611,     0,     0,     0,     0,     0,
     867,     0,     0,     0,    27,    29,     4,     8,    25,     5,
       6,     7,     0,     0,   502,   502,   502,     0,   102,   105,
     106,   107,   108,    85,    28,    86,    24,    46,    84,   109,
     502,     0,   124,   126,   130,   133,   136,   141,   144,   146,
     148,   150,   152,   154,   165,     0,    30,   732,     0,  1132,
       0,   503,   502,   514,   493,   568,   494,   593,   495,   600,
     604,   597,   618,   867,   619,     0,     0,   728,   733,   718,
     722,   502,   734,  1085,  1086,   502,   735,   737,   884,   502,
       0,  1112,   590,   913,   931,  1116,  1109,  1107,  1114,   438,
     437,     0,   173,   751,   172,     0,   446,     0,     0,     0,
       0,     0,   452,     0,     0,     0,   436,  1000,  1001,     0,
       0,   475,   865,   867,   865,   887,   867,   867,   485,   502,
     867,   865,   944,   867,   867,   484,   867,   963,   867,   941,
       0,   583,   584,     0,     0,   502,   502,   502,   502,   455,
     865,   505,   515,   576,     0,   605,     0,   848,   502,     0,
       0,   745,   456,   590,   569,   586,   601,     0,   848,   502,
       0,   518,   570,   577,   578,   489,   587,   491,   492,   490,
     592,   602,   606,     0,   620,     0,   817,   502,     2,   846,
     905,   907,   502,     0,   502,     0,     0,   590,   502,     0,
    1120,   590,  1123,   865,   865,     3,     0,   590,     0,     0,
     468,   867,   860,   862,   861,   863,   502,   502,   865,     0,
     821,     0,     0,   780,   782,   781,   783,     0,     0,   776,
       0,   765,     0,   774,   786,     0,   687,   502,   502,  1132,
     503,   568,   593,   600,     0,   734,   735,   689,   607,   613,
     690,   691,   692,     0,   689,   870,     0,   797,   785,     0,
     882,   881,   877,   880,     0,   875,   878,     0,     0,   109,
       0,   157,     0,     0,   614,   502,   502,   791,   741,   743,
     790,     0,   740,   744,     0,     0,     0,     0,     0,     0,
       0,     0,   885,   911,   867,   921,   929,   933,   939,   502,
      92,     0,   502,   100,   502,   502,     0,    87,   502,    94,
       0,    36,    40,    41,    37,    38,    39,   502,    90,    91,
     502,   502,   502,   502,   105,   106,     0,     0,   194,     0,
       0,   634,     0,  1107,  1130,   502,     0,     0,   503,   502,
     607,     0,     0,  1118,   502,  1121,     0,     0,  1038,     0,
       0,     0,     0,    26,    59,     0,    65,    66,   158,     0,
       0,   158,     0,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   172,     0,   170,   171,   502,    88,
    1087,   503,   499,   500,   501,  1091,  1082,  1083,  1089,    89,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1037,     2,   502,     2,   103,     0,  1047,   867,  1132,   982,
     867,   867,  1132,   867,   997,   867,   867,  1061,  1132,  1043,
     867,   867,  1052,  1059,   726,     0,   502,   502,   598,   736,
     503,   594,   595,   599,     0,     0,   463,   502,  1124,   502,
    1097,   503,  1102,  1132,  1094,   502,  1099,     2,  1132,   502,
     914,   932,  1108,     2,    27,     0,     0,   751,    28,     0,
     749,   752,  1130,     0,     0,   758,   747,   746,   502,     0,
     850,     0,     2,   467,   469,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   890,
     947,   970,   867,   481,     0,   886,   894,  1028,   745,   888,
     889,     0,   848,   502,   943,   951,   745,   945,   946,     0,
     962,   964,     0,   471,   502,   502,   574,   503,     0,   590,
     502,     0,  1113,  1117,  1115,   453,   591,   821,     0,   848,
     865,     0,   447,   457,   516,     0,   848,   502,   821,     0,
     848,   795,   571,   572,   588,   603,   609,   612,   607,   613,
     631,   632,     0,   796,   704,   738,   503,     0,   705,   707,
     708,     0,   216,   430,   847,     0,   428,   485,   484,   590,
     102,     0,   449,     2,   450,   818,   473,     0,     0,     0,
     502,   865,   502,   821,     0,     0,     0,     0,   779,   778,
     777,   771,   513,     0,   769,   787,   566,   502,   502,   103,
    1047,   736,   688,   867,     0,   867,   689,   689,     0,     0,
     502,     0,     0,   872,   877,   158,     0,     0,   440,     0,
     502,  1012,   742,  1009,   867,   867,  1017,   615,   502,   873,
     912,   867,   922,   930,   934,   940,   502,   915,   917,   919,
     502,   935,   937,     0,     0,     0,     0,   502,     0,     0,
     691,     0,     0,     0,     0,     0,     0,     0,     0,   502,
       0,     0,   123,   122,     0,   119,   118,    31,     0,    32,
       0,     0,     0,  1131,     0,   502,  1093,   502,  1098,   185,
       0,   502,   102,   502,     0,   605,     0,   503,     2,     2,
    1119,  1122,   158,     0,    55,     0,    56,    63,     0,    62,
     162,     0,   159,   160,   164,    58,     0,    57,    61,     0,
       0,    54,   751,   166,  1084,   125,   127,   128,   129,   131,
     132,   134,   135,   139,   140,   137,   138,   142,   143,   145,
     147,   149,   151,   153,     0,     0,   502,   502,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1062,     0,   867,
    1133,  1048,   985,  1002,  1049,   502,   980,   988,   724,   983,
     984,   725,   502,   995,  1005,   998,   999,   727,  1045,  1046,
    1060,  1125,     0,  1088,  1092,  1090,  1132,   596,     0,    33,
     631,     0,   720,   719,   723,   729,  1096,  1101,   502,   730,
       0,     0,   760,   157,     0,     0,  1130,   757,  1131,     0,
     753,     0,     0,   756,   759,     0,     2,     0,   502,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   867,   899,   903,   942,   867,   956,   960,   968,   867,
     978,   891,   948,   971,     0,     0,  1024,     0,  1029,  1030,
       0,   479,   851,     0,     0,   480,   852,   472,     0,     0,
       0,     0,   470,     0,     2,   853,     0,   451,     0,   821,
       0,   848,     2,   854,     0,     0,     0,     0,   646,   908,
     502,   926,     0,     0,   502,   431,   429,  1040,  1039,   502,
       0,   822,     2,     0,   773,   814,   809,   810,     0,   503,
       0,   805,     0,     0,   502,   767,   766,   767,   567,   565,
     867,  1048,   683,   689,   689,     0,     0,   699,   698,  1130,
       0,   689,   800,   798,     0,     0,   876,     0,   833,     0,
       0,     0,     0,     0,  1013,  1014,  1010,  1011,   793,   792,
     502,   916,   918,   920,   502,   936,   938,     0,     0,    93,
      96,   502,   101,     0,    99,    95,    97,   502,     0,   113,
       0,     0,     0,   502,   117,   121,   120,   195,     0,     0,
       0,   751,   110,   191,   190,  1130,   187,   712,     0,   713,
     714,  1095,  1100,   186,  1130,  1103,  1104,  1105,     0,   502,
     502,     0,    49,    50,    82,     0,    82,    82,     0,    70,
      72,     0,    52,     0,     0,    48,     0,    51,  1130,   156,
       0,     3,     0,  1056,   867,   991,   994,   867,   867,  1055,
    1058,   502,     3,     0,  1044,   867,   867,   986,  1003,  1050,
       0,     0,  1126,   731,     0,   464,   502,     3,   745,     0,
       0,   761,     0,   762,     0,   754,     0,   748,   867,   502,
     867,     3,   474,   867,   900,   904,   867,   957,   961,   969,
     867,   979,   502,   892,   895,   897,   502,   949,   952,   954,
     502,   972,   974,   976,   865,   482,  1025,  1036,     0,  1035,
       0,  1027,     0,   856,   965,   580,   579,   582,   581,     2,
     822,   857,   802,     0,     2,   855,     0,   822,   858,   646,
     646,   646,   502,   706,   709,   710,   739,   434,     0,     0,
       0,   502,     0,     0,   355,     0,     0,     0,     0,     0,
     196,     0,   350,   351,     0,     0,   355,     0,   403,   402,
       0,   168,   168,   409,   607,   613,   213,   502,   502,     0,
     197,     0,   224,   198,   199,   502,   218,   200,   201,   202,
     203,     0,   204,   205,   356,     0,   370,   206,   376,   378,
     381,   207,   208,   209,   210,   211,     0,   212,   220,   590,
     502,   222,     0,     3,   835,   822,     0,   812,   789,   806,
       0,   807,     0,   808,     0,   502,   784,   784,   689,  1130,
       0,   689,   695,   689,     0,   700,  1130,     0,   871,   879,
     439,  1021,   867,  1020,  1023,  1015,     0,     0,   909,   927,
    1041,     0,     0,     0,    42,     0,   114,   116,   115,   112,
     111,   751,  1130,  1131,     0,  1127,   502,   502,     0,  1106,
       3,     3,    69,    79,     0,    73,    80,    81,    64,     0,
       0,     0,     0,   161,    60,     0,     0,   155,     0,   867,
     502,   987,   989,   990,   502,  1004,  1006,   502,  1051,  1053,
    1054,     0,     0,   102,     0,     3,     0,   981,   996,   992,
    1007,    34,   721,     0,   448,   764,     0,   864,   750,   755,
     849,     3,   866,     0,   502,   893,   896,   898,   502,   950,
     953,   955,   502,   973,   975,   977,     0,     0,     0,   745,
       0,  1031,     0,  1032,  1033,     0,   804,   822,   859,     0,
     502,   502,   502,   502,   502,   502,   502,   629,     0,     0,
       0,   660,   590,   647,     0,     0,     0,   432,   158,     0,
       0,   341,   342,   221,   223,   502,     0,     0,     0,   502,
     502,   337,     0,   335,     0,     0,     0,   751,     0,     0,
     502,     0,   382,   502,     0,   169,     0,     0,   410,     0,
       0,     0,   867,   228,     0,   219,     0,   332,     0,     0,
       0,   355,   355,   361,   360,   355,   372,   371,   355,   355,
       0,   590,     0,     0,     0,   776,   811,   813,   788,   768,
     772,   770,     0,     0,   689,  1130,     0,     0,   678,     0,
     694,     0,   801,     0,   502,  1016,  1018,  1019,   910,   928,
       0,  1042,    98,     0,    35,   502,     0,  1130,     0,   193,
     192,   189,   716,   715,   717,   188,     0,     0,    83,     0,
      71,     0,    77,     0,    75,   163,    47,     0,   167,  1129,
       0,     0,     0,     3,     3,     0,  1064,     0,  1128,   763,
       0,   867,     0,     0,     0,   901,   958,   966,   483,  1026,
       0,   839,     0,   841,   629,   629,   629,   660,   667,   634,
       0,   673,   660,     0,   502,   621,   659,   658,   654,     0,
       0,     0,     0,   661,   663,   867,   675,   675,   675,     0,
     655,   671,   435,     0,   345,   346,   343,   344,   237,     0,
       0,   239,   443,   238,   590,   502,     0,     0,   355,     0,
     320,   322,   321,   323,     0,   355,   196,   277,     0,   270,
       0,   196,   338,   336,     0,   330,  1130,   339,     0,     0,
       0,     0,   391,   392,   393,   394,     0,   384,     0,   385,
     347,     0,   348,     0,     0,   375,     0,   217,   334,   333,
       0,     0,   364,   374,     0,   355,   377,     0,   379,   401,
     433,   867,   837,   767,   689,   679,  1130,     0,   697,   700,
     751,   701,   682,   803,     0,    53,    45,    43,    44,     0,
      67,   867,   867,    74,     0,     0,   993,  1008,  1057,     0,
       0,  1063,  1065,   458,   462,   902,   959,   967,  1034,   843,
     625,   627,   623,     0,     0,  1071,     0,   668,  1076,   670,
    1068,   867,   867,   653,   674,   657,     0,   656,     0,     0,
       0,   677,     0,   649,   867,   648,   664,   676,   665,   666,
     672,   355,   355,   240,   590,     0,     0,   258,   355,   325,
     328,   326,   329,   324,     0,   327,     0,   266,     0,   196,
       0,   355,   502,   278,     0,   303,     0,     0,   331,     0,
       0,   355,   354,   355,   395,     0,   386,   502,     0,     0,
       0,   215,   214,   357,     0,     0,   355,     0,   355,   355,
     355,   461,   784,  1130,     0,   681,   696,  1130,  1022,    68,
     460,   459,    78,    76,  1066,  1067,   651,     0,     0,     0,
    1072,  1073,   867,   652,  1069,  1070,   650,   630,     0,     0,
     353,   229,     0,     0,     0,   251,   355,   231,     0,     0,
     355,   260,   275,   286,   280,   355,   196,     0,   290,     0,
       0,     0,   315,   281,   279,   268,   271,     0,     0,   196,
     304,     0,     0,   234,   352,   383,   502,   389,   396,   503,
     400,   349,     0,     0,   411,   362,     0,   158,   373,   366,
       0,   367,   365,   380,   775,     0,   685,     0,   867,  1079,
    1081,  1074,     0,   662,   355,   246,   241,   244,     0,   243,
     250,   249,     0,   502,   253,   252,   355,   262,     0,   259,
     355,     0,     0,     0,   267,   272,     0,     0,   196,     0,
     291,   316,   317,     0,     0,   355,     0,   306,   307,   305,
     274,   340,     0,   502,   389,     0,     0,     0,  1071,   397,
     398,   399,     0,   404,     0,     0,     0,   412,   413,   358,
       0,     0,     0,   684,   702,   502,  1075,  1077,  1078,   669,
     230,     0,   248,     0,   247,   233,   254,   502,   424,   263,
     355,   264,   261,   276,   289,   287,   283,   295,   293,   294,
     292,   273,   318,   319,   288,   284,   285,   282,   269,     0,
       0,     0,     0,   236,   254,     0,   390,     0,  1072,   411,
       0,     0,     0,   411,     0,   363,   359,   355,     0,     0,
     242,   245,   355,     3,   255,   425,   265,     0,     0,     0,
       0,   314,   312,   309,   313,   310,   311,   308,     3,     0,
     387,     0,     0,     0,   405,     0,   414,   368,   355,  1080,
     225,     0,     0,   355,   302,   300,   297,   301,   298,   299,
     296,     0,   388,   417,     0,   415,     0,   417,   369,   227,
     226,   232,     0,   235,   418,     0,     0,   406,     0,     0,
       0,     0,     0,   419,   420,     0,   416,   407,     0,     0,
     408,   421
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1824,    50,  -107, -1824,    -1,   605,   917,  7634,  -172, -1824,
    -145, -1824,   520, -1824,  -887,  -860, -1824,   358,  8333,  2000,
   -1824,  1149, -1824,  1613,   708,   955,   978,   379,   976,  1530,
    1535,  1532,  1539,  1536, -1824,  -186,  -144,  -564, -1824,   942,
    9328,   815, -1824,  1911, -1824, -1824,  -831,  3434, -1208,  2549,
   -1824,  2063, -1824,   808,    33, -1824, -1824,   618,   118, -1824,
   -1808, -1554,   302,    92, -1824, -1824,   610,   317, -1824, -1633,
   -1824, -1623, -1824, -1824, -1824, -1824,   138, -1253, -1824, -1824,
   -1341,   414, -1824, -1824, -1824, -1824, -1824,    13, -1299, -1824,
   -1824, -1824, -1824, -1824,   158,   436,   437,   229, -1824, -1824,
   -1824, -1824,  -806, -1824,    96,    46, -1824,   163, -1824,  -260,
   -1824, -1824, -1824,   813,  -815,  -924,  -252, -1824,    45,     3,
     419,  7442,  -920,  -690, -1824,  -139, -1824, -1824,    19, -1824,
     -72,   124,   160,  -345,  4555,  2474,  -507,     0,   256,   196,
    1114,  2980, -1824, -1824,  2136, -1824,   341,  5285, -1824,  2074,
   -1824,   130, -1824, -1824,  2869,   625,  6023,  3569,   -43,  1835,
    -157, -1824, -1824, -1824, -1824, -1824,  -356,  6801,  6258, -1824,
    -229,   106, -1824, -1048,   279, -1824,   218,   692, -1824,   -89,
    -276, -1824, -1824, -1824, -1824,   -99,  6983, -1090,   805,   441,
    1812, -1824,  -435,  -414,   294,  3130,  1862,  -602,  -151,   845,
    -228,  -497,  -343,  -296,  -634,  1205, -1824,  1556,  -153, -1104,
    1428, -1824, -1824,   649, -1824, -1361,  -187,  -299,  -672, -1824,
     233, -1824, -1824, -1031, -1051, -1824, -1824, -1824,  2245,  -966,
    -594, -1088,   -51, -1824, -1824, -1824, -1824, -1824, -1824,  -173,
    -923,  -280, -1823,   -87,  8576,   -71,  7544,  -128,  1413, -1824,
    2232,   -67,  -318,  -277,  -275,    16,   -54,   -37,    -9,   363,
     -60,   -59,   -49,  -258,    41,  -256,  -251,  -243,    81,  -242,
    -234,  -232,  -589,  -571,  -555,  -550,  -581,  -152,  -533, -1824,
   -1824,  -783,  1411,  1412,  1414,  2219, -1824,   743,  8224, -1824,
    -604,  -584,  -545,  -531,  -446, -1824, -1229, -1738, -1731, -1718,
    -562,   589,  -235, -1824, -1824,   717,   330,  -121, -1824,  9847,
    2342,  -619,  -417
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   311,   243,   244,   186,    93,  1349,   245,   246,
     247,  1423,  1424,   248,  1208,  1209,  1210,  1443,   249,   480,
     251,   252,   536,   537,   253,   254,   255,   256,   257,   258,
     259,   260,   261,   262,   263,   264,  1014,   921,   922,   923,
     538,  1566,   585,   315,   587,   266,  1185,  1350,  1351,  1352,
    1353,  1354,  1355,  1356,  2131,  1357,  1358,  1709,  1987,  1988,
    1925,  1926,  1927,  2103,  2104,  1359,  1728,  1729,  2011,  1730,
    1855,  1856,  1360,  1361,  1362,  1363,  1364,  1365,  1884,  1888,
    1589,  1581,  1366,  1367,  1588,  1582,  1368,  1369,  1370,  1371,
    1372,  1373,  1374,  1747,  2026,  1748,  1749,  1957,  1375,  1376,
    1377,  1569,  2036,  2037,  2038,  2155,  2165,  2056,  2057,   403,
     404,  1094,  1095,  1318,    95,    96,    97,    98,    99,  1712,
     267,   300,   103,   104,   105,   106,   331,   332,   406,   385,
     269,   488,   270,   109,   417,   111,   112,   166,   272,   273,
     116,   117,   118,   182,   119,  1119,   274,   167,   122,   355,
     123,   168,   364,   276,   452,   278,   169,   483,   128,   129,
     281,   130,   772,  1087,  1085,  1086,  1685,   282,   283,   133,
     134,  1312,  1533,  1692,  1693,  1816,  1817,  1534,  1680,  1836,
    1694,   135,   826,  1398,   178,  1128,   284,  1129,  1130,  1610,
     958,   778,  1188,   285,   286,   779,   288,   289,   290,   781,
     489,   490,   316,   681,   682,   683,   684,   685,   440,  1396,
     441,  1117,  1115,   811,   442,   467,   443,   444,   491,   137,
     188,   189,   138,  1110,  1111,  1112,  1113,     2,  1299,  1300,
     802,  1384,   139,   430,   431,   366,   377,   755,   140,   319,
     141,   420,  1015,   745,   715,   180,   142,   474,   475,   476,
     143,   422,   335,   336,   337,   423,   145,   146,   147,   148,
     149,   150,   151,   340,   424,   342,   343,   344,   425,   346,
     347,   348,   628,   629,   630,   631,   632,   349,   634,   635,
     636,   842,   843,   844,   845,   716,  1060,  1290,   152,  1620,
     638,   639,   640,   641,   642,   643,  1819,  1820,  1821,  1822,
     595,   292,   293,   294,   295,   492,   306,   154,   155,   156,
     297,   894,   644
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      91,   113,   468,    91,   100,   541,   542,   930,   481,   405,
     736,   698,   205,  1397,   192,   322,  1025,   144,   869,   687,
     107,   477,   974,   210,   211,  1585,   454,   464,   752,   207,
     886,   412,   396,  1399,   212,  1600,  1601,   972,   964,  1214,
     334,  1406,   214,  1016,   694,   973,   208,  1303,  1571,   162,
     482,   960,   699,   158,   700,  1002,   662,  1003,  1145,  1389,
     666,    91,    91,  1019,    91,   113,  1990,   961,   100,  1194,
     733,   701,   962,   702,   209,   494,  1907,   965,   703,   722,
     463,   144,    91,  1908,   107,   994,   704,   705,    91,   963,
    2052,   966,   557,    91,  1570,   706,  1909,   707,  1867,  1989,
     698,    91,  1539,  1540,  1860,   449,    91,   415,   749,    91,
     303,  1244,  1753,    91,   648,   555,  1307,   633,  1995,   760,
    1996,  2060,  1558,   338,  1249,   108,   367,  -815,   164,   618,
     378,   124,  1751,    64,   172,   320,  1385,   173,   174,   228,
     175,   699,    64,   700,   815,   499,   500, -1000,   341,   317,
     157,   496,    91,  1074, -1000,    91,   501,    91,   113,   371,
     701,   100,   702,    91,  1082,   721,  1886,   703,   497,   815,
      91,  1381,   729,   619,   144,   704,   705,   107,    91,   317,
    2034,   977,  1579,    80,   706,  1754,   707,   984,   345,   108,
      91,   793,    80,   793,   748,   124,   498,   115,    82,  1335,
     405,  1887,    91,    91,    91,   759,   398,    82,   397,  1102,
     502,   978,  1515,  2051,  1752,   981,   974,   625,   301,  1519,
     303,   987,   714,  1541,  -816,  -848,  1940,  1997,  2061,   405,
    1989,  1922,  1923,  1538,   964,    91,  1991,  1069,  1071,   398,
     405,  1947,  1948,  1450,   375,   713,  1005,   718,  1252,  1922,
    1923,  1009,  1389,   318,   726,  -848,  1704,   114,   696,  2053,
    2054,   115,   170,   709,   888,   785,  1765,    91,    91,   210,
     211,  1137,   108,   965,  2003,   207,   410,  1451,   124,  1378,
     212,   863,   108,  1583,   866,   301,   868,   966,   124,  1907,
     871,    91,   208,   648,  1995,    91,  1908,  1594,  1218,   874,
     171,    91,   876,   877,   878,   825,   362,  1584,   795,  1909,
     898,  1605,   678,   829,  2006,  2007,   831,    91,   670,  1924,
     209,   114,    91,    91,    91,  1995,   792,   794,    91,    91,
    1755,   153,  1764,  1133,   153,  1570,  1767,  1952,  1517,   334,
     928,   994,   120,  1390,   832,  1445,   303,   797,  1201,    91,
     767,  1149,   709,   398,   115,   176,  1583,    91,   824,    91,
     553,  1415,  1391,  1606,   207,   828,   865,  1239,    91,    91,
    1706,   710,    91,   964,   303,  1061,  1613,  1176,  1031,    91,
    1584,   208,  1237,  1065,  1586,  1455,   960,  1020,  1528,   756,
    1238,   648,  1529,    91,    91,   153,    91,  1254,  1544,   213,
      70,    91,   961,  1287,   408,    91,   120,   962,  1587,   209,
     590,   711,   965,    20,   114,  1250,   312,    91,    91,  1032,
     101,  1033,   787,   163,  1222,  1289,   966,   313,    91,  1672,
    1007,  2097,  1381,   911,   648,   303,    91,    91,  1034,  1251,
    1035,    91,  1394,   314,   153,  1036,  1335,   341,   783,   191,
    1252,   660,  1067,  1037,  1038,   664,    91,   304,  1072,   481,
     710,   449,  1039,   108,  1040,    91,  1571,   362,    91,   124,
     339,    91,   309,   368,   633,  1021,  1022,   379,   559,  1031,
     738,  1189,   742,   560,   101,  1304,  1392,   788,   153,   120,
    1013,   549,   108,   648,    91,    64,  1023,  1081,   124,   120,
     711,   992,  2102,   108,   648,  1393,  1543,   193,  1004,   124,
    1404,   670,  1570,   977,  1776,    91, -1131,  1239,  1858,  1027,
    1032,  1221,  1033,  1866,  1536,   194,   164,  1127,   108,   359,
    2102,  1974,   460,   372,   124,  1890,    69,    70,   375,  1034,
    1378,  1035,  1189,  1537,    91,    80,  1036,  1432,   555,  1063,
    2133,   742,  1580,   323,  1037,  1038,  1092,  1639,   398,    91,
      82,    91,  1389,  1039,    91,  1040,  1434,   304,    91,   754,
     202,    91,  1687,  1247,   449,  1438,  1076,   101,  -626,  1243,
     397,   202,   898,  1080,   714,   808,  1911,  1084,    85,  1640,
    1642,  1644,   203,  1261,  1189,   408,  1528,  1528,  1528,  1456,
    1529,  1529,  1529,   362,   324,    81,    92,    64,   590,   160,
     301,   767,   561,   761,  1189,   809,   810,   562,   405,   648,
     310,    91,  1530,  1688,   408,    91,   125,   840,   773,  1813,
    2042,   714,   214,   350,  1826,   408,   499,   500,    87,    88,
     888,  1051,   496,   326,  1482,  -680,   362,   501,    91,  1699,
    2014,  1939,  -680,    91,    91,   608,   609,    80,  1536,   497,
     408,  1861,  1192,  1077,  1383,   671,  1862,  1139,  1700,  1892,
      92,   956,    82,   968,    91,    -3,   678,  1770,   910,  1824,
     120,  1981,  1833,   323,  1893,  1189,   324,   498,    92,  1834,
     125,  1699,   177,   304,   298,    91,  1126,   888,  1825,    92,
     325,   610,   611,   549,  1180,   896,  1834,    91,  1835,   120,
    1827,  1181,    92,  1873,  1077,    92,  1868,  1008,  1862,    92,
     120,   304,  1255,  1010,   670,  1906,   370,  1125,  2005,  2030,
    1710,    91,  1051,  1912,  1710,  1731,   757,    91,  1834,   202,
      91,  2020,  1028,   496,   362,   120,   -23,   202,  1731,  1052,
     648,  1101,  1913,  -496,  1794, -1001,  1795,  1916,   555,   502,
     497,   714, -1001,    92,  1440,  1441,   455,   888,  1219,    92,
    1963,   888,    64,   125,  1284,  1964,   393,  2080,   606,   607,
    1603,  1466,  1469,   125,   461,  1189,  1189,  1611,   498,  1053,
    2001,   395,   753,   553,   888,   449,    91,  1627,    91,  1911,
    2071,    91,   113,    91,  2082,   100,   398,   993,   397,    92,
      92,   305,    91,  1628,   416,   670,  1621,  1236,   144,   633,
    -836,   107,    80,   466,  1491,  1006,   438,   880,  1387,   449,
    1530,  1530,  1530,    91,  1002,  1003,   481,    82,   888,   596,
    1052,   552,    81,  1099,   549,  1189,   754,   469,  2108,  -693,
     812,  1106,   888,   228,   813,  1287,  -693,   723,   671,  1981,
    1127,   714,  2110,   511,   818,   460,   620,  1288,   623,   833,
     503,   881,    64,   834,    92,    87,   819,  1289,   305,   504,
    1053,   108,   882,   883,   601,   505,  1471,   124,  1739,  -497,
     172,   602,   603,   173,   174,    19,   175,   506,  1599,    14,
      15,    16,    17,    18,  2092,  1484,   507,    92,  1306,  2093,
      14,    15,    16,    17,    18,  2146,   514,    91,    92,    91,
    2147,   508,    80,  1736,   515,    91,   108,   520,   693,    92,
     695,   678,   124,   558,    92,    92,   563,    82,   600,  1425,
     880,    54,    55,    56,    57,    58,    59,    60,    61,   838,
     847,  1001,   648,   839,   848,    92,    91,   800,  1199,  1200,
    1489,   714,   614,    92,   125,    64,  1108,   616,    91,   849,
      91,    64,   362,   834,  1703,   860,    64,   896,    92,   714,
      14,    15,    16,    17,    18,   754,  1777,   943,   944,   945,
     946,   351,   352,   125,   353,   615,   549,    91,   115,   770,
     354,   887,   775,   553,   125,   888,   617,   160,  1789,    91,
      91,    92,  1828,   408,  1091,    80,    81,   678,  1092,   620,
      91,    80,  1004,    92,    92,  1400,    80,   645,  1132,   125,
      82,   397,   813,   502,  1457,   714,    82,   596,  1814,    81,
    -499,    82,   714,   654,  1134,   621,    64,  1731,   813,    87,
      88,  1135,   667,    91,   673,   834,   856,  1191,   114,   686,
     714,   818,   823,   993,   481,   623,   481,  1849,  1850,  1851,
    1852,   671,    87,   819,    64,   743,  1259,   723,   688,  1044,
     888,   714,    69,    70,   800,   691,   502,    64,   714,  1420,
    1853,    91,    91,   678,   113,  1004,    80,   100,   120,  1859,
      92,   449,   712,   754,   692,  1154,  1486,    91,  1487,   714,
     320,    82,  1597,   107,  1458,   734,  1258,  1869,   541,   542,
     848,   870,   455,  1106,    80,  1474,   975,   735,   502,    91,
     623,    64,   153,   747,    85,   317,   153,    80,   397,    82,
    1483,   723,   714,   120,   743,   714,  1897,  1295,   782,  1297,
     552,   888,    82,   888,  1493,  1106,   800,  1894,   758,   975,
     714,   880,    91,   623,  1598,   914,  1668,   916,   848,  1624,
     919,  1845,   982,  1625,   804,   888,   623,   931,  2058,   786,
     678,    80,   200,  1849,  1850,  1851,  1852,   803,  -498,  1190,
      64,  1849,  1850,  1851,  1852,   880,    82,  1509,    14,    15,
      16,    17,    18,   305,    91,   827,  1853,  1870,  2058,   468,
     468,   888,   822,  1871,  1853,  1854,  1150,   848,   108,    81,
     714,   101,  1872,    64,   124,  1107,   888,    92,   460,   381,
     679,    92,    91,   830,   382,   455,   596,   386,  1108,   391,
      80,   818,  2105,   604,   605,   623,   835,  1917,  1574,   836,
    1190,   848,    87,   819,  1968,    82,   850,  1998,   888,   999,
    1000,   888,  2096,   851,    64,   625,   888,    64,  2162,   852,
    1108,   853,  2159,    80,  1975,  1272,  1593,   551,  1977,   714,
      92,  2168,    92,  2121,   854,  2169,  1004,  2125,    82,   855,
     115,   612,   613,  1711,   889,  1231,  1679,  1711,   892,  1602,
     893,    92,  1190,    14,    15,    16,    17,    18,  1276,   936,
     937,   938,   714,    92,    80,   656,   657,    80,  1810,  1811,
    1812,   113,  1190,   678,   100,   899,    91,    91,    91,    82,
     658,   659,    82,  1636,  1637,   678,   908,    92,   525,   909,
     107,   766,    70,    92,   816,   393,   552,   625,  1106,   158,
    1380,   912,  1280,   932,   678,   113,   714,   969,   100,   409,
      91,  1742,  1743,  1744,  1745,  1746,  1655,   -18,  1657,    64,
     517,   698,   890,   891,   107,    91,   620,   900,    91,    91,
    1017,    91,   125,  1018,  1660,    91,  1026,   381,   382,    91,
     652,    91,   391,  1190,  1029,   367,   378,  1425,  1041,   589,
     599,  1042,    92,  2040,    92,  1043,  1713,    92,   153,  1045,
    1713,  1046,   699,  1047,   700,  1520,  1521,  1522,  1048,    80,
    1837,  1837,  1837,   481,   153,   371,  1049,   125,   658,  1173,
     678,   701,  1050,   702,    82,   120,  1531,    64,   703,  1055,
     153,  1215,  1216,    91,  1089,   108,   704,   705,    91,    91,
      91,   124,  1106,   888,  1220,   706,   455,   707,  1078,    64,
    1245,  1246,  1090,  1108,  1696,  1781,  1079,    14,    15,    16,
      17,    18,  1291,  1292,  1448,  1449,   381,  1454,  1449,   108,
    1481,  1449,  1485,  1449,  -624,   124,  -622,    80,  1579,  1580,
     455,  1088,   596,  1190,  1190,  1646,  1647,  1107,  1669,   888,
    1793,  1449,    82,    14,    15,    16,    17,    18,  1093,    80,
     375,  1031,  1096,   101,  1902,  1449,  1097,   115,  1903,  1449,
    1966,  1967,  1460,    92,    82,  1211,   714,  1922,  1923,  1107,
      91,    92,  1098,    64,    91,    91,  1104,    92,  1114,    14,
      15,    16,    17,    18,  1464,   113,  1799,  1800,   623,   113,
     113,   115,  1032,  1190,  1033,  1118,   678,  2159,  2160,   939,
     940,  1120,    92,   113,  1446,  1447,  1123,  1108,   162,    64,
    1131,  1034,  1697,  1035,    92,   362,    92,  1380,  1036,   900,
     678,   678,  1880,    80,   941,   942,  1037,  1038,   947,   948,
      91,  1138,  1012,  1159,   679,  1039,  1160,  1040,    82,  -500,
    1217,  1766,  1768,    92,  1161,    64,    91,   756,  1838,  1839,
    1696,  1380,  1698,  1162,  1163,  1696,    92,    64,  1829,    80,
    1167,  1164,  1165,    92,   709,  1166,    92,    64,  1168,   589,
    1170,  1171,  1172,   153,    82,   589,    64,  1178,  1179,  1212,
      91,  1223,    91,  1186,  1531,  1531,  1531,   164,  1677,  1678,
    1682,   153,  1195,  1196,  1197,    80,  1202,  1203,  1106,    92,
    1224,  1213,   120,  1225,  1226,  1227,  1228,    80,  1229,   108,
      82,  -501,  1230,   108,   108,   124,    91,    80,  2029,   124,
     124,    91,    82,  1242,  1253,   153,    80,   108,    91,  -165,
      91,  1257,    82,   124,  1285,  1260,   120,  1316,    91,  1263,
    1264,    82,  1467,  1265,  -819,   468,   623,  1962,  1266,   698,
     153,  1267,  1494,    92,  1382,  1268,   714,   678,  1697,   125,
    1269,  1498,  1107,  1697,   678,   714,  1270,  1271,  1294,  1296,
    1633,    64,   710,  1298,  1386,  1395,   481,   153,  1309,  1403,
     101,   115,   368,   379,    64,   115,   115,    64,  1310,   935,
     699,  1311,   700,  1401,  1407,   678,  1408,  1409,  1698,   115,
     371,  1410,   455,  1698,  1411,  1051,  1412,  1413,  1421,   701,
    1414,   702,   711,  1108,   101,  1418,   703,  1419,  1986,   678,
    1431,    80,  1433,  1435,   704,   705,    92,   754,   525,  1436,
    1437,  1439,  2035,   706,    80,   707,    82,    80,   359,   372,
    1459,  1715,   648,  1442,  1452,  1715,  1715,  1479,  1480,    82,
      92,  1488,    82,  -820,  1568,  1545,  1502,  1546,  1549,  1715,
     714,  1550,  1559,  1560,  1561,  1563,  1107,   -22,    91,  1614,
    1618,    91,  2045,   714,  1572,  1207,   714,   888,    92,  2086,
     678,   678,  1604,  1207,  1573,   375,  1608,   678,  1619,   679,
    1609,  1622,  1696,   153,  -123,  -123,  -123,  -123,  -123,  -123,
     678,  1623,   113,  1626,  1631,   481,  1635,   481,   381,  1638,
     678,  1648,   678,  1052,    -3,   153,  1649,   502,  1656,   153,
     153,  1658,  1659,  1661,  1665,   678,   120,   678,   678,   678,
     120,   120,  1666,   153,   195,     6,     7,     8,     9,    10,
      11,    12,    13,  1667,   120,   481,  1671,  2100,  1673,  1986,
     362,  1684,  1686,  1053,  1538,  1207,  1717,  2035,    91,  1732,
    1733,  2035,  2035,  1735,  1737,   678,  1580,  1757,  1207,   678,
    1750,  1758,  1760,  1759,   678,   679,  1761,  1335,  1012,  1771,
    1772,  1774,   163,   153,  1775,  1782,   125,  2123,  1790,  1785,
    2144,  1823,  1791,  1792,   757,  1796,   797,    91,  1246,    91,
    1697,   709,   380,  1797,   101,  1798,  1690,  1801,   101,   101,
     481,  2154,  1802,   207,  1993,  2154,  1803,  1805,  1809,  1806,
     125,   775,   101,   678,    92,    92,   108,  2163,  1807,  1841,
     208,  1842,   124,   113,   318,   678,  2132,    92,  1846,   678,
    1698,    94,  2023,  1848,   161,  1898,  1877,  1879,    91,  1895,
     753,  2141,  2161,  1899,   678,  1930,  1904,  1905,   209,  1935,
    1936,  1051,  1949,   113,  1169,  1956,    91,    91,  1951,  1535,
    1174,  1970,  1107,  1961,    64,  1965,    92,  1958,  1976,  1978,
     754,  1182,  1979,  1980,   303,   153,   714,  1984,  2000,    92,
    2002,  2013,   589,  2008,    92,    92,    92,   113,   115,   678,
    2015,  2021,  2032,  2025,  2043,    94,  2044,  2055,   159,   710,
     327,   328,    71,    72,    73,    74,    75,    76,    77,    78,
    2064,  2033,  2079,   204,    80,  2081,   250,    91,  2083,  1849,
    1850,  1851,  1852,  2087,    94,  2089,   678,  2090,   679,    82,
    2091,   678,   888,  2107,   670,  2094,  2095,   330,  2109,   711,
     358,  2111,  1853,  2119,    94,  2124,  1958,   108,  1715,  1233,
      84,  -197,  1207,   124,  2128,  2122,  2129,   678,  2134,  1052,
     678,  2151,   678,   359,   372,  2143,    92,  2145,  2153,  2157,
    2156,  2167,    89,  2166,   949,  1787,  2170,   108,   951,   884,
     950,   678,   161,   124,   953,  1453,   952,  1567,    94,    91,
     586,   161,    92,  1575,   419,   427,  2152,  1719,    91,  1053,
     125,  2101,  1953,  1741,   125,   125,  2118,   448,  1889,  1946,
    2098,   108,  2085,  1875,  1876,  2024,  2084,   124,   125,   115,
    2126,   183,   153,  2158,  1576,  1592,    92,   388,  1983,   746,
    2049,   470,   486,   120,   204,   204,   159,   399,  1607,   777,
      71,    72,    73,    74,    75,    76,    77,    78,  1683,   115,
    1896,  1590,   159,  1256,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,   486,   250,   161,  1535,  1535,  1535,
    1024,  1116,  1681,  1535,  1773,     3,    92,  1136,    92,  1715,
     250,  1141,  1142,   115,  1143,  1670,     0,  1233,    84,   780,
       0,     0,     0,   753,     0,     0,     0,     0,     0,   626,
       0,   647,  -122,  -122,  -122,  -122,  -122,  -122,     0,  1715,
      89,   101,    92,   471,     0,     0,     0,    92,     0,     0,
       0,   448,     0,     0,    92,   448,    92,   540,  1577,   250,
       0,   159,   358,   184,   185,    71,    72,    73,    74,    75,
      76,    77,    78,  1715,     0,   206,     0,     0,     0,  1426,
    1427,  1428,     0,   153,   419,     0,     0,  1429,  1430,   330,
     330,     0,     0,     0,   120,     0,    64,     0,     0,   333,
       0,     0,    14,    15,    16,    17,    18,     0,   679,     0,
     419,     0,   472,   153,     0,     0,     0,   448,    94,     0,
       0,  1207,     0,     0,   120,     0,  1207,  1207,  1207,     0,
     159,     0,     0,   358,    71,    72,    73,    74,    75,    76,
      77,    78,   690,     0,   159,    92,    80,   153,    71,    72,
      73,    74,    75,    76,    77,    78,   421,     0,   120,    81,
       0,    82,     0,     0,     0,     0,   419,     0,    64,     0,
       0,   427,   101,     0,     0,     0,     0,   427,   419,   419,
       0,    83,    84,     0,     0,     0,   448,   161,     0,     0,
       0,     0,    87,    88,    92,     0,   308,    92,     0,     0,
       0,     0,   101,     0,    89,     0,     0,   250,   448,   820,
     647,    14,    15,    16,    17,    18,   885,     0,    80,    14,
      15,    16,    17,    18,  1175,     0,   784,     0,     0,     0,
     517,    81,     0,    82,   679,     0,   101,     0,     0,     0,
       0,     0,     0,     0,   780,   250,   486,   125,   841,     0,
       0,     0,     0,   840,     0,   204,     0,   714,     0,     0,
       0,   627,     0,   308,    87,    88,     0,     0,     0,   486,
       0,     0,   486,     0,   161,   161,     0,     0,   486,     0,
       0,     0,     0,     0,    92,     0,     0,   486,     0,     0,
     161,   161,   161,   250,     0,  1064,     0,   564,     0,   565,
     566,   567,   199,     0,   493,   448,     0,     0,   903,     0,
       0,     0,     0,     0,   161,     0,   421,     0,  1207,   725,
    1207,   697,   333,    92,     0,  2031,     0,     0,     0,     0,
     568,     0,   540,   569,   570,   540,     0,     0,   571,   572,
       0,   540,   421,     0,   361,   780,     0,     0,   250,     0,
     540,   647,   598,     0,  1960,     0,   383,     0,   390,   159,
     392,   184,   185,    71,    72,    73,    74,    75,    76,    77,
      78,     0,   777,     0,  2072,     0,     0,     0,   125,   540,
       0,     0,   626,     0,     0,     0,   626,     0,     0,     0,
       0,     0,    92,    92,     0,     0,     0,     0,   421,   361,
       0,     0,   390,   392,     0,     0,   448,   448,   125,     0,
     798,   421,  1140,     0,     0,     0,     0,   448,     0,   448,
       0,   647,   780,     0,     0,   448,     0,     0,  1157,   161,
       0,     0,  1158,  1960,     0,  1551,     0,     0,     0,     0,
     780,     0,   125,     0,     0,     0,     0,     0,   486,     0,
       0,     0,     0,    92,     0,     0,   330,   679,   524,     0,
     744,     0,   780,     0,     0,   159,     0,   846,   330,    71,
      72,    73,    74,    75,    76,    77,    78,   917,   780,     0,
       0,     0,   780,   858,     0,     0,   861,     0,     0,     0,
       0,     0,   419,     0,   486,   486,     0,     0,   419,     0,
     250,     0,     0,     0,     0,   361,     0,     0,     0,     0,
       0,   653,     0,   392,     0,    14,    15,    16,    17,    18,
     918,     0,     0,     0,     0,  2164,     0,     0,     0,   744,
       0,     0,     0,     0,  2171,     0,   361,     0,     0,     0,
       0,     0,     0,     0,  1062,     0,     0,  1240,     0,     0,
     598,     0,  1066,     0,  1241,     0,     0,   419,     0,   419,
       0,     0,    94,     0,     0,     0,   161,     0,     0,     0,
       0,  1075,     0,     0,     0,     0,     0,   448,   820,   780,
     820,    64,  1083,     0,     0,     0,     0,     0,   308,   350,
     161,     0,     0,     0,     0,     0,     0,   780,     0,     0,
     841,   841,     0,     0,   780,     0,     0,   361,   486,   390,
     392,   864,     0,     0,   959,     0,     0,   493,   627,     0,
     872,     0,     0,     0,     0,     0,     0,   250,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,   161,
       0,   361,     0,     0,    81,   361,    82,     0,     0,     0,
       0,   361,     0,     0,     0,   448,     0,   448,     0,     0,
       0,   250,     0,   250,     0,     0,  1814,     0,     0,  1883,
     714,     0,     0,     0,     0,     0,     0,    87,    88,     0,
     725,     0,     0,   432,   361,     0,   653,   392,  1030,     0,
       0,   433,   434,   435,   436,     0,     0,     0,     0,   159,
     333,   327,   328,    71,    72,    73,    74,    75,    76,    77,
      78,     0,  1315,     0,     0,   277,   448,   626,     0,     0,
       0,     0,  1416,     0,   421,     0,  1417,     0,    81,   361,
     421,   626,   159,     0,   184,   185,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,     0,   995,
    1689,    84,     0,     0,     0,     0,     0,  1690,     0,     0,
       0,    87,    88,     0,     0,     0,     0,     0,   448,     0,
       0,     0,   780,    89,     0,     0,   780,     0,     0,     0,
       0,     0,   361,     0,     0,     0,     0,   437,   448,  1100,
     493,   421,   159,     0,   458,   459,    71,    72,    73,    74,
      75,    76,    77,    78,     0,   438,     0,     0,   201,     0,
       0,     0,     0,     0,   419,     0,     0,     0,     0,   846,
     846,     0,     0,     0,     0,   361,     0,   653,   392,     0,
    1152,     0,     0,  1155,     0,     0,   493,   493,   161,     0,
       0,     0,     0,     0,  1506,     0,    85,     0,  1507,     0,
     363,     0,  1508,     0,   486,     0,     0,     0,   460,   448,
       0,     0,   384,   387,   277,     0,     0,     0,   419,   427,
     161,     0,     0,     0,   486,  2039,     0,     0,     0,   593,
       0,   820,     0,     0,   361,   653,  1293,     0,     0,     0,
       0,     0,     0,     0,   780,   361,     0,     0,   780,  1301,
       0,   651,   780,  1305,   841,   363,   159,  1308,   524,     0,
      71,    72,    73,    74,    75,    76,    77,    78,  1204,     0,
     593,     0,     0,  1205,   593,  1206,     0,   161,   277,     0,
     159,     0,   493,   448,    71,    72,    73,    74,    75,    76,
      77,    78,   361,     0,     0,     0,     0,     0,     0,   959,
     493,     0,     0,     0,     0,     0,     0,     0,    84,   448,
     448,  1235,     0,   627,   526,     0,   361,     0,     0,     0,
       0,   361,     0,   361,     0,     0,   287,     0,     0,     0,
       0,     0,    84,   159,     0,  1011,   277,    71,    72,    73,
      74,    75,    76,    77,    78,  1204,   361,     0,   361,   361,
    1205,     0,  1206,     0,     0,   995,   448,     0,     0,     0,
     361,   363,     0,   725,     0,     0,     0,     0,     0,   448,
    1274,     0,     0,   361,  1278,     0,     0,     0,  1282,     0,
       0,     0,  1650,   361,     0,    84,  1651,     0,  1444,  1652,
       0,     0,   363,   159,     0,  1073,   421,    71,    72,    73,
      74,    75,    76,    77,    78,   277,     0,     0,     0,     0,
       0,     0,     0,   161,     0,     0,  1662,     0,     0,     0,
    1663,     0,   161,     0,  1664,     0,   277,   593,     0,     0,
       0,   486,   780,     0,     0,     0,   780,     0,   159,   780,
     184,   185,    71,    72,    73,    74,    75,    76,    77,    78,
     421,     0,     0,     0,     0,     0,     0,   486,   250,     0,
       0,     0,     0,   363,   277,   486,   780,     0,     0,     0,
     780,     0,     0,   846,   780,   287,     0,     0,     0,  1518,
     195,     6,     7,     8,     9,    10,    11,    12,    13,   358,
      94,   361,     0,  1542,     0,     0,     0,   363,     0,     0,
       0,   363,   419,     0,     0,   161,    19,   363,     0,     0,
       0,     0,   277,  1564,  1553,     0,     0,   161,     0,     0,
     540,     0,     0,     0,   593,     0,     0,     0,   651,     0,
     820,     0,     0,     0,     0,     0,  1784,     0,     0,   287,
     363,     0,     0,     0,     0,     0,   448,   448,     0,     0,
       0,     0,     0,  1462,     0,   361,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,   493,   277,     0,     0,
       0,     0,     0,     0,     0,  1476,     0,     0,     0,     0,
       0,   361,     0,     0,     0,     0,   780,     0,     0,     0,
       0,     0,  1496,     0,     0,  1500,     0,   287,     0,  1504,
       0,   159,     0,   184,   185,    71,    72,    73,    74,    75,
      76,    77,    78,   925,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   277,   593,     0,     0,     0,
     161,   161,   161,   161,     0,   161,   161,     0,   363,     0,
       0,  1691,   427,     0,   593,     0,     0,     0,     0,     0,
     470,     0,     0,     0,     0,   486,   926,     0,     0,   486,
     486,     0,     0,     0,     0,     0,   287,     0,     0,     0,
     486,     0,     0,   486,     0,     0,     0,     0,     0,     0,
     127,   363,     0,   127,     0,     0,     0,   287,     0,     0,
       0,     0,     0,   361,     0,     0,     0,     0,     0,     0,
       0,   358,     0,     0,     0,   361,     0,     0,     0,     0,
       0,     0,  1705,  1707,     0,     0,     0,     0,     0,   277,
       0,     0,     0,   161,     0,   287,     0,     0,   361,     0,
       0,     0,   471,     0,   421,   161,     0,     0,     0,     0,
     363,  1616,     0,     0,   127,     0,     0,     0,     0,     0,
     159,   363,   184,   185,    71,    72,    73,    74,    75,    76,
      77,    78,     0,  1769,   526,   280,     0,     0,     0,     0,
       0,     0,     0,   127,     0,     0,     0,     0,   539,     0,
       0,     0,     0,     0,     0,   593,     0,  1691,  1815,   365,
       0,     0,  1691,   127,   486,     0,   593,     0,     0,  1691,
       0,  1691,     0,   159,  1058,   327,   328,    71,    72,    73,
      74,    75,    76,    77,    78,     0,   361,     0,     0,     0,
       0,     0,     0,     0,   427,   161,     0,   127,   287,   363,
       0,   127,     0,     0,     0,     0,     0,   127,     0,     0,
     127,     0,     0,     0,   365,     0,   277,   493,     0,     0,
       0,     0,   363,     0,     0,   445,   127,     0,   462,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,   460,
       0,     0,     0,  1695,     0,     0,   593,     0,     0,   363,
     277,   280,   593,   159,     0,   327,   328,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,   287,   159,   287,
     184,   185,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,    81,   280,   280,   127,     0,     0,     0,  1874,
       0,     0,     0,     0,  1815,  1815,     0,     0,     0,   280,
       0,     0,     0,     0,   622,    84,     0,     0,   623,  1691,
       0,     0,  1691,     0,     0,    87,   624,     0,     0,     0,
     365,   127,     0,     0,   427,     0,     0,    89,   625,     0,
    1555,     0,     0,   361,     0,     0,     0,  2099,     0,     0,
     127,     0,   486,     0,   127,     0,     0,     0,   280,     0,
     287,   365,     0,  1548,     0,     0,     0,   161,     0,     0,
       0,     0,     0,     0,   159,  1562,     0,   363,    71,    72,
      73,    74,    75,    76,    77,    78,  1204,  1818,     0,     0,
       0,  1205,   493,  1206,     0,     0,     0,   780,   127,  1695,
    1815,     0,     0,     0,  1695,     0,     0,     0,     0,  1691,
       0,  1830,     0,  1695,     0,   127,   127,   127,     0,     0,
       0,     0,     0,     0,     0,     0,    84,   127,     0,  1641,
       0,     0,   365,   539,     0,     0,   539,   593,   127,     0,
       0,     0,   539,     0,     0,     0,   161,     0,  2028,   427,
       0,   539,     0,   771,     0,     0,   127,     0,     0,     0,
       0,   127,     0,   127,     0,     0,   365,   127,     0,   593,
     365,     0,  1815,     0,     0,     0,   365,     0,     0,     0,
     539,     0,     0,   161,     0,   127,   127,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   361,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   280,   127,     0,   365,
       0,     0,     0,   161,     0,   287,   493,  2028,  2028,     0,
       0,     0,     0,  1818,  1818,     0,     0,     0,     0,     0,
       0,   159,     0,   327,   328,    71,    72,    73,    74,    75,
      76,    77,    78,   955,   280,   280,     0,   161,     0,     0,
       0,  1918,     0,     0,  1695,   361,     0,     0,     0,     0,
      81,     0,     0,     0,     0,     0,     0,     0,   280,     0,
       0,   280,     0,   127,   127,   462,   287,   280,  2028,   363,
       0,     0,   329,    84,     0,     0,   280,     0,     0,   127,
     127,   127,   280,    87,    88,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   127,    89,     0,   905,   280,     0,
       0,     0,     0,   127,     0,     0,     0,     0,     0,  1818,
    1762,  1763,     0,     0,     0,     0,     0,   159,   287,   184,
     185,    71,    72,    73,    74,    75,    76,    77,    78,     0,
    2120,  1695,     0,     0,     0,     0,     0,   280,   287,     0,
     365,     0,     0,     0,     0,     0,     0,     0,  1068,  1070,
       0,     0,   593,     0,     0,     0,     0,  1818,     0,     0,
       0,     0,  2142,     0,     0,     0,     0,     0,   361,     0,
     421,   127,     0,     0,     0,     0,   806,  2047,     0,     0,
       0,  1818,     0,   159,     0,   327,   328,    71,    72,    73,
      74,    75,    76,    77,    78,   127,   127,   277,     0,   365,
    1314,     0,     0,     0,   771,     0,   127,     0,   127,   287,
     365,     0,    81,     0,   127,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,  1818,  1818,     0,     0,
       0,     0,     0,     0,  1689,    84,     0,   280,     0,   798,
     421,  1690,     0,     0,     0,    87,    88,  1847,     0,     0,
       0,     0,  1513,     0,  1857,     0,   593,    89,     0,     0,
       0,     0,     0,  1059,     0,     0,     0,     0,     0,     0,
       0,     0,   127,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,   280,   280,  1882,   361,  1818,   365,   280,
       0,     0,     0,     0,     0,     0,     0,     0,   361,     0,
     421,     0,     0,     0,     0,     0,   127,     0,     0,   287,
     287,   159,     0,   327,   328,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,   365,   159,    80,   213,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   365,   363,
      81,     0,    82,     0,     0,     0,     0,     0,     0,   127,
       0,   127,     0,     0,     0,   127,   287,     0,     0,     0,
       0,     0,   329,    84,   361,     0,   127,   127,     0,   287,
    1920,  1921,     0,    87,    88,     0,     0,  1931,    84,   127,
       0,  1011,     0,     0,     0,    89,     0,     0,     0,   127,
    1945,     0,     0,     0,     0,   361,   392,   280,     0,     0,
    1954,     0,  1955,     0,     0,   127,     0,     0,     0,   127,
       0,     0,     0,   361,     0,  1969,   280,  1971,  1972,  1973,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,   127,     0,   127,     0,     0,     0,
     280,     0,   280,     0,     0,  1994,   905,     0,   287,  1999,
       0,     0,   593,     0,  2004,     0,     0,     0,     0,     0,
       0,   159,     0,   327,   328,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,    80,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,   127,   127,     0,     0,     0,
       0,     0,     0,  2050,     0,     0,     0,     0,     0,     0,
       0,     0,  2027,    84,   127,  2059,   714,     0,    64,  2062,
       0,   127,     0,    87,    88,     0,   110,     0,     0,   165,
       0,     0,   361,     0,  2078,    89,   287,   287,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   127,     0,     0,
       0,     0,   159,     0,   327,   328,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,   127,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2106,
       0,    81,     0,    82,     0,     0,     0,     0,     0,   223,
     110,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   418,    84,     0,     0,     0,     0,     0,
       0,   271,     0,     0,    87,    88,  2127,   127,     0,   302,
       0,  2130,     0,     0,     0,     0,    89,     0,     0,   127,
       0,     0,     0,   280,     0,     0,     0,     0,   127,   373,
       0,     0,   556,     0,     0,    85,   478,  2148,   365,   127,
    2150,     0,  2130,   280,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,  1402,     0,     0,     0,     0,
       0,  2150,     0,   407,     0,     0,     0,   411,     0,     0,
       0,    64,     0,   110,     0,     0,     0,     0,     0,   127,
       0,     0,  1510,   127,     0,     0,     0,     0,     0,     0,
     127,     0,   450,     0,     0,     0,   127,     0,     0,     0,
       0,     0,   127,     0,     0,   159,   593,   327,   328,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,   495,
       0,    80,     0,     0,     0,     0,     0,     0,   127,   127,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,   539,     0,     0,     0,     0,     0,     0,     0,     0,
     548,   411,     0,     0,     0,     0,  1689,    84,     0,     0,
       0,     0,     0,     0,     0,   591,     0,    87,    88,     0,
       0,     0,     0,     0,     0,   127,     0,     0,     0,    89,
       0,     0,     0,     0,   363,   593,     0,   650,   127,   159,
       0,   766,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   127,     0,     0,     0,   127,   661,     0,     0,   127,
     661,     0,     0,     0,   271,     0,     0,     0,     0,     0,
     159,  1514,   768,   769,    71,    72,    73,    74,    75,    76,
      77,    78,   127,     0,     0,     0,     0,     0,     0,     0,
       0,   127,     0,     0,   998,   708,     0,     0,     0,     0,
     280,     0,     0,   159,   407,   327,   328,    71,    72,    73,
      74,    75,    76,    77,    78,   732,     0,     0,     0,     0,
     737,   739,   271,   302,    85,     0,   280,   280,     0,     0,
       0,     0,    81,   407,   280,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,   762,     0,   363,
       0,   764,     0,     0,   418,    84,   765,     0,   365,   127,
       0,     0,   776,     0,     0,    87,    88,   739,     0,   407,
       0,     0,     0,   789,   127,     0,     0,    89,     0,     0,
       0,     0,     0,     0,   799,     0,   127,     0,     0,  1708,
    1716,   271,     0,  1708,  1727,     0,     0,     0,     0,  1734,
       0,     0,     0,  1738,     0,  1740,     0,  1727,     0,     0,
       0,     0,   548,   591,     0,   127,   127,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,   127,
       0,     0,     0,   127,     0,     0,   127,     0,     0,     0,
     271,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   127,     0,   320,     0,   127,     0,     0,
     159,   127,   184,   185,    71,    72,    73,    74,    75,    76,
      77,    78,     0,    64,     0,     0,     0,     0,   450,   127,
     127,   127,   127,   127,   127,   127,     0,     0,     0,     0,
     661,   365,     0,     0,   907,     0,     0,     0,     0,   411,
       0,     0,     0,    64,   280,     0,     0,   159,   280,   280,
       0,    71,    72,    73,    74,    75,    76,    77,    78,   280,
       0,     0,   280,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   548,  1843,     0,    81,   159,    82,   327,
     328,    71,    72,    73,    74,    75,    76,    77,    78,     0,
     365,     0,     0,    80,  1863,  1865,     0,     0,    83,    84,
       0,     0,     0,     0,     0,  1778,    81,   776,    82,    87,
      88,   971,   127,   127,     0,     0,     0,     0,     0,     0,
       0,    89,     0,     0,   127,  1885,     0,     0,   329,    84,
       0,   450,   591,     0,     0,     0,     0,   997,     0,    87,
      88,     0,   271,  1985,   271,     0,     0,     0,     0,     0,
     661,    89,   159,     0,   411,     0,    71,    72,    73,    74,
      75,    76,    77,    78,  1204,     0,     0,     0,     0,  1205,
     159,  1206,   184,   185,    71,    72,    73,    74,    75,    76,
      77,    78,   159,   280,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,   215,     0,     0,
     216,     0,   217,   218,    84,   219,     0,  1643,   776,     0,
    1929,     0,     0,   365,   127,     0,   121,     0,  1932,     0,
    1934,   621,   221,  1938,  1944,   548,  1727,     0,     0,     0,
       0,  1950,     0,     0,     0,     0,    85,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,     0,   229,   230,   231,     0,   232,   233,     0,     0,
     121,     0,     0,     0,    81,   776,     0,   110,     0,     0,
       0,  1109,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   275,   661,   776,  2010,  1121,   234,     0,     0,    85,
     478,  2017,  2019,     0,     0,     0,   237,    87,    88,   239,
     240,   241,   242,     0,     0,   776,  1144,     0,     0,   374,
       0,     0,     0,     0,  2041,     0,     0,     0,     0,     0,
       0,   776,     0,   365,     0,   776,     0,     0,     0,     0,
       0,     0,   450,     0,     0,     0,     0,     0,   280,     0,
       0,   280,     0,   121,   411,  2063,     0,  2066,     0,     0,
    2068,  2070,     0,   121,     0,     0,   127,  2075,  2077,     0,
     271,     0,   661,     0,     0,     0,   450,     0,   591,     0,
       0,     0,   451,     0,     0,     0,   159,     0,   184,   185,
      71,    72,    73,    74,    75,    76,    77,    78,     0,   573,
     574,   575,   576,   577,   578,   579,   580,   581,   582,   583,
       0,     0,     0,     0,   312,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,     0,     0,  2012,
     365,   271,   776,     0,  2113,  2115,  2117,   667,     0,     0,
     275,   584,     0,  1232,     0,   127,     0,     0,   365,     0,
     776,     0,     0,     0,     0,   592,     0,   776,     0,     0,
       0,     0,  2136,  2138,  2140,   159,     0,   184,   185,    71,
      72,    73,    74,    75,    76,    77,    78,   374,     0,     0,
       0,     0,   127,   271,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   592,     0,  2073,     0,
     592,     0,     0,   271,   275,     0,     0,     0,     0,     0,
       0,     0,   127,     0,     0,     0,     0,     0,     0,     0,
     159,     0,   327,   328,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,   127,     0,    80,     0,     0,     0,
       0,     0,     0,     0,   121,     0,   127,     0,     0,    81,
       0,    82,     0,  1109,     0,     0,    14,    15,    16,    17,
      18,     0,   275,     0,     0,   776,     0,     0,     0,  1379,
       0,   622,    84,   121,   271,   623,     0,     0,     0,     0,
       0,     0,    87,   624,   121,  1109,     0,   763,     0,     0,
       0,     0,     0,     0,    89,     0,     0,     0,     0,     0,
       0,     0,   451,     0,     0,     0,     0,     0,     0,   121,
       0,     0,     0,   374,     0,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,   776,     0,     0,     0,   776,
       0,   275,     0,     0,     0,     0,   776,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   450,     0,
       0,     0,   275,   592,     0,     0,   159,     0,   327,   328,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,    80,     0,   271,   271,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
     275,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,   737,  2027,    84,     0,
       0,   714,     0,     0,     0,     0,     0,     0,    87,    88,
       0,   271,     0,     0,     0,     0,     0,     0,     0,     0,
      89,     0,     0,     0,   271,     0,     0,     0,   275,     0,
       0,     0,     0,     0,     0,     0,     0,   776,     0,     0,
     592,   776,     0,     0,   374,   776,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1109,     0,
       0,     0,     0,     0,     0,     0,     0,  1532,     0,     0,
       0,     0,     0,   275,     0,   159,  1379,   327,   328,    71,
      72,    73,    74,    75,    76,    77,    78,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   271,    81,     0,    82,   451,   215,     0,
    1379,   216,     0,   217,   218,     0,   219,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   418,    84,     0,     0,
       0,   275,   592,   221,     0,  1591,     0,    87,    88,     0,
       0,     0,   451,     0,   451,     0,     0,     0,     0,    89,
     592,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1109,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,     0,   229,   230,   231,     0,   232,   233,     0,
       0,   271,   271,     0,     0,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   451,     0,
       0,     0,     0,     0,     0,   776,     0,   234,     0,   776,
      85,   478,   776,     0,   126,   275,     0,   237,    87,    88,
     239,   240,   241,   242,     0,     0,     0,     0,     0,     0,
     920,     0,   121,     0,     0,     0,     0,     0,     0,   776,
       0,     0,     0,   776,     0,     0,     0,   776,     0,     0,
     159,     0,   327,   328,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,  1532,  1532,  1532,   165,   739,
       0,     0,     0,     0,     0,   451,     0,   121,   126,    81,
       0,   592,     0,     0,     0,     0,     0,     0,     0,     0,
    1714,     0,   592,   451,  1714,  1714,     0,     0,     0,   279,
       0,  2027,    84,     0,     0,   714,     0,     0,  1714,     0,
       0,     0,    87,    88,     0,   451,     0,     0,     0,     0,
       0,     0,     0,     0,    89,     0,     0,   376,     0,     0,
       0,   451,     0,     0,     0,   451,     0,     0,     0,     0,
       0,     0,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1109,   776,
       0,   126,     0,     0,    14,    15,    16,    17,    18,     0,
     451,   126,   592,     0,     0,     0,   275,     0,   592,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     453,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      64,   451,   451,     0,     0,     0,  1832,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   279,   132,
     451,     0,   132,     0,     0,     0,     0,   451,     0,     0,
    1844,     0,     0,   594,   159,     0,   327,   328,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,   451,     0,   376,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,   451,   594,     0,     0,     0,   594,     0,
       0,     0,   279,   132,     0,  1689,    84,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    89,     0,
       0,     0,   132,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   126,   592,     0,     0,     0,     0,     0,     0,
    1910,     0,   132,     0,     0,   451,     0,     0,     0,   121,
     279,   389,     0,     0,   451,     0,     0,     0,     0,     0,
       0,   126,     0,     0,     0,   592,     0,     0,     0,     0,
       0,     0,   126,     0,     0,     0,   132,     0,     0,     0,
     132,     0,     0,     0,  1941,     0,   132,  1714,     0,   132,
     453,     0,     0,     0,     0,     0,     0,   126,     0,     0,
       0,   376,  1959,     0,     0,   451,     0,     0,     0,   451,
       0,     0,     0,     0,     0,     0,   451,     0,     0,   279,
       0,     0,     0,     0,     0,     0,     0,     0,   451,     0,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     279,   594,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   451,   451,     0,     0,     0,     0,
       0,     0,   132,     0,   132,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   279,     0,
       0,  1959,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   451,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   451,     0,     0,     0,  1714,     0,
       0,     0,     0,     0,     0,     0,   279,   451,     0,     0,
       0,   451,     0,     0,     0,   451,     0,     0,   594,     0,
       0,     0,   376,     0,     0,     0,     0,     0,  1714,     0,
       0,     0,     0,  2088,     0,     0,     0,     0,   592,     0,
       0,     0,     0,     0,     0,     0,     0,   132,     0,     0,
     776,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,   279,  1714,     0,   132,     0,   132,     0,     0,     0,
       0,     0,   132,     0,     0,     0,   132,     0,     0,     0,
       0,     0,     0,   275,     0,     0,     0,   132,     0,     0,
     121,     0,     0,     0,     0,   453,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     132,     0,   132,     0,     0,   374,   132,     0,     0,   279,
     594,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     453,     0,   453,     0,     0,   132,     0,     0,   594,     0,
       0,     0,   592,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   451,   451,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   132,   451,   453,     0,     0,   451,
       0,     0,   451,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   279,     0,     0,     0,   132,     0,     0,
     132,     0,   132,   132,     0,     0,   132,     0,     0,   451,
     126,     0,     0,   451,     0,   132,     0,   451,   132,   132,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   131,     0,     0,   131,     0,     0,     0,     0,
       0,     0,   132,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   453,     0,   126,     0,     0,     0,   594,
     121,     0,     0,     0,   121,   121,     0,     0,     0,     0,
     594,   453,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   453,     0,     0,   131,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   453,
       0,     0,     0,   453,     0,     0,     0,     0,     0,     0,
     279,     0,     0,     0,     0,   131,     0,     0,   592,   451,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   131,     0,     0,   453,     0,
     594,     0,     0,     0,   279,     0,   594,   132,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   132,     0,     0,   131,
       0,     0,     0,   131,     0,     0,     0,     0,     0,   131,
       0,     0,   131,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   453,
     453,     0,     0,     0,   136,     0,     0,   136,     0,     0,
       0,     0,   132,   132,     0,     0,     0,     0,   453,     0,
     374,     0,     0,   131,     0,   453,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   132,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   453,     0,     0,     0,   131,     0,   131,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   136,     0,
       0,   453,     0,     0,     0,     0,     0,     0,     0,     0,
     132,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   136,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   132,     0,
       0,     0,     0,     0,     0,     0,     0,   136,     0,     0,
       0,   594,     0,     0,     0,     0,   132,     0,     0,     0,
       0,     0,     0,   453,     0,     0,     0,   126,     0,     0,
       0,     0,   453,     0,     0,     0,     0,     0,     0,     0,
       0,   136,     0,   594,     0,   136,     0,   132,     0,     0,
     131,   136,     0,     0,   136,     0,     0,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,   131,
       0,     0,   592,     0,     0,   131,     0,     0,     0,   131,
       0,     0,     0,   453,     0,     0,     0,   453,     0,     0,
     131,     0,     0,     0,   453,   136,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   453,     0,     0,     0,
       0,     0,     0,   131,     0,   131,     0,     0,     0,   131,
       0,     0,     0,     0,     0,     0,     0,   136,     0,   136,
       0,     0,   453,   453,     0,     0,     0,     0,   131,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   592,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   453,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,   453,     0,     0,     0,     0,   131,     0,     0,
       0,     0,     0,     0,     0,   453,     0,     0,     0,   453,
       0,     0,     0,   453,     0,     0,     0,     0,   121,     0,
     131,     0,     0,   131,     0,   131,   131,     0,     0,   131,
       0,     0,   136,     0,     0,     0,   594,     0,   131,     0,
     451,   131,   131,   131,     0,     0,     0,     0,     0,   136,
       0,   136,   121,     0,   126,     0,     0,   136,     0,     0,
       0,   136,   132,     0,     0,   131,     0,     0,     0,     0,
       0,     0,   136,     0,     0,     0,     0,     0,     0,     0,
       0,   279,   132,     0,     0,     0,     0,     0,   126,     0,
       0,     0,     0,     0,     0,   136,     0,   136,     0,     0,
       0,   136,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   376,     0,     0,     0,     0,     0,     0,
     136,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   132,     0,     0,     0,     0,
     594,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   102,     0,     0,   102,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   453,
     453,     0,     0,     0,     0,     0,     0,     0,     0,   136,
     131,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   453,     0,     0,     0,   453,     0,   131,
     453,     0,   136,     0,     0,   136,     0,   136,   136,     0,
       0,   136,     0,     0,     0,     0,     0,   102,     0,     0,
     136,     0,     0,   136,   136,   136,     0,   453,     0,     0,
       0,   453,     0,     0,     0,   453,     0,     0,   268,     0,
       0,     0,     0,     0,     0,   131,   131,   136,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,   102,     0,   131,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   126,     0,
     132,     0,   126,   126,     0,     0,     0,     0,     0,   132,
       0,     0,     0,     0,     0,     0,   126,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
     102,     0,     0,   131,     0,   132,     0,   428,     0,     0,
       0,     0,     0,   132,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,     0,     0,
       0,   131,     0,     0,     0,     0,   594,   453,   132,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   131,
       0,     0,   136,   132,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   136,     0,     0,     0,     0,   181,   268,     0,     0,
     131,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,   181,     0,     0,     0,     0,   187,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   136,   136,     0,
       0,     0,   181,     0,   465,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   321,     0,   376,     0,
     136,   268,     0,     0,     0,     0,     0,     0,     0,   465,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   132,   132,
     132,   132,   132,   132,   132,   136,     0,   413,     0,     0,
     414,     0,     0,     0,     0,     0,     0,     0,     0,   268,
       0,     0,     0,   132,     0,   439,     0,   132,   132,     0,
       0,     0,     0,   136,     0,   360,     0,     0,   132,     0,
       0,   132,     0,     0,     0,   473,     0,   181,     0,     0,
       0,   136,     0,     0,     0,     0,     0,   473,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   136,     0,     0,     0,     0,     0,   268,     0,
       0,     0,     0,   181,     0,     0,     0,   181,     0,     0,
     181,   181,  1942,   132,   181,   126,     0,   181,   181,   268,
     181,     0,   181,     0,     0,   131,     0,     0,     0,     0,
     594,     0,   321,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   131,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   663,   268,     0,     0,
     668,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   132,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   689,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,     0,   181,     0,     0,   131,     0,
       0,     0,     0,   132,     0,     0,     0,     0,     0,   594,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   321,     0,     0,     0,     0,     0,
       0,     0,     0,   750,   751,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,   126,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
     268,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   126,     0,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   801,     0,     0,     0,     0,   453,     0,
       0,   805,   807,     0,     0,     0,   814,   136,     0,     0,
     126,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   136,     0,   268,
     439,   268,     0,   439,     0,     0,   473,     0,     0,     0,
       0,     0,     0,   131,     0,     0,     0,     0,     0,     0,
     132,     0,   131,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   131,     0,
     136,     0,     0,     0,     0,     0,   131,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,     0,     0,   181,   181,     0,   181,     0,   181,
     181,   131,   268,     0,   181,   181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   131,    14,    15,    16,
      17,    18,     0,   927,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
     465,    52,     0,    53,   102,     0,     0,     0,   102,     0,
       0,   132,     0,     0,     0,     0,   181,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   132,     0,   996,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   136,     0,     0,     0,     0,
       0,     0,     0,     0,   136,     0,     0,     0,     0,     0,
     291,     0,     0,    80,     0,   132,     0,     0,     0,     0,
       0,   131,   131,   131,   131,   131,   131,   131,    82,     0,
     136,     0,     0,     0,     0,     0,     0,   268,   136,     0,
       0,   465,     0,     0,     0,     0,   131,     0,     0,     0,
     131,   131,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   131,     0,   136,   131,     0,     0,   181,     0,   181,
       0,     0,     0,     0,     0,     0,     0,     0,   136,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,   181,
       0,     0,     0,     0,     0,   181,     0,     0,   268,     0,
       0,   291,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   131,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1103,     0,
       0,     0,     0,     0,     0,     0,     0,   439,     0,     0,
     268,     0,     0,     0,     0,     0,     0,     0,     0,   291,
     554,     0,     0,     0,     0,     0,     0,     0,   473,     0,
     268,     0,     0,     0,   291,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   131,     0,     0,     0,     0,
       0,     0,     0,   637,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   136,   136,   136,   136,   136,   136,   136,
       0,     0,     0,   181,     0,     0,   131,     0,     0,   665,
     102,     0,     0,   291,     0,     0,     0,   479,   136,     0,
       0,     0,   136,   136,     0,     0,   102,     0,     0,     0,
       0,   268,     0,   136,     0,     0,   136,     0,   510,     0,
     513,   428,   102,     0,   479,   519,     0,   717,     0,     0,
       0,     0,     0,     0,   717,   528,   529,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   291,     0,   479,   479,   181,     0,     0,     0,   181,
       0,     0,     0,   181,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   136,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   268,   268,     0,  1248,   717,     0,     0,     0,     0,
     291,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   131,   181,     0,     0,   136,     0,     0,
       0,   291,   291,   637,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   268,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   136,     0,
       0,   268,     0,     0,     0,     0,     0,     0,   394,   291,
       0,     0,   717,     0,     0,     0,     0,     0,     0,     0,
       0,   400,     0,   401,     0,     0,     0,  1317,   717,   859,
       0,   717,   862,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   102,     0,     0,     0,     0,
       0,     0,     0,     0,   457,     0,     0,   291,     0,     0,
       0,     0,     0,   102,  1405,     0,     0,     0,   181,   897,
       0,   181,   181,     0,     0,     0,     0,     0,     0,   181,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     268,     0,     0,     0,   131,     0,   516,   102,     0,     0,
       0,     0,   181,     0,   181,     0,     0,   181,     0,     0,
     181,     0,   291,   479,   181,     0,     0,     0,     0,   479,
       0,   360,   102,     0,   131,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   136,   637,     0,     0,   102,
     637,   637,     0,     0,     0,     0,     0,   637,   131,   655,
       0,     0,     0,     0,     0,     0,     0,   990,     0,     0,
     291,   291,     0,     0,     0,     0,     0,     0,   268,   268,
       0,   291,     0,   291,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   554,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   719,   720,     0,     0,   724,     0,     0,   727,
     728,   717,   730,     0,   731,   717,     0,     0,     0,     0,
       0,     0,     0,   479,   479,   479,   479,   479,   479,   479,
     479,   479,   479,   479,   479,   479,   479,   479,   479,   479,
     479,   479,     0,     0,     0,     0,   181,     0,     0,     0,
       0,  1552,  1554,  1556,   291,   102,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   136,     0,   479,     0,
       0,     0,     0,     0,     0,     0,     0,   102,     0,     0,
       0,   102,   102,     0,     0,  1578,     0,     0,     0,     0,
       0,     0,     0,   181,     0,   102,   136,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1317,     0,     0,     0,
    1595,     0,   717,     0,  1596,     0,     0,     0,     0,     0,
       0,     0,     0,   360,     0,     0,     0,     0,     0,     0,
     136,   897,   637,     0,   637,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   102,     0,     0,     0,     0,
       0,     0,     0,     0,   717,   717,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   717,  1153,     0,   717,  1156,
     857,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   291,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   554,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,     0,   291,
       0,     0,     0,     0,     0,   291,     0,   291,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,   102,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1701,
    1702,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     291,   637,     0,   637,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   637,     0,     0,     0,     0,
       0,     0,     0,   976,     0,   181,   979,   980,   479,   983,
       0,   985,   986,     0,   479,     0,   988,   989,     0,     0,
       0,     0,     0,     0,     0,   479,     0,     0,     0,     0,
       0,     0,   291,   181,     0,     0,   479,     0,     0,   181,
       0,  1779,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   291,     0,   717,     0,     0,     0,   717,     0,
       0,     0,     0,     0,     0,   717,  1275,     0,     0,   717,
    1279,     0,     0,   717,  1283,     0,     0,     0,     0,     0,
    1286,     0,     0,     0,     0,     0,   360,   479,  1054,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   102,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,     0,     0,     0,     0,
       0,     0,     0,   291,   717,     0,     0,     0,     0,     0,
       0,     0,     0,  1840,     0,   181,   181,     0,     0,     0,
       0,     0,     0,     0,     0,   637,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   479,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,   181,     0,   717,     0,
       0,     0,     0,   465,     0,     0,     0,     0,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   291,     0,  1122,
       0,  1124,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   265,     0,     0,     0,     0,     0,
    1146,  1147,     0,   291,   291,     0,     0,  1151,     0,     0,
       0,     0,     0,     0,     0,   102,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   717,  1463,
       0,   637,   637,  1470,     0,     0,   181,     0,     0,     0,
       0,     0,     0,     0,     0,   102,     0,     0,     0,     0,
     291,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   291,     0,     0,     0,   717,  1497,     0,
     717,  1501,     0,     0,   717,  1505,     0,     0,     0,   102,
       0,     0,     0,   479,   479,   479,     0,     0,     0,     0,
       0,   479,   479,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,     0,     0,     0,     0,     0,     0,
     487,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1234,     0,     0,     0,     0,
       0,     0,     0,   479,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   547,     0,     0,     0,     0,     0,     0,
       0,   181,   291,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   479,     0,   479,     0,     0,  2149,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1547,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1273,     0,     0,
       0,  1277,     0,     0,     0,  1281,     0,   265,     0,     0,
       0,     0,     0,     0,     0,     0,   717,  1617,     0,     0,
       0,   680,   215,   680,   637,   216,     0,   217,   218,     0,
     219,     0,     0,     0,   479,     0,     0,     0,     0,     0,
     291,   291,     0,     0,     0,  1320,     0,   221,  1322,     0,
    1323,  -256,  -256,  1324,  1325,  1326,  1327,  1328,  1329,  1330,
    1331,  1332,  1333,  1334,  1335,  -355,  -355,  1336,  1337,  1338,
    1339,  1340,  1341,  1342,     0,  1343,  1234,   222,   223,     0,
     674,   225,  1344,  1345,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,  1346,     0,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,     0,     0,     0,     0,     0,     0,   791,     0,     0,
       0,     0,     0,     0,   796,     0,     0,     0,     0,     0,
    -256,  1347,     0,     0,    85,   478,     0,     0,     0,   398,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,   265,  -196,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1461,     0,     0,  1465,  1468,     0,   837,     0,     0,     0,
       0,  1477,  1478,   547,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1490,     0,  1492,     0,     0,  1495,
       0,     0,  1499,     0,     0,     0,  1503,     0,   873,     0,
       0,     0,     0,     0,     0,     0,  1472,     0,     0,     0,
       0,   547,     0,     0,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,     0,   904,   906,     0,   265,
       0,     0,     0,     0,     0,     0,     0,   913,     0,   915,
       0,     0,     0,     0,     0,     0,   924,     0,   929,   924,
       0,     0,   717,     0,     0,     0,   215,     0,     0,   216,
       0,   217,   218,   933,   219,     0,   547,     0,     0,   717,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      64,   221,     0,   296,     0,     0,     0,     0,     0,     0,
       0,   307,   479,     0,     0,     0,     0,     0,     0,     0,
       0,   265,     0,   970,     0,     0,     0,   369,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
      80,   229,   230,   231,     0,   232,   233,     0,  1615,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,   837,   680,     0,     0,   307,     0,
       0,   680,   429,     0,     0,   234,   487,     0,    85,   478,
       0,     0,     0,     0,   456,   237,  1473,    88,   239,   240,
     241,   242,     0,     0,     0,  1465,     0,     0,   717,   717,
       0,     0,  1057,     0,     0,   215,     0,     0,   216,   307,
     217,   218,     0,   219,   717,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   265,     0,
     221,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   543,   296,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   479,     0,     0,     0,   597,     0,     0,
     222,   223,     0,   674,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,   649,     0,
     229,   230,   231,     0,   232,   233,     0,     0,     0,     0,
       0,     0,    81,     0,   717,     0,     0,     0,     0,     0,
       0,     0,   717,     0,     0,     0,   296,     0,  1756,   672,
       0,     0,     0,     0,   234,    84,     0,   675,   676,     0,
       0,     0,   677,   924,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,  1148,     0,     0,     0,
       0,     0,   717,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   265,     0,     0,     0,     0,
       0,     0,   717,  2048,   296,   307,   717,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1177,     0,     0,     0,
     369,  1184,     0,     0,     0,     0,     0,     0,  1184,   837,
       0,     0,     0,     0,     0,  1198,     0,  1804,     0,     0,
     924,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   717,   717,     0,     0,   479,     0,     0,     0,     0,
     680,     0,     0,     0,   672,     0,     0,     0,     0,     0,
       0,     0,     0,   296,   307,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   296,   597,     0,   821,     0,   479,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   717,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   296,   307,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   680,  1891,     0,     0,
       0,     0,     0,     0,     0,     0,   307,  1262,   479,   543,
       0,   543,   307,     0,     0,   307,     0,  1900,  1901,     0,
       0,     0,     0,     0,   543,     0,     0,   543,   543,   543,
     456,     0,     0,     0,   479,     0,   479,     0,     0,     0,
       0,     0,     0,     0,     0,   649,     0,  1914,  1915,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1919,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   479,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   296,     0,     0,   934,     0,
       0,     0,   487,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1982,   479,
       0,     0,     0,   456,   597,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   296,     0,   296,     0,     0,   680,
       0,     0,  2149,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1547,
       0,     0,     0,     0,     0,   307,     0,     0,     0,     0,
       0,   924,     0,     0,  1184,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2046,     0,     0,     0,     0,     0,
    1475,     0,     0,     0,     0,     0,   215,     0,     0,   216,
       0,   217,   218,     0,   219,     0,     0,     0,     0,     0,
       0,   307,   307,     0,   680,     0,     0,   296,     0,  1320,
       0,   221,  1322,     0,  1323,  -257,  -257,  1324,  1325,  1326,
    1327,  1328,  1329,  1330,  1331,  1332,  1333,  1334,  1335,  -355,
    -355,  1336,  1337,  1338,  1339,  1340,  1341,  1342,  1511,  1343,
    1512,   222,   223,     0,   674,   225,  1344,  1345,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,  1346,
       0,   229,   230,   231,     0,   232,   233,     0,     0,     0,
       0,     0,     0,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1565,
    1565,     0,     0,     0,  -257,  1347,   547,   307,    85,   478,
       0,     0,     0,   398,     0,   237,    87,    88,   239,   240,
     241,   242,     0,     0,     0,   307,     0,     0,     0,     0,
    -196,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   456,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1881,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   296,     0,  1547,     0,     0,     0,   456,     0,
     597,     0,     0,     0,     0,     0,     0,     0,     0,   680,
       0,  1630,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1645,   215,     0,     0,   216,     0,   217,   218,     0,   219,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1653,
    1654,     0,     0,   296,  1320,     0,   221,  1322,     0,  1323,
       0,     0,  1324,  1325,  1326,  1327,  1328,  1329,  1330,  1331,
    1332,  1333,  1334,  1335,  -355,  -355,  1336,  1337,  1338,  1339,
    1340,  1341,  1342,     0,  1343,     0,   222,   223,     0,   674,
     225,  1344,  1345,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,  1346,   296,   229,   230,   231,     0,
     232,   233,     0,     0,     0,     0,   924,     0,    81,     0,
       0,     0,     0,     0,     0,   296,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   680,     0,     0,   487,     0,
    1347,     0,     0,    85,   478,     0,     0,     0,   398,     0,
     237,    87,    88,   239,   240,   241,   242,     0,     0,     0,
       0,     0,     0,     0,     0,  -196,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   296,     0,     0,     0,
       0,  1786,     0,     0,  1788,     0,   429,     0,     0,     0,
       0,   307,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1630,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1808,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   487,     0,   543,     0,     0,     0,  1831,     0,
     456,    14,    15,    16,    17,    18,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   296,   296,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   215,     0,     0,   216,     0,   217,   218,
       0,   219,     0,     0,     0,     0,     0,     0,     0,  1878,
       0,     0,     0,     0,     0,     0,     0,    64,   221,     0,
       0,     0,     0,   296,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   296,     0,   680,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,    80,   229,   230,
     231,     0,   232,   233,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,   543,
       0,     0,     0,     0,     0,  1720,  1721,  1722,  1723,     0,
       0,     0,   234,  1937,     0,    85,   478,     0,     0,     0,
       0,     0,   237,    87,    88,   239,   240,   241,   242,     0,
       0,     0,     0,     0,   543,   296,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,   195,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,    19,   307,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,   351,   352,     0,   353,
      52,     0,    53,   296,   296,   354,  1105,     0,    55,    56,
      57,    58,    59,    60,    61,   924,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -502,  -502,
       0,  -502,    52,     0,    53,     0,     0,  -502,     0,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,   543,   543,   543,
       0,     0,   543,   543,     0,     0,     0,     0,     0,   672,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -478,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,   307,     0,     0,
       0,     0,  -478,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1302,     0,
       0,     0,    85,    86,     0,     0,     0,     0,     0,     0,
      87,    88,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   543,     0,     0,     0,     0,     0,     0,     0,
       4,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1319,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,   307,   215,     0,    52,   216,    53,   217,   218,     0,
     219,    54,    55,    56,    57,    58,    59,    60,    61,    62,
       0,     0,     0,    63,     0,  1320,    64,  1321,  1322,     0,
    1323,   369,     0,  1324,  1325,  1326,  1327,  1328,  1329,  1330,
    1331,  1332,  1333,  1334,  1335,  -355,  -355,  1336,  1337,  1338,
    1339,  1340,  1341,  1342,     0,  1343,     0,   222,   223,    67,
     674,   225,  1344,  1345,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,  1346,    80,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -3,  1347,     0,     0,    85,  1348,     0,     0,     0,   398,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,     0,  -196,     0,     0,     0,
       0,     0,     0,     4,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,  1319,     0,
      20,   369,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,   543,   215,     0,    52,   216,    53,
     217,   218,     0,   219,    54,    55,    56,    57,    58,    59,
      60,    61,    62,     0,     0,     0,    63,     0,  1320,    64,
    1321,  1322,     0,  1323,     0,     0,  1324,  1325,  1326,  1327,
    1328,  1329,  1330,  1331,  1332,  1333,  1334,  1335,  -355,  -355,
    1336,  1337,  1338,  1339,  1340,  1341,  1342,     0,  1343,     0,
     222,   223,    67,   674,   225,  1344,  1345,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,  1346,    80,
     229,   230,   231,   543,   232,   233,   672,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1347,     0,     0,    85,  1348,     0,
       0,     0,   398,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,     0,  -196,
       4,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   215,     0,    52,   216,    53,   217,   218,     0,
     219,    54,    55,    56,    57,    58,    59,    60,    61,    62,
       0,     0,     0,    63,     0,     0,    64,   221,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   222,   223,    67,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,    80,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1720,  1721,  1722,  1723,     0,     0,
       0,   234,  1724,  1725,    85,  1348,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,     0,  1726,     4,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   215,
       0,    52,   216,    53,   217,   218,     0,   219,    54,    55,
      56,    57,    58,    59,    60,    61,    62,     0,     0,     0,
      63,     0,     0,    64,   221,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   222,   223,    67,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,    80,   229,   230,   231,     0,   232,   233,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1720,  1721,  1722,  1723,     0,     0,     0,   234,  1724,
       0,    85,  1348,     0,     0,     0,     0,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,     0,  1726,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,    62,     0,     0,     0,    63,     0,     0,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    66,
       0,     0,     0,    67,    68,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,    79,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    85,    86,
       0,     0,     0,     0,     0,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    89,     0,
      90,   356,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -503,  -503,     0,  -503,    52,     0,    53,     0,     0,
    -503,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   159,     0,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    83,    84,     0,    85,   357,     0,     0,     0,
    -838,     0,     0,    87,    88,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    89,   356,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,  -503,  -503,     0,  -503,
      52,     0,    53,     0,     0,  -503,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   159,     0,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    83,    84,     0,
      85,   357,     0,     0,     0,     0,     0,     0,    87,    88,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      89,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     159,     0,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,   774,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   957,    84,  -703,    85,   623,     0,     0,     0,     0,
       0,     0,    87,    88,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    89,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -503,  -503,     0,  -503,    52,     0,
      53,     0,     0,  -503,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    85,   357,
       0,     0,     0,  -842,     0,     0,    87,    88,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    89,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -503,  -503,
       0,  -503,    52,     0,    53,     0,     0,  -503,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   159,     0,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    83,
      84,     0,    85,   357,     0,     0,     0,     0,     0,     0,
      87,    88,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    89,     4,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   215,     0,    52,   216,    53,
     217,   218,     0,   219,    54,    55,    56,    57,    58,    59,
      60,    61,    62,     0,     0,     0,    63,     0,     0,    64,
     221,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     222,   223,    67,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,    80,
     229,   230,   231,     0,   232,   233,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   234,     0,  1718,    85,  1348,     0,
       0,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,     4,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   215,     0,    52,   216,    53,   217,   218,
       0,   219,    54,    55,    56,    57,    58,    59,    60,    61,
      62,     0,     0,     0,    63,     0,     0,    64,   221,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   222,   223,
      67,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,    80,   229,   230,
     231,     0,   232,   233,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   234,     0,     0,    85,  1348,     0,     0,     0,
       0,     0,   237,    87,    88,   239,   240,   241,   242,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     215,     0,    52,   216,    53,   217,   218,     0,   219,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,   221,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,    80,   229,   230,   231,     0,   232,
     233,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   234,
     544,     0,    85,   235,   545,   546,     0,     0,     0,   237,
     238,    88,   239,   240,   241,   242,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   215,     0,    52,
     216,    53,   217,   218,     0,   219,   220,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   221,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,    80,   229,   230,   231,     0,   232,   233,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   234,   544,     0,    85,
     235,   669,   546,     0,     0,     0,   237,   238,    88,   239,
     240,   241,   242,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   215,     0,    52,   216,    53,   217,
     218,     0,   219,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   221,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,    80,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,   544,     0,    85,   588,   879,   546,
       0,     0,     0,   237,   238,    88,   239,   240,   241,   242,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   215,     0,    52,   216,    53,   217,   218,     0,   219,
     220,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,   221,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,    80,   229,   230,   231,     0,
     232,   233,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     234,   544,     0,    85,   235,   817,   546,     0,     0,     0,
     237,   238,    88,   239,   240,   241,   242,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   215,     0,
      52,   216,    53,   217,   218,     0,   219,   220,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   221,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,    80,   229,   230,   231,     0,   232,   233,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   234,   544,     0,
      85,   235,   236,   546,     0,     0,     0,   237,   238,    88,
     239,   240,   241,   242,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,   215,     0,    52,   216,    53,
     217,   218,     0,   219,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
     221,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     222,   223,     0,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,    80,
     229,   230,   231,     0,   232,   233,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   234,   544,     0,    85,   588,     0,
     546,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   215,     0,    52,   216,    53,   217,   218,     0,
     219,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,   221,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,    80,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   234,     0,     0,    85,   235,   236,     0,     0,     0,
       0,   237,   238,    88,   239,   240,   241,   242,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,   215,
       0,    52,   216,    53,   217,   218,     0,   219,   220,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,   221,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,    80,   229,   230,   231,     0,   232,   233,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   234,     0,
       0,    85,   235,   669,     0,     0,     0,     0,   237,   238,
      88,   239,   240,   241,   242,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   215,     0,    52,   216,
      53,   217,   218,     0,   219,   220,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   221,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
      80,   229,   230,   231,     0,   232,   233,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   234,     0,     0,    85,   235,
     817,     0,     0,     0,     0,   237,   238,    88,   239,   240,
     241,   242,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,   215,     0,    52,   216,    53,   217,   218,
       0,   219,   220,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,   221,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,    80,   229,   230,
     231,     0,   232,   233,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   234,     0,     0,    85,   235,   545,     0,     0,
       0,     0,   237,   238,    88,   239,   240,   241,   242,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     215,     0,    52,   216,    53,   217,   218,     0,   219,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,   221,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,    80,   229,   230,   231,     0,   232,
     233,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   234,
       0,     0,    85,   588,   879,     0,     0,     0,     0,   237,
     238,    88,   239,   240,   241,   242,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,   215,     0,    52,
     216,    53,   217,   218,     0,   219,   220,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,   221,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,    80,   229,   230,   231,     0,   232,   233,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   234,     0,     0,    85,
     901,     0,     0,     0,     0,     0,   237,   902,    88,   239,
     240,   241,   242,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   215,     0,    52,   216,    53,   217,
     218,     0,   219,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   221,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,    80,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   478,     0,     0,
       0,     0,     0,   237,    87,    88,   239,   240,   241,   242,
    1992,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,     0,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,  2022,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,     0,    -2,     0,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,
       0,    -2,     0,     0,    -2,     0,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
    1105,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
      -2,    -2,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,  -502,  -502,     0,  -502,    52,     0,    53,     0,
       0,  -502,     0,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1388,     0,  1105,     0,    85,    86,     0,     0,
       0,     0,     0,     0,    87,    88,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,  -502,  -502,     0,  -502,
      52,     0,    53,     0,     0,  -502,     0,   220,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1516,     0,  1105,     0,
      85,    86,     0,     0,     0,     0,     0,     0,    87,    88,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -502,  -502,     0,  -502,    52,     0,    53,     0,     0,  -502,
       0,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1612,     0,  1105,     0,    85,    86,     0,     0,     0,     0,
       0,     0,    87,    88,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -502,  -502,     0,  -502,    52,     0,
      53,     0,     0,  -502,     0,   220,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1783,     0,  1105,     0,    85,    86,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -502,  -502,
       0,  -502,    52,     0,    53,     0,     0,  -502,     0,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,    86,     0,     0,     0,     0,     0,     0,
      87,    88,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   159,     0,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    83,    84,     0,    85,    86,     0,     0,
       0,  -840,     0,     0,    87,    88,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,    89,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   159,     0,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    83,
      84,     0,    85,   299,     0,     0,     0,     0,     0,     0,
      87,    88,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,    89,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    85,    86,
       0,     0,     0,     0,     0,     0,    87,    88,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    89,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    -503,  -503,     0,  -503,    52,     0,    53,     0,     0,  -503,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     159,     0,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    83,    84,     0,    85,   646,     0,     0,     0,     0,
       0,     0,    87,    88,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    89,     4,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,    62,     0,     0,     0,    63,     0,
       0,    64,     0,     0,     0,     0,  -422,  -422,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    67,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -422,     0,     0,     0,    85,
      86,     0,     0,     0,     0,     0,     0,    87,    88,     4,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,    62,     0,
       0,     0,    63,     0,     0,    64,     0,     0,     0,     0,
    -423,  -423,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    67,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -423,
       0,     0,     0,    85,    86,     0,  1523,     0,  1524,     0,
       0,    87,    88,  1525,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,  1526,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    67,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1527,     0,     0,     0,
      85,   867,     0,  1523,     0,  1524,     0,     0,    87,    88,
    1525,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,    54,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
    1526,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    67,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1674,     0,     0,     0,    85,   867,     0,
    1523,     0,  1524,     0,     0,    87,    88,  1525,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,  1526,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    67,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1675,     0,     0,     0,    85,   867,     0,  1523,     0,  1524,
       0,     0,    87,    88,  1525,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,  1526,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    67,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1676,     0,     0,
       0,    85,   867,     0,     0,     0,     0,     0,     0,    87,
      88,   356,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -503,  -503,     0,  -503,    52,     0,    53,     0,     0,
    -503,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   356,     0,
       0,     0,     0,     0,     0,    85,   357,     0,    14,    15,
      16,    17,    18,    87,    88,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -503,  -503,
       0,  -503,    52,     0,    53,     0,     0,  -503,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,   646,     0,     0,     0,     0,     0,     0,
      87,    88,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,   220,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,   774,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   818,     0,  -703,    85,   623,     0,     0,     0,
       0,     0,     0,    87,    88,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,   220,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,   774,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   840,     0,  -703,    85,   714,
       0,     0,     0,     0,     0,     0,    87,    88,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   220,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,  1187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -711,    85,   740,     0,     0,     0,     0,     0,     0,    87,
      88,   195,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   446,    85,   447,     0,     0,     0,     0,
       0,     0,    87,    88,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   740,   741,
       0,     0,     0,     0,     0,    87,    88,   195,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,   220,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,  1632,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   740,     0,     0,     0,     0,     0,     0,    87,    88,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     220,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,  1634,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,   740,     0,     0,     0,     0,     0,
       0,    87,    88,   195,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,   220,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,   646,     0,     0,
       0,     0,     0,     0,    87,    88,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   220,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
     740,     0,     0,     0,     0,     0,     0,    87,    88,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,   447,     0,     0,     0,     0,     0,     0,
      87,    88,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -503,  -503,     0,  -503,    52,     0,    53,     0,     0,
    -503,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,  1547,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   215,     0,     0,   216,     0,   217,   218,     0,
     219,     0,     0,     0,     0,    85,   357,     0,     0,     0,
       0,     0,     0,    87,    88,  1320,     0,   221,  1322,     0,
    1323,  1922,  1923,  1324,  1325,  1326,  1327,  1328,  1329,  1330,
    1331,  1332,  1333,  1334,  1335,     0,     0,  1336,  1337,  1338,
    1339,  1340,  1341,  1342,     0,  1343,     0,   222,   223,     0,
     674,   225,  1344,  1345,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,  1346,     0,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,     0,  1547,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1347,     0,     0,    85,   478,     0,     0,     0,   398,
       0,   237,    87,    88,   239,   240,   241,   242,     0,   215,
       0,     0,   216,     0,   217,   218,  -196,   219,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1320,     0,   221,  1322,     0,  1323,     0,     0,
    1324,  1325,  1326,  1327,  1328,  1329,  1330,  1331,  1332,  1333,
    1334,  1335,     0,     0,  1336,  1337,  1338,  1339,  1340,  1341,
    1342,     0,  1343,     0,   222,   223,     0,   674,   225,  1344,
    1345,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,  1346,     0,   229,   230,   231,     0,   232,   233,
       0,     0,     0,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1347,     0,
       0,    85,   478,     0,     0,     0,   398,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,     0,  -196,   402,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -426,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,     0,  -426,   402,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -427,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,     0,  -427,   402,   195,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,     0,  -426,    14,    15,    16,    17,    18,    19,
     530,    20,   531,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   215,     0,    52,   216,
      53,   217,   218,     0,   219,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   221,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   532,     0,     0,     0,     0,  1335,     0,
    -355,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
      80,   229,   230,   231,     0,   232,   233,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1347,     0,     0,    85,   533,
       0,     0,     0,   398,     0,   237,    87,    88,   534,   535,
     241,   242,    14,    15,    16,    17,    18,    19,   530,    20,
     531,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   215,     0,    52,   216,    53,   217,
     218,     0,   219,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   221,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   532,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,    80,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   533,     0,     0,
       0,   398,     0,   237,    87,    88,   534,   535,   241,   242,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   215,     0,    52,   216,    53,   217,   218,     0,
     219,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,   221,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,    80,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   234,     0,   484,    85,   485,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     215,     0,    52,   216,    53,   217,   218,     0,   219,    54,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,   221,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,    80,   229,   230,   231,     0,   232,
     233,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   234,
       0,     0,    85,   485,     0,     0,     0,   398,     0,   237,
      87,    88,   239,   240,   241,   242,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,   215,     0,
      52,   216,    53,   217,   218,     0,   219,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,   221,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,    80,   229,   230,   231,     0,   232,   233,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   234,     0,     0,
      85,   533,     0,     0,     0,   398,     0,   237,    87,    88,
     239,   240,   241,   242,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,   215,     0,    52,   216,
      53,   217,   218,     0,   219,   220,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,   221,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
      80,   229,   230,   231,     0,   232,   233,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   234,     0,     0,    85,   588,
       0,     0,     0,     0,     0,   237,    87,    88,   239,   240,
     241,   242,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,   215,     0,    52,   216,    53,   217,
     218,     0,   219,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,   221,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,    80,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   485,     0,     0,
       0,     0,     0,   237,    87,    88,   239,   240,   241,   242,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,   215,     0,    52,   216,    53,   217,   218,     0,
     219,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,   221,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,    80,   229,   230,   231,
       0,   232,   233,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   234,     0,     0,    85,   478,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   521,     0,   522,
     523,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,   356,   195,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,   -17,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -503,  -503,     0,
    -503,    52,     0,    53,     0,     0,  -503,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    82,     0,
       0,     0,     0,     0,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,    85,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   159,     0,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    82,   774,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -703,    85,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   159,     0,
     550,    70,    71,    72,    73,    74,    75,    76,    77,    78,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   895,
       0,     0,    85,   551,     0,     0,     0,     0,     0,     0,
      87,    88,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   159,     0,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,    86,     0,     0,
       0,     0,     0,     0,    87,    88,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,    54,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   159,     0,   550,    70,
      71,    72,    73,    74,    75,    76,    77,    78,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   551,     0,     0,     0,     0,     0,     0,    87,    88,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     220,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,   774,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -703,    85,   195,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    82,  1313,     0,     0,     0,     0,
     195,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,    85,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     220,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,    85,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   875,    85,
     867,     0,     0,     0,     0,     0,     0,    87,    88,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     895,     0,     0,    85,   551,     0,     0,     0,     0,     0,
       0,    87,    88,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   895,     0,     0,    85,   551,     0,
       0,     0,     0,     0,     0,    87,    88,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,  1422,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,   867,     0,     0,     0,     0,     0,     0,    87,
      88,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,   409,     0,     0,     0,
       0,     0,     0,    87,    88,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,    54,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
     299,     0,     0,     0,     0,     0,     0,    87,    88,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
     220,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    81,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,   409,     0,     0,     0,     0,     0,
       0,    87,    88,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,   551,     0,
       0,     0,     0,     0,     0,    87,    88,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,   220,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,   447,     0,     0,     0,     0,     0,     0,    87,
      88,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,    54,    55,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,   867,     0,     0,     0,
       0,     0,     0,    87,    88,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,    52,
       0,    53,     0,     0,     0,     0,   220,    55,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    81,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
     646,     0,     0,     0,     0,     0,     0,    87,    88,   195,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,  -503,  -503,
       0,  -503,    52,     0,    53,     0,     0,  -503,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,    85,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,    52,     0,
      53,     0,     0,     0,     0,    54,    55,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   551,
       0,     0,     0,     0,     0,     0,    87,    88,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,    52,     0,    53,     0,     0,     0,     0,   220,
      55,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    80,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    81,     0,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,   426,     0,     0,     0,     0,     0,     0,
      87,    88,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,    52,     0,    53,     0,
       0,     0,     0,    54,    55,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    81,     0,    82,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,    86,     0,     0,
       0,     0,     0,     0,    87,    88,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
      52,     0,    53,     0,     0,     0,     0,   220,    55,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    81,     0,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,   867,     0,     0,     0,     0,     0,     0,    87,    88,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,    52,     0,    53,     0,     0,     0,
       0,   220,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,    82,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,     0,     0,    14,    15,    16,
      17,    18,    87,    88,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,  -503,  -503,     0,
    -503,    52,     0,    53,     0,     0,  -503,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,   426,     0,    14,    15,    16,    17,    18,    87,
      88,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -503,  -503,     0,  -503,    52,     0,
      53,     0,     0,  -503,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,   646,
       0,    14,    15,    16,    17,    18,    87,    88,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,  -503,  -503,     0,  -503,    52,     0,    53,     0,     0,
    -503,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      81,     0,    82,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,     0,     0,     0,     0,
       0,     0,     0,    87,    88,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
     215,     0,    52,   216,    53,   217,   218,     0,   219,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,     0,   229,   230,   231,     0,   232,
     233,     0,     0,     0,     0,     0,     0,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   234,
       0,     0,    85,   478,  1056,     0,     0,     0,     0,   237,
     238,    88,   239,   240,   241,   242,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,   215,     0,    52,   216,    53,   217,   218,     0,   219,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,     0,   229,   230,   231,     0,
     232,   233,     0,     0,     0,     0,     0,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     234,     0,     0,    85,   478,     0,     0,     0,     0,     0,
     237,    87,    88,   239,   240,   241,   242,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,    52,     0,    53,     0,     0,     0,     0,    54,    55,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   159,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    82,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,    85,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,    52,     0,    53,
       0,     0,     0,     0,   220,    55,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    82,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,    85,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,    52,     0,    53,     0,     0,     0,     0,
      54,    55,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,    20,    85,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,  -503,  -503,     0,  -503,    52,
       0,    53,     0,     0,  -503,     0,   195,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    64,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,    69,    70,    52,
       0,    53,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,     0,    82,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
       0,     0,     0,     0,     0,   196,     0,   197,   198,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,   195,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    82,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,    52,     0,    53,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   521,     0,   522,   523,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,    82,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,  -502,  -502,     0,  -502,    52,     0,
      53,     0,     0,  -502,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      64,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,  -503,  -503,     0,  -503,    52,     0,    53,     0,
       0,  -503,     0,     0,     0,     0,     0,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,    82,   215,     0,     0,   216,
       0,   217,   218,     0,   219,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   222,   223,    82,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
       0,   229,   230,   231,     0,   232,   233,     0,     0,   215,
       0,     0,   216,    81,   217,   218,     0,   219,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1720,  1721,
    1722,  1723,     0,     0,   221,   234,  1864,     0,    85,   478,
       0,     0,     0,     0,     0,   237,    87,    88,   239,   240,
     241,   242,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,     0,   229,   230,   231,   215,   232,   233,
     216,     0,   217,   218,     0,   219,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,     0,     0,     0,     0,     0,   234,   544,
       0,    85,   478,     0,   546,     0,     0,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,     0,   229,   230,   231,   215,   232,   233,   216,     0,
     217,   218,     0,   219,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,     0,     0,     0,     0,     0,   234,  1180,     0,    85,
     478,     0,     0,     0,  1181,     0,   237,    87,    88,   239,
     240,   241,   242,     0,     0,     0,     0,     0,     0,     0,
     222,   223,     0,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,     0,
     229,   230,   231,   215,   232,   233,   216,     0,   217,   218,
       0,   219,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,     0,
       0,     0,     0,     0,   234,  1183,     0,    85,   478,  1193,
       0,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,     0,   229,   230,
     231,   215,   232,   233,   216,     0,   217,   218,     0,   219,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,     0,     0,     0,
       0,     0,   234,     0,     0,    85,   478,     0,     0,     0,
     677,     0,   237,    87,    88,   239,   240,   241,   242,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,     0,   229,   230,   231,   215,
     232,   233,   216,     0,   217,   218,     0,   219,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,     0,     0,     0,     0,     0,
     234,     0,     0,    85,   478,     0,     0,     0,   398,     0,
     237,    87,    88,   239,   240,   241,   242,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,     0,   229,   230,   231,   215,   232,   233,
     216,     0,   217,   218,     0,   219,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,     0,     0,     0,     0,     0,   234,     0,
       0,    85,   478,     0,     0,   954,     0,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,     0,   229,   230,   231,   215,   232,   233,   216,     0,
     217,   218,     0,   219,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,     0,     0,     0,     0,     0,   234,     0,     0,    85,
     478,   967,     0,     0,     0,     0,   237,   238,    88,   239,
     240,   241,   242,     0,     0,     0,     0,     0,     0,     0,
     222,   223,     0,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,     0,
     229,   230,   231,   215,   232,   233,   216,     0,   217,   218,
       0,   219,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,     0,
       0,     0,     0,     0,   234,     0,     0,    85,   478,   991,
       0,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,     0,   229,   230,
     231,   215,   232,   233,   216,     0,   217,   218,     0,   219,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,     0,     0,     0,
       0,     0,   234,  1183,     0,    85,   478,     0,     0,     0,
       0,     0,   237,    87,    88,   239,   240,   241,   242,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,     0,   229,   230,   231,   215,
     232,   233,   216,     0,   217,   218,     0,   219,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   221,     0,     0,     0,     0,     0,
     234,     0,     0,    85,   478,     0,     0,     0,  1557,     0,
     237,    87,    88,   239,   240,   241,   242,     0,     0,     0,
       0,     0,     0,     0,   222,   223,     0,   224,   225,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   226,
     227,   228,     0,     0,   229,   230,   231,   215,   232,   233,
     216,     0,   217,   218,     0,   219,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   221,     0,     0,     0,     0,     0,   234,  1629,
       0,    85,   478,     0,     0,     0,     0,     0,   237,    87,
      88,   239,   240,   241,   242,     0,     0,     0,     0,     0,
       0,     0,   222,   223,     0,   224,   225,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,   226,   227,   228,
       0,     0,   229,   230,   231,   215,   232,   233,   216,     0,
     217,   218,     0,   219,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     221,     0,     0,     0,     0,     0,   234,     0,     0,    85,
     478,     0,     0,     0,  1780,     0,   237,    87,    88,   239,
     240,   241,   242,     0,     0,     0,     0,     0,     0,     0,
     222,   223,     0,   224,   225,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   226,   227,   228,     0,     0,
     229,   230,   231,   215,   232,   233,   216,     0,   217,   218,
       0,   219,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   221,     0,
       0,     0,     0,     0,   234,     0,  1928,    85,   478,     0,
       0,     0,     0,     0,   237,    87,    88,   239,   240,   241,
     242,     0,     0,     0,     0,     0,     0,     0,   222,   223,
       0,   224,   225,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,   226,   227,   228,     0,     0,   229,   230,
     231,   215,   232,   233,   216,     0,   217,   218,     0,   219,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   221,     0,     0,     0,
       0,     0,   234,  1933,     0,    85,   478,     0,     0,     0,
       0,     0,   237,    87,    88,   239,   240,   241,   242,     0,
       0,     0,     0,     0,     0,     0,   222,   223,     0,   224,
     225,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,   226,   227,   228,     0,     0,   229,   230,   231,     0,
     232,   233,     0,     0,   215,     0,     0,   216,    81,   217,
     218,     0,   219,  2009,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   221,
     234,  1943,     0,    85,   478,     0,     0,     0,     0,     0,
     237,    87,    88,   239,   240,   241,   242,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,     0,   229,
     230,   231,   215,   232,   233,   216,     0,   217,   218,     0,
     219,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   221,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   478,     0,     0,
       0,     0,     0,   237,    87,    88,   239,   240,   241,   242,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,     0,   229,   230,   231,
     215,   232,   233,   216,     0,   217,   218,     0,   219,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,   234,  2016,     0,    85,   478,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,     0,   229,   230,   231,   215,   232,
     233,   216,     0,   217,   218,     0,   219,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,     0,     0,     0,     0,     0,   234,
    2018,     0,    85,   478,     0,     0,     0,     0,     0,   237,
      87,    88,   239,   240,   241,   242,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,     0,   229,   230,   231,   215,   232,   233,   216,
       0,   217,   218,     0,   219,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,     0,     0,     0,     0,     0,   234,  2065,     0,
      85,   478,     0,     0,     0,     0,     0,   237,    87,    88,
     239,   240,   241,   242,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
       0,   229,   230,   231,   215,   232,   233,   216,     0,   217,
     218,     0,   219,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   221,
       0,     0,     0,     0,     0,   234,  2067,     0,    85,   478,
       0,     0,     0,     0,     0,   237,    87,    88,   239,   240,
     241,   242,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,     0,   229,
     230,   231,   215,   232,   233,   216,     0,   217,   218,     0,
     219,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   221,     0,     0,
       0,     0,     0,   234,  2069,     0,    85,   478,     0,     0,
       0,     0,     0,   237,    87,    88,   239,   240,   241,   242,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,     0,   229,   230,   231,
     215,   232,   233,   216,     0,   217,   218,     0,   219,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,   234,  2074,     0,    85,   478,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,     0,   229,   230,   231,   215,   232,
     233,   216,     0,   217,   218,     0,   219,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,     0,     0,     0,     0,     0,   234,
    2076,     0,    85,   478,     0,     0,     0,     0,     0,   237,
      87,    88,   239,   240,   241,   242,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,     0,   229,   230,   231,   215,   232,   233,   216,
       0,   217,   218,     0,   219,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,     0,     0,     0,     0,     0,   234,  2112,     0,
      85,   478,     0,     0,     0,     0,     0,   237,    87,    88,
     239,   240,   241,   242,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
       0,   229,   230,   231,   215,   232,   233,   216,     0,   217,
     218,     0,   219,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   221,
       0,     0,     0,     0,     0,   234,  2114,     0,    85,   478,
       0,     0,     0,     0,     0,   237,    87,    88,   239,   240,
     241,   242,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,     0,   229,
     230,   231,   215,   232,   233,   216,     0,   217,   218,     0,
     219,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   221,     0,     0,
       0,     0,     0,   234,  2116,     0,    85,   478,     0,     0,
       0,     0,     0,   237,    87,    88,   239,   240,   241,   242,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,     0,   229,   230,   231,
     215,   232,   233,   216,     0,   217,   218,     0,   219,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,   234,  2135,     0,    85,   478,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,     0,   229,   230,   231,   215,   232,
     233,   216,     0,   217,   218,     0,   219,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,     0,     0,     0,     0,     0,   234,
    2137,     0,    85,   478,     0,     0,     0,     0,     0,   237,
      87,    88,   239,   240,   241,   242,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,     0,   229,   230,   231,   215,   232,   233,   216,
       0,   217,   218,     0,   219,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,     0,     0,     0,     0,     0,   234,  2139,     0,
      85,   478,     0,     0,     0,     0,     0,   237,    87,    88,
     239,   240,   241,   242,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
       0,   229,   230,   231,   215,   232,   233,   216,     0,   217,
     218,     0,   219,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   221,
       0,     0,     0,     0,     0,   234,     0,     0,    85,   478,
       0,     0,     0,     0,     0,   237,    87,    88,   239,   240,
     241,   242,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,     0,   229,
     230,   231,   215,   232,   233,   216,     0,   217,   218,     0,
     219,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   221,     0,     0,
       0,     0,     0,   509,     0,     0,    85,   478,     0,     0,
       0,     0,     0,   237,    87,    88,   239,   240,   241,   242,
       0,     0,     0,     0,     0,     0,     0,   222,   223,     0,
     224,   225,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,   226,   227,   228,     0,     0,   229,   230,   231,
     215,   232,   233,   216,     0,   217,   218,     0,   219,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,   512,     0,     0,    85,   478,     0,     0,     0,     0,
       0,   237,    87,    88,   239,   240,   241,   242,     0,     0,
       0,     0,     0,     0,     0,   222,   223,     0,   224,   225,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
     226,   227,   228,     0,     0,   229,   230,   231,   215,   232,
     233,   216,     0,   217,   218,     0,   219,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,     0,     0,     0,     0,     0,   518,
       0,     0,    85,   478,     0,     0,     0,     0,     0,   237,
      87,    88,   239,   240,   241,   242,     0,     0,     0,     0,
       0,     0,     0,   222,   223,     0,   224,   225,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   226,   227,
     228,     0,     0,   229,   230,   231,   215,   232,   233,   216,
       0,   217,   218,     0,   219,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   221,     0,     0,     0,     0,     0,   527,     0,     0,
      85,   478,     0,     0,     0,     0,     0,   237,    87,    88,
     239,   240,   241,   242,     0,     0,     0,     0,     0,     0,
       0,   222,   223,     0,   224,   225,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   226,   227,   228,     0,
       0,   229,   230,   231,   215,   232,   233,   216,     0,   217,
     218,     0,   219,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   221,
       0,     0,     0,     0,     0,   234,     0,     0,    85,   478,
       0,     0,     0,     0,     0,   237,   790,    88,   239,   240,
     241,   242,     0,     0,     0,     0,     0,     0,     0,   222,
     223,     0,   224,   225,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,   226,   227,   228,     0,     0,   229,
     230,   231,     0,   232,   233,     0,     0,     0,     0,     0,
       0,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   234,     0,     0,    85,   478,     0,     0,
       0,     0,     0,   237,   238,    88,   239,   240,   241,   242
};

static const yytype_int16 yycheck[] =
{
       1,     1,   189,     4,     1,   234,   234,   571,   194,   148,
     355,   329,    83,  1117,    65,   102,   688,     1,   515,   315,
       1,   193,   626,    83,    83,  1366,   177,   179,   371,    83,
     537,   152,   139,  1123,    83,  1396,  1397,   626,   622,   926,
     107,  1131,    85,   677,   324,   626,    83,  1078,  1347,     4,
     194,   622,   329,     3,   329,   657,   291,   659,   841,  1110,
     295,    62,    63,   682,    65,    65,     1,   622,    65,   900,
     350,   329,   622,   329,    83,   203,  1814,   622,   329,   339,
     179,    65,    83,  1814,    65,   647,   329,   329,    89,   622,
       1,   622,   237,    94,  1347,   329,  1814,   329,  1731,  1922,
     418,   102,    68,    69,  1727,   177,   107,   158,   368,   110,
      94,   998,    91,   114,   271,   236,  1082,   269,  1926,   379,
      82,    82,  1330,   107,  1011,     1,   110,     0,     4,   143,
     114,     1,   107,    79,    65,   108,  1102,    68,    69,   127,
      71,   418,    79,   418,   443,   205,   205,   172,   107,    96,
       0,   205,   153,   747,   179,   156,   205,   158,   158,   114,
     418,   158,   418,   164,   758,   338,    82,   418,   205,   468,
     171,  1094,   345,   187,   158,   418,   418,   158,   179,    96,
     168,   627,    98,   129,   418,   164,   418,   633,   107,    65,
     191,   164,   129,   164,   367,    65,   205,     1,   144,    97,
     339,   117,   203,   204,   205,   378,   172,   144,   164,   803,
     166,   628,  1300,   145,   189,   632,   820,   188,    94,  1307,
     204,   638,   168,   189,     0,   172,  1859,   189,   189,   368,
    2053,    84,    85,   164,   818,   236,   171,   734,   735,   172,
     379,  1864,  1865,   133,   114,   332,   663,   334,   180,    84,
      85,   668,  1303,   170,   341,   172,   189,     1,   329,   170,
     171,    65,   127,   330,   170,   404,   164,   268,   269,   329,
     329,   835,   148,   818,   180,   329,   152,   167,   148,  1094,
     329,   509,   158,   119,   512,   161,   514,   818,   158,  2027,
     518,   292,   329,   450,  2102,   296,  2027,  1385,   932,   527,
     164,   302,   530,   531,   532,   457,   110,   143,   415,  2027,
     545,  1401,   313,   466,  1937,  1938,   469,   318,   302,   172,
     329,    65,   323,   324,   325,  2133,   413,   414,   329,   330,
     166,     1,  1585,   830,     4,  1588,  1589,   172,  1304,   406,
     569,   903,     1,   170,   472,  1205,   330,   418,   912,   350,
     393,   848,   419,   172,   158,   164,   119,   358,   457,   360,
     236,  1144,   189,    10,   418,   464,   511,   971,   369,   370,
     189,   330,   373,   957,   358,   718,  1407,   884,   696,   380,
     143,   418,   971,   726,   118,  1216,   957,   683,  1312,   373,
     971,   548,  1312,   394,   395,    65,   397,  1016,  1321,   115,
     116,   402,   957,   158,   148,   406,    65,   957,   142,   418,
     250,   330,   957,    20,   158,   145,   161,   418,   419,   696,
       1,   696,   406,     4,   957,   180,   957,   172,   429,  1517,
     665,    82,  1355,   554,   591,   419,   437,   438,   696,   169,
     696,   442,  1114,   188,   114,   696,    97,   406,   398,   164,
     180,   291,   732,   696,   696,   295,   457,    94,   738,   645,
     419,   533,   696,   339,   696,   466,  1765,   271,   469,   339,
     107,   472,   189,   110,   626,   167,   168,   114,   168,   797,
     356,   895,   358,   173,    65,  1079,   170,   406,   158,   148,
     676,   235,   368,   650,   495,    79,   188,   757,   368,   158,
     419,   645,  2056,   379,   661,   189,  1321,   164,   659,   379,
    1129,   495,  1765,   959,  1604,   516,   163,  1121,  1726,   691,
     797,   956,   797,  1731,   170,   164,   402,   174,   404,   110,
    2084,  1892,   179,   114,   404,    82,   115,   116,   408,   797,
    1355,   797,   956,   189,   545,   129,   797,  1181,   669,   722,
    2104,   427,    99,   170,   797,   797,   170,  1444,   172,   560,
     144,   562,  1613,   797,   565,   797,  1185,   204,   569,   373,
     164,   572,   189,  1008,   646,  1194,   749,   158,   172,   996,
     164,   164,   817,   756,   168,   145,  1815,   760,   167,  1449,
    1450,  1451,   164,  1028,  1008,   339,  1520,  1521,  1522,  1218,
    1520,  1521,  1522,   407,   170,   142,     1,    79,   448,     4,
     486,   654,   168,   380,  1028,   175,   176,   173,   757,   776,
     189,   622,  1312,   189,   368,   626,     1,   164,   395,  1677,
    1971,   168,   675,     3,  1682,   379,   696,   696,   175,   176,
     170,   708,   696,   189,  1246,   172,   450,   696,   649,   170,
     180,  1859,   179,   654,   655,   138,   139,   129,   170,   696,
     404,   166,   897,   750,  1099,   302,   171,   839,   189,  1773,
      65,   621,   144,   623,   675,   170,   677,   189,   554,   170,
     339,  1910,   163,   170,  1774,  1099,   170,   696,    83,   170,
      65,   170,   164,   330,    89,   696,    10,   170,   189,    94,
     170,   184,   185,   447,   165,   545,   170,   708,   189,   368,
     189,   172,   107,   166,   801,   110,   189,   667,   171,   114,
     379,   358,  1018,   673,   708,   189,    76,   826,  1936,  1958,
    1545,   732,   799,   170,  1549,  1550,   373,   738,   170,   164,
     741,  1949,   692,   797,   548,   404,   171,   164,  1563,   708,
     907,   802,   189,     3,  1641,   172,  1643,   189,   879,   166,
     797,   168,   179,   158,  1199,  1200,   177,   170,   954,   164,
     166,   170,    79,   148,  1054,   171,   167,   180,   136,   137,
    1399,  1227,  1228,   158,   179,  1199,  1200,  1406,   797,   708,
     189,   170,   373,   669,   170,   867,   797,  1431,   799,  2028,
    2008,   802,   802,   804,   180,   802,   172,   647,   164,   204,
     205,    94,   813,  1432,   188,   799,  1420,   969,   802,   971,
     172,   802,   129,   164,  1259,   665,   186,   533,  1108,   901,
    1520,  1521,  1522,   834,  1436,  1437,  1022,   144,   170,   250,
     799,   236,   142,   793,   588,  1259,   650,   164,   180,   163,
     166,   806,   170,   127,   170,   158,   170,   164,   495,  2088,
     174,   168,   180,   164,   164,   179,   169,   170,   168,   166,
     166,   166,    79,   170,   269,   175,   176,   180,   161,   166,
     799,   757,   177,   178,   175,   166,  1231,   757,  1560,     3,
      65,   182,   183,    68,    69,    18,    71,   166,  1395,    13,
      14,    15,    16,    17,   166,  1248,   166,   302,  1081,   171,
      13,    14,    15,    16,    17,   166,   164,   918,   313,   920,
     171,   166,   129,  1557,   164,   926,   802,   164,   323,   324,
     325,   932,   802,   169,   329,   330,   127,   144,   181,  1167,
     646,    64,    65,    66,    67,    68,    69,    70,    71,   166,
     166,   657,  1109,   170,   170,   350,   957,   164,   908,   909,
    1256,   168,   176,   358,   339,    79,   806,   186,   969,   166,
     971,    79,   776,   170,  1538,   164,    79,   817,   373,   168,
      13,    14,    15,    16,    17,   789,  1605,   608,   609,   610,
     611,    53,    54,   368,    56,   174,   740,   998,   802,   394,
      62,   166,   397,   879,   379,   170,   142,   402,  1627,  1010,
    1011,   406,  1684,   757,   166,   129,   142,  1018,   170,   169,
    1021,   129,  1173,   418,   419,  1124,   129,   145,   166,   404,
     144,   164,   170,   166,  1220,   168,   144,   448,   164,   142,
     145,   144,   168,   167,   166,   164,    79,  1862,   170,   175,
     176,   166,   164,  1054,   170,   170,   164,   897,   802,    22,
     168,   164,   457,   903,  1250,   168,  1252,   157,   158,   159,
     160,   708,   175,   176,    79,   358,  1026,   164,   164,   166,
     170,   168,   115,   116,   164,   164,   166,    79,   168,  1161,
     180,  1092,  1093,  1094,  1094,  1246,   129,  1094,   757,   189,
     495,  1173,   170,   907,   164,   164,  1250,  1108,  1252,   168,
     108,   144,  1392,  1094,  1221,   164,   166,  1736,  1347,  1347,
     170,   516,   533,  1078,   129,  1232,   164,   164,   166,  1130,
     168,    79,   802,   172,   167,    96,   806,   129,   164,   144,
    1247,   164,   168,   802,   427,   168,  1780,   166,   163,   166,
     545,   170,   144,   170,  1261,  1110,   164,  1776,   172,   164,
     168,   867,  1163,   168,   166,   560,  1509,   562,   170,   166,
     565,   166,   164,   170,   164,   170,   168,   572,  1993,   189,
    1181,   129,    68,   157,   158,   159,   160,   172,     3,   895,
      79,   157,   158,   159,   160,   901,   144,  1284,    13,    14,
      15,    16,    17,   486,  1205,   172,   180,   166,  2023,  1396,
    1397,   170,   166,   166,   180,   189,   164,   170,  1094,   142,
     168,   802,   166,    79,  1094,   806,   170,   622,   179,   115,
     313,   626,  1233,   188,   120,   646,   647,   123,  1078,   125,
     129,   164,  2057,   177,   178,   168,   164,   166,  1355,   166,
     956,   170,   175,   176,   166,   144,   166,   166,   170,   654,
     655,   170,   166,   166,    79,   188,   170,    79,   166,   166,
    1110,   166,   170,   129,  1893,   164,  1383,   168,  1897,   168,
     675,   166,   677,  2089,   166,   170,  1437,  2093,   144,   166,
    1094,   140,   141,  1545,   166,     5,  1525,  1549,   166,  1398,
     170,   696,  1008,    13,    14,    15,    16,    17,   164,   601,
     602,   603,   168,   708,   129,   169,   170,   129,  1674,  1675,
    1676,  1321,  1028,  1324,  1321,   169,  1327,  1328,  1329,   144,
     169,   170,   144,  1440,  1441,  1336,   164,   732,   224,   164,
    1321,   115,   116,   738,   166,   167,   741,   188,  1303,  1299,
    1094,   172,   164,   172,  1355,  1355,   168,    18,  1355,   168,
    1361,   120,   121,   122,   123,   124,  1473,   171,  1475,    79,
     221,  1689,   175,   176,  1355,  1376,   169,   170,  1379,  1380,
     171,  1382,   757,   170,  1491,  1386,   164,   273,   274,  1390,
     276,  1392,   278,  1099,   188,  1379,  1380,  1625,   166,   250,
     251,   166,   797,  1967,   799,   166,  1545,   802,  1078,   166,
    1549,   166,  1689,   166,  1689,  1309,  1310,  1311,   166,   129,
    1696,  1697,  1698,  1609,  1094,  1380,   166,   802,   169,   170,
    1431,  1689,   166,  1689,   144,  1094,  1312,    79,  1689,   188,
    1110,   169,   170,  1444,   166,  1321,  1689,  1689,  1449,  1450,
    1451,  1321,  1407,   170,   171,  1689,   867,  1689,   172,    79,
     169,   170,   170,  1303,  1531,  1609,   172,    13,    14,    15,
      16,    17,   169,   170,   169,   170,   362,   169,   170,  1355,
     169,   170,   169,   170,   172,  1355,   172,   129,    98,    99,
     901,   172,   903,  1199,  1200,   169,   170,  1078,   169,   170,
     169,   170,   144,    13,    14,    15,    16,    17,    77,   129,
    1380,  1829,   189,  1094,   169,   170,   169,  1321,   169,   170,
     170,   171,   164,   918,   144,   920,   168,    84,    85,  1110,
    1531,   926,   169,    79,  1535,  1536,   169,   932,   164,    13,
      14,    15,    16,    17,   164,  1545,  1653,  1654,   168,  1549,
    1550,  1355,  1829,  1259,  1829,    85,  1557,   170,   171,   604,
     605,    18,   957,  1563,  1206,  1207,   172,  1407,  1523,    79,
     172,  1829,  1531,  1829,   969,  1379,   971,  1321,  1829,   170,
    1581,  1582,  1754,   129,   606,   607,  1829,  1829,   612,   613,
    1591,   189,   675,   166,   677,  1829,   166,  1829,   144,   145,
     163,  1588,  1589,   998,   170,    79,  1607,  1591,  1697,  1698,
    1677,  1355,  1531,   166,   170,  1682,  1011,    79,  1689,   129,
     170,   166,   166,  1018,  1691,   166,  1021,    79,   166,   480,
     166,   166,   166,  1303,   144,   486,    79,   166,   166,   166,
    1641,   166,  1643,   169,  1520,  1521,  1522,  1523,  1524,  1525,
    1526,  1321,   169,   169,   169,   129,   169,   169,  1613,  1054,
     166,   170,  1321,   166,   166,   166,   166,   129,   166,  1545,
     144,   145,   166,  1549,  1550,  1545,  1677,   129,  1958,  1549,
    1550,  1682,   144,   169,   169,  1355,   129,  1563,  1689,   170,
    1691,   163,   144,  1563,    22,   166,  1355,  1092,  1699,   166,
     166,   144,   164,   166,   163,  1892,   168,  1879,   166,  2027,
    1380,   166,   164,  1108,    77,   166,   168,  1718,  1677,  1094,
     166,   164,  1303,  1682,  1725,   168,   166,   166,   166,   166,
    1436,    79,  1691,   166,   166,   188,  1922,  1407,   172,   170,
    1321,  1545,  1379,  1380,    79,  1549,  1550,    79,   172,   600,
    2027,   172,  2027,   172,   172,  1756,   166,   166,  1677,  1563,
    1715,   166,  1173,  1682,   166,  1832,   166,   166,  1163,  2027,
     166,  2027,  1691,  1613,  1355,   166,  2027,   166,  1922,  1780,
     172,   129,   170,   166,  2027,  2027,  1181,  1591,   674,   170,
     170,   169,  1964,  2027,   129,  2027,   144,   129,  1379,  1380,
     166,  1545,  1959,   163,   188,  1549,  1550,   166,   166,   144,
    1205,   163,   144,   163,    14,   164,   164,   164,   164,  1563,
     168,   164,   164,   164,   164,   164,  1407,   171,  1829,   164,
     166,  1832,   164,   168,   171,   918,   168,   170,  1233,  2025,
    1841,  1842,   172,   926,   189,  1715,   163,  1848,   166,   932,
     188,   166,  1919,  1523,    13,    14,    15,    16,    17,    18,
    1861,   171,  1862,   171,   169,  2051,   169,  2053,   754,   131,
    1871,   163,  1873,  1832,   169,  1545,   166,   166,   169,  1549,
    1550,   166,   169,   166,   166,  1886,  1545,  1888,  1889,  1890,
    1549,  1550,   166,  1563,     4,     5,     6,     7,     8,     9,
      10,    11,    12,   166,  1563,  2091,   163,  2051,   163,  2053,
    1714,   164,   189,  1832,   164,   998,    87,  2089,  1919,   189,
     189,  2093,  2094,   189,   189,  1926,    99,   163,  1011,  1930,
     189,   189,   164,   189,  1935,  1018,   164,    97,  1021,   166,
     163,   172,  1523,  1613,   163,   163,  1321,  2091,   163,   166,
    2122,   189,   166,   166,  1591,   166,  2027,  1958,   170,  1960,
    1919,  2028,    72,   166,  1545,   166,   171,   169,  1549,  1550,
    2156,  2143,   169,  2027,  1924,  2147,   166,   166,   163,   166,
    1355,  1376,  1563,  1984,  1379,  1380,  1862,  2159,   166,   166,
    2027,   166,  1862,  1993,   170,  1996,  2103,  1392,   164,  2000,
    1919,     1,  1952,   166,     4,   166,   164,   164,  2009,   163,
    1591,  2118,  2156,   163,  2015,    82,   169,   169,  2027,    82,
     189,  2088,   189,  2023,   875,   164,  2027,  2028,   163,  1312,
     881,   164,  1613,   189,    79,   166,  1431,  1877,   163,   166,
    1844,   892,   166,   166,  2028,  1715,   168,    82,    82,  1444,
     180,   180,   903,   189,  1449,  1450,  1451,  2057,  1862,  2060,
      82,   189,   171,   189,   163,    65,   163,   163,   113,  2028,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     165,   189,   180,    83,   129,   180,    86,  2088,   163,   157,
     158,   159,   160,   166,    94,   171,  2097,   113,  1181,   144,
     164,  2102,   170,   180,  2088,   170,   166,   107,   180,  2028,
     110,   165,   180,   166,   114,   189,  1956,  1993,  1862,   164,
     165,   189,  1205,  1993,    82,   169,   166,  2128,   165,  2088,
    2131,   163,  2133,  1714,  1715,   171,  1531,   166,   163,   189,
     164,   189,   187,   166,   614,  1625,   189,  2023,   616,   536,
     615,  2152,   152,  2023,   618,  1213,   617,  1342,   158,  2160,
     249,   161,  1557,  1355,   164,   165,  2133,  1549,  2169,  2088,
    1545,  2053,  1870,  1563,  1549,  1550,  2084,   177,  1764,  1862,
    2042,  2057,  2024,  1747,  1747,  1956,  2023,  2057,  1563,  1993,
    2094,    55,  1862,  2147,    85,  1382,  1591,   123,  1919,   364,
    1982,    13,   202,  1862,   204,   205,   113,   144,  1403,   397,
     117,   118,   119,   120,   121,   122,   123,   124,  1526,  2023,
    1779,  1376,   113,  1018,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   234,   235,   236,  1520,  1521,  1522,
     684,   813,  1525,  1526,  1595,     0,  1641,   834,  1643,  1993,
     250,   840,   840,  2057,   840,  1512,    -1,   164,   165,   397,
      -1,    -1,    -1,  1844,    -1,    -1,    -1,    -1,    -1,   269,
      -1,   271,    13,    14,    15,    16,    17,    18,    -1,  2023,
     187,  1862,  1677,    95,    -1,    -1,    -1,  1682,    -1,    -1,
      -1,   291,    -1,    -1,  1689,   295,  1691,   234,   189,   299,
      -1,   113,   302,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,  2057,    -1,    83,    -1,    -1,    -1,  1170,
    1171,  1172,    -1,  1993,   324,    -1,    -1,  1178,  1179,   329,
     330,    -1,    -1,    -1,  1993,    -1,    79,    -1,    -1,   107,
      -1,    -1,    13,    14,    15,    16,    17,    -1,  1431,    -1,
     350,    -1,   164,  2023,    -1,    -1,    -1,   357,   358,    -1,
      -1,  1444,    -1,    -1,  2023,    -1,  1449,  1450,  1451,    -1,
     113,    -1,    -1,   373,   117,   118,   119,   120,   121,   122,
     123,   124,   319,    -1,   113,  1780,   129,  2057,   117,   118,
     119,   120,   121,   122,   123,   124,   164,    -1,  2057,   142,
      -1,   144,    -1,    -1,    -1,    -1,   406,    -1,    79,    -1,
      -1,   411,  1993,    -1,    -1,    -1,    -1,   417,   418,   419,
      -1,   164,   165,    -1,    -1,    -1,   426,   427,    -1,    -1,
      -1,    -1,   175,   176,  1829,    -1,    94,  1832,    -1,    -1,
      -1,    -1,  2023,    -1,   187,    -1,    -1,   447,   448,   449,
     450,    13,    14,    15,    16,    17,    18,    -1,   129,    13,
      14,    15,    16,    17,    18,    -1,   403,    -1,    -1,    -1,
    1321,   142,    -1,   144,  1557,    -1,  2057,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   622,   485,   486,  1862,   488,    -1,
      -1,    -1,    -1,   164,    -1,   495,    -1,   168,    -1,    -1,
      -1,   269,    -1,   161,   175,   176,    -1,    -1,    -1,   509,
      -1,    -1,   512,    -1,   514,   515,    -1,    -1,   518,    -1,
      -1,    -1,    -1,    -1,  1919,    -1,    -1,   527,    -1,    -1,
     530,   531,   532,   533,    -1,   723,    -1,   131,    -1,   133,
     134,   135,    68,    -1,   202,   545,    -1,    -1,   548,    -1,
      -1,    -1,    -1,    -1,   554,    -1,   324,    -1,  1641,   340,
    1643,   329,   330,  1958,    -1,  1960,    -1,    -1,    -1,    -1,
     164,    -1,   509,   167,   168,   512,    -1,    -1,   172,   173,
      -1,   518,   350,    -1,   110,   723,    -1,    -1,   588,    -1,
     527,   591,   250,    -1,  1877,    -1,   122,    -1,   124,   113,
     126,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   800,    -1,  2009,    -1,    -1,    -1,  1993,   556,
      -1,    -1,   622,    -1,    -1,    -1,   626,    -1,    -1,    -1,
      -1,    -1,  2027,  2028,    -1,    -1,    -1,    -1,   406,   165,
      -1,    -1,   168,   169,    -1,    -1,   646,   647,  2023,    -1,
     418,   419,   840,    -1,    -1,    -1,    -1,   657,    -1,   659,
      -1,   661,   800,    -1,    -1,   665,    -1,    -1,   856,   669,
      -1,    -1,   860,  1956,    -1,   189,    -1,    -1,    -1,    -1,
     818,    -1,  2057,    -1,    -1,    -1,    -1,    -1,   688,    -1,
      -1,    -1,    -1,  2088,    -1,    -1,   696,  1780,   224,    -1,
     358,    -1,   840,    -1,    -1,   113,    -1,   488,   708,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   856,    -1,
      -1,    -1,   860,   504,    -1,    -1,   507,    -1,    -1,    -1,
      -1,    -1,   732,    -1,   734,   735,    -1,    -1,   738,    -1,
     740,    -1,    -1,    -1,    -1,   271,    -1,    -1,    -1,    -1,
      -1,   277,    -1,   279,    -1,    13,    14,    15,    16,    17,
     168,    -1,    -1,    -1,    -1,  2160,    -1,    -1,    -1,   427,
      -1,    -1,    -1,    -1,  2169,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   721,    -1,    -1,   975,    -1,    -1,
     448,    -1,   729,    -1,   982,    -1,    -1,   797,    -1,   799,
      -1,    -1,   802,    -1,    -1,    -1,   806,    -1,    -1,    -1,
      -1,   748,    -1,    -1,    -1,    -1,    -1,   817,   818,   957,
     820,    79,   759,    -1,    -1,    -1,    -1,    -1,   486,     3,
     830,    -1,    -1,    -1,    -1,    -1,    -1,   975,    -1,    -1,
     840,   841,    -1,    -1,   982,    -1,    -1,   373,   848,   375,
     376,   509,    -1,    -1,   622,    -1,    -1,   515,   626,    -1,
     518,    -1,    -1,    -1,    -1,    -1,    -1,   867,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   879,
      -1,   407,    -1,    -1,   142,   411,   144,    -1,    -1,    -1,
      -1,   417,    -1,    -1,    -1,   895,    -1,   897,    -1,    -1,
      -1,   901,    -1,   903,    -1,    -1,   164,    -1,    -1,  1760,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    -1,
     701,    -1,    -1,    64,   450,    -1,   452,   453,   696,    -1,
      -1,    72,    73,    74,    75,    -1,    -1,    -1,    -1,   113,
     708,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,  1090,    -1,    -1,    86,   956,   957,    -1,    -1,
      -1,    -1,  1150,    -1,   732,    -1,  1154,    -1,   142,   495,
     738,   971,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,    -1,   647,
     164,   165,    -1,    -1,    -1,    -1,    -1,   171,    -1,    -1,
      -1,   175,   176,    -1,    -1,    -1,    -1,    -1,  1008,    -1,
      -1,    -1,  1150,   187,    -1,    -1,  1154,    -1,    -1,    -1,
      -1,    -1,   548,    -1,    -1,    -1,    -1,   168,  1028,   797,
     688,   799,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,   186,    -1,    -1,    68,    -1,
      -1,    -1,    -1,    -1,  1054,    -1,    -1,    -1,    -1,   840,
     841,    -1,    -1,    -1,    -1,   591,    -1,   593,   594,    -1,
     851,    -1,    -1,   854,    -1,    -1,   734,   735,  1078,    -1,
      -1,    -1,    -1,    -1,  1272,    -1,   167,    -1,  1276,    -1,
     110,    -1,  1280,    -1,  1094,    -1,    -1,    -1,   179,  1099,
      -1,    -1,   122,   123,   235,    -1,    -1,    -1,  1108,  1109,
    1110,    -1,    -1,    -1,  1114,  1966,    -1,    -1,    -1,   250,
      -1,  1121,    -1,    -1,   650,   651,  1063,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1272,   661,    -1,    -1,  1276,  1076,
      -1,   272,  1280,  1080,  1144,   165,   113,  1084,   674,    -1,
     117,   118,   119,   120,   121,   122,   123,   124,   125,    -1,
     291,    -1,    -1,   130,   295,   132,    -1,  1167,   299,    -1,
     113,    -1,   830,  1173,   117,   118,   119,   120,   121,   122,
     123,   124,   708,    -1,    -1,    -1,    -1,    -1,    -1,   957,
     848,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,  1199,
    1200,   969,    -1,   971,   224,    -1,   732,    -1,    -1,    -1,
      -1,   737,    -1,   739,    -1,    -1,    86,    -1,    -1,    -1,
      -1,    -1,   165,   113,    -1,   168,   357,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   762,    -1,   764,   765,
     130,    -1,   132,    -1,    -1,   903,  1246,    -1,    -1,    -1,
     776,   271,    -1,  1034,    -1,    -1,    -1,    -1,    -1,  1259,
    1041,    -1,    -1,   789,  1045,    -1,    -1,    -1,  1049,    -1,
      -1,    -1,  1460,   799,    -1,   165,  1464,    -1,   168,  1467,
      -1,    -1,   302,   113,    -1,   115,  1054,   117,   118,   119,
     120,   121,   122,   123,   124,   426,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1303,    -1,    -1,  1494,    -1,    -1,    -1,
    1498,    -1,  1312,    -1,  1502,    -1,   447,   448,    -1,    -1,
      -1,  1321,  1460,    -1,    -1,    -1,  1464,    -1,   113,  1467,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
    1108,    -1,    -1,    -1,    -1,    -1,    -1,  1347,  1348,    -1,
      -1,    -1,    -1,   373,   485,  1355,  1494,    -1,    -1,    -1,
    1498,    -1,    -1,  1144,  1502,   235,    -1,    -1,    -1,  1306,
       4,     5,     6,     7,     8,     9,    10,    11,    12,  1379,
    1380,   907,    -1,  1320,    -1,    -1,    -1,   407,    -1,    -1,
      -1,   411,  1392,    -1,    -1,  1395,    18,   417,    -1,    -1,
      -1,    -1,   533,  1340,   189,    -1,    -1,  1407,    -1,    -1,
    1347,    -1,    -1,    -1,   545,    -1,    -1,    -1,   549,    -1,
    1420,    -1,    -1,    -1,    -1,    -1,  1614,    -1,    -1,   299,
     450,    -1,    -1,    -1,    -1,    -1,  1436,  1437,    -1,    -1,
      -1,    -1,    -1,  1224,    -1,   971,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,  1114,   588,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1233,    -1,    -1,    -1,    -1,
      -1,   997,    -1,    -1,    -1,    -1,  1614,    -1,    -1,    -1,
      -1,    -1,  1263,    -1,    -1,  1266,    -1,   357,    -1,  1270,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   646,   647,    -1,    -1,    -1,
    1520,  1521,  1522,  1523,    -1,  1525,  1526,    -1,   548,    -1,
      -1,  1531,  1532,    -1,   665,    -1,    -1,    -1,    -1,    -1,
      13,    -1,    -1,    -1,    -1,  1545,   168,    -1,    -1,  1549,
    1550,    -1,    -1,    -1,    -1,    -1,   426,    -1,    -1,    -1,
    1560,    -1,    -1,  1563,    -1,    -1,    -1,    -1,    -1,    -1,
       1,   591,    -1,     4,    -1,    -1,    -1,   447,    -1,    -1,
      -1,    -1,    -1,  1109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1591,    -1,    -1,    -1,  1121,    -1,    -1,    -1,    -1,
      -1,    -1,  1539,  1540,    -1,    -1,    -1,    -1,    -1,   740,
      -1,    -1,    -1,  1613,    -1,   485,    -1,    -1,  1144,    -1,
      -1,    -1,    95,    -1,  1392,  1625,    -1,    -1,    -1,    -1,
     650,  1412,    -1,    -1,    65,    -1,    -1,    -1,    -1,    -1,
     113,   661,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,  1590,   674,    86,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    94,    -1,    -1,    -1,    -1,   234,    -1,
      -1,    -1,    -1,    -1,    -1,   806,    -1,  1677,  1678,   110,
      -1,    -1,  1682,   114,  1684,    -1,   817,    -1,    -1,  1689,
      -1,  1691,    -1,   113,   714,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,  1232,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1714,  1715,    -1,   148,   588,   739,
      -1,   152,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,
     161,    -1,    -1,    -1,   165,    -1,   867,  1395,    -1,    -1,
      -1,    -1,   762,    -1,    -1,   176,   177,    -1,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   776,    -1,    -1,   179,
      -1,    -1,    -1,  1531,    -1,    -1,   897,    -1,    -1,   789,
     901,   202,   903,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,   657,   113,   659,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,   142,   234,   235,   236,    -1,    -1,    -1,  1746,
      -1,    -1,    -1,    -1,  1814,  1815,    -1,    -1,    -1,   250,
      -1,    -1,    -1,    -1,   164,   165,    -1,    -1,   168,  1829,
      -1,    -1,  1832,    -1,    -1,   175,   176,    -1,    -1,    -1,
     271,   272,    -1,    -1,  1844,    -1,    -1,   187,   188,    -1,
     175,    -1,    -1,  1379,    -1,    -1,    -1,  2045,    -1,    -1,
     291,    -1,  1862,    -1,   295,    -1,    -1,    -1,   299,    -1,
     740,   302,    -1,  1324,    -1,    -1,    -1,  1877,    -1,    -1,
      -1,    -1,    -1,    -1,   113,  1336,    -1,   907,   117,   118,
     119,   120,   121,   122,   123,   124,   125,  1678,    -1,    -1,
      -1,   130,  1560,   132,    -1,    -1,    -1,  2045,   339,  1677,
    1910,    -1,    -1,    -1,  1682,    -1,    -1,    -1,    -1,  1919,
      -1,  1689,    -1,  1691,    -1,   356,   357,   358,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   165,   368,    -1,   168,
      -1,    -1,   373,   509,    -1,    -1,   512,  1078,   379,    -1,
      -1,    -1,   518,    -1,    -1,    -1,  1956,    -1,  1958,  1959,
      -1,   527,    -1,   394,    -1,    -1,   397,    -1,    -1,    -1,
      -1,   402,    -1,   404,    -1,    -1,   407,   408,    -1,  1110,
     411,    -1,  1982,    -1,    -1,    -1,   417,    -1,    -1,    -1,
     556,    -1,    -1,  1993,    -1,   426,   427,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1532,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   447,   448,    -1,   450,
      -1,    -1,    -1,  2023,    -1,   895,  1684,  2027,  2028,    -1,
      -1,    -1,    -1,  1814,  1815,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   619,   485,   486,    -1,  2057,    -1,    -1,
      -1,  1829,    -1,    -1,  1832,  1591,    -1,    -1,    -1,    -1,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   509,    -1,
      -1,   512,    -1,   514,   515,   516,   956,   518,  2088,  1109,
      -1,    -1,   164,   165,    -1,    -1,   527,    -1,    -1,   530,
     531,   532,   533,   175,   176,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   545,   187,    -1,   548,   549,    -1,
      -1,    -1,    -1,   554,    -1,    -1,    -1,    -1,    -1,  1910,
    1581,  1582,    -1,    -1,    -1,    -1,    -1,   113,  1008,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
    2087,  1919,    -1,    -1,    -1,    -1,    -1,   588,  1028,    -1,
     591,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   734,   735,
      -1,    -1,  1303,    -1,    -1,    -1,    -1,  1958,    -1,    -1,
      -1,    -1,  2119,    -1,    -1,    -1,    -1,    -1,  1714,    -1,
    1958,   622,    -1,    -1,    -1,    -1,   172,  1978,    -1,    -1,
      -1,  1982,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   646,   647,  1348,    -1,   650,
    1090,    -1,    -1,    -1,   655,    -1,   657,    -1,   659,  1099,
     661,    -1,   142,    -1,   665,    -1,    -1,    -1,   669,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2027,  2028,    -1,    -1,
      -1,    -1,    -1,    -1,   164,   165,    -1,   688,    -1,  2027,
    2028,   171,    -1,    -1,    -1,   175,   176,  1718,    -1,    -1,
      -1,    -1,  1292,    -1,  1725,    -1,  1407,   187,    -1,    -1,
      -1,    -1,    -1,   714,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   723,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,   734,   735,  1756,  1832,  2088,   739,   740,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1844,    -1,
    2088,    -1,    -1,    -1,    -1,    -1,   757,    -1,    -1,  1199,
    1200,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,   776,   113,   129,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   789,  1379,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,   800,
      -1,   802,    -1,    -1,    -1,   806,  1246,    -1,    -1,    -1,
      -1,    -1,   164,   165,  1910,    -1,   817,   818,    -1,  1259,
    1841,  1842,    -1,   175,   176,    -1,    -1,  1848,   165,   830,
      -1,   168,    -1,    -1,    -1,   187,    -1,    -1,    -1,   840,
    1861,    -1,    -1,    -1,    -1,  1941,  1942,   848,    -1,    -1,
    1871,    -1,  1873,    -1,    -1,   856,    -1,    -1,    -1,   860,
      -1,    -1,    -1,  1959,    -1,  1886,   867,  1888,  1889,  1890,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   879,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,   895,    -1,   897,    -1,    -1,    -1,
     901,    -1,   903,    -1,    -1,  1926,   907,    -1,  1348,  1930,
      -1,    -1,  1613,    -1,  1935,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,  1532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,   956,   957,    -1,    -1,    -1,
      -1,    -1,    -1,  1984,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,   165,   975,  1996,   168,    -1,    79,  2000,
      -1,   982,    -1,   175,   176,    -1,     1,    -1,    -1,     4,
      -1,    -1,  2088,    -1,  2015,   187,  1436,  1437,    -1,    -1,
      -1,  1591,    -1,    -1,    -1,    -1,    -1,  1008,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,  1028,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2060,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,   111,
      65,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,    86,    -1,    -1,   175,   176,  2097,  1078,    -1,    94,
      -1,  2102,    -1,    -1,    -1,    -1,   187,    -1,    -1,  1090,
      -1,    -1,    -1,  1094,    -1,    -1,    -1,    -1,  1099,   114,
      -1,    -1,   164,    -1,    -1,   167,   168,  2128,  1109,  1110,
    2131,    -1,  2133,  1114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1714,  1126,    -1,    -1,    -1,    -1,
      -1,  2152,    -1,   148,    -1,    -1,    -1,   152,    -1,    -1,
      -1,    79,    -1,   158,    -1,    -1,    -1,    -1,    -1,  1150,
      -1,    -1,  1288,  1154,    -1,    -1,    -1,    -1,    -1,    -1,
    1161,    -1,   177,    -1,    -1,    -1,  1167,    -1,    -1,    -1,
      -1,    -1,  1173,    -1,    -1,   113,  1877,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,   204,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,  1199,  1200,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,  1347,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     235,   236,    -1,    -1,    -1,    -1,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   250,    -1,   175,   176,    -1,
      -1,    -1,    -1,    -1,    -1,  1246,    -1,    -1,    -1,   187,
      -1,    -1,    -1,    -1,  1844,  1956,    -1,   272,  1259,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,  1272,    -1,    -1,    -1,  1276,   291,    -1,    -1,  1280,
     295,    -1,    -1,    -1,   299,    -1,    -1,    -1,    -1,    -1,
     113,  1292,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,  1303,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1312,    -1,    -1,   168,   330,    -1,    -1,    -1,    -1,
    1321,    -1,    -1,   113,   339,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   350,    -1,    -1,    -1,    -1,
     355,   356,   357,   358,   167,    -1,  1347,  1348,    -1,    -1,
      -1,    -1,   142,   368,  1355,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   379,    -1,    -1,   382,    -1,  1959,
      -1,   386,    -1,    -1,   164,   165,   391,    -1,  1379,  1380,
      -1,    -1,   397,    -1,    -1,   175,   176,   402,    -1,   404,
      -1,    -1,    -1,   408,  1395,    -1,    -1,   187,    -1,    -1,
      -1,    -1,    -1,    -1,   419,    -1,  1407,    -1,    -1,  1545,
    1546,   426,    -1,  1549,  1550,    -1,    -1,    -1,    -1,  1555,
      -1,    -1,    -1,  1559,    -1,  1561,    -1,  1563,    -1,    -1,
      -1,    -1,   447,   448,    -1,  1436,  1437,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1460,
      -1,    -1,    -1,  1464,    -1,    -1,  1467,    -1,    -1,    -1,
     485,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1494,    -1,   108,    -1,  1498,    -1,    -1,
     113,  1502,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    79,    -1,    -1,    -1,    -1,   533,  1520,
    1521,  1522,  1523,  1524,  1525,  1526,    -1,    -1,    -1,    -1,
     545,  1532,    -1,    -1,   549,    -1,    -1,    -1,    -1,   554,
      -1,    -1,    -1,    79,  1545,    -1,    -1,   113,  1549,  1550,
      -1,   117,   118,   119,   120,   121,   122,   123,   124,  1560,
      -1,    -1,  1563,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   588,  1710,    -1,   142,   113,   144,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
    1591,    -1,    -1,   129,  1730,  1731,    -1,    -1,   164,   165,
      -1,    -1,    -1,    -1,    -1,  1606,   142,   622,   144,   175,
     176,   626,  1613,  1614,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   187,    -1,    -1,  1625,  1761,    -1,    -1,   164,   165,
      -1,   646,   647,    -1,    -1,    -1,    -1,   652,    -1,   175,
     176,    -1,   657,     1,   659,    -1,    -1,    -1,    -1,    -1,
     665,   187,   113,    -1,   669,    -1,   117,   118,   119,   120,
     121,   122,   123,   124,   125,    -1,    -1,    -1,    -1,   130,
     113,   132,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   113,  1684,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,   165,    63,    -1,   168,   723,    -1,
    1846,    -1,    -1,  1714,  1715,    -1,     1,    -1,  1854,    -1,
    1856,   164,    80,  1859,  1860,   740,  1862,    -1,    -1,    -1,
      -1,  1867,    -1,    -1,    -1,    -1,   167,    -1,    -1,    -1,
      -1,    -1,   757,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      65,    -1,    -1,    -1,   142,   800,    -1,   802,    -1,    -1,
      -1,   806,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    86,   817,   818,  1940,   820,   164,    -1,    -1,   167,
     168,  1947,  1948,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,   840,   841,    -1,    -1,   114,
      -1,    -1,    -1,    -1,  1970,    -1,    -1,    -1,    -1,    -1,
      -1,   856,    -1,  1844,    -1,   860,    -1,    -1,    -1,    -1,
      -1,    -1,   867,    -1,    -1,    -1,    -1,    -1,  1859,    -1,
      -1,  1862,    -1,   148,   879,  2001,    -1,  2003,    -1,    -1,
    2006,  2007,    -1,   158,    -1,    -1,  1877,  2013,  2014,    -1,
     895,    -1,   897,    -1,    -1,    -1,   901,    -1,   903,    -1,
      -1,    -1,   177,    -1,    -1,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,   146,
     147,   148,   149,   150,   151,   152,   153,   154,   155,   156,
      -1,    -1,    -1,    -1,   161,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,  1940,
    1941,   956,   957,    -1,  2080,  2081,  2082,   164,    -1,    -1,
     235,   188,    -1,   968,    -1,  1956,    -1,    -1,  1959,    -1,
     975,    -1,    -1,    -1,    -1,   250,    -1,   982,    -1,    -1,
      -1,    -1,  2108,  2109,  2110,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   272,    -1,    -1,
      -1,    -1,  1993,  1008,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   291,    -1,  2009,    -1,
     295,    -1,    -1,  1028,   299,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,  2045,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   339,    -1,  2057,    -1,    -1,   142,
      -1,   144,    -1,  1078,    -1,    -1,    13,    14,    15,    16,
      17,    -1,   357,    -1,    -1,  1090,    -1,    -1,    -1,  1094,
      -1,   164,   165,   368,  1099,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,   379,  1110,    -1,   382,    -1,    -1,
      -1,    -1,    -1,    -1,   187,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   397,    -1,    -1,    -1,    -1,    -1,    -1,   404,
      -1,    -1,    -1,   408,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,  1150,    -1,    -1,    -1,  1154,
      -1,   426,    -1,    -1,    -1,    -1,  1161,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1173,    -1,
      -1,    -1,   447,   448,    -1,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,    -1,   129,    -1,  1199,  1200,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
     485,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,  1231,   164,   165,    -1,
      -1,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      -1,  1246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,    -1,    -1,  1259,    -1,    -1,    -1,   533,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1272,    -1,    -1,
     545,  1276,    -1,    -1,   549,  1280,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1303,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1312,    -1,    -1,
      -1,    -1,    -1,   588,    -1,   113,  1321,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1348,   142,    -1,   144,   622,    55,    -1,
    1355,    58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,    -1,
      -1,   646,   647,    80,    -1,  1380,    -1,   175,   176,    -1,
      -1,    -1,   657,    -1,   659,    -1,    -1,    -1,    -1,   187,
     665,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1407,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,
      -1,  1436,  1437,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   723,    -1,
      -1,    -1,    -1,    -1,    -1,  1460,    -1,   164,    -1,  1464,
     167,   168,  1467,    -1,     1,   740,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,   757,    -1,    -1,    -1,    -1,    -1,    -1,  1494,
      -1,    -1,    -1,  1498,    -1,    -1,    -1,  1502,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,  1520,  1521,  1522,  1523,  1524,
      -1,    -1,    -1,    -1,    -1,   800,    -1,   802,    65,   142,
      -1,   806,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1545,    -1,   817,   818,  1549,  1550,    -1,    -1,    -1,    86,
      -1,   164,   165,    -1,    -1,   168,    -1,    -1,  1563,    -1,
      -1,    -1,   175,   176,    -1,   840,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   187,    -1,    -1,   114,    -1,    -1,
      -1,   856,    -1,    -1,    -1,   860,    -1,    -1,    -1,    -1,
      -1,    -1,   867,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1613,  1614,
      -1,   148,    -1,    -1,    13,    14,    15,    16,    17,    -1,
     895,   158,   897,    -1,    -1,    -1,   901,    -1,   903,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,   956,   957,    -1,    -1,    -1,  1691,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   235,     1,
     975,    -1,     4,    -1,    -1,    -1,    -1,   982,    -1,    -1,
    1715,    -1,    -1,   250,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
     129,    -1,    -1,  1008,    -1,   272,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1028,   291,    -1,    -1,    -1,   295,    -1,
      -1,    -1,   299,    65,    -1,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,
      -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   339,  1078,    -1,    -1,    -1,    -1,    -1,    -1,
    1815,    -1,   114,    -1,    -1,  1090,    -1,    -1,    -1,  1094,
     357,   123,    -1,    -1,  1099,    -1,    -1,    -1,    -1,    -1,
      -1,   368,    -1,    -1,    -1,  1110,    -1,    -1,    -1,    -1,
      -1,    -1,   379,    -1,    -1,    -1,   148,    -1,    -1,    -1,
     152,    -1,    -1,    -1,  1859,    -1,   158,  1862,    -1,   161,
     397,    -1,    -1,    -1,    -1,    -1,    -1,   404,    -1,    -1,
      -1,   408,  1877,    -1,    -1,  1150,    -1,    -1,    -1,  1154,
      -1,    -1,    -1,    -1,    -1,    -1,  1161,    -1,    -1,   426,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1173,    -1,
     202,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     447,   448,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1199,  1200,    -1,    -1,    -1,    -1,
      -1,    -1,   234,    -1,   236,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   485,    -1,
      -1,  1956,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1259,    -1,    -1,    -1,  1993,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   533,  1272,    -1,    -1,
      -1,  1276,    -1,    -1,    -1,  1280,    -1,    -1,   545,    -1,
      -1,    -1,   549,    -1,    -1,    -1,    -1,    -1,  2023,    -1,
      -1,    -1,    -1,  2028,    -1,    -1,    -1,    -1,  1303,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   339,    -1,    -1,
    2045,    -1,    -1,    -1,    -1,    -1,  1321,    -1,    -1,    -1,
      -1,   588,  2057,    -1,   356,    -1,   358,    -1,    -1,    -1,
      -1,    -1,   364,    -1,    -1,    -1,   368,    -1,    -1,    -1,
      -1,    -1,    -1,  1348,    -1,    -1,    -1,   379,    -1,    -1,
    1355,    -1,    -1,    -1,    -1,   622,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     402,    -1,   404,    -1,    -1,  1380,   408,    -1,    -1,   646,
     647,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     657,    -1,   659,    -1,    -1,   427,    -1,    -1,   665,    -1,
      -1,    -1,  1407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1436,  1437,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   486,  1460,   723,    -1,    -1,  1464,
      -1,    -1,  1467,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   740,    -1,    -1,    -1,   509,    -1,    -1,
     512,    -1,   514,   515,    -1,    -1,   518,    -1,    -1,  1494,
     757,    -1,    -1,  1498,    -1,   527,    -1,  1502,   530,   531,
     532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,   554,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   800,    -1,   802,    -1,    -1,    -1,   806,
    1545,    -1,    -1,    -1,  1549,  1550,    -1,    -1,    -1,    -1,
     817,   818,    -1,    -1,    -1,    -1,    -1,    -1,  1563,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   840,    -1,    -1,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   856,
      -1,    -1,    -1,   860,    -1,    -1,    -1,    -1,    -1,    -1,
     867,    -1,    -1,    -1,    -1,    94,    -1,    -1,  1613,  1614,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   114,    -1,    -1,   895,    -1,
     897,    -1,    -1,    -1,   901,    -1,   903,   669,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   688,    -1,    -1,   148,
      -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,   158,
      -1,    -1,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   956,
     957,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,   734,   735,    -1,    -1,    -1,    -1,   975,    -1,
    1715,    -1,    -1,   202,    -1,   982,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   757,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1008,    -1,    -1,    -1,   234,    -1,   236,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,    -1,
      -1,  1028,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,    -1,    -1,
      -1,  1078,    -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,
      -1,    -1,    -1,  1090,    -1,    -1,    -1,  1094,    -1,    -1,
      -1,    -1,  1099,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,    -1,  1110,    -1,   152,    -1,   879,    -1,    -1,
     339,   158,    -1,    -1,   161,    -1,    -1,  1862,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   356,    -1,   358,
      -1,    -1,  1877,    -1,    -1,   364,    -1,    -1,    -1,   368,
      -1,    -1,    -1,  1150,    -1,    -1,    -1,  1154,    -1,    -1,
     379,    -1,    -1,    -1,  1161,   202,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1173,    -1,    -1,    -1,
      -1,    -1,    -1,   402,    -1,   404,    -1,    -1,    -1,   408,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   234,    -1,   236,
      -1,    -1,  1199,  1200,    -1,    -1,    -1,    -1,   427,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1956,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1246,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1993,    -1,
      -1,    -1,  1259,    -1,    -1,    -1,    -1,   486,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1272,    -1,    -1,    -1,  1276,
      -1,    -1,    -1,  1280,    -1,    -1,    -1,    -1,  2023,    -1,
     509,    -1,    -1,   512,    -1,   514,   515,    -1,    -1,   518,
      -1,    -1,   339,    -1,    -1,    -1,  1303,    -1,   527,    -1,
    2045,   530,   531,   532,    -1,    -1,    -1,    -1,    -1,   356,
      -1,   358,  2057,    -1,  1321,    -1,    -1,   364,    -1,    -1,
      -1,   368,  1094,    -1,    -1,   554,    -1,    -1,    -1,    -1,
      -1,    -1,   379,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1348,  1114,    -1,    -1,    -1,    -1,    -1,  1355,    -1,
      -1,    -1,    -1,    -1,    -1,   402,    -1,   404,    -1,    -1,
      -1,   408,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1380,    -1,    -1,    -1,    -1,    -1,    -1,
     427,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1167,    -1,    -1,    -1,    -1,
    1407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1436,
    1437,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   486,
     669,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1460,    -1,    -1,    -1,  1464,    -1,   688,
    1467,    -1,   509,    -1,    -1,   512,    -1,   514,   515,    -1,
      -1,   518,    -1,    -1,    -1,    -1,    -1,    65,    -1,    -1,
     527,    -1,    -1,   530,   531,   532,    -1,  1494,    -1,    -1,
      -1,  1498,    -1,    -1,    -1,  1502,    -1,    -1,    86,    -1,
      -1,    -1,    -1,    -1,    -1,   734,   735,   554,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,    -1,    -1,    -1,   114,    -1,   757,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1545,    -1,
    1312,    -1,  1549,  1550,    -1,    -1,    -1,    -1,    -1,  1321,
      -1,    -1,    -1,    -1,    -1,    -1,  1563,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,    -1,
     158,    -1,    -1,   802,    -1,  1347,    -1,   165,    -1,    -1,
      -1,    -1,    -1,  1355,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,   830,    -1,    -1,    -1,    -1,  1613,  1614,  1380,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   848,
      -1,    -1,   669,  1395,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   688,    -1,    -1,    -1,    -1,   132,   235,    -1,    -1,
     879,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,
      -1,   147,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   734,   735,    -1,
      -1,    -1,   178,    -1,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,  1715,    -1,
     757,   299,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   205,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   220,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1520,  1521,
    1522,  1523,  1524,  1525,  1526,   802,    -1,   153,    -1,    -1,
     156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   357,
      -1,    -1,    -1,  1545,    -1,   171,    -1,  1549,  1550,    -1,
      -1,    -1,    -1,   830,    -1,   373,    -1,    -1,  1560,    -1,
      -1,  1563,    -1,    -1,    -1,   191,    -1,   283,    -1,    -1,
      -1,   848,    -1,    -1,    -1,    -1,    -1,   203,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   879,    -1,    -1,    -1,    -1,    -1,   426,    -1,
      -1,    -1,    -1,   329,    -1,    -1,    -1,   333,    -1,    -1,
     336,   337,  1859,  1625,   340,  1862,    -1,   343,   344,   447,
     346,    -1,   348,    -1,    -1,  1094,    -1,    -1,    -1,    -1,
    1877,    -1,   268,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   292,   485,    -1,    -1,
     296,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1684,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   318,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   418,    -1,    -1,   421,    -1,    -1,  1167,    -1,
      -1,    -1,    -1,  1715,    -1,    -1,    -1,    -1,    -1,  1956,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   360,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   369,   370,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   380,    -1,  1993,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   395,
     588,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2023,    -1,   504,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   429,    -1,    -1,    -1,    -1,  2045,    -1,
      -1,   437,   438,    -1,    -1,    -1,   442,  1094,    -1,    -1,
    2057,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1114,    -1,   657,
     466,   659,    -1,   469,    -1,    -1,   472,    -1,    -1,    -1,
      -1,    -1,    -1,  1312,    -1,    -1,    -1,    -1,    -1,    -1,
    1862,    -1,  1321,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1347,    -1,
    1167,    -1,    -1,    -1,    -1,    -1,  1355,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   627,    -1,    -1,   630,   631,    -1,   633,    -1,   635,
     636,  1380,   740,    -1,   640,   641,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1395,    13,    14,    15,
      16,    17,    -1,   569,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
     696,    57,    -1,    59,   802,    -1,    -1,    -1,   806,    -1,
      -1,  1993,    -1,    -1,    -1,    -1,   712,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2023,    -1,   649,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1312,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1321,    -1,    -1,    -1,    -1,    -1,
      86,    -1,    -1,   129,    -1,  2057,    -1,    -1,    -1,    -1,
      -1,  1520,  1521,  1522,  1523,  1524,  1525,  1526,   144,    -1,
    1347,    -1,    -1,    -1,    -1,    -1,    -1,   895,  1355,    -1,
      -1,   797,    -1,    -1,    -1,    -1,  1545,    -1,    -1,    -1,
    1549,  1550,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1560,    -1,  1380,  1563,    -1,    -1,   823,    -1,   825,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1395,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   844,   845,
      -1,    -1,    -1,    -1,    -1,   851,    -1,    -1,   956,    -1,
      -1,   177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1625,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   804,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   813,    -1,    -1,
    1008,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   235,
     236,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   834,    -1,
    1028,    -1,    -1,    -1,   250,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1684,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   269,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1520,  1521,  1522,  1523,  1524,  1525,  1526,
      -1,    -1,    -1,   969,    -1,    -1,  1715,    -1,    -1,   295,
    1078,    -1,    -1,   299,    -1,    -1,    -1,   194,  1545,    -1,
      -1,    -1,  1549,  1550,    -1,    -1,  1094,    -1,    -1,    -1,
      -1,  1099,    -1,  1560,    -1,    -1,  1563,    -1,   215,    -1,
     217,  1109,  1110,    -1,   221,   222,    -1,   333,    -1,    -1,
      -1,    -1,    -1,    -1,   340,   232,   233,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   357,    -1,   250,   251,  1041,    -1,    -1,    -1,  1045,
      -1,    -1,    -1,  1049,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1625,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1199,  1200,    -1,  1010,   421,    -1,    -1,    -1,    -1,
     426,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1862,  1120,    -1,    -1,  1684,    -1,    -1,
      -1,   447,   448,   449,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1246,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1715,    -1,
      -1,  1259,    -1,    -1,    -1,    -1,    -1,    -1,   132,   485,
      -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   145,    -1,   147,    -1,    -1,    -1,  1093,   504,   505,
      -1,   507,   508,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1303,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   178,    -1,    -1,   533,    -1,    -1,
      -1,    -1,    -1,  1321,  1130,    -1,    -1,    -1,  1224,   545,
      -1,  1227,  1228,    -1,    -1,    -1,    -1,    -1,    -1,  1235,
    1236,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1348,    -1,    -1,    -1,  1993,    -1,   220,  1355,    -1,    -1,
      -1,    -1,  1258,    -1,  1260,    -1,    -1,  1263,    -1,    -1,
    1266,    -1,   588,   480,  1270,    -1,    -1,    -1,    -1,   486,
      -1,  1379,  1380,    -1,  2023,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1862,   622,    -1,    -1,  1407,
     626,   627,    -1,    -1,    -1,    -1,    -1,   633,  2057,   283,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   643,    -1,    -1,
     646,   647,    -1,    -1,    -1,    -1,    -1,    -1,  1436,  1437,
      -1,   657,    -1,   659,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   669,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   336,   337,    -1,    -1,   340,    -1,    -1,   343,
     344,   697,   346,    -1,   348,   701,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   600,   601,   602,   603,   604,   605,   606,
     607,   608,   609,   610,   611,   612,   613,   614,   615,   616,
     617,   618,    -1,    -1,    -1,    -1,  1412,    -1,    -1,    -1,
      -1,  1327,  1328,  1329,   740,  1523,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1993,    -1,   645,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1545,    -1,    -1,
      -1,  1549,  1550,    -1,    -1,  1361,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1459,    -1,  1563,  2023,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1382,    -1,    -1,    -1,
    1386,    -1,   798,    -1,  1390,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1591,    -1,    -1,    -1,    -1,    -1,    -1,
    2057,   817,   818,    -1,   820,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1613,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   840,   841,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   851,   852,    -1,   854,   855,
     504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   867,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   879,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1572,    -1,    -1,   895,
      -1,    -1,    -1,    -1,    -1,   901,    -1,   903,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1714,  1715,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1535,
    1536,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     956,   957,    -1,   959,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   971,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   627,    -1,  1661,   630,   631,   875,   633,
      -1,   635,   636,    -1,   881,    -1,   640,   641,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   892,    -1,    -1,    -1,    -1,
      -1,    -1,  1008,  1689,    -1,    -1,   903,    -1,    -1,  1695,
      -1,  1607,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1028,    -1,  1030,    -1,    -1,    -1,  1034,    -1,
      -1,    -1,    -1,    -1,    -1,  1041,  1042,    -1,    -1,  1045,
    1046,    -1,    -1,  1049,  1050,    -1,    -1,    -1,    -1,    -1,
    1056,    -1,    -1,    -1,    -1,    -1,  1844,   954,   712,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1862,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1771,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1099,  1100,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1699,    -1,  1791,  1792,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1121,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1022,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1821,  1822,    -1,  1144,    -1,
      -1,    -1,    -1,  1829,    -1,    -1,    -1,    -1,  1834,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1173,    -1,   823,
      -1,   825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,
     844,   845,    -1,  1199,  1200,    -1,    -1,   851,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1993,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1224,  1225,
      -1,  1227,  1228,  1229,    -1,    -1,  1912,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2023,    -1,    -1,    -1,    -1,
    1246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1259,    -1,    -1,    -1,  1263,  1264,    -1,
    1266,  1267,    -1,    -1,  1270,  1271,    -1,    -1,    -1,  2057,
      -1,    -1,    -1,  1170,  1171,  1172,    -1,    -1,    -1,    -1,
      -1,  1178,  1179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1978,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     202,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   969,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1220,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   235,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2027,  1348,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1250,    -1,  1252,    -1,    -1,     1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1041,    -1,    -1,
      -1,  1045,    -1,    -1,    -1,  1049,    -1,   299,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1412,  1413,    -1,    -1,
      -1,   313,    55,   315,  1420,    58,    -1,    60,    61,    -1,
      63,    -1,    -1,    -1,  1321,    -1,    -1,    -1,    -1,    -1,
    1436,  1437,    -1,    -1,    -1,    78,    -1,    80,    81,    -1,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,    -1,   108,  1120,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,    -1,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   409,    -1,    -1,
      -1,    -1,    -1,    -1,   416,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   447,   189,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1224,    -1,    -1,  1227,  1228,    -1,   478,    -1,    -1,    -1,
      -1,  1235,  1236,   485,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1258,    -1,  1260,    -1,    -1,  1263,
      -1,    -1,  1266,    -1,    -1,    -1,  1270,    -1,   520,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,
      -1,   533,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   548,   549,    -1,   551,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   559,    -1,   561,
      -1,    -1,    -1,    -1,    -1,    -1,   568,    -1,   570,   571,
      -1,    -1,  1678,    -1,    -1,    -1,    55,    -1,    -1,    58,
      -1,    60,    61,   585,    63,    -1,   588,    -1,    -1,  1695,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    80,    -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    94,  1609,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   623,    -1,   625,    -1,    -1,    -1,   110,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,   132,    -1,   134,   135,    -1,  1412,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   676,   677,    -1,    -1,   161,    -1,
      -1,   683,   165,    -1,    -1,   164,   688,    -1,   167,   168,
      -1,    -1,    -1,    -1,   177,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,  1459,    -1,    -1,  1814,  1815,
      -1,    -1,   714,    -1,    -1,    55,    -1,    -1,    58,   202,
      60,    61,    -1,    63,  1830,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   740,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   234,   235,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1760,    -1,    -1,    -1,   250,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   271,    -1,
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,  1910,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1918,    -1,    -1,    -1,   299,    -1,  1572,   302,
      -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,
      -1,    -1,   172,   835,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,
      -1,    -1,  1958,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   867,    -1,    -1,    -1,    -1,
      -1,    -1,  1978,  1979,   357,   358,  1982,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   888,    -1,    -1,    -1,
     373,   893,    -1,    -1,    -1,    -1,    -1,    -1,   900,   901,
      -1,    -1,    -1,    -1,    -1,   907,    -1,  1661,    -1,    -1,
     912,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2027,  2028,    -1,    -1,  1922,    -1,    -1,    -1,    -1,
     932,    -1,    -1,    -1,   417,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   426,   427,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   447,   448,    -1,   450,    -1,  1966,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2088,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   485,   486,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1018,  1771,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   509,  1029,  2025,   512,
      -1,   514,   515,    -1,    -1,   518,    -1,  1791,  1792,    -1,
      -1,    -1,    -1,    -1,   527,    -1,    -1,   530,   531,   532,
     533,    -1,    -1,    -1,  2051,    -1,  2053,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   548,    -1,  1821,  1822,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1834,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2091,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   588,    -1,    -1,   591,    -1,
      -1,    -1,  1114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1912,  2156,
      -1,    -1,    -1,   646,   647,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   657,    -1,   659,    -1,    -1,  1181,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,
      -1,    -1,    -1,    -1,    -1,   688,    -1,    -1,    -1,    -1,
      -1,  1213,    -1,    -1,  1216,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1978,    -1,    -1,    -1,    -1,    -1,
    1232,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,
      -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,   734,   735,    -1,  1256,    -1,    -1,   740,    -1,    78,
      -1,    80,    81,    -1,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,  1290,   108,
    1292,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
      -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1341,
    1342,    -1,    -1,    -1,   163,   164,  1348,   830,   167,   168,
      -1,    -1,    -1,   172,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,   848,    -1,    -1,    -1,    -1,
     189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   867,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   895,    -1,    18,    -1,    -1,    -1,   901,    -1,
     903,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1431,
      -1,  1433,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1452,    55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1471,
    1472,    -1,    -1,   956,    78,    -1,    80,    81,    -1,    83,
      -1,    -1,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,    -1,   108,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,  1008,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,  1538,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,  1028,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1557,    -1,    -1,  1560,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   189,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1099,    -1,    -1,    -1,
      -1,  1623,    -1,    -1,  1626,    -1,  1109,    -1,    -1,    -1,
      -1,  1114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1647,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1670,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1684,    -1,  1167,    -1,    -1,    -1,  1690,    -1,
    1173,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1199,  1200,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1751,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,  1246,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1259,    -1,  1780,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
     132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,  1312,
      -1,    -1,    -1,    -1,    -1,   157,   158,   159,   160,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,  1347,  1348,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1379,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    18,  1395,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,  1436,  1437,    62,     1,    -1,    65,    66,
      67,    68,    69,    70,    71,  1967,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,  1520,  1521,  1522,
      -1,    -1,  1525,  1526,    -1,    -1,    -1,    -1,    -1,  1532,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,  1560,    -1,    -1,
      -1,    -1,   189,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1591,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1625,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,  1684,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    76,    -1,    78,    79,    80,    81,    -1,
      83,  1714,    -1,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,    -1,   108,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   189,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,  1844,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,  1877,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    -1,    -1,    -1,    76,    -1,    78,    79,
      80,    81,    -1,    83,    -1,    -1,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,    -1,   108,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,  1956,   134,   135,  1959,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,
      -1,    -1,   172,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    76,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,   159,   160,    -1,    -1,
      -1,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   189,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      76,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,   159,   160,    -1,    -1,    -1,   164,   165,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   189,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,   112,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,   128,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,
     189,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
     172,    -1,    -1,   175,   176,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   187,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,   172,    -1,    -1,   175,   176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   187,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,    79,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,   129,
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,    -1,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    -1,    -1,    -1,    76,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
     132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,   169,   170,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,
     168,   169,   170,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,   165,    -1,   167,   168,   169,   170,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,   165,    -1,   167,   168,   169,   170,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,   169,   170,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,   129,
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,
     170,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,   169,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,   129,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,   169,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,
     169,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
     132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,    -1,    -1,   167,   168,   169,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,    -1,    -1,    76,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    -1,    -1,
      -1,    76,    -1,    -1,    79,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
       1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,     1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,     1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,     1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,     1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,
      -1,   172,    -1,    -1,   175,   176,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,   187,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,   187,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,   187,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   187,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    -1,
      -1,    -1,    76,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,    -1,    -1,   167,   168,    -1,     3,    -1,     5,    -1,
      -1,   175,   176,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,    -1,
     167,   168,    -1,     3,    -1,     5,    -1,    -1,   175,   176,
      10,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,    -1,    -1,   167,   168,    -1,
       3,    -1,     5,    -1,    -1,   175,   176,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,    -1,    -1,   167,   168,    -1,     3,    -1,     5,
      -1,    -1,   175,   176,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    13,    14,
      15,    16,    17,   175,   176,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   164,    -1,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,    -1,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,   145,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   175,   176,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
      -1,    -1,    -1,    -1,    -1,   175,   176,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   176,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,    -1,
      63,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,    78,    -1,    80,    81,    -1,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    -1,    -1,   100,   101,   102,
     103,   104,   105,   106,    -1,   108,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,    -1,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    55,
      -1,    -1,    58,    -1,    60,    61,   189,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    -1,    80,    81,    -1,    83,    -1,    -1,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    -1,    -1,   100,   101,   102,   103,   104,   105,
     106,    -1,   108,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,    -1,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,    -1,   172,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   189,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,    -1,    -1,   172,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,    -1,    -1,   172,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,    -1,    -1,   172,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    97,    -1,
      99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,
      -1,    -1,    -1,   172,    -1,   174,   175,   176,   177,   178,
     179,   180,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,   172,    -1,   174,   175,   176,   177,   178,   179,   180,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,    -1,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,   174,
     175,   176,   177,   178,   179,   180,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,
     167,   168,    -1,    -1,    -1,   172,    -1,   174,   175,   176,
     177,   178,   179,   180,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,   171,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,
      -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,   167,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   144,   145,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,   145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   144,   145,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,   167,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,   167,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   176,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    85,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   176,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,   167,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   175,   176,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   175,   176,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,    13,    14,    15,
      16,    17,   175,   176,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,    13,    14,    15,    16,    17,   175,
     176,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,    13,    14,    15,    16,    17,   175,   176,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   175,   176,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   167,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,   167,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    20,   167,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    79,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,   115,   116,    57,
      -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,   144,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,   144,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      79,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    55,    -1,    -1,    58,
      -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,   144,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,    55,
      -1,    -1,    58,   142,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
     159,   160,    -1,    -1,    80,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,
      -1,   167,   168,    -1,   170,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,
     168,    -1,    -1,    -1,   172,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,   169,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,
     172,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,   167,   168,    -1,    -1,   171,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,   169,    -1,    -1,    -1,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,   169,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   167,   168,    -1,    -1,    -1,   172,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,
      -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,
     176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,
     168,    -1,    -1,    -1,   172,    -1,   174,   175,   176,   177,
     178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   164,    -1,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,
      -1,    -1,   174,   175,   176,   177,   178,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    55,    -1,    -1,    58,   142,    60,
      61,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
     164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
     174,   175,   176,   177,   178,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   164,   165,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   164,   165,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,   165,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,
     165,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,   165,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   174,   175,   176,   177,   178,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,   164,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,   174,
     175,   176,   177,   178,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,   174,   175,   176,
     177,   178,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,   174,   175,   176,   177,   178,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,   174,   175,   176,   177,   178,   179,   180
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   191,   417,   418,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    57,    59,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    76,    79,    80,   108,   112,   113,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   128,
     129,   142,   144,   164,   165,   167,   168,   175,   176,   187,
     189,   194,   195,   196,   209,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   318,   321,   323,
     324,   325,   326,   327,   328,   329,   330,   331,   332,   334,
     336,   337,   338,   340,   341,   345,   346,   347,   348,   349,
     351,   357,   358,   359,   360,   371,   376,   409,   412,   422,
     428,   430,   436,   440,   445,   446,   447,   448,   449,   450,
     451,   452,   478,   496,   497,   498,   499,     0,   191,   113,
     195,   209,   308,   310,   321,   324,   327,   337,   341,   346,
     127,   164,    65,    68,    69,    71,   164,   164,   374,   434,
     435,   436,   333,   334,   115,   116,   195,   197,   410,   411,
     197,   164,   422,   164,   164,     4,   113,   115,   116,   325,
     330,   331,   164,   164,   209,   435,   440,   446,   447,   448,
     450,   451,   452,   115,   348,    55,    58,    60,    61,    63,
      64,    80,   110,   111,   113,   114,   125,   126,   127,   130,
     131,   132,   134,   135,   164,   168,   169,   174,   175,   177,
     178,   179,   180,   193,   194,   198,   199,   200,   203,   208,
     209,   210,   211,   214,   215,   216,   217,   218,   219,   220,
     221,   222,   223,   224,   225,   230,   235,   310,   311,   320,
     322,   324,   328,   329,   336,   337,   343,   344,   345,   346,
     347,   350,   357,   358,   376,   383,   384,   385,   386,   387,
     388,   478,   491,   492,   493,   494,   499,   500,   195,   168,
     311,   321,   324,   445,   449,   495,   496,   499,   500,   189,
     189,   192,   161,   172,   188,   233,   392,    96,   170,   429,
     108,   197,   433,   170,   170,   170,   189,   115,   116,   164,
     209,   316,   317,   440,   441,   442,   443,   444,   445,   449,
     453,   454,   455,   456,   457,   458,   459,   460,   461,   467,
       3,    53,    54,    56,    62,   339,     3,   168,   209,   310,
     311,   325,   329,   331,   342,   347,   425,   445,   449,   499,
      76,   308,   310,   324,   337,   341,   346,   426,   445,   449,
      72,   330,   330,   325,   331,   319,   330,   331,   339,   358,
     325,   330,   325,   167,   434,   170,   192,   164,   172,   241,
     434,   434,     3,   299,   300,   315,   318,   324,   328,   168,
     321,   324,   497,   197,   197,   422,   188,   324,   164,   209,
     431,   440,   441,   445,   454,   458,   168,   209,   311,   499,
     423,   424,    64,    72,    73,    74,    75,   168,   186,   197,
     398,   400,   404,   406,   407,   347,   166,   168,   209,   320,
     324,   337,   344,   346,   388,   491,   499,   434,   115,   116,
     179,   195,   347,   375,   467,   436,   164,   405,   406,   164,
      13,    95,   164,   197,   437,   438,   439,   198,   168,   208,
     209,   225,   226,   347,   166,   168,   209,   230,   321,   390,
     391,   408,   495,   500,   437,   324,   446,   447,   448,   450,
     451,   452,   166,   166,   166,   166,   166,   166,   166,   164,
     208,   164,   164,   208,   164,   164,   434,   211,   164,   208,
     164,   113,   115,   116,   325,   330,   331,   164,   208,   208,
      19,    21,    92,   168,   177,   178,   212,   213,   230,   237,
     241,   360,   390,   499,   165,   169,   170,   230,   324,   328,
     115,   168,   195,   321,   478,   497,   164,   200,   169,   168,
     173,   168,   173,   127,   131,   133,   134,   135,   164,   167,
     168,   172,   173,   146,   147,   148,   149,   150,   151,   152,
     153,   154,   155,   156,   188,   232,   233,   234,   168,   211,
     322,   324,   337,   344,   346,   490,   491,   499,   500,   211,
     181,   175,   182,   183,   177,   178,   136,   137,   138,   139,
     184,   185,   140,   141,   176,   174,   186,   142,   143,   187,
     169,   164,   164,   168,   176,   188,   209,   440,   462,   463,
     464,   465,   466,   467,   468,   469,   470,   478,   480,   481,
     482,   483,   484,   485,   502,   145,   168,   209,   350,   499,
     324,   344,   330,   325,   167,   434,   169,   170,   169,   170,
     322,   324,   492,   197,   322,   478,   492,   164,   197,   169,
     445,   449,   499,   170,   113,   167,   168,   172,   194,   196,
     230,   393,   394,   395,   396,   397,    22,   393,   164,   197,
     241,   164,   164,   195,   431,   195,   435,   440,   442,   443,
     444,   453,   455,   456,   457,   459,   460,   461,   324,   441,
     454,   458,   170,   433,   168,   434,   475,   478,   433,   434,
     434,   429,   299,   164,   434,   475,   433,   434,   434,   429,
     434,   434,   324,   431,   164,   164,   323,   324,   321,   324,
     168,   169,   321,   495,   500,   433,   349,   172,   429,   299,
     197,   197,   392,   310,   329,   427,   445,   449,   172,   429,
     299,   410,   324,   337,   324,   324,   115,   348,   115,   116,
     195,   347,   352,   410,   145,   195,   324,   380,   381,   385,
     386,   389,   163,   191,   241,   315,   189,   445,   458,   324,
     175,   230,   433,   164,   433,   192,   230,   435,   440,   324,
     164,   197,   420,   172,   164,   197,   172,   197,   145,   175,
     176,   403,   166,   170,   197,   407,   166,   169,   164,   176,
     209,   499,   166,   195,   375,   467,   372,   172,   375,   398,
     188,   398,   437,   166,   170,   164,   166,   230,   166,   170,
     164,   209,   471,   472,   473,   474,   475,   166,   170,   166,
     166,   166,   166,   166,   166,   166,   164,   434,   475,   478,
     164,   475,   478,   390,   500,   200,   390,   168,   390,   391,
     195,   390,   500,   230,   390,   166,   390,   390,   390,   169,
     384,   166,   177,   178,   213,    18,   326,   166,   170,   166,
     175,   176,   166,   170,   501,   164,   322,   478,   492,   169,
     170,   168,   175,   209,   230,   347,   230,   324,   164,   164,
     321,   497,   172,   230,   195,   230,   195,   125,   168,   195,
     187,   227,   228,   229,   230,   125,   168,   197,   360,   230,
     227,   195,   172,   230,   499,   211,   214,   214,   214,   215,
     215,   216,   216,   217,   217,   217,   217,   218,   218,   219,
     220,   221,   222,   223,   171,   237,   191,   164,   380,   440,
     463,   464,   465,   468,   481,   482,   483,   169,   191,    18,
     230,   324,   462,   466,   480,   164,   434,   484,   502,   434,
     434,   502,   164,   434,   484,   434,   434,   502,   434,   434,
     478,   169,   226,   322,   490,   500,   197,   324,   168,   195,
     195,   384,   387,   387,   388,   502,   322,   492,   191,   502,
     191,   168,   196,   225,   226,   432,   394,   171,   170,   501,
     393,   167,   168,   188,   397,   408,   164,   198,   191,   188,
     440,   442,   443,   444,   453,   455,   456,   457,   459,   460,
     461,   166,   166,   166,   166,   166,   166,   166,   166,   166,
     166,   441,   454,   458,   434,   188,   169,   230,   331,   347,
     476,   392,   241,   429,   380,   392,   241,   431,   237,   391,
     237,   391,   431,   115,   420,   241,   429,   433,   172,   172,
     429,   299,   420,   241,   429,   354,   355,   353,   172,   166,
     170,   166,   170,    77,   301,   302,   189,   169,   169,   191,
     440,   422,   420,   197,   169,     1,   308,   310,   322,   324,
     413,   414,   415,   416,   164,   402,   400,   401,    85,   335,
      18,   324,   434,   172,   434,   375,    10,   174,   375,   377,
     378,   172,   166,   391,   166,   166,   438,   227,   189,   198,
     380,   472,   473,   474,   324,   471,   434,   434,   230,   391,
     164,   434,   475,   478,   164,   475,   478,   380,   380,   166,
     166,   170,   166,   170,   166,   166,   166,   170,   166,   211,
     166,   166,   166,   170,   211,    18,   326,   230,   166,   166,
     165,   172,   211,   165,   230,   236,   169,   145,   382,   383,
     384,   322,   492,   169,   236,   169,   169,   169,   230,   191,
     191,   227,   169,   169,   125,   130,   132,   196,   204,   205,
     206,   195,   166,   170,   204,   169,   170,   163,   394,   225,
     171,   382,   468,   166,   166,   166,   166,   166,   166,   166,
     166,     5,   324,   164,   434,   440,   467,   462,   466,   480,
     380,   380,   169,   502,   204,   169,   170,   382,   197,   204,
     145,   169,   180,   169,   501,   393,   395,   163,   166,   191,
     166,   382,   230,   166,   166,   166,   166,   166,   166,   166,
     166,   166,   164,   434,   475,   478,   164,   434,   475,   478,
     164,   434,   475,   478,   431,    22,   478,   158,   170,   180,
     477,   169,   170,   241,   166,   166,   166,   166,   166,   418,
     419,   241,   163,   413,   420,   241,   429,   419,   241,   172,
     172,   172,   361,   145,   385,   386,   195,   197,   303,    18,
      78,    80,    81,    83,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,   100,   101,   102,   103,
     104,   105,   106,   108,   115,   116,   128,   164,   168,   197,
     237,   238,   239,   240,   241,   242,   243,   245,   246,   255,
     262,   263,   264,   265,   266,   267,   272,   273,   276,   277,
     278,   279,   280,   281,   282,   288,   289,   290,   304,   324,
     328,   430,    77,   382,   421,   419,   166,   431,   163,   414,
     170,   189,   170,   189,   408,   188,   399,   399,   373,   377,
     375,   172,   347,   170,   501,   197,   377,   172,   166,   166,
     166,   166,   166,   166,   166,   471,   380,   380,   166,   166,
     320,   195,    85,   201,   202,   390,   211,   211,   211,   211,
     211,   172,   394,   170,   501,   166,   170,   170,   501,   169,
     382,   382,   163,   207,   168,   205,   207,   207,   169,   170,
     133,   167,   188,   229,   169,   236,   501,   225,   192,   166,
     164,   434,   475,   478,   164,   434,   484,   164,   434,   484,
     478,   323,     5,   175,   192,   230,   440,   434,   434,   166,
     166,   169,   387,   192,   392,   169,   226,   226,   163,   393,
     434,   382,   434,   192,   164,   434,   475,   478,   164,   434,
     475,   478,   164,   434,   475,   478,   380,   380,   380,   433,
     237,   230,   230,   331,   347,   421,   163,   419,   241,   421,
     361,   361,   361,     3,     5,    10,    80,   163,   305,   312,
     313,   321,   324,   362,   367,   495,   170,   189,   164,    68,
      69,   189,   241,   304,   430,   164,   164,    18,   239,   164,
     164,   189,   197,   189,   197,   175,   197,   172,   238,   164,
     164,   164,   239,   164,   241,   230,   231,   231,    14,   291,
     267,   278,   171,   189,   192,   243,    85,   189,   197,    98,
      99,   271,   275,   119,   143,   270,   118,   142,   274,   270,
     389,   324,   303,   192,   421,   197,   197,   431,   166,   391,
     405,   405,   375,   501,   172,   377,    10,   378,   163,   188,
     379,   501,   163,   413,   164,   434,   475,   478,   166,   166,
     479,   480,   166,   171,   166,   170,   171,   394,   501,   165,
     230,   169,   145,   384,   145,   169,   192,   192,   131,   204,
     205,   168,   205,   168,   205,   230,   169,   170,   163,   166,
     380,   380,   380,   230,   230,   192,   169,   192,   166,   169,
     192,   166,   380,   380,   380,   166,   166,   166,   392,   169,
     477,   163,   421,   163,   163,   163,   163,   321,   321,   360,
     368,   495,   321,   367,   164,   356,   189,   189,   189,   164,
     171,   209,   363,   364,   370,   440,   441,   454,   458,   170,
     189,   197,   197,   227,   189,   241,   189,   241,   237,   247,
     304,   306,   309,   315,   324,   328,   237,    87,   166,   247,
     157,   158,   159,   160,   165,   166,   189,   237,   256,   257,
     259,   304,   189,   189,   237,   189,   394,   189,   237,   408,
     237,   256,   120,   121,   122,   123,   124,   283,   285,   286,
     189,   107,   189,    91,   164,   166,   434,   163,   189,   189,
     164,   164,   239,   239,   267,   164,   277,   267,   277,   241,
     189,   166,   163,   403,   172,   163,   377,   501,   347,   197,
     172,   226,   163,   163,   380,   166,   230,   202,   230,   501,
     163,   166,   166,   169,   204,   204,   166,   166,   166,   192,
     192,   169,   169,   166,   434,   166,   166,   166,   230,   163,
     356,   356,   356,   363,   164,   209,   365,   366,   475,   486,
     487,   488,   489,   189,   170,   189,   363,   189,   408,   435,
     440,   230,   324,   163,   170,   189,   369,   370,   369,   369,
     197,   166,   166,   237,   324,   166,   164,   239,   166,   157,
     158,   159,   160,   180,   189,   260,   261,   239,   238,   189,
     261,   166,   171,   237,   165,   237,   238,   259,   189,   501,
     166,   166,   166,   166,   241,   285,   286,   164,   230,   164,
     198,     1,   239,   211,   268,   237,    82,   117,   269,   271,
      82,   434,   399,   377,   501,   163,   379,   394,   166,   163,
     434,   434,   169,   169,   169,   169,   189,   487,   488,   489,
     324,   486,   170,   189,   434,   434,   189,   166,   440,   434,
     239,   239,    84,    85,   172,   250,   251,   252,   166,   237,
      82,   239,   237,   165,   237,    82,   189,   165,   237,   238,
     259,   324,   346,   165,   237,   239,   257,   261,   261,   189,
     237,   163,   172,   252,   239,   239,   164,   287,   322,   324,
     495,   189,   198,   166,   171,   166,   170,   171,   166,   239,
     164,   239,   239,   239,   405,   501,   163,   501,   166,   166,
     166,   486,   434,   364,    82,     1,   226,   248,   249,   432,
       1,   171,     1,   191,   239,   250,    82,   189,   166,   239,
      82,   189,   180,   180,   239,   238,   261,   261,   189,    64,
     237,   258,   347,   180,   180,    82,   165,   237,   165,   237,
     238,   189,     1,   191,   287,   189,   284,   164,   209,   431,
     486,   195,   171,   189,   168,   198,   292,   293,   294,   211,
     227,   237,   270,   163,   163,   164,   434,   475,   478,   366,
     239,   145,     1,   170,   171,   163,   297,   298,   304,   239,
      82,   189,   239,   237,   165,   165,   237,   165,   237,   165,
     237,   238,   195,   347,   165,   237,   165,   237,   239,   180,
     180,   180,   180,   163,   297,   284,   225,   166,   324,   171,
     113,   164,   166,   171,   170,   166,   166,    82,   266,   380,
     226,   248,   251,   253,   254,   304,   239,   180,   180,   180,
     180,   165,   165,   237,   165,   237,   165,   237,   253,   166,
     241,   292,   169,   226,   189,   292,   294,   239,    82,   166,
     239,   244,   192,   251,   165,   165,   237,   165,   237,   165,
     237,   192,   241,   171,   198,   166,   166,   171,   239,     1,
     239,   163,   244,   163,   198,   295,   164,   189,   295,   170,
     171,   226,   166,   198,   195,   296,   166,   189,   166,   170,
     189,   195
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   190,   191,   192,   193,   193,   193,   193,   193,   194,
     194,   194,   194,   194,   194,   194,   194,   195,   195,   196,
     196,   197,   197,   197,   198,   199,   199,   200,   200,   200,
     200,   200,   200,   200,   200,   200,   200,   200,   200,   200,
     200,   200,   201,   201,   202,   202,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     204,   204,   205,   205,   205,   205,   205,   205,   205,   206,
     206,   206,   207,   207,   208,   208,   208,   208,   208,   208,
     208,   208,   208,   208,   208,   208,   208,   208,   208,   208,
     208,   208,   209,   209,   209,   210,   210,   210,   210,   211,
     211,   211,   211,   211,   211,   211,   211,   211,   212,   212,
     212,   212,   213,   213,   214,   214,   215,   215,   215,   215,
     216,   216,   216,   217,   217,   217,   218,   218,   218,   218,
     218,   219,   219,   219,   220,   220,   221,   221,   222,   222,
     223,   223,   224,   224,   225,   225,   225,   226,   227,   227,
     228,   228,   229,   229,   229,   230,   230,   230,   231,   231,
     232,   232,   233,   233,   234,   234,   234,   234,   234,   234,
     234,   234,   234,   234,   234,   235,   235,   235,   235,   235,
     236,   236,   236,   236,   237,   237,   238,   238,   239,   239,
     239,   239,   239,   239,   239,   239,   239,   239,   239,   239,
     239,   239,   239,   239,   240,   240,   241,   241,   242,   242,
     243,   243,   243,   243,   243,   244,   244,   244,   245,   246,
     246,   246,   246,   246,   246,   246,   246,   247,   247,   247,
     247,   248,   248,   248,   249,   249,   250,   250,   250,   250,
     250,   251,   251,   252,   253,   253,   254,   254,   255,   255,
     255,   255,   255,   255,   255,   255,   255,   255,   255,   255,
     256,   256,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   258,   258,   258,
     259,   259,   259,   259,   260,   260,   260,   261,   261,   261,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     263,   263,   264,   265,   266,   267,   267,   268,   268,   269,
     270,   270,   271,   271,   272,   272,   272,   272,   272,   272,
     273,   274,   274,   275,   276,   276,   277,   277,   278,   278,
     278,   279,   280,   281,   282,   282,   282,   283,   283,   284,
     284,   285,   285,   285,   285,   286,   287,   287,   287,   287,
     287,   288,   289,   289,   290,   290,   290,   290,   290,   291,
     291,   292,   292,   293,   293,   294,   294,   295,   295,   295,
     296,   296,   297,   297,   298,   298,   299,   299,   300,   300,
     301,   301,   302,   302,   303,   303,   304,   304,   304,   305,
     305,   306,   306,   306,   306,   306,   307,   307,   307,   308,
     308,   308,   308,   308,   309,   309,   309,   309,   309,   310,
     310,   310,   310,   311,   311,   312,   312,   312,   313,   313,
     313,   313,   313,   314,   314,   315,   315,   315,   315,   316,
     316,   316,   316,   316,   317,   317,   318,   318,   318,   318,
     319,   319,   319,   320,   320,   320,   321,   321,   321,   322,
     322,   322,   323,   323,   324,   324,   325,   325,   326,   326,
     326,   326,   326,   327,   328,   328,   328,   329,   329,   330,
     330,   330,   330,   330,   330,   330,   330,   330,   331,   332,
     332,   332,   332,   332,   332,   332,   332,   332,   332,   332,
     332,   332,   332,   332,   332,   332,   332,   332,   332,   332,
     332,   332,   332,   332,   332,   332,   332,   332,   332,   332,
     332,   332,   332,   333,   333,   334,   335,   335,   336,   336,
     336,   336,   336,   337,   337,   338,   338,   338,   338,   339,
     339,   339,   339,   339,   339,   340,   340,   340,   340,   341,
     342,   341,   341,   343,   343,   343,   343,   344,   344,   344,
     345,   345,   345,   345,   346,   346,   346,   347,   347,   347,
     347,   347,   347,   348,   348,   348,   349,   349,   350,   350,
     352,   351,   353,   351,   354,   351,   355,   351,   351,   356,
     356,   357,   357,   358,   358,   359,   359,   359,   360,   360,
     360,   360,   360,   360,   360,   360,   361,   361,   362,   362,
     362,   362,   362,   362,   362,   362,   362,   362,   362,   362,
     363,   363,   363,   364,   364,   364,   364,   365,   365,   365,
     366,   367,   367,   368,   368,   369,   369,   370,   371,   371,
     372,   371,   371,   373,   371,   371,   371,   374,   374,   375,
     375,   376,   376,   377,   377,   377,   377,   377,   378,   378,
     379,   379,   379,   380,   380,   380,   380,   381,   381,   381,
     381,   382,   382,   382,   382,   382,   382,   382,   383,   383,
     383,   383,   384,   384,   385,   385,   386,   386,   387,   387,
     387,   387,   387,   388,   388,   388,   388,   388,   389,   389,
     390,   390,   390,   391,   391,   392,   392,   392,   392,   393,
     393,   394,   394,   394,   394,   394,   395,   395,   396,   396,
     397,   397,   397,   397,   397,   398,   398,   399,   399,   401,
     400,   402,   400,   400,   400,   400,   403,   403,   403,   403,
     404,   404,   404,   404,   405,   405,   406,   406,   407,   407,
     408,   408,   408,   408,   409,   409,   409,   410,   410,   411,
     411,   412,   412,   412,   412,   413,   413,   414,   414,   415,
     415,   415,   416,   416,   416,   417,   417,   418,   418,   419,
     419,   420,   421,   422,   422,   422,   422,   422,   422,   422,
     422,   422,   422,   422,   423,   422,   424,   422,   425,   422,
     426,   422,   427,   422,   422,   428,   428,   428,   429,   429,
     430,   430,   430,   430,   430,   430,   430,   430,   430,   430,
     431,   431,   431,   431,   432,   433,   433,   434,   434,   435,
     435,   436,   436,   436,   436,   437,   437,   438,   438,   438,
     439,   439,   439,   440,   440,   440,   441,   441,   441,   441,
     442,   442,   442,   442,   443,   443,   443,   443,   443,   443,
     443,   444,   444,   444,   444,   445,   445,   445,   446,   446,
     446,   446,   446,   447,   447,   447,   447,   448,   448,   448,
     448,   448,   448,   449,   449,   449,   450,   450,   450,   450,
     450,   451,   451,   451,   451,   452,   452,   452,   452,   452,
     452,   453,   453,   454,   454,   454,   454,   455,   455,   455,
     455,   456,   456,   456,   456,   456,   456,   456,   457,   457,
     457,   457,   458,   458,   458,   459,   459,   459,   459,   459,
     460,   460,   460,   460,   461,   461,   461,   461,   461,   461,
     462,   462,   462,   462,   462,   463,   463,   463,   464,   464,
     464,   464,   465,   465,   465,   466,   466,   466,   466,   466,
     467,   467,   468,   468,   468,   469,   469,   470,   470,   471,
     471,   471,   472,   472,   472,   472,   472,   473,   473,   473,
     473,   474,   474,   474,   475,   475,   475,   475,   475,   476,
     476,   476,   476,   476,   476,   477,   477,   478,   478,   478,
     478,   479,   479,   480,   480,   480,   480,   481,   481,   481,
     481,   481,   482,   482,   482,   482,   483,   483,   483,   484,
     484,   484,   485,   485,   485,   485,   485,   485,   486,   486,
     486,   487,   487,   487,   487,   487,   488,   488,   488,   488,
     489,   489,   490,   490,   490,   491,   491,   492,   492,   492,
     492,   492,   492,   493,   493,   493,   493,   493,   493,   493,
     493,   493,   493,   494,   494,   494,   494,   495,   495,   495,
     496,   496,   497,   497,   497,   497,   497,   497,   498,   498,
     498,   498,   498,   498,   499,   499,   499,   500,   500,   500,
     501,   501,   502,   502
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     3,     3,     3,     5,     6,     2,     2,     2,     2,
       2,     2,     1,     3,     3,     3,     1,     6,     4,     4,
       4,     4,     4,     7,     3,     3,     3,     3,     3,     2,
       5,     3,     3,     3,     5,     2,     2,     7,     8,     5,
       1,     3,     1,     2,     4,     3,     5,     3,     5,     2,
       2,     2,     0,     2,     1,     1,     1,     2,     2,     2,
       2,     2,     2,     4,     2,     4,     4,     4,     6,     4,
       2,     4,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     5,     5,     4,     5,     5,     5,     4,     2,     2,
       3,     3,     1,     1,     1,     3,     1,     3,     3,     3,
       1,     3,     3,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     3,     1,     5,     4,     1,     0,     1,
       1,     3,     1,     4,     1,     1,     3,     6,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     4,     4,     6,     6,
       1,     1,     3,     3,     1,     3,     0,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     4,     2,     6,     1,     2,
       1,     2,     1,     2,     1,     1,     2,     2,     2,     5,
       7,     5,    10,     7,     5,    10,     7,     1,     1,     1,
       2,     1,     3,     1,     1,     3,     2,     3,     3,     2,
       2,     1,     2,     2,     0,     1,     2,     3,     4,     6,
       5,     7,     6,     7,     7,     8,     4,     6,     5,     7,
       1,     3,     4,     5,     4,     3,     5,     1,     2,     3,
       3,     3,     5,     5,     5,     5,     3,     5,     5,     5,
       3,     4,     5,     5,     5,     5,     7,     7,     7,     7,
       7,     7,     7,     2,     3,     4,     4,     4,     6,     6,
       6,     6,     6,     6,     6,     3,     4,     1,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     4,     2,     3,     3,     2,     3,     2,     3,     3,
       6,     2,     2,     3,     3,     3,     3,     3,     3,     5,
       1,     1,     5,     5,     4,     0,     1,     1,     3,     4,
       1,     1,     4,     6,     3,     5,     5,     5,     8,     9,
       1,     1,     1,     4,     3,     3,     1,     3,     1,     3,
       5,     1,     2,     5,     3,     3,     4,     6,     7,     0,
       2,     1,     1,     1,     1,     2,     1,     2,     2,     2,
       1,     3,     1,     1,     6,     8,    10,    12,    14,     0,
       1,     0,     1,     1,     3,     4,     7,     0,     1,     3,
       1,     3,     0,     1,     1,     2,     0,     1,     2,     3,
       0,     1,     3,     4,     1,     3,     2,     2,     2,     6,
       4,     1,     1,     1,     1,     1,     2,     3,     6,     3,
       3,     4,     2,     3,     1,     2,     2,     3,     8,     9,
       9,     8,     8,     3,     5,     2,     2,     3,     3,     3,
       4,     3,     4,     4,     5,     2,     1,     1,     1,     3,
       3,     2,     4,     6,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     4,     1,     2,     3,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     5,     0,     1,     1,     2,
       2,     3,     3,     1,     3,     1,     2,     2,     2,     4,
       4,     4,     4,     1,     1,     1,     2,     2,     3,     1,
       0,     3,     2,     1,     2,     2,     3,     1,     2,     2,
       1,     2,     2,     3,     1,     2,     2,     1,     2,     3,
       1,     2,     3,     1,     3,     4,     1,     1,     1,     1,
       0,     7,     0,     8,     0,     8,     0,     8,     1,     0,
       3,     3,     3,     1,     1,     2,     1,     1,     1,     2,
       1,     2,     1,     2,     1,     2,     0,     2,     3,     3,
       4,     4,     4,     3,     2,     2,     3,     3,     2,     2,
       0,     1,     4,     1,     2,     2,     2,     0,     1,     4,
       1,     2,     3,     1,     2,     0,     1,     2,     7,     8,
       0,     9,     8,     0,    11,    10,     1,     2,     3,     0,
       1,     3,     3,     0,     3,     2,     5,     4,     1,     1,
       0,     2,     5,     0,     1,     1,     3,     1,     1,     3,
       3,     0,     1,     1,     1,     3,     3,     3,     1,     3,
       3,     5,     1,     3,     3,     3,     2,     3,     1,     3,
       3,     4,     1,     1,     1,     1,     2,     1,     1,     3,
       1,     1,     2,     1,     1,     0,     2,     2,     4,     1,
       4,     0,     1,     2,     3,     4,     2,     2,     1,     2,
       2,     3,     3,     5,     4,     1,     3,     0,     2,     0,
       5,     0,     5,     3,     1,     8,     0,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     1,     2,     5,     4,
       1,     1,     3,     3,     2,     3,     3,     2,     4,     1,
       4,     7,     5,     8,     6,     1,     2,     2,     2,     1,
       1,     3,     2,     3,     1,     0,     1,     3,     4,     0,
       1,     0,     0,     1,     1,     2,     2,     2,     2,     2,
       2,     1,     2,     5,     0,     6,     0,     8,     0,     7,
       0,     7,     0,     8,     1,     1,     2,     3,     0,     5,
       3,     4,     4,     4,     4,     5,     5,     5,     5,     6,
       1,     1,     1,     1,     3,     0,     5,     0,     1,     1,
       2,     6,     4,     4,     1,     1,     3,     0,     1,     4,
       1,     1,     1,     1,     2,     3,     2,     1,     2,     2,
       2,     3,     4,     5,     2,     4,     5,     4,     5,     3,
       4,     6,     7,     3,     4,     2,     1,     2,     4,     6,
       7,     3,     4,     2,     3,     4,     5,     4,     5,     4,
       5,     3,     4,     1,     1,     1,     4,     6,     7,     3,
       4,     2,     3,     3,     4,     4,     5,     4,     5,     3,
       4,     1,     3,     2,     1,     2,     2,     2,     3,     4,
       5,     2,     4,     5,     4,     5,     3,     4,     6,     7,
       3,     4,     2,     1,     2,     4,     6,     7,     3,     4,
       2,     3,     4,     5,     4,     5,     4,     5,     3,     4,
       2,     4,     1,     2,     2,     2,     3,     4,     2,     4,
       4,     3,     4,     6,     3,     2,     4,     1,     2,     2,
       1,     1,     2,     3,     4,     2,     4,     4,     6,     1,
       2,     2,     1,     2,     2,     3,     4,     1,     4,     4,
       3,     3,     6,     3,     2,     3,     5,     3,     1,     1,
       1,     3,     3,     3,     5,     1,     1,     3,     3,     4,
       4,     0,     1,     1,     3,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     3,     6,     3,     1,
       2,     1,     2,     6,     5,     6,     7,     7,     1,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       6,     3,     1,     1,     2,     1,     1,     2,     3,     2,
       3,     2,     3,     3,     2,     4,     3,     2,     3,     2,
       4,     3,     2,     4,     4,     4,     5,     1,     2,     1,
       1,     1,     2,     3,     2,     3,     2,     3,     3,     4,
       2,     3,     4,     2,     3,     4,     5,     5,     6,     6,
       0,     1,     0,     2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (yylocationp);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyo, *yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2:
#line 645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 9649 "Parser/parser.cc"
    break;

  case 3:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 9655 "Parser/parser.cc"
    break;

  case 4:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 9661 "Parser/parser.cc"
    break;

  case 5:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9667 "Parser/parser.cc"
    break;

  case 6:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9673 "Parser/parser.cc"
    break;

  case 7:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 8:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 20:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 9691 "Parser/parser.cc"
    break;

  case 24:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 25:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 9703 "Parser/parser.cc"
    break;

  case 26:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 9713 "Parser/parser.cc"
    break;

  case 27:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9719 "Parser/parser.cc"
    break;

  case 28:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9725 "Parser/parser.cc"
    break;

  case 29:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 9731 "Parser/parser.cc"
    break;

  case 31:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9737 "Parser/parser.cc"
    break;

  case 32:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 9743 "Parser/parser.cc"
    break;

  case 33:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9749 "Parser/parser.cc"
    break;

  case 34:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9755 "Parser/parser.cc"
    break;

  case 35:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 9765 "Parser/parser.cc"
    break;

  case 36:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 9771 "Parser/parser.cc"
    break;

  case 37:
#line 736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 9777 "Parser/parser.cc"
    break;

  case 38:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 9783 "Parser/parser.cc"
    break;

  case 39:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9789 "Parser/parser.cc"
    break;

  case 40:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9795 "Parser/parser.cc"
    break;

  case 41:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9801 "Parser/parser.cc"
    break;

  case 43:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 9813 "Parser/parser.cc"
    break;

  case 44:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 9822 "Parser/parser.cc"
    break;

  case 45:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 9828 "Parser/parser.cc"
    break;

  case 47:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 9834 "Parser/parser.cc"
    break;

  case 48:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9840 "Parser/parser.cc"
    break;

  case 49:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9846 "Parser/parser.cc"
    break;

  case 50:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9852 "Parser/parser.cc"
    break;

  case 51:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 9862 "Parser/parser.cc"
    break;

  case 52:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9868 "Parser/parser.cc"
    break;

  case 53:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 9874 "Parser/parser.cc"
    break;

  case 54:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9880 "Parser/parser.cc"
    break;

  case 55:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9886 "Parser/parser.cc"
    break;

  case 56:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9892 "Parser/parser.cc"
    break;

  case 57:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9898 "Parser/parser.cc"
    break;

  case 58:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9904 "Parser/parser.cc"
    break;

  case 59:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9910 "Parser/parser.cc"
    break;

  case 60:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9916 "Parser/parser.cc"
    break;

  case 61:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 9922 "Parser/parser.cc"
    break;

  case 62:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9928 "Parser/parser.cc"
    break;

  case 63:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9934 "Parser/parser.cc"
    break;

  case 64:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9940 "Parser/parser.cc"
    break;

  case 65:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 9946 "Parser/parser.cc"
    break;

  case 66:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 9952 "Parser/parser.cc"
    break;

  case 67:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 9958 "Parser/parser.cc"
    break;

  case 68:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 9964 "Parser/parser.cc"
    break;

  case 69:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 9974 "Parser/parser.cc"
    break;

  case 71:
#line 855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9980 "Parser/parser.cc"
    break;

  case 73:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9986 "Parser/parser.cc"
    break;

  case 74:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9992 "Parser/parser.cc"
    break;

  case 75:
#line 865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9998 "Parser/parser.cc"
    break;

  case 76:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10004 "Parser/parser.cc"
    break;

  case 77:
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10010 "Parser/parser.cc"
    break;

  case 78:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10016 "Parser/parser.cc"
    break;

  case 79:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10022 "Parser/parser.cc"
    break;

  case 80:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 81:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 10036 "Parser/parser.cc"
    break;

  case 82:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10042 "Parser/parser.cc"
    break;

  case 83:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 10051 "Parser/parser.cc"
    break;

  case 86:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10057 "Parser/parser.cc"
    break;

  case 87:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 10063 "Parser/parser.cc"
    break;

  case 88:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			switch ( (yyvsp[-1].oper) ) {
			case OperKinds::AddressOf:
				(yyval.expr) = new ExpressionNode( new ast::AddressExpr( maybeMoveBuild( (yyvsp[0].expr) ) ) );
				break;
			case OperKinds::PointTo:
				(yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) );
				break;
			case OperKinds::And:
				(yyval.expr) = new ExpressionNode( new ast::AddressExpr( new ast::AddressExpr( maybeMoveBuild( (yyvsp[0].expr) ) ) ) );
				break;
			default:
				assert( false );
			}
		}
#line 10083 "Parser/parser.cc"
    break;

  case 89:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 10089 "Parser/parser.cc"
    break;

  case 90:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 10095 "Parser/parser.cc"
    break;

  case 91:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 10101 "Parser/parser.cc"
    break;

  case 92:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10107 "Parser/parser.cc"
    break;

  case 93:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10113 "Parser/parser.cc"
    break;

  case 94:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10119 "Parser/parser.cc"
    break;

  case 95:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10125 "Parser/parser.cc"
    break;

  case 96:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10131 "Parser/parser.cc"
    break;

  case 97:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10137 "Parser/parser.cc"
    break;

  case 98:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 10143 "Parser/parser.cc"
    break;

  case 99:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 10152 "Parser/parser.cc"
    break;

  case 100:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10158 "Parser/parser.cc"
    break;

  case 101:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10164 "Parser/parser.cc"
    break;

  case 102:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 10170 "Parser/parser.cc"
    break;

  case 103:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 10176 "Parser/parser.cc"
    break;

  case 104:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 10182 "Parser/parser.cc"
    break;

  case 105:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 10188 "Parser/parser.cc"
    break;

  case 106:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 10194 "Parser/parser.cc"
    break;

  case 107:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 10200 "Parser/parser.cc"
    break;

  case 108:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 10206 "Parser/parser.cc"
    break;

  case 110:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 10212 "Parser/parser.cc"
    break;

  case 111:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10218 "Parser/parser.cc"
    break;

  case 112:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10224 "Parser/parser.cc"
    break;

  case 113:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 10230 "Parser/parser.cc"
    break;

  case 114:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 10236 "Parser/parser.cc"
    break;

  case 115:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 10242 "Parser/parser.cc"
    break;

  case 116:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10248 "Parser/parser.cc"
    break;

  case 117:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10254 "Parser/parser.cc"
    break;

  case 125:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10260 "Parser/parser.cc"
    break;

  case 127:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10266 "Parser/parser.cc"
    break;

  case 128:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10272 "Parser/parser.cc"
    break;

  case 129:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10278 "Parser/parser.cc"
    break;

  case 131:
#line 1025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10284 "Parser/parser.cc"
    break;

  case 132:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10290 "Parser/parser.cc"
    break;

  case 134:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10296 "Parser/parser.cc"
    break;

  case 135:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10302 "Parser/parser.cc"
    break;

  case 137:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10308 "Parser/parser.cc"
    break;

  case 138:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10314 "Parser/parser.cc"
    break;

  case 139:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10320 "Parser/parser.cc"
    break;

  case 140:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10326 "Parser/parser.cc"
    break;

  case 142:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10332 "Parser/parser.cc"
    break;

  case 143:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10338 "Parser/parser.cc"
    break;

  case 145:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10344 "Parser/parser.cc"
    break;

  case 147:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10350 "Parser/parser.cc"
    break;

  case 149:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10356 "Parser/parser.cc"
    break;

  case 151:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 10362 "Parser/parser.cc"
    break;

  case 153:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 10368 "Parser/parser.cc"
    break;

  case 155:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10374 "Parser/parser.cc"
    break;

  case 156:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 10380 "Parser/parser.cc"
    break;

  case 158:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10386 "Parser/parser.cc"
    break;

  case 161:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10392 "Parser/parser.cc"
    break;

  case 162:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 10398 "Parser/parser.cc"
    break;

  case 163:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10404 "Parser/parser.cc"
    break;

  case 166:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 10416 "Parser/parser.cc"
    break;

  case 167:
#line 1135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10422 "Parser/parser.cc"
    break;

  case 168:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10428 "Parser/parser.cc"
    break;

  case 172:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 10434 "Parser/parser.cc"
    break;

  case 173:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 10440 "Parser/parser.cc"
    break;

  case 174:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 10446 "Parser/parser.cc"
    break;

  case 175:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 10452 "Parser/parser.cc"
    break;

  case 176:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 10458 "Parser/parser.cc"
    break;

  case 177:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 10464 "Parser/parser.cc"
    break;

  case 178:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 10470 "Parser/parser.cc"
    break;

  case 179:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 10476 "Parser/parser.cc"
    break;

  case 180:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 10482 "Parser/parser.cc"
    break;

  case 181:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 10488 "Parser/parser.cc"
    break;

  case 182:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 10494 "Parser/parser.cc"
    break;

  case 183:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 10500 "Parser/parser.cc"
    break;

  case 184:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 10506 "Parser/parser.cc"
    break;

  case 185:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, nullptr ) ); }
#line 10512 "Parser/parser.cc"
    break;

  case 186:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-2].expr) ) ); }
#line 10518 "Parser/parser.cc"
    break;

  case 187:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10524 "Parser/parser.cc"
    break;

  case 188:
#line 1178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-2].expr) ) ) ); }
#line 10530 "Parser/parser.cc"
    break;

  case 189:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10536 "Parser/parser.cc"
    break;

  case 191:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10542 "Parser/parser.cc"
    break;

  case 192:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10548 "Parser/parser.cc"
    break;

  case 193:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10554 "Parser/parser.cc"
    break;

  case 195:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10560 "Parser/parser.cc"
    break;

  case 196:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10566 "Parser/parser.cc"
    break;

  case 211:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10572 "Parser/parser.cc"
    break;

  case 213:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 10578 "Parser/parser.cc"
    break;

  case 214:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 10584 "Parser/parser.cc"
    break;

  case 215:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 10595 "Parser/parser.cc"
    break;

  case 216:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 10601 "Parser/parser.cc"
    break;

  case 217:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 10607 "Parser/parser.cc"
    break;

  case 219:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10613 "Parser/parser.cc"
    break;

  case 220:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10619 "Parser/parser.cc"
    break;

  case 221:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10625 "Parser/parser.cc"
    break;

  case 222:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10631 "Parser/parser.cc"
    break;

  case 223:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10637 "Parser/parser.cc"
    break;

  case 226:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10643 "Parser/parser.cc"
    break;

  case 227:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 10650 "Parser/parser.cc"
    break;

  case 228:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 10656 "Parser/parser.cc"
    break;

  case 229:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 10662 "Parser/parser.cc"
    break;

  case 230:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10668 "Parser/parser.cc"
    break;

  case 231:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10674 "Parser/parser.cc"
    break;

  case 232:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 10688 "Parser/parser.cc"
    break;

  case 233:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10694 "Parser/parser.cc"
    break;

  case 234:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 235:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 10709 "Parser/parser.cc"
    break;

  case 236:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10715 "Parser/parser.cc"
    break;

  case 237:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( nullptr, (yyvsp[0].expr) ); }
#line 10721 "Parser/parser.cc"
    break;

  case 238:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 10727 "Parser/parser.cc"
    break;

  case 239:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 10733 "Parser/parser.cc"
    break;

  case 240:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 10739 "Parser/parser.cc"
    break;

  case 241:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10745 "Parser/parser.cc"
    break;

  case 242:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10751 "Parser/parser.cc"
    break;

  case 244:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 10757 "Parser/parser.cc"
    break;

  case 245:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 10763 "Parser/parser.cc"
    break;

  case 246:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 10769 "Parser/parser.cc"
    break;

  case 247:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 10775 "Parser/parser.cc"
    break;

  case 248:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 10781 "Parser/parser.cc"
    break;

  case 249:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 10787 "Parser/parser.cc"
    break;

  case 250:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 10793 "Parser/parser.cc"
    break;

  case 252:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 10799 "Parser/parser.cc"
    break;

  case 253:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10805 "Parser/parser.cc"
    break;

  case 254:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 10811 "Parser/parser.cc"
    break;

  case 256:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10817 "Parser/parser.cc"
    break;

  case 257:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 10823 "Parser/parser.cc"
    break;

  case 258:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10829 "Parser/parser.cc"
    break;

  case 259:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10838 "Parser/parser.cc"
    break;

  case 260:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10844 "Parser/parser.cc"
    break;

  case 261:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10850 "Parser/parser.cc"
    break;

  case 262:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 10856 "Parser/parser.cc"
    break;

  case 263:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10865 "Parser/parser.cc"
    break;

  case 264:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 10871 "Parser/parser.cc"
    break;

  case 265:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10877 "Parser/parser.cc"
    break;

  case 266:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10883 "Parser/parser.cc"
    break;

  case 267:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10892 "Parser/parser.cc"
    break;

  case 268:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10898 "Parser/parser.cc"
    break;

  case 269:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10904 "Parser/parser.cc"
    break;

  case 271:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyvsp[-2].forctrl)->init->set_last( (yyvsp[0].forctrl)->init );
			if ( (yyvsp[-2].forctrl)->condition ) {
				if ( (yyvsp[0].forctrl)->condition ) {
					(yyvsp[-2].forctrl)->condition->expr.reset( new ast::LogicalExpr( yylloc, (yyvsp[-2].forctrl)->condition->expr.release(), (yyvsp[0].forctrl)->condition->expr.release(), ast::AndExpr ) );
				} // if
			} else (yyvsp[-2].forctrl)->condition = (yyvsp[0].forctrl)->condition;
			if ( (yyvsp[-2].forctrl)->change ) {
				if ( (yyvsp[0].forctrl)->change ) {
					(yyvsp[-2].forctrl)->change->expr.reset( new ast::CommaExpr( yylloc, (yyvsp[-2].forctrl)->change->expr.release(), (yyvsp[0].forctrl)->change->expr.release() ) );
				} // if
			} else (yyvsp[-2].forctrl)->change = (yyvsp[0].forctrl)->change;
			(yyval.forctrl) = (yyvsp[-2].forctrl);
		}
#line 10923 "Parser/parser.cc"
    break;

  case 272:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10929 "Parser/parser.cc"
    break;

  case 273:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 10937 "Parser/parser.cc"
    break;

  case 274:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10943 "Parser/parser.cc"
    break;

  case 275:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 10949 "Parser/parser.cc"
    break;

  case 276:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10955 "Parser/parser.cc"
    break;

  case 277:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 10961 "Parser/parser.cc"
    break;

  case 278:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10967 "Parser/parser.cc"
    break;

  case 279:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10973 "Parser/parser.cc"
    break;

  case 280:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10982 "Parser/parser.cc"
    break;

  case 281:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 10991 "Parser/parser.cc"
    break;

  case 282:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10997 "Parser/parser.cc"
    break;

  case 283:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11006 "Parser/parser.cc"
    break;

  case 284:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11015 "Parser/parser.cc"
    break;

  case 285:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11021 "Parser/parser.cc"
    break;

  case 286:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11027 "Parser/parser.cc"
    break;

  case 287:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11033 "Parser/parser.cc"
    break;

  case 288:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11039 "Parser/parser.cc"
    break;

  case 289:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11045 "Parser/parser.cc"
    break;

  case 290:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11051 "Parser/parser.cc"
    break;

  case 291:
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11057 "Parser/parser.cc"
    break;

  case 292:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11063 "Parser/parser.cc"
    break;

  case 293:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11072 "Parser/parser.cc"
    break;

  case 294:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11082 "Parser/parser.cc"
    break;

  case 295:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11088 "Parser/parser.cc"
    break;

  case 296:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11094 "Parser/parser.cc"
    break;

  case 297:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11103 "Parser/parser.cc"
    break;

  case 298:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11113 "Parser/parser.cc"
    break;

  case 299:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11119 "Parser/parser.cc"
    break;

  case 300:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11128 "Parser/parser.cc"
    break;

  case 301:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11138 "Parser/parser.cc"
    break;

  case 302:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11144 "Parser/parser.cc"
    break;

  case 303:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 11150 "Parser/parser.cc"
    break;

  case 304:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11156 "Parser/parser.cc"
    break;

  case 305:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11162 "Parser/parser.cc"
    break;

  case 306:
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11171 "Parser/parser.cc"
    break;

  case 307:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11181 "Parser/parser.cc"
    break;

  case 308:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11187 "Parser/parser.cc"
    break;

  case 309:
#line 1588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11196 "Parser/parser.cc"
    break;

  case 310:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11206 "Parser/parser.cc"
    break;

  case 311:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11212 "Parser/parser.cc"
    break;

  case 312:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11221 "Parser/parser.cc"
    break;

  case 313:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11231 "Parser/parser.cc"
    break;

  case 314:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11237 "Parser/parser.cc"
    break;

  case 315:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11245 "Parser/parser.cc"
    break;

  case 316:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctrl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11257 "Parser/parser.cc"
    break;

  case 317:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11266 "Parser/parser.cc"
    break;

  case 318:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 11275 "Parser/parser.cc"
    break;

  case 319:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11284 "Parser/parser.cc"
    break;

  case 320:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11290 "Parser/parser.cc"
    break;

  case 321:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11296 "Parser/parser.cc"
    break;

  case 322:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11302 "Parser/parser.cc"
    break;

  case 323:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11308 "Parser/parser.cc"
    break;

  case 324:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11314 "Parser/parser.cc"
    break;

  case 325:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11320 "Parser/parser.cc"
    break;

  case 326:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11326 "Parser/parser.cc"
    break;

  case 328:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11332 "Parser/parser.cc"
    break;

  case 329:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11338 "Parser/parser.cc"
    break;

  case 330:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 11344 "Parser/parser.cc"
    break;

  case 331:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 11350 "Parser/parser.cc"
    break;

  case 332:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 11356 "Parser/parser.cc"
    break;

  case 333:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 11362 "Parser/parser.cc"
    break;

  case 334:
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 11368 "Parser/parser.cc"
    break;

  case 335:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 11374 "Parser/parser.cc"
    break;

  case 336:
#line 1697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 11380 "Parser/parser.cc"
    break;

  case 337:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 11386 "Parser/parser.cc"
    break;

  case 338:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 11392 "Parser/parser.cc"
    break;

  case 339:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 11398 "Parser/parser.cc"
    break;

  case 340:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 11404 "Parser/parser.cc"
    break;

  case 341:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 11410 "Parser/parser.cc"
    break;

  case 342:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 11416 "Parser/parser.cc"
    break;

  case 343:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 11422 "Parser/parser.cc"
    break;

  case 344:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 11428 "Parser/parser.cc"
    break;

  case 345:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 11434 "Parser/parser.cc"
    break;

  case 346:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 11440 "Parser/parser.cc"
    break;

  case 347:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 11446 "Parser/parser.cc"
    break;

  case 348:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 11452 "Parser/parser.cc"
    break;

  case 349:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 11458 "Parser/parser.cc"
    break;

  case 352:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11464 "Parser/parser.cc"
    break;

  case 353:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 11473 "Parser/parser.cc"
    break;

  case 354:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11479 "Parser/parser.cc"
    break;

  case 355:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11485 "Parser/parser.cc"
    break;

  case 358:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11491 "Parser/parser.cc"
    break;

  case 359:
#line 1765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11497 "Parser/parser.cc"
    break;

  case 362:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11503 "Parser/parser.cc"
    break;

  case 363:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 11509 "Parser/parser.cc"
    break;

  case 364:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11515 "Parser/parser.cc"
    break;

  case 365:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11521 "Parser/parser.cc"
    break;

  case 366:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11527 "Parser/parser.cc"
    break;

  case 367:
#line 1788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11533 "Parser/parser.cc"
    break;

  case 368:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 11539 "Parser/parser.cc"
    break;

  case 369:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11545 "Parser/parser.cc"
    break;

  case 370:
#line 1798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 11551 "Parser/parser.cc"
    break;

  case 373:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11557 "Parser/parser.cc"
    break;

  case 374:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11563 "Parser/parser.cc"
    break;

  case 375:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 11569 "Parser/parser.cc"
    break;

  case 376:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11575 "Parser/parser.cc"
    break;

  case 377:
#line 1822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11581 "Parser/parser.cc"
    break;

  case 378:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11587 "Parser/parser.cc"
    break;

  case 379:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11593 "Parser/parser.cc"
    break;

  case 380:
#line 1831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11599 "Parser/parser.cc"
    break;

  case 381:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 11605 "Parser/parser.cc"
    break;

  case 382:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 11611 "Parser/parser.cc"
    break;

  case 383:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11617 "Parser/parser.cc"
    break;

  case 384:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 11623 "Parser/parser.cc"
    break;

  case 385:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 11629 "Parser/parser.cc"
    break;

  case 386:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 11635 "Parser/parser.cc"
    break;

  case 387:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11641 "Parser/parser.cc"
    break;

  case 388:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 11647 "Parser/parser.cc"
    break;

  case 389:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11653 "Parser/parser.cc"
    break;

  case 390:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11659 "Parser/parser.cc"
    break;

  case 391:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11665 "Parser/parser.cc"
    break;

  case 392:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11671 "Parser/parser.cc"
    break;

  case 393:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 11677 "Parser/parser.cc"
    break;

  case 394:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 11683 "Parser/parser.cc"
    break;

  case 395:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 11689 "Parser/parser.cc"
    break;

  case 397:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11695 "Parser/parser.cc"
    break;

  case 398:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11701 "Parser/parser.cc"
    break;

  case 399:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11707 "Parser/parser.cc"
    break;

  case 404:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 11713 "Parser/parser.cc"
    break;

  case 405:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11719 "Parser/parser.cc"
    break;

  case 406:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11725 "Parser/parser.cc"
    break;

  case 407:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11731 "Parser/parser.cc"
    break;

  case 408:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 11737 "Parser/parser.cc"
    break;

  case 409:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 11743 "Parser/parser.cc"
    break;

  case 410:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 11749 "Parser/parser.cc"
    break;

  case 411:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11755 "Parser/parser.cc"
    break;

  case 414:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11761 "Parser/parser.cc"
    break;

  case 415:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 11767 "Parser/parser.cc"
    break;

  case 416:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 11776 "Parser/parser.cc"
    break;

  case 417:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11782 "Parser/parser.cc"
    break;

  case 418:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11788 "Parser/parser.cc"
    break;

  case 419:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11794 "Parser/parser.cc"
    break;

  case 420:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 11803 "Parser/parser.cc"
    break;

  case 421:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 11812 "Parser/parser.cc"
    break;

  case 422:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11818 "Parser/parser.cc"
    break;

  case 425:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 11824 "Parser/parser.cc"
    break;

  case 426:
#line 1983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11830 "Parser/parser.cc"
    break;

  case 428:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11836 "Parser/parser.cc"
    break;

  case 429:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 11842 "Parser/parser.cc"
    break;

  case 439:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 11848 "Parser/parser.cc"
    break;

  case 440:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 11854 "Parser/parser.cc"
    break;

  case 444:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11860 "Parser/parser.cc"
    break;

  case 446:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 11866 "Parser/parser.cc"
    break;

  case 447:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11872 "Parser/parser.cc"
    break;

  case 448:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 11878 "Parser/parser.cc"
    break;

  case 449:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11884 "Parser/parser.cc"
    break;

  case 450:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11890 "Parser/parser.cc"
    break;

  case 451:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11896 "Parser/parser.cc"
    break;

  case 452:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11902 "Parser/parser.cc"
    break;

  case 453:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11908 "Parser/parser.cc"
    break;

  case 455:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11914 "Parser/parser.cc"
    break;

  case 456:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11920 "Parser/parser.cc"
    break;

  case 457:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11926 "Parser/parser.cc"
    break;

  case 458:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 11937 "Parser/parser.cc"
    break;

  case 459:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11943 "Parser/parser.cc"
    break;

  case 460:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11949 "Parser/parser.cc"
    break;

  case 461:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11955 "Parser/parser.cc"
    break;

  case 462:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 11961 "Parser/parser.cc"
    break;

  case 463:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 11967 "Parser/parser.cc"
    break;

  case 464:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) ); }
#line 11973 "Parser/parser.cc"
    break;

  case 465:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11982 "Parser/parser.cc"
    break;

  case 466:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 11991 "Parser/parser.cc"
    break;

  case 467:
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 12000 "Parser/parser.cc"
    break;

  case 468:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 12011 "Parser/parser.cc"
    break;

  case 469:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 12020 "Parser/parser.cc"
    break;

  case 470:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12026 "Parser/parser.cc"
    break;

  case 471:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12032 "Parser/parser.cc"
    break;

  case 472:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12038 "Parser/parser.cc"
    break;

  case 473:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 12046 "Parser/parser.cc"
    break;

  case 474:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 12054 "Parser/parser.cc"
    break;

  case 475:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 12060 "Parser/parser.cc"
    break;

  case 478:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			assert( (yyvsp[0].decl)->type );
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {			// CV qualifiers ?
				SemanticError( yylloc, "illegal syntax, useless type qualifier(s) in empty declaration." ); (yyval.decl) = nullptr;
			}
			// enums are never empty declarations because there must have at least one enumeration.
			if ( (yyvsp[0].decl)->type->kind == TypeData::AggregateInst && (yyvsp[0].decl)->storageClasses.any() ) { // storage class ?
				SemanticError( yylloc, "illegal syntax, useless storage qualifier(s) in empty aggregate declaration." ); (yyval.decl) = nullptr;
			}
		}
#line 12075 "Parser/parser.cc"
    break;

  case 479:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12081 "Parser/parser.cc"
    break;

  case 480:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12087 "Parser/parser.cc"
    break;

  case 481:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 12093 "Parser/parser.cc"
    break;

  case 482:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 12099 "Parser/parser.cc"
    break;

  case 483:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12105 "Parser/parser.cc"
    break;

  case 489:
#line 2219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 12115 "Parser/parser.cc"
    break;

  case 502:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12121 "Parser/parser.cc"
    break;

  case 505:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12127 "Parser/parser.cc"
    break;

  case 506:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12133 "Parser/parser.cc"
    break;

  case 508:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 12139 "Parser/parser.cc"
    break;

  case 509:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 12145 "Parser/parser.cc"
    break;

  case 510:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 12151 "Parser/parser.cc"
    break;

  case 511:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 12157 "Parser/parser.cc"
    break;

  case 512:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 12163 "Parser/parser.cc"
    break;

  case 513:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12169 "Parser/parser.cc"
    break;

  case 515:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12175 "Parser/parser.cc"
    break;

  case 516:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12181 "Parser/parser.cc"
    break;

  case 518:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12187 "Parser/parser.cc"
    break;

  case 519:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 12193 "Parser/parser.cc"
    break;

  case 520:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 12199 "Parser/parser.cc"
    break;

  case 521:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 12205 "Parser/parser.cc"
    break;

  case 522:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 12211 "Parser/parser.cc"
    break;

  case 523:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 12217 "Parser/parser.cc"
    break;

  case 524:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 12223 "Parser/parser.cc"
    break;

  case 525:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 12229 "Parser/parser.cc"
    break;

  case 526:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 12235 "Parser/parser.cc"
    break;

  case 527:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 12241 "Parser/parser.cc"
    break;

  case 528:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12247 "Parser/parser.cc"
    break;

  case 529:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 12253 "Parser/parser.cc"
    break;

  case 530:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 12259 "Parser/parser.cc"
    break;

  case 531:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 12265 "Parser/parser.cc"
    break;

  case 532:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 12271 "Parser/parser.cc"
    break;

  case 533:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 12277 "Parser/parser.cc"
    break;

  case 534:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 12283 "Parser/parser.cc"
    break;

  case 535:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 12289 "Parser/parser.cc"
    break;

  case 536:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 12295 "Parser/parser.cc"
    break;

  case 537:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float80 ); }
#line 12301 "Parser/parser.cc"
    break;

  case 538:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 12307 "Parser/parser.cc"
    break;

  case 539:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float16 ); }
#line 12313 "Parser/parser.cc"
    break;

  case 540:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32 ); }
#line 12319 "Parser/parser.cc"
    break;

  case 541:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x ); }
#line 12325 "Parser/parser.cc"
    break;

  case 542:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64 ); }
#line 12331 "Parser/parser.cc"
    break;

  case 543:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x ); }
#line 12337 "Parser/parser.cc"
    break;

  case 544:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128 ); }
#line 12343 "Parser/parser.cc"
    break;

  case 545:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128x ); }
#line 12349 "Parser/parser.cc"
    break;

  case 546:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x4 ); }
#line 12355 "Parser/parser.cc"
    break;

  case 547:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x2 ); }
#line 12361 "Parser/parser.cc"
    break;

  case 548:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat32 ); }
#line 12367 "Parser/parser.cc"
    break;

  case 549:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat64 ); }
#line 12373 "Parser/parser.cc"
    break;

  case 550:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svbool ); }
#line 12379 "Parser/parser.cc"
    break;

  case 551:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12385 "Parser/parser.cc"
    break;

  case 552:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12391 "Parser/parser.cc"
    break;

  case 553:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12397 "Parser/parser.cc"
    break;

  case 554:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 12403 "Parser/parser.cc"
    break;

  case 555:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 12409 "Parser/parser.cc"
    break;

  case 556:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 12415 "Parser/parser.cc"
    break;

  case 557:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 12421 "Parser/parser.cc"
    break;

  case 558:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 12427 "Parser/parser.cc"
    break;

  case 559:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 12433 "Parser/parser.cc"
    break;

  case 560:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 12439 "Parser/parser.cc"
    break;

  case 561:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 12445 "Parser/parser.cc"
    break;

  case 563:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12451 "Parser/parser.cc"
    break;

  case 565:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 12457 "Parser/parser.cc"
    break;

  case 566:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12463 "Parser/parser.cc"
    break;

  case 567:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12469 "Parser/parser.cc"
    break;

  case 569:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12475 "Parser/parser.cc"
    break;

  case 570:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12481 "Parser/parser.cc"
    break;

  case 571:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12487 "Parser/parser.cc"
    break;

  case 572:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 12493 "Parser/parser.cc"
    break;

  case 574:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12499 "Parser/parser.cc"
    break;

  case 576:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12505 "Parser/parser.cc"
    break;

  case 577:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12511 "Parser/parser.cc"
    break;

  case 578:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 12517 "Parser/parser.cc"
    break;

  case 579:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12523 "Parser/parser.cc"
    break;

  case 580:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 12529 "Parser/parser.cc"
    break;

  case 581:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 12535 "Parser/parser.cc"
    break;

  case 582:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 12541 "Parser/parser.cc"
    break;

  case 583:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 12547 "Parser/parser.cc"
    break;

  case 584:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 12553 "Parser/parser.cc"
    break;

  case 586:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12559 "Parser/parser.cc"
    break;

  case 587:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12565 "Parser/parser.cc"
    break;

  case 588:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12571 "Parser/parser.cc"
    break;

  case 590:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 12577 "Parser/parser.cc"
    break;

  case 591:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12583 "Parser/parser.cc"
    break;

  case 592:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 12592 "Parser/parser.cc"
    break;

  case 594:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12598 "Parser/parser.cc"
    break;

  case 595:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12604 "Parser/parser.cc"
    break;

  case 596:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12610 "Parser/parser.cc"
    break;

  case 598:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12616 "Parser/parser.cc"
    break;

  case 599:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12622 "Parser/parser.cc"
    break;

  case 601:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12628 "Parser/parser.cc"
    break;

  case 602:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12634 "Parser/parser.cc"
    break;

  case 603:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12640 "Parser/parser.cc"
    break;

  case 604:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12646 "Parser/parser.cc"
    break;

  case 605:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12652 "Parser/parser.cc"
    break;

  case 606:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12658 "Parser/parser.cc"
    break;

  case 607:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 12664 "Parser/parser.cc"
    break;

  case 608:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 12670 "Parser/parser.cc"
    break;

  case 609:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 12676 "Parser/parser.cc"
    break;

  case 611:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 12682 "Parser/parser.cc"
    break;

  case 612:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 12688 "Parser/parser.cc"
    break;

  case 613:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 12694 "Parser/parser.cc"
    break;

  case 614:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 12700 "Parser/parser.cc"
    break;

  case 615:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12706 "Parser/parser.cc"
    break;

  case 620:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 12712 "Parser/parser.cc"
    break;

  case 621:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12718 "Parser/parser.cc"
    break;

  case 622:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 12727 "Parser/parser.cc"
    break;

  case 623:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 12735 "Parser/parser.cc"
    break;

  case 624:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 12744 "Parser/parser.cc"
    break;

  case 625:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 12753 "Parser/parser.cc"
    break;

  case 626:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 12762 "Parser/parser.cc"
    break;

  case 627:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 12771 "Parser/parser.cc"
    break;

  case 629:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12777 "Parser/parser.cc"
    break;

  case 630:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12783 "Parser/parser.cc"
    break;

  case 631:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12793 "Parser/parser.cc"
    break;

  case 632:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			forall = false;								// reset
			// Create new generic declaration with same name as previous forward declaration, where the IDENTIFIER is
			// switched to a TYPEGENname. Link any generic arguments from typegen_name to new generic declaration and
			// delete newFromTypeGen.
			if ( (yyvsp[0].type)->kind == TypeData::SymbolicInst && ! (yyvsp[0].type)->symbolic.isTypedef ) {
				(yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) );
			} else {
				(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].type)->symbolic.name, (yyvsp[0].type)->symbolic.actuals, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
				(yyvsp[0].type)->symbolic.name = nullptr;			// copied to $$
				(yyvsp[0].type)->symbolic.actuals = nullptr;
				delete (yyvsp[0].type);
			}
		}
#line 12812 "Parser/parser.cc"
    break;

  case 635:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 12818 "Parser/parser.cc"
    break;

  case 636:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 12824 "Parser/parser.cc"
    break;

  case 637:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 12830 "Parser/parser.cc"
    break;

  case 638:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 12836 "Parser/parser.cc"
    break;

  case 639:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 12842 "Parser/parser.cc"
    break;

  case 640:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 12848 "Parser/parser.cc"
    break;

  case 641:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12857 "Parser/parser.cc"
    break;

  case 642:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 12863 "Parser/parser.cc"
    break;

  case 643:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12872 "Parser/parser.cc"
    break;

  case 644:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 12878 "Parser/parser.cc"
    break;

  case 645:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12887 "Parser/parser.cc"
    break;

  case 646:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12893 "Parser/parser.cc"
    break;

  case 647:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12899 "Parser/parser.cc"
    break;

  case 648:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 12912 "Parser/parser.cc"
    break;

  case 649:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 12921 "Parser/parser.cc"
    break;

  case 650:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 12927 "Parser/parser.cc"
    break;

  case 651:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12933 "Parser/parser.cc"
    break;

  case 652:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 12946 "Parser/parser.cc"
    break;

  case 653:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12952 "Parser/parser.cc"
    break;

  case 656:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 12958 "Parser/parser.cc"
    break;

  case 657:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12964 "Parser/parser.cc"
    break;

  case 660:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12970 "Parser/parser.cc"
    break;

  case 662:
#line 2744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 12976 "Parser/parser.cc"
    break;

  case 663:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 12982 "Parser/parser.cc"
    break;

  case 664:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12988 "Parser/parser.cc"
    break;

  case 665:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 12994 "Parser/parser.cc"
    break;

  case 666:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13000 "Parser/parser.cc"
    break;

  case 667:
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13006 "Parser/parser.cc"
    break;

  case 669:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13012 "Parser/parser.cc"
    break;

  case 671:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 13018 "Parser/parser.cc"
    break;

  case 672:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13024 "Parser/parser.cc"
    break;

  case 674:
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 13030 "Parser/parser.cc"
    break;

  case 675:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13036 "Parser/parser.cc"
    break;

  case 677:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13042 "Parser/parser.cc"
    break;

  case 678:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 13053 "Parser/parser.cc"
    break;

  case 679:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 13067 "Parser/parser.cc"
    break;

  case 680:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 13073 "Parser/parser.cc"
    break;

  case 681:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 13079 "Parser/parser.cc"
    break;

  case 682:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 13085 "Parser/parser.cc"
    break;

  case 683:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 13096 "Parser/parser.cc"
    break;

  case 684:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 13102 "Parser/parser.cc"
    break;

  case 685:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 13108 "Parser/parser.cc"
    break;

  case 687:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13114 "Parser/parser.cc"
    break;

  case 688:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13120 "Parser/parser.cc"
    break;

  case 689:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13126 "Parser/parser.cc"
    break;

  case 690:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 13132 "Parser/parser.cc"
    break;

  case 691:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13141 "Parser/parser.cc"
    break;

  case 692:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13150 "Parser/parser.cc"
    break;

  case 693:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 13156 "Parser/parser.cc"
    break;

  case 694:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 13162 "Parser/parser.cc"
    break;

  case 695:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 13172 "Parser/parser.cc"
    break;

  case 696:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 13178 "Parser/parser.cc"
    break;

  case 697:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 13184 "Parser/parser.cc"
    break;

  case 699:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13190 "Parser/parser.cc"
    break;

  case 700:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13196 "Parser/parser.cc"
    break;

  case 701:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13202 "Parser/parser.cc"
    break;

  case 702:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13208 "Parser/parser.cc"
    break;

  case 703:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13214 "Parser/parser.cc"
    break;

  case 704:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13220 "Parser/parser.cc"
    break;

  case 706:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13226 "Parser/parser.cc"
    break;

  case 709:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13232 "Parser/parser.cc"
    break;

  case 710:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13238 "Parser/parser.cc"
    break;

  case 711:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13244 "Parser/parser.cc"
    break;

  case 712:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13250 "Parser/parser.cc"
    break;

  case 715:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13256 "Parser/parser.cc"
    break;

  case 716:
#line 2935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13262 "Parser/parser.cc"
    break;

  case 717:
#line 2937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13268 "Parser/parser.cc"
    break;

  case 719:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13274 "Parser/parser.cc"
    break;

  case 720:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13280 "Parser/parser.cc"
    break;

  case 721:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 13286 "Parser/parser.cc"
    break;

  case 723:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13292 "Parser/parser.cc"
    break;

  case 724:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13298 "Parser/parser.cc"
    break;

  case 725:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13304 "Parser/parser.cc"
    break;

  case 726:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13310 "Parser/parser.cc"
    break;

  case 727:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13316 "Parser/parser.cc"
    break;

  case 729:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13322 "Parser/parser.cc"
    break;

  case 730:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13328 "Parser/parser.cc"
    break;

  case 731:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13334 "Parser/parser.cc"
    break;

  case 736:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13340 "Parser/parser.cc"
    break;

  case 738:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13346 "Parser/parser.cc"
    break;

  case 739:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 13352 "Parser/parser.cc"
    break;

  case 742:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13358 "Parser/parser.cc"
    break;

  case 745:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13364 "Parser/parser.cc"
    break;

  case 746:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 13370 "Parser/parser.cc"
    break;

  case 747:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 13376 "Parser/parser.cc"
    break;

  case 748:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13382 "Parser/parser.cc"
    break;

  case 749:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13388 "Parser/parser.cc"
    break;

  case 750:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13394 "Parser/parser.cc"
    break;

  case 751:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13400 "Parser/parser.cc"
    break;

  case 753:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 13406 "Parser/parser.cc"
    break;

  case 754:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 13412 "Parser/parser.cc"
    break;

  case 755:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 13418 "Parser/parser.cc"
    break;

  case 757:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 13424 "Parser/parser.cc"
    break;

  case 759:
#line 3062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 13430 "Parser/parser.cc"
    break;

  case 760:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 13436 "Parser/parser.cc"
    break;

  case 761:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13442 "Parser/parser.cc"
    break;

  case 762:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13448 "Parser/parser.cc"
    break;

  case 763:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 13454 "Parser/parser.cc"
    break;

  case 764:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13460 "Parser/parser.cc"
    break;

  case 766:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13466 "Parser/parser.cc"
    break;

  case 767:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13472 "Parser/parser.cc"
    break;

  case 768:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13478 "Parser/parser.cc"
    break;

  case 769:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 13489 "Parser/parser.cc"
    break;

  case 770:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13495 "Parser/parser.cc"
    break;

  case 771:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 13501 "Parser/parser.cc"
    break;

  case 772:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13507 "Parser/parser.cc"
    break;

  case 773:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 13516 "Parser/parser.cc"
    break;

  case 774:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 13522 "Parser/parser.cc"
    break;

  case 775:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 13532 "Parser/parser.cc"
    break;

  case 776:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13538 "Parser/parser.cc"
    break;

  case 777:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13544 "Parser/parser.cc"
    break;

  case 778:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 13550 "Parser/parser.cc"
    break;

  case 779:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13556 "Parser/parser.cc"
    break;

  case 780:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13562 "Parser/parser.cc"
    break;

  case 781:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13568 "Parser/parser.cc"
    break;

  case 782:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 13574 "Parser/parser.cc"
    break;

  case 783:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13580 "Parser/parser.cc"
    break;

  case 784:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13586 "Parser/parser.cc"
    break;

  case 787:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13592 "Parser/parser.cc"
    break;

  case 788:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13598 "Parser/parser.cc"
    break;

  case 789:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13604 "Parser/parser.cc"
    break;

  case 790:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13610 "Parser/parser.cc"
    break;

  case 792:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13616 "Parser/parser.cc"
    break;

  case 793:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 13622 "Parser/parser.cc"
    break;

  case 794:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13628 "Parser/parser.cc"
    break;

  case 795:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13634 "Parser/parser.cc"
    break;

  case 796:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 13640 "Parser/parser.cc"
    break;

  case 797:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 13646 "Parser/parser.cc"
    break;

  case 798:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 13652 "Parser/parser.cc"
    break;

  case 799:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 13661 "Parser/parser.cc"
    break;

  case 800:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 13670 "Parser/parser.cc"
    break;

  case 801:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 13679 "Parser/parser.cc"
    break;

  case 802:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 13685 "Parser/parser.cc"
    break;

  case 803:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 13694 "Parser/parser.cc"
    break;

  case 804:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 13700 "Parser/parser.cc"
    break;

  case 806:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13706 "Parser/parser.cc"
    break;

  case 811:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13712 "Parser/parser.cc"
    break;

  case 812:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13718 "Parser/parser.cc"
    break;

  case 813:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 13724 "Parser/parser.cc"
    break;

  case 814:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 13730 "Parser/parser.cc"
    break;

  case 816:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13736 "Parser/parser.cc"
    break;

  case 817:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13742 "Parser/parser.cc"
    break;

  case 818:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 13748 "Parser/parser.cc"
    break;

  case 819:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13754 "Parser/parser.cc"
    break;

  case 821:
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 13760 "Parser/parser.cc"
    break;

  case 822:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 13766 "Parser/parser.cc"
    break;

  case 823:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 13772 "Parser/parser.cc"
    break;

  case 824:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Variable declarations of anonymous types requires creating a unique type-name across multiple translation
			// unit, which is a dubious task, especially because C uses name rather than structural typing; hence it is
			// disallowed at the moment.
			if ( (yyvsp[0].decl)->linkage == ast::Linkage::Cforall && ! (yyvsp[0].decl)->storageClasses.is_static &&
				 (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->kind == TypeData::AggregateInst ) {
				if ( (yyvsp[0].decl)->type->aggInst.aggregate->aggregate.anon ) {
					SemanticError( yylloc, "extern anonymous aggregate is currently unimplemented." ); (yyval.decl) = nullptr;
				}
			}
		}
#line 13788 "Parser/parser.cc"
    break;

  case 825:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 13794 "Parser/parser.cc"
    break;

  case 826:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 13800 "Parser/parser.cc"
    break;

  case 827:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 13806 "Parser/parser.cc"
    break;

  case 828:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13812 "Parser/parser.cc"
    break;

  case 829:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13818 "Parser/parser.cc"
    break;

  case 830:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13824 "Parser/parser.cc"
    break;

  case 832:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 13833 "Parser/parser.cc"
    break;

  case 833:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 13839 "Parser/parser.cc"
    break;

  case 834:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 13848 "Parser/parser.cc"
    break;

  case 835:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 13858 "Parser/parser.cc"
    break;

  case 836:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 13867 "Parser/parser.cc"
    break;

  case 837:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13877 "Parser/parser.cc"
    break;

  case 838:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 13888 "Parser/parser.cc"
    break;

  case 839:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13898 "Parser/parser.cc"
    break;

  case 840:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 13909 "Parser/parser.cc"
    break;

  case 841:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13919 "Parser/parser.cc"
    break;

  case 842:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 13930 "Parser/parser.cc"
    break;

  case 843:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13940 "Parser/parser.cc"
    break;

  case 844:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13946 "Parser/parser.cc"
    break;

  case 846:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13952 "Parser/parser.cc"
    break;

  case 847:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 13958 "Parser/parser.cc"
    break;

  case 848:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 13964 "Parser/parser.cc"
    break;

  case 849:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 13976 "Parser/parser.cc"
    break;

  case 850:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 13987 "Parser/parser.cc"
    break;

  case 851:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 13996 "Parser/parser.cc"
    break;

  case 852:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14005 "Parser/parser.cc"
    break;

  case 853:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14011 "Parser/parser.cc"
    break;

  case 854:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14017 "Parser/parser.cc"
    break;

  case 855:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14023 "Parser/parser.cc"
    break;

  case 856:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 14032 "Parser/parser.cc"
    break;

  case 857:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14038 "Parser/parser.cc"
    break;

  case 858:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14044 "Parser/parser.cc"
    break;

  case 859:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 14050 "Parser/parser.cc"
    break;

  case 864:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 14056 "Parser/parser.cc"
    break;

  case 865:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14062 "Parser/parser.cc"
    break;

  case 866:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 14072 "Parser/parser.cc"
    break;

  case 867:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14078 "Parser/parser.cc"
    break;

  case 870:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14084 "Parser/parser.cc"
    break;

  case 871:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 14090 "Parser/parser.cc"
    break;

  case 872:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14096 "Parser/parser.cc"
    break;

  case 873:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14102 "Parser/parser.cc"
    break;

  case 874:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14108 "Parser/parser.cc"
    break;

  case 876:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14114 "Parser/parser.cc"
    break;

  case 877:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14120 "Parser/parser.cc"
    break;

  case 878:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14126 "Parser/parser.cc"
    break;

  case 879:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 14132 "Parser/parser.cc"
    break;

  case 881:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 14138 "Parser/parser.cc"
    break;

  case 882:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 14144 "Parser/parser.cc"
    break;

  case 883:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14150 "Parser/parser.cc"
    break;

  case 884:
#line 3577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14156 "Parser/parser.cc"
    break;

  case 885:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14162 "Parser/parser.cc"
    break;

  case 886:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14168 "Parser/parser.cc"
    break;

  case 888:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14174 "Parser/parser.cc"
    break;

  case 889:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14180 "Parser/parser.cc"
    break;

  case 890:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14186 "Parser/parser.cc"
    break;

  case 891:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14192 "Parser/parser.cc"
    break;

  case 892:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14198 "Parser/parser.cc"
    break;

  case 893:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14204 "Parser/parser.cc"
    break;

  case 894:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14210 "Parser/parser.cc"
    break;

  case 895:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14216 "Parser/parser.cc"
    break;

  case 896:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14222 "Parser/parser.cc"
    break;

  case 897:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14228 "Parser/parser.cc"
    break;

  case 898:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14234 "Parser/parser.cc"
    break;

  case 899:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14240 "Parser/parser.cc"
    break;

  case 900:
#line 3617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14246 "Parser/parser.cc"
    break;

  case 901:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14252 "Parser/parser.cc"
    break;

  case 902:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14258 "Parser/parser.cc"
    break;

  case 903:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14264 "Parser/parser.cc"
    break;

  case 904:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14270 "Parser/parser.cc"
    break;

  case 905:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14276 "Parser/parser.cc"
    break;

  case 907:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14282 "Parser/parser.cc"
    break;

  case 908:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14288 "Parser/parser.cc"
    break;

  case 909:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14294 "Parser/parser.cc"
    break;

  case 910:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14300 "Parser/parser.cc"
    break;

  case 911:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14306 "Parser/parser.cc"
    break;

  case 912:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14312 "Parser/parser.cc"
    break;

  case 913:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14318 "Parser/parser.cc"
    break;

  case 914:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14324 "Parser/parser.cc"
    break;

  case 915:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14330 "Parser/parser.cc"
    break;

  case 916:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14336 "Parser/parser.cc"
    break;

  case 917:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14342 "Parser/parser.cc"
    break;

  case 918:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14348 "Parser/parser.cc"
    break;

  case 919:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14354 "Parser/parser.cc"
    break;

  case 920:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14360 "Parser/parser.cc"
    break;

  case 921:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14366 "Parser/parser.cc"
    break;

  case 922:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14372 "Parser/parser.cc"
    break;

  case 926:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 14378 "Parser/parser.cc"
    break;

  case 927:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14384 "Parser/parser.cc"
    break;

  case 928:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14390 "Parser/parser.cc"
    break;

  case 929:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14396 "Parser/parser.cc"
    break;

  case 930:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14402 "Parser/parser.cc"
    break;

  case 931:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14408 "Parser/parser.cc"
    break;

  case 932:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14414 "Parser/parser.cc"
    break;

  case 933:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14420 "Parser/parser.cc"
    break;

  case 934:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14426 "Parser/parser.cc"
    break;

  case 935:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14432 "Parser/parser.cc"
    break;

  case 936:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14438 "Parser/parser.cc"
    break;

  case 937:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14444 "Parser/parser.cc"
    break;

  case 938:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14450 "Parser/parser.cc"
    break;

  case 939:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14456 "Parser/parser.cc"
    break;

  case 940:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14462 "Parser/parser.cc"
    break;

  case 941:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 14471 "Parser/parser.cc"
    break;

  case 942:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14477 "Parser/parser.cc"
    break;

  case 943:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14483 "Parser/parser.cc"
    break;

  case 945:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14489 "Parser/parser.cc"
    break;

  case 946:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14495 "Parser/parser.cc"
    break;

  case 947:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14501 "Parser/parser.cc"
    break;

  case 948:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14507 "Parser/parser.cc"
    break;

  case 949:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14513 "Parser/parser.cc"
    break;

  case 950:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14519 "Parser/parser.cc"
    break;

  case 951:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14525 "Parser/parser.cc"
    break;

  case 952:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14531 "Parser/parser.cc"
    break;

  case 953:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14537 "Parser/parser.cc"
    break;

  case 954:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14543 "Parser/parser.cc"
    break;

  case 955:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14549 "Parser/parser.cc"
    break;

  case 956:
#line 3784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14555 "Parser/parser.cc"
    break;

  case 957:
#line 3786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14561 "Parser/parser.cc"
    break;

  case 958:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14567 "Parser/parser.cc"
    break;

  case 959:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14573 "Parser/parser.cc"
    break;

  case 960:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14579 "Parser/parser.cc"
    break;

  case 961:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14585 "Parser/parser.cc"
    break;

  case 962:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14591 "Parser/parser.cc"
    break;

  case 964:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14597 "Parser/parser.cc"
    break;

  case 965:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14603 "Parser/parser.cc"
    break;

  case 966:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14609 "Parser/parser.cc"
    break;

  case 967:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14615 "Parser/parser.cc"
    break;

  case 968:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14621 "Parser/parser.cc"
    break;

  case 969:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14627 "Parser/parser.cc"
    break;

  case 970:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14633 "Parser/parser.cc"
    break;

  case 971:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14639 "Parser/parser.cc"
    break;

  case 972:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14645 "Parser/parser.cc"
    break;

  case 973:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14651 "Parser/parser.cc"
    break;

  case 974:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14657 "Parser/parser.cc"
    break;

  case 975:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14663 "Parser/parser.cc"
    break;

  case 976:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14669 "Parser/parser.cc"
    break;

  case 977:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14675 "Parser/parser.cc"
    break;

  case 978:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14681 "Parser/parser.cc"
    break;

  case 979:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14687 "Parser/parser.cc"
    break;

  case 980:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14693 "Parser/parser.cc"
    break;

  case 981:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14700 "Parser/parser.cc"
    break;

  case 983:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14706 "Parser/parser.cc"
    break;

  case 984:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14712 "Parser/parser.cc"
    break;

  case 985:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14718 "Parser/parser.cc"
    break;

  case 986:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14724 "Parser/parser.cc"
    break;

  case 987:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14730 "Parser/parser.cc"
    break;

  case 988:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14736 "Parser/parser.cc"
    break;

  case 989:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14742 "Parser/parser.cc"
    break;

  case 990:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14748 "Parser/parser.cc"
    break;

  case 991:
#line 3886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14754 "Parser/parser.cc"
    break;

  case 992:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14760 "Parser/parser.cc"
    break;

  case 993:
#line 3893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14766 "Parser/parser.cc"
    break;

  case 994:
#line 3895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14772 "Parser/parser.cc"
    break;

  case 995:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14778 "Parser/parser.cc"
    break;

  case 996:
#line 3911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14785 "Parser/parser.cc"
    break;

  case 998:
#line 3915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14791 "Parser/parser.cc"
    break;

  case 999:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14797 "Parser/parser.cc"
    break;

  case 1000:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14803 "Parser/parser.cc"
    break;

  case 1001:
#line 3924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14809 "Parser/parser.cc"
    break;

  case 1002:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14815 "Parser/parser.cc"
    break;

  case 1003:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14821 "Parser/parser.cc"
    break;

  case 1004:
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14827 "Parser/parser.cc"
    break;

  case 1005:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14833 "Parser/parser.cc"
    break;

  case 1006:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14839 "Parser/parser.cc"
    break;

  case 1007:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14845 "Parser/parser.cc"
    break;

  case 1008:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14851 "Parser/parser.cc"
    break;

  case 1010:
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14857 "Parser/parser.cc"
    break;

  case 1011:
#line 3967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14863 "Parser/parser.cc"
    break;

  case 1012:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 14869 "Parser/parser.cc"
    break;

  case 1013:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 14875 "Parser/parser.cc"
    break;

  case 1014:
#line 3976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14881 "Parser/parser.cc"
    break;

  case 1015:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14887 "Parser/parser.cc"
    break;

  case 1016:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14893 "Parser/parser.cc"
    break;

  case 1018:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14899 "Parser/parser.cc"
    break;

  case 1019:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14905 "Parser/parser.cc"
    break;

  case 1020:
#line 3990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14911 "Parser/parser.cc"
    break;

  case 1021:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 14917 "Parser/parser.cc"
    break;

  case 1022:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14923 "Parser/parser.cc"
    break;

  case 1023:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14929 "Parser/parser.cc"
    break;

  case 1024:
#line 4005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 14935 "Parser/parser.cc"
    break;

  case 1025:
#line 4007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 14941 "Parser/parser.cc"
    break;

  case 1026:
#line 4010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-3].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 14947 "Parser/parser.cc"
    break;

  case 1027:
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 14953 "Parser/parser.cc"
    break;

  case 1029:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 14959 "Parser/parser.cc"
    break;

  case 1030:
#line 4030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 14965 "Parser/parser.cc"
    break;

  case 1032:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 14971 "Parser/parser.cc"
    break;

  case 1033:
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 14977 "Parser/parser.cc"
    break;

  case 1035:
#line 4041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 14983 "Parser/parser.cc"
    break;

  case 1036:
#line 4043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 14989 "Parser/parser.cc"
    break;

  case 1037:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 14995 "Parser/parser.cc"
    break;

  case 1038:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 15001 "Parser/parser.cc"
    break;

  case 1039:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15007 "Parser/parser.cc"
    break;

  case 1040:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 15013 "Parser/parser.cc"
    break;

  case 1041:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 15019 "Parser/parser.cc"
    break;

  case 1044:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 15026 "Parser/parser.cc"
    break;

  case 1045:
#line 4098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15032 "Parser/parser.cc"
    break;

  case 1046:
#line 4100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15038 "Parser/parser.cc"
    break;

  case 1047:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15044 "Parser/parser.cc"
    break;

  case 1048:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15050 "Parser/parser.cc"
    break;

  case 1049:
#line 4109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15056 "Parser/parser.cc"
    break;

  case 1050:
#line 4111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15062 "Parser/parser.cc"
    break;

  case 1051:
#line 4113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15068 "Parser/parser.cc"
    break;

  case 1053:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15074 "Parser/parser.cc"
    break;

  case 1054:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15080 "Parser/parser.cc"
    break;

  case 1055:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15086 "Parser/parser.cc"
    break;

  case 1056:
#line 4128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15092 "Parser/parser.cc"
    break;

  case 1057:
#line 4130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15098 "Parser/parser.cc"
    break;

  case 1058:
#line 4132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15104 "Parser/parser.cc"
    break;

  case 1060:
#line 4139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15110 "Parser/parser.cc"
    break;

  case 1062:
#line 4150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15116 "Parser/parser.cc"
    break;

  case 1063:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 15122 "Parser/parser.cc"
    break;

  case 1064:
#line 4155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 15128 "Parser/parser.cc"
    break;

  case 1065:
#line 4158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 15134 "Parser/parser.cc"
    break;

  case 1066:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 15140 "Parser/parser.cc"
    break;

  case 1067:
#line 4162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 15146 "Parser/parser.cc"
    break;

  case 1069:
#line 4177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15152 "Parser/parser.cc"
    break;

  case 1070:
#line 4179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15158 "Parser/parser.cc"
    break;

  case 1071:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 15164 "Parser/parser.cc"
    break;

  case 1072:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15170 "Parser/parser.cc"
    break;

  case 1073:
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15176 "Parser/parser.cc"
    break;

  case 1074:
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15182 "Parser/parser.cc"
    break;

  case 1075:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15188 "Parser/parser.cc"
    break;

  case 1077:
#line 4198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15194 "Parser/parser.cc"
    break;

  case 1078:
#line 4200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15200 "Parser/parser.cc"
    break;

  case 1079:
#line 4202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15206 "Parser/parser.cc"
    break;

  case 1080:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15212 "Parser/parser.cc"
    break;

  case 1081:
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15218 "Parser/parser.cc"
    break;

  case 1084:
#line 4219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15224 "Parser/parser.cc"
    break;

  case 1087:
#line 4230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15230 "Parser/parser.cc"
    break;

  case 1088:
#line 4232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15236 "Parser/parser.cc"
    break;

  case 1089:
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15242 "Parser/parser.cc"
    break;

  case 1090:
#line 4236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15248 "Parser/parser.cc"
    break;

  case 1091:
#line 4238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15254 "Parser/parser.cc"
    break;

  case 1092:
#line 4240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15260 "Parser/parser.cc"
    break;

  case 1093:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15266 "Parser/parser.cc"
    break;

  case 1094:
#line 4249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15272 "Parser/parser.cc"
    break;

  case 1095:
#line 4251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15278 "Parser/parser.cc"
    break;

  case 1096:
#line 4253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15284 "Parser/parser.cc"
    break;

  case 1097:
#line 4255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15290 "Parser/parser.cc"
    break;

  case 1098:
#line 4258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15296 "Parser/parser.cc"
    break;

  case 1099:
#line 4260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15302 "Parser/parser.cc"
    break;

  case 1100:
#line 4262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15308 "Parser/parser.cc"
    break;

  case 1101:
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15314 "Parser/parser.cc"
    break;

  case 1102:
#line 4266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15320 "Parser/parser.cc"
    break;

  case 1103:
#line 4271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-2].decl) ); }
#line 15326 "Parser/parser.cc"
    break;

  case 1104:
#line 4273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), false ); }
#line 15332 "Parser/parser.cc"
    break;

  case 1105:
#line 4278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), true ); }
#line 15338 "Parser/parser.cc"
    break;

  case 1106:
#line 4280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) ), true ); }
#line 15344 "Parser/parser.cc"
    break;

  case 1108:
#line 4307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15350 "Parser/parser.cc"
    break;

  case 1112:
#line 4318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15356 "Parser/parser.cc"
    break;

  case 1113:
#line 4320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15362 "Parser/parser.cc"
    break;

  case 1114:
#line 4322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15368 "Parser/parser.cc"
    break;

  case 1115:
#line 4324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15374 "Parser/parser.cc"
    break;

  case 1116:
#line 4326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15380 "Parser/parser.cc"
    break;

  case 1117:
#line 4328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15386 "Parser/parser.cc"
    break;

  case 1118:
#line 4335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15392 "Parser/parser.cc"
    break;

  case 1119:
#line 4337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15398 "Parser/parser.cc"
    break;

  case 1120:
#line 4339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15404 "Parser/parser.cc"
    break;

  case 1121:
#line 4341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15410 "Parser/parser.cc"
    break;

  case 1122:
#line 4343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15416 "Parser/parser.cc"
    break;

  case 1123:
#line 4345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15422 "Parser/parser.cc"
    break;

  case 1124:
#line 4350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 15428 "Parser/parser.cc"
    break;

  case 1125:
#line 4352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15434 "Parser/parser.cc"
    break;

  case 1126:
#line 4354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15440 "Parser/parser.cc"
    break;

  case 1127:
#line 4359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 15446 "Parser/parser.cc"
    break;

  case 1128:
#line 4361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15452 "Parser/parser.cc"
    break;

  case 1129:
#line 4363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15458 "Parser/parser.cc"
    break;

  case 1132:
#line 4387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 15464 "Parser/parser.cc"
    break;

  case 1133:
#line 4389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 15470 "Parser/parser.cc"
    break;


#line 15474 "Parser/parser.cc"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 4392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
