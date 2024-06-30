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

ForCtrl * enumRangeCtrl( ExpressionNode * index_expr, __attribute__((unused)) OperKinds compop, ExpressionNode * range_over_expr ) {
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index_expr->expr.get()) ) {
		DeclarationNode * indexDecl = DeclarationNode::newName( new std::string(identifier->name) );
		assert( range_over_expr );
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

#line 348 "Parser/parser.cc"

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
    uuFLOAT80 = 291,
    uuFLOAT128 = 292,
    uFLOAT16 = 293,
    uFLOAT32 = 294,
    uFLOAT32X = 295,
    uFLOAT64 = 296,
    uFLOAT64X = 297,
    uFLOAT128 = 298,
    DECIMAL32 = 299,
    DECIMAL64 = 300,
    DECIMAL128 = 301,
    ZERO_T = 302,
    ONE_T = 303,
    SIZEOF = 304,
    TYPEOF = 305,
    VA_LIST = 306,
    VA_ARG = 307,
    AUTO_TYPE = 308,
    COUNTOF = 309,
    OFFSETOF = 310,
    BASETYPEOF = 311,
    TYPEID = 312,
    ENUM = 313,
    STRUCT = 314,
    UNION = 315,
    EXCEPTION = 316,
    GENERATOR = 317,
    COROUTINE = 318,
    MONITOR = 319,
    THREAD = 320,
    OTYPE = 321,
    FTYPE = 322,
    DTYPE = 323,
    TTYPE = 324,
    TRAIT = 325,
    LABEL = 326,
    SUSPEND = 327,
    ATTRIBUTE = 328,
    EXTENSION = 329,
    IF = 330,
    ELSE = 331,
    SWITCH = 332,
    CASE = 333,
    DEFAULT = 334,
    DO = 335,
    WHILE = 336,
    FOR = 337,
    BREAK = 338,
    CONTINUE = 339,
    GOTO = 340,
    RETURN = 341,
    CHOOSE = 342,
    FALLTHRU = 343,
    FALLTHROUGH = 344,
    WITH = 345,
    WHEN = 346,
    WAITFOR = 347,
    WAITUNTIL = 348,
    CORUN = 349,
    COFOR = 350,
    DISABLE = 351,
    ENABLE = 352,
    TRY = 353,
    THROW = 354,
    THROWRESUME = 355,
    AT = 356,
    ASM = 357,
    ALIGNAS = 358,
    ALIGNOF = 359,
    GENERIC = 360,
    STATICASSERT = 361,
    IDENTIFIER = 362,
    TYPEDIMname = 363,
    TYPEDEFname = 364,
    TYPEGENname = 365,
    TIMEOUT = 366,
    WAND = 367,
    WOR = 368,
    CATCH = 369,
    RECOVER = 370,
    CATCHRESUME = 371,
    FIXUP = 372,
    FINALLY = 373,
    INTEGERconstant = 374,
    CHARACTERconstant = 375,
    STRINGliteral = 376,
    DIRECTIVE = 377,
    FLOATING_DECIMALconstant = 378,
    FLOATING_FRACTIONconstant = 379,
    FLOATINGconstant = 380,
    ARROW = 381,
    ICR = 382,
    DECR = 383,
    LS = 384,
    RS = 385,
    LE = 386,
    GE = 387,
    EQ = 388,
    NE = 389,
    ANDAND = 390,
    OROR = 391,
    ATTR = 392,
    ELLIPSIS = 393,
    EXPassign = 394,
    MULTassign = 395,
    DIVassign = 396,
    MODassign = 397,
    PLUSassign = 398,
    MINUSassign = 399,
    LSassign = 400,
    RSassign = 401,
    ANDassign = 402,
    ERassign = 403,
    ORassign = 404,
    ErangeUp = 405,
    ErangeUpEq = 406,
    ErangeDown = 407,
    ErangeDownEq = 408,
    ATassign = 409,
    THEN = 410
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
#define uuFLOAT80 291
#define uuFLOAT128 292
#define uFLOAT16 293
#define uFLOAT32 294
#define uFLOAT32X 295
#define uFLOAT64 296
#define uFLOAT64X 297
#define uFLOAT128 298
#define DECIMAL32 299
#define DECIMAL64 300
#define DECIMAL128 301
#define ZERO_T 302
#define ONE_T 303
#define SIZEOF 304
#define TYPEOF 305
#define VA_LIST 306
#define VA_ARG 307
#define AUTO_TYPE 308
#define COUNTOF 309
#define OFFSETOF 310
#define BASETYPEOF 311
#define TYPEID 312
#define ENUM 313
#define STRUCT 314
#define UNION 315
#define EXCEPTION 316
#define GENERATOR 317
#define COROUTINE 318
#define MONITOR 319
#define THREAD 320
#define OTYPE 321
#define FTYPE 322
#define DTYPE 323
#define TTYPE 324
#define TRAIT 325
#define LABEL 326
#define SUSPEND 327
#define ATTRIBUTE 328
#define EXTENSION 329
#define IF 330
#define ELSE 331
#define SWITCH 332
#define CASE 333
#define DEFAULT 334
#define DO 335
#define WHILE 336
#define FOR 337
#define BREAK 338
#define CONTINUE 339
#define GOTO 340
#define RETURN 341
#define CHOOSE 342
#define FALLTHRU 343
#define FALLTHROUGH 344
#define WITH 345
#define WHEN 346
#define WAITFOR 347
#define WAITUNTIL 348
#define CORUN 349
#define COFOR 350
#define DISABLE 351
#define ENABLE 352
#define TRY 353
#define THROW 354
#define THROWRESUME 355
#define AT 356
#define ASM 357
#define ALIGNAS 358
#define ALIGNOF 359
#define GENERIC 360
#define STATICASSERT 361
#define IDENTIFIER 362
#define TYPEDIMname 363
#define TYPEDEFname 364
#define TYPEGENname 365
#define TIMEOUT 366
#define WAND 367
#define WOR 368
#define CATCH 369
#define RECOVER 370
#define CATCHRESUME 371
#define FIXUP 372
#define FINALLY 373
#define INTEGERconstant 374
#define CHARACTERconstant 375
#define STRINGliteral 376
#define DIRECTIVE 377
#define FLOATING_DECIMALconstant 378
#define FLOATING_FRACTIONconstant 379
#define FLOATINGconstant 380
#define ARROW 381
#define ICR 382
#define DECR 383
#define LS 384
#define RS 385
#define LE 386
#define GE 387
#define EQ 388
#define NE 389
#define ANDAND 390
#define OROR 391
#define ATTR 392
#define ELLIPSIS 393
#define EXPassign 394
#define MULTassign 395
#define DIVassign 396
#define MODassign 397
#define PLUSassign 398
#define MINUSassign 399
#define LSassign 400
#define RSassign 401
#define ANDassign 402
#define ERassign 403
#define ORassign 404
#define ErangeUp 405
#define ErangeUpEq 406
#define ErangeDown 407
#define ErangeDownEq 408
#define ATassign 409
#define THEN 410

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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
	CondCtl * ifctl;
	ForCtrl * forctl;
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

#line 743 "Parser/parser.cc"

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
#define YYFINAL  148
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   26247

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  183
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1118
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2202

#define YYUNDEFTOK  2
#define YYMAXUTOK   410


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
       2,     2,     2,   172,     2,     2,     2,   176,   169,     2,
     157,   159,   168,   170,   163,   171,   160,   175,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   164,   182,
     177,   181,   178,   180,   158,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   161,   174,   162,   167,     2,   166,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   165,   179,   156,   173,     2,     2,     2,
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
     155
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   643,   643,   647,   654,   655,   656,   657,   658,   662,
     663,   664,   665,   666,   667,   668,   669,   673,   674,   678,
     679,   684,   685,   686,   690,   694,   695,   706,   708,   710,
     712,   713,   715,   717,   719,   721,   731,   733,   735,   737,
     739,   741,   746,   747,   758,   763,   768,   769,   774,   780,
     782,   784,   790,   792,   796,   798,   800,   820,   823,   825,
     827,   829,   831,   833,   835,   837,   839,   841,   843,   845,
     855,   856,   860,   861,   866,   869,   873,   874,   878,   879,
     881,   883,   885,   887,   889,   894,   896,   898,   906,   907,
     915,   918,   919,   921,   926,   942,   944,   946,   948,   950,
     952,   954,   959,   961,   964,   966,   971,   973,   978,   979,
     981,   985,   986,   987,   988,   992,   993,   995,   997,   999,
    1001,  1003,  1005,  1007,  1014,  1015,  1016,  1017,  1021,  1022,
    1026,  1027,  1032,  1033,  1035,  1037,  1042,  1043,  1045,  1050,
    1051,  1053,  1058,  1059,  1061,  1063,  1065,  1070,  1071,  1073,
    1078,  1079,  1084,  1085,  1090,  1091,  1096,  1097,  1102,  1103,
    1108,  1109,  1111,  1116,  1121,  1122,  1130,  1136,  1137,  1141,
    1142,  1146,  1147,  1151,  1152,  1153,  1154,  1155,  1156,  1157,
    1158,  1159,  1160,  1161,  1171,  1173,  1178,  1179,  1181,  1183,
    1188,  1189,  1195,  1196,  1202,  1203,  1204,  1205,  1206,  1207,
    1208,  1209,  1210,  1211,  1212,  1213,  1214,  1215,  1217,  1218,
    1224,  1226,  1236,  1238,  1246,  1247,  1252,  1254,  1256,  1258,
    1260,  1264,  1265,  1267,  1273,  1302,  1305,  1307,  1309,  1319,
    1321,  1323,  1328,  1333,  1335,  1337,  1339,  1347,  1348,  1350,
    1354,  1356,  1360,  1362,  1363,  1365,  1367,  1372,  1373,  1377,
    1382,  1383,  1387,  1389,  1394,  1396,  1401,  1403,  1405,  1407,
    1412,  1414,  1416,  1418,  1423,  1425,  1430,  1431,  1453,  1455,
    1459,  1462,  1464,  1467,  1469,  1472,  1474,  1479,  1484,  1486,
    1491,  1496,  1498,  1500,  1502,  1504,  1509,  1511,  1514,  1516,
    1521,  1527,  1530,  1532,  1537,  1543,  1545,  1550,  1556,  1559,
    1561,  1564,  1566,  1571,  1578,  1580,  1585,  1591,  1593,  1598,
    1604,  1607,  1611,  1622,  1624,  1626,  1634,  1636,  1638,  1640,
    1645,  1647,  1649,  1654,  1655,  1657,  1662,  1664,  1669,  1671,
    1673,  1675,  1678,  1682,  1685,  1689,  1691,  1693,  1695,  1697,
    1699,  1701,  1703,  1705,  1707,  1709,  1714,  1715,  1719,  1725,
    1733,  1738,  1739,  1743,  1744,  1749,  1753,  1754,  1757,  1759,
    1764,  1767,  1769,  1771,  1774,  1776,  1781,  1786,  1787,  1791,
    1796,  1798,  1803,  1805,  1810,  1812,  1814,  1819,  1824,  1829,
    1834,  1836,  1838,  1843,  1845,  1851,  1852,  1856,  1857,  1858,
    1859,  1863,  1868,  1869,  1871,  1873,  1875,  1879,  1883,  1884,
    1888,  1890,  1892,  1894,  1896,  1902,  1903,  1909,  1910,  1914,
    1915,  1920,  1922,  1931,  1932,  1934,  1939,  1944,  1955,  1956,
    1960,  1961,  1967,  1968,  1972,  1974,  1978,  1980,  1984,  1985,
    1989,  1990,  1994,  1995,  1996,  2000,  2002,  2017,  2018,  2019,
    2020,  2022,  2026,  2028,  2032,  2039,  2041,  2043,  2051,  2053,
    2058,  2059,  2061,  2063,  2065,  2075,  2077,  2089,  2092,  2097,
    2099,  2105,  2110,  2115,  2126,  2133,  2138,  2140,  2142,  2148,
    2152,  2159,  2161,  2162,  2163,  2179,  2181,  2184,  2186,  2189,
    2194,  2195,  2199,  2200,  2201,  2202,  2211,  2212,  2213,  2222,
    2223,  2224,  2228,  2229,  2230,  2239,  2240,  2241,  2246,  2247,
    2256,  2257,  2262,  2264,  2268,  2270,  2272,  2274,  2281,  2286,
    2291,  2292,  2294,  2304,  2305,  2310,  2312,  2314,  2316,  2318,
    2320,  2323,  2325,  2327,  2332,  2338,  2340,  2342,  2344,  2346,
    2348,  2350,  2352,  2354,  2356,  2358,  2360,  2362,  2364,  2366,
    2368,  2370,  2372,  2374,  2376,  2378,  2380,  2382,  2384,  2386,
    2388,  2390,  2392,  2397,  2398,  2402,  2408,  2409,  2415,  2416,
    2418,  2420,  2422,  2427,  2429,  2434,  2435,  2437,  2439,  2444,
    2446,  2448,  2450,  2452,  2454,  2459,  2460,  2462,  2464,  2469,
    2471,  2470,  2474,  2482,  2483,  2485,  2487,  2492,  2493,  2495,
    2500,  2501,  2503,  2505,  2510,  2512,  2514,  2519,  2521,  2523,
    2525,  2526,  2528,  2533,  2535,  2537,  2542,  2543,  2547,  2548,
    2555,  2554,  2559,  2558,  2568,  2567,  2578,  2577,  2587,  2592,
    2593,  2598,  2604,  2622,  2623,  2627,  2629,  2631,  2636,  2638,
    2640,  2642,  2647,  2649,  2654,  2656,  2665,  2666,  2671,  2680,
    2685,  2687,  2689,  2698,  2700,  2701,  2702,  2704,  2706,  2707,
    2712,  2713,  2714,  2719,  2721,  2724,  2727,  2734,  2735,  2736,
    2742,  2747,  2749,  2755,  2756,  2762,  2763,  2767,  2775,  2782,
    2795,  2794,  2798,  2801,  2800,  2809,  2813,  2817,  2819,  2825,
    2826,  2831,  2836,  2844,  2846,  2852,  2854,  2859,  2860,  2866,
    2867,  2868,  2877,  2878,  2880,  2881,  2886,  2887,  2888,  2890,
    2896,  2897,  2899,  2900,  2901,  2903,  2905,  2912,  2913,  2915,
    2917,  2922,  2923,  2932,  2934,  2939,  2941,  2946,  2947,  2949,
    2952,  2954,  2958,  2959,  2960,  2962,  2964,  2972,  2974,  2979,
    2980,  2981,  2986,  2987,  2992,  2993,  2994,  2995,  2999,  3000,
    3005,  3006,  3007,  3008,  3009,  3023,  3024,  3029,  3030,  3036,
    3038,  3041,  3043,  3045,  3068,  3069,  3075,  3076,  3082,  3081,
    3091,  3090,  3094,  3100,  3102,  3112,  3113,  3115,  3119,  3124,
    3126,  3128,  3130,  3136,  3137,  3141,  3142,  3147,  3149,  3156,
    3158,  3159,  3161,  3166,  3168,  3170,  3175,  3177,  3182,  3187,
    3195,  3200,  3202,  3207,  3212,  3213,  3218,  3219,  3223,  3224,
    3225,  3230,  3232,  3238,  3240,  3245,  3247,  3253,  3254,  3258,
    3262,  3266,  3268,  3280,  3282,  3284,  3286,  3288,  3290,  3292,
    3293,  3298,  3301,  3300,  3312,  3311,  3324,  3323,  3337,  3336,
    3350,  3349,  3365,  3371,  3373,  3379,  3380,  3391,  3398,  3403,
    3409,  3412,  3415,  3419,  3425,  3428,  3431,  3436,  3437,  3438,
    3439,  3443,  3451,  3452,  3464,  3465,  3469,  3470,  3475,  3477,
    3479,  3484,  3485,  3491,  3492,  3494,  3499,  3500,  3502,  3537,
    3539,  3544,  3546,  3547,  3549,  3554,  3556,  3558,  3560,  3565,
    3567,  3569,  3571,  3573,  3575,  3577,  3582,  3584,  3586,  3588,
    3597,  3599,  3600,  3605,  3607,  3609,  3611,  3613,  3618,  3620,
    3622,  3624,  3629,  3631,  3633,  3635,  3637,  3639,  3651,  3652,
    3653,  3657,  3659,  3661,  3663,  3665,  3670,  3672,  3674,  3676,
    3681,  3683,  3685,  3687,  3689,  3691,  3703,  3708,  3713,  3715,
    3716,  3718,  3723,  3725,  3727,  3729,  3734,  3736,  3738,  3740,
    3742,  3744,  3746,  3751,  3753,  3755,  3757,  3766,  3768,  3769,
    3774,  3776,  3778,  3780,  3782,  3787,  3789,  3791,  3793,  3798,
    3800,  3802,  3804,  3806,  3808,  3818,  3820,  3823,  3824,  3826,
    3831,  3833,  3835,  3840,  3842,  3844,  3846,  3851,  3853,  3855,
    3869,  3871,  3874,  3875,  3877,  3882,  3884,  3889,  3891,  3893,
    3898,  3900,  3905,  3907,  3924,  3925,  3927,  3932,  3934,  3936,
    3938,  3940,  3945,  3946,  3948,  3950,  3955,  3957,  3959,  3965,
    3967,  3970,  3977,  3979,  3988,  3990,  3992,  3993,  3995,  3997,
    4001,  4003,  4008,  4010,  4012,  4014,  4049,  4050,  4054,  4055,
    4058,  4060,  4065,  4067,  4069,  4071,  4073,  4078,  4079,  4081,
    4083,  4088,  4090,  4092,  4098,  4099,  4101,  4110,  4113,  4115,
    4118,  4120,  4122,  4136,  4137,  4139,  4144,  4146,  4148,  4150,
    4152,  4157,  4158,  4160,  4162,  4167,  4169,  4177,  4178,  4179,
    4184,  4185,  4190,  4192,  4194,  4196,  4198,  4200,  4207,  4209,
    4211,  4213,  4215,  4218,  4220,  4222,  4224,  4226,  4231,  4233,
    4235,  4240,  4266,  4267,  4269,  4273,  4274,  4278,  4280,  4282,
    4284,  4286,  4288,  4295,  4297,  4299,  4301,  4303,  4305,  4310,
    4312,  4314,  4319,  4321,  4323,  4341,  4343,  4348,  4349
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
  "INT128", "UINT128", "uuFLOAT80", "uuFLOAT128", "uFLOAT16", "uFLOAT32",
  "uFLOAT32X", "uFLOAT64", "uFLOAT64X", "uFLOAT128", "DECIMAL32",
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
  "CHARACTERconstant", "STRINGliteral", "DIRECTIVE",
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
  "postfix_expression", "argument_expression_list_opt",
  "argument_expression_list", "argument_expression", "field_name_list",
  "field", "field_name", "fraction_constants_opt", "unary_expression",
  "ptrref_operator", "unary_operator", "cast_expression",
  "qualifier_cast_list", "cast_modifier", "exponential_expression",
  "multiplicative_expression", "additive_expression", "shift_expression",
  "relational_expression", "equality_expression", "AND_expression",
  "exclusive_OR_expression", "inclusive_OR_expression",
  "logical_AND_expression", "logical_OR_expression",
  "conditional_expression", "constant_expression", "assignment_expression",
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
     405,   406,   407,   408,   409,   410,   125,    40,    64,    41,
      46,    91,    93,    44,    58,   123,    96,    94,    42,    38,
      43,    45,    33,   126,    92,    47,    37,    60,    62,   124,
      63,    61,    59
};
# endif

#define YYPACT_NINF (-1873)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1117)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     144, 13283,   206,   259, 19659,   215, -1873, -1873, -1873, -1873,
   -1873, -1873, -1873, -1873, -1873, -1873, -1873, -1873,   298,   981,
     378, -1873, -1873, -1873, -1873, -1873, -1873, -1873, -1873, -1873,
   -1873, -1873, -1873, -1873, -1873, -1873, -1873, -1873, -1873, -1873,
   -1873, -1873, -1873, -1873, -1873, -1873, -1873, -1873,   171,   432,
   -1873, -1873, -1873, -1873, -1873, -1873,  5754,  5754,   381, 13283,
     395,   460, 23199, -1873,   474, -1873, -1873, -1873, -1873, -1873,
   -1873, -1873, -1873, -1873, -1873,   483,  4164, -1873,   408,   322,
   -1873, -1873, -1873, -1873, -1873, 19188, -1873, -1873,   376,   527,
     347,   384, -1873,  5518,   539,   554,   608,   493,  5578,   771,
     825, 13450, -1873, -1873,   663, 19031,  1959, -1873, -1873, -1873,
   -1873,  2565,   780,  8257,  7099,   895,  2565,  1011,   638, -1873,
   -1873, -1873, -1873,    86, -1873, -1873, -1873, -1873,   667, -1873,
   -1873, -1873, -1873, -1873,   659,   676,    86, -1873,    86, 17469,
   -1873, -1873, -1873, 20921,  5754, -1873, -1873,  5754, -1873, 13283,
   -1873,   688, 20975, -1873, -1873,  5605, 22197, -1873, -1873,   748,
     748,   735,  2741, -1873, -1873, -1873, -1873,   373, 15743,    86,
    3629,    86, -1873, -1873, -1873, -1873, -1873, -1873,   763, -1873,
     750,   779,  2479, -1873,   826, 25424, -1873, -1873, -1873, -1873,
   -1873, -1873, -1873, 17853,  2368,  4519,  4164,   598,   793,   802,
     807,   810,   817,   820, -1873, -1873, 19816, 11898,   830,   840,
   -1873, 20037, -1873, -1873, -1873, -1873,   863, -1873, -1873,   822,
   -1873, 23340,   965, 23494, -1873,   865,  5754,   676,   879,   890,
   -1873,  1803,  5605,  1803, -1873, -1873, -1873,  3594,  6269,   837,
     905,   437,   905, -1873,    86,    86,   312, 17143,   481,   905,
   -1873,    86,    86,   312,    86, -1873,    86, -1873,  6468, -1873,
   -1873,   896,   904,   748, 23006,   894, 19188, -1873,  5518, -1873,
    2565, -1873,  1755,   638,   903,   986, 17143,  5754,  5754,   347,
   -1873, 14913, -1873,   748,   748,   939,   986, 17143,  5754, -1873,
   25585, -1873, -1873, -1873,   748, -1873, -1873, -1873, -1873,   748,
   -1873,   804,  5076,  5754, -1873, 18883,   953, -1873, -1873, -1873,
   22867,   676, 17306,   936,  5605, 18829, 23006,  2565, -1873, -1873,
   22402, -1873,   905,    35, -1873, 25424, 22347,  4670,  6468, -1873,
     512, -1873, -1873, -1873, -1873, -1873, 20975,   905,  5754, -1873,
     960,   966, -1873, -1873, -1873, -1873,  5754,  3434,   354,   264,
   -1873,  5754,   750, -1873,  1049,    86, -1873,   984, 21132,   973,
   16241, 23060,  2565, -1873,  2565,   748,  2565,   748, -1873, -1873,
      86, -1873, -1873,  1003, 21186, -1873, -1873, -1873, 21343,   863,
   -1873,  3879,   308,   574, -1873,   423,   638,  1002,  1043, -1873,
    2741,   989,   750,  2741, -1873, -1873,  2368, -1873,   416, -1873,
    1018, -1873,  1024,  1108, 25588,  1132, 25672,  1135,  1158, 25424,
   25749,  1187, 23251, -1873, -1873, -1873, -1873, -1873, -1873, 25826,
   25826, 17692,  1085,  4486, -1873, -1873, -1873, -1873,   609, -1873,
     661, -1873,  1707, -1873, 25424, 25424, -1873,  1098,   650,  1067,
    1022,   478,  1175,  1130,  1153,  1123,  1196,     5, -1873,   494,
   -1873,  1178, -1873,  1194,  5642, 18336, -1873, -1873,   931,  1178,
   -1873, -1873,   744, -1873, -1873,   790,  4519,  1195,  1200,  1202,
    1217,  1225,  1228, -1873, -1873,   597,  1239, -1873,   769,  1239,
    1248, -1873,  1250, -1873, 20921, -1873,  1211,  1269, 18497, -1873,
   -1873,  4962,  3719,  1296, 16241,  1298,  1058,  1197,  1282,  1285,
   -1873, -1873, -1873,  5754,  5297, 20396, -1873, -1873, -1873, -1873,
   -1873, -1873, -1873,  9023,  3901,  1085, 23340,  1286,  1289, -1873,
   -1873,  1300, 23494,   814, -1873, -1873, -1873, 18336,  1302, -1873,
     826, -1873, -1873, -1873,  1280,  3594,   891,  1316,  1318,  1320,
     944,  1326,  1330,  1334,  1338,  1340,  1348,  6269, -1873, -1873,
   -1873,    86,  1328,  1351, -1873, -1873,  1354,   347, -1873, -1873,
     676,   986, 19982, -1873, -1873,   347, -1873, -1873,   676, -1873,
   -1873,  6468, -1873, 18336, 18336, -1873,   748,  5605,  8118,  2440,
   16407, -1873, -1873, -1873, -1873, -1873, -1873,   676,   986,    35,
    1364, -1873, -1873,  2565,  1368,   986, 17143, -1873,   676,   986,
   -1873, 25987, -1873,   748,   748, -1873, -1873,  1370,   463,  1372,
     638,  1380, -1873, -1873, -1873, 20342,  1395,  1398, -1873, -1873,
     843, -1873,  1498, -1873,  1392, -1873, -1873, -1873, 21509, 25990,
   -1873, -1873, -1873, -1873, -1873,  4670,   998,  6468, 19982,   905,
   13283, -1873,  5754,  1415, -1873,  1422, -1873, -1873, -1873, -1873,
   -1873,  2741, -1873, -1873,  1511,  5094, 20553, 11898, -1873, 21563,
   -1873,   748,   748, -1873, -1873,   863, -1873, 15245,  1436,  1585,
   25424,  1467,  1354,  1424, -1873,    86,    86, -1873,  1239, -1873,
   21132, -1873, -1873, 20342,   748,   748, -1873,  5094, -1873, -1873,
   22142, -1873, -1873, 21186, -1873,    86,  1442,    86,  1043,   295,
    1448,   850, 20975,   860,   899, -1873,  2368, 23571,  1434, -1873,
   18014, -1873,  4486, 18175, -1873, 21720, 20975, -1873, 18014, -1873,
   25424, -1873, -1873, -1873, -1873, -1873, -1873, 18175, -1873, -1873,
   20607, 21720, 21720,  1219,  1174,  1399,   434,  1488, -1873,   920,
    1462,  1220,  1466, -1873, 23648, 25424, 23725,  1468, 25424,  1803,
   25424,  1803, -1873,  2514, -1873, -1873, 23571,  3286, 25424, 23571,
    1803, -1873, -1873, 25424, 25424, 25424, 25424, 25424, 25424, 25424,
   25424, 25424, 25424, 25424, 25424, 25424, 25424, 25424, 25424, 25424,
   25424, 25424, 23802,  1450,   826,  4699, 11898, -1873, -1873, -1873,
   -1873, -1873, -1873, -1873, -1873, -1873, -1873, -1873,  1469, 25424,
   -1873, -1873, 15411,  2149, -1873, -1873,    86,    86, -1873, -1873,
   18336, -1873, -1873,   653,  1239, -1873,   925,  1239, 19982, -1873,
   -1873,  1354, 19982, -1873,  1354, -1873, 26074, -1873, -1873, -1873,
   19502, 11898,  1476,  1230,  1478, 14747,  1623,  4855,   656,  1424,
   -1873,    86,    86,  1424,   662, -1873,    86,    86, 25424,  5754,
   16407,  1481, 16407,  1482,  1424,    -6, 15577, 15577, 16573, 15577,
    5754, -1873, -1873, 25424,  1300, -1873, 23340,  1489, -1873,  2349,
   -1873, -1873, -1873,   951, -1873,  1490, 15577, 25424,   958,  1492,
    1495,  1496,   963,  1497,  1500,  1502,  1503,  1504,  1506,   694,
    1239, -1873, -1873,   695,  1239, -1873, -1873,   706,  1239, -1873,
   -1873, -1873,  5605,  1645,  1239, 22552, -1873, -1873,   676,  1509,
   -1873, -1873, -1873,   969,  1512,  1015,  1513, -1873,  1248,  1508,
    1519, -1873,   676, -1873,  1523, -1873,   676,   986,  1519, -1873,
     676,  1515,  1517,  1518, -1873, -1873, 20203, -1873,  1803,  5754,
   11017,  1613, -1873,  1269, -1873, 15577,  1020, -1873,  1519,  1526,
   -1873, 21774, 18336,  1514, -1873,  1514, -1873, -1873, -1873, -1873,
   21186, -1873, 12068, 18658, -1873,  1528,  1530,  1532,  1537, -1873,
   12987,    86, -1873,  1467, -1873, -1873, -1873, -1873,  1354, -1873,
   -1873, -1873,   748, -1873, -1873, -1873, -1873,   295,  1043,  1534,
     373, -1873, -1873,  1539,  5754,   295, -1873, -1873,  1540,  1547,
   -1873, -1873,  1021, -1873, -1873, -1873, -1873,  1551,  1552,  1550,
    1555,  1553,  1556,  1561,  1562,  1564,  1563,  1565, 25424,  1566,
    1569,  1570, 21931, 12238, 25424, -1873, -1873,  1510, -1873, -1873,
   -1873, 25424, -1873,  1573,  1574, 23417, -1873, -1873,  1232, -1873,
   23571,  1576, -1873,  1577, -1873, -1873,  5194, -1873,  1069, -1873,
    5194, -1873, -1873,  1241,   641, -1873, -1873,  1098,  1098,  1098,
     650,   650,  1067,  1067,  1022,  1022,  1022,  1022,   478,   478,
    1175,  1130,  1153,  1123,  1196, 25424,  1258, -1873,  1581,  5194,
   -1873, -1873, 23340, -1873,  1582,  1587,  1589,  1591,  2149, -1873,
   -1873, -1873, -1873, -1873, 19982, -1873, -1873,  1354, 19982, -1873,
    1354,  1592,  1594, 15577, 15577, -1873, -1873, 14747,  1042,  1595,
    1597,  1598,  1599,  2831,  4855, -1873, -1873, 19982, -1873, -1873,
   -1873, -1873, -1873, -1873, 19982, -1873, -1873, -1873, -1873,  1590,
   -1873,  1424,  1572, -1873, -1873, -1873, -1873, -1873, -1873, -1873,
   -1873,  1600,  1601,  1602, -1873,  1604, -1873,   347,  5194,  1261,
     158, -1873, -1873,  1588, -1873, 23494, -1873, 25424,    86, 15577,
      86, -1873, -1873,   721,  1239, -1873,   762,  1239, -1873, -1873,
     767,  1239, 19982, -1873, -1873,  1354, 19982, -1873, -1873,  1354,
   19982, -1873, -1873,  1354,   905, -1873,  1354,    31, -1873,  1178,
    1605, -1873, -1873, -1873, -1873, -1873, -1873,  1606, -1873, -1873,
   -1873, 21774,  1519, -1873,   676, -1873, -1873, -1873, -1873, -1873,
    9992, -1873, -1873, -1873, -1873, -1873,   323,   570,   324, 11728,
    1617,  1618, 16963,  1620,  1621,  3063,  3184,  4336, 23879,  1622,
   -1873, -1873,  1625,  1626, 16963,  1628, -1873, -1873,   676, 25424,
   25424,  1747,  1616,   462, -1873, 17531,  1276,  1629,  1624,  1610,
   -1873, -1873, -1873, 10837, -1873, -1873, -1873, -1873, -1873,  2659,
   -1873, -1873, -1873,  1349,    43, -1873,   352, -1873,    43, -1873,
   -1873, -1873, -1873, -1873,  1803, -1873, -1873, 13617, 19345, -1873,
    5754,  1627,  1632, -1873, -1873, -1873,  5754, -1873, -1873,  5605,
   -1873, -1873,  1619,  1630,  1071, 20975,   750,   750, -1873, -1873,
    1085,  1269, 18497, -1873,  1178, -1873, 12408, -1873,   798,  1239,
   -1873,   748, 13112, -1873, -1873,  1043,  1539,  1634,   295,   638,
     279,  1646,  1642,  1539,  1647, -1873, -1873, 23571,   572, -1873,
   20342,   572, 12238,  1803, -1873,   572, -1873, 20764,   572, -1873,
   25424, 25424, 25424, -1873, -1873, -1873, -1873, 25424, 25424,  1640,
   23340, -1873, -1873, 23956,  1643,   646, -1873, -1873, -1873,  2212,
   -1873, -1873,  1293, -1873,   204, -1873,  1295, -1873, 23648, -1873,
   -1873, 25424,  1644,  1304,  1308,  1300, -1873,   873,  1239, -1873,
   -1873,  1648,  1649, -1873, -1873, -1873, -1873,  1651,   898,  1239,
   -1873,   930,  3048,    86,    86, -1873, -1873,  1665,  1666, -1873,
    1667, -1873, 16407,  1674, -1873, 15909, 16075,  1669, 16573,  1678,
   -1873,  1675, 25424, 25424,  1310,  1679, -1873, -1873, -1873, -1873,
   -1873, -1873,  1684, 19982, -1873, -1873,  1354, 19982, -1873, -1873,
    1354, 19982, -1873, -1873,  1354,  1685,  1686,  1689,   347, -1873,
   -1873,  1329, 25424, 22706,  1687,  1695, -1873, -1873, -1873,  1697,
   14111, 14271, 14431, 21774, 23006, 21720, 21720,  1698, -1873,   401,
     499,  3864, 14581, -1873,   515,  5754,  5754, -1873, 23571,   -74,
     263, -1873, -1873, -1873, -1873, 11728, 25424,  1699,  1773, 11557,
   11197, -1873,  1676, -1873,  1681, 25424,  1693, 23340,  1694, 25424,
   18336, 25424, -1873, 11377,   980, -1873,  1696,     6, -1873,   142,
    1784,   396,    86, -1873,  1710, -1873,  1701, -1873,  1703,  1722,
    1724, 16963, 16963, -1873, -1873,  1799, -1873, -1873,    20,    20,
     697, 15079,   519, -1873, -1873,  1732,  1741,   354, -1873,  1749,
   -1873,  1740, -1873,  1744, -1873, -1873, -1873, -1873, 12578,  1746,
    1760,  1762, -1873, 19982, -1873, -1873,  1354, 25424, 25424,  1269,
    1764, -1873,  1763,  1753,   295,  1539,   373,  5754, -1873, 24033,
   -1873,  1771, -1873, 21774, -1873,   976,  1752,  1766,  1076, -1873,
    1767, -1873, -1873, -1873, -1873, -1873, 23340,  1300, -1873, -1873,
   23648, -1873,  1808,  5194, -1873,  1808,  1808, -1873,  5194,  3345,
    4105, -1873,  1333, -1873, -1873, -1873,  1777, 19982, -1873, -1873,
    1354, -1873, -1873,  1775,  1776,    86, 19982, -1873, -1873,  1354,
   19982, -1873, -1873,  1780, -1873, -1873, -1873, -1873, -1873, -1873,
   -1873, -1873,  1572, -1873, -1873, -1873,  1774, -1873, -1873, -1873,
   -1873,  1778,  1790,    86,  1791,  1792,  1793, -1873, -1873, -1873,
   -1873, 25424, -1873,    31, -1873,  1178, -1873, -1873,  1798,  1801,
   -1873,  1698,  1698,  1698,  4976,   992,  1796,   521, -1873,  4976,
     526, 18336, -1873, -1873, -1873,  4840, 25424,  6883,   421, -1873,
   -1873,    85,  1795,  1795,  1795,  5754, -1873, -1873, -1873,  1091,
   -1873, -1873, -1873, -1873,  1624,  1813, 25424,   376,  1812,   493,
    9486, 21774,  1094,  1824, 16963,  1825, -1873, -1873, -1873, -1873,
    1145, 16963, 25424,   781,   685, -1873, 25424, 23258, -1873, -1873,
     530, -1873,  1300, -1873,  1101,  1103,  1112,   686, -1873, -1873,
   -1873, -1873,   676,   980,  1826, -1873, -1873, 25424, -1873,  1828,
     826, -1873, 10656, -1873, -1873, -1873, 25424, 25424, -1873, -1873,
     192,    20, -1873,   383, -1873, -1873, -1873,    86, -1873,  1514,
   -1873, 21774, -1873, -1873, -1873, -1873, -1873,  1827,  1834, -1873,
   -1873,  1832, -1873,  1835,   295, -1873,  1539,  1830,   638,  1642,
   23340, -1873, -1873, -1873,  1837, -1873, -1873, 25424, -1873, 20764,
   25424,  1300,  1831,  1369, -1873,  1379, -1873,  5194, -1873,  5194,
   -1873, -1873, -1873,  1840,    86,    86,  1843,  1844, -1873,  1845,
   -1873, -1873, -1873, -1873, -1873,  1385, 25424, -1873, -1873, -1873,
   -1873, -1873,   536,   992,  2371,   560, -1873, -1873, -1873, -1873,
      86,    86, -1873, -1873, -1873,   562, -1873,  1115,  4840,   730,
   -1873,  6883, -1873,    86, -1873, -1873, -1873, -1873, -1873, -1873,
   16963, 16963,  1624, 16739,   155, 24110,  1929, 16963, -1873, -1873,
   -1873, -1873, -1873, 25424, -1873, 24187,  1930,  1829, 10520, 24264,
   16963, 11377,  1624,   495,  1054,  1836, 25424, -1873,  1854,   391,
   16963, -1873, 16963, -1873,  1856, -1873, -1873,  1839,   826,   702,
   -1873, -1873,  1864,  1388,  1128, 16963,  1869, 16963, 16963, 16963,
   -1873,   750, -1873,  5754,  5605, -1873, -1873,  1867,  1868, -1873,
   -1873,  1539,  1875, -1873, -1873,  1300,  1881, -1873, -1873, -1873,
   -1873,  1882, -1873, -1873, -1873,  1400,  1402, -1873, -1873, -1873,
   -1873, -1873, -1873, -1873, -1873, -1873,  1883,  1886,  1887,  2371,
   -1873,    86, -1873, -1873, -1873, -1873, -1873,  1888,  4976, -1873,
    1971,  7409,   126, 12751, -1873, 16837, -1873,    66,  1141, 16963,
    1972,   590,  1884,   181, 16963, 25424,  5094,   495,  1054,  1872,
   -1873, 24346,  1178,  1885,   351,  1984, -1873, 24423, 24500, 25424,
    1624,  1879, 12920, -1873, -1873, -1873, -1873, 21985, -1873,  1898,
    1891,    80, -1873, 25424, 23571, -1873, -1873, 25424,    43, -1873,
   -1873, -1873, -1873, -1873, -1873, -1873,  1907, -1873,  1908, -1873,
   -1873, -1873, -1873,   932,  1239, -1873, -1873,   992, -1873, 16963,
   -1873,   252, -1873,   111, -1873, -1873, -1873,  1909, 13784, -1873,
   -1873, 16963, -1873,   115, -1873, 16963, 25424,  1910, 24577, -1873,
   -1873, -1873,   638, 24654, 24731, 25424,  1624, -1873, 24808, 24885,
   16963,  1893,   429,  1901,   613, -1873, -1873,  1919, 13784, 21985,
   -1873,  5064, 21563,  1803,  1913, -1873,  1973,  1921,   728,  1920,
   -1873, -1873,  1142,  1163,   327, -1873, -1873, 19982, -1873, -1873,
    1354, -1873, -1873, 25424, -1873, 25424, -1873, -1873,  1493, 13951,
   -1873, -1873, 16963, -1873, -1873,  1624, -1873, -1873,  1624,  1911,
     615,  1914,   628, -1873, -1873,  1624, -1873,  1624, -1873,  1928,
   24962, 25039, 25116, -1873,  1493, -1873,  1906,  2980,  5861, -1873,
   -1873, -1873,    80,  1927, 25424,  1912,    80,    80, -1873, -1873,
   16963,  2014,  1933, -1873, -1873, 16837, -1873,  1493, -1873, -1873,
    1935, 25193, 25270, 25347, -1873, -1873,  1624, -1873,  1624, -1873,
    1624, -1873,  1906, 25424,  1936,  5861,  1932,   826,  1940, -1873,
     738, -1873, -1873, 16963, -1873, -1873, 10221,  1944, 16837, -1873,
   -1873,  1624, -1873,  1624, -1873,  1624,  1946,  1951, -1873,   676,
     826,  1948, -1873,  1931,   826, -1873, -1873, -1873, -1873, 10436,
   -1873,   676, -1873, -1873,  1403, 25424, -1873,  1166, -1873,   826,
    1803,  1952,  1938, -1873, -1873,  1177, -1873, -1873,  1941,  1803,
   -1873, -1873
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   498,     0,     2,   498,   515,   516,   517,   518,   519,
     520,   521,   522,   523,   504,   506,   505,   507,     0,     0,
       0,   525,   527,   548,   528,   549,   531,   532,   546,   547,
     526,   544,   545,   529,   530,   533,   534,   535,   536,   537,
     538,   539,   540,   541,   542,   543,   550,   551,   854,   553,
     626,   627,   630,   632,   628,   634,     0,     0,     0,   498,
       0,     0,    17,   597,   603,     9,    10,    11,    12,    13,
      14,    15,    16,   811,   110,     0,     0,    20,     0,     2,
     108,   109,    18,    19,   869,   498,   812,   434,     0,   437,
     734,   439,   450,   852,   438,   472,   473,     0,     0,     0,
       0,   580,   500,   502,   508,   498,   510,   513,   565,   524,
     552,   482,   558,   563,   484,   575,   483,   590,   594,   600,
     579,   606,   618,   854,   623,   624,   607,   676,   440,   441,
       3,   819,   832,   503,     0,     0,   854,   891,   854,   498,
     908,   909,   910,   498,     0,  1095,  1096,     0,     1,   498,
      17,     0,   498,   461,   462,     0,   580,   508,   492,   493,
     494,   822,     0,   629,   631,   633,   635,     0,   498,   854,
     679,   855,   856,   625,   554,    22,    23,    21,   788,   783,
     773,     0,   863,   820,     0,     0,   515,   813,   817,   818,
     814,   815,   816,   498,   863,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   598,   601,   498,   498,     2,     0,
    1097,   580,   898,   916,  1101,  1094,  1092,  1099,   433,     0,
     172,   740,   171,     0,   442,     0,     0,     0,     0,     0,
     448,     0,     0,     0,   432,   985,   986,     0,     0,   471,
     852,   854,   852,   872,   854,   854,   481,   498,   854,   852,
     929,   854,   854,   480,   854,   948,   854,   926,     0,   573,
     574,     0,     0,   498,   498,     2,   498,   451,   852,   501,
     511,   566,     0,   595,     0,   835,   498,     0,     0,   734,
     452,   580,   559,   576,   591,     0,   835,   498,     0,   514,
     560,   567,   568,   485,   577,   487,   488,   486,   582,   592,
     596,     0,   610,     0,   805,   498,     2,   833,   890,   892,
     498,     0,   498,     0,     0,   580,   498,   510,     2,  1105,
     580,  1108,   852,   852,     3,     0,   580,     0,     0,   464,
     854,   847,   849,   848,   850,     2,   498,   852,     0,   809,
       0,     0,   769,   771,   770,   772,     0,     0,   765,     0,
     754,     0,   763,   775,     0,   854,   677,     2,   498,  1117,
     499,   498,   489,   558,   490,   583,   491,   590,   587,   608,
     854,   609,   722,     0,   498,   723,  1070,  1071,   498,   724,
     726,   679,   597,   603,   680,   681,   682,     0,   679,   857,
       0,   786,   774,     0,   868,   867,   863,   866,     0,   861,
     864,    25,     0,    24,     0,     0,     0,     0,     0,     0,
       0,     0,    27,    29,     4,     8,     5,     6,     7,     0,
       0,   498,     2,     0,   111,   112,   113,   114,    91,    28,
      92,    46,    90,   115,     0,     0,   130,   132,   136,   139,
     142,   147,   150,   152,   154,   156,   158,   160,   163,     0,
      30,     0,   604,     2,   115,   498,   164,   780,   730,   594,
     732,   779,     0,   729,   733,     0,     0,     0,     0,     0,
       0,     0,     0,   870,   896,   854,   906,   914,   918,   924,
     597,     2,     0,  1103,   498,  1106,     2,   108,   498,     3,
     721,     0,  1117,     0,   499,   558,   583,   590,     3,     3,
     717,   707,   711,   723,   724,   498,     2,     2,   899,   917,
    1093,     2,     2,    27,     0,     2,   740,    28,     0,   738,
     741,  1115,     0,     0,   747,   736,   735,   498,     0,   837,
       0,     2,   463,   465,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   875,   932,
     955,   854,   477,     2,   871,   879,  1013,   734,   873,   874,
       0,   835,   498,   928,   936,   734,   930,   931,     0,   947,
     949,     0,   467,   498,   498,   564,   499,     0,   580,     0,
     498,  1098,  1102,  1100,   449,   581,   809,     0,   835,   852,
       0,   443,   453,   512,     0,   835,   498,   809,     0,   835,
     784,   561,   562,   578,   593,   599,   602,   597,   603,   621,
     622,     0,   785,   693,   727,   499,     0,   694,   696,   697,
       0,   212,   426,   834,     0,   424,   481,   480,   580,     0,
     445,     2,   446,   806,   469,     0,     0,     0,   498,   852,
     498,   809,     0,     0,     2,     0,   768,   767,   766,   760,
     509,     0,   758,   776,   556,     0,   498,   498,  1072,   499,
     495,   496,   497,  1076,  1067,  1068,  1074,   498,     2,   109,
       0,  1032,  1046,  1117,  1028,   854,   854,  1037,  1044,   715,
     498,   588,   725,   499,   584,   585,   589,     0,   678,  1082,
     499,  1087,  1079,   498,  1084,   854,     0,   854,   679,   679,
       0,     0,   498,     0,     0,   859,   863,    70,     0,    26,
     498,    98,     0,   498,   107,   498,   498,    93,   498,   100,
       0,    36,    40,    41,    37,    38,    39,   498,    96,    97,
     498,   498,   498,     2,   111,   112,     0,     0,   190,     0,
       0,   624,     0,  1092,     0,     0,     0,     0,     0,     0,
       0,     0,    59,     0,    65,    66,    70,     0,     0,    70,
       0,    94,    95,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   498,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   183,   171,     0,
     169,   170,   498,   997,   731,   994,   854,   854,  1002,   605,
     498,   860,   897,   854,   907,   915,   919,   925,   498,   900,
     902,   904,   498,   920,   922,     2,     0,     2,  1104,  1107,
     498,   498,     0,     2,     0,   498,   109,  1032,   854,  1117,
     967,   854,   854,  1117,   854,   982,   854,   854,     3,   725,
     498,     0,   498,     0,  1117,  1117,   498,   498,   498,   498,
       0,     2,   749,     0,  1115,   746,  1116,     0,   742,     0,
       2,   745,   748,     0,     2,     0,   498,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   854,
     884,   888,   927,   854,   941,   945,   953,   854,   963,   876,
     933,   956,     0,     0,  1009,     0,   475,   838,     0,     0,
     476,   839,   468,     0,     0,     0,     0,   466,     0,     2,
       2,   840,     0,   447,     2,   809,     0,   835,     2,   841,
       0,     0,     0,     0,   636,   893,   498,   911,     0,     0,
     498,   427,   425,   108,     3,   498,     0,   810,     2,     0,
     762,   498,   498,   756,   755,   756,   557,   555,   681,  1078,
     498,  1083,   499,   498,  1069,     0,     0,     0,     0,  1047,
       0,   854,  1118,  1033,  1034,   716,  1030,  1031,  1045,  1073,
    1077,  1075,   586,   621,  1081,  1086,   673,   679,   679,     0,
       0,   688,   687,  1115,     0,   679,   789,   787,     0,     0,
     862,    74,     0,    71,    72,    75,   821,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   498,   498,     0,   129,   128,     0,   125,   124,
      31,     0,    32,     0,     0,     0,   187,   186,     0,     3,
      70,     0,    55,     0,    56,    63,     0,    62,     0,    58,
       0,    57,    61,     0,     0,    54,   131,   133,   134,   135,
     137,   138,   140,   141,   145,   146,   143,   144,   148,   149,
     151,   153,   155,   157,   159,     0,     0,   436,     0,     0,
      33,     3,   740,   165,     0,     0,     0,     0,   998,   999,
     995,   996,   782,   781,   498,   901,   903,   905,   498,   921,
     923,     0,     0,   498,   498,  1023,  1022,   498,     0,     0,
       0,     0,     0,   854,  1033,   970,   987,   498,   965,   973,
     713,   968,   969,   714,   498,   980,   990,   983,   984,     0,
       3,  1117,     3,   709,   459,   708,   712,  1109,   718,   719,
     701,     0,   702,   703,     3,     3,     3,   734,     0,   163,
       0,     3,     3,     0,   743,     0,   737,     0,   854,   498,
     854,     3,   470,   854,   885,   889,   854,   942,   946,   954,
     854,   964,   498,   877,   880,   882,   498,   934,   937,   939,
     498,   957,   959,   961,   852,   478,  1010,     3,  1014,  1015,
       3,   843,   950,   570,   569,   572,   571,     2,   810,   844,
     791,   498,     2,   842,     0,   810,   845,   636,   636,   636,
     498,   695,   698,   699,   728,   430,     0,     0,     0,   498,
       0,     0,   351,     0,     0,     0,     0,     0,   192,     0,
     346,   347,     0,     0,   351,     0,   399,   398,     0,   167,
     167,   405,   597,   603,   209,   498,     2,     0,   193,     0,
     220,   194,   195,   498,   214,   196,   197,   198,   199,     0,
     200,   201,   352,     0,   366,   202,   372,   374,   377,   203,
     204,   205,   206,   207,     0,   208,   216,   580,   498,   218,
       0,     0,     0,     3,   823,   810,     0,   798,   799,     0,
       3,   794,     3,     3,     0,   498,   773,   773,  1080,  1085,
       2,   108,   498,     3,   595,     3,   499,  1041,   854,  1040,
    1043,   498,     3,  1029,  1035,   679,  1115,     0,   679,   684,
     679,     0,   689,  1115,     2,   858,   865,     0,    99,   102,
     498,   106,   498,     0,   105,   101,   103,   498,     0,   119,
       0,     0,     0,   123,   127,   126,   191,     0,     0,     0,
     740,   116,   184,     0,     0,     0,    49,    50,    88,     0,
      88,    88,     0,    76,    78,    52,     0,    48,     0,    51,
     162,     0,     0,     0,     0,  1115,  1006,   854,  1005,  1008,
    1000,     0,     0,   894,   912,     3,     3,     0,   854,   976,
     979,   854,     0,   854,   854,   971,   988,     0,     0,  1110,
       0,   720,   498,     0,  1112,   498,   498,     0,   498,     0,
     444,     3,     0,     0,     0,     0,   739,   744,     3,   836,
       3,   853,     0,   498,   878,   881,   883,   498,   935,   938,
     940,   498,   958,   960,   962,     0,     0,     0,   734,  1021,
    1020,     0,     0,     0,     0,     0,     3,   810,   846,     0,
     498,   498,   498,   498,   498,   498,   498,   619,   649,     0,
       0,   650,   580,   637,     0,     0,     0,   428,    70,     0,
       0,   337,   338,   217,   219,   498,     0,     0,     0,   498,
     498,   333,     0,   331,     0,     0,     0,   740,     0,     0,
     498,     0,   378,   498,     0,   168,     0,     0,   406,     0,
       0,     0,   854,   224,     0,   215,     0,   328,     0,     0,
       0,   351,   351,   357,   356,   351,   368,   367,   351,   351,
       0,   580,     0,  1025,  1024,     0,     0,   765,   801,     2,
     796,     0,   797,     0,   777,   757,   761,   759,   498,     0,
       0,     0,     3,   498,  1036,  1038,  1039,     0,     0,   108,
       0,     3,     0,     0,   679,  1115,     0,     0,   668,     0,
     683,     0,   790,   498,    73,  1026,     0,     0,     0,    42,
       0,   120,   122,   121,   118,   117,   740,  1115,   189,   188,
       0,    69,    85,     0,    79,    86,    87,    64,     0,     0,
       0,    60,     0,   161,   435,    34,     0,   498,  1001,  1003,
    1004,   895,   913,     0,     0,   854,   498,   972,   974,   975,
     498,   989,   991,     0,   966,   981,   977,   992,  1111,   710,
     460,   705,   704,   706,  1114,  1113,     0,     3,   851,   750,
     751,     0,     0,   854,     0,     0,     0,   886,   943,   951,
     479,     0,  1016,     0,  1017,  1018,  1012,   827,     2,     0,
     829,   619,   619,   619,   650,   657,   624,     0,   663,   650,
       0,   498,   611,   648,   644,     0,     0,     0,     0,   651,
     653,   854,   665,   665,   665,     0,   645,   661,   431,     0,
     341,   342,   339,   340,   233,     0,     0,   235,   439,   234,
     580,   498,     0,     0,   351,     0,   316,   318,   317,   319,
       0,   351,   192,   273,     0,   266,     0,   192,   334,   332,
       0,   326,  1115,   335,     0,     0,     0,     0,   387,   388,
     389,   390,     0,   380,     0,   381,   343,     0,   344,     0,
       0,   371,     0,   213,   330,   329,     0,     0,   360,   370,
       0,   351,   373,     0,   375,   397,   429,   854,   825,   756,
     778,   498,     2,     2,  1088,  1089,  1090,     0,     0,     3,
       3,     0,  1049,     0,   679,   669,  1115,     0,   686,   689,
     740,   690,   672,     3,     0,  1027,   104,     0,    35,   498,
       0,  1115,     0,     0,    89,     0,    77,     0,    83,     0,
      81,    47,   166,     0,   854,   854,     0,     0,   753,     0,
     454,   458,   887,   944,   952,     0,     0,   793,   831,   615,
     617,   613,     0,     0,  1056,     0,   658,  1061,   660,  1053,
     854,   854,   643,   664,   647,     0,   646,     0,     0,     0,
     667,     0,   639,   854,   638,   654,   666,   655,   656,   662,
     351,   351,   236,   580,     0,     0,   254,   351,   321,   324,
     322,   325,   320,     0,   323,     0,   262,     0,   192,     0,
     351,   498,   274,     0,   299,     0,     0,   327,     0,     0,
     351,   350,   351,   391,     0,   382,     2,     0,     0,     0,
     211,   210,   353,     0,     0,   351,     0,   351,   351,   351,
     457,   773,   795,     0,     0,  1091,  1042,     0,     0,  1048,
    1050,  1115,     0,   671,   685,  1115,     2,    53,    45,    43,
      44,     0,    67,   185,    80,     0,     0,  1007,   456,   455,
     978,   993,   752,  1011,  1019,   641,     0,     0,     0,  1057,
    1058,   854,   642,  1054,  1055,   640,   620,     0,     0,   349,
     225,     0,     0,     0,   247,   351,   227,     0,     0,   351,
     256,   271,   282,   276,   351,   192,     0,     0,   286,     0,
     311,     0,   313,   277,   275,   264,   267,     0,     0,   192,
     300,     0,     0,   230,   348,   379,     2,   498,   345,     0,
       0,   407,   358,     0,    70,   369,   362,     0,   363,   361,
     376,   764,   800,   802,  1051,  1052,     0,   675,     0,   792,
      68,    84,    82,   854,  1064,  1066,  1059,     0,   652,   351,
     242,   237,   240,     0,   239,   246,   245,     0,   498,   249,
     248,   351,   258,     0,   255,   351,     0,     0,     0,   263,
     268,   314,   315,     0,     0,   192,   287,   312,     0,     0,
     351,     0,   302,   303,   301,   270,   336,     0,   498,   498,
       3,   392,   499,   396,     0,   400,     0,     0,     0,   408,
     409,   354,     0,     0,     0,   674,   691,   498,  1060,  1062,
    1063,   659,   226,     0,   244,     0,   243,   229,   250,   498,
     420,   259,   351,   260,   257,   272,   285,   283,   279,   291,
     289,   290,   288,   269,   284,   280,   281,   278,   265,     0,
       0,     0,     0,   232,   250,     3,   385,     0,  1056,   393,
     394,   395,   407,     0,     0,     0,   407,     0,   359,   355,
     351,     0,     0,   238,   241,   351,     3,   251,   421,   261,
       0,     0,     0,     0,   310,   308,   305,   309,   306,   307,
     304,     3,   385,     0,     0,  1057,     0,     0,     0,   401,
       0,   410,   364,   351,  1065,   221,     0,     0,   351,   298,
     296,   293,   297,   294,   295,   292,     0,     0,   386,     0,
     413,     0,   411,     0,   413,   365,   223,   222,   228,     0,
     231,     0,   383,   414,     0,     0,   402,     0,   384,     0,
       0,     0,     0,   415,   416,     0,   412,   403,     0,     0,
     404,   417
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1873,  6154,  3978, -1873,    -1,   684,  2329,  5275,  -170, -1873,
    -334, -1873,   335, -1873,  -719, -1873,   776,  -984, -1175, -1873,
     224,  3172,  2032, -1873,   974, -1873,  1382,   374,   828,   823,
     592,   836,  1345,  1347,  1350,  1346,  1355, -1873,  -169,  -132,
    8929,   887, -1873,  1692, -1873, -1873, -1289,  8112, -1182,  2537,
   -1873,  -102, -1873,   875,   -19, -1873, -1873,   668,    65, -1873,
   -1872, -1766,   272,    44, -1873, -1873,   657,   288,   190, -1619,
   -1873, -1415, -1873, -1873, -1873, -1873,    89, -1082, -1873, -1873,
   -1237,   414, -1873, -1873, -1873, -1873, -1873,    82, -1168, -1873,
   -1873, -1873, -1873, -1873,    14,   435,   436,   112, -1873, -1873,
   -1873, -1873,  -779, -1873,    50,   -14, -1873,   120, -1873,  -121,
   -1873, -1873, -1873,   892,  -442, -1020,  -134, -1873,    45,    40,
     737,  1593, -1012,  -828, -1873,   -69, -1873, -1873,    75, -1873,
    -166,  1164,  -273,  -257,  2962,   380,  -655,   167,   420,    37,
     856,  2877, -1873, -1873,  2121, -1873,    64,  4738, -1873,  2060,
   -1873,  1431, -1873, -1873,  2428,   882,  5370,  4043,   -61,  1904,
    -349, -1873, -1873, -1873, -1873, -1873,  -305,  8148,  7689, -1873,
    -398,   143, -1873,  -711,   239, -1873,   173,   726, -1873,   -44,
    -294, -1873, -1873, -1873, -1873,  -118,  8524,  -927,   871,   425,
    1607, -1873,  -660,  -120,   -50,  2476,  5009,  -816,  -161,   909,
    -165,  -529,  -259,  -215,  -486,  1331, -1873,  1672,     3,  -937,
    1541, -1873, -1873,   669, -1873, -1254,  -175,   -59,  -526, -1873,
     294, -1873, -1873, -1153,   448, -1873, -1873, -1873,  2201,  -808,
    -469, -1038,   -34, -1873, -1873, -1873, -1873, -1873, -1873,  -214,
    -890,  -204, -1861,    83,  7072,   -66,  5561,  -125,  1501, -1873,
    3190,   -86,  -218,  -213,  -211,    34,   -72,   -67,   -63,   349,
      77,   174,   175,  -178,   -76,  -147,  -131,  -127,   -36,  -119,
    -116,  -106,  -735,  -772,  -752,  -741,  -715,  -123,  -732, -1873,
   -1873,  -746,  1401,  1404,  1406,   968, -1873,   559,  6386, -1873,
    -620,  -600,  -586,  -581,  -708, -1873, -1667, -1749, -1742, -1739,
    -659,  -139,   -35, -1873, -1873,   -54,  1181,  -128, -1873,  7150,
    1524,  -595,  -560
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   853,   428,   429,   177,    84,  1247,   430,   403,
     431,  1568,  1569,   432,  1002,  1003,  1004,  1362,  1363,  1364,
    1582,   454,   434,   435,   436,   736,   737,   437,   438,   439,
     440,   441,   442,   443,   444,   445,   446,   447,   456,  1150,
     738,  1496,   799,   223,   801,   450,  1038,  1248,  1249,  1250,
    1251,  1252,  1253,  1254,  2156,  1255,  1256,  1685,  2012,  2013,
    1944,  1945,  1946,  2126,  2127,  1257,  1704,  1705,  1960,  1706,
    1854,  1855,  1258,  1259,  1260,  1261,  1262,  1263,  1883,  1887,
    1519,  1511,  1264,  1265,  1518,  1512,  1266,  1267,  1268,  1269,
    1270,  1271,  1272,  1723,  2144,  1724,  1725,  2050,  1273,  1274,
    1275,  1499,  2058,  2059,  2060,  2184,  2195,  2078,  2079,   311,
     312,   940,   941,  1216,    86,    87,    88,    89,    90,  1688,
     490,   209,    94,    95,    96,    97,   239,   240,   314,   293,
     492,   458,   493,   100,   326,   102,   103,   157,   361,   317,
     107,   108,   109,   173,   110,   957,   362,   158,   113,   263,
     114,   159,   272,   364,   365,   366,   160,   451,   119,   120,
     368,   121,   611,   933,   931,   932,  1662,   122,   123,   124,
     125,  1210,  1463,  1668,  1669,  1815,  1816,  1464,  1657,  1835,
    1670,   126,   698,  1315,   169,   992,   127,   993,   994,  1560,
     965,   617,  1141,  1142,  1143,   618,   372,   501,   502,   620,
     460,   461,   224,   520,   521,   522,   523,   524,   349,  1296,
     350,   955,   953,   649,   351,   391,   352,   353,   462,   128,
     179,   180,   129,  1290,  1291,  1292,  1293,     2,  1197,  1198,
     640,  1284,   130,   339,   340,   274,   285,   594,   131,   227,
     132,   329,  1152,   584,   554,   171,   133,   398,   399,   400,
     134,   331,   243,   244,   245,   332,   136,   137,   138,   139,
     140,   141,   142,   248,   333,   250,   251,   252,   334,   254,
     255,   256,   839,   840,   841,   842,   843,   257,   845,   846,
     847,   804,   805,   806,   807,   555,  1190,  1442,   143,  1774,
     673,   674,   675,   676,   677,   678,  1818,  1819,  1820,  1821,
     663,   503,   376,   377,   378,   463,   215,   145,   146,   147,
     380,   867,   679
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   873,   359,    82,   198,   392,   575,   373,   526,   199,
     196,   681,   242,   200,   402,   321,   448,   205,  1297,   537,
     591,   980,   249,   741,   538,   183,   539,  1515,   533,   375,
     864,   214,   560,   307,  1133,   135,  1135,  1048,   106,   568,
    1054,    91,  1536,  1537,   914,   916,  1488,   388,  1446,   153,
    1279,   974,   387,   449,   572,    82,    82,  1089,    82,   540,
    1316,   587,   253,  1109,  1926,   111,  1366,   966,  1323,   465,
     313,  1927,   598,  2020,  1928,    82,    98,  1501,   485,  1592,
    2014,   967,  1029,  1110,    82,   658,   968,   498,  1866,   747,
     541,   306,    82,   135,  1111,  1373,   106,    82,   214,    91,
      82,   689,  1115,  1112,    82,   692,   542,  1727,  1680,   537,
     543,  1233,  2074,   975,   538,   324,   539,   920,   544,   212,
    1205,   545,  1116,   111,   467,   529,   561,  2015,   928,   468,
    1119,   546,   246,   469,    98,   275,  1126,   228,   270,   286,
    1285,   781,  2021,    82,  -803,   681,    82,  1930,    82,   540,
     279,   631,   548,   201,    82,   588,  1513,   499,    58,    58,
    1445,    82,   549,  1500,  1411,   198,   599,  1449,   104,    82,
     199,   535,   948,   997,   200,   670,   230,  1741,   313,  1514,
     541,    82,  1439,   135,  1584,   782,   106,  1013,  1728,    91,
    1458,  2082,   631,    82,    82,    82,   542,  1144,  1459,  1146,
     543,   401,   550,   111,  1440,    82,   148,   313,   544,   623,
      82,   545,   582,   111,  2014,   658,  1161,   974,   313,   664,
     517,   546,    75,    75,    98,    82,   104,  1729,   242,   212,
      82,    82,    82,  1941,  1942,   966,    82,    82,   249,  1961,
     606,  2056,   548,   624,    58,   508,   553,  1526,  2022,   967,
     202,   203,   549,  2020,   968,   198,   742,    82,   697,  -804,
     199,   635,  2006,   696,   200,    82,   681,    82,  1885,  1153,
     700,   704,   212,   470,  2075,  2076,    82,    82,   627,  1120,
      82,  1093,   582,  1123,  1509,  1283,  2020,    82,  1859,  1556,
    2016,  1783,   550,   653,  1138,  1139,  1412,  2083,   906,  1730,
     212,    82,    82,  1886,    82,   990,   910,   868,    75,    82,
     681,   111,  2125,    82,   201,   595,   104,   879,   593,   740,
    1943,  1355,   880,   552,   881,   557,    82,    82,   168,  1474,
    1589,  1413,   565,   653,   681,  1109,   161,    82,  2125,   691,
     111,   681,  1380,   694,  1031,    82,    82,   908,   626,   664,
      82,   111,   270,  1314,  2028,  1110,   829,   882,  1926,  1052,
     875,  2158,   212,  1279,  1590,  1927,  1111,   912,  1928,   844,
     471,   472,  1345,   917,   922,  1387,   111,   485,  1009,  1395,
      82,   926,  1460,   959,  2110,   930,  1469,  1470,   883,    82,
    2073,  1555,    82,   701,  1447,    82,   703,   270,  1321,  1396,
    1119,   214,   225,  2120,   884,   630,   632,   979,   885,  1649,
    1773,   202,   203,  1786,  1788,  1790,   886,   879,  1233,   887,
     984,   105,   880,   650,   881,  1413,  1294,   651,   306,   888,
    1458,  1458,  1458,  1740,   213, -1116,  1500,  1743,  1459,  1459,
    1459,  1930,   190,  1385,  1386,  1682,   991,   247,  1967,  1968,
     276,   384,    20,   205,   287,   162,  1202,   882,   907,  1889,
     498,   899,   991,   467,  1516,    82,   911,   384,   468,  1941,
    1942,   900,   469,  -985,   225,   927,  1510,  -835,  2006,   105,
    -985,   269,    63,    64,   206,   921,  1466,  1517,   883,   306,
      82,    82,   646,   291,  1314,   298,   929,   300,  1276,  1420,
     508,   220,    82,    82,   884,  1467,  1471,   966,   885,  1513,
      58,   901,   221,    82,  1031,   517,   886,   204,    64,   887,
    1857,   967,   647,   648,  2039,  1865,   968,   313,   222,   888,
     499,   270,  1514,    78,    82,   167,   269,   498,   182,   298,
     300,   664,  2033,  2034,   213,  1007,    82,   226,  1010,  -835,
    1012,   899,   184,  1014,    58,  1731,  1972,   959,   218,   316,
     509,   900,  1017,   467,   231,  1019,  1020,  1021,   468,   105,
      82,  1401,   469,  1501,    75,   705,    82,  1832,    82,   706,
     989,   508,   600,  1663,  1833,    58,  1619,   213,  -670,  1133,
    1135,   269,  1031,  1024,   305,  -670,  1375,   612,   553,  1785,
    1545,   901,  2100,  1834,  1025,  1026,   947,   499,   740,   771,
     772,   740,   470,   681,  1078,   213,   740,   185,    75,   193,
     193,   961,  1460,  1460,  1460,   740,   -23,  1766,  -616,   163,
     596,   193,   164,   165,    82,   166,    82,  1991,   562,    82,
     194,    82,   553,   980,   740,  1848,  1849,  1850,  1851,    75,
      82,  1154,   270,   783,    82,   773,   774,   784,   985,  1500,
     111,   269,   232,   298,   300,   593,   498,   316,  1852,   638,
      58,   508,   923,   553,   135,   234,  1959,   106,  1675,   448,
      91,  1664,  1466,  1612,  1823,    83,    82,  1298,   151,  1675,
      -3,  1136,   359,  1031,  1149,   269,   316,  1676,  1184,  1833,
     269,  1746,   231,  1824,   111,    82,   269,   316,  1826,   471,
     472,   498,  1867,  1204,   844,    98,  1130,   232,  1925,   375,
     593,  1553,   923,  1931,   606,  1833,    58,  1468,  1561,    58,
    1349,   193,   316,   278,    75,    58,   499,  1350,    92,  -986,
     269,   154,  1932,    83,  1935,   686,  -986,   300,    82,  1679,
      82,  2064,    82,  1031,   818,   305,    82,   473,   553,    82,
      83,    14,    15,    16,    17,    18,  1535,    58,    58,    83,
     748,   233,  2026,  2030,   258,   749,  1031,  1473,  1031,    58,
    1596,   499,    83,  -492,    82,    83,  2102,  2045,  2131,    83,
      75,  1031,   724,    75,    58,   961,    92,  1369,   301,    75,
    1132,  2133,  1581,  1915,  1327,  1916,  1191,   104,  1145,  1327,
    1094,  1276,  1891,  1117,   553,   509,   305,   668,   764,  1124,
    1199,    58,   750,   668,  1203,   765,   766,   751,  1206,    82,
     303,    75,    75,    83,    82,    58,    82,  1901,   267,    83,
      58,   306,   280,    75,  1860,  1872,   269,   741,    82,  1861,
    1861,  1172,  1176,  2093,   385,   553,   553,   359,    75,    82,
     938,  1980,   306,  1180,  1577,   517,  1981,   553,    82,   325,
    1317,    58,   259,   260,   269,   261,   686,   300,  1423,    83,
      83,   262,   553,   116,   375,    75,    92,  2115,  1410,   473,
     482,   553,  2116,   724,   485,    83,   509,  2173,  -493,    75,
    -824,    82,  2174,   809,    75,    83,  1370,   810,    14,    15,
      16,    17,    18,   605,    64,   532,    83,   534,   191,  1427,
     390,    83,    83,   553,  1431,  1299,   822,   269,   553,   347,
     553,  1848,  1849,  1850,  1851,    75,   393,    82,    82,   517,
    1417,   116,    83,  1812,  1031,  1775,    58,   401,  1825,   811,
      83,   269,   474,   706,  1852,  1543,   269,   681,   269,   668,
    1767,   475,   289,  1858,  1715,    83,   476,   290,    58,   477,
     294,    58,   299,  1145,   869,   870,   478,   106,   871,   479,
      91,   269,  1782,   269,   269,   512,   609,   525,  1149,   614,
    1394,   844,   505,    82,   151,   269,  1287,   506,    83,   270,
     551,  1712,   937,    58,   111,    58,   938,   228,   269,   996,
      75,    83,    83,   651,  -494,    98,   316,   269,   592,   998,
     511,   116,   527,   651,    14,    15,    16,    17,    18,   979,
    1597,   116,    75,  1686,   553,    75,   530,  1686,  1707,   269,
     163,   686,   300,   164,   165,    82,   166,   531,   305,    82,
     473,  1707,   553,   573,  1547,  1606,   579,  1656,   999,   553,
     105,   574,   706,   269,   686,   695,    74,    75,   586,    75,
     269,    14,    15,    16,    17,    18,   225,   963,    82,  1030,
     742,   517,  1098,  1031,    58,  1528,   553,  1610,   802,  2067,
    1781,   668,   553,   553,  1718,  1719,  1720,  1721,  1722,    80,
      81,   562,  1448,   892,   597,   553,    82,   104,    74,   621,
    1158,    74,    82,    82,   810,   305,  1472,  1868,   625,   553,
     562,   392,   392,   642,   553,   641,   289,    74,  1193,   116,
     667,    58,  1031,   667,   668,  1827,  1494,   668,  1057,  1058,
    1059,    80,   669,   740,    80,   669,   656,    82,    75,  1813,
      83,   769,   770,   553,   670,   638,  1465,   473,   116,   553,
      80,    81,   688,   664,  1565,    99,   359,   699,   155,   116,
     702,  1902,  1570,   289,  1195,   707,    83,   638,  1031,  1640,
    1326,   553,   144,   708,  1327,   144,  1911,  -129,  -129,  -129,
    -129,  -129,  -129,   375,   116,    75,  -496,  1552,    83,  1117,
      83,   473,  1593,   668,  1848,  1849,  1850,  1851,   654,   301,
      14,    15,    16,    17,    18,   384,   564,  1031,   290,    83,
     685,   517,   299,    99,    82,    82,    82,  1852,  1365,   709,
    1534,    83,  1327,   517,   810,  1778,  -193,   767,   768,  1779,
     144,  1136,   359,   448,   448,  1136,  1287,  1136,   744,   210,
    1840,   963,   517,  1844,  1327,    83,   106,  1031,    82,    91,
    1869,    83,  1870,   482,  1031,  2062,   810,  1438,   725,   375,
      58,  1871,   763,    82,  1936,  1031,    82,    82,   810,    82,
    1627,  1628,  1145,   111,  1905,    82,   144,  1985,    82,   712,
     106,  1031,   715,    91,    98,  1848,  1849,  1850,  1851,   777,
    2023,  2118,   779,    99,  1031,  1327,  1996,   319,   775,   776,
    1998,   275,   286,    99,   270,   716,   210,   111,  1852,    83,
     778,    83,  2119,   279,    83,  2192,  1031,  1853,    98,  2189,
     144,   780,    82,  2146,    75,  -497,  2198,  2150,   785,   958,
    2199,  1687,   269,   593,   720,  1687,  1809,  1810,  1811,   517,
    1450,  1451,  1452,   269,   812,  1622,   505,   744,    82,   813,
    1278,   814,   269,  1064,  1065,  1066,  1067,  1681,  1683,   725,
     483,   983,   359,   830,   744,  1672,   815,    92,  1836,  1836,
    1836,  1022,   744,   717,   816,  1673,   104,   817,  1033,  1034,
     448,    82,   656,   744,  1352,  1353,  1465,  1465,  1465,   375,
     481,  1658,  1465,  1367,  1368,   825,  1689,   827,   761,   762,
    1689,    99,  -128,  -128,  -128,  -128,  -128,  -128,  1745,  1707,
     104,  1031,  1371,  -164,  -164,  1674,   808,  1771,   577,   761,
     581,    -3,   115,  1042,   848,  1044,  -495,  1047,   206,   744,
      99,  1509,  1510,   820,  1055,   850,   823,   537,   852,   289,
     -18,    99,   538,   865,   539,  1587,  1588,  1591,  1588,   874,
      82,   877,   761,   866,    82,    82,  1595,  1588,   269,  1080,
    1106,  1580,  1629,  1580,   155,   889,    99,   890,   116,   891,
      14,    15,    16,    17,    18,   893,   517,   540,  1145,   894,
     115,  1106,  1641,   895,   269,  1791,  1353,   896,   153,   897,
     581,    14,    15,    16,    17,    18,  1028,   898,   564,   903,
     517,   517,   106,   904,   482,   318,   106,   106,   541,    83,
      82,    83,   116,    14,    15,    16,    17,    18,  1344,   924,
     106,  1913,  1353,   925,   542,  -614,   283,  -612,   543,   111,
      58,  1914,  1588,   111,   111,   934,   544,  1923,  1031,   545,
      83,  1983,  1984,    83,   935,   595,    82,   111,   593,   546,
    1879,   936,  2001,  1588,  2002,  1588,  2189,  2190,  1672,   939,
     115,  1941,  1942,  1672,   942,   517,  2080,   950,  1673,   952,
     115,   548,    82,  1673,  1585,  1586,    83,    82,    82,    82,
     956,   549,  1062,  1063,    93,  1060,  1061,    93,   969,  1828,
    1742,  1744,    74,   971,    75,   670,  2080,   987,  1287,   217,
     879,  1068,  1069,   995,  1570,   880,  1006,   881,  1674,   210,
    1873,  1032,  1214,  1674,   667,  1035,   276,   287,   668,  1837,
    1838,   550,  1077,  1040,  1082,    80,   669,  2128,  1105,  1278,
    1106,  1113,   104,  1134,  1137,  1156,   104,   104,   828,  1160,
     882,  1163,    93,    82,  1164,  1165,  1166,   269,    82,  1167,
     104,  1168,  1169,  1170,    82,  1171,    82,  1185,  1192,   483,
     830,  1194,  1196,  1278,    82,  -807,   217,    92,   115,  1200,
    1207,   883,  1208,  1209,  1280,  1286,   269,  1307,  1288,  1308,
    1993,  1309,   269,   517,   268,  1295,  1310,   884,    93,  1318,
     517,   885,  1320,   681,  2051,  1324,  1325,   115,  1979,   886,
    1328,  1329,   887,  1330,  1331,  1022,   392,   464,   115,  1333,
    1334,  1335,   888,  1336,  1338,  1340,  1337,   270,  1341,  1342,
      83,   517,  1347,  1348,    83,  1402,   279,  1056,  1356,  1357,
    1372,  1376,    93,   115,  1416,   899,  1377,   283,  1378,   337,
    1379,  1383,  1399,  1384,  1388,   900,  1389,  1390,  1391,  1404,
      99,  1498,  -808,    83,  1405,  1406,    83,  1408,  1443,   517,
     808,   808,   448,    19,  1475,  1476,  2051,  1479,  1480,  1489,
     -22,  1096,  1490,  1491,  1099,  1493,    82,  1031,    82,  1523,
     583,    83,  1503,  1502,  1524,   901,  1287,    83,    83,  1554,
     491,  1530,  1558,  1562,    99,  1576,  1580,  1601,  1602,  2011,
    1605,  2057,  1532,    48,    49,    50,    51,    52,    53,    54,
      55,   144,   116,  1559,  1616,  1617,  1594,    82,  1624,  1618,
      82,   752,    83,   753,   754,   755,  1620,  1625,  1588,   517,
     517,  1630,   269,  1633,  1637,  1638,   517,  2109,  1639,  1646,
     564,  1647,  1672,  1650,  1693,  1661,  1468,  1174,  1708,   517,
     583,  1178,  1673,  1709,   756,  1182,  1733,   757,   758,   517,
     596,   517,   759,   760,   268,  1711,  1713,  1510,  1726,  1736,
     593,  1737,   666,  1734,   517,  1735,   517,   517,   517,   537,
    1233,  1747,    82,    82,   538,  1691,   539,  1748,   106,  1691,
    1691,   269,  1674,  1752,   448,  1750,   448,  1753,  1754,  1765,
     150,  1776,   616,  1691,    65,    66,    67,    68,    69,    70,
      71,    72,  1755,  2053,  1756,   111,  1762,  1772,  1764,   540,
    1777,  1780,  1784,  1792,  1794,  1795,  1798,    82,  1288,   473,
    1629,  2123,  2057,  2011,   517,   448,  2057,  2057,   517,  1800,
    1802,  1803,  1804,   517,  1807,    82,    92,  1808,   614,  1666,
     541,    83,    83,   186,     6,     7,     8,     9,    10,    11,
      12,    13,  1841,    83,  2168,   226,   542,  2171,  1822,   217,
     543,  1845,  2148,  1876,  1847,  1878,  1903,  1912,   544,  1895,
      92,   545,  1339,  1896,  1899,  2053,  1907,  1900,  1343,  1917,
    2183,   546,  1920,  1921,  2183,  1949,  1954,  1922,   517,  1351,
    1971,  1955,   666,  1976,   267,   280,   448,  1566,  1969,  2193,
     517,  1978,   548,  1982,   517,   288,  1987,   115,   104,  1994,
    1995,  1997,   549,    85,    83,   198,   152,  1999,  2000,   517,
     199,   635,  2003,    83,   200,  2004,  2005,  2009,  2025,   553,
      82,   464,    82,  2191,  2035,   106,   808,  2027,  2038,   899,
    2040,  2046,  2054,  2065,  2066,  2077,  2099,  2182,  2086,   900,
     269,   115,   550,  2055,  2101,  2103,    83,  2112,  2114,  2188,
    2113,   517,   111,  2117,  2130,   106,  2134,  2132,  2143,  2147,
    2153,    85,  2154,  2159,  2149,  2169,  2170,   464,   464,  2172,
    2178,   116,  2180,  1564,    99,  2185,    82,    82,   195,   901,
    2181,  2196,   111,  2186,  1909,  1289,   106,    85,  1027,   517,
    2197,   144,  1070,  2200,   517,  1071,  1073,  1497,  1505,  1072,
     238,  1425,   144,   266,  1429,   116,  1074,    85,  1433,  2179,
    2124,  1973,   212,   111,    82,    83,   800,  1695,  2141,  1966,
    1717,  2037,   517,  2121,  1888,   517,  2167,   517,  1874,  1875,
    2187,  2105,    14,    15,    16,    17,    18,  2151,  2104,   909,
     174,    83,  1522,   491,   296,   152,   585,  2008,   517,   508,
    2071,    85,  1660,  1520,   152,   104,   483,   328,   336,    82,
     154,  1557,   954,   717,  1904,   872,  1749,  1155,    82,  1892,
     358,     3,  1806,  1085,   981,    83,  1086,  1000,  1087,     0,
       0,   269,    92,     0,     0,   104,    92,    92,     0,     0,
       0,     0,    58,   269,     0,   455,   464,   195,   195,     0,
      92,     0,     0,    93,  1008,     0,     0,     0,   152,   488,
     464,     0,  1015,   266,     0,   616,   104,     0,     0,     0,
     491,     0,     0,     0,     0,     0,     0,     0,   592,     0,
      83,     0,     0,     0,   328,     0,     0,    83,     0,   238,
     238,     0,    83,    83,    83,     0,   761,     0,     0,     0,
       0,  1691,     0,     0,    74,     0,    75,     0,     0,     0,
     328,     0,     0,     0,     0,     0,     0,     0,    85,     0,
    1288,     0,     0,     0,     0,     0,   802,     0,     0,   269,
     553,     0,     0,   266,  1571,  1572,  1573,    80,    81,   150,
       0,  1574,  1575,    65,    66,    67,    68,    69,    70,    71,
      72,  1358,     0,     0,   464,  1359,     0,  1360,    83,     0,
       0,     0,     0,    83,     0,  1599,   328,     0,     0,    83,
       0,    83,   336,     0,     0,     0,  1608,   116,   336,   328,
     328,   116,   116,     0,     0,  1289,     0,     0,   152,     0,
      77,   115,     0,  1583,  1461,   116,     0,     0,     0,   491,
       0,   394,   144,    99,    14,    15,    16,    17,    18,     0,
     358,   671,   680,     0,     0,     0,     0,     0,     0,     0,
     144,     0,     0,     0,     0,     0,   358,     0,     0,  1084,
     358,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,     0,     0,     0,   491,  1101,     0,   267,   280,  1102,
       0,     0,   269,     0,   144,     0,     0,     0,  1691,     0,
       0,     0,     0,   491,    58,   491,     0,     0,     0,   491,
     491,     0,   491,   455,    83,     0,   150,   395,     0,   144,
      65,    66,    67,    68,    69,    70,    71,    72,  1691,   491,
       0,    83,     0,    83,     0,   150,   464,   175,   176,    65,
      66,    67,    68,    69,    70,    71,    72,   455,  1288,     0,
     803,     0,   394,     0,     0,     0,     0,     0,   195,  1691,
       0,     0,     0,     0,     0,     0,    74,    77,    75,     0,
     861,     0,    83,     0,     0,    83,   152,     0,     0,     0,
     488,     0,     0,     0,   837,   269,   680,     0,  1813,     0,
       0,     0,   553,    93,     0,     0,     0,   152,   491,    80,
      81,     0,     0,     0,    93,     0,     0,   150,     0,   918,
     518,    65,    66,    67,    68,    69,    70,    71,    72,   455,
       0,     0,     0,     0,     0,     0,     0,   238,   395,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    83,   238,
     592,     0,     0,     0,     0,     0,   150,     0,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,    92,     0,
       0,     0,     0,   328,     0,   455,   455,     0,     0,   328,
       0,     0,   358,     0,  1461,  1461,  1461,   155,  1654,  1655,
    1659,   150,    83,  1817,     0,    65,    66,    67,    68,    69,
      70,    71,    72,  1045,   144,   496,   396,     0,     0,    99,
    2031,     0,     0,    99,    99,     0,     0,     0,     0,     0,
     115,     0,     0,     0,     0,     0,   144,    99,     0,     0,
     144,   144,     0,     0,     0,     0,     0,   328,     0,   328,
       0,     0,    85,     0,   144,  1046,     0,     0,     0,     0,
       0,     0,     0,   500,   115,     0,     0,     0,   358,   488,
       0,   680,     0,     0,     0,     0,   491,   491,     0,   671,
       0,  1381,     0,   671,     0,  1382,     0,     0,     0,   283,
    1882,     0,   358,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   680,     0,  1397,   358,     0,  1289,     0,     0,
       0,  1398,     0,     0,   152,    83,     0,  2111,  1506,     0,
       0,     0,   455,   116,   144,   455,     0,   152,   152,     0,
     455,     0,   491,     0,     0,    92,     0,     0,     0,   455,
       0,     0,   152,   152,   152,     0,   150,     0,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,     0,  1435,
       0,  1817,  1817,  1436,     0,    92,   661,  1437,     0,   684,
       0,    83,    83,     0,    93,     0,     0,     0,     0,   341,
       0,     0,   661,     0,     0,     0,   661,   342,   343,   344,
     345,     0,    93,     0,     0,     0,    92,     0,   488,   464,
       0,     0,     0,     0,     0,     0,   981,     0,     0,    83,
       0,     0,     0,     0,   803,   803,     0,     0,     0,     0,
       0,  1507,   455,   862,     0,   518,    93,     0,   150,     0,
     175,   176,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,   358,   488,     0,     0,     0,   837,     0,   837,
     268,    93,   144,     0,  2194,     0,     0,     0,     0,     0,
       0,     0,   358,  2201,   358,     0,     0,     0,   358,   358,
     358,   358,     0,     0,     0,     0,     0,  1817,     0,     0,
     116,     0,   346,     0,    58,     0,   115,     0,   358,     0,
     115,   115,     0,     0,     0,  1289,   661,     0,     0,     0,
     347,     0,     0,     0,   115,     0,     0,     0,     0,     0,
     116,     0,   144,     0,   328,     0,     0,     0,   150,   192,
     235,   236,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,  2061,     0,     0,
       0,   116,     0,   101,     0,     0,   156,     0,    75,     0,
       0,  2069,   455,     0,     0,  1817,     0,   358,   271,     0,
       0,     0,     0,   152,   455,     0,     0,     0,  1392,    77,
     292,   295,   358,     0,  1302,   491,     0,     0,   491,   491,
       0,     0,     0,     0,     0,   671,     0,     0,   496,     0,
       0,     0,     0,     0,   464,     0,     0,     0,     0,  1817,
       0,   101,     0,     0,     0,    99,     0,     0,     0,     0,
    1634,     0,     0,   271,  1635,     0,     0,     0,  1636,     0,
       0,     0,   144,     0,     0,     0,    93,   211,     0,     0,
       0,     0,     0,    58,   152,   488,   500,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   281,    93,     0,
       0,     0,    93,    93,     0,  1817,  1817,     0,     0,     0,
       0,     0,     0,     0,   661,   496,    93,   150,   271,   235,
     236,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,   315,     0,     0,     0,   320,     0,     0,   661,     0,
       0,   101,     0,  1817,   268,    74,     0,    75,     0,     0,
     803,   661,   283,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,   500,     0,   358,   358,  2107,    77,   837,
       0,   553,     0,     0,     0,     0,   837,     0,    80,    81,
    1758,     0,     0,     0,     0,   150,    93,   466,   271,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   320,   494,
     150,     0,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,    99,     0,     0,   464,     0,     0,     0,     0,
       0,   358,   271,     0,     0,   518,     0,   271,   862,   144,
     547,     0,     0,   271,  1793,  1392,    77,     0,     0,   315,
       0,     0,    99,  1796,   496,     0,     0,  1797,     0,     0,
     571,     0,     0,     0,     0,   576,   578,     0,   211,   144,
       0,     0,     0,   152,     0,     0,     0,   271,   315,     0,
       0,     0,   152,    99,     0,  1481,     0,     0,     0,   315,
       0,   455,   601,     0,     0,     0,   603,     0,   661,   496,
     144,   604,   500,     0,     0,     0,   197,   615,     0,     0,
       0,     0,   578,     0,   315,     0,     0,   455,   628,     0,
       0,     0,     0,   268,    93,   455,   496,     0,   241,   726,
     637,   150,   115,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,     0,    19,     0,     0,   500,     0,   266,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     659,   328,     0,   683,     0,     0,   500,   152,   500,     0,
       0,     0,   500,   500,   488,   500,   690,     0,     0,     0,
     690,     0,     0,     0,    93,   330,     0,     0,    52,    53,
      54,    55,   500,     0,     0,     0,     0,   433,     0,     0,
       0,     0,     0,     0,   488,     0,  1483,     0,     0,   152,
       0,   271,     0,     0,     0,  1361,     0,     0,     0,  1361,
       0,     0,     0,     0,     0,     0,     0,     0,   661,     0,
     726,   684,     0,   150,     0,   175,   176,    65,    66,    67,
      68,    69,    70,    71,    72,  1049,     0,     0,  1361,     0,
       0,   518,  1212,     0,     0,     0,     0,     0,     0,     0,
       0,   500,   330,     0,     0,     0,     0,   536,   241,     0,
       0,     0,     0,     0,   358,     0,   268,   358,   358,     0,
     358,     0,     0,     0,     0,     0,   320,  1050,   330,   115,
     659,   496,   150,     0,    93,   271,    65,    66,    67,    68,
      69,    70,    71,    72,  1358,     0,     0,   320,  1359,     0,
    1360,     0,     0,     0,     0,     0,     0,  1361,   271,   115,
       0,     0,   152,   152,   152,   152,     0,   152,   152,     0,
       0,     0,   271,  1667,   336,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   330,   271,  1787,   455,     0,     0,
     115,   455,   455,     0,     0,     0,     0,   636,   330,     0,
       0,     0,   455,     0,   615,   455,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   271,     0,     0,     0,
       0,   150,   494,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,   266,     0,     0,     0,     0,   315,     0,
     271,     0,     0,     0,     0,     0,     0,   271,     0,     0,
     488,     0,     0,     0,     0,     0,   711,     0,   714,   500,
     500,   433,   719,     0,     0,     0,     0,     0,     0,     0,
       0,   728,   729,     0,     0,   152,     0,   671,     0,   644,
     615,     0,   101,     0,     0,     0,   433,   433,     0,     0,
       0,    93,     0,     0,     0,     0,     0,     0,   690,   962,
       0,     0,     0,     0,     0,     0,     0,   433,     0,   615,
       0,     0,     0,   973,     0,   500,     0,     0,     0,     0,
       0,    93,   659,     0,     0,     0,     0,   982,     0,     0,
       0,     0,     0,     0,     0,   690,     0,     0,     0,     0,
     433,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,    93,     0,  2122,     0,     0,     0,     0,   518,
       0,     0,   838,     0,     0,     0,  1667,  1814,  1361,     0,
       0,  1667,     0,   455,     0,     0,     0,  1667,     0,  1667,
       0,   150,     0,   235,   236,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   336,   152,     0,   878,     0,     0,     0,    74,
     661,    75,     0,     0,     0,     0,   150,   241,   382,   383,
      65,    66,    67,    68,    69,    70,    71,    72,   494,     0,
       0,   237,    77,     0,     0,     0,     0,     0,     0,  1478,
     496,   330,    80,    81,   615,  1088,     0,   330,     0,     0,
       0,  1492,     0,     0,     0,     0,     0,     0,     0,     0,
     615,     0,  1188,   152,   615,     0,     0,     0,     0,    78,
       0,     0,   690,   962,     0,     0,     0,   615,     0,  1114,
       0,   384,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   152,   494,     0,   494,     0,   518,     0,   494,   494,
     360,   494,     0,     0,     0,   946,   150,   330,   235,   236,
      65,    66,    67,    68,    69,    70,    71,    72,   494,   271,
       0,     0,     0,     0,     0,  1814,  1814,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
    1667,     0,     0,  1667,     0,     0,     0,   258,     0,     0,
       0,     0,     0,     0,     0,   336,   835,    77,   500,     0,
     668,   500,   500,     0,     0,     0,     0,    80,   836,     0,
       0,     0,     0,   455,     0,     0,     0,     0,   615,     0,
     670,     0,  1277,     0,     0,   518,     0,   494,     0,     0,
       0,     0,  1361,   156,     0,     0,     0,  1361,  1361,  1361,
       0,     0,   690,     0,     0,  1306,   328,     0,     0,     0,
       0,     0,  1312,     0,     0,   433,   433,   433,   433,   433,
     433,   433,   433,   433,   433,   433,   433,   433,   433,   433,
     433,   433,   433,   433,     0,     0,     0,     0,     0,     0,
       0,  1814,     0,     0,     0,     0,   496,     0,     0,     0,
    1667,   150,     0,   235,   236,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   320,   360,   150,     0,   235,   236,
      65,    66,    67,    68,    69,    70,    71,    72,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,   150,   152,
     204,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     433,  1665,    77,     0,     0,  1108,     0,   838,  1666,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,  1814,
       0,     0,     0,     0,   118,     0,     0,   118,  1738,  1739,
     152,   384,     0,     0,     0,     0,   615,     0,     0,    77,
     615,     0,   861,     0,     0,   494,   494,   219,     0,   615,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   615,
     152,   152,     0,  2108,   336,     0,   615,     0,     0,     0,
       0,     0,   330,     0,     0,     0,     0,     0,     0,   518,
       0,     0,   118,     0,     0,     0,     0,     0,   304,     0,
       0,   152,     0,     0,     0,     0,  1361,     0,  1361,     0,
       0,   494,     0,     0,     0,     0,     0,     0,   118,     0,
       0,     0,     0,     0,   615,     0,     0,     0,   615,  2108,
    2108,     0,   615,     0,   273,     0,     0,     0,   118,     0,
       0,     0,     0,     0,   271,     0,     0,     0,     0,     0,
       0,     0,     0,   156,     0,     0,     0,     0,     0,     0,
       0,     0,  1462,     0,     0,     0,     0,  2108,     0,     0,
       0,  1277,   118,   271,     0,     0,   118,     0,     0,     0,
     433,     0,   118,     0,     0,   118,   433,     0,     0,   273,
       0,     0,     0,     0,     0,     0,     0,   433,     0,     0,
     354,   118,   150,   386,     0,  1277,    65,    66,    67,    68,
      69,    70,    71,    72,  1358,     0,     0,     0,  1359,     0,
    1360,  1846,     0,     0,     0,     0,   459,    58,  1856,     0,
    1521,     0,     0,     0,     0,     0,     0,   433,     0,   118,
     459,     0,     0,     0,   273,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   659,     0,  1789,     0,     0,  1881,
       0,   150,     0,   576,     0,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
     118,     0,   615,     0,   360,     0,     0,  1108,     0,    74,
       0,    75,   633,  1393,   838,     0,     0,   118,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
    1644,    76,    77,     0,   273,     0,     0,     0,     0,     0,
     118,     0,    80,    81,     0,     0,     0,     0,     0,   271,
       0,     0,     0,     0,     0,   610,     0,     0,   118,     0,
       0,     0,     0,   118,     0,   118,     0,     0,   273,   118,
       0,     0,     0,   273,   494,     0,     0,   494,   494,   273,
     360,     0,     0,     0,     0,     0,     0,  1939,  1940,   118,
       0,     0,     0,     0,  1950,   615,     0,     0,     0,   615,
       0,   433,     0,   615,     0,     0,     0,  1965,   271,     0,
       0,   118,     0,   273,   118,   661,     0,  1974,     0,  1975,
       0,     0,  1462,  1462,  1462,   156,   578,   118,     0,     0,
       0,   118,  1986,     0,  1988,  1989,  1990,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1690,     0,     0,
       0,  1690,  1690,   150,     0,   175,   176,    65,    66,    67,
      68,    69,    70,    71,    72,  1690,     0,     0,     0,     0,
       0,     0,     0,     0,   459,   832,     0,   834,     0,     0,
       0,     0,     0,     0,   433,     0,   851,   661,     0,   330,
       0,     0,  2019,     0,     0,     0,  2024,     0,     0,     0,
       0,  2029,     0,     0,     0,     0,     0,     0,   459,     0,
     360,     0,     0,     0,  1485,   615,     0,     0,     0,     0,
       0,     0,   433,   433,   433,     0,     0,     0,     0,   433,
     433,     0,     0,     0,     0,   156,     0,   118,     0,     0,
       0,   459,    14,    15,    16,    17,    18,   273,     0,     0,
       0,     0,     0,   433,     0,     0,  2072,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2081,   615,
       0,     0,  2084,     0,     0,     0,     0,   271,   615,     0,
     459,     0,   615,     0,     0,     0,     0,  2098,     0,     0,
       0,     0,  1613,     0,   433,   433,     0,     0,     0,     0,
       0,   411,    58,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   459,   459,     0,  2129,
       0,   273,     0,   118,     0,     0,   150,     0,     0,  1831,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   118,
       0,     0,     0,   746,     0,     0,    78,   422,     0,     0,
       0,  1671,     0,  1843,    74,     0,    75,  2152,   273,     0,
       0,     0,  2155,     0,     0,     0,     0,     0,     0,     0,
       0,   273,     0,     0,     0,     0,    76,    77,     0,     0,
       0,   118,     0,   118,     0,     0,     0,    80,    81,     0,
    2175,     0,     0,  2177,     0,  2155,     0,     0,   386,   118,
     459,     0,   273,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,   156,     0,     0,  2177,     0,     0,     0,
     271,     0,     0,   118,     0,     0,   273,     0,     0,     0,
     610,   433,     0,   273,     0,     0,   118,     0,     0,   112,
       0,     0,     0,    58,     0,   118,     0,     0,     0,     0,
       0,     0,     0,   459,     0,     0,   459,     0,   118,   118,
       0,   459,     0,     0,     0,     0,     0,     0,     0,     0,
     459,     0,     0,   118,   118,   118,  1929,   150,     0,   235,
     236,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
       0,     0,     0,     0,     0,    74,   150,    75,   605,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,  1690,     0,     0,  1129,   327,    77,   459,
       0,     0,     0,     0,     0,     0,     0,     0,    80,    81,
       0,     0,     0,   282,  1671,   118,     0,     0,     0,  1671,
       0,     0,     0,   459,     0,  1829,     0,  1671,     0,     0,
    1079,   118,     0,     0,     0,   118,     0,     0,    14,    15,
      16,    17,    18,   118,   459,     0,     0,   112,   118,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
       0,     0,     0,   118,     0,   118,     0,     0,     0,   118,
     118,   118,   118,     0,     0,     0,   363,     0,   433,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,   118,
       0,  1281,  1282,     0,     0,     0,     0,     0,    58,   271,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2052,
       0,     0,     0,     0,     0,   495,     0,   150,  1189,   235,
     236,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   150,     0,   235,   236,    65,    66,    67,    68,
      69,    70,    71,    72,     0,    74,     0,    75,     0,   118,
    1690,     0,     0,   459,     0,   112,     0,     0,   118,     0,
      74,     0,    75,     0,   118,   459,     0,  1665,    77,     0,
       0,     0,     0,   118,     0,  1304,   459,     0,    80,    81,
    1690,  2052,   835,    77,   112,     0,   668,  1354,  1937,     0,
       0,  1671,     0,    80,   836,   112,     0,     0,   602,   615,
       0,     0,     0,  1319,     0,     0,     0,     0,     0,     0,
       0,  1690,     0,   363,     0,     0,     0,     0,     0,     0,
     112,     0,     0,     0,   282,     0,     0,     0,     0,  1374,
       0,     0,     0,     0,     0,   118,   459,     0,     0,   150,
    2145,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   150,   330,   235,   236,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   660,     0,     0,   282,
       0,     0,     0,     0,     0,     0,     0,     0,  1400,     0,
    1403,    74,   660,   433,     0,     0,   660,     0,     0,   506,
       0,     0,  1407,     0,  1409,     0,     0,     0,  1671,  1414,
    1415,     0,     0,  1665,    77,     0,     0,   118,     0,  1422,
    1666,   118,     0,     0,    80,    81,   118,   118,     0,     0,
     118,     0,     0,     0,     0,   433,     0,     0,     0,     0,
     118,     0,     0,     0,     0,  1441,     0,   118,  1444,     0,
       0,   150,     0,   235,   236,    65,    66,    67,    68,    69,
      70,    71,    72,   150,     0,   607,   608,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,    74,
       0,   150,   118,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   118,     0,     0,     0,   118,
       0,  2107,    77,   118,     0,   553,   660,     0,     0,     0,
       0,  1504,    80,    81,     0,     0,    78,     0,     0,     0,
       0,   330,     0,     0,   118,   433,     0,   433,     0,     0,
       0,     0,     0,   118,    78,     0,     0,     0,     0,     0,
       0,  1525,   459,     0,     0,     0,     0,     0,  1529,     0,
    1531,  1533,     0,     0,     0,     0,     0,     0,     0,  1539,
       0,  1540,     0,  1541,     0,     0,   433,     0,   459,     0,
    1550,     0,     0,     0,     0,     0,   459,   636,   330,     0,
     363,   150,     0,     0,     0,    65,    66,    67,    68,    69,
      70,    71,    72,  1358,   619,   433,     0,  1359,   495,  1360,
     273,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   178,   181,     0,   112,   330,     0,     0,   118,     0,
       0,     0,     0,     0,     0,   459,     0,     0,     0,  1304,
       0,     0,    77,     0,     0,     0,     0,   433,     0,     0,
       0,     0,     0,  1603,  1604,     0,     0,     0,   229,     0,
       0,   117,     0,   118,     0,   459,   363,     0,   112,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,  1626,
       0,     0,     0,     0,   660,   495,  1631,     0,  1632,     0,
       0,     0,     0,     0,   150,   363,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   660,   322,
       0,     0,   323,     0,  1648,     0,     0,     0,     0,   117,
       0,   660,     0,     0,     0,     0,     0,   348,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,   118,   118,
       0,   118,     0,     0,   511,     0,     0,   397,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,   397,
     118,     0,     0,     0,   118,   284,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1645,     0,     0,     0,
       0,     0,     0,   118,   118,   118,   118,   118,   118,   118,
       0,   528,     0,     0,     0,   273,     0,     0,     0,   117,
       0,     0,     0,     0,     0,     0,     0,     0,   459,   117,
    1757,     0,   459,   459,   495,     0,     0,  1761,     0,  1763,
       0,     0,     0,   459,     0,     0,   459,     0,   367,     0,
     363,     0,     0,   229,     0,     0,     0,     0,     0,     0,
       0,     0,   589,   590,     0,     0,   363,     0,     0,     0,
     363,     0,     0,   178,   273,     0,     0,     0,   660,   495,
       0,   619,     0,   363,     0,     0,     0,   497,   178,     0,
       0,   459,     0,     0,     0,     0,   118,     0,   363,     0,
     363,     0,     0,     0,   363,   363,   495,   363,     0,  1768,
       0,     0,     0,     0,     0,  1799,   118,     0,     0,   172,
       0,     0,     0,   639,   363,     0,     0,   117,     0,     0,
     228,   643,   645,     0,     0,   150,   652,   175,   176,    65,
      66,    67,    68,    69,    70,    71,    72,   172,     0,     0,
     118,     0,     0,     0,     0,     0,   117,   619,     0,   118,
       0,     0,     0,   118,     0,     0,     0,   117,     0,     0,
       0,     0,     0,     0,     0,   348,     0,     0,   348,     0,
       0,   397,     0,     0,   363,   367,   619,     0,   112,     0,
       0,     0,   117,   363,   172,   150,   284,   235,   236,    65,
      66,    67,    68,    69,    70,    71,    72,   172,   660,   172,
       0,   282,     0,     0,   459,     0,     0,     0,     0,     0,
       0,     0,   150,    74,   235,   236,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,   662,     0,
     172,   284,   389,   273,   118,   237,    77,  1897,  1898,     0,
      74,     0,     0,     0,   662,     0,    80,    81,   662,     0,
       0,  1906,     0,     0,     0,     0,     0,   389,     0,     0,
       0,   495,   327,    77,     0,     0,   229,     0,     0,     0,
       0,     0,     0,    80,    81,     0,     0,     0,   854,   855,
       0,   787,   788,   789,   790,   791,   792,   793,   794,   795,
     796,   797,     0,     0,   118,     0,   220,     0,   172,     0,
       0,     0,   172,     0,     0,   172,   172,     0,     0,   172,
       0,   619,   172,   172,     0,   172,     0,   172,     0,     0,
       0,     0,   118,   798,     0,     0,     0,   619,     0,     0,
       0,   619,   363,     0,     0,     0,   363,     0,     0,     0,
       0,   363,   363,     0,   619,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,   662,     0,
       0,   150,   363,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,     0,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,     0,   273,     0,   172,     0,
       0,   172,     0,     0,     0,     0,     0,   363,     0,     0,
       0,  1962,     0,     0,   459,     0,     0,     0,     0,     0,
     363,     0,     0,     0,   363,     0,   172,   949,   363,     0,
       0,     0,     0,     0,     0,     0,   348,     0,     0,     0,
       0,   172,   367,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1213,     0,     0,     0,     0,
     497,     0,     0,     0,     0,     0,     0,   112,     0,     0,
       0,     0,     0,     0,     0,     0,   117,     0,   150,     0,
     235,   236,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   397,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,     0,     0,     0,     0,    74,     0,    75,  2032,
       0,     0,     0,     0,  1962,     0,     0,     0,   367,     0,
     117,     0,     0,     0,     0,     0,   282,     0,  2107,    77,
     118,     0,   553,     0,     0,     0,   662,   497,  2106,    80,
      81,     0,  1051,     0,     0,     0,   172,   367,     0,     0,
     660,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     662,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,   662,     0,     0,     0,     0,   363,     0,
     495,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2142,     0,     0,     0,     0,     0,     0,
       0,   118,   118,     0,     0,   273,   389,     0,     0,     0,
       0,     0,     0,   619,  2157,     0,     0,   619,     0,     0,
     118,     0,   172,     0,     0,     0,   619,     0,     0,  2166,
       0,     0,   118,     0,  1131,     0,   619,     0,     0,     0,
       0,     0,     0,   619,     0,  1147,     0,     0,     0,     0,
     363,     0,     0,   363,   363,     0,   363,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   497,   149,     0,     0,
       0,   363,     0,     0,     0,   363,     0,     0,     0,   363,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
       0,   619,     0,     0,     0,   619,     0,     0,   367,   619,
       0,     0,   367,     0,     0,     0,   389,     0,     0,     0,
     662,   497,     0,     0,     0,   367,     0,     0,     0,     0,
       0,     0,     0,   112,  1215,     0,     0,   112,   112,     0,
     367,     0,   367,     0,     0,     0,   367,   367,   497,   367,
       0,   112,     0,   207,     0,     0,   172,   172,     0,     0,
       0,     0,     0,     0,     0,     0,   367,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   172,     0,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1322,
       0,     0,     0,     0,     0,     0,   495,     0,     0,     0,
       0,   363,    14,    15,    16,    17,    18,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   367,     0,     0,     0,
     117,     0,     0,     0,     0,   367,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     662,     0,     0,   284,     0,   363,     0,     0,     0,     0,
       0,     0,    58,     0,   363,     0,     0,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   207,     0,     0,     0,     0,   172,   172,     0,
       0,     0,     0,     0,   172,     0,   150,     0,   235,   236,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,   497,     0,     0,     0,     0,     0,   172,
       0,     0,   172,   172,    74,   172,    75,   172,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   580,
       0,     0,     0,     0,     0,     0,   237,    77,     0,   282,
       0,     0,   619,     0,     0,     0,   619,    80,    81,     0,
     619,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     172,     0,     0,     0,   172,     0,     0,     0,   172,     0,
     622,     0,     0,     0,   367,     0,     0,     0,   367,     0,
       0,     0,   629,   367,   367,     0,     0,   367,     0,     0,
       0,    14,    15,    16,    17,    18,     0,   367,     0,   580,
       0,     0,     0,     0,   367,     0,     0,     0,     0,     0,
    1482,  1484,  1486,     0,     0,     0,     0,     0,     0,     0,
       0,   657,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   367,
       0,     0,   172,     0,  1508,     0,     0,     0,     0,     0,
       0,    58,   367,     0,     0,     0,   367,     0,     0,     0,
     367,     0,   619,     0,   374,  1215,     0,     0,     0,     0,
       0,  1527,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   150,   745,   235,   236,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   117,
       0,     0,   484,   374,     0,     0,     0,     0,     0,   112,
       0,     0,     0,    74,     0,    75,   619,   786,     0,     0,
       0,     0,     0,     0,     0,   619,     0,     0,     0,   619,
       0,     0,     0,   117,     0,   327,    77,   556,     0,     0,
       0,     0,     0,     0,   556,   826,    80,    81,     0,     0,
     831,     0,     0,     0,     0,     0,     0,     0,   284,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     857,   858,     0,     0,     0,   859,   860,     0,     0,   863,
       0,     0,   662,     0,   172,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   876,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     367,     0,   497,     0,     0,     0,     0,   905,     0,     0,
       0,     0,     0,     0,     0,   660,   556,     0,     0,   172,
       0,   172,     0,     0,   172,     0,     0,   172,     0,     0,
       0,   172,     0,     0,     0,     0,     0,     0,     0,     0,
    1677,  1678,     0,     0,   374,   672,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,   693,     0,     0,     0,     0,     0,
       0,     0,   367,     0,     0,   367,   367,     0,   367,     0,
       0,     0,     0,     0,     0,   945,   112,   660,     0,     0,
       0,     0,     0,   367,     0,     0,     0,   367,   951,     0,
       0,   367,     0,     0,     0,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
       0,     0,   970,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1769,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   556,   117,     0,     0,     0,   117,
     117,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   556,   821,   117,   556,   824,     0,     0,     0,   172,
       0,     0,     0,     0,   374,     0,     0,     0,   672,     0,
       0,     0,     0,     0,     0,     0,     0,  1023,     0,     0,
       0,   484,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,     0,   497,     0,
       0,     0,     0,   367,     0,     0,     0,     0,     0,     0,
       0,     0,   556,     0,     0,     0,   556,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   172,
    1839,     0,   172,     0,   172,   172,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,   367,     0,     0,
       0,     0,     0,     0,     0,     0,   367,     0,     0,  1103,
     367,  1104,     0,     0,     0,     0,     0,   831,     0,     0,
     150,     0,   235,   236,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1148,     0,     0,    74,     0,
      75,     0,   556,     0,  1157,     0,     0,     0,  1159,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1665,    77,   960,   374,     0,     0,     0,     0,     0,     0,
       0,    80,    81,   672,     0,     0,     0,   672,     0,     0,
       0,   284,     0,   172,   978,     0,   374,     0,     0,     0,
       0,     0,     0,   657,     0,     0,   619,     0,  1201,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   186,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,    19,     0,    20,
     170,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,   259,   260,     0,   261,
      46,     0,    47,     0,     0,   262,     0,     0,    49,    50,
      51,    52,    53,    54,    55,  1332,   172,     0,  1992,     0,
       0,     0,   374,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   556,   556,
       0,     0,     0,     0,   172,   302,     0,     0,     0,   556,
    1097,     0,   556,  1100,     0,     0,     0,     0,   308,     0,
     309,     0,     0,     0,     0,     0,   960,   374,     0,     0,
       0,   672,     0,   672,   672,     0,   172,     0,     0,     0,
     672,   117,   172,     0,     0,   216,   374,     0,   374,     0,
       0,   381,   374,   374,   374,   374,     0,     0,     0,     0,
       0,   277,     0,     0,     0,     0,     0,     0,  -474,     0,
       0,     0,   374,     0,   556,     0,     0,     0,   556,     0,
       0,     0,     0,     0,     0,   556,  1175,     0,     0,   556,
    1179,  -474,     0,   556,  1183,     0,     0,     0,     0,     0,
    1186,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,     0,     0,     0,   338,     0,   172,     0,
       0,     0,     0,     0,     0,     0,   558,   559,   379,     0,
     563,     0,     0,   566,   567,     0,   569,     0,   570,     0,
       0,   374,   556,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   216,     0,     0,     0,   662,     0,     0,
       0,   149,     0,     0,     0,   172,   172,   504,     0,   672,
       0,   510,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   172,   172,     0,     0,     0,     0,     0,   117,   389,
       0,     0,     0,     0,   172,     0,     0,     0,     0,     0,
     786,     0,     0,     0,     0,     0,     0,     0,   484,   374,
    2010,     0,     0,     0,     0,     0,   216,     0,   117,   662,
       0,     0,     0,     0,     0,     0,     0,   655,     0,     0,
       0,   277,     0,     0,     0,     0,     0,   367,     0,     0,
       0,     0,   687,     0,     0,     0,     0,     0,     0,   117,
       0,     0,     0,     0,  1538,     0,     0,     0,   404,     0,
       0,   405,     0,   406,   407,     0,   408,     0,     0,     0,
       0,     0,     0,     0,   556,     0,   510,     0,  1563,     0,
       0,     0,     0,   409,     0,     0,   216,     0,     0,   374,
     374,     0,   172,   672,   672,     0,     0,     0,     0,     0,
     672,     0,     0,     0,     0,     0,     0,     0,   665,     0,
     682,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,     0,   419,   420,     0,     0,
       0,     0,     0,     0,    74,   374,     0,   819,     0,   556,
    1426,     0,   556,  1430,     0,     0,   556,  1434,     0,     0,
       0,     0,     0,     0,   172,     0,   421,     0,     0,    78,
     422,   743,     0,     0,     0,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   216,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   902,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   665,     0,
       0,     0,     0,     0,   849,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   172,     0,
       0,     0,     0,     0,     0,     0,     0,   216,     0,     0,
       0,     0,     0,  1751,     0,     0,     0,     0,   374,     0,
       0,     0,     0,     0,   672,  1546,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,     0,
       0,     0,     0,   216,   216,     0,     0,     0,     0,     0,
     504,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   976,   977,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   556,  1600,     0,     0,   986,     0,   988,
       0,     0,     0,     0,   556,  1609,     0,   672,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,     0,
       0,   374,   374,     0,   374,     0,     0,     0,     0,     0,
       0,     0,  1751,   297,     0,     0,     0,   504,     0,   964,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     665,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,     0,     0,     0,     0,   370,     0,     0,
     216,     0,     0,   743,     0,   743,   216,     0,   216,     0,
       0,     0,     0,     0,     0,     0,     0,   743,  1090,  1091,
     743,   743,   743,     0,     0,  1095,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1893,  1894,     0,     0,
    1118,     0,     0,  1121,  1122,     0,  1125,     0,  1127,  1128,
       0,     0,     0,     0,   374,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   504,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   672,     0,     0,     0,     0,     0,     0,     0,     0,
     216,  1173,     0,     0,     0,  1177,     0,     0,     0,  1181,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   504,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   370,     0,     0,     0,     0,     0,
     504,     0,   504,     0,     0,     0,   504,   504,   379,   504,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   504,     0,     0,     0,
    1977,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   556,     0,  1313,     0,     0,     0,   370,     0,   370,
     370,     0,     0,     0,     0,     0,     0,   556,     0,     0,
    1751,     0,     0,   370,     0,     0,     0,   370,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   504,     0,  2018,     0,     0,
       0,     0,   216,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   849,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2048,     0,     0,     0,
    2049,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -499,  -499,     0,  -499,    46,
       0,    47,     0,   379,  -499,     0,     0,   370,     0,     0,
       0,     0,     0,   370,     0,  1313,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,   556,
     556,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   556,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
    1419,     0,  1421,     0,     0,  1424,     0,     0,  1428,     0,
       0,     0,  1432,     0,     0,     0,     0,     0,     0,     0,
       0,   370,     0,   504,   504,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   370,
      14,    15,    16,    17,    18,     0,     0,    20,    78,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,   370,     0,     0,     0,    46,   504,
      47,     0,     0,     0,     0,   556,   369,     0,     0,     0,
       0,     0,     0,   556,     0,     0,     0,   370,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,   370,     0,   370,     0,
       0,     0,     0,     0,     0,   369,   370,     0,     0,     0,
     743,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,   370,     0,     0,     0,     0,     0,     0,   370,
    1544,     0,   370,     0,     0,     0,     0,     0,     0,   556,
    2070,     0,     0,   556,    75,   743,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   277,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   556,     0,     0,
       0,     0,     0,     0,     0,   216,     0,     0,     0,  1598,
       0,     0,   665,   369,     0,     0,     0,     0,     0,     0,
    1607,     0,     0,  1611,     0,  1614,  1615,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   743,     0,     0,
       0,   370,     0,   556,   556,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,   370,   369,   369,
       0,   370,     0,     0,     0,     0,     0,     0,     0,   370,
     370,     0,   369,     0,   370,     0,   369,     0,     0,     0,
       0,   556,     0,   739,     0,     0,     0,     0,     0,   370,
       0,   370,     0,     0,     0,   370,   370,   370,   370,     0,
       0,     0,   504,     0,     0,   504,   504,     0,   379,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,     0,
       0,     0,     0,     0,  1732,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     743,   743,   743,     0,     0,   743,   743,     0,     0,     0,
       0,     0,   510,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,     0,
       0,     0,     0,     0,   370,     0,   369,     0,     0,     0,
     216,     0,   369,     0,     0,     0,     0,     0,     0,   370,
       0,   370,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   277,     0,     0,     0,     0,     0,  1611,     0,     0,
       0,     0,     0,     0,     0,   913,   915,     0,   379,     0,
       0,     0,   371,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1801,     0,     0,     0,     0,
     369,     0,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,   371,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   369,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   370,     0,     0,   369,   370,     0,     0,
       0,     0,   370,   370,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,   369,   369,   370,   369,     0,     0,
       0,   216,     0,   370,     0,   369,     0,     0,     0,  1890,
       0,     0,   739,     0,     0,   739,     0,     0,   369,   371,
     739,   369,     0,     0,     0,     0,     0,     0,   369,   739,
     277,   369,     0,     0,     0,     0,     0,     0,   370,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   739,     0,
       0,   370,     0,     0,     0,   370,  1918,  1919,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   371,     0,   371,   371,     0,     0,     0,     0,
       0,     0,  1933,  1934,  1076,     0,     0,     0,   371,     0,
       0,     0,   371,     0,     0,  1938,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   743,
       0,     0,     0,     0,   369,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,     0,     0,
     369,     0,     0,     0,     0,     0,     0,     0,   369,   369,
       0,     0,     0,   369,     0,     0,     0,     0,     0,     0,
       0,   370,     0,   277,     0,   370,     0,     0,   369,     0,
     369,     0,     0,  2007,   369,   369,   369,   369,     0,     0,
       0,     0,   371,     0,     0,     0,     0,     0,   371,   370,
       0,   370,     0,     0,   369,     0,     0,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,  2068,    47,     0,     0,     0,
       0,     0,     0,     0,   369,     0,   371,     0,     0,     0,
       0,   370,     0,   369,   370,   370,    58,   370,     0,     0,
       0,     0,     0,     0,   371,     0,     0,     0,   369,     0,
     369,   369,   370,     0,     0,     0,   370,     0,     0,     0,
     370,     0,   457,     0,     0,     0,     0,   743,     0,     0,
     721,     0,   722,   723,     0,     0,   489,     0,     0,   371,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     519,     0,   519,     0,     0,     0,     0,     0,     0,     0,
      75,     0,   371,     0,     0,     0,     0,     0,     0,     0,
       0,   369,     0,     0,     0,     0,     0,     0,     0,     0,
     371,   371,     0,   371,     0,     0,     0,   -17,     0,     0,
       0,   371,     0,     0,     0,     0,     0,     0,     0,   743,
       0,     0,   510,     0,   371,     0,     0,   371,     0,     0,
       0,     0,     0,     0,   371,     0,     0,   371,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   370,     0,     0,
       0,     0,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,     0,     0,   369,     0,     0,     0,
       0,   369,   369,     0,   634,   369,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   369,     0,     0,     0,     0,
       0,     0,   369,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,   369,     0,     0,
     371,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,     0,   369,     0,   371,     0,   369,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   371,     0,     0,     0,   371,     0,     0,     0,
       0,     0,     0,     0,   371,   371,     0,   739,     0,   371,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   371,     0,   371,     0,     0,     0,
     371,   371,   371,   371,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     371,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   519,     0,     0,     0,     0,
     369,   519,     0,     0,   369,     0,   457,     0,     0,     0,
     371,     0,     0,     0,     0,     0,     0,     0,     0,   371,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
     369,     0,     0,     0,   371,     0,   371,   371,     0,   264,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -499,  -499,     0,  -499,    46,     0,    47,
       0,     0,  -499,     0,     0,     0,     0,   371,     0,     0,
     369,     0,     0,   369,   369,     0,   369,     0,   944,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   369,     0,     0,     0,   369,     0,     0,     0,   369,
       0,     0,     0,     0,     0,     0,   489,  1684,  1692,     0,
       0,  1684,  1703,     0,     0,    63,    64,  1710,     0,   972,
       0,  1714,     0,  1716,     0,  1703,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   371,     0,
       0,    74,   371,    75,     0,     0,     0,   371,   371,     0,
       0,   371,     0,     0,     0,     0,  1005,     0,     0,     0,
       0,   371,     0,     0,     0,     0,    78,   265,   371,  1016,
       0,     0,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,  1037,  1039,     0,     0,  1041,     0,  1043,
       0,     0,     0,   371,     0,  1005,   369,  1053,  1005,     0,
       0,   369,     0,     0,     0,     0,   371,     0,     0,     0,
     371,     0,     0,     0,   371,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1081,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1083,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   370,  1092,
       0,   370,     0,     0,     0,   369,     0,     0,     0,     0,
       0,     0,     0,  1805,   369,   489,   370,     0,   369,     0,
    1081,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1151,     0,     0,   519,     0,     0,  1842,     0,
       0,     0,     0,     0,     0,     0,  1162,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1862,  1864,
       0,     0,     0,     0,     0,     0,   371,     0,     0,     0,
     371,     0,     0,     0,  1187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1884,
       0,     0,     0,     0,   371,     0,   371,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1303,  1305,     0,     0,     0,     0,     0,     0,   489,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   371,     0,     0,   371,
     371,     0,   371,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   371,     0,     0,
       0,   371,  1081,     0,     0,   371,     0,  1948,     0,     0,
    1346,     0,     0,     0,     0,  1951,     0,  1953,     0,  1005,
    1958,  1964,     0,  1703,     0,     0,     0,     0,  1970,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1453,     0,  1454,     0,     0,
       0,     0,  1455,     0,     0,    14,    15,    16,    17,    18,
      19,   519,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,   371,     0,     0,    58,  1456,   371,     0,     0,
       0,     0,     0,  2036,     0,     0,     0,     0,     0,  2042,
    2044,     0,     0,     0,   519,     0,  1418,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,  2063,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   371,     0,     0,     0,   369,     0,    74,     0,    75,
     371,     0,     0,     0,   371,     0,     0,     0,  2085,     0,
    2088,     0,     0,     0,     0,  2090,  2092,     0,  1457,     0,
    2095,  2097,    78,  1011,     0,     0,     0,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,  1495,  1495,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   369,     0,     0,
     369,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2136,  2138,  2140,   369,     0,     0,     0,     0,
       0,     0,  2176,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1542,     0,     0,     0,  1477,
       0,  1551,     0,  2161,  2163,  2165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1005,     0,     0,     0,
       0,   489,     0,     0,     0,     0,     0,     0,     0,     0,
     404,     0,     0,   405,     0,   406,   407,     0,   408,   519,
       0,     0,  1579,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1218,     0,   409,  1220,  1037,  1221,  -252,
    -252,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,
    1231,  1232,  1233,  -351,  -351,  1234,  1235,  1236,  1237,  1238,
    1239,  1240,     0,  1241,     0,   410,   411,     0,   513,   413,
    1242,  1243,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,  1244,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1642,  1643,     0,     0,     0,     0,  -252,  1245,     0,
       0,    78,   422,     0,     0,     0,   306,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,  1005,     0,     0,
       0,     0,     0,  -192,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   519,     0,     0,   457,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2176,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1477,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1039,     0,     0,
       0,     0,     0,     0,     0,     0,  1759,  1760,     0,     0,
       0,     0,     0,     0,     0,   404,     0,     0,   405,     0,
     406,   407,     0,   408,     0,     0,     0,     0,     0,     0,
       0,   371,     0,     0,     0,   519,     0,     0,  1218,  1037,
     409,  1220,     0,  1221,  -253,  -253,  1222,  1223,  1224,  1225,
    1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  -351,  -351,
    1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,     0,
     410,   411,     0,   513,   413,  1242,  1243,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,  1244,   416,
     417,   418,     0,   419,   420,     0,     0,     0,     0,   404,
       0,    74,   405,   371,   406,   407,   371,   408,  1956,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     457,   371,  -253,  1245,   409,  1830,    78,   422,     0,     0,
       0,   306,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,     0,  -192,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,     0,   419,   420,     0,
       0,     0,     0,     0,     0,    74,  1877,  1880,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1696,  1697,  1698,  1699,  1477,     0,     0,   421,  1957,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,   519,
       0,     0,     0,     0,     0,   404,  1908,     0,   405,  1910,
     406,   407,     0,   408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1218,     0,
     409,  1220,     0,  1221,     0,  1924,  1222,  1223,  1224,  1225,
    1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  -351,  -351,
    1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,     0,
     410,   411,     0,   513,   413,  1242,  1243,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,  1244,   416,
     417,   418,     0,   419,   420,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1245,     0,     0,    78,   422,     0,     0,
       0,   306,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,     0,  -192,     0,
       4,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1217,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,   407,     0,   408,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1218,
      58,  1219,  1220,  1005,  1221,     0,     0,  1222,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  -351,
    -351,  1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,
       0,   410,   411,    61,   513,   413,  1242,  1243,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,  1244,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -3,  1245,     0,     0,    78,  1246,     0,
       0,     0,   306,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,     0,  -192,
       4,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1217,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,   407,     0,   408,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1218,
      58,  1219,  1220,     0,  1221,     0,     0,  1222,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  -351,
    -351,  1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,
       0,   410,   411,    61,   513,   413,  1242,  1243,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,  1244,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1245,     0,     0,    78,  1246,     0,
       0,     0,   306,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,     0,  -192,
       4,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,   407,     0,   408,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   409,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,    61,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1696,  1697,  1698,
    1699,     0,     0,     0,   421,  1700,  1701,    78,  1246,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,     0,  1702,
       4,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,   407,     0,   408,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   409,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,    61,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1696,  1697,  1698,
    1699,     0,     0,     0,   421,  1700,     0,    78,  1246,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,     0,  1702,
       4,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,   407,     0,   408,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   409,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,    61,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   421,     0,  1694,    78,  1246,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     4,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,   407,     0,   408,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,    61,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,  1246,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,   407,     0,   408,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   486,
       0,     0,     0,     0,     0,   423,   487,    81,   424,   425,
     426,   427,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,   407,     0,   408,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,  1300,
       0,     0,     0,     0,     0,   423,  1301,    81,   424,   425,
     426,   427,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,   407,     0,   408,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   833,
       0,     0,     0,     0,     0,   423,   487,    81,   424,   425,
     426,   427,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,   407,     0,   408,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,   407,     0,   408,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   833,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,  2017,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,     0,    -2,     0,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,
       0,    -2,     0,     0,    -2,     0,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,    -2,
      -2,  2047,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,    -2,     0,    -2,     0,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,  1311,    -2,     0,     0,     0,     0,    -2,    -2,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,   404,     0,     0,   405,
       0,   406,   407,     0,   408,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,    -2,     0,     0,
      58,   409,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,    -2,    -2,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,     0,   419,   420,     0,  1548,     0,     0,
       0,     0,    74,     0,    75,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   421,     0,     0,    78,   422,     0,
       0,     0,     0,     0,   423,   487,    81,   424,   425,   426,
     427,   404,     0,     0,   405,     0,   406,   407,     0,   408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   421,
       0,     0,    78,   422,     0,     0,     0,     0,     0,   423,
    1549,    81,   424,   425,   426,   427,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,    59,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    61,
      62,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,    79,     0,     0,     0,     0,     0,
       0,    80,    81,   264,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -499,  -499,     0,
    -499,    46,     0,    47,     0,     0,  -499,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,   265,     0,     0,     0,  -826,     0,     0,    80,    81,
     264,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -499,  -499,     0,  -499,    46,     0,
      47,     0,     0,  -499,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,   265,     0,
       0,     0,     0,     0,     0,    80,    81,     4,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,     0,     0,
       0,     0,  -418,  -418,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -418,     0,     0,     0,    78,    79,     0,     0,     0,     0,
       0,     0,    80,    81,     4,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,     0,     0,     0,     0,  -419,
    -419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -419,     0,     0,
       0,    78,    79,     0,  1453,     0,  1454,     0,     0,    80,
      81,  1455,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1456,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1651,     0,     0,
       0,    78,  1011,     0,  1453,     0,  1454,     0,     0,    80,
      81,  1455,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1456,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1652,     0,     0,
       0,    78,  1011,     0,  1453,     0,  1454,     0,     0,    80,
      81,  1455,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1456,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   264,     0,     0,  1653,     0,     0,
       0,    78,  1011,     0,    14,    15,    16,    17,    18,    80,
      81,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -499,  -499,
       0,  -499,    46,     0,    47,     0,     0,  -499,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   507,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,   355,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,   613,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1107,    77,  -692,    78,   668,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -499,  -499,     0,  -499,    46,     0,    47,     0,     0,  -499,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     150,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   265,     0,     0,     0,  -830,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -499,  -499,     0,  -499,
      46,     0,    47,     0,     0,  -499,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   150,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,    78,
     265,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,   613,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   667,     0,  -692,    78,   668,     0,     0,     0,
       0,     0,     0,    80,    81,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,   355,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,   613,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   802,     0,
    -692,    78,   553,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,   355,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,  1140,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -700,    78,   919,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   356,    78,   357,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,  1621,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     919,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,  1623,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   919,     0,     0,     0,
       0,     0,     0,    80,    81,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,   355,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   507,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,   355,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   919,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   357,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -499,  -499,     0,  -499,
      46,     0,    47,     0,     0,  -499,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,  1477,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   404,     0,     0,   405,
       0,   406,   407,     0,   408,     0,     0,     0,     0,    78,
     265,     0,     0,     0,     0,     0,     0,    80,    81,  1218,
       0,   409,  1220,     0,  1221,  1941,  1942,  1222,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,     0,
       0,  1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,
       0,   410,   411,     0,   513,   413,  1242,  1243,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,  1244,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,  1477,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1245,     0,     0,    78,   422,     0,
       0,     0,   306,     0,   423,    80,    81,   424,   425,   426,
     427,     0,   404,     0,     0,   405,     0,   406,   407,  -192,
     408,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1218,     0,   409,  1220,     0,
    1221,     0,     0,  1222,  1223,  1224,  1225,  1226,  1227,  1228,
    1229,  1230,  1231,  1232,  1233,     0,     0,  1234,  1235,  1236,
    1237,  1238,  1239,  1240,     0,  1241,     0,   410,   411,     0,
     513,   413,  1242,  1243,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,  1244,   416,   417,   418,     0,
     419,   420,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1245,     0,     0,    78,   422,     0,     0,     0,   306,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,     0,  -192,   310,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -422,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,     0,     0,     0,     0,  -422,   310,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -423,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,     0,
       0,  -423,   310,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,    14,    15,    16,    17,    18,    19,
     730,    20,   731,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    63,    64,
     404,     0,    46,   405,    47,   406,   407,     0,   408,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   732,     0,     0,
       0,     0,  1233,     0,  -351,     0,     0,     0,     0,    78,
       0,     0,     0,     0,  -422,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1245,     0,
       0,    78,   733,     0,     0,     0,   306,     0,   423,    80,
      81,   734,   735,   426,   427,    14,    15,    16,    17,    18,
      19,   730,    20,   731,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,   407,     0,   408,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   732,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   421,
       0,     0,    78,   733,     0,     0,     0,   306,     0,   423,
      80,    81,   734,   735,   426,   427,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,   407,     0,
     408,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,     0,
     419,   420,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,     0,   452,    78,   453,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   404,     0,    46,   405,    47,   406,   407,
       0,   408,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   409,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
       0,   419,   420,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   421,     0,     0,    78,   453,     0,     0,     0,   306,
       0,   423,    80,    81,   424,   425,   426,   427,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   404,     0,    46,   405,    47,   406,
     407,     0,   408,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   409,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,     0,   419,   420,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   421,     0,     0,    78,   733,     0,     0,     0,
     306,     0,   423,    80,    81,   424,   425,   426,   427,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   404,     0,    46,   405,    47,
     406,   407,     0,   408,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     409,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,     0,   419,   420,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   421,     0,     0,    78,   453,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,   407,     0,   408,   355,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   409,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   421,     0,     0,    78,   833,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,   407,     0,   408,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,   264,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -499,  -499,     0,  -499,
      46,     0,    47,     0,     0,  -499,     0,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
       0,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     150,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   613,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -692,    78,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,    77,
       0,    78,    79,     0,     0,     0,  -828,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   150,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,     0,    78,   208,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   150,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,   480,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   856,
       0,     0,    78,   481,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   150,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
      79,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,   480,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   481,     0,     0,
       0,     0,     0,     0,    80,    81,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
      14,    15,    16,    17,    18,    58,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -499,  -499,     0,  -499,    46,     0,
      47,    63,    64,  -499,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     613,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -692,    78,     0,   150,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,   507,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,  1211,     0,     0,     0,     0,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,    78,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   856,     0,     0,    78,   481,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,   355,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     856,     0,     0,    78,   481,     0,    63,    64,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1018,    78,  1011,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,  1567,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,  1011,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   318,     0,    63,    64,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   208,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,     0,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   357,     0,    63,    64,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   318,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   481,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -499,  -499,     0,  -499,
      46,     0,    47,     0,     0,  -499,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
       0,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   507,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,  1011,     0,    63,    64,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,    79,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,     0,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   481,     0,    63,    64,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,  1011,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
      14,    15,    16,    17,    18,    58,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -499,  -499,     0,  -499,    46,     0,
      47,    63,    64,  -499,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,     0,    63,    64,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   335,     0,
      14,    15,    16,    17,    18,    80,    81,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -499,  -499,     0,  -499,    46,     0,
      47,     0,     0,  -499,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
      58,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -499,
    -499,     0,  -499,    46,     0,    47,    63,    64,  -499,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   507,     0,
       0,    63,    64,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,     0,     0,     0,     0,     0,
      80,    81,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,   407,     0,   408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   421,
       0,     0,    78,   422,     0,     0,     0,     0,     0,   423,
     487,    81,   424,   425,   426,   427,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   404,     0,    46,   405,    47,
     406,   407,     0,   408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,     0,   419,   420,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   421,     0,     0,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,    78,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,     0,   355,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
      78,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,    58,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,   187,   404,   188,   189,
     405,     0,   406,   407,     0,   408,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   721,     0,
     722,   723,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,    75,   404,
       0,     0,   405,    74,   406,   407,     0,   408,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1696,  1697,
    1698,  1699,     0,     0,   409,   421,  1863,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   513,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,     0,   421,    77,     0,
     514,   515,     0,     0,     0,   516,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,     0,   421,  1349,     0,    78,   422,     0,
       0,     0,  1350,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
     404,   419,   420,   405,     0,   406,   407,     0,   408,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
       0,   421,     0,     0,    78,   422,     0,     0,     0,   516,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,   404,   419,   420,
     405,     0,   406,   407,     0,   408,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,     0,   421,  1001,
       0,    78,   422,     0,     0,     0,     0,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,   404,   419,   420,   405,     0,   406,
     407,     0,   408,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
       0,     0,     0,     0,     0,   421,  1036,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,   404,   419,   420,   405,     0,   406,   407,     0,   408,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,     0,   421,     0,     0,    78,   422,     0,     0,     0,
     306,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,   404,   419,
     420,   405,     0,   406,   407,     0,   408,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,     0,   421,
       0,     0,    78,   422,     0,     0,  1075,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,   404,   419,   420,   405,     0,
     406,   407,     0,   408,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,     0,   421,     0,     0,    78,
     422,     0,     0,     0,  1487,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,   404,   419,   420,   405,     0,   406,   407,     0,
     408,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,     0,   421,  1578,     0,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,     0,
     421,     0,     0,    78,   422,     0,     0,     0,  1770,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,     0,   421,     0,  1947,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,     0,   421,  1952,     0,    78,   422,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
       0,   419,   420,     0,     0,   404,     0,     0,   405,    74,
     406,   407,     0,   408,  1956,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,   421,  1963,     0,    78,   422,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,   404,   419,   420,   405,     0,   406,   407,     0,
     408,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,     0,   421,     0,     0,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,     0,
     421,  2041,     0,    78,   422,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,     0,   421,  2043,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,     0,   421,  2087,     0,    78,   422,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
     404,   419,   420,   405,     0,   406,   407,     0,   408,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
       0,   421,  2089,     0,    78,   422,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,   404,   419,   420,
     405,     0,   406,   407,     0,   408,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,     0,   421,  2091,
       0,    78,   422,     0,     0,     0,     0,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,   404,   419,   420,   405,     0,   406,
     407,     0,   408,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
       0,     0,     0,     0,     0,   421,  2094,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,   404,   419,   420,   405,     0,   406,   407,     0,   408,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,     0,   421,  2096,     0,    78,   422,     0,     0,     0,
       0,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,   404,   419,
     420,   405,     0,   406,   407,     0,   408,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,     0,   421,
    2135,     0,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,   404,   419,   420,   405,     0,
     406,   407,     0,   408,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,     0,   421,  2137,     0,    78,
     422,     0,     0,     0,     0,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,   404,   419,   420,   405,     0,   406,   407,     0,
     408,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,     0,   421,  2139,     0,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,     0,
     421,  2160,     0,    78,   422,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,     0,   421,  2162,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,     0,   421,  2164,     0,    78,   422,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
       0,   419,   420,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   421,     0,     0,    78,   422,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -498,  -498,     0,  -498,    46,   404,    47,     0,
     405,  -498,   406,   407,     0,   408,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,   404,    75,    74,   405,     0,   406,   407,     0,   408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   710,   409,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,   404,   419,
     420,   405,     0,   406,   407,     0,   408,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,     0,   713,
       0,     0,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,   404,   419,   420,   405,     0,
     406,   407,     0,   408,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,     0,   718,     0,     0,    78,
     422,     0,     0,     0,     0,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,     0,   419,   420,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   727,     0,     0,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -499,  -499,     0,  -499,    46,   404,
      47,     0,   405,  -499,   406,   407,     0,   408,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,   409,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,     0,   419,   420,     0,
       0,     0,     0,   404,    75,    74,   405,     0,   406,   407,
       0,   408,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,   409,     0,
      78,   422,     0,     0,     0,     0,     0,   423,   943,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
       0,   419,   420,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   421,     0,     0,    78,   422,     0,     0,     0,     0,
       0,   423,   487,    81,   424,   425,   426,   427
};

static const yytype_int16 yycheck[] =
{
       1,   527,   168,     4,    76,   180,   263,   168,   223,    76,
      76,   360,    98,    76,   184,   143,   185,    78,   955,   237,
     279,   680,    98,   421,   237,    59,   237,  1264,   232,   168,
     516,    85,   246,   135,   850,     1,   852,   756,     1,   253,
     759,     1,  1296,  1297,   573,   574,  1228,   170,  1201,     4,
     940,   671,   170,   185,   258,    56,    57,   803,    59,   237,
     987,   275,    98,   835,  1813,     1,  1050,   667,   995,   194,
     139,  1813,   286,  1945,  1813,    76,     1,  1245,   206,  1368,
    1941,   667,   737,   835,    85,   358,   667,   207,  1707,   423,
     237,   165,    93,    59,   835,  1079,    59,    98,   152,    59,
     101,   374,   837,   835,   105,   378,   237,   101,   182,   327,
     237,    91,     1,   673,   327,   149,   327,   586,   237,    85,
     928,   237,   837,    59,   196,   227,   247,     1,   597,   196,
     838,   237,    98,   196,    59,   101,   844,   102,   101,   105,
     948,   136,    76,   144,     0,   494,   147,  1814,   149,   327,
     105,   157,   238,    76,   155,   276,   113,   207,    73,    73,
    1198,   162,   238,  1245,  1148,   237,   287,  1205,     1,   170,
     237,   237,   641,   702,   237,   181,    93,   157,   247,   136,
     327,   182,   151,   149,  1359,   180,   149,   716,   182,   149,
    1210,    76,   157,   194,   195,   196,   327,   857,  1210,   859,
     327,   121,   238,   139,   173,   206,     0,   276,   327,   311,
     211,   327,   266,   149,  2075,   488,   876,   837,   287,   358,
     221,   327,   137,   137,   149,   226,    59,    85,   314,   195,
     231,   232,   233,    78,    79,   835,   237,   238,   314,  1858,
     301,   161,   328,   312,    73,   211,   161,  1285,   182,   835,
      76,    76,   328,  2125,   835,   327,   421,   258,   381,     0,
     327,   327,  1929,   381,   327,   266,   615,   268,    76,   864,
     388,   396,   238,   196,   163,   164,   277,   278,   314,   839,
     281,   810,   336,   843,    92,   945,  2158,   288,  1703,    10,
     164,  1580,   328,   352,   854,   855,   138,   182,   557,   157,
     266,   302,   303,   111,   305,    10,   565,   522,   137,   310,
     659,   247,  2078,   314,   237,   281,   149,   535,   281,   421,
     165,  1040,   535,   240,   535,   242,   327,   328,   157,  1219,
     126,   173,   249,   392,   683,  1107,   121,   338,  2104,   374,
     276,   690,  1088,   378,   163,   346,   347,   561,   314,   488,
     351,   287,   315,   973,   173,  1107,   484,   535,  2107,   757,
     530,  2127,   328,  1253,   160,  2107,  1107,   571,  2107,   492,
     196,   196,  1027,   577,   588,  1107,   312,   505,   712,  1114,
     381,   595,  1210,   656,  2051,   599,    62,    63,   535,   390,
     138,  1318,   393,   390,  1202,   396,   393,   360,   993,  1114,
    1108,   455,    90,    76,   535,   322,   323,   680,   535,  1447,
    1563,   237,   237,  1588,  1589,  1590,   535,   635,    91,   535,
     693,     1,   635,   159,   635,   173,   952,   163,   165,   535,
    1450,  1451,  1452,  1515,    85,   156,  1518,  1519,  1450,  1451,
    1452,  2108,    62,  1103,  1104,   182,   167,    98,  1863,  1864,
     101,   172,    20,   514,   105,   157,   925,   635,   560,    76,
     580,   547,   167,   535,   112,   466,   568,   172,   535,    78,
      79,   547,   535,   165,    90,   596,    93,   165,  2145,    59,
     172,   101,   109,   110,   162,   587,   163,   135,   635,   165,
     491,   492,   138,   113,  1114,   115,   598,   117,   940,  1159,
     466,   154,   503,   504,   635,   182,   182,  1107,   635,   113,
      73,   547,   165,   514,   163,   516,   635,   109,   110,   635,
    1702,  1107,   168,   169,   173,  1707,  1107,   596,   181,   635,
     580,   494,   136,   160,   535,   157,   156,   657,   157,   159,
     160,   680,  1957,  1958,   195,   710,   547,   163,   713,   165,
     715,   637,   157,   718,    73,   159,   165,   830,   182,   139,
     211,   637,   727,   635,   163,   730,   731,   732,   635,   149,
     571,  1131,   635,  1741,   137,   159,   577,   156,   579,   163,
     698,   547,   288,   182,   163,    73,  1402,   238,   165,  1405,
    1406,   211,   163,   159,   157,   172,  1082,   303,   161,  1583,
    1308,   637,   173,   182,   170,   171,   640,   657,   710,   131,
     132,   713,   535,   962,   784,   266,   718,   157,   137,   157,
     157,   656,  1450,  1451,  1452,   727,   164,  1554,   165,    59,
     281,   157,    62,    63,   635,    65,   637,  1891,   157,   640,
     157,   642,   161,  1302,   746,   150,   151,   152,   153,   137,
     651,   866,   615,   159,   655,   177,   178,   163,   693,  1741,
     596,   281,   163,   283,   284,   628,   786,   247,   173,   157,
      73,   637,   589,   161,   640,   182,  1858,   640,   163,   848,
     640,   182,   163,  1391,   163,     1,   687,   960,     4,   163,
     163,   852,   858,   163,   863,   315,   276,   182,   902,   163,
     320,   182,   163,   182,   640,   706,   326,   287,   182,   535,
     535,   831,   182,   927,   837,   640,   848,   163,   182,   858,
     683,  1316,   639,   163,   785,   163,    73,   157,  1323,    73,
     158,   157,   312,    70,   137,    73,   786,   165,     1,   165,
     360,     4,   182,    59,   182,   365,   172,   367,   749,  1468,
     751,  1988,   753,   163,   157,   157,   757,   159,   161,   760,
      76,    13,    14,    15,    16,    17,  1295,    73,    73,    85,
     161,   163,   182,  1955,     3,   166,   163,  1219,   163,    73,
    1375,   831,    98,     3,   785,   101,   173,  1969,   173,   105,
     137,   163,   412,   137,    73,   830,    59,   156,   160,   137,
     850,   173,   156,  1787,   163,  1789,   908,   640,   858,   163,
     157,  1253,  1749,   157,   161,   466,   157,   161,   168,   157,
     922,    73,   161,   161,   926,   175,   176,   166,   930,   830,
     163,   137,   137,   149,   835,    73,   837,  1764,   101,   155,
      73,   165,   105,   137,   159,   159,   466,  1245,   849,   164,
     164,   157,   157,  2035,   170,   161,   161,  1023,   137,   860,
     163,   159,   165,   157,  1350,   866,   164,   161,   869,   181,
     988,    73,    47,    48,   494,    50,   496,   497,   157,   195,
     196,    56,   161,     1,  1023,   137,   149,   159,  1147,   159,
     206,   161,   164,   513,  1022,   211,   547,   159,     3,   137,
     165,   902,   164,   159,   137,   221,  1075,   163,    13,    14,
      15,    16,    17,   109,   110,   231,   232,   233,    62,   157,
     157,   237,   238,   161,   157,   960,   157,   547,   161,   179,
     161,   150,   151,   152,   153,   137,   157,   938,   939,   940,
    1155,    59,   258,  1654,   163,  1565,    73,   121,  1659,   159,
     266,   571,   159,   163,   173,   157,   576,  1306,   578,   161,
    1555,   159,   106,   182,  1490,   281,   159,   111,    73,   159,
     114,    73,   116,  1023,   160,   161,   159,   940,   164,   159,
     940,   601,  1577,   603,   604,   163,   302,    22,  1157,   305,
    1113,  1114,   162,   994,   310,   615,   951,   157,   314,   962,
     163,  1487,   159,    73,   940,    73,   163,   102,   628,   159,
     137,   327,   328,   163,     3,   940,   596,   637,   281,   159,
     157,   139,   157,   163,    13,    14,    15,    16,    17,  1302,
     157,   149,   137,  1475,   161,   137,   157,  1479,  1480,   659,
      59,   661,   662,    62,    63,  1046,    65,   157,   157,  1050,
     159,  1493,   161,   157,  1311,   157,   162,  1455,   159,   161,
     640,   157,   163,   683,   684,   381,   135,   137,   165,   137,
     690,    13,    14,    15,    16,    17,    90,   657,  1079,   159,
    1245,  1082,   157,   163,    73,  1289,   161,   157,   157,   157,
    1576,   161,   161,   161,   114,   115,   116,   117,   118,   168,
     169,   157,  1204,   159,   165,   161,  1107,   940,   135,   156,
     159,   135,  1113,  1114,   163,   157,  1218,  1712,   182,   161,
     157,  1296,  1297,   157,   161,   165,   270,   135,   159,   247,
     157,    73,   163,   157,   161,  1661,  1238,   161,   764,   765,
     766,   168,   169,  1245,   168,   169,   162,  1148,   137,   157,
     466,   129,   130,   161,   181,   157,  1210,   159,   276,   161,
     168,   169,   159,  1302,  1330,     1,  1332,   165,     4,   287,
     181,  1766,  1337,   317,   159,   157,   492,   157,   163,  1438,
     159,   161,     1,   159,   163,     4,  1781,    13,    14,    15,
      16,    17,    18,  1332,   312,   137,   138,  1315,   514,   157,
     516,   159,  1371,   161,   150,   151,   152,   153,   159,   160,
      13,    14,    15,    16,    17,   172,   248,   163,   362,   535,
     364,  1222,   366,    59,  1225,  1226,  1227,   173,   159,   121,
     159,   547,   163,  1234,   163,   159,   182,   170,   171,   163,
      59,  1402,  1408,  1412,  1413,  1406,  1201,  1408,   163,    85,
     159,   831,  1253,   159,   163,   571,  1219,   163,  1259,  1219,
     159,   577,   159,   579,   163,  1984,   163,  1184,   412,  1408,
      73,   159,   174,  1274,   159,   163,  1277,  1278,   163,  1280,
    1412,  1413,  1332,  1219,  1770,  1286,   105,   159,  1289,   157,
    1253,   163,   157,  1253,  1219,   150,   151,   152,   153,   169,
     159,   159,   179,   139,   163,   163,  1901,   143,   133,   134,
    1905,  1277,  1278,   149,  1277,   157,   152,  1253,   173,   635,
     167,   637,   159,  1278,   640,   159,   163,   182,  1253,   163,
     149,   135,  1333,  2112,   137,   138,   159,  2116,   160,   655,
     163,  1475,   962,  1306,   157,  1479,  1651,  1652,  1653,  1350,
    1207,  1208,  1209,   973,   159,  1405,   162,   163,  1359,   159,
     940,   159,   982,   771,   772,   773,   774,  1469,  1470,   513,
     206,   687,  1538,   162,   163,  1461,   159,   640,  1672,  1673,
    1674,   162,   163,   409,   159,  1461,  1219,   159,   168,   169,
    1559,  1392,   162,   163,   162,   163,  1450,  1451,  1452,  1538,
     161,  1455,  1456,   162,   163,   157,  1475,   157,   434,   435,
    1479,   247,    13,    14,    15,    16,    17,    18,  1520,  1861,
    1253,   163,   164,   162,   163,  1461,   458,  1559,   264,   455,
     266,   162,     1,   749,   138,   751,   138,   753,   162,   163,
     276,    92,    93,   475,   760,   163,   478,  1665,   163,   593,
     164,   287,  1665,   164,  1665,   162,   163,   162,   163,   157,
    1461,   181,   488,   163,  1465,  1466,   162,   163,  1088,   785,
     162,   163,   162,   163,   310,   159,   312,   159,   596,   159,
      13,    14,    15,    16,    17,   159,  1487,  1665,  1538,   159,
      59,   162,   163,   159,  1114,   162,   163,   159,  1453,   159,
     336,    13,    14,    15,    16,    17,    18,   159,   540,   181,
    1511,  1512,  1475,   162,   830,   161,  1479,  1480,  1665,   835,
    1521,   837,   640,    13,    14,    15,    16,    17,    18,   165,
    1493,   162,   163,   165,  1665,   165,   105,   165,  1665,  1475,
      73,   162,   163,  1479,  1480,   165,  1665,   162,   163,  1665,
     866,   163,   164,   869,   159,  1521,  1557,  1493,  1521,  1665,
    1730,   163,   162,   163,   162,   163,   163,   164,  1654,    71,
     139,    78,    79,  1659,   182,  1576,  2018,   162,  1654,   157,
     149,  1667,  1583,  1659,  1360,  1361,   902,  1588,  1589,  1590,
      79,  1667,   769,   770,     1,   767,   768,     4,   162,  1665,
    1518,  1519,   135,    18,   137,   181,  2048,   165,  1563,    85,
    1828,   775,   776,   165,  1779,  1828,   182,  1828,  1654,   455,
    1722,   159,   938,  1659,   157,   159,  1277,  1278,   161,  1673,
    1674,  1667,   182,   165,   165,   168,   169,  2079,   162,  1219,
     162,    18,  1475,   162,   162,   156,  1479,  1480,   484,   159,
    1828,   159,    59,  1654,   159,   159,   159,  1277,  1659,   159,
    1493,   159,   159,   159,  1665,   159,  1667,    22,   159,   505,
     162,   159,   159,  1253,  1675,   156,   152,   940,   247,   156,
     165,  1828,   165,   165,    71,   159,  1306,   159,   951,   159,
    1894,   159,  1312,  1694,   101,   181,   159,  1828,   105,   165,
    1701,  1828,   163,  2052,  1977,   165,   159,   276,  1878,  1828,
     159,   159,  1828,   163,   159,   162,  1891,   193,   287,   163,
     159,   159,  1828,   159,   159,   159,   163,  1690,   159,   159,
    1046,  1732,   159,   159,  1050,   163,  1691,   763,   162,   162,
     159,   159,   149,   312,   156,  1831,   159,   316,   159,   156,
     159,   159,   162,   159,   159,  1831,   159,   159,   159,   159,
     596,    14,   156,  1079,   163,   163,  1082,   163,   163,  1770,
     802,   803,  1941,    18,   157,   157,  2049,   157,   157,   157,
     164,   813,   157,   157,   816,   157,  1787,   163,  1789,   162,
     266,  1107,   182,   164,   162,  1831,  1751,  1113,  1114,   165,
     207,   182,   156,   156,   640,   165,   163,   159,   159,  1941,
     159,  1981,   182,    58,    59,    60,    61,    62,    63,    64,
      65,   640,   940,   181,   159,   159,   182,  1828,   159,   162,
    1831,   124,  1148,   126,   127,   128,   162,   159,   163,  1840,
    1841,   162,  1462,   159,   159,   159,  1847,  2051,   159,   162,
     882,   156,  1938,   156,    81,   157,   157,   889,   182,  1860,
     336,   893,  1938,   182,   157,   897,   156,   160,   161,  1870,
    1521,  1872,   165,   166,   281,   182,   182,    93,   182,   157,
    1843,   157,   358,   182,  1885,   182,  1887,  1888,  1889,  2107,
      91,   159,  1893,  1894,  2107,  1475,  2107,   156,  1861,  1479,
    1480,  1521,  1938,   163,  2073,   156,  2075,   163,   162,   156,
     107,   159,   305,  1493,   111,   112,   113,   114,   115,   116,
     117,   118,   162,  1977,   162,  1861,   162,   156,   165,  2107,
     164,   164,   124,   156,   159,   159,   162,  1938,  1201,   159,
     162,  2073,  2112,  2075,  1945,  2114,  2116,  2117,  1949,   159,
     159,   159,   159,  1954,   156,  1956,  1219,   156,  1274,   164,
    2107,  1277,  1278,     4,     5,     6,     7,     8,     9,    10,
      11,    12,   159,  1289,  2143,   163,  2107,  2147,   182,   455,
    2107,   157,  2114,   157,   159,   157,   156,   156,  2107,   162,
    1253,  2107,  1018,   159,   162,  2049,   159,   162,  1024,   159,
    2170,  2107,   159,   159,  2174,    76,    76,   162,  2009,  1035,
     156,   182,   488,   157,  1277,  1278,  2185,  1333,   182,  2189,
    2021,   182,  2108,   159,  2025,    66,   157,   596,  1861,   162,
     162,   156,  2108,     1,  1350,  2107,     4,   156,   156,  2040,
    2107,  2107,   159,  1359,  2107,   159,   159,    76,    76,   161,
    2051,   527,  2053,  2185,   182,  2018,  1088,   173,   173,  2145,
      76,   182,   164,   156,   156,   156,   173,  2169,   158,  2145,
    1690,   640,  2108,   182,   173,   156,  1392,   164,   157,  2181,
     107,  2082,  2018,   163,   173,  2048,   158,   173,   182,   162,
      76,    59,   159,   158,   182,   159,   164,   573,   574,   159,
     156,  1219,   156,  1327,   940,   157,  2107,  2108,    76,  2145,
     159,   159,  2048,   182,  1779,   951,  2079,    85,   736,  2120,
     182,   940,   777,   182,  2125,   778,   780,  1240,  1253,   779,
      98,  1163,   951,   101,  1166,  1253,   781,   105,  1170,  2158,
    2075,  1869,  2108,  2079,  2145,  1461,   454,  1479,  2104,  1861,
    1493,  1961,  2153,  2064,  1740,  2156,  2142,  2158,  1723,  1723,
    2174,  2049,    13,    14,    15,    16,    17,  2117,  2048,   562,
      49,  1487,  1280,   580,   114,   143,   272,  1938,  2179,  2145,
    2007,   149,  1456,  1274,   152,  2018,  1022,   155,   156,  2190,
    1453,  1320,   651,  1219,  1769,   523,  1527,   866,  2199,  1751,
     168,     0,  1643,   802,   680,  1521,   802,   706,   802,    -1,
      -1,  1831,  1475,    -1,    -1,  2048,  1479,  1480,    -1,    -1,
      -1,    -1,    73,  1843,    -1,   193,   702,   195,   196,    -1,
    1493,    -1,    -1,   640,   710,    -1,    -1,    -1,   206,   207,
     716,    -1,   718,   211,    -1,   638,  2079,    -1,    -1,    -1,
     657,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1521,    -1,
    1576,    -1,    -1,    -1,   232,    -1,    -1,  1583,    -1,   237,
     238,    -1,  1588,  1589,  1590,    -1,  1302,    -1,    -1,    -1,
      -1,  1861,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
     258,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   266,    -1,
    1563,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,  1929,
     161,    -1,    -1,   281,  1340,  1341,  1342,   168,   169,   107,
      -1,  1347,  1348,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,   810,   123,    -1,   125,  1654,    -1,
      -1,    -1,    -1,  1659,    -1,  1377,   314,    -1,    -1,  1665,
      -1,  1667,   320,    -1,    -1,    -1,  1388,  1475,   326,   327,
     328,  1479,  1480,    -1,    -1,  1201,    -1,    -1,   336,    -1,
     158,   940,    -1,   161,  1210,  1493,    -1,    -1,    -1,   786,
      -1,    13,  1201,  1219,    13,    14,    15,    16,    17,    -1,
     358,   359,   360,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1219,    -1,    -1,    -1,    -1,    -1,   374,    -1,    -1,   802,
     378,    -1,    -1,    -1,    -1,    -1,    -1,  1253,    -1,    -1,
      -1,    -1,    -1,    -1,   831,   818,    -1,  1690,  1691,   822,
      -1,    -1,  2052,    -1,  1253,    -1,    -1,    -1,  2018,    -1,
      -1,    -1,    -1,   850,    73,   852,    -1,    -1,    -1,   856,
     857,    -1,   859,   421,  1770,    -1,   107,    89,    -1,  1278,
     111,   112,   113,   114,   115,   116,   117,   118,  2048,   876,
      -1,  1787,    -1,  1789,    -1,   107,   952,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   455,  1751,    -1,
     458,    -1,    13,    -1,    -1,    -1,    -1,    -1,   466,  2079,
      -1,    -1,    -1,    -1,    -1,    -1,   135,   158,   137,    -1,
     161,    -1,  1828,    -1,    -1,  1831,   484,    -1,    -1,    -1,
     488,    -1,    -1,    -1,   492,  2145,   494,    -1,   157,    -1,
      -1,    -1,   161,   940,    -1,    -1,    -1,   505,   945,   168,
     169,    -1,    -1,    -1,   951,    -1,    -1,   107,    -1,   109,
     221,   111,   112,   113,   114,   115,   116,   117,   118,   527,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   535,    89,     4,
       5,     6,     7,     8,     9,    10,    11,    12,  1894,   547,
    1843,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,  1861,    -1,
      -1,    -1,    -1,   571,    -1,   573,   574,    -1,    -1,   577,
      -1,    -1,   580,    -1,  1450,  1451,  1452,  1453,  1454,  1455,
    1456,   107,  1938,  1655,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,  1453,   207,   157,    -1,    -1,  1475,
    1956,    -1,    -1,  1479,  1480,    -1,    -1,    -1,    -1,    -1,
    1219,    -1,    -1,    -1,    -1,    -1,  1475,  1493,    -1,    -1,
    1479,  1480,    -1,    -1,    -1,    -1,    -1,   635,    -1,   637,
      -1,    -1,   640,    -1,  1493,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   207,  1253,    -1,    -1,    -1,   656,   657,
      -1,   659,    -1,    -1,    -1,    -1,  1103,  1104,    -1,   667,
      -1,  1094,    -1,   671,    -1,  1098,    -1,    -1,    -1,  1278,
    1736,    -1,   680,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   690,    -1,  1117,   693,    -1,  1563,    -1,    -1,
      -1,  1124,    -1,    -1,   702,  2051,    -1,  2053,    79,    -1,
      -1,    -1,   710,  1861,  1563,   713,    -1,   715,   716,    -1,
     718,    -1,  1159,    -1,    -1,  2018,    -1,    -1,    -1,   727,
      -1,    -1,   730,   731,   732,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,  1172,
      -1,  1813,  1814,  1176,    -1,  2048,   358,  1180,    -1,   361,
      -1,  2107,  2108,    -1,  1201,    -1,    -1,    -1,    -1,    58,
      -1,    -1,   374,    -1,    -1,    -1,   378,    66,    67,    68,
      69,    -1,  1219,    -1,    -1,    -1,  2079,    -1,   786,  1295,
      -1,    -1,    -1,    -1,    -1,    -1,  1302,    -1,    -1,  2145,
      -1,    -1,    -1,    -1,   802,   803,    -1,    -1,    -1,    -1,
      -1,   182,   810,   514,    -1,   516,  1253,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,   830,   831,    -1,    -1,    -1,   835,    -1,   837,
    1277,  1278,  1691,    -1,  2190,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   850,  2199,   852,    -1,    -1,    -1,   856,   857,
     858,   859,    -1,    -1,    -1,    -1,    -1,  1929,    -1,    -1,
    2018,    -1,   161,    -1,    73,    -1,  1475,    -1,   876,    -1,
    1479,  1480,    -1,    -1,    -1,  1751,   488,    -1,    -1,    -1,
     179,    -1,    -1,    -1,  1493,    -1,    -1,    -1,    -1,    -1,
    2048,    -1,  1751,    -1,   902,    -1,    -1,    -1,   107,    62,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1983,    -1,    -1,
      -1,  2079,    -1,     1,    -1,    -1,     4,    -1,   137,    -1,
      -1,  2003,   940,    -1,    -1,  2007,    -1,   945,   101,    -1,
      -1,    -1,    -1,   951,   952,    -1,    -1,    -1,   157,   158,
     113,   114,   960,    -1,   962,  1402,    -1,    -1,  1405,  1406,
      -1,    -1,    -1,    -1,    -1,   973,    -1,    -1,   580,    -1,
      -1,    -1,    -1,    -1,  1490,    -1,    -1,    -1,    -1,  2051,
      -1,    59,    -1,    -1,    -1,  1861,    -1,    -1,    -1,    -1,
    1423,    -1,    -1,   156,  1427,    -1,    -1,    -1,  1431,    -1,
      -1,    -1,  1861,    -1,    -1,    -1,  1453,    85,    -1,    -1,
      -1,    -1,    -1,    73,  1022,  1023,   580,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,  1475,    -1,
      -1,    -1,  1479,  1480,    -1,  2107,  2108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   656,   657,  1493,   107,   211,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,   139,    -1,    -1,    -1,   143,    -1,    -1,   680,    -1,
      -1,   149,    -1,  2145,  1521,   135,    -1,   137,    -1,    -1,
    1088,   693,  1691,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,   657,    -1,  1103,  1104,   157,   158,  1107,
      -1,   161,    -1,    -1,    -1,    -1,  1114,    -1,   168,   169,
    1543,    -1,    -1,    -1,    -1,   107,  1563,   195,   281,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   206,   207,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,  2018,    -1,    -1,  1661,    -1,    -1,    -1,    -1,
      -1,  1159,   315,    -1,    -1,   866,    -1,   320,   869,  2018,
     238,    -1,    -1,   326,  1597,   157,   158,    -1,    -1,   247,
      -1,    -1,  2048,  1606,   786,    -1,    -1,  1610,    -1,    -1,
     258,    -1,    -1,    -1,    -1,   263,   264,    -1,   266,  2048,
      -1,    -1,    -1,  1201,    -1,    -1,    -1,   360,   276,    -1,
      -1,    -1,  1210,  2079,    -1,   182,    -1,    -1,    -1,   287,
      -1,  1219,   290,    -1,    -1,    -1,   294,    -1,   830,   831,
    2079,   299,   786,    -1,    -1,    -1,    76,   305,    -1,    -1,
      -1,    -1,   310,    -1,   312,    -1,    -1,  1245,   316,    -1,
      -1,    -1,    -1,  1690,  1691,  1253,   858,    -1,    98,   412,
     328,   107,  1861,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    18,    -1,    -1,   831,    -1,  1277,
    1278,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     358,  1289,    -1,   361,    -1,    -1,   850,  1295,   852,    -1,
      -1,    -1,   856,   857,  1302,   859,   374,    -1,    -1,    -1,
     378,    -1,    -1,    -1,  1751,   155,    -1,    -1,    62,    63,
      64,    65,   876,    -1,    -1,    -1,    -1,   185,    -1,    -1,
      -1,    -1,    -1,    -1,  1332,    -1,   182,    -1,    -1,  1337,
      -1,   494,    -1,    -1,    -1,  1046,    -1,    -1,    -1,  1050,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   960,    -1,
     513,   963,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,  1079,    -1,
      -1,  1082,   936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   945,   232,    -1,    -1,    -1,    -1,   237,   238,    -1,
      -1,    -1,    -1,    -1,  1402,    -1,  1843,  1405,  1406,    -1,
    1408,    -1,    -1,    -1,    -1,    -1,   484,   161,   258,  2018,
     488,  1023,   107,    -1,  1861,   578,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,   505,   123,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,  1148,   601,  2048,
      -1,    -1,  1450,  1451,  1452,  1453,    -1,  1455,  1456,    -1,
      -1,    -1,   615,  1461,  1462,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   314,   628,   161,  1475,    -1,    -1,
    2079,  1479,  1480,    -1,    -1,    -1,    -1,   327,   328,    -1,
      -1,    -1,  1490,    -1,   562,  1493,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   659,    -1,    -1,    -1,
      -1,   107,   580,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,  1521,    -1,    -1,    -1,    -1,   596,    -1,
     683,    -1,    -1,    -1,    -1,    -1,    -1,   690,    -1,    -1,
    1538,    -1,    -1,    -1,    -1,    -1,   404,    -1,   406,  1103,
    1104,   409,   410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   419,   420,    -1,    -1,  1563,    -1,  1565,    -1,   165,
     638,    -1,   640,    -1,    -1,    -1,   434,   435,    -1,    -1,
      -1,  2018,    -1,    -1,    -1,    -1,    -1,    -1,   656,   657,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   455,    -1,   667,
      -1,    -1,    -1,   671,    -1,  1159,    -1,    -1,    -1,    -1,
      -1,  2048,   680,    -1,    -1,    -1,    -1,   685,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   693,    -1,    -1,    -1,    -1,
     488,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,  2079,    -1,  2067,    -1,    -1,    -1,    -1,  1350,
      -1,    -1,   492,    -1,    -1,    -1,  1654,  1655,  1359,    -1,
      -1,  1659,    -1,  1661,    -1,    -1,    -1,  1665,    -1,  1667,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1690,  1691,    -1,   535,    -1,    -1,    -1,   135,
    1302,   137,    -1,    -1,    -1,    -1,   107,   547,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   786,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,  1222,
    1332,   571,   168,   169,   802,   803,    -1,   577,    -1,    -1,
      -1,  1234,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     818,    -1,   905,  1751,   822,    -1,    -1,    -1,    -1,   160,
      -1,    -1,   830,   831,    -1,    -1,    -1,   835,    -1,   837,
      -1,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1779,   850,    -1,   852,    -1,  1487,    -1,   856,   857,
     858,   859,    -1,    -1,    -1,   635,   107,   637,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   876,   962,
      -1,    -1,    -1,    -1,    -1,  1813,  1814,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,
    1828,    -1,    -1,  1831,    -1,    -1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1843,   157,   158,  1402,    -1,
     161,  1405,  1406,    -1,    -1,    -1,    -1,   168,   169,    -1,
      -1,    -1,    -1,  1861,    -1,    -1,    -1,    -1,   936,    -1,
     181,    -1,   940,    -1,    -1,  1576,    -1,   945,    -1,    -1,
      -1,    -1,  1583,   951,    -1,    -1,    -1,  1588,  1589,  1590,
      -1,    -1,   960,    -1,    -1,   963,  1894,    -1,    -1,    -1,
      -1,    -1,   970,    -1,    -1,   763,   764,   765,   766,   767,
     768,   769,   770,   771,   772,   773,   774,   775,   776,   777,
     778,   779,   780,   781,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1929,    -1,    -1,    -1,    -1,  1538,    -1,    -1,    -1,
    1938,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,  1022,  1023,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,  1977,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     848,   157,   158,    -1,    -1,   835,    -1,   837,   164,    -1,
      -1,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,  2007,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,  1511,  1512,
    2018,   172,    -1,    -1,    -1,    -1,  1094,    -1,    -1,   158,
    1098,    -1,   161,    -1,    -1,  1103,  1104,    89,    -1,  1107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1117,
    2048,  2049,    -1,  2051,  2052,    -1,  1124,    -1,    -1,    -1,
      -1,    -1,   902,    -1,    -1,    -1,    -1,    -1,    -1,  1770,
      -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,  2079,    -1,    -1,    -1,    -1,  1787,    -1,  1789,    -1,
      -1,  1159,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,
      -1,    -1,    -1,    -1,  1172,    -1,    -1,    -1,  1176,  2107,
    2108,    -1,  1180,    -1,   101,    -1,    -1,    -1,   105,    -1,
      -1,    -1,    -1,    -1,  1277,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1201,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1210,    -1,    -1,    -1,    -1,  2145,    -1,    -1,
      -1,  1219,   139,  1306,    -1,    -1,   143,    -1,    -1,    -1,
    1018,    -1,   149,    -1,    -1,   152,  1024,    -1,    -1,   156,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1035,    -1,    -1,
     167,   168,   107,   170,    -1,  1253,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,    -1,   123,    -1,
     125,  1694,    -1,    -1,    -1,    -1,   193,    73,  1701,    -1,
    1278,    -1,    -1,    -1,    -1,    -1,    -1,  1075,    -1,   206,
     207,    -1,    -1,    -1,   211,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,  1302,    -1,   161,    -1,    -1,  1732,
      -1,   107,    -1,  1311,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     247,    -1,  1330,    -1,  1332,    -1,    -1,  1107,    -1,   135,
      -1,   137,   324,  1113,  1114,    -1,    -1,   264,    -1,   266,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   276,
    1443,   157,   158,    -1,   281,    -1,    -1,    -1,    -1,    -1,
     287,    -1,   168,   169,    -1,    -1,    -1,    -1,    -1,  1462,
      -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,   305,    -1,
      -1,    -1,    -1,   310,    -1,   312,    -1,    -1,   315,   316,
      -1,    -1,    -1,   320,  1402,    -1,    -1,  1405,  1406,   326,
    1408,    -1,    -1,    -1,    -1,    -1,    -1,  1840,  1841,   336,
      -1,    -1,    -1,    -1,  1847,  1423,    -1,    -1,    -1,  1427,
      -1,  1219,    -1,  1431,    -1,    -1,    -1,  1860,  1521,    -1,
      -1,   358,    -1,   360,   361,  1977,    -1,  1870,    -1,  1872,
      -1,    -1,  1450,  1451,  1452,  1453,  1454,   374,    -1,    -1,
      -1,   378,  1885,    -1,  1887,  1888,  1889,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1475,    -1,    -1,
      -1,  1479,  1480,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,  1493,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   421,   487,    -1,   489,    -1,    -1,
      -1,    -1,    -1,    -1,  1302,    -1,   498,  2049,    -1,  1289,
      -1,    -1,  1945,    -1,    -1,    -1,  1949,    -1,    -1,    -1,
      -1,  1954,    -1,    -1,    -1,    -1,    -1,    -1,   455,    -1,
    1538,    -1,    -1,    -1,   168,  1543,    -1,    -1,    -1,    -1,
      -1,    -1,  1340,  1341,  1342,    -1,    -1,    -1,    -1,  1347,
    1348,    -1,    -1,    -1,    -1,  1563,    -1,   484,    -1,    -1,
      -1,   488,    13,    14,    15,    16,    17,   494,    -1,    -1,
      -1,    -1,    -1,  1371,    -1,    -1,  2009,    -1,   505,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2021,  1597,
      -1,    -1,  2025,    -1,    -1,    -1,    -1,  1690,  1606,    -1,
     527,    -1,  1610,    -1,    -1,    -1,    -1,  2040,    -1,    -1,
      -1,    -1,  1392,    -1,  1412,  1413,    -1,    -1,    -1,    -1,
      -1,   105,    73,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   562,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   573,   574,    -1,  2082,
      -1,   578,    -1,   580,    -1,    -1,   107,    -1,    -1,  1667,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   596,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,  1461,    -1,  1691,   135,    -1,   137,  2120,   615,    -1,
      -1,    -1,  2125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   628,    -1,    -1,    -1,    -1,   157,   158,    -1,    -1,
      -1,   638,    -1,   640,    -1,    -1,    -1,   168,   169,    -1,
    2153,    -1,    -1,  2156,    -1,  2158,    -1,    -1,   655,   656,
     657,    -1,   659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     667,    -1,    -1,  1751,    -1,    -1,  2179,    -1,    -1,    -1,
    1843,    -1,    -1,   680,    -1,    -1,   683,    -1,    -1,    -1,
     687,  1559,    -1,   690,    -1,    -1,   693,    -1,    -1,     1,
      -1,    -1,    -1,    73,    -1,   702,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   710,    -1,    -1,   713,    -1,   715,   716,
      -1,   718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     727,    -1,    -1,   730,   731,   732,  1814,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,   107,   137,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,  1861,    -1,    -1,   848,   157,   158,   786,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
      -1,    -1,    -1,   105,  1654,   802,    -1,    -1,    -1,  1659,
      -1,    -1,    -1,   810,    -1,  1665,    -1,  1667,    -1,    -1,
     161,   818,    -1,    -1,    -1,   822,    -1,    -1,    13,    14,
      15,    16,    17,   830,   831,    -1,    -1,   139,   835,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
      -1,    -1,    -1,   850,    -1,   852,    -1,    -1,    -1,   856,
     857,   858,   859,    -1,    -1,    -1,   168,    -1,  1736,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   876,
      -1,   943,   944,    -1,    -1,    -1,    -1,    -1,    73,  2052,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1977,
      -1,    -1,    -1,    -1,    -1,   207,    -1,   107,   905,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   135,    -1,   137,    -1,   936,
    2018,    -1,    -1,   940,    -1,   247,    -1,    -1,   945,    -1,
     135,    -1,   137,    -1,   951,   952,    -1,   157,   158,    -1,
      -1,    -1,    -1,   960,    -1,   962,   963,    -1,   168,   169,
    2048,  2049,   157,   158,   276,    -1,   161,  1039,  1828,    -1,
      -1,  1831,    -1,   168,   169,   287,    -1,    -1,   290,  2067,
      -1,    -1,    -1,   990,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2079,    -1,   305,    -1,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,   316,    -1,    -1,    -1,    -1,  1081,
      -1,    -1,    -1,    -1,    -1,  1022,  1023,    -1,    -1,   107,
    2108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,   107,  1894,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   358,    -1,    -1,   361,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1130,    -1,
    1132,   135,   374,  1941,    -1,    -1,   378,    -1,    -1,   157,
      -1,    -1,  1144,    -1,  1146,    -1,    -1,    -1,  1938,  1151,
    1152,    -1,    -1,   157,   158,    -1,    -1,  1094,    -1,  1161,
     164,  1098,    -1,    -1,   168,   169,  1103,  1104,    -1,    -1,
    1107,    -1,    -1,    -1,    -1,  1983,    -1,    -1,    -1,    -1,
    1117,    -1,    -1,    -1,    -1,  1187,    -1,  1124,  1190,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,   135,
      -1,   107,  1159,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,  1172,    -1,    -1,    -1,  1176,
      -1,   157,   158,  1180,    -1,   161,   488,    -1,    -1,    -1,
      -1,  1253,   168,   169,    -1,    -1,   160,    -1,    -1,    -1,
      -1,  2051,    -1,    -1,  1201,  2073,    -1,  2075,    -1,    -1,
      -1,    -1,    -1,  1210,   160,    -1,    -1,    -1,    -1,    -1,
      -1,  1283,  1219,    -1,    -1,    -1,    -1,    -1,  1290,    -1,
    1292,  1293,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1301,
      -1,  1303,    -1,  1305,    -1,    -1,  2114,    -1,  1245,    -1,
    1312,    -1,    -1,    -1,    -1,    -1,  1253,  2107,  2108,    -1,
     562,   107,    -1,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   305,  2143,    -1,   123,   580,   125,
    1277,  1278,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    56,    57,    -1,   596,  2145,    -1,    -1,  1295,    -1,
      -1,    -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,  1306,
      -1,    -1,   158,    -1,    -1,    -1,    -1,  2185,    -1,    -1,
      -1,    -1,    -1,  1385,  1386,    -1,    -1,    -1,    93,    -1,
      -1,     1,    -1,  1330,    -1,  1332,   638,    -1,   640,    -1,
    1337,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1411,
      -1,    -1,    -1,    -1,   656,   657,  1418,    -1,  1420,    -1,
      -1,    -1,    -1,    -1,   107,   667,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,   680,   144,
      -1,    -1,   147,    -1,  1446,    -1,    -1,    -1,    -1,    59,
      -1,   693,    -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1402,    -1,    -1,  1405,  1406,
      -1,  1408,    -1,    -1,   157,    -1,    -1,   182,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1423,    -1,    -1,   194,
    1427,    -1,    -1,    -1,  1431,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1443,    -1,    -1,    -1,
      -1,    -1,    -1,  1450,  1451,  1452,  1453,  1454,  1455,  1456,
      -1,   226,    -1,    -1,    -1,  1462,    -1,    -1,    -1,   139,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1475,   149,
    1542,    -1,  1479,  1480,   786,    -1,    -1,  1549,    -1,  1551,
      -1,    -1,    -1,  1490,    -1,    -1,  1493,    -1,   168,    -1,
     802,    -1,    -1,   268,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   277,   278,    -1,    -1,   818,    -1,    -1,    -1,
     822,    -1,    -1,   288,  1521,    -1,    -1,    -1,   830,   831,
      -1,   562,    -1,   835,    -1,    -1,    -1,   207,   303,    -1,
      -1,  1538,    -1,    -1,    -1,    -1,  1543,    -1,   850,    -1,
     852,    -1,    -1,    -1,   856,   857,   858,   859,    -1,  1556,
      -1,    -1,    -1,    -1,    -1,  1627,  1563,    -1,    -1,    48,
      -1,    -1,    -1,   338,   876,    -1,    -1,   247,    -1,    -1,
     102,   346,   347,    -1,    -1,   107,   351,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    76,    -1,    -1,
    1597,    -1,    -1,    -1,    -1,    -1,   276,   638,    -1,  1606,
      -1,    -1,    -1,  1610,    -1,    -1,    -1,   287,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   390,    -1,    -1,   393,    -1,
      -1,   396,    -1,    -1,   936,   305,   667,    -1,   940,    -1,
      -1,    -1,   312,   945,   123,   107,   316,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   136,   960,   138,
      -1,   963,    -1,    -1,  1661,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,   135,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,   358,    -1,
     169,   361,   171,  1690,  1691,   157,   158,  1759,  1760,    -1,
     135,    -1,    -1,    -1,   374,    -1,   168,   169,   378,    -1,
      -1,  1773,    -1,    -1,    -1,    -1,    -1,   196,    -1,    -1,
      -1,  1023,   157,   158,    -1,    -1,   491,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,    -1,    -1,   503,   504,
      -1,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,    -1,    -1,  1751,    -1,   154,    -1,   237,    -1,
      -1,    -1,   241,    -1,    -1,   244,   245,    -1,    -1,   248,
      -1,   802,   251,   252,    -1,   254,    -1,   256,    -1,    -1,
      -1,    -1,  1779,   181,    -1,    -1,    -1,   818,    -1,    -1,
      -1,   822,  1094,    -1,    -1,    -1,  1098,    -1,    -1,    -1,
      -1,  1103,  1104,    -1,   835,  1107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1117,    -1,    -1,   488,    -1,
      -1,   107,  1124,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1843,    -1,   327,    -1,
      -1,   330,    -1,    -1,    -1,    -1,    -1,  1159,    -1,    -1,
      -1,  1858,    -1,    -1,  1861,    -1,    -1,    -1,    -1,    -1,
    1172,    -1,    -1,    -1,  1176,    -1,   355,   642,  1180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   651,    -1,    -1,    -1,
      -1,   370,   562,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,
     580,    -1,    -1,    -1,    -1,    -1,    -1,  1219,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   596,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   706,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1253,    -1,    -1,    -1,    -1,   135,    -1,   137,  1956,
      -1,    -1,    -1,    -1,  1961,    -1,    -1,    -1,   638,    -1,
     640,    -1,    -1,    -1,    -1,    -1,  1278,    -1,   157,   158,
    1977,    -1,   161,    -1,    -1,    -1,   656,   657,  2050,   168,
     169,    -1,   757,    -1,    -1,    -1,   475,   667,    -1,    -1,
    1302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     680,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2018,    -1,   693,    -1,    -1,    -1,    -1,  1330,    -1,
    1332,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2048,  2049,    -1,    -1,  2052,   535,    -1,    -1,    -1,
      -1,    -1,    -1,  1094,  2126,    -1,    -1,  1098,    -1,    -1,
    2067,    -1,   551,    -1,    -1,    -1,  1107,    -1,    -1,  2141,
      -1,    -1,  2079,    -1,   849,    -1,  1117,    -1,    -1,    -1,
      -1,    -1,    -1,  1124,    -1,   860,    -1,    -1,    -1,    -1,
    1402,    -1,    -1,  1405,  1406,    -1,  1408,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   786,     3,    -1,    -1,
      -1,  1423,    -1,    -1,    -1,  1427,    -1,    -1,    -1,  1431,
      -1,    -1,   802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1172,    -1,    -1,    -1,  1176,    -1,    -1,   818,  1180,
      -1,    -1,   822,    -1,    -1,    -1,   635,    -1,    -1,    -1,
     830,   831,    -1,    -1,    -1,   835,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1475,   939,    -1,    -1,  1479,  1480,    -1,
     850,    -1,   852,    -1,    -1,    -1,   856,   857,   858,   859,
      -1,  1493,    -1,    79,    -1,    -1,   675,   676,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   876,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   695,    -1,   697,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   994,
      -1,    -1,    -1,    -1,    -1,    -1,  1538,    -1,    -1,    -1,
      -1,  1543,    13,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,
     940,    -1,    -1,    -1,    -1,   945,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     960,    -1,    -1,   963,    -1,  1597,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,  1606,    -1,    -1,    -1,  1610,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   208,    -1,    -1,    -1,    -1,   806,   807,    -1,
      -1,    -1,    -1,    -1,   813,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,  1023,    -1,    -1,    -1,    -1,    -1,   838,
      -1,    -1,   841,   842,   135,   844,   137,   846,   847,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   265,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,  1691,
      -1,    -1,  1423,    -1,    -1,    -1,  1427,   168,   169,    -1,
    1431,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     889,    -1,    -1,    -1,   893,    -1,    -1,    -1,   897,    -1,
     306,    -1,    -1,    -1,  1094,    -1,    -1,    -1,  1098,    -1,
      -1,    -1,   318,  1103,  1104,    -1,    -1,  1107,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,  1117,    -1,   335,
      -1,    -1,    -1,    -1,  1124,    -1,    -1,    -1,    -1,    -1,
    1225,  1226,  1227,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   357,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1159,
      -1,    -1,   971,    -1,  1259,    -1,    -1,    -1,    -1,    -1,
      -1,    73,  1172,    -1,    -1,    -1,  1176,    -1,    -1,    -1,
    1180,    -1,  1543,    -1,   168,  1280,    -1,    -1,    -1,    -1,
      -1,  1286,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   422,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,  1219,
      -1,    -1,   206,   207,    -1,    -1,    -1,    -1,    -1,  1861,
      -1,    -1,    -1,   135,    -1,   137,  1597,   453,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1606,    -1,    -1,    -1,  1610,
      -1,    -1,    -1,  1253,    -1,   157,   158,   241,    -1,    -1,
      -1,    -1,    -1,    -1,   248,   481,   168,   169,    -1,    -1,
     486,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1278,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     506,   507,    -1,    -1,    -1,   511,   512,    -1,    -1,   515,
      -1,    -1,  1302,    -1,  1113,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   531,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1330,    -1,  1332,    -1,    -1,    -1,    -1,   553,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1977,   330,    -1,    -1,  1158,
      -1,  1160,    -1,    -1,  1163,    -1,    -1,  1166,    -1,    -1,
      -1,  1170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1465,  1466,    -1,    -1,   358,   359,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2018,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   378,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1402,    -1,    -1,  1405,  1406,    -1,  1408,    -1,
      -1,    -1,    -1,    -1,    -1,   631,  2048,  2049,    -1,    -1,
      -1,    -1,    -1,  1423,    -1,    -1,    -1,  1427,   644,    -1,
      -1,  1431,    -1,    -1,    -1,  2067,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2079,    -1,    -1,
      -1,    -1,   668,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   458,  1475,    -1,    -1,    -1,  1479,
    1480,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   475,   476,  1493,   478,   479,    -1,    -1,    -1,  1308,
      -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,   492,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   733,    -1,    -1,
      -1,   505,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1538,    -1,
      -1,    -1,    -1,  1543,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   536,    -1,    -1,    -1,   540,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1377,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1388,
    1675,    -1,  1391,    -1,  1393,  1394,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   580,  1597,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1606,    -1,    -1,   825,
    1610,   827,    -1,    -1,    -1,    -1,    -1,   833,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,   135,    -1,
     137,    -1,   636,    -1,   870,    -1,    -1,    -1,   874,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,   656,   657,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,   667,    -1,    -1,    -1,   671,    -1,    -1,
      -1,  1691,    -1,  1502,   678,    -1,   680,    -1,    -1,    -1,
      -1,    -1,    -1,   919,    -1,    -1,  2067,    -1,   924,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    -1,    -1,    -1,    18,    -1,    20,
      48,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,    -1,    59,    60,
      61,    62,    63,    64,    65,  1011,  1605,    -1,  1893,    -1,
      -1,    -1,   786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   802,   803,
      -1,    -1,    -1,    -1,  1633,   123,    -1,    -1,    -1,   813,
     814,    -1,   816,   817,    -1,    -1,    -1,    -1,   136,    -1,
     138,    -1,    -1,    -1,    -1,    -1,   830,   831,    -1,    -1,
      -1,   835,    -1,   837,   838,    -1,  1665,    -1,    -1,    -1,
     844,  1861,  1671,    -1,    -1,    85,   850,    -1,   852,    -1,
      -1,   169,   856,   857,   858,   859,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,   159,    -1,
      -1,    -1,   876,    -1,   878,    -1,    -1,    -1,   882,    -1,
      -1,    -1,    -1,    -1,    -1,   889,   890,    -1,    -1,   893,
     894,   182,    -1,   897,   898,    -1,    -1,    -1,    -1,    -1,
     904,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,    -1,   156,    -1,  1747,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   244,   245,   168,    -1,
     248,    -1,    -1,   251,   252,    -1,   254,    -1,   256,    -1,
      -1,   945,   946,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   193,    -1,    -1,    -1,  1977,    -1,    -1,
      -1,  1197,    -1,    -1,    -1,  1794,  1795,   207,    -1,   973,
      -1,   211,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1820,  1821,    -1,    -1,    -1,    -1,    -1,  2018,  1828,
      -1,    -1,    -1,    -1,  1833,    -1,    -1,    -1,    -1,    -1,
    1246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1022,  1023,
       1,    -1,    -1,    -1,    -1,    -1,   266,    -1,  2048,  2049,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   355,    -1,    -1,
      -1,   281,    -1,    -1,    -1,    -1,    -1,  2067,    -1,    -1,
      -1,    -1,   370,    -1,    -1,    -1,    -1,    -1,    -1,  2079,
      -1,    -1,    -1,    -1,  1300,    -1,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    55,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1088,    -1,   326,    -1,  1324,    -1,
      -1,    -1,    -1,    74,    -1,    -1,   336,    -1,    -1,  1103,
    1104,    -1,  1931,  1107,  1108,    -1,    -1,    -1,    -1,    -1,
    1114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   358,    -1,
     360,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,
      -1,    -1,    -1,    -1,   135,  1159,    -1,   475,    -1,  1163,
    1164,    -1,  1166,  1167,    -1,    -1,  1170,  1171,    -1,    -1,
      -1,    -1,    -1,    -1,  2003,    -1,   157,    -1,    -1,   160,
     161,   421,    -1,    -1,    -1,    -1,   167,   168,   169,   170,
     171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   455,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   551,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   488,    -1,
      -1,    -1,    -1,    -1,   494,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   527,    -1,    -1,
      -1,    -1,    -1,  1529,    -1,    -1,    -1,    -1,  1302,    -1,
      -1,    -1,    -1,    -1,  1308,  1309,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1332,    -1,
      -1,    -1,    -1,   573,   574,    -1,    -1,    -1,    -1,    -1,
     580,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   675,   676,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1377,  1378,    -1,    -1,   695,    -1,   697,
      -1,    -1,    -1,    -1,  1388,  1389,    -1,  1391,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1402,    -1,
      -1,  1405,  1406,    -1,  1408,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1648,   114,    -1,    -1,    -1,   657,    -1,   659,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     680,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   702,    -1,    -1,    -1,    -1,   168,    -1,    -1,
     710,    -1,    -1,   713,    -1,   715,   716,    -1,   718,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   727,   806,   807,
     730,   731,   732,    -1,    -1,   813,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1752,  1753,    -1,    -1,
     838,    -1,    -1,   841,   842,    -1,   844,    -1,   846,   847,
      -1,    -1,    -1,    -1,  1538,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   786,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1565,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     810,   889,    -1,    -1,    -1,   893,    -1,    -1,    -1,   897,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   831,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   305,    -1,    -1,    -1,    -1,    -1,
     850,    -1,   852,    -1,    -1,    -1,   856,   857,   858,   859,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   876,    -1,    -1,    -1,
    1876,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1655,    -1,   971,    -1,    -1,    -1,   358,    -1,   360,
     361,    -1,    -1,    -1,    -1,    -1,    -1,  1671,    -1,    -1,
    1906,    -1,    -1,   374,    -1,    -1,    -1,   378,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   945,    -1,  1943,    -1,    -1,
      -1,    -1,   952,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   962,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1972,    -1,    -1,    -1,
    1976,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,  1023,    56,    -1,    -1,   488,    -1,    -1,
      -1,    -1,    -1,   494,    -1,  1113,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1813,
    1814,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1829,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
    1158,    -1,  1160,    -1,    -1,  1163,    -1,    -1,  1166,    -1,
      -1,    -1,  1170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   562,    -1,  1103,  1104,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   580,
      13,    14,    15,    16,    17,    -1,    -1,    20,   160,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,   615,    -1,    -1,    -1,    51,  1159,
      53,    -1,    -1,    -1,    -1,  1929,   168,    -1,    -1,    -1,
      -1,    -1,    -1,  1937,    -1,    -1,    -1,   638,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   656,   657,    -1,   659,    -1,
      -1,    -1,    -1,    -1,    -1,   207,   667,    -1,    -1,    -1,
    1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   680,
      -1,    -1,   683,    -1,    -1,    -1,    -1,    -1,    -1,   690,
    1308,    -1,   693,    -1,    -1,    -1,    -1,    -1,    -1,  2003,
    2004,    -1,    -1,  2007,   137,  1245,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1277,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2051,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1295,    -1,    -1,    -1,  1377,
      -1,    -1,  1302,   305,    -1,    -1,    -1,    -1,    -1,    -1,
    1388,    -1,    -1,  1391,    -1,  1393,  1394,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   786,    -1,    -1,    -1,    -1,
      -1,    -1,  1332,    -1,    -1,    -1,    -1,  1337,    -1,    -1,
      -1,   802,    -1,  2107,  2108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   358,   818,   360,   361,
      -1,   822,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,
     831,    -1,   374,    -1,   835,    -1,   378,    -1,    -1,    -1,
      -1,  2145,    -1,   421,    -1,    -1,    -1,    -1,    -1,   850,
      -1,   852,    -1,    -1,    -1,   856,   857,   858,   859,    -1,
      -1,    -1,  1402,    -1,    -1,  1405,  1406,    -1,  1408,    -1,
      -1,    -1,    -1,    -1,    -1,   876,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1502,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1450,  1451,  1452,    -1,    -1,  1455,  1456,    -1,    -1,    -1,
      -1,    -1,  1462,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   945,    -1,   488,    -1,    -1,    -1,
    1490,    -1,   494,    -1,    -1,    -1,    -1,    -1,    -1,   960,
      -1,   962,   963,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1521,    -1,    -1,    -1,    -1,    -1,  1605,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   573,   574,    -1,  1538,    -1,
      -1,    -1,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1633,    -1,    -1,    -1,    -1,
     562,    -1,  1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   580,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   615,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1094,    -1,    -1,   638,  1098,    -1,    -1,
      -1,    -1,  1103,  1104,    -1,    -1,  1107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   656,   657,  1117,   659,    -1,    -1,
      -1,  1661,    -1,  1124,    -1,   667,    -1,    -1,    -1,  1747,
      -1,    -1,   710,    -1,    -1,   713,    -1,    -1,   680,   305,
     718,   683,    -1,    -1,    -1,    -1,    -1,    -1,   690,   727,
    1690,   693,    -1,    -1,    -1,    -1,    -1,    -1,  1159,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   746,    -1,
      -1,  1172,    -1,    -1,    -1,  1176,  1794,  1795,    -1,  1180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   358,    -1,   360,   361,    -1,    -1,    -1,    -1,
      -1,    -1,  1820,  1821,   782,    -1,    -1,    -1,   374,    -1,
      -1,    -1,   378,    -1,    -1,  1833,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1779,
      -1,    -1,    -1,    -1,   786,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   818,    -1,    -1,    -1,
     822,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,   831,
      -1,    -1,    -1,   835,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1302,    -1,  1843,    -1,  1306,    -1,    -1,   850,    -1,
     852,    -1,    -1,  1931,   856,   857,   858,   859,    -1,    -1,
      -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,   494,  1330,
      -1,  1332,    -1,    -1,   876,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,  2003,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   936,    -1,   562,    -1,    -1,    -1,
      -1,  1402,    -1,   945,  1405,  1406,    73,  1408,    -1,    -1,
      -1,    -1,    -1,    -1,   580,    -1,    -1,    -1,   960,    -1,
     962,   963,  1423,    -1,    -1,    -1,  1427,    -1,    -1,    -1,
    1431,    -1,   193,    -1,    -1,    -1,    -1,  1977,    -1,    -1,
     107,    -1,   109,   110,    -1,    -1,   207,    -1,    -1,   615,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     221,    -1,   223,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,    -1,   638,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     656,   657,    -1,   659,    -1,    -1,    -1,   164,    -1,    -1,
      -1,   667,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2049,
      -1,    -1,  2052,    -1,   680,    -1,    -1,   683,    -1,    -1,
      -1,    -1,    -1,    -1,   690,    -1,    -1,   693,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1538,    -1,    -1,
      -1,    -1,  1543,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1094,    -1,    -1,    -1,  1098,    -1,    -1,    -1,
      -1,  1103,  1104,    -1,   325,  1107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1117,    -1,    -1,    -1,    -1,
      -1,    -1,  1124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1597,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1606,    -1,    -1,    -1,  1610,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1159,    -1,    -1,
     786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1172,    -1,    -1,    -1,  1176,    -1,   802,    -1,  1180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   818,    -1,    -1,    -1,   822,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   830,   831,    -1,  1245,    -1,   835,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   850,    -1,   852,    -1,    -1,    -1,
     856,   857,   858,   859,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     876,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   516,    -1,    -1,    -1,    -1,
    1302,   522,    -1,    -1,  1306,    -1,   527,    -1,    -1,    -1,
     936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   945,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1330,    -1,
    1332,    -1,    -1,    -1,   960,    -1,   962,   963,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,  1023,    -1,    -1,
    1402,    -1,    -1,  1405,  1406,    -1,  1408,    -1,   629,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1423,    -1,    -1,    -1,  1427,    -1,    -1,    -1,  1431,
      -1,    -1,    -1,    -1,    -1,    -1,   657,  1475,  1476,    -1,
      -1,  1479,  1480,    -1,    -1,   109,   110,  1485,    -1,   670,
      -1,  1489,    -1,  1491,    -1,  1493,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1094,    -1,
      -1,   135,  1098,   137,    -1,    -1,    -1,  1103,  1104,    -1,
      -1,  1107,    -1,    -1,    -1,    -1,   707,    -1,    -1,    -1,
      -1,  1117,    -1,    -1,    -1,    -1,   160,   161,  1124,   720,
      -1,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1977,    -1,    -1,    -1,
      -1,    -1,    -1,   744,   745,    -1,    -1,   748,    -1,   750,
      -1,    -1,    -1,  1159,    -1,   756,  1538,   758,   759,    -1,
      -1,  1543,    -1,    -1,    -1,    -1,  1172,    -1,    -1,    -1,
    1176,    -1,    -1,    -1,  1180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   786,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2049,   810,
      -1,  2052,    -1,    -1,    -1,  1597,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1641,  1606,   826,  2067,    -1,  1610,    -1,
     831,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   863,    -1,    -1,   866,    -1,    -1,  1686,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   877,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1706,  1707,
      -1,    -1,    -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,
    1306,    -1,    -1,    -1,   905,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1737,
      -1,    -1,    -1,    -1,  1330,    -1,  1332,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   952,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   962,   963,    -1,    -1,    -1,    -1,    -1,    -1,   970,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1402,    -1,    -1,  1405,
    1406,    -1,  1408,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1423,    -1,    -1,
      -1,  1427,  1023,    -1,    -1,  1431,    -1,  1845,    -1,    -1,
    1031,    -1,    -1,    -1,    -1,  1853,    -1,  1855,    -1,  1040,
    1858,  1859,    -1,  1861,    -1,    -1,    -1,    -1,  1866,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,    -1,     5,    -1,    -1,
      -1,    -1,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,  1082,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,  1538,    -1,    -1,    73,    74,  1543,    -1,    -1,
      -1,    -1,    -1,  1961,    -1,    -1,    -1,    -1,    -1,  1967,
    1968,    -1,    -1,    -1,  1155,    -1,  1157,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,  1987,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1597,    -1,    -1,    -1,  1977,    -1,   135,    -1,   137,
    1606,    -1,    -1,    -1,  1610,    -1,    -1,    -1,  2026,    -1,
    2028,    -1,    -1,    -1,    -1,  2033,  2034,    -1,   156,    -1,
    2038,  2039,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,  1239,  1240,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2049,    -1,    -1,
    2052,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2100,  2101,  2102,  2067,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1306,    -1,    -1,    -1,    18,
      -1,  1312,    -1,  2131,  2132,  2133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1327,    -1,    -1,    -1,
      -1,  1332,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,    55,    -1,    57,  1350,
      -1,    -1,  1353,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    74,    75,  1368,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,    -1,   102,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1442,  1443,    -1,    -1,    -1,    -1,   156,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,
     169,   170,   171,   172,   173,    -1,    -1,  1468,    -1,    -1,
      -1,    -1,    -1,   182,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1487,    -1,    -1,  1490,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1538,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1547,  1548,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1977,    -1,    -1,    -1,  1576,    -1,    -1,    72,  1580,
      74,    75,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,    -1,   102,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    49,
      -1,   135,    52,  2049,    54,    55,  2052,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1661,  2067,   156,   157,    74,  1666,   160,   161,    -1,    -1,
      -1,   165,    -1,   167,   168,   169,   170,   171,   172,   173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   182,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,  1727,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,   151,   152,   153,    18,    -1,    -1,   157,   158,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,  1770,
      -1,    -1,    -1,    -1,    -1,    49,  1777,    -1,    52,  1780,
      54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      74,    75,    -1,    77,    -1,  1806,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,    -1,   102,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,   165,    -1,   167,   168,   169,   170,   171,   172,   173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   182,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    72,
      73,    74,    75,  1984,    77,    -1,    -1,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    -1,    -1,   160,   161,    -1,
      -1,    -1,   165,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   182,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    72,
      73,    74,    75,    -1,    77,    -1,    -1,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,
      -1,    -1,   165,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   182,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,   152,
     153,    -1,    -1,    -1,   157,   158,   159,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   182,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,   152,
     153,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   182,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,   159,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    -1,    -1,
      -1,    70,    -1,    -1,    73,    -1,    -1,    -1,    -1,    78,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    -1,    56,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      70,    -1,     5,    73,    -1,    -1,    -1,    -1,    78,    79,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,     5,    -1,    -1,
      -1,    -1,   135,    -1,   137,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,    49,    -1,    -1,    52,    -1,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    70,    -1,    -1,    73,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   106,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
     160,   161,    -1,    -1,    -1,   165,    -1,    -1,   168,   169,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    70,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    78,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    -1,    -1,
      -1,    70,    -1,    -1,    73,    -1,    -1,    -1,    -1,    78,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
      -1,   160,   161,    -1,     3,    -1,     5,    -1,    -1,   168,
     169,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
      -1,   160,   161,    -1,     3,    -1,     5,    -1,    -1,   168,
     169,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
      -1,   160,   161,    -1,     3,    -1,     5,    -1,    -1,   168,
     169,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,    -1,    -1,   156,    -1,    -1,
      -1,   160,   161,    -1,    13,    14,    15,    16,    17,   168,
     169,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,   138,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,   159,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,
      -1,   168,   169,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,    -1,   159,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,   138,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,   138,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,   138,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    72,
      -1,    74,    75,    -1,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    -1,
      -1,    94,    95,    96,    97,    98,    99,   100,    -1,   102,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,
      -1,    -1,   165,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    49,    -1,    -1,    52,    -1,    54,    55,   182,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    75,    -1,
      77,    -1,    -1,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    -1,    -1,    94,    95,    96,
      97,    98,    99,   100,    -1,   102,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   182,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,    -1,    -1,    -1,    -1,   165,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,    -1,    -1,
      -1,   165,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,   109,   110,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,
      -1,    -1,    91,    -1,    93,    -1,    -1,    -1,    -1,   160,
      -1,    -1,    -1,    -1,   165,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,
     169,   170,   171,   172,   173,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    55,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,   167,
     168,   169,   170,   171,   172,   173,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    55,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    55,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      55,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,
     165,    -1,   167,   168,   169,   170,   171,   172,   173,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    55,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    73,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   109,   110,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   159,   160,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,   165,    -1,    -1,   168,
     169,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      13,    14,    15,    16,    17,    73,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,   109,   110,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
     138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   159,   160,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,   138,    -1,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,   160,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    13,
      14,    15,    16,    17,    18,    73,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,   109,   110,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      13,    14,    15,    16,    17,    18,    73,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   109,   110,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,   109,   110,    -1,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    13,    14,
      15,    16,    17,    18,    73,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     109,   110,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,   109,   110,    -1,    -1,    -1,   168,
     169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    13,
      14,    15,    16,    17,    18,    73,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,   109,   110,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,   109,   110,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    18,    73,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   109,   110,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    13,    14,    15,
      16,    17,    18,    73,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   109,
     110,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,   109,   110,    -1,    -1,    -1,   168,   169,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    13,    14,
      15,    16,    17,    18,    73,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     109,   110,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,   109,   110,    -1,    -1,    -1,   168,
     169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      13,    14,    15,    16,    17,    73,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,   109,   110,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,    -1,    -1,    -1,   109,   110,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,
      13,    14,    15,    16,    17,   168,   169,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      73,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,   109,   110,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,   109,   110,    -1,    -1,   168,   169,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,   160,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    13,    14,    15,    16,    17,    18,    73,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   109,   110,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
     160,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    73,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,   107,    49,   109,   110,
      52,    -1,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,
     109,   110,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,   137,    49,
      -1,    -1,    52,   135,    54,    55,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,
     152,   153,    -1,    -1,    74,   157,   158,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
     160,   161,    -1,    -1,    -1,   165,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,   165,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      49,   127,   128,    52,    -1,    54,    55,    -1,    57,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    49,   127,   128,
      52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    49,   127,   128,    52,    -1,    54,
      55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    49,   127,   128,    52,    -1,    54,    55,    -1,    57,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,
     165,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    49,   127,
     128,    52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,   164,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    49,   127,   128,    52,    -1,
      54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,   165,    -1,   167,   168,   169,   170,
     171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    49,   127,   128,    52,    -1,    54,    55,    -1,
      57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    49,
     127,   128,    52,    -1,    54,    55,    -1,    57,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,   165,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    49,    -1,    -1,    52,   135,
      54,    55,    -1,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,   157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    49,   127,   128,    52,    -1,    54,    55,    -1,
      57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    49,
     127,   128,    52,    -1,    54,    55,    -1,    57,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      49,   127,   128,    52,    -1,    54,    55,    -1,    57,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    49,   127,   128,
      52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    49,   127,   128,    52,    -1,    54,
      55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    49,   127,   128,    52,    -1,    54,    55,    -1,    57,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,   167,   168,   169,   170,   171,   172,   173,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    49,   127,
     128,    52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,
     158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    49,   127,   128,    52,    -1,
      54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,
     171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    49,   127,   128,    52,    -1,    54,    55,    -1,
      57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    49,
     127,   128,    52,    -1,    54,    55,    -1,    57,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,   172,   173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,   172,
     173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    49,    53,    -1,
      52,    56,    54,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    49,   137,   135,    52,    -1,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,    74,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    49,   127,
     128,    52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,
     168,   169,   170,   171,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    49,   127,   128,    52,    -1,
      54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,
     171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,   172,   173,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    49,
      53,    -1,    52,    56,    54,    55,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    49,   137,   135,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    74,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,   169,
     170,   171,   172,   173,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   167,   168,   169,   170,   171,   172,   173
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   184,   410,   411,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    70,    73,    74,
     102,   106,   107,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   122,   135,   137,   157,   158,   160,   161,
     168,   169,   187,   188,   189,   205,   297,   298,   299,   300,
     301,   302,   303,   304,   305,   306,   307,   308,   311,   314,
     316,   317,   318,   319,   320,   321,   322,   323,   324,   325,
     327,   329,   330,   331,   333,   334,   338,   339,   340,   341,
     342,   344,   350,   351,   352,   353,   364,   369,   402,   405,
     415,   421,   423,   429,   433,   438,   439,   440,   441,   442,
     443,   444,   445,   471,   489,   490,   491,   492,     0,   184,
     107,   188,   205,   301,   303,   314,   317,   320,   330,   334,
     339,   121,   157,    59,    62,    63,    65,   157,   157,   367,
     427,   428,   429,   326,   327,   109,   110,   188,   190,   403,
     404,   190,   157,   415,   157,   157,     4,   107,   109,   110,
     318,   323,   324,   157,   157,   205,   428,   433,   439,   440,
     441,   443,   444,   445,   109,   341,   162,   184,   161,   304,
     314,   317,   438,   442,   488,   489,   492,   493,   182,   185,
     154,   165,   181,   226,   385,    90,   163,   422,   102,   190,
     426,   163,   163,   163,   182,   109,   110,   157,   205,   309,
     310,   433,   434,   435,   436,   437,   438,   442,   446,   447,
     448,   449,   450,   451,   452,   453,   454,   460,     3,    47,
      48,    50,    56,   332,     3,   161,   205,   303,   304,   318,
     322,   324,   335,   340,   418,   438,   442,   492,    70,   301,
     303,   317,   330,   334,   339,   419,   438,   442,    66,   323,
     323,   318,   324,   312,   323,   324,   332,   351,   318,   323,
     318,   160,   427,   163,   185,   157,   165,   234,   427,   427,
       3,   292,   293,   308,   311,   317,   321,   322,   161,   314,
     317,   490,   190,   190,   415,   181,   317,   157,   205,   424,
     433,   434,   438,   447,   451,   161,   205,   304,   492,   416,
     417,    58,    66,    67,    68,    69,   161,   179,   190,   391,
     393,   397,   399,   400,   340,    58,   159,   161,   205,   313,
     317,   321,   329,   330,   336,   337,   338,   339,   343,   350,
     351,   369,   379,   381,   471,   484,   485,   486,   487,   492,
     493,   427,   109,   110,   172,   188,   340,   368,   460,   429,
     157,   398,   399,   157,    13,    89,   157,   190,   430,   431,
     432,   121,   191,   192,    49,    52,    54,    55,    57,    74,
     104,   105,   107,   108,   119,   120,   123,   124,   125,   127,
     128,   157,   161,   167,   170,   171,   172,   173,   186,   187,
     191,   193,   196,   204,   205,   206,   207,   210,   211,   212,
     213,   214,   215,   216,   217,   218,   219,   220,   221,   222,
     228,   340,   159,   161,   204,   205,   221,   223,   314,   340,
     383,   384,   401,   488,   493,   430,   317,   439,   440,   441,
     443,   444,   445,   159,   159,   159,   159,   159,   159,   159,
     109,   161,   188,   314,   471,   490,   161,   168,   205,   223,
     303,   304,   313,   315,   317,   330,   337,   339,   376,   377,
     378,   380,   381,   484,   492,   162,   157,   161,   438,   442,
     492,   157,   163,   107,   160,   161,   165,   187,   189,   223,
     386,   387,   388,   389,   390,    22,   386,   157,   190,   234,
     157,   157,   188,   424,   188,   428,   433,   435,   436,   437,
     446,   448,   449,   450,   452,   453,   454,   317,   434,   447,
     451,   163,   426,   161,   427,   468,   471,   426,   427,   427,
     422,   292,   157,   427,   468,   426,   427,   427,   422,   427,
     427,   317,   424,   157,   157,   316,   317,   314,   317,   162,
     184,   314,   488,   493,   426,   342,   165,   422,   292,   190,
     190,   385,   303,   322,   420,   438,   442,   165,   422,   292,
     403,   317,   330,   317,   317,   109,   341,   109,   110,   188,
     340,   345,   403,   138,   188,   317,   373,   374,   378,   379,
     382,   156,   184,   234,   308,   182,   438,   451,   317,   184,
     426,   157,   426,   185,   223,   428,   433,   317,   157,   190,
     413,   165,   157,   190,   165,   190,   138,   168,   169,   396,
     159,   163,   190,   400,   159,   427,   162,   184,   315,   317,
     330,   337,   339,   483,   484,   492,   493,   157,   161,   169,
     181,   205,   471,   473,   474,   475,   476,   477,   478,   495,
     205,   343,   492,   317,   337,   323,   318,   427,   159,   315,
     317,   485,   315,   471,   485,   188,   368,   460,   365,   165,
     368,   391,   181,   391,   430,   159,   163,   157,   159,   121,
     157,   204,   157,   157,   204,   157,   157,   207,   157,   204,
     157,   107,   109,   110,   318,   323,   324,   157,   204,   204,
      19,    21,    86,   161,   170,   171,   208,   209,   223,   230,
     234,   353,   383,   492,   163,   184,   157,   193,   161,   166,
     161,   166,   124,   126,   127,   128,   157,   160,   161,   165,
     166,   207,   207,   174,   168,   175,   176,   170,   171,   129,
     130,   131,   132,   177,   178,   133,   134,   169,   167,   179,
     135,   136,   180,   159,   163,   160,   184,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   181,   225,
     226,   227,   157,   205,   464,   465,   466,   467,   468,   159,
     163,   159,   159,   159,   159,   159,   159,   159,   157,   427,
     468,   471,   157,   468,   471,   157,   184,   157,   314,   490,
     162,   184,   185,   161,   185,   157,   169,   205,   433,   455,
     456,   457,   458,   459,   460,   461,   462,   463,   138,   492,
     163,   185,   163,   185,   190,   190,   157,   184,   184,   184,
     184,   161,   189,   184,   387,   164,   163,   494,   386,   160,
     161,   164,   390,   401,   157,   191,   184,   181,   433,   435,
     436,   437,   446,   448,   449,   450,   452,   453,   454,   159,
     159,   159,   159,   159,   159,   159,   159,   159,   159,   434,
     447,   451,   427,   181,   162,   184,   385,   234,   422,   373,
     385,   234,   424,   230,   384,   230,   384,   424,   109,   161,
     413,   234,   422,   426,   165,   165,   422,   292,   413,   234,
     422,   347,   348,   346,   165,   159,   163,   159,   163,    71,
     294,   295,   182,   168,   223,   184,   433,   415,   413,   190,
     162,   184,   157,   395,   393,   394,    79,   328,   188,   315,
     471,   485,   317,   321,   492,   373,   474,   475,   476,   162,
     184,    18,   223,   317,   473,   495,   427,   427,   471,   315,
     483,   493,   317,   188,   315,   485,   427,   165,   427,   368,
      10,   167,   368,   370,   371,   165,   159,   384,   159,   159,
     431,   158,   197,   198,   199,   223,   182,   383,   493,   193,
     383,   161,   383,   384,   383,   493,   223,   383,   159,   383,
     383,   383,   162,   184,   159,   170,   171,   209,    18,   319,
     159,   163,   159,   168,   169,   159,   158,   223,   229,   223,
     165,   223,   188,   223,   188,   119,   161,   188,   197,   119,
     161,   190,   353,   223,   197,   188,   207,   210,   210,   210,
     211,   211,   212,   212,   213,   213,   213,   213,   214,   214,
     215,   216,   217,   218,   219,   164,   230,   182,   191,   161,
     188,   223,   165,   223,   373,   465,   466,   467,   317,   464,
     427,   427,   223,   384,   157,   427,   468,   471,   157,   468,
     471,   373,   373,   184,   184,   162,   162,   157,   433,   456,
     457,   458,   461,    18,   317,   455,   459,   157,   427,   477,
     495,   427,   427,   495,   157,   427,   477,   427,   427,   185,
     222,   190,   377,   380,   162,   380,   381,   162,   495,   495,
     138,   375,   376,   377,   375,   377,   375,   190,   184,   221,
     222,   223,   425,   494,   386,   388,   156,   184,   159,   184,
     159,   375,   223,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   157,   427,   468,   471,   157,   427,   468,   471,
     157,   427,   468,   471,   424,    22,   471,   223,   324,   340,
     469,   234,   159,   159,   159,   159,   159,   411,   412,   234,
     156,   184,   413,   234,   422,   412,   234,   165,   165,   165,
     354,   138,   378,   379,   188,   190,   296,    18,    72,    74,
      75,    77,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    94,    95,    96,    97,    98,    99,
     100,   102,   109,   110,   122,   157,   161,   190,   230,   231,
     232,   233,   234,   235,   236,   238,   239,   248,   255,   256,
     257,   258,   259,   260,   265,   266,   269,   270,   271,   272,
     273,   274,   275,   281,   282,   283,   297,   317,   321,   423,
      71,   185,   185,   375,   414,   412,   159,   301,   303,   314,
     406,   407,   408,   409,   401,   181,   392,   392,   315,   485,
     161,   168,   205,   223,   340,   223,   317,   159,   159,   159,
     159,     5,   317,   427,   473,   366,   370,   368,   165,   340,
     163,   494,   190,   370,   165,   159,   159,   163,   159,   159,
     163,   159,   184,   163,   159,   159,   159,   163,   159,   207,
     159,   159,   159,   207,    18,   319,   223,   159,   159,   158,
     165,   207,   162,   163,   185,   197,   162,   162,   119,   123,
     125,   189,   200,   201,   202,   159,   200,   162,   163,   156,
     221,   164,   159,   200,   185,   387,   159,   159,   159,   159,
     464,   373,   373,   159,   159,   375,   375,   461,   159,   159,
     159,   159,   157,   433,   460,   455,   459,   373,   373,   162,
     185,   495,   163,   185,   159,   163,   163,   185,   163,   185,
     385,   200,   138,   173,   185,   185,   156,   386,   223,   427,
     375,   427,   185,   157,   427,   468,   471,   157,   427,   468,
     471,   157,   427,   468,   471,   373,   373,   373,   426,   151,
     173,   185,   470,   163,   185,   414,   406,   412,   234,   414,
     354,   354,   354,     3,     5,    10,    74,   156,   298,   305,
     306,   314,   317,   355,   360,   488,   163,   182,   157,    62,
      63,   182,   234,   297,   423,   157,   157,    18,   232,   157,
     157,   182,   190,   182,   190,   168,   190,   165,   231,   157,
     157,   157,   232,   157,   234,   223,   224,   224,    14,   284,
     260,   271,   164,   182,   185,   236,    79,   182,   190,    92,
      93,   264,   268,   113,   136,   263,   112,   135,   267,   263,
     382,   317,   296,   162,   162,   185,   414,   190,   424,   185,
     182,   185,   182,   185,   159,   384,   398,   398,   184,   185,
     185,   185,   223,   157,   427,   477,   471,   316,     5,   168,
     185,   223,   368,   494,   165,   370,    10,   371,   156,   181,
     372,   494,   156,   184,   199,   313,   188,    79,   194,   195,
     383,   207,   207,   207,   207,   207,   165,   387,   158,   223,
     163,   156,   203,   161,   201,   203,   203,   162,   163,   126,
     160,   162,   229,   221,   182,   162,   494,   157,   427,   468,
     471,   159,   159,   185,   185,   159,   157,   427,   468,   471,
     157,   427,   477,   433,   427,   427,   159,   159,   162,   380,
     162,   138,   377,   138,   159,   159,   185,   222,   222,   162,
     162,   185,   185,   159,   373,   373,   373,   159,   159,   159,
     385,   163,   223,   223,   324,   340,   162,   156,   185,   414,
     156,   156,   156,   156,   314,   314,   353,   361,   488,   314,
     360,   157,   349,   182,   182,   157,   164,   205,   356,   357,
     363,   433,   434,   447,   451,   163,   182,   190,   190,   197,
     182,   234,   182,   234,   230,   240,   297,   299,   302,   308,
     317,   321,   230,    81,   159,   240,   150,   151,   152,   153,
     158,   159,   182,   230,   249,   250,   252,   297,   182,   182,
     230,   182,   387,   182,   230,   401,   230,   249,   114,   115,
     116,   117,   118,   276,   278,   279,   182,   101,   182,    85,
     157,   159,   427,   156,   182,   182,   157,   157,   232,   232,
     260,   157,   270,   260,   270,   234,   182,   159,   156,   396,
     156,   184,   163,   163,   162,   162,   162,   185,   373,   223,
     223,   185,   162,   185,   165,   156,   370,   494,   340,   190,
     165,   222,   156,   406,   472,   473,   159,   164,   159,   163,
     164,   387,   494,   229,   124,   200,   201,   161,   201,   161,
     201,   162,   156,   373,   159,   159,   373,   373,   162,   185,
     159,   427,   159,   159,   159,   230,   470,   156,   156,   349,
     349,   349,   356,   157,   205,   358,   359,   468,   479,   480,
     481,   482,   182,   163,   182,   356,   182,   401,   428,   433,
     223,   317,   156,   163,   182,   362,   363,   362,   362,   190,
     159,   159,   230,   317,   159,   157,   232,   159,   150,   151,
     152,   153,   173,   182,   253,   254,   232,   231,   182,   254,
     159,   164,   230,   158,   230,   231,   252,   182,   494,   159,
     159,   159,   159,   234,   278,   279,   157,   223,   157,   191,
       1,   232,   207,   261,   230,    76,   111,   262,   264,    76,
     427,   392,   407,   184,   184,   162,   159,   185,   185,   162,
     162,   370,   494,   156,   372,   387,   185,   159,   223,   195,
     223,   494,   156,   162,   162,   200,   200,   159,   427,   427,
     159,   159,   162,   162,   223,   182,   480,   481,   482,   317,
     479,   163,   182,   427,   427,   182,   159,   433,   427,   232,
     232,    78,    79,   165,   243,   244,   245,   159,   230,    76,
     232,   230,   158,   230,    76,   182,    58,   158,   230,   231,
     251,   252,   340,   158,   230,   232,   250,   254,   254,   182,
     230,   156,   165,   245,   232,   232,   157,   184,   182,   191,
     159,   164,   159,   163,   164,   159,   232,   157,   232,   232,
     232,   398,   190,   424,   162,   162,   494,   156,   494,   156,
     156,   162,   162,   159,   159,   159,   479,   427,   357,    76,
       1,   222,   241,   242,   425,     1,   164,     1,   184,   232,
     243,    76,   182,   159,   232,    76,   182,   173,   173,   232,
     231,   188,   340,   254,   254,   182,   230,   251,   173,   173,
      76,   158,   230,   158,   230,   231,   182,     1,   184,   184,
     280,   315,   317,   488,   164,   182,   161,   191,   285,   286,
     287,   207,   197,   230,   263,   156,   156,   157,   427,   468,
     471,   359,   232,   138,     1,   163,   164,   156,   290,   291,
     297,   232,    76,   182,   232,   230,   158,   158,   230,   158,
     230,   158,   230,   231,   158,   230,   158,   230,   232,   173,
     173,   173,   173,   156,   290,   280,   185,   157,   205,   424,
     479,   188,   164,   107,   157,   159,   164,   163,   159,   159,
      76,   259,   373,   222,   241,   244,   246,   247,   297,   232,
     173,   173,   173,   173,   158,   158,   230,   158,   230,   158,
     230,   246,   185,   182,   277,   317,   285,   162,   222,   182,
     285,   287,   232,    76,   159,   232,   237,   185,   244,   158,
     158,   230,   158,   230,   158,   230,   185,   277,   221,   159,
     164,   191,   159,   159,   164,   232,     1,   232,   156,   237,
     156,   159,   234,   191,   288,   157,   182,   288,   234,   163,
     164,   222,   159,   191,   188,   289,   159,   182,   159,   163,
     182,   188
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   183,   184,   185,   186,   186,   186,   186,   186,   187,
     187,   187,   187,   187,   187,   187,   187,   188,   188,   189,
     189,   190,   190,   190,   191,   192,   192,   193,   193,   193,
     193,   193,   193,   193,   193,   193,   193,   193,   193,   193,
     193,   193,   194,   194,   195,   195,   196,   196,   196,   196,
     196,   196,   196,   196,   196,   196,   196,   196,   196,   196,
     196,   196,   196,   196,   196,   196,   196,   196,   196,   196,
     197,   197,   198,   198,   199,   199,   200,   200,   201,   201,
     201,   201,   201,   201,   201,   202,   202,   202,   203,   203,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   205,   205,
     205,   206,   206,   206,   206,   207,   207,   207,   207,   207,
     207,   207,   207,   207,   208,   208,   208,   208,   209,   209,
     210,   210,   211,   211,   211,   211,   212,   212,   212,   213,
     213,   213,   214,   214,   214,   214,   214,   215,   215,   215,
     216,   216,   217,   217,   218,   218,   219,   219,   220,   220,
     221,   221,   221,   222,   223,   223,   223,   224,   224,   225,
     225,   226,   226,   227,   227,   227,   227,   227,   227,   227,
     227,   227,   227,   227,   228,   228,   229,   229,   229,   229,
     230,   230,   231,   231,   232,   232,   232,   232,   232,   232,
     232,   232,   232,   232,   232,   232,   232,   232,   232,   232,
     233,   233,   234,   234,   235,   235,   236,   236,   236,   236,
     236,   237,   237,   237,   238,   239,   239,   239,   239,   239,
     239,   239,   239,   240,   240,   240,   240,   241,   241,   241,
     242,   242,   243,   243,   243,   243,   243,   244,   244,   245,
     246,   246,   247,   247,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   249,   249,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,   250,   250,   250,   250,   250,
     250,   250,   250,   251,   251,   251,   252,   252,   252,   252,
     253,   253,   253,   254,   254,   254,   255,   255,   255,   255,
     255,   255,   255,   255,   255,   255,   255,   255,   255,   255,
     255,   255,   255,   255,   255,   255,   256,   256,   257,   258,
     259,   260,   260,   261,   261,   262,   263,   263,   264,   264,
     265,   265,   265,   265,   265,   265,   266,   267,   267,   268,
     269,   269,   270,   270,   271,   271,   271,   272,   273,   274,
     275,   275,   275,   276,   276,   277,   277,   278,   278,   278,
     278,   279,   280,   280,   280,   280,   280,   281,   282,   282,
     283,   283,   283,   283,   283,   284,   284,   285,   285,   286,
     286,   287,   287,   288,   288,   288,   289,   289,   290,   290,
     291,   291,   292,   292,   293,   293,   294,   294,   295,   295,
     296,   296,   297,   297,   297,   298,   298,   299,   299,   299,
     299,   299,   300,   300,   300,   301,   301,   301,   301,   301,
     302,   302,   302,   302,   302,   303,   303,   303,   303,   304,
     304,   305,   305,   305,   306,   306,   306,   306,   306,   307,
     307,   308,   308,   308,   308,   309,   309,   309,   309,   309,
     310,   310,   311,   311,   311,   311,   312,   312,   312,   313,
     313,   313,   314,   314,   314,   315,   315,   315,   316,   316,
     317,   317,   318,   318,   319,   319,   319,   319,   319,   320,
     321,   321,   321,   322,   322,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   324,   325,   325,   325,   325,   325,
     325,   325,   325,   325,   325,   325,   325,   325,   325,   325,
     325,   325,   325,   325,   325,   325,   325,   325,   325,   325,
     325,   325,   325,   326,   326,   327,   328,   328,   329,   329,
     329,   329,   329,   330,   330,   331,   331,   331,   331,   332,
     332,   332,   332,   332,   332,   333,   333,   333,   333,   334,
     335,   334,   334,   336,   336,   336,   336,   337,   337,   337,
     338,   338,   338,   338,   339,   339,   339,   340,   340,   340,
     340,   340,   340,   341,   341,   341,   342,   342,   343,   343,
     345,   344,   346,   344,   347,   344,   348,   344,   344,   349,
     349,   350,   350,   351,   351,   352,   352,   352,   353,   353,
     353,   353,   353,   353,   353,   353,   354,   354,   355,   355,
     355,   355,   355,   355,   355,   355,   355,   355,   355,   355,
     356,   356,   356,   357,   357,   357,   357,   358,   358,   358,
     359,   360,   360,   361,   361,   362,   362,   363,   364,   364,
     365,   364,   364,   366,   364,   364,   364,   367,   367,   368,
     368,   369,   369,   370,   370,   370,   370,   371,   371,   372,
     372,   372,   373,   373,   373,   373,   374,   374,   374,   374,
     375,   375,   375,   375,   375,   375,   375,   376,   376,   376,
     376,   377,   377,   378,   378,   379,   379,   380,   380,   380,
     380,   380,   381,   381,   381,   381,   381,   382,   382,   383,
     383,   383,   384,   384,   385,   385,   385,   385,   386,   386,
     387,   387,   387,   387,   387,   388,   388,   389,   389,   390,
     390,   390,   390,   390,   391,   391,   392,   392,   394,   393,
     395,   393,   393,   393,   393,   396,   396,   396,   396,   397,
     397,   397,   397,   398,   398,   399,   399,   400,   400,   401,
     401,   401,   401,   402,   402,   402,   403,   403,   404,   404,
     405,   405,   405,   405,   406,   406,   407,   407,   408,   408,
     408,   409,   409,   410,   410,   411,   411,   412,   412,   413,
     414,   415,   415,   415,   415,   415,   415,   415,   415,   415,
     415,   415,   416,   415,   417,   415,   418,   415,   419,   415,
     420,   415,   421,   421,   421,   422,   422,   423,   423,   423,
     423,   423,   423,   423,   423,   423,   423,   424,   424,   424,
     424,   425,   426,   426,   427,   427,   428,   428,   429,   429,
     429,   430,   430,   431,   431,   431,   432,   432,   432,   433,
     433,   434,   434,   434,   434,   435,   435,   435,   435,   436,
     436,   436,   436,   436,   436,   436,   437,   437,   437,   437,
     438,   438,   438,   439,   439,   439,   439,   439,   440,   440,
     440,   440,   441,   441,   441,   441,   441,   441,   442,   442,
     442,   443,   443,   443,   443,   443,   444,   444,   444,   444,
     445,   445,   445,   445,   445,   445,   446,   446,   447,   447,
     447,   447,   448,   448,   448,   448,   449,   449,   449,   449,
     449,   449,   449,   450,   450,   450,   450,   451,   451,   451,
     452,   452,   452,   452,   452,   453,   453,   453,   453,   454,
     454,   454,   454,   454,   454,   455,   455,   455,   455,   455,
     456,   456,   456,   457,   457,   457,   457,   458,   458,   458,
     459,   459,   459,   459,   459,   460,   460,   461,   461,   461,
     462,   462,   463,   463,   464,   464,   464,   465,   465,   465,
     465,   465,   466,   466,   466,   466,   467,   467,   467,   468,
     468,   468,   468,   468,   469,   469,   469,   469,   469,   469,
     470,   470,   471,   471,   471,   471,   472,   472,   473,   473,
     473,   473,   474,   474,   474,   474,   474,   475,   475,   475,
     475,   476,   476,   476,   477,   477,   477,   478,   478,   478,
     478,   478,   478,   479,   479,   479,   480,   480,   480,   480,
     480,   481,   481,   481,   481,   482,   482,   483,   483,   483,
     484,   484,   485,   485,   485,   485,   485,   485,   486,   486,
     486,   486,   486,   486,   486,   486,   486,   486,   487,   487,
     487,   487,   488,   488,   488,   489,   489,   490,   490,   490,
     490,   490,   490,   491,   491,   491,   491,   491,   491,   492,
     492,   492,   493,   493,   493,   494,   494,   495,   495
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
       0,     1,     1,     3,     1,     1,     1,     3,     1,     2,
       4,     3,     5,     3,     5,     2,     2,     2,     0,     2,
       1,     1,     1,     2,     2,     2,     2,     2,     2,     4,
       2,     4,     4,     4,     6,     4,     4,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     5,     5,     4,
       5,     5,     5,     4,     2,     2,     3,     3,     1,     1,
       1,     3,     1,     3,     3,     3,     1,     3,     3,     1,
       3,     3,     1,     3,     3,     3,     3,     1,     3,     3,
       1,     3,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     5,     4,     1,     1,     3,     6,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     7,     1,     1,     3,     3,
       1,     3,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     4,     2,     6,     1,     2,     1,     2,     1,     2,
       1,     1,     2,     2,     2,     5,     7,     5,    10,     7,
       5,    10,     7,     1,     1,     1,     2,     1,     3,     1,
       1,     3,     2,     3,     3,     2,     2,     1,     2,     2,
       0,     1,     2,     3,     4,     6,     5,     7,     6,     7,
       7,     8,     4,     6,     5,     7,     1,     3,     4,     5,
       4,     3,     5,     1,     2,     3,     3,     3,     5,     5,
       5,     5,     3,     5,     5,     5,     3,     4,     5,     5,
       5,     5,     7,     7,     7,     7,     7,     7,     7,     2,
       3,     4,     4,     4,     6,     6,     6,     6,     6,     6,
       6,     3,     4,     1,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     4,     2,     3,
       3,     2,     3,     2,     3,     3,     6,     2,     2,     3,
       3,     3,     3,     3,     3,     5,     1,     1,     5,     5,
       4,     0,     1,     1,     3,     4,     1,     1,     4,     6,
       3,     5,     5,     5,     8,     9,     1,     1,     1,     4,
       3,     3,     1,     3,     1,     3,     5,     1,     2,     5,
       3,     3,     4,     8,     9,     0,     2,     1,     1,     1,
       1,     2,     1,     2,     2,     2,     1,     3,     1,     1,
       6,     8,    10,    12,    14,     0,     1,     0,     1,     1,
       3,     4,     7,     0,     1,     3,     1,     3,     0,     1,
       1,     2,     0,     1,     2,     3,     0,     1,     3,     4,
       1,     3,     2,     2,     1,     7,     5,     1,     1,     1,
       1,     1,     2,     3,     6,     3,     3,     4,     2,     3,
       1,     2,     2,     3,     8,     9,     9,     8,     8,     5,
       7,     2,     2,     3,     3,     3,     4,     3,     4,     4,
       5,     2,     1,     1,     1,     3,     3,     2,     4,     6,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     4,
       1,     2,     3,     1,     2,     1,     1,     1,     1,     1,
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
       4,     4,     4,     3,     2,     2,     3,     3,     2,     1,
       0,     1,     4,     1,     2,     2,     2,     0,     1,     4,
       1,     2,     3,     1,     2,     0,     1,     2,     7,     8,
       0,     9,     8,     0,    11,    10,     1,     2,     3,     0,
       1,     3,     3,     3,     2,     5,     4,     1,     1,     0,
       2,     5,     0,     1,     1,     3,     1,     1,     3,     3,
       0,     1,     1,     1,     3,     3,     3,     1,     3,     3,
       5,     1,     3,     3,     3,     2,     3,     1,     3,     3,
       4,     1,     1,     1,     1,     2,     1,     1,     3,     1,
       1,     2,     1,     1,     0,     2,     2,     4,     1,     4,
       0,     1,     2,     3,     4,     2,     2,     1,     2,     2,
       5,     5,     7,     6,     1,     3,     0,     2,     0,     5,
       0,     5,     3,     1,     8,     0,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     2,     5,     6,     1,
       1,     3,     3,     2,     3,     3,     2,     4,     1,     4,
       7,     5,    10,     8,     1,     4,     2,     2,     1,     1,
       5,     2,     5,     0,     1,     3,     4,     0,     1,     0,
       0,     1,     1,     2,     2,     2,     2,     2,     2,     1,
       2,     5,     0,     6,     0,     8,     0,     7,     0,     7,
       0,     8,     1,     2,     3,     0,     5,     3,     4,     4,
       4,     4,     5,     5,     5,     5,     6,     1,     1,     1,
       1,     3,     0,     5,     0,     1,     1,     2,     6,     4,
       4,     1,     3,     0,     1,     4,     1,     1,     1,     1,
       3,     2,     1,     2,     2,     2,     3,     4,     5,     2,
       4,     5,     4,     5,     3,     4,     6,     7,     3,     4,
       2,     1,     2,     4,     6,     7,     3,     4,     2,     3,
       4,     5,     4,     5,     4,     5,     3,     4,     1,     1,
       1,     4,     6,     7,     3,     4,     2,     3,     3,     4,
       4,     5,     4,     5,     3,     4,     1,     3,     2,     1,
       2,     2,     2,     3,     4,     5,     2,     4,     5,     4,
       5,     3,     4,     6,     7,     3,     4,     2,     1,     2,
       4,     6,     7,     3,     4,     2,     3,     4,     5,     4,
       5,     4,     5,     3,     4,     2,     4,     1,     2,     2,
       2,     3,     4,     2,     4,     4,     3,     4,     6,     3,
       2,     4,     1,     2,     2,     1,     1,     2,     3,     4,
       2,     4,     4,     6,     1,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     3,     6,     3,     2,
       3,     7,     5,     1,     1,     1,     3,     3,     3,     5,
       1,     1,     5,     5,     6,     6,     0,     1,     1,     3,
       2,     2,     1,     2,     2,     3,     4,     1,     4,     4,
       3,     3,     6,     3,     1,     2,     1,     2,     6,     5,
       6,     7,     7,     1,     2,     2,     1,     2,     2,     3,
       4,     1,     4,     4,     3,     6,     3,     1,     1,     2,
       1,     1,     2,     3,     2,     3,     2,     3,     3,     2,
       4,     3,     2,     3,     2,     4,     3,     2,     6,     6,
       6,     7,     1,     2,     1,     1,     1,     2,     3,     2,
       3,     2,     3,     3,     4,     2,     3,     4,     2,     5,
       6,     7,     5,     6,     6,     0,     1,     0,     2
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
#line 643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 8486 "Parser/parser.cc"
    break;

  case 3:
#line 647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8492 "Parser/parser.cc"
    break;

  case 4:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8498 "Parser/parser.cc"
    break;

  case 5:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 6:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8510 "Parser/parser.cc"
    break;

  case 7:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8516 "Parser/parser.cc"
    break;

  case 8:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8522 "Parser/parser.cc"
    break;

  case 20:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8528 "Parser/parser.cc"
    break;

  case 24:
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8534 "Parser/parser.cc"
    break;

  case 25:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8540 "Parser/parser.cc"
    break;

  case 26:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8550 "Parser/parser.cc"
    break;

  case 27:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8556 "Parser/parser.cc"
    break;

  case 28:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8562 "Parser/parser.cc"
    break;

  case 29:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8568 "Parser/parser.cc"
    break;

  case 31:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8574 "Parser/parser.cc"
    break;

  case 32:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8580 "Parser/parser.cc"
    break;

  case 33:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8586 "Parser/parser.cc"
    break;

  case 34:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8592 "Parser/parser.cc"
    break;

  case 35:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8602 "Parser/parser.cc"
    break;

  case 36:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8608 "Parser/parser.cc"
    break;

  case 37:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8614 "Parser/parser.cc"
    break;

  case 38:
#line 736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8620 "Parser/parser.cc"
    break;

  case 39:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8626 "Parser/parser.cc"
    break;

  case 40:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8632 "Parser/parser.cc"
    break;

  case 41:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8638 "Parser/parser.cc"
    break;

  case 43:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8650 "Parser/parser.cc"
    break;

  case 44:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8659 "Parser/parser.cc"
    break;

  case 45:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8665 "Parser/parser.cc"
    break;

  case 47:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8671 "Parser/parser.cc"
    break;

  case 48:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8677 "Parser/parser.cc"
    break;

  case 49:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8683 "Parser/parser.cc"
    break;

  case 50:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8689 "Parser/parser.cc"
    break;

  case 51:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8699 "Parser/parser.cc"
    break;

  case 52:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8705 "Parser/parser.cc"
    break;

  case 53:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8712 "Parser/parser.cc"
    break;

  case 54:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8718 "Parser/parser.cc"
    break;

  case 55:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8724 "Parser/parser.cc"
    break;

  case 56:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8730 "Parser/parser.cc"
    break;

  case 57:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8736 "Parser/parser.cc"
    break;

  case 58:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8742 "Parser/parser.cc"
    break;

  case 59:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8748 "Parser/parser.cc"
    break;

  case 60:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8754 "Parser/parser.cc"
    break;

  case 61:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8760 "Parser/parser.cc"
    break;

  case 62:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8766 "Parser/parser.cc"
    break;

  case 63:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8772 "Parser/parser.cc"
    break;

  case 64:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8778 "Parser/parser.cc"
    break;

  case 65:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8784 "Parser/parser.cc"
    break;

  case 66:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8790 "Parser/parser.cc"
    break;

  case 67:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8796 "Parser/parser.cc"
    break;

  case 68:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8802 "Parser/parser.cc"
    break;

  case 69:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8812 "Parser/parser.cc"
    break;

  case 70:
#line 855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8818 "Parser/parser.cc"
    break;

  case 73:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8824 "Parser/parser.cc"
    break;

  case 74:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8830 "Parser/parser.cc"
    break;

  case 77:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8836 "Parser/parser.cc"
    break;

  case 79:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8842 "Parser/parser.cc"
    break;

  case 80:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8848 "Parser/parser.cc"
    break;

  case 81:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8854 "Parser/parser.cc"
    break;

  case 82:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8860 "Parser/parser.cc"
    break;

  case 83:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8866 "Parser/parser.cc"
    break;

  case 84:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8872 "Parser/parser.cc"
    break;

  case 85:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8878 "Parser/parser.cc"
    break;

  case 86:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8884 "Parser/parser.cc"
    break;

  case 87:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8892 "Parser/parser.cc"
    break;

  case 88:
#line 906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8898 "Parser/parser.cc"
    break;

  case 89:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8907 "Parser/parser.cc"
    break;

  case 92:
#line 920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8913 "Parser/parser.cc"
    break;

  case 93:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8919 "Parser/parser.cc"
    break;

  case 94:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8939 "Parser/parser.cc"
    break;

  case 95:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8945 "Parser/parser.cc"
    break;

  case 96:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8951 "Parser/parser.cc"
    break;

  case 97:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8957 "Parser/parser.cc"
    break;

  case 98:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8963 "Parser/parser.cc"
    break;

  case 99:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8969 "Parser/parser.cc"
    break;

  case 100:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8975 "Parser/parser.cc"
    break;

  case 101:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8981 "Parser/parser.cc"
    break;

  case 102:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8987 "Parser/parser.cc"
    break;

  case 103:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8993 "Parser/parser.cc"
    break;

  case 104:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8999 "Parser/parser.cc"
    break;

  case 105:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9008 "Parser/parser.cc"
    break;

  case 106:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9014 "Parser/parser.cc"
    break;

  case 107:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "countof for expressions is currently unimplemented. "); (yyval.expr) = nullptr; }
#line 9020 "Parser/parser.cc"
    break;

  case 108:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9026 "Parser/parser.cc"
    break;

  case 109:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9032 "Parser/parser.cc"
    break;

  case 110:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9038 "Parser/parser.cc"
    break;

  case 111:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9044 "Parser/parser.cc"
    break;

  case 112:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9050 "Parser/parser.cc"
    break;

  case 113:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9056 "Parser/parser.cc"
    break;

  case 114:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9062 "Parser/parser.cc"
    break;

  case 116:
#line 994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9068 "Parser/parser.cc"
    break;

  case 117:
#line 996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9074 "Parser/parser.cc"
    break;

  case 118:
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9080 "Parser/parser.cc"
    break;

  case 119:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9086 "Parser/parser.cc"
    break;

  case 120:
#line 1002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9092 "Parser/parser.cc"
    break;

  case 121:
#line 1004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9098 "Parser/parser.cc"
    break;

  case 122:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9104 "Parser/parser.cc"
    break;

  case 123:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9110 "Parser/parser.cc"
    break;

  case 131:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9116 "Parser/parser.cc"
    break;

  case 133:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9122 "Parser/parser.cc"
    break;

  case 134:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9128 "Parser/parser.cc"
    break;

  case 135:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9134 "Parser/parser.cc"
    break;

  case 137:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9140 "Parser/parser.cc"
    break;

  case 138:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9146 "Parser/parser.cc"
    break;

  case 140:
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9152 "Parser/parser.cc"
    break;

  case 141:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9158 "Parser/parser.cc"
    break;

  case 143:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9164 "Parser/parser.cc"
    break;

  case 144:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9170 "Parser/parser.cc"
    break;

  case 145:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9176 "Parser/parser.cc"
    break;

  case 146:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9182 "Parser/parser.cc"
    break;

  case 148:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9188 "Parser/parser.cc"
    break;

  case 149:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9194 "Parser/parser.cc"
    break;

  case 151:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9200 "Parser/parser.cc"
    break;

  case 153:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9206 "Parser/parser.cc"
    break;

  case 155:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9212 "Parser/parser.cc"
    break;

  case 157:
#line 1098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9218 "Parser/parser.cc"
    break;

  case 159:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9224 "Parser/parser.cc"
    break;

  case 161:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9230 "Parser/parser.cc"
    break;

  case 162:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9236 "Parser/parser.cc"
    break;

  case 165:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 9248 "Parser/parser.cc"
    break;

  case 166:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9254 "Parser/parser.cc"
    break;

  case 167:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9260 "Parser/parser.cc"
    break;

  case 171:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9266 "Parser/parser.cc"
    break;

  case 172:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9272 "Parser/parser.cc"
    break;

  case 173:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9278 "Parser/parser.cc"
    break;

  case 174:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9284 "Parser/parser.cc"
    break;

  case 175:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9290 "Parser/parser.cc"
    break;

  case 176:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9296 "Parser/parser.cc"
    break;

  case 177:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9302 "Parser/parser.cc"
    break;

  case 178:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9308 "Parser/parser.cc"
    break;

  case 179:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9314 "Parser/parser.cc"
    break;

  case 180:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9320 "Parser/parser.cc"
    break;

  case 181:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9326 "Parser/parser.cc"
    break;

  case 182:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9332 "Parser/parser.cc"
    break;

  case 183:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9338 "Parser/parser.cc"
    break;

  case 184:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9344 "Parser/parser.cc"
    break;

  case 185:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9350 "Parser/parser.cc"
    break;

  case 187:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9356 "Parser/parser.cc"
    break;

  case 188:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9362 "Parser/parser.cc"
    break;

  case 189:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9368 "Parser/parser.cc"
    break;

  case 191:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9374 "Parser/parser.cc"
    break;

  case 192:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9380 "Parser/parser.cc"
    break;

  case 207:
#line 1216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9386 "Parser/parser.cc"
    break;

  case 209:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9392 "Parser/parser.cc"
    break;

  case 210:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9398 "Parser/parser.cc"
    break;

  case 211:
#line 1227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9409 "Parser/parser.cc"
    break;

  case 212:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9415 "Parser/parser.cc"
    break;

  case 213:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9421 "Parser/parser.cc"
    break;

  case 215:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9427 "Parser/parser.cc"
    break;

  case 216:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9433 "Parser/parser.cc"
    break;

  case 217:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9439 "Parser/parser.cc"
    break;

  case 218:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9445 "Parser/parser.cc"
    break;

  case 219:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9451 "Parser/parser.cc"
    break;

  case 222:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9457 "Parser/parser.cc"
    break;

  case 223:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9464 "Parser/parser.cc"
    break;

  case 224:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9470 "Parser/parser.cc"
    break;

  case 225:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9476 "Parser/parser.cc"
    break;

  case 226:
#line 1306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9482 "Parser/parser.cc"
    break;

  case 227:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9488 "Parser/parser.cc"
    break;

  case 228:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9502 "Parser/parser.cc"
    break;

  case 229:
#line 1320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9508 "Parser/parser.cc"
    break;

  case 230:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9514 "Parser/parser.cc"
    break;

  case 231:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9523 "Parser/parser.cc"
    break;

  case 232:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9529 "Parser/parser.cc"
    break;

  case 233:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9535 "Parser/parser.cc"
    break;

  case 234:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9541 "Parser/parser.cc"
    break;

  case 235:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9547 "Parser/parser.cc"
    break;

  case 236:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9553 "Parser/parser.cc"
    break;

  case 237:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9559 "Parser/parser.cc"
    break;

  case 238:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9565 "Parser/parser.cc"
    break;

  case 240:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9571 "Parser/parser.cc"
    break;

  case 241:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9577 "Parser/parser.cc"
    break;

  case 242:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9583 "Parser/parser.cc"
    break;

  case 243:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9589 "Parser/parser.cc"
    break;

  case 244:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9595 "Parser/parser.cc"
    break;

  case 245:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 246:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9607 "Parser/parser.cc"
    break;

  case 248:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9613 "Parser/parser.cc"
    break;

  case 249:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9619 "Parser/parser.cc"
    break;

  case 250:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9625 "Parser/parser.cc"
    break;

  case 252:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 253:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 254:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9643 "Parser/parser.cc"
    break;

  case 255:
#line 1397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9652 "Parser/parser.cc"
    break;

  case 256:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9658 "Parser/parser.cc"
    break;

  case 257:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9664 "Parser/parser.cc"
    break;

  case 258:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9670 "Parser/parser.cc"
    break;

  case 259:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9679 "Parser/parser.cc"
    break;

  case 260:
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 261:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 262:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 263:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9706 "Parser/parser.cc"
    break;

  case 264:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9712 "Parser/parser.cc"
    break;

  case 265:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9718 "Parser/parser.cc"
    break;

  case 267:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyvsp[-2].forctl)->init->set_last( (yyvsp[0].forctl)->init );
			if ( (yyvsp[-2].forctl)->condition ) {
				if ( (yyvsp[0].forctl)->condition ) {
					(yyvsp[-2].forctl)->condition->expr.reset( new ast::LogicalExpr( yylloc, (yyvsp[-2].forctl)->condition->expr.release(), (yyvsp[0].forctl)->condition->expr.release(), ast::AndExpr ) );
				} // if
			} else (yyvsp[-2].forctl)->condition = (yyvsp[0].forctl)->condition;
			if ( (yyvsp[-2].forctl)->change ) {
				if ( (yyvsp[0].forctl)->change ) {
					(yyvsp[-2].forctl)->change->expr.reset( new ast::CommaExpr( yylloc, (yyvsp[-2].forctl)->change->expr.release(), (yyvsp[0].forctl)->change->expr.release() ) );
				} // if
			} else (yyvsp[-2].forctl)->change = (yyvsp[0].forctl)->change;
			(yyval.forctl) = (yyvsp[-2].forctl);
		}
#line 9737 "Parser/parser.cc"
    break;

  case 268:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9743 "Parser/parser.cc"
    break;

  case 269:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9751 "Parser/parser.cc"
    break;

  case 270:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9757 "Parser/parser.cc"
    break;

  case 271:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9763 "Parser/parser.cc"
    break;

  case 272:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9769 "Parser/parser.cc"
    break;

  case 273:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9775 "Parser/parser.cc"
    break;

  case 274:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9781 "Parser/parser.cc"
    break;

  case 275:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9787 "Parser/parser.cc"
    break;

  case 276:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9796 "Parser/parser.cc"
    break;

  case 277:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9805 "Parser/parser.cc"
    break;

  case 278:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9811 "Parser/parser.cc"
    break;

  case 279:
#line 1487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9820 "Parser/parser.cc"
    break;

  case 280:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9829 "Parser/parser.cc"
    break;

  case 281:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9835 "Parser/parser.cc"
    break;

  case 282:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9841 "Parser/parser.cc"
    break;

  case 283:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9847 "Parser/parser.cc"
    break;

  case 284:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9853 "Parser/parser.cc"
    break;

  case 285:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9859 "Parser/parser.cc"
    break;

  case 286:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9865 "Parser/parser.cc"
    break;

  case 287:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9871 "Parser/parser.cc"
    break;

  case 288:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9877 "Parser/parser.cc"
    break;

  case 289:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9886 "Parser/parser.cc"
    break;

  case 290:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9896 "Parser/parser.cc"
    break;

  case 291:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9902 "Parser/parser.cc"
    break;

  case 292:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9908 "Parser/parser.cc"
    break;

  case 293:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9917 "Parser/parser.cc"
    break;

  case 294:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9927 "Parser/parser.cc"
    break;

  case 295:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9933 "Parser/parser.cc"
    break;

  case 296:
#line 1546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9942 "Parser/parser.cc"
    break;

  case 297:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9952 "Parser/parser.cc"
    break;

  case 298:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9958 "Parser/parser.cc"
    break;

  case 299:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9964 "Parser/parser.cc"
    break;

  case 300:
#line 1562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9970 "Parser/parser.cc"
    break;

  case 301:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9976 "Parser/parser.cc"
    break;

  case 302:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9985 "Parser/parser.cc"
    break;

  case 303:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9995 "Parser/parser.cc"
    break;

  case 304:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10001 "Parser/parser.cc"
    break;

  case 305:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10010 "Parser/parser.cc"
    break;

  case 306:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10020 "Parser/parser.cc"
    break;

  case 307:
#line 1592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10026 "Parser/parser.cc"
    break;

  case 308:
#line 1594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10035 "Parser/parser.cc"
    break;

  case 309:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10045 "Parser/parser.cc"
    break;

  case 310:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10051 "Parser/parser.cc"
    break;

  case 311:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->buildType() ) ) );
		}
#line 10059 "Parser/parser.cc"
    break;

  case 312:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->buildType() ) ) );
		}
#line 10071 "Parser/parser.cc"
    break;

  case 313:
#line 1623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10077 "Parser/parser.cc"
    break;

  case 314:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false ); }
#line 10083 "Parser/parser.cc"
    break;

  case 315:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10089 "Parser/parser.cc"
    break;

  case 316:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10095 "Parser/parser.cc"
    break;

  case 317:
#line 1637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10101 "Parser/parser.cc"
    break;

  case 318:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10107 "Parser/parser.cc"
    break;

  case 319:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10113 "Parser/parser.cc"
    break;

  case 320:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10119 "Parser/parser.cc"
    break;

  case 321:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10125 "Parser/parser.cc"
    break;

  case 322:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10131 "Parser/parser.cc"
    break;

  case 324:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10137 "Parser/parser.cc"
    break;

  case 325:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10143 "Parser/parser.cc"
    break;

  case 326:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10149 "Parser/parser.cc"
    break;

  case 327:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10155 "Parser/parser.cc"
    break;

  case 328:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10161 "Parser/parser.cc"
    break;

  case 329:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10167 "Parser/parser.cc"
    break;

  case 330:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10173 "Parser/parser.cc"
    break;

  case 331:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 332:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 333:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 334:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 335:
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10203 "Parser/parser.cc"
    break;

  case 336:
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10209 "Parser/parser.cc"
    break;

  case 337:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10215 "Parser/parser.cc"
    break;

  case 338:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10221 "Parser/parser.cc"
    break;

  case 339:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10227 "Parser/parser.cc"
    break;

  case 340:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10233 "Parser/parser.cc"
    break;

  case 341:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10239 "Parser/parser.cc"
    break;

  case 342:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10245 "Parser/parser.cc"
    break;

  case 343:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10251 "Parser/parser.cc"
    break;

  case 344:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10257 "Parser/parser.cc"
    break;

  case 345:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10263 "Parser/parser.cc"
    break;

  case 348:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10269 "Parser/parser.cc"
    break;

  case 349:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10278 "Parser/parser.cc"
    break;

  case 350:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10284 "Parser/parser.cc"
    break;

  case 351:
#line 1738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10290 "Parser/parser.cc"
    break;

  case 354:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10296 "Parser/parser.cc"
    break;

  case 355:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10302 "Parser/parser.cc"
    break;

  case 358:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10308 "Parser/parser.cc"
    break;

  case 359:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10314 "Parser/parser.cc"
    break;

  case 360:
#line 1766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10320 "Parser/parser.cc"
    break;

  case 361:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10326 "Parser/parser.cc"
    break;

  case 362:
#line 1770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10332 "Parser/parser.cc"
    break;

  case 363:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10338 "Parser/parser.cc"
    break;

  case 364:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10344 "Parser/parser.cc"
    break;

  case 365:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10350 "Parser/parser.cc"
    break;

  case 366:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10356 "Parser/parser.cc"
    break;

  case 369:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10362 "Parser/parser.cc"
    break;

  case 370:
#line 1797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10368 "Parser/parser.cc"
    break;

  case 371:
#line 1799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10374 "Parser/parser.cc"
    break;

  case 372:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10380 "Parser/parser.cc"
    break;

  case 373:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10386 "Parser/parser.cc"
    break;

  case 374:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10392 "Parser/parser.cc"
    break;

  case 375:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10398 "Parser/parser.cc"
    break;

  case 376:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10404 "Parser/parser.cc"
    break;

  case 377:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10410 "Parser/parser.cc"
    break;

  case 378:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10416 "Parser/parser.cc"
    break;

  case 379:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10422 "Parser/parser.cc"
    break;

  case 380:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10428 "Parser/parser.cc"
    break;

  case 381:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10434 "Parser/parser.cc"
    break;

  case 382:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10440 "Parser/parser.cc"
    break;

  case 383:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10446 "Parser/parser.cc"
    break;

  case 384:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10452 "Parser/parser.cc"
    break;

  case 385:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10458 "Parser/parser.cc"
    break;

  case 386:
#line 1852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10464 "Parser/parser.cc"
    break;

  case 387:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10470 "Parser/parser.cc"
    break;

  case 388:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10476 "Parser/parser.cc"
    break;

  case 389:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10482 "Parser/parser.cc"
    break;

  case 390:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10488 "Parser/parser.cc"
    break;

  case 391:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10494 "Parser/parser.cc"
    break;

  case 393:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10500 "Parser/parser.cc"
    break;

  case 394:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10506 "Parser/parser.cc"
    break;

  case 395:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10512 "Parser/parser.cc"
    break;

  case 400:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10518 "Parser/parser.cc"
    break;

  case 401:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10524 "Parser/parser.cc"
    break;

  case 402:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10530 "Parser/parser.cc"
    break;

  case 403:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10536 "Parser/parser.cc"
    break;

  case 404:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10542 "Parser/parser.cc"
    break;

  case 405:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10548 "Parser/parser.cc"
    break;

  case 406:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10554 "Parser/parser.cc"
    break;

  case 407:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10560 "Parser/parser.cc"
    break;

  case 410:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10566 "Parser/parser.cc"
    break;

  case 411:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10572 "Parser/parser.cc"
    break;

  case 412:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10581 "Parser/parser.cc"
    break;

  case 413:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10587 "Parser/parser.cc"
    break;

  case 414:
#line 1933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10593 "Parser/parser.cc"
    break;

  case 415:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10599 "Parser/parser.cc"
    break;

  case 416:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10608 "Parser/parser.cc"
    break;

  case 417:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10617 "Parser/parser.cc"
    break;

  case 418:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10623 "Parser/parser.cc"
    break;

  case 421:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10629 "Parser/parser.cc"
    break;

  case 422:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10635 "Parser/parser.cc"
    break;

  case 424:
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10641 "Parser/parser.cc"
    break;

  case 425:
#line 1975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10647 "Parser/parser.cc"
    break;

  case 435:
#line 2001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10653 "Parser/parser.cc"
    break;

  case 436:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10659 "Parser/parser.cc"
    break;

  case 440:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10665 "Parser/parser.cc"
    break;

  case 442:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10671 "Parser/parser.cc"
    break;

  case 443:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10677 "Parser/parser.cc"
    break;

  case 444:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10683 "Parser/parser.cc"
    break;

  case 445:
#line 2040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10689 "Parser/parser.cc"
    break;

  case 446:
#line 2042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10695 "Parser/parser.cc"
    break;

  case 447:
#line 2044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10701 "Parser/parser.cc"
    break;

  case 448:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10707 "Parser/parser.cc"
    break;

  case 449:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10713 "Parser/parser.cc"
    break;

  case 451:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10719 "Parser/parser.cc"
    break;

  case 452:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10725 "Parser/parser.cc"
    break;

  case 453:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10731 "Parser/parser.cc"
    break;

  case 454:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10742 "Parser/parser.cc"
    break;

  case 455:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10748 "Parser/parser.cc"
    break;

  case 456:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10754 "Parser/parser.cc"
    break;

  case 457:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10760 "Parser/parser.cc"
    break;

  case 458:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10766 "Parser/parser.cc"
    break;

  case 459:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10772 "Parser/parser.cc"
    break;

  case 460:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10778 "Parser/parser.cc"
    break;

  case 461:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10787 "Parser/parser.cc"
    break;

  case 462:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10796 "Parser/parser.cc"
    break;

  case 463:
#line 2116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10805 "Parser/parser.cc"
    break;

  case 464:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10816 "Parser/parser.cc"
    break;

  case 465:
#line 2134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10825 "Parser/parser.cc"
    break;

  case 466:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10831 "Parser/parser.cc"
    break;

  case 467:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10837 "Parser/parser.cc"
    break;

  case 468:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10843 "Parser/parser.cc"
    break;

  case 469:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10851 "Parser/parser.cc"
    break;

  case 470:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10859 "Parser/parser.cc"
    break;

  case 471:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10865 "Parser/parser.cc"
    break;

  case 474:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10880 "Parser/parser.cc"
    break;

  case 475:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10886 "Parser/parser.cc"
    break;

  case 476:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10892 "Parser/parser.cc"
    break;

  case 477:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10898 "Parser/parser.cc"
    break;

  case 478:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10904 "Parser/parser.cc"
    break;

  case 479:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10910 "Parser/parser.cc"
    break;

  case 485:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10920 "Parser/parser.cc"
    break;

  case 498:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10926 "Parser/parser.cc"
    break;

  case 501:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10932 "Parser/parser.cc"
    break;

  case 502:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 504:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10944 "Parser/parser.cc"
    break;

  case 505:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10950 "Parser/parser.cc"
    break;

  case 506:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10956 "Parser/parser.cc"
    break;

  case 507:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10962 "Parser/parser.cc"
    break;

  case 508:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10968 "Parser/parser.cc"
    break;

  case 509:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10974 "Parser/parser.cc"
    break;

  case 511:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10980 "Parser/parser.cc"
    break;

  case 512:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10986 "Parser/parser.cc"
    break;

  case 514:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 515:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10998 "Parser/parser.cc"
    break;

  case 516:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 11004 "Parser/parser.cc"
    break;

  case 517:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 11010 "Parser/parser.cc"
    break;

  case 518:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11016 "Parser/parser.cc"
    break;

  case 519:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11022 "Parser/parser.cc"
    break;

  case 520:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11028 "Parser/parser.cc"
    break;

  case 521:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11034 "Parser/parser.cc"
    break;

  case 522:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11040 "Parser/parser.cc"
    break;

  case 523:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11046 "Parser/parser.cc"
    break;

  case 524:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11052 "Parser/parser.cc"
    break;

  case 525:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11058 "Parser/parser.cc"
    break;

  case 526:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11064 "Parser/parser.cc"
    break;

  case 527:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11070 "Parser/parser.cc"
    break;

  case 528:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11076 "Parser/parser.cc"
    break;

  case 529:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11082 "Parser/parser.cc"
    break;

  case 530:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11088 "Parser/parser.cc"
    break;

  case 531:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11094 "Parser/parser.cc"
    break;

  case 532:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11100 "Parser/parser.cc"
    break;

  case 533:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11106 "Parser/parser.cc"
    break;

  case 534:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11112 "Parser/parser.cc"
    break;

  case 535:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11118 "Parser/parser.cc"
    break;

  case 536:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11124 "Parser/parser.cc"
    break;

  case 537:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11130 "Parser/parser.cc"
    break;

  case 538:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11136 "Parser/parser.cc"
    break;

  case 539:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11142 "Parser/parser.cc"
    break;

  case 540:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11148 "Parser/parser.cc"
    break;

  case 541:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11154 "Parser/parser.cc"
    break;

  case 542:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11160 "Parser/parser.cc"
    break;

  case 543:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11166 "Parser/parser.cc"
    break;

  case 544:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11172 "Parser/parser.cc"
    break;

  case 545:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11178 "Parser/parser.cc"
    break;

  case 546:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11184 "Parser/parser.cc"
    break;

  case 547:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11190 "Parser/parser.cc"
    break;

  case 548:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11196 "Parser/parser.cc"
    break;

  case 549:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11202 "Parser/parser.cc"
    break;

  case 550:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11208 "Parser/parser.cc"
    break;

  case 551:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11214 "Parser/parser.cc"
    break;

  case 553:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11220 "Parser/parser.cc"
    break;

  case 555:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11226 "Parser/parser.cc"
    break;

  case 556:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11232 "Parser/parser.cc"
    break;

  case 557:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11238 "Parser/parser.cc"
    break;

  case 559:
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11244 "Parser/parser.cc"
    break;

  case 560:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11250 "Parser/parser.cc"
    break;

  case 561:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11256 "Parser/parser.cc"
    break;

  case 562:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11262 "Parser/parser.cc"
    break;

  case 564:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11268 "Parser/parser.cc"
    break;

  case 566:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11274 "Parser/parser.cc"
    break;

  case 567:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11280 "Parser/parser.cc"
    break;

  case 568:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11286 "Parser/parser.cc"
    break;

  case 569:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11292 "Parser/parser.cc"
    break;

  case 570:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11298 "Parser/parser.cc"
    break;

  case 571:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11304 "Parser/parser.cc"
    break;

  case 572:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11310 "Parser/parser.cc"
    break;

  case 573:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11316 "Parser/parser.cc"
    break;

  case 574:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11322 "Parser/parser.cc"
    break;

  case 576:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11328 "Parser/parser.cc"
    break;

  case 577:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11334 "Parser/parser.cc"
    break;

  case 578:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 580:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11346 "Parser/parser.cc"
    break;

  case 581:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11352 "Parser/parser.cc"
    break;

  case 582:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11361 "Parser/parser.cc"
    break;

  case 584:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11367 "Parser/parser.cc"
    break;

  case 585:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11373 "Parser/parser.cc"
    break;

  case 586:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11379 "Parser/parser.cc"
    break;

  case 588:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11385 "Parser/parser.cc"
    break;

  case 589:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11391 "Parser/parser.cc"
    break;

  case 591:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11397 "Parser/parser.cc"
    break;

  case 592:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11403 "Parser/parser.cc"
    break;

  case 593:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11409 "Parser/parser.cc"
    break;

  case 594:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11415 "Parser/parser.cc"
    break;

  case 595:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11421 "Parser/parser.cc"
    break;

  case 596:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11427 "Parser/parser.cc"
    break;

  case 597:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11433 "Parser/parser.cc"
    break;

  case 598:
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11439 "Parser/parser.cc"
    break;

  case 599:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11445 "Parser/parser.cc"
    break;

  case 601:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11451 "Parser/parser.cc"
    break;

  case 602:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11457 "Parser/parser.cc"
    break;

  case 603:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11463 "Parser/parser.cc"
    break;

  case 604:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11469 "Parser/parser.cc"
    break;

  case 605:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11475 "Parser/parser.cc"
    break;

  case 610:
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11481 "Parser/parser.cc"
    break;

  case 611:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11487 "Parser/parser.cc"
    break;

  case 612:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11496 "Parser/parser.cc"
    break;

  case 613:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11504 "Parser/parser.cc"
    break;

  case 614:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11513 "Parser/parser.cc"
    break;

  case 615:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11522 "Parser/parser.cc"
    break;

  case 616:
#line 2578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11531 "Parser/parser.cc"
    break;

  case 617:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11540 "Parser/parser.cc"
    break;

  case 619:
#line 2592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11546 "Parser/parser.cc"
    break;

  case 620:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11552 "Parser/parser.cc"
    break;

  case 621:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11562 "Parser/parser.cc"
    break;

  case 622:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11581 "Parser/parser.cc"
    break;

  case 625:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11587 "Parser/parser.cc"
    break;

  case 626:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11593 "Parser/parser.cc"
    break;

  case 627:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11599 "Parser/parser.cc"
    break;

  case 628:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11605 "Parser/parser.cc"
    break;

  case 629:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11611 "Parser/parser.cc"
    break;

  case 630:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11617 "Parser/parser.cc"
    break;

  case 631:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11626 "Parser/parser.cc"
    break;

  case 632:
#line 2648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11632 "Parser/parser.cc"
    break;

  case 633:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11641 "Parser/parser.cc"
    break;

  case 634:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11647 "Parser/parser.cc"
    break;

  case 635:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11656 "Parser/parser.cc"
    break;

  case 636:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11662 "Parser/parser.cc"
    break;

  case 637:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11668 "Parser/parser.cc"
    break;

  case 638:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11681 "Parser/parser.cc"
    break;

  case 639:
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11690 "Parser/parser.cc"
    break;

  case 640:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11696 "Parser/parser.cc"
    break;

  case 641:
#line 2688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11702 "Parser/parser.cc"
    break;

  case 642:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11715 "Parser/parser.cc"
    break;

  case 643:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11721 "Parser/parser.cc"
    break;

  case 646:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11727 "Parser/parser.cc"
    break;

  case 647:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11733 "Parser/parser.cc"
    break;

  case 650:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11739 "Parser/parser.cc"
    break;

  case 652:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11745 "Parser/parser.cc"
    break;

  case 653:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11751 "Parser/parser.cc"
    break;

  case 654:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11757 "Parser/parser.cc"
    break;

  case 655:
#line 2726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11763 "Parser/parser.cc"
    break;

  case 656:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11769 "Parser/parser.cc"
    break;

  case 657:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11775 "Parser/parser.cc"
    break;

  case 659:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11781 "Parser/parser.cc"
    break;

  case 661:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11787 "Parser/parser.cc"
    break;

  case 662:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11793 "Parser/parser.cc"
    break;

  case 664:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11799 "Parser/parser.cc"
    break;

  case 665:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11805 "Parser/parser.cc"
    break;

  case 667:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11811 "Parser/parser.cc"
    break;

  case 668:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11822 "Parser/parser.cc"
    break;

  case 669:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11836 "Parser/parser.cc"
    break;

  case 670:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11842 "Parser/parser.cc"
    break;

  case 671:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11848 "Parser/parser.cc"
    break;

  case 672:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11854 "Parser/parser.cc"
    break;

  case 673:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11865 "Parser/parser.cc"
    break;

  case 674:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11871 "Parser/parser.cc"
    break;

  case 675:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11877 "Parser/parser.cc"
    break;

  case 677:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11883 "Parser/parser.cc"
    break;

  case 678:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11889 "Parser/parser.cc"
    break;

  case 679:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11895 "Parser/parser.cc"
    break;

  case 680:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11901 "Parser/parser.cc"
    break;

  case 681:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11910 "Parser/parser.cc"
    break;

  case 682:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11919 "Parser/parser.cc"
    break;

  case 683:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11925 "Parser/parser.cc"
    break;

  case 684:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11935 "Parser/parser.cc"
    break;

  case 685:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11941 "Parser/parser.cc"
    break;

  case 686:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 11947 "Parser/parser.cc"
    break;

  case 688:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11953 "Parser/parser.cc"
    break;

  case 689:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11959 "Parser/parser.cc"
    break;

  case 690:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11965 "Parser/parser.cc"
    break;

  case 691:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11971 "Parser/parser.cc"
    break;

  case 692:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11977 "Parser/parser.cc"
    break;

  case 693:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11983 "Parser/parser.cc"
    break;

  case 695:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11989 "Parser/parser.cc"
    break;

  case 698:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11995 "Parser/parser.cc"
    break;

  case 699:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12001 "Parser/parser.cc"
    break;

  case 700:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12007 "Parser/parser.cc"
    break;

  case 701:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12013 "Parser/parser.cc"
    break;

  case 704:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12019 "Parser/parser.cc"
    break;

  case 705:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12025 "Parser/parser.cc"
    break;

  case 706:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12031 "Parser/parser.cc"
    break;

  case 708:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12037 "Parser/parser.cc"
    break;

  case 709:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12043 "Parser/parser.cc"
    break;

  case 710:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12049 "Parser/parser.cc"
    break;

  case 712:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12055 "Parser/parser.cc"
    break;

  case 713:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12061 "Parser/parser.cc"
    break;

  case 714:
#line 2935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12067 "Parser/parser.cc"
    break;

  case 715:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12073 "Parser/parser.cc"
    break;

  case 716:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12079 "Parser/parser.cc"
    break;

  case 718:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12085 "Parser/parser.cc"
    break;

  case 719:
#line 2951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12091 "Parser/parser.cc"
    break;

  case 720:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12097 "Parser/parser.cc"
    break;

  case 725:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12103 "Parser/parser.cc"
    break;

  case 727:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12109 "Parser/parser.cc"
    break;

  case 728:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12115 "Parser/parser.cc"
    break;

  case 731:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12121 "Parser/parser.cc"
    break;

  case 734:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12127 "Parser/parser.cc"
    break;

  case 735:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12133 "Parser/parser.cc"
    break;

  case 736:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12139 "Parser/parser.cc"
    break;

  case 737:
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12145 "Parser/parser.cc"
    break;

  case 738:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12151 "Parser/parser.cc"
    break;

  case 739:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12157 "Parser/parser.cc"
    break;

  case 740:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12163 "Parser/parser.cc"
    break;

  case 742:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12169 "Parser/parser.cc"
    break;

  case 743:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12175 "Parser/parser.cc"
    break;

  case 744:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12181 "Parser/parser.cc"
    break;

  case 746:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12187 "Parser/parser.cc"
    break;

  case 748:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12193 "Parser/parser.cc"
    break;

  case 749:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12199 "Parser/parser.cc"
    break;

  case 750:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12205 "Parser/parser.cc"
    break;

  case 751:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12211 "Parser/parser.cc"
    break;

  case 752:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12217 "Parser/parser.cc"
    break;

  case 753:
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12223 "Parser/parser.cc"
    break;

  case 755:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12229 "Parser/parser.cc"
    break;

  case 756:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12235 "Parser/parser.cc"
    break;

  case 757:
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12241 "Parser/parser.cc"
    break;

  case 758:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12252 "Parser/parser.cc"
    break;

  case 759:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12258 "Parser/parser.cc"
    break;

  case 760:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12264 "Parser/parser.cc"
    break;

  case 761:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12270 "Parser/parser.cc"
    break;

  case 762:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12279 "Parser/parser.cc"
    break;

  case 763:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12285 "Parser/parser.cc"
    break;

  case 764:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12295 "Parser/parser.cc"
    break;

  case 765:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12301 "Parser/parser.cc"
    break;

  case 766:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12307 "Parser/parser.cc"
    break;

  case 767:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12313 "Parser/parser.cc"
    break;

  case 768:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12319 "Parser/parser.cc"
    break;

  case 769:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12325 "Parser/parser.cc"
    break;

  case 770:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12331 "Parser/parser.cc"
    break;

  case 771:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12337 "Parser/parser.cc"
    break;

  case 772:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12343 "Parser/parser.cc"
    break;

  case 773:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12349 "Parser/parser.cc"
    break;

  case 776:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12355 "Parser/parser.cc"
    break;

  case 777:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12361 "Parser/parser.cc"
    break;

  case 778:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12367 "Parser/parser.cc"
    break;

  case 779:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12373 "Parser/parser.cc"
    break;

  case 781:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12379 "Parser/parser.cc"
    break;

  case 782:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12385 "Parser/parser.cc"
    break;

  case 783:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12391 "Parser/parser.cc"
    break;

  case 784:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12397 "Parser/parser.cc"
    break;

  case 785:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12403 "Parser/parser.cc"
    break;

  case 786:
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12409 "Parser/parser.cc"
    break;

  case 787:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12415 "Parser/parser.cc"
    break;

  case 788:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12424 "Parser/parser.cc"
    break;

  case 789:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12433 "Parser/parser.cc"
    break;

  case 790:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12442 "Parser/parser.cc"
    break;

  case 791:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12448 "Parser/parser.cc"
    break;

  case 792:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12457 "Parser/parser.cc"
    break;

  case 793:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12463 "Parser/parser.cc"
    break;

  case 795:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12469 "Parser/parser.cc"
    break;

  case 800:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12475 "Parser/parser.cc"
    break;

  case 801:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12481 "Parser/parser.cc"
    break;

  case 802:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12487 "Parser/parser.cc"
    break;

  case 804:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12493 "Parser/parser.cc"
    break;

  case 805:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12499 "Parser/parser.cc"
    break;

  case 806:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12505 "Parser/parser.cc"
    break;

  case 807:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12511 "Parser/parser.cc"
    break;

  case 809:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12517 "Parser/parser.cc"
    break;

  case 810:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12523 "Parser/parser.cc"
    break;

  case 811:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12529 "Parser/parser.cc"
    break;

  case 812:
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12545 "Parser/parser.cc"
    break;

  case 813:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12551 "Parser/parser.cc"
    break;

  case 814:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12557 "Parser/parser.cc"
    break;

  case 815:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12563 "Parser/parser.cc"
    break;

  case 816:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12569 "Parser/parser.cc"
    break;

  case 817:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12575 "Parser/parser.cc"
    break;

  case 818:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12581 "Parser/parser.cc"
    break;

  case 820:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12590 "Parser/parser.cc"
    break;

  case 821:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12596 "Parser/parser.cc"
    break;

  case 822:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12605 "Parser/parser.cc"
    break;

  case 823:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12615 "Parser/parser.cc"
    break;

  case 824:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12624 "Parser/parser.cc"
    break;

  case 825:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12634 "Parser/parser.cc"
    break;

  case 826:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12645 "Parser/parser.cc"
    break;

  case 827:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12655 "Parser/parser.cc"
    break;

  case 828:
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12666 "Parser/parser.cc"
    break;

  case 829:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12676 "Parser/parser.cc"
    break;

  case 830:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12687 "Parser/parser.cc"
    break;

  case 831:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12697 "Parser/parser.cc"
    break;

  case 833:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12703 "Parser/parser.cc"
    break;

  case 834:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12709 "Parser/parser.cc"
    break;

  case 835:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12715 "Parser/parser.cc"
    break;

  case 836:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12727 "Parser/parser.cc"
    break;

  case 837:
#line 3392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12738 "Parser/parser.cc"
    break;

  case 838:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12747 "Parser/parser.cc"
    break;

  case 839:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12756 "Parser/parser.cc"
    break;

  case 840:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12762 "Parser/parser.cc"
    break;

  case 841:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12768 "Parser/parser.cc"
    break;

  case 842:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12774 "Parser/parser.cc"
    break;

  case 843:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12783 "Parser/parser.cc"
    break;

  case 844:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12789 "Parser/parser.cc"
    break;

  case 845:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12795 "Parser/parser.cc"
    break;

  case 846:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12801 "Parser/parser.cc"
    break;

  case 851:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12807 "Parser/parser.cc"
    break;

  case 852:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12813 "Parser/parser.cc"
    break;

  case 853:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12823 "Parser/parser.cc"
    break;

  case 854:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12829 "Parser/parser.cc"
    break;

  case 857:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12835 "Parser/parser.cc"
    break;

  case 858:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12841 "Parser/parser.cc"
    break;

  case 859:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12847 "Parser/parser.cc"
    break;

  case 860:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12853 "Parser/parser.cc"
    break;

  case 862:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 863:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12865 "Parser/parser.cc"
    break;

  case 864:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12871 "Parser/parser.cc"
    break;

  case 865:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 867:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12883 "Parser/parser.cc"
    break;

  case 868:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12889 "Parser/parser.cc"
    break;

  case 869:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12895 "Parser/parser.cc"
    break;

  case 870:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12901 "Parser/parser.cc"
    break;

  case 871:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12907 "Parser/parser.cc"
    break;

  case 873:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12913 "Parser/parser.cc"
    break;

  case 874:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12919 "Parser/parser.cc"
    break;

  case 875:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12925 "Parser/parser.cc"
    break;

  case 876:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12931 "Parser/parser.cc"
    break;

  case 877:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12937 "Parser/parser.cc"
    break;

  case 878:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12943 "Parser/parser.cc"
    break;

  case 879:
#line 3566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12949 "Parser/parser.cc"
    break;

  case 880:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12955 "Parser/parser.cc"
    break;

  case 881:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12961 "Parser/parser.cc"
    break;

  case 882:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12967 "Parser/parser.cc"
    break;

  case 883:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12973 "Parser/parser.cc"
    break;

  case 884:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12979 "Parser/parser.cc"
    break;

  case 885:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12985 "Parser/parser.cc"
    break;

  case 886:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12991 "Parser/parser.cc"
    break;

  case 887:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12997 "Parser/parser.cc"
    break;

  case 888:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13003 "Parser/parser.cc"
    break;

  case 889:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13009 "Parser/parser.cc"
    break;

  case 890:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13015 "Parser/parser.cc"
    break;

  case 892:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13021 "Parser/parser.cc"
    break;

  case 893:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13027 "Parser/parser.cc"
    break;

  case 894:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13033 "Parser/parser.cc"
    break;

  case 895:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13039 "Parser/parser.cc"
    break;

  case 896:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13045 "Parser/parser.cc"
    break;

  case 897:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13051 "Parser/parser.cc"
    break;

  case 898:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13057 "Parser/parser.cc"
    break;

  case 899:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13063 "Parser/parser.cc"
    break;

  case 900:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13069 "Parser/parser.cc"
    break;

  case 901:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13075 "Parser/parser.cc"
    break;

  case 902:
#line 3630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13081 "Parser/parser.cc"
    break;

  case 903:
#line 3632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13087 "Parser/parser.cc"
    break;

  case 904:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13093 "Parser/parser.cc"
    break;

  case 905:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13099 "Parser/parser.cc"
    break;

  case 906:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13105 "Parser/parser.cc"
    break;

  case 907:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13111 "Parser/parser.cc"
    break;

  case 911:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13117 "Parser/parser.cc"
    break;

  case 912:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13123 "Parser/parser.cc"
    break;

  case 913:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13129 "Parser/parser.cc"
    break;

  case 914:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13135 "Parser/parser.cc"
    break;

  case 915:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13141 "Parser/parser.cc"
    break;

  case 916:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13147 "Parser/parser.cc"
    break;

  case 917:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13153 "Parser/parser.cc"
    break;

  case 918:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13159 "Parser/parser.cc"
    break;

  case 919:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13165 "Parser/parser.cc"
    break;

  case 920:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13171 "Parser/parser.cc"
    break;

  case 921:
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13177 "Parser/parser.cc"
    break;

  case 922:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13183 "Parser/parser.cc"
    break;

  case 923:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13189 "Parser/parser.cc"
    break;

  case 924:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13195 "Parser/parser.cc"
    break;

  case 925:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13201 "Parser/parser.cc"
    break;

  case 926:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13210 "Parser/parser.cc"
    break;

  case 927:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13216 "Parser/parser.cc"
    break;

  case 928:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13222 "Parser/parser.cc"
    break;

  case 930:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13228 "Parser/parser.cc"
    break;

  case 931:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13234 "Parser/parser.cc"
    break;

  case 932:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13240 "Parser/parser.cc"
    break;

  case 933:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13246 "Parser/parser.cc"
    break;

  case 934:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13252 "Parser/parser.cc"
    break;

  case 935:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13258 "Parser/parser.cc"
    break;

  case 936:
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13264 "Parser/parser.cc"
    break;

  case 937:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13270 "Parser/parser.cc"
    break;

  case 938:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13276 "Parser/parser.cc"
    break;

  case 939:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13282 "Parser/parser.cc"
    break;

  case 940:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13288 "Parser/parser.cc"
    break;

  case 941:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13294 "Parser/parser.cc"
    break;

  case 942:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13300 "Parser/parser.cc"
    break;

  case 943:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13306 "Parser/parser.cc"
    break;

  case 944:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13312 "Parser/parser.cc"
    break;

  case 945:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13318 "Parser/parser.cc"
    break;

  case 946:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13324 "Parser/parser.cc"
    break;

  case 947:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13330 "Parser/parser.cc"
    break;

  case 949:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13336 "Parser/parser.cc"
    break;

  case 950:
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13342 "Parser/parser.cc"
    break;

  case 951:
#line 3777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13348 "Parser/parser.cc"
    break;

  case 952:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13354 "Parser/parser.cc"
    break;

  case 953:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13360 "Parser/parser.cc"
    break;

  case 954:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13366 "Parser/parser.cc"
    break;

  case 955:
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13372 "Parser/parser.cc"
    break;

  case 956:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13378 "Parser/parser.cc"
    break;

  case 957:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13384 "Parser/parser.cc"
    break;

  case 958:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13390 "Parser/parser.cc"
    break;

  case 959:
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13396 "Parser/parser.cc"
    break;

  case 960:
#line 3801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13402 "Parser/parser.cc"
    break;

  case 961:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13408 "Parser/parser.cc"
    break;

  case 962:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13414 "Parser/parser.cc"
    break;

  case 963:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13420 "Parser/parser.cc"
    break;

  case 964:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13426 "Parser/parser.cc"
    break;

  case 965:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13432 "Parser/parser.cc"
    break;

  case 966:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13439 "Parser/parser.cc"
    break;

  case 968:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13445 "Parser/parser.cc"
    break;

  case 969:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13451 "Parser/parser.cc"
    break;

  case 970:
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13457 "Parser/parser.cc"
    break;

  case 971:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13463 "Parser/parser.cc"
    break;

  case 972:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13469 "Parser/parser.cc"
    break;

  case 973:
#line 3841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13475 "Parser/parser.cc"
    break;

  case 974:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13481 "Parser/parser.cc"
    break;

  case 975:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13487 "Parser/parser.cc"
    break;

  case 976:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13493 "Parser/parser.cc"
    break;

  case 977:
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13499 "Parser/parser.cc"
    break;

  case 978:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13505 "Parser/parser.cc"
    break;

  case 979:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13511 "Parser/parser.cc"
    break;

  case 980:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13517 "Parser/parser.cc"
    break;

  case 981:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13524 "Parser/parser.cc"
    break;

  case 983:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13530 "Parser/parser.cc"
    break;

  case 984:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13536 "Parser/parser.cc"
    break;

  case 985:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13542 "Parser/parser.cc"
    break;

  case 986:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13548 "Parser/parser.cc"
    break;

  case 987:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13554 "Parser/parser.cc"
    break;

  case 988:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13560 "Parser/parser.cc"
    break;

  case 989:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13566 "Parser/parser.cc"
    break;

  case 990:
#line 3899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13572 "Parser/parser.cc"
    break;

  case 991:
#line 3901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13578 "Parser/parser.cc"
    break;

  case 992:
#line 3906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13584 "Parser/parser.cc"
    break;

  case 993:
#line 3908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13590 "Parser/parser.cc"
    break;

  case 995:
#line 3926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13596 "Parser/parser.cc"
    break;

  case 996:
#line 3928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13602 "Parser/parser.cc"
    break;

  case 997:
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13608 "Parser/parser.cc"
    break;

  case 998:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13614 "Parser/parser.cc"
    break;

  case 999:
#line 3937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13620 "Parser/parser.cc"
    break;

  case 1000:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13626 "Parser/parser.cc"
    break;

  case 1001:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13632 "Parser/parser.cc"
    break;

  case 1003:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13638 "Parser/parser.cc"
    break;

  case 1004:
#line 3949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13644 "Parser/parser.cc"
    break;

  case 1005:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13650 "Parser/parser.cc"
    break;

  case 1006:
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13656 "Parser/parser.cc"
    break;

  case 1007:
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13662 "Parser/parser.cc"
    break;

  case 1008:
#line 3960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13668 "Parser/parser.cc"
    break;

  case 1009:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13674 "Parser/parser.cc"
    break;

  case 1010:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13680 "Parser/parser.cc"
    break;

  case 1011:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13686 "Parser/parser.cc"
    break;

  case 1012:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13692 "Parser/parser.cc"
    break;

  case 1014:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13698 "Parser/parser.cc"
    break;

  case 1015:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13704 "Parser/parser.cc"
    break;

  case 1017:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13710 "Parser/parser.cc"
    break;

  case 1018:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13716 "Parser/parser.cc"
    break;

  case 1020:
#line 4002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13722 "Parser/parser.cc"
    break;

  case 1021:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13728 "Parser/parser.cc"
    break;

  case 1022:
#line 4009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13734 "Parser/parser.cc"
    break;

  case 1023:
#line 4011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13740 "Parser/parser.cc"
    break;

  case 1024:
#line 4013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13746 "Parser/parser.cc"
    break;

  case 1025:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13752 "Parser/parser.cc"
    break;

  case 1026:
#line 4049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13758 "Parser/parser.cc"
    break;

  case 1029:
#line 4056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13765 "Parser/parser.cc"
    break;

  case 1030:
#line 4059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13771 "Parser/parser.cc"
    break;

  case 1031:
#line 4061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13777 "Parser/parser.cc"
    break;

  case 1032:
#line 4066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13783 "Parser/parser.cc"
    break;

  case 1033:
#line 4068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13789 "Parser/parser.cc"
    break;

  case 1034:
#line 4070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13795 "Parser/parser.cc"
    break;

  case 1035:
#line 4072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13801 "Parser/parser.cc"
    break;

  case 1036:
#line 4074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13807 "Parser/parser.cc"
    break;

  case 1038:
#line 4080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13813 "Parser/parser.cc"
    break;

  case 1039:
#line 4082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13819 "Parser/parser.cc"
    break;

  case 1040:
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13825 "Parser/parser.cc"
    break;

  case 1041:
#line 4089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13831 "Parser/parser.cc"
    break;

  case 1042:
#line 4091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13837 "Parser/parser.cc"
    break;

  case 1043:
#line 4093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13843 "Parser/parser.cc"
    break;

  case 1045:
#line 4100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13849 "Parser/parser.cc"
    break;

  case 1047:
#line 4111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13855 "Parser/parser.cc"
    break;

  case 1048:
#line 4114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13861 "Parser/parser.cc"
    break;

  case 1049:
#line 4116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13867 "Parser/parser.cc"
    break;

  case 1050:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13873 "Parser/parser.cc"
    break;

  case 1051:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13879 "Parser/parser.cc"
    break;

  case 1052:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13885 "Parser/parser.cc"
    break;

  case 1054:
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13891 "Parser/parser.cc"
    break;

  case 1055:
#line 4140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13897 "Parser/parser.cc"
    break;

  case 1056:
#line 4145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13903 "Parser/parser.cc"
    break;

  case 1057:
#line 4147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13909 "Parser/parser.cc"
    break;

  case 1058:
#line 4149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13915 "Parser/parser.cc"
    break;

  case 1059:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13921 "Parser/parser.cc"
    break;

  case 1060:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13927 "Parser/parser.cc"
    break;

  case 1062:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13933 "Parser/parser.cc"
    break;

  case 1063:
#line 4161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13939 "Parser/parser.cc"
    break;

  case 1064:
#line 4163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13945 "Parser/parser.cc"
    break;

  case 1065:
#line 4168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13951 "Parser/parser.cc"
    break;

  case 1066:
#line 4170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13957 "Parser/parser.cc"
    break;

  case 1069:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13963 "Parser/parser.cc"
    break;

  case 1072:
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13969 "Parser/parser.cc"
    break;

  case 1073:
#line 4193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13975 "Parser/parser.cc"
    break;

  case 1074:
#line 4195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13981 "Parser/parser.cc"
    break;

  case 1075:
#line 4197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13987 "Parser/parser.cc"
    break;

  case 1076:
#line 4199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13993 "Parser/parser.cc"
    break;

  case 1077:
#line 4201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13999 "Parser/parser.cc"
    break;

  case 1078:
#line 4208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14005 "Parser/parser.cc"
    break;

  case 1079:
#line 4210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14011 "Parser/parser.cc"
    break;

  case 1080:
#line 4212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14017 "Parser/parser.cc"
    break;

  case 1081:
#line 4214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14023 "Parser/parser.cc"
    break;

  case 1082:
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14029 "Parser/parser.cc"
    break;

  case 1083:
#line 4219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14035 "Parser/parser.cc"
    break;

  case 1084:
#line 4221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14041 "Parser/parser.cc"
    break;

  case 1085:
#line 4223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14047 "Parser/parser.cc"
    break;

  case 1086:
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14053 "Parser/parser.cc"
    break;

  case 1087:
#line 4227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14059 "Parser/parser.cc"
    break;

  case 1088:
#line 4232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14065 "Parser/parser.cc"
    break;

  case 1089:
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14071 "Parser/parser.cc"
    break;

  case 1090:
#line 4239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14077 "Parser/parser.cc"
    break;

  case 1091:
#line 4241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14083 "Parser/parser.cc"
    break;

  case 1093:
#line 4268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14089 "Parser/parser.cc"
    break;

  case 1097:
#line 4279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14095 "Parser/parser.cc"
    break;

  case 1098:
#line 4281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14101 "Parser/parser.cc"
    break;

  case 1099:
#line 4283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14107 "Parser/parser.cc"
    break;

  case 1100:
#line 4285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14113 "Parser/parser.cc"
    break;

  case 1101:
#line 4287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14119 "Parser/parser.cc"
    break;

  case 1102:
#line 4289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14125 "Parser/parser.cc"
    break;

  case 1103:
#line 4296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14131 "Parser/parser.cc"
    break;

  case 1104:
#line 4298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14137 "Parser/parser.cc"
    break;

  case 1105:
#line 4300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14143 "Parser/parser.cc"
    break;

  case 1106:
#line 4302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14149 "Parser/parser.cc"
    break;

  case 1107:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14155 "Parser/parser.cc"
    break;

  case 1108:
#line 4306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14161 "Parser/parser.cc"
    break;

  case 1109:
#line 4311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14167 "Parser/parser.cc"
    break;

  case 1110:
#line 4313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14173 "Parser/parser.cc"
    break;

  case 1111:
#line 4315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14179 "Parser/parser.cc"
    break;

  case 1112:
#line 4320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14185 "Parser/parser.cc"
    break;

  case 1113:
#line 4322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14191 "Parser/parser.cc"
    break;

  case 1114:
#line 4324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14197 "Parser/parser.cc"
    break;

  case 1117:
#line 4348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14203 "Parser/parser.cc"
    break;

  case 1118:
#line 4350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14209 "Parser/parser.cc"
    break;


#line 14213 "Parser/parser.cc"

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
#line 4353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
