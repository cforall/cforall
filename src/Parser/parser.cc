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

#include "DeclarationNode.h"                            // for DeclarationNode, ...
#include "ExpressionNode.h"                             // for ExpressionNode, ...
#include "InitializerNode.h"                            // for InitializerNode, ...
#include "ParserTypes.h"
#include "StatementNode.h"                              // for build_...
#include "TypedefTable.h"
#include "TypeData.h"
#include "AST/Type.hpp"                                 // for BasicType, BasicKind
#include "Common/SemanticError.h"						// error_str
#include "Common/utility.h"								// for maybeMoveBuild, maybeBuild, CodeLo...

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
#define MISSING_ANON_FIELD "syntax error, missing loop fields with an anonymous loop index is meaningless as loop index is unavailable in loop body."
#define MISSING_LOW "syntax error, missing low value for up-to range so index is uninitialized."
#define MISSING_HIGH "syntax error, missing high value for down-to range so index is uninitialized."

static ForCtrl * makeForCtrl(
		const CodeLocation & location,
		DeclarationNode * init,
		enum OperKinds compop,
		ExpressionNode * comp,
		ExpressionNode * inc ) {
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

ForCtrl * forCtrl( const CodeLocation & location, DeclarationNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( index->initializer ) {
		SemanticError( yylloc, "syntax error, direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "syntax error, multiple loop indexes disallowed in for-loop declaration." );
	} // if
	DeclarationNode * initDecl = index->addInitializer( new InitializerNode( start ) );
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, string * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
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

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, ExpressionNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index->expr.get()) ) {
		return forCtrl( location, type, new string( identifier->name ), start, compop, comp, inc );
	} else if ( auto commaExpr = dynamic_cast<ast::CommaExpr *>( index->expr.get() ) ) {
		if ( auto identifier = commaExpr->arg1.as<ast::NameExpr>() ) {
			return forCtrl( location, type, new string( identifier->name ), start, compop, comp, inc );
		} else {
			SemanticError( yylloc, "syntax error, loop-index name missing. Expression disallowed." ); return nullptr;
		} // if
	} else {
		SemanticError( yylloc, "syntax error, loop-index name missing. Expression disallowed." ); return nullptr;
	} // if
} // forCtrl

static void IdentifierBeforeIdentifier( string & identifier1, string & identifier2, const char * kind ) {
	SemanticError( yylloc, "syntax error, adjacent identifiers \"%s\" and \"%s\" are not meaningful in an %s.\n"
				   "Possible cause is misspelled type name or missing generic parameter.",
				   identifier1.c_str(), identifier2.c_str(), kind );
} // IdentifierBeforeIdentifier

static void IdentifierBeforeType( string & identifier, const char * kind ) {
	SemanticError( yylloc, "syntax error, identifier \"%s\" cannot appear before a %s.\n"
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

#line 347 "Parser/parser.cc"

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
    OFFSETOF = 309,
    BASETYPEOF = 310,
    TYPEID = 311,
    ENUM = 312,
    STRUCT = 313,
    UNION = 314,
    EXCEPTION = 315,
    GENERATOR = 316,
    COROUTINE = 317,
    MONITOR = 318,
    THREAD = 319,
    OTYPE = 320,
    FTYPE = 321,
    DTYPE = 322,
    TTYPE = 323,
    TRAIT = 324,
    LABEL = 325,
    SUSPEND = 326,
    ATTRIBUTE = 327,
    EXTENSION = 328,
    IF = 329,
    ELSE = 330,
    SWITCH = 331,
    CASE = 332,
    DEFAULT = 333,
    DO = 334,
    WHILE = 335,
    FOR = 336,
    BREAK = 337,
    CONTINUE = 338,
    GOTO = 339,
    RETURN = 340,
    CHOOSE = 341,
    FALLTHRU = 342,
    FALLTHROUGH = 343,
    WITH = 344,
    WHEN = 345,
    WAITFOR = 346,
    WAITUNTIL = 347,
    CORUN = 348,
    COFOR = 349,
    DISABLE = 350,
    ENABLE = 351,
    TRY = 352,
    THROW = 353,
    THROWRESUME = 354,
    AT = 355,
    ASM = 356,
    ALIGNAS = 357,
    ALIGNOF = 358,
    GENERIC = 359,
    STATICASSERT = 360,
    IDENTIFIER = 361,
    TYPEDIMname = 362,
    TYPEDEFname = 363,
    TYPEGENname = 364,
    TIMEOUT = 365,
    WAND = 366,
    WOR = 367,
    CATCH = 368,
    RECOVER = 369,
    CATCHRESUME = 370,
    FIXUP = 371,
    FINALLY = 372,
    INTEGERconstant = 373,
    CHARACTERconstant = 374,
    STRINGliteral = 375,
    DIRECTIVE = 376,
    FLOATING_DECIMALconstant = 377,
    FLOATING_FRACTIONconstant = 378,
    FLOATINGconstant = 379,
    ARROW = 380,
    ICR = 381,
    DECR = 382,
    LS = 383,
    RS = 384,
    LE = 385,
    GE = 386,
    EQ = 387,
    NE = 388,
    ANDAND = 389,
    OROR = 390,
    ATTR = 391,
    ELLIPSIS = 392,
    EXPassign = 393,
    MULTassign = 394,
    DIVassign = 395,
    MODassign = 396,
    PLUSassign = 397,
    MINUSassign = 398,
    LSassign = 399,
    RSassign = 400,
    ANDassign = 401,
    ERassign = 402,
    ORassign = 403,
    ErangeUpEq = 404,
    ErangeDown = 405,
    ErangeDownEq = 406,
    ATassign = 407,
    THEN = 408
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
#define OFFSETOF 309
#define BASETYPEOF 310
#define TYPEID 311
#define ENUM 312
#define STRUCT 313
#define UNION 314
#define EXCEPTION 315
#define GENERATOR 316
#define COROUTINE 317
#define MONITOR 318
#define THREAD 319
#define OTYPE 320
#define FTYPE 321
#define DTYPE 322
#define TTYPE 323
#define TRAIT 324
#define LABEL 325
#define SUSPEND 326
#define ATTRIBUTE 327
#define EXTENSION 328
#define IF 329
#define ELSE 330
#define SWITCH 331
#define CASE 332
#define DEFAULT 333
#define DO 334
#define WHILE 335
#define FOR 336
#define BREAK 337
#define CONTINUE 338
#define GOTO 339
#define RETURN 340
#define CHOOSE 341
#define FALLTHRU 342
#define FALLTHROUGH 343
#define WITH 344
#define WHEN 345
#define WAITFOR 346
#define WAITUNTIL 347
#define CORUN 348
#define COFOR 349
#define DISABLE 350
#define ENABLE 351
#define TRY 352
#define THROW 353
#define THROWRESUME 354
#define AT 355
#define ASM 356
#define ALIGNAS 357
#define ALIGNOF 358
#define GENERIC 359
#define STATICASSERT 360
#define IDENTIFIER 361
#define TYPEDIMname 362
#define TYPEDEFname 363
#define TYPEGENname 364
#define TIMEOUT 365
#define WAND 366
#define WOR 367
#define CATCH 368
#define RECOVER 369
#define CATCHRESUME 370
#define FIXUP 371
#define FINALLY 372
#define INTEGERconstant 373
#define CHARACTERconstant 374
#define STRINGliteral 375
#define DIRECTIVE 376
#define FLOATING_DECIMALconstant 377
#define FLOATING_FRACTIONconstant 378
#define FLOATINGconstant 379
#define ARROW 380
#define ICR 381
#define DECR 382
#define LS 383
#define RS 384
#define LE 385
#define GE 386
#define EQ 387
#define NE 388
#define ANDAND 389
#define OROR 390
#define ATTR 391
#define ELLIPSIS 392
#define EXPassign 393
#define MULTassign 394
#define DIVassign 395
#define MODassign 396
#define PLUSassign 397
#define MINUSassign 398
#define LSassign 399
#define RSassign 400
#define ANDassign 401
#define ERassign 402
#define ORassign 403
#define ErangeUpEq 404
#define ErangeDown 405
#define ErangeDownEq 406
#define ATassign 407
#define THEN 408

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 738 "Parser/parser.cc"

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
#define YYLAST   26426

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  181
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  314
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1117
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2196

#define YYUNDEFTOK  2
#define YYMAXUTOK   408


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
       2,     2,     2,   170,     2,     2,     2,   174,   167,     2,
     155,   157,   166,   168,   161,   169,   158,   173,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   162,   180,
     175,   179,   176,   178,   156,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   159,   172,   160,   165,     2,   164,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   163,   177,   154,   171,     2,     2,     2,
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
     145,   146,   147,   148,   149,   150,   151,   152,   153
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   642,   642,   646,   653,   654,   655,   656,   657,   661,
     662,   663,   664,   665,   666,   667,   668,   672,   673,   677,
     678,   683,   687,   688,   699,   701,   703,   705,   706,   708,
     710,   712,   714,   724,   726,   728,   730,   732,   734,   739,
     740,   751,   756,   761,   762,   767,   773,   775,   777,   783,
     785,   789,   791,   793,   813,   815,   817,   820,   822,   824,
     826,   828,   830,   832,   834,   836,   838,   840,   842,   852,
     853,   857,   858,   863,   866,   870,   871,   875,   876,   878,
     880,   882,   884,   886,   891,   893,   895,   903,   904,   912,
     915,   916,   918,   923,   939,   941,   943,   945,   947,   949,
     951,   956,   958,   961,   963,   971,   972,   974,   978,   979,
     980,   981,   985,   986,   988,   990,   992,   994,   996,   998,
    1000,  1007,  1008,  1009,  1010,  1014,  1015,  1019,  1020,  1025,
    1026,  1028,  1030,  1035,  1036,  1038,  1043,  1044,  1046,  1051,
    1052,  1054,  1056,  1058,  1063,  1064,  1066,  1071,  1072,  1077,
    1078,  1083,  1084,  1089,  1090,  1095,  1096,  1101,  1102,  1104,
    1109,  1114,  1115,  1123,  1129,  1130,  1134,  1135,  1139,  1140,
    1144,  1145,  1146,  1147,  1148,  1149,  1150,  1151,  1152,  1153,
    1154,  1164,  1166,  1171,  1172,  1174,  1176,  1181,  1182,  1188,
    1189,  1195,  1196,  1197,  1198,  1199,  1200,  1201,  1202,  1203,
    1204,  1205,  1206,  1207,  1208,  1210,  1211,  1217,  1219,  1229,
    1231,  1239,  1240,  1245,  1247,  1249,  1251,  1253,  1257,  1258,
    1260,  1266,  1295,  1298,  1300,  1302,  1312,  1314,  1316,  1321,
    1326,  1328,  1330,  1332,  1340,  1341,  1343,  1347,  1349,  1353,
    1355,  1356,  1358,  1360,  1365,  1366,  1370,  1375,  1376,  1380,
    1382,  1387,  1389,  1394,  1396,  1398,  1400,  1405,  1407,  1409,
    1411,  1416,  1418,  1423,  1424,  1446,  1448,  1453,  1456,  1458,
    1461,  1463,  1466,  1468,  1473,  1478,  1480,  1485,  1490,  1492,
    1494,  1496,  1498,  1501,  1503,  1506,  1508,  1513,  1519,  1522,
    1524,  1529,  1535,  1537,  1542,  1548,  1551,  1553,  1556,  1558,
    1563,  1570,  1572,  1577,  1583,  1585,  1590,  1596,  1599,  1604,
    1614,  1615,  1619,  1621,  1623,  1628,  1630,  1635,  1636,  1638,
    1643,  1645,  1650,  1652,  1654,  1656,  1659,  1663,  1666,  1670,
    1672,  1674,  1676,  1678,  1680,  1682,  1684,  1686,  1688,  1690,
    1695,  1696,  1700,  1706,  1714,  1719,  1720,  1724,  1725,  1730,
    1734,  1735,  1738,  1740,  1745,  1748,  1750,  1752,  1755,  1757,
    1762,  1767,  1768,  1772,  1777,  1779,  1784,  1786,  1791,  1793,
    1795,  1800,  1805,  1810,  1815,  1817,  1819,  1824,  1826,  1832,
    1833,  1837,  1838,  1839,  1840,  1844,  1849,  1850,  1852,  1854,
    1856,  1860,  1864,  1865,  1869,  1871,  1873,  1875,  1877,  1883,
    1884,  1890,  1891,  1895,  1896,  1901,  1903,  1912,  1913,  1915,
    1920,  1925,  1936,  1937,  1941,  1942,  1948,  1949,  1953,  1955,
    1959,  1961,  1965,  1966,  1970,  1971,  1975,  1976,  1977,  1981,
    1983,  1998,  1999,  2000,  2001,  2003,  2007,  2009,  2013,  2020,
    2022,  2024,  2029,  2030,  2032,  2034,  2036,  2046,  2048,  2060,
    2063,  2068,  2070,  2076,  2081,  2086,  2097,  2104,  2109,  2111,
    2113,  2119,  2123,  2130,  2132,  2133,  2134,  2150,  2152,  2155,
    2157,  2160,  2165,  2166,  2170,  2171,  2172,  2173,  2182,  2183,
    2184,  2193,  2194,  2195,  2199,  2200,  2201,  2210,  2211,  2212,
    2217,  2218,  2227,  2228,  2233,  2235,  2239,  2241,  2243,  2245,
    2252,  2257,  2262,  2263,  2265,  2275,  2276,  2281,  2283,  2285,
    2287,  2289,  2291,  2294,  2296,  2298,  2303,  2309,  2311,  2313,
    2315,  2317,  2319,  2321,  2323,  2325,  2327,  2329,  2331,  2333,
    2335,  2337,  2339,  2341,  2343,  2345,  2347,  2349,  2351,  2353,
    2355,  2357,  2359,  2361,  2363,  2368,  2369,  2373,  2379,  2380,
    2386,  2387,  2389,  2391,  2393,  2398,  2400,  2405,  2406,  2408,
    2410,  2415,  2417,  2419,  2421,  2423,  2425,  2430,  2431,  2433,
    2435,  2440,  2442,  2441,  2445,  2453,  2454,  2456,  2458,  2463,
    2464,  2466,  2471,  2472,  2474,  2476,  2481,  2483,  2485,  2490,
    2492,  2494,  2496,  2497,  2499,  2504,  2506,  2508,  2513,  2514,
    2518,  2519,  2526,  2525,  2530,  2529,  2539,  2538,  2549,  2548,
    2558,  2563,  2564,  2569,  2575,  2593,  2594,  2598,  2600,  2602,
    2607,  2609,  2611,  2613,  2618,  2620,  2625,  2627,  2636,  2637,
    2642,  2651,  2656,  2658,  2660,  2669,  2671,  2672,  2673,  2675,
    2677,  2678,  2683,  2684,  2688,  2689,  2694,  2696,  2699,  2702,
    2709,  2710,  2711,  2717,  2722,  2724,  2730,  2731,  2737,  2738,
    2742,  2750,  2757,  2770,  2769,  2773,  2776,  2775,  2784,  2788,
    2792,  2794,  2800,  2801,  2806,  2811,  2819,  2821,  2827,  2829,
    2834,  2835,  2841,  2842,  2843,  2852,  2853,  2855,  2856,  2861,
    2862,  2863,  2865,  2871,  2872,  2874,  2875,  2876,  2878,  2880,
    2887,  2888,  2890,  2892,  2897,  2898,  2907,  2909,  2914,  2916,
    2921,  2922,  2924,  2927,  2929,  2933,  2934,  2935,  2937,  2939,
    2947,  2949,  2954,  2955,  2956,  2960,  2961,  2962,  2967,  2968,
    2973,  2974,  2975,  2976,  2980,  2981,  2986,  2987,  2988,  2989,
    2990,  3004,  3005,  3010,  3011,  3017,  3019,  3022,  3024,  3026,
    3049,  3050,  3056,  3057,  3063,  3062,  3072,  3071,  3075,  3081,
    3083,  3093,  3094,  3096,  3100,  3105,  3107,  3109,  3111,  3117,
    3118,  3122,  3123,  3128,  3130,  3137,  3139,  3140,  3142,  3147,
    3149,  3151,  3156,  3158,  3163,  3168,  3176,  3181,  3183,  3188,
    3193,  3194,  3199,  3200,  3204,  3205,  3206,  3211,  3213,  3219,
    3221,  3226,  3228,  3234,  3235,  3239,  3243,  3247,  3249,  3261,
    3263,  3265,  3267,  3269,  3271,  3273,  3274,  3279,  3282,  3281,
    3293,  3292,  3305,  3304,  3318,  3317,  3331,  3330,  3346,  3352,
    3354,  3360,  3361,  3372,  3379,  3384,  3390,  3393,  3396,  3400,
    3406,  3409,  3412,  3417,  3418,  3419,  3420,  3424,  3432,  3433,
    3445,  3446,  3450,  3451,  3456,  3458,  3460,  3465,  3466,  3472,
    3473,  3475,  3480,  3481,  3482,  3483,  3484,  3486,  3521,  3523,
    3528,  3530,  3531,  3533,  3538,  3540,  3542,  3544,  3549,  3551,
    3553,  3555,  3557,  3559,  3561,  3566,  3568,  3570,  3572,  3581,
    3583,  3584,  3589,  3591,  3593,  3595,  3597,  3602,  3604,  3606,
    3608,  3613,  3615,  3617,  3619,  3621,  3623,  3635,  3636,  3637,
    3641,  3643,  3645,  3647,  3649,  3654,  3656,  3658,  3660,  3665,
    3667,  3669,  3671,  3673,  3675,  3687,  3692,  3697,  3699,  3700,
    3702,  3707,  3709,  3711,  3713,  3718,  3720,  3722,  3724,  3726,
    3728,  3730,  3735,  3737,  3739,  3741,  3750,  3752,  3753,  3758,
    3760,  3762,  3764,  3766,  3771,  3773,  3775,  3777,  3782,  3784,
    3786,  3788,  3790,  3792,  3802,  3804,  3807,  3808,  3810,  3815,
    3817,  3819,  3824,  3826,  3828,  3830,  3835,  3837,  3839,  3853,
    3855,  3858,  3859,  3861,  3866,  3868,  3873,  3875,  3877,  3882,
    3884,  3889,  3891,  3908,  3909,  3911,  3916,  3918,  3920,  3922,
    3924,  3929,  3930,  3932,  3934,  3939,  3941,  3943,  3949,  3951,
    3954,  3961,  3963,  3972,  3974,  3976,  3977,  3979,  3981,  3985,
    3987,  3992,  3994,  3996,  3998,  4033,  4034,  4038,  4039,  4042,
    4044,  4049,  4051,  4053,  4055,  4057,  4062,  4063,  4065,  4067,
    4072,  4074,  4076,  4082,  4083,  4085,  4094,  4097,  4099,  4102,
    4104,  4106,  4120,  4121,  4123,  4128,  4130,  4132,  4134,  4136,
    4141,  4142,  4144,  4146,  4151,  4153,  4161,  4162,  4163,  4168,
    4169,  4174,  4176,  4178,  4180,  4182,  4184,  4191,  4193,  4195,
    4197,  4199,  4202,  4204,  4206,  4208,  4210,  4215,  4217,  4219,
    4224,  4250,  4251,  4253,  4257,  4258,  4262,  4264,  4266,  4268,
    4270,  4272,  4279,  4281,  4283,  4285,  4287,  4289,  4294,  4296,
    4298,  4303,  4305,  4307,  4325,  4327,  4332,  4333
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
  "VA_LIST", "VA_ARG", "AUTO_TYPE", "OFFSETOF", "BASETYPEOF", "TYPEID",
  "ENUM", "STRUCT", "UNION", "EXCEPTION", "GENERATOR", "COROUTINE",
  "MONITOR", "THREAD", "OTYPE", "FTYPE", "DTYPE", "TTYPE", "TRAIT",
  "LABEL", "SUSPEND", "ATTRIBUTE", "EXTENSION", "IF", "ELSE", "SWITCH",
  "CASE", "DEFAULT", "DO", "WHILE", "FOR", "BREAK", "CONTINUE", "GOTO",
  "RETURN", "CHOOSE", "FALLTHRU", "FALLTHROUGH", "WITH", "WHEN", "WAITFOR",
  "WAITUNTIL", "CORUN", "COFOR", "DISABLE", "ENABLE", "TRY", "THROW",
  "THROWRESUME", "AT", "ASM", "ALIGNAS", "ALIGNOF", "GENERIC",
  "STATICASSERT", "IDENTIFIER", "TYPEDIMname", "TYPEDEFname",
  "TYPEGENname", "TIMEOUT", "WAND", "WOR", "CATCH", "RECOVER",
  "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
  "CHARACTERconstant", "STRINGliteral", "DIRECTIVE",
  "FLOATING_DECIMALconstant", "FLOATING_FRACTIONconstant",
  "FLOATINGconstant", "ARROW", "ICR", "DECR", "LS", "RS", "LE", "GE", "EQ",
  "NE", "ANDAND", "OROR", "ATTR", "ELLIPSIS", "EXPassign", "MULTassign",
  "DIVassign", "MODassign", "PLUSassign", "MINUSassign", "LSassign",
  "RSassign", "ANDassign", "ERassign", "ORassign", "ErangeUpEq",
  "ErangeDown", "ErangeDownEq", "ATassign", "THEN", "'}'", "'('", "'@'",
  "')'", "'.'", "'['", "']'", "','", "':'", "'{'", "'`'", "'^'", "'*'",
  "'&'", "'+'", "'-'", "'!'", "'~'", "'\\\\'", "'/'", "'%'", "'<'", "'>'",
  "'|'", "'?'", "'='", "';'", "$accept", "push", "pop", "constant",
  "quasi_keyword", "identifier", "identifier_at", "string_literal",
  "string_literal_list", "primary_expression", "generic_assoc_list",
  "generic_association", "postfix_expression",
  "argument_expression_list_opt", "argument_expression_list",
  "argument_expression", "field_name_list", "field", "field_name",
  "fraction_constants_opt", "unary_expression", "ptrref_operator",
  "unary_operator", "cast_expression", "qualifier_cast_list",
  "cast_modifier", "exponential_expression", "multiplicative_expression",
  "additive_expression", "shift_expression", "relational_expression",
  "equality_expression", "AND_expression", "exclusive_OR_expression",
  "inclusive_OR_expression", "logical_AND_expression",
  "logical_OR_expression", "conditional_expression", "constant_expression",
  "assignment_expression", "assignment_expression_opt",
  "assignment_operator", "simple_assignment_operator",
  "compound_assignment_operator", "tuple", "tuple_expression_list",
  "comma_expression", "comma_expression_opt", "statement",
  "labeled_statement", "compound_statement", "statement_decl_list",
  "statement_decl", "statement_list_nodecl", "expression_statement",
  "selection_statement", "conditional_declaration", "case_value",
  "case_value_list", "case_label", "case_label_list", "case_clause",
  "switch_clause_list_opt", "switch_clause_list", "iteration_statement",
  "for_control_expression_list", "for_control_expression", "enum_key",
  "downupdowneq", "updown", "updowneq", "jump_statement",
  "fall_through_name", "with_statement", "mutex_statement", "when_clause",
  "when_clause_opt", "cast_expression_list", "timeout", "wor", "waitfor",
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
  "field_declaration", "field_declaring_list_opt", "field_declaring_list",
  "field_declarator", "field_abstract_list_opt", "field_abstract",
  "cfa_field_declaring_list", "cfa_field_abstract_list",
  "bit_subrange_size_opt", "bit_subrange_size", "enum_type", "$@6", "$@7",
  "enumerator_type", "hide_opt", "enum_type_nobody", "enumerator_list",
  "visible_hide_opt", "enumerator_value_opt",
  "parameter_list_ellipsis_opt", "parameter_list",
  "cfa_parameter_list_ellipsis_opt", "cfa_parameter_list",
  "cfa_abstract_parameter_list", "parameter_declaration",
  "abstract_parameter_declaration", "cfa_parameter_declaration",
  "cfa_abstract_parameter_declaration", "identifier_list",
  "identifier_or_type_name", "type_no_function", "type", "initializer_opt",
  "initializer", "initializer_list_opt", "designation", "designator_list",
  "designator", "type_parameter_list", "type_initializer_opt",
  "type_parameter", "$@8", "$@9", "new_type_class", "type_class",
  "assertion_list_opt", "assertion_list", "assertion", "type_list",
  "type_declaring_list", "type_declarator", "type_declarator_name",
  "trait_specifier", "trait_declaration_list", "trait_declaration",
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
     405,   406,   407,   408,   125,    40,    64,    41,    46,    91,
      93,    44,    58,   123,    96,    94,    42,    38,    43,    45,
      33,   126,    92,    47,    37,    60,    62,   124,    63,    61,
      59
};
# endif

#define YYPACT_NINF (-1887)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1116)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      62, 13194,    73,   155, 19953,    53, -1887, -1887, -1887, -1887,
   -1887, -1887, -1887, -1887, -1887, -1887, -1887, -1887,   101,  1112,
     166, -1887, -1887, -1887, -1887, -1887, -1887, -1887, -1887, -1887,
   -1887, -1887, -1887, -1887, -1887, -1887, -1887, -1887, -1887, -1887,
   -1887, -1887, -1887, -1887, -1887, -1887, -1887, -1887,    96,   348,
   -1887, -1887, -1887, -1887, -1887, -1887,  4194,  4194,   274, 13194,
     392,   437, 10280, -1887,   457, -1887, -1887, -1887, -1887, -1887,
   -1887, -1887, -1887, -1887, -1887,   466,  1703, -1887,   416,   443,
   -1887, -1887, -1887, -1887, -1887, 19488, -1887, -1887,   444,   476,
     251,   446, -1887,  4194,   488,   504,   524,   550,  3784,   743,
     916, 13359, -1887, -1887,   684, 19333,  1530, -1887, -1887, -1887,
   -1887,  2207,   756, 14712, 10121,   981,  2207,  1003,   609, -1887,
   -1887, -1887, -1887,   114, -1887, -1887, -1887, -1887,   613, -1887,
   -1887, -1887, -1887, -1887,   627,   634,   114, -1887,   114, 17651,
   -1887, -1887, -1887, 21283,  4194, -1887, -1887,  4194, -1887, 13194,
   -1887,   635, 21336, -1887, -1887,  5089, 22635, -1887, -1887,  1224,
    1224,   673,  3563, -1887, -1887, -1887, -1887,   552, 15947,   114,
    2750,   114, -1887, -1887, -1887, -1887, -1887, -1887,   656, -1887,
     665,   693,  2434, -1887,   778, 25875, -1887, -1887, -1887, -1887,
   -1887, -1887, -1887, 18030,   972,  2738,  1703,   210,   748,   762,
     773,   784,   796,   801, -1887, -1887, 20108, 12019,   826,   806,
   -1887, 20411, -1887, -1887, -1887, -1887,   834, -1887, -1887,   839,
   -1887, 23823,   993, 23975, -1887,   875,  4194,   634,   880,  3488,
    5089,  3488, -1887, -1887, -1887,  3045,  4833,   877,   947,   499,
     947, -1887,   114,   114,    74, 17329,   512,   947, -1887,   114,
     114,    74,   114, -1887,   114, -1887,  4940, -1887, -1887,   902,
     908,  1224, 23379,   919, 19488, -1887, -1887,  2207, -1887,  1727,
     609,   944,  1071, 17329,  4194,  4194,   251, -1887, 15127, -1887,
    1224,  1224,   951,  1071, 17329,  4194, -1887, 10709, -1887, -1887,
   -1887,  1224, -1887, -1887, -1887, -1887,  1224, -1887,   776,  4372,
    4194, -1887, 19187,  1035, -1887, -1887, -1887, 23242,   634, 17490,
     890,  5089, 19134, 23379,  2207, -1887, -1887, 22783, -1887,   947,
     102, -1887, 25875, 22635,  4508,  4940, -1887,   614, -1887, -1887,
   -1887, -1887, -1887, 21336,  4194, -1887,  1002,  1022, -1887, -1887,
   -1887, -1887,  4194,  3125,   542,   751, -1887,  4194,   665, -1887,
     816,   114, -1887,  1053, 21491,  1123, 16439, 23432,  2207, -1887,
    2207,  1224,  2207,  1224, -1887, -1887,   114, -1887, -1887,  1045,
   21544, -1887, -1887, -1887, 21699,   834, -1887,  2895,   229,   586,
   -1887,   299,   609,  1066,  1064, -1887,  3563,  1063,   665,  3563,
   -1887, -1887, -1887, -1887, -1887,   972, -1887,   785, -1887,  1092,
   -1887,  1097,  1129, 25951,  1133,  1138,  1140, 25875, 26027,  1156,
   23622, -1887, -1887, -1887, -1887, -1887, -1887, 26103, 26103, 17871,
    1116,  4088, -1887, -1887, -1887, -1887,   553, -1887,   569, -1887,
    1182, -1887, 25875, 25875, -1887,  1145,   261,   812,   893,  1032,
     998,  1175,  1183,  1178,  1242,   127, -1887,   883, -1887,  1200,
   -1887,   986,  3994, 18507, -1887, -1887,   685,  1200, -1887, -1887,
     901, -1887, -1887,   907,  2738,  1210,  1222,  1236,  1258,  1267,
    1271, -1887, -1887,   622,  1284, -1887,   910,  1284,  1235, -1887,
    1275, -1887, 21283, -1887,  1102,  1293, 18666, -1887, -1887,  5228,
    4345,  1332, 16439,  1335,  1315,  1357,  1318,  1320, -1887, -1887,
   -1887,  4194,  5299, 20765, -1887, -1887, -1887, -1887, -1887, -1887,
   18993,  4123,  1116, 23823,  1334,  1337, -1887, -1887,  1362, 23975,
     742, -1887, -1887, -1887, 18507,  1377, -1887, -1887, -1887, -1887,
    1367,  3045,   671,  1392,  1401,  1407,   780,  1409,  1411,  1415,
    1418,  1420,  1422,  4833, -1887, -1887, -1887,   114,  1442,  1424,
    1434, -1887, -1887,  1446,   251, -1887, -1887,   634,  1071, 20272,
   -1887, -1887,   251, -1887, -1887,   634, -1887, -1887,  4940, -1887,
   18507, 18507, -1887,  1224,  5089, 23569,  3381, 16603, -1887, -1887,
   -1887, -1887, -1887,   634,  1071,   102,  1456, -1887, -1887,  2207,
    1472,  1071, 17329, -1887,   634,  1071, -1887, 23737, -1887,  1224,
    1224, -1887, -1887,  1476,   462,  1478,   609,  1480, -1887, -1887,
   -1887, 20712,  1459,  1463, -1887, -1887,   961, -1887,  1575, -1887,
    1485, -1887, -1887, -1887, 21863, 26179, -1887, -1887, -1887, -1887,
   -1887,  4508,   846,  4940, 20272, 16767,   947, 13194, -1887,  4194,
    1496, -1887,  1503, -1887, -1887, -1887, -1887, -1887,  3563, -1887,
   -1887,  1582,  4759, 20920, 12019, -1887, 21916, -1887,  1224,  1224,
   -1887, -1887,   834, -1887, 15455,  1506,  1651, 25875,   900,  1446,
    1493, -1887,   114,   114, -1887,  1284, -1887, 21491, -1887, -1887,
   20712,  1224,  1224, -1887,  4759, -1887, -1887, 22487, -1887, -1887,
   21544, -1887,   114,  1511,   114,  1064,   235,  1517,   979, 21336,
     992,  1011, -1887,   972, 24051,  1501, -1887, 18189, -1887,  4088,
   22071, 21336, -1887, 18189, -1887, 25875, -1887, -1887, -1887, -1887,
   -1887, -1887, 18348, -1887, -1887, 20973, 22071, 22071,  1261,  1432,
    2025,   639,  2048, -1887,  1031,  1525,  1290,  1528, -1887, 24127,
   25875, 24203,  1529, 25875,  3488, 25875,  3488, -1887,  3013, -1887,
   -1887, 24051,  2541, 25875, 24051,  3488, -1887, -1887, 25875, 25875,
   25875, 25875, 25875, 25875, 25875, 25875, 25875, 25875, 25875, 25875,
   25875, 25875, 25875, 25875, 25875, 25875, 25875, 24279,  1508,   778,
    3635, 12019, -1887, -1887, -1887, -1887, -1887, -1887, -1887, -1887,
   -1887, -1887, -1887,  1531, 25875, -1887, -1887, 15619,  1419, -1887,
   -1887,   114,   114, -1887, -1887, 18507, -1887, -1887,   628,  1284,
   -1887,  1040,  1284, 20272, -1887, -1887,  1446, 20272, -1887,  1446,
   -1887, 26255, -1887, -1887, -1887, 19798, 12019,  1537,  1306,  1539,
   14963,  1683,  3181,   662,  1493, -1887,   114,   114,  1493,   668,
   -1887,   114,   114, 25875,  4194, 16603,  1542, 16603,  1546,  1493,
      17, 15783, 15783, 15783,  4194, -1887, -1887, 25875,  1362, -1887,
   23823,  1555, -1887,  2171, -1887, -1887, -1887,  1039, -1887, 15783,
   25875,  1050,  1553,  1557,  1559,  1056,  1561,  1562,  1565,  1566,
    1567,  1570,   720,  1284, -1887, -1887,   763,  1284, -1887, -1887,
     789,  1284, -1887, -1887, -1887,  5089,   778,  1706,  1284, 22931,
   -1887, -1887,   634,  1572, -1887, -1887, -1887,  1074,  1573,  1075,
    1577, -1887,  1235,  1576,  1578, -1887,   634, -1887,  1586, -1887,
     634,  1071,  1578, -1887,   634,  1579,  1581,  1583, -1887, -1887,
   20575, -1887,  3488,  4194, 11148,  1671, -1887,  1293, -1887, 15783,
    1100,  1587, -1887,  1578,  1590, -1887, 22124, 18507,  1571, -1887,
    1571, -1887, -1887, -1887, -1887, 21544, -1887, 12187, 18825, -1887,
    1592,  1597,  1598,  1599, -1887,  6189,   114, -1887,   900, -1887,
   -1887, -1887, -1887,  1446, -1887, -1887, -1887,  1224, -1887, -1887,
   -1887, -1887,   235,  1064,  1596,   552, -1887, -1887,  1600,  4194,
     235, -1887, -1887,  1602,  1603, -1887, -1887,  1111, -1887, -1887,
   -1887, -1887,  1609,  1612,  1611,  1610,  1615,  1616,  1620,  1621,
    1632,  1625, 25875,  1637,  1638,  1655, 22279, 12355, 25875, -1887,
   -1887,  2080, -1887, -1887, -1887, 25875, -1887,  1664,  1669, 23899,
   -1887, -1887,  1314, -1887, 24051,  1648, -1887,  1667, -1887, -1887,
    2356, -1887,  1119, -1887, -1887, -1887,  2356, -1887, -1887,  1324,
     313, -1887, -1887,  1145,  1145,  1145,   261,   261,   812,   812,
     893,   893,   893,   893,  1032,  1032,   998,  1175,  1183,  1178,
    1242, 25875,  1340, -1887,  1672,  2356, -1887, -1887, 23823, -1887,
    1673,  1676,  1679,  1681,  1419, -1887, -1887, -1887, -1887, -1887,
   20272, -1887, -1887,  1446, 20272, -1887,  1446,  1685,  1686, 15783,
   15783, -1887, -1887, 14963,   997,  1693,  1697,  1698,  1704,  2506,
    3181, -1887, -1887, 20272, -1887, -1887, -1887, -1887, -1887, -1887,
   20272, -1887, -1887, -1887, -1887,  1668, -1887,  1493,  1699, -1887,
   -1887, -1887, -1887, -1887, -1887, -1887, -1887,  1705,  1710,  1711,
   -1887, -1887,   251,  2356,  1351,    76, -1887, -1887,  1712, -1887,
   23975, -1887, 25875,   114, 15783, -1887, -1887,   855,  1284, -1887,
     868,  1284, -1887, -1887,   897,  1284, 20272, -1887, -1887,  1446,
   20272, -1887, -1887,  1446, 20272, -1887, -1887,  1446,   947,  1707,
   -1887,  1446,   273, -1887,  1200,  1713, -1887, -1887, -1887, -1887,
   -1887, -1887,  1723, -1887, -1887, -1887, 22124,  1578, -1887,   634,
   -1887, -1887, -1887, -1887, -1887, 14012, -1887, -1887, -1887, -1887,
     352, -1887,   663,   418, 11851,  1728,  1729, 17151,  1733,  1737,
    2856,  2878,  3415, 24355,  1740, -1887, -1887,  1741,  1743, 17151,
    1744, -1887, -1887,   634, 25875, 25875,  1853,  1720,   401, -1887,
   17712,  1356,  1739,  1722, -1887, -1887, -1887, 10970, -1887, -1887,
   -1887, -1887, -1887,  1811, -1887, -1887, -1887,  1427,   152, -1887,
     181, -1887,   152, -1887, -1887, -1887, -1887, -1887,  3488, -1887,
   -1887, 13524, 19643,  1746, -1887,  4194,  1750,  1752, -1887, 16767,
   -1887, -1887,  4194, -1887, -1887,  5089, -1887, -1887,  1734,  1735,
    1122, 21336,   665,   665, -1887, -1887,  1116,  1293, 18666, -1887,
    1200, -1887, 12523, -1887,   957,  1284, -1887,  1224,  6822, -1887,
   -1887,  1064,  1600,  1753,   235,   609,   329,  1749,  1754,  1600,
    1775, -1887, -1887, 24051,   422, -1887, 20712, 12355,  3488, -1887,
     422, -1887, 21128,   422, -1887, 25875, 25875, 25875, -1887, -1887,
   -1887, -1887, 25875, 25875,  1768, 23823, -1887, -1887, 24431,  1773,
     429, -1887, -1887, -1887,  2238, -1887, -1887,  1361, -1887,   142,
   -1887,  1368, -1887, 24127, -1887, -1887, 25875,  1755,  1391,  1402,
    1362, -1887,   970,  1284, -1887, -1887,  1779,  1780, -1887, -1887,
   -1887, -1887,  1784,  1023,  1284, -1887,  1025,  2993,   114,   114,
   -1887, -1887,  1785,  1787, -1887,  1789, -1887, 16603,  1790, -1887,
   16111, 16275,  1788,  1797, -1887,  1786, 25875, 25875,  1410,  1795,
   -1887, -1887, -1887, -1887, -1887,  1802, 20272, -1887, -1887,  1446,
   20272, -1887, -1887,  1446, 20272, -1887, -1887,  1446,  1806,  1809,
    1810,   251,   114, -1887, -1887,  1429, 25875, 23083,  1808,  1815,
   -1887, -1887, -1887,  1816, 14170, 14328, 14486, 22124, 23379, 22071,
   22071,  1817, -1887,   360,   385,  1690, 14799, -1887,   409,  4194,
    4194, -1887, 24051,   375,   510, -1887, -1887, -1887, -1887, 11851,
   25875,  1818,  1899, 11682, 11326, -1887,  1800, -1887,  1801, 25875,
    1803, 23823,  1804, 25875, 18507, 25875, -1887, 11504,  1109, -1887,
    1813,   -30, -1887,   -20,  1904,    47, -1887,  1843, -1887,  1819,
   -1887,  1820,  1846,  1847, 17151, 17151, -1887, -1887,  1914, -1887,
   -1887,    87,    87,   888, 15291,   114,   449, -1887, -1887,  1848,
    1852,   542, -1887,  1855, -1887,  1849, -1887,  1850, -1887, -1887,
   -1887, -1887, 12691,  1854,  1856,  1858, -1887, 20272, -1887, -1887,
    1446, 25875, 25875,  1293,  1861, -1887,  1844,  1866,   235,  1600,
     552,  4194, -1887, 24507, -1887,  1868, -1887, 22124, -1887,   691,
    1867,  1863,  1155, -1887,  1865, -1887, -1887, -1887, -1887, -1887,
   23823,  1362, -1887, -1887, 24127, -1887,  1889,  2356, -1887,  1889,
    1889, -1887,  2356,  3210,  3752, -1887,  1439, -1887, -1887, -1887,
    1874, 20272, -1887, -1887,  1446, -1887, -1887,  1875,  1877,   114,
   20272, -1887, -1887,  1446, 20272, -1887, -1887,  1892, -1887, -1887,
   -1887, -1887, -1887, -1887, -1887, -1887,  1699, -1887, -1887, -1887,
    1891, -1887, -1887, -1887, -1887,  1893,  1895,   114,  1897,  1902,
    1911, -1887, -1887, -1887, -1887, -1887, 25875, -1887,   273, -1887,
    1200, -1887, -1887,  1903,  1915, -1887,  1817,  1817,  1817,  3670,
     937,  1876,   473, -1887,  3670,   517, 18507, -1887, -1887, -1887,
    4725, 25875,  5506,   278,  1910, -1887, -1887,   144,  1912,  1912,
    1912,  4194, -1887, -1887, -1887,  1157, -1887, -1887, -1887, -1887,
    1739,  1919, 25875,   444,  1916,   550, 14651, 22124,  1186,  1923,
   17151,  1924, -1887, -1887, -1887,  1376, 17151, 25875,  1233,   632,
   -1887, 25875, 23742, -1887, -1887,   519, -1887,  1362, -1887,  1193,
    1220,  1228,   648, -1887, -1887, -1887, -1887,   634,  1109,  1927,
   -1887, -1887, 25875, -1887,  1928,   778, -1887, -1887, -1887, -1887,
   25875, 25875, -1887, -1887,   448,    87, -1887,   630, -1887, -1887,
   10792, -1887,   114, -1887,  1571, -1887, 22124, -1887, -1887, -1887,
   -1887, -1887,  1929,  1930, -1887, -1887,  1931, -1887,  1939,   235,
   -1887,  1600,  1936,   258,  1754, 23823, -1887, -1887, -1887,  1945,
   -1887, -1887, 25875, -1887, 21128, 25875,  1362,  1949,  1441, -1887,
    1447, -1887,  2356, -1887,  2356, -1887, -1887, -1887,  1951,   114,
     114,  1957,  1958, -1887,  1956, -1887, -1887, -1887, -1887, -1887,
    1452, 25875, -1887, -1887, -1887, -1887, -1887,  1937,   937,  1425,
     555, -1887, -1887, -1887, -1887,   114,   114, -1887, -1887, -1887,
    1940, -1887,  1231,  4725,   941, -1887,  5506, -1887, -1887,   114,
   -1887, -1887, -1887, -1887, -1887, 17151, 17151,  1739, 16931,   425,
   24583,  2044, 17151, -1887, -1887, -1887, -1887, 25875, -1887, 24659,
    2050,  1943,  9934, 24735, 17151, 11504,  1739,  1148,  1312,  1946,
   25875, -1887,  1973,   431, 17151, -1887, 17151, -1887,  1975, -1887,
   -1887,  1952,   778,   698,  1976,  1453,  1239, 17151,  1980, 17151,
   17151, 17151, -1887, -1887, -1887,   665, -1887,  4194,  5089, -1887,
   -1887,  1977,  1981, -1887, -1887,  1600,  1982, -1887, -1887, -1887,
    1362,  1986, -1887, -1887, -1887, -1887,  1989, -1887, -1887, -1887,
    1461,  1466, -1887, -1887, -1887, -1887, -1887, -1887, -1887, -1887,
   -1887,  1987,  1988,  1991,  1425, -1887,   114, -1887, -1887, -1887,
   -1887, -1887,  1994,  3670, -1887,  2075,  4240,    79, 12862, -1887,
   17028, -1887,    11,  1241, 17151,  2083,   558,  1984,   -51, 17151,
   25875,  2051,  2002,  1148,  1312,  1983, -1887, 24811,  1995,   157,
    2087, -1887, 24887, 24963, 25875,  1739,  1993, 13029, -1887, -1887,
   -1887, -1887, 22332, -1887,  2005,  1996,   150, -1887, 25875, 24051,
   -1887, -1887, 25875,   152, -1887, -1887, -1887, -1887, -1887, -1887,
   -1887,  2017, -1887,  2020, -1887, -1887, -1887, -1887,  1051,  1284,
   -1887, -1887,   937, -1887, 17151, -1887,   141, -1887,   288, -1887,
   -1887, -1887,  2021, 13689, -1887, -1887, 17151, -1887,    58, -1887,
   17151, 25875,  2023, 25039, -1887, -1887, -1887, 25115, 25191, 25875,
    1739, -1887, 25267, 25343, 17151,  2006,   455,  2009,   594, -1887,
   -1887,  2028, 13689, 22332, -1887,  4572, 21916,  3488,  2024, -1887,
    2079,  2032,   706,  2027, -1887, -1887,  1244,  1250,   459, -1887,
   -1887, 20272, -1887, -1887,  1446, -1887, -1887, 25875, -1887, 25875,
   -1887, -1887,  1552, 13854, -1887, -1887, 17151, -1887, -1887,  1739,
   -1887, -1887,  1739,  2018,   601,  2022,   624, -1887, -1887,  1739,
   -1887,  1739, -1887,  2035, 25419, 25495, 25571, -1887,  1552, -1887,
    2014,  4309,  3500, -1887, -1887, -1887,   150,  2036, 25875,  2015,
     150,   150, -1887, -1887, 17151,  2122,  2042, -1887, -1887, 17028,
   -1887,  1552, -1887, -1887,  2045, 25647, 25723, 25799, -1887, -1887,
    1739, -1887,  1739, -1887,  1739, -1887,  2014, 25875,  2052,  3500,
    2040,   778,  2064, -1887,   724, -1887, -1887, 17151, -1887, -1887,
   10346,  2068, 17028, -1887, -1887,  1739, -1887,  1739, -1887,  1739,
    2069,  2070, -1887,   634,   778,  2071, -1887,  2053,   778, -1887,
   -1887, -1887, -1887, 10550, -1887,   634, -1887, -1887,  1470, 25875,
   -1887,  1251, -1887,   778,  3488,  2077,  2055, -1887, -1887,  1253,
   -1887, -1887,  2056,  3488, -1887, -1887
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   490,     0,     2,   490,   507,   508,   509,   510,   511,
     512,   513,   514,   515,   496,   498,   497,   499,     0,     0,
       0,   517,   519,   540,   520,   541,   523,   524,   538,   539,
     518,   536,   537,   521,   522,   525,   526,   527,   528,   529,
     530,   531,   532,   533,   534,   535,   542,   543,   850,   545,
     618,   619,   622,   624,   620,   626,     0,     0,     0,   490,
       0,     0,    17,   589,   595,     9,    10,    11,    12,    13,
      14,    15,    16,   807,   107,     0,     0,    20,     0,     2,
     105,   106,    18,    19,   868,   490,   808,   428,     0,   431,
     730,   433,   442,     0,   432,   464,   465,     0,     0,     0,
       0,   572,   492,   494,   500,   490,   502,   505,   557,   516,
     544,   474,   550,   555,   476,   567,   475,   582,   586,   592,
     571,   598,   610,   850,   615,   616,   599,   669,   434,   435,
       3,   815,   828,   495,     0,     0,   850,   890,   850,   490,
     907,   908,   909,   490,     0,  1094,  1095,     0,     1,   490,
      17,     0,   490,   453,   454,     0,   572,   500,   484,   485,
     486,   818,     0,   621,   623,   625,   627,     0,   490,   850,
     672,   851,   852,   617,   546,   723,   724,   722,   784,   779,
     769,     0,   859,   816,     0,     0,   507,   809,   813,   814,
     810,   811,   812,   490,   859,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   590,   593,   490,   490,     2,     0,
    1096,   572,   897,   915,  1100,  1093,  1091,  1098,   427,     0,
     169,   736,   168,     0,   436,     0,     0,     0,     0,     0,
       0,     0,   426,   984,   985,     0,     0,   463,   848,   850,
     848,   871,   850,   850,   473,   490,   850,   848,   928,   850,
     850,   472,   850,   947,   850,   925,     0,   565,   566,     0,
       0,   490,   490,     2,   490,   443,   493,   503,   558,     0,
     587,     0,   831,   490,     0,     0,   730,   444,   572,   551,
     568,   583,     0,   831,   490,     0,   506,   552,   559,   560,
     477,   569,   479,   480,   478,   574,   584,   588,     0,   602,
       0,   801,   490,     2,   829,   889,   891,   490,     0,   490,
       0,     0,   572,   490,   502,     2,  1104,   572,  1107,   848,
     848,     3,     0,   572,     0,     0,   456,   850,   843,   845,
     844,   846,     2,   490,     0,   805,     0,     0,   765,   767,
     766,   768,     0,     0,   761,     0,   750,     0,   759,   771,
       0,   850,   670,     2,   490,  1116,   491,   490,   481,   550,
     482,   575,   483,   582,   579,   600,   850,   601,   715,     0,
     490,   716,  1069,  1070,   490,   717,   719,   672,   589,   595,
     673,   674,   675,     0,   672,   853,     0,   782,   770,     0,
     867,   866,   862,   864,   865,   859,   863,     0,   857,   860,
      22,     0,    21,     0,     0,     0,     0,     0,     0,     0,
      24,    26,     4,     8,     5,     6,     7,     0,     0,   490,
       2,     0,   108,   109,   110,   111,    90,    25,    91,    43,
      89,   112,     0,     0,   127,   129,   133,   136,   139,   144,
     147,   149,   151,   153,   155,   157,   160,     0,    27,     0,
     596,     2,   112,   490,   161,   776,   726,   586,   728,   775,
       0,   725,   729,     0,     0,     0,     0,     0,     0,     0,
       0,   869,   895,   850,   905,   913,   917,   923,   589,     2,
       0,  1102,   490,  1105,     2,   105,   490,     3,   714,     0,
    1116,     0,   491,   550,   575,   582,     3,     3,   710,   700,
     704,   716,   717,   490,     2,   898,   916,  1092,     2,     2,
      24,     0,     2,   736,    25,     0,   734,   737,  1114,     0,
       0,   743,   732,   731,   490,     0,   833,     2,   455,   457,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   874,   931,   954,   850,     0,   469,
       2,   870,   878,  1012,   730,   872,   873,     0,   831,   490,
     927,   935,   730,   929,   930,     0,   946,   948,     0,   459,
     490,   490,   556,   491,     0,   572,     0,   490,  1097,  1101,
    1099,   573,   805,     0,   831,   848,     0,   437,   445,   504,
       0,   831,   490,   805,     0,   831,   780,   553,   554,   570,
     585,   591,   594,   589,   595,   613,   614,     0,   781,   686,
     720,   491,     0,   687,   689,   690,     0,   209,   420,   830,
       0,   418,   473,   472,   572,     0,   439,     2,   440,   802,
     461,     0,     0,     0,   490,   490,   848,   490,   805,     0,
       0,     2,     0,   764,   763,   762,   756,   501,     0,   754,
     772,   548,     0,   490,   490,  1071,   491,   487,   488,   489,
    1075,  1066,  1067,  1073,   490,     2,   106,     0,  1031,  1045,
    1116,  1027,   850,   850,  1036,  1043,   708,   490,   580,   718,
     491,   576,   577,   581,     0,   671,  1081,   491,  1086,  1078,
     490,  1083,   850,     0,   850,   672,   672,     0,     0,   490,
       0,     0,   855,   859,    69,     0,    23,   490,    97,     0,
     490,   490,    92,   490,    99,     0,    33,    37,    38,    34,
      35,    36,   490,    95,    96,   490,   490,   490,     2,   108,
     109,     0,     0,   187,     0,     0,   616,     0,  1091,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,    64,
      65,    69,     0,     0,    69,     0,    93,    94,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   490,   170,   171,   172,   173,   174,   175,   176,   177,
     178,   179,   180,   168,     0,   166,   167,   490,   996,   727,
     993,   850,   850,  1001,   597,   490,   856,   896,   850,   906,
     914,   918,   924,   490,   899,   901,   903,   490,   919,   921,
       2,     0,     2,  1103,  1106,   490,   490,     0,     2,     0,
     490,   106,  1031,   850,  1116,   966,   850,   850,  1116,   850,
     981,   850,   850,     3,   718,   490,     0,   490,     0,  1116,
    1116,   490,   490,   490,     0,     2,   745,     0,  1114,   742,
    1115,     0,   738,     0,     2,   741,   744,     0,     2,   490,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   850,   883,   887,   926,   850,   940,   944,   952,
     850,   962,   875,   932,   955,     0,     0,     0,  1008,     0,
     467,   834,     0,     0,   468,   835,   460,     0,     0,     0,
       0,   458,     0,     2,     2,   836,     0,   441,     2,   805,
       0,   831,     2,   837,     0,     0,     0,     0,   628,   892,
     490,   910,     0,     0,   490,   421,   419,   105,     3,   490,
       0,     3,   806,     2,     0,   758,   490,   490,   752,   751,
     752,   549,   547,   674,  1077,   490,  1082,   491,   490,  1068,
       0,     0,     0,     0,  1046,     0,   850,  1117,  1032,  1033,
     709,  1029,  1030,  1044,  1072,  1076,  1074,   578,   613,  1080,
    1085,   666,   672,   672,     0,     0,   681,   680,  1114,     0,
     672,   785,   783,     0,     0,   858,    73,     0,    70,    71,
      74,   817,     0,     0,     0,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   490,   490,     0,   126,
     125,     0,   122,   121,    28,     0,    29,     0,     0,     0,
     184,   183,     0,     3,    69,     0,    52,     0,    53,    62,
       0,    61,     0,    55,    56,    57,     0,    54,    60,     0,
       0,    51,   128,   130,   131,   132,   134,   135,   137,   138,
     142,   143,   140,   141,   145,   146,   148,   150,   152,   154,
     156,     0,     0,   430,     0,     0,    30,     3,   736,   162,
       0,     0,     0,     0,   997,   998,   994,   995,   778,   777,
     490,   900,   902,   904,   490,   920,   922,     0,     0,   490,
     490,  1022,  1021,   490,     0,     0,     0,     0,     0,   850,
    1032,   969,   986,   490,   964,   972,   706,   967,   968,   707,
     490,   979,   989,   982,   983,     0,     3,  1116,     3,   702,
     451,   701,   705,  1108,   711,   712,   694,     0,   695,   696,
       3,     3,   730,     0,   160,     0,     3,     3,     0,   739,
       0,   733,     0,   850,   490,     3,   462,   850,   884,   888,
     850,   941,   945,   953,   850,   963,   490,   876,   879,   881,
     490,   933,   936,   938,   490,   956,   958,   960,   848,     0,
     470,  1009,     3,  1013,  1014,     3,   839,   949,   562,   561,
     564,   563,     2,   806,   840,   787,   490,     2,   838,     0,
     806,   841,   628,   628,   628,   490,   688,   691,   692,   721,
       0,   424,     0,     0,   490,     0,     0,   345,     0,     0,
       0,     0,     0,   189,     0,   340,   341,     0,     0,   345,
       0,   393,   392,     0,   164,   164,   399,   589,   595,   206,
     490,     2,   190,     0,   217,   191,   192,   490,   211,   193,
     194,   195,   196,     0,   197,   198,   346,     0,   360,   199,
     366,   368,   371,   200,   201,   202,   203,   204,     0,   205,
     213,   572,   490,     0,   215,     0,     0,     0,     3,   490,
     819,   806,     0,   794,   795,     0,     3,   790,     3,     3,
       0,   490,   769,   769,  1079,  1084,     2,   105,   490,     3,
     587,     3,   491,  1040,   850,  1039,  1042,   490,     3,  1028,
    1034,   672,  1114,     0,   672,   677,   672,     0,   682,  1114,
       2,   854,   861,     0,    98,   101,   490,   490,     0,   104,
     100,   102,   490,     0,   116,     0,     0,     0,   120,   124,
     123,   188,     0,     0,     0,   736,   113,   181,     0,     0,
       0,    46,    47,    87,     0,    87,    87,     0,    75,    77,
      49,     0,    45,     0,    48,   159,     0,     0,     0,     0,
    1114,  1005,   850,  1004,  1007,   999,     0,     0,   893,   911,
       3,     3,     0,   850,   975,   978,   850,     0,   850,   850,
     970,   987,     0,     0,  1109,     0,   713,   490,     0,  1111,
     490,   490,     0,     0,   438,     3,     0,     0,     0,     0,
     735,   740,     3,   832,     3,     0,   490,   877,   880,   882,
     490,   934,   937,   939,   490,   957,   959,   961,     0,     0,
       0,   730,   850,  1020,  1019,     0,     0,     0,     0,     0,
       3,   806,   842,     0,   490,   490,   490,   490,   490,   490,
     490,   611,   641,     0,     0,   642,   572,   629,     0,     0,
       0,   422,    69,     0,     0,   331,   332,   214,   216,   490,
       0,     0,     0,   490,   490,   327,     0,   325,     0,     0,
       0,   736,     0,     0,   490,     0,   372,   490,     0,   165,
       0,     0,   400,     0,     0,     0,   221,     0,   212,     0,
     322,     0,     0,     0,   345,   345,   351,   350,   345,   362,
     361,   345,   345,     0,   572,   850,     0,  1024,  1023,     0,
       0,   761,   797,     2,   792,     0,   793,     0,   773,   753,
     757,   755,   490,     0,     0,     0,     3,   490,  1035,  1037,
    1038,     0,     0,   105,     0,     3,     0,     0,   672,  1114,
       0,     0,   661,     0,   676,     0,   786,   490,    72,  1025,
       0,     0,     0,    39,     0,   117,   119,   118,   115,   114,
     736,  1114,   186,   185,     0,    68,    84,     0,    78,    85,
      86,    63,     0,     0,     0,    59,     0,   158,   429,    31,
       0,   490,  1000,  1002,  1003,   894,   912,     0,     0,   850,
     490,   971,   973,   974,   490,   988,   990,     0,   965,   980,
     976,   991,  1110,   703,   452,   698,   697,   699,  1113,  1112,
       0,     3,   847,   746,   747,     0,     0,   850,     0,     0,
       0,   885,   942,   950,   471,   849,     0,  1015,     0,  1016,
    1017,  1011,   823,     2,     0,   825,   611,   611,   611,   642,
     650,   616,     0,   656,   642,     0,   490,   603,   640,   636,
       0,     0,     0,     0,   643,   644,   646,   850,   658,   658,
     658,     0,   637,   654,   425,     0,   335,   336,   333,   334,
     230,     0,     0,   232,   433,   231,   572,   490,     0,     0,
     345,     0,   313,   312,   314,     0,   345,   189,   270,     0,
     263,     0,   189,   328,   326,     0,   320,  1114,   329,     0,
       0,     0,     0,   381,   382,   383,   384,     0,   374,     0,
     375,   337,     0,   338,     0,     0,   365,   210,   324,   323,
       0,     0,   354,   364,     0,   345,   367,     0,   369,   391,
       0,   423,   850,   821,   752,   774,   490,     2,     2,  1087,
    1088,  1089,     0,     0,     3,     3,     0,  1048,     0,   672,
     662,  1114,     0,   682,   682,   736,   683,   665,     3,     0,
    1026,   103,     0,    32,   490,     0,  1114,     0,     0,    88,
       0,    76,     0,    82,     0,    80,    44,   163,     0,   850,
     850,     0,     0,   749,     0,   446,   450,   886,   943,   951,
       0,     0,   789,   827,   607,   609,   605,     0,     0,  1055,
       0,   651,  1060,   653,  1052,   850,   850,   635,   657,   639,
       0,   638,     0,     0,     0,   660,     0,   631,   630,   850,
     647,   659,   648,   649,   655,   345,   345,   233,   572,     0,
       0,   251,   345,   318,   316,   319,   315,     0,   317,     0,
     259,     0,   189,     0,   345,   490,   271,     0,   296,     0,
       0,   321,     0,     0,   345,   344,   345,   385,     0,   376,
       2,     0,     0,     0,   347,     0,     0,   345,     0,   345,
     345,   345,   208,   207,   449,   769,   791,     0,     0,  1090,
    1041,     0,     0,  1047,  1049,  1114,     0,   664,   679,   678,
    1114,     2,    50,    42,    40,    41,     0,    66,   182,    79,
       0,     0,  1006,   448,   447,   977,   992,   748,  1010,  1018,
     633,     0,     0,     0,  1056,  1057,   850,   634,  1053,  1054,
     632,   612,     0,     0,   343,   222,     0,     0,     0,   244,
     345,   224,     0,     0,   345,   253,   268,   279,   273,   345,
     189,     0,   310,     0,   283,     0,   308,     0,   274,   272,
     261,   264,     0,     0,   189,   297,     0,     0,   227,   342,
     373,     2,   490,   339,     0,     0,   401,   352,     0,    69,
     363,   356,     0,   357,   355,   370,   760,   796,   798,  1050,
    1051,     0,   668,     0,   788,    67,    83,    81,   850,  1063,
    1065,  1058,     0,   645,   345,   239,   234,   237,     0,   236,
     243,   242,     0,   490,   246,   245,   345,   255,     0,   252,
     345,     0,     0,     0,   260,   265,   311,     0,     0,   189,
     284,   309,     0,     0,   345,     0,   299,   300,   298,   267,
     330,     0,   490,   490,     3,   386,   491,   390,     0,   394,
       0,     0,     0,   402,   403,   348,     0,     0,     0,   667,
     684,   490,  1059,  1061,  1062,   652,   223,     0,   241,     0,
     240,   226,   247,   490,   414,   256,   345,   257,   254,   269,
     282,   280,   276,   288,   286,   287,   285,   266,   281,   277,
     278,   275,   262,     0,     0,     0,     0,   229,   247,     3,
     379,     0,  1055,   387,   388,   389,   401,     0,     0,     0,
     401,     0,   353,   349,   345,     0,     0,   235,   238,   345,
       3,   248,   415,   258,     0,     0,     0,     0,   307,   305,
     302,   306,   303,   304,   301,     3,   379,     0,     0,  1056,
       0,     0,     0,   395,     0,   404,   358,   345,  1064,   218,
       0,     0,   345,   295,   293,   290,   294,   291,   292,   289,
       0,     0,   380,     0,   407,     0,   405,     0,   407,   359,
     220,   219,   225,     0,   228,     0,   377,   408,     0,     0,
     396,     0,   378,     0,     0,     0,     0,   409,   410,     0,
     406,   397,     0,     0,   398,   411
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1887,  6552,  2779, -1887,    -1,   389,  1900,     1, -1887,  -380,
   -1887,   454, -1887,  -723, -1887,   882,  -998, -1106, -1887,   282,
    5841,  2105, -1887,  -311, -1887,  1509,   113,   884,   885,   739,
     881,  1460,  1468,  1465,  1467,  1469, -1887,  -162,  -156,  9046,
    1009, -1887,  1794, -1887, -1887, -1303,  6585, -1149,  1857, -1887,
    1168, -1887,  1004,    97, -1887, -1887,   777,   183, -1887, -1886,
   -1697,   395,   164, -1887, -1887,   787,   408,   307, -1637, -1887,
   -1437, -1887, -1887, -1887, -1887,   207, -1129, -1887, -1887, -1239,
     533, -1887, -1887, -1887, -1887, -1887,   143, -1204, -1887, -1887,
   -1887, -1887, -1887,   135,   554,   557,   236, -1887, -1887, -1887,
   -1887,  -690, -1887,   180,   110, -1887,   250, -1887,  -186, -1887,
   -1887, -1887,  1018,  -887, -1006,   -46, -1887,    -3,    77,   833,
    4619, -1003,  -974, -1887,  -117, -1887, -1887,    78, -1887,  -129,
    1042,  -328,  -253,  3165,   296,  -705,   217,   484,   521,   999,
    2820, -1887, -1887,  2245, -1887,   284,  5297, -1887,  2181, -1887,
      13, -1887, -1887,   513,   683,  5863,  4434,   -54,  2030,  -343,
   -1887, -1887, -1887, -1887, -1887,  -639,  8432,  8203, -1887,  -399,
     131, -1887,  -878, -1887,   363, -1887,   301,   854, -1887,    -7,
    -180, -1887, -1887, -1887, -1887,   -94,  8808,  -957,   989,   -87,
    1034, -1887,  -176,  -113,  -170,  1860,  2284,  -756,  -136,  1041,
    4877,  -375,  -488,  -260,  -205,  -479,  1448, -1887,  1793,   -98,
    -935,  1662, -1887, -1887,   793, -1887, -1242,  -173,  -182,  -522,
   -1887,   452, -1887, -1887, -1147,   571, -1887, -1887, -1887,  2315,
    -724,  -392,  -813,     2, -1887, -1887, -1887, -1887, -1887, -1887,
     254,  -796,  -213, -1869,  -131,  7545,   -64,  3974,  -126,  1618,
   -1887,  2454,   -89,  -200,  -195,  -183,    29,   -71,   -70,   -65,
     715,   138,   167,   173,  -150,   -29,  -145,  -140,  -127,    59,
    -116,  -112,  -103,  -720,  -725,  -691,  -686,  -715,  -160,  -677,
   -1887, -1887,  -695,  1521,  1522,  1526,  3169, -1887,   686,  6841,
   -1887,  -623,  -558,  -551,  -549,  -740, -1887, -1649, -1745, -1742,
   -1727,  -656,  -115,  -194, -1887, -1887,   -81,   491,  -105, -1887,
    7451,  2475,  -801,  -530
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   848,   426,   427,    83,    84,   428,   402,   429,
    1562,  1563,   430,   997,   998,   999,  1357,  1358,  1359,  1576,
     452,   432,   433,   434,   731,   732,   435,   436,   437,   438,
     439,   440,   441,   442,   443,   444,   445,   454,  1145,   733,
    1490,   794,   223,   796,   448,  1032,  1242,  1243,  1244,  1245,
    1246,  1247,  1248,  2150,  1249,  1250,  1681,  2007,  2008,  1939,
    1940,  1941,  2120,  2121,  1251,  1699,  1700,  1956,  1701,  1848,
    1849,  1252,  1253,  1254,  1255,  1256,  1257,  1875,  1879,  1512,
    1504,  1258,  1259,  1511,  1505,  1260,  1261,  1262,  1263,  1264,
    1265,  1266,  1718,  2138,  1719,  1720,  2044,  1267,  1268,  1269,
    1493,  2052,  2053,  2054,  2178,  2189,  2072,  2073,   308,   309,
     934,   935,  1210,    86,    87,    88,    89,    90,  1684,   488,
      93,    94,    95,    96,    97,   237,   238,   311,   290,   490,
     456,   491,   100,   323,   102,   103,   157,   357,   314,   107,
     108,   109,   173,   110,   952,   358,   158,   113,   261,   114,
     159,   269,   360,   361,   362,   160,   449,   119,   120,   364,
     121,   607,   927,   925,   926,  1657,   122,   123,   124,   125,
    1205,  1457,  1663,  1664,  1665,  1810,  1811,  1458,  1652,  1830,
    1666,   126,   695,  1311,   169,   987,   127,   988,   989,  1554,
     960,   613,  1137,  1138,  1139,   614,   368,   499,   500,   616,
    1273,   458,   459,   224,   517,   518,   519,   520,   521,   345,
    1292,   346,   950,   948,   646,   347,   387,   348,   349,   460,
     128,   179,   180,   129,  1286,  1287,  1288,  1289,     2,  1192,
    1193,   637,  1280,   130,   335,   336,   271,   282,   590,   131,
     227,   132,   326,  1147,   917,   551,   171,   133,   397,   398,
     399,   134,   328,   241,   242,   243,   329,   136,   137,   138,
     139,   140,   141,   142,   246,   330,   248,   249,   250,   331,
     252,   253,   254,   834,   835,   836,   837,   838,   255,   840,
     841,   842,   799,   800,   801,   802,   552,  1185,  1436,   143,
    1769,   670,   671,   672,   673,   674,   675,  1813,  1814,  1815,
    1816,   660,   501,   372,   373,   374,   461,   215,   145,   146,
     147,   376,   861,   676
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   153,   867,    82,   214,   198,   199,   388,   572,   240,
     384,   200,   196,   678,   115,  1293,   587,   529,   523,  1508,
     736,   975,   310,   446,   205,  1312,   655,  1023,  1042,   447,
     135,  1050,   369,  1319,   858,   533,  1495,   497,   318,   355,
     534,   742,   686,   569,   737,   969,   689,  1270,  1361,  1440,
    1530,  1531,   535,   371,  2015,    82,    82,  1148,    82,   558,
    1586,   183,  -799,  1921,  1724,  1860,  1922,  2009,   463,   247,
    1722,   214,   115,   148,  1482,    82,   383,  1368,    91,    98,
    2010,  1923,   908,   910,    82,   536,  2016,   584,   135,  1129,
     537,  1131,    82,  1115,   496,   538,   712,    82,   595,  1122,
      82,   483,   276,  1085,    82,  1105,   961,   549,   539,   554,
    1025,  1494,  1111,   962,   212,   963,   562,  1112,   280,   540,
    2023,   756,   757,   541,   533,   465,   466,   244,   310,   534,
     272,   467,   542,  2076,   283,  1725,    91,    98,  1274,  1106,
     970,   535,   756,    82,  1107,  1405,    82,   544,    82,   678,
    1723,   321,   115,  1108,    82,  -800,   310,   251,   655,  1506,
    1925,    82,   115,   225,   198,   199,   650,   310,    58,    82,
     200,   531,   627,   161,   536,   756,   688,  1228,   135,   537,
     691,   396,  1507,   579,   538,   401,    58,  1317,   626,   628,
     914,  2017,   620,   396,    82,    82,   667,   539,  1200,  1452,
    2009,   922,  1453,   548,  1726,    82,   650,   545,   540,   969,
      82,   992,   541,  1406,   201,  1957,    58,   694,   104,  1281,
     514,   542,   240,  1007,   212,    82,    91,    98,    82,    82,
      82,  1454,    75,  2015,    82,    82,   544,  -831,  2077,   661,
     505,  2011,  1735,   202,   602,   985,   943,  1407,  1578,   203,
      75,   168,   579,   198,   199,    82,   162,   627,   115,   200,
     631,  1853,   776,    82,  1506,   212,  2015,  1583,   678,   701,
     400,  1778,   961,    82,    82,  2001,   104,    82,  2067,   962,
      75,   963,   247,   693,    82,   111,   115,  1507,   698,  2068,
     697,   700,  1509,   212,   900,   546,   545,   115,    82,    82,
    1584,    82,   904,   550,  1116,   777,    82,   591,  1119,  2050,
      82,  1350,  1407,   678,   862,  1510,  1340,  1089,  1025,  1134,
    1135,   167,   115,    82,    82,   954,   280,  1467,  2033,  1004,
     839,   872,  1002,    82,   468,  1006,   873,   678,  1008,  1550,
     622,    82,    82,   111,   678,  1310,    82,  1011,   874,   974,
    1013,  1014,  1015,  1048,   212,   906,  1921,  1549,   190,  1922,
    1270,   911,   979,   469,  1115,   302,   104,   471,    20,   470,
     623,   661,   214,   201,  1923,  2119,    82,   824,  1105,  1734,
    1439,   875,  1494,  1737,   546,    82,   876,  1443,    82,  1375,
    1390,   877,  -984,   151,   396,  1391,  2104,   266,   483,  -984,
     986,  2119,   202,   220,   878,   380,   921,   497,   203,   288,
    1768,   295,  1106,   297,   221,   879,   298,  1107,  1468,   880,
    1962,  1963,  1433,   111,  2152,  1290,  1382,   759,   881,   182,
     222,   872,  1827,   111,   760,   761,   873,  1553,  1452,  1452,
    1452,  1453,  1453,  1453,  1434,   177,   177,  1052,   874,  2069,
    2070,  1274,   266,  1925,   892,   295,   297,   205,  1828,   956,
     465,   466,  -663,    82,   496,   941,   467,  1364,  1520,  -663,
    1454,  1454,  1454,  1441,  1323,   310,  1781,  1783,  1785,  1463,
    1464,   875,   177, -1115,   497,   105,   876,  1310,    82,    82,
    2001,   877,   144,   505,   986,   144,   980,   954,   557,   380,
      82,    82,  1936,  1937,   878,   565,   355,   266,  1936,  1937,
      82,  1547,   514,  1460,   893,   879,  2027,  2028,  1555,   880,
     371,   229,   106,  1877,   204,    64,   583,  1197,   881,   111,
      82,  1495,  1461,   177,  2114,   225,   177,   594,   303,  1502,
    1658,   496,    82,   105,   892,   961,   230,   184,  1851,  1228,
     144,   177,   962,  1859,   963,  1676,   193,   111,  1878,   381,
     465,   466,   661,  -724,  1539,  1659,   467,    82,   111,  1590,
    1671,    58,   505,    82,   266,    82,   295,   297,  1344,  1780,
     106,   303,  1682,  1575,    58,  1345,  1682,  1702,  1938,  1672,
    1323,  1761,   185,   111,  1967,   480,   144,  1396,  1465,  1370,
    1702,   984,   894,   206,   893,   115,  1494,   226,   266,  -831,
    1460,   497,   193,   266,   678,   177,  1025,   193,   528,   266,
     530,   194,   267,   313,   218,  -608,  2094,  1294,  1644,  1741,
      82,   956,    82,   105,  1818,    75,    82,    -3,    82,   942,
     144,  1613,   975,  1986,  1129,  1131,  1606,    82,    75,   229,
     115,    82,   266,  1819,   302,  1149,   497,   683,   550,   297,
      63,    64,   505,   177,   177,   230,   135,   559,   496,   468,
     106,   550,   839,   303,   177,  1128,  1140,  1141,  1671,   643,
    1025,   446,  1178,    82,   116,   231,    58,  1126,   605,   177,
    1678,   610,   894,  1155,    58,  1144,   151,  1821,   469,  1861,
      58,  1334,   396,  1955,   470,  1881,   719,  1338,   644,   645,
      78,  1132,   743,   496,    91,    98,  1926,   744,  1346,  1025,
     494,   163,  1503,   177,   164,   165,   602,   166,   745,   313,
     232,   177,   177,   746,    58,  1927,   177,   596,  2021,  1675,
      58,   193,   116,    82,  2058,    82,   256,    82,  1762,  -985,
      75,    82,   608,   275,    82,  1025,  -985,   313,    75,  -484,
     266,  1295,  1025,  1278,    75,  2096,   692,   298,   313,   634,
    1777,  1807,  2125,   550,   300,   177,  1820,   813,   177,    82,
    1074,   550,   302,  1090,  1910,  1025,  1911,   550,   266,  1854,
     683,   297,    58,   313,  1855,  2127,  1018,   303,    75,   589,
     213,  2025,  1895,  1529,    75,  1866,   719,  1019,  1020,  1885,
    1855,   386,   902,   245,   322,  2039,   273,  1113,  1462,    74,
     284,   665,   116,  1120,    82,    74,   302,   665,   471,    82,
     550,    82,   116,   267,    92,    58,  -820,   154,   916,   266,
     797,   736,   343,    82,   550,   920,   664,   941,   389,   924,
     665,    80,    81,    82,   104,  1975,    75,    80,   666,   514,
    1976,    58,    82,  2109,   266,   737,  1571,   658,  2110,   266,
     681,   266,  1053,  1054,  1055,  1166,   111,   267,   177,   550,
    2087,  2167,  1404,   658,   601,    64,  2168,   658,   355,  1313,
     177,   177,    92,   266,    82,   266,   266,  1179,   400,    75,
     863,   864,   371,   712,   865,   472,  1862,   266,   647,  1365,
     213,   483,   648,    14,    15,    16,    17,    18,  1170,   473,
     266,   111,   550,  1380,  1381,    75,   506,    58,   116,   266,
     474,    82,    82,   514,   265,   559,  1770,   885,   277,   550,
      58,   475,   702,  1283,  1174,  1411,   703,   115,   550,  1389,
     839,   213,   266,   476,   683,   297,   116,  1564,   477,   678,
    1896,   504,  1710,   257,   258,   480,   259,   116,  1702,    58,
     974,   260,    58,   651,   298,  1906,   266,   683,  1414,   213,
     762,   763,    92,   266,  -485,   390,   503,   756,    82,   508,
    1144,    75,   116,   592,    14,    15,    16,    17,    18,   658,
     509,   634,  1707,   471,    75,   550,  -486,  1804,  1805,  1806,
    1416,    91,    98,   267,   550,   522,    14,    15,    16,    17,
      18,   764,   765,  1420,  1565,  1566,  1567,   550,   177,    58,
     524,  1568,  1569,    75,    74,   527,    75,   177,   547,    82,
     778,   953,    58,    99,   779,    82,   155,  1431,   548,   932,
    1651,   303,  1424,    58,  1541,   664,   550,   570,   804,   665,
     391,   191,   805,   571,   806,   817,    80,   666,   703,   550,
     621,    74,  1522,   978,    82,    58,   313,   514,   392,   576,
     393,   394,    65,    66,    67,    68,    69,    70,    71,    72,
     494,  1776,  1808,    75,  1991,    58,   550,    58,   471,  1993,
     550,    99,    82,    80,    81,   286,    75,   582,    82,    82,
     287,   588,  1537,   291,   593,   296,   665,    75,   931,   388,
     388,   105,   932,    58,  1459,  1591,  2074,   210,   144,   550,
     770,   771,   267,  1036,  1822,  1038,   991,  1041,   958,    75,
     648,  1047,    82,  1132,  1051,   589,   503,   739,   494,   993,
     355,   104,  1113,   648,   471,  2074,   665,   941,   106,    75,
     225,    75,   766,   767,   371,   638,   658,   494,   994,  1076,
     163,  1634,   703,   164,   165,  1199,   166,   639,  1600,   506,
    1604,    99,   550,   661,   665,   316,  2122,    75,  1024,   617,
     658,    99,  1025,  1283,   210,  1094,  1153,  1559,   355,   550,
     805,   589,   685,   658,  1587,   302,  2061,   768,   769,   550,
     550,   559,   371,   653,   480,   550,   514,  1546,   111,    82,
      82,    82,  1713,  1714,  1715,  1716,  1717,   115,   514,   696,
    1616,  1188,  1190,   177,   380,  1025,  1025,    14,    15,    16,
      17,    18,   699,   177,   446,   446,   514,   704,   481,   706,
    1621,  1622,    82,   266,   705,   634,  2056,    74,   506,   550,
     115,  1132,   825,   739,   266,  1132,   286,    82,  1322,   276,
      82,    82,  1323,   266,    82,   116,  1360,   739,   664,  1528,
    1323,    82,   665,   805,    82,   280,  1900,    99,   709,    80,
     666,    91,    98,   710,   494,   711,    58,  1843,  1844,  1845,
     272,   283,   667,   304,   574,   747,   578,   748,   749,   750,
     958,   715,  1773,   286,  1835,    99,  1774,   758,  1323,  1846,
     116,  1209,   177,   177,    91,    98,    99,    82,    14,    15,
      16,    17,    18,  1444,  1445,  1446,   612,   751,   658,   494,
     752,   753,   772,  1839,   514,   754,   755,  1025,   773,   155,
    1863,    99,  1685,    82,  1025,   774,  1685,   287,   780,   682,
      75,   296,   941,  1459,  1459,  1459,  1668,   807,  1653,  1459,
      14,    15,    16,    17,    18,   578,   775,  1864,   177,   808,
     266,   805,  1843,  1844,  1845,  1865,    82,    58,  1931,  1025,
     820,   446,   805,   809,  1025,   526,  1980,  1766,  2018,  1564,
    1025,  2112,  1025,   355,  1846,  1323,   266,  2113,  2186,   720,
    2192,  1025,  2183,  1852,  2193,   810,  2140,   371,  1272,  1874,
    2144,  1016,   739,  1683,   811,   144,  1669,  1683,   812,    58,
     822,   104,    14,    15,    16,    17,    18,   144,    14,    15,
      16,    17,    18,   479,   153,  -126,  -126,  -126,  -126,  -126,
    -126,    75,  -488,    -3,    82,   106,  1027,  1028,    82,    82,
     533,  1843,  1844,  1845,   104,   534,   653,   739,   658,   843,
      92,   681,  -487,  1025,  1347,  1348,   619,   535,   267,   845,
     514,   847,   115,  1846,  1362,  1363,   115,   115,  1831,  1831,
    1831,    58,  -190,    75,  -489,   210,   -18,    58,   111,   859,
     115,  1025,  1366,   514,   514,  1060,  1061,  1062,  1063,   720,
     536,  -161,  -161,    82,  1670,   537,   206,   739,  1502,  1503,
     538,  1581,  1582,   860,   823,  1843,  1844,  1845,  1585,  1582,
     494,   111,   868,   539,   186,     6,     7,     8,     9,    10,
      11,    12,    13,   591,   540,   481,   870,  1846,   541,   882,
      82,  1589,  1582,    74,  1283,    75,  1847,   542,   883,    74,
    1668,    75,  1102,  1574,   884,  1668,   886,   266,   887,   514,
    1623,  1574,   888,   544,   797,   889,    82,   890,   550,   891,
    1808,    82,    82,    82,   550,    80,    81,   735,   286,  1102,
    1636,    80,    81,   903,   898,   285,  1823,   896,   266,  1786,
    1348,  1908,  1348,   897,   266,   315,   177,  1909,  1582,   177,
     177,   177,  1918,  1025,  1978,  1979,   929,   116,   177,   918,
    1669,  1996,  1582,   872,   930,  1669,  1997,  1582,   873,  1936,
    1937,  2183,  2184,   545,    99,   919,   177,  1579,  1580,  -606,
     874,  -604,   177,   928,  2045,   933,  1056,  1057,    82,  1058,
    1059,  1064,  1065,    82,  1736,  1738,   945,   610,   947,    82,
     951,    82,  1832,  1833,   177,   936,   964,  2055,   612,   966,
      82,   177,   667,   875,   982,  1988,  1898,  1899,   876,    99,
     990,  1001,  1026,   877,   276,  1029,   104,   144,  1073,   514,
     104,   104,  1034,   256,  1078,   514,   878,  1101,  1272,  1102,
     280,  1109,  1130,   678,   104,   144,  1133,   879,  1670,  1151,
    1157,   880,   388,  1670,  1158,  2045,  1159,  1560,  1160,  1161,
     881,   546,  1162,  1163,  1164,   901,  1873,  1165,  1180,  1187,
    1189,  1272,  -803,   905,  1191,   106,   825,   892,   144,   514,
    1195,  1275,  1202,  1283,  1203,    19,  1204,  1282,  1279,  1303,
    1291,   915,   266,   111,  1304,  1305,  1306,   111,   111,  1314,
    1321,  1316,   923,   144,   514,  1320,  1324,    92,   106,  1325,
    1016,   111,  1326,  1329,   446,    58,  1328,  1330,  1331,  1284,
    2006,    82,  1333,    82,    48,    49,    50,    51,    52,    53,
      54,    55,   267,  1332,  1335,  1336,   150,   893,   233,   234,
      65,    66,    67,    68,    69,    70,    71,    72,  1351,   150,
     266,   658,  1337,    65,    66,    67,    68,    69,    70,    71,
      72,  1342,    82,   589,    74,    82,  1343,  1352,  1394,  1367,
    1371,  1080,  2103,  1372,   514,   514,  1373,    74,  1374,    75,
     494,   514,  1378,  1379,  1668,  1660,    77,  1097,   177,   177,
    1383,  1098,  1661,   514,  1384,  1385,    80,    81,    76,    77,
    1397,  1386,  1399,   514,  1432,   514,  1410,  1492,   115,    80,
      81,  1400,  1401,  1974,  1437,   735,   514,  -804,   514,   514,
     514,   735,  -723,  1469,  1470,   894,    82,    82,  1473,  1499,
     735,  2047,  1474,   177,   177,  1483,  1484,   116,  1485,  1487,
    1025,   533,  1496,  1552,  1669,   446,   534,   446,  1515,   735,
    1517,  2117,  1518,  2006,  1524,  1526,  1548,   150,   535,   175,
     176,    65,    66,    67,    68,    69,    70,    71,    72,  1556,
     116,  1570,    82,  1553,  1574,  1588,  1595,  1596,   144,   514,
     177,  1599,  1610,   514,  1611,  1618,   446,  1582,   514,  1612,
    1614,   536,  2142,  1687,  1619,  1624,   537,  1687,  1687,  1627,
     144,   538,  2047,  1631,   144,   144,  1632,  1633,  1641,  1642,
    1645,  1687,  1656,  1462,   539,  2162,    99,  2051,   144,  1689,
    1703,  1704,   266,  1706,  1708,   540,   273,   284,  1285,   541,
     106,  1500,  1670,  1721,   106,   106,  1503,  1727,   542,  1728,
    1729,  1730,  1731,   514,  1228,  1742,  1743,  1759,   106,  1745,
    1747,  1748,  1779,   544,  1749,   514,  1750,   446,  1751,   514,
    1760,  1757,  1767,  2185,  1771,  1772,   115,  1775,  1787,  1284,
     198,   199,  1789,   514,  1790,   589,   200,   631,  -125,  -125,
    -125,  -125,  -125,  -125,    82,   494,    82,    92,   144,   471,
     892,  1793,  1795,  1623,  1797,   115,  1817,  1802,   481,  1798,
     177,    14,    15,    16,    17,    18,  1022,   498,  1799,  1803,
    1186,  1829,   104,   545,  1661,   514,  1836,   226,  1840,   177,
      92,  1842,  1870,  1872,  1194,   177,   115,  1890,  1198,  1889,
    1897,  1893,  1201,    14,    15,    16,    17,    18,  1339,  1894,
      82,    82,  1902,  1907,   265,   277,    85,  2051,  1912,   152,
     893,  2051,  2051,   514,  1915,  1916,  1917,  1920,   514,  1944,
    1930,   515,   266,  1950,  1376,  1949,  1964,  1966,  1377,   177,
    1971,   212,  1973,  1977,   266,  1982,  1992,  1989,    82,   111,
    1994,  1990,  2165,  1995,  1998,  1999,   514,  1392,  2000,   514,
    2004,   514,   116,   550,  1393,  2022,   116,   116,  2020,  2026,
    -589,   546,  2034,  2029,    85,  2177,  2032,  2048,   505,  2177,
     116,  2059,   514,  2040,  2060,  2071,  2049,  2093,   144,  2080,
    2095,   195,  2097,    82,  2187,  2107,  2106,  2108,  2111,  2124,
      85,  2128,    82,  2126,  2137,  2143,  2141,  2147,   894,  2148,
    1428,  2153,  2164,   236,  1429,  1558,   264,   267,  1430,  2163,
      85,   186,     6,     7,     8,     9,    10,    11,    12,    13,
     266,  2166,  2172,  2174,   177,   177,  2179,  2175,  1904,   592,
     104,   177,  1066,  2180,  2190,  2191,  2194,   144,  1285,  1068,
    1021,  1067,  1069,   177,  1491,  1070,   795,  1455,   152,  2173,
    1691,  1498,  2118,   177,    85,   177,    99,   152,  1968,   104,
     325,   333,  2135,  1961,  2031,  2115,   177,  1880,   177,   177,
     177,  2161,  1868,   354,  1712,  1869,   177,   150,  2181,  2099,
     154,    65,    66,    67,    68,    69,    70,    71,    72,    99,
     104,  2145,  2098,  1516,   174,   293,  2003,   111,   453,   581,
     195,   195,    92,  2065,  1655,  1551,    92,    92,  1150,  1513,
     949,   152,   486,   866,  1744,     3,   264,  1886,  1081,  1082,
      92,   995,     0,  1083,  1801,     0,   111,    77,     0,   177,
     855,     0,     0,   177,     0,   325,     0,     0,   177,  1687,
     236,   236,   266,     0,   150,     0,   144,   588,    65,    66,
      67,    68,    69,    70,    71,    72,  1353,   111,     0,   589,
    1354,   325,  1355,     0,     0,     0,     0,  1442,     0,    85,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,  1466,     0,   264,     0,     0,     0,     0,     0,     0,
    1284,     0,     0,   177,    77,     0,     0,  1577,     0,     0,
       0,  1488,     0,     0,     0,   177,     0,     0,   735,   177,
       0,   856,     0,   515,     0,     0,   325,     0,     0,     0,
       0,     0,   333,   177,     0,     0,     0,     0,   333,   325,
     325,     0,     0,     0,     0,   266,  2105,   498,   152,     0,
       0,     0,     0,     0,     0,     0,     0,   390,     0,     0,
    1628,     0,     0,     0,  1629,     0,     0,     0,  1630,   354,
     668,   677,   150,     0,     0,   177,    65,    66,    67,    68,
      69,    70,    71,    72,  1353,   354,     0,     0,  1354,   354,
    1355,     0,     0,     0,     0,   658,  1455,  1455,  1455,   155,
    1649,  1650,  1654,     0,     0,     0,     0,  1687,     0,     0,
       0,     0,     0,   177,   144,     0,     0,     0,   177,     0,
       0,    99,    77,     0,   498,    99,    99,     0,     0,   265,
     277,     0,   391,     0,   453,     0,  1687,     0,     0,    99,
     197,     0,     0,   144,   106,     0,   177,     0,   116,   177,
     392,   177,   393,   394,    65,    66,    67,    68,    69,    70,
      71,    72,   239,     0,     0,     0,   658,  1687,   453,    19,
     217,   798,   177,   106,   144,     0,     0,     0,     0,   195,
       0,  1753,     0,  2188,     0,     0,     0,     0,    58,  1284,
       0,     0,  2195,     0,     0,     0,   615,   152,     0,   395,
       0,   486,     0,     0,   106,   832,     0,   677,     0,  1285,
       0,     0,    52,    53,    54,    55,     0,     0,   152,   327,
       0,     0,   150,     0,   233,   234,    65,    66,    67,    68,
      69,    70,    71,    72,     0,  1788,     0,   217,     0,   453,
       0,  1677,  1679,     0,  1791,     0,   236,     0,  1792,     0,
       0,   498,    75,     0,     0,     0,     0,   150,   236,  1043,
    1044,    65,    66,    67,    68,    69,    70,    71,    72,  1045,
       0,  1387,    77,     0,     0,     0,     0,     0,   462,     0,
       0,   588,     0,   325,     0,   453,   453,     0,     0,   325,
       0,  1739,   354,     0,   327,     0,   498,     0,    92,   532,
     239,     0,     0,     0,     0,     0,   116,     0,     0,     0,
    1046,     0,     0,     0,     0,   498,     0,   498,     0,     0,
     327,   498,   498,   498,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,     0,     0,     0,   498,
       0,     0,     0,     0,     0,     0,   325,     0,   325,   580,
     354,     0,    85,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,   116,     0,   354,   486,
     515,   677,     0,   856,     0,   327,     0,     0,     0,   668,
       0,     0,     0,   668,     0,     0,     0,     0,   632,   327,
       0,     0,   354,     0,     0,     0,     0,     0,  1285,     0,
    1207,     0,   677,     0,     0,   354,     0,     0,     0,   498,
       0,     0,     0,     0,   152,     0,     0,     0,   580,     0,
      58,     0,   453,     0,     0,   152,   152,     0,   453,     0,
       0,     0,     0,     0,     0,     0,     0,   453,     0,   663,
     152,   152,   152,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   615,   150,     0,    92,     0,    65,    66,
      67,    68,    69,    70,    71,    72,   150,     0,   378,   379,
      65,    66,    67,    68,    69,    70,    71,    72,   219,     0,
       0,     0,    74,     0,    75,    92,     0,     0,     0,     0,
       0,     0,   192,     0,     0,  1867,   486,     0,     0,     0,
       0,     0,     0,    76,    77,     0,     0,    99,     0,     0,
       0,     0,   798,   798,    80,    81,    92,     0,    78,   301,
     453,     0,     0,     0,     0,     0,     0,     0,   615,     0,
     380,   268,     0,     0,     0,     0,     0,     0,   217,     0,
     354,   486,     0,   289,   292,   832,     0,   832,     0,     0,
    1356,     0,     0,     0,   833,     0,  1356,     0,   615,     0,
     354,     0,   354,     0,     0,     0,   354,   354,   354,   498,
     498,   663,   150,     0,   175,   176,    65,    66,    67,    68,
      69,    70,    71,    72,   354,  1356,   268,     0,   515,     0,
       0,     0,     0,     0,   150,   871,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   239,     0,   462,
     325,   150,     0,   233,   234,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   498,     0,     0,     0,     0,     0,
       0,     0,   327,     0,     0,     0,     0,     0,   327,     0,
       0,   268,     0,     0,     0,     0,  1475,     0,     0,   453,
       0,     0,     0,  1356,   354,   462,   462,     0,     0,     0,
       0,   152,   453,     0,     0,    99,     0,     0,  1477,     0,
     354,     0,  1298,     0,     0,   380,     0,     0,     0,     0,
       0,     0,     0,   668,  1472,     0,     0,     0,     0,     0,
       0,   615,     0,     0,    99,   940,  1486,   327,     0,     0,
       0,     0,     0,     0,     0,  2116,     0,   615,   268,   150,
     629,   615,     0,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,   615,    99,     0,    58,     0,   150,
       0,   152,   486,    65,    66,    67,    68,    69,    70,    71,
      72,  1039,   268,     0,     0,     0,     0,   268,     0,     0,
       0,     0,     0,   268,     0,     0,     0,     0,  1387,    77,
       0,   150,   976,   233,   234,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,   101,     0,     0,   156,
       0,     0,  1040,     0,   462,     0,   268,     0,     0,    74,
       0,    75,  1003,     0,     0,     0,   462,     0,  1009,   798,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
     235,    77,     0,     0,   354,   354,     0,     0,   832,     0,
       0,    80,    81,     0,  1208,   832,     0,     0,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,     0,
     721,   150,     0,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   515,     0,     0,     0,     0,
     211,     0,     0,    58,  1356,     0,     0,   498,     0,   354,
     498,   498,     0,     0,   827,     0,   829,     0,     0,     0,
     278,     0,     0,     0,     0,   846,     0,     0,     0,     0,
     462,     0,     0,     0,  1104,     0,   833,   150,   641,   233,
     234,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,   152,     0,     0,   312,     0,     0,     0,   317,     0,
     152,     0,   268,     0,   101,    74,   150,    75,     0,   453,
      65,    66,    67,    68,    69,    70,    71,    72,  1353,     0,
     721,  2176,  1354,   356,  1355,     0,   830,    77,     0,     0,
     665,     0,     0,  2182,     0,   453,     0,    80,   831,   327,
       0,     0,   453,     0,     0,     0,     0,     0,     0,     0,
     464,  1732,  1733,     0,     0,     0,    77,     0,     0,  1782,
       0,   317,   492,     0,   615,     0,   264,    85,   615,     0,
       0,   515,     0,     0,   354,     0,     0,   615,     0,     0,
     325,     0,     0,     0,     0,   268,   152,   615,     0,     0,
       0,   543,     0,   486,   615,     0,     0,     0,     0,     0,
     312,     0,     0,     0,     0,   561,     0,   268,     0,     0,
       0,   568,   462,     0,     0,     0,   573,   575,     0,   211,
       0,   268,   486,     0,     0,     0,     0,   152,   312,     0,
       0,     0,     0,     0,   268,     0,     0,     0,     0,   312,
     615,     0,   597,     0,   615,     0,   599,     0,   615,     0,
       0,   600,     0,     0,     0,     0,     0,   611,     0,     0,
     515,     0,   575,     0,   312,     0,   268,  1356,   624,     0,
       0,     0,  1356,  1356,  1356,     0,     0,   150,     0,   912,
     633,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     268,     0,   354,     0,     0,   354,   354,   268,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,   656,
       0,   150,   680,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   687,     0,     0,     0,   687,
       0,     0,     0,     0,     0,     0,     0,  1841,     0,   152,
     152,   152,   152,  1850,   152,   152,     0,  1104,     0,     0,
    1662,   333,     0,  1388,   833,     0,     0,     0,     0,     0,
       0,     0,    58,     0,   453,     0,     0,     0,   453,   453,
       0,  1479,     0,     0,     0,     0,     0,     0,     0,   453,
       0,     0,   453,     0,   150,     0,     0,  1883,    65,    66,
      67,    68,    69,    70,    71,    72,   150,     0,   233,   234,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   264,
     337,     0,  1125,     0,     0,   803,     0,     0,   338,   339,
     340,   341,     0,     0,    74,     0,    75,   486,     0,     0,
       0,     0,   815,     0,     0,   818,     0,   317,     0,     0,
       0,   656,     0,     0,     0,  2101,    77,     0,     0,   550,
       0,     0,   152,     0,   668,   515,    80,    81,   317,   150,
       0,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,     0,  1356,     0,  1356,     0,     0,     0,     0,     0,
       0,     0,  1934,  1935,     0,     0,     0,     0,     0,  1945,
     615,     0,     0,     0,   615,   561,     0,     0,   615,     0,
       0,  1960,     0,     0,     0,     0,  1276,  1277,     0,  1183,
       0,  1969,   342,  1970,   611,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1981,     0,  1983,  1984,  1985,   327,
     343,   150,   492,   601,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,  1662,  1809,     0,   312,     0,  1662,
       0,   453,     0,     0,     0,  1662,   462,  1662,     0,     0,
       0,     0,     0,   976,     0,     0,   150,   268,   233,   234,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   333,   152,     0,  1075,     0,     0,  2014,     0,   611,
     356,  2019,   101,     0,    74,     0,  2024,     0,     0,     0,
       0,     0,  1349,     0,     0,     0,     0,     0,   687,   957,
       0,   615,     0,     0,     0,  1660,    77,     0,     0,   611,
       0,     0,  1661,   968,     0,     0,    80,    81,     0,     0,
       0,  1607,   656,     0,     0,     0,     0,   977,     0,     0,
       0,   152,     0,     0,     0,   687,  1369,     0,   150,     0,
       0,  2066,    65,    66,    67,    68,    69,    70,    71,    72,
    1353,     0,     0,  2075,  1354,   615,  1355,  2078,     0,   152,
       0,     0,     0,     0,   615,     0,     0,     0,   615,     0,
     150,  2092,   233,   234,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,  1395,     0,  1398,    77,  1667,
       0,  1784,     0,  1809,  1809,     0,     0,     0,    74,  1402,
    1403,     0,     0,     0,     0,  1408,  1409,     0,  1662,     0,
       0,  1662,     0,  2123,  1415,     0,     0,     0,     0,   235,
      77,     0,     0,   333,     0,     0,   492,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,   462,
     453,  1435,   611,  1084,  1438,     0,   803,   803,     0,     0,
       0,  2146,     0,     0,     0,     0,  2149,  1092,   611,     0,
    1095,     0,   611,     0,     0,     0,     0,     0,     0,     0,
     687,   957,     0,   325,     0,   611,     0,  1110,     0,     0,
       0,     0,     0,     0,  2169,     0,     0,  2171,     0,  2149,
     492,     0,   492,     0,     0,     0,   492,   492,   492,     0,
       0,     0,   172,     0,     0,     0,  1497,     0,     0,  1809,
    2171,     0,     0,     0,   492,     0,     0,     0,  1662,     0,
       0,     0,     0,     0,   561,     0,     0,     0,     0,     0,
     172,  1168,     0,     0,     0,  1172,     0,  1519,     0,  1176,
       0,     0,     0,     0,     0,  1523,     0,  1525,  1527,     0,
       0,     0,     0,     0,     0,     0,  1533,   152,  1534,     0,
    1535,     0,     0,     0,     0,     0,     0,  1544,     0,     0,
       0,   268,     0,     0,     0,   611,     0,   172,     0,  1271,
       0,     0,     0,  1667,   492,     0,     0,  1809,  1667,     0,
     172,   156,   172,     0,  1824,     0,  1667,     0,   152,     0,
     687,     0,   268,  1302,     0,     0,     0,     0,     0,     0,
    1308,   462,   782,   783,   784,   785,   786,   787,   788,   789,
     790,   791,   792,   172,     0,   385,   220,   152,   152,     0,
    2102,   333,     0,     0,     0,     0,     0,     0,     0,  1597,
    1598,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,     0,   793,     0,     0,     0,     0,   152,     0,
       0,   317,   356,     0,  1620,     0,     0,     0,     0,     0,
       0,  1625,   409,  1626,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,  2102,  2102,     0,   172,
       0,     0,     0,   172,     0,     0,   172,   172,     0,  1643,
     172,     0,     0,   172,   172,     0,   172,     0,   172,   150,
       0,   204,    64,    65,    66,    67,    68,    69,    70,    71,
      72,  2005,     0,   741,  2102,     0,    78,   420,     0,     0,
       0,     0,     0,   803,     0,   611,     0,  1639,     0,   611,
       0,     0,     0,     0,   492,   492,     0,     0,   611,     0,
       0,     0,     0,     0,     0,     0,   268,  1932,   611,    77,
    1667,     0,   855,     0,     0,   611,     0,     0,     0,   403,
       0,     0,   404,     0,   405,     0,   406,     0,   172,     0,
     150,   172,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   407,     0,  1752,     0,     0,     0,   492,
       0,     0,  1756,     0,  1758,   172,  1418,     0,     0,  1422,
       0,   611,     0,  1426,   268,   611,     0,     0,     0,   611,
     172,     0,   327,   408,   409,   615,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,   156,   414,   415,   416,     0,   417,   418,     0,     0,
    1456,     0,     0,     0,    74,     0,     0,     0,     0,  1271,
       0,    58,     0,     0,     0,     0,     0,  1667,     0,     0,
       0,     0,     0,     0,     0,   419,     0,     0,    78,   420,
    1794,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,  1271,     0,     0,   150,     0,   233,   234,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,  1514,   118,     0,
       0,     0,     0,    74,   356,    75,     0,   172,     0,     0,
       0,   150,     0,   233,   234,    65,    66,    67,    68,    69,
      70,    71,    72,   656,  2101,    77,     0,     0,   550,     0,
       0,     0,   573,     0,     0,    80,    81,     0,   150,    74,
     603,   604,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   611,   356,   118,     0,     0,     0,     0,     0,   327,
     830,    77,     0,     0,   665,   385,   268,     0,     0,     0,
       0,    80,   831,     0,     0,     0,     0,     0,     0,   118,
       0,   172,     0,     0,   667,     0,     0,     0,     0,     0,
      78,     0,     0,  1891,  1892,   270,     0,     0,     0,   118,
       0,  1593,     0,     0,     0,     0,     0,  1901,     0,     0,
       0,     0,  1602,     0,     0,   632,   327,     0,     0,     0,
       0,     0,   492,     0,     0,   492,   492,     0,     0,     0,
       0,     0,     0,   118,     0,     0,     0,   118,     0,     0,
      58,   611,     0,   118,     0,   611,   118,     0,     0,   611,
     270,     0,     0,   327,     0,     0,     0,     0,     0,     0,
       0,   350,   118,     0,   382,   385,     0,     0,     0,  1456,
    1456,  1456,   156,   575,   150,     0,   233,   234,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   457,     0,     0,
       0,     0,     0,     0,  1686,     0,     0,     0,  1686,  1686,
     118,   457,    74,     0,    75,   270,   172,   172,     0,     0,
       0,     0,  1686,     0,     0,     0,     0,     0,   268,     0,
       0,     0,     0,   324,    77,     0,   172,     0,   172,     0,
       0,     0,     0,     0,    80,    81,     0,     0,   150,   118,
     233,   234,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,   118,   356,   118,     0,
       0,     0,   611,     0,   209,     0,    74,   118,     0,     0,
       0,     0,   270,     0,     0,     0,     0,     0,   118,     0,
       0,     0,   156,     0,     0,     0,     0,  2101,    77,     0,
       0,   550,     0,   606,     0,     0,   118,     0,    80,    81,
       0,   118,     0,   118,     0,     0,   270,   118,     0,     0,
       0,   270,     0,     0,     0,     0,   611,   270,     0,     0,
       0,     0,     0,     0,     0,   611,     0,   118,     0,   611,
       0,   209,     0,     0,     0,   172,   172,     0,     0,     0,
       0,     0,   172,     0,     0,     0,     0,   209,   118,     0,
     270,   118,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,   118,     0,     0,   172,   118,     0,
     172,   172,   209,   172,     0,   172,   172,     0,     0,  1812,
       0,     0,     0,  2100,     0,     0,   489,  1826,     0,     0,
       0,   150,     0,   233,   234,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,    14,    15,    16,    17,
      18,     0,  1838,   457,     0,     0,   172,     0,     0,    74,
     172,    75,     0,     0,   172,   150,   268,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,  2136,     0,
    1660,    77,     0,   209,     0,     0,     0,   457,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,  2151,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,   156,     0,     0,  2160,     0,   118,    78,     0,     0,
     457,     0,     0,     0,     0,     0,   270,     0,     0,     0,
       0,     0,     0,   178,   181,     0,     0,   118,     0,   150,
     172,   233,   234,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   209,    14,    15,    16,    17,    18,   457,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
     228,     0,     0,   209,  1924,     0,     0,  1812,  1812,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   235,    77,
       0,     0,     0,   118,     0,     0,     0,     0,     0,    80,
      81,     0,     0,     0,   457,   457,     0,     0,     0,   270,
       0,   118,    58,     0,     0,     0,     0,     0,     0,     0,
    1686,   319,     0,     0,   320,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   344,
       0,     0,     0,     0,     0,   270,   150,     0,   233,   234,
      65,    66,    67,    68,    69,    70,    71,    72,   270,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,   118,
       0,   118,   209,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,   172,     0,     0,   382,   118,   457,     0,
     270,     0,     0,  1812,     0,   324,    77,     0,   118,     0,
       0,     0,     0,   525,     0,   209,    80,    81,     0,     0,
       0,   118,     0,     0,   270,     0,     0,     0,   606,     0,
       0,   270,     0,     0,   118,     0,     0,   172,     0,     0,
       0,   172,     0,   118,   172,     0,     0,  2046,   172,     0,
       0,   457,     0,   209,   118,   118,     0,   457,     0,     0,
       0,   585,   586,     0,     0,     0,   457,     0,     0,   118,
     118,   118,   178,     0,     0,     0,     0,  2063,     0,     0,
       0,  1812,     0,     0,     0,     0,     0,   178,  1686,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   209,
     209,     0,     0,     0,     0,   150,   489,   233,   234,    65,
      66,    67,    68,    69,    70,    71,    72,  1686,  2046,     0,
       0,   636,     0,     0,  1812,   457,     0,     0,     0,   640,
     642,     0,     0,    74,   649,     0,   611,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,  1686,   457,
       0,     0,     0,     0,   324,    77,     0,   118,     0,     0,
       0,   118,     0,     0,   209,    80,    81,     0,     0,   118,
     457,     0,     0,   344,   118,     0,   344,  2139,     0,     0,
    1812,  1812,     0,   489,     0,     0,     0,     0,   172,   118,
       0,   118,     0,     0,     0,   118,   118,   118,     0,     0,
       0,     0,     0,     0,     0,     0,   209,     0,   112,     0,
       0,     0,     0,   118,     0,     0,     0,     0,  1812,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   209,     0,
       0,     0,     0,     0,     0,     0,   209,     0,     0,     0,
     209,     0,   209,  1184,   150,     0,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,   172,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,   172,     0,     0,
     172,     0,   172,   172,   118,     0,   228,     0,   457,     0,
       0,     0,     0,   118,     0,     0,     0,     0,   849,   850,
     118,   457,     0,   504,     0,     0,     0,     0,     0,   118,
       0,  1300,   457,     0,     0,     0,     0,     0,     0,     0,
     489,     0,   279,     0,     0,   150,   172,   175,   176,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,  1315,
       0,     0,     0,     0,   209,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,   489,   112,     0,     0,     0,
     118,   457,     0,     0,   508,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   489,   359,   489,     0,     0,     0,
     489,   489,   489,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   489,   172,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   493,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   944,     0,     0,    14,
      15,    16,    17,    18,   118,   344,     0,     0,   118,     0,
       0,     0,     0,   118,   118,     0,     0,   118,     0,     0,
       0,     0,   112,     0,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,   489,     0,
       0,     0,     0,     0,     0,     0,   209,     0,     0,     0,
     112,     0,     0,   172,     0,     0,     0,     0,    58,     0,
       0,   112,     0,     0,   598,     0,     0,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   359,
     118,   172,     0,     0,   118,     0,   112,     0,   118,     0,
     279,     0,   150,     0,   233,   234,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,   172,     0,   209,     0,     0,   118,
      74,   172,    75,     0,     0,     0,     0,     0,   457,     0,
       0,   657,     0,     0,   279,     0,     0,     0,     0,     0,
       0,  1660,    77,     0,     0,     0,     0,   657,     0,     0,
       0,   657,    80,    81,   457,     0,     0,     0,     0,     0,
       0,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   270,   118,     0,     0,     0,
       0,     0,     0,   118,     0,     0,   172,     0,   489,   489,
       0,  1127,     0,     0,     0,   118,     0,     0,     0,     0,
       0,  1142,   457,     0,     0,     0,  1300,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,   457,     0,   172,   172,     0,   118,     0,     0,     0,
       0,     0,     0,   489,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   657,     0,     0,     0,     0,     0,   172,
     172,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,     0,   172,     0,     0,     0,     0,     0,     0,
    1211,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,   118,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,   118,     0,   359,     0,   118,     0,
       0,     0,     0,     0,   117,     0,  1318,     0,     0,     0,
       0,  1640,     0,     0,   493,     0,     0,     0,   118,   118,
     118,   118,   118,   118,   118,     0,     0,     0,     0,   112,
     270,     0,     0,     0,     0,     0,     0,     0,   209,     0,
     172,     0,     0,   457,     0,     0,     0,   457,   457,     0,
     209,     0,     0,     0,     0,     0,     0,   209,   457,     0,
       0,   457,   117,     0,     0,     0,     0,     0,     0,     0,
       0,   359,   493,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   209,     0,   270,     0,
     657,   493,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   359,     0,     0,     0,     0,   457,     0,   281,     0,
       0,   118,   172,     0,   657,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1763,     0,     0,   657,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   117,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   117,     0,     0,     0,   489,     0,     0,   489,
     489,     0,     0,     0,     0,   118,   431,     0,     0,     0,
       0,   363,     0,     0,   118,     0,     0,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     495,     0,     0,     0,     0,   172,     0,     0,   493,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     457,     0,     0,     0,   359,     0,     0,  1476,  1478,  1480,
       0,     0,     0,   209,     0,     0,     0,     0,   117,     0,
     359,     0,     0,     0,   359,     0,     0,     0,     0,     0,
     270,   118,   657,   493,     0,     0,     0,   359,     0,     0,
    1501,     0,     0,     0,     0,     0,   117,     0,     0,     0,
       0,     0,   359,     0,   359,     0,     0,   117,   359,   359,
     359,   209,  1211,     0,     0,     0,     0,     0,     0,  1521,
       0,     0,     0,     0,     0,   363,   359,     0,     0,     0,
       0,     0,   117,     0,     0,     0,   281,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1307,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,   659,     0,     0,
     281,     0,     0,     0,     0,     0,     0,   359,     0,     0,
       0,   112,     0,   659,     0,     0,   359,   659,   403,     0,
       0,   404,     0,   405,   708,   406,     0,     0,   431,   714,
       0,     0,   657,     0,     0,   279,     0,     0,   723,   724,
       0,    58,   407,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   270,   431,   431,   209,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   457,
       0,     0,   408,   409,   431,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   493,   417,   418,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,   431,     0,     0,
       0,     0,     0,     0,     0,     0,  1673,  1674,     0,     0,
       0,     0,     0,     0,   419,     0,     0,    78,   420,   659,
       0,     0,     0,     0,   421,   485,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   359,     0,     0,
       0,   359,     0,     0,     0,     0,   359,   359,     0,     0,
     359,     0,     0,     0,     0,     0,   118,     0,     0,     0,
     359,     0,     0,     0,     0,     0,     0,   359,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,  1764,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     495,     0,     0,     0,     0,     0,     0,   118,     0,     0,
       0,   359,     0,     0,     0,   117,     0,     0,     0,     0,
       0,     0,     0,   359,     0,     0,     0,   359,     0,     0,
       0,   359,     0,     0,     0,     0,   118,   118,     0,     0,
     270,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,   363,   495,     0,
     117,     0,     0,     0,     0,     0,     0,   118,     0,     0,
       0,   112,     0,     0,     0,     0,   659,   495,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     659,     0,     0,     0,   112,     0,     0,     0,  1834,     0,
       0,     0,     0,   659,     0,   149,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   279,
       0,     0,     0,     0,     0,     0,   359,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   657,     0,     0,     0,   431,
     431,   431,   431,   431,   431,   431,   431,   431,   431,   431,
     431,   431,   431,   431,   431,   431,   431,   431,     0,     0,
       0,     0,     0,   359,   493,     0,     0,     0,     0,     0,
       0,   207,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   495,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,     0,
     363,     0,     0,     0,   431,     0,     0,     0,   659,   495,
       0,     0,     0,   363,   359,     0,     0,   359,   359,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
     363,     0,     0,   359,   363,   363,   363,   359,     0,     0,
       0,   359,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     207,     0,     0,     0,  1987,     0,   112,     0,     0,     0,
     112,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,   117,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   577,     0,     0,   659,     0,
       0,   281,     0,     0,     0,     0,     0,  1542,     0,   493,
       0,     0,     0,     0,   359,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   431,     0,   618,     0,     0,     0,   431,
       0,     0,     0,     0,     0,     0,     0,   625,     0,     0,
     431,   403,     0,     0,   404,     0,   405,     0,   406,     0,
     495,     0,     0,     0,   635,     0,     0,     0,   359,     0,
       0,     0,     0,     0,    58,   407,     0,   359,     0,     0,
       0,   359,     0,     0,     0,   654,     0,     0,     0,     0,
       0,     0,   431,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,     0,   417,   418,
       0,     0,     0,   363,     0,     0,    74,   363,    75,     0,
       0,     0,   363,   363,     0,     0,   363,     0,     0,     0,
       0,     0,   740,     0,     0,     0,   363,   419,     0,     0,
      78,   420,     0,   363,   279,     0,     0,   421,  1543,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   781,   734,     0,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,   821,     0,   363,     0,     0,   826,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   482,   370,     0,
       0,     0,     0,     0,     0,   431,   852,     0,     0,     0,
     853,   854,     0,     0,   857,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   117,     0,   869,
     553,     0,     0,     0,     0,     0,     0,   553,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   899,     0,     0,     0,     0,     0,     0,     0,
     117,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   281,     0,     0,     0,   431,
       0,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   112,     0,     0,   907,   909,     0,     0,     0,
       0,   659,     0,     0,     0,     0,     0,     0,   553,     0,
       0,     0,     0,     0,     0,     0,   431,   431,   431,   939,
       0,     0,     0,   431,   431,     0,     0,     0,     0,   363,
     495,     0,     0,   946,     0,   370,   669,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   431,     0,     0,
       0,     0,     0,     0,     0,   690,     0,   965,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   431,   431,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,   363,   363,     0,     0,     0,     0,   657,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
    1017,     0,     0,   363,     0,     0,     0,   363,     0,     0,
       0,     0,   734,     0,     0,     0,     0,   553,   734,     0,
       0,     0,     0,     0,     0,     0,     0,   734,     0,     0,
     112,     0,     0,     0,   553,   816,     0,   553,   819,     0,
       0,     0,     0,     0,     0,     0,   734,   370,     0,     0,
       0,   669,   117,     0,     0,     0,   117,   117,     0,   112,
     657,     0,     0,     0,   482,     0,     0,     0,     0,     0,
     117,     0,     0,     0,     0,     0,     0,     0,   359,     0,
       0,     0,  1072,     0,     0,     0,     0,     0,     0,     0,
     112,     0,  1099,   553,  1100,     0,     0,   553,     0,     0,
     826,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   431,   495,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,     0,  1143,     0,     0,
       0,     0,     0,     0,     0,     0,  1152,     0,   370,     0,
    1154,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,   654,     0,   363,     0,     0,
    1196,     0,     0,   553,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   955,   370,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   669,     0,     0,     0,   669,
       0,     0,     0,     0,     0,     0,   973,     0,   370,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   216,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     281,     0,   274,     0,     0,     0,     0,  1327,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   431,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   170,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   216,     0,     0,     0,   334,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   553,   553,
       0,     0,     0,     0,   216,     0,     0,     0,     0,   553,
    1093,     0,   553,  1096,     0,     0,     0,     0,   502,     0,
       0,     0,   507,     0,     0,     0,   955,   370,   299,     0,
       0,   669,     0,   669,   669,     0,     0,     0,     0,     0,
     669,   305,     0,   306,     0,     0,   370,     0,   370,     0,
       0,     0,   370,   370,   370,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     370,     0,   553,     0,   377,   216,   553,     0,   117,     0,
       0,     0,     0,   553,  1169,     0,     0,   553,  1173,   274,
       0,   553,  1177,     0,     0,     0,     0,     0,     0,  1181,
       0,     0,     0,     0,   149,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   507,     0,     0,   431,     0,     0,
     370,   553,     0,     0,   216,     0,     0,   555,   556,     0,
       0,   560,     0,   781,   563,   564,     0,   566,     0,   567,
       0,     0,     0,     0,     0,   662,     0,   679,     0,   669,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   431,
       0,     0,     0,     0,     0,   734,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   659,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1532,     0,
       0,     0,     0,     0,     0,     0,     0,   482,   370,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     738,     0,  1557,     0,     0,     0,   117,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   652,     0,     0,     0,
       0,     0,     0,     0,   216,   117,   659,     0,   431,     0,
     431,   684,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,   553,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   117,   662,     0,     0,
     370,   370,     0,   844,   669,   669,     0,     0,     0,   431,
       0,   669,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   216,     0,     0,   431,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,   553,  1419,
       0,   553,  1423,     0,     0,   553,  1427,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   814,     0,
     431,   216,   216,     0,     0,     0,     0,     0,   502,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1680,  1688,     0,     0,  1680,  1698,
       0,     0,     0,     0,  1705,     0,     0,     0,  1709,     0,
    1711,     0,  1698,     0,     0,  1746,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,     0,
       0,     0,   895,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   502,     0,   959,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     370,     0,     0,     0,     0,     0,     0,     0,   662,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,     0,     0,     0,   669,  1540,     0,     0,     0,
     216,     0,     0,     0,     0,     0,     0,     0,   216,     0,
       0,   738,   216,     0,   216,     0,     0,     0,   370,     0,
       0,     0,     0,   738,     0,     0,   738,   738,   738,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1746,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   553,  1594,     0,     0,   971,   972,     0,
       0,  1800,     0,     0,   553,  1603,     0,   669,     0,     0,
       0,     0,   502,     0,     0,     0,     0,   981,   370,   983,
       0,   370,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   216,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1837,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   502,     0,     0,
       0,     0,     0,     0,     0,     0,  1856,  1858,     0,     0,
       0,     0,     0,     0,     0,     0,   502,     0,   502,  1887,
    1888,     0,   502,   502,   502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1876,   294,     0,     0,
     502,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1086,  1087,     0,     0,
       0,     0,     0,  1091,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   366,     0,   370,     0,     0,     0,     0,  1114,     0,
       0,  1117,  1118,     0,  1121,     0,  1123,  1124,     0,     0,
     502,     0,     0,     0,     0,     0,     0,     0,   216,     0,
     669,     0,     0,     0,     0,     0,     0,     0,   844,     0,
     366,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1972,     0,     0,  1943,     0,  1167,     0,     0,
       0,  1171,  1946,     0,  1948,  1175,     0,  1954,  1959,     0,
    1698,     0,     0,     0,     0,  1965,     0,     0,     0,     0,
       0,     0,     0,  1746,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2013,   553,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   366,     0,     0,   553,     0,
       0,  1309,     0,     0,     0,     0,     0,     0,     0,  2042,
       0,     0,     0,  2043,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2030,     0,     0,     0,     0,  2036,  2038,     0,
     502,   502,     0,     0,     0,     0,     0,   366,     0,   366,
     366,     0,     0,     0,     0,     0,     0,  2057,     0,     0,
       0,     0,     0,   366,     0,     0,     0,   366,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,     0,   502,  2079,     0,  2082,     0,
       0,     0,  2084,  2086,     0,     0,     0,  2089,  2091,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   553,
     553,     0,     0,     0,  1309,     0,   738,     0,     0,     0,
       0,     0,     0,     0,     0,   553,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2130,
    2132,  2134,     0,     0,     0,     0,     0,     0,     0,   366,
       0,   738,     0,     0,     0,   366,     0,     0,  1413,     0,
       0,     0,  1417,     0,     0,  1421,     0,     0,     0,  1425,
    2155,  2157,  2159,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   274,     0,     0,     0,     0,     0,     0,     0,
     375,     0,     0,     0,   365,     0,     0,     0,     0,     0,
       0,     0,   216,     0,     0,     0,     0,     0,     0,   662,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   366,     0,     0,   553,     0,     0,     0,     0,
       0,     0,     0,   553,     0,     0,     0,     0,   375,     0,
     366,     0,     0,   738,     0,     0,   365,     0,   365,   365,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,   365,     0,     0,     0,
       0,     0,     0,     0,   366,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   366,   366,   553,
    2064,     0,     0,   553,     0,     0,     0,     0,   502,  1538,
       0,   502,   502,     0,     0,     0,   366,   366,     0,   366,
       0,     0,     0,     0,     0,     0,     0,   366,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     366,     0,     0,   366,     0,     0,   553,     0,     0,     0,
     366,     0,     0,   366,     0,   738,   738,   738,     0,     0,
     738,   738,     0,     0,     0,     0,     0,   507,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1592,   365,     0,
       0,     0,     0,     0,   365,     0,     0,     0,  1601,     0,
       0,  1605,     0,  1608,  1609,   216,     0,     0,     0,     0,
       0,     0,   553,   553,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   274,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   367,  1635,     0,     0,
     553,     0,     0,   375,   366,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,     0,     0,     0,
     366,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,     0,   367,   366,     0,     0,     0,
     366,     0,     0,     0,     0,     0,     0,     0,   366,   366,
       0,     0,     0,   366,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,   366,     0,
     366,     0,     0,     0,   366,   366,   366,     0,     0,     0,
    1740,     0,     0,     0,     0,     0,   365,   365,     0,     0,
       0,     0,   366,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,   365,     0,   365,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   216,     0,   365,
     367,     0,   365,     0,     0,     0,     0,     0,     0,   365,
       0,     0,   365,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   366,     0,     0,     0,   274,     0,     0,
       0,     0,   366,     0,  1605,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   366,     0,
     366,   366,   367,     0,   367,   367,     0,     0,     0,     0,
       0,     0,  1796,     0,     0,     0,     0,     0,   367,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,     0,
     366,     0,     0,     0,     0,   738,     0,     0,     0,   365,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   455,
       0,     0,     0,     0,     0,   365,     0,     0,     0,   365,
       0,     0,     0,   487,     0,     0,     0,   365,   365,     0,
       0,     0,   365,     0,     0,     0,     0,   516,     0,   516,
       0,     0,     0,     0,     0,     0,     0,   365,     0,   365,
       0,     0,     0,   365,   365,   365,     0,  1884,     0,   274,
       0,     0,     0,   366,   367,     0,     0,   366,     0,     0,
     367,   365,   366,   366,     0,     0,   366,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   366,     0,     0,     0,
       0,     0,     0,   366,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1913,  1914,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   366,     0,     0,
    1928,  1929,   365,     0,     0,     0,     0,   367,   630,   366,
       0,   365,     0,   366,  1933,     0,     0,   366,     0,     0,
       0,     0,     0,     0,     0,   367,     0,   365,     0,   365,
     365,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   367,
       0,     0,     0,   738,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   367,   367,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   367,   367,     0,   367,     0,     0,     0,     0,     0,
       0,  2002,   367,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   366,     0,     0,   367,     0,     0,   367,     0,
       0,     0,     0,     0,   738,   367,     0,   507,   367,     0,
       0,   366,     0,     0,     0,   366,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,   365,     0,     0,   366,
     366,   365,   365,     0,     0,   365,     0,     0,     0,     0,
       0,     0,     0,  2062,     0,   365,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,     0,   516,
       0,     0,     0,     0,     0,   516,     0,     0,     0,     0,
     455,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,   367,
       0,     0,     0,     0,     0,     0,     0,     0,   365,     0,
     366,     0,   365,   366,   366,   367,   365,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   366,
       0,   367,     0,   366,     0,   367,     0,   366,     0,     0,
       0,     0,     0,   367,   367,     0,     0,     0,   367,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   367,     0,   367,     0,     0,     0,   367,
     367,   367,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   938,     0,     0,     0,     0,     0,   367,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     487,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,   967,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,   365,   366,     0,     0,   367,     0,
     366,     0,     0,     0,     0,     0,     0,   367,     0,     0,
    1000,     0,     0,     0,     0,     0,     0,     0,   365,   365,
       0,  1010,     0,   367,     0,   367,   367,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1031,  1033,     0,     0,  1035,
       0,  1037,     0,     0,   366,     0,     0,  1000,     0,  1049,
    1000,     0,     0,   366,     0,     0,     0,   366,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   367,     0,  1077,     0,   365,
       0,     0,   365,   365,     0,     0,     0,     0,     0,     0,
    1079,     0,     0,     0,     0,     0,     0,     0,   365,     0,
       0,  1088,   365,     0,     0,     0,   365,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   487,     0,     0,
       0,     0,  1077,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   367,     0,
       0,     0,   367,  1146,     0,     0,   516,   367,   367,     0,
       0,   367,     0,     0,     0,     0,  1156,     0,     0,     0,
       0,   367,     0,     0,     0,     0,     0,     0,   367,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1182,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   367,     0,   365,     0,     0,     0,     0,   365,
       0,     0,     0,     0,   367,     0,     0,     0,   367,     0,
       0,     0,   367,   403,     0,     0,   404,     0,   405,     0,
     406,  1951,     0,   455,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1299,  1301,     0,     0,   407,     0,     0,
       0,   487,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,   365,   408,   409,     0,
     410,   411,  1952,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,     0,
     417,   418,     0,  1077,     0,     0,     0,     0,    74,     0,
       0,  1341,     0,     0,     0,     0,     0,     0,     0,     0,
    1000,     0,     0,  1692,  1693,  1694,     0,   367,     0,   419,
    1953,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,   367,     0,     0,     0,
     367,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   516,   186,     6,     7,     8,     9,
      10,    11,    12,    13,   367,   367,     0,     0,     0,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,   257,   258,
       0,   259,    46,     0,    47,   366,   260,     0,     0,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   516,     0,  1412,     0,
       0,     0,     0,     0,     0,   367,     0,     0,   367,   367,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   367,     0,     0,     0,   367,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   366,     0,     0,   366,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   366,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -466,     0,
    1489,  1489,     0,     0,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,  -466,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,     0,     0,
     367,     0,     0,     0,     0,   367,     0,  2170,  1536,     0,
       0,     0,    58,     0,  1545,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1471,     0,     0,     0,     0,  1000,
       0,     0,     0,   487,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,   188,   189,
       0,   516,     0,     0,  1573,   403,     0,     0,   404,   367,
     405,     0,   406,     0,   365,     0,     0,     0,   367,  1031,
       0,     0,   367,     0,     0,     0,    75,  1213,     0,   407,
    1215,     0,  1216,  -249,  -249,  1217,  1218,  1219,  1220,  1221,
    1222,  1223,  1224,  1225,  1226,  1227,  1228,  -345,  -345,  1229,
    1230,  1231,  1232,  1233,  1234,  1235,     0,  1236,     0,   408,
     409,     0,   510,   411,  1237,  1238,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,  1239,   414,   415,
     416,     0,   417,   418,     0,   365,     0,     0,   365,     0,
      74,     0,  1637,  1638,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,     0,
    -249,  1240,     0,     0,    78,   420,     0,     0,  1000,   303,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,     0,  -189,   516,     0,     0,
     455,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2170,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1471,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1033,     0,
       0,     0,     0,     0,     0,     0,     0,  1754,  1755,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   403,
       0,     0,   404,     0,   405,     0,   406,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   516,     0,     0,     0,
    1031,  1213,     0,   407,  1215,     0,  1216,  -250,  -250,  1217,
    1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  -345,  -345,  1229,  1230,  1231,  1232,  1233,  1234,  1235,
       0,  1236,     0,   408,   409,     0,   510,   411,  1237,  1238,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,  1239,   414,   415,   416,     0,   417,   418,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   455,     0,  -250,  1240,     0,  1825,    78,   420,
       0,     0,     0,   303,     0,   421,    80,    81,   422,   423,
     424,   425,    14,    15,    16,    17,    18,     0,     0,    20,
    -189,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -490,  -490,     0,  -490,
      46,     0,    47,     0,  -490,     0,     0,     0,  1871,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     367,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1882,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1471,   516,     0,     0,     0,     0,     0,     0,  1903,     0,
       0,  1905,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,     0,   404,    75,   405,  1919,   406,     0,
       0,   367,     0,     0,   367,     0,     0,     0,     0,     0,
       0,     0,     0,  1213,     0,   407,  1215,     0,  1216,   367,
       0,  1217,  1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,
    1226,  1227,  1228,  -345,  -345,  1229,  1230,  1231,  1232,  1233,
    1234,  1235,     0,  1236,     0,   408,   409,     0,   510,   411,
    1237,  1238,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,  1239,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1240,     0,     0,
      78,   420,     0,     0,     0,   303,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,     0,  -189,     4,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,  1212,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   403,
       0,    46,   404,    47,   405,  1000,   406,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,  1213,    58,  1214,  1215,     0,  1216,     0,     0,  1217,
    1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  -345,  -345,  1229,  1230,  1231,  1232,  1233,  1234,  1235,
       0,  1236,     0,   408,   409,    61,   510,   411,  1237,  1238,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,  1239,   414,   415,   416,     0,   417,   418,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -3,  1240,     0,     0,    78,  1241,
       0,     0,     0,   303,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,     0,
    -189,     4,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1212,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   403,     0,    46,
     404,    47,   405,     0,   406,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1213,
      58,  1214,  1215,     0,  1216,     0,     0,  1217,  1218,  1219,
    1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  -345,
    -345,  1229,  1230,  1231,  1232,  1233,  1234,  1235,     0,  1236,
       0,   408,   409,    61,   510,   411,  1237,  1238,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,  1239,
     414,   415,   416,     0,   417,   418,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1240,     0,     0,    78,  1241,     0,     0,
       0,   303,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,     0,  -189,     4,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   403,     0,    46,   404,    47,
     405,     0,   406,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
     409,    61,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,     0,   417,   418,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1692,  1693,  1694,     0,     0,
       0,   419,  1695,  1696,    78,  1241,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,     0,  1697,     4,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   403,     0,    46,   404,    47,   405,     0,
     406,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   407,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,   409,    61,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,     0,
     417,   418,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1692,  1693,  1694,     0,     0,     0,   419,
    1695,     0,    78,  1241,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,     0,  1697,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   403,     0,    46,   404,    47,   405,     0,   406,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,   409,    61,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,  1690,
      78,  1241,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     4,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     403,     0,    46,   404,    47,   405,     0,   406,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,   407,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   408,   409,    61,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,     0,   417,   418,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,    78,
    1241,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   403,     0,
      46,   404,    47,   405,     0,   406,   351,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   407,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,     0,   417,   418,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,    78,   484,     0,
       0,     0,     0,     0,   421,   485,    81,   422,   423,   424,
     425,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   403,     0,    46,   404,
      47,   405,     0,   406,   351,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     407,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,     0,   417,   418,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,    78,  1296,     0,     0,     0,
       0,     0,   421,  1297,    81,   422,   423,   424,   425,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   403,     0,    46,   404,    47,   405,
       0,   406,   351,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   407,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   828,     0,     0,     0,     0,     0,
     421,   485,    81,   422,   423,   424,   425,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   403,     0,    46,   404,    47,   405,     0,   406,
     351,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,     0,   417,
     418,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     403,     0,    46,   404,    47,   405,     0,   406,   351,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   407,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,     0,   417,   418,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,    78,
     828,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,  2012,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,    -2,     0,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,
       0,    -2,     0,     0,    -2,     0,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,    -2,    -2,
    2041,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
      -2,     0,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,
       0,    -2,     0,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,    -2,    -2,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,    59,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    61,
      62,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,    78,    79,     0,     0,     0,     0,     0,     0,
      80,    81,   262,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -491,  -491,     0,  -491,
      46,     0,    47,     0,  -491,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   150,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,   263,     0,
       0,     0,  -822,     0,     0,    80,    81,   262,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -491,  -491,     0,  -491,    46,     0,    47,     0,  -491,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     150,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,    78,   263,     0,     0,     0,     0,     0,     0,
      80,    81,     4,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,     0,     0,     0,     0,  -412,  -412,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -412,     0,     0,     0,    78,    79,     0,
       0,     0,     0,     0,     0,    80,    81,     4,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,     0,     0,     0,
       0,  -413,  -413,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -413,     0,
       0,     0,    78,    79,     0,  1447,     0,  1448,     0,     0,
      80,    81,  1449,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1450,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1451,     0,     0,     0,
      78,  1005,     0,  1447,     0,  1448,     0,     0,    80,    81,
    1449,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1450,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1646,     0,     0,     0,    78,  1005,
       0,  1447,     0,  1448,     0,     0,    80,    81,  1449,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1450,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1647,     0,     0,     0,    78,  1005,     0,  1447,
       0,  1448,     0,     0,    80,    81,  1449,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1450,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1648,     0,     0,     0,    78,  1005,     0,     0,     0,     0,
       0,     0,    80,    81,   262,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -491,  -491,
       0,  -491,    46,     0,    47,     0,  -491,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    63,
      64,     0,     0,    46,     0,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   262,     0,     0,     0,     0,     0,     0,    78,
     263,     0,    14,    15,    16,    17,    18,    80,    81,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -491,  -491,    75,  -491,
      46,     0,    47,     0,  -491,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   332,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     351,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
     609,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1103,    77,
    -685,    78,   665,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -491,  -491,     0,  -491,    46,     0,
      47,     0,  -491,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,   263,     0,     0,     0,
    -826,     0,     0,    80,    81,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -491,  -491,
       0,  -491,    46,     0,    47,     0,  -491,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,    78,
     263,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   351,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,   609,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     664,     0,  -685,    78,   665,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   351,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,   609,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   797,     0,  -685,    78,   550,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     351,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
    1136,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -693,    78,   913,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   351,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   352,    78,   353,     0,     0,     0,
       0,     0,     0,    80,    81,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   351,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,  1615,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     913,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   351,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,  1617,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   913,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   351,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   332,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     351,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   913,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   351,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   353,     0,     0,     0,
       0,     0,     0,    80,    81,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -491,  -491,
       0,  -491,    46,     0,    47,     0,  -491,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,  1471,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   403,     0,     0,
     404,     0,   405,     0,   406,     0,     0,     0,     0,    78,
     263,     0,     0,     0,     0,     0,     0,    80,    81,  1213,
       0,   407,  1215,     0,  1216,  1936,  1937,  1217,  1218,  1219,
    1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,     0,
       0,  1229,  1230,  1231,  1232,  1233,  1234,  1235,     0,  1236,
       0,   408,   409,     0,   510,   411,  1237,  1238,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,  1239,
     414,   415,   416,     0,   417,   418,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,  1471,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1240,     0,     0,    78,   420,     0,     0,
       0,   303,     0,   421,    80,    81,   422,   423,   424,   425,
     403,     0,     0,   404,     0,   405,     0,   406,  -189,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1213,     0,   407,  1215,     0,  1216,     0,     0,
    1217,  1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,
    1227,  1228,     0,     0,  1229,  1230,  1231,  1232,  1233,  1234,
    1235,     0,  1236,     0,   408,   409,     0,   510,   411,  1237,
    1238,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,  1239,   414,   415,   416,     0,   417,   418,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1240,     0,     0,    78,
     420,     0,     0,     0,   303,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
       0,  -189,   307,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -416,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,     0,     0,
       0,     0,  -416,   307,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -417,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,     0,     0,  -417,   307,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,    14,    15,    16,    17,    18,
      19,   725,    20,   726,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    63,
      64,   403,     0,    46,   404,    47,   405,     0,   406,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   407,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   727,     0,     0,
       0,     0,  1228,     0,  -345,     0,     0,     0,     0,    78,
       0,     0,     0,     0,  -416,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1240,     0,     0,
      78,   728,     0,     0,     0,   303,     0,   421,    80,    81,
     729,   730,   424,   425,    14,    15,    16,    17,    18,    19,
     725,    20,   726,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     403,     0,    46,   404,    47,   405,     0,   406,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   407,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   727,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,     0,   417,   418,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,    78,
     728,     0,     0,     0,   303,     0,   421,    80,    81,   729,
     730,   424,   425,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   403,
       0,    46,   404,    47,   405,     0,   406,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   407,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,     0,   417,   418,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   419,     0,   450,    78,   451,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   403,     0,
      46,   404,    47,   405,     0,   406,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   407,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,     0,   417,   418,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,    78,   451,     0,
       0,     0,   303,     0,   421,    80,    81,   422,   423,   424,
     425,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   403,     0,    46,
     404,    47,   405,     0,   406,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   407,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,     0,   417,   418,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,    78,   728,     0,     0,
       0,   303,     0,   421,    80,    81,   422,   423,   424,   425,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   403,     0,    46,   404,
      47,   405,     0,   406,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     407,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,     0,   417,   418,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,    78,   451,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   403,     0,    46,   404,    47,
     405,     0,   406,   351,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,     0,   417,   418,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,     0,    78,   828,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   403,     0,    46,   404,    47,   405,
       0,   406,   351,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   407,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   716,
       0,   717,   718,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,   262,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,   -17,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -491,  -491,     0,  -491,    46,     0,    47,     0,  -491,
       0,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   351,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   609,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -685,    78,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,    77,
       0,    78,    79,     0,     0,     0,  -824,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,    77,     0,    78,   208,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,    77,
       0,    78,    79,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   351,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,   478,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   851,     0,     0,    78,   479,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,   478,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   479,     0,     0,
       0,     0,     0,     0,    80,    81,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   351,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   609,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,  -685,
      78,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -491,  -491,
       0,  -491,    46,     0,    47,     0,  -491,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,    78,
     332,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   351,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,  1206,     0,     0,     0,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,    78,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   351,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     851,     0,     0,    78,   479,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   351,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   851,     0,     0,    78,   479,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1012,    78,  1005,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,  1561,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,  1005,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   315,     0,    63,    64,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   208,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   351,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,   351,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     353,     0,    63,    64,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   315,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   351,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   479,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -491,  -491,     0,  -491,    46,     0,    47,     0,  -491,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,   351,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   332,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
    1005,     0,    63,    64,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,   351,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   479,     0,
      63,    64,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,  1005,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   351,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,     0,     0,    14,    15,
      16,    17,    18,    80,    81,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -491,  -491,     0,  -491,    46,     0,    47,     0,
    -491,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   332,     0,    14,    15,    16,    17,
      18,    80,    81,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -491,  -491,     0,  -491,    46,     0,    47,     0,  -491,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,     0,     0,     0,     0,     0,    80,
      81,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     403,     0,    46,   404,    47,   405,     0,   406,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,     0,   417,   418,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,    78,
     420,     0,     0,     0,     0,     0,   421,   485,    81,   422,
     423,   424,   425,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   403,     0,    46,   404,    47,   405,     0,   406,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,     0,   417,
     418,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
      78,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,   351,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      78,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -491,  -491,     0,  -491,
      46,     0,    47,     0,  -491,     0,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   716,     0,
     717,   718,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    75,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -491,  -491,     0,  -491,    46,     0,
      47,   403,  -491,     0,   404,     0,   405,     0,   406,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,     0,   417,   418,
       0,     0,   403,    75,     0,   404,    74,   405,     0,   406,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1692,  1693,  1694,     0,     0,   407,   419,  1857,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   510,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,    77,
       0,   511,   512,     0,     0,     0,   513,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,  1344,     0,    78,   420,     0,
       0,     0,  1345,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,     0,   513,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,   996,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,  1030,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,     0,
       0,    78,   420,     0,     0,     0,   303,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,     0,     0,    78,   420,     0,
       0,  1071,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,     0,  1481,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,  1572,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,     0,     0,    78,   420,     0,     0,     0,
    1765,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,     0,
    1942,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,  1947,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,  1951,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,  1958,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,  1952,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,     0,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,  2035,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,  2037,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,  2081,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,  2083,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,  2085,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,  2088,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,  2090,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,  2129,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,  2131,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,  2133,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,  2154,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,  2156,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,  2158,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   707,     0,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   713,     0,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   722,     0,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,     0,     0,    78,   420,     0,
       0,     0,     0,     0,   421,   937,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,     0,     0,     0,
     421,   485,    81,   422,   423,   424,   425
};

static const yytype_int16 yycheck[] =
{
       1,     4,   524,     4,    85,    76,    76,   180,   261,    98,
     170,    76,    76,   356,     1,   950,   276,   230,   223,  1258,
     419,   677,   139,   185,    78,   982,   354,   732,   751,   185,
       1,   754,   168,   990,   513,   235,  1240,   207,   143,   168,
     235,   421,   370,   256,   419,   668,   374,   934,  1046,  1196,
    1292,  1293,   235,   168,  1940,    56,    57,   858,    59,   245,
    1363,    59,     0,  1808,    84,  1702,  1808,  1936,   194,    98,
     100,   152,    59,     0,  1223,    76,   170,  1075,     1,     1,
       1,  1808,   570,   571,    85,   235,    75,   273,    59,   845,
     235,   847,    93,   833,   207,   235,   407,    98,   284,   839,
     101,   206,   105,   798,   105,   830,   664,   238,   235,   240,
     161,  1240,   832,   664,    85,   664,   247,   832,   105,   235,
     171,   432,   433,   235,   324,   196,   196,    98,   245,   324,
     101,   196,   235,    75,   105,   155,    59,    59,   934,   830,
     670,   324,   453,   144,   830,  1143,   147,   236,   149,   492,
     180,   149,   139,   830,   155,     0,   273,    98,   486,   112,
    1809,   162,   149,    89,   235,   235,   348,   284,    72,   170,
     235,   235,   155,   120,   324,   486,   370,    90,   149,   324,
     374,   182,   135,   264,   324,   184,    72,   988,   319,   320,
     582,   180,   309,   194,   195,   196,   179,   324,   922,  1205,
    2069,   593,  1205,   101,   157,   206,   388,   236,   324,   832,
     211,   699,   324,   137,    76,  1852,    72,   377,     1,   943,
     221,   324,   311,   711,   195,   226,   149,   149,   229,   230,
     231,  1205,   136,  2119,   235,   236,   325,   163,   180,   354,
     211,   162,   155,    76,   298,    10,   638,   171,  1354,    76,
     136,   155,   333,   324,   324,   256,   155,   155,   245,   324,
     324,  1698,   135,   264,   112,   236,  2152,   125,   611,   395,
     120,  1574,   830,   274,   275,  1924,    59,   278,   137,   830,
     136,   830,   311,   377,   285,     1,   273,   135,   386,     1,
     384,   389,   111,   264,   554,   236,   325,   284,   299,   300,
     158,   302,   562,   159,   834,   178,   307,   278,   838,   159,
     311,  1034,   171,   656,   519,   134,  1021,   805,   161,   849,
     850,   155,   309,   324,   325,   653,   313,  1214,   171,   709,
     490,   531,   707,   334,   196,   710,   531,   680,   713,    10,
     311,   342,   343,    59,   687,   968,   347,   722,   531,   677,
     725,   726,   727,   752,   325,   568,  2101,  1314,    62,  2101,
    1247,   574,   690,   196,  1104,   155,   149,   157,    20,   196,
     311,   486,   453,   235,  2101,  2072,   377,   482,  1103,  1508,
    1193,   531,  1511,  1512,   325,   386,   531,  1200,   389,  1084,
    1110,   531,   163,     4,   395,  1110,  2045,   101,   503,   170,
     165,  2098,   235,   152,   531,   170,   592,   577,   235,   113,
    1557,   115,  1103,   117,   163,   531,   158,  1103,  1214,   531,
    1857,  1858,   149,   139,  2121,   947,  1103,   166,   531,   155,
     179,   631,   154,   149,   173,   174,   631,   179,  1444,  1445,
    1446,  1444,  1445,  1446,   171,    56,    57,   758,   631,   161,
     162,  1247,   156,  2102,   543,   159,   160,   511,   180,   653,
     531,   531,   163,   464,   577,   635,   531,   154,  1281,   170,
    1444,  1445,  1446,  1197,   161,   592,  1582,  1583,  1584,    61,
      62,   631,    93,   154,   654,     1,   631,  1110,   489,   490,
    2139,   631,     1,   464,   165,     4,   690,   825,   244,   170,
     501,   502,    77,    78,   631,   251,   635,   211,    77,    78,
     511,  1312,   513,   161,   543,   631,  1953,  1954,  1319,   631,
     635,   161,     1,    75,   108,   109,   272,   919,   631,   245,
     531,  1735,   180,   144,    75,    89,   147,   283,   163,    91,
     180,   654,   543,    59,   633,  1103,   161,   155,  1697,    90,
      59,   162,  1103,  1702,  1103,   180,   155,   273,   110,   170,
     631,   631,   677,   162,  1304,   180,   631,   568,   284,  1370,
     161,    72,   543,   574,   278,   576,   280,   281,   156,  1577,
      59,   163,  1469,   154,    72,   163,  1473,  1474,   163,   180,
     161,  1548,   155,   309,   163,   206,   105,  1127,   180,  1078,
    1487,   695,   543,   160,   633,   592,  1735,   161,   312,   163,
     161,   781,   155,   317,   957,   226,   161,   155,   229,   323,
     231,   155,   101,   139,   180,   163,   171,   955,  1441,   180,
     631,   825,   633,   149,   161,   136,   637,   161,   639,   637,
     149,  1397,  1298,  1885,  1400,  1401,  1386,   648,   136,   161,
     637,   652,   356,   180,   155,   860,   826,   361,   159,   363,
     108,   109,   633,   274,   275,   161,   637,   155,   781,   531,
     149,   159,   832,   163,   285,   845,   852,   853,   161,   137,
     161,   843,   895,   684,     1,   161,    72,   843,   299,   300,
     180,   302,   633,   869,    72,   857,   307,   180,   531,   180,
      72,  1012,   703,  1852,   531,    75,   410,  1018,   166,   167,
     158,   847,   159,   826,   637,   637,   161,   164,  1029,   161,
     207,    58,    92,   334,    61,    62,   780,    64,   159,   245,
     180,   342,   343,   164,    72,   180,   347,   285,   180,  1462,
      72,   155,    59,   744,  1983,   746,     3,   748,  1549,   163,
     136,   752,   300,    69,   755,   161,   170,   273,   136,     3,
     464,   955,   161,   939,   136,   171,   377,   158,   284,   155,
    1571,  1649,   171,   159,   161,   386,  1654,   155,   389,   780,
     779,   159,   155,   155,  1782,   161,  1784,   159,   492,   157,
     494,   495,    72,   309,   162,   171,   157,   163,   136,   278,
      85,  1950,  1759,  1291,   136,   157,   510,   168,   169,  1744,
     162,   155,   558,    98,   179,  1964,   101,   155,   155,   134,
     105,   159,   139,   155,   825,   134,   155,   159,   157,   830,
     159,   832,   149,   312,     1,    72,   163,     4,   584,   543,
     155,  1240,   177,   844,   159,   591,   155,  1017,   155,   595,
     159,   166,   167,   854,   637,   157,   136,   166,   167,   860,
     162,    72,   863,   157,   568,  1240,  1345,   354,   162,   573,
     357,   575,   759,   760,   761,   155,   592,   356,   489,   159,
    2029,   157,  1142,   370,   108,   109,   162,   374,  1017,   983,
     501,   502,    59,   597,   895,   599,   600,   896,   120,   136,
     158,   159,  1017,  1214,   162,   157,  1707,   611,   157,  1071,
     195,  1016,   161,    13,    14,    15,    16,    17,   155,   157,
     624,   637,   159,  1099,  1100,   136,   211,    72,   245,   633,
     157,   932,   933,   934,   101,   155,  1559,   157,   105,   159,
      72,   157,   157,   946,   155,  1150,   161,   934,   159,  1109,
    1110,   236,   656,   157,   658,   659,   273,  1332,   157,  1302,
    1761,   155,  1484,    47,    48,   576,    50,   284,  1855,    72,
    1298,    55,    72,   157,   158,  1776,   680,   681,  1154,   264,
     168,   169,   149,   687,     3,    13,   160,  1298,   989,   155,
    1152,   136,   309,   278,    13,    14,    15,    16,    17,   486,
     161,   155,  1481,   157,   136,   159,     3,  1646,  1647,  1648,
     155,   934,   934,   492,   159,    22,    13,    14,    15,    16,
      17,   128,   129,   155,  1335,  1336,  1337,   159,   639,    72,
     155,  1342,  1343,   136,   134,   155,   136,   648,   161,  1040,
     157,   652,    72,     1,   161,  1046,     4,  1178,   101,   161,
    1449,   163,   155,    72,  1307,   155,   159,   155,   157,   159,
      88,    62,   161,   155,   157,   155,   166,   167,   161,   159,
     180,   134,  1285,   684,  1075,    72,   592,  1078,   106,   160,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     577,  1570,   155,   136,  1895,    72,   159,    72,   157,  1900,
     159,    59,  1103,   166,   167,   106,   136,   163,  1109,  1110,
     111,   278,   155,   114,   163,   116,   159,   136,   157,  1292,
    1293,   637,   161,    72,  1205,   155,  2013,    85,   637,   159,
     132,   133,   611,   744,  1656,   746,   157,   748,   654,   136,
     161,   752,  1143,  1279,   755,   624,   160,   161,   635,   157,
    1279,   934,   155,   161,   157,  2042,   159,  1327,   637,   136,
      89,   136,   130,   131,  1279,   163,   653,   654,   157,   780,
      58,  1431,   161,    61,    62,   921,    64,   155,   155,   464,
     155,   139,   159,  1298,   159,   143,  2073,   136,   157,   154,
     677,   149,   161,  1196,   152,   155,   157,  1326,  1327,   159,
     161,   680,   157,   690,  1366,   155,   155,   175,   176,   159,
     159,   155,  1327,   160,   825,   159,  1217,  1311,   934,  1220,
    1221,  1222,   113,   114,   115,   116,   117,  1214,  1229,   163,
    1400,   157,   157,   844,   170,   161,   161,    13,    14,    15,
      16,    17,   179,   854,  1406,  1407,  1247,   155,   206,   120,
    1406,  1407,  1253,   957,   157,   155,  1979,   134,   543,   159,
    1247,  1397,   160,   161,   968,  1401,   267,  1268,   157,  1272,
    1271,  1272,   161,   977,  1275,   592,   157,   161,   155,   157,
     161,  1282,   159,   161,  1285,  1272,  1765,   245,   155,   166,
     167,  1214,  1214,   155,   781,   155,    72,   149,   150,   151,
    1271,  1272,   179,   135,   262,   123,   264,   125,   126,   127,
     826,   155,   157,   314,   157,   273,   161,   172,   161,   171,
     637,   932,   933,   934,  1247,  1247,   284,  1328,    13,    14,
      15,    16,    17,  1202,  1203,  1204,   302,   155,   825,   826,
     158,   159,   167,   157,  1345,   163,   164,   161,   165,   307,
     157,   309,  1469,  1354,   161,   177,  1473,   358,   158,   360,
     136,   362,  1532,  1444,  1445,  1446,  1455,   157,  1449,  1450,
      13,    14,    15,    16,    17,   333,   134,   157,   989,   157,
    1084,   161,   149,   150,   151,   157,  1387,    72,   157,   161,
     155,  1553,   161,   157,   161,   227,   157,  1553,   157,  1774,
     161,   157,   161,  1532,   171,   161,  1110,   157,   157,   410,
     157,   161,   161,   180,   161,   157,  2106,  1532,   934,  1730,
    2110,   160,   161,  1469,   157,   934,  1455,  1473,   157,    72,
     155,  1214,    13,    14,    15,    16,    17,   946,    13,    14,
      15,    16,    17,   159,  1447,    13,    14,    15,    16,    17,
      18,   136,   137,   160,  1455,   934,   166,   167,  1459,  1460,
    1660,   149,   150,   151,  1247,  1660,   160,   161,   955,   137,
     637,   958,   137,   161,   160,   161,   308,  1660,   957,   161,
    1481,   161,  1469,   171,   160,   161,  1473,  1474,  1668,  1669,
    1670,    72,   180,   136,   137,   453,   162,    72,  1214,   162,
    1487,   161,   162,  1504,  1505,   766,   767,   768,   769,   510,
    1660,   160,   161,  1514,  1455,  1660,   160,   161,    91,    92,
    1660,   160,   161,   161,   482,   149,   150,   151,   160,   161,
    1017,  1247,   155,  1660,     4,     5,     6,     7,     8,     9,
      10,    11,    12,  1514,  1660,   503,   179,   171,  1660,   157,
    1551,   160,   161,   134,  1557,   136,   180,  1660,   157,   134,
    1649,   136,   160,   161,   157,  1654,   157,  1271,   157,  1570,
     160,   161,   157,  1662,   155,   157,  1577,   157,   159,   157,
     155,  1582,  1583,  1584,   159,   166,   167,   419,   589,   160,
     161,   166,   167,   559,   160,    65,  1660,   155,  1302,   160,
     161,   160,   161,   179,  1308,   159,  1217,   160,   161,  1220,
    1221,  1222,   160,   161,   161,   162,   157,   934,  1229,   163,
    1649,   160,   161,  1823,   161,  1654,   160,   161,  1823,    77,
      78,   161,   162,  1662,   592,   163,  1247,  1355,  1356,   163,
    1823,   163,  1253,   163,  1972,    70,   762,   763,  1649,   764,
     765,   770,   771,  1654,  1511,  1512,   160,  1268,   155,  1660,
      78,  1662,  1669,  1670,  1275,   180,   160,  1978,   634,    18,
    1671,  1282,   179,  1823,   163,  1888,  1763,  1764,  1823,   637,
     163,   180,   157,  1823,  1687,   157,  1469,  1196,   180,  1690,
    1473,  1474,   163,     3,   163,  1696,  1823,   160,  1214,   160,
    1687,    18,   160,  2046,  1487,  1214,   160,  1823,  1649,   154,
     157,  1823,  1885,  1654,   157,  2043,   157,  1328,   157,   157,
    1823,  1662,   157,   157,   157,   557,  1725,   157,    22,   157,
     157,  1247,   154,   565,   157,  1214,   160,  1826,  1247,  1740,
     154,    70,   163,  1746,   163,    18,   163,   157,   161,   157,
     179,   583,  1456,  1469,   157,   157,   157,  1473,  1474,   163,
     157,   161,   594,  1272,  1765,   163,   157,   934,  1247,   157,
     160,  1487,   161,   157,  1936,    72,   161,   157,   157,   946,
    1936,  1782,   157,  1784,    57,    58,    59,    60,    61,    62,
      63,    64,  1271,   161,   157,   157,   106,  1826,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   160,   106,
    1514,  1298,   157,   110,   111,   112,   113,   114,   115,   116,
     117,   157,  1823,  1302,   134,  1826,   157,   160,   160,   157,
     157,   797,  2045,   157,  1835,  1836,   157,   134,   157,   136,
    1327,  1842,   157,   157,  1933,   155,   156,   813,  1459,  1460,
     157,   817,   162,  1854,   157,   157,   166,   167,   155,   156,
     161,   157,   157,  1864,   157,  1866,   154,    14,  1855,   166,
     167,   161,   161,  1872,   161,   707,  1877,   154,  1879,  1880,
    1881,   713,   162,   155,   155,  1826,  1887,  1888,   155,    78,
     722,  1972,   155,  1504,  1505,   155,   155,  1214,   155,   155,
     161,  2101,   180,   154,  1933,  2067,  2101,  2069,   162,   741,
     160,  2067,   160,  2069,   180,   180,   163,   106,  2101,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   154,
    1247,   163,  1933,   179,   161,   180,   157,   157,  1447,  1940,
    1551,   157,   157,  1944,   157,   157,  2108,   161,  1949,   160,
     160,  2101,  2108,  1469,   157,   160,  2101,  1473,  1474,   157,
    1469,  2101,  2043,   157,  1473,  1474,   157,   157,   160,   154,
     154,  1487,   155,   155,  2101,  2137,   934,  1976,  1487,    80,
     180,   180,  1686,   180,   180,  2101,  1271,  1272,   946,  2101,
    1469,   180,  1933,   180,  1473,  1474,    92,   154,  2101,   180,
     180,   155,   155,  2004,    90,   157,   154,   163,  1487,   154,
     161,   161,   123,  2102,   160,  2016,   160,  2179,   160,  2020,
     154,   160,   154,  2179,   157,   162,  2013,   162,   154,  1196,
    2101,  2101,   157,  2034,   157,  1514,  2101,  2101,    13,    14,
      15,    16,    17,    18,  2045,  1532,  2047,  1214,  1557,   157,
    2139,   160,   157,   160,   157,  2042,   180,   154,  1016,   157,
    1671,    13,    14,    15,    16,    17,    18,   207,   157,   154,
     902,   161,  1855,  2102,   162,  2076,   157,   161,   155,  1690,
    1247,   157,   155,   155,   916,  1696,  2073,   157,   920,   160,
     154,   160,   924,    13,    14,    15,    16,    17,    18,   160,
    2101,  2102,   157,   154,  1271,  1272,     1,  2106,   157,     4,
    2139,  2110,  2111,  2114,   157,   157,   160,   180,  2119,    75,
     180,   221,  1826,   180,  1090,    75,   180,   154,  1094,  1740,
     155,  2102,   180,   157,  1838,   155,   154,   160,  2139,  1855,
     154,   160,  2141,   154,   157,   157,  2147,  1113,   157,  2150,
      75,  2152,  1469,   159,  1120,   171,  1473,  1474,    75,   108,
     158,  2102,    75,   180,    59,  2164,   171,   162,  2139,  2168,
    1487,   154,  2173,   180,   154,   154,   180,   171,  1687,   156,
     171,    76,   154,  2184,  2183,   106,   162,   155,   161,   171,
      85,   156,  2193,   171,   180,   180,   160,    75,  2139,   157,
    1166,   156,   162,    98,  1170,  1323,   101,  1686,  1174,   157,
     105,     4,     5,     6,     7,     8,     9,    10,    11,    12,
    1924,   157,   154,   154,  1835,  1836,   155,   157,  1774,  1514,
    2013,  1842,   772,   180,   157,   180,   180,  1746,  1196,   774,
     731,   773,   775,  1854,  1235,   776,   452,  1205,   143,  2152,
    1473,  1247,  2069,  1864,   149,  1866,  1214,   152,  1863,  2042,
     155,   156,  2098,  1855,  1957,  2058,  1877,  1734,  1879,  1880,
    1881,  2136,  1718,   168,  1487,  1718,  1887,   106,  2168,  2043,
    1447,   110,   111,   112,   113,   114,   115,   116,   117,  1247,
    2073,  2111,  2042,  1275,    49,   114,  1933,  2013,   193,   269,
     195,   196,  1469,  2002,  1450,  1316,  1473,  1474,   860,  1268,
     648,   206,   207,   520,  1521,     0,   211,  1746,   797,   797,
    1487,   703,    -1,   797,  1638,    -1,  2042,   156,    -1,  1940,
     159,    -1,    -1,  1944,    -1,   230,    -1,    -1,  1949,  1855,
     235,   236,  2046,    -1,   106,    -1,  1855,  1514,   110,   111,
     112,   113,   114,   115,   116,   117,   118,  2073,    -1,  1838,
     122,   256,   124,    -1,    -1,    -1,    -1,  1199,    -1,   264,
      -1,    -1,    -1,    -1,    -1,    -1,  1855,    -1,    -1,    -1,
      -1,  1213,    -1,   278,    -1,    -1,    -1,    -1,    -1,    -1,
    1557,    -1,    -1,  2004,   156,    -1,    -1,   159,    -1,    -1,
      -1,  1233,    -1,    -1,    -1,  2016,    -1,    -1,  1240,  2020,
      -1,   511,    -1,   513,    -1,    -1,   311,    -1,    -1,    -1,
      -1,    -1,   317,  2034,    -1,    -1,    -1,    -1,   323,   324,
     325,    -1,    -1,    -1,    -1,  2139,  2047,   577,   333,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    -1,    -1,
    1416,    -1,    -1,    -1,  1420,    -1,    -1,    -1,  1424,   354,
     355,   356,   106,    -1,    -1,  2076,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   370,    -1,    -1,   122,   374,
     124,    -1,    -1,    -1,    -1,  1972,  1444,  1445,  1446,  1447,
    1448,  1449,  1450,    -1,    -1,    -1,    -1,  2013,    -1,    -1,
      -1,    -1,    -1,  2114,  2013,    -1,    -1,    -1,  2119,    -1,
      -1,  1469,   156,    -1,   654,  1473,  1474,    -1,    -1,  1686,
    1687,    -1,    88,    -1,   419,    -1,  2042,    -1,    -1,  1487,
      76,    -1,    -1,  2042,  2013,    -1,  2147,    -1,  1855,  2150,
     106,  2152,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    98,    -1,    -1,    -1,  2043,  2073,   453,    18,
      85,   456,  2173,  2042,  2073,    -1,    -1,    -1,    -1,   464,
      -1,  1537,    -1,  2184,    -1,    -1,    -1,    -1,    72,  1746,
      -1,    -1,  2193,    -1,    -1,    -1,   302,   482,    -1,   155,
      -1,   486,    -1,    -1,  2073,   490,    -1,   492,    -1,  1557,
      -1,    -1,    61,    62,    63,    64,    -1,    -1,   503,   155,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,  1591,    -1,   152,    -1,   524,
      -1,  1463,  1464,    -1,  1600,    -1,   531,    -1,  1604,    -1,
      -1,   781,   136,    -1,    -1,    -1,    -1,   106,   543,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   193,    -1,
      -1,  1838,    -1,   568,    -1,   570,   571,    -1,    -1,   574,
      -1,  1513,   577,    -1,   230,    -1,   826,    -1,  1855,   235,
     236,    -1,    -1,    -1,    -1,    -1,  2013,    -1,    -1,    -1,
     159,    -1,    -1,    -1,    -1,   845,    -1,   847,    -1,    -1,
     256,   851,   852,   853,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2042,    -1,    -1,    -1,   869,
      -1,    -1,    -1,    -1,    -1,    -1,   631,    -1,   633,   264,
     635,    -1,   637,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,  2073,    -1,   653,   654,
     860,   656,    -1,   863,    -1,   311,    -1,    -1,    -1,   664,
      -1,    -1,    -1,   668,    -1,    -1,    -1,    -1,   324,   325,
      -1,    -1,   677,    -1,    -1,    -1,    -1,    -1,  1746,    -1,
     930,    -1,   687,    -1,    -1,   690,    -1,    -1,    -1,   939,
      -1,    -1,    -1,    -1,   699,    -1,    -1,    -1,   333,    -1,
      72,    -1,   707,    -1,    -1,   710,   711,    -1,   713,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   722,    -1,   354,
     725,   726,   727,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   559,   106,    -1,  2013,    -1,   110,   111,
     112,   113,   114,   115,   116,   117,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    89,    -1,
      -1,    -1,   134,    -1,   136,  2042,    -1,    -1,    -1,    -1,
      -1,    -1,    62,    -1,    -1,  1717,   781,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,  1855,    -1,    -1,
      -1,    -1,   797,   798,   166,   167,  2073,    -1,   158,   130,
     805,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   634,    -1,
     170,   101,    -1,    -1,    -1,    -1,    -1,    -1,   453,    -1,
     825,   826,    -1,   113,   114,   830,    -1,   832,    -1,    -1,
    1040,    -1,    -1,    -1,   490,    -1,  1046,    -1,   664,    -1,
     845,    -1,   847,    -1,    -1,    -1,   851,   852,   853,  1099,
    1100,   486,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   869,  1075,   156,    -1,  1078,    -1,
      -1,    -1,    -1,    -1,   106,   531,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   543,    -1,   524,
     895,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,  1154,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   568,    -1,    -1,    -1,    -1,    -1,   574,    -1,
      -1,   211,    -1,    -1,    -1,    -1,   180,    -1,    -1,   934,
      -1,    -1,    -1,  1143,   939,   570,   571,    -1,    -1,    -1,
      -1,   946,   947,    -1,    -1,  2013,    -1,    -1,   180,    -1,
     955,    -1,   957,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   968,  1217,    -1,    -1,    -1,    -1,    -1,
      -1,   797,    -1,    -1,  2042,   631,  1229,   633,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2061,    -1,   813,   278,   106,
     321,   817,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,   830,  2073,    -1,    72,    -1,   106,
      -1,  1016,  1017,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   312,    -1,    -1,    -1,    -1,   317,    -1,    -1,
      -1,    -1,    -1,   323,    -1,    -1,    -1,    -1,   155,   156,
      -1,   106,   677,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,     1,    -1,    -1,     4,
      -1,    -1,   159,    -1,   699,    -1,   356,    -1,    -1,   134,
      -1,   136,   707,    -1,    -1,    -1,   711,    -1,   713,  1084,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
     155,   156,    -1,    -1,  1099,  1100,    -1,    -1,  1103,    -1,
      -1,   166,   167,    -1,   930,  1110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,
     410,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,  1345,    -1,    -1,    -1,    -1,
      85,    -1,    -1,    72,  1354,    -1,    -1,  1397,    -1,  1154,
    1400,  1401,    -1,    -1,   485,    -1,   487,    -1,    -1,    -1,
     105,    -1,    -1,    -1,    -1,   496,    -1,    -1,    -1,    -1,
     805,    -1,    -1,    -1,   830,    -1,   832,   106,   163,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,  1196,    -1,    -1,   139,    -1,    -1,    -1,   143,    -1,
    1205,    -1,   492,    -1,   149,   134,   106,   136,    -1,  1214,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     510,  2163,   122,   168,   124,    -1,   155,   156,    -1,    -1,
     159,    -1,    -1,  2175,    -1,  1240,    -1,   166,   167,   895,
      -1,    -1,  1247,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     195,  1504,  1505,    -1,    -1,    -1,   156,    -1,    -1,   159,
      -1,   206,   207,    -1,  1090,    -1,  1271,  1272,  1094,    -1,
      -1,  1481,    -1,    -1,  1279,    -1,    -1,  1103,    -1,    -1,
    1285,    -1,    -1,    -1,    -1,   575,  1291,  1113,    -1,    -1,
      -1,   236,    -1,  1298,  1120,    -1,    -1,    -1,    -1,    -1,
     245,    -1,    -1,    -1,    -1,   246,    -1,   597,    -1,    -1,
      -1,   256,   947,    -1,    -1,    -1,   261,   262,    -1,   264,
      -1,   611,  1327,    -1,    -1,    -1,    -1,  1332,   273,    -1,
      -1,    -1,    -1,    -1,   624,    -1,    -1,    -1,    -1,   284,
    1166,    -1,   287,    -1,  1170,    -1,   291,    -1,  1174,    -1,
      -1,   296,    -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,
    1570,    -1,   307,    -1,   309,    -1,   656,  1577,   313,    -1,
      -1,    -1,  1582,  1583,  1584,    -1,    -1,   106,    -1,   108,
     325,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     680,    -1,  1397,    -1,    -1,  1400,  1401,   687,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,   354,
      -1,   106,   357,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,   370,    -1,    -1,    -1,   374,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1690,    -1,  1444,
    1445,  1446,  1447,  1696,  1449,  1450,    -1,  1103,    -1,    -1,
    1455,  1456,    -1,  1109,  1110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,  1469,    -1,    -1,    -1,  1473,  1474,
      -1,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1484,
      -1,    -1,  1487,    -1,   106,    -1,    -1,  1740,   110,   111,
     112,   113,   114,   115,   116,   117,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,  1514,
      57,    -1,   843,    -1,    -1,   456,    -1,    -1,    65,    66,
      67,    68,    -1,    -1,   134,    -1,   136,  1532,    -1,    -1,
      -1,    -1,   473,    -1,    -1,   476,    -1,   482,    -1,    -1,
      -1,   486,    -1,    -1,    -1,   155,   156,    -1,    -1,   159,
      -1,    -1,  1557,    -1,  1559,  1765,   166,   167,   503,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,  1782,    -1,  1784,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1835,  1836,    -1,    -1,    -1,    -1,    -1,  1842,
    1416,    -1,    -1,    -1,  1420,   536,    -1,    -1,  1424,    -1,
      -1,  1854,    -1,    -1,    -1,    -1,   937,   938,    -1,   899,
      -1,  1864,   159,  1866,   559,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1877,    -1,  1879,  1880,  1881,  1285,
     177,   106,   577,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,  1649,  1650,    -1,   592,    -1,  1654,
      -1,  1656,    -1,    -1,    -1,  1660,  1291,  1662,    -1,    -1,
      -1,    -1,    -1,  1298,    -1,    -1,   106,   957,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,  1686,  1687,    -1,   159,    -1,    -1,  1940,    -1,   634,
     635,  1944,   637,    -1,   134,    -1,  1949,    -1,    -1,    -1,
      -1,    -1,  1033,    -1,    -1,    -1,    -1,    -1,   653,   654,
      -1,  1537,    -1,    -1,    -1,   155,   156,    -1,    -1,   664,
      -1,    -1,   162,   668,    -1,    -1,   166,   167,    -1,    -1,
      -1,  1387,   677,    -1,    -1,    -1,    -1,   682,    -1,    -1,
      -1,  1746,    -1,    -1,    -1,   690,  1077,    -1,   106,    -1,
      -1,  2004,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,  2016,   122,  1591,   124,  2020,    -1,  1774,
      -1,    -1,    -1,    -1,  1600,    -1,    -1,    -1,  1604,    -1,
     106,  2034,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,  1126,    -1,  1128,   156,  1455,
      -1,   159,    -1,  1808,  1809,    -1,    -1,    -1,   134,  1140,
    1141,    -1,    -1,    -1,    -1,  1146,  1147,    -1,  1823,    -1,
      -1,  1826,    -1,  2076,  1155,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,  1838,    -1,    -1,   781,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1484,
    1855,  1182,   797,   798,  1185,    -1,   797,   798,    -1,    -1,
      -1,  2114,    -1,    -1,    -1,    -1,  2119,   808,   813,    -1,
     811,    -1,   817,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     825,   826,    -1,  1888,    -1,   830,    -1,   832,    -1,    -1,
      -1,    -1,    -1,    -1,  2147,    -1,    -1,  2150,    -1,  2152,
     845,    -1,   847,    -1,    -1,    -1,   851,   852,   853,    -1,
      -1,    -1,    48,    -1,    -1,    -1,  1247,    -1,    -1,  1924,
    2173,    -1,    -1,    -1,   869,    -1,    -1,    -1,  1933,    -1,
      -1,    -1,    -1,    -1,   875,    -1,    -1,    -1,    -1,    -1,
      76,   882,    -1,    -1,    -1,   886,    -1,  1278,    -1,   890,
      -1,    -1,    -1,    -1,    -1,  1286,    -1,  1288,  1289,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1297,  1972,  1299,    -1,
    1301,    -1,    -1,    -1,    -1,    -1,    -1,  1308,    -1,    -1,
      -1,  1271,    -1,    -1,    -1,   930,    -1,   123,    -1,   934,
      -1,    -1,    -1,  1649,   939,    -1,    -1,  2002,  1654,    -1,
     136,   946,   138,    -1,  1660,    -1,  1662,    -1,  2013,    -1,
     955,    -1,  1302,   958,    -1,    -1,    -1,    -1,    -1,    -1,
     965,  1656,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   169,    -1,   171,   152,  2042,  2043,    -1,
    2045,  2046,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1380,
    1381,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     196,    -1,    -1,   179,    -1,    -1,    -1,    -1,  2073,    -1,
      -1,  1016,  1017,    -1,  1405,    -1,    -1,    -1,    -1,    -1,
      -1,  1412,   104,  1414,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,  2101,  2102,    -1,   235,
      -1,    -1,    -1,   239,    -1,    -1,   242,   243,    -1,  1440,
     246,    -1,    -1,   249,   250,    -1,   252,    -1,   254,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,     1,    -1,   155,  2139,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,  1084,    -1,  1090,    -1,  1437,    -1,  1094,
      -1,    -1,    -1,    -1,  1099,  1100,    -1,    -1,  1103,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1456,  1823,  1113,   156,
    1826,    -1,   159,    -1,    -1,  1120,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,   324,    -1,
     106,   327,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    73,    -1,  1536,    -1,    -1,    -1,  1154,
      -1,    -1,  1543,    -1,  1545,   351,  1157,    -1,    -1,  1160,
      -1,  1166,    -1,  1164,  1514,  1170,    -1,    -1,    -1,  1174,
     366,    -1,  1888,   103,   104,  2061,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,  1196,   122,   123,   124,    -1,   126,   127,    -1,    -1,
    1205,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,  1214,
      -1,    72,    -1,    -1,    -1,    -1,    -1,  1933,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
    1621,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,  1247,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,  1272,     4,    -1,
      -1,    -1,    -1,   134,  1279,   136,    -1,   473,    -1,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,  1298,   155,   156,    -1,    -1,   159,    -1,
      -1,    -1,  1307,    -1,    -1,   166,   167,    -1,   106,   134,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,  1326,  1327,    59,    -1,    -1,    -1,    -1,    -1,  2045,
     155,   156,    -1,    -1,   159,   531,  1686,    -1,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      -1,   547,    -1,    -1,   179,    -1,    -1,    -1,    -1,    -1,
     158,    -1,    -1,  1754,  1755,   101,    -1,    -1,    -1,   105,
      -1,  1372,    -1,    -1,    -1,    -1,    -1,  1768,    -1,    -1,
      -1,    -1,  1383,    -1,    -1,  2101,  2102,    -1,    -1,    -1,
      -1,    -1,  1397,    -1,    -1,  1400,  1401,    -1,    -1,    -1,
      -1,    -1,    -1,   139,    -1,    -1,    -1,   143,    -1,    -1,
      72,  1416,    -1,   149,    -1,  1420,   152,    -1,    -1,  1424,
     156,    -1,    -1,  2139,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,   170,   631,    -1,    -1,    -1,  1444,
    1445,  1446,  1447,  1448,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   193,    -1,    -1,
      -1,    -1,    -1,    -1,  1469,    -1,    -1,    -1,  1473,  1474,
     206,   207,   134,    -1,   136,   211,   672,   673,    -1,    -1,
      -1,    -1,  1487,    -1,    -1,    -1,    -1,    -1,  1838,    -1,
      -1,    -1,    -1,   155,   156,    -1,   692,    -1,   694,    -1,
      -1,    -1,    -1,    -1,   166,   167,    -1,    -1,   106,   245,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,   262,  1532,   264,    -1,
      -1,    -1,  1537,    -1,    85,    -1,   134,   273,    -1,    -1,
      -1,    -1,   278,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,  1557,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,   159,    -1,   299,    -1,    -1,   302,    -1,   166,   167,
      -1,   307,    -1,   309,    -1,    -1,   312,   313,    -1,    -1,
      -1,   317,    -1,    -1,    -1,    -1,  1591,   323,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1600,    -1,   333,    -1,  1604,
      -1,   152,    -1,    -1,    -1,   801,   802,    -1,    -1,    -1,
      -1,    -1,   808,    -1,    -1,    -1,    -1,   168,   354,    -1,
     356,   357,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,   370,    -1,    -1,   833,   374,    -1,
     836,   837,   193,   839,    -1,   841,   842,    -1,    -1,  1650,
      -1,    -1,    -1,  2044,    -1,    -1,   207,  1662,    -1,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,  1687,   419,    -1,    -1,   882,    -1,    -1,   134,
     886,   136,    -1,    -1,   890,   106,  2046,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,  2099,    -1,
     155,   156,    -1,   264,    -1,    -1,    -1,   453,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,  2120,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,  1746,    -1,    -1,  2135,    -1,   482,   158,    -1,    -1,
     486,    -1,    -1,    -1,    -1,    -1,   492,    -1,    -1,    -1,
      -1,    -1,    -1,    56,    57,    -1,    -1,   503,    -1,   106,
     966,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   333,    13,    14,    15,    16,    17,   524,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      93,    -1,    -1,   354,  1809,    -1,    -1,  1808,  1809,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,    -1,    -1,   559,    -1,    -1,    -1,    -1,    -1,   166,
     167,    -1,    -1,    -1,   570,   571,    -1,    -1,    -1,   575,
      -1,   577,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1855,   144,    -1,    -1,   147,    -1,   592,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,
      -1,    -1,    -1,    -1,    -1,   611,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   624,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   634,   635,
      -1,   637,   453,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,  1109,    -1,    -1,   652,   653,   654,    -1,
     656,    -1,    -1,  1924,    -1,   155,   156,    -1,   664,    -1,
      -1,    -1,    -1,   226,    -1,   486,   166,   167,    -1,    -1,
      -1,   677,    -1,    -1,   680,    -1,    -1,    -1,   684,    -1,
      -1,   687,    -1,    -1,   690,    -1,    -1,  1153,    -1,    -1,
      -1,  1157,    -1,   699,  1160,    -1,    -1,  1972,  1164,    -1,
      -1,   707,    -1,   524,   710,   711,    -1,   713,    -1,    -1,
      -1,   274,   275,    -1,    -1,    -1,   722,    -1,    -1,   725,
     726,   727,   285,    -1,    -1,    -1,    -1,  1998,    -1,    -1,
      -1,  2002,    -1,    -1,    -1,    -1,    -1,   300,  2013,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   570,
     571,    -1,    -1,    -1,    -1,   106,   577,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,  2042,  2043,    -1,
      -1,   334,    -1,    -1,  2045,   781,    -1,    -1,    -1,   342,
     343,    -1,    -1,   134,   347,    -1,  2061,    -1,    -1,    -1,
      -1,   797,    -1,    -1,    -1,    -1,    -1,    -1,  2073,   805,
      -1,    -1,    -1,    -1,   155,   156,    -1,   813,    -1,    -1,
      -1,   817,    -1,    -1,   635,   166,   167,    -1,    -1,   825,
     826,    -1,    -1,   386,   830,    -1,   389,  2102,    -1,    -1,
    2101,  2102,    -1,   654,    -1,    -1,    -1,    -1,  1304,   845,
      -1,   847,    -1,    -1,    -1,   851,   852,   853,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,     1,    -1,
      -1,    -1,    -1,   869,    -1,    -1,    -1,    -1,  2139,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   699,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   707,    -1,    -1,    -1,
     711,    -1,   713,   899,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,  1372,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    59,  1383,    -1,    -1,
    1386,    -1,  1388,  1389,   930,    -1,   489,    -1,   934,    -1,
      -1,    -1,    -1,   939,    -1,    -1,    -1,    -1,   501,   502,
     946,   947,    -1,   155,    -1,    -1,    -1,    -1,    -1,   955,
      -1,   957,   958,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     781,    -1,   105,    -1,    -1,   106,  1432,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,   985,
      -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   139,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   826,   149,    -1,    -1,    -1,
    1016,  1017,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   845,   168,   847,    -1,    -1,    -1,
     851,   852,   853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   869,  1515,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   639,    -1,    -1,    13,
      14,    15,    16,    17,  1090,   648,    -1,    -1,  1094,    -1,
      -1,    -1,    -1,  1099,  1100,    -1,    -1,  1103,    -1,    -1,
      -1,    -1,   245,    -1,    -1,    -1,    -1,  1113,    -1,    -1,
      -1,    -1,    -1,    -1,  1120,    -1,    -1,    -1,   939,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   947,    -1,    -1,    -1,
     273,    -1,    -1,  1599,    -1,    -1,    -1,    -1,    72,    -1,
      -1,   284,    -1,    -1,   287,    -1,    -1,    -1,  1154,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,
    1166,  1627,    -1,    -1,  1170,    -1,   309,    -1,  1174,    -1,
     313,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
    1196,    -1,    -1,    -1,  1660,    -1,  1017,    -1,    -1,  1205,
     134,  1667,   136,    -1,    -1,    -1,    -1,    -1,  1214,    -1,
      -1,   354,    -1,    -1,   357,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,   370,    -1,    -1,
      -1,   374,   166,   167,  1240,    -1,    -1,    -1,    -1,    -1,
      -1,  1247,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1271,  1272,    -1,    -1,    -1,
      -1,    -1,    -1,  1279,    -1,    -1,  1742,    -1,  1099,  1100,
      -1,   844,    -1,    -1,    -1,  1291,    -1,    -1,    -1,    -1,
      -1,   854,  1298,    -1,    -1,    -1,  1302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1326,  1327,    -1,  1789,  1790,    -1,  1332,    -1,    -1,    -1,
      -1,    -1,    -1,  1154,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   486,    -1,    -1,    -1,    -1,    -1,  1815,
    1816,    -1,    -1,    -1,    -1,    -1,    -1,  1823,    -1,    -1,
      -1,    -1,    -1,  1829,    -1,    -1,    -1,    -1,    -1,    -1,
     933,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1397,    -1,    -1,  1400,  1401,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1416,    -1,    -1,    -1,  1420,    -1,   559,    -1,  1424,    -1,
      -1,    -1,    -1,    -1,     1,    -1,   989,    -1,    -1,    -1,
      -1,  1437,    -1,    -1,   577,    -1,    -1,    -1,  1444,  1445,
    1446,  1447,  1448,  1449,  1450,    -1,    -1,    -1,    -1,   592,
    1456,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1279,    -1,
    1926,    -1,    -1,  1469,    -1,    -1,    -1,  1473,  1474,    -1,
    1291,    -1,    -1,    -1,    -1,    -1,    -1,  1298,  1484,    -1,
      -1,  1487,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   634,   635,    -1,   637,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1327,    -1,  1514,    -1,
     653,   654,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   664,    -1,    -1,    -1,    -1,  1532,    -1,   105,    -1,
      -1,  1537,  1998,    -1,   677,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1550,    -1,    -1,   690,    -1,    -1,
      -1,  1557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,    -1,  1397,    -1,    -1,  1400,
    1401,    -1,    -1,    -1,    -1,  1591,   185,    -1,    -1,    -1,
      -1,   168,    -1,    -1,  1600,    -1,    -1,    -1,  1604,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     207,    -1,    -1,    -1,    -1,  2101,    -1,    -1,   781,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1656,    -1,    -1,    -1,   797,    -1,    -1,  1220,  1221,  1222,
      -1,    -1,    -1,  1484,    -1,    -1,    -1,    -1,   245,    -1,
     813,    -1,    -1,    -1,   817,    -1,    -1,    -1,    -1,    -1,
    1686,  1687,   825,   826,    -1,    -1,    -1,   830,    -1,    -1,
    1253,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
      -1,    -1,   845,    -1,   847,    -1,    -1,   284,   851,   852,
     853,  1532,  1275,    -1,    -1,    -1,    -1,    -1,    -1,  1282,
      -1,    -1,    -1,    -1,    -1,   302,   869,    -1,    -1,    -1,
      -1,    -1,   309,    -1,    -1,    -1,   313,    -1,    -1,    -1,
    1746,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,  1774,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   354,    -1,    -1,
     357,    -1,    -1,    -1,    -1,    -1,    -1,   930,    -1,    -1,
      -1,   934,    -1,   370,    -1,    -1,   939,   374,    49,    -1,
      -1,    52,    -1,    54,   403,    56,    -1,    -1,   407,   408,
      -1,    -1,   955,    -1,    -1,   958,    -1,    -1,   417,   418,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1838,   432,   433,  1656,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1855,
      -1,    -1,   103,   104,   453,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,  1017,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,   486,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1459,  1460,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,   486,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1090,    -1,    -1,
      -1,  1094,    -1,    -1,    -1,    -1,  1099,  1100,    -1,    -1,
    1103,    -1,    -1,    -1,    -1,    -1,  1972,    -1,    -1,    -1,
    1113,    -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,    -1,
      -1,    -1,   559,    -1,    -1,    -1,    -1,    -1,  1551,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     577,    -1,    -1,    -1,    -1,    -1,    -1,  2013,    -1,    -1,
      -1,  1154,    -1,    -1,    -1,   592,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1166,    -1,    -1,    -1,  1170,    -1,    -1,
      -1,  1174,    -1,    -1,    -1,    -1,  2042,  2043,    -1,    -1,
    2046,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2061,    -1,   634,   635,    -1,
     637,    -1,    -1,    -1,    -1,    -1,    -1,  2073,    -1,    -1,
      -1,  1214,    -1,    -1,    -1,    -1,   653,   654,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   664,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     677,    -1,    -1,    -1,  1247,    -1,    -1,    -1,  1671,    -1,
      -1,    -1,    -1,   690,    -1,     3,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1272,
      -1,    -1,    -1,    -1,    -1,    -1,  1279,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1298,    -1,    -1,    -1,   758,
     759,   760,   761,   762,   763,   764,   765,   766,   767,   768,
     769,   770,   771,   772,   773,   774,   775,   776,    -1,    -1,
      -1,    -1,    -1,  1326,  1327,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   781,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     797,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   813,    -1,    -1,    -1,
     817,    -1,    -1,    -1,   843,    -1,    -1,    -1,   825,   826,
      -1,    -1,    -1,   830,  1397,    -1,    -1,  1400,  1401,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,
     847,    -1,    -1,  1416,   851,   852,   853,  1420,    -1,    -1,
      -1,  1424,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   869,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     208,    -1,    -1,    -1,  1887,    -1,  1469,    -1,    -1,    -1,
    1473,  1474,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1487,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   930,    -1,    -1,    -1,   934,    -1,    -1,
      -1,    -1,   939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   263,    -1,    -1,   955,    -1,
      -1,   958,    -1,    -1,    -1,    -1,    -1,     5,    -1,  1532,
      -1,    -1,    -1,    -1,  1537,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1012,    -1,   303,    -1,    -1,    -1,  1018,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   315,    -1,    -1,
    1029,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
    1017,    -1,    -1,    -1,   332,    -1,    -1,    -1,  1591,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,  1600,    -1,    -1,
      -1,  1604,    -1,    -1,    -1,   353,    -1,    -1,    -1,    -1,
      -1,    -1,  1071,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,  1090,    -1,    -1,   134,  1094,   136,    -1,
      -1,    -1,  1099,  1100,    -1,    -1,  1103,    -1,    -1,    -1,
      -1,    -1,   420,    -1,    -1,    -1,  1113,   155,    -1,    -1,
     158,   159,    -1,  1120,  1687,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   451,   419,    -1,    -1,    -1,    -1,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1154,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1166,
      -1,   479,    -1,  1170,    -1,    -1,   484,  1174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   206,   207,    -1,
      -1,    -1,    -1,    -1,    -1,  1214,   504,    -1,    -1,    -1,
     508,   509,    -1,    -1,   512,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1214,    -1,   527,
     239,    -1,    -1,    -1,    -1,    -1,    -1,   246,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   550,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1247,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1272,    -1,    -1,    -1,  1298,
      -1,    -1,  1279,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1855,    -1,    -1,   570,   571,    -1,    -1,    -1,
      -1,  1298,    -1,    -1,    -1,    -1,    -1,    -1,   327,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1335,  1336,  1337,   627,
      -1,    -1,    -1,  1342,  1343,    -1,    -1,    -1,    -1,  1326,
    1327,    -1,    -1,   641,    -1,   354,   355,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1366,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   374,    -1,   665,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1406,  1407,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1397,    -1,    -1,  1400,  1401,    -1,    -1,    -1,    -1,  1972,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1416,
     728,    -1,    -1,  1420,    -1,    -1,    -1,  1424,    -1,    -1,
      -1,    -1,   707,    -1,    -1,    -1,    -1,   456,   713,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   722,    -1,    -1,
    2013,    -1,    -1,    -1,   473,   474,    -1,   476,   477,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   741,   486,    -1,    -1,
      -1,   490,  1469,    -1,    -1,    -1,  1473,  1474,    -1,  2042,
    2043,    -1,    -1,    -1,   503,    -1,    -1,    -1,    -1,    -1,
    1487,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2061,    -1,
      -1,    -1,   777,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2073,    -1,   820,   532,   822,    -1,    -1,   536,    -1,    -1,
     828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1553,  1532,    -1,    -1,    -1,    -1,
    1537,    -1,    -1,    -1,    -1,    -1,    -1,   855,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   864,    -1,   577,    -1,
     868,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1591,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1600,    -1,   913,    -1,  1604,    -1,    -1,
     918,    -1,    -1,   632,    -1,    -1,   635,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   653,   654,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   664,    -1,    -1,    -1,   668,
      -1,    -1,    -1,    -1,    -1,    -1,   675,    -1,   677,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1687,    -1,   101,    -1,    -1,    -1,    -1,  1005,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1730,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,    -1,   156,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,   781,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   797,   798,
      -1,    -1,    -1,    -1,   193,    -1,    -1,    -1,    -1,   808,
     809,    -1,   811,   812,    -1,    -1,    -1,    -1,   207,    -1,
      -1,    -1,   211,    -1,    -1,    -1,   825,   826,   123,    -1,
      -1,   830,    -1,   832,   833,    -1,    -1,    -1,    -1,    -1,
     839,   136,    -1,   138,    -1,    -1,   845,    -1,   847,    -1,
      -1,    -1,   851,   852,   853,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     869,    -1,   871,    -1,   169,   264,   875,    -1,  1855,    -1,
      -1,    -1,    -1,   882,   883,    -1,    -1,   886,   887,   278,
      -1,   890,   891,    -1,    -1,    -1,    -1,    -1,    -1,   898,
      -1,    -1,    -1,    -1,  1192,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   323,    -1,    -1,  1936,    -1,    -1,
     939,   940,    -1,    -1,   333,    -1,    -1,   242,   243,    -1,
      -1,   246,    -1,  1241,   249,   250,    -1,   252,    -1,   254,
      -1,    -1,    -1,    -1,    -1,   354,    -1,   356,    -1,   968,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1978,
      -1,    -1,    -1,    -1,    -1,  1240,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1972,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1296,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1016,  1017,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     419,    -1,  1320,    -1,    -1,    -1,  2013,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   351,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   453,  2042,  2043,    -1,  2067,    -1,
    2069,   366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2061,  1084,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2073,   486,    -1,    -1,
    1099,  1100,    -1,   492,  1103,  1104,    -1,    -1,    -1,  2108,
      -1,  1110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   524,    -1,    -1,  2137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1154,    -1,    -1,  1157,  1158,
      -1,  1160,  1161,    -1,    -1,  1164,  1165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   473,    -1,
    2179,   570,   571,    -1,    -1,    -1,    -1,    -1,   577,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1469,  1470,    -1,    -1,  1473,  1474,
      -1,    -1,    -1,    -1,  1479,    -1,    -1,    -1,  1483,    -1,
    1485,    -1,  1487,    -1,    -1,  1523,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,
      -1,    -1,   547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   654,    -1,   656,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1279,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1298,
      -1,    -1,    -1,    -1,    -1,  1304,  1305,    -1,    -1,    -1,
     699,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   707,    -1,
      -1,   710,   711,    -1,   713,    -1,    -1,    -1,  1327,    -1,
      -1,    -1,    -1,   722,    -1,    -1,   725,   726,   727,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1643,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1372,  1373,    -1,    -1,   672,   673,    -1,
      -1,  1636,    -1,    -1,  1383,  1384,    -1,  1386,    -1,    -1,
      -1,    -1,   781,    -1,    -1,    -1,    -1,   692,  1397,   694,
      -1,  1400,  1401,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1682,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   826,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1701,  1702,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,   847,  1747,
    1748,    -1,   851,   852,   853,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1731,   114,    -1,    -1,
     869,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   801,   802,    -1,    -1,
      -1,    -1,    -1,   808,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,  1532,    -1,    -1,    -1,    -1,   833,    -1,
      -1,   836,   837,    -1,   839,    -1,   841,   842,    -1,    -1,
     939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   947,    -1,
    1559,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   957,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1870,    -1,    -1,  1840,    -1,   882,    -1,    -1,
      -1,   886,  1847,    -1,  1849,   890,    -1,  1852,  1853,    -1,
    1855,    -1,    -1,    -1,    -1,  1860,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1901,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1017,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1938,  1650,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,  1667,    -1,
      -1,   966,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1967,
      -1,    -1,    -1,  1971,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1957,    -1,    -1,    -1,    -1,  1962,  1963,    -1,
    1099,  1100,    -1,    -1,    -1,    -1,    -1,   354,    -1,   356,
     357,    -1,    -1,    -1,    -1,    -1,    -1,  1982,    -1,    -1,
      -1,    -1,    -1,   370,    -1,    -1,    -1,   374,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,    -1,    -1,  1154,  2021,    -1,  2023,    -1,
      -1,    -1,  2027,  2028,    -1,    -1,    -1,  2032,  2033,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1808,
    1809,    -1,    -1,    -1,  1109,    -1,  1205,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1824,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2094,
    2095,  2096,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   486,
      -1,  1240,    -1,    -1,    -1,   492,    -1,    -1,  1153,    -1,
      -1,    -1,  1157,    -1,    -1,  1160,    -1,    -1,    -1,  1164,
    2125,  2126,  2127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1271,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1279,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1291,    -1,    -1,    -1,    -1,    -1,    -1,  1298,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   559,    -1,    -1,  1924,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1932,    -1,    -1,    -1,    -1,  1327,    -1,
     577,    -1,    -1,  1332,    -1,    -1,   354,    -1,   356,   357,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   370,    -1,    -1,    -1,   374,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   611,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   634,   635,  1998,
    1999,    -1,    -1,  2002,    -1,    -1,    -1,    -1,  1397,  1304,
      -1,  1400,  1401,    -1,    -1,    -1,   653,   654,    -1,   656,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   664,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     677,    -1,    -1,   680,    -1,    -1,  2045,    -1,    -1,    -1,
     687,    -1,    -1,   690,    -1,  1444,  1445,  1446,    -1,    -1,
    1449,  1450,    -1,    -1,    -1,    -1,    -1,  1456,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1372,   486,    -1,
      -1,    -1,    -1,    -1,   492,    -1,    -1,    -1,  1383,    -1,
      -1,  1386,    -1,  1388,  1389,  1484,    -1,    -1,    -1,    -1,
      -1,    -1,  2101,  2102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1514,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,  1432,    -1,    -1,
    2139,    -1,    -1,  1532,   781,    -1,    -1,    -1,    -1,    -1,
      -1,   559,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     797,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   577,
      -1,    -1,    -1,    -1,    -1,   207,   813,    -1,    -1,    -1,
     817,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   825,   826,
      -1,    -1,    -1,   830,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   611,    -1,    -1,    -1,    -1,   845,    -1,
     847,    -1,    -1,    -1,   851,   852,   853,    -1,    -1,    -1,
    1515,    -1,    -1,    -1,    -1,    -1,   634,   635,    -1,    -1,
      -1,    -1,   869,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   653,   654,    -1,   656,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   664,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1656,    -1,   677,
     302,    -1,   680,    -1,    -1,    -1,    -1,    -1,    -1,   687,
      -1,    -1,   690,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   930,    -1,    -1,    -1,  1686,    -1,    -1,
      -1,    -1,   939,    -1,  1599,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   955,    -1,
     957,   958,   354,    -1,   356,   357,    -1,    -1,    -1,    -1,
      -1,    -1,  1627,    -1,    -1,    -1,    -1,    -1,   370,    -1,
      -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   781,    -1,    -1,    -1,    -1,    -1,    -1,
    1017,    -1,    -1,    -1,    -1,  1774,    -1,    -1,    -1,   797,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   193,
      -1,    -1,    -1,    -1,    -1,   813,    -1,    -1,    -1,   817,
      -1,    -1,    -1,   207,    -1,    -1,    -1,   825,   826,    -1,
      -1,    -1,   830,    -1,    -1,    -1,    -1,   221,    -1,   223,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,   847,
      -1,    -1,    -1,   851,   852,   853,    -1,  1742,    -1,  1838,
      -1,    -1,    -1,  1090,   486,    -1,    -1,  1094,    -1,    -1,
     492,   869,  1099,  1100,    -1,    -1,  1103,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1113,    -1,    -1,    -1,
      -1,    -1,    -1,  1120,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1789,  1790,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1154,    -1,    -1,
    1815,  1816,   930,    -1,    -1,    -1,    -1,   559,   322,  1166,
      -1,   939,    -1,  1170,  1829,    -1,    -1,  1174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   577,    -1,   955,    -1,   957,
     958,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   611,
      -1,    -1,    -1,  1972,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   634,   635,    -1,    -1,    -1,    -1,    -1,  1017,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   653,   654,    -1,   656,    -1,    -1,    -1,    -1,    -1,
      -1,  1926,   664,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1279,    -1,    -1,   677,    -1,    -1,   680,    -1,
      -1,    -1,    -1,    -1,  2043,   687,    -1,  2046,   690,    -1,
      -1,  1298,    -1,    -1,    -1,  1302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1090,    -1,    -1,    -1,  1094,    -1,    -1,  1326,
    1327,  1099,  1100,    -1,    -1,  1103,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1998,    -1,  1113,    -1,    -1,    -1,    -1,
      -1,    -1,  1120,    -1,    -1,    -1,    -1,    -1,    -1,   513,
      -1,    -1,    -1,    -1,    -1,   519,    -1,    -1,    -1,    -1,
     524,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1154,    -1,    -1,   781,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1166,    -1,
    1397,    -1,  1170,  1400,  1401,   797,  1174,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1416,
      -1,   813,    -1,  1420,    -1,   817,    -1,  1424,    -1,    -1,
      -1,    -1,    -1,   825,   826,    -1,    -1,    -1,   830,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   845,    -1,   847,    -1,    -1,    -1,   851,
     852,   853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   625,    -1,    -1,    -1,    -1,    -1,   869,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     654,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1279,    -1,   667,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1298,    -1,    -1,    -1,  1302,  1532,    -1,    -1,   930,    -1,
    1537,    -1,    -1,    -1,    -1,    -1,    -1,   939,    -1,    -1,
     704,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1326,  1327,
      -1,   715,    -1,   955,    -1,   957,   958,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   739,   740,    -1,    -1,   743,
      -1,   745,    -1,    -1,  1591,    -1,    -1,   751,    -1,   753,
     754,    -1,    -1,  1600,    -1,    -1,    -1,  1604,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1017,    -1,   781,    -1,  1397,
      -1,    -1,  1400,  1401,    -1,    -1,    -1,    -1,    -1,    -1,
     794,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1416,    -1,
      -1,   805,  1420,    -1,    -1,    -1,  1424,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   821,    -1,    -1,
      -1,    -1,   826,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1090,    -1,
      -1,    -1,  1094,   857,    -1,    -1,   860,  1099,  1100,    -1,
      -1,  1103,    -1,    -1,    -1,    -1,   870,    -1,    -1,    -1,
      -1,  1113,    -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   899,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1154,    -1,  1532,    -1,    -1,    -1,    -1,  1537,
      -1,    -1,    -1,    -1,  1166,    -1,    -1,    -1,  1170,    -1,
      -1,    -1,  1174,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    57,    -1,   947,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   957,   958,    -1,    -1,    73,    -1,    -1,
      -1,   965,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1591,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1600,    -1,    -1,    -1,  1604,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,  1017,    -1,    -1,    -1,    -1,   134,    -1,
      -1,  1025,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1034,    -1,    -1,   149,   150,   151,    -1,  1279,    -1,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,  1298,    -1,    -1,    -1,
    1302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1078,     4,     5,     6,     7,     8,
       9,    10,    11,    12,  1326,  1327,    -1,    -1,    -1,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,  1972,    55,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1150,    -1,  1152,    -1,
      -1,    -1,    -1,    -1,    -1,  1397,    -1,    -1,  1400,  1401,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1416,    -1,    -1,    -1,  1420,    -1,
      -1,    -1,  1424,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2043,    -1,    -1,  2046,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2061,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
    1234,  1235,    -1,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,   180,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
    1532,    -1,    -1,    -1,    -1,  1537,    -1,     1,  1302,    -1,
      -1,    -1,    72,    -1,  1308,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,  1323,
      -1,    -1,    -1,  1327,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,
      -1,  1345,    -1,    -1,  1348,    49,    -1,    -1,    52,  1591,
      54,    -1,    56,    -1,  1972,    -1,    -1,    -1,  1600,  1363,
      -1,    -1,  1604,    -1,    -1,    -1,   136,    71,    -1,    73,
      74,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,    -1,   101,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   126,   127,    -1,  2043,    -1,    -1,  2046,    -1,
     134,    -1,  1436,  1437,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2061,    -1,    -1,    -1,    -1,    -1,    -1,
     154,   155,    -1,    -1,   158,   159,    -1,    -1,  1462,   163,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   180,  1481,    -1,    -1,
    1484,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1532,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1541,  1542,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1570,    -1,    -1,    -1,
    1574,    71,    -1,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
      -1,   101,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1656,    -1,   154,   155,    -1,  1661,   158,   159,
      -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,   169,
     170,   171,    13,    14,    15,    16,    17,    -1,    -1,    20,
     180,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,  1722,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1972,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      18,  1765,    -1,    -1,    -1,    -1,    -1,    -1,  1772,    -1,
      -1,  1775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,   136,    54,  1801,    56,    -1,
      -1,  2043,    -1,    -1,  2046,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    73,    74,    -1,    76,  2061,
      -1,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,    -1,   101,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,  1979,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    71,    72,    73,    74,    -1,    76,    -1,    -1,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,   155,    -1,    -1,   158,   159,
      -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     180,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    71,
      72,    73,    74,    -1,    76,    -1,    -1,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,   163,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,   151,    -1,    -1,
      -1,   155,   156,   157,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   180,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,   151,    -1,    -1,    -1,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   180,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,   157,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    77,
      78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,   163,    -1,    -1,   166,   167,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    77,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,
      -1,    -1,   158,   159,    -1,     3,    -1,     5,    -1,    -1,
     166,   167,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,    -1,
     158,   159,    -1,     3,    -1,     5,    -1,    -1,   166,   167,
      10,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,    -1,    -1,    -1,   158,   159,
      -1,     3,    -1,     5,    -1,    -1,   166,   167,    10,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,    -1,    -1,    -1,   158,   159,    -1,     3,
      -1,     5,    -1,    -1,   166,   167,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,   108,
     109,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    13,    14,    15,    16,    17,   166,   167,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,   136,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
     157,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,
     163,    -1,    -1,   166,   167,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,   157,   158,   159,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,   157,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    71,
      -1,    73,    74,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    -1,
      -1,    93,    94,    95,    96,    97,    98,    99,    -1,   101,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,   163,    -1,   165,   166,   167,   168,   169,   170,   171,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    73,    74,    -1,    76,    -1,    -1,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    -1,    -1,    93,    94,    95,    96,    97,    98,
      99,    -1,   101,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   180,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,
      -1,    -1,   163,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,    -1,    -1,   163,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,   108,
     109,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,
      -1,    -1,    90,    -1,    92,    -1,    -1,    -1,    -1,   158,
      -1,    -1,    -1,    -1,   163,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
     168,   169,   170,   171,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,
     169,   170,   171,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,   157,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,   163,    -1,   165,   166,   167,   168,   169,   170,
     171,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,   163,    -1,   165,   166,   167,   168,   169,   170,   171,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,   162,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   108,   109,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   136,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   158,   159,    -1,    -1,    -1,   163,    -1,    -1,   166,
     167,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,   157,
     158,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   136,   137,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,   158,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    13,    14,
      15,    16,    17,    18,    72,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     108,   109,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    13,    14,    15,    16,
      17,    18,    72,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   108,   109,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
      -1,   108,   109,    -1,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    13,
      14,    15,    16,    17,    18,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,   108,   109,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   159,    -1,   108,   109,    -1,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    13,    14,    15,
      16,    17,    18,    72,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   108,
     109,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,   108,   109,    -1,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,   108,   109,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    13,    14,    15,
      16,    17,    18,    72,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   108,
     109,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,   108,   109,    -1,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    13,    14,    15,    16,    17,
      18,    72,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,   108,   109,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
     108,   109,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    13,    14,
      15,    16,    17,   166,   167,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,    13,    14,    15,    16,
      17,   166,   167,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
     158,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    13,    14,    15,    16,    17,
      18,    72,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,   108,   109,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
     158,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    72,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   106,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,   136,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    49,    55,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    49,   136,    -1,    52,   134,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,   151,    -1,    -1,    73,   155,   156,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,   163,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,   162,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
     163,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,    -1,
     157,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    57,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    49,   126,   127,    52,    -1,    54,    -1,    56,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    49,   126,
     127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    49,   126,   127,    52,    -1,    54,
      -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   182,   409,   410,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
     101,   105,   106,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   121,   134,   136,   155,   156,   158,   159,
     166,   167,   185,   186,   187,   202,   294,   295,   296,   297,
     298,   299,   300,   301,   302,   303,   304,   305,   308,   311,
     313,   314,   315,   316,   317,   318,   319,   320,   321,   322,
     324,   326,   327,   328,   330,   331,   335,   336,   337,   338,
     339,   341,   347,   348,   349,   350,   362,   367,   401,   404,
     414,   420,   422,   428,   432,   437,   438,   439,   440,   441,
     442,   443,   444,   470,   488,   489,   490,   491,     0,   182,
     106,   186,   202,   298,   300,   311,   314,   317,   327,   331,
     336,   120,   155,    58,    61,    62,    64,   155,   155,   365,
     426,   427,   428,   323,   324,   108,   109,   186,   381,   402,
     403,   381,   155,   414,   155,   155,     4,   106,   108,   109,
     315,   320,   321,   155,   155,   202,   427,   432,   438,   439,
     440,   442,   443,   444,   108,   338,   160,   182,   159,   301,
     311,   314,   437,   441,   487,   488,   491,   492,   180,   183,
     152,   163,   179,   223,   384,    89,   161,   421,   381,   161,
     161,   161,   180,   108,   109,   155,   202,   306,   307,   432,
     433,   434,   435,   436,   437,   441,   445,   446,   447,   448,
     449,   450,   451,   452,   453,   459,     3,    47,    48,    50,
      55,   329,     3,   159,   202,   300,   315,   319,   321,   332,
     337,   417,   437,   441,   491,    69,   298,   300,   314,   327,
     331,   336,   418,   437,   441,    65,   320,   320,   315,   321,
     309,   320,   321,   329,   348,   315,   320,   315,   158,   426,
     161,   183,   155,   163,   231,   426,   426,     3,   289,   290,
     305,   308,   314,   318,   319,   159,   311,   314,   489,   381,
     381,   414,   179,   314,   155,   202,   423,   432,   433,   437,
     446,   450,   159,   202,   491,   415,   416,    57,    65,    66,
      67,    68,   159,   177,   381,   390,   392,   396,   398,   399,
     337,    57,   157,   159,   202,   310,   314,   318,   326,   327,
     333,   334,   335,   336,   340,   347,   348,   367,   377,   379,
     470,   483,   484,   485,   486,   491,   492,   426,   108,   109,
     170,   186,   337,   366,   459,   428,   155,   397,   398,   155,
      13,    88,   106,   108,   109,   155,   185,   429,   430,   431,
     120,   188,   189,    49,    52,    54,    56,    73,   103,   104,
     106,   107,   118,   119,   122,   123,   124,   126,   127,   155,
     159,   165,   168,   169,   170,   171,   184,   185,   188,   190,
     193,   201,   202,   203,   204,   207,   208,   209,   210,   211,
     212,   213,   214,   215,   216,   217,   218,   219,   225,   337,
     157,   159,   201,   202,   218,   220,   311,   337,   382,   383,
     400,   487,   492,   429,   314,   438,   439,   440,   442,   443,
     444,   157,   157,   157,   157,   157,   157,   157,   108,   159,
     186,   311,   470,   489,   159,   166,   202,   220,   300,   301,
     310,   312,   314,   327,   334,   336,   374,   375,   376,   378,
     379,   483,   491,   160,   155,   437,   441,   491,   155,   161,
     106,   158,   159,   163,   185,   187,   220,   385,   386,   387,
     388,   389,    22,   385,   155,   381,   231,   155,   186,   423,
     186,   427,   432,   434,   435,   436,   445,   447,   448,   449,
     451,   452,   453,   314,   433,   446,   450,   161,   101,   425,
     159,   426,   467,   470,   425,   426,   426,   421,   289,   155,
     426,   467,   425,   426,   426,   421,   426,   426,   314,   423,
     155,   155,   313,   314,   311,   314,   160,   182,   311,   487,
     492,   339,   163,   421,   289,   381,   381,   384,   300,   319,
     419,   437,   441,   163,   421,   289,   402,   314,   327,   314,
     314,   108,   338,   108,   109,   186,   337,   342,   402,   137,
     186,   314,   371,   372,   376,   377,   380,   154,   182,   231,
     305,   180,   437,   450,   314,   182,   425,   155,   425,   183,
     220,   427,   432,   314,   155,   182,   381,   412,   163,   155,
     381,   163,   381,   137,   166,   167,   395,   157,   161,   381,
     399,   157,   426,   160,   182,   312,   314,   327,   334,   336,
     482,   483,   491,   492,   155,   159,   167,   179,   202,   470,
     472,   473,   474,   475,   476,   477,   494,   202,   340,   491,
     314,   334,   320,   315,   426,   157,   312,   314,   484,   312,
     470,   484,   186,   366,   459,   363,   163,   366,   390,   179,
     390,   429,   157,   161,   155,   157,   120,   155,   201,   155,
     155,   155,   204,   155,   201,   155,   106,   108,   109,   315,
     320,   321,   155,   201,   201,    19,    21,    85,   159,   168,
     169,   205,   206,   220,   227,   231,   350,   382,   491,   161,
     182,   155,   190,   159,   164,   159,   164,   123,   125,   126,
     127,   155,   158,   159,   163,   164,   204,   204,   172,   166,
     173,   174,   168,   169,   128,   129,   130,   131,   175,   176,
     132,   133,   167,   165,   177,   134,   135,   178,   157,   161,
     158,   182,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   179,   222,   223,   224,   155,   202,   463,
     464,   465,   466,   467,   157,   161,   157,   157,   157,   157,
     157,   157,   157,   155,   426,   467,   470,   155,   467,   470,
     155,   182,   155,   311,   489,   160,   182,   183,   159,   183,
     155,   167,   202,   432,   454,   455,   456,   457,   458,   459,
     460,   461,   462,   137,   491,   161,   183,   161,   183,   381,
     381,   155,   182,   182,   182,   159,   187,   182,   386,   162,
     161,   493,   385,   158,   159,   162,   389,   400,   155,   182,
     179,   432,   434,   435,   436,   445,   447,   448,   449,   451,
     452,   453,   157,   157,   157,   157,   157,   157,   157,   157,
     157,   157,   433,   446,   450,   426,   155,   179,   160,   182,
     384,   231,   421,   371,   384,   231,   423,   227,   383,   227,
     383,   423,   108,   159,   412,   231,   421,   425,   163,   163,
     421,   289,   412,   231,   421,   344,   345,   343,   163,   157,
     161,   157,   161,    70,   291,   292,   180,   166,   220,   182,
     432,   375,   414,   412,   381,   160,   182,   155,   394,   392,
     393,    78,   325,   186,   312,   470,   484,   314,   318,   491,
     371,   473,   474,   475,   160,   182,    18,   220,   314,   472,
     494,   426,   426,   470,   312,   482,   492,   314,   186,   312,
     484,   426,   163,   426,   366,    10,   165,   366,   368,   369,
     163,   157,   383,   157,   157,   430,   156,   194,   195,   196,
     220,   180,   382,   492,   190,   159,   382,   383,   382,   492,
     220,   382,   157,   382,   382,   382,   160,   182,   157,   168,
     169,   206,    18,   316,   157,   161,   157,   166,   167,   157,
     156,   220,   226,   220,   163,   220,   186,   220,   186,   118,
     159,   186,   194,   108,   109,   118,   159,   186,   350,   220,
     194,   186,   204,   207,   207,   207,   208,   208,   209,   209,
     210,   210,   210,   210,   211,   211,   212,   213,   214,   215,
     216,   162,   227,   180,   188,   159,   186,   220,   163,   220,
     371,   464,   465,   466,   314,   463,   426,   426,   220,   383,
     155,   426,   467,   470,   155,   467,   470,   371,   371,   182,
     182,   160,   160,   155,   432,   455,   456,   457,   460,    18,
     314,   454,   458,   155,   426,   476,   494,   426,   426,   494,
     155,   426,   476,   426,   426,   183,   219,   381,   375,   378,
     160,   378,   379,   160,   494,   494,   137,   373,   374,   375,
     373,   373,   381,   182,   218,   219,   220,   424,   493,   385,
     387,   154,   182,   157,   182,   373,   220,   157,   157,   157,
     157,   157,   157,   157,   157,   157,   155,   426,   467,   470,
     155,   426,   467,   470,   155,   426,   467,   470,   423,   188,
      22,   470,   220,   321,   337,   468,   231,   157,   157,   157,
     157,   157,   410,   411,   231,   154,   182,   412,   231,   421,
     411,   231,   163,   163,   163,   351,   137,   376,   377,   186,
     293,   381,    18,    71,    73,    74,    76,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    93,
      94,    95,    96,    97,    98,    99,   101,   108,   109,   121,
     155,   159,   227,   228,   229,   230,   231,   232,   233,   235,
     236,   245,   252,   253,   254,   255,   256,   257,   262,   263,
     266,   267,   268,   269,   270,   271,   272,   278,   279,   280,
     294,   314,   318,   381,   422,    70,   183,   183,   373,   161,
     413,   411,   157,   298,   300,   311,   405,   406,   407,   408,
     400,   179,   391,   391,   312,   484,   159,   166,   202,   220,
     337,   220,   314,   157,   157,   157,   157,     5,   314,   426,
     472,   364,   368,   366,   163,   337,   161,   493,   381,   368,
     163,   157,   157,   161,   157,   157,   161,   182,   161,   157,
     157,   157,   161,   157,   204,   157,   157,   157,   204,    18,
     316,   220,   157,   157,   156,   163,   204,   160,   161,   183,
     194,   160,   160,   118,   122,   124,   187,   197,   198,   199,
     157,   197,   160,   161,   154,   218,   162,   157,   197,   183,
     386,   157,   157,   157,   157,   463,   371,   371,   157,   157,
     373,   373,   460,   157,   157,   157,   157,   155,   432,   459,
     454,   458,   371,   371,   160,   183,   494,   161,   183,   157,
     161,   161,   183,   183,   384,   197,   137,   171,   183,   183,
     154,   385,   220,   426,   373,   183,   155,   426,   467,   470,
     155,   426,   467,   470,   155,   426,   467,   470,   371,   371,
     371,   425,   157,   149,   171,   183,   469,   161,   183,   413,
     405,   411,   231,   413,   351,   351,   351,     3,     5,    10,
      73,   154,   295,   302,   303,   311,   314,   352,   358,   487,
     161,   180,   155,    61,    62,   180,   231,   294,   422,   155,
     155,    18,   229,   155,   155,   180,   381,   180,   381,   166,
     381,   163,   228,   155,   155,   155,   229,   155,   231,   220,
     221,   221,    14,   281,   257,   268,   180,   183,   233,    78,
     180,   381,    91,    92,   261,   265,   112,   135,   260,   111,
     134,   264,   260,   380,   314,   162,   293,   160,   160,   183,
     413,   381,   423,   183,   180,   183,   180,   183,   157,   383,
     397,   397,   182,   183,   183,   183,   220,   155,   426,   476,
     470,   313,     5,   166,   183,   220,   366,   493,   163,   368,
      10,   369,   154,   179,   370,   493,   154,   182,   196,   310,
     186,    78,   191,   192,   382,   204,   204,   204,   204,   204,
     163,   386,   156,   220,   161,   154,   200,   159,   198,   200,
     200,   160,   161,   125,   158,   160,   226,   218,   180,   160,
     493,   155,   426,   467,   470,   157,   157,   183,   183,   157,
     155,   426,   467,   470,   155,   426,   476,   432,   426,   426,
     157,   157,   160,   378,   160,   137,   375,   137,   157,   157,
     183,   219,   219,   160,   160,   183,   183,   157,   371,   371,
     371,   157,   157,   157,   384,   426,   161,   220,   220,   321,
     337,   160,   154,   183,   413,   154,   154,   154,   154,   311,
     311,   350,   359,   487,   311,   358,   155,   346,   180,   180,
     155,   162,   202,   353,   354,   355,   361,   432,   433,   446,
     450,   161,   180,   381,   381,   194,   180,   231,   180,   231,
     227,   237,   294,   296,   299,   305,   314,   318,   227,    80,
     157,   237,   149,   150,   151,   156,   157,   180,   227,   246,
     247,   249,   294,   180,   180,   227,   180,   386,   180,   227,
     400,   227,   246,   113,   114,   115,   116,   117,   273,   275,
     276,   180,   100,   180,    84,   155,   157,   154,   180,   180,
     155,   155,   229,   229,   257,   155,   267,   257,   267,   231,
     426,   180,   157,   154,   395,   154,   182,   161,   161,   160,
     160,   160,   183,   371,   220,   220,   183,   160,   183,   163,
     154,   368,   493,   337,   381,   163,   219,   154,   405,   471,
     472,   157,   162,   157,   161,   162,   386,   493,   226,   123,
     197,   198,   159,   198,   159,   198,   160,   154,   371,   157,
     157,   371,   371,   160,   183,   157,   426,   157,   157,   157,
     227,   469,   154,   154,   346,   346,   346,   353,   155,   202,
     356,   357,   467,   478,   479,   480,   481,   180,   161,   180,
     353,   180,   400,   427,   432,   220,   314,   154,   180,   161,
     360,   361,   360,   360,   381,   157,   157,   227,   314,   157,
     155,   229,   157,   149,   150,   151,   171,   180,   250,   251,
     229,   228,   180,   251,   157,   162,   227,   156,   227,   228,
     249,   180,   493,   157,   157,   157,   157,   231,   275,   276,
     155,   220,   155,   188,   204,   258,   227,    75,   110,   259,
     261,    75,     1,   229,   426,   391,   406,   182,   182,   160,
     157,   183,   183,   160,   160,   368,   493,   154,   370,   370,
     386,   183,   157,   220,   192,   220,   493,   154,   160,   160,
     197,   197,   157,   426,   426,   157,   157,   160,   160,   220,
     180,   479,   480,   481,   314,   478,   161,   180,   426,   426,
     180,   157,   432,   426,   229,   229,    77,    78,   163,   240,
     241,   242,   157,   227,    75,   229,   227,   156,   227,    75,
     180,    57,   108,   156,   227,   228,   248,   249,   156,   227,
     229,   247,   251,   251,   180,   227,   154,   163,   242,   229,
     229,   155,   182,   180,   188,   157,   162,   157,   161,   162,
     157,   229,   155,   229,   229,   229,   397,   381,   423,   160,
     160,   493,   154,   493,   154,   154,   160,   160,   157,   157,
     157,   478,   426,   355,    75,     1,   219,   238,   239,   424,
       1,   162,     1,   182,   229,   240,    75,   180,   157,   229,
      75,   180,   171,   171,   229,   228,   108,   251,   251,   180,
     227,   248,   171,   171,    75,   156,   227,   156,   227,   228,
     180,     1,   182,   182,   277,   312,   314,   487,   162,   180,
     159,   188,   282,   283,   284,   204,   194,   227,   260,   154,
     154,   155,   426,   467,   470,   357,   229,   137,     1,   161,
     162,   154,   287,   288,   294,   229,    75,   180,   229,   227,
     156,   156,   227,   156,   227,   156,   227,   228,   156,   227,
     156,   227,   229,   171,   171,   171,   171,   154,   287,   277,
     183,   155,   202,   423,   478,   186,   162,   106,   155,   157,
     162,   161,   157,   157,    75,   256,   371,   219,   238,   241,
     243,   244,   294,   229,   171,   171,   171,   171,   156,   156,
     227,   156,   227,   156,   227,   243,   183,   180,   274,   314,
     282,   160,   219,   180,   282,   284,   229,    75,   157,   229,
     234,   183,   241,   156,   156,   227,   156,   227,   156,   227,
     183,   274,   218,   157,   162,   188,   157,   157,   162,   229,
       1,   229,   154,   234,   154,   157,   231,   188,   285,   155,
     180,   285,   231,   161,   162,   219,   157,   188,   186,   286,
     157,   180,   157,   161,   180,   186
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   181,   182,   183,   184,   184,   184,   184,   184,   185,
     185,   185,   185,   185,   185,   185,   185,   186,   186,   187,
     187,   188,   189,   189,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   191,
     191,   192,   192,   193,   193,   193,   193,   193,   193,   193,
     193,   193,   193,   193,   193,   193,   193,   193,   193,   193,
     193,   193,   193,   193,   193,   193,   193,   193,   193,   194,
     194,   195,   195,   196,   196,   197,   197,   198,   198,   198,
     198,   198,   198,   198,   199,   199,   199,   200,   200,   201,
     201,   201,   201,   201,   201,   201,   201,   201,   201,   201,
     201,   201,   201,   201,   201,   202,   202,   202,   203,   203,
     203,   203,   204,   204,   204,   204,   204,   204,   204,   204,
     204,   205,   205,   205,   205,   206,   206,   207,   207,   208,
     208,   208,   208,   209,   209,   209,   210,   210,   210,   211,
     211,   211,   211,   211,   212,   212,   212,   213,   213,   214,
     214,   215,   215,   216,   216,   217,   217,   218,   218,   218,
     219,   220,   220,   220,   221,   221,   222,   222,   223,   223,
     224,   224,   224,   224,   224,   224,   224,   224,   224,   224,
     224,   225,   225,   226,   226,   226,   226,   227,   227,   228,
     228,   229,   229,   229,   229,   229,   229,   229,   229,   229,
     229,   229,   229,   229,   229,   229,   229,   230,   230,   231,
     231,   232,   232,   233,   233,   233,   233,   233,   234,   234,
     234,   235,   236,   236,   236,   236,   236,   236,   236,   236,
     237,   237,   237,   237,   238,   238,   238,   239,   239,   240,
     240,   240,   240,   240,   241,   241,   242,   243,   243,   244,
     244,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   246,   246,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     248,   248,   249,   249,   249,   250,   250,   251,   251,   251,
     252,   252,   252,   252,   252,   252,   252,   252,   252,   252,
     252,   252,   252,   252,   252,   252,   252,   252,   252,   252,
     253,   253,   254,   255,   256,   257,   257,   258,   258,   259,
     260,   260,   261,   261,   262,   262,   262,   262,   262,   262,
     263,   264,   264,   265,   266,   266,   267,   267,   268,   268,
     268,   269,   270,   271,   272,   272,   272,   273,   273,   274,
     274,   275,   275,   275,   275,   276,   277,   277,   277,   277,
     277,   278,   279,   279,   280,   280,   280,   280,   280,   281,
     281,   282,   282,   283,   283,   284,   284,   285,   285,   285,
     286,   286,   287,   287,   288,   288,   289,   289,   290,   290,
     291,   291,   292,   292,   293,   293,   294,   294,   294,   295,
     295,   296,   296,   296,   296,   296,   297,   297,   297,   298,
     298,   298,   299,   299,   299,   299,   299,   300,   300,   300,
     300,   301,   301,   302,   302,   302,   303,   303,   303,   303,
     303,   304,   304,   305,   305,   305,   305,   306,   306,   306,
     306,   306,   307,   307,   308,   308,   308,   308,   309,   309,
     309,   310,   310,   310,   311,   311,   311,   312,   312,   312,
     313,   313,   314,   314,   315,   315,   316,   316,   316,   316,
     316,   317,   318,   318,   318,   319,   319,   320,   320,   320,
     320,   320,   320,   320,   320,   320,   321,   322,   322,   322,
     322,   322,   322,   322,   322,   322,   322,   322,   322,   322,
     322,   322,   322,   322,   322,   322,   322,   322,   322,   322,
     322,   322,   322,   322,   322,   323,   323,   324,   325,   325,
     326,   326,   326,   326,   326,   327,   327,   328,   328,   328,
     328,   329,   329,   329,   329,   329,   329,   330,   330,   330,
     330,   331,   332,   331,   331,   333,   333,   333,   333,   334,
     334,   334,   335,   335,   335,   335,   336,   336,   336,   337,
     337,   337,   337,   337,   337,   338,   338,   338,   339,   339,
     340,   340,   342,   341,   343,   341,   344,   341,   345,   341,
     341,   346,   346,   347,   347,   348,   348,   349,   349,   349,
     350,   350,   350,   350,   350,   350,   350,   350,   351,   351,
     352,   352,   352,   352,   352,   352,   352,   352,   352,   352,
     352,   352,   353,   353,   354,   354,   355,   355,   355,   355,
     356,   356,   356,   357,   358,   358,   359,   359,   360,   360,
     361,   362,   362,   363,   362,   362,   364,   362,   362,   362,
     365,   365,   366,   366,   367,   367,   368,   368,   368,   368,
     369,   369,   370,   370,   370,   371,   371,   371,   371,   372,
     372,   372,   372,   373,   373,   373,   373,   373,   373,   373,
     374,   374,   374,   374,   375,   375,   376,   376,   377,   377,
     378,   378,   378,   378,   378,   379,   379,   379,   379,   379,
     380,   380,   381,   381,   381,   382,   382,   382,   383,   383,
     384,   384,   384,   384,   385,   385,   386,   386,   386,   386,
     386,   387,   387,   388,   388,   389,   389,   389,   389,   389,
     390,   390,   391,   391,   393,   392,   394,   392,   392,   392,
     392,   395,   395,   395,   395,   396,   396,   396,   396,   397,
     397,   398,   398,   399,   399,   400,   400,   400,   400,   401,
     401,   401,   402,   402,   403,   403,   404,   404,   404,   404,
     405,   405,   406,   406,   407,   407,   407,   408,   408,   409,
     409,   410,   410,   411,   411,   412,   413,   414,   414,   414,
     414,   414,   414,   414,   414,   414,   414,   414,   415,   414,
     416,   414,   417,   414,   418,   414,   419,   414,   420,   420,
     420,   421,   421,   422,   422,   422,   422,   422,   422,   422,
     422,   422,   422,   423,   423,   423,   423,   424,   425,   425,
     426,   426,   427,   427,   428,   428,   428,   429,   429,   430,
     430,   430,   431,   431,   431,   431,   431,   431,   432,   432,
     433,   433,   433,   433,   434,   434,   434,   434,   435,   435,
     435,   435,   435,   435,   435,   436,   436,   436,   436,   437,
     437,   437,   438,   438,   438,   438,   438,   439,   439,   439,
     439,   440,   440,   440,   440,   440,   440,   441,   441,   441,
     442,   442,   442,   442,   442,   443,   443,   443,   443,   444,
     444,   444,   444,   444,   444,   445,   445,   446,   446,   446,
     446,   447,   447,   447,   447,   448,   448,   448,   448,   448,
     448,   448,   449,   449,   449,   449,   450,   450,   450,   451,
     451,   451,   451,   451,   452,   452,   452,   452,   453,   453,
     453,   453,   453,   453,   454,   454,   454,   454,   454,   455,
     455,   455,   456,   456,   456,   456,   457,   457,   457,   458,
     458,   458,   458,   458,   459,   459,   460,   460,   460,   461,
     461,   462,   462,   463,   463,   463,   464,   464,   464,   464,
     464,   465,   465,   465,   465,   466,   466,   466,   467,   467,
     467,   467,   467,   468,   468,   468,   468,   468,   468,   469,
     469,   470,   470,   470,   470,   471,   471,   472,   472,   472,
     472,   473,   473,   473,   473,   473,   474,   474,   474,   474,
     475,   475,   475,   476,   476,   476,   477,   477,   477,   477,
     477,   477,   478,   478,   478,   479,   479,   479,   479,   479,
     480,   480,   480,   480,   481,   481,   482,   482,   482,   483,
     483,   484,   484,   484,   484,   484,   484,   485,   485,   485,
     485,   485,   485,   485,   485,   485,   485,   486,   486,   486,
     486,   487,   487,   487,   488,   488,   489,   489,   489,   489,
     489,   489,   490,   490,   490,   490,   490,   490,   491,   491,
     491,   492,   492,   492,   493,   493,   494,   494
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     1,     3,     3,
       3,     5,     6,     2,     2,     2,     2,     2,     2,     1,
       3,     3,     3,     1,     6,     4,     4,     4,     4,     4,
       7,     3,     3,     3,     3,     3,     3,     3,     2,     5,
       3,     3,     3,     5,     2,     2,     7,     8,     5,     0,
       1,     1,     3,     1,     1,     1,     3,     1,     2,     4,
       3,     5,     3,     5,     2,     2,     2,     0,     2,     1,
       1,     1,     2,     2,     2,     2,     2,     2,     4,     2,
       4,     4,     4,     6,     4,     1,     1,     1,     1,     1,
       1,     1,     1,     4,     5,     5,     4,     5,     5,     5,
       4,     2,     2,     3,     3,     1,     1,     1,     3,     1,
       3,     3,     3,     1,     3,     3,     1,     3,     3,     1,
       3,     3,     3,     3,     1,     3,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     3,     1,     5,     4,
       1,     1,     3,     6,     0,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     4,     7,     1,     1,     3,     3,     1,     3,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     4,     2,
       6,     1,     2,     1,     2,     1,     2,     1,     1,     2,
       2,     2,     5,     7,     5,    10,     7,     5,    10,     7,
       1,     1,     1,     2,     1,     3,     1,     1,     3,     2,
       3,     3,     2,     2,     1,     2,     2,     0,     1,     2,
       3,     4,     6,     5,     7,     6,     7,     7,     8,     4,
       6,     5,     7,     1,     3,     4,     5,     4,     3,     5,
       1,     2,     3,     3,     3,     5,     5,     5,     5,     3,
       5,     5,     5,     3,     4,     5,     5,     5,     5,     7,
       7,     7,     7,     7,     7,     7,     2,     3,     4,     4,
       4,     6,     6,     6,     6,     6,     6,     6,     3,     4,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     4,     2,     3,     3,     2,     3,     2,     3,     3,
       6,     2,     2,     3,     3,     3,     3,     3,     3,     5,
       1,     1,     5,     5,     4,     0,     1,     1,     3,     4,
       1,     1,     4,     6,     3,     5,     5,     5,     8,     9,
       1,     1,     1,     4,     3,     3,     1,     3,     1,     3,
       5,     1,     2,     5,     3,     3,     4,     8,     9,     0,
       2,     1,     1,     1,     1,     2,     1,     2,     2,     2,
       1,     3,     1,     1,     6,     8,    10,    12,    14,     0,
       1,     0,     1,     1,     3,     4,     7,     0,     1,     3,
       1,     3,     0,     1,     1,     2,     0,     1,     2,     3,
       0,     1,     3,     4,     1,     3,     2,     2,     1,     7,
       5,     1,     1,     1,     1,     1,     2,     3,     6,     3,
       3,     4,     1,     2,     2,     3,     8,     9,     9,     8,
       8,     5,     7,     2,     2,     3,     3,     3,     4,     3,
       4,     4,     5,     2,     1,     1,     1,     3,     3,     2,
       4,     6,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     4,     1,     2,     3,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     5,     0,     1,
       1,     2,     2,     3,     3,     1,     3,     1,     2,     2,
       2,     4,     4,     4,     4,     1,     1,     1,     2,     2,
       3,     1,     0,     3,     2,     1,     2,     2,     3,     1,
       2,     2,     1,     2,     2,     3,     1,     2,     2,     1,
       2,     3,     1,     2,     3,     1,     3,     4,     1,     1,
       1,     1,     0,     7,     0,     8,     0,     8,     0,     8,
       1,     0,     3,     3,     3,     1,     1,     2,     1,     1,
       1,     2,     1,     2,     1,     2,     1,     2,     0,     2,
       3,     3,     4,     4,     4,     3,     2,     2,     3,     3,
       2,     1,     0,     1,     1,     4,     1,     2,     2,     2,
       0,     1,     4,     1,     2,     3,     1,     2,     0,     1,
       2,     7,     8,     0,     9,     8,     0,    11,    10,     1,
       2,     3,     0,     1,     3,     3,     3,     2,     5,     5,
       1,     1,     0,     2,     5,     0,     1,     1,     3,     1,
       1,     3,     3,     0,     1,     1,     1,     3,     3,     3,
       1,     3,     3,     5,     1,     3,     3,     3,     2,     3,
       1,     3,     3,     4,     1,     1,     1,     1,     2,     1,
       1,     3,     1,     1,     1,     1,     1,     2,     1,     1,
       0,     2,     2,     4,     1,     4,     0,     1,     2,     3,
       4,     2,     2,     1,     2,     2,     5,     5,     7,     6,
       1,     3,     0,     2,     0,     5,     0,     5,     3,     1,
       8,     0,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     2,     5,     6,     1,     1,     3,     3,     2,
       3,     3,     2,     4,     1,     4,     7,     5,    10,     8,
       1,     4,     2,     2,     1,     1,     5,     2,     5,     0,
       1,     3,     4,     0,     1,     0,     0,     1,     1,     2,
       2,     2,     2,     2,     2,     1,     2,     5,     0,     6,
       0,     8,     0,     7,     0,     7,     0,     8,     1,     2,
       3,     0,     5,     3,     4,     4,     4,     4,     5,     5,
       5,     5,     6,     1,     1,     1,     1,     3,     0,     5,
       0,     1,     1,     2,     6,     4,     4,     1,     3,     0,
       1,     4,     1,     1,     1,     1,     1,     1,     1,     3,
       2,     1,     2,     2,     2,     3,     4,     5,     2,     4,
       5,     4,     5,     3,     4,     6,     7,     3,     4,     2,
       1,     2,     4,     6,     7,     3,     4,     2,     3,     4,
       5,     4,     5,     4,     5,     3,     4,     1,     1,     1,
       4,     6,     7,     3,     4,     2,     3,     3,     4,     4,
       5,     4,     5,     3,     4,     1,     3,     2,     1,     2,
       2,     2,     3,     4,     5,     2,     4,     5,     4,     5,
       3,     4,     6,     7,     3,     4,     2,     1,     2,     4,
       6,     7,     3,     4,     2,     3,     4,     5,     4,     5,
       4,     5,     3,     4,     2,     4,     1,     2,     2,     2,
       3,     4,     2,     4,     4,     3,     4,     6,     3,     2,
       4,     1,     2,     2,     1,     1,     2,     3,     4,     2,
       4,     4,     6,     1,     2,     2,     1,     2,     2,     3,
       4,     1,     4,     4,     3,     3,     6,     3,     2,     3,
       7,     5,     1,     1,     1,     3,     3,     3,     5,     1,
       1,     5,     5,     6,     6,     0,     1,     1,     3,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       3,     6,     3,     1,     2,     1,     2,     6,     5,     6,
       7,     7,     1,     2,     2,     1,     2,     2,     3,     4,
       1,     4,     4,     3,     6,     3,     1,     1,     2,     1,
       1,     2,     3,     2,     3,     2,     3,     3,     2,     4,
       3,     2,     3,     2,     4,     3,     2,     6,     6,     6,
       7,     1,     2,     1,     1,     1,     2,     3,     2,     3,
       2,     3,     3,     4,     2,     3,     4,     2,     5,     6,
       7,     5,     6,     6,     0,     1,     0,     2
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
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 8513 "Parser/parser.cc"
    break;

  case 3:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8519 "Parser/parser.cc"
    break;

  case 4:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8525 "Parser/parser.cc"
    break;

  case 5:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8531 "Parser/parser.cc"
    break;

  case 6:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8537 "Parser/parser.cc"
    break;

  case 7:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8543 "Parser/parser.cc"
    break;

  case 8:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8549 "Parser/parser.cc"
    break;

  case 20:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8555 "Parser/parser.cc"
    break;

  case 21:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8561 "Parser/parser.cc"
    break;

  case 22:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8567 "Parser/parser.cc"
    break;

  case 23:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8577 "Parser/parser.cc"
    break;

  case 24:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8583 "Parser/parser.cc"
    break;

  case 25:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8589 "Parser/parser.cc"
    break;

  case 26:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8595 "Parser/parser.cc"
    break;

  case 28:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8601 "Parser/parser.cc"
    break;

  case 29:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8607 "Parser/parser.cc"
    break;

  case 30:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8613 "Parser/parser.cc"
    break;

  case 31:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8619 "Parser/parser.cc"
    break;

  case 32:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8629 "Parser/parser.cc"
    break;

  case 33:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8635 "Parser/parser.cc"
    break;

  case 34:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8641 "Parser/parser.cc"
    break;

  case 35:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8647 "Parser/parser.cc"
    break;

  case 36:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8653 "Parser/parser.cc"
    break;

  case 37:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8659 "Parser/parser.cc"
    break;

  case 38:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8665 "Parser/parser.cc"
    break;

  case 40:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8677 "Parser/parser.cc"
    break;

  case 41:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8686 "Parser/parser.cc"
    break;

  case 42:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8692 "Parser/parser.cc"
    break;

  case 44:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8698 "Parser/parser.cc"
    break;

  case 45:
#line 772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8704 "Parser/parser.cc"
    break;

  case 46:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8710 "Parser/parser.cc"
    break;

  case 47:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8716 "Parser/parser.cc"
    break;

  case 48:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8726 "Parser/parser.cc"
    break;

  case 49:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8732 "Parser/parser.cc"
    break;

  case 50:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8739 "Parser/parser.cc"
    break;

  case 51:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8745 "Parser/parser.cc"
    break;

  case 52:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 53:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8757 "Parser/parser.cc"
    break;

  case 54:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8763 "Parser/parser.cc"
    break;

  case 55:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8769 "Parser/parser.cc"
    break;

  case 56:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8775 "Parser/parser.cc"
    break;

  case 57:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8781 "Parser/parser.cc"
    break;

  case 58:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8787 "Parser/parser.cc"
    break;

  case 59:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8793 "Parser/parser.cc"
    break;

  case 60:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8799 "Parser/parser.cc"
    break;

  case 61:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8805 "Parser/parser.cc"
    break;

  case 62:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8811 "Parser/parser.cc"
    break;

  case 63:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8817 "Parser/parser.cc"
    break;

  case 64:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8823 "Parser/parser.cc"
    break;

  case 65:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8829 "Parser/parser.cc"
    break;

  case 66:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8835 "Parser/parser.cc"
    break;

  case 67:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8841 "Parser/parser.cc"
    break;

  case 68:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8851 "Parser/parser.cc"
    break;

  case 69:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8857 "Parser/parser.cc"
    break;

  case 72:
#line 859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8863 "Parser/parser.cc"
    break;

  case 73:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8869 "Parser/parser.cc"
    break;

  case 76:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8875 "Parser/parser.cc"
    break;

  case 78:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8881 "Parser/parser.cc"
    break;

  case 79:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8887 "Parser/parser.cc"
    break;

  case 80:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8893 "Parser/parser.cc"
    break;

  case 81:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8899 "Parser/parser.cc"
    break;

  case 82:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8905 "Parser/parser.cc"
    break;

  case 83:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8911 "Parser/parser.cc"
    break;

  case 84:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8917 "Parser/parser.cc"
    break;

  case 85:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8923 "Parser/parser.cc"
    break;

  case 86:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8931 "Parser/parser.cc"
    break;

  case 87:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8937 "Parser/parser.cc"
    break;

  case 88:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8946 "Parser/parser.cc"
    break;

  case 91:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8952 "Parser/parser.cc"
    break;

  case 92:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8958 "Parser/parser.cc"
    break;

  case 93:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8978 "Parser/parser.cc"
    break;

  case 94:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8984 "Parser/parser.cc"
    break;

  case 95:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8990 "Parser/parser.cc"
    break;

  case 96:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8996 "Parser/parser.cc"
    break;

  case 97:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9002 "Parser/parser.cc"
    break;

  case 98:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9008 "Parser/parser.cc"
    break;

  case 99:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9014 "Parser/parser.cc"
    break;

  case 100:
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9020 "Parser/parser.cc"
    break;

  case 101:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9026 "Parser/parser.cc"
    break;

  case 102:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9032 "Parser/parser.cc"
    break;

  case 103:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9038 "Parser/parser.cc"
    break;

  case 104:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9047 "Parser/parser.cc"
    break;

  case 105:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9053 "Parser/parser.cc"
    break;

  case 106:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9059 "Parser/parser.cc"
    break;

  case 107:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9065 "Parser/parser.cc"
    break;

  case 108:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9071 "Parser/parser.cc"
    break;

  case 109:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9077 "Parser/parser.cc"
    break;

  case 110:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9083 "Parser/parser.cc"
    break;

  case 111:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9089 "Parser/parser.cc"
    break;

  case 113:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9095 "Parser/parser.cc"
    break;

  case 114:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9101 "Parser/parser.cc"
    break;

  case 115:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 116:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9113 "Parser/parser.cc"
    break;

  case 117:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9119 "Parser/parser.cc"
    break;

  case 118:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9125 "Parser/parser.cc"
    break;

  case 119:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9131 "Parser/parser.cc"
    break;

  case 120:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9137 "Parser/parser.cc"
    break;

  case 128:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9143 "Parser/parser.cc"
    break;

  case 130:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9149 "Parser/parser.cc"
    break;

  case 131:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9155 "Parser/parser.cc"
    break;

  case 132:
#line 1031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9161 "Parser/parser.cc"
    break;

  case 134:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9167 "Parser/parser.cc"
    break;

  case 135:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9173 "Parser/parser.cc"
    break;

  case 137:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9179 "Parser/parser.cc"
    break;

  case 138:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9185 "Parser/parser.cc"
    break;

  case 140:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9191 "Parser/parser.cc"
    break;

  case 141:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9197 "Parser/parser.cc"
    break;

  case 142:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9203 "Parser/parser.cc"
    break;

  case 143:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9209 "Parser/parser.cc"
    break;

  case 145:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9215 "Parser/parser.cc"
    break;

  case 146:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9221 "Parser/parser.cc"
    break;

  case 148:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9227 "Parser/parser.cc"
    break;

  case 150:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9233 "Parser/parser.cc"
    break;

  case 152:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9239 "Parser/parser.cc"
    break;

  case 154:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9245 "Parser/parser.cc"
    break;

  case 156:
#line 1097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9251 "Parser/parser.cc"
    break;

  case 158:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9257 "Parser/parser.cc"
    break;

  case 159:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9263 "Parser/parser.cc"
    break;

  case 162:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 9275 "Parser/parser.cc"
    break;

  case 163:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9281 "Parser/parser.cc"
    break;

  case 164:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9287 "Parser/parser.cc"
    break;

  case 168:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9293 "Parser/parser.cc"
    break;

  case 169:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9299 "Parser/parser.cc"
    break;

  case 170:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9305 "Parser/parser.cc"
    break;

  case 171:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9311 "Parser/parser.cc"
    break;

  case 172:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9317 "Parser/parser.cc"
    break;

  case 173:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9323 "Parser/parser.cc"
    break;

  case 174:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9329 "Parser/parser.cc"
    break;

  case 175:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9335 "Parser/parser.cc"
    break;

  case 176:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9341 "Parser/parser.cc"
    break;

  case 177:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9347 "Parser/parser.cc"
    break;

  case 178:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9353 "Parser/parser.cc"
    break;

  case 179:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9359 "Parser/parser.cc"
    break;

  case 180:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9365 "Parser/parser.cc"
    break;

  case 181:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9371 "Parser/parser.cc"
    break;

  case 182:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9377 "Parser/parser.cc"
    break;

  case 184:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9383 "Parser/parser.cc"
    break;

  case 185:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9389 "Parser/parser.cc"
    break;

  case 186:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9395 "Parser/parser.cc"
    break;

  case 188:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9401 "Parser/parser.cc"
    break;

  case 189:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9407 "Parser/parser.cc"
    break;

  case 204:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9413 "Parser/parser.cc"
    break;

  case 206:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9419 "Parser/parser.cc"
    break;

  case 207:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9425 "Parser/parser.cc"
    break;

  case 208:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9436 "Parser/parser.cc"
    break;

  case 209:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9442 "Parser/parser.cc"
    break;

  case 210:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9448 "Parser/parser.cc"
    break;

  case 212:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9454 "Parser/parser.cc"
    break;

  case 213:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9460 "Parser/parser.cc"
    break;

  case 214:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9466 "Parser/parser.cc"
    break;

  case 215:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9472 "Parser/parser.cc"
    break;

  case 216:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9478 "Parser/parser.cc"
    break;

  case 219:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9484 "Parser/parser.cc"
    break;

  case 220:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9491 "Parser/parser.cc"
    break;

  case 221:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9497 "Parser/parser.cc"
    break;

  case 222:
#line 1297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9503 "Parser/parser.cc"
    break;

  case 223:
#line 1299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9509 "Parser/parser.cc"
    break;

  case 224:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9515 "Parser/parser.cc"
    break;

  case 225:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9529 "Parser/parser.cc"
    break;

  case 226:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9535 "Parser/parser.cc"
    break;

  case 227:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9541 "Parser/parser.cc"
    break;

  case 228:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9550 "Parser/parser.cc"
    break;

  case 229:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9556 "Parser/parser.cc"
    break;

  case 230:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9562 "Parser/parser.cc"
    break;

  case 231:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9568 "Parser/parser.cc"
    break;

  case 232:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9574 "Parser/parser.cc"
    break;

  case 233:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9580 "Parser/parser.cc"
    break;

  case 234:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9586 "Parser/parser.cc"
    break;

  case 235:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9592 "Parser/parser.cc"
    break;

  case 237:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9598 "Parser/parser.cc"
    break;

  case 238:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9604 "Parser/parser.cc"
    break;

  case 239:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9610 "Parser/parser.cc"
    break;

  case 240:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9616 "Parser/parser.cc"
    break;

  case 241:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9622 "Parser/parser.cc"
    break;

  case 242:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9628 "Parser/parser.cc"
    break;

  case 243:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9634 "Parser/parser.cc"
    break;

  case 245:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9640 "Parser/parser.cc"
    break;

  case 246:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9646 "Parser/parser.cc"
    break;

  case 247:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9652 "Parser/parser.cc"
    break;

  case 249:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9658 "Parser/parser.cc"
    break;

  case 250:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9664 "Parser/parser.cc"
    break;

  case 251:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9670 "Parser/parser.cc"
    break;

  case 252:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9679 "Parser/parser.cc"
    break;

  case 253:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 254:
#line 1397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 255:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 256:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9706 "Parser/parser.cc"
    break;

  case 257:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9712 "Parser/parser.cc"
    break;

  case 258:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9718 "Parser/parser.cc"
    break;

  case 259:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9724 "Parser/parser.cc"
    break;

  case 260:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9733 "Parser/parser.cc"
    break;

  case 261:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9739 "Parser/parser.cc"
    break;

  case 262:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9745 "Parser/parser.cc"
    break;

  case 264:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9764 "Parser/parser.cc"
    break;

  case 265:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9770 "Parser/parser.cc"
    break;

  case 266:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9779 "Parser/parser.cc"
    break;

  case 267:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9785 "Parser/parser.cc"
    break;

  case 268:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9791 "Parser/parser.cc"
    break;

  case 269:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9797 "Parser/parser.cc"
    break;

  case 270:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9803 "Parser/parser.cc"
    break;

  case 271:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9809 "Parser/parser.cc"
    break;

  case 272:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9815 "Parser/parser.cc"
    break;

  case 273:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9824 "Parser/parser.cc"
    break;

  case 274:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9833 "Parser/parser.cc"
    break;

  case 275:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9839 "Parser/parser.cc"
    break;

  case 276:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9848 "Parser/parser.cc"
    break;

  case 277:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9857 "Parser/parser.cc"
    break;

  case 278:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9863 "Parser/parser.cc"
    break;

  case 279:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9869 "Parser/parser.cc"
    break;

  case 280:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9875 "Parser/parser.cc"
    break;

  case 281:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9881 "Parser/parser.cc"
    break;

  case 282:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9887 "Parser/parser.cc"
    break;

  case 283:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9893 "Parser/parser.cc"
    break;

  case 284:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9899 "Parser/parser.cc"
    break;

  case 285:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9905 "Parser/parser.cc"
    break;

  case 286:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9914 "Parser/parser.cc"
    break;

  case 287:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9924 "Parser/parser.cc"
    break;

  case 288:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9930 "Parser/parser.cc"
    break;

  case 289:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9936 "Parser/parser.cc"
    break;

  case 290:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9945 "Parser/parser.cc"
    break;

  case 291:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9955 "Parser/parser.cc"
    break;

  case 292:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9961 "Parser/parser.cc"
    break;

  case 293:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9970 "Parser/parser.cc"
    break;

  case 294:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9980 "Parser/parser.cc"
    break;

  case 295:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9986 "Parser/parser.cc"
    break;

  case 296:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9992 "Parser/parser.cc"
    break;

  case 297:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9998 "Parser/parser.cc"
    break;

  case 298:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10004 "Parser/parser.cc"
    break;

  case 299:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10013 "Parser/parser.cc"
    break;

  case 300:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10023 "Parser/parser.cc"
    break;

  case 301:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10029 "Parser/parser.cc"
    break;

  case 302:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10038 "Parser/parser.cc"
    break;

  case 303:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10048 "Parser/parser.cc"
    break;

  case 304:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10054 "Parser/parser.cc"
    break;

  case 305:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10063 "Parser/parser.cc"
    break;

  case 306:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10073 "Parser/parser.cc"
    break;

  case 307:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10079 "Parser/parser.cc"
    break;

  case 308:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 10088 "Parser/parser.cc"
    break;

  case 309:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 10099 "Parser/parser.cc"
    break;

  case 312:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10105 "Parser/parser.cc"
    break;

  case 313:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10111 "Parser/parser.cc"
    break;

  case 314:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10117 "Parser/parser.cc"
    break;

  case 315:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10123 "Parser/parser.cc"
    break;

  case 316:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10129 "Parser/parser.cc"
    break;

  case 318:
#line 1637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10135 "Parser/parser.cc"
    break;

  case 319:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10141 "Parser/parser.cc"
    break;

  case 320:
#line 1644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10147 "Parser/parser.cc"
    break;

  case 321:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10153 "Parser/parser.cc"
    break;

  case 322:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10159 "Parser/parser.cc"
    break;

  case 323:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10165 "Parser/parser.cc"
    break;

  case 324:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10171 "Parser/parser.cc"
    break;

  case 325:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10177 "Parser/parser.cc"
    break;

  case 326:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10183 "Parser/parser.cc"
    break;

  case 327:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10189 "Parser/parser.cc"
    break;

  case 328:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10195 "Parser/parser.cc"
    break;

  case 329:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10201 "Parser/parser.cc"
    break;

  case 330:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10207 "Parser/parser.cc"
    break;

  case 331:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10213 "Parser/parser.cc"
    break;

  case 332:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10219 "Parser/parser.cc"
    break;

  case 333:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10225 "Parser/parser.cc"
    break;

  case 334:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10231 "Parser/parser.cc"
    break;

  case 335:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10237 "Parser/parser.cc"
    break;

  case 336:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10243 "Parser/parser.cc"
    break;

  case 337:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10249 "Parser/parser.cc"
    break;

  case 338:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10255 "Parser/parser.cc"
    break;

  case 339:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10261 "Parser/parser.cc"
    break;

  case 342:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10267 "Parser/parser.cc"
    break;

  case 343:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10276 "Parser/parser.cc"
    break;

  case 344:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10282 "Parser/parser.cc"
    break;

  case 345:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10288 "Parser/parser.cc"
    break;

  case 348:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10294 "Parser/parser.cc"
    break;

  case 349:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10300 "Parser/parser.cc"
    break;

  case 352:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10306 "Parser/parser.cc"
    break;

  case 353:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10312 "Parser/parser.cc"
    break;

  case 354:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10318 "Parser/parser.cc"
    break;

  case 355:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10324 "Parser/parser.cc"
    break;

  case 356:
#line 1751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10330 "Parser/parser.cc"
    break;

  case 357:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10336 "Parser/parser.cc"
    break;

  case 358:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10342 "Parser/parser.cc"
    break;

  case 359:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10348 "Parser/parser.cc"
    break;

  case 360:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10354 "Parser/parser.cc"
    break;

  case 363:
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10360 "Parser/parser.cc"
    break;

  case 364:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10366 "Parser/parser.cc"
    break;

  case 365:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10372 "Parser/parser.cc"
    break;

  case 366:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10378 "Parser/parser.cc"
    break;

  case 367:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10384 "Parser/parser.cc"
    break;

  case 368:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10390 "Parser/parser.cc"
    break;

  case 369:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10396 "Parser/parser.cc"
    break;

  case 370:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10402 "Parser/parser.cc"
    break;

  case 371:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10408 "Parser/parser.cc"
    break;

  case 372:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10414 "Parser/parser.cc"
    break;

  case 373:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 374:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10426 "Parser/parser.cc"
    break;

  case 375:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10432 "Parser/parser.cc"
    break;

  case 376:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 377:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10444 "Parser/parser.cc"
    break;

  case 378:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 379:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10456 "Parser/parser.cc"
    break;

  case 380:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10462 "Parser/parser.cc"
    break;

  case 381:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10468 "Parser/parser.cc"
    break;

  case 382:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10474 "Parser/parser.cc"
    break;

  case 383:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10480 "Parser/parser.cc"
    break;

  case 384:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10486 "Parser/parser.cc"
    break;

  case 385:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10492 "Parser/parser.cc"
    break;

  case 387:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10498 "Parser/parser.cc"
    break;

  case 388:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10504 "Parser/parser.cc"
    break;

  case 389:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 394:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10516 "Parser/parser.cc"
    break;

  case 395:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10522 "Parser/parser.cc"
    break;

  case 396:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10528 "Parser/parser.cc"
    break;

  case 397:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10534 "Parser/parser.cc"
    break;

  case 398:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10540 "Parser/parser.cc"
    break;

  case 399:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10546 "Parser/parser.cc"
    break;

  case 400:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10552 "Parser/parser.cc"
    break;

  case 401:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10558 "Parser/parser.cc"
    break;

  case 404:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10564 "Parser/parser.cc"
    break;

  case 405:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 406:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10579 "Parser/parser.cc"
    break;

  case 407:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10585 "Parser/parser.cc"
    break;

  case 408:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10591 "Parser/parser.cc"
    break;

  case 409:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10597 "Parser/parser.cc"
    break;

  case 410:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10606 "Parser/parser.cc"
    break;

  case 411:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10615 "Parser/parser.cc"
    break;

  case 412:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10621 "Parser/parser.cc"
    break;

  case 415:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10627 "Parser/parser.cc"
    break;

  case 416:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10633 "Parser/parser.cc"
    break;

  case 418:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10639 "Parser/parser.cc"
    break;

  case 419:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10645 "Parser/parser.cc"
    break;

  case 429:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10651 "Parser/parser.cc"
    break;

  case 430:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10657 "Parser/parser.cc"
    break;

  case 434:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10663 "Parser/parser.cc"
    break;

  case 436:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10669 "Parser/parser.cc"
    break;

  case 437:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10675 "Parser/parser.cc"
    break;

  case 438:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10681 "Parser/parser.cc"
    break;

  case 439:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10687 "Parser/parser.cc"
    break;

  case 440:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10693 "Parser/parser.cc"
    break;

  case 441:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10699 "Parser/parser.cc"
    break;

  case 443:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10705 "Parser/parser.cc"
    break;

  case 444:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10711 "Parser/parser.cc"
    break;

  case 445:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10717 "Parser/parser.cc"
    break;

  case 446:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10728 "Parser/parser.cc"
    break;

  case 447:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10734 "Parser/parser.cc"
    break;

  case 448:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10740 "Parser/parser.cc"
    break;

  case 449:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10746 "Parser/parser.cc"
    break;

  case 450:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10752 "Parser/parser.cc"
    break;

  case 451:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10758 "Parser/parser.cc"
    break;

  case 452:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10764 "Parser/parser.cc"
    break;

  case 453:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10773 "Parser/parser.cc"
    break;

  case 454:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10782 "Parser/parser.cc"
    break;

  case 455:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10791 "Parser/parser.cc"
    break;

  case 456:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10802 "Parser/parser.cc"
    break;

  case 457:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10811 "Parser/parser.cc"
    break;

  case 458:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10817 "Parser/parser.cc"
    break;

  case 459:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10823 "Parser/parser.cc"
    break;

  case 460:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10829 "Parser/parser.cc"
    break;

  case 461:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10837 "Parser/parser.cc"
    break;

  case 462:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10845 "Parser/parser.cc"
    break;

  case 463:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10851 "Parser/parser.cc"
    break;

  case 466:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			assert( (yyvsp[0].decl)->type );
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {			// CV qualifiers ?
				SemanticError( yylloc, "syntax error, useless type qualifier(s) in empty declaration." ); (yyval.decl) = nullptr;
			}
			// enums are never empty declarations because there must have at least one enumeration.
			if ( (yyvsp[0].decl)->type->kind == TypeData::AggregateInst && (yyvsp[0].decl)->storageClasses.any() ) { // storage class ?
				SemanticError( yylloc, "syntax error, useless storage qualifier(s) in empty aggregate declaration." ); (yyval.decl) = nullptr;
			}
		}
#line 10866 "Parser/parser.cc"
    break;

  case 467:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 468:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10878 "Parser/parser.cc"
    break;

  case 469:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10884 "Parser/parser.cc"
    break;

  case 470:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 471:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 477:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10906 "Parser/parser.cc"
    break;

  case 490:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10912 "Parser/parser.cc"
    break;

  case 493:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10918 "Parser/parser.cc"
    break;

  case 494:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10924 "Parser/parser.cc"
    break;

  case 496:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10930 "Parser/parser.cc"
    break;

  case 497:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10936 "Parser/parser.cc"
    break;

  case 498:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10942 "Parser/parser.cc"
    break;

  case 499:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10948 "Parser/parser.cc"
    break;

  case 500:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10954 "Parser/parser.cc"
    break;

  case 501:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10960 "Parser/parser.cc"
    break;

  case 503:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10966 "Parser/parser.cc"
    break;

  case 504:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10972 "Parser/parser.cc"
    break;

  case 506:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10978 "Parser/parser.cc"
    break;

  case 507:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10984 "Parser/parser.cc"
    break;

  case 508:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10990 "Parser/parser.cc"
    break;

  case 509:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10996 "Parser/parser.cc"
    break;

  case 510:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11002 "Parser/parser.cc"
    break;

  case 511:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11008 "Parser/parser.cc"
    break;

  case 512:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11014 "Parser/parser.cc"
    break;

  case 513:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11020 "Parser/parser.cc"
    break;

  case 514:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11026 "Parser/parser.cc"
    break;

  case 515:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11032 "Parser/parser.cc"
    break;

  case 516:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11038 "Parser/parser.cc"
    break;

  case 517:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11044 "Parser/parser.cc"
    break;

  case 518:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11050 "Parser/parser.cc"
    break;

  case 519:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11056 "Parser/parser.cc"
    break;

  case 520:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11062 "Parser/parser.cc"
    break;

  case 521:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11068 "Parser/parser.cc"
    break;

  case 522:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11074 "Parser/parser.cc"
    break;

  case 523:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11080 "Parser/parser.cc"
    break;

  case 524:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11086 "Parser/parser.cc"
    break;

  case 525:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11092 "Parser/parser.cc"
    break;

  case 526:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11098 "Parser/parser.cc"
    break;

  case 527:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11104 "Parser/parser.cc"
    break;

  case 528:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11110 "Parser/parser.cc"
    break;

  case 529:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11116 "Parser/parser.cc"
    break;

  case 530:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11122 "Parser/parser.cc"
    break;

  case 531:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11128 "Parser/parser.cc"
    break;

  case 532:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11134 "Parser/parser.cc"
    break;

  case 533:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11140 "Parser/parser.cc"
    break;

  case 534:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11146 "Parser/parser.cc"
    break;

  case 535:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11152 "Parser/parser.cc"
    break;

  case 536:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11158 "Parser/parser.cc"
    break;

  case 537:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11164 "Parser/parser.cc"
    break;

  case 538:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11170 "Parser/parser.cc"
    break;

  case 539:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11176 "Parser/parser.cc"
    break;

  case 540:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11182 "Parser/parser.cc"
    break;

  case 541:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11188 "Parser/parser.cc"
    break;

  case 542:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11194 "Parser/parser.cc"
    break;

  case 543:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11200 "Parser/parser.cc"
    break;

  case 545:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11206 "Parser/parser.cc"
    break;

  case 547:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11212 "Parser/parser.cc"
    break;

  case 548:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11218 "Parser/parser.cc"
    break;

  case 549:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11224 "Parser/parser.cc"
    break;

  case 551:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11230 "Parser/parser.cc"
    break;

  case 552:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11236 "Parser/parser.cc"
    break;

  case 553:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11242 "Parser/parser.cc"
    break;

  case 554:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11248 "Parser/parser.cc"
    break;

  case 556:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11254 "Parser/parser.cc"
    break;

  case 558:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11260 "Parser/parser.cc"
    break;

  case 559:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 560:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11272 "Parser/parser.cc"
    break;

  case 561:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11278 "Parser/parser.cc"
    break;

  case 562:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11284 "Parser/parser.cc"
    break;

  case 563:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11290 "Parser/parser.cc"
    break;

  case 564:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11296 "Parser/parser.cc"
    break;

  case 565:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11302 "Parser/parser.cc"
    break;

  case 566:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11308 "Parser/parser.cc"
    break;

  case 568:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11314 "Parser/parser.cc"
    break;

  case 569:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11320 "Parser/parser.cc"
    break;

  case 570:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11326 "Parser/parser.cc"
    break;

  case 572:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11332 "Parser/parser.cc"
    break;

  case 573:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11338 "Parser/parser.cc"
    break;

  case 574:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11347 "Parser/parser.cc"
    break;

  case 576:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11353 "Parser/parser.cc"
    break;

  case 577:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 578:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 580:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11371 "Parser/parser.cc"
    break;

  case 581:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11377 "Parser/parser.cc"
    break;

  case 583:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11383 "Parser/parser.cc"
    break;

  case 584:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11389 "Parser/parser.cc"
    break;

  case 585:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11395 "Parser/parser.cc"
    break;

  case 586:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 587:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11407 "Parser/parser.cc"
    break;

  case 588:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 589:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11419 "Parser/parser.cc"
    break;

  case 590:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11425 "Parser/parser.cc"
    break;

  case 591:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11431 "Parser/parser.cc"
    break;

  case 593:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 594:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 595:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11449 "Parser/parser.cc"
    break;

  case 596:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11455 "Parser/parser.cc"
    break;

  case 597:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 602:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11467 "Parser/parser.cc"
    break;

  case 603:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11473 "Parser/parser.cc"
    break;

  case 604:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11482 "Parser/parser.cc"
    break;

  case 605:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11490 "Parser/parser.cc"
    break;

  case 606:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11499 "Parser/parser.cc"
    break;

  case 607:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11508 "Parser/parser.cc"
    break;

  case 608:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11517 "Parser/parser.cc"
    break;

  case 609:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11526 "Parser/parser.cc"
    break;

  case 611:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11532 "Parser/parser.cc"
    break;

  case 612:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11538 "Parser/parser.cc"
    break;

  case 613:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11548 "Parser/parser.cc"
    break;

  case 614:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11567 "Parser/parser.cc"
    break;

  case 617:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11573 "Parser/parser.cc"
    break;

  case 618:
#line 2601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11579 "Parser/parser.cc"
    break;

  case 619:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11585 "Parser/parser.cc"
    break;

  case 620:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11591 "Parser/parser.cc"
    break;

  case 621:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11597 "Parser/parser.cc"
    break;

  case 622:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11603 "Parser/parser.cc"
    break;

  case 623:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11612 "Parser/parser.cc"
    break;

  case 624:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11618 "Parser/parser.cc"
    break;

  case 625:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11627 "Parser/parser.cc"
    break;

  case 626:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11633 "Parser/parser.cc"
    break;

  case 627:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11642 "Parser/parser.cc"
    break;

  case 628:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11648 "Parser/parser.cc"
    break;

  case 629:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11654 "Parser/parser.cc"
    break;

  case 630:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11667 "Parser/parser.cc"
    break;

  case 631:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11676 "Parser/parser.cc"
    break;

  case 632:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11682 "Parser/parser.cc"
    break;

  case 633:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11688 "Parser/parser.cc"
    break;

  case 634:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11701 "Parser/parser.cc"
    break;

  case 635:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11707 "Parser/parser.cc"
    break;

  case 638:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11713 "Parser/parser.cc"
    break;

  case 639:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11719 "Parser/parser.cc"
    break;

  case 642:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11725 "Parser/parser.cc"
    break;

  case 645:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11731 "Parser/parser.cc"
    break;

  case 646:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11737 "Parser/parser.cc"
    break;

  case 647:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11743 "Parser/parser.cc"
    break;

  case 648:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11749 "Parser/parser.cc"
    break;

  case 649:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11755 "Parser/parser.cc"
    break;

  case 650:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11761 "Parser/parser.cc"
    break;

  case 652:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11767 "Parser/parser.cc"
    break;

  case 654:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11773 "Parser/parser.cc"
    break;

  case 655:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11779 "Parser/parser.cc"
    break;

  case 657:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11785 "Parser/parser.cc"
    break;

  case 658:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11791 "Parser/parser.cc"
    break;

  case 660:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11797 "Parser/parser.cc"
    break;

  case 661:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11808 "Parser/parser.cc"
    break;

  case 662:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11822 "Parser/parser.cc"
    break;

  case 663:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11828 "Parser/parser.cc"
    break;

  case 664:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11834 "Parser/parser.cc"
    break;

  case 665:
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11840 "Parser/parser.cc"
    break;

  case 666:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11851 "Parser/parser.cc"
    break;

  case 667:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11857 "Parser/parser.cc"
    break;

  case 668:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11863 "Parser/parser.cc"
    break;

  case 670:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11869 "Parser/parser.cc"
    break;

  case 671:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11875 "Parser/parser.cc"
    break;

  case 672:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11881 "Parser/parser.cc"
    break;

  case 673:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11887 "Parser/parser.cc"
    break;

  case 674:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11896 "Parser/parser.cc"
    break;

  case 675:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11905 "Parser/parser.cc"
    break;

  case 676:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11911 "Parser/parser.cc"
    break;

  case 677:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11921 "Parser/parser.cc"
    break;

  case 678:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 679:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11933 "Parser/parser.cc"
    break;

  case 681:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11939 "Parser/parser.cc"
    break;

  case 682:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11945 "Parser/parser.cc"
    break;

  case 683:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11951 "Parser/parser.cc"
    break;

  case 684:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11957 "Parser/parser.cc"
    break;

  case 685:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11963 "Parser/parser.cc"
    break;

  case 686:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11969 "Parser/parser.cc"
    break;

  case 688:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11975 "Parser/parser.cc"
    break;

  case 691:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11981 "Parser/parser.cc"
    break;

  case 692:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 693:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 694:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11999 "Parser/parser.cc"
    break;

  case 697:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12005 "Parser/parser.cc"
    break;

  case 698:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12011 "Parser/parser.cc"
    break;

  case 699:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12017 "Parser/parser.cc"
    break;

  case 701:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12023 "Parser/parser.cc"
    break;

  case 702:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12029 "Parser/parser.cc"
    break;

  case 703:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12035 "Parser/parser.cc"
    break;

  case 705:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 706:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12047 "Parser/parser.cc"
    break;

  case 707:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12053 "Parser/parser.cc"
    break;

  case 708:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12059 "Parser/parser.cc"
    break;

  case 709:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12065 "Parser/parser.cc"
    break;

  case 711:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 712:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 713:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12083 "Parser/parser.cc"
    break;

  case 718:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 720:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 721:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12101 "Parser/parser.cc"
    break;

  case 727:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12107 "Parser/parser.cc"
    break;

  case 730:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12113 "Parser/parser.cc"
    break;

  case 731:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12119 "Parser/parser.cc"
    break;

  case 732:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12125 "Parser/parser.cc"
    break;

  case 733:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12131 "Parser/parser.cc"
    break;

  case 734:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12137 "Parser/parser.cc"
    break;

  case 735:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12143 "Parser/parser.cc"
    break;

  case 736:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12149 "Parser/parser.cc"
    break;

  case 738:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12155 "Parser/parser.cc"
    break;

  case 739:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 740:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 742:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 744:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 745:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12185 "Parser/parser.cc"
    break;

  case 746:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12191 "Parser/parser.cc"
    break;

  case 747:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12197 "Parser/parser.cc"
    break;

  case 748:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12203 "Parser/parser.cc"
    break;

  case 749:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12209 "Parser/parser.cc"
    break;

  case 751:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12215 "Parser/parser.cc"
    break;

  case 752:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12221 "Parser/parser.cc"
    break;

  case 753:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12227 "Parser/parser.cc"
    break;

  case 754:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12238 "Parser/parser.cc"
    break;

  case 755:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12244 "Parser/parser.cc"
    break;

  case 756:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12250 "Parser/parser.cc"
    break;

  case 757:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12256 "Parser/parser.cc"
    break;

  case 758:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12265 "Parser/parser.cc"
    break;

  case 759:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12271 "Parser/parser.cc"
    break;

  case 760:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12281 "Parser/parser.cc"
    break;

  case 761:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12287 "Parser/parser.cc"
    break;

  case 762:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12293 "Parser/parser.cc"
    break;

  case 763:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12299 "Parser/parser.cc"
    break;

  case 764:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12305 "Parser/parser.cc"
    break;

  case 765:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12311 "Parser/parser.cc"
    break;

  case 766:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12317 "Parser/parser.cc"
    break;

  case 767:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12323 "Parser/parser.cc"
    break;

  case 768:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12329 "Parser/parser.cc"
    break;

  case 769:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12335 "Parser/parser.cc"
    break;

  case 772:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12341 "Parser/parser.cc"
    break;

  case 773:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 774:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12353 "Parser/parser.cc"
    break;

  case 775:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12359 "Parser/parser.cc"
    break;

  case 777:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 778:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 779:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12377 "Parser/parser.cc"
    break;

  case 780:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12383 "Parser/parser.cc"
    break;

  case 781:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12389 "Parser/parser.cc"
    break;

  case 782:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12395 "Parser/parser.cc"
    break;

  case 783:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12401 "Parser/parser.cc"
    break;

  case 784:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12410 "Parser/parser.cc"
    break;

  case 785:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12419 "Parser/parser.cc"
    break;

  case 786:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12428 "Parser/parser.cc"
    break;

  case 787:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12434 "Parser/parser.cc"
    break;

  case 788:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12443 "Parser/parser.cc"
    break;

  case 789:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12449 "Parser/parser.cc"
    break;

  case 791:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12455 "Parser/parser.cc"
    break;

  case 796:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12461 "Parser/parser.cc"
    break;

  case 797:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12467 "Parser/parser.cc"
    break;

  case 798:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12473 "Parser/parser.cc"
    break;

  case 800:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 12479 "Parser/parser.cc"
    break;

  case 801:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12485 "Parser/parser.cc"
    break;

  case 802:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12491 "Parser/parser.cc"
    break;

  case 803:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12497 "Parser/parser.cc"
    break;

  case 805:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12503 "Parser/parser.cc"
    break;

  case 806:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12509 "Parser/parser.cc"
    break;

  case 807:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12515 "Parser/parser.cc"
    break;

  case 808:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12531 "Parser/parser.cc"
    break;

  case 809:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12537 "Parser/parser.cc"
    break;

  case 810:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12543 "Parser/parser.cc"
    break;

  case 811:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12549 "Parser/parser.cc"
    break;

  case 812:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12555 "Parser/parser.cc"
    break;

  case 813:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12561 "Parser/parser.cc"
    break;

  case 814:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12567 "Parser/parser.cc"
    break;

  case 816:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12576 "Parser/parser.cc"
    break;

  case 817:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12582 "Parser/parser.cc"
    break;

  case 818:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12591 "Parser/parser.cc"
    break;

  case 819:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12601 "Parser/parser.cc"
    break;

  case 820:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12610 "Parser/parser.cc"
    break;

  case 821:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12620 "Parser/parser.cc"
    break;

  case 822:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12631 "Parser/parser.cc"
    break;

  case 823:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12641 "Parser/parser.cc"
    break;

  case 824:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12652 "Parser/parser.cc"
    break;

  case 825:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12662 "Parser/parser.cc"
    break;

  case 826:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12673 "Parser/parser.cc"
    break;

  case 827:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12683 "Parser/parser.cc"
    break;

  case 829:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12689 "Parser/parser.cc"
    break;

  case 830:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12695 "Parser/parser.cc"
    break;

  case 831:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12701 "Parser/parser.cc"
    break;

  case 832:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12713 "Parser/parser.cc"
    break;

  case 833:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12724 "Parser/parser.cc"
    break;

  case 834:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12733 "Parser/parser.cc"
    break;

  case 835:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12742 "Parser/parser.cc"
    break;

  case 836:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12748 "Parser/parser.cc"
    break;

  case 837:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12754 "Parser/parser.cc"
    break;

  case 838:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12760 "Parser/parser.cc"
    break;

  case 839:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12769 "Parser/parser.cc"
    break;

  case 840:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12775 "Parser/parser.cc"
    break;

  case 841:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 842:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12787 "Parser/parser.cc"
    break;

  case 847:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12793 "Parser/parser.cc"
    break;

  case 848:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12799 "Parser/parser.cc"
    break;

  case 849:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12809 "Parser/parser.cc"
    break;

  case 850:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12815 "Parser/parser.cc"
    break;

  case 853:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12821 "Parser/parser.cc"
    break;

  case 854:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12827 "Parser/parser.cc"
    break;

  case 855:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12833 "Parser/parser.cc"
    break;

  case 856:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12839 "Parser/parser.cc"
    break;

  case 858:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12845 "Parser/parser.cc"
    break;

  case 859:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12851 "Parser/parser.cc"
    break;

  case 860:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12857 "Parser/parser.cc"
    break;

  case 861:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12863 "Parser/parser.cc"
    break;

  case 866:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12869 "Parser/parser.cc"
    break;

  case 867:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12875 "Parser/parser.cc"
    break;

  case 868:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12881 "Parser/parser.cc"
    break;

  case 869:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12887 "Parser/parser.cc"
    break;

  case 870:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12893 "Parser/parser.cc"
    break;

  case 872:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12899 "Parser/parser.cc"
    break;

  case 873:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12905 "Parser/parser.cc"
    break;

  case 874:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12911 "Parser/parser.cc"
    break;

  case 875:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12917 "Parser/parser.cc"
    break;

  case 876:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12923 "Parser/parser.cc"
    break;

  case 877:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12929 "Parser/parser.cc"
    break;

  case 878:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12935 "Parser/parser.cc"
    break;

  case 879:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12941 "Parser/parser.cc"
    break;

  case 880:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12947 "Parser/parser.cc"
    break;

  case 881:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12953 "Parser/parser.cc"
    break;

  case 882:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12959 "Parser/parser.cc"
    break;

  case 883:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12965 "Parser/parser.cc"
    break;

  case 884:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12971 "Parser/parser.cc"
    break;

  case 885:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12977 "Parser/parser.cc"
    break;

  case 886:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12983 "Parser/parser.cc"
    break;

  case 887:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12989 "Parser/parser.cc"
    break;

  case 888:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12995 "Parser/parser.cc"
    break;

  case 889:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13001 "Parser/parser.cc"
    break;

  case 891:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13007 "Parser/parser.cc"
    break;

  case 892:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13013 "Parser/parser.cc"
    break;

  case 893:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13019 "Parser/parser.cc"
    break;

  case 894:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13025 "Parser/parser.cc"
    break;

  case 895:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13031 "Parser/parser.cc"
    break;

  case 896:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13037 "Parser/parser.cc"
    break;

  case 897:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13043 "Parser/parser.cc"
    break;

  case 898:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13049 "Parser/parser.cc"
    break;

  case 899:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13055 "Parser/parser.cc"
    break;

  case 900:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13061 "Parser/parser.cc"
    break;

  case 901:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13067 "Parser/parser.cc"
    break;

  case 902:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13073 "Parser/parser.cc"
    break;

  case 903:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13079 "Parser/parser.cc"
    break;

  case 904:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13085 "Parser/parser.cc"
    break;

  case 905:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13091 "Parser/parser.cc"
    break;

  case 906:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13097 "Parser/parser.cc"
    break;

  case 910:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13103 "Parser/parser.cc"
    break;

  case 911:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13109 "Parser/parser.cc"
    break;

  case 912:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13115 "Parser/parser.cc"
    break;

  case 913:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13121 "Parser/parser.cc"
    break;

  case 914:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13127 "Parser/parser.cc"
    break;

  case 915:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13133 "Parser/parser.cc"
    break;

  case 916:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13139 "Parser/parser.cc"
    break;

  case 917:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13145 "Parser/parser.cc"
    break;

  case 918:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13151 "Parser/parser.cc"
    break;

  case 919:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13157 "Parser/parser.cc"
    break;

  case 920:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13163 "Parser/parser.cc"
    break;

  case 921:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13169 "Parser/parser.cc"
    break;

  case 922:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13175 "Parser/parser.cc"
    break;

  case 923:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13181 "Parser/parser.cc"
    break;

  case 924:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13187 "Parser/parser.cc"
    break;

  case 925:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13196 "Parser/parser.cc"
    break;

  case 926:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13202 "Parser/parser.cc"
    break;

  case 927:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13208 "Parser/parser.cc"
    break;

  case 929:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13214 "Parser/parser.cc"
    break;

  case 930:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13220 "Parser/parser.cc"
    break;

  case 931:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13226 "Parser/parser.cc"
    break;

  case 932:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13232 "Parser/parser.cc"
    break;

  case 933:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13238 "Parser/parser.cc"
    break;

  case 934:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13244 "Parser/parser.cc"
    break;

  case 935:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13250 "Parser/parser.cc"
    break;

  case 936:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13256 "Parser/parser.cc"
    break;

  case 937:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13262 "Parser/parser.cc"
    break;

  case 938:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13268 "Parser/parser.cc"
    break;

  case 939:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13274 "Parser/parser.cc"
    break;

  case 940:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13280 "Parser/parser.cc"
    break;

  case 941:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13286 "Parser/parser.cc"
    break;

  case 942:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13292 "Parser/parser.cc"
    break;

  case 943:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13298 "Parser/parser.cc"
    break;

  case 944:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13304 "Parser/parser.cc"
    break;

  case 945:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13310 "Parser/parser.cc"
    break;

  case 946:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13316 "Parser/parser.cc"
    break;

  case 948:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13322 "Parser/parser.cc"
    break;

  case 949:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13328 "Parser/parser.cc"
    break;

  case 950:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13334 "Parser/parser.cc"
    break;

  case 951:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13340 "Parser/parser.cc"
    break;

  case 952:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13346 "Parser/parser.cc"
    break;

  case 953:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13352 "Parser/parser.cc"
    break;

  case 954:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13358 "Parser/parser.cc"
    break;

  case 955:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13364 "Parser/parser.cc"
    break;

  case 956:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13370 "Parser/parser.cc"
    break;

  case 957:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13376 "Parser/parser.cc"
    break;

  case 958:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13382 "Parser/parser.cc"
    break;

  case 959:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13388 "Parser/parser.cc"
    break;

  case 960:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13394 "Parser/parser.cc"
    break;

  case 961:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13400 "Parser/parser.cc"
    break;

  case 962:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13406 "Parser/parser.cc"
    break;

  case 963:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13412 "Parser/parser.cc"
    break;

  case 964:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13418 "Parser/parser.cc"
    break;

  case 965:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13425 "Parser/parser.cc"
    break;

  case 967:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13431 "Parser/parser.cc"
    break;

  case 968:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13437 "Parser/parser.cc"
    break;

  case 969:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13443 "Parser/parser.cc"
    break;

  case 970:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13449 "Parser/parser.cc"
    break;

  case 971:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13455 "Parser/parser.cc"
    break;

  case 972:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13461 "Parser/parser.cc"
    break;

  case 973:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13467 "Parser/parser.cc"
    break;

  case 974:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13473 "Parser/parser.cc"
    break;

  case 975:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13479 "Parser/parser.cc"
    break;

  case 976:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13485 "Parser/parser.cc"
    break;

  case 977:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13491 "Parser/parser.cc"
    break;

  case 978:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13497 "Parser/parser.cc"
    break;

  case 979:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13503 "Parser/parser.cc"
    break;

  case 980:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13510 "Parser/parser.cc"
    break;

  case 982:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13516 "Parser/parser.cc"
    break;

  case 983:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13522 "Parser/parser.cc"
    break;

  case 984:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13528 "Parser/parser.cc"
    break;

  case 985:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13534 "Parser/parser.cc"
    break;

  case 986:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13540 "Parser/parser.cc"
    break;

  case 987:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13546 "Parser/parser.cc"
    break;

  case 988:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13552 "Parser/parser.cc"
    break;

  case 989:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13558 "Parser/parser.cc"
    break;

  case 990:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13564 "Parser/parser.cc"
    break;

  case 991:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13570 "Parser/parser.cc"
    break;

  case 992:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13576 "Parser/parser.cc"
    break;

  case 994:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13582 "Parser/parser.cc"
    break;

  case 995:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13588 "Parser/parser.cc"
    break;

  case 996:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13594 "Parser/parser.cc"
    break;

  case 997:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13600 "Parser/parser.cc"
    break;

  case 998:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13606 "Parser/parser.cc"
    break;

  case 999:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13612 "Parser/parser.cc"
    break;

  case 1000:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13618 "Parser/parser.cc"
    break;

  case 1002:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13624 "Parser/parser.cc"
    break;

  case 1003:
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13630 "Parser/parser.cc"
    break;

  case 1004:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13636 "Parser/parser.cc"
    break;

  case 1005:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13642 "Parser/parser.cc"
    break;

  case 1006:
#line 3942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13648 "Parser/parser.cc"
    break;

  case 1007:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13654 "Parser/parser.cc"
    break;

  case 1008:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13660 "Parser/parser.cc"
    break;

  case 1009:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13666 "Parser/parser.cc"
    break;

  case 1010:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13672 "Parser/parser.cc"
    break;

  case 1011:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13678 "Parser/parser.cc"
    break;

  case 1013:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13684 "Parser/parser.cc"
    break;

  case 1014:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13690 "Parser/parser.cc"
    break;

  case 1016:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13696 "Parser/parser.cc"
    break;

  case 1017:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13702 "Parser/parser.cc"
    break;

  case 1019:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13708 "Parser/parser.cc"
    break;

  case 1020:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13714 "Parser/parser.cc"
    break;

  case 1021:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13720 "Parser/parser.cc"
    break;

  case 1022:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13726 "Parser/parser.cc"
    break;

  case 1023:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13732 "Parser/parser.cc"
    break;

  case 1024:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13738 "Parser/parser.cc"
    break;

  case 1025:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13744 "Parser/parser.cc"
    break;

  case 1028:
#line 4040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13751 "Parser/parser.cc"
    break;

  case 1029:
#line 4043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13757 "Parser/parser.cc"
    break;

  case 1030:
#line 4045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13763 "Parser/parser.cc"
    break;

  case 1031:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13769 "Parser/parser.cc"
    break;

  case 1032:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13775 "Parser/parser.cc"
    break;

  case 1033:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13781 "Parser/parser.cc"
    break;

  case 1034:
#line 4056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13787 "Parser/parser.cc"
    break;

  case 1035:
#line 4058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13793 "Parser/parser.cc"
    break;

  case 1037:
#line 4064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13799 "Parser/parser.cc"
    break;

  case 1038:
#line 4066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13805 "Parser/parser.cc"
    break;

  case 1039:
#line 4068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13811 "Parser/parser.cc"
    break;

  case 1040:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13817 "Parser/parser.cc"
    break;

  case 1041:
#line 4075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13823 "Parser/parser.cc"
    break;

  case 1042:
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13829 "Parser/parser.cc"
    break;

  case 1044:
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13835 "Parser/parser.cc"
    break;

  case 1046:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13841 "Parser/parser.cc"
    break;

  case 1047:
#line 4098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13847 "Parser/parser.cc"
    break;

  case 1048:
#line 4100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13853 "Parser/parser.cc"
    break;

  case 1049:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13859 "Parser/parser.cc"
    break;

  case 1050:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13865 "Parser/parser.cc"
    break;

  case 1051:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13871 "Parser/parser.cc"
    break;

  case 1053:
#line 4122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13877 "Parser/parser.cc"
    break;

  case 1054:
#line 4124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13883 "Parser/parser.cc"
    break;

  case 1055:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13889 "Parser/parser.cc"
    break;

  case 1056:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13895 "Parser/parser.cc"
    break;

  case 1057:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13901 "Parser/parser.cc"
    break;

  case 1058:
#line 4135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13907 "Parser/parser.cc"
    break;

  case 1059:
#line 4137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13913 "Parser/parser.cc"
    break;

  case 1061:
#line 4143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13919 "Parser/parser.cc"
    break;

  case 1062:
#line 4145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13925 "Parser/parser.cc"
    break;

  case 1063:
#line 4147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13931 "Parser/parser.cc"
    break;

  case 1064:
#line 4152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13937 "Parser/parser.cc"
    break;

  case 1065:
#line 4154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13943 "Parser/parser.cc"
    break;

  case 1068:
#line 4164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13949 "Parser/parser.cc"
    break;

  case 1071:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13955 "Parser/parser.cc"
    break;

  case 1072:
#line 4177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13961 "Parser/parser.cc"
    break;

  case 1073:
#line 4179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13967 "Parser/parser.cc"
    break;

  case 1074:
#line 4181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13973 "Parser/parser.cc"
    break;

  case 1075:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13979 "Parser/parser.cc"
    break;

  case 1076:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13985 "Parser/parser.cc"
    break;

  case 1077:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13991 "Parser/parser.cc"
    break;

  case 1078:
#line 4194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13997 "Parser/parser.cc"
    break;

  case 1079:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14003 "Parser/parser.cc"
    break;

  case 1080:
#line 4198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14009 "Parser/parser.cc"
    break;

  case 1081:
#line 4200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14015 "Parser/parser.cc"
    break;

  case 1082:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14021 "Parser/parser.cc"
    break;

  case 1083:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14027 "Parser/parser.cc"
    break;

  case 1084:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14033 "Parser/parser.cc"
    break;

  case 1085:
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14039 "Parser/parser.cc"
    break;

  case 1086:
#line 4211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14045 "Parser/parser.cc"
    break;

  case 1087:
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14051 "Parser/parser.cc"
    break;

  case 1088:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14057 "Parser/parser.cc"
    break;

  case 1089:
#line 4223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14063 "Parser/parser.cc"
    break;

  case 1090:
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14069 "Parser/parser.cc"
    break;

  case 1092:
#line 4252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14075 "Parser/parser.cc"
    break;

  case 1096:
#line 4263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14081 "Parser/parser.cc"
    break;

  case 1097:
#line 4265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14087 "Parser/parser.cc"
    break;

  case 1098:
#line 4267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14093 "Parser/parser.cc"
    break;

  case 1099:
#line 4269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14099 "Parser/parser.cc"
    break;

  case 1100:
#line 4271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14105 "Parser/parser.cc"
    break;

  case 1101:
#line 4273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14111 "Parser/parser.cc"
    break;

  case 1102:
#line 4280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14117 "Parser/parser.cc"
    break;

  case 1103:
#line 4282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14123 "Parser/parser.cc"
    break;

  case 1104:
#line 4284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14129 "Parser/parser.cc"
    break;

  case 1105:
#line 4286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14135 "Parser/parser.cc"
    break;

  case 1106:
#line 4288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14141 "Parser/parser.cc"
    break;

  case 1107:
#line 4290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14147 "Parser/parser.cc"
    break;

  case 1108:
#line 4295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14153 "Parser/parser.cc"
    break;

  case 1109:
#line 4297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14159 "Parser/parser.cc"
    break;

  case 1110:
#line 4299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14165 "Parser/parser.cc"
    break;

  case 1111:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14171 "Parser/parser.cc"
    break;

  case 1112:
#line 4306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14177 "Parser/parser.cc"
    break;

  case 1113:
#line 4308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14183 "Parser/parser.cc"
    break;

  case 1116:
#line 4332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14189 "Parser/parser.cc"
    break;

  case 1117:
#line 4334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14195 "Parser/parser.cc"
    break;


#line 14199 "Parser/parser.cc"

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
#line 4337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
