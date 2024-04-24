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
#define YYLAST   26453

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

#define YYPACT_NINF (-1843)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1116)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     140, 13514,   246,   288, 19814,    94, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,   170,  1092,
     226, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,   360,   349,
   -1843, -1843, -1843, -1843, -1843, -1843,  4374,  4374,   242, 13514,
     286,   314, 23483, -1843,   330, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843,   356,  3868, -1843,   889,   287,
   -1843, -1843, -1843, -1843, -1843, 19349, -1843, -1843,   340,   370,
     479,   189, -1843,  4374,   379,   411,   420,   423,  5270,   609,
     750, 13679, -1843, -1843,   566,  9229,  2427, -1843, -1843, -1843,
   -1843,  3166,   641,  8216, 11773,   891,  3166,   914,   515, -1843,
   -1843, -1843, -1843,    51, -1843, -1843, -1843, -1843,   536, -1843,
   -1843, -1843, -1843, -1843,   617,   651,    51, -1843,    51, 17807,
   -1843, -1843, -1843, 21144,  4374, -1843, -1843,  4374, -1843, 13514,
   -1843,   643, 21197, -1843, -1843,  5338, 22496, -1843, -1843,   991,
     991,   692,  2230, -1843, -1843, -1843, -1843,   456, 16103,    51,
    3596,    51, -1843, -1843, -1843, -1843, -1843, -1843,   678, -1843,
     712,   711,  2266, -1843,   759, 25902, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843, 18186,  2813,  2800,  3868,   605,   735,   766,
     805,   818,   823,   828, -1843, -1843, 19969, 12188,   785,   817,
   -1843, 20272, -1843, -1843, -1843, -1843,   832, -1843, -1843,   842,
   -1843, 23850,   987, 24002, -1843,   864,  4374,   651,   876,  3110,
    5338,  3110, -1843, -1843, -1843,  3451,  3903,   899,   924,   336,
     924, -1843,    51,    51,    23, 17485,   348,   924, -1843,    51,
      51,    23,    51, -1843,    51, -1843,  4149, -1843, -1843,   915,
     948,   991, 23240,   951, 19349, -1843, -1843,  3166, -1843,  1412,
     515,   953,  1032, 17485,  4374,  4374,   479, -1843, 15447, -1843,
     991,   991,   961,  1032, 17485,  4374, -1843, 23713, -1843, -1843,
   -1843,   991, -1843, -1843, -1843, -1843,   991, -1843,   908,  3391,
    4374, -1843, 19203,   984, -1843, -1843, -1843, 23103,   651, 17646,
     963,  5338, 19150, 23240,  3166, -1843, -1843, 22644, -1843,   924,
      35, -1843, 25902, 22496,  4016,  4149, -1843,   621, -1843, -1843,
   -1843, -1843, -1843, 21197,  4374, -1843,   986,  1018, -1843, -1843,
   -1843, -1843,  4374,  3725,   443,   258, -1843,  4374,   712, -1843,
     883,    51, -1843,  1015, 21352,   834, 16595, 23293,  3166, -1843,
    3166,   991,  3166,   991, -1843, -1843,    51, -1843, -1843,  1023,
   21405, -1843, -1843, -1843, 21560,   832, -1843,  3286,   551,   603,
   -1843,   665,   515,  1025,  1033, -1843,  2230,  1035,   712,  2230,
   -1843, -1843, -1843, -1843, -1843,  2813, -1843,   626, -1843,  1083,
   -1843,  1062,  1123, 25978,  1095,  1097,  1105, 25902, 26054,  1110,
   23598, -1843, -1843, -1843, -1843, -1843, -1843, 26130, 26130, 18027,
    1096,  4119, -1843, -1843, -1843, -1843,   564, -1843,   652, -1843,
    1630, -1843, 25902, 25902, -1843,  1098,   630,   967,   950,   440,
    1019,  1100,  1116,  1107,  1153,    -5, -1843,   881, -1843,  1142,
   -1843,  1064,  4501, 18663, -1843, -1843,   390,  1142, -1843, -1843,
     890, -1843, -1843,   898,  2800,  1162,  1180,  1192,  1205,  1208,
    1220, -1843, -1843,   689,  1221, -1843,    -6,  1221,  1253, -1843,
    1255, -1843, 21144, -1843,  1118,  1274, 18822, -1843, -1843,  4131,
    3146,  1276, 16595,  1302,  1184,  1280,  1281,  1283, -1843, -1843,
   -1843,  4374,  4303, 20626, -1843, -1843, -1843, -1843, -1843, -1843,
    5197,  3986,  1096, 23850,  1285,  1319, -1843, -1843,  1303, 24002,
     798, -1843, -1843, -1843, 24078,  1328, -1843, -1843, -1843, -1843,
    1313,  3451,   755,  1341,  1363,  1374,   812,  1377,  1381,  1410,
    1421,  1423,  1429,  3903, -1843, -1843, -1843,    51,  1354,  1391,
    1325, -1843, -1843,  1437,   479, -1843, -1843,   651,  1032, 20133,
   -1843, -1843,   479, -1843, -1843,   651, -1843, -1843,  4149, -1843,
   18663, 18663, -1843,   991,  5338, 23430,  3084, 16759, -1843, -1843,
   -1843, -1843, -1843,   651,  1032,    35,  1425, -1843, -1843,  3166,
    1435,  1032, 17485, -1843,   651,  1032, -1843, 23764, -1843,   991,
     991, -1843, -1843,  1454,   391,  1457,   515,  1459, -1843, -1843,
   -1843, 20573,  1467,  1465, -1843, -1843,   910, -1843,  1559, -1843,
    1466, -1843, -1843, -1843, 21724, 26206, -1843, -1843, -1843, -1843,
   -1843,  4016,   837,  4149, 20133, 16923,   924, 13514, -1843,  4374,
    1477, -1843,  1497, -1843, -1843, -1843, -1843, -1843,  2230, -1843,
   -1843,  1576,  4096, 20781, 12188, -1843, 21777, -1843,   991,   991,
   -1843, -1843,   832, -1843, 15775,  1495,  1638, 25902,  1068,  1437,
    1478, -1843,    51,    51, -1843,  1221, -1843, 21352, -1843, -1843,
   20573,   991,   991, -1843,  4096, -1843, -1843, 22348, -1843, -1843,
   21405, -1843,    51,  1500,    51,  1033,   214,  1501,   911, 21197,
     930,   936, -1843,  2813, 24154,  1480, -1843, 18345, -1843,  4119,
   21932, 21197, -1843, 18345, -1843, 25902, -1843, -1843, -1843, -1843,
   -1843, -1843, 18504, -1843, -1843, 20834, 21932, 21932,  1260,  1326,
    1509,   471,  1720, -1843,   944,  1510,  1288,  1511, -1843, 24078,
   25902, 24230,  1503, 25902,  3110, 25902,  3110, -1843,  1433, -1843,
   -1843, 24154,  2741, 25902, 24154,  3110, -1843, -1843, 25902, 25902,
   25902, 25902, 25902, 25902, 25902, 25902, 25902, 25902, 25902, 25902,
   25902, 25902, 25902, 25902, 25902, 25902, 25902, 24306,  1489,   759,
    3797, 12188, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843, -1843, -1843,  1508, 25902, -1843, -1843, 15939,  1168, -1843,
   -1843,    51,    51, -1843, -1843, 18663, -1843, -1843,   702,  1221,
   -1843,   893,  1221, 20133, -1843, -1843,  1437, 20133, -1843,  1437,
   -1843, 26282, -1843, -1843, -1843, 19659, 12188,  1512,  1306,  1513,
   15283,  1658,  3052,   722,  1478, -1843,    51,    51,  1478,   727,
   -1843,    51,    51, 25902,  4374, 16759,  1517, 16759,  1519,  1478,
     234,  7459,  7459,  7459,  4374, -1843, -1843, 25902,  1303, -1843,
   23850,  1526, -1843,  2520, -1843, -1843, -1843, -1843, -1843,   962,
   -1843,  7459, 25902,   906,  1531,  1534,  1536,  1002,  1537,  1540,
    1544,  1545,  1546,  1547,   788,  1221, -1843, -1843,   800,  1221,
   -1843, -1843,   815,  1221, -1843, -1843, -1843,  5338,   759,  1661,
    1221, 22792, -1843, -1843,   651,  1548, -1843, -1843, -1843,  1011,
    1550,  1052,  1551, -1843,  1253,  1552,  1556, -1843,   651, -1843,
    1561, -1843,   651,  1032,  1556, -1843,   651,  1553,  1554,  1555,
   -1843, -1843, 20436, -1843,  3110,  4374, 11240,  1650, -1843,  1274,
   -1843,  7459,  1004,  1562, -1843,  1556,  1565, -1843, 21985, 18663,
    1549, -1843,  1549, -1843, -1843, -1843, -1843, 21405, -1843, 12356,
   18981, -1843,  1567,  1568,  1572,  1573, -1843, 13264,    51, -1843,
    1068, -1843, -1843, -1843, -1843,  1437, -1843, -1843, -1843,   991,
   -1843, -1843, -1843, -1843,   214,  1033,  1577,   456, -1843, -1843,
    1581,  4374,   214, -1843, -1843,  1582,  1587, -1843, -1843,  1054,
   -1843, -1843, -1843, -1843,  1590,  1591,  1588,  1598,  1589,  1594,
    1605,  1606,  1604,  1609, 25902,  1610,  1614,  1617, 22140, 12524,
   25902, -1843, -1843,  1894, -1843, -1843, -1843, 25902, -1843,  1620,
    1621, 23926,  1330, -1843, 24154,  1622, -1843,  1627, -1843, -1843,
    5464, -1843,  1102, -1843, -1843, -1843,  5464, -1843, -1843,  1334,
     713, -1843, -1843,  1098,  1098,  1098,   630,   630,   967,   967,
     950,   950,   950,   950,   440,   440,  1019,  1100,  1116,  1107,
    1153, 25902,  1335, -1843,  1623,  5464, -1843, -1843, 23850, -1843,
    1639,  1642,  1643,  1644,  1168, -1843, -1843, -1843, -1843, -1843,
   20133, -1843, -1843,  1437, 20133, -1843,  1437,  1645,  1646,  7459,
    7459, -1843, -1843, 15283,   867,  1649,  1651,  1652,  1653,  2835,
    3052, -1843, -1843, 20133, -1843, -1843, -1843, -1843, -1843, -1843,
   20133, -1843, -1843, -1843, -1843,  1647, -1843,  1478,  1654, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843,  1655,  1656,  1657,
   -1843, -1843,   479,  5464,  1340,    67, -1843, -1843,  1659, -1843,
   24002, -1843, 25902,    51, 24382,  7459, -1843, -1843,   878,  1221,
   -1843,   939,  1221, -1843, -1843,   940,  1221, 20133, -1843, -1843,
    1437, 20133, -1843, -1843,  1437, 20133, -1843, -1843,  1437,   924,
    1663, -1843,  1437,   228, -1843,  1142,  1660, -1843, -1843, -1843,
   -1843, -1843, -1843,  1669, -1843, -1843, -1843, 21985,  1556, -1843,
     651, -1843, -1843, -1843, -1843, -1843, 14332, -1843, -1843, -1843,
   -1843,   262, -1843,   392,   415, 12020,  1671,  1672, 17307,  1673,
    1674,  2681,  2937,  2099, 24458,  1677, -1843, -1843,  1678,  1681,
   17307,  1682, -1843, -1843,   651, 25902, 25902,  1800,  1676,   733,
   -1843, 17868,  1343,  1679,  1662, -1843, -1843, -1843, 11062, -1843,
   -1843, -1843, -1843, -1843,  2363, -1843, -1843, -1843,  1416,   197,
   -1843,   185, -1843,   197, -1843, -1843, -1843, -1843, -1843,  3110,
   -1843, -1843, 13844, 19504,  1683, -1843,  4374,  1686,  1690, -1843,
   16923, -1843, -1843,  4374, -1843, -1843,  5338, -1843, -1843,  1675,
    1684,  1150, 21197,   712,   712, -1843, -1843,  1096,  1274, 18822,
   -1843,  1142, -1843, 12692, -1843,   974,  1221, -1843,   991, 13345,
   -1843, -1843,  1033,  1581,  1688,   214,   515,   292,  1698,  1680,
    1581,  1700, -1843, -1843, 24154,   753, -1843, 20573, 12524,  3110,
   -1843,   753, -1843, 20989,   753, -1843, 25902, 25902, 25902, -1843,
   -1843, -1843, -1843, 25902, 25902,  1694, 23850, -1843, -1843,  1697,
     783, -1843, -1843, -1843,  4685, -1843, -1843,  1352, -1843,     8,
   -1843,  1356, -1843, 24078, -1843, -1843, 25902,  1687,  1368,  1376,
    1303, -1843,  1053,  1221, -1843, -1843,  1704,  1705, -1843, -1843,
   -1843, -1843,  1709,  1133,  1221, -1843,  1167,  3962,    51,    51,
   -1843, -1843,  1712,  1713, -1843,  1714, -1843, 16759,  1715, -1843,
   16267, 16431,  1716,  1724, -1843,  1711, 25902, 25902,  1380,  1717,
   -1843, -1843, -1843, -1843, -1843, -1843, -1843,  1725, 20133, -1843,
   -1843,  1437, 20133, -1843, -1843,  1437, 20133, -1843, -1843,  1437,
    1726,  1728,  1733,   479,    51, -1843, -1843,  1393, 25902, 22944,
    1734,  1739, -1843, -1843, -1843,  1741, 14490, 14648, 14806, 21985,
   23240, 21932, 21932,  1742, -1843,   457,   485,  2483, 15119, -1843,
     524,  4374,  4374, -1843, 24154,   -25,    33, -1843, -1843, -1843,
   -1843, 12020, 25902,  1744,  1820, 11851, 11418, -1843,  1721, -1843,
    1736, 25902,  1740, 23850,  1743, 25902, 24078, 25902, -1843, 11596,
    1078, -1843,  1747,   -35, -1843,    21,  1812,   582, -1843,  1751,
   -1843,  1748, -1843,  1749,  1758,  1759, 17307, 17307, -1843, -1843,
    1829, -1843, -1843,     3,     3,   715, 15611,    51,   529, -1843,
   -1843,  1764,  1768,   443, -1843,  1777, -1843,  1772, -1843,  1773,
   -1843, -1843, -1843, -1843, 12860,  1781,  1782,  1784, -1843, 20133,
   -1843, -1843,  1437, 25902, 25902,  1274,  1785, -1843,  1774,  1792,
     214,  1581,   456,  4374, -1843, 24534, -1843,  1795, -1843, 21985,
   -1843,  1146,  1794,  1791,  1190, -1843,  1793, -1843, -1843, -1843,
   -1843, -1843, 23850,  1303, 24078, -1843,  1831,  5464, -1843,  1831,
    1831, -1843,  5464,  4923,  5317, -1843,  1396, -1843, -1843, -1843,
    1803, 20133, -1843, -1843,  1437, -1843, -1843,  1802,  1804,    51,
   20133, -1843, -1843,  1437, 20133, -1843, -1843,  1806, -1843, -1843,
   -1843, -1843, -1843, -1843, -1843, -1843,  1654, -1843, -1843, -1843,
    1798, -1843, -1843, -1843, -1843,  1811,  1815,    51,  1821,  1824,
    1825, -1843, -1843, -1843, -1843, -1843, 25902, -1843,   228, -1843,
    1142, -1843, -1843,  1823,  1832, -1843,  1742,  1742,  1742,  4820,
    1223,  1805,   558, -1843,  4820,   573, 18663, -1843, -1843, -1843,
    4626, 25902,  4423,   127,  1822, -1843, -1843,   112,  1827,  1827,
    1827,  4374, -1843, -1843, -1843,  1197, -1843, -1843, -1843, -1843,
    1679,  1830, 25902,   340,  1833,   423, 14971, 21985,  1198,  1835,
   17307,  1839, -1843, -1843, -1843,  1016, 17307, 25902,   669,   670,
   -1843, 25902, 23769, -1843, -1843,   595, -1843,  1303, -1843,  1199,
    1207,  1213,   679, -1843, -1843, -1843, -1843,   651,  1078,  1837,
   -1843, -1843, 25902, -1843,  1843,   759, -1843, -1843, -1843, -1843,
   25902, 25902, -1843, -1843,   407,     3, -1843,   225, -1843, -1843,
   10884, -1843,    51, -1843,  1549, -1843, 21985, -1843, -1843, -1843,
   -1843, -1843,  1840,  1842, -1843, -1843,  1844, -1843,  1846,   214,
   -1843,  1581,  1848,   301,  1680, 23850, -1843, -1843, -1843,  1850,
   -1843, -1843, 25902, -1843, 20989, 25902,  1303,  1855,  1398, -1843,
    1401, -1843,  5464, -1843,  5464, -1843, -1843, -1843,  1853,    51,
      51,  1856,  1857, -1843,  1852, -1843, -1843, -1843, -1843, -1843,
    1405, 25902, -1843, -1843, -1843, -1843, -1843,  1841,  1223,  1953,
     604, -1843, -1843, -1843, -1843,    51,    51, -1843, -1843, -1843,
    1849, -1843,  1218,  4626,   781, -1843,  4423, -1843, -1843,    51,
   -1843, -1843, -1843, -1843, -1843, 17307, 17307,  1679, 17087,   102,
   24610,  1948, 17307, -1843, -1843, -1843, -1843, 25902, -1843, 24686,
    1949,  1854,  8906, 24762, 17307, 11596,  1679,   751,  1248,  1860,
   25902, -1843,  1878,   459, 17307, -1843, 17307, -1843,  1880, -1843,
   -1843,  1861,   759,   736,  1886,  1413,  1226, 17307,  1892, 17307,
   17307, 17307, -1843, -1843, -1843,   712, -1843,  4374,  5338, -1843,
   -1843,  1888,  1890, -1843, -1843,  1581,  1897, -1843, -1843, -1843,
    1303,  1898, -1843, -1843, -1843, -1843,  1903, -1843, -1843, -1843,
    1424,  1430, -1843, -1843, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843,  1901,  1902,  1904,  1953, -1843,    51, -1843, -1843, -1843,
   -1843, -1843,  1905,  4820, -1843,  1985,  4551,   146, 13031, -1843,
   17184, -1843,    11,  1227, 17307,  1988,   629,  1895,   372, 17307,
   25902,  1959,  1910,   751,  1248,  1889, -1843, 24838,  1899,   436,
    1996, -1843, 24914, 24990, 25902,  1679,  1893, 13198, -1843, -1843,
   -1843, -1843, 22193, -1843,  1912,  1900,     1, -1843, 25902, 24154,
   -1843, -1843, 25902,   197, -1843, -1843, -1843, -1843, -1843, -1843,
   -1843,  1923, -1843,  1927, -1843, -1843, -1843, -1843,  1170,  1221,
   -1843, -1843,  1223, -1843, 17307, -1843,    85, -1843,    89, -1843,
   -1843, -1843,  1928, 14009, -1843, -1843, 17307, -1843,    53, -1843,
   17307, 25902,  1932, 25066, -1843, -1843, -1843, 25142, 25218, 25902,
    1679, -1843, 25294, 25370, 17307,  1913,   458,  1919,   501, -1843,
   -1843,  1929, 14009, 22193, -1843,  4947, 21777,  3110,  1930, -1843,
    1990,  1939,   763,  1937, -1843, -1843,  1235,  1250,   502, -1843,
   -1843, 20133, -1843, -1843,  1437, -1843, -1843, 25902, -1843, 25902,
   -1843, -1843,  1524, 14174, -1843, -1843, 17307, -1843, -1843,  1679,
   -1843, -1843,  1679,  1931,   527,  1933,   542, -1843, -1843,  1679,
   -1843,  1679, -1843,  1943, 25446, 25522, 25598, -1843,  1524, -1843,
    1925,  3422,  3647, -1843, -1843, -1843,     1,  1946, 25902,  1935,
       1,     1, -1843, -1843, 17307,  2028,  1950, -1843, -1843, 17184,
   -1843,  1524, -1843, -1843,  1954, 25674, 25750, 25826, -1843, -1843,
    1679, -1843,  1679, -1843,  1679, -1843,  1925, 25902,  1960,  3647,
    1962,   759,  1964, -1843,   784, -1843, -1843, 17307, -1843, -1843,
   10596,  1957, 17184, -1843, -1843,  1679, -1843,  1679, -1843,  1679,
    1971,  1969, -1843,   651,   759,  1972, -1843,  1952,   759, -1843,
   -1843, -1843, -1843, 10738, -1843,   651, -1843, -1843,  1447, 25902,
   -1843,  1261, -1843,   759,  3110,  1973,  1955, -1843, -1843,  1272,
   -1843, -1843,  1961,  3110, -1843, -1843
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
       0,   743,   732,   731,     0,     0,   833,     2,   455,   457,
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
    1115,     0,   738,     0,     2,   741,   744,   184,   183,     0,
       2,   490,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   850,   883,   887,   926,   850,   940,
     944,   952,   850,   962,   875,   932,   955,     0,     0,     0,
    1008,     0,   467,   834,     0,     0,   468,   835,   460,     0,
       0,     0,     0,   458,     0,     2,     2,   836,     0,   441,
       2,   805,     0,   831,     2,   837,     0,     0,     0,     0,
     628,   892,   490,   910,     0,     0,   490,   421,   419,   105,
       3,   490,     0,     3,   806,     2,     0,   758,   490,   490,
     752,   751,   752,   549,   547,   674,  1077,   490,  1082,   491,
     490,  1068,     0,     0,     0,     0,  1046,     0,   850,  1117,
    1032,  1033,   709,  1029,  1030,  1044,  1072,  1076,  1074,   578,
     613,  1080,  1085,   666,   672,   672,     0,     0,   681,   680,
    1114,     0,   672,   785,   783,     0,     0,   858,    73,     0,
      70,    71,    74,   817,     0,     0,     0,     2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   490,   490,
       0,   126,   125,     0,   122,   121,    28,     0,    29,     0,
       0,     0,     0,     3,    69,     0,    52,     0,    53,    62,
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
       0,   733,     0,   850,     0,   490,     3,   462,   850,   884,
     888,   850,   941,   945,   953,   850,   963,   490,   876,   879,
     881,   490,   933,   936,   938,   490,   956,   958,   960,   848,
       0,   470,  1009,     3,  1013,  1014,     3,   839,   949,   562,
     561,   564,   563,     2,   806,   840,   787,   490,     2,   838,
       0,   806,   841,   628,   628,   628,   490,   688,   691,   692,
     721,     0,   424,     0,     0,   490,     0,     0,   345,     0,
       0,     0,     0,     0,   189,     0,   340,   341,     0,     0,
     345,     0,   393,   392,     0,   164,   164,   399,   589,   595,
     206,   490,     2,   190,     0,   217,   191,   192,   490,   211,
     193,   194,   195,   196,     0,   197,   198,   346,     0,   360,
     199,   366,   368,   371,   200,   201,   202,   203,   204,     0,
     205,   213,   572,   490,     0,   215,     0,     0,     0,     3,
     490,   819,   806,     0,   794,   795,     0,     3,   790,     3,
       3,     0,   490,   769,   769,  1079,  1084,     2,   105,   490,
       3,   587,     3,   491,  1040,   850,  1039,  1042,   490,     3,
    1028,  1034,   672,  1114,     0,   672,   677,   672,     0,   682,
    1114,     2,   854,   861,     0,    98,   101,   490,   490,     0,
     104,   100,   102,   490,     0,   116,     0,     0,     0,   120,
     124,   123,   188,     0,     0,     0,   736,   113,   181,     0,
       0,    46,    47,    87,     0,    87,    87,     0,    75,    77,
      49,     0,    45,     0,    48,   159,     0,     0,     0,     0,
    1114,  1005,   850,  1004,  1007,   999,     0,     0,   893,   911,
       3,     3,     0,   850,   975,   978,   850,     0,   850,   850,
     970,   987,     0,     0,  1109,     0,   713,   490,     0,  1111,
     490,   490,     0,     0,   438,     3,     0,     0,     0,     0,
     735,   740,     3,   832,   186,   185,     3,     0,   490,   877,
     880,   882,   490,   934,   937,   939,   490,   957,   959,   961,
       0,     0,     0,   730,   850,  1020,  1019,     0,     0,     0,
       0,     0,     3,   806,   842,     0,   490,   490,   490,   490,
     490,   490,   490,   611,   641,     0,     0,   642,   572,   629,
       0,     0,     0,   422,    69,     0,     0,   331,   332,   214,
     216,   490,     0,     0,     0,   490,   490,   327,     0,   325,
       0,     0,     0,   736,     0,     0,     0,     0,   372,   490,
       0,   165,     0,     0,   400,     0,     0,     0,   221,     0,
     212,     0,   322,     0,     0,     0,   345,   345,   351,   350,
     345,   362,   361,   345,   345,     0,   572,   850,     0,  1024,
    1023,     0,     0,   761,   797,     2,   792,     0,   793,     0,
     773,   753,   757,   755,   490,     0,     0,     0,     3,   490,
    1035,  1037,  1038,     0,     0,   105,     0,     3,     0,     0,
     672,  1114,     0,     0,   661,     0,   676,     0,   786,   490,
      72,  1025,     0,     0,     0,    39,     0,   117,   119,   118,
     115,   114,   736,  1114,     0,    68,    84,     0,    78,    85,
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
   -1843,  6012,  2336, -1843,    -1,   382,  2369,  -146, -1843,  -371,
   -1843,   355, -1843,  -731, -1843,   809,  -993, -1172, -1843,   255,
    2314,  2038, -1843,   432, -1843,  1403,   549,   853,   870,   464,
     868,  1365,  1367,  1370,  1372,  1366, -1843,  -117,  -152,  9007,
     909, -1843,  1696, -1843, -1843,  -694,  8164, -1108,  3681, -1843,
    1801, -1843,   902,     0, -1843, -1843,   680,    88, -1843, -1837,
   -1738,   291,    58, -1843, -1843,   672,   304,   206, -1640, -1843,
   -1432, -1843, -1843, -1843, -1843,   106,  -914, -1843, -1843, -1237,
     433, -1843, -1843, -1843, -1843, -1843,   128, -1212, -1843, -1843,
   -1843, -1843, -1843,    30,   450,   455,   131, -1843, -1843, -1843,
   -1843,  -674, -1843,    65,     9, -1843,   136, -1843,  -164, -1843,
   -1843, -1843,   903,  -797, -1003,   -34, -1843,   184,    29,   238,
    9226,  -771,  -740, -1843,   -26, -1843, -1843,   100, -1843,  -149,
    6443,  -297,  -252,  3097,   489,  -656,    43,   156,   315,   904,
    3676, -1843, -1843,  2133, -1843,   210,  4973, -1843,  2070, -1843,
     118, -1843, -1843,  1884,   504,  5572,  4197,   -30,  1916,  -325,
   -1843, -1843, -1843, -1843, -1843,  -316,  6489,  5889, -1843,  -407,
     190, -1843,  -803, -1843,   253, -1843,   186,   737, -1843,   -20,
    -190, -1843, -1843, -1843, -1843,  -168,  6825,  -933,   879,  -120,
     256, -1843,  -318,  -140,    87,  1731,  1869,  -776,  -160,   926,
    4470,     5,  -137,  -269,  -208,  -492,  1337, -1843,  1699,   113,
    -936,  1574, -1843, -1843,   676, -1843, -1230,  -179,  -180,  -921,
   -1843,    28, -1843, -1843, -1154,   454, -1843, -1843, -1843,  2201,
    -789,  -504,  -723,   -17, -1843, -1843, -1843, -1843, -1843, -1843,
      -8,  -895,  -196, -1842,   -40,  8626,   -65,  6978,  -103,  1499,
   -1843,   793,   -94,  -217,  -210,  -209,    26,   -71,   -70,   -52,
     500,    95,   130,   165,  -195,   -85,  -183,  -161,  -139,   162,
    -127,  -125,   -63,  -766,  -735,  -732,  -713,  -752,  -133,  -708,
   -1843, -1843,  -749,  1406,  1407,  1431,  2222, -1843,   583,  7839,
   -1843,  -658,  -629,  -585,  -512,  -563, -1843, -1672, -1761, -1754,
   -1747,  -645,  -122,  -287, -1843, -1843,   -46,    13,  -107, -1843,
    8258,  2073,  -788,  -341
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   848,   426,   427,    83,    84,   428,   402,   429,
    1564,  1565,   430,   999,  1000,  1001,  1357,  1358,  1359,  1576,
     452,   432,   433,   434,   731,   732,   435,   436,   437,   438,
     439,   440,   441,   442,   443,   444,   445,   454,  1145,   733,
    1492,   794,   223,   796,   448,   869,  1243,  1244,  1245,  1246,
    1247,  1248,  1249,  2150,  1250,  1251,  1681,  2007,  2008,  1939,
    1940,  1941,  2120,  2121,  1252,  1699,  1700,  1956,  1701,  1848,
    1849,  1253,  1254,  1255,  1256,  1257,  1258,  1875,  1879,  1514,
    1506,  1259,  1260,  1513,  1507,  1261,  1262,  1263,  1264,  1265,
    1266,  1267,  1718,  2138,  1719,  1720,  2044,  1268,  1269,  1270,
    1495,  2052,  2053,  2054,  2178,  2189,  2072,  2073,   308,   309,
     936,   937,  1211,    86,    87,    88,    89,    90,  1684,   488,
      93,    94,    95,    96,    97,   237,   238,   311,   290,   490,
      99,   491,   100,   611,   102,   103,   157,   357,   314,   107,
     108,   109,   173,   110,   954,   358,   158,   113,   261,   114,
     159,   269,   360,   361,   362,   160,   449,   119,   120,   364,
     121,   607,   929,   927,   928,  1657,   365,   366,   124,   125,
    1206,  1459,  1663,  1664,  1665,  1810,  1811,  1460,  1652,  1830,
    1666,   126,   695,  1312,   169,   989,   367,   990,   991,  1556,
     962,   613,  1137,  1138,  1139,   614,   368,   499,   500,   616,
    1274,   458,   459,   224,   517,   518,   519,   520,   521,   345,
    1293,   346,   952,   950,   646,   347,   387,   348,   349,   460,
     128,   179,   180,   129,  1287,  1288,  1289,  1290,     2,  1193,
    1194,   637,  1281,   130,   335,   336,   271,   282,   590,   131,
     227,   132,   326,  1147,   919,   551,   171,   133,   397,   398,
     399,   134,   328,   241,   242,   243,   329,   136,   137,   138,
     139,   140,   141,   142,   246,   330,   248,   249,   250,   331,
     252,   253,   254,   834,   835,   836,   837,   838,   255,   840,
     841,   842,   799,   800,   801,   802,   552,  1186,  1438,   143,
    1769,   670,   671,   672,   673,   674,   675,  1813,  1814,  1815,
    1816,   660,   501,   372,   373,   374,   461,   215,   145,   146,
     147,   376,   861,   676
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   388,   383,    82,   240,   198,   199,   587,   369,   572,
     971,   196,   736,   247,   144,   523,  1294,   144,   533,   355,
    1042,   858,  1510,  1050,   200,   534,   535,   135,  1291,  1497,
      91,   678,   977,   447,   529,   963,   318,   384,   401,   214,
     536,  1275,   183,  1442,   104,  1032,   371,  1921,   205,  1085,
     742,  1313,   537,  1361,  1922,    82,    82,   655,    82,  1320,
     569,  1923,  1860,  1532,  1533,  1722,  1111,   496,   446,  1129,
    1148,  1131,   144,   686,   538,    82,  1025,   689,   916,   964,
    1112,   558,  1368,   688,    82,   135,  2016,   691,    91,   924,
    2068,   463,    82,  1229,  2009,  1105,   539,    82,  1106,   483,
      82,    98,   104,  2015,    82,  1724,   214,   533,   540,   584,
     541,   212,   225,   310,   534,   535,  1484,  1107,   144,   115,
     595,   400,  1108,    58,   244,   465,   466,   272,  2076,   536,
     776,   283,   321,  1583,   945,  1201,   548,  1925,   303,  1271,
    -799,   537,   544,    82,   467,  1723,    82,  2010,    82,   817,
    1405,   545,   965,   550,    82,  1676,  1282,   105,  1735,    98,
    2050,    82,   144,   538,   198,   199,  1584,   678,   650,    82,
     531,   201,   542,   777,   971,   135,  1725,   115,    91,  1936,
    1937,   396,  1578,   200,    58,   539,  -831,    75,   153,   655,
     627,  2017,   104,   396,    82,    82,   303,   540,   549,   541,
     554,   963,  1318,  1454,  1406,    82,   202,   562,   650,   693,
      82,   111,  1957,  1678,   161,   105,   697,   240,   579,   310,
     514,   212,  2067,   280,   987,    82,   247,  2009,    82,    82,
      82,   544,   661,  2077,    82,    82,   557,   505,  1407,    92,
     545,   203,   154,   565,   694,   964,   148,   310,    75,    98,
    2069,  2070,  2001,   198,   199,    82,  1407,   115,   310,   631,
     251,   542,   212,    82,   583,  1938,  1853,   115,   602,   111,
    1115,   550,   200,    82,    82,   594,  1122,    82,   225,   626,
     628,  1827,  2015,   620,    82,   902,   678,   579,  -800,   276,
     212,   468,   701,   906,   497,   313,  1511,    92,    82,    82,
    1881,    82,  1552,  1350,   591,   105,    82,  1828,  2011,  1508,
      82,   862,  1311,   596,   874,  2015,   106,  1505,   965,  1512,
    1470,   875,   876,    82,    82,   162,   469,  1496,   608,   972,
     201,   678,  1509,    82,  2119,  1375,   877,   622,  1006,   265,
    1921,    82,    82,   277,  1390,  1048,    82,  1922,   878,   111,
     226,   212,  -831,  1275,  1923,   678,   956,   839,  1391,   111,
    2119,   470,   678,   115,   661,   202,   958,  1341,  1105,    20,
     879,  1106,   908,  2104,   106,   824,    82,  1435,   913,   988,
     976,   167,  1551,  2152,   380,    82,   151,    92,    82,   627,
    1107,   115,   880,   981,   396,  1382,   483,   182,   546,  1436,
     203,   313,   115,   982,   881,  1768,   882,   214,    58,  1443,
    1781,  1783,  1785,   667,   874,   647,   267,  1198,  1469,   648,
      58,   875,   876,  1462,   737,  1962,  1963,   115,   923,   313,
    1925,   280,    58,   910,   912,  1455,   877,   496,   177,   177,
     313,   184,  1463,  1454,  1454,  1454, -1115,   206,   878,   894,
     163,  1271,  1311,   164,   165,   111,   166,   988,   895,   298,
     465,   466,   380,    82,   106,   313,  1456,  2001,   883,   185,
     879,  1441,    75,   623,   963,   177,  1465,  1466,  1445,   467,
    1555,   205,  1877,   111,    75,   193,   355,   546,    82,    82,
     505,   302,   880,  1116,   111,   550,    75,  1119,  1504,   698,
      82,    82,   700,   559,   881,   116,   882,   550,  1134,  1135,
      82,   194,   514,   371,   496,   168,   588,  1878,   964,   111,
     218,  2027,  2028,  1497,    74,  1549,   177,   986,   956,   177,
      82,    -3,  1557,  1027,  1140,  1141,  1936,  1937,   958,   894,
     229,  1115,    82,  2023,   177,   797,   193,  1464,   895,   550,
     904,   190,   381,  1156,  -608,   661,    80,    81,   612,  1522,
     465,   466,   994,   116,    63,    64,   310,    82,   883,   505,
     766,   767,   230,    82,  1009,    82,   918,  2114,   303,   467,
     643,   231,  1590,   922,  1780,   213,  1370,   926,   480,  1851,
     266,   965,  1229,   589,  1859,  1467,  1734,  1027,   245,  1496,
    1737,   273,   288,   232,   295,   284,   297,  2033,   177,   644,
     645,   528,   256,   530,    78,   768,   769,  1761,   229,  1027,
     944,  1613,  1967,  1279,  1129,  1131,   468,   267,  1020,  2094,
      82,   220,    82,  1074,   678,   275,    82,  1658,    82,  1021,
    1022,   496,   221,   116,  -484,   266,   230,    82,   295,   297,
     144,    82,  1149,   116,   977,  1986,   177,   177,   222,   505,
    1295,   469,  1027,   135,   497,  1659,    91,   177,  1089,  1586,
    1296,   267,  2096,   298,  1682,  1455,  1455,  1455,  1682,  1702,
     104,   605,   177,    82,   610,  1671,   496,  1132,  1027,   151,
    1462,  1126,  1702,    58,  1508,   213,   470,   300,  2125,   839,
     266,  1179,   396,  1027,  1672,   896,  1456,  1456,  1456,  1741,
     115,   506,  1004,  2127,  -984,  1008,   177,  1509,  1010,  1818,
    1644,  -984,   943,   743,   177,   177,   446,  1013,   744,   177,
    1015,  1016,  1017,  1675,  1671,  1822,   213,    98,  1819,  1726,
    1144,   497,  1541,    82,  1955,    82,  2058,    82,   313,   116,
     602,    82,  1180,  1821,    82,   115,  1027,    75,   193,   692,
     302,    58,   471,  1762,   213,  1926,  -985,   266,   177,   295,
     297,   177,   302,  -985,    58,  1861,   634,   116,   592,    82,
     550,  1380,  1381,   702,  1927,  1777,  1396,   703,   116,  1910,
    1027,  1911,  1710,   105,    58,   896,   759,   257,   258,    58,
     259,   266,   111,   760,   761,   260,   266,   267,  1885,  2021,
     960,   745,   266,   116,   303,   905,   746,  1314,  1843,  1844,
    1845,  1496,   322,  1606,    82,    75,  1895,  1854,  -663,    82,
    1027,    82,  1855,   386,   736,  -663,  1866,  1416,    75,   712,
    1846,  1855,  2025,    82,   813,   266,  1807,   111,   550,  1852,
     683,  1820,   297,    82,  1573,  -820,  2039,  1090,    75,   514,
      58,   550,    82,    75,   756,   757,   389,  1364,   497,   197,
     355,   177,    58,  1404,  1324,    92,   934,  1113,   303,   400,
    1778,   665,  1120,   177,   177,   756,   665,    58,   193,   343,
     612,   239,   472,  1975,  -485,  -724,    82,   371,  1976,   719,
    1843,  1844,  1845,  1770,    14,    15,    16,    17,    18,  1345,
     302,   483,   471,   497,   550,  1200,  1346,  -486,   756,  1862,
    2109,  2087,  1846,   473,    75,  2110,   267,    14,    15,    16,
      17,    18,  1128,    82,    82,   514,    75,  1575,   471,   589,
     550,  2167,  1411,  1167,  1324,   503,  2168,   550,   327,   144,
      58,    75,   106,   266,  1365,  1171,   863,   864,   480,   550,
     865,   144,   474,    58,   506,    91,   191,   559,    74,   887,
    1175,   550,   504,  1896,   550,   475,  1389,   839,   678,   104,
     476,   266,   960,   683,   297,   477,    58,   508,  1906,   664,
      82,  1707,   634,   665,   471,   589,   550,   204,    64,   719,
      80,   666,   976,   509,    14,    15,    16,    17,    18,   522,
     286,    58,    58,   667,    75,   287,   601,    64,   291,   524,
     296,   177,  1113,   327,   471,   548,   665,    75,   532,   239,
     177,   527,   266,  1418,   955,  1144,    98,   550,   778,    82,
     651,   298,   779,   506,  1651,    82,    58,   804,  1094,   327,
      75,   805,   550,  1080,   115,   806,  1543,   266,  1702,   703,
     547,   302,   266,    58,   266,   550,   980,   933,   993,  1097,
     570,   934,   648,  1098,    82,    75,    75,   514,   764,   765,
    1776,    14,    15,    16,    17,    18,   266,   995,   266,   266,
    1524,   648,  1273,   996,  1422,  1426,   116,   703,   550,   550,
     266,  1026,    82,   571,   327,  1027,   943,  1991,    82,    82,
      75,   576,  1993,   266,   388,   388,   582,   632,   327,  1153,
    1132,   225,   266,  1154,   593,    58,  1036,    75,  1038,  1539,
    1041,   355,  1284,   665,  1047,   762,   763,  1051,   617,  1433,
      58,   116,    82,   621,  1548,   266,   111,   683,   297,   638,
     163,   770,   771,   164,   165,  1531,   166,   559,   371,   634,
    1461,   550,  1076,   550,  1634,  1843,  1844,  1845,  1189,   266,
     683,   286,  1027,   639,    92,   653,   266,   661,  1561,   355,
     685,    14,    15,    16,    17,    18,  1285,  1846,   696,    75,
    1052,  1713,  1714,  1715,  1716,  1717,  1847,    14,    15,    16,
      17,    18,    74,   380,    75,    58,   371,   480,  1591,  1191,
     144,  1323,   550,  1027,   699,  1324,  2074,   514,   286,   705,
      82,    82,    82,   664,   503,   739,   177,   665,   144,   514,
    1060,  1061,  1062,  1063,    80,   666,   177,  1132,   704,    58,
      58,  1132,    58,   706,    91,  2074,   737,   514,  2056,  1587,
     709,   106,   710,    82,  1621,  1622,    58,   739,   104,  1360,
     711,   144,   287,  1324,   682,   715,   296,   772,    82,    75,
     758,    82,    82,  1900,   267,    82,  2122,    91,   825,   739,
      74,   773,    82,   833,   774,    82,   144,   775,  1600,   446,
     446,   104,   550,    14,    15,    16,    17,    18,   272,   283,
     780,   664,    74,    75,    75,   665,    75,  1530,  1053,  1054,
    1055,   805,    80,   666,   720,    98,  1210,   177,   177,   807,
      75,  -488,  1604,   797,   873,  2061,   665,   550,    82,   550,
    1804,  1805,  1806,   115,    80,    81,   239,   808,  1566,  -126,
    -126,  -126,  -126,  -126,  -126,   514,  1376,  1773,    98,   809,
    1377,  1774,    58,    82,  1835,  1839,  1863,    74,  1324,  1027,
    1027,   327,   810,  1668,  1864,   811,   115,   327,  1154,  1392,
    1865,  1273,  1669,   177,  1027,  1931,  1393,   812,  1808,   805,
     479,  1284,   550,  1980,  2018,   355,    82,  1027,  1027,    80,
      81,   280,  2112,  1446,  1447,  1448,  1324,  1843,  1844,  1845,
    1461,  1461,  1461,  1766,  1273,  1653,  1461,  2113,   820,  1027,
     822,  1027,   371,   843,   720,   943,    75,  -489,  2186,  1846,
    1018,   739,  2183,  1430,   942,   111,   327,  1431,  -190,  2192,
      19,  1432,  2140,  2193,    -3,  1285,  2144,  1683,   446,  -487,
     116,  1683,   845,   533,   847,  1685,  1335,   -18,   266,  1685,
     534,   535,  1339,    92,  1029,  1030,    82,   276,   111,   266,
      82,    82,   144,  1347,   860,   536,   653,   739,   266,    48,
      49,    50,    51,    52,    53,    54,    55,   537,  1831,  1831,
    1831,   859,   514,   870,   144,   900,    92,  1616,   144,   144,
    1348,  1154,   872,   286,  1362,  1363,  1027,  1366,   884,   538,
    -161,  -161,   144,   206,   739,   514,   514,  1504,  1505,   898,
     265,   277,  1581,  1582,   104,    82,  1585,  1582,   104,   104,
     885,   539,  -125,  -125,  -125,  -125,  -125,  -125,  1589,  1582,
     106,   886,   104,   540,   888,   541,  1102,  1574,   889,   150,
    1623,  1574,   591,    65,    66,    67,    68,    69,    70,    71,
      72,  1039,    82,  1102,  1636,  1668,  1786,  1154,  1908,  1154,
    1668,  1909,  1582,   106,  1669,  1918,  1027,   890,   544,  1669,
     899,   514,   144,   266,  1978,  1979,    82,   545,   891,  1873,
     892,    82,    82,    82,  1996,  1582,   893,   267,   920,   115,
    1997,  1582,  1040,   115,   115,  1823,   315,   542,   921,   266,
     177,  1936,  1937,   177,   177,   177,   874,   115,  2183,  2184,
    1579,  1580,   177,   875,   876,  1056,  1057,  -606,   589,  1670,
    -604,   943,   930,  1104,   931,   833,   932,  1687,   877,   935,
     177,  1687,  1687,   153,  1058,  1059,   177,   947,  1064,  1065,
     878,  1736,  1738,  1898,  1899,  1687,   938,   712,    82,  1832,
    1833,   610,   949,    82,   953,   966,   968,   667,   177,    82,
    1003,    82,   879,   984,   992,   177,  1034,  1028,  1031,  1073,
      82,  1078,  1101,  1102,  1628,  2045,  1109,  1130,  1629,  1133,
    1151,   111,  1630,  1181,   880,   111,   111,   154,  1158,   514,
     327,  1159,  1988,  1160,  1161,   514,   881,  1162,   882,   111,
     144,  1163,  1164,  1165,  1166,  1188,   388,  1190,  1192,    92,
    -803,  1562,   825,    92,    92,  1196,  1203,  1204,  1205,   116,
    1276,   678,  1283,  1280,  1304,  1305,  1974,    92,  1292,  1306,
    1307,   756,   894,    14,    15,    16,    17,    18,  1024,   514,
    1315,   895,  1317,  1284,  1322,  1321,  2045,  1325,  1326,  1327,
    1329,  1330,   116,   747,   588,   748,   749,   750,  1018,   144,
     883,   266,  1331,  1332,   514,  1333,  1334,  1336,  1567,  1568,
    1569,  1337,   273,   284,  1338,  1570,  1571,  1343,  1344,  1566,
    1367,    82,  1351,    82,  2006,   751,   106,  1352,   752,   753,
     106,   106,   266,   754,   755,  1753,  1371,  1285,   266,  1372,
    1373,  1374,  1378,  1379,   106,   280,  1383,  1394,  1384,  1385,
    1386,  1670,  1399,  1410,  1494,  1397,  1670,  1400,  1401,   446,
    1434,  1439,    82,  -804,   546,    82,  1471,  1472,  1475,  1476,
    2051,   589,  1485,  1486,   514,   514,  1487,  1489,  -723,  1668,
    1027,   514,  1498,   177,   177,  1517,  1519,  1788,  1669,  2103,
    1520,  1550,  1554,   514,  1558,  1526,  1791,  1572,  1574,  1555,
    1792,  1595,  1596,   514,  1528,   514,  1599,  1588,   144,  1610,
    1611,   276,  1582,  1618,  1612,  1614,   514,  1624,   514,   514,
     514,  1619,  1627,  1631,   533,  1632,    82,    82,   177,   177,
    1633,   534,   535,  1642,  1641,  1645,  1104,  1656,   104,  1464,
    1689,  1703,  1388,   833,  1505,  1727,   536,    14,    15,    16,
      17,    18,  1340,  1730,  1731,  2117,  1704,  2006,   537,  1229,
    1706,  1742,  1743,  1708,   265,   277,  2047,  1721,  1728,  1729,
    1284,  1745,    82,  1747,  1748,   177,   304,  1759,   498,   514,
     538,  1749,  1750,   514,  1751,  1757,  1760,   266,   514,  1767,
     446,  1771,   446,  1772,  1779,  1775,  2142,  1787,  1793,  1789,
    2051,  1790,   539,   471,  2051,  2051,    14,    15,    16,    17,
      18,  1623,  1795,   115,   540,   116,   541,  1802,  1797,   116,
     116,  1798,  1799,  1829,  1285,  1817,  1803,  1836,   896,  1661,
    1840,   446,  1870,   116,   226,  2165,  1842,  2047,  1872,  1890,
    1889,   267,  1897,   514,  1893,   266,  1894,  1902,   544,  1907,
    1912,  1687,  1917,  1915,  1916,   514,   592,   545,  2177,   514,
    2162,  1920,  2177,  1944,  1949,    58,   144,  2185,   526,  1930,
     198,   199,  1966,   514,  1950,  1971,   631,  2187,   542,    85,
    1964,  1973,   152,  1977,    82,   894,    82,  1982,  1989,   200,
    1990,  1992,  1994,   177,   895,   144,   104,  1995,  1998,  1999,
    2004,  2000,   446,  2020,   550,   111,  2022,  2026,  -589,  2029,
    2032,  2034,   177,  2040,  2048,   514,   588,  2059,   177,   327,
    2049,  2060,  2071,  2097,  2093,   104,   144,    74,  2080,    75,
    2095,   494,  2106,    92,  2108,  1670,  2107,    85,  2111,  2128,
      82,    82,  2124,  2147,  2126,  2137,  2141,  2148,  1808,   619,
    2153,  2172,   550,   514,   195,  2143,   104,  2163,   514,    80,
      81,  2166,   177,    85,  2164,  2174,  2175,  2179,   212,  1904,
    2190,   115,  2180,  1560,  1023,  2191,   236,  1066,    82,   264,
    1067,  2194,  1070,    85,  1068,  1493,   514,  1069,   795,   514,
    1500,   514,  2173,   589,  1968,  1691,  2135,  2118,   217,  1961,
     115,  1712,  1874,  2031,  2115,   505,  2161,  1880,  1868,  1687,
     106,   615,   514,  1869,  2099,   266,  2145,  2181,  2098,  1518,
    1607,   152,   174,    82,   293,   581,  2003,    85,  2065,  1655,
     152,   115,    82,   325,   333,  1515,  1553,  1150,  1687,  1744,
    1886,     3,   997,  1081,  1082,   150,   354,   175,   176,    65,
      66,    67,    68,    69,    70,    71,    72,   177,   177,   866,
     735,  1801,   951,   111,   177,   217,     0,     0,  1083,  1687,
       0,   453,     0,   195,   195,     0,   177,     0,   658,     0,
       0,   681,     0,     0,   152,   486,   177,     0,   177,   264,
    1667,    92,   111,     0,   658,     0,     0,     0,   658,   177,
       0,   177,   177,   177,   546,  1481,   462,     0,   325,   177,
       0,     0,     0,   236,   236,     0,     0,     0,     0,   390,
      92,     0,     0,   111,     0,     0,     0,   337,     0,     0,
       0,     0,     0,     0,   325,   338,   339,   340,   341,     0,
       0,   896,    85,     0,     0,     0,     0,     0,   498,     0,
       0,    92,     0,     0,     0,   266,   264,  2116,     0,     0,
       0,     0,   177,     0,     0,     0,   177,   266,   106,     0,
       0,   177,     0,     0,     0,     0,   150,   580,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   325,
       0,     0,     0,     0,   391,   333,     0,   106,   903,   116,
       0,   333,   325,   325,     0,     0,   907,     0,     0,     0,
     658,   152,   392,     0,   393,   394,    65,    66,    67,    68,
      69,    70,    71,    72,   917,   498,   177,     0,   106,   342,
       0,     0,   354,   668,   677,   925,     0,     0,   177,     0,
       0,     0,   177,     0,     0,     0,   580,   343,   354,     0,
    2055,     0,   354,   266,     0,     0,   177,     0,     0,     0,
       0,   395,     0,     0,     0,   219,     0,   663,   615,  2105,
       0,   186,     6,     7,     8,     9,    10,    11,    12,    13,
       0,  1501,  1667,     0,     0,     0,     0,  1667,     0,     0,
       0,     0,     0,  1824,     0,  1667,     0,   453,   177,     0,
       0,   494,     0,     0,     0,     0,   301,     0,   561,   150,
       0,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,   256,     0,     0,     0,
       0,   453,   285,     0,   798,     0,   177,     0,     0,   431,
       0,   177,   195,   615,     0,     0,     0,     0,   735,     0,
       0,     0,   498,     0,   735,     0,     0,   116,     0,   494,
     152,     0,     0,   735,   486,     0,   217,     0,   832,   177,
     677,     0,   177,   615,   177,   266,     0,   658,   494,     0,
       0,   152,   735,  1502,     0,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,   177,     0,   498,     0,   663,
       0,   658,     0,     0,     0,     0,  2188,     0,     0,   236,
       0,     0,     0,     0,   658,  2195,   498,   116,   498,     0,
       0,   236,   498,   498,   498,     0,     0,     0,     0,   150,
     515,   233,   234,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   498,     0,     0,     0,   325,     0,   453,   453,
       0,     0,   325,     0,     0,   354,  1932,    74,     0,  1667,
       0,     0,     0,     0,     0,     0,   150,     0,   266,     0,
      65,    66,    67,    68,    69,    70,    71,    72,  1660,    77,
       0,     0,     0,   462,   462,  1661,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,   629,     0,     0,
       0,     0,     0,  1208,     0,   494,   615,     0,     0,   325,
       0,   325,   498,   354,     0,    85,    77,     0,   803,   855,
       0,   327,   615,     0,     0,     0,   615,     0,     0,     0,
       0,   354,   486,     0,   677,   815,     0,     0,   818,   615,
       0,     0,   668,     0,     0,  1187,   668,     0,     0,   658,
     494,     0,     0,     0,     0,   354,     0,   708,     0,  1195,
       0,   431,   714,  1199,     0,   677,  1667,  1202,   354,     0,
       0,   723,   724,     0,     0,     0,     0,   152,     0,     0,
       0,     0,     0,     0,     0,   453,   431,   431,   152,   152,
     978,   453,     0,     0,     0,     0,     0,     0,   561,    19,
     453,     0,     0,   152,   152,   152,     0,   431,     0,     0,
       0,     0,   462,     0,     0,     0,     0,     0,     0,     0,
    1005,     0,     0,     0,   462,     0,  1011,   150,     0,   175,
     176,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     431,  1209,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,   486,
       0,   827,     0,   829,     0,     0,   390,     0,     0,     0,
     498,   498,   846,     0,     0,   798,   798,     0,   327,     0,
       0,   658,     0,   453,   681,     0,     0,   150,     0,  1043,
    1044,    65,    66,    67,    68,    69,    70,    71,    72,  1045,
       0,  1477,     0,   354,   486,     0,     0,     0,   832,     0,
     832,     0,    58,     0,     0,     0,     0,     0,   462,     0,
     856,     0,   515,   354,     0,   354,   498,     0,     0,   354,
     354,   354,     0,     0,   632,   327,     0,     0,     0,     0,
    1046,   391,     0,   494,     0,     0,   150,    58,     0,   354,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   392,
       0,   393,   394,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   327,     0,    74,   325,    75,     0,     0,     0,
       0,   150,     0,   233,   234,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,    76,    77,     0,     0,   615,
       0,     0,     0,   615,     0,     0,    80,    81,     0,     0,
       0,    75,   615,     0,   453,     0,     0,     0,     0,   354,
       0,     0,   615,     0,     0,     0,   152,   453,     0,   615,
    1387,    77,     0,     0,     0,   354,     0,  1299,     0,     0,
       0,  1444,     0,     0,     0,     0,     0,     0,   668,     0,
       0,     0,     0,     0,     0,  1468,     0,     0,     0,   803,
     803,     0,   462,     0,     0,     0,     0,     0,     0,     0,
    1092,     0,     0,  1095,     0,  1490,   615,     0,     0,     0,
     615,     0,   735,   150,   615,   175,   176,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   152,   486,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,   431,   431,   431,   431,   431,   431,   431,   431,
     431,   431,   431,   431,   431,   431,   431,   431,   431,   431,
     431,     0,     0,     0,     0,     0,     0,     0,   101,   561,
       0,   156,     0,     0,     0,     0,  1169,     0,     0,     0,
    1173,     0,     0,     0,  1177,     0,     0,  1479,     0,     0,
       0,     0,   798,     0,    58,     0,     0,     0,   498,     0,
       0,   498,   498,     0,     0,     0,     0,   354,   354,     0,
       0,   832,     0,     0,     0,     0,     0,     0,   832,     0,
       0,     0,     0,     0,     0,     0,   101,   431,   150,     0,
     233,   234,    65,    66,    67,    68,    69,    70,    71,    72,
     186,     6,     7,     8,     9,    10,    11,    12,    13,  1125,
       0,     0,   211,   658,     0,     0,    74,     0,    75,     0,
     150,     0,   914,   354,    65,    66,    67,    68,    69,    70,
      71,    72,   278,     0,     0,     0,     0,   830,    77,     0,
       0,   665,   494,     0,     0,     0,   150,     0,    80,   831,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   515,
       0,     0,   856,     0,     0,   152,   312,     0,     0,     0,
     317,     0,     0,     0,   152,     0,   101,     0,     0,   323,
       0,     0,   150,   453,   233,   234,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   356,  1677,  1679,     0,     0,
       0,     0,     0,     0,     0,  1277,  1278,     0,     0,   453,
      74,     0,     0,     0,     0,     0,   453,   615,     0,     0,
     323,   615,   464,     0,     0,   615,     0,     0,     0,     0,
       0,   830,    77,   317,   492,   665,   803,     0,     0,     0,
     264,    85,    80,   831,     0,     0,  1739,     0,   354,     0,
       0,     0,     0,     0,   325,   667,     0,     0,   431,     0,
     152,     0,     0,   543,   431,     0,     0,   486,     0,     0,
       0,     0,   312,     0,     0,   431,     0,     0,     0,     0,
       0,     0,     0,   568,     0,     0,     0,     0,   573,   575,
       0,   211,     0,     0,     0,   462,   486,     0,     0,  1349,
     312,   152,   978,     0,     0,     0,     0,     0,     0,     0,
    1420,   312,     0,  1424,   597,   431,     0,  1428,   599,     0,
       0,     0,   150,   600,   233,   234,    65,    66,    67,    68,
      69,    70,    71,    72,   575,     0,   312,     0,   615,  1356,
     624,     0,     0,  1369,     0,  1356,     0,     0,   494,     0,
       0,     0,   633,     0,     0,     0,     0,     0,     0,     0,
     323,     0,     0,     0,     0,   354,     0,     0,   354,   354,
       0,     0,     0,     0,  1356,     0,     0,   515,     0,     0,
       0,   656,     0,     0,   680,     0,   380,     0,     0,     0,
     615,     0,  1395,     0,  1398,     0,     0,   687,     0,   615,
       0,   687,     0,   615,     0,     0,  1402,  1403,     0,     0,
       0,     0,  1408,  1409,   152,   152,   152,   152,     0,   152,
     152,     0,  1417,     0,    58,  1662,   333,   150,     0,   603,
     604,    65,    66,    67,    68,    69,    70,    71,    72,   453,
       0,     0,  1356,   453,   453,     0,   323,     0,  1867,  1437,
       0,     0,  1440,    58,     0,     0,     0,   453,   150,   431,
     233,   234,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     323,     0,     0,     0,   264,     0,    74,   150,    75,   233,
     234,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   486,     0,     0,     0,     0,  2101,    77,   317,
       0,   550,     0,   656,  1499,    74,     0,    75,    80,    81,
       0,     0,     0,     0,  1593,     0,     0,   152,     0,   668,
     317,     0,     0,     0,     0,  1602,   235,    77,     0,     0,
       0,     0,     0,   431,     0,  1521,     0,    80,    81,     0,
       0,     0,     0,  1525,     0,  1527,  1529,     0,     0,     0,
       0,     0,     0,     0,  1535,     0,  1536,     0,  1537,     0,
       0,     0,     0,     0,     0,  1546,     0,     0,     0,     0,
     431,   431,   431,     0,     0,     0,     0,   431,   431,     0,
      14,    15,    16,    17,    18,     0,     0,   323,   323,     0,
       0,     0,     0,     0,   492,     0,     0,     0,     0,     0,
     431,     0,     0,     0,     0,     0,     0,  1662,  1809,   312,
       0,     0,  1662,     0,   453,     0,     0,     0,  1662,     0,
    1662,     0,   150,     0,   378,   379,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   515,  1597,  1598,     0,    58,
     431,   431,     0,  1356,   333,   152,     0,     0,     0,   462,
       0,     0,   356,     0,   101,     0,     0,     0,   192,     0,
       0,  1620,     0,     0,     0,     0,     0,     0,  1625,     0,
     687,   959,  1626,   150,    78,   233,   234,    65,    66,    67,
      68,    69,    70,    71,    72,   970,   380,     0,     0,     0,
       0,     0,     0,     0,   656,     0,     0,   268,  1643,   979,
       0,    74,     0,    75,   152,     0,     0,   687,     0,   289,
     292,     0,     0,     0,     0,     0,   323,     0,     0,     0,
       0,     0,  2101,    77,   323,     0,   550,   323,   323,     0,
     323,     0,   152,    80,    81,     0,     0,     0,     0,   323,
       0,     0,   323,   323,   323,     0,     0,     0,     0,     0,
       0,   150,   268,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,  1809,  1809,     0,     0,
       0,     0,   515,     0,     0,     0,   658,     0,     0,     0,
       0,  1662,     0,     0,  1662,     0,     0,     0,     0,   431,
       0,     0,  1812,     0,  1752,     0,   333,     0,   492,     0,
       0,  1756,     0,  1758,     0,     0,     0,   268,   641,     0,
       0,     0,     0,   453,     0,  1084,     0,     0,     0,     0,
       0,     0,   323,   150,     0,   601,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,    14,    15,    16,    17,
      18,     0,   687,   959,     0,     0,   325,   658,     0,  1110,
     615,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,   515,   492,     0,   492,     0,  1356,     0,   492,   492,
     492,  1356,  1356,  1356,   268,     0,  1075,  1794,     0,     0,
       0,     0,  1809,     0,  2176,     0,     0,     0,   492,     0,
       0,  1662,     0,     0,   150,    58,  2182,     0,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   268,     0,
       0,     0,     0,   268,     0,     0,     0,     0,     0,   268,
       0,     0,    74,     0,    75,     0,     0,     0,     0,   150,
     152,   233,   234,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,    76,    77,     0,     0,     0,     0,     0,
    1812,  1812,   268,  1272,    80,    81,     0,    74,   492,    75,
    1809,     0,     0,     0,   431,   156,   323,     0,     0,     0,
       0,   152,     0,     0,   687,     0,     0,  1303,   235,    77,
       0,     0,     0,     0,  1309,     0,     0,     0,   150,    80,
      81,     0,    65,    66,    67,    68,    69,    70,    71,    72,
     152,   152,     0,  2102,   333,     0,   721,     0,    58,     0,
    1891,  1892,   150,     0,   204,    64,    65,    66,    67,    68,
      69,    70,    71,    72,  1901,     0,     0,     0,     0,     0,
       0,   152,     0,     0,     0,   317,   356,  1387,    77,     0,
       0,     0,   150,     0,   233,   234,    65,    66,    67,    68,
      69,    70,    71,    72,   515,     0,     0,     0,     0,  2102,
    2102,     0,    77,     0,     0,   855,  1812,     0,     0,     0,
      74,  1356,    75,  1356,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,   268,     0,
       0,   324,    77,     0,     0,     0,     0,  2102,     0,     0,
       0,     0,    80,    81,     0,     0,   721,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   492,   492,   118,     0,
       0,   118,   150,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
    2063,    58,     0,   409,  1812,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   150,     0,   175,
     176,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     431,   268,   492,     0,    78,   150,   118,   233,   234,    65,
      66,    67,    68,    69,    70,    71,    72,  1812,     0,     0,
       0,     0,     0,   268,   741,     0,     0,    78,   420,     0,
       0,     0,   118,    74,     0,    75,   504,   268,     0,     0,
       0,     0,   431,     0,   156,     0,     0,     0,   270,     0,
     268,     0,   118,  1458,   324,    77,     0,     0,     0,     0,
       0,     0,  1272,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,  1812,  1812,     0,     0,     0,     0,     0,
       0,     0,   268,     0,     0,     0,   118,     0,   323,     0,
     118,     0,     0,     0,     0,  1272,   118,     0,     0,   118,
       0,     0,     0,   270,     0,     0,   268,     0,     0,     0,
       0,  1812,     0,   268,   350,   118,     0,   382,     0,     0,
    1516,     0,     0,     0,     0,     0,     0,   356,     0,     0,
    2100,   431,     0,   431,     0,     0,     0,     0,     0,   323,
     457,     0,     0,     0,     0,     0,   656,     0,     0,     0,
       0,     0,     0,   118,   457,   573,     0,     0,   270,   150,
       0,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   431,     0,     0,   356,     0,     0,     0,     0,
     323,     0,     0,     0,     0,  2136,    14,    15,    16,    17,
      18,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,   431,     0,     0,     0,     0,  2151,     0,   508,   118,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
     118,  2160,     0,     0,     0,   270,     0,     0,     0,     0,
     150,   118,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   431,   492,    58,   606,   492,   492,   118,
       0,     0,     0,     0,   118,     0,   118,     0,     0,   270,
     118,     0,     0,     0,   270,     0,     0,     0,     0,     0,
     270,     0,     0,     0,     0,     0,   178,   181,     0,   150,
     118,   233,   234,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,  1458,  1458,  1458,   156,   575,   323,   323,
       0,   118,  2005,   270,   118,     0,     0,    74,     0,    75,
       0,     0,     0,   228,     0,     0,     0,   118,  1686,     0,
       0,   118,  1686,  1686,     0,     0,     0,  1184,  1660,    77,
       0,     0,     0,     0,     0,     0,  1686,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     403,     0,     0,   404,     0,   405,     0,   406,     0,     0,
       0,     0,     0,     0,   319,     0,   457,   320,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
       0,   356,   344,     0,     0,   268,     0,     0,     0,   782,
     783,   784,   785,   786,   787,   788,   789,   790,   791,   792,
     457,     0,     0,   220,   408,   409,   156,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,     0,   417,   418,   118,
     793,     0,     0,   457,     0,    74,     0,     0,     0,   270,
       0,     0,     0,     0,     0,     0,   525,     0,    58,     0,
     118,     0,     0,     0,     0,     0,   419,     0,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   150,     0,   233,   234,    65,    66,    67,    68,
      69,    70,    71,    72,   585,   586,     0,     0,     0,     0,
       0,     0,     0,   323,     0,   178,   118,     0,     0,  1826,
      74,     0,    75,     0,     0,     0,     0,   457,   457,     0,
     178,     0,   270,     0,   118,     0,     0,     0,     0,     0,
       0,  1660,    77,     0,  1838,     0,     0,     0,     0,   118,
       0,   150,    80,    81,     0,    65,    66,    67,    68,    69,
      70,    71,    72,  1353,   636,     0,     0,  1354,   270,  1355,
       0,     0,   640,   642,     0,     0,     0,   649,     0,     0,
       0,   270,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,   118,     0,   118,     0,     0,     0,     0,     0,
       0,    77,     0,   156,  1577,     0,     0,     0,     0,   382,
     118,   457,     0,   270,     0,     0,   344,     0,     0,   344,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   323,     0,     0,   118,     0,     0,   270,     0,     0,
       0,   606,     0,     0,   270,     0,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,  1474,
       0,     0,     0,     0,   457,     0,  1924,   118,   118,     0,
     457,  1488,     0,     0,     0,     0,     0,     0,     0,   457,
       0,     0,   118,   118,   118,     0,   150,     0,   233,   234,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   268,     0,
       0,     0,  1686,     0,    74,     0,     0,     0,     0,   228,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   849,   850,     0,   112,  1660,    77,     0,   457,   268,
       0,     0,  1661,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,   457,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,   118,   457,     0,     0,     0,   118,     0,   150,
       0,     0,   112,    65,    66,    67,    68,    69,    70,    71,
      72,  1353,   118,     0,   118,  1354,     0,  1355,   118,   118,
     118,     0,     0,   150,     0,   233,   234,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,   118,  2046,
       0,     0,     0,     0,     0,     0,     0,     0,   279,    77,
       0,    74,  1782,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1185,     0,
       0,     0,  2101,    77,     0,     0,   550,     0,     0,   946,
    1686,     0,   112,    80,    81,  1639,     0,     0,   344,     0,
       0,     0,   112,     0,     0,     0,     0,     0,     0,   118,
       0,     0,     0,   457,   268,     0,     0,     0,   118,  1686,
    2046,   359,     0,     0,     0,   118,   457,     0,     0,     0,
       0,     0,     0,     0,   118,     0,  1301,   457,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1686,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     493,     0,     0,     0,  1316,     0,     0,  1732,  1733,     0,
       0,     0,   268,     0,     0,     0,     0,     0,     0,  2139,
       0,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,   118,   457,    20,   112,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   112,     0,    46,     0,
      47,     0,     0,     0,     0,     0,     0,   112,     0,     0,
     598,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,   359,     0,     0,     0,     0,
       0,     0,   112,     0,     0,     0,   279,   118,     0,     0,
       0,   118,     0,     0,     0,     0,   118,   118,     0,     0,
     118,     0,     0,   716,     0,   717,   718,     0,     0,     0,
     118,     0,     0,     0,  1127,     0,     0,   118,     0,     0,
       0,     0,     0,     0,  1142,     0,     0,   657,     0,     0,
     279,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   657,     0,     0,     0,   657,     0,     0,
       0,     0,   118,     0,     0,     0,     0,     0,     0,   -17,
       0,     0,   268,     0,   118,     0,     0,     0,   118,     0,
       0,  1841,   118,     0,     0,     0,   150,  1850,   233,   234,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,   118,    74,  1212,     0,     0,     0,     0,
       0,     0,   457,     0,     0,     0,     0,     0,     0,     0,
       0,  1883,     0,   150,     0,   235,    77,    65,    66,    67,
      68,    69,    70,    71,    72,  1353,    80,    81,   457,  1354,
       0,  1355,     0,     0,   150,   457,   233,   234,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,   657,
       0,  1319,     0,     0,     0,     0,     0,     0,     0,   270,
     118,     0,    74,    77,     0,     0,  1784,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,     0,     0,   324,    77,     0,   457,     0,     0,     0,
    1301,     0,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,     0,     0,   268,     0,  1934,  1935,     0,     0,
       0,     0,     0,  1945,   118,   457,     0,     0,     0,     0,
     118,     0,   359,     0,     0,  1960,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1969,     0,  1970,     0,     0,
     493,     0,     0,     0,     0,     0,     0,     0,  1981,     0,
    1983,  1984,  1985,     0,     0,   112,     0,     0,     0,     0,
     150,     0,     0,   117,    65,    66,    67,    68,    69,    70,
      71,    72,  1353,     0,     0,     0,  1354,     0,  1355,     0,
       0,     0,     0,     0,   118,     0,     0,   118,   118,     0,
       0,     0,     0,     0,     0,     0,     0,   359,   493,     0,
     112,     0,     0,     0,     0,   118,     0,     0,     0,   118,
      77,  2014,     0,   118,     0,  2019,   657,   493,     0,     0,
    2024,   117,     0,     0,     0,     0,  1640,   359,     0,     0,
       0,     0,     0,   118,   118,   118,   118,   118,   118,   118,
     657,     0,     0,     0,     0,   270,     0,     0,     0,     0,
       0,     0,     0,   657,     0,     0,     0,     0,   457,     0,
       0,     0,   457,   457,     0,     0,     0,   281,     0,     0,
       0,     0,     0,     0,     0,  2066,   457,     0,     0,     0,
       0,  1478,  1480,  1482,     0,     0,     0,  2075,     0,     0,
       0,  2078,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   117,     0,   270,     0,  2092,     0,     0,     0,     0,
       0,   117,   268,     0,  1503,     0,     0,     0,     0,     0,
       0,   457,     0,     0,     0,     0,   118,     0,     0,     0,
     363,     0,     0,     0,     0,     0,  1212,     0,     0,  1763,
       0,     0,     0,  1523,   493,     0,   118,  2123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     359,     0,     0,     0,     0,     0,     0,     0,     0,   495,
       0,     0,     0,     0,     0,     0,   359,     0,   118,     0,
     359,     0,     0,     0,     0,  2146,     0,   118,   657,   493,
    2149,   118,     0,   359,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   117,   359,     0,
     359,     0,     0,     0,   359,   359,   359,     0,  2169,     0,
       0,  2171,     0,  2149,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   359,   117,     0,     0,     0,     0,
       0,     0,     0,   457,  2171,     0,   117,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
       0,   117,     0,   270,   118,   281,     0,     0,     0,     0,
     123,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   359,     0,     0,     0,   112,
       0,     0,     0,     0,   359,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   659,     0,     0,   281,
     657,  1673,  1674,   279,     0,     0,     0,     0,     0,     0,
       0,     0,   659,   118,     0,     0,   659,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   493,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,   294,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   149,     0,     0,     0,     0,
       0,     0,     0,  1764,     0,     0,     0,     0,   123,     0,
       0,     0,   123,     0,     0,   270,     0,     0,   123,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   457,     0,     0,     0,     0,     0,   659,     0,
       0,     0,     0,   359,     0,     0,     0,   359,     0,     0,
       0,     0,   359,   359,     0,     0,   359,     0,     0,     0,
       0,     0,   123,     0,     0,     0,   359,     0,     0,     0,
       0,   207,     0,   359,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   359,     0,
       0,   363,     0,     0,   123,     0,     0,     0,     0,     0,
     359,  1834,     0,     0,   359,     0,     0,     0,   359,   495,
       0,   123,     0,   123,     0,     0,     0,     0,   123,     0,
       0,     0,   123,     0,   117,     0,     0,     0,     0,   118,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,     0,     0,     0,     0,     0,   123,     0,   123,     0,
       0,     0,   123,     0,     0,     0,   363,   495,     0,   117,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     207,   112,   123,     0,     0,   659,   495,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,   118,
     118,     0,     0,   270,     0,     0,   279,     0,     0,   659,
       0,     0,     0,   359,     0,     0,     0,     0,   118,     0,
       0,     0,   659,     0,     0,     0,     0,     0,     0,     0,
     118,     0,   657,     0,     0,   577,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     359,   493,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,   618,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   625,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,   635,     0,     0,     0,     0,     0,
       0,     0,     0,   495,     0,     0,     0,  1987,     0,     0,
       0,     0,     0,     0,     0,   654,     0,     0,     0,   363,
     359,   123,     0,   359,   359,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,   363,
       0,   359,   123,     0,     0,   359,     0,   659,   495,   359,
       0,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,   363,
       0,     0,     0,   363,   363,   363,     0,     0,     0,     0,
       0,     0,   740,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,   112,     0,     0,   155,   112,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
     123,     0,   112,   781,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
     122,   821,     0,   122,     0,     0,   826,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,   493,   117,     0,
       0,     0,   359,   363,     0,     0,   852,     0,     0,     0,
     853,   854,     0,     0,   857,     0,   123,     0,   210,   659,
       0,     0,   281,     0,     0,     0,     0,     0,     0,   871,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   901,     0,   359,     0,     0,     0,     0,     0,
       0,     0,     0,   359,   122,     0,     0,   359,     0,     0,
       0,     0,     0,     0,     0,     0,   316,     0,   123,     0,
       0,   495,     0,     0,   122,   210,   123,     0,     0,   123,
     123,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,   123,   123,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,   122,     0,     0,     0,   456,     0,   122,   941,
       0,   122,     0,     0,     0,     0,     0,     0,     0,   481,
       0,     0,     0,   948,     0,     0,     0,     0,     0,     0,
     279,     0,   363,     0,     0,     0,   363,     0,     0,     0,
       0,   363,   363,     0,     0,   363,     0,   967,     0,     0,
       0,     0,   122,     0,     0,   363,     0,     0,     0,     0,
       0,     0,   363,     0,   123,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   574,     0,   578,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,     0,   363,
    1019,     0,     0,   363,     0,     0,     0,   363,     0,     0,
     155,   122,     0,   122,     0,     0,     0,     0,   122,     0,
       0,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   122,     0,     0,   578,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   117,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,   122,     0,
       0,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     117,     0,   122,     0,     0,   123,   127,     0,   112,   127,
       0,     0,  1099,     0,  1100,     0,     0,   123,   123,     0,
     826,     0,     0,     0,     0,   281,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   456,     0,     0,     0,     0,  1143,     0,     0,
       0,   659,     0,     0,     0,     0,  1152,     0,     0,     0,
       0,     0,  1155,     0,   127,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   210,     0,     0,   363,
     495,     0,     0,     0,     0,     0,     0,   123,   122,     0,
     127,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   823,     0,   654,     0,     0,
     127,     0,  1197,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,     0,     0,   657,   481,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   127,     0,     0,     0,   127,   363,
       0,   122,   363,   363,   127,     0,     0,   127,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
     363,     0,   122,     0,   363,     0,     0,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   456,   456,   112,   657,     0,   127,  1328,
       0,     0,     0,     0,     0,     0,   172,     0,     0,     0,
       0,   127,     0,     0,   359,     0,     0,     0,     0,     0,
       0,     0,     0,   117,     0,     0,   112,   117,   117,     0,
       0,     0,     0,     0,   172,     0,     0,     0,     0,   122,
     122,   117,     0,     0,     0,     0,     0,     0,     0,     0,
     127,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   122,     0,     0,     0,     0,   123,   127,     0,   127,
       0,     0,     0,     0,   127,   123,     0,     0,   127,     0,
       0,   172,     0,     0,   123,     0,   495,     0,     0,   127,
       0,   363,     0,     0,   172,     0,   172,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,     0,     0,
     123,     0,   127,     0,   127,     0,     0,   123,   127,     0,
       0,     0,   456,     0,     0,     0,     0,   172,     0,   385,
     456,     0,     0,   456,   456,     0,   456,     0,   127,     0,
       0,     0,   123,   363,     0,   456,     0,     0,   456,   456,
     456,     0,   363,     0,   385,     0,   363,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,     0,     0,   122,     0,     0,   122,
     122,     0,   122,     0,     0,   149,     0,     0,     0,     0,
       0,   122,     0,   172,   122,   122,   122,   172,     0,     0,
     172,   172,   123,     0,   172,     0,     0,   172,   172,     0,
     172,     0,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   127,     0,     0,     0,   456,     0,
       0,     0,     0,     0,   781,     0,     0,     0,     0,   281,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
       0,     0,   172,     0,     0,   172,     0,   127,     0,  1534,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,   172,
       0,     0,     0,  1559,     0,   123,   123,   123,   123,   123,
     123,   123,     0,     0,   172,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,   123,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1286,   456,     0,     0,   127,   127,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   127,     0,     0,
       0,     0,     0,     0,     0,   122,     0,   117,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   122,   122,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,   172,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   481,   127,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,   122,     0,   385,
      46,     0,    47,     0,     0,     0,   351,    49,    50,    51,
      52,    53,    54,    55,   127,   172,     0,     0,     0,     0,
       0,    58,   127,     0,     0,   127,   127,  1746,   127,     0,
       0,     0,     0,     0,   659,   123,     0,   127,     0,     0,
     127,   127,   127,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,   117,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,  1136,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,     0,     0,   117,   659,  -693,    78,   915,     0,
       0,     0,     0,     0,     0,    80,    81,     0,     0,     0,
     127,     0,     0,   363,     0,   123,     0,     0,     0,     0,
    1286,     0,     0,     0,     0,   117,     0,     0,     0,  1457,
     172,   172,     0,     0,     0,  1746,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
     172,     0,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   456,     0,   122,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,     0,     0,     0,   456,     0,   122,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1887,
    1888,   127,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   127,   127,     0,   456,     0,     0,   172,
     172,   122,     0,     0,     0,     0,   172,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   172,     0,     0,   172,   172,     0,   172,     0,   172,
     172,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   127,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   172,     0,     0,     0,   172,     0,     0,     0,
     172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1972,     0,     0,     0,     0,     0,     0,  1457,
    1457,  1457,   155,  1649,  1650,  1654,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1746,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,   122,   122,   122,   122,   122,
     122,   122,     0,     0,     0,     0,   172,     0,     0,     0,
    2013,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,   123,     0,   122,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,  2042,
       0,     0,     0,  2043,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1286,     0,     0,     0,     0,   370,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   127,     0,     0,     0,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,     0,     0,     0,     0,
     127,     0,     0,     0,     0,   482,   370,     0,   122,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   127,     0,     0,     0,
       0,     0,     0,   127,     0,     0,     0,     0,   553,     0,
       0,     0,     0,     0,     0,   553,     0,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,   456,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   127,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   172,     0,     0,     0,     0,   172,     0,     0,   172,
       0,     0,     0,   172,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,   553,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1286,
       0,     0,     0,   370,   669,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   690,     0,     0,     0,   456,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,   122,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,   122,     0,     0,     0,    46,     0,    47,
       0,   127,   127,   127,   127,   127,   127,   127,     0,     0,
       0,     0,     0,   172,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,   553,   127,     0,     0,     0,
     127,   127,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   553,   816,   127,   553,   819,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,   669,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   482,   216,   122,     0,     0,     0,     0,     0,
     172,     0,    75,     0,     0,     0,     0,     0,     0,   274,
       0,   172,     0,     0,   172,     0,   172,   172,     0,     0,
       0,   553,     0,     0,     0,   553,     0,     0,     0,     0,
       0,     0,     0,     0,   127,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     216,     0,   172,     0,   334,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   216,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   502,     0,     0,     0,   507,
       0,   553,     0,     0,   370,     0,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   957,   370,     0,   172,     0,     0,     0,     0,
       0,     0,   122,   669,     0,     0,     0,   669,     0,     0,
       0,     0,   127,     0,   975,     0,   370,     0,     0,     0,
       0,     0,   216,     0,     0,     0,     0,     0,     0,     0,
       0,   122,     0,     0,     0,     0,   274,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,     0,   172,     0,     0,
       0,   507,     0,   734,     0,     0,     0,     0,     0,     0,
       0,   216,     0,     0,     0,     0,     0,     0,     0,   127,
       0,     0,     0,     0,     0,   172,     0,     0,     0,     0,
       0,     0,   662,     0,   679,     0,     0,     0,     0,     0,
     370,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   553,   553,   172,     0,
       0,     0,     0,     0,     0,   172,     0,   553,  1093,     0,
     553,  1096,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   957,   370,     0,     0,     0,   669,
       0,   669,   669,     0,   170,     0,     0,   738,   669,     0,
     127,     0,     0,     0,   370,     0,   370,     0,     0,     0,
     370,   370,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     370,   216,   553,     0,     0,     0,   553,     0,     0,     0,
     172,     0,     0,   553,  1170,     0,     0,   553,  1174,     0,
       0,   553,  1178,     0,   909,   911,     0,     0,     0,  1182,
       0,     0,     0,     0,   662,     0,     0,     0,     0,   299,
     844,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   305,     0,   306,     0,     0,   172,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     370,   553,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   172,   172,   377,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,     0,   172,     0,   669,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   216,   216,
       0,     0,     0,     0,     0,   502,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   482,   370,     0,
       0,     0,     0,     0,     0,     0,     0,   127,   555,   556,
       0,   734,   560,     0,     0,   563,   564,   734,   566,     0,
     567,     0,     0,     0,     0,     0,   734,     0,     0,     0,
       0,     0,     0,   375,     0,     0,     0,     0,   127,     0,
       0,     0,     0,     0,   172,   734,     0,     0,     0,     0,
       0,     0,   502,     0,   961,     0,     0,     0,     0,     0,
       0,     0,     0,   553,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   662,     0,     0,   370,   370,
       0,  1072,   669,   669,     0,     0,     0,     0,     0,   669,
       0,     0,     0,     0,     0,   403,     0,   216,   404,     0,
     405,     0,   406,  1951,     0,   216,     0,     0,   738,   216,
       0,   216,     0,     0,     0,     0,   172,   652,     0,   407,
     738,     0,     0,   738,   738,   738,     0,     0,     0,     0,
       0,     0,   684,     0,   370,     0,     0,   553,  1421,     0,
     553,  1425,     0,     0,   553,  1429,     0,     0,     0,   408,
     409,     0,   410,   411,  1952,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,     0,   417,   418,     0,     0,     0,     0,     0,   502,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1692,  1693,  1694,     0,     0,
       0,   419,  1953,   216,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,   172,
       0,     0,     0,     0,   502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   814,
       0,     0,     0,   502,     0,   502,     0,     0,     0,   502,
     502,   502,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   502,
       0,     0,     0,     0,     0,     0,     0,     0,   370,     0,
       0,     0,     0,     0,   669,  1542,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   370,     0,     0,
       0,     0,     0,   897,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   502,
     455,     0,     0,     0,     0,     0,     0,   216,     0,     0,
       0,   553,  1594,     0,   487,     0,     0,   844,     0,     0,
       0,     0,   553,  1603,     0,   669,     0,     0,   516,     0,
     516,     0,     0,     0,     0,     0,   370,     0,     0,   370,
     370,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,   375,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,   973,   974,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   209,     0,     0,     0,     0,     0,     0,   983,     0,
     985,     0,     0,     0,     0,     0,     0,     0,     0,   630,
       0,     0,     0,     0,     0,   150,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   502,   502,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,   370,     0,     0,     0,     0,   209,     0,
       0,     0,     0,     0,    76,    77,     0,    78,    79,     0,
       0,     0,  -824,     0,   209,    80,    81,     0,     0,     0,
     669,     0,     0,     0,     0,   734,     0,     0,     0,     0,
       0,     0,     0,   502,     0,     0,     0,     0,     0,   209,
       0,     0,     0,     0,     0,     0,     0,  1086,  1087,     0,
       0,     0,     0,   489,  1091,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1114,
       0,     0,  1117,  1118,   738,  1121,     0,  1123,  1124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   553,
     209,     0,     0,     0,     0,     0,     0,     0,     0,   738,
       0,     0,     0,     0,     0,     0,   553,     0,     0,     0,
    1168,     0,     0,     0,  1172,     0,     0,     0,  1176,     0,
     516,     0,     0,     0,     0,     0,   516,     0,     0,     0,
     274,   868,     0,     0,     0,     0,     0,     0,   375,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     216,     0,     0,     0,     0,     0,     0,   662,     0,   209,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     209,     0,     0,     0,     0,     0,   375,     0,     0,     0,
       0,   738,     0,     0,  1310,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   940,     0,     0,  1680,  1688,     0,     0,  1680,
    1698,     0,     0,     0,     0,  1705,     0,   553,   553,  1709,
       0,  1711,     0,  1698,     0,   502,     0,     0,   502,   502,
       0,   487,     0,   553,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   969,     0,     0,     0,     0,   209,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   738,   738,   738,     0,     0,   738,
     738,  1002,   209,     0,     0,     0,   507,     0,     0,     0,
       0,     0,  1012,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1310,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   868,  1033,     0,     0,
    1035,     0,  1037,     0,     0,     0,     0,     0,  1002,     0,
    1049,  1002,     0,   553,     0,     0,     0,     0,     0,     0,
       0,   553,     0,     0,   274,     0,     0,     0,     0,  1413,
       0,     0,     0,     0,  1419,     0,     0,  1423,  1077,     0,
       0,  1427,   375,     0,     0,     0,   209,   209,     0,     0,
    1800,  1079,     0,   489,     0,     0,     0,     0,     0,     0,
       0,     0,  1088,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   487,     0,
       0,     0,     0,  1077,     0,     0,     0,   553,  2064,     0,
       0,   553,     0,     0,     0,     0,  1837,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   209,     0,     0,  1146,  1856,  1858,   516,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1157,
     489,     0,     0,     0,   553,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1876,     0,     0,     0,     0,
       0,     0,     0,   209,     0,     0,     0,     0,  1183,     0,
       0,     0,     0,     0,   216,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,     0,     0,
       0,  1540,     0,   209,     0,     0,     0,   209,     0,   209,
     553,   553,     0,     0,   274,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   455,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1300,  1302,     0,     0,
       0,     0,     0,     0,   487,     0,     0,     0,   553,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1592,     0,
       0,     0,     0,     0,  1943,     0,     0,   489,     0,  1601,
       0,  1946,  1605,  1948,  1608,  1609,  1954,  1959,     0,  1698,
       0,     0,     0,     0,  1965,     0,  1077,     0,     0,     0,
       0,   209,   738,     0,  1342,     0,     0,     0,     0,     0,
       0,  1002,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   489,     0,     0,     0,     0,     0,     0,     0,
    1635,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   489,     0,   489,     0,     0,     0,   489,   489,   489,
       0,     0,     0,     0,     0,   516,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   274,   489,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2030,     0,     0,     0,     0,  2036,  2038,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1740,     0,     0,  2057,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   516,     0,  1412,
       0,  1415,     0,     0,     0,     0,     0,   489,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2079,     0,  2082,     0,     0,
       0,  2084,  2086,     0,     0,     0,  2089,  2091,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1605,     0,     0,     0,     0,
     738,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1491,  1491,     0,   209,     0,     0,     0,     0,
       0,     0,     0,  1796,     0,     0,     0,     0,  2130,  2132,
    2134,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2155,
    2157,  2159,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   738,     0,     0,   507,     0,     0,     0,     0,     0,
    1538,     0,     0,     0,     0,     0,  1547,     0,     0,     0,
       0,     0,     0,     0,     0,   489,   489,     0,     0,     0,
       0,  1002,     0,     0,     0,   487,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   516,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1884,     0,
     868,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   489,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1913,  1914,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1928,  1929,     0,     0,  1637,  1638,     0,     0,     0,
       0,     0,     0,     0,     0,  1933,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1002,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     516,     0,     0,   868,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   209,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   209,     0,
       0,     0,     0,     0,     0,   209,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1033,     0,     0,     0,     0,     0,     0,     0,     0,
    1754,  1755,  2002,     0,   209,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   516,
       0,   868,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2170,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1473,     0,     0,     0,     0,     0,
       0,     0,     0,   489,  2062,     0,   489,   489,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   403,     0,     0,   404,     0,
     405,     0,   406,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   455,     0,     0,     0,  1214,  1825,   407,
    1216,     0,  1217,  -249,  -249,  1218,  1219,  1220,  1221,  1222,
    1223,  1224,  1225,  1226,  1227,  1228,  1229,  -345,  -345,  1230,
    1231,  1232,  1233,  1234,  1235,  1236,     0,  1237,     0,   408,
     409,     0,   510,   411,  1238,  1239,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,  1240,   414,   415,
     416,     0,   417,   418,     0,     0,     0,     0,     0,  1871,
      74,     0,     0,     0,     0,     0,     0,     0,     0,  2170,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -249,  1241,     0,     0,    78,   420,  1473,     0,     0,   303,
     209,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,   516,     0,     0,     0,  -189,     0,     0,  1903,
       0,     0,  1905,     0,     0,     0,     0,   403,     0,     0,
     404,     0,   405,     0,   406,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1919,  1214,
       0,   407,  1216,     0,  1217,  -250,  -250,  1218,  1219,  1220,
    1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  -345,
    -345,  1230,  1231,  1232,  1233,  1234,  1235,  1236,     0,  1237,
       0,   408,   409,     0,   510,   411,  1238,  1239,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,  1240,
     414,   415,   416,     0,   417,   418,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   209,     0,     0,  1882,     0,     0,     0,     0,
       0,     0,  -250,  1241,     0,     0,    78,   420,     0,     0,
       0,   303,  1473,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,     0,  -189,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,   404,     0,   405,     0,
     406,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1214,     0,   407,  1216,     0,
    1217,     0,     0,  1218,  1219,  1220,  1221,  1222,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  -345,  -345,  1230,  1231,  1232,
    1233,  1234,  1235,  1236,     0,  1237,  1002,   408,   409,     0,
     510,   411,  1238,  1239,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,  1240,   414,   415,   416,     0,
     417,   418,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1241,
       0,     0,    78,   420,     0,     0,     0,   303,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,     0,  -189,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    1213,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   403,     0,    46,   404,    47,   405,     0,   406,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,  1214,    58,  1215,  1216,     0,  1217,     0,
       0,  1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,
    1227,  1228,  1229,  -345,  -345,  1230,  1231,  1232,  1233,  1234,
    1235,  1236,     0,  1237,     0,   408,   409,    61,   510,   411,
    1238,  1239,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,  1240,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -3,  1241,     0,     0,
      78,  1242,     0,     0,     0,   303,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,     0,  -189,     4,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,  1213,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   403,
       0,    46,   404,    47,   405,     0,   406,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,  1214,    58,  1215,  1216,     0,  1217,     0,     0,  1218,
    1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,
    1229,  -345,  -345,  1230,  1231,  1232,  1233,  1234,  1235,  1236,
       0,  1237,     0,   408,   409,    61,   510,   411,  1238,  1239,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,  1240,   414,   415,   416,     0,   417,   418,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1241,     0,     0,    78,  1242,
       0,     0,     0,   303,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,     0,
    -189,     4,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   403,     0,    46,
     404,    47,   405,     0,   406,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   407,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,   409,    61,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,     0,   417,   418,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1692,  1693,  1694,
       0,     0,     0,   419,  1695,  1696,    78,  1242,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,     0,  1697,     4,
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
       0,   419,  1695,     0,    78,  1242,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,     0,  1697,   186,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     257,   258,     0,   259,    46,     0,    47,     0,   260,     0,
       0,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     4,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     403,     0,    46,   404,    47,   405,     0,   406,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,   407,     0,     0,     0,     0,     0,
    -466,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -466,   408,   409,    61,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,     0,   417,   418,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,  1690,    78,
    1242,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     4,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   403,
       0,    46,   404,    47,   405,     0,   406,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   407,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,   409,    61,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,     0,   417,   418,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   419,     0,     0,    78,  1242,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   403,     0,    46,
     404,    47,   405,     0,   406,   351,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   407,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,     0,   417,   418,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,    78,   484,     0,     0,
       0,     0,     0,   421,   485,    81,   422,   423,   424,   425,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
       0,   419,     0,     0,    78,  1297,     0,     0,     0,     0,
       0,   421,  1298,    81,   422,   423,   424,   425,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   403,     0,    46,   404,    47,   405,     0,
     406,   351,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   407,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,     0,
     417,   418,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,    78,   828,     0,     0,     0,     0,     0,   421,
     485,    81,   422,   423,   424,   425,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   403,     0,    46,   404,    47,   405,     0,   406,   351,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   403,
       0,    46,   404,    47,   405,     0,   406,   351,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   407,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,     0,   417,   418,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   419,     0,     0,    78,   828,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,  2012,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,     0,    -2,     0,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,    -2,    -2,  2041,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,
       0,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,  1308,
      -2,     0,     0,     0,     0,    -2,    -2,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,   403,     0,     0,   404,     0,   405,     0,
     406,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,    -2,     0,    58,   407,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1544,     0,    -2,     0,     0,     0,    -2,    -2,    14,    15,
      16,    17,    18,     0,    -2,    -2,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,     0,
     417,   418,     0,     0,   403,     0,     0,   404,    74,   405,
      75,   406,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,   407,   419,
       0,     0,    78,   420,     0,     0,     0,     0,     0,   421,
     485,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,     0,     0,     0,
     421,  1545,    81,   422,   423,   424,   425,     4,     5,     6,
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
       0,     0,    78,    79,     0,  1449,     0,  1450,     0,     0,
      80,    81,  1451,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1452,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1453,     0,     0,     0,
      78,  1007,     0,  1449,     0,  1450,     0,     0,    80,    81,
    1451,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1452,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1646,     0,     0,     0,    78,  1007,
       0,  1449,     0,  1450,     0,     0,    80,    81,  1451,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1452,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1647,     0,     0,     0,    78,  1007,     0,  1449,
       0,  1450,     0,     0,    80,    81,  1451,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1452,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1648,     0,     0,     0,    78,  1007,     0,     0,     0,     0,
       0,     0,    80,    81,   262,   186,     6,     7,     8,     9,
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
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   262,     0,     0,     0,     0,     0,     0,    78,
     263,     0,    14,    15,    16,    17,    18,    80,    81,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -491,  -491,     0,  -491,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     352,    78,   353,     0,     0,     0,     0,     0,     0,    80,
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
       0,    74,     0,    75,  1615,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   915,     0,     0,     0,
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
       0,     0,     0,     0,     0,    74,     0,    75,  1617,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     915,     0,     0,     0,     0,     0,     0,    80,    81,   186,
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
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   332,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,    78,   915,     0,
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
       0,    78,   353,     0,     0,     0,     0,     0,     0,    80,
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
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,  1473,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,   404,     0,   405,     0,
     406,     0,     0,     0,     0,    78,   263,     0,     0,     0,
       0,     0,     0,    80,    81,  1214,     0,   407,  1216,     0,
    1217,  1936,  1937,  1218,  1219,  1220,  1221,  1222,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,     0,     0,  1230,  1231,  1232,
    1233,  1234,  1235,  1236,     0,  1237,     0,   408,   409,     0,
     510,   411,  1238,  1239,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,  1240,   414,   415,   416,     0,
     417,   418,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,  1473,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1241,
       0,     0,    78,   420,     0,     0,     0,   303,     0,   421,
      80,    81,   422,   423,   424,   425,   403,     0,     0,   404,
       0,   405,     0,   406,  -189,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1214,     0,
     407,  1216,     0,  1217,     0,     0,  1218,  1219,  1220,  1221,
    1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,     0,     0,
    1230,  1231,  1232,  1233,  1234,  1235,  1236,     0,  1237,     0,
     408,   409,     0,   510,   411,  1238,  1239,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,  1240,   414,
     415,   416,     0,   417,   418,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1241,     0,     0,    78,   420,     0,     0,     0,
     303,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,     0,  -189,   307,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -416,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,     0,     0,     0,     0,  -416,   307,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -417,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,     0,     0,  -417,
     307,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,    14,    15,    16,    17,    18,    19,   725,    20,   726,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    63,    64,   403,     0,    46,
     404,    47,   405,     0,   406,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   407,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   727,     0,     0,     0,     0,  1229,     0,
    -345,     0,     0,     0,     0,    78,     0,     0,     0,     0,
    -416,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,     0,   417,   418,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1241,     0,     0,    78,   728,     0,     0,
       0,   303,     0,   421,    80,    81,   729,   730,   424,   425,
      14,    15,    16,    17,    18,    19,   725,    20,   726,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   403,     0,    46,   404,
      47,   405,     0,   406,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     407,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   727,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,     0,   417,   418,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,    78,   728,     0,     0,     0,
     303,     0,   421,    80,    81,   729,   730,   424,   425,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   403,     0,    46,   404,    47,
     405,     0,   406,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,     0,   417,   418,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,   450,    78,   451,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   403,     0,    46,   404,    47,   405,
       0,   406,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   407,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   451,     0,     0,     0,   303,     0,
     421,    80,    81,   422,   423,   424,   425,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   403,     0,    46,   404,    47,   405,     0,
     406,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   407,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,     0,
     417,   418,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,    78,   728,     0,     0,     0,   303,     0,   421,
      80,    81,   422,   423,   424,   425,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   403,     0,    46,   404,    47,   405,     0,   406,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,     0,   417,
     418,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,    78,   451,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   403,     0,    46,   404,    47,   405,     0,   406,   351,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,     0,
      78,   828,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,    14,    15,    16,    17,    18,    19,
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
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,   262,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -491,  -491,     0,
    -491,    46,     0,    47,     0,  -491,     0,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
     351,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   150,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     609,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -685,    78,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   150,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,   208,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     150,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,    78,    79,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   351,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   150,     0,   478,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   851,     0,     0,    78,   479,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     150,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   150,     0,   478,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   479,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     609,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
    -685,    78,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -491,
    -491,     0,  -491,    46,     0,    47,     0,  -491,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,   332,     0,     0,     0,     0,     0,     0,    80,    81,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   351,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,  1207,     0,     0,     0,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,    78,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     351,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   851,     0,     0,    78,   479,     0,     0,     0,     0,
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
      64,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   851,     0,     0,    78,
     479,     0,    63,    64,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1014,    78,  1007,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,  1563,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,  1007,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   315,     0,    63,    64,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   208,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   351,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,   351,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   353,     0,    63,    64,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   315,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   351,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   479,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -491,  -491,     0,  -491,    46,     0,    47,     0,  -491,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   351,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   332,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,  1007,     0,    63,    64,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,    79,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
     351,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   479,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,  1007,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   351,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,    14,
      15,    16,    17,    18,    80,    81,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -491,  -491,     0,  -491,    46,     0,    47,
       0,  -491,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   332,     0,    14,    15,    16,
      17,    18,    80,    81,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -491,  -491,     0,  -491,    46,     0,    47,     0,  -491,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,     0,     0,     0,     0,     0,
      80,    81,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   403,     0,    46,   404,    47,   405,     0,   406,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,     0,
      78,   420,     0,     0,     0,     0,     0,   421,   485,    81,
     422,   423,   424,   425,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   403,     0,    46,   404,    47,   405,     0,
     406,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,     0,
     417,   418,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,    14,    15,    16,    17,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,    78,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
     351,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
      20,    78,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -491,  -491,     0,
    -491,    46,     0,    47,     0,  -491,     0,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   187,
       0,   188,   189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,    75,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   716,     0,   717,   718,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    75,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -490,  -490,     0,  -490,    46,     0,    47,     0,  -490,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -491,  -491,     0,  -491,    46,     0,    47,   403,  -491,
       0,   404,     0,   405,     0,   406,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,     0,   417,   418,     0,     0,   403,
      75,     0,   404,    74,   405,     0,   406,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1692,  1693,
    1694,     0,     0,   407,   419,  1857,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   510,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,    77,     0,   511,   512,
       0,     0,     0,   513,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,  1345,     0,    78,   420,     0,     0,     0,  1346,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,     0,     0,
      78,   420,     0,     0,     0,   513,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   419,   867,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
     998,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,     0,     0,    78,   420,
       0,     0,     0,   303,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,     0,     0,    78,   420,     0,     0,  1071,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,  1414,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   419,     0,     0,    78,   420,     0,     0,
       0,  1483,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
       0,     0,    78,   420,     0,     0,     0,  1765,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,     0,  1942,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,  1947,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,  1951,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,  1958,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,  1952,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   419,     0,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
    2035,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,  2037,     0,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,  2081,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,  2083,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   419,  2085,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
    2088,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,  2090,     0,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,  2129,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,  2131,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   419,  2133,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
    2154,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,  2156,     0,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,  2158,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,     0,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   707,     0,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   713,
       0,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   722,     0,     0,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,     0,     0,    78,   420,     0,     0,     0,     0,
       0,   421,   939,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,     0,
      78,   420,     0,     0,     0,     0,     0,   421,   485,    81,
     422,   423,   424,   425
};

static const yytype_int16 yycheck[] =
{
       1,   180,   170,     4,    98,    76,    76,   276,   168,   261,
     668,    76,   419,    98,     1,   223,   952,     4,   235,   168,
     751,   513,  1259,   754,    76,   235,   235,     1,   949,  1241,
       1,   356,   677,   185,   230,   664,   143,   170,   184,    85,
     235,   936,    59,  1197,     1,   739,   168,  1808,    78,   798,
     421,   984,   235,  1046,  1808,    56,    57,   354,    59,   992,
     256,  1808,  1702,  1293,  1294,   100,   832,   207,   185,   845,
     858,   847,    59,   370,   235,    76,   732,   374,   582,   664,
     832,   245,  1075,   370,    85,    59,    75,   374,    59,   593,
       1,   194,    93,    90,  1936,   830,   235,    98,   830,   206,
     101,     1,    59,  1940,   105,    84,   152,   324,   235,   273,
     235,    85,    89,   139,   324,   324,  1224,   830,   105,     1,
     284,   120,   830,    72,    98,   196,   196,   101,    75,   324,
     135,   105,   149,   125,   638,   924,   101,  1809,   163,   936,
       0,   324,   236,   144,   196,   180,   147,     1,   149,   155,
    1143,   236,   664,   159,   155,   180,   945,     1,   155,    59,
     159,   162,   149,   324,   235,   235,   158,   492,   348,   170,
     235,    76,   235,   178,   832,   149,   155,    59,   149,    77,
      78,   182,  1354,   235,    72,   324,   163,   136,     4,   486,
     155,   180,   149,   194,   195,   196,   163,   324,   238,   324,
     240,   830,   990,  1206,   137,   206,    76,   247,   388,   377,
     211,     1,  1852,   180,   120,    59,   384,   311,   264,   245,
     221,   195,   137,   105,    10,   226,   311,  2069,   229,   230,
     231,   325,   354,   180,   235,   236,   244,   211,   171,     1,
     325,    76,     4,   251,   377,   830,     0,   273,   136,   149,
     161,   162,  1924,   324,   324,   256,   171,   139,   284,   324,
      98,   324,   236,   264,   272,   163,  1698,   149,   298,    59,
     833,   159,   324,   274,   275,   283,   839,   278,    89,   319,
     320,   154,  2119,   309,   285,   554,   611,   333,     0,   105,
     264,   196,   395,   562,   207,   139,   111,    59,   299,   300,
      75,   302,    10,  1034,   278,   149,   307,   180,   162,   112,
     311,   519,   970,   285,   531,  2152,     1,    92,   830,   134,
    1215,   531,   531,   324,   325,   155,   196,  1241,   300,   670,
     235,   656,   135,   334,  2072,  1084,   531,   311,   709,   101,
    2101,   342,   343,   105,  1110,   752,   347,  2101,   531,   139,
     161,   325,   163,  1248,  2101,   680,   653,   490,  1110,   149,
    2098,   196,   687,   245,   486,   235,   653,  1023,  1103,    20,
     531,  1103,   568,  2045,    59,   482,   377,   149,   574,   165,
     677,   155,  1315,  2121,   170,   386,     4,   149,   389,   155,
    1103,   273,   531,   690,   395,  1103,   503,   155,   236,   171,
     235,   245,   284,   690,   531,  1559,   531,   453,    72,  1198,
    1582,  1583,  1584,   179,   631,   157,   101,   921,  1215,   161,
      72,   631,   631,   161,   419,  1857,  1858,   309,   592,   273,
    2102,   313,    72,   570,   571,  1206,   631,   577,    56,    57,
     284,   155,   180,  1446,  1447,  1448,   154,   160,   631,   543,
      58,  1248,  1110,    61,    62,   245,    64,   165,   543,   158,
     531,   531,   170,   464,   149,   309,  1206,  2139,   531,   155,
     631,  1194,   136,   311,  1103,    93,    61,    62,  1201,   531,
     179,   511,    75,   273,   136,   155,   635,   325,   489,   490,
     464,   155,   631,   834,   284,   159,   136,   838,    91,   386,
     501,   502,   389,   155,   631,     1,   631,   159,   849,   850,
     511,   155,   513,   635,   654,   155,   278,   110,  1103,   309,
     180,  1953,  1954,  1735,   134,  1313,   144,   695,   825,   147,
     531,   161,  1320,   161,   852,   853,    77,    78,   825,   633,
     161,  1104,   543,   171,   162,   155,   155,   155,   633,   159,
     558,    62,   170,   871,   163,   677,   166,   167,   302,  1282,
     631,   631,   699,    59,   108,   109,   592,   568,   631,   543,
     130,   131,   161,   574,   711,   576,   584,    75,   163,   631,
     137,   161,  1370,   591,  1577,    85,  1078,   595,   206,  1697,
     101,  1103,    90,   278,  1702,   180,  1510,   161,    98,  1513,
    1514,   101,   113,   180,   115,   105,   117,   171,   226,   166,
     167,   229,     3,   231,   158,   175,   176,  1550,   161,   161,
     637,  1397,   163,   941,  1400,  1401,   531,   312,   157,   171,
     631,   152,   633,   779,   959,    69,   637,   180,   639,   168,
     169,   781,   163,   139,     3,   156,   161,   648,   159,   160,
     637,   652,   860,   149,  1299,  1885,   274,   275,   179,   633,
     957,   531,   161,   637,   577,   180,   637,   285,   805,  1363,
     957,   356,   171,   158,  1471,  1446,  1447,  1448,  1475,  1476,
     637,   299,   300,   684,   302,   161,   826,   847,   161,   307,
     161,   843,  1489,    72,   112,   195,   531,   161,   171,   832,
     211,   897,   703,   161,   180,   543,  1446,  1447,  1448,   180,
     592,   211,   707,   171,   163,   710,   334,   135,   713,   161,
    1443,   170,   635,   159,   342,   343,   843,   722,   164,   347,
     725,   726,   727,  1464,   161,  1656,   236,   637,   180,   157,
     857,   654,  1305,   744,  1852,   746,  1983,   748,   592,   245,
     780,   752,   898,   180,   755,   637,   161,   136,   155,   377,
     155,    72,   157,  1551,   264,   161,   163,   278,   386,   280,
     281,   389,   155,   170,    72,   180,   155,   273,   278,   780,
     159,  1099,  1100,   157,   180,  1573,  1127,   161,   284,  1782,
     161,  1784,  1486,   637,    72,   633,   166,    47,    48,    72,
      50,   312,   592,   173,   174,    55,   317,   492,  1744,   180,
     654,   159,   323,   309,   163,   559,   164,   985,   149,   150,
     151,  1735,   179,  1386,   825,   136,  1759,   157,   163,   830,
     161,   832,   162,   155,  1241,   170,   157,  1155,   136,   407,
     171,   162,  1950,   844,   155,   356,  1649,   637,   159,   180,
     361,  1654,   363,   854,  1346,   163,  1964,   155,   136,   860,
      72,   159,   863,   136,   432,   433,   155,   154,   781,    76,
    1019,   489,    72,  1142,   161,   637,   161,   155,   163,   120,
    1574,   159,   155,   501,   502,   453,   159,    72,   155,   177,
     634,    98,   157,   157,     3,   162,   897,  1019,   162,   410,
     149,   150,   151,  1561,    13,    14,    15,    16,    17,   156,
     155,  1018,   157,   826,   159,   923,   163,     3,   486,  1707,
     157,  2029,   171,   157,   136,   162,   611,    13,    14,    15,
      16,    17,   845,   934,   935,   936,   136,   154,   157,   624,
     159,   157,  1150,   155,   161,   160,   162,   159,   155,   936,
      72,   136,   637,   464,  1071,   155,   158,   159,   576,   159,
     162,   948,   157,    72,   464,   936,    62,   155,   134,   157,
     155,   159,   155,  1761,   159,   157,  1109,  1110,  1303,   936,
     157,   492,   826,   494,   495,   157,    72,   155,  1776,   155,
     991,  1483,   155,   159,   157,   680,   159,   108,   109,   510,
     166,   167,  1299,   161,    13,    14,    15,    16,    17,    22,
     106,    72,    72,   179,   136,   111,   108,   109,   114,   155,
     116,   639,   155,   230,   157,   101,   159,   136,   235,   236,
     648,   155,   543,   155,   652,  1152,   936,   159,   157,  1040,
     157,   158,   161,   543,  1451,  1046,    72,   157,   155,   256,
     136,   161,   159,   797,   936,   157,  1308,   568,  1855,   161,
     161,   155,   573,    72,   575,   159,   684,   157,   157,   813,
     155,   161,   161,   817,  1075,   136,   136,  1078,   128,   129,
    1572,    13,    14,    15,    16,    17,   597,   157,   599,   600,
    1286,   161,   936,   157,   155,   155,   592,   161,   159,   159,
     611,   157,  1103,   155,   311,   161,  1019,  1895,  1109,  1110,
     136,   160,  1900,   624,  1293,  1294,   163,   324,   325,   157,
    1280,    89,   633,   161,   163,    72,   744,   136,   746,   155,
     748,  1280,   948,   159,   752,   168,   169,   755,   154,  1179,
      72,   637,  1143,   180,  1312,   656,   936,   658,   659,   163,
      58,   132,   133,    61,    62,  1292,    64,   155,  1280,   155,
    1206,   159,   780,   159,  1433,   149,   150,   151,   157,   680,
     681,   267,   161,   155,   936,   160,   687,  1299,  1327,  1328,
     157,    13,    14,    15,    16,    17,   948,   171,   163,   136,
     758,   113,   114,   115,   116,   117,   180,    13,    14,    15,
      16,    17,   134,   170,   136,    72,  1328,   825,   155,   157,
    1197,   157,   159,   161,   179,   161,  2013,  1218,   314,   157,
    1221,  1222,  1223,   155,   160,   161,   844,   159,  1215,  1230,
     766,   767,   768,   769,   166,   167,   854,  1397,   155,    72,
      72,  1401,    72,   120,  1215,  2042,  1241,  1248,  1979,  1366,
     155,   936,   155,  1254,  1406,  1407,    72,   161,  1215,   157,
     155,  1248,   358,   161,   360,   155,   362,   167,  1269,   136,
     172,  1272,  1273,  1765,   959,  1276,  2073,  1248,   160,   161,
     134,   165,  1283,   490,   177,  1286,  1273,   134,   155,  1406,
    1407,  1248,   159,    13,    14,    15,    16,    17,  1272,  1273,
     158,   155,   134,   136,   136,   159,   136,   157,   759,   760,
     761,   161,   166,   167,   410,  1215,   934,   935,   936,   157,
     136,   137,   155,   155,   531,   155,   159,   159,  1329,   159,
    1646,  1647,  1648,  1215,   166,   167,   543,   157,  1333,    13,
      14,    15,    16,    17,    18,  1346,  1090,   157,  1248,   157,
    1094,   161,    72,  1354,   157,   157,   157,   134,   161,   161,
     161,   568,   157,  1457,   157,   157,  1248,   574,   161,  1113,
     157,  1215,  1457,   991,   161,   157,  1120,   157,   155,   161,
     159,  1197,   159,   157,   157,  1534,  1387,   161,   161,   166,
     167,  1273,   157,  1203,  1204,  1205,   161,   149,   150,   151,
    1446,  1447,  1448,  1555,  1248,  1451,  1452,   157,   155,   161,
     155,   161,  1534,   137,   510,  1328,   136,   137,   157,   171,
     160,   161,   161,  1167,   631,  1215,   633,  1171,   180,   157,
      18,  1175,  2106,   161,   160,  1197,  2110,  1471,  1555,   137,
     936,  1475,   161,  1660,   161,  1471,  1014,   162,   959,  1475,
    1660,  1660,  1020,  1215,   166,   167,  1457,  1273,  1248,   970,
    1461,  1462,  1449,  1031,   161,  1660,   160,   161,   979,    57,
      58,    59,    60,    61,    62,    63,    64,  1660,  1668,  1669,
    1670,   162,  1483,   155,  1471,   160,  1248,  1400,  1475,  1476,
     160,   161,   179,   589,   160,   161,   161,   162,   157,  1660,
     160,   161,  1489,   160,   161,  1506,  1507,    91,    92,   155,
    1272,  1273,   160,   161,  1471,  1516,   160,   161,  1475,  1476,
     157,  1660,    13,    14,    15,    16,    17,    18,   160,   161,
    1215,   157,  1489,  1660,   157,  1660,   160,   161,   157,   106,
     160,   161,  1516,   110,   111,   112,   113,   114,   115,   116,
     117,   118,  1553,   160,   161,  1649,   160,   161,   160,   161,
    1654,   160,   161,  1248,  1649,   160,   161,   157,  1662,  1654,
     179,  1572,  1559,  1084,   161,   162,  1577,  1662,   157,  1725,
     157,  1582,  1583,  1584,   160,   161,   157,  1272,   163,  1471,
     160,   161,   159,  1475,  1476,  1660,   159,  1660,   163,  1110,
    1218,    77,    78,  1221,  1222,  1223,  1823,  1489,   161,   162,
    1355,  1356,  1230,  1823,  1823,   762,   763,   163,  1303,  1457,
     163,  1534,   163,   830,   157,   832,   161,  1471,  1823,    70,
    1248,  1475,  1476,  1449,   764,   765,  1254,   160,   770,   771,
    1823,  1513,  1514,  1763,  1764,  1489,   180,  1215,  1649,  1669,
    1670,  1269,   155,  1654,    78,   160,    18,   179,  1276,  1660,
     180,  1662,  1823,   163,   163,  1283,   163,   157,   157,   180,
    1671,   163,   160,   160,  1418,  1972,    18,   160,  1422,   160,
     154,  1471,  1426,    22,  1823,  1475,  1476,  1449,   157,  1690,
     897,   157,  1888,   157,   157,  1696,  1823,   157,  1823,  1489,
    1687,   157,   157,   157,   157,   157,  1885,   157,   157,  1471,
     154,  1329,   160,  1475,  1476,   154,   163,   163,   163,  1215,
      70,  2046,   157,   161,   157,   157,  1872,  1489,   179,   157,
     157,  1299,  1826,    13,    14,    15,    16,    17,    18,  1740,
     163,  1826,   161,  1559,   157,   163,  2043,   157,   157,   161,
     161,   157,  1248,   123,  1516,   125,   126,   127,   160,  1746,
    1823,  1272,   157,   157,  1765,   161,   157,   157,  1336,  1337,
    1338,   157,  1272,  1273,   157,  1343,  1344,   157,   157,  1774,
     157,  1782,   160,  1784,  1936,   155,  1471,   160,   158,   159,
    1475,  1476,  1303,   163,   164,  1539,   157,  1559,  1309,   157,
     157,   157,   157,   157,  1489,  1687,   157,   160,   157,   157,
     157,  1649,   157,   154,    14,   161,  1654,   161,   161,  1936,
     157,   161,  1823,   154,  1662,  1826,   155,   155,   155,   155,
    1976,  1516,   155,   155,  1835,  1836,   155,   155,   162,  1933,
     161,  1842,   180,  1461,  1462,   162,   160,  1591,  1933,  2045,
     160,   163,   154,  1854,   154,   180,  1600,   163,   161,   179,
    1604,   157,   157,  1864,   180,  1866,   157,   180,  1855,   157,
     157,  1687,   161,   157,   160,   160,  1877,   160,  1879,  1880,
    1881,   157,   157,   157,  2101,   157,  1887,  1888,  1506,  1507,
     157,  2101,  2101,   154,   160,   154,  1103,   155,  1855,   155,
      80,   180,  1109,  1110,    92,   154,  2101,    13,    14,    15,
      16,    17,    18,   155,   155,  2067,   180,  2069,  2101,    90,
     180,   157,   154,   180,  1686,  1687,  1972,   180,   180,   180,
    1746,   154,  1933,   161,   161,  1553,   135,   163,   207,  1940,
    2101,   160,   160,  1944,   160,   160,   154,  1458,  1949,   154,
    2067,   157,  2069,   162,   123,   162,  2108,   154,   160,   157,
    2106,   157,  2101,   157,  2110,  2111,    13,    14,    15,    16,
      17,   160,   157,  1855,  2101,  1471,  2101,   154,   157,  1475,
    1476,   157,   157,   161,  1746,   180,   154,   157,  1826,   162,
     155,  2108,   155,  1489,   161,  2141,   157,  2043,   155,   157,
     160,  1686,   154,  2004,   160,  1516,   160,   157,  2102,   154,
     157,  1855,   160,   157,   157,  2016,  1516,  2102,  2164,  2020,
    2137,   180,  2168,    75,    75,    72,  2013,  2179,   227,   180,
    2101,  2101,   154,  2034,   180,   155,  2101,  2183,  2101,     1,
     180,   180,     4,   157,  2045,  2139,  2047,   155,   160,  2101,
     160,   154,   154,  1671,  2139,  2042,  2013,   154,   157,   157,
      75,   157,  2179,    75,   159,  1855,   171,   108,   158,   180,
     171,    75,  1690,   180,   162,  2076,  1838,   154,  1696,  1286,
     180,   154,   154,   154,   171,  2042,  2073,   134,   156,   136,
     171,   207,   162,  1855,   155,  1933,   106,    59,   161,   156,
    2101,  2102,   171,    75,   171,   180,   160,   157,   155,   308,
     156,   154,   159,  2114,    76,   180,  2073,   157,  2119,   166,
     167,   157,  1740,    85,   162,   154,   157,   155,  2102,  1774,
     157,  2013,   180,  1324,   731,   180,    98,   772,  2139,   101,
     773,   180,   776,   105,   774,  1236,  2147,   775,   452,  2150,
    1248,  2152,  2152,  1838,  1863,  1475,  2098,  2069,    85,  1855,
    2042,  1489,  1730,  1957,  2058,  2139,  2136,  1734,  1718,  2013,
    1855,   302,  2173,  1718,  2043,  1686,  2111,  2168,  2042,  1276,
    1387,   143,    49,  2184,   114,   269,  1933,   149,  2002,  1452,
     152,  2073,  2193,   155,   156,  1269,  1317,   860,  2042,  1523,
    1746,     0,   703,   797,   797,   106,   168,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,  1835,  1836,   520,
     419,  1638,   648,  2013,  1842,   152,    -1,    -1,   797,  2073,
      -1,   193,    -1,   195,   196,    -1,  1854,    -1,   354,    -1,
      -1,   357,    -1,    -1,   206,   207,  1864,    -1,  1866,   211,
    1457,  2013,  2042,    -1,   370,    -1,    -1,    -1,   374,  1877,
      -1,  1879,  1880,  1881,  2102,   166,   193,    -1,   230,  1887,
      -1,    -1,    -1,   235,   236,    -1,    -1,    -1,    -1,    13,
    2042,    -1,    -1,  2073,    -1,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,   256,    65,    66,    67,    68,    -1,
      -1,  2139,   264,    -1,    -1,    -1,    -1,    -1,   577,    -1,
      -1,  2073,    -1,    -1,    -1,  1826,   278,  2061,    -1,    -1,
      -1,    -1,  1940,    -1,    -1,    -1,  1944,  1838,  2013,    -1,
      -1,  1949,    -1,    -1,    -1,    -1,   106,   264,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   311,
      -1,    -1,    -1,    -1,    88,   317,    -1,  2042,   557,  1855,
      -1,   323,   324,   325,    -1,    -1,   565,    -1,    -1,    -1,
     486,   333,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   583,   654,  2004,    -1,  2073,   159,
      -1,    -1,   354,   355,   356,   594,    -1,    -1,  2016,    -1,
      -1,    -1,  2020,    -1,    -1,    -1,   333,   177,   370,    -1,
    1978,    -1,   374,  1924,    -1,    -1,  2034,    -1,    -1,    -1,
      -1,   155,    -1,    -1,    -1,    89,    -1,   354,   559,  2047,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    78,  1649,    -1,    -1,    -1,    -1,  1654,    -1,    -1,
      -1,    -1,    -1,  1660,    -1,  1662,    -1,   419,  2076,    -1,
      -1,   577,    -1,    -1,    -1,    -1,   130,    -1,   246,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,
      -1,   453,    65,    -1,   456,    -1,  2114,    -1,    -1,   185,
      -1,  2119,   464,   634,    -1,    -1,    -1,    -1,   707,    -1,
      -1,    -1,   781,    -1,   713,    -1,    -1,  2013,    -1,   635,
     482,    -1,    -1,   722,   486,    -1,   453,    -1,   490,  2147,
     492,    -1,  2150,   664,  2152,  2046,    -1,   653,   654,    -1,
      -1,   503,   741,   180,    -1,    -1,  2042,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2173,    -1,   826,    -1,   486,
      -1,   677,    -1,    -1,    -1,    -1,  2184,    -1,    -1,   531,
      -1,    -1,    -1,    -1,   690,  2193,   845,  2073,   847,    -1,
      -1,   543,   851,   852,   853,    -1,    -1,    -1,    -1,   106,
     221,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   871,    -1,    -1,    -1,   568,    -1,   570,   571,
      -1,    -1,   574,    -1,    -1,   577,  1823,   134,    -1,  1826,
      -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,  2139,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   155,   156,
      -1,    -1,    -1,   570,   571,   162,    -1,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,   321,    -1,    -1,
      -1,    -1,    -1,   932,    -1,   781,   797,    -1,    -1,   631,
      -1,   633,   941,   635,    -1,   637,   156,    -1,   456,   159,
      -1,  1888,   813,    -1,    -1,    -1,   817,    -1,    -1,    -1,
      -1,   653,   654,    -1,   656,   473,    -1,    -1,   476,   830,
      -1,    -1,   664,    -1,    -1,   904,   668,    -1,    -1,   825,
     826,    -1,    -1,    -1,    -1,   677,    -1,   403,    -1,   918,
      -1,   407,   408,   922,    -1,   687,  1933,   926,   690,    -1,
      -1,   417,   418,    -1,    -1,    -1,    -1,   699,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   707,   432,   433,   710,   711,
     677,   713,    -1,    -1,    -1,    -1,    -1,    -1,   536,    18,
     722,    -1,    -1,   725,   726,   727,    -1,   453,    -1,    -1,
      -1,    -1,   699,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     707,    -1,    -1,    -1,   711,    -1,   713,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     486,   932,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,   781,
      -1,   485,    -1,   487,    -1,    -1,    13,    -1,    -1,    -1,
    1099,  1100,   496,    -1,    -1,   797,   798,    -1,  2045,    -1,
      -1,   957,    -1,   805,   960,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   180,    -1,   825,   826,    -1,    -1,    -1,   830,    -1,
     832,    -1,    72,    -1,    -1,    -1,    -1,    -1,   805,    -1,
     511,    -1,   513,   845,    -1,   847,  1155,    -1,    -1,   851,
     852,   853,    -1,    -1,  2101,  2102,    -1,    -1,    -1,    -1,
     159,    88,    -1,  1019,    -1,    -1,   106,    72,    -1,   871,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,  2139,    -1,   134,   897,   136,    -1,    -1,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,   155,   156,    -1,    -1,  1090,
      -1,    -1,    -1,  1094,    -1,    -1,   166,   167,    -1,    -1,
      -1,   136,  1103,    -1,   936,    -1,    -1,    -1,    -1,   941,
      -1,    -1,  1113,    -1,    -1,    -1,   948,   949,    -1,  1120,
     155,   156,    -1,    -1,    -1,   957,    -1,   959,    -1,    -1,
      -1,  1200,    -1,    -1,    -1,    -1,    -1,    -1,   970,    -1,
      -1,    -1,    -1,    -1,    -1,  1214,    -1,    -1,    -1,   797,
     798,    -1,   949,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     808,    -1,    -1,   811,    -1,  1234,  1167,    -1,    -1,    -1,
    1171,    -1,  1241,   106,  1175,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,  1018,  1019,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,   758,   759,   760,   761,   762,   763,   764,   765,
     766,   767,   768,   769,   770,   771,   772,   773,   774,   775,
     776,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,   877,
      -1,     4,    -1,    -1,    -1,    -1,   884,    -1,    -1,    -1,
     888,    -1,    -1,    -1,   892,    -1,    -1,   180,    -1,    -1,
      -1,    -1,  1084,    -1,    72,    -1,    -1,    -1,  1397,    -1,
      -1,  1400,  1401,    -1,    -1,    -1,    -1,  1099,  1100,    -1,
      -1,  1103,    -1,    -1,    -1,    -1,    -1,    -1,  1110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    59,   843,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
       4,     5,     6,     7,     8,     9,    10,    11,    12,   843,
      -1,    -1,    85,  1299,    -1,    -1,   134,    -1,   136,    -1,
     106,    -1,   108,  1155,   110,   111,   112,   113,   114,   115,
     116,   117,   105,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,   159,  1328,    -1,    -1,    -1,   106,    -1,   166,   167,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   860,
      -1,    -1,   863,    -1,    -1,  1197,   139,    -1,    -1,    -1,
     143,    -1,    -1,    -1,  1206,    -1,   149,    -1,    -1,   152,
      -1,    -1,   106,  1215,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,   168,  1465,  1466,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   939,   940,    -1,    -1,  1241,
     134,    -1,    -1,    -1,    -1,    -1,  1248,  1418,    -1,    -1,
     193,  1422,   195,    -1,    -1,  1426,    -1,    -1,    -1,    -1,
      -1,   155,   156,   206,   207,   159,  1084,    -1,    -1,    -1,
    1272,  1273,   166,   167,    -1,    -1,  1515,    -1,  1280,    -1,
      -1,    -1,    -1,    -1,  1286,   179,    -1,    -1,  1014,    -1,
    1292,    -1,    -1,   236,  1020,    -1,    -1,  1299,    -1,    -1,
      -1,    -1,   245,    -1,    -1,  1031,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   256,    -1,    -1,    -1,    -1,   261,   262,
      -1,   264,    -1,    -1,    -1,  1292,  1328,    -1,    -1,  1033,
     273,  1333,  1299,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1158,   284,    -1,  1161,   287,  1071,    -1,  1165,   291,    -1,
      -1,    -1,   106,   296,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   307,    -1,   309,    -1,  1539,  1040,
     313,    -1,    -1,  1077,    -1,  1046,    -1,    -1,  1534,    -1,
      -1,    -1,   325,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     333,    -1,    -1,    -1,    -1,  1397,    -1,    -1,  1400,  1401,
      -1,    -1,    -1,    -1,  1075,    -1,    -1,  1078,    -1,    -1,
      -1,   354,    -1,    -1,   357,    -1,   170,    -1,    -1,    -1,
    1591,    -1,  1126,    -1,  1128,    -1,    -1,   370,    -1,  1600,
      -1,   374,    -1,  1604,    -1,    -1,  1140,  1141,    -1,    -1,
      -1,    -1,  1146,  1147,  1446,  1447,  1448,  1449,    -1,  1451,
    1452,    -1,  1156,    -1,    72,  1457,  1458,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,  1471,
      -1,    -1,  1143,  1475,  1476,    -1,   419,    -1,  1717,  1183,
      -1,    -1,  1186,    72,    -1,    -1,    -1,  1489,   106,  1215,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     453,    -1,    -1,    -1,  1516,    -1,   134,   106,   136,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,  1534,    -1,    -1,    -1,    -1,   155,   156,   482,
      -1,   159,    -1,   486,  1248,   134,    -1,   136,   166,   167,
      -1,    -1,    -1,    -1,  1372,    -1,    -1,  1559,    -1,  1561,
     503,    -1,    -1,    -1,    -1,  1383,   155,   156,    -1,    -1,
      -1,    -1,    -1,  1299,    -1,  1279,    -1,   166,   167,    -1,
      -1,    -1,    -1,  1287,    -1,  1289,  1290,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1298,    -1,  1300,    -1,  1302,    -1,
      -1,    -1,    -1,    -1,    -1,  1309,    -1,    -1,    -1,    -1,
    1336,  1337,  1338,    -1,    -1,    -1,    -1,  1343,  1344,    -1,
      13,    14,    15,    16,    17,    -1,    -1,   570,   571,    -1,
      -1,    -1,    -1,    -1,   577,    -1,    -1,    -1,    -1,    -1,
    1366,    -1,    -1,    -1,    -1,    -1,    -1,  1649,  1650,   592,
      -1,    -1,  1654,    -1,  1656,    -1,    -1,    -1,  1660,    -1,
    1662,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,  1346,  1380,  1381,    -1,    72,
    1406,  1407,    -1,  1354,  1686,  1687,    -1,    -1,    -1,  1656,
      -1,    -1,   635,    -1,   637,    -1,    -1,    -1,    62,    -1,
      -1,  1405,    -1,    -1,    -1,    -1,    -1,    -1,  1412,    -1,
     653,   654,  1416,   106,   158,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   668,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   677,    -1,    -1,   101,  1442,   682,
      -1,   134,    -1,   136,  1746,    -1,    -1,   690,    -1,   113,
     114,    -1,    -1,    -1,    -1,    -1,   699,    -1,    -1,    -1,
      -1,    -1,   155,   156,   707,    -1,   159,   710,   711,    -1,
     713,    -1,  1774,   166,   167,    -1,    -1,    -1,    -1,   722,
      -1,    -1,   725,   726,   727,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   156,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,  1808,  1809,    -1,    -1,
      -1,    -1,  1483,    -1,    -1,    -1,  1972,    -1,    -1,    -1,
      -1,  1823,    -1,    -1,  1826,    -1,    -1,    -1,    -1,  1555,
      -1,    -1,  1650,    -1,  1538,    -1,  1838,    -1,   781,    -1,
      -1,  1545,    -1,  1547,    -1,    -1,    -1,   211,   163,    -1,
      -1,    -1,    -1,  1855,    -1,   798,    -1,    -1,    -1,    -1,
      -1,    -1,   805,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    13,    14,    15,    16,
      17,    -1,   825,   826,    -1,    -1,  1888,  2043,    -1,   832,
    2061,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,  1572,   845,    -1,   847,    -1,  1577,    -1,   851,   852,
     853,  1582,  1583,  1584,   278,    -1,   159,  1621,    -1,    -1,
      -1,    -1,  1924,    -1,  2163,    -1,    -1,    -1,   871,    -1,
      -1,  1933,    -1,    -1,   106,    72,  2175,    -1,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,   312,    -1,
      -1,    -1,    -1,   317,    -1,    -1,    -1,    -1,    -1,   323,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,   106,
    1972,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
    1808,  1809,   356,   936,   166,   167,    -1,   134,   941,   136,
    2002,    -1,    -1,    -1,  1730,   948,   949,    -1,    -1,    -1,
      -1,  2013,    -1,    -1,   957,    -1,    -1,   960,   155,   156,
      -1,    -1,    -1,    -1,   967,    -1,    -1,    -1,   106,   166,
     167,    -1,   110,   111,   112,   113,   114,   115,   116,   117,
    2042,  2043,    -1,  2045,  2046,    -1,   410,    -1,    72,    -1,
    1754,  1755,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,  1768,    -1,    -1,    -1,    -1,    -1,
      -1,  2073,    -1,    -1,    -1,  1018,  1019,   155,   156,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,  1765,    -1,    -1,    -1,    -1,  2101,
    2102,    -1,   156,    -1,    -1,   159,  1924,    -1,    -1,    -1,
     134,  1782,   136,  1784,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,   492,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,  2139,    -1,    -1,
      -1,    -1,   166,   167,    -1,    -1,   510,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1099,  1100,     1,    -1,
      -1,     4,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
    1998,    72,    -1,   104,  2002,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
    1936,   575,  1155,    -1,   158,   106,    59,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,  2045,    -1,    -1,
      -1,    -1,    -1,   597,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,    85,   134,    -1,   136,   155,   611,    -1,    -1,
      -1,    -1,  1978,    -1,  1197,    -1,    -1,    -1,   101,    -1,
     624,    -1,   105,  1206,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,  1215,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,  2101,  2102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   656,    -1,    -1,    -1,   139,    -1,  1241,    -1,
     143,    -1,    -1,    -1,    -1,  1248,   149,    -1,    -1,   152,
      -1,    -1,    -1,   156,    -1,    -1,   680,    -1,    -1,    -1,
      -1,  2139,    -1,   687,   167,   168,    -1,   170,    -1,    -1,
    1273,    -1,    -1,    -1,    -1,    -1,    -1,  1280,    -1,    -1,
    2044,  2067,    -1,  2069,    -1,    -1,    -1,    -1,    -1,  1292,
     193,    -1,    -1,    -1,    -1,    -1,  1299,    -1,    -1,    -1,
      -1,    -1,    -1,   206,   207,  1308,    -1,    -1,   211,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,  2108,    -1,    -1,  1328,    -1,    -1,    -1,    -1,
    1333,    -1,    -1,    -1,    -1,  2099,    13,    14,    15,    16,
      17,    -1,   245,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2137,    -1,    -1,    -1,    -1,  2120,    -1,   155,   262,
      -1,   264,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     273,  2135,    -1,    -1,    -1,   278,    -1,    -1,    -1,    -1,
     106,   284,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,  2179,  1397,    72,   299,  1400,  1401,   302,
      -1,    -1,    -1,    -1,   307,    -1,   309,    -1,    -1,   312,
     313,    -1,    -1,    -1,   317,    -1,    -1,    -1,    -1,    -1,
     323,    -1,    -1,    -1,    -1,    -1,    56,    57,    -1,   106,
     333,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,  1446,  1447,  1448,  1449,  1450,  1451,  1452,
      -1,   354,     1,   356,   357,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    93,    -1,    -1,    -1,   370,  1471,    -1,
      -1,   374,  1475,  1476,    -1,    -1,    -1,   901,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,  1489,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,   144,    -1,   419,   147,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,  1534,   162,    -1,    -1,   959,    -1,    -1,    -1,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     453,    -1,    -1,   152,   103,   104,  1559,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,   482,
     179,    -1,    -1,   486,    -1,   134,    -1,    -1,    -1,   492,
      -1,    -1,    -1,    -1,    -1,    -1,   226,    -1,    72,    -1,
     503,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   274,   275,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1656,    -1,   285,   559,    -1,    -1,  1662,
     134,    -1,   136,    -1,    -1,    -1,    -1,   570,   571,    -1,
     300,    -1,   575,    -1,   577,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,  1687,    -1,    -1,    -1,    -1,   592,
      -1,   106,   166,   167,    -1,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   334,    -1,    -1,   122,   611,   124,
      -1,    -1,   342,   343,    -1,    -1,    -1,   347,    -1,    -1,
      -1,   624,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   634,   635,    -1,   637,    -1,    -1,    -1,    -1,    -1,
      -1,   156,    -1,  1746,   159,    -1,    -1,    -1,    -1,   652,
     653,   654,    -1,   656,    -1,    -1,   386,    -1,    -1,   389,
      -1,   664,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1774,    -1,    -1,   677,    -1,    -1,   680,    -1,    -1,
      -1,   684,    -1,    -1,   687,    -1,    -1,   690,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   699,    -1,    -1,  1218,
      -1,    -1,    -1,    -1,   707,    -1,  1809,   710,   711,    -1,
     713,  1230,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   722,
      -1,    -1,   725,   726,   727,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1272,    -1,
      -1,    -1,  1855,    -1,   134,    -1,    -1,    -1,    -1,   489,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   501,   502,    -1,     1,   155,   156,    -1,   781,  1303,
      -1,    -1,   162,    -1,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,   797,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   805,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     813,    -1,    -1,    -1,   817,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   825,   826,    -1,    -1,    -1,   830,    -1,   106,
      -1,    -1,    59,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   845,    -1,   847,   122,    -1,   124,   851,   852,
     853,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,   871,  1972,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,   156,
      -1,   134,   159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   901,    -1,
      -1,    -1,   155,   156,    -1,    -1,   159,    -1,    -1,   639,
    2013,    -1,   139,   166,   167,  1439,    -1,    -1,   648,    -1,
      -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,   932,
      -1,    -1,    -1,   936,  1458,    -1,    -1,    -1,   941,  2042,
    2043,   168,    -1,    -1,    -1,   948,   949,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   957,    -1,   959,   960,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2073,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     207,    -1,    -1,    -1,   987,    -1,    -1,  1506,  1507,    -1,
      -1,    -1,  1516,    -1,    -1,    -1,    -1,    -1,    -1,  2102,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,  1018,  1019,    20,   245,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,   273,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
     287,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,   309,    -1,    -1,    -1,   313,  1090,    -1,    -1,
      -1,  1094,    -1,    -1,    -1,    -1,  1099,  1100,    -1,    -1,
    1103,    -1,    -1,   106,    -1,   108,   109,    -1,    -1,    -1,
    1113,    -1,    -1,    -1,   844,    -1,    -1,  1120,    -1,    -1,
      -1,    -1,    -1,    -1,   854,    -1,    -1,   354,    -1,    -1,
     357,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   370,    -1,    -1,    -1,   374,    -1,    -1,
      -1,    -1,  1155,    -1,    -1,    -1,    -1,    -1,    -1,   162,
      -1,    -1,  1686,    -1,  1167,    -1,    -1,    -1,  1171,    -1,
      -1,  1690,  1175,    -1,    -1,    -1,   106,  1696,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,    -1,    -1,  1197,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1206,   134,   935,    -1,    -1,    -1,    -1,
      -1,    -1,  1215,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1740,    -1,   106,    -1,   155,   156,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   166,   167,  1241,   122,
      -1,   124,    -1,    -1,   106,  1248,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,    -1,   486,
      -1,   991,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1272,
    1273,    -1,   134,   156,    -1,    -1,   159,  1280,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1292,
      -1,    -1,    -1,   155,   156,    -1,  1299,    -1,    -1,    -1,
    1303,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1838,    -1,  1835,  1836,    -1,    -1,
      -1,    -1,    -1,  1842,  1327,  1328,    -1,    -1,    -1,    -1,
    1333,    -1,   559,    -1,    -1,  1854,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1864,    -1,  1866,    -1,    -1,
     577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1877,    -1,
    1879,  1880,  1881,    -1,    -1,   592,    -1,    -1,    -1,    -1,
     106,    -1,    -1,     1,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,   122,    -1,   124,    -1,
      -1,    -1,    -1,    -1,  1397,    -1,    -1,  1400,  1401,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   634,   635,    -1,
     637,    -1,    -1,    -1,    -1,  1418,    -1,    -1,    -1,  1422,
     156,  1940,    -1,  1426,    -1,  1944,   653,   654,    -1,    -1,
    1949,    59,    -1,    -1,    -1,    -1,  1439,   664,    -1,    -1,
      -1,    -1,    -1,  1446,  1447,  1448,  1449,  1450,  1451,  1452,
     677,    -1,    -1,    -1,    -1,  1458,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   690,    -1,    -1,    -1,    -1,  1471,    -1,
      -1,    -1,  1475,  1476,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2004,  1489,    -1,    -1,    -1,
      -1,  1221,  1222,  1223,    -1,    -1,    -1,  2016,    -1,    -1,
      -1,  2020,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   139,    -1,  1516,    -1,  2034,    -1,    -1,    -1,    -1,
      -1,   149,  2046,    -1,  1254,    -1,    -1,    -1,    -1,    -1,
      -1,  1534,    -1,    -1,    -1,    -1,  1539,    -1,    -1,    -1,
     168,    -1,    -1,    -1,    -1,    -1,  1276,    -1,    -1,  1552,
      -1,    -1,    -1,  1283,   781,    -1,  1559,  2076,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     797,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,   813,    -1,  1591,    -1,
     817,    -1,    -1,    -1,    -1,  2114,    -1,  1600,   825,   826,
    2119,  1604,    -1,   830,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   245,   845,    -1,
     847,    -1,    -1,    -1,   851,   852,   853,    -1,  2147,    -1,
      -1,  2150,    -1,  2152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   871,   273,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1656,  2173,    -1,   284,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,   309,    -1,  1686,  1687,   313,    -1,    -1,    -1,    -1,
       1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   932,    -1,    -1,    -1,   936,
      -1,    -1,    -1,    -1,   941,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   354,    -1,    -1,   357,
     957,  1461,  1462,   960,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   370,  1746,    -1,    -1,   374,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1774,    -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1019,    -1,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1553,    -1,    -1,    -1,    -1,   139,    -1,
      -1,    -1,   143,    -1,    -1,  1838,    -1,    -1,   149,    -1,
      -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1855,    -1,    -1,    -1,    -1,    -1,   486,    -1,
      -1,    -1,    -1,  1090,    -1,    -1,    -1,  1094,    -1,    -1,
      -1,    -1,  1099,  1100,    -1,    -1,  1103,    -1,    -1,    -1,
      -1,    -1,   193,    -1,    -1,    -1,  1113,    -1,    -1,    -1,
      -1,    79,    -1,  1120,    -1,   206,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1155,    -1,
      -1,   559,    -1,    -1,   245,    -1,    -1,    -1,    -1,    -1,
    1167,  1671,    -1,    -1,  1171,    -1,    -1,    -1,  1175,   577,
      -1,   262,    -1,   264,    -1,    -1,    -1,    -1,   269,    -1,
      -1,    -1,   273,    -1,   592,    -1,    -1,    -1,    -1,  1972,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   307,    -1,   309,    -1,
      -1,    -1,   313,    -1,    -1,    -1,   634,   635,    -1,   637,
    2013,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     208,  1248,   333,    -1,    -1,   653,   654,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   664,    -1,    -1,  2042,
    2043,    -1,    -1,  2046,    -1,    -1,  1273,    -1,    -1,   677,
      -1,    -1,    -1,  1280,    -1,    -1,    -1,    -1,  2061,    -1,
      -1,    -1,   690,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2073,    -1,  1299,    -1,    -1,   263,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1327,  1328,    -1,    -1,    -1,    -1,    -1,    -1,   419,    -1,
      -1,    -1,    -1,    -1,    -1,   303,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   315,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   453,    -1,   332,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   781,    -1,    -1,    -1,  1887,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   353,    -1,    -1,    -1,   797,
    1397,   482,    -1,  1400,  1401,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   813,    -1,    -1,    -1,   817,
      -1,  1418,   503,    -1,    -1,  1422,    -1,   825,   826,  1426,
      -1,    -1,   830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,   847,
      -1,    -1,    -1,   851,   852,   853,    -1,    -1,    -1,    -1,
      -1,    -1,   420,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   871,  1471,    -1,    -1,     4,  1475,  1476,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   570,
     571,    -1,  1489,   451,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   592,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,   479,    -1,     4,    -1,    -1,   484,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   932,    -1,    -1,  1534,   936,    -1,
      -1,    -1,  1539,   941,    -1,    -1,   504,    -1,    -1,    -1,
     508,   509,    -1,    -1,   512,    -1,   637,    -1,    85,   957,
      -1,    -1,   960,    -1,    -1,    -1,    -1,    -1,    -1,   527,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   550,    -1,  1591,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1600,    85,    -1,    -1,  1604,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,   699,    -1,
      -1,  1019,    -1,    -1,   105,   152,   707,    -1,    -1,   710,
     711,    -1,   713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   722,    -1,    -1,   725,   726,   727,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   139,    -1,
      -1,    -1,   143,    -1,    -1,    -1,   193,    -1,   149,   627,
      -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   206,
      -1,    -1,    -1,   641,    -1,    -1,    -1,    -1,    -1,    -1,
    1687,    -1,  1090,    -1,    -1,    -1,  1094,    -1,    -1,    -1,
      -1,  1099,  1100,    -1,    -1,  1103,    -1,   665,    -1,    -1,
      -1,    -1,   193,    -1,    -1,  1113,    -1,    -1,    -1,    -1,
      -1,    -1,  1120,    -1,   805,   206,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   262,    -1,   264,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1155,    -1,    -1,
      -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,  1167,
     728,    -1,    -1,  1171,    -1,    -1,    -1,  1175,    -1,    -1,
     307,   262,    -1,   264,    -1,    -1,    -1,    -1,   269,    -1,
      -1,    -1,   273,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,   333,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   307,    -1,   309,    -1,
      -1,    -1,   313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1248,    -1,   333,    -1,    -1,   936,     1,    -1,  1855,     4,
      -1,    -1,   820,    -1,   822,    -1,    -1,   948,   949,    -1,
     828,    -1,    -1,    -1,    -1,  1273,    -1,    -1,    -1,    -1,
      -1,    -1,  1280,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   419,    -1,    -1,    -1,    -1,   855,    -1,    -1,
      -1,  1299,    -1,    -1,    -1,    -1,   864,    -1,    -1,    -1,
      -1,    -1,   870,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   453,    -1,    -1,  1327,
    1328,    -1,    -1,    -1,    -1,    -1,    -1,  1018,   419,    -1,
      85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   482,    -1,   915,    -1,    -1,
     105,    -1,   920,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   453,    -1,    -1,  1972,   503,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   139,    -1,    -1,    -1,   143,  1397,
      -1,   482,  1400,  1401,   149,    -1,    -1,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2013,    -1,    -1,    -1,
    1418,    -1,   503,    -1,  1422,    -1,    -1,    -1,  1426,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   570,   571,  2042,  2043,    -1,   193,  1007,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      -1,   206,    -1,    -1,  2061,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1471,    -1,    -1,  2073,  1475,  1476,    -1,
      -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,   570,
     571,  1489,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     245,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   592,    -1,    -1,    -1,    -1,  1197,   262,    -1,   264,
      -1,    -1,    -1,    -1,   269,  1206,    -1,    -1,   273,    -1,
      -1,   123,    -1,    -1,  1215,    -1,  1534,    -1,    -1,   284,
      -1,  1539,    -1,    -1,   136,    -1,   138,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   637,    -1,    -1,    -1,
    1241,    -1,   307,    -1,   309,    -1,    -1,  1248,   313,    -1,
      -1,    -1,   699,    -1,    -1,    -1,    -1,   169,    -1,   171,
     707,    -1,    -1,   710,   711,    -1,   713,    -1,   333,    -1,
      -1,    -1,  1273,  1591,    -1,   722,    -1,    -1,   725,   726,
     727,    -1,  1600,    -1,   196,    -1,  1604,    -1,    -1,    -1,
      -1,  1292,    -1,    -1,    -1,    -1,    -1,    -1,   699,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   707,    -1,    -1,   710,
     711,    -1,   713,    -1,    -1,  1193,    -1,    -1,    -1,    -1,
      -1,   722,    -1,   235,   725,   726,   727,   239,    -1,    -1,
     242,   243,  1333,    -1,   246,    -1,    -1,   249,   250,    -1,
     252,    -1,   254,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   419,    -1,    -1,    -1,   805,    -1,
      -1,    -1,    -1,    -1,  1242,    -1,    -1,    -1,    -1,  1687,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   453,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   324,    -1,    -1,   327,    -1,   482,    -1,  1297,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   503,   351,
      -1,    -1,    -1,  1321,    -1,  1446,  1447,  1448,  1449,  1450,
    1451,  1452,    -1,    -1,   366,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1471,    -1,    -1,    -1,  1475,  1476,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1489,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   948,   949,    -1,    -1,   570,   571,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   936,    -1,  1855,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   948,   949,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1559,    -1,
      -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1018,   637,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,  1018,    -1,   531,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,   699,   547,    -1,    -1,    -1,    -1,
      -1,    72,   707,    -1,    -1,   710,   711,  1525,   713,    -1,
      -1,    -1,    -1,    -1,  1972,  1656,    -1,   722,    -1,    -1,
     725,   726,   727,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1687,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2013,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   631,
      -1,    -1,    -1,    -1,  2042,  2043,   157,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
     805,    -1,    -1,  2061,    -1,  1746,    -1,    -1,    -1,    -1,
    1197,    -1,    -1,    -1,    -1,  2073,    -1,    -1,    -1,  1206,
     672,   673,    -1,    -1,    -1,  1643,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1774,    -1,    -1,    -1,    -1,    -1,    -1,
     692,    -1,   694,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1241,    -1,  1197,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1206,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1215,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1241,    -1,    -1,    -1,    -1,  1292,    -1,  1248,    -1,    -1,
      -1,    -1,    -1,    -1,  1855,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1747,
    1748,   936,  1273,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   948,   949,    -1,  1333,    -1,    -1,   801,
     802,  1292,    -1,    -1,    -1,    -1,   808,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   833,    -1,    -1,   836,   837,    -1,   839,    -1,   841,
     842,    -1,  1333,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1018,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   884,    -1,    -1,    -1,   888,    -1,    -1,    -1,
     892,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1870,    -1,    -1,    -1,    -1,    -1,    -1,  1446,
    1447,  1448,  1449,  1450,  1451,  1452,    -1,    -1,    -1,    -1,
      -1,    -1,  2013,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1901,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2042,    -1,    -1,    -1,  1446,  1447,  1448,  1449,  1450,
    1451,  1452,    -1,    -1,    -1,    -1,   968,    -1,    -1,    -1,
    1938,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1471,    -1,  2073,    -1,  1475,  1476,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1489,  1967,
      -1,    -1,    -1,  1971,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1559,    -1,    -1,    -1,    -1,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1197,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1206,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1215,    -1,    -1,    -1,    -1,   206,   207,    -1,  1559,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1241,    -1,    -1,    -1,
      -1,    -1,    -1,  1248,    -1,    -1,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,    -1,   246,    -1,  1109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1273,  1656,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1292,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1153,    -1,    -1,    -1,    -1,  1158,    -1,    -1,  1161,
      -1,    -1,    -1,  1165,    -1,  1656,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1333,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   327,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1687,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1746,
      -1,    -1,    -1,   354,   355,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   374,    -1,    -1,    -1,  1774,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,  1746,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,  1774,    -1,    -1,    -1,    51,    -1,    53,
      -1,  1446,  1447,  1448,  1449,  1450,  1451,  1452,    -1,    -1,
      -1,    -1,    -1,  1305,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,   456,  1471,    -1,    -1,    -1,
    1475,  1476,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   473,   474,  1489,   476,   477,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   486,    -1,    -1,    -1,   490,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   503,    85,  1855,    -1,    -1,    -1,    -1,    -1,
    1372,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,  1383,    -1,    -1,  1386,    -1,  1388,  1389,    -1,    -1,
      -1,   532,    -1,    -1,    -1,   536,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1559,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,  1434,    -1,   156,    -1,   577,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   207,    -1,    -1,    -1,   211,
      -1,   632,    -1,    -1,   635,    -1,    -1,    -1,    -1,    -1,
      -1,  1656,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   653,   654,    -1,  1517,    -1,    -1,    -1,    -1,
      -1,    -1,  2013,   664,    -1,    -1,    -1,   668,    -1,    -1,
      -1,    -1,  1687,    -1,   675,    -1,   677,    -1,    -1,    -1,
      -1,    -1,   264,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2042,    -1,    -1,    -1,    -1,   278,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2073,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1746,    -1,    -1,    -1,    -1,    -1,  1599,    -1,    -1,
      -1,   323,    -1,   419,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   333,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1774,
      -1,    -1,    -1,    -1,    -1,  1627,    -1,    -1,    -1,    -1,
      -1,    -1,   354,    -1,   356,    -1,    -1,    -1,    -1,    -1,
     781,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   797,   798,  1660,    -1,
      -1,    -1,    -1,    -1,    -1,  1667,    -1,   808,   809,    -1,
     811,   812,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   825,   826,    -1,    -1,    -1,   830,
      -1,   832,   833,    -1,    48,    -1,    -1,   419,   839,    -1,
    1855,    -1,    -1,    -1,   845,    -1,   847,    -1,    -1,    -1,
     851,   852,   853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     871,   453,   873,    -1,    -1,    -1,   877,    -1,    -1,    -1,
    1742,    -1,    -1,   884,   885,    -1,    -1,   888,   889,    -1,
      -1,   892,   893,    -1,   570,   571,    -1,    -1,    -1,   900,
      -1,    -1,    -1,    -1,   486,    -1,    -1,    -1,    -1,   123,
     492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   136,    -1,   138,    -1,    -1,  1789,  1790,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     941,   942,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1815,  1816,   169,    -1,    -1,    -1,    -1,
      -1,  1823,    -1,    -1,    -1,    -1,    -1,  1829,    -1,   970,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   570,   571,
      -1,    -1,    -1,    -1,    -1,   577,    -1,    -1,  2013,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1018,  1019,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2042,   242,   243,
      -1,   707,   246,    -1,    -1,   249,   250,   713,   252,    -1,
     254,    -1,    -1,    -1,    -1,    -1,   722,    -1,    -1,    -1,
      -1,    -1,    -1,   635,    -1,    -1,    -1,    -1,  2073,    -1,
      -1,    -1,    -1,    -1,  1926,   741,    -1,    -1,    -1,    -1,
      -1,    -1,   654,    -1,   656,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1084,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   677,    -1,    -1,  1099,  1100,
      -1,   777,  1103,  1104,    -1,    -1,    -1,    -1,    -1,  1110,
      -1,    -1,    -1,    -1,    -1,    49,    -1,   699,    52,    -1,
      54,    -1,    56,    57,    -1,   707,    -1,    -1,   710,   711,
      -1,   713,    -1,    -1,    -1,    -1,  1998,   351,    -1,    73,
     722,    -1,    -1,   725,   726,   727,    -1,    -1,    -1,    -1,
      -1,    -1,   366,    -1,  1155,    -1,    -1,  1158,  1159,    -1,
    1161,  1162,    -1,    -1,  1165,  1166,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,   781,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,   151,    -1,    -1,
      -1,   155,   156,   805,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,  2101,
      -1,    -1,    -1,    -1,   826,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   473,
      -1,    -1,    -1,   845,    -1,   847,    -1,    -1,    -1,   851,
     852,   853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1280,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   871,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1299,    -1,
      -1,    -1,    -1,    -1,  1305,  1306,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1328,    -1,    -1,
      -1,    -1,    -1,   547,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   941,
     193,    -1,    -1,    -1,    -1,    -1,    -1,   949,    -1,    -1,
      -1,  1372,  1373,    -1,   207,    -1,    -1,   959,    -1,    -1,
      -1,    -1,  1383,  1384,    -1,  1386,    -1,    -1,   221,    -1,
     223,    -1,    -1,    -1,    -1,    -1,  1397,    -1,    -1,  1400,
    1401,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,  1019,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,   672,   673,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,   692,    -1,
     694,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   322,
      -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1099,  1100,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1534,    -1,    -1,    -1,    -1,   152,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,   163,    -1,   168,   166,   167,    -1,    -1,    -1,
    1561,    -1,    -1,    -1,    -1,  1241,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1155,    -1,    -1,    -1,    -1,    -1,   193,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   801,   802,    -1,
      -1,    -1,    -1,   207,   808,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   833,
      -1,    -1,   836,   837,  1206,   839,    -1,   841,   842,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1650,
     264,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1241,
      -1,    -1,    -1,    -1,    -1,    -1,  1667,    -1,    -1,    -1,
     884,    -1,    -1,    -1,   888,    -1,    -1,    -1,   892,    -1,
     513,    -1,    -1,    -1,    -1,    -1,   519,    -1,    -1,    -1,
    1272,   524,    -1,    -1,    -1,    -1,    -1,    -1,  1280,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1292,    -1,    -1,    -1,    -1,    -1,    -1,  1299,    -1,   333,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     354,    -1,    -1,    -1,    -1,    -1,  1328,    -1,    -1,    -1,
      -1,  1333,    -1,    -1,   968,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   625,    -1,    -1,  1471,  1472,    -1,    -1,  1475,
    1476,    -1,    -1,    -1,    -1,  1481,    -1,  1808,  1809,  1485,
      -1,  1487,    -1,  1489,    -1,  1397,    -1,    -1,  1400,  1401,
      -1,   654,    -1,  1824,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   667,    -1,    -1,    -1,    -1,   453,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1446,  1447,  1448,    -1,    -1,  1451,
    1452,   704,   486,    -1,    -1,    -1,  1458,    -1,    -1,    -1,
      -1,    -1,   715,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   739,   740,    -1,    -1,
     743,    -1,   745,    -1,    -1,    -1,    -1,    -1,   751,    -1,
     753,   754,    -1,  1924,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1932,    -1,    -1,  1516,    -1,    -1,    -1,    -1,  1153,
      -1,    -1,    -1,    -1,  1158,    -1,    -1,  1161,   781,    -1,
      -1,  1165,  1534,    -1,    -1,    -1,   570,   571,    -1,    -1,
    1636,   794,    -1,   577,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   805,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   821,    -1,
      -1,    -1,    -1,   826,    -1,    -1,    -1,  1998,  1999,    -1,
      -1,  2002,    -1,    -1,    -1,    -1,  1682,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   635,    -1,    -1,   857,  1701,  1702,   860,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   872,
     654,    -1,    -1,    -1,  2045,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1731,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   677,    -1,    -1,    -1,    -1,   901,    -1,
      -1,    -1,    -1,    -1,  1656,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   699,    -1,    -1,    -1,    -1,
      -1,  1305,    -1,   707,    -1,    -1,    -1,   711,    -1,   713,
    2101,  2102,    -1,    -1,  1686,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   949,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   959,   960,    -1,    -1,
      -1,    -1,    -1,    -1,   967,    -1,    -1,    -1,  2139,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1372,    -1,
      -1,    -1,    -1,    -1,  1840,    -1,    -1,   781,    -1,  1383,
      -1,  1847,  1386,  1849,  1388,  1389,  1852,  1853,    -1,  1855,
      -1,    -1,    -1,    -1,  1860,    -1,  1019,    -1,    -1,    -1,
      -1,   805,  1774,    -1,  1027,    -1,    -1,    -1,    -1,    -1,
      -1,  1034,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   826,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1434,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   845,    -1,   847,    -1,    -1,    -1,   851,   852,   853,
      -1,    -1,    -1,    -1,    -1,  1078,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1838,   871,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1957,    -1,    -1,    -1,    -1,  1962,  1963,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1517,    -1,    -1,  1982,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1150,    -1,  1152,
      -1,  1154,    -1,    -1,    -1,    -1,    -1,   941,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2021,    -1,  2023,    -1,    -1,
      -1,  2027,  2028,    -1,    -1,    -1,  2032,  2033,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1599,    -1,    -1,    -1,    -1,
    1972,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1235,  1236,    -1,  1019,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1627,    -1,    -1,    -1,    -1,  2094,  2095,
    2096,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2125,
    2126,  2127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2043,    -1,    -1,  2046,    -1,    -1,    -1,    -1,    -1,
    1303,    -1,    -1,    -1,    -1,    -1,  1309,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1099,  1100,    -1,    -1,    -1,
      -1,  1324,    -1,    -1,    -1,  1328,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1346,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1742,    -1,
    1363,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1789,  1790,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1815,  1816,    -1,    -1,  1438,  1439,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1829,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1464,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1483,    -1,    -1,  1486,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1280,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1292,    -1,
      -1,    -1,    -1,    -1,    -1,  1299,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1534,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1543,  1544,  1926,    -1,  1328,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1572,
      -1,  1574,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1397,  1998,    -1,  1400,  1401,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1656,    -1,    -1,    -1,    71,  1661,    73,
      74,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,    -1,   101,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,  1722,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,   155,    -1,    -1,   158,   159,    18,    -1,    -1,   163,
    1534,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,  1765,    -1,    -1,    -1,   180,    -1,    -1,  1772,
      -1,    -1,  1775,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1801,    71,
      -1,    73,    74,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,    -1,   101,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1656,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,   163,    18,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,
      76,    -1,    -1,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,    -1,   101,  1979,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   180,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    71,    72,    73,    74,    -1,    76,    -1,
      -1,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    71,    72,    73,    74,    -1,    76,    -1,    -1,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
      -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     180,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,   151,
      -1,    -1,    -1,   155,   156,   157,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
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
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   180,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   180,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,   157,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
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
      -1,   165,   166,   167,   168,   169,   170,   171,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    77,    78,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,     5,
      72,    -1,    -1,    -1,    -1,    77,    78,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       5,    -1,   154,    -1,    -1,    -1,   158,   159,    13,    14,
      15,    16,    17,    -1,   166,   167,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    49,    -1,    -1,    52,   134,    54,
     136,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,     3,     4,     5,
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
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    13,    14,    15,    16,    17,   166,   167,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,   134,    -1,   136,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
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
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    71,    -1,    73,    74,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    -1,    -1,    93,    94,    95,
      96,    97,    98,    99,    -1,   101,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,
     166,   167,   168,   169,   170,   171,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      73,    74,    -1,    76,    -1,    -1,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    -1,    -1,
      93,    94,    95,    96,    97,    98,    99,    -1,   101,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
     163,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,   163,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,   163,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,   108,   109,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    90,    -1,
      92,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,
     163,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,   163,    -1,   165,   166,   167,   168,   169,   170,   171,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
     163,    -1,   165,   166,   167,   168,   169,   170,   171,    13,
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
      -1,   155,    -1,   157,   158,   159,    -1,    -1,    -1,    -1,
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
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,
     165,   166,   167,   168,   169,   170,   171,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,
     166,   167,   168,   169,   170,   171,    13,    14,    15,    16,
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
     167,   168,   169,   170,   171,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    13,    14,    15,    16,    17,    18,
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
     169,   170,   171,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    72,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   108,   109,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
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
     166,   167,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
     157,   158,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   136,   137,    -1,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,   158,    22,    23,    24,    25,    26,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,   108,   109,    -1,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      13,    14,    15,    16,    17,    18,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   108,   109,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   159,    -1,   108,   109,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
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
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,   159,    -1,   108,   109,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    18,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   108,   109,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
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
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,   159,    -1,   108,   109,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,
      -1,   108,   109,    -1,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    13,
      14,    15,    16,    17,   166,   167,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,   159,    -1,    13,    14,    15,
      16,    17,   166,   167,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    13,    14,    15,    16,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   158,    22,    23,    24,    25,    26,    27,    28,    29,
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
      -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,   158,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    72,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   106,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,   136,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,   136,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,    72,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    49,    55,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,    49,
     136,    -1,    52,   134,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
     151,    -1,    -1,    73,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,
      -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,   163,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
      -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,   162,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,   163,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,   155,    -1,   157,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    57,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171
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
     161,   493,   385,   158,   159,   162,   389,   156,   220,   226,
     155,   182,   179,   432,   434,   435,   436,   445,   447,   448,
     449,   451,   452,   453,   157,   157,   157,   157,   157,   157,
     157,   157,   157,   157,   433,   446,   450,   426,   155,   179,
     160,   182,   384,   231,   421,   371,   384,   231,   423,   227,
     383,   227,   383,   423,   108,   159,   412,   231,   421,   425,
     163,   163,   421,   289,   412,   231,   421,   344,   345,   343,
     163,   157,   161,   157,   161,    70,   291,   292,   180,   166,
     220,   182,   432,   375,   414,   412,   381,   160,   182,   155,
     394,   392,   393,    78,   325,   186,   312,   470,   484,   314,
     318,   491,   371,   473,   474,   475,   160,   182,    18,   220,
     314,   472,   494,   426,   426,   470,   312,   482,   492,   314,
     186,   312,   484,   426,   163,   426,   366,    10,   165,   366,
     368,   369,   163,   157,   383,   157,   157,   430,   156,   194,
     195,   196,   220,   180,   382,   492,   190,   159,   382,   383,
     382,   492,   220,   382,   157,   382,   382,   382,   160,   182,
     157,   168,   169,   206,    18,   316,   157,   161,   157,   166,
     167,   157,   226,   220,   163,   220,   186,   220,   186,   118,
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
     387,   154,   182,   157,   161,   182,   373,   220,   157,   157,
     157,   157,   157,   157,   157,   157,   157,   155,   426,   467,
     470,   155,   426,   467,   470,   155,   426,   467,   470,   423,
     188,    22,   470,   220,   321,   337,   468,   231,   157,   157,
     157,   157,   157,   410,   411,   231,   154,   182,   412,   231,
     421,   411,   231,   163,   163,   163,   351,   137,   376,   377,
     186,   293,   381,    18,    71,    73,    74,    76,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      93,    94,    95,    96,    97,    98,    99,   101,   108,   109,
     121,   155,   159,   227,   228,   229,   230,   231,   232,   233,
     235,   236,   245,   252,   253,   254,   255,   256,   257,   262,
     263,   266,   267,   268,   269,   270,   271,   272,   278,   279,
     280,   294,   314,   318,   381,   422,    70,   183,   183,   373,
     161,   413,   411,   157,   298,   300,   311,   405,   406,   407,
     408,   400,   179,   391,   391,   312,   484,   159,   166,   202,
     220,   337,   220,   314,   157,   157,   157,   157,     5,   314,
     426,   472,   364,   368,   366,   163,   337,   161,   493,   381,
     368,   163,   157,   157,   161,   157,   157,   161,   182,   161,
     157,   157,   157,   161,   157,   204,   157,   157,   157,   204,
      18,   316,   220,   157,   157,   156,   163,   204,   160,   183,
     194,   160,   160,   118,   122,   124,   187,   197,   198,   199,
     157,   197,   160,   161,   154,   218,   162,   157,   197,   183,
     386,   157,   157,   157,   157,   463,   371,   371,   157,   157,
     373,   373,   460,   157,   157,   157,   157,   155,   432,   459,
     454,   458,   371,   371,   160,   183,   494,   161,   183,   157,
     161,   161,   183,   183,   384,   197,   137,   171,   183,   183,
     154,   385,   220,   426,   156,   220,   373,   183,   155,   426,
     467,   470,   155,   426,   467,   470,   155,   426,   467,   470,
     371,   371,   371,   425,   157,   149,   171,   183,   469,   161,
     183,   413,   405,   411,   231,   413,   351,   351,   351,     3,
       5,    10,    73,   154,   295,   302,   303,   311,   314,   352,
     358,   487,   161,   180,   155,    61,    62,   180,   231,   294,
     422,   155,   155,    18,   229,   155,   155,   180,   381,   180,
     381,   166,   381,   163,   228,   155,   155,   155,   229,   155,
     231,   220,   221,   221,    14,   281,   257,   268,   180,   183,
     233,    78,   180,   381,    91,    92,   261,   265,   112,   135,
     260,   111,   134,   264,   260,   380,   314,   162,   293,   160,
     160,   183,   413,   381,   423,   183,   180,   183,   180,   183,
     157,   383,   397,   397,   182,   183,   183,   183,   220,   155,
     426,   476,   470,   313,     5,   166,   183,   220,   366,   493,
     163,   368,    10,   369,   154,   179,   370,   493,   154,   182,
     196,   310,   186,    78,   191,   192,   382,   204,   204,   204,
     204,   204,   163,   386,   161,   154,   200,   159,   198,   200,
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
     226,   227,   246,   113,   114,   115,   116,   117,   273,   275,
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
#line 8519 "Parser/parser.cc"
    break;

  case 3:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8525 "Parser/parser.cc"
    break;

  case 4:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8531 "Parser/parser.cc"
    break;

  case 5:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8537 "Parser/parser.cc"
    break;

  case 6:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8543 "Parser/parser.cc"
    break;

  case 7:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8549 "Parser/parser.cc"
    break;

  case 8:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8555 "Parser/parser.cc"
    break;

  case 20:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8561 "Parser/parser.cc"
    break;

  case 21:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8567 "Parser/parser.cc"
    break;

  case 22:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8573 "Parser/parser.cc"
    break;

  case 23:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8583 "Parser/parser.cc"
    break;

  case 24:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8589 "Parser/parser.cc"
    break;

  case 25:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8595 "Parser/parser.cc"
    break;

  case 26:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8601 "Parser/parser.cc"
    break;

  case 28:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8607 "Parser/parser.cc"
    break;

  case 29:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8613 "Parser/parser.cc"
    break;

  case 30:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8619 "Parser/parser.cc"
    break;

  case 31:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8625 "Parser/parser.cc"
    break;

  case 32:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8635 "Parser/parser.cc"
    break;

  case 33:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8641 "Parser/parser.cc"
    break;

  case 34:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8647 "Parser/parser.cc"
    break;

  case 35:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8653 "Parser/parser.cc"
    break;

  case 36:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8659 "Parser/parser.cc"
    break;

  case 37:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8665 "Parser/parser.cc"
    break;

  case 38:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8671 "Parser/parser.cc"
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
#line 8683 "Parser/parser.cc"
    break;

  case 41:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8692 "Parser/parser.cc"
    break;

  case 42:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8698 "Parser/parser.cc"
    break;

  case 44:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8704 "Parser/parser.cc"
    break;

  case 45:
#line 772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8710 "Parser/parser.cc"
    break;

  case 46:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8716 "Parser/parser.cc"
    break;

  case 47:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8722 "Parser/parser.cc"
    break;

  case 48:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8732 "Parser/parser.cc"
    break;

  case 49:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8738 "Parser/parser.cc"
    break;

  case 50:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8745 "Parser/parser.cc"
    break;

  case 51:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 52:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8757 "Parser/parser.cc"
    break;

  case 53:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8763 "Parser/parser.cc"
    break;

  case 54:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8769 "Parser/parser.cc"
    break;

  case 55:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8775 "Parser/parser.cc"
    break;

  case 56:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8781 "Parser/parser.cc"
    break;

  case 57:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8787 "Parser/parser.cc"
    break;

  case 58:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8793 "Parser/parser.cc"
    break;

  case 59:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8799 "Parser/parser.cc"
    break;

  case 60:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8805 "Parser/parser.cc"
    break;

  case 61:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8811 "Parser/parser.cc"
    break;

  case 62:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8817 "Parser/parser.cc"
    break;

  case 63:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8823 "Parser/parser.cc"
    break;

  case 64:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8829 "Parser/parser.cc"
    break;

  case 65:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8835 "Parser/parser.cc"
    break;

  case 66:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8841 "Parser/parser.cc"
    break;

  case 67:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8847 "Parser/parser.cc"
    break;

  case 68:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8857 "Parser/parser.cc"
    break;

  case 69:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8863 "Parser/parser.cc"
    break;

  case 72:
#line 859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8869 "Parser/parser.cc"
    break;

  case 73:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8875 "Parser/parser.cc"
    break;

  case 76:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8881 "Parser/parser.cc"
    break;

  case 78:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8887 "Parser/parser.cc"
    break;

  case 79:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8893 "Parser/parser.cc"
    break;

  case 80:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8899 "Parser/parser.cc"
    break;

  case 81:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8905 "Parser/parser.cc"
    break;

  case 82:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8911 "Parser/parser.cc"
    break;

  case 83:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8917 "Parser/parser.cc"
    break;

  case 84:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8923 "Parser/parser.cc"
    break;

  case 85:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8929 "Parser/parser.cc"
    break;

  case 86:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8937 "Parser/parser.cc"
    break;

  case 87:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8943 "Parser/parser.cc"
    break;

  case 88:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8952 "Parser/parser.cc"
    break;

  case 91:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8958 "Parser/parser.cc"
    break;

  case 92:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8964 "Parser/parser.cc"
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
#line 8984 "Parser/parser.cc"
    break;

  case 94:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8990 "Parser/parser.cc"
    break;

  case 95:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8996 "Parser/parser.cc"
    break;

  case 96:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 9002 "Parser/parser.cc"
    break;

  case 97:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9008 "Parser/parser.cc"
    break;

  case 98:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9014 "Parser/parser.cc"
    break;

  case 99:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9020 "Parser/parser.cc"
    break;

  case 100:
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9026 "Parser/parser.cc"
    break;

  case 101:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9032 "Parser/parser.cc"
    break;

  case 102:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9038 "Parser/parser.cc"
    break;

  case 103:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9044 "Parser/parser.cc"
    break;

  case 104:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9053 "Parser/parser.cc"
    break;

  case 105:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9059 "Parser/parser.cc"
    break;

  case 106:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9065 "Parser/parser.cc"
    break;

  case 107:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9071 "Parser/parser.cc"
    break;

  case 108:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9077 "Parser/parser.cc"
    break;

  case 109:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9083 "Parser/parser.cc"
    break;

  case 110:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9089 "Parser/parser.cc"
    break;

  case 111:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9095 "Parser/parser.cc"
    break;

  case 113:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9101 "Parser/parser.cc"
    break;

  case 114:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 115:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9113 "Parser/parser.cc"
    break;

  case 116:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9119 "Parser/parser.cc"
    break;

  case 117:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9125 "Parser/parser.cc"
    break;

  case 118:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9131 "Parser/parser.cc"
    break;

  case 119:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9137 "Parser/parser.cc"
    break;

  case 120:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9143 "Parser/parser.cc"
    break;

  case 128:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9149 "Parser/parser.cc"
    break;

  case 130:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9155 "Parser/parser.cc"
    break;

  case 131:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9161 "Parser/parser.cc"
    break;

  case 132:
#line 1031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9167 "Parser/parser.cc"
    break;

  case 134:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9173 "Parser/parser.cc"
    break;

  case 135:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9179 "Parser/parser.cc"
    break;

  case 137:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9185 "Parser/parser.cc"
    break;

  case 138:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9191 "Parser/parser.cc"
    break;

  case 140:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9197 "Parser/parser.cc"
    break;

  case 141:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9203 "Parser/parser.cc"
    break;

  case 142:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9209 "Parser/parser.cc"
    break;

  case 143:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9215 "Parser/parser.cc"
    break;

  case 145:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9221 "Parser/parser.cc"
    break;

  case 146:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9227 "Parser/parser.cc"
    break;

  case 148:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9233 "Parser/parser.cc"
    break;

  case 150:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9239 "Parser/parser.cc"
    break;

  case 152:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9245 "Parser/parser.cc"
    break;

  case 154:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9251 "Parser/parser.cc"
    break;

  case 156:
#line 1097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9257 "Parser/parser.cc"
    break;

  case 158:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9263 "Parser/parser.cc"
    break;

  case 159:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9269 "Parser/parser.cc"
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
#line 9281 "Parser/parser.cc"
    break;

  case 163:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9287 "Parser/parser.cc"
    break;

  case 164:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9293 "Parser/parser.cc"
    break;

  case 168:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9299 "Parser/parser.cc"
    break;

  case 169:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9305 "Parser/parser.cc"
    break;

  case 170:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9311 "Parser/parser.cc"
    break;

  case 171:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9317 "Parser/parser.cc"
    break;

  case 172:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9323 "Parser/parser.cc"
    break;

  case 173:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9329 "Parser/parser.cc"
    break;

  case 174:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9335 "Parser/parser.cc"
    break;

  case 175:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9341 "Parser/parser.cc"
    break;

  case 176:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9347 "Parser/parser.cc"
    break;

  case 177:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9353 "Parser/parser.cc"
    break;

  case 178:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9359 "Parser/parser.cc"
    break;

  case 179:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9365 "Parser/parser.cc"
    break;

  case 180:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9371 "Parser/parser.cc"
    break;

  case 181:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9377 "Parser/parser.cc"
    break;

  case 182:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9383 "Parser/parser.cc"
    break;

  case 184:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9389 "Parser/parser.cc"
    break;

  case 185:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9395 "Parser/parser.cc"
    break;

  case 186:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9401 "Parser/parser.cc"
    break;

  case 188:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9407 "Parser/parser.cc"
    break;

  case 189:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9413 "Parser/parser.cc"
    break;

  case 204:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9419 "Parser/parser.cc"
    break;

  case 206:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9425 "Parser/parser.cc"
    break;

  case 207:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9431 "Parser/parser.cc"
    break;

  case 208:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9442 "Parser/parser.cc"
    break;

  case 209:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9448 "Parser/parser.cc"
    break;

  case 210:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9454 "Parser/parser.cc"
    break;

  case 212:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9460 "Parser/parser.cc"
    break;

  case 213:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9466 "Parser/parser.cc"
    break;

  case 214:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9472 "Parser/parser.cc"
    break;

  case 215:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9478 "Parser/parser.cc"
    break;

  case 216:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9484 "Parser/parser.cc"
    break;

  case 219:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9490 "Parser/parser.cc"
    break;

  case 220:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9497 "Parser/parser.cc"
    break;

  case 221:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9503 "Parser/parser.cc"
    break;

  case 222:
#line 1297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9509 "Parser/parser.cc"
    break;

  case 223:
#line 1299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9515 "Parser/parser.cc"
    break;

  case 224:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9521 "Parser/parser.cc"
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
#line 9535 "Parser/parser.cc"
    break;

  case 226:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9541 "Parser/parser.cc"
    break;

  case 227:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9547 "Parser/parser.cc"
    break;

  case 228:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9556 "Parser/parser.cc"
    break;

  case 229:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9562 "Parser/parser.cc"
    break;

  case 230:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9568 "Parser/parser.cc"
    break;

  case 231:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9574 "Parser/parser.cc"
    break;

  case 232:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9580 "Parser/parser.cc"
    break;

  case 233:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9586 "Parser/parser.cc"
    break;

  case 234:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9592 "Parser/parser.cc"
    break;

  case 235:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9598 "Parser/parser.cc"
    break;

  case 237:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9604 "Parser/parser.cc"
    break;

  case 238:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9610 "Parser/parser.cc"
    break;

  case 239:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9616 "Parser/parser.cc"
    break;

  case 240:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9622 "Parser/parser.cc"
    break;

  case 241:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9628 "Parser/parser.cc"
    break;

  case 242:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9634 "Parser/parser.cc"
    break;

  case 243:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9640 "Parser/parser.cc"
    break;

  case 245:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9646 "Parser/parser.cc"
    break;

  case 246:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9652 "Parser/parser.cc"
    break;

  case 247:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9658 "Parser/parser.cc"
    break;

  case 249:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9664 "Parser/parser.cc"
    break;

  case 250:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9670 "Parser/parser.cc"
    break;

  case 251:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9676 "Parser/parser.cc"
    break;

  case 252:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9685 "Parser/parser.cc"
    break;

  case 253:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 254:
#line 1397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 255:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9703 "Parser/parser.cc"
    break;

  case 256:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9712 "Parser/parser.cc"
    break;

  case 257:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9718 "Parser/parser.cc"
    break;

  case 258:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9724 "Parser/parser.cc"
    break;

  case 259:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9730 "Parser/parser.cc"
    break;

  case 260:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9739 "Parser/parser.cc"
    break;

  case 261:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9745 "Parser/parser.cc"
    break;

  case 262:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9751 "Parser/parser.cc"
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
#line 9770 "Parser/parser.cc"
    break;

  case 265:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9776 "Parser/parser.cc"
    break;

  case 266:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9785 "Parser/parser.cc"
    break;

  case 267:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9791 "Parser/parser.cc"
    break;

  case 268:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9797 "Parser/parser.cc"
    break;

  case 269:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9803 "Parser/parser.cc"
    break;

  case 270:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9809 "Parser/parser.cc"
    break;

  case 271:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9815 "Parser/parser.cc"
    break;

  case 272:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9821 "Parser/parser.cc"
    break;

  case 273:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9830 "Parser/parser.cc"
    break;

  case 274:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9839 "Parser/parser.cc"
    break;

  case 275:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9845 "Parser/parser.cc"
    break;

  case 276:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9854 "Parser/parser.cc"
    break;

  case 277:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9863 "Parser/parser.cc"
    break;

  case 278:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9869 "Parser/parser.cc"
    break;

  case 279:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9875 "Parser/parser.cc"
    break;

  case 280:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9881 "Parser/parser.cc"
    break;

  case 281:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9887 "Parser/parser.cc"
    break;

  case 282:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9893 "Parser/parser.cc"
    break;

  case 283:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9899 "Parser/parser.cc"
    break;

  case 284:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9905 "Parser/parser.cc"
    break;

  case 285:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9911 "Parser/parser.cc"
    break;

  case 286:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9920 "Parser/parser.cc"
    break;

  case 287:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9930 "Parser/parser.cc"
    break;

  case 288:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9936 "Parser/parser.cc"
    break;

  case 289:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9942 "Parser/parser.cc"
    break;

  case 290:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9951 "Parser/parser.cc"
    break;

  case 291:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9961 "Parser/parser.cc"
    break;

  case 292:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9967 "Parser/parser.cc"
    break;

  case 293:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9976 "Parser/parser.cc"
    break;

  case 294:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9986 "Parser/parser.cc"
    break;

  case 295:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9992 "Parser/parser.cc"
    break;

  case 296:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9998 "Parser/parser.cc"
    break;

  case 297:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 10004 "Parser/parser.cc"
    break;

  case 298:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 10010 "Parser/parser.cc"
    break;

  case 299:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10019 "Parser/parser.cc"
    break;

  case 300:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10029 "Parser/parser.cc"
    break;

  case 301:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10035 "Parser/parser.cc"
    break;

  case 302:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10044 "Parser/parser.cc"
    break;

  case 303:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10054 "Parser/parser.cc"
    break;

  case 304:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10060 "Parser/parser.cc"
    break;

  case 305:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10069 "Parser/parser.cc"
    break;

  case 306:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10079 "Parser/parser.cc"
    break;

  case 307:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10085 "Parser/parser.cc"
    break;

  case 308:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 10094 "Parser/parser.cc"
    break;

  case 309:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 10105 "Parser/parser.cc"
    break;

  case 312:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10111 "Parser/parser.cc"
    break;

  case 313:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10117 "Parser/parser.cc"
    break;

  case 314:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10123 "Parser/parser.cc"
    break;

  case 315:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10129 "Parser/parser.cc"
    break;

  case 316:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10135 "Parser/parser.cc"
    break;

  case 318:
#line 1637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10141 "Parser/parser.cc"
    break;

  case 319:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10147 "Parser/parser.cc"
    break;

  case 320:
#line 1644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10153 "Parser/parser.cc"
    break;

  case 321:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10159 "Parser/parser.cc"
    break;

  case 322:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10165 "Parser/parser.cc"
    break;

  case 323:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10171 "Parser/parser.cc"
    break;

  case 324:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10177 "Parser/parser.cc"
    break;

  case 325:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10183 "Parser/parser.cc"
    break;

  case 326:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10189 "Parser/parser.cc"
    break;

  case 327:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10195 "Parser/parser.cc"
    break;

  case 328:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10201 "Parser/parser.cc"
    break;

  case 329:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10207 "Parser/parser.cc"
    break;

  case 330:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10213 "Parser/parser.cc"
    break;

  case 331:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10219 "Parser/parser.cc"
    break;

  case 332:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10225 "Parser/parser.cc"
    break;

  case 333:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10231 "Parser/parser.cc"
    break;

  case 334:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10237 "Parser/parser.cc"
    break;

  case 335:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10243 "Parser/parser.cc"
    break;

  case 336:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10249 "Parser/parser.cc"
    break;

  case 337:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10255 "Parser/parser.cc"
    break;

  case 338:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10261 "Parser/parser.cc"
    break;

  case 339:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10267 "Parser/parser.cc"
    break;

  case 342:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10273 "Parser/parser.cc"
    break;

  case 343:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10282 "Parser/parser.cc"
    break;

  case 344:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10288 "Parser/parser.cc"
    break;

  case 345:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10294 "Parser/parser.cc"
    break;

  case 348:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10300 "Parser/parser.cc"
    break;

  case 349:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10306 "Parser/parser.cc"
    break;

  case 352:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10312 "Parser/parser.cc"
    break;

  case 353:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10318 "Parser/parser.cc"
    break;

  case 354:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10324 "Parser/parser.cc"
    break;

  case 355:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10330 "Parser/parser.cc"
    break;

  case 356:
#line 1751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10336 "Parser/parser.cc"
    break;

  case 357:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10342 "Parser/parser.cc"
    break;

  case 358:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10348 "Parser/parser.cc"
    break;

  case 359:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10354 "Parser/parser.cc"
    break;

  case 360:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10360 "Parser/parser.cc"
    break;

  case 363:
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10366 "Parser/parser.cc"
    break;

  case 364:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10372 "Parser/parser.cc"
    break;

  case 365:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10378 "Parser/parser.cc"
    break;

  case 366:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10384 "Parser/parser.cc"
    break;

  case 367:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10390 "Parser/parser.cc"
    break;

  case 368:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10396 "Parser/parser.cc"
    break;

  case 369:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10402 "Parser/parser.cc"
    break;

  case 370:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10408 "Parser/parser.cc"
    break;

  case 371:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10414 "Parser/parser.cc"
    break;

  case 372:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 373:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10426 "Parser/parser.cc"
    break;

  case 374:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10432 "Parser/parser.cc"
    break;

  case 375:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 376:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10444 "Parser/parser.cc"
    break;

  case 377:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 378:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10456 "Parser/parser.cc"
    break;

  case 379:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10462 "Parser/parser.cc"
    break;

  case 380:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10468 "Parser/parser.cc"
    break;

  case 381:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10474 "Parser/parser.cc"
    break;

  case 382:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10480 "Parser/parser.cc"
    break;

  case 383:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10486 "Parser/parser.cc"
    break;

  case 384:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10492 "Parser/parser.cc"
    break;

  case 385:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10498 "Parser/parser.cc"
    break;

  case 387:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10504 "Parser/parser.cc"
    break;

  case 388:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 389:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10516 "Parser/parser.cc"
    break;

  case 394:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10522 "Parser/parser.cc"
    break;

  case 395:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10528 "Parser/parser.cc"
    break;

  case 396:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10534 "Parser/parser.cc"
    break;

  case 397:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10540 "Parser/parser.cc"
    break;

  case 398:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10546 "Parser/parser.cc"
    break;

  case 399:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10552 "Parser/parser.cc"
    break;

  case 400:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10558 "Parser/parser.cc"
    break;

  case 401:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10564 "Parser/parser.cc"
    break;

  case 404:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 405:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 406:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10585 "Parser/parser.cc"
    break;

  case 407:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10591 "Parser/parser.cc"
    break;

  case 408:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10597 "Parser/parser.cc"
    break;

  case 409:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10603 "Parser/parser.cc"
    break;

  case 410:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10612 "Parser/parser.cc"
    break;

  case 411:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10621 "Parser/parser.cc"
    break;

  case 412:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10627 "Parser/parser.cc"
    break;

  case 415:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10633 "Parser/parser.cc"
    break;

  case 416:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10639 "Parser/parser.cc"
    break;

  case 418:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10645 "Parser/parser.cc"
    break;

  case 419:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10651 "Parser/parser.cc"
    break;

  case 429:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10657 "Parser/parser.cc"
    break;

  case 430:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10663 "Parser/parser.cc"
    break;

  case 434:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10669 "Parser/parser.cc"
    break;

  case 436:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10675 "Parser/parser.cc"
    break;

  case 437:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10681 "Parser/parser.cc"
    break;

  case 438:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10687 "Parser/parser.cc"
    break;

  case 439:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10693 "Parser/parser.cc"
    break;

  case 440:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10699 "Parser/parser.cc"
    break;

  case 441:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10705 "Parser/parser.cc"
    break;

  case 443:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10711 "Parser/parser.cc"
    break;

  case 444:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10717 "Parser/parser.cc"
    break;

  case 445:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10723 "Parser/parser.cc"
    break;

  case 446:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10734 "Parser/parser.cc"
    break;

  case 447:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10740 "Parser/parser.cc"
    break;

  case 448:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10746 "Parser/parser.cc"
    break;

  case 449:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10752 "Parser/parser.cc"
    break;

  case 450:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10758 "Parser/parser.cc"
    break;

  case 451:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10764 "Parser/parser.cc"
    break;

  case 452:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10770 "Parser/parser.cc"
    break;

  case 453:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10779 "Parser/parser.cc"
    break;

  case 454:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10788 "Parser/parser.cc"
    break;

  case 455:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10797 "Parser/parser.cc"
    break;

  case 456:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10808 "Parser/parser.cc"
    break;

  case 457:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10817 "Parser/parser.cc"
    break;

  case 458:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10823 "Parser/parser.cc"
    break;

  case 459:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10829 "Parser/parser.cc"
    break;

  case 460:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10835 "Parser/parser.cc"
    break;

  case 461:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10843 "Parser/parser.cc"
    break;

  case 462:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10851 "Parser/parser.cc"
    break;

  case 463:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10857 "Parser/parser.cc"
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
#line 10872 "Parser/parser.cc"
    break;

  case 467:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10878 "Parser/parser.cc"
    break;

  case 468:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10884 "Parser/parser.cc"
    break;

  case 469:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10890 "Parser/parser.cc"
    break;

  case 470:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 471:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 477:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10912 "Parser/parser.cc"
    break;

  case 490:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10918 "Parser/parser.cc"
    break;

  case 493:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10924 "Parser/parser.cc"
    break;

  case 494:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10930 "Parser/parser.cc"
    break;

  case 496:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10936 "Parser/parser.cc"
    break;

  case 497:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10942 "Parser/parser.cc"
    break;

  case 498:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10948 "Parser/parser.cc"
    break;

  case 499:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10954 "Parser/parser.cc"
    break;

  case 500:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10960 "Parser/parser.cc"
    break;

  case 501:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10966 "Parser/parser.cc"
    break;

  case 503:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10972 "Parser/parser.cc"
    break;

  case 504:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10978 "Parser/parser.cc"
    break;

  case 506:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10984 "Parser/parser.cc"
    break;

  case 507:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10990 "Parser/parser.cc"
    break;

  case 508:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10996 "Parser/parser.cc"
    break;

  case 509:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 11002 "Parser/parser.cc"
    break;

  case 510:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11008 "Parser/parser.cc"
    break;

  case 511:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11014 "Parser/parser.cc"
    break;

  case 512:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11020 "Parser/parser.cc"
    break;

  case 513:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11026 "Parser/parser.cc"
    break;

  case 514:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11032 "Parser/parser.cc"
    break;

  case 515:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11038 "Parser/parser.cc"
    break;

  case 516:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11044 "Parser/parser.cc"
    break;

  case 517:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11050 "Parser/parser.cc"
    break;

  case 518:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11056 "Parser/parser.cc"
    break;

  case 519:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11062 "Parser/parser.cc"
    break;

  case 520:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11068 "Parser/parser.cc"
    break;

  case 521:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11074 "Parser/parser.cc"
    break;

  case 522:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11080 "Parser/parser.cc"
    break;

  case 523:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11086 "Parser/parser.cc"
    break;

  case 524:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11092 "Parser/parser.cc"
    break;

  case 525:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11098 "Parser/parser.cc"
    break;

  case 526:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11104 "Parser/parser.cc"
    break;

  case 527:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11110 "Parser/parser.cc"
    break;

  case 528:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11116 "Parser/parser.cc"
    break;

  case 529:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11122 "Parser/parser.cc"
    break;

  case 530:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11128 "Parser/parser.cc"
    break;

  case 531:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11134 "Parser/parser.cc"
    break;

  case 532:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11140 "Parser/parser.cc"
    break;

  case 533:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11146 "Parser/parser.cc"
    break;

  case 534:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11152 "Parser/parser.cc"
    break;

  case 535:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11158 "Parser/parser.cc"
    break;

  case 536:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11164 "Parser/parser.cc"
    break;

  case 537:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11170 "Parser/parser.cc"
    break;

  case 538:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11176 "Parser/parser.cc"
    break;

  case 539:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11182 "Parser/parser.cc"
    break;

  case 540:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11188 "Parser/parser.cc"
    break;

  case 541:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11194 "Parser/parser.cc"
    break;

  case 542:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11200 "Parser/parser.cc"
    break;

  case 543:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11206 "Parser/parser.cc"
    break;

  case 545:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11212 "Parser/parser.cc"
    break;

  case 547:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11218 "Parser/parser.cc"
    break;

  case 548:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11224 "Parser/parser.cc"
    break;

  case 549:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11230 "Parser/parser.cc"
    break;

  case 551:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11236 "Parser/parser.cc"
    break;

  case 552:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11242 "Parser/parser.cc"
    break;

  case 553:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11248 "Parser/parser.cc"
    break;

  case 554:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11254 "Parser/parser.cc"
    break;

  case 556:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11260 "Parser/parser.cc"
    break;

  case 558:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 559:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11272 "Parser/parser.cc"
    break;

  case 560:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11278 "Parser/parser.cc"
    break;

  case 561:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11284 "Parser/parser.cc"
    break;

  case 562:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11290 "Parser/parser.cc"
    break;

  case 563:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11296 "Parser/parser.cc"
    break;

  case 564:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11302 "Parser/parser.cc"
    break;

  case 565:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11308 "Parser/parser.cc"
    break;

  case 566:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11314 "Parser/parser.cc"
    break;

  case 568:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11320 "Parser/parser.cc"
    break;

  case 569:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11326 "Parser/parser.cc"
    break;

  case 570:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11332 "Parser/parser.cc"
    break;

  case 572:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11338 "Parser/parser.cc"
    break;

  case 573:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11344 "Parser/parser.cc"
    break;

  case 574:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11353 "Parser/parser.cc"
    break;

  case 576:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 577:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 578:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11371 "Parser/parser.cc"
    break;

  case 580:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11377 "Parser/parser.cc"
    break;

  case 581:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11383 "Parser/parser.cc"
    break;

  case 583:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11389 "Parser/parser.cc"
    break;

  case 584:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11395 "Parser/parser.cc"
    break;

  case 585:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 586:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11407 "Parser/parser.cc"
    break;

  case 587:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 588:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11419 "Parser/parser.cc"
    break;

  case 589:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11425 "Parser/parser.cc"
    break;

  case 590:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11431 "Parser/parser.cc"
    break;

  case 591:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 593:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 594:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11449 "Parser/parser.cc"
    break;

  case 595:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11455 "Parser/parser.cc"
    break;

  case 596:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11461 "Parser/parser.cc"
    break;

  case 597:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11467 "Parser/parser.cc"
    break;

  case 602:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11473 "Parser/parser.cc"
    break;

  case 603:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11479 "Parser/parser.cc"
    break;

  case 604:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11488 "Parser/parser.cc"
    break;

  case 605:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11496 "Parser/parser.cc"
    break;

  case 606:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11505 "Parser/parser.cc"
    break;

  case 607:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11514 "Parser/parser.cc"
    break;

  case 608:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11523 "Parser/parser.cc"
    break;

  case 609:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11532 "Parser/parser.cc"
    break;

  case 611:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11538 "Parser/parser.cc"
    break;

  case 612:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11544 "Parser/parser.cc"
    break;

  case 613:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11554 "Parser/parser.cc"
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
#line 11573 "Parser/parser.cc"
    break;

  case 617:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11579 "Parser/parser.cc"
    break;

  case 618:
#line 2601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11585 "Parser/parser.cc"
    break;

  case 619:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11591 "Parser/parser.cc"
    break;

  case 620:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11597 "Parser/parser.cc"
    break;

  case 621:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11603 "Parser/parser.cc"
    break;

  case 622:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11609 "Parser/parser.cc"
    break;

  case 623:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11618 "Parser/parser.cc"
    break;

  case 624:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11624 "Parser/parser.cc"
    break;

  case 625:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11633 "Parser/parser.cc"
    break;

  case 626:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11639 "Parser/parser.cc"
    break;

  case 627:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11648 "Parser/parser.cc"
    break;

  case 628:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11654 "Parser/parser.cc"
    break;

  case 629:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11660 "Parser/parser.cc"
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
#line 11673 "Parser/parser.cc"
    break;

  case 631:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11682 "Parser/parser.cc"
    break;

  case 632:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11688 "Parser/parser.cc"
    break;

  case 633:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11694 "Parser/parser.cc"
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
#line 11707 "Parser/parser.cc"
    break;

  case 635:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11713 "Parser/parser.cc"
    break;

  case 638:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11719 "Parser/parser.cc"
    break;

  case 639:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11725 "Parser/parser.cc"
    break;

  case 642:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11731 "Parser/parser.cc"
    break;

  case 645:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11737 "Parser/parser.cc"
    break;

  case 646:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11743 "Parser/parser.cc"
    break;

  case 647:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11749 "Parser/parser.cc"
    break;

  case 648:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11755 "Parser/parser.cc"
    break;

  case 649:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11761 "Parser/parser.cc"
    break;

  case 650:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11767 "Parser/parser.cc"
    break;

  case 652:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11773 "Parser/parser.cc"
    break;

  case 654:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11779 "Parser/parser.cc"
    break;

  case 655:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11785 "Parser/parser.cc"
    break;

  case 657:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11791 "Parser/parser.cc"
    break;

  case 658:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11797 "Parser/parser.cc"
    break;

  case 660:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11803 "Parser/parser.cc"
    break;

  case 661:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11814 "Parser/parser.cc"
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
#line 11828 "Parser/parser.cc"
    break;

  case 663:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11834 "Parser/parser.cc"
    break;

  case 664:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11840 "Parser/parser.cc"
    break;

  case 665:
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11846 "Parser/parser.cc"
    break;

  case 666:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11857 "Parser/parser.cc"
    break;

  case 667:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11863 "Parser/parser.cc"
    break;

  case 668:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11869 "Parser/parser.cc"
    break;

  case 670:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11875 "Parser/parser.cc"
    break;

  case 671:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11881 "Parser/parser.cc"
    break;

  case 672:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11887 "Parser/parser.cc"
    break;

  case 673:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11893 "Parser/parser.cc"
    break;

  case 674:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11902 "Parser/parser.cc"
    break;

  case 675:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11911 "Parser/parser.cc"
    break;

  case 676:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11917 "Parser/parser.cc"
    break;

  case 677:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11927 "Parser/parser.cc"
    break;

  case 678:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11933 "Parser/parser.cc"
    break;

  case 679:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11939 "Parser/parser.cc"
    break;

  case 681:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11945 "Parser/parser.cc"
    break;

  case 682:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11951 "Parser/parser.cc"
    break;

  case 683:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11957 "Parser/parser.cc"
    break;

  case 684:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11963 "Parser/parser.cc"
    break;

  case 685:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11969 "Parser/parser.cc"
    break;

  case 686:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11975 "Parser/parser.cc"
    break;

  case 688:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11981 "Parser/parser.cc"
    break;

  case 691:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 692:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 693:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 694:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12005 "Parser/parser.cc"
    break;

  case 697:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12011 "Parser/parser.cc"
    break;

  case 698:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12017 "Parser/parser.cc"
    break;

  case 699:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12023 "Parser/parser.cc"
    break;

  case 701:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12029 "Parser/parser.cc"
    break;

  case 702:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12035 "Parser/parser.cc"
    break;

  case 703:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 705:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 706:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12053 "Parser/parser.cc"
    break;

  case 707:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12059 "Parser/parser.cc"
    break;

  case 708:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12065 "Parser/parser.cc"
    break;

  case 709:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12071 "Parser/parser.cc"
    break;

  case 711:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 712:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12083 "Parser/parser.cc"
    break;

  case 713:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 718:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 720:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12101 "Parser/parser.cc"
    break;

  case 721:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12107 "Parser/parser.cc"
    break;

  case 727:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12113 "Parser/parser.cc"
    break;

  case 730:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12119 "Parser/parser.cc"
    break;

  case 731:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12125 "Parser/parser.cc"
    break;

  case 732:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12131 "Parser/parser.cc"
    break;

  case 733:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12137 "Parser/parser.cc"
    break;

  case 734:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12143 "Parser/parser.cc"
    break;

  case 735:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12149 "Parser/parser.cc"
    break;

  case 736:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12155 "Parser/parser.cc"
    break;

  case 738:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 739:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 740:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 742:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 744:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12185 "Parser/parser.cc"
    break;

  case 745:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12191 "Parser/parser.cc"
    break;

  case 746:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12197 "Parser/parser.cc"
    break;

  case 747:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12203 "Parser/parser.cc"
    break;

  case 748:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12209 "Parser/parser.cc"
    break;

  case 749:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12215 "Parser/parser.cc"
    break;

  case 751:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12221 "Parser/parser.cc"
    break;

  case 752:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12227 "Parser/parser.cc"
    break;

  case 753:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12233 "Parser/parser.cc"
    break;

  case 754:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12244 "Parser/parser.cc"
    break;

  case 755:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12250 "Parser/parser.cc"
    break;

  case 756:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12256 "Parser/parser.cc"
    break;

  case 757:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12262 "Parser/parser.cc"
    break;

  case 758:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12271 "Parser/parser.cc"
    break;

  case 759:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12277 "Parser/parser.cc"
    break;

  case 760:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12287 "Parser/parser.cc"
    break;

  case 761:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12293 "Parser/parser.cc"
    break;

  case 762:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12299 "Parser/parser.cc"
    break;

  case 763:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12305 "Parser/parser.cc"
    break;

  case 764:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12311 "Parser/parser.cc"
    break;

  case 765:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12317 "Parser/parser.cc"
    break;

  case 766:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12323 "Parser/parser.cc"
    break;

  case 767:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12329 "Parser/parser.cc"
    break;

  case 768:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12335 "Parser/parser.cc"
    break;

  case 769:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12341 "Parser/parser.cc"
    break;

  case 772:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 773:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12353 "Parser/parser.cc"
    break;

  case 774:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12359 "Parser/parser.cc"
    break;

  case 775:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 777:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 778:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 779:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12383 "Parser/parser.cc"
    break;

  case 780:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12389 "Parser/parser.cc"
    break;

  case 781:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12395 "Parser/parser.cc"
    break;

  case 782:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12401 "Parser/parser.cc"
    break;

  case 783:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12407 "Parser/parser.cc"
    break;

  case 784:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12416 "Parser/parser.cc"
    break;

  case 785:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12425 "Parser/parser.cc"
    break;

  case 786:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12434 "Parser/parser.cc"
    break;

  case 787:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12440 "Parser/parser.cc"
    break;

  case 788:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12449 "Parser/parser.cc"
    break;

  case 789:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12455 "Parser/parser.cc"
    break;

  case 791:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12461 "Parser/parser.cc"
    break;

  case 796:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12467 "Parser/parser.cc"
    break;

  case 797:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12473 "Parser/parser.cc"
    break;

  case 798:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12479 "Parser/parser.cc"
    break;

  case 800:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 12485 "Parser/parser.cc"
    break;

  case 801:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12491 "Parser/parser.cc"
    break;

  case 802:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12497 "Parser/parser.cc"
    break;

  case 803:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12503 "Parser/parser.cc"
    break;

  case 805:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12509 "Parser/parser.cc"
    break;

  case 806:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12515 "Parser/parser.cc"
    break;

  case 807:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12521 "Parser/parser.cc"
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
#line 12537 "Parser/parser.cc"
    break;

  case 809:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12543 "Parser/parser.cc"
    break;

  case 810:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12549 "Parser/parser.cc"
    break;

  case 811:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12555 "Parser/parser.cc"
    break;

  case 812:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12561 "Parser/parser.cc"
    break;

  case 813:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12567 "Parser/parser.cc"
    break;

  case 814:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12573 "Parser/parser.cc"
    break;

  case 816:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12582 "Parser/parser.cc"
    break;

  case 817:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12588 "Parser/parser.cc"
    break;

  case 818:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12597 "Parser/parser.cc"
    break;

  case 819:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12607 "Parser/parser.cc"
    break;

  case 820:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12616 "Parser/parser.cc"
    break;

  case 821:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12626 "Parser/parser.cc"
    break;

  case 822:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12637 "Parser/parser.cc"
    break;

  case 823:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12647 "Parser/parser.cc"
    break;

  case 824:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12658 "Parser/parser.cc"
    break;

  case 825:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12668 "Parser/parser.cc"
    break;

  case 826:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12679 "Parser/parser.cc"
    break;

  case 827:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12689 "Parser/parser.cc"
    break;

  case 829:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12695 "Parser/parser.cc"
    break;

  case 830:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12701 "Parser/parser.cc"
    break;

  case 831:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12707 "Parser/parser.cc"
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
#line 12719 "Parser/parser.cc"
    break;

  case 833:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12730 "Parser/parser.cc"
    break;

  case 834:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12739 "Parser/parser.cc"
    break;

  case 835:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12748 "Parser/parser.cc"
    break;

  case 836:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12754 "Parser/parser.cc"
    break;

  case 837:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12760 "Parser/parser.cc"
    break;

  case 838:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12766 "Parser/parser.cc"
    break;

  case 839:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12775 "Parser/parser.cc"
    break;

  case 840:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 841:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12787 "Parser/parser.cc"
    break;

  case 842:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12793 "Parser/parser.cc"
    break;

  case 847:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12799 "Parser/parser.cc"
    break;

  case 848:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12805 "Parser/parser.cc"
    break;

  case 849:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12815 "Parser/parser.cc"
    break;

  case 850:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12821 "Parser/parser.cc"
    break;

  case 853:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12827 "Parser/parser.cc"
    break;

  case 854:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12833 "Parser/parser.cc"
    break;

  case 855:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12839 "Parser/parser.cc"
    break;

  case 856:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12845 "Parser/parser.cc"
    break;

  case 858:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12851 "Parser/parser.cc"
    break;

  case 859:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12857 "Parser/parser.cc"
    break;

  case 860:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12863 "Parser/parser.cc"
    break;

  case 861:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12869 "Parser/parser.cc"
    break;

  case 866:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12875 "Parser/parser.cc"
    break;

  case 867:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12881 "Parser/parser.cc"
    break;

  case 868:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12887 "Parser/parser.cc"
    break;

  case 869:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12893 "Parser/parser.cc"
    break;

  case 870:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12899 "Parser/parser.cc"
    break;

  case 872:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12905 "Parser/parser.cc"
    break;

  case 873:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12911 "Parser/parser.cc"
    break;

  case 874:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12917 "Parser/parser.cc"
    break;

  case 875:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12923 "Parser/parser.cc"
    break;

  case 876:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12929 "Parser/parser.cc"
    break;

  case 877:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12935 "Parser/parser.cc"
    break;

  case 878:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12941 "Parser/parser.cc"
    break;

  case 879:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12947 "Parser/parser.cc"
    break;

  case 880:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12953 "Parser/parser.cc"
    break;

  case 881:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12959 "Parser/parser.cc"
    break;

  case 882:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12965 "Parser/parser.cc"
    break;

  case 883:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12971 "Parser/parser.cc"
    break;

  case 884:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12977 "Parser/parser.cc"
    break;

  case 885:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12983 "Parser/parser.cc"
    break;

  case 886:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12989 "Parser/parser.cc"
    break;

  case 887:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12995 "Parser/parser.cc"
    break;

  case 888:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13001 "Parser/parser.cc"
    break;

  case 889:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13007 "Parser/parser.cc"
    break;

  case 891:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13013 "Parser/parser.cc"
    break;

  case 892:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13019 "Parser/parser.cc"
    break;

  case 893:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13025 "Parser/parser.cc"
    break;

  case 894:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13031 "Parser/parser.cc"
    break;

  case 895:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13037 "Parser/parser.cc"
    break;

  case 896:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13043 "Parser/parser.cc"
    break;

  case 897:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13049 "Parser/parser.cc"
    break;

  case 898:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13055 "Parser/parser.cc"
    break;

  case 899:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13061 "Parser/parser.cc"
    break;

  case 900:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13067 "Parser/parser.cc"
    break;

  case 901:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13073 "Parser/parser.cc"
    break;

  case 902:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13079 "Parser/parser.cc"
    break;

  case 903:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13085 "Parser/parser.cc"
    break;

  case 904:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13091 "Parser/parser.cc"
    break;

  case 905:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13097 "Parser/parser.cc"
    break;

  case 906:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13103 "Parser/parser.cc"
    break;

  case 910:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13109 "Parser/parser.cc"
    break;

  case 911:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13115 "Parser/parser.cc"
    break;

  case 912:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13121 "Parser/parser.cc"
    break;

  case 913:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13127 "Parser/parser.cc"
    break;

  case 914:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13133 "Parser/parser.cc"
    break;

  case 915:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13139 "Parser/parser.cc"
    break;

  case 916:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13145 "Parser/parser.cc"
    break;

  case 917:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13151 "Parser/parser.cc"
    break;

  case 918:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13157 "Parser/parser.cc"
    break;

  case 919:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13163 "Parser/parser.cc"
    break;

  case 920:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13169 "Parser/parser.cc"
    break;

  case 921:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13175 "Parser/parser.cc"
    break;

  case 922:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13181 "Parser/parser.cc"
    break;

  case 923:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13187 "Parser/parser.cc"
    break;

  case 924:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13193 "Parser/parser.cc"
    break;

  case 925:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13202 "Parser/parser.cc"
    break;

  case 926:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13208 "Parser/parser.cc"
    break;

  case 927:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13214 "Parser/parser.cc"
    break;

  case 929:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13220 "Parser/parser.cc"
    break;

  case 930:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13226 "Parser/parser.cc"
    break;

  case 931:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13232 "Parser/parser.cc"
    break;

  case 932:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13238 "Parser/parser.cc"
    break;

  case 933:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13244 "Parser/parser.cc"
    break;

  case 934:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13250 "Parser/parser.cc"
    break;

  case 935:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13256 "Parser/parser.cc"
    break;

  case 936:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13262 "Parser/parser.cc"
    break;

  case 937:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13268 "Parser/parser.cc"
    break;

  case 938:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13274 "Parser/parser.cc"
    break;

  case 939:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13280 "Parser/parser.cc"
    break;

  case 940:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13286 "Parser/parser.cc"
    break;

  case 941:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13292 "Parser/parser.cc"
    break;

  case 942:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13298 "Parser/parser.cc"
    break;

  case 943:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13304 "Parser/parser.cc"
    break;

  case 944:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13310 "Parser/parser.cc"
    break;

  case 945:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13316 "Parser/parser.cc"
    break;

  case 946:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13322 "Parser/parser.cc"
    break;

  case 948:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13328 "Parser/parser.cc"
    break;

  case 949:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13334 "Parser/parser.cc"
    break;

  case 950:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13340 "Parser/parser.cc"
    break;

  case 951:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13346 "Parser/parser.cc"
    break;

  case 952:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13352 "Parser/parser.cc"
    break;

  case 953:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13358 "Parser/parser.cc"
    break;

  case 954:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13364 "Parser/parser.cc"
    break;

  case 955:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13370 "Parser/parser.cc"
    break;

  case 956:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13376 "Parser/parser.cc"
    break;

  case 957:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13382 "Parser/parser.cc"
    break;

  case 958:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13388 "Parser/parser.cc"
    break;

  case 959:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13394 "Parser/parser.cc"
    break;

  case 960:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13400 "Parser/parser.cc"
    break;

  case 961:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13406 "Parser/parser.cc"
    break;

  case 962:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13412 "Parser/parser.cc"
    break;

  case 963:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13418 "Parser/parser.cc"
    break;

  case 964:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13424 "Parser/parser.cc"
    break;

  case 965:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13431 "Parser/parser.cc"
    break;

  case 967:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13437 "Parser/parser.cc"
    break;

  case 968:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13443 "Parser/parser.cc"
    break;

  case 969:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13449 "Parser/parser.cc"
    break;

  case 970:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13455 "Parser/parser.cc"
    break;

  case 971:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13461 "Parser/parser.cc"
    break;

  case 972:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13467 "Parser/parser.cc"
    break;

  case 973:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13473 "Parser/parser.cc"
    break;

  case 974:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13479 "Parser/parser.cc"
    break;

  case 975:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13485 "Parser/parser.cc"
    break;

  case 976:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13491 "Parser/parser.cc"
    break;

  case 977:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13497 "Parser/parser.cc"
    break;

  case 978:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13503 "Parser/parser.cc"
    break;

  case 979:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13509 "Parser/parser.cc"
    break;

  case 980:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13516 "Parser/parser.cc"
    break;

  case 982:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13522 "Parser/parser.cc"
    break;

  case 983:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13528 "Parser/parser.cc"
    break;

  case 984:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13534 "Parser/parser.cc"
    break;

  case 985:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13540 "Parser/parser.cc"
    break;

  case 986:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13546 "Parser/parser.cc"
    break;

  case 987:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13552 "Parser/parser.cc"
    break;

  case 988:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13558 "Parser/parser.cc"
    break;

  case 989:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13564 "Parser/parser.cc"
    break;

  case 990:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13570 "Parser/parser.cc"
    break;

  case 991:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13576 "Parser/parser.cc"
    break;

  case 992:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13582 "Parser/parser.cc"
    break;

  case 994:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13588 "Parser/parser.cc"
    break;

  case 995:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13594 "Parser/parser.cc"
    break;

  case 996:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13600 "Parser/parser.cc"
    break;

  case 997:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13606 "Parser/parser.cc"
    break;

  case 998:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13612 "Parser/parser.cc"
    break;

  case 999:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13618 "Parser/parser.cc"
    break;

  case 1000:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13624 "Parser/parser.cc"
    break;

  case 1002:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13630 "Parser/parser.cc"
    break;

  case 1003:
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13636 "Parser/parser.cc"
    break;

  case 1004:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13642 "Parser/parser.cc"
    break;

  case 1005:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13648 "Parser/parser.cc"
    break;

  case 1006:
#line 3942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13654 "Parser/parser.cc"
    break;

  case 1007:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13660 "Parser/parser.cc"
    break;

  case 1008:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13666 "Parser/parser.cc"
    break;

  case 1009:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13672 "Parser/parser.cc"
    break;

  case 1010:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13678 "Parser/parser.cc"
    break;

  case 1011:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13684 "Parser/parser.cc"
    break;

  case 1013:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13690 "Parser/parser.cc"
    break;

  case 1014:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13696 "Parser/parser.cc"
    break;

  case 1016:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13702 "Parser/parser.cc"
    break;

  case 1017:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13708 "Parser/parser.cc"
    break;

  case 1019:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13714 "Parser/parser.cc"
    break;

  case 1020:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13720 "Parser/parser.cc"
    break;

  case 1021:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13726 "Parser/parser.cc"
    break;

  case 1022:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13732 "Parser/parser.cc"
    break;

  case 1023:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13738 "Parser/parser.cc"
    break;

  case 1024:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13744 "Parser/parser.cc"
    break;

  case 1025:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13750 "Parser/parser.cc"
    break;

  case 1028:
#line 4040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13757 "Parser/parser.cc"
    break;

  case 1029:
#line 4043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13763 "Parser/parser.cc"
    break;

  case 1030:
#line 4045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13769 "Parser/parser.cc"
    break;

  case 1031:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13775 "Parser/parser.cc"
    break;

  case 1032:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13781 "Parser/parser.cc"
    break;

  case 1033:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13787 "Parser/parser.cc"
    break;

  case 1034:
#line 4056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13793 "Parser/parser.cc"
    break;

  case 1035:
#line 4058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13799 "Parser/parser.cc"
    break;

  case 1037:
#line 4064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13805 "Parser/parser.cc"
    break;

  case 1038:
#line 4066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13811 "Parser/parser.cc"
    break;

  case 1039:
#line 4068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13817 "Parser/parser.cc"
    break;

  case 1040:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13823 "Parser/parser.cc"
    break;

  case 1041:
#line 4075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13829 "Parser/parser.cc"
    break;

  case 1042:
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13835 "Parser/parser.cc"
    break;

  case 1044:
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13841 "Parser/parser.cc"
    break;

  case 1046:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13847 "Parser/parser.cc"
    break;

  case 1047:
#line 4098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13853 "Parser/parser.cc"
    break;

  case 1048:
#line 4100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13859 "Parser/parser.cc"
    break;

  case 1049:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13865 "Parser/parser.cc"
    break;

  case 1050:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13871 "Parser/parser.cc"
    break;

  case 1051:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13877 "Parser/parser.cc"
    break;

  case 1053:
#line 4122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13883 "Parser/parser.cc"
    break;

  case 1054:
#line 4124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13889 "Parser/parser.cc"
    break;

  case 1055:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13895 "Parser/parser.cc"
    break;

  case 1056:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13901 "Parser/parser.cc"
    break;

  case 1057:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13907 "Parser/parser.cc"
    break;

  case 1058:
#line 4135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13913 "Parser/parser.cc"
    break;

  case 1059:
#line 4137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13919 "Parser/parser.cc"
    break;

  case 1061:
#line 4143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13925 "Parser/parser.cc"
    break;

  case 1062:
#line 4145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13931 "Parser/parser.cc"
    break;

  case 1063:
#line 4147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13937 "Parser/parser.cc"
    break;

  case 1064:
#line 4152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13943 "Parser/parser.cc"
    break;

  case 1065:
#line 4154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13949 "Parser/parser.cc"
    break;

  case 1068:
#line 4164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13955 "Parser/parser.cc"
    break;

  case 1071:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13961 "Parser/parser.cc"
    break;

  case 1072:
#line 4177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13967 "Parser/parser.cc"
    break;

  case 1073:
#line 4179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13973 "Parser/parser.cc"
    break;

  case 1074:
#line 4181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13979 "Parser/parser.cc"
    break;

  case 1075:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13985 "Parser/parser.cc"
    break;

  case 1076:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13991 "Parser/parser.cc"
    break;

  case 1077:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13997 "Parser/parser.cc"
    break;

  case 1078:
#line 4194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14003 "Parser/parser.cc"
    break;

  case 1079:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14009 "Parser/parser.cc"
    break;

  case 1080:
#line 4198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14015 "Parser/parser.cc"
    break;

  case 1081:
#line 4200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14021 "Parser/parser.cc"
    break;

  case 1082:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14027 "Parser/parser.cc"
    break;

  case 1083:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14033 "Parser/parser.cc"
    break;

  case 1084:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14039 "Parser/parser.cc"
    break;

  case 1085:
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14045 "Parser/parser.cc"
    break;

  case 1086:
#line 4211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14051 "Parser/parser.cc"
    break;

  case 1087:
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14057 "Parser/parser.cc"
    break;

  case 1088:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14063 "Parser/parser.cc"
    break;

  case 1089:
#line 4223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14069 "Parser/parser.cc"
    break;

  case 1090:
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14075 "Parser/parser.cc"
    break;

  case 1092:
#line 4252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14081 "Parser/parser.cc"
    break;

  case 1096:
#line 4263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14087 "Parser/parser.cc"
    break;

  case 1097:
#line 4265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14093 "Parser/parser.cc"
    break;

  case 1098:
#line 4267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14099 "Parser/parser.cc"
    break;

  case 1099:
#line 4269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14105 "Parser/parser.cc"
    break;

  case 1100:
#line 4271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14111 "Parser/parser.cc"
    break;

  case 1101:
#line 4273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14117 "Parser/parser.cc"
    break;

  case 1102:
#line 4280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14123 "Parser/parser.cc"
    break;

  case 1103:
#line 4282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14129 "Parser/parser.cc"
    break;

  case 1104:
#line 4284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14135 "Parser/parser.cc"
    break;

  case 1105:
#line 4286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14141 "Parser/parser.cc"
    break;

  case 1106:
#line 4288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14147 "Parser/parser.cc"
    break;

  case 1107:
#line 4290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14153 "Parser/parser.cc"
    break;

  case 1108:
#line 4295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14159 "Parser/parser.cc"
    break;

  case 1109:
#line 4297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14165 "Parser/parser.cc"
    break;

  case 1110:
#line 4299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14171 "Parser/parser.cc"
    break;

  case 1111:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14177 "Parser/parser.cc"
    break;

  case 1112:
#line 4306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14183 "Parser/parser.cc"
    break;

  case 1113:
#line 4308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14189 "Parser/parser.cc"
    break;

  case 1116:
#line 4332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14195 "Parser/parser.cc"
    break;

  case 1117:
#line 4334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14201 "Parser/parser.cc"
    break;


#line 14205 "Parser/parser.cc"

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
