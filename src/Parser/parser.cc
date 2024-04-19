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
#define YYLAST   26147

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  181
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1116
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2195

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
    2677,  2678,  2683,  2684,  2685,  2690,  2692,  2695,  2698,  2705,
    2706,  2707,  2713,  2718,  2720,  2726,  2727,  2733,  2734,  2738,
    2746,  2753,  2766,  2765,  2769,  2772,  2771,  2780,  2784,  2788,
    2790,  2796,  2797,  2802,  2807,  2815,  2817,  2823,  2825,  2830,
    2831,  2837,  2838,  2839,  2848,  2849,  2851,  2852,  2857,  2858,
    2859,  2861,  2867,  2868,  2870,  2871,  2872,  2874,  2876,  2883,
    2884,  2886,  2888,  2893,  2894,  2903,  2905,  2910,  2912,  2917,
    2918,  2920,  2923,  2925,  2929,  2930,  2931,  2933,  2935,  2943,
    2945,  2950,  2951,  2952,  2956,  2957,  2958,  2963,  2964,  2969,
    2970,  2971,  2972,  2976,  2977,  2982,  2983,  2984,  2985,  2986,
    3000,  3001,  3006,  3007,  3013,  3015,  3018,  3020,  3022,  3045,
    3046,  3052,  3053,  3059,  3058,  3068,  3067,  3071,  3077,  3079,
    3089,  3090,  3092,  3096,  3101,  3103,  3105,  3107,  3113,  3114,
    3118,  3119,  3124,  3126,  3133,  3135,  3136,  3138,  3143,  3145,
    3147,  3152,  3154,  3159,  3164,  3172,  3177,  3179,  3184,  3189,
    3190,  3195,  3196,  3200,  3201,  3202,  3207,  3209,  3215,  3217,
    3222,  3224,  3230,  3231,  3235,  3239,  3243,  3245,  3257,  3259,
    3261,  3263,  3265,  3267,  3269,  3270,  3275,  3278,  3277,  3289,
    3288,  3301,  3300,  3314,  3313,  3327,  3326,  3342,  3348,  3350,
    3356,  3357,  3368,  3375,  3380,  3386,  3389,  3392,  3396,  3402,
    3405,  3408,  3413,  3414,  3415,  3416,  3420,  3428,  3429,  3441,
    3442,  3446,  3447,  3452,  3454,  3456,  3461,  3462,  3468,  3469,
    3471,  3476,  3477,  3478,  3479,  3480,  3482,  3517,  3519,  3524,
    3526,  3527,  3529,  3534,  3536,  3538,  3540,  3545,  3547,  3549,
    3551,  3553,  3555,  3557,  3562,  3564,  3566,  3568,  3577,  3579,
    3580,  3585,  3587,  3589,  3591,  3593,  3598,  3600,  3602,  3604,
    3609,  3611,  3613,  3615,  3617,  3619,  3631,  3632,  3633,  3637,
    3639,  3641,  3643,  3645,  3650,  3652,  3654,  3656,  3661,  3663,
    3665,  3667,  3669,  3671,  3683,  3688,  3693,  3695,  3696,  3698,
    3703,  3705,  3707,  3709,  3714,  3716,  3718,  3720,  3722,  3724,
    3726,  3731,  3733,  3735,  3737,  3746,  3748,  3749,  3754,  3756,
    3758,  3760,  3762,  3767,  3769,  3771,  3773,  3778,  3780,  3782,
    3784,  3786,  3788,  3798,  3800,  3803,  3804,  3806,  3811,  3813,
    3815,  3820,  3822,  3824,  3826,  3831,  3833,  3835,  3849,  3851,
    3854,  3855,  3857,  3862,  3864,  3869,  3871,  3873,  3878,  3880,
    3885,  3887,  3904,  3905,  3907,  3912,  3914,  3916,  3918,  3920,
    3925,  3926,  3928,  3930,  3935,  3937,  3939,  3945,  3947,  3950,
    3957,  3959,  3968,  3970,  3972,  3973,  3975,  3977,  3981,  3983,
    3988,  3990,  3992,  3994,  4029,  4030,  4034,  4035,  4038,  4040,
    4045,  4047,  4049,  4051,  4053,  4058,  4059,  4061,  4063,  4068,
    4070,  4072,  4078,  4079,  4081,  4090,  4093,  4095,  4098,  4100,
    4102,  4116,  4117,  4119,  4124,  4126,  4128,  4130,  4132,  4137,
    4138,  4140,  4142,  4147,  4149,  4157,  4158,  4159,  4164,  4165,
    4170,  4172,  4174,  4176,  4178,  4180,  4187,  4189,  4191,  4193,
    4195,  4198,  4200,  4202,  4204,  4206,  4211,  4213,  4215,  4220,
    4246,  4247,  4249,  4253,  4254,  4258,  4260,  4262,  4264,  4266,
    4268,  4275,  4277,  4279,  4281,  4283,  4285,  4290,  4292,  4294,
    4299,  4301,  4303,  4321,  4323,  4328,  4329
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

#define YYPACT_NINF (-1823)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1115)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      90, 13096,   101,   152, 19546,    88, -1823, -1823, -1823, -1823,
   -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823,    78,   858,
     134, -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823,
   -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823,
   -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823,   268,   199,
   -1823, -1823, -1823, -1823, -1823, -1823,  4215,  4215,   154, 13096,
     205,   253, 23162, -1823,   269, -1823, -1823, -1823, -1823, -1823,
   -1823, -1823, -1823, -1823, -1823,   277,  3145, -1823,   705,   364,
   -1823, -1823, -1823, -1823, -1823, 19081, -1823, -1823,   378,   415,
     217,   308, -1823,  4215,   433,   470,   491,   500,  5498,   702,
    1129, 13261, -1823, -1823,   659, 18926,  1556, -1823, -1823, -1823,
   -1823,  2711,   738, 10308,  5052,   852,  2711,   952,   600, -1823,
   -1823, -1823, -1823,   196, -1823, -1823, -1823, -1823,   599, -1823,
   -1823, -1823, -1823, -1823,   623,   617,   196, -1823,   196, 17244,
   -1823, -1823, -1823, 20876,  4215, -1823, -1823,  4215, -1823, 13096,
   -1823,   612, 20929, -1823, -1823,  5809, 22228, -1823, -1823,  1195,
    1195,   634,  3094, -1823, -1823, -1823, -1823,   327, 15701,   196,
    1308,   196, -1823, -1823, -1823, -1823, -1823, -1823,   644, -1823,
     649,   677,  2337, -1823,   720, 25596, -1823, -1823, -1823, -1823,
   -1823, -1823, -1823, 17623,  2744,  2927,  3145,    25,   701,   703,
     744,   752,   754,   779, -1823, -1823, 19701, 11921,   712,   774,
   -1823, 20004, -1823, -1823, -1823, -1823,   792, -1823, -1823,   799,
   -1823, 23544,   950, 23696, -1823,   819,  4215,   617,   826,  2487,
    5809,  2487, -1823, -1823, -1823,  3797,  5149,   822,   890,   128,
     890, -1823,   196,   196,   112,  8312,   402,   890, -1823,   196,
     196,   112,   196, -1823,   196, -1823,  5223, -1823, -1823,   862,
     888,  1195, 22972,   895, 19081, -1823, -1823,  2711, -1823,  1687,
     600,   887,   963,  8312,  4215,  4215,   217, -1823, 14881, -1823,
    1195,  1195,   899,   963,  8312,  4215, -1823, 23328, -1823, -1823,
   -1823,  1195, -1823, -1823, -1823, -1823,  1195, -1823,   786,  3888,
    4215, -1823, 18780,   910, -1823, -1823, -1823, 22835,   617, 17083,
     901,  5809, 18727, 22972,  2711, -1823, -1823, 22376, -1823,   890,
     252, -1823, 25596, 22228,  4044,  5223, -1823,   460, -1823, -1823,
   -1823, -1823, -1823, 20929,  4215, -1823,   909,   920, -1823, -1823,
   -1823, -1823,  4215,  3645,   426,   689, -1823,  4215,   649, -1823,
     850,   196, -1823,   935, 21084,   972, 16193, 23025,  2711, -1823,
    2711,  1195,  2711,  1195, -1823, -1823,   196, -1823, -1823,   944,
   21137, -1823, -1823, -1823, 21292,   792, -1823,  3630,   144,   485,
   -1823,   263,   600,   947,   937, -1823,  3094,   961,   649,  3094,
   -1823, -1823, -1823, -1823, -1823,  2744, -1823,   769, -1823,   974,
   -1823,   959,   998, 25672,   980,   993,   999, 25596, 25748,  1001,
   23213, -1823, -1823, -1823, -1823, -1823, -1823, 25824, 25824, 17464,
     982,  3876, -1823, -1823, -1823, -1823,   580, -1823,   637, -1823,
    2179, -1823, 25596, 25596, -1823,   997,   645,   854,   912,   321,
    1034,  1013,  1018,  1029,  1061,    21, -1823,   859, -1823,  1067,
   -1823,  1037,  4792, 18100, -1823, -1823,   682,  1067, -1823, -1823,
     885, -1823, -1823,   904,  2927,  1092,  1094,  1099,  1102,  1107,
    1126, -1823, -1823,   499,  1127, -1823,   894,  1127,  1133, -1823,
    1156, -1823, 20876, -1823,  1082,  1134, 18259, -1823, -1823,  4304,
    4706,  1192, 16193,  1198,   876,  1144,  1153,  1180, -1823, -1823,
   -1823,  4215,  4340, 20358, -1823, -1823, -1823, -1823, -1823, -1823,
   18586,  4076,   982, 23544,  1205,  1212, -1823, -1823,  1190, 23696,
     869, -1823, -1823, -1823, 23772,  1229, -1823, -1823, -1823, -1823,
    1213,  3797,   921,  1233,  1253,  1290,   927,  1298,  1307,  1317,
    1319,  1322,  1324,  5149, -1823, -1823, -1823,   196,  1242,  1224,
    1299, -1823, -1823,  1331,   217, -1823, -1823,   617,   963, 19865,
   -1823, -1823,   217, -1823, -1823,   617, -1823, -1823,  5223, -1823,
   18100, 18100, -1823,  1195,  5809,  9851,  1565, 16357, -1823, -1823,
   -1823, -1823, -1823,   617,   963,   252,  1320, -1823, -1823,  2711,
    1330,   963,  8312, -1823,   617,   963, -1823, 23379, -1823,  1195,
    1195, -1823, -1823,  1341,   265,  1346,   600,  1351, -1823, -1823,
   -1823, 20305,  1363,  1365, -1823, -1823,   956, -1823,  1484, -1823,
    1394, -1823, -1823, -1823, 21456, 25900, -1823, -1823, -1823, -1823,
   -1823,  4044,   939,  5223, 19865, 16521,   890, 13096, -1823,  4215,
    1424, -1823,  1432, -1823, -1823, -1823, -1823, -1823,  3094, -1823,
   -1823,  1514,  3959, 20513, 11921, -1823, 21509, -1823,  1195,  1195,
   -1823, -1823,   792, -1823, 15209,  1436,  1581, 25596,  1188,  1331,
    1438, -1823,   196,   196, -1823,  1127, -1823, 21084, -1823, -1823,
   20305,  1195,  1195, -1823,  3959, -1823, -1823, 22080, -1823, -1823,
   21137, -1823,   196,  1459,   196,   937,   116,  1461,   975, 20929,
     988,   989, -1823,  2744, 23848,  1448, -1823, 17782, -1823,  3876,
   21664, 20929, -1823, 17782, -1823, 25596, -1823, -1823, -1823, -1823,
   -1823, -1823, 17941, -1823, -1823, 20566, 21664, 21664,  1142,  1454,
    1482,   785,  1522, -1823,  1021,  1472,  1182,  1476, -1823, 23772,
   25596, 23924,  1471, 25596,  2487, 25596,  2487, -1823,  1661, -1823,
   -1823, 23848,  2713, 25596, 23848,  2487, -1823, -1823, 25596, 25596,
   25596, 25596, 25596, 25596, 25596, 25596, 25596, 25596, 25596, 25596,
   25596, 25596, 25596, 25596, 25596, 25596, 25596, 24000,  1457,   720,
    3741, 11921, -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823,
   -1823, -1823, -1823,  1477, 25596, -1823, -1823, 15373,  1857, -1823,
   -1823,   196,   196, -1823, -1823, 18100, -1823, -1823,   666,  1127,
   -1823,  1041,  1127, 19865, -1823, -1823,  1331, 19865, -1823,  1331,
   -1823, 25976, -1823, -1823, -1823, 19391, 11921,  1486,  1240,  1489,
   14717,  1626,  4111,   715,  1438, -1823,   196,   196,  1438,   772,
   -1823,   196,   196, 25596,  4215, 16357,  1490, 16357,  1492,  1438,
     141, 15537, 15537, 15537,  4215, -1823, -1823, 25596,  1190, -1823,
   23544,  1501, -1823,  1246, -1823, -1823, -1823, -1823, -1823,  1057,
   -1823, 15537, 25596,  1060,  1497,  1503,  1505,  1068,  1507,  1509,
    1510,  1513,  1515,  1527,   782,  1127, -1823, -1823,   804,  1127,
   -1823, -1823,   834,  1127, -1823, -1823, -1823,  5809,   720,  1635,
    1127, 22524, -1823, -1823,   617,  1528, -1823, -1823, -1823,  1073,
    1529,  1076,  1534, -1823,  1133,  1535,  1543, -1823,   617, -1823,
    1544, -1823,   617,   963,  1543, -1823,   617,  1537,  1538,  1540,
   -1823, -1823, 20168, -1823,  2487,  4215, 11050,  1637, -1823,  1134,
   -1823, 15537,  1086,  1547, -1823,  1543,  1555, -1823, 21717, 18100,
    1536, -1823,  1536, -1823, -1823, -1823, -1823, 21137, -1823, 12089,
   18418, -1823,  1559,  1560,  1561,  1563, -1823,  7258,   196, -1823,
    1188, -1823, -1823, -1823, -1823,  1331, -1823, -1823, -1823,  1195,
   -1823, -1823, -1823, -1823,   116,   937,  1569,   327, -1823, -1823,
    1572,  4215,   116, -1823, -1823,  1571,  1567, -1823, -1823,  1105,
   -1823, -1823, -1823, -1823,  1579,  1580,  1578,  1583,  1592,  1584,
    1599,  1600,  1597,  1603, 25596,  1605,  1609,  1612, 21872, 12257,
   25596, -1823, -1823,  1530, -1823, -1823, -1823, 25596, -1823,  1624,
    1627, 23620,  1276, -1823, 23848,  1623, -1823,  1628, -1823, -1823,
    4673, -1823,  1108, -1823, -1823, -1823,  4673, -1823, -1823,  1278,
     420, -1823, -1823,   997,   997,   997,   645,   645,   854,   854,
     912,   912,   912,   912,   321,   321,  1034,  1013,  1018,  1029,
    1061, 25596,  1280, -1823,  1629,  4673, -1823, -1823, 23544, -1823,
    1634,  1636,  1638,  1639,  1857, -1823, -1823, -1823, -1823, -1823,
   19865, -1823, -1823,  1331, 19865, -1823,  1331,  1641,  1642, 15537,
   15537, -1823, -1823, 14717,   966,  1643,  1644,  1645,  1648,  3431,
    4111, -1823, -1823, 19865, -1823, -1823, -1823, -1823, -1823, -1823,
   19865, -1823, -1823, -1823, -1823,  1647, -1823,  1438,  1633, -1823,
   -1823, -1823, -1823, -1823, -1823, -1823, -1823,  1652,  1649,  1653,
   -1823, -1823,   217,  4673,  1285,   176, -1823, -1823,  1586, -1823,
   23696, -1823, 25596,   196, 24076, 15537, -1823, -1823,   840,  1127,
   -1823,   842,  1127, -1823, -1823,   843,  1127, 19865, -1823, -1823,
    1331, 19865, -1823, -1823,  1331, 19865, -1823, -1823,  1331,   890,
    1656, -1823,  1331,   313, -1823,  1067,  1654, -1823, -1823, -1823,
   -1823, -1823, -1823,  1663, -1823, -1823, -1823, 21717,  1543, -1823,
     617, -1823, -1823, -1823, -1823, -1823, 13914, -1823, -1823, -1823,
   -1823,   437, -1823,   445,   385, 11753,  1664,  1667, 16905,  1668,
    1672,  2998,  3523,  3862, 24152,  1673, -1823, -1823,  1674,  1676,
   16905,  1677, -1823, -1823,   617, 25596, 25596,  1821,  1675,   571,
   -1823, 17305,  1289,  1678,  1658, -1823, -1823, -1823, 10872, -1823,
   -1823, -1823, -1823, -1823,  2572, -1823, -1823, -1823,  1366,   352,
   -1823,   409, -1823,   352, -1823, -1823, -1823, -1823, -1823,  2487,
   -1823, -1823, 13426, 19236,  1679, -1823,  4215,  1682,  1685, -1823,
   16521, -1823, -1823,  4215, -1823, -1823,  5809, -1823, -1823,  1666,
    1670,  1113, 20929,   649,   649, -1823, -1823,   982,  1134, 18259,
   -1823,  1067, -1823, 12425, -1823,   860,  1127, -1823,  1195, 10148,
   -1823, -1823,   937,  1572,  1691,   116,   600,   164,  1694,  1680,
    1572,  1703, -1823, -1823, 23848,   621, -1823, 20305, 12257,  2487,
   -1823,   621, -1823, 20721,   621, -1823, 25596, 25596, 25596, -1823,
   -1823, -1823, -1823, 25596, 25596,  1695, 23544, -1823, -1823,  1699,
     681, -1823, -1823, -1823,  2847, -1823, -1823,  1302, -1823,   133,
   -1823,  1347, -1823, 23772, -1823, -1823, 25596,  1681,  1352,  1356,
    1190, -1823,   878,  1127, -1823, -1823,  1708,  1711, -1823, -1823,
   -1823, -1823,  1719,   889,  1127, -1823,   932,  1952,   196,   196,
   -1823, -1823,  1723,  1724, -1823,  1722, -1823, 16357,  1727, -1823,
   15865, 16029,  1726,  1733, -1823,  1731, 25596, 25596,  1358,  1734,
   -1823, -1823, -1823, -1823, -1823, -1823, -1823,  1736, 19865, -1823,
   -1823,  1331, 19865, -1823, -1823,  1331, 19865, -1823, -1823,  1331,
    1738,  1739,  1740,   217,   196, -1823, -1823,  1362, 25596, 22676,
    1742,  1744, -1823, -1823, -1823,  1746, 14072, 14230, 14388, 21717,
   22972, 21664, 21664,  1749, -1823,   441,   457,  2902,  4692, -1823,
     481,  4215,  4215, -1823, 23848,   427,   447, -1823, -1823, -1823,
   -1823, 11753, 25596,  1750,  1826, 11584, 11228, -1823,  1728, -1823,
    1729, 25596,  1730, 23544,  1747, 25596, 23772, 25596, -1823, 11406,
    1074, -1823,  1752,    89, -1823,    48,  1822,   452, -1823,  1762,
   -1823,  1753, -1823,  1756,  1764,  1767, 16905, 16905, -1823, -1823,
    1840, -1823, -1823,    32,    32,   374, 15045,   196,   482, -1823,
   -1823,  1780,  1785,   426, -1823,  1789, -1823,  1784, -1823,  1790,
   -1823, -1823, -1823, -1823, 12593,  1786,  1794,  1796, -1823, 19865,
   -1823, -1823,  1331, 25596, 25596,  1134,  1799, -1823,  1787,  1806,
     116,  1572,   327,  4215, -1823, 24228, -1823,  1808, -1823, 21717,
   -1823,   892,  1807,  1801,  1136, -1823,  1811, -1823, -1823, -1823,
   -1823, -1823, 23544,  1190, 23772, -1823,  1847,  4673, -1823,  1847,
    1847, -1823,  4673,  3301,  4525, -1823,  1364, -1823, -1823, -1823,
    1824, 19865, -1823, -1823,  1331, -1823, -1823,  1819,  1823,   196,
   19865, -1823, -1823,  1331, 19865, -1823, -1823,  1825, -1823, -1823,
   -1823, -1823, -1823, -1823, -1823, -1823,  1633, -1823, -1823, -1823,
    1827, -1823, -1823, -1823, -1823,  1830,  1828,   196,  1837,  1838,
    1839, -1823, -1823, -1823, -1823, -1823, 25596, -1823,   313, -1823,
    1067, -1823, -1823,  1843,  1844, -1823,  1749,  1749,  1749,  4852,
     945,  1804,   527, -1823,  4852,   531, 18100, -1823, -1823, -1823,
    4324, 25596,  5403,   511, -1823, -1823,   317,  1817,  1817,  1817,
    4215, -1823, -1823, -1823,  1139, -1823, -1823, -1823, -1823,  1678,
    1848, 25596,   378,  1820,   500, 14553, 21717,  1162,  1845, 16905,
    1849, -1823, -1823, -1823,   656, 16905, 25596,  1014,   546, -1823,
   25596, 23465, -1823, -1823,   551, -1823,  1190, -1823,  1164,  1175,
    1177,   723, -1823, -1823, -1823, -1823,   617,  1074,  1846, -1823,
   -1823, 25596, -1823,  1852,   720, -1823, -1823, -1823, -1823, 25596,
   25596, -1823, -1823,   339,    32, -1823,    75, -1823, -1823, 10694,
   -1823,   196, -1823,  1536, -1823, 21717, -1823, -1823, -1823, -1823,
   -1823,  1853,  1851, -1823, -1823,  1862, -1823,  1875,   116, -1823,
    1572,  1855,   142,  1680, 23544, -1823, -1823, -1823,  1860, -1823,
   -1823, 25596, -1823, 20721, 25596,  1190,  1882,  1369, -1823,  1371,
   -1823,  4673, -1823,  4673, -1823, -1823, -1823,  1883,   196,   196,
    1885,  1889, -1823,  1879, -1823, -1823, -1823, -1823, -1823,  1390,
   25596, -1823, -1823, -1823, -1823, -1823,   554,   945,  2490,   556,
   -1823, -1823, -1823, -1823,   196,   196, -1823, -1823, -1823,   562,
   -1823,  1189,  4324,   356, -1823,  5403, -1823,   196, -1823, -1823,
   -1823, -1823, -1823, -1823, 16905, 16905,  1678, 16685,   140, 24304,
    1972, 16905, -1823, -1823, -1823, -1823, 25596, -1823, 24380,  1973,
    1869, 23384, 24456, 16905, 11406,  1678,   632,  1159,  1870, 25596,
   -1823,  1897,   365, 16905, -1823, 16905, -1823,  1898, -1823, -1823,
    1876,   720,   741,  1902,  1395,  1208, 16905,  1915, 16905, 16905,
   16905, -1823, -1823, -1823,   649, -1823,  4215,  5809, -1823, -1823,
    1912,  1913, -1823, -1823,  1572,  1924, -1823, -1823, -1823,  1190,
    1925, -1823, -1823, -1823, -1823,  1926, -1823, -1823, -1823,  1409,
    1412, -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823, -1823,
    1927,  1929,  1931,  2490, -1823,   196, -1823, -1823, -1823, -1823,
   -1823,  1922,  4852, -1823,  2007,  5708,    94, 12764, -1823, 16782,
   -1823,    65,  1211, 16905,  2008,   576,  1918,   309, 16905, 25596,
    1985,  1936,   632,  1159,  1923, -1823, 24532,  1933,   362,  2020,
   -1823, 24608, 24684, 25596,  1678,  1934, 12931, -1823, -1823, -1823,
   -1823, 21925, -1823,  1940,  1938,    27, -1823, 25596, 23848, -1823,
   -1823, 25596,   352, -1823, -1823, -1823, -1823, -1823, -1823, -1823,
    1951, -1823,  1955, -1823, -1823, -1823, -1823,   934,  1127, -1823,
   -1823,   945, -1823, 16905, -1823,   288, -1823,    82, -1823, -1823,
   -1823,  1957, 13591, -1823, -1823, 16905, -1823,    80, -1823, 16905,
   25596,  1950, 24760, -1823, -1823, -1823, 24836, 24912, 25596,  1678,
   -1823, 24988, 25064, 16905,  1945,   411,  1948,   601, -1823, -1823,
    1970, 13591, 21925, -1823,  5236, 21509,  2487,  1963, -1823,  2022,
    1971,   781,  1968, -1823, -1823,  1218,  1220,   495, -1823, -1823,
   19865, -1823, -1823,  1331, -1823, -1823, 25596, -1823, 25596, -1823,
   -1823,  1500, 13756, -1823, -1823, 16905, -1823, -1823,  1678, -1823,
   -1823,  1678,  1960,   603,  1961,   604, -1823, -1823,  1678, -1823,
    1678, -1823,  1977, 25140, 25216, 25292, -1823,  1500, -1823,  1954,
    3494,  4512, -1823, -1823, -1823,    27,  1976, 25596,  1964,    27,
      27, -1823, -1823, 16905,  2068,  1989, -1823, -1823, 16782, -1823,
    1500, -1823, -1823,  1991, 25368, 25444, 25520, -1823, -1823,  1678,
   -1823,  1678, -1823,  1678, -1823,  1954, 25596,  1992,  4512,  1993,
     720,  1994, -1823,   800, -1823, -1823, 16905, -1823, -1823, 10406,
    1999, 16782, -1823, -1823,  1678, -1823,  1678, -1823,  1678,  2002,
    2000, -1823,   617,   720,  2005, -1823,  1981,   720, -1823, -1823,
   -1823, -1823, 10567, -1823,   617, -1823, -1823,  1418, 25596, -1823,
    1226, -1823,   720,  2487,  2006,  1986, -1823, -1823,  1228, -1823,
   -1823,  1987,  2487, -1823, -1823
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
     530,   531,   532,   533,   534,   535,   542,   543,   849,   545,
     618,   619,   622,   624,   620,   626,     0,     0,     0,   490,
       0,     0,    17,   589,   595,     9,    10,    11,    12,    13,
      14,    15,    16,   806,   107,     0,     0,    20,     0,     2,
     105,   106,    18,    19,   867,   490,   807,   428,     0,   431,
     729,   433,   442,     0,   432,   464,   465,     0,     0,     0,
       0,   572,   492,   494,   500,   490,   502,   505,   557,   516,
     544,   474,   550,   555,   476,   567,   475,   582,   586,   592,
     571,   598,   610,   849,   615,   616,   599,   668,   434,   435,
       3,   814,   827,   495,     0,     0,   849,   889,   849,   490,
     906,   907,   908,   490,     0,  1093,  1094,     0,     1,   490,
      17,     0,   490,   453,   454,     0,   572,   500,   484,   485,
     486,   817,     0,   621,   623,   625,   627,     0,   490,   849,
     671,   850,   851,   617,   546,   722,   723,   721,   783,   778,
     768,     0,   858,   815,     0,     0,   507,   808,   812,   813,
     809,   810,   811,   490,   858,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   590,   593,   490,   490,     2,     0,
    1095,   572,   896,   914,  1099,  1092,  1090,  1097,   427,     0,
     169,   735,   168,     0,   436,     0,     0,     0,     0,     0,
       0,     0,   426,   983,   984,     0,     0,   463,   847,   849,
     847,   870,   849,   849,   473,   490,   849,   847,   927,   849,
     849,   472,   849,   946,   849,   924,     0,   565,   566,     0,
       0,   490,   490,     2,   490,   443,   493,   503,   558,     0,
     587,     0,   830,   490,     0,     0,   729,   444,   572,   551,
     568,   583,     0,   830,   490,     0,   506,   552,   559,   560,
     477,   569,   479,   480,   478,   574,   584,   588,     0,   602,
       0,   800,   490,     2,   828,   888,   890,   490,     0,   490,
       0,     0,   572,   490,   502,     2,  1103,   572,  1106,   847,
     847,     3,     0,   572,     0,     0,   456,   849,   842,   844,
     843,   845,     2,   490,     0,   804,     0,     0,   764,   766,
     765,   767,     0,     0,   760,     0,   749,     0,   758,   770,
       0,   849,   669,     2,   490,  1115,   491,   490,   481,   550,
     482,   575,   483,   582,   579,   600,   849,   601,   714,     0,
     490,   715,  1068,  1069,   490,   716,   718,   671,   589,   595,
     672,   673,   674,     0,   671,   852,     0,   781,   769,     0,
     866,   865,   861,   863,   864,   858,   862,     0,   856,   859,
      22,     0,    21,     0,     0,     0,     0,     0,     0,     0,
      24,    26,     4,     8,     5,     6,     7,     0,     0,   490,
       2,     0,   108,   109,   110,   111,    90,    25,    91,    43,
      89,   112,     0,     0,   127,   129,   133,   136,   139,   144,
     147,   149,   151,   153,   155,   157,   160,     0,    27,     0,
     596,     2,   112,   490,   161,   775,   725,   586,   727,   774,
       0,   724,   728,     0,     0,     0,     0,     0,     0,     0,
       0,   868,   894,   849,   904,   912,   916,   922,   589,     2,
       0,  1101,   490,  1104,     2,   105,   490,     3,   713,     0,
    1115,     0,   491,   550,   575,   582,     3,     3,   709,   699,
     703,   715,   716,   490,     2,   897,   915,  1091,     2,     2,
      24,     0,     2,   735,    25,     0,   733,   736,  1113,     0,
       0,   742,   731,   730,     0,     0,   832,     2,   455,   457,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   873,   930,   953,   849,     0,   469,
       2,   869,   877,  1011,   729,   871,   872,     0,   830,   490,
     926,   934,   729,   928,   929,     0,   945,   947,     0,   459,
     490,   490,   556,   491,     0,   572,     0,   490,  1096,  1100,
    1098,   573,   804,     0,   830,   847,     0,   437,   445,   504,
       0,   830,   490,   804,     0,   830,   779,   553,   554,   570,
     585,   591,   594,   589,   595,   613,   614,     0,   780,   685,
     719,   491,     0,   686,   688,   689,     0,   209,   420,   829,
       0,   418,   473,   472,   572,     0,   439,     2,   440,   801,
     461,     0,     0,     0,   490,   490,   847,   490,   804,     0,
       0,     2,     0,   763,   762,   761,   755,   501,     0,   753,
     771,   548,     0,   490,   490,  1070,   491,   487,   488,   489,
    1074,  1065,  1066,  1072,   490,     2,   106,     0,  1030,  1044,
    1115,  1026,   849,   849,  1035,  1042,   707,   490,   580,   717,
     491,   576,   577,   581,     0,   670,  1080,   491,  1085,  1077,
     490,  1082,   849,     0,   849,   671,   671,     0,     0,   490,
       0,     0,   854,   858,    69,     0,    23,   490,    97,     0,
     490,   490,    92,   490,    99,     0,    33,    37,    38,    34,
      35,    36,   490,    95,    96,   490,   490,   490,     2,   108,
     109,     0,     0,   187,     0,     0,   616,     0,  1090,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,    64,
      65,    69,     0,     0,    69,     0,    93,    94,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   490,   170,   171,   172,   173,   174,   175,   176,   177,
     178,   179,   180,   168,     0,   166,   167,   490,   995,   726,
     992,   849,   849,  1000,   597,   490,   855,   895,   849,   905,
     913,   917,   923,   490,   898,   900,   902,   490,   918,   920,
       2,     0,     2,  1102,  1105,   490,   490,     0,     2,     0,
     490,   106,  1030,   849,  1115,   965,   849,   849,  1115,   849,
     980,   849,   849,     3,   717,   490,     0,   490,     0,  1115,
    1115,   490,   490,   490,     0,     2,   744,     0,  1113,   741,
    1114,     0,   737,     0,     2,   740,   743,   184,   183,     0,
       2,   490,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   849,   882,   886,   925,   849,   939,
     943,   951,   849,   961,   874,   931,   954,     0,     0,     0,
    1007,     0,   467,   833,     0,     0,   468,   834,   460,     0,
       0,     0,     0,   458,     0,     2,     2,   835,     0,   441,
       2,   804,     0,   830,     2,   836,     0,     0,     0,     0,
     628,   891,   490,   909,     0,     0,   490,   421,   419,   105,
       3,   490,     0,     3,   805,     2,     0,   757,   490,   490,
     751,   750,   751,   549,   547,   673,  1076,   490,  1081,   491,
     490,  1067,     0,     0,     0,     0,  1045,     0,   849,  1116,
    1031,  1032,   708,  1028,  1029,  1043,  1071,  1075,  1073,   578,
     613,  1079,  1084,   665,   671,   671,     0,     0,   680,   679,
    1113,     0,   671,   784,   782,     0,     0,   857,    73,     0,
      70,    71,    74,   816,     0,     0,     0,     2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   490,   490,
       0,   126,   125,     0,   122,   121,    28,     0,    29,     0,
       0,     0,     0,     3,    69,     0,    52,     0,    53,    62,
       0,    61,     0,    55,    56,    57,     0,    54,    60,     0,
       0,    51,   128,   130,   131,   132,   134,   135,   137,   138,
     142,   143,   140,   141,   145,   146,   148,   150,   152,   154,
     156,     0,     0,   430,     0,     0,    30,     3,   735,   162,
       0,     0,     0,     0,   996,   997,   993,   994,   777,   776,
     490,   899,   901,   903,   490,   919,   921,     0,     0,   490,
     490,  1021,  1020,   490,     0,     0,     0,     0,     0,   849,
    1031,   968,   985,   490,   963,   971,   705,   966,   967,   706,
     490,   978,   988,   981,   982,     0,     3,  1115,     3,   701,
     451,   700,   704,  1107,   710,   711,   693,     0,   694,   695,
       3,     3,   729,     0,   160,     0,     3,     3,     0,   738,
       0,   732,     0,   849,     0,   490,     3,   462,   849,   883,
     887,   849,   940,   944,   952,   849,   962,   490,   875,   878,
     880,   490,   932,   935,   937,   490,   955,   957,   959,   847,
       0,   470,  1008,     3,  1012,  1013,     3,   838,   948,   562,
     561,   564,   563,     2,   805,   839,   786,   490,     2,   837,
       0,   805,   840,   628,   628,   628,   490,   687,   690,   691,
     720,     0,   424,     0,     0,   490,     0,     0,   345,     0,
       0,     0,     0,     0,   189,     0,   340,   341,     0,     0,
     345,     0,   393,   392,     0,   164,   164,   399,   589,   595,
     206,   490,     2,   190,     0,   217,   191,   192,   490,   211,
     193,   194,   195,   196,     0,   197,   198,   346,     0,   360,
     199,   366,   368,   371,   200,   201,   202,   203,   204,     0,
     205,   213,   572,   490,     0,   215,     0,     0,     0,     3,
     490,   818,   805,     0,   793,   794,     0,     3,   789,     3,
       3,     0,   490,   768,   768,  1078,  1083,     2,   105,   490,
       3,   587,     3,   491,  1039,   849,  1038,  1041,   490,     3,
    1027,  1033,   671,  1113,     0,   671,   676,   671,     0,   681,
    1113,     2,   853,   860,     0,    98,   101,   490,   490,     0,
     104,   100,   102,   490,     0,   116,     0,     0,     0,   120,
     124,   123,   188,     0,     0,     0,   735,   113,   181,     0,
       0,    46,    47,    87,     0,    87,    87,     0,    75,    77,
      49,     0,    45,     0,    48,   159,     0,     0,     0,     0,
    1113,  1004,   849,  1003,  1006,   998,     0,     0,   892,   910,
       3,     3,     0,   849,   974,   977,   849,     0,   849,   849,
     969,   986,     0,     0,  1108,     0,   712,   490,     0,  1110,
     490,   490,     0,     0,   438,     3,     0,     0,     0,     0,
     734,   739,     3,   831,   186,   185,     3,     0,   490,   876,
     879,   881,   490,   933,   936,   938,   490,   956,   958,   960,
       0,     0,     0,   729,   849,  1019,  1018,     0,     0,     0,
       0,     0,     3,   805,   841,     0,   490,   490,   490,   490,
     490,   490,   490,   611,   641,     0,     0,   642,   572,   629,
       0,     0,     0,   422,    69,     0,     0,   331,   332,   214,
     216,   490,     0,     0,     0,   490,   490,   327,     0,   325,
       0,     0,     0,   735,     0,     0,     0,     0,   372,   490,
       0,   165,     0,     0,   400,     0,     0,     0,   221,     0,
     212,     0,   322,     0,     0,     0,   345,   345,   351,   350,
     345,   362,   361,   345,   345,     0,   572,   849,     0,  1023,
    1022,     0,     0,   760,   796,     2,   791,     0,   792,     0,
     772,   752,   756,   754,   490,     0,     0,     0,     3,   490,
    1034,  1036,  1037,     0,     0,   105,     0,     3,     0,     0,
     671,  1113,     0,     0,   660,     0,   675,     0,   785,   490,
      72,  1024,     0,     0,     0,    39,     0,   117,   119,   118,
     115,   114,   735,  1113,     0,    68,    84,     0,    78,    85,
      86,    63,     0,     0,     0,    59,     0,   158,   429,    31,
       0,   490,   999,  1001,  1002,   893,   911,     0,     0,   849,
     490,   970,   972,   973,   490,   987,   989,     0,   964,   979,
     975,   990,  1109,   702,   452,   697,   696,   698,  1112,  1111,
       0,     3,   846,   745,   746,     0,     0,   849,     0,     0,
       0,   884,   941,   949,   471,   848,     0,  1014,     0,  1015,
    1016,  1010,   822,     2,     0,   824,   611,   611,   611,   642,
     649,   616,     0,   655,   642,     0,   490,   603,   640,   636,
       0,     0,     0,     0,   643,   645,   849,   657,   657,   657,
       0,   637,   653,   425,     0,   335,   336,   333,   334,   230,
       0,     0,   232,   433,   231,   572,   490,     0,     0,   345,
       0,   313,   312,   314,     0,   345,   189,   270,     0,   263,
       0,   189,   328,   326,     0,   320,  1113,   329,     0,     0,
       0,     0,   381,   382,   383,   384,     0,   374,     0,   375,
     337,     0,   338,     0,     0,   365,   210,   324,   323,     0,
       0,   354,   364,     0,   345,   367,     0,   369,   391,     0,
     423,   849,   820,   751,   773,   490,     2,     2,  1086,  1087,
    1088,     0,     0,     3,     3,     0,  1047,     0,   671,   661,
    1113,     0,   681,   681,   735,   682,   664,     3,     0,  1025,
     103,     0,    32,   490,     0,  1113,     0,     0,    88,     0,
      76,     0,    82,     0,    80,    44,   163,     0,   849,   849,
       0,     0,   748,     0,   446,   450,   885,   942,   950,     0,
       0,   788,   826,   607,   609,   605,     0,     0,  1054,     0,
     650,  1059,   652,  1051,   849,   849,   635,   656,   639,     0,
     638,     0,     0,     0,   659,     0,   631,   849,   630,   646,
     658,   647,   648,   654,   345,   345,   233,   572,     0,     0,
     251,   345,   318,   316,   319,   315,     0,   317,     0,   259,
       0,   189,     0,   345,   490,   271,     0,   296,     0,     0,
     321,     0,     0,   345,   344,   345,   385,     0,   376,     2,
       0,     0,     0,   347,     0,     0,   345,     0,   345,   345,
     345,   208,   207,   449,   768,   790,     0,     0,  1089,  1040,
       0,     0,  1046,  1048,  1113,     0,   663,   678,   677,  1113,
       2,    50,    42,    40,    41,     0,    66,   182,    79,     0,
       0,  1005,   448,   447,   976,   991,   747,  1009,  1017,   633,
       0,     0,     0,  1055,  1056,   849,   634,  1052,  1053,   632,
     612,     0,     0,   343,   222,     0,     0,     0,   244,   345,
     224,     0,     0,   345,   253,   268,   279,   273,   345,   189,
       0,   310,     0,   283,     0,   308,     0,   274,   272,   261,
     264,     0,     0,   189,   297,     0,     0,   227,   342,   373,
       2,   490,   339,     0,     0,   401,   352,     0,    69,   363,
     356,     0,   357,   355,   370,   759,   795,   797,  1049,  1050,
       0,   667,     0,   787,    67,    83,    81,   849,  1062,  1064,
    1057,     0,   644,   345,   239,   234,   237,     0,   236,   243,
     242,     0,   490,   246,   245,   345,   255,     0,   252,   345,
       0,     0,     0,   260,   265,   311,     0,     0,   189,   284,
     309,     0,     0,   345,     0,   299,   300,   298,   267,   330,
       0,   490,   490,     3,   386,   491,   390,     0,   394,     0,
       0,     0,   402,   403,   348,     0,     0,     0,   666,   683,
     490,  1058,  1060,  1061,   651,   223,     0,   241,     0,   240,
     226,   247,   490,   414,   256,   345,   257,   254,   269,   282,
     280,   276,   288,   286,   287,   285,   266,   281,   277,   278,
     275,   262,     0,     0,     0,     0,   229,   247,     3,   379,
       0,  1054,   387,   388,   389,   401,     0,     0,     0,   401,
       0,   353,   349,   345,     0,     0,   235,   238,   345,     3,
     248,   415,   258,     0,     0,     0,     0,   307,   305,   302,
     306,   303,   304,   301,     3,   379,     0,     0,  1055,     0,
       0,     0,   395,     0,   404,   358,   345,  1063,   218,     0,
       0,   345,   295,   293,   290,   294,   291,   292,   289,     0,
       0,   380,     0,   407,     0,   405,     0,   407,   359,   220,
     219,   225,     0,   228,     0,   377,   408,     0,     0,   396,
       0,   378,     0,     0,     0,     0,   409,   410,     0,   406,
     397,     0,     0,   398,   411
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1823,  7387,  6501, -1823,    -1,   382,  1771,  -106, -1823,  -350,
   -1823,   392, -1823,  -715, -1823,   845,  -902, -1089, -1823,   235,
    3251,  2037, -1823,  -392, -1823,  1439,   127,   835,   837,   661,
     838,  1400,  1401,  1399,  1402,  1405, -1823,  -167,   -80,  8634,
     940, -1823,  1732, -1823, -1823,  -717,  8061, -1155,   633, -1823,
    1558, -1823,   946,    24, -1823, -1823,   708,   117, -1823, -1735,
   -1616,   325,    91, -1823, -1823,   706,   342,   243, -1613, -1823,
   -1186, -1823, -1823, -1823, -1823,   143, -1135, -1823, -1823, -1242,
     468, -1823, -1823, -1823, -1823, -1823,    97, -1213, -1823, -1823,
   -1823, -1823, -1823,    67,   486,   489,   165, -1823, -1823, -1823,
   -1823,  -874, -1823,    99,    43, -1823,   170, -1823,   -33, -1823,
   -1823, -1823,   936,  -757, -1036,  -170, -1823,    16,    58,   238,
    4791,  -868,  -749, -1823,  -131, -1823, -1823,    64, -1823,  -156,
    8797,  -236,  -252,  3096,  1131,  -688,    13,   156,   544,   563,
    2876, -1823, -1823,  2164, -1823,   477,  4897, -1823,  2100, -1823,
     118, -1823, -1823,  1601,   555,  5598,  4200,   -28,  1949,  -326,
   -1823, -1823, -1823, -1823, -1823,  -370,  6373,  5849, -1823,  -394,
     203, -1823,  -674,   287, -1823,   219,   770, -1823,   -54,  -235,
   -1823, -1823, -1823, -1823,  -168,  6723,  -938,   907,  -143,   736,
   -1823,  -560,  -140,    52,   158,  2288,  -796,  -147,   957,  3755,
      63,  -497,  -260,  -194,  -512,  1361, -1823,  1705,   646,  -945,
    1589, -1823, -1823,   711, -1823, -1260,  -174,  -228,  -926, -1823,
     368, -1823, -1823, -1140,   493, -1823, -1823, -1823,  2231,  -748,
    -454,  -970,   -14, -1823, -1823, -1823, -1823, -1823, -1823,   162,
    -898,  -219, -1822,  -161,  8642,   -72,  6925,  -124,  1539, -1823,
    2235,   266,  -216,  -209,  -187,    26,   -71,   -63,   -45,   449,
     -13,    31,    53,  -183,   272,  -175,  -159,  -139,   295,  -133,
    -132,  -126,  -779,  -741,  -700,  -694,  -739,  -123,  -685, -1823,
   -1823,  -716,  1443,  1449,  1452,  2430, -1823,   613,  7626, -1823,
    -644,  -554,  -552,  -548,  -752, -1823, -1717, -1764, -1743, -1727,
    -635,  -158,  -308, -1823, -1823,   -53,   167,  -108, -1823,  8401,
    2224,  -468,  -602
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   848,   426,   427,    83,    84,   428,   402,   429,
    1564,  1565,   430,   999,  1000,  1001,  1357,  1358,  1359,  1576,
     452,   432,   433,   434,   731,   732,   435,   436,   437,   438,
     439,   440,   441,   442,   443,   444,   445,   454,  1145,   733,
    1492,   794,   223,   796,   448,   869,  1243,  1244,  1245,  1246,
    1247,  1248,  1249,  2149,  1250,  1251,  1680,  2006,  2007,  1938,
    1939,  1940,  2119,  2120,  1252,  1698,  1699,  1955,  1700,  1847,
    1848,  1253,  1254,  1255,  1256,  1257,  1258,  1874,  1878,  1514,
    1506,  1259,  1260,  1513,  1507,  1261,  1262,  1263,  1264,  1265,
    1266,  1267,  1717,  2137,  1718,  1719,  2043,  1268,  1269,  1270,
    1495,  2051,  2052,  2053,  2177,  2188,  2071,  2072,   308,   309,
     936,   937,  1211,    86,    87,    88,    89,    90,  1683,   488,
      93,    94,    95,    96,    97,   237,   238,   311,   290,   490,
      99,   491,   100,   611,   102,   103,   157,   357,   314,   107,
     108,   109,   173,   110,   954,   358,   158,   113,   261,   114,
     159,   269,   360,   361,   362,   160,   449,   119,   120,   364,
     121,   607,   929,   927,   928,  1657,   365,   366,   124,   125,
    1206,  1459,  1663,  1664,  1809,  1810,  1460,  1652,  1829,  1665,
     126,   695,  1312,   169,   989,   367,   990,   991,  1556,   962,
     613,  1137,  1138,  1139,   614,   368,   499,   500,   616,  1274,
     458,   459,   224,   517,   518,   519,   520,   521,   345,  1293,
     346,   952,   950,   646,   347,   387,   348,   349,   460,   128,
     179,   180,   129,  1287,  1288,  1289,  1290,     2,  1193,  1194,
     637,  1281,   130,   335,   336,   271,   282,   590,   131,   227,
     132,   326,  1147,   919,   551,   171,   133,   397,   398,   399,
     134,   328,   241,   242,   243,   329,   136,   137,   138,   139,
     140,   141,   142,   246,   330,   248,   249,   250,   331,   252,
     253,   254,   834,   835,   836,   837,   838,   255,   840,   841,
     842,   799,   800,   801,   802,   552,  1186,  1438,   143,  1768,
     670,   671,   672,   673,   674,   675,  1812,  1813,  1814,  1815,
     660,   501,   372,   373,   374,   461,   215,   145,   146,   147,
     376,   861,   676
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   858,   383,    82,   196,   198,   388,  1294,   310,   572,
     371,   529,   355,   199,   104,   712,   587,  1510,   446,   533,
     153,   369,  1032,  1291,   971,   736,   534,   135,  1497,   523,
     678,   200,   214,  1532,  1533,   318,  1042,   569,  1275,  1050,
     756,   757,   977,  1920,  1025,   183,  1313,   384,   535,  1129,
     205,  1131,   536,  1111,  1320,    82,    82,  1442,    82,    91,
     537,   756,   688,   201,  1921,    98,   691,   496,   972,  1484,
     463,   742,   104,   910,   912,    82,   538,   549,   401,   554,
    1922,  1115,  1085,  2067,    82,   135,   562,  1122,  1859,  1105,
    -798,  1924,    82,  1112,   756,  2009,   539,    82,   483,   214,
      82,   148,   540,   541,    82,   447,  1496,   202,   533,   542,
     963,   212,   964,  2008,   310,   534,   965,    91,   655,   115,
     650,   276,  1229,    98,   244,   465,   987,   272,   916,   203,
    1106,   283,  1723,   466,   686,   321,  1107,   535,   689,   924,
    2015,   536,   310,    82,  1361,  1108,    82,   400,    82,   537,
    1880,   467,  -799,   310,    82,  2075,   776,   105,   626,   628,
     650,    82,   104,   531,   198,   538,   678,  1505,   144,    82,
    1454,   144,   199,  1368,  1552,   135,  1201,   115,   620,  1271,
     302,   396,   471,   468,   945,   539,  2049,  1734,   971,  1721,
     200,   540,   541,   396,    82,    82,   661,  1282,   542,   777,
      58,   225,   994,  1724,  2014,    82,  2000,    91,   161,   693,
      82,   579,   558,    98,  1009,   105,   697,  1935,  1936,    20,
     514,   212,   201,   280,  1441,    82,   144,   469,    82,    82,
      82,  1445,  1116,   162,    82,    82,  1119,   505,  1956,    92,
     584,  1405,   154,  2068,  2069,  2016,  2008,  1134,  1135,   470,
     655,   595,   631,   198,   694,    82,  2010,   115,  1583,   497,
    2076,   199,   212,    82,    75,  1578,   202,   115,    58,  1722,
     602,   701,   144,    82,    82,  -830,   963,    82,   964,   200,
     579,   988,   965,   302,    82,   678,   380,   550,   203,   167,
     212,  1584,  1140,  1141,   902,   313,   627,    92,    82,    82,
     298,    82,   906,  1937,   591,   105,    82,  -983,  1089,   182,
      82,  1156,  1522,  1406,  -983,   874,   144,  1470, -1114,  1350,
     667,  1555,   875,    82,    82,   862,  1311,  2103,   661,   988,
     678,  1390,    75,    82,   380,  1341,  1920,   622,  1455,   265,
      58,    82,    82,   277,   876,   958,    82,  1407,   877,   908,
    1275,   212,  1115,   548,   678,   913,   878,  1921,  1048,  1006,
     184,   678,  1105,   115,   240,   498,  1052,   839,  1375,   220,
     247,  1391,   879,  1922,   824,  1733,    82,  1551,  1496,  1736,
     221,  1279,   982,  2014,  1924,    82,   151,    92,    82,    58,
    1148,   115,   880,   251,   396,   483,   222,   225,   881,   882,
     214,   313,   115,  1106,    75,   883,   557,   627,   185,  1107,
    1454,  1454,  1454,   565,  1876,   874,  2014,   956,  1382,  1767,
     193,  2000,   875,   168,   193,  2066,  -662,   115,  -608,   313,
    1504,   280,   194,  -662,   583,    63,    64,   496,   177,   177,
     313,   976,  1935,  1936,   876,   594,  1465,  1466,   877,  1877,
    1443,   766,   767,    75,   981,  2118,   878,  1456,  1469,  1407,
     465,   310,  1435,    82,  1508,   313,  1311,  1198,   466,   226,
    1027,  -830,   879,  1644,    58,   177,   550,   371,   111,   355,
    2022,  2118,   737,   205,  1436,    78,   467,  1509,    82,    82,
     505,  1271,   880,  1780,  1782,  1784,   768,   769,   881,   882,
      82,    82,   544,   163,  2151,   883,   164,   165,   545,   166,
      82,  1852,   514,   471,   496,   550,   588,   958,   468,   661,
    1511,  1497,  1318,  1027,   206,  1396,   177,   986,  1966,   177,
      82,   546,    58,  2032,   213,   934,   111,   303,    75,  1380,
    1381,  1850,    82,  1512,   177,   106,  1858,   245,   303,   963,
     273,   964,   381,  1541,   284,   965,   116,   559,   218,   923,
     465,   550,   469,   643,  1508,  1467,  1370,    82,   466,   505,
    2113,    58,  1027,    82,  1364,    82,    -3,   240,  1455,  1455,
    1455,  1324,  2093,   247,   470,  1229,   467,  1509,   480,   956,
     303,   544,   644,   645,   229,  1416,    75,   545,  1462,  1496,
    1464,  1613,   229,   106,  1129,  1131,   623,  1675,   177,  1725,
     303,   528,  1760,   530,   116,   634,   111,  1463,   230,   550,
     546,  1658,  1335,   944,  1985,   191,   111,  1677,  1339,   497,
      82,   230,    82,   678,  1606,    75,    82,  1659,    82,  1347,
     193,   496,  1670,  1462,   213,   267,  1586,    82,  -984,  1296,
     104,    82,   231,   596,   813,  -984,   177,   177,   550,   505,
     506,  1671,  1740,   135,   977,  1826,  1149,   177,   608,   286,
    1961,  1962,  1827,  1074,   287,  1779,   446,   291,  1179,   296,
     232,   605,   177,    82,   610,   213,   496,   943,  1817,   151,
    1144,  1828,  1670,   106,   116,    91,  1954,  1456,  1456,  1456,
    1132,    98,   396,  1853,   116,   256,   497,  1818,  1854,   839,
     115,  1820,  1027,   213,  1681,  1827,   177,  1925,  1681,  1701,
     904,  1295,   111,  1827,   177,   177,   193,   592,   275,   177,
    1821,  1860,  1701,  -723,  1919,   498,  1926,  1027,    58,   743,
    2057,  -484,  1929,    82,   744,    82,   918,    82,   313,  1674,
     111,    82,   602,   922,    82,   115,  2020,   926,   298,   692,
     300,   111,  1027,  1126,  1027,  1027,  2026,  2027,   177,  1709,
    1004,   177,  2095,  1008,  2124,  2126,  1010,  1345,   302,    82,
     303,  1842,  1843,  1844,  1346,  1013,   111,    58,  1015,  1016,
    1017,   322,  1180,   105,  2024,  1531,   745,  -819,  1884,   386,
     116,   746,    75,  1845,   144,  1842,  1843,  1844,  2038,   894,
     960,   759,   498,   204,    64,   895,    74,  1314,   760,   761,
    1894,  1090,   589,   712,    82,   550,   343,  1845,   116,    82,
     286,    82,   389,   497,  1573,  1575,  1846,   797,   896,   116,
     400,   550,  1324,    82,    58,  1549,   647,   736,    80,    81,
     648,    75,  1557,    82,    58,  -485,   267,  1777,   472,   514,
     473,   371,    82,   355,   116,    14,    15,    16,    17,    18,
    1113,   177,   503,  2086,   665,    92,    58,   286,   497,  1909,
    1865,  1910,  1404,   177,   177,  1854,  1053,  1054,  1055,    14,
      15,    16,    17,    18,   601,    64,    82,  1128,  1974,   894,
     267,   474,  1590,  1975,  1365,   895,    58,   756,    75,   475,
     483,   476,    58,   506,    58,    58,   163,  1769,    75,   164,
     165,   287,   166,   682,    58,   296,   702,  1120,   896,   504,
     703,   665,    58,    82,    82,   514,   477,  1167,  2108,   498,
      75,   550,  1020,  2109,  1567,  1568,  1569,   508,    58,   104,
      58,  1570,  1571,  1021,  1022,  -486,  1411,  2166,   480,  1171,
     509,    58,  2167,   550,  1284,    14,    15,    16,    17,    18,
      75,  1706,   522,   720,   524,  1806,    75,   678,    75,    75,
    1819,   527,   960,   547,   498,  1144,  1389,   839,    75,  1175,
      82,   548,   506,   550,    91,  1418,    75,  1422,  1426,   550,
      98,   550,   550,   498,    58,   498,    58,   651,   298,   498,
     498,   498,    75,  -488,    75,  1539,   778,   570,  1433,   665,
     779,   177,   762,   763,    58,    75,    74,   863,   864,   498,
     177,   865,   698,  1591,   955,   700,   267,   550,   612,    82,
     764,   765,   804,   571,  1600,    82,   805,   664,   550,   817,
     582,   665,   225,   550,   115,   576,  1543,  1651,    80,   666,
    1775,   806,   593,   976,   617,   703,   980,  1524,    75,   111,
      75,   943,   638,   720,    82,   639,   302,   514,   471,    74,
     550,   621,   559,  1761,   887,  1200,   550,  1604,    75,  2060,
    1208,   665,  1273,   550,   634,   653,   471,  1701,   550,   498,
    1807,   685,    82,   144,   550,  1776,    74,   380,    82,    82,
     696,    80,    81,   933,   111,   144,   705,   934,   706,   388,
     388,  1113,   371,   471,   355,   665,  1036,   664,  1038,   704,
    1041,   665,   993,  1132,  1047,   709,   648,  1051,    80,   666,
     699,   661,    82,   739,  1548,   995,   996,   116,   710,   648,
     703,   667,   286,  1461,   711,   267,   715,    14,    15,    16,
      17,    18,  1076,  1842,  1843,  1844,   770,   771,   589,   758,
     371,  1561,   355,  1634,    92,  1027,   257,   258,  1026,   259,
     772,   106,  1027,   773,   260,  1845,  1285,  1712,  1713,  1714,
    1715,  1716,   116,   190,  1851,   775,  1094,   503,   739,  1587,
     550,    14,    15,    16,    17,    18,   774,   480,    14,    15,
      16,    17,    18,  1284,  1153,   302,    58,   514,  1154,   550,
      82,    82,    82,   559,   589,   780,   177,   550,   104,   514,
    1189,  2139,   266,  1191,  1027,  2143,   177,  1027,  1861,   446,
     446,   634,   825,   739,   288,   550,   295,   514,   297,   807,
    1132,   808,  1899,    82,  1132,  2073,   809,   498,   498,   810,
      58,   104,  1323,  2055,   811,  1360,  1324,    58,    82,  1324,
    1530,    82,    82,    91,   805,    82,  1803,  1804,  1805,    98,
      75,  -489,    82,   812,  2073,    82,   479,   266,   820,   276,
     295,   297,  1895,  1772,    -3,   905,  1834,  1773,   272,   283,
    1324,  1682,  1018,   739,   737,  1682,    91,  1905,  1842,  1843,
    1844,   822,    98,   498,   845,  2121,  1210,   177,   177,  1838,
    1027,  1862,    74,  1027,    75,  1027,  1621,  1622,    82,   843,
    1845,    75,  1863,   115,  1864,  -487,  1154,  1873,  1027,  -190,
    1684,   847,   266,   664,  1684,   514,  1930,   665,  1029,  1030,
     805,   860,   150,    82,    80,   666,    65,    66,    67,    68,
      69,    70,    71,    72,   144,  1979,   115,   -18,  2017,  1027,
     612,  1273,  1027,   177,   859,  2111,   371,  2112,   355,  1324,
     943,  1027,   144,  2185,   870,  2191,    82,  2182,   446,  2192,
     884,   280,   872,  1461,  1461,  1461,  1566,   898,  1653,  1461,
     653,   739,    77,   899,  1273,   855,  1446,  1447,  1448,   266,
     885,   295,   297,   111,   150,   144,   378,   379,    65,    66,
      67,    68,    69,    70,    71,    72,  1990,  1060,  1061,  1062,
    1063,  1992,  1830,  1830,  1830,  1285,  1348,  1154,  1362,  1363,
     144,  1027,  1366,   266,   533,  -161,  -161,   886,   266,   206,
     739,   534,  1616,    92,   266,   888,    82,  1504,  1505,   900,
      82,    82,  1581,  1582,   889,   153,    78,  -126,  -126,  -126,
    -126,  -126,  -126,   535,   890,  1765,   891,   536,   380,   892,
     106,   893,   514,   920,   104,   537,    92,   266,   104,   104,
     315,   116,   683,   921,   297,  -125,  -125,  -125,  -125,  -125,
    -125,   538,   104,   267,  -606,   514,   514,  1585,  1582,  -604,
     265,   277,  1589,  1582,   930,    82,  1102,  1574,  1623,  1574,
     931,   539,  1102,  1636,  1785,  1154,   932,   540,   541,  1907,
    1154,  1908,  1582,  1080,   542,    14,    15,    16,    17,    18,
    1024,   719,   591,    14,    15,    16,    17,    18,  1340,  1097,
    1917,  1027,    82,  1098,   935,   498,  1977,  1978,   498,   498,
     186,     6,     7,     8,     9,    10,    11,    12,    13,  1995,
    1582,   514,  1996,  1582,   938,  1284,    82,  1935,  1936,  2182,
    2183,    82,    82,    82,   947,  2054,   943,   949,  1822,   115,
    1579,  1580,   953,   115,   115,   266,   966,  1056,  1057,   968,
     177,  1058,  1059,   177,   177,   177,   874,   115,  1064,  1065,
    1735,  1737,   177,   875,  1831,  1832,   144,   667,  1872,  1897,
    1898,   285,   984,   266,   992,   683,   297,  1686,  1003,  1028,
     177,  1686,  1686,  1031,  1034,   876,   177,  1073,   144,   877,
    1078,   719,   144,   144,  1109,  1686,  1101,   878,    82,  1102,
    1130,   610,  1133,    82,  1158,  1151,   144,  1181,   177,    82,
    1159,    82,  1160,   879,  1161,   177,  1162,  1163,  1987,    82,
    1164,   150,  1165,   914,   266,    65,    66,    67,    68,    69,
      70,    71,    72,   880,  1166,  1188,  1190,   154,   514,   881,
     882,  1192,   111,   304,   514,   825,   883,  -802,  1196,   266,
    1203,  1204,   276,  1205,   266,    19,   266,  1276,  1280,    92,
     388,  1562,  1283,    92,    92,  1292,  1304,  1305,  1306,   678,
    1307,   273,   284,  1667,  1322,   111,   144,    92,   266,  1668,
     266,   266,  1315,  1317,  1321,  2044,  1325,  1326,   514,  1327,
    1410,  1330,   266,  1018,    48,    49,    50,    51,    52,    53,
      54,    55,  1669,  1329,   588,   266,  1331,  1332,  1333,   106,
    1334,  1284,  1336,   514,   266,  1973,  1337,   150,   446,  1338,
     116,    65,    66,    67,    68,    69,    70,    71,    72,  1039,
      82,  1343,    82,  1351,  1344,   526,  1367,   266,  1352,   683,
     297,  1371,   106,  1372,  1397,  1373,  1374,  1285,  1378,  1379,
    1383,  1384,  1385,   116,   280,  1386,  2044,  1394,   494,  1399,
    1400,   266,   683,  1434,  1401,  1439,   267,  -803,   266,  1471,
    1040,    82,  1472,  1475,    82,  2102,  1376,  1476,  1485,  1486,
    1377,  1487,  1489,   514,   514,  1494,  1566,  -722,  1498,  1027,
     514,  1517,  1519,   177,   177,  1520,  1526,   589,  1554,  1392,
    1528,  1474,   514,   144,  1550,  2005,  1393,  1558,  1572,  1555,
    1574,  1588,   514,  1488,   514,  1595,   619,   104,  1596,  2050,
      14,    15,    16,    17,    18,   514,  1599,   514,   514,   514,
    1610,  1611,  1612,  1618,   533,    82,    82,  1614,   177,   177,
    1619,   534,  1582,  1627,  1624,  1631,  1632,  1633,  1642,   446,
    1645,   446,  1641,  1430,  1656,  1464,  1688,  1431,  1702,  1703,
    1705,  1432,   144,   535,  1505,  1667,  1726,   536,  2046,  1729,
    1667,  1668,  1730,   265,   277,   537,  1668,  1707,   544,    58,
    1229,    82,  1720,  1727,   545,   177,  1728,  1741,   514,  1742,
     446,   538,   514,  1744,  1669,  1746,  1748,   514,   111,  1669,
    1758,  1747,   111,   111,  1749,   658,  1750,   546,   681,  1756,
    1759,   539,  1766,  1771,  1770,   592,   111,   540,   541,  2161,
    1778,   658,   115,  1774,   542,   658,  1788,   735,  1786,  1661,
    1789,   226,   471,  1285,  1816,  1794,  2116,  1792,  2005,  2046,
    1623,    74,   515,    75,  1796,  1797,  1798,  1801,  1802,  2050,
    1839,  1869,   514,  2050,  2050,  1835,  1841,  1871,  1889,  1896,
    1686,   446,   797,  1888,   514,   106,   550,  1901,   514,   106,
     106,   144,  1892,    80,    81,   104,   116,  2141,   631,   198,
     116,   116,   514,   106,  2164,  1893,  1906,   199,    85,  1916,
    1911,   152,  1914,    82,   116,    82,  1915,  1943,  1948,  1949,
    1963,  1965,   177,  1970,   104,   200,  1972,  2176,   150,  1976,
     589,  2176,    65,    66,    67,    68,    69,    70,    71,    72,
    1981,   177,  1988,  1989,   514,   588,  2186,   177,  1991,  1993,
    1994,   550,  2003,  2019,  1997,   104,  1998,   658,  1999,  2021,
     266,   894,    92,  2025,  -589,  2033,    85,   895,  2184,    82,
      82,   266,  2047,  2028,  2031,  2058,  2079,  1387,    77,  2059,
     266,  2070,   514,   195,  2039,   903,  2092,   514,  2048,  2094,
     896,   177,    85,   907,  2096,  2105,  2107,   212,  2106,  2110,
     115,  2123,  2125,  2127,  2136,   236,  2140,    82,   264,  1731,
    1732,   917,    85,  2146,  2142,   514,  2147,  2152,   514,  2162,
     514,  2165,   925,  2171,  1628,  2163,  2173,  2174,  1629,   115,
    2178,  2179,  1630,  2189,   505,  1903,  2190,  2193,  1686,  1560,
    1023,   514,  1066,  1068,  1067,  2172,  1493,  1069,   494,   144,
     152,  1070,    82,  1690,   795,  2117,    85,  1967,  2134,   152,
     115,    82,   325,   333,  1500,  1711,  1960,  1686,  1667,  2030,
    2114,  1879,  2160,  1867,  1668,   354,  1868,  2098,   144,  2144,
    2180,  2097,  1518,   174,   293,   266,   177,   177,   581,  2002,
    2064,  1150,  1655,   177,  1553,   866,  1515,  1669,  1686,   267,
     453,     3,   195,   195,  1743,   177,   494,   951,  1885,   144,
    1081,   266,   997,   152,   486,   177,  1082,   177,   264,  1083,
      92,  1800,     0,     0,   658,   494,     0,     0,   177,     0,
     177,   177,   177,     0,     0,   735,     0,   325,   177,     0,
       0,   735,   236,   236,     0,  1752,     0,     0,   658,    92,
     735,     0,   856,     0,   515,     0,     0,     0,     0,     0,
       0,   658,     0,   325,     0,     0,     0,     0,     0,   735,
       0,    85,   747,     0,   748,   749,   750,     0,     0,   217,
      92,   197,     0,     0,     0,   264,     0,     0,     0,     0,
       0,   177,  1840,     0,     0,   177,     0,  1787,  1849,     0,
     177,   111,     0,   239,   751,     0,  1790,   752,   753,     0,
    1791,     0,   754,   755,     0,     0,     0,     0,   325,     0,
     390,     0,     0,     0,   333,     0,     0,     0,     0,     0,
     333,   325,   325,     0,     0,     0,     0,   544,     0,     0,
     152,     0,  1882,   545,     0,     0,   217,     0,     0,     0,
       0,   589,   494,     0,     0,   177,     0,     0,     0,     0,
     327,   354,   668,   677,     0,     0,   546,   177,   106,     0,
       0,   177,     0,   266,   894,     0,     0,   354,     0,   116,
     895,   354,     0,     0,     0,   177,     0,   462,     0,     0,
       0,     0,     0,     0,     0,   391,   658,   494,  2104,     0,
       0,     0,     0,   896,   266,     0,     0,     0,     0,     0,
     266,     0,     0,   392,     0,   393,   394,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   453,   177,     0,     0,
       0,     0,  1187,     0,     0,   327,     0,  1933,  1934,     0,
     532,   239,     0,     0,  1944,     0,  1195,     0,     0,     0,
    1199,     0,     0,     0,  1202,     0,  1959,     0,   580,   111,
     453,   327,   395,   798,     0,   177,  1968,     0,  1969,     0,
     177,   195,     0,    14,    15,    16,    17,    18,     0,  1980,
       0,  1982,  1983,  1984,     0,     0,     0,     0,   111,   152,
       0,     0,     0,   486,     0,     0,     0,   832,   177,   677,
       0,   177,     0,   177,     0,     0,     0,     0,     0,     0,
     152,     0,     0,     0,     0,     0,   327,     0,     0,   111,
       0,     0,     0,     0,   177,     0,   106,   580,   658,   632,
     327,   681,    58,     0,     0,  2187,     0,   116,   236,     0,
       0,     0,  2013,     0,  2194,     0,  2018,     0,   663,     0,
     236,  2023,     0,     0,     0,   106,     0,     0,     0,   266,
     615,     0,     0,   150,     0,     0,   116,    65,    66,    67,
      68,    69,    70,    71,    72,   325,     0,   453,   453,     0,
       0,   325,     0,     0,   354,     0,   106,     0,     0,     0,
     494,     0,     0,     0,    74,     0,    75,   116,     0,     0,
       0,   515,     0,     0,   856,     0,  2065,     0,     0,     0,
       0,     0,     0,     0,     0,  1807,     0,   266,  2074,   550,
    1501,     0,  2077,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,  2091,     0,   325,     0,
     325,     0,   354,     0,    85,     0,   561,   217,   150,     0,
     175,   176,    65,    66,    67,    68,    69,    70,    71,    72,
     354,   486,     0,   677,     0,     0,     0,     0,     0,     0,
       0,   668,     0,     0,     0,   668,     0,     0,  2122,     0,
     663,     0,     0,     0,   354,   186,     6,     7,     8,     9,
      10,    11,    12,    13,   677,   833,     0,   354,     0,     0,
       0,    19,     0,     0,     0,     0,   152,     0,     0,     0,
       0,     0,     0,     0,   453,     0,  2145,   152,   152,     0,
     453,  2148,  1502,     0,     0,     0,     0,   390,  1444,   453,
       0,     0,   152,   152,   152,     0,   873,     0,     0,     0,
       0,     0,  1468,     0,    52,    53,    54,    55,   239,  2168,
       0,     0,  2170,     0,  2148,     0,     0,     0,     0,     0,
       0,     0,  1490,     0,   462,   462,  2115,     0,     0,   735,
       0,     0,     0,   327,     0,  2170,     0,     0,     0,   327,
       0,  1356,     0,     0,     0,     0,   266,  1356,   486,   150,
       0,  1043,  1044,    65,    66,    67,    68,    69,    70,    71,
      72,  1045,   391,     0,   798,   798,     0,     0,     0,     0,
       0,     0,   453,     0,     0,     0,  1356,   615,     0,   515,
     392,     0,   393,   394,    65,    66,    67,    68,    69,    70,
      71,    72,   354,   486,     0,     0,   942,   832,   327,   832,
       0,     0,  1046,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   354,     0,   354,     0,   803,     0,   354,   354,
     354,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     658,   978,     0,   815,     0,   256,   818,     0,   354,     0,
       0,     0,     0,     0,  1356,     0,     0,     0,     0,     0,
       0,     0,   615,   462,     0,     0,     0,     0,     0,   494,
       0,  1005,     0,     0,   325,   462,     0,  1011,   192,     0,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,   615,   150,     0,     0,   266,    65,    66,    67,
      68,    69,    70,    71,    72,  1353,   561,     0,   266,  1354,
       0,  1355,     0,   453,     0,     0,     0,   268,   354,     0,
       0,     0,     0,     0,     0,   152,   453,     0,     0,   289,
     292,     0,     0,     0,   354,     0,  1299,     0,     0,    58,
       0,     0,     0,    77,     0,     0,  1577,   668,   150,     0,
     233,   234,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,  1676,  1678,     0,     0,     0,     0,   462,
       0,     0,   268,   150,     0,     0,    74,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   266,   152,   486,  1660,    77,     0,
       0,    74,     0,    75,  1661,  1104,     0,   833,    80,    81,
       0,     0,     0,  1738,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,   615,     0,   268,     0,     0,
       0,     0,     0,    80,    81,     0,     0,   101,     0,     0,
     156,   615,     0,     0,   150,   615,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   515,   615,     0,
       0,   798,     0,     0,     0,  1356,     0,     0,     0,     0,
       0,     0,   327,     0,     0,   494,   354,   354,     0,     0,
     832,     0,     0,     0,     0,     0,     0,   832,     0,     0,
       0,   337,     0,     0,   268,   101,     0,     0,     0,   338,
     339,   340,   341,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   462,     0,     0,   266,     0,  1477,     0,
       0,   211,     0,     0,     0,     0,     0,     0,   268,     0,
       0,     0,   354,   268,     0,     0,     0,     0,     0,   268,
     150,   278,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,    58,     0,     0,
    1209,     0,     0,     0,     0,     0,     0,   803,   803,     0,
       0,     0,   268,     0,   152,   312,     0,     0,  1092,   317,
       0,  1095,     0,   152,     0,   101,     0,     0,   323,     0,
       0,   150,   453,   342,   515,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   356,     0,     0,     0,     0,   266,
       0,   343,     0,     0,  1866,     0,     0,     0,   453,    74,
       0,    75,     0,     0,     0,   453,   721,     0,     0,   323,
       0,   464,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,   317,   492,     0,     0,     0,   561,     0,   264,
      85,    80,    81,     0,  1169,     0,     0,   354,  1173,     0,
       0,     0,  1177,   325,     0,     0,     0,     0,     0,   152,
       0,     0,   543,     0,     0,     0,   486,     0,  1104,     0,
       0,   312,     0,   515,  1388,   833,     0,     0,  1356,     0,
       0,     0,   568,  1356,  1356,  1356,     0,   573,   575,     0,
     211,     0,     0,     0,     0,   486,     0,     0,   268,   312,
     152,     0,     0,     0,     0,     0,     0,     0,   615,     0,
     312,     0,   615,   597,     0,     0,   721,   599,     0,     0,
       0,   615,   600,     0,     0,     0,     0,     0,     0,     0,
       0,   615,     0,   575,     0,   312,     0,   150,   615,   624,
       0,    65,    66,    67,    68,    69,    70,    71,    72,  1353,
       0,   633,     0,  1354,     0,  1355,     0,     0,     0,   323,
       0,     0,     0,     0,   354,     0,   431,   354,   354,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     656,   268,     0,   680,     0,   615,     0,    77,     0,   615,
    1781,     0,     0,   615,     0,     0,   687,     0,     0,     0,
     687,     0,     0,   268,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   152,   152,   152,   152,   268,   152,   152,
       0,     0,     0,     0,  1662,   333,     0,     0,     0,     0,
     268,     0,     0,    58,     0,     0,     0,     0,   453,     0,
       0,     0,   453,   453,   803,   323,   462,     0,     0,     0,
       0,   327,     0,   978,     0,     0,   453,     0,     0,     0,
       0,     0,   268,     0,     0,   515,     0,   150,     0,   233,
     234,    65,    66,    67,    68,    69,    70,    71,    72,   323,
       0,     0,  1356,   264,  1356,     0,   268,     0,     0,     0,
       0,     0,     0,   268,     0,     0,    58,    75,     0,     0,
       0,   486,   658,     0,     0,     0,     0,     0,   317,     0,
       0,     0,   656,     0,     0,     0,  1387,    77,  1420,     0,
       0,  1424,     0,     0,     0,  1428,   152,     0,   668,   317,
     150,     0,   233,   234,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1607,     0,     0,     0,     0,     0,    74,   150,
      75,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   658,     0,     0,     0,     0,     0,  2100,
      77,     0,     0,   550,   708,     0,     0,     0,   431,   714,
      80,    81,     0,     0,     0,     0,   323,   323,   723,   724,
       0,     0,     0,   492,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   431,   431,     0,  1662,  1808,   312,     0,
       0,  1662,  1666,   453,     0,     0,     0,  1662,     0,  1662,
       0,     0,     0,  1479,   431,     0,   615,     0,     0,     0,
     615,     0,     0,     0,   615,     0,     0,     0,     0,     0,
    2175,     0,   333,   152,     0,     0,     0,     0,     0,     0,
       0,   356,  2181,   101,     0,     0,   150,   431,   233,   234,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   687,
     959,   150,     0,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   970,     0,     0,     0,     0,     0,
       0,     0,     0,   656,     0,     0,     0,  1184,   979,     0,
       0,     0,   152,     0,     0,     0,   687,     0,     0,     0,
       0,     0,     0,     0,     0,   323,     0,     0,     0,     0,
     380,     0,  1593,   323,     0,     0,   323,   323,   641,   323,
     152,   178,   181,  1602,     0,     0,     0,     0,   323,     0,
       0,   323,   323,   323,     0,     0,     0,   615,     0,     0,
       0,     0,     0,     0,     0,   268,     0,     0,     0,     0,
       0,     0,     0,     0,  1808,  1808,     0,   150,   228,   601,
      64,    65,    66,    67,    68,    69,    70,    71,    72,  1662,
       0,     0,  1662,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,   333,     0,     0,   492,     0,   615,
     462,     0,     0,     0,  1666,     0,     0,     0,   615,  1666,
       0,   453,   615,     0,  1084,  1823,     0,  1666,     0,   319,
    1075,   323,   320,   150,     0,   233,   234,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,   344,     0,     0,
       0,   687,   959,     0,   325,     0,     0,     0,  1110,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,   492,     0,   492,     0,     0,     0,   492,   492,   492,
       0,     0,   235,    77,     0,     0,     0,     0,     0,     0,
    1808,     0,     0,    80,    81,     0,     0,   492,   150,  1662,
     175,   176,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   525,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   150,     0,   603,   604,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   152,   431,
     431,   431,   431,   431,   431,   431,   431,   431,   431,   431,
     431,   431,   431,   431,   431,   431,   431,   431,  1481,   585,
     586,   741,  1272,     0,    78,   420,     0,   492,  1808,     0,
     178,     0,     0,     0,   156,   323,    78,     0,     0,   152,
       0,     0,     0,   687,     0,   178,  1303,  1931,     0,     0,
    1666,     0,     0,  1309,     0,   150,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   152,   152,
    1811,  2101,   333,     0,     0,     0,     0,     0,     0,   636,
       0,     0,     0,     0,   431,     0,     0,   640,   642,     0,
       0,     0,   649,     0,     0,     0,     0,     0,     0,   152,
       0,     0,     0,     0,   317,   356,    58,    78,     0,     0,
       0,     0,   327,     0,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,  2101,  2101,     0,
       0,   344,     0,     0,   344,     0,     0,     0,   268,     0,
     150,     0,   233,   234,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,  1666,     0,     0,
       0,     0,     0,     0,     0,  2101,     0,     0,    74,   268,
      75,     0,   150,    58,   204,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   492,   492,     0,     0,   324,
      77,   118,     0,     0,   118,     0,     0,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,   150,     0,   233,
     234,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,    77,     0,     0,   855,     0,  1811,  1811,     0,
       0,     0,     0,     0,   228,    74,     0,    75,     0,     0,
       0,   492,     0,     0,     0,     0,   849,   850,     0,   118,
       0,     0,     0,     0,     0,   431,   830,    77,     0,     0,
     665,   431,     0,     0,     0,     0,     0,    80,   831,   327,
       0,     0,   431,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,   156,     0,     0,     0,     0,     0,     0,
       0,   270,  1458,     0,     0,   118,     0,     0,     0,     0,
       0,  1272,     0,     0,     0,  1639,     0,     0,     0,     0,
       0,   150,   431,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   268,   632,   327,   323,     0,   118,
       0,     0,     0,   118,  1272,     0,     0,     0,   615,   118,
       0,     0,   118,  1811,     0,     0,   270,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   350,   118,  1516,
     382,     0,     0,   327,     0,     0,   356,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   323,     0,
       0,     0,   268,   457,   946,   656,    58,     0,     0,     0,
       0,     0,     0,   344,   573,     0,   118,   457,     0,     0,
     150,   270,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,   356,     0,     0,  2062,     0,   323,
     150,  1811,   233,   234,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,   118,   150,     0,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,    74,   504,
      75,     0,   118,     0,   118,     0,   431,     0,     0,     0,
       0,     0,     0,   118,  1811,     0,     0,     0,   270,  1660,
      77,     0,     0,     0,   118,     0,     0,     0,     0,     0,
      80,    81,     0,   492,     0,   508,   492,   492,     0,   606,
       0,     0,   118,     0,     0,     0,     0,   118,     0,   118,
       0,     0,   270,   118,     0,     0,     0,   270,     0,     0,
       0,     0,     0,   270,     0,    14,    15,    16,    17,    18,
    1811,  1811,     0,   118,     0,     0,     0,     0,     0,     0,
       0,     0,  1458,  1458,  1458,   156,   575,   323,   323,     0,
     431,     0,     0,     0,   118,     0,   270,   118,     0,     0,
       0,   268,     0,     0,     0,     0,     0,  1685,  1811,     0,
     118,  1685,  1685,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,  1685,     0,   431,   431,   431,
       0,     0,     0,     0,   431,   431,     0,     0,     0,  1127,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1142,
       0,     0,     0,     0,     0,     0,     0,   431,   150,   457,
     233,   234,    65,    66,    67,    68,    69,    70,    71,    72,
     356,   150,     0,     0,     0,    65,    66,    67,    68,    69,
      70,    71,    72,  1353,     0,     0,    74,  1354,    75,  1355,
       0,     0,     0,   457,     0,   156,     0,   431,   431,     0,
       0,     0,     0,     0,     0,     0,     0,  2100,    77,     0,
       0,   550,     0,     0,     0,     0,     0,     0,    80,    81,
       0,    77,   118,     0,  1783,     0,   457,     0,     0,     0,
    1212,     0,   270,     0,     0,   262,     0,     0,     0,     0,
       0,     0,     0,   118,     0,    14,    15,    16,    17,    18,
       0,     0,    20,   268,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -491,
    -491,     0,  -491,    46,     0,    47,  1319,  -491,     0,     0,
       0,     0,   323,     0,     0,     0,     0,     0,  1825,   118,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
     457,   457,     0,     0,     0,   270,     0,   118,     0,   150,
       0,     0,  1837,    65,    66,    67,    68,    69,    70,    71,
      72,  1353,   118,     0,     0,  1354,     0,  1355,     0,     0,
      63,    64,     0,     0,     0,     0,   431,     0,     0,     0,
       0,   270,   150,     0,   233,   234,    65,    66,    67,    68,
      69,    70,    71,    72,   270,     0,    74,     0,    75,    77,
       0,     0,     0,     0,   118,   118,     0,   118,     0,     0,
      74,   156,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   332,   382,   118,   457,     0,   270,     0,    80,    81,
       0,   830,    77,     0,   118,   665,     0,     0,     0,   323,
       0,     0,    80,   831,     0,     0,   209,   118,     0,     0,
     270,     0,     0,     0,   606,   667,     0,   270,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,   112,   118,
       0,     0,     0,     0,  1923,     0,     0,   457,     0,     0,
     118,   118,     0,   457,     0,     0,     0,     0,     0,     0,
       0,   268,   457,     0,     0,   118,   118,   118,     0,     0,
     782,   783,   784,   785,   786,   787,   788,   789,   790,   791,
     792,     0,     0,   209,   220,     0,     0,     0,     0,     0,
    1685,     0,     0,     0,     0,     0,   112,     0,   150,   209,
     233,   234,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   793,     0,     0,     0,     0,  1478,  1480,  1482,     0,
     431,   457,     0,     0,   209,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   118,   489,     0,
       0,     0,   279,     0,     0,   457,     0,  1660,    77,  1503,
       0,     0,     0,   118,  1661,     0,     0,   118,    80,    81,
       0,     0,     0,     0,     0,   118,   457,     0,     0,     0,
     118,  1212,     0,     0,     0,     0,   112,     0,  1523,     0,
       0,     0,     0,     0,     0,   118,   112,   118,     0,     0,
       0,   118,   118,   118,     0,   209,   186,     6,     7,     8,
       9,    10,    11,    12,    13,   359,     0,  2045,     0,     0,
      19,   118,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,   257,
     258,  1185,   259,    46,   493,    47,     0,   260,  1685,     0,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,   209,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,     0,   457,  1685,  2045,     0,
       0,   118,   112,     0,     0,   209,     0,     0,   118,   457,
       0,     0,     0,     0,     0,     0,     0,   118,     0,  1301,
     457,     0,    14,    15,    16,    17,    18,     0,  1685,     0,
     112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,     0,     0,   598,     0,   431,  1316,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2138,     0,   359,
       0,     0,     0,     0,     0,     0,   112,     0,     0,  -466,
     279,     0,     0,     0,     0,     0,  1672,  1673,   118,   457,
       0,    58,     0,     0,     0,     0,     0,     0,   431,     0,
       0,     0,  -466,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,     0,   209,     0,     0,     0,     0,     0,
       0,   657,     0,     0,   279,   150,     0,   233,   234,    65,
      66,    67,    68,    69,    70,    71,    72,   657,     0,     0,
       0,   657,     0,     0,     0,     0,     0,   209,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
     118,     0,     0,     0,   118,    58,     0,     0,     0,   118,
     118,     0,     0,   118,   235,    77,     0,     0,  1763,     0,
       0,     0,     0,   118,     0,    80,    81,   431,     0,   431,
     118,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,   233,   234,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   150,     0,   233,   234,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   118,     0,    74,   431,    75,
       0,   209,   209,     0,     0,     0,     0,   118,   489,     0,
      74,   118,     0,     0,     0,   118,     0,     0,   324,    77,
       0,     0,     0,   657,     0,     0,     0,   431,     0,    80,
      81,  2100,    77,     0,     0,   550,     0,   118,     0,     0,
       0,     0,    80,    81,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,   457,    14,    15,    16,    17,
      18,     0,     0,     0,     0,  1833,   209,     0,     0,   431,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   457,     0,     0,     0,   489,     0,     0,   457,     0,
       0,     0,     0,     0,     0,     0,   359,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   209,     0,
       0,     0,   270,   118,   493,    58,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,   112,
     209,     0,   118,     0,     0,     0,     0,     0,   209,   457,
       0,     0,   209,  1301,   209,     0,     0,     0,     0,   150,
       0,   233,   234,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,   118,   457,     0,
       0,   359,   493,   118,   112,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     657,   493,     0,     0,     0,     0,     0,     0,  1660,    77,
       0,   359,     0,     0,     0,     0,     0,     0,     0,    80,
      81,     0,   489,     0,   657,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   657,     0,     0,
       0,     0,     0,     0,     0,     0,   209,   118,     0,   117,
     118,   118,     0,     0,   150,     0,   233,   234,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   489,   118,     0,
       0,     0,   118,     0,     0,     0,   118,     0,     0,     0,
       0,     0,    74,     0,     0,     0,   489,     0,   489,  1640,
       0,  1986,   489,   489,   489,     0,   118,   118,   118,   118,
     118,   118,   118,   235,    77,     0,     0,   117,   270,     0,
       0,     0,   489,     0,    80,    81,     0,     0,     0,     0,
       0,   457,     0,     0,     0,   457,   457,     0,   493,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   457,
       0,     0,     0,     0,   359,     0,     0,     0,     0,     0,
       0,     0,     0,   281,     0,     0,     0,     0,     0,  2004,
     359,     0,     0,     0,   359,     0,   270,     0,     0,     0,
       0,     0,   657,   493,     0,     0,     0,   359,     0,     0,
       0,     0,   489,     0,   457,     0,     0,   117,     0,   118,
     209,     0,   359,     0,   359,     0,     0,   117,   359,   359,
     359,     0,  1762,     0,     0,     0,     0,   403,     0,   118,
     404,     0,   405,     0,   406,     0,   363,     0,   359,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,   118,   495,     0,     0,     0,     0,
     209,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,   359,
     414,   415,   416,   112,   417,   418,     0,     0,   359,     0,
       0,     0,    74,   117,     0,     0,     0,     0,     0,     0,
     123,     0,     0,   123,   657,     0,   457,   279,     0,     0,
       0,     0,     0,   419,     0,     0,    78,   420,     0,     0,
       0,   117,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,   117,     0,     0,   270,   118,     0,     0,     0,
     489,   489,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,     0,   117,   123,     0,
       0,   281,     0,     0,     0,   150,   493,   233,   234,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,   118,   489,     0,     0,     0,
       0,     0,   659,     0,   123,   281,     0,     0,     0,     0,
       0,     0,     0,   294,   324,    77,     0,     0,   659,     0,
       0,     0,   659,   118,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   359,   123,     0,
       0,   359,   123,     0,     0,     0,   359,   359,   123,     0,
     359,   123,     0,     0,     0,     0,     0,     0,     0,     0,
     359,     0,     0,     0,     0,     0,     0,   359,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   270,     0,     0,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   359,     0,   457,   123,     0,     0,     0,     0,
       0,     0,     0,     0,   359,     0,     0,     0,   359,     0,
       0,   209,   359,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   209,   659,     0,     0,     0,     0,     0,
     209,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,   112,   123,     0,     0,     0,     0,   123,   209,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,   363,   123,     0,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
     279,   118,     0,     0,     0,   495,     0,   359,     0,     0,
       0,     0,   123,     0,     0,     0,     0,     0,   489,     0,
     117,   489,   489,     0,     0,     0,   657,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   359,   493,     0,     0,     0,     0,
       0,     0,   363,   495,     0,   117,     0,     0,     0,     0,
       0,   118,   118,     0,     0,   270,     0,     0,     0,     0,
       0,   659,   495,     0,     0,     0,     0,     0,     0,     0,
     118,     0,   363,     0,     0,     0,     0,     0,   123,     0,
       0,     0,   118,     0,     0,   659,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   659,     0,
       0,     0,     0,     0,   359,     0,     0,   359,   359,     0,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   359,     0,     0,     0,   359,
       0,     0,     0,   359,     0,   209,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,     0,   112,   112,   122,     0,     0,   122,     0,   495,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,   363,     0,     0,     0,   123,
     123,     0,     0,   659,   495,     0,     0,     0,   363,     0,
       0,   493,   122,     0,     0,     0,   359,     0,     0,     0,
       0,   123,     0,   363,     0,   363,     0,   209,     0,   363,
     363,   363,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,     0,     0,   123,     0,   359,     0,
       0,     0,     0,     0,     0,     0,     0,   359,     0,     0,
       0,   359,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,   122,     0,     0,     0,
       0,     0,   122,     0,     0,   122,     0,     0,     0,     0,
     363,     0,     0,     0,   117,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,   659,   123,     0,   281,   123,
     123,     0,   123,     0,     0,     0,   122,     0,     0,     0,
       0,   123,     0,     0,   123,   123,   123,     0,     0,   122,
       0,     0,     0,   279,     0,     0,     0,     0,     0,     0,
     219,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   495,   122,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   301,     0,     0,     0,   122,     0,   122,     0,     0,
       0,     0,   122,     0,     0,     0,   122,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,   122,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,   122,     0,     0,     0,   122,     0,   363,     0,
       0,     0,   363,     0,     0,     0,     0,   363,   363,     0,
       0,   363,     0,     0,     0,     0,   122,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,   127,     0,     0,   127,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,     0,   363,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,   363,
       0,     0,     0,   363,     0,     0,     0,     0,     0,     0,
       0,     0,   127,     0,     0,   123,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,     0,   123,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,     0,   117,     0,     0,     0,     0,     0,     0,
       0,     0,   629,     0,     0,     0,   122,     0,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   117,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,   127,     0,     0,     0,   127,   123,   657,     0,
       0,   281,   127,     0,     0,   127,   122,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   659,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   112,
       0,     0,     0,     0,     0,     0,   127,     0,     0,     0,
       0,     0,     0,     0,     0,   363,   495,     0,     0,   127,
       0,     0,     0,     0,     0,     0,     0,     0,   112,   657,
       0,     0,     0,   122,   122,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   359,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,   127,   112,
       0,     0,     0,   172,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   127,   827,   127,   829,     0,
       0,     0,   127,     0,     0,   363,   127,   846,   363,   363,
       0,   172,     0,     0,     0,     0,     0,   127,     0,     0,
     122,     0,     0,     0,     0,     0,   363,     0,     0,     0,
     363,     0,     0,     0,   363,     0,     0,     0,     0,     0,
     127,     0,   127,     0,     0,     0,   127,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,   172,     0,
       0,     0,     0,     0,     0,   123,   127,     0,     0,     0,
       0,   172,     0,   172,   123,     0,     0,     0,     0,   117,
       0,     0,   122,   117,   117,     0,     0,     0,     0,     0,
     122,     0,     0,   122,   122,     0,   122,   117,     0,     0,
     123,     0,     0,     0,   172,   122,   385,   123,   122,   122,
     122,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   495,     0,     0,     0,     0,   363,     0,     0,
       0,   123,   127,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     172,     0,     0,     0,   172,     0,     0,   172,   172,     0,
       0,   172,     0,     0,   172,   172,   127,   172,   122,   172,
       0,     0,   123,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,   363,     0,     0,   127,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   127,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   172,
       0,     0,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1308,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,   172,     0,     0,     0,
       0,     0,     0,     0,   281,     0,     0,     0,     0,     0,
       0,   172,     0,   127,   127,   123,   123,   123,   123,   123,
     123,   123,     0,     0,     0,     0,     0,   403,     0,   122,
     404,     0,   405,     0,   406,   127,     0,     0,     0,     0,
     123,   122,   122,     0,   123,   123,     0,     0,     0,     0,
      58,   407,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,  1125,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     127,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,     0,   417,   418,     0,     0,     0,     0,
     149,   122,    74,     0,    75,     0,     0,     0,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,   419,     0,     0,    78,   420,     0,     0,
       0,     0,   127,   421,   485,    81,   422,   423,   424,   425,
     127,     0,     0,   127,   127,     0,   127,     0,     0,     0,
    1277,  1278,     0,     0,     0,   127,     0,     0,   127,   127,
     127,     0,   117,     0,     0,     0,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   207,     0,     0,     0,
       0,     0,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,     0,     0,  1349,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   659,
     122,     0,     0,     0,     0,     0,     0,     0,  1369,   122,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,   123,   207,     0,   172,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     117,     0,     0,     0,   122,     0,     0,   172,     0,   172,
       0,   122,   123,     0,     0,     0,     0,  1395,     0,  1398,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   117,
     659,  1402,  1403,     0,     0,     0,   122,  1408,  1409,     0,
     577,     0,     0,     0,     0,     0,     0,  1417,   363,   127,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
     117,   127,   127,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1437,     0,     0,  1440,     0,     0,
     618,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   625,   123,     0,     0,   122,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   635,
       0,     0,     0,     0,     0,     0,   172,   172,     0,     0,
       0,     0,     0,   172,     0,     0,     0,     0,     0,     0,
     654,   127,     0,     0,     0,     0,     0,     0,     0,  1499,
       0,     0,     0,     0,     0,     0,     0,     0,   172,     0,
       0,   172,   172,     0,   172,     0,   172,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1521,     0,     0,     0,     0,     0,     0,     0,  1525,     0,
    1527,  1529,     0,     0,   370,     0,     0,     0,     0,  1535,
       0,  1536,     0,  1537,     0,     0,     0,   740,     0,   172,
    1546,     0,     0,   172,     0,     0,     0,   172,     0,   122,
     122,   122,   122,   122,   122,   122,     0,     0,     0,     0,
       0,     0,   482,   370,     0,     0,     0,     0,   781,     0,
       0,     0,     0,     0,   122,     0,     0,     0,   122,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,   122,     0,     0,   553,   821,     0,     0,     0,
       0,   826,   553,     0,     0,     0,     0,     0,     0,     0,
       0,  1597,  1598,     0,     0,     0,     0,     0,     0,     0,
     123,   852,     0,   172,     0,   853,   854,     0,     0,   857,
       0,     0,     0,     0,     0,     0,  1620,     0,     0,     0,
       0,     0,     0,  1625,   871,     0,     0,  1626,     0,     0,
     127,   123,     0,     0,     0,     0,     0,     0,     0,   127,
       0,     0,   122,     0,     0,     0,     0,   901,   127,     0,
       0,     0,     0,  1643,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   553,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   127,     0,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,     0,     0,     0,     0,
     370,   669,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   127,     0,     0,     0,
     690,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   941,   127,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   948,   122,
       0,     0,     0,     0,   172,     0,     0,     0,     0,  1751,
       0,     0,     0,     0,     0,     0,  1755,     0,  1757,     0,
       0,     0,   967,     0,     0,     0,   127,     0,     0,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   172,     0,
       0,     0,   553,   172,     0,     0,   172,     0,     0,     0,
     172,     0,     0,     0,     0,     0,     0,     0,     0,   553,
     816,     0,   553,   819,     0,     0,     0,     0,     0,     0,
       0,     0,   370,     0,     0,  1019,   669,     0,   122,     0,
       0,     0,  1793,     0,     0,     0,     0,     0,     0,   482,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   553,     0,
       0,     0,   553,     0,     0,     0,     0,     0,     0,   127,
     127,   127,   127,   127,   127,   127,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   127,     0,     0,     0,   127,   127,
       0,     0,     0,   370,     0,     0,     0,  1099,     0,  1100,
       0,     0,   127,     0,     0,   826,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   122,     0,     0,
     172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1143,     0,     0,     0,     0,     0,     0,     0,
       0,  1152,     0,     0,  1890,  1891,     0,  1155,   553,     0,
       0,   370,     0,     0,     0,     0,     0,     0,  1900,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   957,
     370,     0,   127,     0,     0,     0,     0,     0,     0,     0,
     669,     0,     0,     0,   669,     0,     0,   172,     0,     0,
       0,   975,   654,   370,     0,     0,     0,  1197,   172,     0,
       0,   172,     0,   172,   172,   307,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,   172,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,   127,
       0,     0,     0,     0,    58,   122,     0,     0,     0,     0,
       0,     0,     0,     0,  1328,     0,     0,     0,     0,     0,
       0,  -416,     0,     0,     0,     0,     0,   370,     0,   127,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
      63,    64,     0,   553,   553,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   553,  1093,     0,   553,  1096,     0,
       0,     0,   172,     0,     0,   122,     0,     0,    75,     0,
       0,   957,   370,     0,     0,     0,   669,     0,   669,   669,
       0,     0,     0,     0,     0,   669,     0,     0,   127,     0,
      78,   370,     0,   370,     0,  -416,     0,   370,   370,   370,
     734,     0,     0,     0,     0,     0,   216,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   127,   370,     0,   553,
       0,     0,   274,   553,     0,     0,     0,     0,     0,     0,
     553,  1170,     0,     0,   553,  1174,     0,     0,   553,  1178,
       0,     0,     0,     0,   172,     0,  1182,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2099,     0,     0,     0,     0,     0,
       0,     0,   172,   216,     0,     0,     0,   334,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   370,   553,   375,
       0,     0,     0,     0,     0,     0,     0,   127,     0,     0,
     149,     0,     0,     0,     0,   172,     0,     0,     0,     0,
       0,   172,     0,     0,   216,     0,   669,     0,     0,  2135,
       0,     0,     0,     0,     0,     0,     0,     0,   502,     0,
       0,     0,   507,     0,     0,     0,     0,     0,     0,     0,
    2150,     0,     0,     0,     0,     0,     0,     0,     0,   781,
       0,   909,   911,     0,     0,  2159,     0,     0,     0,     0,
       0,     0,     0,     0,   482,   370,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   216,   172,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   274,
       0,     0,     0,     0,  1534,     0,     0,     0,     0,     0,
     170,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1559,     0,
     553,     0,     0,   172,   172,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   507,   370,   370,     0,     0,   669,
     669,     0,     0,     0,   216,   127,   669,     0,     0,   172,
     172,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,   172,     0,     0,   662,     0,   679,     0,     0,
       0,     0,     0,     0,   127,   299,     0,     0,   734,     0,
       0,     0,     0,     0,   734,     0,     0,     0,   305,     0,
     306,   370,     0,   734,   553,  1421,     0,   553,  1425,     0,
       0,   553,  1429,     0,     0,   127,     0,     0,     0,     0,
       0,   155,   734,     0,     0,     0,     0,     0,     0,     0,
       0,   377,     0,     0,     0,     0,     0,     0,     0,     0,
     738,     0,     0,     0,     0,     0,     0,   455,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1072,     0,
       0,   487,     0,     0,     0,     0,     0,     0,     0,     0,
     172,     0,     0,     0,   216,   516,     0,   516,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   210,     0,   555,   556,     0,   662,   560,     0,
       0,   563,   564,   844,   566,     0,   567,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,  1745,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   172,     0,     0,   370,     0,     0,     0,     0,
       0,   669,  1542,     0,     0,     0,     0,     0,     0,     0,
     316,     0,     0,     0,     0,     0,     0,     0,     0,   210,
       0,     0,     0,     0,   370,     0,   630,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   216,   216,     0,     0,     0,     0,     0,   502,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     456,     0,     0,   652,     0,     0,     0,     0,   553,  1594,
       0,     0,     0,   481,     0,     0,     0,     0,   684,   553,
    1603,     0,   669,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   370,     0,   172,   370,   370,     0,     0,
    1745,     0,     0,     0,     0,     0,   375,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   502,     0,   961,     0,   574,
       0,   578,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   662,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     216,     0,     0,     0,   155,     0,     0,     0,   216,     0,
       0,   738,   216,     0,   216,   814,     0,     0,     0,     0,
       0,     0,     0,   738,     0,     0,   738,   738,   738,     0,
     578,     0,     0,  1886,  1887,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   516,     0,     0,
       0,     0,     0,   516,     0,     0,     0,     0,   868,     0,
     370,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   502,     0,     0,     0,     0,   669,     0,   897,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   216,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   456,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   502,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   502,     0,   502,     0,
     210,     0,   502,   502,   502,     0,  1971,     0,     0,   940,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   502,     0,     0,     0,   553,     0,     0,   823,
       0,     0,     0,     0,     0,     0,     0,  1745,   487,     0,
       0,     0,   553,     0,     0,     0,     0,     0,     0,     0,
     481,   969,   734,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   973,   974,     0,     0,     0,     0,
       0,     0,     0,     0,  2012,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   983,     0,   985,     0,  1002,     0,
       0,     0,   502,     0,     0,     0,     0,     0,     0,  1012,
     216,     0,     0,  2041,     0,     0,     0,  2042,     0,     0,
     844,     0,     0,     0,     0,     0,     0,   456,   456,     0,
       0,     0,     0,   868,  1033,     0,     0,  1035,     0,  1037,
       0,     0,     0,     0,     0,  1002,     0,  1049,  1002,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1077,     0,     0,     0,     0,
     375,     0,     0,     0,     0,     0,     0,     0,  1079,     0,
       0,     0,     0,   553,   553,     0,     0,     0,     0,  1088,
       0,     0,     0,  1086,  1087,     0,     0,     0,     0,   553,
    1091,     0,     0,     0,     0,   487,     0,     0,     0,     0,
    1077,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1114,     0,     0,  1117,  1118,
       0,  1121,     0,  1123,  1124,     0,     0,     0,     0,     0,
       0,  1146,     0,     0,   516,     0,   456,     0,     0,     0,
     502,   502,     0,     0,   456,     0,  1157,   456,   456,     0,
     456,     0,     0,     0,     0,     0,     0,     0,     0,   456,
       0,     0,   456,   456,   456,     0,  1168,     0,     0,     0,
    1172,     0,  1679,  1687,  1176,  1183,  1679,  1697,     0,     0,
       0,     0,  1704,     0,     0,     0,  1708,     0,  1710,   553,
    1697,     0,     0,     0,     0,     0,   502,   553,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   455,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1300,  1302,     0,     0,     0,     0,     0,
       0,   487,   456,     0,     0,     0,     0,   738,     0,     0,
    1310,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   553,  2063,     0,     0,   553,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   738,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1077,     0,     0,     0,     0,     0,     0,
       0,  1342,     0,     0,     0,     0,     0,     0,  1002,     0,
     553,     0,     0,   274,     0,     0,     0,     0,     0,     0,
       0,   375,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   216,     0,     0,     0,  1799,     0,     0,
     662,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   516,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   553,   553,     0,   375,
       0,     0,     0,     0,   738,     0,     0,     0,     0,     0,
       0,     0,  1836,     0,     0,  1286,   456,     0,     0,     0,
       0,  1310,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1855,  1857,     0,   553,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   516,     0,  1412,     0,  1415,     0,
       0,  1875,     0,     0,     0,  1413,     0,     0,   502,     0,
    1419,   502,   502,  1423,     0,     0,     0,  1427,     0,     0,
       0,     0,     0,     0,     0,   481,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   738,   738,   738,
       0,     0,   738,   738,     0,     0,     0,     0,     0,   507,
       0,     0,     0,     0,    14,    15,    16,    17,    18,  1491,
    1491,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -491,  -491,
    1942,  -491,    46,     0,    47,     0,  -491,  1945,     0,  1947,
       0,     0,  1953,  1958,     0,  1697,     0,   274,     0,     0,
    1964,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   375,     0,  1538,     0,     0,
       0,     0,     0,  1547,     0,     0,     0,  1540,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1002,    63,
      64,     0,   487,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     516,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,  1286,     0,     0,   868,     0,     0,
       0,     0,     0,  1457,     0,     0,     0,     0,     0,    78,
       0,     0,     0,     0,  1592,     0,     0,  2029,     0,     0,
       0,     0,  2035,  2037,     0,  1601,     0,     0,  1605,     0,
    1608,  1609,     0,     0,     0,     0,     0,     0,   456,     0,
       0,     0,  2056,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   216,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1637,  1638,     0,     0,  1635,     0,     0,     0,
       0,  2078,     0,  2081,     0,     0,   274,  2083,  2085,   456,
       0,     0,  2088,  2090,     0,     0,     0,     0,  1002,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   516,     0,     0,
     868,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     456,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1544,  2129,  2131,  2133,     0,     0,  1739,
       0,    14,    15,    16,    17,    18,     0,     0,  1033,     0,
       0,     0,     0,     0,   738,     0,     0,  1753,  1754,     0,
       0,     0,     0,     0,     0,  2154,  2156,  2158,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   403,     0,     0,
     404,     0,   405,     0,   406,     0,   516,     0,   868,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,   407,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   274,     0,
       0,  1605,     0,  1457,  1457,  1457,   155,  1649,  1650,  1654,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,  1795,
     414,   415,   416,     0,   417,   418,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
     455,     0,     0,     0,     0,  1824,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,    78,   420,     0,     0,
       0,     0,     0,   421,  1545,    81,   422,   423,   424,   425,
       0,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  1870,  1286,     0,     0,    46,
       0,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   738,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,  1883,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   516,     0,
       0,     0,     0,     0,     0,  1902,     0,  2169,  1904,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1473,     0,     0,     0,     0,     0,
    1912,  1913,     0,     0,  1918,     0,     0,     0,     0,     0,
       0,     0,     0,   738,    75,     0,   507,     0,     0,     0,
       0,     0,     0,   456,     0,   403,  1927,  1928,   404,     0,
     405,     0,   406,     0,     0,     0,     0,     0,     0,  1932,
       0,     0,     0,     0,     0,     0,     0,  1214,     0,   407,
    1216,     0,  1217,  -249,  -249,  1218,  1219,  1220,  1221,  1222,
    1223,  1224,  1225,  1226,  1227,  1228,  1229,  -345,  -345,  1230,
    1231,  1232,  1233,  1234,  1235,  1236,     0,  1237,     0,   408,
     409,     0,   510,   411,  1238,  1239,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,  1240,   414,   415,
     416,     0,   417,   418,     0,     0,     0,     0,     0,     0,
      74,     0,  1286,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -249,  1241,     0,     0,    78,   420,     0,  2001,  2169,   303,
     456,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,  1473,  -189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1002,     0,     0,     0,   403,     0,     0,   404,
       0,   405,     0,   406,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1214,  2061,
     407,  1216,     0,  1217,  -250,  -250,  1218,  1219,  1220,  1221,
    1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  -345,  -345,
    1230,  1231,  1232,  1233,  1234,  1235,  1236,     0,  1237,     0,
     408,   409,     0,   510,   411,  1238,  1239,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,  1240,   414,
     415,   416,     0,   417,   418,  1881,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1473,     0,     0,     0,     0,     0,     0,     0,
       0,  -250,  1241,     0,     0,    78,   420,     0,     0,     0,
     303,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,   403,     0,     0,   404,  -189,   405,     0,
     406,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1214,     0,   407,  1216,     0,
    1217,     0,     0,  1218,  1219,  1220,  1221,  1222,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  -345,  -345,  1230,  1231,  1232,
    1233,  1234,  1235,  1236,     0,  1237,     0,   408,   409,     0,
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
       0,     0,     0,     0,     0,     0,     0,  1691,  1692,  1693,
       0,     0,     0,   419,  1694,  1695,    78,  1242,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,     0,  1696,     4,
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
       0,     0,     0,     0,     0,  1691,  1692,  1693,     0,     0,
       0,   419,  1694,     0,    78,  1242,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,     0,  1696,     4,   186,     6,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,  1689,    78,  1242,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     4,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   403,     0,    46,   404,    47,   405,     0,   406,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   408,   409,    61,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,     0,   417,
     418,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,    78,  1242,     0,     0,     0,     0,     0,   421,    80,
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
     484,     0,     0,     0,     0,     0,   421,   485,    81,   422,
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
       0,     0,     0,     0,   419,     0,     0,    78,  1297,     0,
       0,     0,     0,     0,   421,  1298,    81,   422,   423,   424,
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
       0,     0,   419,     0,     0,    78,   828,     0,     0,     0,
       0,     0,   421,   485,    81,   422,   423,   424,   425,   186,
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
     419,     0,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,   186,     6,     7,
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
       0,    78,   828,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,  2011,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,     0,     0,    -2,     0,     0,    -2,     0,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
      -2,    -2,  2040,     0,    -2,    -2,    -2,    -2,    -2,    -2,
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
      -2,     0,     0,     0,     0,     0,     0,    -2,    -2,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,    59,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    60,     0,     0,
       0,    61,    62,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,    78,    79,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,    78,
     263,     0,     0,     0,  -821,     0,     0,    80,    81,   262,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -491,  -491,     0,  -491,    46,     0,    47,
       0,  -491,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   150,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,    78,   263,     0,     0,     0,     0,
       0,     0,    80,    81,     4,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,     0,     0,     0,     0,  -412,  -412,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -412,     0,     0,     0,    78,
      79,     0,     0,     0,     0,     0,     0,    80,    81,     4,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,     0,
       0,     0,     0,  -413,  -413,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -413,     0,     0,     0,    78,    79,     0,  1449,     0,  1450,
       0,     0,    80,    81,  1451,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1452,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1453,     0,
       0,     0,    78,  1007,     0,  1449,     0,  1450,     0,     0,
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
       0,     0,     0,     0,     0,     0,  1646,     0,     0,     0,
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
       0,     0,     0,     0,  1647,     0,     0,     0,    78,  1007,
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
       0,     0,  1648,     0,     0,     0,    78,  1007,     0,     0,
       0,     0,     0,     0,    80,    81,   262,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
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
       0,    78,   263,     0,     0,     0,     0,     0,     0,    80,
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
       0,     0,     0,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,   609,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1103,    77,  -684,    78,   665,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,    78,
     263,     0,     0,     0,  -825,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -491,  -491,     0,  -491,    46,     0,    47,     0,
    -491,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   263,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,   664,     0,  -684,    78,   665,     0,
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
     609,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   797,     0,
    -684,    78,   550,     0,     0,     0,     0,     0,     0,    80,
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
       0,    74,     0,    75,  1136,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -692,    78,   915,     0,     0,     0,
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
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   352,    78,
     353,     0,     0,     0,     0,     0,     0,    80,    81,   186,
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
       0,    75,  1615,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   915,     0,     0,     0,     0,     0,
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
       0,     0,     0,    74,     0,    75,  1617,     0,     0,     0,
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
       0,    78,   332,     0,     0,     0,     0,     0,     0,    80,
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
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     353,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -491,  -491,     0,  -491,    46,     0,    47,     0,
    -491,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
    1473,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,     0,   404,     0,   405,     0,   406,     0,
       0,     0,     0,    78,   263,     0,     0,     0,     0,     0,
       0,    80,    81,  1214,     0,   407,  1216,     0,  1217,  1935,
    1936,  1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,
    1227,  1228,  1229,     0,     0,  1230,  1231,  1232,  1233,  1234,
    1235,  1236,     0,  1237,     0,   408,   409,     0,   510,   411,
    1238,  1239,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,  1240,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,  1473,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1241,     0,     0,
      78,   420,     0,     0,     0,   303,     0,   421,    80,    81,
     422,   423,   424,   425,   403,     0,     0,   404,     0,   405,
       0,   406,  -189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1214,     0,   407,  1216,
       0,  1217,     0,     0,  1218,  1219,  1220,  1221,  1222,  1223,
    1224,  1225,  1226,  1227,  1228,  1229,     0,     0,  1230,  1231,
    1232,  1233,  1234,  1235,  1236,     0,  1237,     0,   408,   409,
       0,   510,   411,  1238,  1239,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,  1240,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1241,     0,     0,    78,   420,     0,     0,     0,   303,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,     0,  -189,   307,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -417,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,     0,     0,  -417,   307,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,    14,    15,
      16,    17,    18,    19,   725,    20,   726,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    63,    64,   403,     0,    46,   404,    47,   405,
       0,   406,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   407,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     727,     0,     0,     0,     0,  1229,     0,  -345,     0,     0,
       0,     0,    78,     0,     0,     0,     0,  -416,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1241,     0,     0,    78,   728,     0,     0,     0,   303,     0,
     421,    80,    81,   729,   730,   424,   425,    14,    15,    16,
      17,    18,    19,   725,    20,   726,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   403,     0,    46,   404,    47,   405,     0,
     406,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   407,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   727,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,     0,
     417,   418,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,    78,   728,     0,     0,     0,   303,     0,   421,
      80,    81,   729,   730,   424,   425,    14,    15,    16,    17,
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
     450,    78,   451,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   403,     0,    46,   404,    47,   405,     0,   406,    48,
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
      78,   451,     0,     0,     0,   303,     0,   421,    80,    81,
     422,   423,   424,   425,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     403,     0,    46,   404,    47,   405,     0,   406,    48,    49,
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
     728,     0,     0,     0,   303,     0,   421,    80,    81,   422,
     423,   424,   425,    14,    15,    16,    17,    18,    19,     0,
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
       0,     0,     0,     0,     0,   419,     0,     0,    78,   451,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,    14,    15,    16,    17,    18,    19,     0,    20,
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
       0,     0,     0,     0,   419,     0,     0,    78,   828,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,    14,    15,    16,    17,    18,    19,     0,    20,     0,
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
       0,     0,     0,   419,     0,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   716,     0,   717,   718,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
     262,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,   -17,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -491,  -491,     0,  -491,    46,     0,
      47,     0,  -491,     0,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,   351,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   150,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   609,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -684,    78,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
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
       0,    76,    77,     0,    78,    79,     0,     0,     0,  -823,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,    78,
     208,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
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
       0,    76,    77,     0,    78,    79,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   351,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   150,     0,   478,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   851,     0,     0,    78,
     479,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
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
       0,     0,     0,     0,    78,    79,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   150,     0,   478,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     479,     0,     0,     0,     0,     0,     0,    80,    81,   186,
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
       0,    75,   609,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,  -684,    78,    20,     0,    21,    22,    23,    24,
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
      77,     0,    78,   332,     0,     0,     0,     0,     0,     0,
      80,    81,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
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
       0,     0,     0,     0,    75,  1207,     0,     0,     0,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,    78,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   351,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   851,     0,     0,    78,   479,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
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
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   851,     0,
       0,    78,   479,     0,    63,    64,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1014,    78,  1007,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,  1563,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
    1007,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   315,     0,    63,    64,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   208,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   351,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   351,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   353,     0,    63,    64,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   315,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
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
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   479,     0,     0,     0,     0,     0,     0,    80,    81,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -491,  -491,     0,  -491,    46,     0,    47,
       0,  -491,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,   351,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   332,     0,
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
       0,     0,    78,  1007,     0,    63,    64,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,    79,     0,     0,     0,
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
      63,    64,   351,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   479,     0,    63,    64,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,  1007,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,    14,    15,    16,    17,    18,    80,    81,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -491,  -491,     0,  -491,    46,
       0,    47,     0,  -491,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   332,     0,    14,
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
       0,     0,     0,     0,    78,     0,     0,     0,     0,     0,
       0,     0,    80,    81,    20,     0,    21,    22,    23,    24,
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
     485,    81,   422,   423,   424,   425,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   403,     0,    46,   404,    47,
     405,     0,   406,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,     0,   417,   418,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,    78,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,   351,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,    78,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,   187,     0,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   716,
       0,   717,   718,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    20,    75,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -490,  -490,     0,  -490,    46,
       0,    47,     0,  -490,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      58,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -491,  -491,     0,  -491,
      46,     0,    47,   403,  -491,     0,   404,     0,   405,     0,
     406,  1950,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,  1951,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,     0,
     417,   418,     0,     0,   403,    75,     0,   404,    74,   405,
       0,   406,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1691,  1692,  1693,     0,     0,   407,   419,
    1952,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,   403,     0,     0,   404,     0,   405,    74,
     406,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1691,  1692,  1693,   407,     0,     0,
     419,  1856,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     510,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
      77,     0,   511,   512,     0,     0,     0,   513,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,  1345,     0,    78,   420,
       0,     0,     0,  1346,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,     0,     0,    78,   420,     0,     0,     0,   513,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,   867,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   419,   998,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
       0,     0,    78,   420,     0,     0,     0,   303,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,     0,     0,    78,   420,
       0,     0,  1071,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,  1414,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,     0,     0,
      78,   420,     0,     0,     0,  1483,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   419,     0,     0,    78,   420,     0,     0,
       0,  1764,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
       0,  1941,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,  1946,     0,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,  1950,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,  1957,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
    1951,    64,    65,    66,    67,    68,    69,    70,    71,    72,
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
       0,     0,     0,   419,  2034,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
    2036,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,  2080,     0,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,  2082,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,  2084,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   419,  2087,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
    2089,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,  2128,     0,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,  2130,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   419,  2132,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   419,  2153,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   419,
    2155,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,  2157,     0,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,   403,   417,   418,   404,     0,   405,     0,   406,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,     0,     0,
       0,   419,     0,     0,    78,   420,     0,     0,     0,     0,
       0,   421,    80,    81,   422,   423,   424,   425,     0,     0,
       0,     0,     0,     0,     0,   408,   409,     0,   410,   411,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,     0,   414,   415,   416,   403,   417,   418,
     404,     0,   405,     0,   406,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,     0,     0,     0,     0,     0,   707,     0,     0,
      78,   420,     0,     0,     0,     0,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,   408,   409,     0,   410,   411,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   412,   413,   400,     0,
     414,   415,   416,   403,   417,   418,   404,     0,   405,     0,
     406,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,     0,
       0,     0,     0,   713,     0,     0,    78,   420,     0,     0,
       0,     0,     0,   421,    80,    81,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,   408,   409,     0,
     410,   411,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,     0,   414,   415,   416,   403,
     417,   418,   404,     0,   405,     0,   406,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,     0,     0,     0,     0,     0,   722,
       0,     0,    78,   420,     0,     0,     0,     0,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,   403,   417,   418,   404,     0,
     405,     0,   406,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,     0,     0,     0,     0,   419,     0,     0,    78,   420,
       0,     0,     0,     0,     0,   421,   939,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,     0,   417,   418,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,     0,    78,   420,     0,     0,     0,     0,
       0,   421,   485,    81,   422,   423,   424,   425
};

static const yytype_int16 yycheck[] =
{
       1,   513,   170,     4,    76,    76,   180,   952,   139,   261,
     168,   230,   168,    76,     1,   407,   276,  1259,   185,   235,
       4,   168,   739,   949,   668,   419,   235,     1,  1241,   223,
     356,    76,    85,  1293,  1294,   143,   751,   256,   936,   754,
     432,   433,   677,  1807,   732,    59,   984,   170,   235,   845,
      78,   847,   235,   832,   992,    56,    57,  1197,    59,     1,
     235,   453,   370,    76,  1807,     1,   374,   207,   670,  1224,
     194,   421,    59,   570,   571,    76,   235,   238,   184,   240,
    1807,   833,   798,     1,    85,    59,   247,   839,  1701,   830,
       0,  1808,    93,   832,   486,     1,   235,    98,   206,   152,
     101,     0,   235,   235,   105,   185,  1241,    76,   324,   235,
     664,    85,   664,  1935,   245,   324,   664,    59,   354,     1,
     348,   105,    90,    59,    98,   196,    10,   101,   582,    76,
     830,   105,    84,   196,   370,   149,   830,   324,   374,   593,
      75,   324,   273,   144,  1046,   830,   147,   120,   149,   324,
      75,   196,     0,   284,   155,    75,   135,     1,   319,   320,
     388,   162,   149,   235,   235,   324,   492,    92,     1,   170,
    1206,     4,   235,  1075,    10,   149,   924,    59,   309,   936,
     155,   182,   157,   196,   638,   324,   159,   155,   832,   100,
     235,   324,   324,   194,   195,   196,   354,   945,   324,   178,
      72,    89,   699,   155,  1939,   206,  1923,   149,   120,   377,
     211,   264,   245,   149,   711,    59,   384,    77,    78,    20,
     221,   195,   235,   105,  1194,   226,    59,   196,   229,   230,
     231,  1201,   834,   155,   235,   236,   838,   211,  1851,     1,
     273,  1143,     4,   161,   162,   180,  2068,   849,   850,   196,
     486,   284,   324,   324,   377,   256,   162,   139,   125,   207,
     180,   324,   236,   264,   136,  1354,   235,   149,    72,   180,
     298,   395,   105,   274,   275,   163,   830,   278,   830,   324,
     333,   165,   830,   155,   285,   611,   170,   159,   235,   155,
     264,   158,   852,   853,   554,   139,   155,    59,   299,   300,
     158,   302,   562,   163,   278,   149,   307,   163,   805,   155,
     311,   871,  1282,   137,   170,   531,   149,  1215,   154,  1034,
     179,   179,   531,   324,   325,   519,   970,  2044,   486,   165,
     656,  1110,   136,   334,   170,  1023,  2100,   311,  1206,   101,
      72,   342,   343,   105,   531,   653,   347,   171,   531,   568,
    1248,   325,  1104,   101,   680,   574,   531,  2100,   752,   709,
     155,   687,  1103,   245,    98,   207,   758,   490,  1084,   152,
      98,  1110,   531,  2100,   482,  1510,   377,  1315,  1513,  1514,
     163,   941,   690,  2118,  2101,   386,     4,   149,   389,    72,
     858,   273,   531,    98,   395,   503,   179,    89,   531,   531,
     453,   245,   284,  1103,   136,   531,   244,   155,   155,  1103,
    1446,  1447,  1448,   251,    75,   631,  2151,   653,  1103,  1559,
     155,  2138,   631,   155,   155,   137,   163,   309,   163,   273,
      91,   313,   155,   170,   272,   108,   109,   577,    56,    57,
     284,   677,    77,    78,   631,   283,    61,    62,   631,   110,
    1198,   130,   131,   136,   690,  2071,   631,  1206,  1215,   171,
     531,   592,   149,   464,   112,   309,  1110,   921,   531,   161,
     161,   163,   631,  1443,    72,    93,   159,   635,     1,   635,
     171,  2097,   419,   511,   171,   158,   531,   135,   489,   490,
     464,  1248,   631,  1582,  1583,  1584,   175,   176,   631,   631,
     501,   502,   236,    58,  2120,   631,    61,    62,   236,    64,
     511,  1697,   513,   157,   654,   159,   278,   825,   531,   677,
     111,  1734,   990,   161,   160,  1127,   144,   695,   163,   147,
     531,   236,    72,   171,    85,   161,    59,   163,   136,  1099,
    1100,  1696,   543,   134,   162,     1,  1701,    98,   163,  1103,
     101,  1103,   170,  1305,   105,  1103,     1,   155,   180,   592,
     631,   159,   531,   137,   112,   180,  1078,   568,   631,   543,
      75,    72,   161,   574,   154,   576,   161,   311,  1446,  1447,
    1448,   161,   171,   311,   531,    90,   631,   135,   206,   825,
     163,   325,   166,   167,   161,  1155,   136,   325,   161,  1734,
     155,  1397,   161,    59,  1400,  1401,   311,   180,   226,   157,
     163,   229,  1550,   231,    59,   155,   139,   180,   161,   159,
     325,   180,  1014,   637,  1884,    62,   149,   180,  1020,   577,
     631,   161,   633,   959,  1386,   136,   637,   180,   639,  1031,
     155,   781,   161,   161,   195,   101,  1363,   648,   163,   957,
     637,   652,   161,   285,   155,   170,   274,   275,   159,   633,
     211,   180,   180,   637,  1299,   154,   860,   285,   300,   106,
    1856,  1857,   161,   779,   111,  1577,   843,   114,   897,   116,
     180,   299,   300,   684,   302,   236,   826,   635,   161,   307,
     857,   180,   161,   149,   139,   637,  1851,  1446,  1447,  1448,
     847,   637,   703,   157,   149,     3,   654,   180,   162,   832,
     592,   180,   161,   264,  1471,   161,   334,   161,  1475,  1476,
     558,   957,   245,   161,   342,   343,   155,   278,    69,   347,
    1656,   180,  1489,   162,   180,   577,   180,   161,    72,   159,
    1982,     3,   180,   744,   164,   746,   584,   748,   592,  1464,
     273,   752,   780,   591,   755,   637,   180,   595,   158,   377,
     161,   284,   161,   843,   161,   161,  1952,  1953,   386,  1486,
     707,   389,   171,   710,   171,   171,   713,   156,   155,   780,
     163,   149,   150,   151,   163,   722,   309,    72,   725,   726,
     727,   179,   898,   637,  1949,  1292,   159,   163,  1743,   155,
     245,   164,   136,   171,   637,   149,   150,   151,  1963,   543,
     654,   166,   654,   108,   109,   543,   134,   985,   173,   174,
    1758,   155,   278,  1215,   825,   159,   177,   171,   273,   830,
     267,   832,   155,   781,  1346,   154,   180,   155,   543,   284,
     120,   159,   161,   844,    72,  1313,   157,  1241,   166,   167,
     161,   136,  1320,   854,    72,     3,   312,  1574,   157,   860,
     157,  1019,   863,  1019,   309,    13,    14,    15,    16,    17,
     155,   489,   160,  2028,   159,   637,    72,   314,   826,  1781,
     157,  1783,  1142,   501,   502,   162,   759,   760,   761,    13,
      14,    15,    16,    17,   108,   109,   897,   845,   157,   633,
     356,   157,  1370,   162,  1071,   633,    72,  1299,   136,   157,
    1018,   157,    72,   464,    72,    72,    58,  1561,   136,    61,
      62,   358,    64,   360,    72,   362,   157,   155,   633,   155,
     161,   159,    72,   934,   935,   936,   157,   155,   157,   781,
     136,   159,   157,   162,  1336,  1337,  1338,   155,    72,   936,
      72,  1343,  1344,   168,   169,     3,  1150,   157,   576,   155,
     161,    72,   162,   159,   948,    13,    14,    15,    16,    17,
     136,  1483,    22,   410,   155,  1649,   136,  1303,   136,   136,
    1654,   155,   826,   161,   826,  1152,  1109,  1110,   136,   155,
     991,   101,   543,   159,   936,   155,   136,   155,   155,   159,
     936,   159,   159,   845,    72,   847,    72,   157,   158,   851,
     852,   853,   136,   137,   136,   155,   157,   155,  1179,   159,
     161,   639,   168,   169,    72,   136,   134,   158,   159,   871,
     648,   162,   386,   155,   652,   389,   492,   159,   302,  1040,
     128,   129,   157,   155,   155,  1046,   161,   155,   159,   155,
     163,   159,    89,   159,   936,   160,  1308,  1451,   166,   167,
    1572,   157,   163,  1299,   154,   161,   684,  1286,   136,   592,
     136,  1019,   163,   510,  1075,   155,   155,  1078,   157,   134,
     159,   180,   155,  1551,   157,   923,   159,   155,   136,   155,
     932,   159,   936,   159,   155,   160,   157,  1854,   159,   941,
     155,   157,  1103,   936,   159,  1573,   134,   170,  1109,  1110,
     163,   166,   167,   157,   637,   948,   157,   161,   120,  1293,
    1294,   155,  1280,   157,  1280,   159,   744,   155,   746,   155,
     748,   159,   157,  1280,   752,   155,   161,   755,   166,   167,
     179,  1299,  1143,   161,  1312,   157,   157,   592,   155,   161,
     161,   179,   589,  1206,   155,   611,   155,    13,    14,    15,
      16,    17,   780,   149,   150,   151,   132,   133,   624,   172,
    1328,  1327,  1328,  1433,   936,   161,    47,    48,   157,    50,
     167,   637,   161,   165,    55,   171,   948,   113,   114,   115,
     116,   117,   637,    62,   180,   134,   155,   160,   161,  1366,
     159,    13,    14,    15,    16,    17,   177,   825,    13,    14,
      15,    16,    17,  1197,   157,   155,    72,  1218,   161,   159,
    1221,  1222,  1223,   155,   680,   158,   844,   159,  1215,  1230,
     157,  2105,   101,   157,   161,  2109,   854,   161,  1706,  1406,
    1407,   155,   160,   161,   113,   159,   115,  1248,   117,   157,
    1397,   157,  1764,  1254,  1401,  2012,   157,  1099,  1100,   157,
      72,  1248,   157,  1978,   157,   157,   161,    72,  1269,   161,
     157,  1272,  1273,  1215,   161,  1276,  1646,  1647,  1648,  1215,
     136,   137,  1283,   157,  2041,  1286,   159,   156,   155,  1273,
     159,   160,  1760,   157,   160,   559,   157,   161,  1272,  1273,
     161,  1471,   160,   161,  1241,  1475,  1248,  1775,   149,   150,
     151,   155,  1248,  1155,   161,  2072,   934,   935,   936,   157,
     161,   157,   134,   161,   136,   161,  1406,  1407,  1329,   137,
     171,   136,   157,  1215,   157,   137,   161,  1729,   161,   180,
    1471,   161,   211,   155,  1475,  1346,   157,   159,   166,   167,
     161,   161,   106,  1354,   166,   167,   110,   111,   112,   113,
     114,   115,   116,   117,  1197,   157,  1248,   162,   157,   161,
     634,  1215,   161,   991,   162,   157,  1534,   157,  1534,   161,
    1328,   161,  1215,   157,   155,   157,  1387,   161,  1555,   161,
     157,  1273,   179,  1446,  1447,  1448,  1333,   155,  1451,  1452,
     160,   161,   156,   179,  1248,   159,  1203,  1204,  1205,   278,
     157,   280,   281,   936,   106,  1248,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,  1894,   766,   767,   768,
     769,  1899,  1667,  1668,  1669,  1197,   160,   161,   160,   161,
    1273,   161,   162,   312,  1660,   160,   161,   157,   317,   160,
     161,  1660,  1400,  1215,   323,   157,  1457,    91,    92,   160,
    1461,  1462,   160,   161,   157,  1449,   158,    13,    14,    15,
      16,    17,    18,  1660,   157,  1555,   157,  1660,   170,   157,
     936,   157,  1483,   163,  1471,  1660,  1248,   356,  1475,  1476,
     159,   936,   361,   163,   363,    13,    14,    15,    16,    17,
      18,  1660,  1489,   959,   163,  1506,  1507,   160,   161,   163,
    1272,  1273,   160,   161,   163,  1516,   160,   161,   160,   161,
     157,  1660,   160,   161,   160,   161,   161,  1660,  1660,   160,
     161,   160,   161,   797,  1660,    13,    14,    15,    16,    17,
      18,   410,  1516,    13,    14,    15,    16,    17,    18,   813,
     160,   161,  1553,   817,    70,  1397,   161,   162,  1400,  1401,
       4,     5,     6,     7,     8,     9,    10,    11,    12,   160,
     161,  1572,   160,   161,   180,  1559,  1577,    77,    78,   161,
     162,  1582,  1583,  1584,   160,  1977,  1534,   155,  1660,  1471,
    1355,  1356,    78,  1475,  1476,   464,   160,   762,   763,    18,
    1218,   764,   765,  1221,  1222,  1223,  1822,  1489,   770,   771,
    1513,  1514,  1230,  1822,  1668,  1669,  1449,   179,  1724,  1762,
    1763,    65,   163,   492,   163,   494,   495,  1471,   180,   157,
    1248,  1475,  1476,   157,   163,  1822,  1254,   180,  1471,  1822,
     163,   510,  1475,  1476,    18,  1489,   160,  1822,  1649,   160,
     160,  1269,   160,  1654,   157,   154,  1489,    22,  1276,  1660,
     157,  1662,   157,  1822,   157,  1283,   157,   157,  1887,  1670,
     157,   106,   157,   108,   543,   110,   111,   112,   113,   114,
     115,   116,   117,  1822,   157,   157,   157,  1449,  1689,  1822,
    1822,   157,  1215,   135,  1695,   160,  1822,   154,   154,   568,
     163,   163,  1686,   163,   573,    18,   575,    70,   161,  1471,
    1884,  1329,   157,  1475,  1476,   179,   157,   157,   157,  2045,
     157,  1272,  1273,  1457,   157,  1248,  1559,  1489,   597,  1457,
     599,   600,   163,   161,   163,  1971,   157,   157,  1739,   161,
     154,   157,   611,   160,    57,    58,    59,    60,    61,    62,
      63,    64,  1457,   161,  1516,   624,   157,   157,   161,  1215,
     157,  1745,   157,  1764,   633,  1871,   157,   106,  1935,   157,
    1215,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    1781,   157,  1783,   160,   157,   227,   157,   656,   160,   658,
     659,   157,  1248,   157,   161,   157,   157,  1559,   157,   157,
     157,   157,   157,  1248,  1686,   157,  2042,   160,   207,   157,
     161,   680,   681,   157,   161,   161,  1272,   154,   687,   155,
     159,  1822,   155,   155,  1825,  2044,  1090,   155,   155,   155,
    1094,   155,   155,  1834,  1835,    14,  1773,   162,   180,   161,
    1841,   162,   160,  1461,  1462,   160,   180,  1303,   154,  1113,
     180,  1218,  1853,  1686,   163,  1935,  1120,   154,   163,   179,
     161,   180,  1863,  1230,  1865,   157,   308,  1854,   157,  1975,
      13,    14,    15,    16,    17,  1876,   157,  1878,  1879,  1880,
     157,   157,   160,   157,  2100,  1886,  1887,   160,  1506,  1507,
     157,  2100,   161,   157,   160,   157,   157,   157,   154,  2066,
     154,  2068,   160,  1167,   155,   155,    80,  1171,   180,   180,
     180,  1175,  1745,  2100,    92,  1649,   154,  2100,  1971,   155,
    1654,  1649,   155,  1685,  1686,  2100,  1654,   180,  1662,    72,
      90,  1932,   180,   180,  1662,  1553,   180,   157,  1939,   154,
    2107,  2100,  1943,   154,  1649,   161,   160,  1948,  1471,  1654,
     163,   161,  1475,  1476,   160,   354,   160,  1662,   357,   160,
     154,  2100,   154,   162,   157,  1516,  1489,  2100,  2100,  2136,
     123,   370,  1854,   162,  2100,   374,   157,   419,   154,   162,
     157,   161,   157,  1745,   180,   157,  2066,   160,  2068,  2042,
     160,   134,   221,   136,   157,   157,   157,   154,   154,  2105,
     155,   155,  2003,  2109,  2110,   157,   157,   155,   157,   154,
    1854,  2178,   155,   160,  2015,  1471,   159,   157,  2019,  1475,
    1476,  1854,   160,   166,   167,  2012,  1471,  2107,  2100,  2100,
    1475,  1476,  2033,  1489,  2140,   160,   154,  2100,     1,   160,
     157,     4,   157,  2044,  1489,  2046,   157,    75,    75,   180,
     180,   154,  1670,   155,  2041,  2100,   180,  2163,   106,   157,
    1516,  2167,   110,   111,   112,   113,   114,   115,   116,   117,
     155,  1689,   160,   160,  2075,  1837,  2182,  1695,   154,   154,
     154,   159,    75,    75,   157,  2072,   157,   486,   157,   171,
     959,  1825,  1854,   108,   158,    75,    59,  1825,  2178,  2100,
    2101,   970,   162,   180,   171,   154,   156,   155,   156,   154,
     979,   154,  2113,    76,   180,   557,   171,  2118,   180,   171,
    1825,  1739,    85,   565,   154,   162,   155,  2101,   106,   161,
    2012,   171,   171,   156,   180,    98,   160,  2138,   101,  1506,
    1507,   583,   105,    75,   180,  2146,   157,   156,  2149,   157,
    2151,   157,   594,   154,  1418,   162,   154,   157,  1422,  2041,
     155,   180,  1426,   157,  2138,  1773,   180,   180,  2012,  1324,
     731,  2172,   772,   774,   773,  2151,  1236,   775,   577,  2012,
     143,   776,  2183,  1475,   452,  2068,   149,  1862,  2097,   152,
    2072,  2192,   155,   156,  1248,  1489,  1854,  2041,  1932,  1956,
    2057,  1733,  2135,  1717,  1932,   168,  1717,  2042,  2041,  2110,
    2167,  2041,  1276,    49,   114,  1084,  1834,  1835,   269,  1932,
    2001,   860,  1452,  1841,  1317,   520,  1269,  1932,  2072,  1685,
     193,     0,   195,   196,  1523,  1853,   635,   648,  1745,  2072,
     797,  1110,   703,   206,   207,  1863,   797,  1865,   211,   797,
    2012,  1638,    -1,    -1,   653,   654,    -1,    -1,  1876,    -1,
    1878,  1879,  1880,    -1,    -1,   707,    -1,   230,  1886,    -1,
      -1,   713,   235,   236,    -1,  1539,    -1,    -1,   677,  2041,
     722,    -1,   511,    -1,   513,    -1,    -1,    -1,    -1,    -1,
      -1,   690,    -1,   256,    -1,    -1,    -1,    -1,    -1,   741,
      -1,   264,   123,    -1,   125,   126,   127,    -1,    -1,    85,
    2072,    76,    -1,    -1,    -1,   278,    -1,    -1,    -1,    -1,
      -1,  1939,  1689,    -1,    -1,  1943,    -1,  1591,  1695,    -1,
    1948,  1854,    -1,    98,   155,    -1,  1600,   158,   159,    -1,
    1604,    -1,   163,   164,    -1,    -1,    -1,    -1,   311,    -1,
      13,    -1,    -1,    -1,   317,    -1,    -1,    -1,    -1,    -1,
     323,   324,   325,    -1,    -1,    -1,    -1,  2101,    -1,    -1,
     333,    -1,  1739,  2101,    -1,    -1,   152,    -1,    -1,    -1,
      -1,  1837,   781,    -1,    -1,  2003,    -1,    -1,    -1,    -1,
     155,   354,   355,   356,    -1,    -1,  2101,  2015,  1854,    -1,
      -1,  2019,    -1,  1272,  2138,    -1,    -1,   370,    -1,  1854,
    2138,   374,    -1,    -1,    -1,  2033,    -1,   193,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,   825,   826,  2046,    -1,
      -1,    -1,    -1,  2138,  1303,    -1,    -1,    -1,    -1,    -1,
    1309,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   419,  2075,    -1,    -1,
      -1,    -1,   904,    -1,    -1,   230,    -1,  1834,  1835,    -1,
     235,   236,    -1,    -1,  1841,    -1,   918,    -1,    -1,    -1,
     922,    -1,    -1,    -1,   926,    -1,  1853,    -1,   264,  2012,
     453,   256,   155,   456,    -1,  2113,  1863,    -1,  1865,    -1,
    2118,   464,    -1,    13,    14,    15,    16,    17,    -1,  1876,
      -1,  1878,  1879,  1880,    -1,    -1,    -1,    -1,  2041,   482,
      -1,    -1,    -1,   486,    -1,    -1,    -1,   490,  2146,   492,
      -1,  2149,    -1,  2151,    -1,    -1,    -1,    -1,    -1,    -1,
     503,    -1,    -1,    -1,    -1,    -1,   311,    -1,    -1,  2072,
      -1,    -1,    -1,    -1,  2172,    -1,  2012,   333,   957,   324,
     325,   960,    72,    -1,    -1,  2183,    -1,  2012,   531,    -1,
      -1,    -1,  1939,    -1,  2192,    -1,  1943,    -1,   354,    -1,
     543,  1948,    -1,    -1,    -1,  2041,    -1,    -1,    -1,  1458,
     302,    -1,    -1,   106,    -1,    -1,  2041,   110,   111,   112,
     113,   114,   115,   116,   117,   568,    -1,   570,   571,    -1,
      -1,   574,    -1,    -1,   577,    -1,  2072,    -1,    -1,    -1,
    1019,    -1,    -1,    -1,   134,    -1,   136,  2072,    -1,    -1,
      -1,   860,    -1,    -1,   863,    -1,  2003,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,  1516,  2015,   159,
      78,    -1,  2019,    -1,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2033,    -1,   631,    -1,
     633,    -1,   635,    -1,   637,    -1,   246,   453,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     653,   654,    -1,   656,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   664,    -1,    -1,    -1,   668,    -1,    -1,  2075,    -1,
     486,    -1,    -1,    -1,   677,     4,     5,     6,     7,     8,
       9,    10,    11,    12,   687,   490,    -1,   690,    -1,    -1,
      -1,    18,    -1,    -1,    -1,    -1,   699,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   707,    -1,  2113,   710,   711,    -1,
     713,  2118,   180,    -1,    -1,    -1,    -1,    13,  1200,   722,
      -1,    -1,   725,   726,   727,    -1,   531,    -1,    -1,    -1,
      -1,    -1,  1214,    -1,    61,    62,    63,    64,   543,  2146,
      -1,    -1,  2149,    -1,  2151,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1234,    -1,   570,   571,  2060,    -1,    -1,  1241,
      -1,    -1,    -1,   568,    -1,  2172,    -1,    -1,    -1,   574,
      -1,  1040,    -1,    -1,    -1,    -1,  1685,  1046,   781,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    88,    -1,   797,   798,    -1,    -1,    -1,    -1,
      -1,    -1,   805,    -1,    -1,    -1,  1075,   559,    -1,  1078,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   825,   826,    -1,    -1,   631,   830,   633,   832,
      -1,    -1,   159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   845,    -1,   847,    -1,   456,    -1,   851,   852,
     853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1299,   677,    -1,   473,    -1,     3,   476,    -1,   871,    -1,
      -1,    -1,    -1,    -1,  1143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   634,   699,    -1,    -1,    -1,    -1,    -1,  1328,
      -1,   707,    -1,    -1,   897,   711,    -1,   713,    62,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   664,   106,    -1,    -1,  1825,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   536,    -1,  1837,   122,
      -1,   124,    -1,   936,    -1,    -1,    -1,   101,   941,    -1,
      -1,    -1,    -1,    -1,    -1,   948,   949,    -1,    -1,   113,
     114,    -1,    -1,    -1,   957,    -1,   959,    -1,    -1,    72,
      -1,    -1,    -1,   156,    -1,    -1,   159,   970,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,  1465,  1466,    -1,    -1,    -1,    -1,   805,
      -1,    -1,   156,   106,    -1,    -1,   134,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1923,  1018,  1019,   155,   156,    -1,
      -1,   134,    -1,   136,   162,   830,    -1,   832,   166,   167,
      -1,    -1,    -1,  1515,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,   797,    -1,   211,    -1,    -1,
      -1,    -1,    -1,   166,   167,    -1,    -1,     1,    -1,    -1,
       4,   813,    -1,    -1,   106,   817,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,  1346,   830,    -1,
      -1,  1084,    -1,    -1,    -1,  1354,    -1,    -1,    -1,    -1,
      -1,    -1,   897,    -1,    -1,  1534,  1099,  1100,    -1,    -1,
    1103,    -1,    -1,    -1,    -1,    -1,    -1,  1110,    -1,    -1,
      -1,    57,    -1,    -1,   278,    59,    -1,    -1,    -1,    65,
      66,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   949,    -1,    -1,  2045,    -1,   180,    -1,
      -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,  1155,   317,    -1,    -1,    -1,    -1,    -1,   323,
     106,   105,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
     932,    -1,    -1,    -1,    -1,    -1,    -1,   797,   798,    -1,
      -1,    -1,   356,    -1,  1197,   139,    -1,    -1,   808,   143,
      -1,   811,    -1,  1206,    -1,   149,    -1,    -1,   152,    -1,
      -1,   106,  1215,   159,  1483,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   168,    -1,    -1,    -1,    -1,  2138,
      -1,   177,    -1,    -1,  1716,    -1,    -1,    -1,  1241,   134,
      -1,   136,    -1,    -1,    -1,  1248,   410,    -1,    -1,   193,
      -1,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,   156,   206,   207,    -1,    -1,    -1,   877,    -1,  1272,
    1273,   166,   167,    -1,   884,    -1,    -1,  1280,   888,    -1,
      -1,    -1,   892,  1286,    -1,    -1,    -1,    -1,    -1,  1292,
      -1,    -1,   236,    -1,    -1,    -1,  1299,    -1,  1103,    -1,
      -1,   245,    -1,  1572,  1109,  1110,    -1,    -1,  1577,    -1,
      -1,    -1,   256,  1582,  1583,  1584,    -1,   261,   262,    -1,
     264,    -1,    -1,    -1,    -1,  1328,    -1,    -1,   492,   273,
    1333,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1090,    -1,
     284,    -1,  1094,   287,    -1,    -1,   510,   291,    -1,    -1,
      -1,  1103,   296,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1113,    -1,   307,    -1,   309,    -1,   106,  1120,   313,
      -1,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   325,    -1,   122,    -1,   124,    -1,    -1,    -1,   333,
      -1,    -1,    -1,    -1,  1397,    -1,   185,  1400,  1401,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     354,   575,    -1,   357,    -1,  1167,    -1,   156,    -1,  1171,
     159,    -1,    -1,  1175,    -1,    -1,   370,    -1,    -1,    -1,
     374,    -1,    -1,   597,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1446,  1447,  1448,  1449,   611,  1451,  1452,
      -1,    -1,    -1,    -1,  1457,  1458,    -1,    -1,    -1,    -1,
     624,    -1,    -1,    72,    -1,    -1,    -1,    -1,  1471,    -1,
      -1,    -1,  1475,  1476,  1084,   419,  1292,    -1,    -1,    -1,
      -1,  1286,    -1,  1299,    -1,    -1,  1489,    -1,    -1,    -1,
      -1,    -1,   656,    -1,    -1,  1764,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   453,
      -1,    -1,  1781,  1516,  1783,    -1,   680,    -1,    -1,    -1,
      -1,    -1,    -1,   687,    -1,    -1,    72,   136,    -1,    -1,
      -1,  1534,  1971,    -1,    -1,    -1,    -1,    -1,   482,    -1,
      -1,    -1,   486,    -1,    -1,    -1,   155,   156,  1158,    -1,
      -1,  1161,    -1,    -1,    -1,  1165,  1559,    -1,  1561,   503,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1387,    -1,    -1,    -1,    -1,    -1,   134,   106,
     136,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,  2042,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,   159,   403,    -1,    -1,    -1,   407,   408,
     166,   167,    -1,    -1,    -1,    -1,   570,   571,   417,   418,
      -1,    -1,    -1,   577,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   432,   433,    -1,  1649,  1650,   592,    -1,
      -1,  1654,  1457,  1656,    -1,    -1,    -1,  1660,    -1,  1662,
      -1,    -1,    -1,   180,   453,    -1,  1418,    -1,    -1,    -1,
    1422,    -1,    -1,    -1,  1426,    -1,    -1,    -1,    -1,    -1,
    2162,    -1,  1685,  1686,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   635,  2174,   637,    -1,    -1,   106,   486,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   653,
     654,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   668,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   677,    -1,    -1,    -1,   901,   682,    -1,
      -1,    -1,  1745,    -1,    -1,    -1,   690,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   699,    -1,    -1,    -1,    -1,
     170,    -1,  1372,   707,    -1,    -1,   710,   711,   163,   713,
    1773,    56,    57,  1383,    -1,    -1,    -1,    -1,   722,    -1,
      -1,   725,   726,   727,    -1,    -1,    -1,  1539,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   959,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1807,  1808,    -1,   106,    93,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,  1822,
      -1,    -1,  1825,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,  1837,    -1,    -1,   781,    -1,  1591,
    1656,    -1,    -1,    -1,  1649,    -1,    -1,    -1,  1600,  1654,
      -1,  1854,  1604,    -1,   798,  1660,    -1,  1662,    -1,   144,
     159,   805,   147,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,   162,    -1,    -1,
      -1,   825,   826,    -1,  1887,    -1,    -1,    -1,   832,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   845,    -1,   847,    -1,    -1,    -1,   851,   852,   853,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
    1923,    -1,    -1,   166,   167,    -1,    -1,   871,   106,  1932,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     104,   226,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,  1971,   758,
     759,   760,   761,   762,   763,   764,   765,   766,   767,   768,
     769,   770,   771,   772,   773,   774,   775,   776,   166,   274,
     275,   155,   936,    -1,   158,   159,    -1,   941,  2001,    -1,
     285,    -1,    -1,    -1,   948,   949,   158,    -1,    -1,  2012,
      -1,    -1,    -1,   957,    -1,   300,   960,  1822,    -1,    -1,
    1825,    -1,    -1,   967,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,  2041,  2042,
    1650,  2044,  2045,    -1,    -1,    -1,    -1,    -1,    -1,   334,
      -1,    -1,    -1,    -1,   843,    -1,    -1,   342,   343,    -1,
      -1,    -1,   347,    -1,    -1,    -1,    -1,    -1,    -1,  2072,
      -1,    -1,    -1,    -1,  1018,  1019,    72,   158,    -1,    -1,
      -1,    -1,  1887,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2100,  2101,    -1,
      -1,   386,    -1,    -1,   389,    -1,    -1,    -1,  1272,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,  1932,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2138,    -1,    -1,   134,  1303,
     136,    -1,   106,    72,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,  1099,  1100,    -1,    -1,   155,
     156,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,   156,    -1,    -1,   159,    -1,  1807,  1808,    -1,
      -1,    -1,    -1,    -1,   489,   134,    -1,   136,    -1,    -1,
      -1,  1155,    -1,    -1,    -1,    -1,   501,   502,    -1,    59,
      -1,    -1,    -1,    -1,    -1,  1014,   155,   156,    -1,    -1,
     159,  1020,    -1,    -1,    -1,    -1,    -1,   166,   167,  2044,
      -1,    -1,  1031,    -1,    -1,    85,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1197,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,  1206,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,  1215,    -1,    -1,    -1,  1439,    -1,    -1,    -1,    -1,
      -1,   106,  1071,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,  1458,  2100,  2101,  1241,    -1,   139,
      -1,    -1,    -1,   143,  1248,    -1,    -1,    -1,  2060,   149,
      -1,    -1,   152,  1923,    -1,    -1,   156,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,  1273,
     170,    -1,    -1,  2138,    -1,    -1,  1280,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1292,    -1,
      -1,    -1,  1516,   193,   639,  1299,    72,    -1,    -1,    -1,
      -1,    -1,    -1,   648,  1308,    -1,   206,   207,    -1,    -1,
     106,   211,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,  1328,    -1,    -1,  1997,    -1,  1333,
     106,  2001,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,   245,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   134,   155,
     136,    -1,   262,    -1,   264,    -1,  1215,    -1,    -1,    -1,
      -1,    -1,    -1,   273,  2044,    -1,    -1,    -1,   278,   155,
     156,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,    -1,
     166,   167,    -1,  1397,    -1,   155,  1400,  1401,    -1,   299,
      -1,    -1,   302,    -1,    -1,    -1,    -1,   307,    -1,   309,
      -1,    -1,   312,   313,    -1,    -1,    -1,   317,    -1,    -1,
      -1,    -1,    -1,   323,    -1,    13,    14,    15,    16,    17,
    2100,  2101,    -1,   333,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1446,  1447,  1448,  1449,  1450,  1451,  1452,    -1,
    1299,    -1,    -1,    -1,   354,    -1,   356,   357,    -1,    -1,
      -1,  1685,    -1,    -1,    -1,    -1,    -1,  1471,  2138,    -1,
     370,  1475,  1476,    -1,   374,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,  1489,    -1,  1336,  1337,  1338,
      -1,    -1,    -1,    -1,  1343,  1344,    -1,    -1,    -1,   844,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   854,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1366,   106,   419,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
    1534,   106,    -1,    -1,    -1,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,   134,   122,   136,   124,
      -1,    -1,    -1,   453,    -1,  1559,    -1,  1406,  1407,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
      -1,   156,   482,    -1,   159,    -1,   486,    -1,    -1,    -1,
     935,    -1,   492,    -1,    -1,     3,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   503,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    20,  1837,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,   991,    55,    -1,    -1,
      -1,    -1,  1656,    -1,    -1,    -1,    -1,    -1,  1662,   559,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     570,   571,    -1,    -1,    -1,   575,    -1,   577,    -1,   106,
      -1,    -1,  1686,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   592,    -1,    -1,   122,    -1,   124,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,  1555,    -1,    -1,    -1,
      -1,   611,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   624,    -1,   134,    -1,   136,   156,
      -1,    -1,    -1,    -1,   634,   635,    -1,   637,    -1,    -1,
     134,  1745,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,   159,   652,   653,   654,    -1,   656,    -1,   166,   167,
      -1,   155,   156,    -1,   664,   159,    -1,    -1,    -1,  1773,
      -1,    -1,   166,   167,    -1,    -1,    85,   677,    -1,    -1,
     680,    -1,    -1,    -1,   684,   179,    -1,   687,    -1,    -1,
     690,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,   699,
      -1,    -1,    -1,    -1,  1808,    -1,    -1,   707,    -1,    -1,
     710,   711,    -1,   713,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2045,   722,    -1,    -1,   725,   726,   727,    -1,    -1,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,    -1,    -1,   152,   152,    -1,    -1,    -1,    -1,    -1,
    1854,    -1,    -1,    -1,    -1,    -1,    59,    -1,   106,   168,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   179,    -1,    -1,    -1,    -1,  1221,  1222,  1223,    -1,
    1729,   781,    -1,    -1,   193,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   797,   207,    -1,
      -1,    -1,   105,    -1,    -1,   805,    -1,   155,   156,  1254,
      -1,    -1,    -1,   813,   162,    -1,    -1,   817,   166,   167,
      -1,    -1,    -1,    -1,    -1,   825,   826,    -1,    -1,    -1,
     830,  1276,    -1,    -1,    -1,    -1,   139,    -1,  1283,    -1,
      -1,    -1,    -1,    -1,    -1,   845,   149,   847,    -1,    -1,
      -1,   851,   852,   853,    -1,   264,     4,     5,     6,     7,
       8,     9,    10,    11,    12,   168,    -1,  1971,    -1,    -1,
      18,   871,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,   901,    50,    51,   207,    53,    -1,    55,  2012,    -1,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   333,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   932,    -1,    -1,    -1,   936,  2041,  2042,    -1,
      -1,   941,   245,    -1,    -1,   354,    -1,    -1,   948,   949,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   957,    -1,   959,
     960,    -1,    13,    14,    15,    16,    17,    -1,  2072,    -1,
     273,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,   287,    -1,  1935,   987,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2101,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,   309,    -1,    -1,   157,
     313,    -1,    -1,    -1,    -1,    -1,  1461,  1462,  1018,  1019,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,  1977,    -1,
      -1,    -1,   180,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,   453,    -1,    -1,    -1,    -1,    -1,
      -1,   354,    -1,    -1,   357,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   370,    -1,    -1,
      -1,   374,    -1,    -1,    -1,    -1,    -1,   486,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
    1090,    -1,    -1,    -1,  1094,    72,    -1,    -1,    -1,  1099,
    1100,    -1,    -1,  1103,   155,   156,    -1,    -1,  1553,    -1,
      -1,    -1,    -1,  1113,    -1,   166,   167,  2066,    -1,  2068,
    1120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,  1155,    -1,   134,  2107,   136,
      -1,   570,   571,    -1,    -1,    -1,    -1,  1167,   577,    -1,
     134,  1171,    -1,    -1,    -1,  1175,    -1,    -1,   155,   156,
      -1,    -1,    -1,   486,    -1,    -1,    -1,  2136,    -1,   166,
     167,   155,   156,    -1,    -1,   159,    -1,  1197,    -1,    -1,
      -1,    -1,   166,   167,    -1,    -1,  1206,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1215,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,  1670,   635,    -1,    -1,  2178,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1241,    -1,    -1,    -1,   654,    -1,    -1,  1248,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   559,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,
      -1,    -1,  1272,  1273,   577,    72,    -1,    -1,    -1,    -1,
    1280,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,
     699,    -1,  1292,    -1,    -1,    -1,    -1,    -1,   707,  1299,
      -1,    -1,   711,  1303,   713,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,  1327,  1328,    -1,
      -1,   634,   635,  1333,   637,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     653,   654,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   664,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,    -1,   781,    -1,   677,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   690,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   805,  1397,    -1,     1,
    1400,  1401,    -1,    -1,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   826,  1418,    -1,
      -1,    -1,  1422,    -1,    -1,    -1,  1426,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,   845,    -1,   847,  1439,
      -1,  1886,   851,   852,   853,    -1,  1446,  1447,  1448,  1449,
    1450,  1451,  1452,   155,   156,    -1,    -1,    59,  1458,    -1,
      -1,    -1,   871,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,  1471,    -1,    -1,    -1,  1475,  1476,    -1,   781,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1489,
      -1,    -1,    -1,    -1,   797,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,     1,
     813,    -1,    -1,    -1,   817,    -1,  1516,    -1,    -1,    -1,
      -1,    -1,   825,   826,    -1,    -1,    -1,   830,    -1,    -1,
      -1,    -1,   941,    -1,  1534,    -1,    -1,   139,    -1,  1539,
     949,    -1,   845,    -1,   847,    -1,    -1,   149,   851,   852,
     853,    -1,  1552,    -1,    -1,    -1,    -1,    49,    -1,  1559,
      52,    -1,    54,    -1,    56,    -1,   168,    -1,   871,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1591,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1600,    -1,    -1,    -1,  1604,   207,    -1,    -1,    -1,    -1,
    1019,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   932,
     122,   123,   124,   936,   126,   127,    -1,    -1,   941,    -1,
      -1,    -1,   134,   245,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,     4,   957,    -1,  1656,   960,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,   273,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,   284,    -1,    -1,  1685,  1686,    -1,    -1,    -1,
    1099,  1100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,   309,    59,    -1,
      -1,   313,    -1,    -1,    -1,   106,  1019,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,  1745,  1155,    -1,    -1,    -1,
      -1,    -1,   354,    -1,   105,   357,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   114,   155,   156,    -1,    -1,   370,    -1,
      -1,    -1,   374,  1773,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1090,   139,    -1,
      -1,  1094,   143,    -1,    -1,    -1,  1099,  1100,   149,    -1,
    1103,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1113,    -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1837,    -1,    -1,
      -1,    -1,   193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1155,    -1,  1854,   206,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1167,    -1,    -1,    -1,  1171,    -1,
      -1,  1280,  1175,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1292,   486,    -1,    -1,    -1,    -1,    -1,
    1299,    -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   262,  1215,   264,    -1,    -1,    -1,    -1,   269,  1328,
      -1,    -1,   273,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1248,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   307,   559,   309,    -1,
      -1,    -1,   313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1273,  1971,    -1,    -1,    -1,   577,    -1,  1280,    -1,    -1,
      -1,    -1,   333,    -1,    -1,    -1,    -1,    -1,  1397,    -1,
     592,  1400,  1401,    -1,    -1,    -1,  1299,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2012,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1327,  1328,    -1,    -1,    -1,    -1,
      -1,    -1,   634,   635,    -1,   637,    -1,    -1,    -1,    -1,
      -1,  2041,  2042,    -1,    -1,  2045,    -1,    -1,    -1,    -1,
      -1,   653,   654,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2060,    -1,   664,    -1,    -1,    -1,    -1,    -1,   419,    -1,
      -1,    -1,  2072,    -1,    -1,   677,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   690,    -1,
      -1,    -1,    -1,    -1,  1397,    -1,    -1,  1400,  1401,    -1,
      -1,    -1,   453,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1418,    -1,    -1,    -1,  1422,
      -1,    -1,    -1,  1426,    -1,  1534,    -1,    -1,    -1,    -1,
      -1,   482,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   503,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1471,    -1,
      -1,    -1,  1475,  1476,     1,    -1,    -1,     4,    -1,   781,
      -1,    -1,    -1,    -1,    -1,    -1,  1489,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   797,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   813,    -1,    -1,    -1,   817,    -1,    -1,    -1,   570,
     571,    -1,    -1,   825,   826,    -1,    -1,    -1,   830,    -1,
      -1,  1534,    59,    -1,    -1,    -1,  1539,    -1,    -1,    -1,
      -1,   592,    -1,   845,    -1,   847,    -1,  1656,    -1,   851,
     852,   853,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   871,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   637,    -1,  1591,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,    -1,
      -1,  1604,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   139,    -1,    -1,    -1,   143,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,    -1,    -1,    -1,    -1,
     932,    -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,   941,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   699,    -1,
      -1,    -1,    -1,    -1,    -1,   957,   707,    -1,   960,   710,
     711,    -1,   713,    -1,    -1,    -1,   193,    -1,    -1,    -1,
      -1,   722,    -1,    -1,   725,   726,   727,    -1,    -1,   206,
      -1,    -1,    -1,  1686,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1019,   245,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,   262,    -1,   264,    -1,    -1,
      -1,    -1,   269,    -1,    -1,    -1,   273,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   805,    -1,    -1,   284,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     307,    -1,   309,    -1,    -1,    -1,   313,    -1,  1090,    -1,
      -1,    -1,  1094,    -1,    -1,    -1,    -1,  1099,  1100,    -1,
      -1,  1103,    -1,    -1,    -1,    -1,   333,    -1,    -1,    -1,
      -1,  1113,    -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1854,    -1,  1155,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1167,    -1,    -1,    -1,  1171,
      -1,    -1,    -1,  1175,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,   936,    -1,    -1,    -1,    -1,
      -1,    -1,   419,    -1,    -1,    -1,    -1,   948,   949,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,
      -1,    -1,    -1,  1215,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   321,    -1,    -1,    -1,   453,    -1,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1248,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   482,    -1,    -1,    -1,    -1,
      -1,    -1,   139,    -1,    -1,    -1,   143,  1018,  1971,    -1,
      -1,  1273,   149,    -1,    -1,   152,   503,    -1,  1280,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1299,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2012,
      -1,    -1,    -1,    -1,    -1,    -1,   193,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1327,  1328,    -1,    -1,   206,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2041,  2042,
      -1,    -1,    -1,   570,   571,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2060,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,   245,  2072,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   262,   485,   264,   487,    -1,
      -1,    -1,   269,    -1,    -1,  1397,   273,   496,  1400,  1401,
      -1,    76,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
     637,    -1,    -1,    -1,    -1,    -1,  1418,    -1,    -1,    -1,
    1422,    -1,    -1,    -1,  1426,    -1,    -1,    -1,    -1,    -1,
     307,    -1,   309,    -1,    -1,    -1,   313,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1197,    -1,   123,    -1,
      -1,    -1,    -1,    -1,    -1,  1206,   333,    -1,    -1,    -1,
      -1,   136,    -1,   138,  1215,    -1,    -1,    -1,    -1,  1471,
      -1,    -1,   699,  1475,  1476,    -1,    -1,    -1,    -1,    -1,
     707,    -1,    -1,   710,   711,    -1,   713,  1489,    -1,    -1,
    1241,    -1,    -1,    -1,   169,   722,   171,  1248,   725,   726,
     727,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   196,  1273,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1534,    -1,    -1,    -1,    -1,  1539,    -1,    -1,
      -1,  1292,   419,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     235,    -1,    -1,    -1,   239,    -1,    -1,   242,   243,    -1,
      -1,   246,    -1,    -1,   249,   250,   453,   252,   805,   254,
      -1,    -1,  1333,    -1,    -1,    -1,    -1,    -1,    -1,  1591,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,
      -1,    -1,  1604,    -1,    -1,   482,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   503,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   324,
      -1,    -1,   327,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,   351,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1686,    -1,    -1,    -1,    -1,    -1,
      -1,   366,    -1,   570,   571,  1446,  1447,  1448,  1449,  1450,
    1451,  1452,    -1,    -1,    -1,    -1,    -1,    49,    -1,   936,
      52,    -1,    54,    -1,    56,   592,    -1,    -1,    -1,    -1,
    1471,   948,   949,    -1,  1475,  1476,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,  1489,    -1,
      -1,    -1,    -1,    -1,   843,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     637,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
       3,  1018,   134,    -1,   136,    -1,    -1,    -1,   473,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1559,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,   699,   165,   166,   167,   168,   169,   170,   171,
     707,    -1,    -1,   710,   711,    -1,   713,    -1,    -1,    -1,
     939,   940,    -1,    -1,    -1,   722,    -1,    -1,   725,   726,
     727,    -1,  1854,    -1,    -1,    -1,   531,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,   547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1656,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,
      -1,    -1,    -1,    -1,  1033,  1686,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   631,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1971,
    1197,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1077,  1206,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,
      -1,    -1,    -1,    -1,  1745,   208,    -1,   672,   673,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2012,    -1,    -1,    -1,  1241,    -1,    -1,   692,    -1,   694,
      -1,  1248,  1773,    -1,    -1,    -1,    -1,  1126,    -1,  1128,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2041,
    2042,  1140,  1141,    -1,    -1,    -1,  1273,  1146,  1147,    -1,
     263,    -1,    -1,    -1,    -1,    -1,    -1,  1156,  2060,   936,
      -1,    -1,    -1,    -1,    -1,  1292,    -1,    -1,    -1,    -1,
    2072,   948,   949,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1183,    -1,    -1,  1186,    -1,    -1,
     303,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   315,  1854,    -1,    -1,  1333,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   332,
      -1,    -1,    -1,    -1,    -1,    -1,   801,   802,    -1,    -1,
      -1,    -1,    -1,   808,    -1,    -1,    -1,    -1,    -1,    -1,
     353,  1018,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1248,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   833,    -1,
      -1,   836,   837,    -1,   839,    -1,   841,   842,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1279,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1287,    -1,
    1289,  1290,    -1,    -1,   168,    -1,    -1,    -1,    -1,  1298,
      -1,  1300,    -1,  1302,    -1,    -1,    -1,   420,    -1,   884,
    1309,    -1,    -1,   888,    -1,    -1,    -1,   892,    -1,  1446,
    1447,  1448,  1449,  1450,  1451,  1452,    -1,    -1,    -1,    -1,
      -1,    -1,   206,   207,    -1,    -1,    -1,    -1,   451,    -1,
      -1,    -1,    -1,    -1,  1471,    -1,    -1,    -1,  1475,  1476,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2012,  1489,    -1,    -1,   239,   479,    -1,    -1,    -1,
      -1,   484,   246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1380,  1381,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2041,   504,    -1,   968,    -1,   508,   509,    -1,    -1,   512,
      -1,    -1,    -1,    -1,    -1,    -1,  1405,    -1,    -1,    -1,
      -1,    -1,    -1,  1412,   527,    -1,    -1,  1416,    -1,    -1,
    1197,  2072,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1206,
      -1,    -1,  1559,    -1,    -1,    -1,    -1,   550,  1215,    -1,
      -1,    -1,    -1,  1442,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   327,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1241,    -1,    -1,    -1,    -1,    -1,
      -1,  1248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     354,   355,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1273,    -1,    -1,    -1,
     374,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   627,  1292,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   641,  1656,
      -1,    -1,    -1,    -1,  1109,    -1,    -1,    -1,    -1,  1538,
      -1,    -1,    -1,    -1,    -1,    -1,  1545,    -1,  1547,    -1,
      -1,    -1,   665,    -1,    -1,    -1,  1333,    -1,    -1,  1686,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1153,    -1,
      -1,    -1,   456,  1158,    -1,    -1,  1161,    -1,    -1,    -1,
    1165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   473,
     474,    -1,   476,   477,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   486,    -1,    -1,   728,   490,    -1,  1745,    -1,
      -1,    -1,  1621,    -1,    -1,    -1,    -1,    -1,    -1,   503,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1773,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   532,    -1,
      -1,    -1,   536,    -1,    -1,    -1,    -1,    -1,    -1,  1446,
    1447,  1448,  1449,  1450,  1451,  1452,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1471,    -1,    -1,    -1,  1475,  1476,
      -1,    -1,    -1,   577,    -1,    -1,    -1,   820,    -1,   822,
      -1,    -1,  1489,    -1,    -1,   828,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1854,    -1,    -1,
    1305,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   855,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   864,    -1,    -1,  1753,  1754,    -1,   870,   632,    -1,
      -1,   635,    -1,    -1,    -1,    -1,    -1,    -1,  1767,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   653,
     654,    -1,  1559,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     664,    -1,    -1,    -1,   668,    -1,    -1,  1372,    -1,    -1,
      -1,   675,   915,   677,    -1,    -1,    -1,   920,  1383,    -1,
      -1,  1386,    -1,  1388,  1389,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,  1434,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,  1656,
      -1,    -1,    -1,    -1,    72,  2012,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1007,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    -1,    -1,    -1,   781,    -1,  1686,
      -1,    -1,    -1,    -1,  2041,    -1,    -1,    -1,    -1,    -1,
     108,   109,    -1,   797,   798,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   808,   809,    -1,   811,   812,    -1,
      -1,    -1,  1517,    -1,    -1,  2072,    -1,    -1,   136,    -1,
      -1,   825,   826,    -1,    -1,    -1,   830,    -1,   832,   833,
      -1,    -1,    -1,    -1,    -1,   839,    -1,    -1,  1745,    -1,
     158,   845,    -1,   847,    -1,   163,    -1,   851,   852,   853,
     419,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1773,   871,    -1,   873,
      -1,    -1,   101,   877,    -1,    -1,    -1,    -1,    -1,    -1,
     884,   885,    -1,    -1,   888,   889,    -1,    -1,   892,   893,
      -1,    -1,    -1,    -1,  1599,    -1,   900,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2043,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1627,   152,    -1,    -1,    -1,   156,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   941,   942,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1854,    -1,    -1,
    1193,    -1,    -1,    -1,    -1,  1660,    -1,    -1,    -1,    -1,
      -1,  1666,    -1,    -1,   193,    -1,   970,    -1,    -1,  2098,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   207,    -1,
      -1,    -1,   211,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1242,
      -1,   570,   571,    -1,    -1,  2134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1018,  1019,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   264,  1741,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   278,
      -1,    -1,    -1,    -1,  1297,    -1,    -1,    -1,    -1,    -1,
      48,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1321,    -1,
    1084,    -1,    -1,  1788,  1789,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   323,  1099,  1100,    -1,    -1,  1103,
    1104,    -1,    -1,    -1,   333,  2012,  1110,    -1,    -1,  1814,
    1815,    -1,    -1,    -1,    -1,    -1,    -1,  1822,    -1,    -1,
      -1,    -1,  1827,    -1,    -1,   354,    -1,   356,    -1,    -1,
      -1,    -1,    -1,    -1,  2041,   123,    -1,    -1,   707,    -1,
      -1,    -1,    -1,    -1,   713,    -1,    -1,    -1,   136,    -1,
     138,  1155,    -1,   722,  1158,  1159,    -1,  1161,  1162,    -1,
      -1,  1165,  1166,    -1,    -1,  2072,    -1,    -1,    -1,    -1,
      -1,     4,   741,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     419,    -1,    -1,    -1,    -1,    -1,    -1,   193,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   777,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1925,    -1,    -1,    -1,   453,   221,    -1,   223,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    -1,   242,   243,    -1,   486,   246,    -1,
      -1,   249,   250,   492,   252,    -1,   254,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1280,    -1,    -1,    -1,
      -1,    -1,  1525,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1997,    -1,    -1,  1299,    -1,    -1,    -1,    -1,
      -1,  1305,  1306,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,    -1,    -1,  1328,    -1,   322,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   570,   571,    -1,    -1,    -1,    -1,    -1,   577,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     193,    -1,    -1,   351,    -1,    -1,    -1,    -1,  1372,  1373,
      -1,    -1,    -1,   206,    -1,    -1,    -1,    -1,   366,  1383,
    1384,    -1,  1386,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1397,    -1,  2100,  1400,  1401,    -1,    -1,
    1643,    -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   654,    -1,   656,    -1,   262,
      -1,   264,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     699,    -1,    -1,    -1,   307,    -1,    -1,    -1,   707,    -1,
      -1,   710,   711,    -1,   713,   473,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   722,    -1,    -1,   725,   726,   727,    -1,
     333,    -1,    -1,  1746,  1747,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   513,    -1,    -1,
      -1,    -1,    -1,   519,    -1,    -1,    -1,    -1,   524,    -1,
    1534,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   781,    -1,    -1,    -1,    -1,  1561,    -1,   547,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   419,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   826,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,   847,    -1,
     453,    -1,   851,   852,   853,    -1,  1869,    -1,    -1,   625,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   871,    -1,    -1,    -1,  1650,    -1,    -1,   482,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1900,   654,    -1,
      -1,    -1,  1666,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     503,   667,  1241,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   672,   673,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1937,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   692,    -1,   694,    -1,   704,    -1,
      -1,    -1,   941,    -1,    -1,    -1,    -1,    -1,    -1,   715,
     949,    -1,    -1,  1966,    -1,    -1,    -1,  1970,    -1,    -1,
     959,    -1,    -1,    -1,    -1,    -1,    -1,   570,   571,    -1,
      -1,    -1,    -1,   739,   740,    -1,    -1,   743,    -1,   745,
      -1,    -1,    -1,    -1,    -1,   751,    -1,   753,   754,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   781,    -1,    -1,    -1,    -1,
    1019,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   794,    -1,
      -1,    -1,    -1,  1807,  1808,    -1,    -1,    -1,    -1,   805,
      -1,    -1,    -1,   801,   802,    -1,    -1,    -1,    -1,  1823,
     808,    -1,    -1,    -1,    -1,   821,    -1,    -1,    -1,    -1,
     826,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   833,    -1,    -1,   836,   837,
      -1,   839,    -1,   841,   842,    -1,    -1,    -1,    -1,    -1,
      -1,   857,    -1,    -1,   860,    -1,   699,    -1,    -1,    -1,
    1099,  1100,    -1,    -1,   707,    -1,   872,   710,   711,    -1,
     713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   722,
      -1,    -1,   725,   726,   727,    -1,   884,    -1,    -1,    -1,
     888,    -1,  1471,  1472,   892,   901,  1475,  1476,    -1,    -1,
      -1,    -1,  1481,    -1,    -1,    -1,  1485,    -1,  1487,  1923,
    1489,    -1,    -1,    -1,    -1,    -1,  1155,  1931,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   959,   960,    -1,    -1,    -1,    -1,    -1,
      -1,   967,   805,    -1,    -1,    -1,    -1,  1206,    -1,    -1,
     968,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1997,  1998,    -1,    -1,  2001,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1241,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1019,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1027,    -1,    -1,    -1,    -1,    -1,    -1,  1034,    -1,
    2044,    -1,    -1,  1272,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1280,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1292,    -1,    -1,    -1,  1636,    -1,    -1,
    1299,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1078,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2100,  2101,    -1,  1328,
      -1,    -1,    -1,    -1,  1333,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1681,    -1,    -1,   948,   949,    -1,    -1,    -1,
      -1,  1109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1700,  1701,    -1,  2138,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1150,    -1,  1152,    -1,  1154,    -1,
      -1,  1730,    -1,    -1,    -1,  1153,    -1,    -1,  1397,    -1,
    1158,  1400,  1401,  1161,    -1,    -1,    -1,  1165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1018,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1446,  1447,  1448,
      -1,    -1,  1451,  1452,    -1,    -1,    -1,    -1,    -1,  1458,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,  1235,
    1236,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
    1839,    50,    51,    -1,    53,    -1,    55,  1846,    -1,  1848,
      -1,    -1,  1851,  1852,    -1,  1854,    -1,  1516,    -1,    -1,
    1859,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1534,    -1,  1303,    -1,    -1,
      -1,    -1,    -1,  1309,    -1,    -1,    -1,  1305,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1324,   108,
     109,    -1,  1328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1346,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,  1197,    -1,    -1,  1363,    -1,    -1,
      -1,    -1,    -1,  1206,    -1,    -1,    -1,    -1,    -1,   158,
      -1,    -1,    -1,    -1,  1372,    -1,    -1,  1956,    -1,    -1,
      -1,    -1,  1961,  1962,    -1,  1383,    -1,    -1,  1386,    -1,
    1388,  1389,    -1,    -1,    -1,    -1,    -1,    -1,  1241,    -1,
      -1,    -1,  1981,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1656,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1438,  1439,    -1,    -1,  1434,    -1,    -1,    -1,
      -1,  2020,    -1,  2022,    -1,    -1,  1685,  2026,  2027,  1292,
      -1,    -1,  2031,  2032,    -1,    -1,    -1,    -1,  1464,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1483,    -1,    -1,
    1486,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1333,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     5,  2093,  2094,  2095,    -1,    -1,  1517,
      -1,    13,    14,    15,    16,    17,    -1,    -1,  1534,    -1,
      -1,    -1,    -1,    -1,  1773,    -1,    -1,  1543,  1544,    -1,
      -1,    -1,    -1,    -1,    -1,  2124,  2125,  2126,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,  1572,    -1,  1574,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1837,    -1,
      -1,  1599,    -1,  1446,  1447,  1448,  1449,  1450,  1451,  1452,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,  1627,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
    1656,    -1,    -1,    -1,    -1,  1661,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,  1721,  1559,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1971,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,  1741,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1764,    -1,
      -1,    -1,    -1,    -1,    -1,  1771,    -1,     1,  1774,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
    1788,  1789,    -1,    -1,  1800,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2042,   136,    -1,  2045,    -1,    -1,    -1,
      -1,    -1,    -1,  1656,    -1,    49,  1814,  1815,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,  1827,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    73,
      74,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,    -1,   101,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,  1745,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,   155,    -1,    -1,   158,   159,    -1,  1925,     1,   163,
    1773,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1978,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,  1997,
      73,    74,    -1,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,    -1,   101,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,   126,   127,     1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
     163,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,   180,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,
      76,    -1,    -1,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,    -1,   101,    -1,   103,   104,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,   157,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,   105,   106,
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
     167,   168,   169,   170,   171,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
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
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,     1,    -1,     3,     4,     5,     6,     7,     8,
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
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,   121,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
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
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,    -1,   158,   159,    -1,     3,    -1,     5,
      -1,    -1,   166,   167,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   154,    -1,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
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
      -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,   157,   158,   159,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
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
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
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
      -1,    -1,    -1,   134,    -1,   136,   137,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,    71,    -1,    73,    74,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    -1,    -1,    93,    94,    95,    96,    97,
      98,    99,    -1,   101,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
     168,   169,   170,   171,    49,    -1,    -1,    52,    -1,    54,
      -1,    56,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,
      -1,    76,    -1,    -1,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    -1,    -1,    93,    94,
      95,    96,    97,    98,    99,    -1,   101,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   180,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,    -1,    -1,    -1,    -1,   163,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,   108,   109,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    -1,    -1,    -1,    -1,    90,    -1,    92,    -1,    -1,
      -1,    -1,   158,    -1,    -1,    -1,    -1,   163,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,
     165,   166,   167,   168,   169,   170,   171,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
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
     157,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
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
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
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
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
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
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
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
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,   162,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   108,   109,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   136,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,   163,
      -1,    -1,   166,   167,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   136,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,   157,   158,    20,    -1,    22,    23,    24,    25,
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
     166,   167,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,    -1,    -1,    -1,   136,   137,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,   158,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    13,    14,    15,    16,    17,    18,    72,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,   108,   109,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,   158,   159,    -1,   108,   109,    -1,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    78,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    13,    14,    15,    16,    17,    18,    72,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,   108,   109,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,   159,    -1,   108,   109,    -1,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
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
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,    72,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,   108,   109,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,    -1,    -1,   108,   109,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,    13,    14,    15,    16,    17,   166,   167,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    13,
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
      -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    20,    -1,    22,    23,    24,    25,
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
     166,   167,   168,   169,   170,   171,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   136,    -1,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,   158,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    72,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   106,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    20,   136,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      72,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    49,    55,    -1,    52,    -1,    54,    -1,
      56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    49,   136,    -1,    52,   134,    54,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,   151,    -1,    -1,    73,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    49,    -1,    -1,    52,    -1,    54,   134,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,   151,    73,    -1,    -1,
     155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
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
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,
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
      -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
      -1,    -1,   162,    -1,    -1,   165,   166,   167,   168,   169,
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
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
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
      -1,   157,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
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
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    57,
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
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
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
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   182,   408,   409,     3,     4,     5,     6,     7,     8,
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
     339,   341,   347,   348,   349,   350,   361,   366,   400,   403,
     413,   419,   421,   427,   431,   436,   437,   438,   439,   440,
     441,   442,   443,   469,   487,   488,   489,   490,     0,   182,
     106,   186,   202,   298,   300,   311,   314,   317,   327,   331,
     336,   120,   155,    58,    61,    62,    64,   155,   155,   364,
     425,   426,   427,   323,   324,   108,   109,   186,   380,   401,
     402,   380,   155,   413,   155,   155,     4,   106,   108,   109,
     315,   320,   321,   155,   155,   202,   426,   431,   437,   438,
     439,   441,   442,   443,   108,   338,   160,   182,   159,   301,
     311,   314,   436,   440,   486,   487,   490,   491,   180,   183,
     152,   163,   179,   223,   383,    89,   161,   420,   380,   161,
     161,   161,   180,   108,   109,   155,   202,   306,   307,   431,
     432,   433,   434,   435,   436,   440,   444,   445,   446,   447,
     448,   449,   450,   451,   452,   458,     3,    47,    48,    50,
      55,   329,     3,   159,   202,   300,   315,   319,   321,   332,
     337,   416,   436,   440,   490,    69,   298,   300,   314,   327,
     331,   336,   417,   436,   440,    65,   320,   320,   315,   321,
     309,   320,   321,   329,   348,   315,   320,   315,   158,   425,
     161,   183,   155,   163,   231,   425,   425,     3,   289,   290,
     305,   308,   314,   318,   319,   159,   311,   314,   488,   380,
     380,   413,   179,   314,   155,   202,   422,   431,   432,   436,
     445,   449,   159,   202,   490,   414,   415,    57,    65,    66,
      67,    68,   159,   177,   380,   389,   391,   395,   397,   398,
     337,    57,   157,   159,   202,   310,   314,   318,   326,   327,
     333,   334,   335,   336,   340,   347,   348,   366,   376,   378,
     469,   482,   483,   484,   485,   490,   491,   425,   108,   109,
     170,   186,   337,   365,   458,   427,   155,   396,   397,   155,
      13,    88,   106,   108,   109,   155,   185,   428,   429,   430,
     120,   188,   189,    49,    52,    54,    56,    73,   103,   104,
     106,   107,   118,   119,   122,   123,   124,   126,   127,   155,
     159,   165,   168,   169,   170,   171,   184,   185,   188,   190,
     193,   201,   202,   203,   204,   207,   208,   209,   210,   211,
     212,   213,   214,   215,   216,   217,   218,   219,   225,   337,
     157,   159,   201,   202,   218,   220,   311,   337,   381,   382,
     399,   486,   491,   428,   314,   437,   438,   439,   441,   442,
     443,   157,   157,   157,   157,   157,   157,   157,   108,   159,
     186,   311,   469,   488,   159,   166,   202,   220,   300,   301,
     310,   312,   314,   327,   334,   336,   373,   374,   375,   377,
     378,   482,   490,   160,   155,   436,   440,   490,   155,   161,
     106,   158,   159,   163,   185,   187,   220,   384,   385,   386,
     387,   388,    22,   384,   155,   380,   231,   155,   186,   422,
     186,   426,   431,   433,   434,   435,   444,   446,   447,   448,
     450,   451,   452,   314,   432,   445,   449,   161,   101,   424,
     159,   425,   466,   469,   424,   425,   425,   420,   289,   155,
     425,   466,   424,   425,   425,   420,   425,   425,   314,   422,
     155,   155,   313,   314,   311,   314,   160,   182,   311,   486,
     491,   339,   163,   420,   289,   380,   380,   383,   300,   319,
     418,   436,   440,   163,   420,   289,   401,   314,   327,   314,
     314,   108,   338,   108,   109,   186,   337,   342,   401,   137,
     186,   314,   370,   371,   375,   376,   379,   154,   182,   231,
     305,   180,   436,   449,   314,   182,   424,   155,   424,   183,
     220,   426,   431,   314,   155,   182,   380,   411,   163,   155,
     380,   163,   380,   137,   166,   167,   394,   157,   161,   380,
     398,   157,   425,   160,   182,   312,   314,   327,   334,   336,
     481,   482,   490,   491,   155,   159,   167,   179,   202,   469,
     471,   472,   473,   474,   475,   476,   493,   202,   340,   490,
     314,   334,   320,   315,   425,   157,   312,   314,   483,   312,
     469,   483,   186,   365,   458,   362,   163,   365,   389,   179,
     389,   428,   157,   161,   155,   157,   120,   155,   201,   155,
     155,   155,   204,   155,   201,   155,   106,   108,   109,   315,
     320,   321,   155,   201,   201,    19,    21,    85,   159,   168,
     169,   205,   206,   220,   227,   231,   350,   381,   490,   161,
     182,   155,   190,   159,   164,   159,   164,   123,   125,   126,
     127,   155,   158,   159,   163,   164,   204,   204,   172,   166,
     173,   174,   168,   169,   128,   129,   130,   131,   175,   176,
     132,   133,   167,   165,   177,   134,   135,   178,   157,   161,
     158,   182,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   179,   222,   223,   224,   155,   202,   462,
     463,   464,   465,   466,   157,   161,   157,   157,   157,   157,
     157,   157,   157,   155,   425,   466,   469,   155,   466,   469,
     155,   182,   155,   311,   488,   160,   182,   183,   159,   183,
     155,   167,   202,   431,   453,   454,   455,   456,   457,   458,
     459,   460,   461,   137,   490,   161,   183,   161,   183,   380,
     380,   155,   182,   182,   182,   159,   187,   182,   385,   162,
     161,   492,   384,   158,   159,   162,   388,   156,   220,   226,
     155,   182,   179,   431,   433,   434,   435,   444,   446,   447,
     448,   450,   451,   452,   157,   157,   157,   157,   157,   157,
     157,   157,   157,   157,   432,   445,   449,   425,   155,   179,
     160,   182,   383,   231,   420,   370,   383,   231,   422,   227,
     382,   227,   382,   422,   108,   159,   411,   231,   420,   424,
     163,   163,   420,   289,   411,   231,   420,   344,   345,   343,
     163,   157,   161,   157,   161,    70,   291,   292,   180,   166,
     220,   182,   431,   374,   413,   411,   380,   160,   182,   155,
     393,   391,   392,    78,   325,   186,   312,   469,   483,   314,
     318,   490,   370,   472,   473,   474,   160,   182,    18,   220,
     314,   471,   493,   425,   425,   469,   312,   481,   491,   314,
     186,   312,   483,   425,   163,   425,   365,    10,   165,   365,
     367,   368,   163,   157,   382,   157,   157,   429,   156,   194,
     195,   196,   220,   180,   381,   491,   190,   159,   381,   382,
     381,   491,   220,   381,   157,   381,   381,   381,   160,   182,
     157,   168,   169,   206,    18,   316,   157,   161,   157,   166,
     167,   157,   226,   220,   163,   220,   186,   220,   186,   118,
     159,   186,   194,   108,   109,   118,   159,   186,   350,   220,
     194,   186,   204,   207,   207,   207,   208,   208,   209,   209,
     210,   210,   210,   210,   211,   211,   212,   213,   214,   215,
     216,   162,   227,   180,   188,   159,   186,   220,   163,   220,
     370,   463,   464,   465,   314,   462,   425,   425,   220,   382,
     155,   425,   466,   469,   155,   466,   469,   370,   370,   182,
     182,   160,   160,   155,   431,   454,   455,   456,   459,    18,
     314,   453,   457,   155,   425,   475,   493,   425,   425,   493,
     155,   425,   475,   425,   425,   183,   219,   380,   374,   377,
     160,   377,   378,   160,   493,   493,   137,   372,   373,   374,
     372,   372,   380,   182,   218,   219,   220,   423,   492,   384,
     386,   154,   182,   157,   161,   182,   372,   220,   157,   157,
     157,   157,   157,   157,   157,   157,   157,   155,   425,   466,
     469,   155,   425,   466,   469,   155,   425,   466,   469,   422,
     188,    22,   469,   220,   321,   337,   467,   231,   157,   157,
     157,   157,   157,   409,   410,   231,   154,   182,   411,   231,
     420,   410,   231,   163,   163,   163,   351,   137,   375,   376,
     186,   293,   380,    18,    71,    73,    74,    76,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      93,    94,    95,    96,    97,    98,    99,   101,   108,   109,
     121,   155,   159,   227,   228,   229,   230,   231,   232,   233,
     235,   236,   245,   252,   253,   254,   255,   256,   257,   262,
     263,   266,   267,   268,   269,   270,   271,   272,   278,   279,
     280,   294,   314,   318,   380,   421,    70,   183,   183,   372,
     161,   412,   410,   157,   298,   300,   311,   404,   405,   406,
     407,   399,   179,   390,   390,   312,   483,   159,   166,   202,
     220,   337,   220,   314,   157,   157,   157,   157,     5,   314,
     425,   471,   363,   367,   365,   163,   337,   161,   492,   380,
     367,   163,   157,   157,   161,   157,   157,   161,   182,   161,
     157,   157,   157,   161,   157,   204,   157,   157,   157,   204,
      18,   316,   220,   157,   157,   156,   163,   204,   160,   183,
     194,   160,   160,   118,   122,   124,   187,   197,   198,   199,
     157,   197,   160,   161,   154,   218,   162,   157,   197,   183,
     385,   157,   157,   157,   157,   462,   370,   370,   157,   157,
     372,   372,   459,   157,   157,   157,   157,   155,   431,   458,
     453,   457,   370,   370,   160,   183,   493,   161,   183,   157,
     161,   161,   183,   183,   383,   197,   137,   171,   183,   183,
     154,   384,   220,   425,   156,   220,   372,   183,   155,   425,
     466,   469,   155,   425,   466,   469,   155,   425,   466,   469,
     370,   370,   370,   424,   157,   149,   171,   183,   468,   161,
     183,   412,   404,   410,   231,   412,   351,   351,   351,     3,
       5,    10,    73,   154,   295,   302,   303,   311,   314,   352,
     357,   486,   161,   180,   155,    61,    62,   180,   231,   294,
     421,   155,   155,    18,   229,   155,   155,   180,   380,   180,
     380,   166,   380,   163,   228,   155,   155,   155,   229,   155,
     231,   220,   221,   221,    14,   281,   257,   268,   180,   183,
     233,    78,   180,   380,    91,    92,   261,   265,   112,   135,
     260,   111,   134,   264,   260,   379,   314,   162,   293,   160,
     160,   183,   412,   380,   422,   183,   180,   183,   180,   183,
     157,   382,   396,   396,   182,   183,   183,   183,   220,   155,
     425,   475,   469,   313,     5,   166,   183,   220,   365,   492,
     163,   367,    10,   368,   154,   179,   369,   492,   154,   182,
     196,   310,   186,    78,   191,   192,   381,   204,   204,   204,
     204,   204,   163,   385,   161,   154,   200,   159,   198,   200,
     200,   160,   161,   125,   158,   160,   226,   218,   180,   160,
     492,   155,   425,   466,   469,   157,   157,   183,   183,   157,
     155,   425,   466,   469,   155,   425,   475,   431,   425,   425,
     157,   157,   160,   377,   160,   137,   374,   137,   157,   157,
     183,   219,   219,   160,   160,   183,   183,   157,   370,   370,
     370,   157,   157,   157,   383,   425,   161,   220,   220,   321,
     337,   160,   154,   183,   412,   154,   154,   154,   154,   311,
     311,   350,   358,   486,   311,   357,   155,   346,   180,   180,
     155,   162,   202,   353,   354,   360,   431,   432,   445,   449,
     161,   180,   380,   380,   194,   180,   231,   180,   231,   227,
     237,   294,   296,   299,   305,   314,   318,   227,    80,   157,
     237,   149,   150,   151,   156,   157,   180,   227,   246,   247,
     249,   294,   180,   180,   227,   180,   385,   180,   227,   226,
     227,   246,   113,   114,   115,   116,   117,   273,   275,   276,
     180,   100,   180,    84,   155,   157,   154,   180,   180,   155,
     155,   229,   229,   257,   155,   267,   257,   267,   231,   425,
     180,   157,   154,   394,   154,   182,   161,   161,   160,   160,
     160,   183,   370,   220,   220,   183,   160,   183,   163,   154,
     367,   492,   337,   380,   163,   219,   154,   404,   470,   471,
     157,   162,   157,   161,   162,   385,   492,   226,   123,   197,
     198,   159,   198,   159,   198,   160,   154,   370,   157,   157,
     370,   370,   160,   183,   157,   425,   157,   157,   157,   227,
     468,   154,   154,   346,   346,   346,   353,   155,   202,   355,
     356,   466,   477,   478,   479,   480,   180,   161,   180,   353,
     180,   399,   426,   431,   220,   314,   154,   161,   180,   359,
     360,   359,   359,   380,   157,   157,   227,   314,   157,   155,
     229,   157,   149,   150,   151,   171,   180,   250,   251,   229,
     228,   180,   251,   157,   162,   227,   156,   227,   228,   249,
     180,   492,   157,   157,   157,   157,   231,   275,   276,   155,
     220,   155,   188,   204,   258,   227,    75,   110,   259,   261,
      75,     1,   229,   425,   390,   405,   182,   182,   160,   157,
     183,   183,   160,   160,   367,   492,   154,   369,   369,   385,
     183,   157,   220,   192,   220,   492,   154,   160,   160,   197,
     197,   157,   425,   425,   157,   157,   160,   160,   220,   180,
     478,   479,   480,   314,   477,   161,   180,   425,   425,   180,
     157,   431,   425,   229,   229,    77,    78,   163,   240,   241,
     242,   157,   227,    75,   229,   227,   156,   227,    75,   180,
      57,   108,   156,   227,   228,   248,   249,   156,   227,   229,
     247,   251,   251,   180,   227,   154,   163,   242,   229,   229,
     155,   182,   180,   188,   157,   162,   157,   161,   162,   157,
     229,   155,   229,   229,   229,   396,   380,   422,   160,   160,
     492,   154,   492,   154,   154,   160,   160,   157,   157,   157,
     477,   425,   354,    75,     1,   219,   238,   239,   423,     1,
     162,     1,   182,   229,   240,    75,   180,   157,   229,    75,
     180,   171,   171,   229,   228,   108,   251,   251,   180,   227,
     248,   171,   171,    75,   156,   227,   156,   227,   228,   180,
       1,   182,   182,   277,   312,   314,   486,   162,   180,   159,
     188,   282,   283,   284,   204,   194,   227,   260,   154,   154,
     155,   425,   466,   469,   356,   229,   137,     1,   161,   162,
     154,   287,   288,   294,   229,    75,   180,   229,   227,   156,
     156,   227,   156,   227,   156,   227,   228,   156,   227,   156,
     227,   229,   171,   171,   171,   171,   154,   287,   277,   183,
     155,   202,   422,   477,   186,   162,   106,   155,   157,   162,
     161,   157,   157,    75,   256,   370,   219,   238,   241,   243,
     244,   294,   229,   171,   171,   171,   171,   156,   156,   227,
     156,   227,   156,   227,   243,   183,   180,   274,   314,   282,
     160,   219,   180,   282,   284,   229,    75,   157,   229,   234,
     183,   241,   156,   156,   227,   156,   227,   156,   227,   183,
     274,   218,   157,   162,   188,   157,   157,   162,   229,     1,
     229,   154,   234,   154,   157,   231,   188,   285,   155,   180,
     285,   231,   161,   162,   219,   157,   188,   186,   286,   157,
     180,   157,   161,   180,   186
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
     352,   352,   353,   353,   353,   354,   354,   354,   354,   355,
     355,   355,   356,   357,   357,   358,   358,   359,   359,   360,
     361,   361,   362,   361,   361,   363,   361,   361,   361,   364,
     364,   365,   365,   366,   366,   367,   367,   367,   367,   368,
     368,   369,   369,   369,   370,   370,   370,   370,   371,   371,
     371,   371,   372,   372,   372,   372,   372,   372,   372,   373,
     373,   373,   373,   374,   374,   375,   375,   376,   376,   377,
     377,   377,   377,   377,   378,   378,   378,   378,   378,   379,
     379,   380,   380,   380,   381,   381,   381,   382,   382,   383,
     383,   383,   383,   384,   384,   385,   385,   385,   385,   385,
     386,   386,   387,   387,   388,   388,   388,   388,   388,   389,
     389,   390,   390,   392,   391,   393,   391,   391,   391,   391,
     394,   394,   394,   394,   395,   395,   395,   395,   396,   396,
     397,   397,   398,   398,   399,   399,   399,   399,   400,   400,
     400,   401,   401,   402,   402,   403,   403,   403,   403,   404,
     404,   405,   405,   406,   406,   406,   407,   407,   408,   408,
     409,   409,   410,   410,   411,   412,   413,   413,   413,   413,
     413,   413,   413,   413,   413,   413,   413,   414,   413,   415,
     413,   416,   413,   417,   413,   418,   413,   419,   419,   419,
     420,   420,   421,   421,   421,   421,   421,   421,   421,   421,
     421,   421,   422,   422,   422,   422,   423,   424,   424,   425,
     425,   426,   426,   427,   427,   427,   428,   428,   429,   429,
     429,   430,   430,   430,   430,   430,   430,   431,   431,   432,
     432,   432,   432,   433,   433,   433,   433,   434,   434,   434,
     434,   434,   434,   434,   435,   435,   435,   435,   436,   436,
     436,   437,   437,   437,   437,   437,   438,   438,   438,   438,
     439,   439,   439,   439,   439,   439,   440,   440,   440,   441,
     441,   441,   441,   441,   442,   442,   442,   442,   443,   443,
     443,   443,   443,   443,   444,   444,   445,   445,   445,   445,
     446,   446,   446,   446,   447,   447,   447,   447,   447,   447,
     447,   448,   448,   448,   448,   449,   449,   449,   450,   450,
     450,   450,   450,   451,   451,   451,   451,   452,   452,   452,
     452,   452,   452,   453,   453,   453,   453,   453,   454,   454,
     454,   455,   455,   455,   455,   456,   456,   456,   457,   457,
     457,   457,   457,   458,   458,   459,   459,   459,   460,   460,
     461,   461,   462,   462,   462,   463,   463,   463,   463,   463,
     464,   464,   464,   464,   465,   465,   465,   466,   466,   466,
     466,   466,   467,   467,   467,   467,   467,   467,   468,   468,
     469,   469,   469,   469,   470,   470,   471,   471,   471,   471,
     472,   472,   472,   472,   472,   473,   473,   473,   473,   474,
     474,   474,   475,   475,   475,   476,   476,   476,   476,   476,
     476,   477,   477,   477,   478,   478,   478,   478,   478,   479,
     479,   479,   479,   480,   480,   481,   481,   481,   482,   482,
     483,   483,   483,   483,   483,   483,   484,   484,   484,   484,
     484,   484,   484,   484,   484,   484,   485,   485,   485,   485,
     486,   486,   486,   487,   487,   488,   488,   488,   488,   488,
     488,   489,   489,   489,   489,   489,   489,   490,   490,   490,
     491,   491,   491,   492,   492,   493,   493
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
       2,     1,     0,     1,     4,     1,     2,     2,     2,     0,
       1,     4,     1,     2,     3,     1,     2,     0,     1,     2,
       7,     8,     0,     9,     8,     0,    11,    10,     1,     2,
       3,     0,     1,     3,     3,     3,     2,     5,     5,     1,
       1,     0,     2,     5,     0,     1,     1,     3,     1,     1,
       3,     3,     0,     1,     1,     1,     3,     3,     3,     1,
       3,     3,     5,     1,     3,     3,     3,     2,     3,     1,
       3,     3,     4,     1,     1,     1,     1,     2,     1,     1,
       3,     1,     1,     1,     1,     1,     2,     1,     1,     0,
       2,     2,     4,     1,     4,     0,     1,     2,     3,     4,
       2,     2,     1,     2,     2,     5,     5,     7,     6,     1,
       3,     0,     2,     0,     5,     0,     5,     3,     1,     8,
       0,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       1,     2,     5,     6,     1,     1,     3,     3,     2,     3,
       3,     2,     4,     1,     4,     7,     5,    10,     8,     1,
       4,     2,     2,     1,     1,     5,     2,     5,     0,     1,
       3,     4,     0,     1,     0,     0,     1,     1,     2,     2,
       2,     2,     2,     2,     1,     2,     5,     0,     6,     0,
       8,     0,     7,     0,     7,     0,     8,     1,     2,     3,
       0,     5,     3,     4,     4,     4,     4,     5,     5,     5,
       5,     6,     1,     1,     1,     1,     3,     0,     5,     0,
       1,     1,     2,     6,     4,     4,     1,     3,     0,     1,
       4,     1,     1,     1,     1,     1,     1,     1,     3,     2,
       1,     2,     2,     2,     3,     4,     5,     2,     4,     5,
       4,     5,     3,     4,     6,     7,     3,     4,     2,     1,
       2,     4,     6,     7,     3,     4,     2,     3,     4,     5,
       4,     5,     4,     5,     3,     4,     1,     1,     1,     4,
       6,     7,     3,     4,     2,     3,     3,     4,     4,     5,
       4,     5,     3,     4,     1,     3,     2,     1,     2,     2,
       2,     3,     4,     5,     2,     4,     5,     4,     5,     3,
       4,     6,     7,     3,     4,     2,     1,     2,     4,     6,
       7,     3,     4,     2,     3,     4,     5,     4,     5,     4,
       5,     3,     4,     2,     4,     1,     2,     2,     2,     3,
       4,     2,     4,     4,     3,     4,     6,     3,     2,     4,
       1,     2,     2,     1,     1,     2,     3,     4,     2,     4,
       4,     6,     1,     2,     2,     1,     2,     2,     3,     4,
       1,     4,     4,     3,     3,     6,     3,     2,     3,     7,
       5,     1,     1,     1,     3,     3,     3,     5,     1,     1,
       5,     5,     6,     6,     0,     1,     1,     3,     2,     2,
       1,     2,     2,     3,     4,     1,     4,     4,     3,     3,
       6,     3,     1,     2,     1,     2,     6,     5,     6,     7,
       7,     1,     2,     2,     1,     2,     2,     3,     4,     1,
       4,     4,     3,     6,     3,     1,     1,     2,     1,     1,
       2,     3,     2,     3,     2,     3,     3,     2,     4,     3,
       2,     3,     2,     4,     3,     2,     6,     6,     6,     7,
       1,     2,     1,     1,     1,     2,     3,     2,     3,     2,
       3,     3,     4,     2,     3,     4,     2,     5,     6,     7,
       5,     6,     6,     0,     1,     0,     2
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
#line 8456 "Parser/parser.cc"
    break;

  case 3:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8462 "Parser/parser.cc"
    break;

  case 4:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8468 "Parser/parser.cc"
    break;

  case 5:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8474 "Parser/parser.cc"
    break;

  case 6:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8480 "Parser/parser.cc"
    break;

  case 7:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8486 "Parser/parser.cc"
    break;

  case 8:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8492 "Parser/parser.cc"
    break;

  case 20:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8498 "Parser/parser.cc"
    break;

  case 21:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 22:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8510 "Parser/parser.cc"
    break;

  case 23:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8520 "Parser/parser.cc"
    break;

  case 24:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8526 "Parser/parser.cc"
    break;

  case 25:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8532 "Parser/parser.cc"
    break;

  case 26:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8538 "Parser/parser.cc"
    break;

  case 28:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8544 "Parser/parser.cc"
    break;

  case 29:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8550 "Parser/parser.cc"
    break;

  case 30:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8556 "Parser/parser.cc"
    break;

  case 31:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8562 "Parser/parser.cc"
    break;

  case 32:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8572 "Parser/parser.cc"
    break;

  case 33:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8578 "Parser/parser.cc"
    break;

  case 34:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8584 "Parser/parser.cc"
    break;

  case 35:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8590 "Parser/parser.cc"
    break;

  case 36:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8596 "Parser/parser.cc"
    break;

  case 37:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8602 "Parser/parser.cc"
    break;

  case 38:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8608 "Parser/parser.cc"
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
#line 8620 "Parser/parser.cc"
    break;

  case 41:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8629 "Parser/parser.cc"
    break;

  case 42:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8635 "Parser/parser.cc"
    break;

  case 44:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8641 "Parser/parser.cc"
    break;

  case 45:
#line 772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8647 "Parser/parser.cc"
    break;

  case 46:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8653 "Parser/parser.cc"
    break;

  case 47:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8659 "Parser/parser.cc"
    break;

  case 48:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8669 "Parser/parser.cc"
    break;

  case 49:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8675 "Parser/parser.cc"
    break;

  case 50:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8682 "Parser/parser.cc"
    break;

  case 51:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8688 "Parser/parser.cc"
    break;

  case 52:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8694 "Parser/parser.cc"
    break;

  case 53:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8700 "Parser/parser.cc"
    break;

  case 54:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8706 "Parser/parser.cc"
    break;

  case 55:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8712 "Parser/parser.cc"
    break;

  case 56:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8718 "Parser/parser.cc"
    break;

  case 57:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8724 "Parser/parser.cc"
    break;

  case 58:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8730 "Parser/parser.cc"
    break;

  case 59:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8736 "Parser/parser.cc"
    break;

  case 60:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8742 "Parser/parser.cc"
    break;

  case 61:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8748 "Parser/parser.cc"
    break;

  case 62:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8754 "Parser/parser.cc"
    break;

  case 63:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8760 "Parser/parser.cc"
    break;

  case 64:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8766 "Parser/parser.cc"
    break;

  case 65:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8772 "Parser/parser.cc"
    break;

  case 66:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8778 "Parser/parser.cc"
    break;

  case 67:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8784 "Parser/parser.cc"
    break;

  case 68:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8794 "Parser/parser.cc"
    break;

  case 69:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8800 "Parser/parser.cc"
    break;

  case 72:
#line 859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8806 "Parser/parser.cc"
    break;

  case 73:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8812 "Parser/parser.cc"
    break;

  case 76:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8818 "Parser/parser.cc"
    break;

  case 78:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8824 "Parser/parser.cc"
    break;

  case 79:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8830 "Parser/parser.cc"
    break;

  case 80:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8836 "Parser/parser.cc"
    break;

  case 81:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8842 "Parser/parser.cc"
    break;

  case 82:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8848 "Parser/parser.cc"
    break;

  case 83:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8854 "Parser/parser.cc"
    break;

  case 84:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8860 "Parser/parser.cc"
    break;

  case 85:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8866 "Parser/parser.cc"
    break;

  case 86:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8874 "Parser/parser.cc"
    break;

  case 87:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8880 "Parser/parser.cc"
    break;

  case 88:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8889 "Parser/parser.cc"
    break;

  case 91:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8895 "Parser/parser.cc"
    break;

  case 92:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8901 "Parser/parser.cc"
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
#line 8921 "Parser/parser.cc"
    break;

  case 94:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8927 "Parser/parser.cc"
    break;

  case 95:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8933 "Parser/parser.cc"
    break;

  case 96:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8939 "Parser/parser.cc"
    break;

  case 97:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8945 "Parser/parser.cc"
    break;

  case 98:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8951 "Parser/parser.cc"
    break;

  case 99:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8957 "Parser/parser.cc"
    break;

  case 100:
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8963 "Parser/parser.cc"
    break;

  case 101:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8969 "Parser/parser.cc"
    break;

  case 102:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8975 "Parser/parser.cc"
    break;

  case 103:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8981 "Parser/parser.cc"
    break;

  case 104:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8990 "Parser/parser.cc"
    break;

  case 105:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8996 "Parser/parser.cc"
    break;

  case 106:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9002 "Parser/parser.cc"
    break;

  case 107:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9008 "Parser/parser.cc"
    break;

  case 108:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9014 "Parser/parser.cc"
    break;

  case 109:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9020 "Parser/parser.cc"
    break;

  case 110:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9026 "Parser/parser.cc"
    break;

  case 111:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9032 "Parser/parser.cc"
    break;

  case 113:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9038 "Parser/parser.cc"
    break;

  case 114:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9044 "Parser/parser.cc"
    break;

  case 115:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9050 "Parser/parser.cc"
    break;

  case 116:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9056 "Parser/parser.cc"
    break;

  case 117:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9062 "Parser/parser.cc"
    break;

  case 118:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9068 "Parser/parser.cc"
    break;

  case 119:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9074 "Parser/parser.cc"
    break;

  case 120:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9080 "Parser/parser.cc"
    break;

  case 128:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9086 "Parser/parser.cc"
    break;

  case 130:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9092 "Parser/parser.cc"
    break;

  case 131:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9098 "Parser/parser.cc"
    break;

  case 132:
#line 1031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9104 "Parser/parser.cc"
    break;

  case 134:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9110 "Parser/parser.cc"
    break;

  case 135:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9116 "Parser/parser.cc"
    break;

  case 137:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9122 "Parser/parser.cc"
    break;

  case 138:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9128 "Parser/parser.cc"
    break;

  case 140:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9134 "Parser/parser.cc"
    break;

  case 141:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9140 "Parser/parser.cc"
    break;

  case 142:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9146 "Parser/parser.cc"
    break;

  case 143:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9152 "Parser/parser.cc"
    break;

  case 145:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9158 "Parser/parser.cc"
    break;

  case 146:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9164 "Parser/parser.cc"
    break;

  case 148:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9170 "Parser/parser.cc"
    break;

  case 150:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9176 "Parser/parser.cc"
    break;

  case 152:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9182 "Parser/parser.cc"
    break;

  case 154:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9188 "Parser/parser.cc"
    break;

  case 156:
#line 1097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9194 "Parser/parser.cc"
    break;

  case 158:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9200 "Parser/parser.cc"
    break;

  case 159:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9206 "Parser/parser.cc"
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
#line 9218 "Parser/parser.cc"
    break;

  case 163:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9224 "Parser/parser.cc"
    break;

  case 164:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9230 "Parser/parser.cc"
    break;

  case 168:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9236 "Parser/parser.cc"
    break;

  case 169:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9242 "Parser/parser.cc"
    break;

  case 170:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9248 "Parser/parser.cc"
    break;

  case 171:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9254 "Parser/parser.cc"
    break;

  case 172:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9260 "Parser/parser.cc"
    break;

  case 173:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9266 "Parser/parser.cc"
    break;

  case 174:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9272 "Parser/parser.cc"
    break;

  case 175:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9278 "Parser/parser.cc"
    break;

  case 176:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9284 "Parser/parser.cc"
    break;

  case 177:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9290 "Parser/parser.cc"
    break;

  case 178:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9296 "Parser/parser.cc"
    break;

  case 179:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9302 "Parser/parser.cc"
    break;

  case 180:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9308 "Parser/parser.cc"
    break;

  case 181:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9314 "Parser/parser.cc"
    break;

  case 182:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9320 "Parser/parser.cc"
    break;

  case 184:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9326 "Parser/parser.cc"
    break;

  case 185:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9332 "Parser/parser.cc"
    break;

  case 186:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9338 "Parser/parser.cc"
    break;

  case 188:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9344 "Parser/parser.cc"
    break;

  case 189:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9350 "Parser/parser.cc"
    break;

  case 204:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9356 "Parser/parser.cc"
    break;

  case 206:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9362 "Parser/parser.cc"
    break;

  case 207:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9368 "Parser/parser.cc"
    break;

  case 208:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9379 "Parser/parser.cc"
    break;

  case 209:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9385 "Parser/parser.cc"
    break;

  case 210:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9391 "Parser/parser.cc"
    break;

  case 212:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9397 "Parser/parser.cc"
    break;

  case 213:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9403 "Parser/parser.cc"
    break;

  case 214:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9409 "Parser/parser.cc"
    break;

  case 215:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9415 "Parser/parser.cc"
    break;

  case 216:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9421 "Parser/parser.cc"
    break;

  case 219:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9427 "Parser/parser.cc"
    break;

  case 220:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9434 "Parser/parser.cc"
    break;

  case 221:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9440 "Parser/parser.cc"
    break;

  case 222:
#line 1297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9446 "Parser/parser.cc"
    break;

  case 223:
#line 1299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9452 "Parser/parser.cc"
    break;

  case 224:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9458 "Parser/parser.cc"
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
#line 9472 "Parser/parser.cc"
    break;

  case 226:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9478 "Parser/parser.cc"
    break;

  case 227:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9484 "Parser/parser.cc"
    break;

  case 228:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9493 "Parser/parser.cc"
    break;

  case 229:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9499 "Parser/parser.cc"
    break;

  case 230:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9505 "Parser/parser.cc"
    break;

  case 231:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9511 "Parser/parser.cc"
    break;

  case 232:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9517 "Parser/parser.cc"
    break;

  case 233:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9523 "Parser/parser.cc"
    break;

  case 234:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9529 "Parser/parser.cc"
    break;

  case 235:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9535 "Parser/parser.cc"
    break;

  case 237:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9541 "Parser/parser.cc"
    break;

  case 238:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9547 "Parser/parser.cc"
    break;

  case 239:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9553 "Parser/parser.cc"
    break;

  case 240:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9559 "Parser/parser.cc"
    break;

  case 241:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9565 "Parser/parser.cc"
    break;

  case 242:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9571 "Parser/parser.cc"
    break;

  case 243:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9577 "Parser/parser.cc"
    break;

  case 245:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9583 "Parser/parser.cc"
    break;

  case 246:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9589 "Parser/parser.cc"
    break;

  case 247:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9595 "Parser/parser.cc"
    break;

  case 249:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 250:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9607 "Parser/parser.cc"
    break;

  case 251:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9613 "Parser/parser.cc"
    break;

  case 252:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9622 "Parser/parser.cc"
    break;

  case 253:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9628 "Parser/parser.cc"
    break;

  case 254:
#line 1397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9634 "Parser/parser.cc"
    break;

  case 255:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9640 "Parser/parser.cc"
    break;

  case 256:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9649 "Parser/parser.cc"
    break;

  case 257:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9655 "Parser/parser.cc"
    break;

  case 258:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9661 "Parser/parser.cc"
    break;

  case 259:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9667 "Parser/parser.cc"
    break;

  case 260:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9676 "Parser/parser.cc"
    break;

  case 261:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9682 "Parser/parser.cc"
    break;

  case 262:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9688 "Parser/parser.cc"
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
#line 9707 "Parser/parser.cc"
    break;

  case 265:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9713 "Parser/parser.cc"
    break;

  case 266:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9722 "Parser/parser.cc"
    break;

  case 267:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9728 "Parser/parser.cc"
    break;

  case 268:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9734 "Parser/parser.cc"
    break;

  case 269:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9740 "Parser/parser.cc"
    break;

  case 270:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9746 "Parser/parser.cc"
    break;

  case 271:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9752 "Parser/parser.cc"
    break;

  case 272:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9758 "Parser/parser.cc"
    break;

  case 273:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9767 "Parser/parser.cc"
    break;

  case 274:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9776 "Parser/parser.cc"
    break;

  case 275:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9782 "Parser/parser.cc"
    break;

  case 276:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9791 "Parser/parser.cc"
    break;

  case 277:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9800 "Parser/parser.cc"
    break;

  case 278:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9806 "Parser/parser.cc"
    break;

  case 279:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9812 "Parser/parser.cc"
    break;

  case 280:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9818 "Parser/parser.cc"
    break;

  case 281:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9824 "Parser/parser.cc"
    break;

  case 282:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9830 "Parser/parser.cc"
    break;

  case 283:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9836 "Parser/parser.cc"
    break;

  case 284:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9842 "Parser/parser.cc"
    break;

  case 285:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9848 "Parser/parser.cc"
    break;

  case 286:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9857 "Parser/parser.cc"
    break;

  case 287:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9867 "Parser/parser.cc"
    break;

  case 288:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9873 "Parser/parser.cc"
    break;

  case 289:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9879 "Parser/parser.cc"
    break;

  case 290:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9888 "Parser/parser.cc"
    break;

  case 291:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9898 "Parser/parser.cc"
    break;

  case 292:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9904 "Parser/parser.cc"
    break;

  case 293:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9913 "Parser/parser.cc"
    break;

  case 294:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9923 "Parser/parser.cc"
    break;

  case 295:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9929 "Parser/parser.cc"
    break;

  case 296:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9935 "Parser/parser.cc"
    break;

  case 297:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9941 "Parser/parser.cc"
    break;

  case 298:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9947 "Parser/parser.cc"
    break;

  case 299:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9956 "Parser/parser.cc"
    break;

  case 300:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9966 "Parser/parser.cc"
    break;

  case 301:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9972 "Parser/parser.cc"
    break;

  case 302:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9981 "Parser/parser.cc"
    break;

  case 303:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9991 "Parser/parser.cc"
    break;

  case 304:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9997 "Parser/parser.cc"
    break;

  case 305:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10006 "Parser/parser.cc"
    break;

  case 306:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10016 "Parser/parser.cc"
    break;

  case 307:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10022 "Parser/parser.cc"
    break;

  case 308:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 10031 "Parser/parser.cc"
    break;

  case 309:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 10042 "Parser/parser.cc"
    break;

  case 312:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10048 "Parser/parser.cc"
    break;

  case 313:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10054 "Parser/parser.cc"
    break;

  case 314:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10060 "Parser/parser.cc"
    break;

  case 315:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10066 "Parser/parser.cc"
    break;

  case 316:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10072 "Parser/parser.cc"
    break;

  case 318:
#line 1637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10078 "Parser/parser.cc"
    break;

  case 319:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10084 "Parser/parser.cc"
    break;

  case 320:
#line 1644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10090 "Parser/parser.cc"
    break;

  case 321:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10096 "Parser/parser.cc"
    break;

  case 322:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10102 "Parser/parser.cc"
    break;

  case 323:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10108 "Parser/parser.cc"
    break;

  case 324:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10114 "Parser/parser.cc"
    break;

  case 325:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10120 "Parser/parser.cc"
    break;

  case 326:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10126 "Parser/parser.cc"
    break;

  case 327:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10132 "Parser/parser.cc"
    break;

  case 328:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10138 "Parser/parser.cc"
    break;

  case 329:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10144 "Parser/parser.cc"
    break;

  case 330:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10150 "Parser/parser.cc"
    break;

  case 331:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10156 "Parser/parser.cc"
    break;

  case 332:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10162 "Parser/parser.cc"
    break;

  case 333:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10168 "Parser/parser.cc"
    break;

  case 334:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10174 "Parser/parser.cc"
    break;

  case 335:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10180 "Parser/parser.cc"
    break;

  case 336:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10186 "Parser/parser.cc"
    break;

  case 337:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10192 "Parser/parser.cc"
    break;

  case 338:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10198 "Parser/parser.cc"
    break;

  case 339:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10204 "Parser/parser.cc"
    break;

  case 342:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10210 "Parser/parser.cc"
    break;

  case 343:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10219 "Parser/parser.cc"
    break;

  case 344:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10225 "Parser/parser.cc"
    break;

  case 345:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10231 "Parser/parser.cc"
    break;

  case 348:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10237 "Parser/parser.cc"
    break;

  case 349:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10243 "Parser/parser.cc"
    break;

  case 352:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10249 "Parser/parser.cc"
    break;

  case 353:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10255 "Parser/parser.cc"
    break;

  case 354:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10261 "Parser/parser.cc"
    break;

  case 355:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10267 "Parser/parser.cc"
    break;

  case 356:
#line 1751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10273 "Parser/parser.cc"
    break;

  case 357:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10279 "Parser/parser.cc"
    break;

  case 358:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10285 "Parser/parser.cc"
    break;

  case 359:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10291 "Parser/parser.cc"
    break;

  case 360:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10297 "Parser/parser.cc"
    break;

  case 363:
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10303 "Parser/parser.cc"
    break;

  case 364:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10309 "Parser/parser.cc"
    break;

  case 365:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10315 "Parser/parser.cc"
    break;

  case 366:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10321 "Parser/parser.cc"
    break;

  case 367:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10327 "Parser/parser.cc"
    break;

  case 368:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10333 "Parser/parser.cc"
    break;

  case 369:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10339 "Parser/parser.cc"
    break;

  case 370:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10345 "Parser/parser.cc"
    break;

  case 371:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10351 "Parser/parser.cc"
    break;

  case 372:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10357 "Parser/parser.cc"
    break;

  case 373:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10363 "Parser/parser.cc"
    break;

  case 374:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10369 "Parser/parser.cc"
    break;

  case 375:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10375 "Parser/parser.cc"
    break;

  case 376:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10381 "Parser/parser.cc"
    break;

  case 377:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10387 "Parser/parser.cc"
    break;

  case 378:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10393 "Parser/parser.cc"
    break;

  case 379:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10399 "Parser/parser.cc"
    break;

  case 380:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10405 "Parser/parser.cc"
    break;

  case 381:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10411 "Parser/parser.cc"
    break;

  case 382:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10417 "Parser/parser.cc"
    break;

  case 383:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10423 "Parser/parser.cc"
    break;

  case 384:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10429 "Parser/parser.cc"
    break;

  case 385:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10435 "Parser/parser.cc"
    break;

  case 387:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10441 "Parser/parser.cc"
    break;

  case 388:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10447 "Parser/parser.cc"
    break;

  case 389:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10453 "Parser/parser.cc"
    break;

  case 394:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10459 "Parser/parser.cc"
    break;

  case 395:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10465 "Parser/parser.cc"
    break;

  case 396:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10471 "Parser/parser.cc"
    break;

  case 397:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10477 "Parser/parser.cc"
    break;

  case 398:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10483 "Parser/parser.cc"
    break;

  case 399:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10489 "Parser/parser.cc"
    break;

  case 400:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10495 "Parser/parser.cc"
    break;

  case 401:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10501 "Parser/parser.cc"
    break;

  case 404:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10507 "Parser/parser.cc"
    break;

  case 405:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10513 "Parser/parser.cc"
    break;

  case 406:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10522 "Parser/parser.cc"
    break;

  case 407:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10528 "Parser/parser.cc"
    break;

  case 408:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10534 "Parser/parser.cc"
    break;

  case 409:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10540 "Parser/parser.cc"
    break;

  case 410:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10549 "Parser/parser.cc"
    break;

  case 411:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10558 "Parser/parser.cc"
    break;

  case 412:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10564 "Parser/parser.cc"
    break;

  case 415:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 416:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10576 "Parser/parser.cc"
    break;

  case 418:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10582 "Parser/parser.cc"
    break;

  case 419:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10588 "Parser/parser.cc"
    break;

  case 429:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10594 "Parser/parser.cc"
    break;

  case 430:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10600 "Parser/parser.cc"
    break;

  case 434:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10606 "Parser/parser.cc"
    break;

  case 436:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10612 "Parser/parser.cc"
    break;

  case 437:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10618 "Parser/parser.cc"
    break;

  case 438:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10624 "Parser/parser.cc"
    break;

  case 439:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10630 "Parser/parser.cc"
    break;

  case 440:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10636 "Parser/parser.cc"
    break;

  case 441:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10642 "Parser/parser.cc"
    break;

  case 443:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10648 "Parser/parser.cc"
    break;

  case 444:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10654 "Parser/parser.cc"
    break;

  case 445:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10660 "Parser/parser.cc"
    break;

  case 446:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10671 "Parser/parser.cc"
    break;

  case 447:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10677 "Parser/parser.cc"
    break;

  case 448:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10683 "Parser/parser.cc"
    break;

  case 449:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10689 "Parser/parser.cc"
    break;

  case 450:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10695 "Parser/parser.cc"
    break;

  case 451:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10701 "Parser/parser.cc"
    break;

  case 452:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10707 "Parser/parser.cc"
    break;

  case 453:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10716 "Parser/parser.cc"
    break;

  case 454:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10725 "Parser/parser.cc"
    break;

  case 455:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10734 "Parser/parser.cc"
    break;

  case 456:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10745 "Parser/parser.cc"
    break;

  case 457:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10754 "Parser/parser.cc"
    break;

  case 458:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10760 "Parser/parser.cc"
    break;

  case 459:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10766 "Parser/parser.cc"
    break;

  case 460:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10772 "Parser/parser.cc"
    break;

  case 461:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10780 "Parser/parser.cc"
    break;

  case 462:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10788 "Parser/parser.cc"
    break;

  case 463:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10794 "Parser/parser.cc"
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
#line 10809 "Parser/parser.cc"
    break;

  case 467:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10815 "Parser/parser.cc"
    break;

  case 468:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10821 "Parser/parser.cc"
    break;

  case 469:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10827 "Parser/parser.cc"
    break;

  case 470:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10833 "Parser/parser.cc"
    break;

  case 471:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10839 "Parser/parser.cc"
    break;

  case 477:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10849 "Parser/parser.cc"
    break;

  case 490:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10855 "Parser/parser.cc"
    break;

  case 493:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10861 "Parser/parser.cc"
    break;

  case 494:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10867 "Parser/parser.cc"
    break;

  case 496:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10873 "Parser/parser.cc"
    break;

  case 497:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10879 "Parser/parser.cc"
    break;

  case 498:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10885 "Parser/parser.cc"
    break;

  case 499:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10891 "Parser/parser.cc"
    break;

  case 500:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10897 "Parser/parser.cc"
    break;

  case 501:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10903 "Parser/parser.cc"
    break;

  case 503:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10909 "Parser/parser.cc"
    break;

  case 504:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10915 "Parser/parser.cc"
    break;

  case 506:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10921 "Parser/parser.cc"
    break;

  case 507:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10927 "Parser/parser.cc"
    break;

  case 508:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10933 "Parser/parser.cc"
    break;

  case 509:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10939 "Parser/parser.cc"
    break;

  case 510:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 10945 "Parser/parser.cc"
    break;

  case 511:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 10951 "Parser/parser.cc"
    break;

  case 512:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 10957 "Parser/parser.cc"
    break;

  case 513:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 10963 "Parser/parser.cc"
    break;

  case 514:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 10969 "Parser/parser.cc"
    break;

  case 515:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 10975 "Parser/parser.cc"
    break;

  case 516:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10981 "Parser/parser.cc"
    break;

  case 517:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 10987 "Parser/parser.cc"
    break;

  case 518:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 10993 "Parser/parser.cc"
    break;

  case 519:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 10999 "Parser/parser.cc"
    break;

  case 520:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11005 "Parser/parser.cc"
    break;

  case 521:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11011 "Parser/parser.cc"
    break;

  case 522:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11017 "Parser/parser.cc"
    break;

  case 523:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11023 "Parser/parser.cc"
    break;

  case 524:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11029 "Parser/parser.cc"
    break;

  case 525:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11035 "Parser/parser.cc"
    break;

  case 526:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11041 "Parser/parser.cc"
    break;

  case 527:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11047 "Parser/parser.cc"
    break;

  case 528:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11053 "Parser/parser.cc"
    break;

  case 529:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11059 "Parser/parser.cc"
    break;

  case 530:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11065 "Parser/parser.cc"
    break;

  case 531:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11071 "Parser/parser.cc"
    break;

  case 532:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11077 "Parser/parser.cc"
    break;

  case 533:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11083 "Parser/parser.cc"
    break;

  case 534:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11089 "Parser/parser.cc"
    break;

  case 535:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11095 "Parser/parser.cc"
    break;

  case 536:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11101 "Parser/parser.cc"
    break;

  case 537:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11107 "Parser/parser.cc"
    break;

  case 538:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11113 "Parser/parser.cc"
    break;

  case 539:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11119 "Parser/parser.cc"
    break;

  case 540:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11125 "Parser/parser.cc"
    break;

  case 541:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11131 "Parser/parser.cc"
    break;

  case 542:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11137 "Parser/parser.cc"
    break;

  case 543:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11143 "Parser/parser.cc"
    break;

  case 545:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11149 "Parser/parser.cc"
    break;

  case 547:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11155 "Parser/parser.cc"
    break;

  case 548:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11161 "Parser/parser.cc"
    break;

  case 549:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11167 "Parser/parser.cc"
    break;

  case 551:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11173 "Parser/parser.cc"
    break;

  case 552:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11179 "Parser/parser.cc"
    break;

  case 553:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11185 "Parser/parser.cc"
    break;

  case 554:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11191 "Parser/parser.cc"
    break;

  case 556:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11197 "Parser/parser.cc"
    break;

  case 558:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11203 "Parser/parser.cc"
    break;

  case 559:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 560:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11215 "Parser/parser.cc"
    break;

  case 561:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11221 "Parser/parser.cc"
    break;

  case 562:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11227 "Parser/parser.cc"
    break;

  case 563:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11233 "Parser/parser.cc"
    break;

  case 564:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11239 "Parser/parser.cc"
    break;

  case 565:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11245 "Parser/parser.cc"
    break;

  case 566:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11251 "Parser/parser.cc"
    break;

  case 568:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11257 "Parser/parser.cc"
    break;

  case 569:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11263 "Parser/parser.cc"
    break;

  case 570:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 572:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11275 "Parser/parser.cc"
    break;

  case 573:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 574:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11290 "Parser/parser.cc"
    break;

  case 576:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11296 "Parser/parser.cc"
    break;

  case 577:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11302 "Parser/parser.cc"
    break;

  case 578:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11308 "Parser/parser.cc"
    break;

  case 580:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11314 "Parser/parser.cc"
    break;

  case 581:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11320 "Parser/parser.cc"
    break;

  case 583:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11326 "Parser/parser.cc"
    break;

  case 584:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11332 "Parser/parser.cc"
    break;

  case 585:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11338 "Parser/parser.cc"
    break;

  case 586:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11344 "Parser/parser.cc"
    break;

  case 587:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11350 "Parser/parser.cc"
    break;

  case 588:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11356 "Parser/parser.cc"
    break;

  case 589:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11362 "Parser/parser.cc"
    break;

  case 590:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11368 "Parser/parser.cc"
    break;

  case 591:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11374 "Parser/parser.cc"
    break;

  case 593:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11380 "Parser/parser.cc"
    break;

  case 594:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11386 "Parser/parser.cc"
    break;

  case 595:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11392 "Parser/parser.cc"
    break;

  case 596:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11398 "Parser/parser.cc"
    break;

  case 597:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11404 "Parser/parser.cc"
    break;

  case 602:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11410 "Parser/parser.cc"
    break;

  case 603:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11416 "Parser/parser.cc"
    break;

  case 604:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11425 "Parser/parser.cc"
    break;

  case 605:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11433 "Parser/parser.cc"
    break;

  case 606:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11442 "Parser/parser.cc"
    break;

  case 607:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11451 "Parser/parser.cc"
    break;

  case 608:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11460 "Parser/parser.cc"
    break;

  case 609:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11469 "Parser/parser.cc"
    break;

  case 611:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11475 "Parser/parser.cc"
    break;

  case 612:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11481 "Parser/parser.cc"
    break;

  case 613:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11491 "Parser/parser.cc"
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
#line 11510 "Parser/parser.cc"
    break;

  case 617:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11516 "Parser/parser.cc"
    break;

  case 618:
#line 2601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11522 "Parser/parser.cc"
    break;

  case 619:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11528 "Parser/parser.cc"
    break;

  case 620:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11534 "Parser/parser.cc"
    break;

  case 621:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11540 "Parser/parser.cc"
    break;

  case 622:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11546 "Parser/parser.cc"
    break;

  case 623:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11555 "Parser/parser.cc"
    break;

  case 624:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11561 "Parser/parser.cc"
    break;

  case 625:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11570 "Parser/parser.cc"
    break;

  case 626:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11576 "Parser/parser.cc"
    break;

  case 627:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11585 "Parser/parser.cc"
    break;

  case 628:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11591 "Parser/parser.cc"
    break;

  case 629:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11597 "Parser/parser.cc"
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
#line 11610 "Parser/parser.cc"
    break;

  case 631:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11619 "Parser/parser.cc"
    break;

  case 632:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11625 "Parser/parser.cc"
    break;

  case 633:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11631 "Parser/parser.cc"
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
#line 11644 "Parser/parser.cc"
    break;

  case 635:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11650 "Parser/parser.cc"
    break;

  case 638:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11656 "Parser/parser.cc"
    break;

  case 639:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11662 "Parser/parser.cc"
    break;

  case 642:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11668 "Parser/parser.cc"
    break;

  case 644:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11674 "Parser/parser.cc"
    break;

  case 645:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11680 "Parser/parser.cc"
    break;

  case 646:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11686 "Parser/parser.cc"
    break;

  case 647:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11692 "Parser/parser.cc"
    break;

  case 648:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11698 "Parser/parser.cc"
    break;

  case 649:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11704 "Parser/parser.cc"
    break;

  case 651:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11710 "Parser/parser.cc"
    break;

  case 653:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11716 "Parser/parser.cc"
    break;

  case 654:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11722 "Parser/parser.cc"
    break;

  case 656:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11728 "Parser/parser.cc"
    break;

  case 657:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11734 "Parser/parser.cc"
    break;

  case 659:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11740 "Parser/parser.cc"
    break;

  case 660:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11751 "Parser/parser.cc"
    break;

  case 661:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11765 "Parser/parser.cc"
    break;

  case 662:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11771 "Parser/parser.cc"
    break;

  case 663:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11777 "Parser/parser.cc"
    break;

  case 664:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11783 "Parser/parser.cc"
    break;

  case 665:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11794 "Parser/parser.cc"
    break;

  case 666:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11800 "Parser/parser.cc"
    break;

  case 667:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11806 "Parser/parser.cc"
    break;

  case 669:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11812 "Parser/parser.cc"
    break;

  case 670:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11818 "Parser/parser.cc"
    break;

  case 671:
#line 2796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11824 "Parser/parser.cc"
    break;

  case 672:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11830 "Parser/parser.cc"
    break;

  case 673:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11839 "Parser/parser.cc"
    break;

  case 674:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11848 "Parser/parser.cc"
    break;

  case 675:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11854 "Parser/parser.cc"
    break;

  case 676:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11864 "Parser/parser.cc"
    break;

  case 677:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11870 "Parser/parser.cc"
    break;

  case 678:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11876 "Parser/parser.cc"
    break;

  case 680:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11882 "Parser/parser.cc"
    break;

  case 681:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11888 "Parser/parser.cc"
    break;

  case 682:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11894 "Parser/parser.cc"
    break;

  case 683:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11900 "Parser/parser.cc"
    break;

  case 684:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11906 "Parser/parser.cc"
    break;

  case 685:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11912 "Parser/parser.cc"
    break;

  case 687:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11918 "Parser/parser.cc"
    break;

  case 690:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11924 "Parser/parser.cc"
    break;

  case 691:
#line 2862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11930 "Parser/parser.cc"
    break;

  case 692:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 11936 "Parser/parser.cc"
    break;

  case 693:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11942 "Parser/parser.cc"
    break;

  case 696:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11948 "Parser/parser.cc"
    break;

  case 697:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11954 "Parser/parser.cc"
    break;

  case 698:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11960 "Parser/parser.cc"
    break;

  case 700:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11966 "Parser/parser.cc"
    break;

  case 701:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11972 "Parser/parser.cc"
    break;

  case 702:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 11978 "Parser/parser.cc"
    break;

  case 704:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11984 "Parser/parser.cc"
    break;

  case 705:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11990 "Parser/parser.cc"
    break;

  case 706:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11996 "Parser/parser.cc"
    break;

  case 707:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12002 "Parser/parser.cc"
    break;

  case 708:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12008 "Parser/parser.cc"
    break;

  case 710:
#line 2919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12014 "Parser/parser.cc"
    break;

  case 711:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12020 "Parser/parser.cc"
    break;

  case 712:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12026 "Parser/parser.cc"
    break;

  case 717:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12032 "Parser/parser.cc"
    break;

  case 719:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12038 "Parser/parser.cc"
    break;

  case 720:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12044 "Parser/parser.cc"
    break;

  case 726:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12050 "Parser/parser.cc"
    break;

  case 729:
#line 2969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12056 "Parser/parser.cc"
    break;

  case 730:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12062 "Parser/parser.cc"
    break;

  case 731:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12068 "Parser/parser.cc"
    break;

  case 732:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12074 "Parser/parser.cc"
    break;

  case 733:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12080 "Parser/parser.cc"
    break;

  case 734:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12086 "Parser/parser.cc"
    break;

  case 735:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12092 "Parser/parser.cc"
    break;

  case 737:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12098 "Parser/parser.cc"
    break;

  case 738:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12104 "Parser/parser.cc"
    break;

  case 739:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12110 "Parser/parser.cc"
    break;

  case 741:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12116 "Parser/parser.cc"
    break;

  case 743:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12122 "Parser/parser.cc"
    break;

  case 744:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12128 "Parser/parser.cc"
    break;

  case 745:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12134 "Parser/parser.cc"
    break;

  case 746:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12140 "Parser/parser.cc"
    break;

  case 747:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12146 "Parser/parser.cc"
    break;

  case 748:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12152 "Parser/parser.cc"
    break;

  case 750:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12158 "Parser/parser.cc"
    break;

  case 751:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12164 "Parser/parser.cc"
    break;

  case 752:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12170 "Parser/parser.cc"
    break;

  case 753:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12181 "Parser/parser.cc"
    break;

  case 754:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12187 "Parser/parser.cc"
    break;

  case 755:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12193 "Parser/parser.cc"
    break;

  case 756:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12199 "Parser/parser.cc"
    break;

  case 757:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12208 "Parser/parser.cc"
    break;

  case 758:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12214 "Parser/parser.cc"
    break;

  case 759:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12224 "Parser/parser.cc"
    break;

  case 760:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12230 "Parser/parser.cc"
    break;

  case 761:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12236 "Parser/parser.cc"
    break;

  case 762:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12242 "Parser/parser.cc"
    break;

  case 763:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12248 "Parser/parser.cc"
    break;

  case 764:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12254 "Parser/parser.cc"
    break;

  case 765:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12260 "Parser/parser.cc"
    break;

  case 766:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12266 "Parser/parser.cc"
    break;

  case 767:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12272 "Parser/parser.cc"
    break;

  case 768:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12278 "Parser/parser.cc"
    break;

  case 771:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12284 "Parser/parser.cc"
    break;

  case 772:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12290 "Parser/parser.cc"
    break;

  case 773:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12296 "Parser/parser.cc"
    break;

  case 774:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12302 "Parser/parser.cc"
    break;

  case 776:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12308 "Parser/parser.cc"
    break;

  case 777:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12314 "Parser/parser.cc"
    break;

  case 778:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12320 "Parser/parser.cc"
    break;

  case 779:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12326 "Parser/parser.cc"
    break;

  case 780:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12332 "Parser/parser.cc"
    break;

  case 781:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12338 "Parser/parser.cc"
    break;

  case 782:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12344 "Parser/parser.cc"
    break;

  case 783:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12353 "Parser/parser.cc"
    break;

  case 784:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12362 "Parser/parser.cc"
    break;

  case 785:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12371 "Parser/parser.cc"
    break;

  case 786:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12377 "Parser/parser.cc"
    break;

  case 787:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12386 "Parser/parser.cc"
    break;

  case 788:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12392 "Parser/parser.cc"
    break;

  case 790:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12398 "Parser/parser.cc"
    break;

  case 795:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12404 "Parser/parser.cc"
    break;

  case 796:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12410 "Parser/parser.cc"
    break;

  case 797:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12416 "Parser/parser.cc"
    break;

  case 799:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 12422 "Parser/parser.cc"
    break;

  case 800:
#line 3223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12428 "Parser/parser.cc"
    break;

  case 801:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12434 "Parser/parser.cc"
    break;

  case 802:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12440 "Parser/parser.cc"
    break;

  case 804:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12446 "Parser/parser.cc"
    break;

  case 805:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12452 "Parser/parser.cc"
    break;

  case 806:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12458 "Parser/parser.cc"
    break;

  case 807:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12474 "Parser/parser.cc"
    break;

  case 808:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12480 "Parser/parser.cc"
    break;

  case 809:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12486 "Parser/parser.cc"
    break;

  case 810:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12492 "Parser/parser.cc"
    break;

  case 811:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12498 "Parser/parser.cc"
    break;

  case 812:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12504 "Parser/parser.cc"
    break;

  case 813:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12510 "Parser/parser.cc"
    break;

  case 815:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12519 "Parser/parser.cc"
    break;

  case 816:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12525 "Parser/parser.cc"
    break;

  case 817:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12534 "Parser/parser.cc"
    break;

  case 818:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12544 "Parser/parser.cc"
    break;

  case 819:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12553 "Parser/parser.cc"
    break;

  case 820:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12563 "Parser/parser.cc"
    break;

  case 821:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12574 "Parser/parser.cc"
    break;

  case 822:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12584 "Parser/parser.cc"
    break;

  case 823:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12595 "Parser/parser.cc"
    break;

  case 824:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12605 "Parser/parser.cc"
    break;

  case 825:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12616 "Parser/parser.cc"
    break;

  case 826:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12626 "Parser/parser.cc"
    break;

  case 828:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12632 "Parser/parser.cc"
    break;

  case 829:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12638 "Parser/parser.cc"
    break;

  case 830:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12644 "Parser/parser.cc"
    break;

  case 831:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12656 "Parser/parser.cc"
    break;

  case 832:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12667 "Parser/parser.cc"
    break;

  case 833:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12676 "Parser/parser.cc"
    break;

  case 834:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12685 "Parser/parser.cc"
    break;

  case 835:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12691 "Parser/parser.cc"
    break;

  case 836:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12697 "Parser/parser.cc"
    break;

  case 837:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12703 "Parser/parser.cc"
    break;

  case 838:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12712 "Parser/parser.cc"
    break;

  case 839:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12718 "Parser/parser.cc"
    break;

  case 840:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12724 "Parser/parser.cc"
    break;

  case 841:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12730 "Parser/parser.cc"
    break;

  case 846:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12736 "Parser/parser.cc"
    break;

  case 847:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12742 "Parser/parser.cc"
    break;

  case 848:
#line 3430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12752 "Parser/parser.cc"
    break;

  case 849:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12758 "Parser/parser.cc"
    break;

  case 852:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12764 "Parser/parser.cc"
    break;

  case 853:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12770 "Parser/parser.cc"
    break;

  case 854:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12776 "Parser/parser.cc"
    break;

  case 855:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12782 "Parser/parser.cc"
    break;

  case 857:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12788 "Parser/parser.cc"
    break;

  case 858:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12794 "Parser/parser.cc"
    break;

  case 859:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12800 "Parser/parser.cc"
    break;

  case 860:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12806 "Parser/parser.cc"
    break;

  case 865:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12812 "Parser/parser.cc"
    break;

  case 866:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12818 "Parser/parser.cc"
    break;

  case 867:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12824 "Parser/parser.cc"
    break;

  case 868:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12830 "Parser/parser.cc"
    break;

  case 869:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12836 "Parser/parser.cc"
    break;

  case 871:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12842 "Parser/parser.cc"
    break;

  case 872:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12848 "Parser/parser.cc"
    break;

  case 873:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12854 "Parser/parser.cc"
    break;

  case 874:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12860 "Parser/parser.cc"
    break;

  case 875:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12866 "Parser/parser.cc"
    break;

  case 876:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12872 "Parser/parser.cc"
    break;

  case 877:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12878 "Parser/parser.cc"
    break;

  case 878:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12884 "Parser/parser.cc"
    break;

  case 879:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12890 "Parser/parser.cc"
    break;

  case 880:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12896 "Parser/parser.cc"
    break;

  case 881:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12902 "Parser/parser.cc"
    break;

  case 882:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12908 "Parser/parser.cc"
    break;

  case 883:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12914 "Parser/parser.cc"
    break;

  case 884:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12920 "Parser/parser.cc"
    break;

  case 885:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12926 "Parser/parser.cc"
    break;

  case 886:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12932 "Parser/parser.cc"
    break;

  case 887:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12938 "Parser/parser.cc"
    break;

  case 888:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12944 "Parser/parser.cc"
    break;

  case 890:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12950 "Parser/parser.cc"
    break;

  case 891:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12956 "Parser/parser.cc"
    break;

  case 892:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12962 "Parser/parser.cc"
    break;

  case 893:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12968 "Parser/parser.cc"
    break;

  case 894:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12974 "Parser/parser.cc"
    break;

  case 895:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12980 "Parser/parser.cc"
    break;

  case 896:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12986 "Parser/parser.cc"
    break;

  case 897:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12992 "Parser/parser.cc"
    break;

  case 898:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12998 "Parser/parser.cc"
    break;

  case 899:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13004 "Parser/parser.cc"
    break;

  case 900:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13010 "Parser/parser.cc"
    break;

  case 901:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13016 "Parser/parser.cc"
    break;

  case 902:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13022 "Parser/parser.cc"
    break;

  case 903:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13028 "Parser/parser.cc"
    break;

  case 904:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13034 "Parser/parser.cc"
    break;

  case 905:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13040 "Parser/parser.cc"
    break;

  case 909:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13046 "Parser/parser.cc"
    break;

  case 910:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13052 "Parser/parser.cc"
    break;

  case 911:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13058 "Parser/parser.cc"
    break;

  case 912:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13064 "Parser/parser.cc"
    break;

  case 913:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13070 "Parser/parser.cc"
    break;

  case 914:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13076 "Parser/parser.cc"
    break;

  case 915:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13082 "Parser/parser.cc"
    break;

  case 916:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13088 "Parser/parser.cc"
    break;

  case 917:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13094 "Parser/parser.cc"
    break;

  case 918:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13100 "Parser/parser.cc"
    break;

  case 919:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13106 "Parser/parser.cc"
    break;

  case 920:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13112 "Parser/parser.cc"
    break;

  case 921:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13118 "Parser/parser.cc"
    break;

  case 922:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13124 "Parser/parser.cc"
    break;

  case 923:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13130 "Parser/parser.cc"
    break;

  case 924:
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13139 "Parser/parser.cc"
    break;

  case 925:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13145 "Parser/parser.cc"
    break;

  case 926:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13151 "Parser/parser.cc"
    break;

  case 928:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13157 "Parser/parser.cc"
    break;

  case 929:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13163 "Parser/parser.cc"
    break;

  case 930:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13169 "Parser/parser.cc"
    break;

  case 931:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13175 "Parser/parser.cc"
    break;

  case 932:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13181 "Parser/parser.cc"
    break;

  case 933:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13187 "Parser/parser.cc"
    break;

  case 934:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13193 "Parser/parser.cc"
    break;

  case 935:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13199 "Parser/parser.cc"
    break;

  case 936:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13205 "Parser/parser.cc"
    break;

  case 937:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13211 "Parser/parser.cc"
    break;

  case 938:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13217 "Parser/parser.cc"
    break;

  case 939:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13223 "Parser/parser.cc"
    break;

  case 940:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13229 "Parser/parser.cc"
    break;

  case 941:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13235 "Parser/parser.cc"
    break;

  case 942:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13241 "Parser/parser.cc"
    break;

  case 943:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13247 "Parser/parser.cc"
    break;

  case 944:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13253 "Parser/parser.cc"
    break;

  case 945:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13259 "Parser/parser.cc"
    break;

  case 947:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13265 "Parser/parser.cc"
    break;

  case 948:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13271 "Parser/parser.cc"
    break;

  case 949:
#line 3757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13277 "Parser/parser.cc"
    break;

  case 950:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13283 "Parser/parser.cc"
    break;

  case 951:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13289 "Parser/parser.cc"
    break;

  case 952:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13295 "Parser/parser.cc"
    break;

  case 953:
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13301 "Parser/parser.cc"
    break;

  case 954:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13307 "Parser/parser.cc"
    break;

  case 955:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13313 "Parser/parser.cc"
    break;

  case 956:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13319 "Parser/parser.cc"
    break;

  case 957:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13325 "Parser/parser.cc"
    break;

  case 958:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13331 "Parser/parser.cc"
    break;

  case 959:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13337 "Parser/parser.cc"
    break;

  case 960:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13343 "Parser/parser.cc"
    break;

  case 961:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13349 "Parser/parser.cc"
    break;

  case 962:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13355 "Parser/parser.cc"
    break;

  case 963:
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13361 "Parser/parser.cc"
    break;

  case 964:
#line 3801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13368 "Parser/parser.cc"
    break;

  case 966:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13374 "Parser/parser.cc"
    break;

  case 967:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13380 "Parser/parser.cc"
    break;

  case 968:
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13386 "Parser/parser.cc"
    break;

  case 969:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13392 "Parser/parser.cc"
    break;

  case 970:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13398 "Parser/parser.cc"
    break;

  case 971:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13404 "Parser/parser.cc"
    break;

  case 972:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13410 "Parser/parser.cc"
    break;

  case 973:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13416 "Parser/parser.cc"
    break;

  case 974:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13422 "Parser/parser.cc"
    break;

  case 975:
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13428 "Parser/parser.cc"
    break;

  case 976:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13434 "Parser/parser.cc"
    break;

  case 977:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13440 "Parser/parser.cc"
    break;

  case 978:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13446 "Parser/parser.cc"
    break;

  case 979:
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13453 "Parser/parser.cc"
    break;

  case 981:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13459 "Parser/parser.cc"
    break;

  case 982:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13465 "Parser/parser.cc"
    break;

  case 983:
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13471 "Parser/parser.cc"
    break;

  case 984:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13477 "Parser/parser.cc"
    break;

  case 985:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13483 "Parser/parser.cc"
    break;

  case 986:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13489 "Parser/parser.cc"
    break;

  case 987:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13495 "Parser/parser.cc"
    break;

  case 988:
#line 3879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13501 "Parser/parser.cc"
    break;

  case 989:
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13507 "Parser/parser.cc"
    break;

  case 990:
#line 3886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13513 "Parser/parser.cc"
    break;

  case 991:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13519 "Parser/parser.cc"
    break;

  case 993:
#line 3906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13525 "Parser/parser.cc"
    break;

  case 994:
#line 3908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13531 "Parser/parser.cc"
    break;

  case 995:
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13537 "Parser/parser.cc"
    break;

  case 996:
#line 3915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13543 "Parser/parser.cc"
    break;

  case 997:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13549 "Parser/parser.cc"
    break;

  case 998:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13555 "Parser/parser.cc"
    break;

  case 999:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13561 "Parser/parser.cc"
    break;

  case 1001:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13567 "Parser/parser.cc"
    break;

  case 1002:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13573 "Parser/parser.cc"
    break;

  case 1003:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13579 "Parser/parser.cc"
    break;

  case 1004:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13585 "Parser/parser.cc"
    break;

  case 1005:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13591 "Parser/parser.cc"
    break;

  case 1006:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13597 "Parser/parser.cc"
    break;

  case 1007:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13603 "Parser/parser.cc"
    break;

  case 1008:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13609 "Parser/parser.cc"
    break;

  case 1009:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13615 "Parser/parser.cc"
    break;

  case 1010:
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13621 "Parser/parser.cc"
    break;

  case 1012:
#line 3969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13627 "Parser/parser.cc"
    break;

  case 1013:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13633 "Parser/parser.cc"
    break;

  case 1015:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13639 "Parser/parser.cc"
    break;

  case 1016:
#line 3976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13645 "Parser/parser.cc"
    break;

  case 1018:
#line 3982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13651 "Parser/parser.cc"
    break;

  case 1019:
#line 3984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13657 "Parser/parser.cc"
    break;

  case 1020:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13663 "Parser/parser.cc"
    break;

  case 1021:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13669 "Parser/parser.cc"
    break;

  case 1022:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13675 "Parser/parser.cc"
    break;

  case 1023:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13681 "Parser/parser.cc"
    break;

  case 1024:
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13687 "Parser/parser.cc"
    break;

  case 1027:
#line 4036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13694 "Parser/parser.cc"
    break;

  case 1028:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13700 "Parser/parser.cc"
    break;

  case 1029:
#line 4041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13706 "Parser/parser.cc"
    break;

  case 1030:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13712 "Parser/parser.cc"
    break;

  case 1031:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13718 "Parser/parser.cc"
    break;

  case 1032:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13724 "Parser/parser.cc"
    break;

  case 1033:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13730 "Parser/parser.cc"
    break;

  case 1034:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13736 "Parser/parser.cc"
    break;

  case 1036:
#line 4060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13742 "Parser/parser.cc"
    break;

  case 1037:
#line 4062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13748 "Parser/parser.cc"
    break;

  case 1038:
#line 4064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13754 "Parser/parser.cc"
    break;

  case 1039:
#line 4069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13760 "Parser/parser.cc"
    break;

  case 1040:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13766 "Parser/parser.cc"
    break;

  case 1041:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13772 "Parser/parser.cc"
    break;

  case 1043:
#line 4080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13778 "Parser/parser.cc"
    break;

  case 1045:
#line 4091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13784 "Parser/parser.cc"
    break;

  case 1046:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13790 "Parser/parser.cc"
    break;

  case 1047:
#line 4096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13796 "Parser/parser.cc"
    break;

  case 1048:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13802 "Parser/parser.cc"
    break;

  case 1049:
#line 4101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13808 "Parser/parser.cc"
    break;

  case 1050:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13814 "Parser/parser.cc"
    break;

  case 1052:
#line 4118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13820 "Parser/parser.cc"
    break;

  case 1053:
#line 4120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13826 "Parser/parser.cc"
    break;

  case 1054:
#line 4125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13832 "Parser/parser.cc"
    break;

  case 1055:
#line 4127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13838 "Parser/parser.cc"
    break;

  case 1056:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13844 "Parser/parser.cc"
    break;

  case 1057:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13850 "Parser/parser.cc"
    break;

  case 1058:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13856 "Parser/parser.cc"
    break;

  case 1060:
#line 4139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13862 "Parser/parser.cc"
    break;

  case 1061:
#line 4141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13868 "Parser/parser.cc"
    break;

  case 1062:
#line 4143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13874 "Parser/parser.cc"
    break;

  case 1063:
#line 4148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13880 "Parser/parser.cc"
    break;

  case 1064:
#line 4150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13886 "Parser/parser.cc"
    break;

  case 1067:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13892 "Parser/parser.cc"
    break;

  case 1070:
#line 4171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13898 "Parser/parser.cc"
    break;

  case 1071:
#line 4173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13904 "Parser/parser.cc"
    break;

  case 1072:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13910 "Parser/parser.cc"
    break;

  case 1073:
#line 4177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13916 "Parser/parser.cc"
    break;

  case 1074:
#line 4179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13922 "Parser/parser.cc"
    break;

  case 1075:
#line 4181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13928 "Parser/parser.cc"
    break;

  case 1076:
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13934 "Parser/parser.cc"
    break;

  case 1077:
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13940 "Parser/parser.cc"
    break;

  case 1078:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13946 "Parser/parser.cc"
    break;

  case 1079:
#line 4194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13952 "Parser/parser.cc"
    break;

  case 1080:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13958 "Parser/parser.cc"
    break;

  case 1081:
#line 4199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13964 "Parser/parser.cc"
    break;

  case 1082:
#line 4201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13970 "Parser/parser.cc"
    break;

  case 1083:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13976 "Parser/parser.cc"
    break;

  case 1084:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13982 "Parser/parser.cc"
    break;

  case 1085:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13988 "Parser/parser.cc"
    break;

  case 1086:
#line 4212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13994 "Parser/parser.cc"
    break;

  case 1087:
#line 4214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14000 "Parser/parser.cc"
    break;

  case 1088:
#line 4219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14006 "Parser/parser.cc"
    break;

  case 1089:
#line 4221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14012 "Parser/parser.cc"
    break;

  case 1091:
#line 4248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14018 "Parser/parser.cc"
    break;

  case 1095:
#line 4259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14024 "Parser/parser.cc"
    break;

  case 1096:
#line 4261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14030 "Parser/parser.cc"
    break;

  case 1097:
#line 4263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14036 "Parser/parser.cc"
    break;

  case 1098:
#line 4265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14042 "Parser/parser.cc"
    break;

  case 1099:
#line 4267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14048 "Parser/parser.cc"
    break;

  case 1100:
#line 4269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14054 "Parser/parser.cc"
    break;

  case 1101:
#line 4276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14060 "Parser/parser.cc"
    break;

  case 1102:
#line 4278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14066 "Parser/parser.cc"
    break;

  case 1103:
#line 4280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14072 "Parser/parser.cc"
    break;

  case 1104:
#line 4282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14078 "Parser/parser.cc"
    break;

  case 1105:
#line 4284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14084 "Parser/parser.cc"
    break;

  case 1106:
#line 4286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14090 "Parser/parser.cc"
    break;

  case 1107:
#line 4291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14096 "Parser/parser.cc"
    break;

  case 1108:
#line 4293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14102 "Parser/parser.cc"
    break;

  case 1109:
#line 4295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14108 "Parser/parser.cc"
    break;

  case 1110:
#line 4300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14114 "Parser/parser.cc"
    break;

  case 1111:
#line 4302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14120 "Parser/parser.cc"
    break;

  case 1112:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14126 "Parser/parser.cc"
    break;

  case 1115:
#line 4328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14132 "Parser/parser.cc"
    break;

  case 1116:
#line 4330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14138 "Parser/parser.cc"
    break;


#line 14142 "Parser/parser.cc"

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
#line 4333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
