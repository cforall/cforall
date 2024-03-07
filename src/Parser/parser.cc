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
	if ( ! fieldList ) {								// field declarator ?
		if ( ! ( typeSpec->type && (typeSpec->type->kind == TypeData::Aggregate || typeSpec->type->kind == TypeData::Enum) ) ) {
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
		type = new ExpressionNode( new ast::CastExpr( location, maybeMoveBuild(type), new ast::BasicType( ast::BasicType::SignedInt ) ) );
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

#line 346 "Parser/parser.cc"

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
#line 318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 737 "Parser/parser.cc"

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
#define YYLAST   26235

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  181
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  311
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1118
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2223

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
       0,   641,   641,   645,   652,   653,   654,   655,   656,   660,
     661,   662,   663,   664,   665,   666,   667,   671,   672,   676,
     677,   682,   686,   687,   698,   700,   702,   704,   705,   707,
     709,   711,   713,   723,   725,   727,   729,   731,   733,   738,
     739,   750,   755,   760,   761,   766,   772,   774,   776,   782,
     784,   788,   790,   792,   812,   814,   816,   819,   821,   823,
     825,   827,   829,   831,   833,   835,   837,   839,   841,   851,
     852,   856,   857,   862,   865,   869,   870,   874,   875,   877,
     879,   881,   883,   885,   890,   892,   894,   902,   903,   911,
     914,   915,   917,   922,   938,   940,   942,   944,   946,   948,
     950,   955,   957,   960,   962,   970,   971,   973,   977,   978,
     979,   980,   984,   985,   987,   989,   991,   993,   995,   997,
     999,  1006,  1007,  1008,  1009,  1013,  1014,  1018,  1019,  1024,
    1025,  1027,  1029,  1034,  1035,  1037,  1042,  1043,  1045,  1050,
    1051,  1053,  1055,  1057,  1062,  1063,  1065,  1070,  1071,  1076,
    1077,  1082,  1083,  1088,  1089,  1094,  1095,  1100,  1101,  1103,
    1108,  1113,  1114,  1122,  1128,  1129,  1133,  1134,  1138,  1139,
    1143,  1144,  1145,  1146,  1147,  1148,  1149,  1150,  1151,  1152,
    1153,  1163,  1165,  1170,  1171,  1173,  1175,  1180,  1181,  1187,
    1188,  1194,  1195,  1196,  1197,  1198,  1199,  1200,  1201,  1202,
    1203,  1204,  1205,  1206,  1207,  1209,  1210,  1216,  1218,  1228,
    1230,  1238,  1239,  1244,  1246,  1248,  1250,  1252,  1256,  1257,
    1259,  1265,  1294,  1297,  1299,  1301,  1311,  1313,  1315,  1320,
    1325,  1327,  1329,  1331,  1339,  1340,  1342,  1346,  1348,  1352,
    1354,  1355,  1357,  1359,  1364,  1365,  1369,  1374,  1375,  1379,
    1381,  1386,  1388,  1393,  1395,  1397,  1399,  1404,  1406,  1408,
    1410,  1415,  1417,  1422,  1423,  1445,  1447,  1452,  1455,  1457,
    1460,  1462,  1465,  1467,  1472,  1477,  1479,  1484,  1489,  1491,
    1493,  1495,  1497,  1500,  1502,  1505,  1507,  1512,  1518,  1521,
    1523,  1528,  1534,  1536,  1541,  1547,  1550,  1552,  1555,  1557,
    1562,  1569,  1571,  1576,  1582,  1584,  1589,  1595,  1598,  1603,
    1613,  1615,  1617,  1622,  1624,  1629,  1630,  1632,  1637,  1639,
    1644,  1646,  1648,  1650,  1653,  1657,  1660,  1664,  1666,  1668,
    1670,  1672,  1674,  1676,  1678,  1680,  1682,  1684,  1689,  1690,
    1694,  1700,  1708,  1713,  1714,  1718,  1719,  1724,  1728,  1729,
    1732,  1734,  1739,  1742,  1744,  1746,  1749,  1751,  1756,  1761,
    1762,  1766,  1771,  1773,  1778,  1780,  1785,  1787,  1789,  1794,
    1799,  1804,  1809,  1811,  1813,  1818,  1820,  1826,  1827,  1831,
    1832,  1833,  1834,  1838,  1843,  1844,  1846,  1848,  1850,  1854,
    1858,  1859,  1863,  1865,  1867,  1869,  1871,  1877,  1878,  1884,
    1885,  1889,  1890,  1895,  1897,  1906,  1907,  1909,  1914,  1919,
    1930,  1931,  1935,  1936,  1942,  1943,  1947,  1949,  1953,  1955,
    1959,  1960,  1964,  1965,  1969,  1970,  1971,  1975,  1977,  1992,
    1993,  1994,  1995,  1997,  2001,  2003,  2007,  2014,  2016,  2018,
    2023,  2024,  2026,  2028,  2030,  2040,  2042,  2054,  2057,  2062,
    2064,  2070,  2075,  2080,  2091,  2098,  2103,  2105,  2107,  2113,
    2117,  2124,  2126,  2127,  2128,  2144,  2146,  2149,  2151,  2154,
    2159,  2160,  2164,  2165,  2166,  2167,  2176,  2177,  2178,  2187,
    2188,  2189,  2193,  2194,  2195,  2204,  2205,  2206,  2211,  2212,
    2221,  2222,  2227,  2229,  2233,  2235,  2237,  2239,  2246,  2251,
    2256,  2257,  2259,  2269,  2270,  2275,  2277,  2279,  2281,  2283,
    2285,  2288,  2290,  2292,  2297,  2303,  2305,  2307,  2309,  2311,
    2313,  2315,  2317,  2319,  2321,  2323,  2325,  2327,  2329,  2331,
    2333,  2335,  2337,  2339,  2341,  2343,  2345,  2347,  2349,  2351,
    2353,  2355,  2357,  2362,  2363,  2367,  2373,  2374,  2380,  2381,
    2383,  2385,  2387,  2392,  2394,  2399,  2400,  2402,  2404,  2409,
    2411,  2413,  2415,  2417,  2419,  2424,  2425,  2427,  2429,  2434,
    2436,  2435,  2439,  2447,  2448,  2450,  2452,  2457,  2458,  2460,
    2465,  2466,  2468,  2470,  2475,  2477,  2479,  2484,  2486,  2488,
    2490,  2491,  2493,  2498,  2500,  2502,  2507,  2508,  2512,  2513,
    2520,  2519,  2524,  2523,  2533,  2532,  2543,  2542,  2552,  2557,
    2558,  2563,  2569,  2586,  2587,  2591,  2593,  2595,  2600,  2602,
    2604,  2606,  2611,  2613,  2618,  2620,  2629,  2630,  2635,  2644,
    2649,  2651,  2653,  2662,  2664,  2665,  2666,  2668,  2670,  2671,
    2676,  2677,  2678,  2683,  2685,  2688,  2691,  2698,  2699,  2700,
    2706,  2711,  2713,  2719,  2720,  2726,  2727,  2731,  2738,  2740,
    2743,  2742,  2746,  2748,  2755,  2757,  2761,  2764,  2763,  2774,
    2778,  2782,  2786,  2791,  2792,  2797,  2802,  2810,  2812,  2814,
    2816,  2821,  2822,  2828,  2829,  2830,  2839,  2840,  2842,  2843,
    2848,  2849,  2850,  2852,  2858,  2859,  2861,  2862,  2863,  2865,
    2867,  2874,  2875,  2877,  2879,  2884,  2885,  2894,  2896,  2901,
    2903,  2908,  2909,  2911,  2914,  2916,  2920,  2921,  2922,  2924,
    2926,  2934,  2936,  2941,  2942,  2943,  2947,  2948,  2949,  2954,
    2955,  2960,  2961,  2962,  2963,  2967,  2968,  2973,  2974,  2975,
    2976,  2977,  2991,  2992,  2997,  2998,  3004,  3006,  3009,  3011,
    3013,  3036,  3037,  3043,  3044,  3050,  3049,  3059,  3058,  3062,
    3068,  3070,  3080,  3081,  3083,  3087,  3092,  3094,  3096,  3098,
    3104,  3105,  3109,  3110,  3115,  3117,  3124,  3126,  3127,  3129,
    3134,  3136,  3138,  3143,  3145,  3150,  3155,  3163,  3168,  3170,
    3175,  3180,  3181,  3186,  3187,  3191,  3192,  3193,  3198,  3200,
    3206,  3208,  3213,  3215,  3221,  3222,  3226,  3230,  3234,  3236,
    3250,  3252,  3254,  3256,  3258,  3260,  3262,  3263,  3268,  3271,
    3270,  3282,  3281,  3294,  3293,  3307,  3306,  3320,  3319,  3335,
    3341,  3343,  3349,  3350,  3361,  3368,  3373,  3379,  3382,  3385,
    3389,  3395,  3398,  3401,  3406,  3407,  3408,  3409,  3413,  3421,
    3422,  3434,  3435,  3439,  3440,  3445,  3447,  3449,  3454,  3455,
    3461,  3462,  3464,  3469,  3470,  3471,  3472,  3473,  3475,  3510,
    3512,  3517,  3519,  3520,  3522,  3527,  3529,  3531,  3533,  3538,
    3540,  3542,  3544,  3546,  3548,  3550,  3555,  3557,  3559,  3561,
    3570,  3572,  3573,  3578,  3580,  3582,  3584,  3586,  3591,  3593,
    3595,  3597,  3602,  3604,  3606,  3608,  3610,  3612,  3624,  3625,
    3626,  3630,  3632,  3634,  3636,  3638,  3643,  3645,  3647,  3649,
    3654,  3656,  3658,  3660,  3662,  3664,  3676,  3681,  3686,  3688,
    3689,  3691,  3696,  3698,  3700,  3702,  3707,  3709,  3711,  3713,
    3715,  3717,  3719,  3724,  3726,  3728,  3730,  3739,  3741,  3742,
    3747,  3749,  3751,  3753,  3755,  3760,  3762,  3764,  3766,  3771,
    3773,  3775,  3777,  3779,  3781,  3791,  3793,  3796,  3797,  3799,
    3804,  3806,  3808,  3813,  3815,  3817,  3819,  3824,  3826,  3828,
    3842,  3844,  3847,  3848,  3850,  3855,  3857,  3862,  3864,  3866,
    3871,  3873,  3878,  3880,  3897,  3898,  3900,  3905,  3907,  3909,
    3911,  3913,  3918,  3919,  3921,  3923,  3928,  3930,  3932,  3938,
    3940,  3943,  3950,  3952,  3961,  3963,  3965,  3966,  3968,  3970,
    3974,  3976,  3981,  3983,  3985,  3987,  4022,  4023,  4027,  4028,
    4030,  4032,  4037,  4039,  4041,  4043,  4045,  4050,  4051,  4053,
    4055,  4060,  4062,  4064,  4070,  4071,  4073,  4082,  4085,  4087,
    4090,  4092,  4094,  4108,  4109,  4111,  4116,  4118,  4120,  4122,
    4124,  4129,  4130,  4132,  4134,  4139,  4141,  4149,  4150,  4151,
    4156,  4157,  4162,  4164,  4166,  4168,  4170,  4172,  4179,  4181,
    4183,  4185,  4187,  4190,  4192,  4194,  4196,  4198,  4203,  4205,
    4207,  4212,  4238,  4239,  4241,  4245,  4246,  4250,  4252,  4254,
    4256,  4258,  4260,  4267,  4269,  4271,  4273,  4275,  4277,  4282,
    4284,  4286,  4291,  4293,  4295,  4313,  4315,  4320,  4321
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
  "for_control_expression_list", "for_control_expression", "downupdowneq",
  "updown", "updowneq", "jump_statement", "fall_through_name",
  "with_statement", "mutex_statement", "when_clause", "when_clause_opt",
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
  "enum_type", "$@6", "$@7", "hide_opt", "enum_type_nobody",
  "enumerator_list", "visible_hide_opt", "enumerator_value_opt",
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

#define YYPACT_NINF (-1959)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1117)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     187, 13076,   203,   295, 19677,   290, -1959, -1959, -1959, -1959,
   -1959, -1959, -1959, -1959, -1959, -1959, -1959, -1959,   261,   930,
     289, -1959, -1959, -1959, -1959, -1959, -1959, -1959, -1959, -1959,
   -1959, -1959, -1959, -1959, -1959, -1959, -1959, -1959, -1959, -1959,
   -1959, -1959, -1959, -1959, -1959, -1959, -1959, -1959,    42,   483,
   -1959, -1959, -1959, -1959, -1959, -1959,  4764,  4764,   391, 13076,
     436,   448, 23346, -1959,   471, -1959, -1959, -1959, -1959, -1959,
   -1959, -1959, -1959, -1959, -1959,   485,  2683, -1959,  1009,   457,
   -1959, -1959, -1959, -1959, -1959, 19212, -1959, -1959,   526,   490,
     300,    14, -1959,  4764,   524,   554,   583,   566,  5153,   752,
     855, 13241, -1959, -1959,   697, 19057,  1631, -1959, -1959, -1959,
   -1959,  1978,   772,  4893,  9112,   909,  1978,   993,   632, -1959,
   -1959, -1959, -1959,   171, -1959, -1959, -1959, -1959,   639, -1959,
   -1959, -1959, -1959, -1959,   658,   711,   171, -1959,   171, 17375,
   -1959, -1959, -1959, 21007,  4764, -1959, -1959,  4764, -1959, 13076,
   -1959,   637, 21060, -1959, -1959,  5826, 22359, -1959, -1959,   879,
     879,   724,  4383, -1959, -1959, -1959, -1959,   293, 15671,  3649,
     171, -1959, -1959, -1959, -1959, -1959, -1959,   705, -1959,   685,
     744,  2479, -1959,   787, 25608, -1959, -1959, -1959, -1959, -1959,
   -1959, -1959, 17754,  1699,  5609,  2683,   111,   760,   792,   798,
     805,   807,   816, -1959, -1959, 19832, 11750,   825,   866, -1959,
   20135, -1959, -1959, -1959, -1959,   869, -1959, -1959,   868, -1959,
   23632,  1020, 23784, -1959,   881,  4764,   711,   889,  2326,  5826,
    2326, -1959, -1959, -1959,  4475,  5060,   878,   946,   401,   946,
   -1959,   171,   171,    55, 17053,   505,   946, -1959,   171,   171,
      55,   171, -1959,   171, -1959,  5188, -1959, -1959,   904,   906,
     879, 23103,   944, 19212, -1959, -1959,  1978, -1959,  1438,   632,
     901,   980, 17053,  4764,  4764,   300, -1959, 14851, -1959,   879,
     879,   962,   980, 17053,  4764, -1959, 23576, -1959, -1959, -1959,
     879, -1959, -1959, -1959, -1959,   879, -1959,  1072,  4595,  4764,
   -1959, 18911,   923, -1959, -1959, -1959, 22966,   711, 17214,   913,
    5826, 18858, 23103,  1978, -1959, -1959, 22507, -1959,   946,   180,
   -1959, 25608, 22359,  4489,  5188, -1959,   537, -1959, -1959, -1959,
   -1959, -1959, 21060,  4764, -1959,   964,   956, -1959, -1959, -1959,
   -1959,  4764,  4160,   430,   299, -1959,  4764,   685, -1959,  1089,
     171,   171,   970, 21215,  1073, 16163, 23156,  1978, -1959,  1978,
     879,  1978,   879, -1959, -1959,   171, -1959, -1959,   983, 21268,
   -1959, -1959, -1959, 21423,   869, -1959,   556,   337,   278,   979,
     578,   632,   988, -1959,  4383,  1024,   685,  4383, -1959, -1959,
   -1959, -1959, -1959,  1699, -1959,   686, -1959,  1038, -1959,  1043,
    1088, 25684,  1056,  1058,  1061, 25608, 25760,  1063, 23461, -1959,
   -1959, -1959, -1959, -1959, -1959, 25836, 25836, 17595,  1062,  5524,
   -1959, -1959, -1959, -1959,   456, -1959,   635, -1959,  1760, -1959,
   25608, 25608, -1959,  1084,   747,  1301,  1353,   653,  1351,  1091,
    1112,  1108,  1173,   -28, -1959,   933, -1959,  1152, -1959,  1331,
    4322, 18231, -1959, -1959,  1095,  1152, -1959, -1959,   938, -1959,
   -1959,   953,  5609,  1158,  1164,  1166,  1169,  1186,  1188, -1959,
   -1959,   563,  1202, -1959,   973,  1202,  1208, -1959,  1214, -1959,
   21007, -1959,  1350,  1226, 18390, -1959, -1959,  5310,  5439,  1251,
   16163,  1278,  1059,  1069,  1259,  1265, -1959, -1959, -1959,  4764,
    5675, 20489, -1959, -1959, -1959, -1959, -1959, -1959, 18717,  4148,
    1062, 23632,  1267,  1282, -1959, -1959,  1290, 23784,   864, -1959,
   -1959, -1959, 23860,  1298, -1959, -1959, -1959, -1959,  1279,  4475,
     759,  1303,  1307,  1317,   783,  1319,  1329,  1348,  1356,  1368,
    1370,  5060, -1959, -1959, -1959,   171,  1323,  1328,  1375, -1959,
   -1959,  1371,   300, -1959, -1959,   711,   980, 19996, -1959, -1959,
     300, -1959, -1959,   711, -1959, -1959,  5188, -1959, 18231, 18231,
   -1959,   879,  5826, 23293,  2949, 16327, -1959, -1959, -1959, -1959,
   -1959,   711,   980,   180,  1388, -1959, -1959,  1978,  1392,   980,
   17053, -1959,   711,   980, -1959, 23627, -1959,   879,   879, -1959,
   -1959,  1394,   389,  1400,   632,  1404, -1959, -1959, -1959, 20436,
    1439,  1424, -1959, -1959,  1000, -1959,  1521, -1959,  1442, -1959,
   -1959, -1959, 21587, 25912, -1959, -1959, -1959, -1959, -1959,  4489,
     898,  5188, 19996, 16491,   946, 13076, -1959,  4764,  1443, -1959,
    1459, -1959, -1959, -1959, -1959, -1959,  4383, -1959, -1959,  1546,
    5284,  3769, 20644, 11750, -1959, 21640, -1959,   879,   879, -1959,
   -1959,   869, -1959, 15179,  1466,  1615, 25608,  1700,  1371,  1472,
   -1959,   171,   171, -1959,  1202, -1959, 21215, -1959, -1959, 20436,
     879,   879, -1959,  5284,   171, -1959, 22211, -1959, -1959, 21268,
   -1959,   293, -1959, -1959, -1959,  1495,  4764,   278,   988,  1496,
    1003, 21060,  1014,  1021, -1959,  1699, 23936,  1498, -1959, 17913,
   -1959,  5524, 21795, 21060, -1959, 17913, -1959, 25608, -1959, -1959,
   -1959, -1959, -1959, -1959, 18072, -1959, -1959, 20697, 21795, 21795,
    1358,   916,  1676,   611,  1727, -1959,  1029,  1506,  1354,  1519,
   -1959, 23860, 25608, 24012,  1517, 25608,  2326, 25608,  2326, -1959,
    2967, -1959, -1959, 23936,  3678, 25608, 23936,  2326, -1959, -1959,
   25608, 25608, 25608, 25608, 25608, 25608, 25608, 25608, 25608, 25608,
   25608, 25608, 25608, 25608, 25608, 25608, 25608, 25608, 25608, 24088,
    1522,   787,  4931, 11750, -1959, -1959, -1959, -1959, -1959, -1959,
   -1959, -1959, -1959, -1959, -1959,  1543, 25608, -1959, -1959, 15343,
    1734, -1959, -1959,   171,   171, -1959, -1959, 18231, -1959, -1959,
     699,  1202, -1959,  1010,  1202, 19996, -1959, -1959,  1371, 19996,
   -1959,  1371, -1959, 25988, -1959, -1959, -1959, 19522, 11750,  1549,
    1363,  1551, 14687,  1702,  4400,   710,  1472, -1959,   171,   171,
    1472,   742, -1959,   171,   171, 25608,  4764, 16327,  1562, 16327,
    1564,  1472,   256, 15507, 15507, 15507,  4764, -1959, -1959, 25608,
    1290, -1959, 23632,  1571, -1959,  1321, -1959, -1959, -1959, -1959,
   -1959,  1053, -1959, 15507, 25608,  1032,  1572,  1574,  1575,  1075,
    1580,  1589,  1595,  1596,  1604,  1606,   745,  1202, -1959, -1959,
     875,  1202, -1959, -1959,   876,  1202, -1959, -1959, -1959,  5826,
     787,  1706,  1202, 22655, -1959, -1959,   711,  1608, -1959, -1959,
   -1959,  1092,  1609,  1106,  1610, -1959,  1208,  1616,  1614, -1959,
     711, -1959,  1621, -1959,   711,   980,  1614, -1959,   711,  1617,
    1618,  1620, -1959, -1959, 20299, -1959,  2326,  4764, 10879,  1664,
   -1959,  1226, -1959, 15507,  1109,  1624, -1959,  1614,  1612, -1959,
   21848, 18231,  1611, -1959,  1611, -1959, -1959, -1959,   278,  1625,
     171,   171, -1959, 21268, -1959, 11918, 18549, -1959,  1636,  1637,
    1639,  1640, -1959, 12826,   171, -1959,  1700, -1959, -1959, -1959,
   -1959,  1371, -1959, -1959, -1959,   879, -1959,  3783, -1959, -1959,
     632,   462,  1647,  1641,  1495,  1656,   278, -1959, -1959,  1658,
    1646, -1959, -1959,  1121, -1959, -1959, -1959, -1959,  1669,  1671,
    1643,  1670,  1668,  1674,  1675,  1678,  1679,  1680, 25608,  1682,
    1686,  1688, 22003, 12086, 25608, -1959, -1959,  1742, -1959, -1959,
   -1959, 25608, -1959,  1690,  1692, 23708,  1381, -1959, 23936,  1696,
   -1959,  1697, -1959, -1959,  4287, -1959,  1126, -1959, -1959, -1959,
    4287, -1959, -1959,  1384,   589, -1959, -1959,  1084,  1084,  1084,
     747,   747,  1301,  1301,  1353,  1353,  1353,  1353,   653,   653,
    1351,  1091,  1112,  1108,  1173, 25608,  1386, -1959,  1693,  4287,
   -1959, -1959, 23632, -1959,  1695,  1701,  1704,  1705,  1734, -1959,
   -1959, -1959, -1959, -1959, 19996, -1959, -1959,  1371, 19996, -1959,
    1371,  1708,  1712, 15507, 15507, -1959, -1959, 14687,   948,  1714,
    1718,  1719,  1723,  4049,  4400, -1959, -1959, 19996, -1959, -1959,
   -1959, -1959, -1959, -1959, 19996, -1959, -1959, -1959, -1959,  1703,
   -1959,  1472,  1720, -1959, -1959, -1959, -1959, -1959, -1959, -1959,
   -1959,  1731,  1729,  1730, -1959, -1959,   300,  4287,  1393,    88,
   -1959, -1959,  1740, -1959, 23784, -1959, 25608,   171, 24164, 15507,
   -1959, -1959,   882,  1202, -1959,   891,  1202, -1959, -1959,   960,
    1202, 19996, -1959, -1959,  1371, 19996, -1959, -1959,  1371, 19996,
   -1959, -1959,  1371,   946,  1745, -1959,  1371,   335, -1959,  1152,
    1743, -1959, -1959, -1959, -1959, -1959, -1959,  1749, -1959, -1959,
   -1959, 21848,  1614, -1959,   711, -1959, -1959, -1959, -1959, -1959,
    9819, -1959, -1959, -1959, -1959,   334, -1959,   666,   257, 11582,
    1752,  1754, 16875,  1755,  1757,  2428,  3300,  3616, 24240,  1758,
   -1959, -1959,  1759,  1761, 16875,  1765, -1959, -1959,   711, 25608,
   25608,  1907,  1766,   615, -1959, 17436,  1399,  1768,  1751, -1959,
   -1959, -1959, 10701, -1959, -1959, -1959, -1959, -1959,  1262, -1959,
   -1959, -1959,  1470,   226, -1959,   312, -1959,   226, -1959, -1959,
   -1959, -1959, -1959,  2326, -1959, -1959, 13406, 19367,  1771, -1959,
    4764,  1774,  1776, -1959, 16491, -1959, -1959,  4764, -1959, -1959,
    5826, -1959, -1959,  1762,  1763,  1133, 21060,   685,   685,  1495,
     278,   988,   988, -1959, -1959,  1062,  1226, 18390, -1959,  1152,
   -1959, 12254, -1959,   961,  1202, -1959,   879, 12907, -1959, -1959,
     278,  1775,   171,   171,   293,  4764, -1959, 24316, -1959,  1785,
     278,  1495,  1786, -1959, -1959, 23936,   647, -1959, 20436, 12086,
    2326, -1959,   647, -1959, 20852,   647, -1959, 25608, 25608, 25608,
   -1959, -1959, -1959, -1959, 25608, 25608,  1778, 23632, -1959, -1959,
    1783,   722, -1959, -1959, -1959,  3473, -1959, -1959,  1405, -1959,
     113, -1959,  1410, -1959, 23860, -1959, -1959, 25608,  1769,  1414,
    1420,  1290, -1959,  1008,  1202, -1959, -1959,  1789,  1791, -1959,
   -1959, -1959, -1959,  1793,  1015,  1202, -1959,  1017,  2847,   171,
     171, -1959, -1959,  1794,  1795, -1959,  1796, -1959, 16327,  1797,
   -1959, 15835, 15999,  1801,  1802, -1959,  1792, 25608, 25608,  1423,
    1800, -1959, -1959, -1959, -1959, -1959, -1959, -1959,  1805, 19996,
   -1959, -1959,  1371, 19996, -1959, -1959,  1371, 19996, -1959, -1959,
    1371,  1806,  1809,  1810,   300,   171, -1959, -1959,  1428, 25608,
   22807,  1808,  1819, -1959, -1959, -1959,  1820, 13894, 14052, 14210,
   21848, 23103, 21795, 21795,  1825, -1959,   386,   392,  3327, 14523,
   -1959,   413,  4764,  4764, -1959, 23936,   106,   420, -1959, -1959,
   -1959, -1959, 11582, 25608,  1838,  1895, 11413, 11057, -1959,  1814,
   -1959,  1817, 25608,  1822, 23632,  1823, 25608, 23860, 25608, -1959,
   11235,   884, -1959,  1824,     5, -1959,   210,  1904,   305, -1959,
    1846, -1959,  1827, -1959,  1828,  1855,  1858, 16875, 16875, -1959,
   -1959,  1924, -1959, -1959,    44,    44,   165, 15015,   171,   428,
   -1959, -1959,  1859,  1867,   430, -1959,  1869, -1959,  1864, -1959,
    1866, -1959, -1959, -1959, -1959,  1874,  1495,  1868,  1871, 12422,
    1870,  1872,  1875, -1959, 19996, -1959, -1959,  1371, 25608, 25608,
    1226,  1881, -1959,  1495,   278, -1959,   988,   384,  1641, 23632,
   -1959, -1959,  1495,  1889, -1959, 21848, -1959,  1161,  1887,  1883,
    1135, -1959,  1885, -1959, -1959, -1959, -1959, -1959, 23632,  1290,
   23860, -1959,  1906,  4287, -1959,  1906,  1906, -1959,  4287,  3910,
    4021, -1959,  1434, -1959, -1959, -1959,  1900, 19996, -1959, -1959,
    1371, -1959, -1959,  1899,  1902,   171, 19996, -1959, -1959,  1371,
   19996, -1959, -1959,  1905, -1959, -1959, -1959, -1959, -1959, -1959,
   -1959, -1959,  1720, -1959, -1959, -1959,  1897, -1959, -1959, -1959,
   -1959,  1909,  1913,   171,  1916,  1919,  1920, -1959, -1959, -1959,
   -1959, -1959, 25608, -1959,   335, -1959,  1152, -1959, -1959,  1910,
    1925, -1959,  1825,  1825,  1825,  6100,  1163,  1894,   438, -1959,
    6100,   445, 18231, -1959, -1959, -1959,  4842, 25608,  5297,   378,
   -1959, -1959,    29,  1918,  1918,  1918,  4764, -1959, -1959, -1959,
    1151, -1959, -1959, -1959, -1959,  1768,  1921, 25608,   526,  1922,
     566, 14375, 21848,  1174,  1929, 16875,  1928, -1959, -1959, -1959,
     673, 16875, 25608,   688,   713, -1959, 25608,  9517, -1959, -1959,
     476, -1959,  1290, -1959,  1175,  1177,  1180,   782, -1959, -1959,
   -1959, -1959,   711,   884,  1932, -1959, -1959, 25608, -1959,  1934,
     787, -1959, -1959, -1959, -1959, 25608, 25608, -1959, -1959,    60,
      44, -1959,    66, -1959, -1959, 10523, -1959,   171, -1959,  1611,
   -1959, 21848, -1959, -1959, -1959,  1936,   278,   278, -1959, -1959,
   -1959,  1931,  1935, -1959, -1959,  1933, -1959,  1937,  1942,  1495,
     988,  1938, -1959, -1959,  1290,  1944, -1959, -1959,  1946, -1959,
   -1959, 25608, -1959, 20852, 25608,  1290,  1951,  1440, -1959,  1445,
   -1959,  4287, -1959,  4287, -1959, -1959, -1959,  1950,   171,   171,
    1952,  1953, -1959,  1954, -1959, -1959, -1959, -1959, -1959,  1450,
   25608, -1959, -1959, -1959, -1959, -1959,   481,  1163,  2314,   522,
   -1959, -1959, -1959, -1959,   171,   171, -1959, -1959, -1959,   560,
   -1959,  1187,  4842,   332, -1959,  5297, -1959,   171, -1959, -1959,
   -1959, -1959, -1959, -1959, 16875, 16875,  1768, 16655,   318, 24392,
    2036, 16875, -1959, -1959, -1959, -1959, 25608, -1959, 24468,  2037,
    1940, 10071, 24544, 16875, 11235,  1768,   587,  1233,  1943, 25608,
   -1959,  1959,   361, 16875, -1959, 16875, -1959,  1962, -1959, -1959,
    1955,   787,   809,  1961,  1451,  1198, 16875,  1976, 16875, 16875,
   16875, -1959, -1959, -1959,   685, -1959,  4764,  5826, -1959,  1495,
    1495, -1959, -1959,  1972,  1974, -1959, -1959, -1959,  1982,  1975,
     278,  1985, -1959,  1986, -1959, -1959, -1959, -1959,  1987, -1959,
   -1959, -1959,  1460,  1469, -1959, -1959, -1959, -1959, -1959, -1959,
   -1959, -1959, -1959,  1994,  1995,  1996,  2314, -1959,   171, -1959,
   -1959, -1959, -1959, -1959,  1983,  6100, -1959,  2080,  7969,    82,
   12593, -1959, 16752, -1959,    20,  1232, 16875,  2081,   596,  1988,
      12, 16875, 25608,  1999,   587,  1233,  1989, 26064,  1991,   377,
    2083, -1959, 24620, 24696, 25608,  1768,  1990, 12760, -1959, -1959,
   -1959, -1959, 22056, -1959,  2001,  2000,    -3, -1959, 25608, 23936,
   -1959, -1959, 25608,   226, -1959, -1959, -1959, -1959, -1959,  2011,
    2012, -1959, -1959, -1959,   278,  1495, -1959, -1959, -1959, -1959,
   -1959,  1030,  1202, -1959, -1959,  1163, -1959, 16875, -1959,   271,
   -1959,    51, -1959, -1959, -1959,  2017, 13571, -1959, -1959, 16875,
   -1959,    62, -1959, 16875, 25608,  2025, 24772, -1959, -1959, 24848,
   24924, 25608,  1999,  1768, 25000, 25076, 16875,  2013,   501,  2014,
     515, -1959, -1959,  2028, 13571, 22056, -1959,  5751, 21640,  2326,
    2021, -1959,  2084,  2031,   818,  2032, -1959, -1959,  1242,  1250,
     599, -1959, -1959,  1495,  2038, 19996, -1959, -1959,  1371, -1959,
   -1959, 25608, -1959, 25608, -1959, -1959,  1577, 13736, -1959, -1959,
   16875, -1959, -1959,  1768, -1959, -1959,  1768,  2024,   625,  2040,
     627, -1959, -1959,  1768, -1959,  1768, -1959,  2052, 25152, 25228,
   25304, -1959,  1577, -1959,  2016,  3895,  4670, -1959, -1959, -1959,
      -3,  2054, 25608,  2035,    -3,    -3, -1959, -1959, 16875,  2145,
    2067, -1959,  2065, -1959, -1959, 16752, -1959,  1577, -1959, -1959,
    2069, 25380, 25456, 25532, -1959, -1959,  1768, -1959,  1768, -1959,
    1768, -1959,  2016, 25608,  2066,  4670,  2068,   787,  2070, -1959,
     820, -1959, -1959, 16875, -1959, -1959, -1959, 10266,  2077, 16752,
   -1959, -1959,  1768, -1959,  1768, -1959,  1768,  2078,  2079, -1959,
     711,   787,  2086, -1959,  2055,   787, -1959, -1959, -1959, -1959,
   10393, -1959,   711, -1959, -1959,  1505, 25608, -1959,  1261, -1959,
     787,  2326,  2085,  2058, -1959, -1959,  1264, -1959, -1959,  2063,
    2326, -1959, -1959
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   488,     0,     2,   488,   505,   506,   507,   508,   509,
     510,   511,   512,   513,   494,   496,   495,   497,     0,     0,
       0,   515,   517,   538,   518,   539,   521,   522,   536,   537,
     516,   534,   535,   519,   520,   523,   524,   525,   526,   527,
     528,   529,   530,   531,   532,   533,   540,   541,   851,   543,
     616,   617,   620,   622,   618,   624,     0,     0,     0,   488,
       0,     0,    17,   587,   593,     9,    10,    11,    12,    13,
      14,    15,    16,   808,   107,     0,     0,    20,     0,     2,
     105,   106,    18,    19,   869,   488,   809,   426,     0,   429,
     731,   431,   440,     0,   430,   462,   463,     0,     0,     0,
       0,   570,   490,   492,   498,   488,   500,   503,   555,   514,
     542,   472,   548,   553,   474,   565,   473,   580,   584,   590,
     569,   596,   608,   851,   613,   614,   597,   672,   432,   433,
       3,   816,   829,   493,     0,     0,   851,   891,   851,   488,
     908,   909,   910,   488,     0,  1095,  1096,     0,     1,   488,
      17,     0,   488,   451,   452,     0,   570,   498,   482,   483,
     484,   819,     0,   619,   621,   623,   625,     0,   488,     0,
     852,   853,   615,   544,   724,   725,   723,   785,   780,   770,
       0,   860,   817,     0,     0,   505,   810,   814,   815,   811,
     812,   813,   488,   860,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   588,   591,   488,   488,     2,     0,  1097,
     570,   898,   916,  1101,  1094,  1092,  1099,   425,     0,   169,
     737,   168,     0,   434,     0,     0,     0,     0,     0,     0,
       0,   424,   985,   986,     0,     0,   461,   849,   851,   849,
     872,   851,   851,   471,   488,   851,   849,   929,   851,   851,
     470,   851,   948,   851,   926,     0,   563,   564,     0,     0,
     488,   488,     2,   488,   441,   491,   501,   556,     0,   585,
       0,   832,   488,     0,     0,   731,   442,   570,   549,   566,
     581,     0,   832,   488,     0,   504,   550,   557,   558,   475,
     567,   477,   478,   476,   572,   582,   586,     0,   600,     0,
     802,   488,     2,   830,   890,   892,   488,     0,   488,     0,
       0,   570,   488,   500,     2,  1105,   570,  1108,   849,   849,
       3,     0,   570,     0,     0,   454,   851,   844,   846,   845,
     847,     2,   488,     0,   806,     0,     0,   766,   768,   767,
     769,     0,     0,   762,     0,   751,     0,   760,   772,     0,
     851,   851,     2,   488,  1117,   489,   488,   479,   548,   480,
     573,   481,   580,   577,   598,   851,   599,   716,     0,   488,
     717,  1070,  1071,   488,   718,   720,   587,   593,   673,     0,
     675,   676,   673,   854,     0,   783,   771,     0,   868,   867,
     863,   865,   866,   860,   864,     0,   858,   861,    22,     0,
      21,     0,     0,     0,     0,     0,     0,     0,    24,    26,
       4,     8,     5,     6,     7,     0,     0,   488,     2,     0,
     108,   109,   110,   111,    90,    25,    91,    43,    89,   112,
       0,     0,   127,   129,   133,   136,   139,   144,   147,   149,
     151,   153,   155,   157,   160,     0,    27,     0,   594,     2,
     112,   488,   161,   777,   727,   584,   729,   776,     0,   726,
     730,     0,     0,     0,     0,     0,     0,     0,     0,   870,
     896,   851,   906,   914,   918,   924,   587,     2,     0,  1103,
     488,  1106,     2,   105,   488,     3,   715,     0,  1117,     0,
     489,   548,   573,   580,     3,     3,   711,   701,   705,   717,
     718,   488,     2,   899,   917,  1093,     2,     2,    24,     0,
       2,   737,    25,     0,   735,   738,  1115,     0,     0,   744,
     733,   732,     0,     0,   834,     2,   453,   455,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   875,   932,   955,   851,     0,   467,     2,   871,
     879,  1013,   731,   873,   874,     0,   832,   488,   928,   936,
     731,   930,   931,     0,   947,   949,     0,   457,   488,   488,
     554,   489,     0,   570,     0,   488,  1098,  1102,  1100,   571,
     806,     0,   832,   849,     0,   435,   443,   502,     0,   832,
     488,   806,     0,   832,   781,   551,   552,   568,   583,   589,
     592,   587,   593,   611,   612,     0,   782,   687,   721,   489,
       0,   688,   690,   691,     0,   209,   418,   831,     0,   416,
     471,   470,   570,     0,   437,     2,   438,   803,   459,     0,
       0,     0,   488,   488,   849,   488,   806,     0,     0,     2,
       0,   765,   764,   763,   757,   499,     0,   755,   773,   546,
       0,     0,   488,   488,  1072,   489,   485,   486,   487,  1076,
    1067,  1068,  1074,   488,     2,   106,     0,  1032,  1046,  1117,
    1028,   851,   851,  1037,  1044,   709,   488,   578,   719,   489,
     574,   575,   579,     0,   851,  1082,   489,  1087,  1079,   488,
    1084,     0,   682,   674,   681,  1115,     0,   673,   673,     0,
       0,   488,     0,     0,   856,   860,    69,     0,    23,   488,
      97,     0,   488,   488,    92,   488,    99,     0,    33,    37,
      38,    34,    35,    36,   488,    95,    96,   488,   488,   488,
       2,   108,   109,     0,     0,   187,     0,     0,   614,     0,
    1092,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,    64,    65,    69,     0,     0,    69,     0,    93,    94,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   488,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   168,     0,   166,   167,   488,
     997,   728,   994,   851,   851,  1002,   595,   488,   857,   897,
     851,   907,   915,   919,   925,   488,   900,   902,   904,   488,
     920,   922,     2,     0,     2,  1104,  1107,   488,   488,     0,
       2,     0,   488,   106,  1032,   851,  1117,   967,   851,   851,
    1117,   851,   982,   851,   851,     3,   719,   488,     0,   488,
       0,  1117,  1117,   488,   488,   488,     0,     2,   746,     0,
    1115,   743,  1116,     0,   739,     0,     2,   742,   745,   184,
     183,     0,     2,   488,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   851,   884,   888,   927,
     851,   941,   945,   953,   851,   963,   876,   933,   956,     0,
       0,     0,  1009,     0,   465,   835,     0,     0,   466,   836,
     458,     0,     0,     0,     0,   456,     0,     2,     2,   837,
       0,   439,     2,   806,     0,   832,     2,   838,     0,     0,
       0,     0,   626,   893,   488,   911,     0,     0,   488,   419,
     417,   105,     3,   488,     0,     3,   807,     2,     0,   759,
     488,   488,   753,   752,   753,   547,   545,   675,   673,     0,
     851,   851,  1078,   488,  1083,   489,   488,  1069,     0,     0,
       0,     0,  1047,     0,   851,  1118,  1033,  1034,   710,  1030,
    1031,  1045,  1073,  1077,  1075,   576,   611,     0,  1081,  1086,
     678,   673,     0,   683,  1115,     0,   673,   786,   784,     0,
       0,   859,    73,     0,    70,    71,    74,   818,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   488,   488,     0,   126,   125,     0,   122,   121,
      28,     0,    29,     0,     0,     0,     0,     3,    69,     0,
      52,     0,    53,    62,     0,    61,     0,    55,    56,    57,
       0,    54,    60,     0,     0,    51,   128,   130,   131,   132,
     134,   135,   137,   138,   142,   143,   140,   141,   145,   146,
     148,   150,   152,   154,   156,     0,     0,   428,     0,     0,
      30,     3,   737,   162,     0,     0,     0,     0,   998,   999,
     995,   996,   779,   778,   488,   901,   903,   905,   488,   921,
     923,     0,     0,   488,   488,  1023,  1022,   488,     0,     0,
       0,     0,     0,   851,  1033,   970,   987,   488,   965,   973,
     707,   968,   969,   708,   488,   980,   990,   983,   984,     0,
       3,  1117,     3,   703,   449,   702,   706,  1109,   712,   713,
     695,     0,   696,   697,     3,     3,   731,     0,   160,     0,
       3,     3,     0,   740,     0,   734,     0,   851,     0,   488,
       3,   460,   851,   885,   889,   851,   942,   946,   954,   851,
     964,   488,   877,   880,   882,   488,   934,   937,   939,   488,
     957,   959,   961,   849,     0,   468,  1010,     3,  1014,  1015,
       3,   840,   950,   560,   559,   562,   561,     2,   807,   841,
     788,   488,     2,   839,     0,   807,   842,   626,   626,   626,
     488,   689,   692,   693,   722,     0,   422,     0,     0,   488,
       0,     0,   343,     0,     0,     0,     0,     0,   189,     0,
     338,   339,     0,     0,   343,     0,   391,   390,     0,   164,
     164,   397,   587,   593,   206,   488,     2,   190,     0,   217,
     191,   192,   488,   211,   193,   194,   195,   196,     0,   197,
     198,   344,     0,   358,   199,   364,   366,   369,   200,   201,
     202,   203,   204,     0,   205,   213,   570,   488,     0,   215,
       0,     0,     0,     3,   488,   820,   807,     0,   795,   796,
       0,     3,   791,     3,     3,     0,   488,   770,   770,  1115,
     673,   673,   673,  1080,  1085,     2,   105,   488,     3,   585,
       3,   489,  1041,   851,  1040,  1043,   488,     3,  1029,  1035,
     673,     0,   851,   851,     0,     0,   658,     0,   677,     0,
     673,  1115,     2,   855,   862,     0,    98,   101,   488,   488,
       0,   104,   100,   102,   488,     0,   116,     0,     0,     0,
     120,   124,   123,   188,     0,     0,     0,   737,   113,   181,
       0,     0,    46,    47,    87,     0,    87,    87,     0,    75,
      77,    49,     0,    45,     0,    48,   159,     0,     0,     0,
       0,  1115,  1006,   851,  1005,  1008,  1000,     0,     0,   894,
     912,     3,     3,     0,   851,   976,   979,   851,     0,   851,
     851,   971,   988,     0,     0,  1110,     0,   714,   488,     0,
    1112,   488,   488,     0,     0,   436,     3,     0,     0,     0,
       0,   736,   741,     3,   833,   186,   185,     3,     0,   488,
     878,   881,   883,   488,   935,   938,   940,   488,   958,   960,
     962,     0,     0,     0,   731,   851,  1021,  1020,     0,     0,
       0,     0,     0,     3,   807,   843,     0,   488,   488,   488,
     488,   488,   488,   488,   609,   639,     0,     0,   640,   570,
     627,     0,     0,     0,   420,    69,     0,     0,   329,   330,
     214,   216,   488,     0,     0,     0,   488,   488,   325,     0,
     323,     0,     0,     0,   737,     0,     0,     0,     0,   370,
     488,     0,   165,     0,     0,   398,     0,     0,     0,   221,
       0,   212,     0,   320,     0,     0,     0,   343,   343,   349,
     348,   343,   360,   359,   343,   343,     0,   570,   851,     0,
    1025,  1024,     0,     0,   762,   798,     2,   793,     0,   794,
       0,   774,   754,   758,   756,     0,  1115,     0,     0,   488,
       0,     0,     0,     3,   488,  1036,  1038,  1039,     0,     0,
     105,     0,     3,  1115,   673,   667,   673,   683,   683,   737,
     684,   659,  1115,     0,   787,   488,    72,  1026,     0,     0,
       0,    39,     0,   117,   119,   118,   115,   114,   737,  1115,
       0,    68,    84,     0,    78,    85,    86,    63,     0,     0,
       0,    59,     0,   158,   427,    31,     0,   488,  1001,  1003,
    1004,   895,   913,     0,     0,   851,   488,   972,   974,   975,
     488,   989,   991,     0,   966,   981,   977,   992,  1111,   704,
     450,   699,   698,   700,  1114,  1113,     0,     3,   848,   747,
     748,     0,     0,   851,     0,     0,     0,   886,   943,   951,
     469,   850,     0,  1016,     0,  1017,  1018,  1012,   824,     2,
       0,   826,   609,   609,   609,   640,   647,   614,     0,   653,
     640,     0,   488,   601,   638,   634,     0,     0,     0,     0,
     641,   643,   851,   655,   655,   655,     0,   635,   651,   423,
       0,   333,   334,   331,   332,   230,     0,     0,   232,   431,
     231,   570,   488,     0,     0,   343,     0,   311,   310,   312,
       0,   343,   189,   270,     0,   263,     0,   189,   326,   324,
       0,   318,  1115,   327,     0,     0,     0,     0,   379,   380,
     381,   382,     0,   372,     0,   373,   335,     0,   336,     0,
       0,   363,   210,   322,   321,     0,     0,   352,   362,     0,
     343,   365,     0,   367,   389,     0,   421,   851,   822,   753,
     775,   488,     2,     2,   665,     0,   673,   673,  1088,  1089,
    1090,     0,     0,     3,     3,     0,  1049,     0,     0,  1115,
     673,     0,   680,   679,  1115,     0,   662,     3,     0,  1027,
     103,     0,    32,   488,     0,  1115,     0,     0,    88,     0,
      76,     0,    82,     0,    80,    44,   163,     0,   851,   851,
       0,     0,   750,     0,   444,   448,   887,   944,   952,     0,
       0,   790,   828,   605,   607,   603,     0,     0,  1056,     0,
     648,  1061,   650,  1053,   851,   851,   633,   654,   637,     0,
     636,     0,     0,     0,   657,     0,   629,   851,   628,   644,
     656,   645,   646,   652,   343,   343,   233,   570,     0,     0,
     251,   343,   316,   314,   317,   313,     0,   315,     0,   259,
       0,   189,     0,   343,   488,   271,     0,   296,     0,     0,
     319,     0,     0,   343,   342,   343,   383,     0,   374,     2,
       0,     0,     0,   345,     0,     0,   343,     0,   343,   343,
     343,   208,   207,   447,   770,   792,     0,     0,   666,  1115,
    1115,  1091,  1042,     0,     0,  1048,  1050,   663,     0,     0,
     673,     0,   661,     2,    50,    42,    40,    41,     0,    66,
     182,    79,     0,     0,  1007,   446,   445,   978,   993,   749,
    1011,  1019,   631,     0,     0,     0,  1057,  1058,   851,   632,
    1054,  1055,   630,   610,     0,     0,   341,   222,     0,     0,
       0,   244,   343,   224,     0,     0,   343,   253,   268,   279,
     273,   343,   189,   308,     0,   283,     0,     0,   274,   272,
     261,   264,     0,     0,   189,   297,     0,     0,   227,   340,
     371,     2,   488,   337,     0,     0,   399,   350,     0,    69,
     361,   354,     0,   355,   353,   368,   761,   797,   799,     0,
       0,  1051,  1052,   664,   673,  1115,   685,   789,    67,    83,
      81,   851,  1064,  1066,  1059,     0,   642,   343,   239,   234,
     237,     0,   236,   243,   242,     0,   488,   246,   245,   343,
     255,     0,   252,   343,     0,     0,     0,   260,   265,     0,
       0,   189,   309,   284,     0,     0,   343,     0,   299,   300,
     298,   267,   328,     0,   488,   488,     3,   384,   489,   388,
       0,   392,     0,     0,     0,   400,   401,   346,     0,     0,
       0,   669,   671,  1115,     0,   488,  1060,  1062,  1063,   649,
     223,     0,   241,     0,   240,   226,   247,   488,   412,   256,
     343,   257,   254,   269,   282,   280,   276,   288,   286,   287,
     285,   266,   281,   277,   278,   275,   262,     0,     0,     0,
       0,   229,   247,     3,   377,     0,  1056,   385,   386,   387,
     399,     0,     0,     0,   399,     0,   351,   347,   343,     0,
       0,   670,     0,   235,   238,   343,     3,   248,   413,   258,
       0,     0,     0,     0,   307,   305,   302,   306,   303,   304,
     301,     3,   377,     0,     0,  1057,     0,     0,     0,   393,
       0,   402,   356,   343,   668,  1065,   218,     0,     0,   343,
     295,   293,   290,   294,   291,   292,   289,     0,     0,   378,
       0,   405,     0,   403,     0,   405,   357,   220,   219,   225,
       0,   228,     0,   375,   406,     0,     0,   394,     0,   376,
       0,     0,     0,     0,   407,   408,     0,   404,   395,     0,
       0,   396,   409
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1959,  7088,  2672, -1959,    -1,   306,  1779,  -158, -1959,  -342,
   -1959,   441, -1959,  -711, -1959,   910,  -924, -1081, -1959,   304,
    4340,  2045, -1959,  -199, -1959,  1511,   475,   917,   907,   531,
     914,  1473,  1474,  1476,  1471,  1475, -1959,   400,  -112,  8577,
    1016, -1959,  1804, -1959, -1959,  -719,  4085, -1142,  4782, -1959,
     142, -1959,  1006,    83, -1959, -1959,   774,   170, -1959, -1892,
   -1958,   382,   144, -1959, -1959,   767,   395, -1621, -1959, -1546,
   -1959, -1959, -1959, -1959,   190, -1169, -1959, -1959, -1244,   527,
   -1959, -1959, -1959, -1959, -1959,   174, -1217, -1959, -1959, -1959,
   -1959, -1959,   109,   540,   542,   213, -1959, -1959, -1959, -1959,
    -864, -1959,   146,    89, -1959,   219, -1959,  -193, -1959, -1959,
   -1959,  1007,  -816,  -908,   -21, -1959,    34,     8,    90,  8863,
    -871,  -780, -1959,   175, -1959, -1959,    65, -1959,  -137,  2906,
    -339,  -248,  2971,   857,  -646,    52,   197,    22,  1816,  2627,
   -1959, -1959,  2237, -1959,   221,  4881, -1959,  2175, -1959,   162,
   -1959, -1959,  2003,   519,  5223,  4039,   -15,  2023,  -306, -1959,
   -1959, -1959, -1959, -1959,  -310,  6223,  5869, -1959,  -411,   193,
   -1959,  -651,   338, -1959,   267,   832, -1959,    16,  -275, -1959,
   -1959, -1959,  -374,  6540,  -569,  1305,   151,  1483, -1959,    18,
    -182,  -108,    35,  1917,  -639,  -139,  1026,  2172,    80,  -456,
    -235,  -209,  -490,  1435, -1959,  1787,   330,  -939,  1655, -1959,
   -1959,   769, -1959, -1250,  -174,    47,  -910, -1959,   410, -1959,
   -1959, -1151,   543, -1959, -1959, -1959,  2306,  -882,  -302,  -773,
     -16, -1959, -1959, -1959, -1959, -1959, -1959,   259,  -877,  -219,
   -1856,  -200,  8066,   -60,  6987,  -100,  1602, -1959,  1893,   -63,
    -217,  -214,  -208,    81,   -75,   -74,   -65,   497,    40,   179,
     184,  -170,   -20,  -147,  -107,  -103,   -18,   -72,   -53,   -44,
    -760,  -778,  -707,  -703,  -724,   -22,  -700, -1959, -1959,  -740,
    1510,  1512,  1513,  1541, -1959,   656,  7381, -1959,  -598,  -630,
    -606,  -592,  -699, -1959, -1532, -1768, -1759, -1754,  -644,    78,
    -162, -1959, -1959,   -67,     3,  -116, -1959,  8242,  2624,  -596,
    -464
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   850,   424,   425,    83,    84,   426,   400,   427,
    1580,  1581,   428,  1003,  1004,  1005,  1368,  1369,  1370,  1592,
     450,   430,   431,   432,   733,   734,   433,   434,   435,   436,
     437,   438,   439,   440,   441,   442,   443,   452,  1149,   735,
    1503,   796,   222,   798,   446,   871,  1247,  1248,  1249,  1250,
    1251,  1252,  1253,  2177,  1254,  1255,  1696,  2030,  2031,  1961,
    1962,  1963,  2146,  2147,  1256,  1714,  1715,  1716,  1867,  1868,
    1257,  1258,  1259,  1260,  1261,  1262,  1894,  1898,  1525,  1517,
    1263,  1264,  1524,  1518,  1265,  1266,  1267,  1268,  1269,  1270,
    1271,  1733,  2164,  1734,  1735,  2066,  1272,  1273,  1274,  1506,
    2074,  2075,  2076,  2205,  2216,  2096,  2097,   307,   308,   938,
     939,  1215,    86,    87,    88,    89,    90,  1699,   486,    93,
      94,    95,    96,    97,   236,   237,   310,   289,   488,    99,
     489,   100,   609,   102,   103,   157,   356,   313,   107,   108,
     109,   172,   110,   956,   357,   158,   113,   260,   114,   159,
     268,   359,   360,   361,   160,   447,   119,   120,   363,   121,
     605,   931,   929,   930,  1673,   364,   365,   124,   125,  1210,
    1470,  1679,  1680,  1829,  1830,  1471,  1668,  1849,  1681,   126,
     698,  1780,   694,   366,   695,   696,  1328,   968,   611,  1141,
    1142,  1143,   612,   367,   497,   498,   614,  1278,   456,   457,
     223,   515,   516,   517,   518,   519,   344,  1297,   345,   954,
     952,   644,   346,   385,   347,   348,   458,   128,   178,   179,
     129,  1291,  1292,  1293,  1294,     2,  1197,  1198,   635,  1285,
     130,   334,   335,   270,   281,   588,   131,   226,   132,   325,
    1151,   921,   549,   170,   133,   395,   396,   397,   134,   327,
     240,   241,   242,   328,   136,   137,   138,   139,   140,   141,
     142,   245,   329,   247,   248,   249,   330,   251,   252,   253,
     836,   837,   838,   839,   840,   254,   842,   843,   844,   801,
     802,   803,   804,   550,  1190,  1449,   143,  1788,   669,   670,
     671,   672,   673,   674,  1832,  1833,  1834,  1835,   659,   499,
     371,   372,   373,   459,   214,   145,   146,   147,   375,   863,
     675
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   197,   198,    82,   144,   386,   738,   144,   699,    91,
     527,   199,   570,   521,   654,  1298,   195,   531,   213,  1521,
     532,   860,  1036,   106,   494,   399,   533,   317,  1508,   368,
     685,   354,   983,   969,   688,   239,   567,   547,   153,   552,
     585,  1295,  1046,   182,  1205,  1054,   560,  1543,  1544,   677,
    1453,   556,  2092,   104,  1109,    82,    82,   970,    82,  1943,
    1089,  1279,   144,   204,   534,  1286,    98,    91,  1944,   977,
    2038,   971,   445,  1945,  1115,    82,  1507,   744,   246,   582,
     250,   106,   135,  2033,    82,   213,  1495,   535,  1029,   481,
     593,    92,    82,   461,   154,  2039,  1879,    82,   495,   992,
      82,    58,  2032,   224,    82,  1737,   531,   778,   144,   532,
    1116,   104,   912,   914,    58,   533,   200,   398,   624,   626,
     463,   464,  1275,   266,    98,  1110,  1372,   536,   994,  1111,
     465,   537,  1112,   320,  1233,  1896,  1119,  2100,  2145,   275,
     135,  1900,  1126,    82,   224,   654,    82,   382,    82,    92,
     779,  1515,   144,   534,    82,  1379,  2072,    91,  1516,   197,
     198,    82,   538,   115,  2145,    75,   211,  1872,    82,   199,
    1897,   106,   542,  1031,   529,   225,   535,  -832,    75,   243,
     394,   539,   271,  2046,   677,  1738,   282,  -800,   548,  2179,
     540,   264,   394,    82,    82,   276,   577,   168,   105,  1750,
    2040,   104,   969,   148,    82,   978,   714,   687,  1133,    82,
    1135,   690,  2093,  2094,    98,   543,   536,   544,  -832,   512,
     537,   115,   111,  1416,    82,  1417,   970,    82,    82,    82,
     135,   758,   759,    82,    82,   466,   977,  2032,  1599,    92,
     971,   496,  2101,    58,  2034,   998,   370,   239,   197,   198,
    1977,   538,   758,  2038,    82,   201,   105,  1013,   199,  1418,
     202,   542,    82,   629,  1152,   577,   301,   279,   469,   302,
     539,  1600,    82,    82,   200,   211,    82,   303,   918,   540,
     111,   546,   600,    82,  1594,   758,  1691,  2038,   691,   926,
     246,   503,   621,   703,  1739,  -801,  1947,    82,    82,   587,
      82,   115,  1465,   677,   543,    82,   544,    75,   864,    82,
     151,   115,   876,   962,   309,   877,   211,   904,  1476,  1477,
    1454,   878,    82,    82,   995,   908,   936,  1361,   302,  1109,
    1982,  1983,    82,   266,   947,   625,   312,   982,  1519,  1466,
      82,    82,  1481,  1052,   211,    82,   105,   910,  1386,   677,
     988,  1093,  1749,   915,  1401,  1507,  1752,  1943,   589,   879,
     111,  1520,   176,   176,   826,  1740,  1944,   586,   524,  1010,
     111,  1945,  1120,   677,   467,  1279,  1123,   266,  1319,   468,
     677,  1352,   880,    82,   213,   481,    82,  1138,  1139,  1299,
    1402,   620,   394,   494,   648,  1958,  1959,   925,  1329,   176,
    1110,    63,    64,  1480,  1111,   211,   115,  1393,  2091,  1119,
     161,   625,   876,   201,  2024,   877,   162,  1519,   202,   309,
     302,   878,   881,  1522,  1787,  1452,   882,  1331,  2049,  2050,
    1467,   660,  1456,   648,   115,   666,  1275,  1478,  1958,  1959,
    1520,   312,  1418,   692,   167,   115,  1523,   309,   693,   617,
     176,    78,   219,   176,   463,   464,   645,   883,   309,   879,
     646,    82,  1741,   220,   465,   111,   841,   495,   176,   312,
     115,   494,  1324,    58,   279,   380,   884,   969,   896,   221,
     312,  1960,   880,   618,  1446,   885,    82,    82,   962,   469,
     964,   548,   192,   111,   204,  1473,   354,   739,    82,    82,
    -986,   970,   555,    20,   111,   312,  1447,  -986,    82,   563,
     512,   478,   266,  1533,  1474,   971,  1319,  1800,  1802,  1804,
     116,   897,   881,   898,  1987,   945,   882,   989,    82,   111,
     581,   176,  1846,  1508,   526,  2128,   528,    75,  1031,  1847,
      82,   592,   297,   503,   192,   495,   181,   228,  2055,  1465,
    1465,  1465,  -606,   229,   463,   464,   301,   883,  1848,   737,
     548,  1056,   660,  1327,   465,    82,  1674,   641,   896,   466,
    1870,    82,  1675,    82,  1686,  1878,   884,    58,   116,   176,
     176,  1507,   212,   302,   444,   885,  1466,  1466,  1466,  1473,
     176,   183,  1381,  1687,  1947,   244,   642,   643,   272,  1837,
    1693,   494,   283,   184,   603,   176,  1686,   608,  1756,    58,
     496,   897,   151,   898,  1556,   745, -1116,   205,  1838,   946,
     746,  1202,   503,  1078,  1303,  1840,   192,   692,    82,   961,
      82,   266,   693,  2024,    82,    58,    82,  1031,   144,   176,
     193,    75,  1847,    91,   587,    82,   494,   176,   176,    82,
      82,    -3,   176,  1153,  2006,  1602,  1880,   106,   116,   677,
     557,  1942,  1031,   983,   548,   964,  1697,  1407,   116,  1799,
    1697,  1717,  2118,    75,  2138,   495,  1031,  1467,  1467,  1467,
    1183,  1660,    82,  1948,  1717,   228,  2120,   104,   496,  1233,
     176,   212,   632,   176,   594,    82,   548,   905,  1622,    75,
      98,   587,  1949,  1545,   394,   909,   217,   504,   467,   606,
    1136,   370,   503,   468,   700,   229,   135,   702,   815,  -985,
     495,  1847,   548,   919,   163,    92,  -985,   164,   165,  1976,
     166,  1546,   212,  1130,   927,  1573,  1862,  1863,  1864,  1132,
    1952,  -660,  1184,  1375,   230,    82,   231,    82,  -660,    82,
    1335,  1563,   115,    82,   660,   255,    82,  1031,  1865,  2080,
     212,  1572,  1841,   116,  1690,   309,   274,   600,  1024,  1629,
     192,    58,  1133,  1135,   590,  -482,  2044,  -725,  1725,  1025,
    1026,    82,    58,   768,   769,  1606,  1031,   312,  1031,  1008,
     297,   116,  1012,   176,   747,  1014,  2151,   115,  2153,   748,
     299,  1304,   116,  1356,  1017,   176,   176,  1019,  1020,  1021,
    1357,   111,   841,   301,    58,   906,   321,    58,   496,  1346,
    1904,  1475,  1862,  1863,  1864,  1350,    82,   116,   770,   771,
    2048,    82,   105,    82,   738,    75,  1358,  1862,  1863,  1864,
    1542,   920,  2061,   704,  1865,    82,    75,   705,   924,  1031,
     966,   737,   928,  1866,  1094,    82,   111,   737,   548,  1865,
     384,   512,   342,   496,    82,  1117,   737,  1589,  1871,   664,
    1873,  1797,  1144,  1145,   302,  1874,  1591,  1932,    75,  1933,
     478,    75,   496,  1335,   496,   737,   354,  -821,   496,   496,
     496,  1160,    14,    15,    16,    17,    18,  1124,    82,   387,
    1171,   664,   256,   257,   548,   258,   481,   398,   496,  2111,
     259,  1415,  -483,   761,   301,   945,   469,   470,   548,   189,
     762,   763,    14,    15,    16,    17,    18,  1547,  1548,  -126,
    -126,  -126,  -126,  -126,  -126,    82,    82,   512,   557,  1885,
     889,   144,   548,   176,  1874,  1422,    91,    58,    58,   471,
    1765,    58,   176,   144,    58,   472,   957,   960,   265,   504,
     106,  1283,   473,    58,   474,  1323,  1995,  1778,   982,  1212,
     287,  1996,   294,   475,   296,  2133,  1785,  2194,   496,  1789,
    2134,    58,  2195,  1444,  1288,   501,    82,   266,   163,   986,
     104,   164,   165,  1796,   166,  1779,  -484,  1728,  1729,  1730,
    1731,  1732,   176,    98,  1722,   677,    14,    15,    16,    17,
      18,    75,    75,   265,  1826,    75,   294,   296,    75,  1839,
     714,   502,   865,   866,   506,   966,   867,    75,    92,   507,
    1175,  1179,    58,    58,   548,   548,   522,  1429,   504,   545,
    1289,   548,   520,    82,   525,    75,  1433,   546,  1191,    82,
     548,  1667,  1040,   632,  1042,   469,  1045,   548,  1717,   568,
    1051,   569,  1199,  1055,   580,    58,  1203,   265,  1558,   224,
    1206,  1535,    14,    15,    16,    17,    18,   615,    82,  1784,
      58,   512,    14,    15,    16,    17,    18,    58,  1080,    58,
     780,  1400,   841,   619,   781,   806,    75,    75,  1795,   807,
     115,   370,    58,  1117,   574,   469,    82,   664,   758,   116,
     808,   637,    82,    82,   705,  1437,  1554,   203,    64,   548,
     664,  1391,  1392,   386,   386,   591,  1881,   636,   819,    75,
     652,    58,   548,   478,   265,  1277,   294,   296,   496,   496,
     684,    58,   697,  1472,    75,  1136,    82,   354,  1583,  1584,
    1585,    75,   176,    75,   116,  1586,  1587,   935,   693,   111,
     997,   936,   176,  1607,   646,  1098,    75,   548,   265,   548,
    1616,   999,  1620,   265,   548,   646,   664,  1427,  1000,   265,
     599,    64,   705,  1918,  1204,  2085,  1030,   301,  1921,   548,
    1031,   548,  1781,   706,   496,    75,  -486,  1909,  1910,  1928,
     707,  1577,   354,   701,   144,    75,  -487,    74,   708,  1650,
    1157,   711,   265,   712,  1158,  1570,   713,   682,   717,   296,
    2098,   512,   144,   741,    82,    82,    82,    91,   663,    74,
     557,   945,   664,   512,   548,  1288,  1057,  1058,  1059,    80,
     665,   106,  1214,   176,   176,   444,   649,   297,  2098,  1193,
     799,   512,   666,  1031,   548,   144,   760,    82,   774,  1148,
      91,    80,    81,  1195,   632,   721,  2166,  1031,   548,  1136,
    2170,   104,    82,  1136,   106,    82,    82,   775,  1334,    82,
     144,  2148,  1335,  1371,    98,   776,    82,  1335,  2078,    82,
    1541,  1289,  1792,  1322,   807,    74,  1793,    74,   266,  1064,
    1065,  1066,  1067,  1632,   104,  1637,  1638,   777,  1854,    92,
     782,   275,  1335,  2009,  2010,   809,   663,    98,  1827,   265,
     664,   810,   548,   811,    82,   739,   812,    80,   665,    80,
      81,  1858,  1882,   587,  1883,  1031,  1031,  1884,  1158,    82,
    1512,  1031,    92,   813,  1953,   814,  1455,   265,   807,   682,
     296,  2015,  1823,  1824,  1825,  2000,   512,   271,   282,  1031,
    1479,   477,   370,   822,    82,   721,   264,   276,   150,   824,
     174,   175,    65,    66,    67,    68,    69,    70,    71,    72,
    1501,   115,  1862,  1863,  1864,   660,    -3,   737,   845,  2041,
    1472,  1472,  1472,  1031,  1031,  1669,  1472,    82,   265,  2136,
    1457,  1458,  1459,  1335,  1865,  1683,  1919,  2137,  1850,  1850,
    1850,  1031,   354,  -190,   115,  -485,  1277,   370,  2213,  2084,
     847,  2219,  2210,   265,  1582,  2220,   849,   150,   265,   -18,
     265,    65,    66,    67,    68,    69,    70,    71,    72,   279,
     111,   945,  1513,   496,   861,  2083,   496,   496,  1684,  1277,
    1685,   862,   265,   872,   265,   265,    19,   116,   874,   531,
     886,  1698,   532,   144,   887,  1698,   265,    82,   533,   764,
     765,    82,    82,   111,   888,  1376,   890,    77,   900,   265,
     857,   766,   767,   772,   773,   144,   891,  2140,   265,   144,
     144,   501,   741,   512,   153,    48,    49,    50,    51,    52,
      53,    54,    55,   144,   106,   892,   534,   901,   106,   106,
     827,   741,   265,   893,   682,   296,   512,   512,  1022,   741,
    1033,  1034,   106,   652,   741,   894,    82,   895,   176,   535,
     314,   176,   176,   176,   104,   902,   265,   682,   104,   104,
     176,  1359,  1158,   265,  1373,  1374,  1893,  1031,  1377,   587,
     154,   922,   104,  -161,  -161,   923,  1148,  -604,   176,   205,
     741,  1515,  1516,  -602,   176,  1597,  1598,   932,   512,   536,
    1601,  1598,    92,   537,  1605,  1598,    92,    92,   144,   608,
    1106,  1590,  1892,  1639,  1590,   934,   176,   512,  1106,  1652,
      92,   937,    82,   176,  1805,  1158,   933,    82,    82,    82,
    1930,  1158,  1683,   949,   538,  1931,  1598,  1683,   589,  1288,
    1940,  1031,  1998,  1999,   951,   542,  1842,   586,  1692,  1694,
    2019,  1598,   940,   539,   955,   876,   972,   370,   877,  2020,
    1598,   176,   540,   974,   878,   185,     6,     7,     8,     9,
      10,    11,    12,    13,   115,  1684,  1578,  1685,   115,   115,
    1684,   666,  1685,  2067,  1958,  1959,   991,  1700,   543,   996,
     544,  1700,   115,  1032,    82,  1289,  2210,  2211,  1754,    82,
    1595,  1596,   879,  1062,  1063,    82,  1035,    82,  1007,  1702,
    1038,  1060,  1061,  1702,  1702,    82,  1068,  1069,  2008,  -125,
    -125,  -125,  -125,  -125,  -125,   880,   284,  1702,  1751,  1753,
    1851,  1852,  1077,   111,   512,   144,  1082,   111,   111,  1105,
     512,  1106,   388,    14,    15,    16,    17,    18,  1782,  1783,
    1113,   111,  1134,   266,  1137,  1155,  2067,   444,  1185,  1162,
     386,  1163,  1164,  1994,  1280,   881,   275,  1165,   116,   882,
      14,    15,    16,    17,    18,  1028,  1166,    14,    15,    16,
      17,    18,  1167,  1168,   512,    14,    15,    16,    17,    18,
    1351,  1169,   677,  1170,   144,  1192,  1194,  1196,  -804,  1287,
     883,   116,    58,   272,   283,  1200,   827,  1603,   176,   176,
    1207,  1208,   896,  1209,   610,  1284,   559,   389,  1300,   884,
    1296,   264,   276,  1312,  1313,  1288,  1314,  1315,   885,  2077,
      82,  1326,    82,  1333,  1338,   390,    58,   391,   392,    65,
      66,    67,    68,    69,    70,    71,    72,   444,   444,  1330,
    1327,  1332,   265,   176,   176,   897,  1336,   898,  1337,  1340,
    1022,  1341,  1342,   265,    74,  1343,    75,  1345,  2073,  1347,
    1344,    82,   265,  1348,    82,  1349,  2029,  1354,  2127,  1355,
    1378,  1289,  1382,   512,   512,   663,  1362,  1363,  1383,   664,
     512,  1384,  1385,  1405,   279,  1389,    80,   665,    74,  1390,
      75,  1394,   512,  1582,  1886,  1395,  1396,   144,   190,   587,
    1397,  1408,   512,   749,   512,   750,   751,   752,  1410,   799,
    1411,  1412,  1683,   548,  1421,   512,   106,   512,   512,   512,
      80,    81,  1445,  -805,  1450,    82,    82,  1482,   531,  1483,
    1486,   532,  1487,  1496,  1497,   753,  1498,   533,   754,   755,
    1500,  1505,   285,   756,   757,  2069,   104,   286,  -724,  1031,
     290,  1509,   295,  1528,  1530,  1684,  1531,  1685,  1564,  1571,
    1574,  1588,  1537,  1539,  1590,   265,  1611,   586,  1612,  1604,
    1615,  1626,  1627,  1598,    82,   534,  1628,  1630,  1634,  1635,
    1640,   512,  1643,  1647,    92,   512,  1648,  1649,  1657,   196,
     512,   265,  2073,  1658,  1661,  1704,  2073,  2073,   535,  2143,
    1672,  2029,   185,     6,     7,     8,     9,    10,    11,    12,
      13,   238,   176,  1475,  1718,   805,  1516,  1719,  2069,   513,
    1742,   116,  1721,  1723,  1736,   116,   116,  1743,  1744,  2192,
    1745,   176,   817,  1746,  1233,   820,  1757,   176,   536,   116,
    2168,  1758,   537,  1760,   590,  1762,   512,  1763,  1764,  1798,
    1768,  1766,  1769,  2204,  1767,  1770,   115,  2204,   512,   144,
     907,  1776,   512,  1786,  1790,  1791,    85,  1794,   326,   152,
     197,   198,  2214,   538,  1806,   512,  1808,  1812,   106,  1809,
     199,   176,   469,   542,  1821,   629,    82,   144,    82,  1639,
    1814,  1702,   539,  1816,  1836,   559,  1817,  1818,  1855,  1822,
    1677,   540,   285,   225,  1859,  1861,   106,  1889,   104,  1891,
    1908,  1911,  1912,  1915,  2212,   111,  1917,  1916,  1922,   512,
     144,  1920,   896,  1924,    85,  1929,   543,  1934,   544,  1937,
    1938,  1966,  1971,  1986,  1939,   610,   104,  1991,  1997,   106,
    1972,   194,   326,  1984,    82,    82,    92,   530,   238,   285,
      85,  2002,  2011,   265,  2012,  1993,  2013,   512,  2014,  2016,
    2017,  2018,   548,   235,   512,   897,   263,   898,   326,   104,
      85,  2021,  2022,  2023,    92,  2027,  2043,  -587,  2056,  2045,
     176,   176,  2054,  2070,    82,  2081,  2082,   176,   265,  2051,
    2062,  2095,   512,   286,   265,   681,   512,   295,   512,   176,
    2071,  2104,  2121,  2130,  2117,  2119,  2132,    92,   152,   176,
    2131,   176,  2141,  2135,    85,  2150,  2163,   152,   115,   512,
     324,   332,   176,   326,   176,   176,   176,   211,  2154,   492,
      82,  2152,   176,   353,  2167,  2169,   630,   326,   613,    82,
    2173,  2174,  2175,  2190,   722,  2180,   115,  2193,   177,   180,
    2191,  2199,  2201,  1702,  1926,  2207,  2202,   451,  2218,   194,
     194,  2206,  2217,  2221,  1027,  1576,   503,  1070,  1073,  1071,
     152,   484,  1072,  1074,   797,   263,  1504,   111,  1511,   115,
    1706,  1702,  2200,  2144,  1988,   227,  2161,  1727,   176,  1981,
    2139,  2188,   176,  1887,   324,  1888,  1899,   176,  2123,   235,
     235,  2171,  1084,  2122,  2208,   111,   173,  1529,   858,   292,
     513,   579,  2089,  2026,  1702,  1671,  1325,  1154,  1101,  1526,
     324,   953,  1102,  1759,  1905,   868,     3,  1001,    85,  1085,
    1820,  1086,  1087,     0,     0,     0,   318,     0,   111,   319,
       0,     0,   263,     0,   722,     0,   265,    14,    15,    16,
      17,    18,  2203,   176,   343,     0,     0,     0,     0,     0,
     805,   805,     0,     0,  2209,   176,     0,     0,     0,   176,
       0,  1096,     0,     0,  1099,   324,   657,     0,   444,   680,
       0,   332,   176,     0,     0,     0,     0,   332,   324,   324,
       0,     0,   657,     0,     0,  2129,   657,   152,     0,     0,
       0,   835,     0,     0,   265,     0,    58,     0,     0,     0,
       0,     0,     0,   116,     0,     0,     0,   523,   353,   667,
     676,     0,     0,   285,     0,     0,   176,     0,     0,     0,
       0,     0,     0,     0,   353,     0,     0,     0,   353,     0,
     559,     0,   875,     0,     0,     0,     0,  1173,     0,     0,
       0,  1177,   150,     0,   238,  1181,    65,    66,    67,    68,
      69,    70,    71,    72,   176,   583,   584,     0,    74,     0,
      75,   176,     0,     0,     0,     0,   177,     0,     0,   326,
       0,     0,   451,     0,     0,   326,     0,     0,     0,  1827,
       0,   177,     0,   548,   613,     0,     0,     0,     0,   176,
      80,    81,     0,   176,     0,   176,     0,   657,     0,     0,
       0,   444,   388,   444,     0,     0,   451,     0,     0,   800,
       0,     0,     0,     0,     0,   634,   176,   194,     0,     0,
       0,     0,     0,   638,   640,     0,     0,  2215,   647,     0,
       0,     0,   944,     0,   326,   152,  2222,     0,     0,   484,
       0,     0,   444,   834,   150,   676,   174,   175,    65,    66,
      67,    68,    69,    70,    71,    72,   152,     0,     0,   613,
       0,     0,     0,     0,     0,   116,   343,     0,   265,   343,
       0,     0,     0,  2189,     0,     0,     0,   389,     0,     0,
       0,     0,     0,     0,   235,     0,     0,  1387,   492,     0,
     613,  1388,     0,   116,     0,   390,   235,   391,   392,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
    1403,     0,     0,     0,     0,     0,   444,  1404,  1488,     0,
       0,   324,     0,   451,   451,     0,   116,   324,     0,     0,
     353,     0,     0,     0,     0,     0,     0,     0,     0,   805,
       0,     0,     0,     0,   393,     0,   492,     0,     0,     0,
       0,   513,     0,     0,   858,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1441,   657,   492,     0,  1442,   227,
       0,     0,  1443,     0,     0,     0,     0,     0,     0,     0,
       0,   851,   852,     0,   324,     0,   324,     0,   353,   657,
      85,     0,     0,     0,     0,     0,     0,     0,     0,   191,
       0,     0,   657,     0,     0,     0,     0,   353,   484,     0,
     676,     0,   265,  1431,     0,     0,  1435,     0,   667,   216,
    1439,     0,   667,     0,   265,     0,   613,     0,     0,     0,
       0,   353,     0,     0,     0,  1108,     0,   835,   267,     0,
       0,   676,   613,     0,   353,     0,   613,     0,     0,     0,
     288,   291,     0,     0,     0,     0,   152,     0,     0,   613,
       0,     0,     0,     0,   451,    58,     0,   152,   152,     0,
     451,   218,     0,     0,     0,     0,     0,     0,     0,   451,
       0,     0,   152,   152,   152,     0,   216,     0,     0,     0,
       0,     0,     0,   267,     0,     0,   492,     0,     0,   150,
       0,     0,   326,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   300,   265,     0,     0,     0,     0,     0,   948,
       0,     0,     0,     0,     0,     0,   460,    74,   343,    75,
       0,     0,     0,  1367,     0,     0,     0,     0,   484,  1367,
     657,   492,     0,     0,     0,     0,     0,   267,    76,    77,
       0,     0,     0,     0,   800,   800,     0,     0,     0,    80,
      81,  1213,   451,     0,     0,     0,     0,     0,  1367,     0,
       0,   513,     0,     0,     0,     0,     0,     0,   993,     0,
       0,     0,   353,   484,     0,     0,     0,   834,     0,   834,
       0,     0,     0,     0,     0,     0,     0,   578,     0,     0,
       0,     0,   353,     0,   353,     0,     0,     0,   353,   353,
     353,     0,     0,     0,   267,     0,     0,     0,     0,     0,
     155,     0,  1644,     0,     0,     0,  1645,     0,   353,     0,
    1646,     0,     0,     0,  1609,   265,  1367,     0,     0,     0,
       0,     0,     0,     0,     0,  1618,     0,     0,   267,     0,
       0,     0,     0,   267,   324,     0,     0,     0,     0,   267,
       0,     0,     0,   150,     0,     0,   578,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   657,     0,     0,   680,
       0,     0,   101,     0,     0,   156,     0,   662,     0,     0,
       0,     0,   267,   451,     0,     0,     0,     0,   353,     0,
       0,   209,   627,     0,     0,   152,   451,     0,     0,     0,
    1108,     0,  1398,    77,     0,     0,  1399,   835,   353,     0,
    1307,   613,     0,     0,     0,   613,     0,     0,  1131,     0,
       0,   667,   265,     0,   613,     0,   492,     0,  1146,     0,
     101,     0,     0,     0,   613,   723,     0,  1772,     0,     0,
       0,   613,     0,     0,     0,     0,     0,     0,     0,   315,
       0,     0,     0,     0,     0,   150,   210,   916,   209,    65,
      66,    67,    68,    69,    70,    71,    72,   152,   484,     0,
       0,     0,     0,   150,     0,   216,   277,    65,    66,    67,
      68,    69,    70,    71,    72,  1043,     0,     0,   613,     0,
    1807,     0,   613,     0,     0,     0,   613,     0,   454,  1810,
       0,     0,     0,  1811,     0,     0,     0,     0,   662,  1216,
     311,   479,     0,     0,   316,     0,     0,   267,     0,     0,
     101,     0,     0,   322,     0,     0,  1044,     0,     0,     0,
       0,     0,     0,   800,     0,   723,   513,     0,     0,   355,
       0,     0,     0,     0,  1367,     0,     0,     0,   353,   353,
       0,     0,   834,     0,     0,   829,     0,   831,     0,   834,
       0,     0,     0,   322,     0,   462,   848,   572,     0,   576,
       0,     0,     0,     0,     0,     0,   316,   490,     0,     0,
       0,     0,     0,   326,     0,     0,     0,     0,     0,     0,
       0,     0,   460,   460,     0,     0,     0,     0,     0,     0,
     267,     0,     0,     0,   353,     0,   541,  1831,     0,     0,
       0,     0,   155,     0,     0,   311,     0,     0,     0,     0,
       0,     0,   267,     0,     0,     0,   566,     0,     0,     0,
       0,   571,   573,     0,   210,     0,   267,     0,   576,     0,
       0,     0,     0,   311,     0,     0,   152,     0,     0,   267,
       0,     0,     0,     0,   311,   152,     0,   595,     0,     0,
       0,   597,     0,     0,   451,     0,   598,     0,     0,     0,
       0,     0,     0,   513,     0,     0,     0,   573,     0,   311,
       0,     0,   267,   622,     0,     0,     0,     0,     0,     0,
     451,  1623,     0,     0,     0,   631,     0,   451,     0,     0,
     984,     0,     0,   322,     0,     0,   267,     0,     0,     0,
     657,     0,     0,   267,     0,     0,     0,     0,     0,     0,
       0,   263,    85,   454,   655,   460,     0,   679,     0,   353,
     255,     0,     0,  1009,     0,   324,     0,   460,     0,  1015,
     686,   152,   492,     0,   686,     0,   613,     0,   513,     0,
     613,     0,   484,     0,   613,     0,     0,   209,     0,     0,
       0,  1682,     0,     0,     0,     0,     0,   513,  1831,  1831,
       0,     0,  1367,     0,     0,     0,     0,  1367,  1367,  1367,
       0,     0,     0,     0,   484,     0,   825,     0,   322,   152,
       0,     0,     0,     0,     0,     0,     0,  1489,  1491,  1493,
       0,     0,     0,     0,     0,     0,   150,   479,   174,   175,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,   322,     0,     0,     0,     0,     0,     0,     0,
    1514,   460,     0,   150,     0,   232,   233,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,   316,  1216,   353,     0,   655,   353,   353,     0,  1534,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   613,   316,     0,   454,   454,     0,     0,     0,     0,
    1490,     0,  1676,    77,     0,     0,     0,  1831,     0,  1677,
       0,     0,     0,    80,    81,     0,     0,  1568,     0,     0,
       0,     0,   152,   152,   152,   152,     0,   152,   152,     0,
       0,     0,     0,  1678,   332,     0,     0,  1129,     0,     0,
       0,     0,     0,     0,   613,     0,     0,   451,     0,     0,
    1188,   451,   451,   613,     0,     0,     0,   613,     0,   322,
     322,     0,     0,     0,     0,   451,   490,     0,     0,     0,
       0,     0,   492,     0,     0,     0,     0,     0,  1682,     0,
       0,   311,  2087,  1682,     0,     0,  1831,     0,  2142,  1843,
       0,  1682,   263,     0,     0,   460,     0,     0,     0,   150,
    1367,     0,  1367,    65,    66,    67,    68,    69,    70,    71,
      72,  1364,   267,     0,   484,  1365,     0,  1366,     0,     0,
       0,     0,     0,     0,   355,     0,   101,   454,  1831,     0,
       0,     0,     0,  1281,  1282,   454,     0,     0,   454,   454,
     152,   454,   667,   686,   965,     0,     0,     0,     0,    77,
     454,     0,  1593,   454,   454,   454,     0,     0,   976,     0,
       0,     0,     0,     0,  1688,  1689,     0,   655,     0,     0,
       0,     0,   985,     0,     0,     0,     0,     0,     0,     0,
     686,     0,     0,     0,     0,     0,  1831,  1831,     0,     0,
       0,     0,   322,     0,     0,     0,     0,     0,     0,     0,
     322,     0,     0,   322,   322,     0,   322,     0,     0,     0,
       0,     0,     0,     0,     0,   322,    19,     0,   322,   322,
     322,     0,     0,     0,     0,     0,  1831,     0,     0,  1360,
    1678,  1828,     0,   454,     0,  1678,     0,   451,     0,     0,
       0,  1678,   150,  1678,   174,   175,    65,    66,    67,    68,
      69,    70,    71,    72,     0,  1954,     0,     0,  1682,    52,
      53,    54,    55,     0,     0,     0,   332,   152,     0,     0,
       0,     0,     0,  1380,   490,   150,     0,   376,   377,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,  1088,     0,     0,     0,     0,     0,     0,   322,     0,
       0,     0,  1492,     0,   150,     0,  1047,  1048,    65,    66,
      67,    68,    69,    70,    71,    72,  1049,     0,   686,   965,
     326,     0,  1406,     0,  1409,  1114,   152,    78,     0,     0,
       0,     0,   378,     0,     0,     0,  1413,  1414,   490,   379,
     490,     0,  1419,  1420,   490,   490,   490,     0,     0,     0,
       0,     0,  1428,     0,     0,     0,     0,  1050,   152,     0,
       0,     0,     0,     0,   490,     0,     0,     0,  1682,     0,
       0,     0,     0,     0,     0,     0,  1290,   454,  1853,  1448,
       0,     0,  1451,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1828,  1828,     0,   150,     0,   232,   233,    65,
      66,    67,    68,    69,    70,    71,    72,  1678,     0,   150,
    1678,   232,   233,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   332,   267,     0,     0,     0,     0,     0,  1276,
       0,     0,     0,     0,   490,     0,     0,     0,     0,   451,
     460,   156,   322,     0,  1510,     0,     0,     0,   479,     0,
       0,   984,   958,     0,   686,     0,     0,  1311,   267,   959,
       0,     0,     0,     0,  1317,     0,  1320,     0,     0,     0,
       0,     0,   324,  1321,     0,  1532,     0,     0,     0,     0,
     326,     0,     0,  1536,     0,  1538,  1540,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1550,     0,
    1551,     0,  1552,     0,     0,     0,     0,     0,     0,  1561,
       0,  1828,     0,   316,   355,   657,     0,     0,     0,     0,
    1678,   150,   613,   232,   233,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,   150,     0,   630,   326,
      65,    66,    67,    68,    69,    70,    71,    72,  1364,    74,
       0,    75,  1365,     0,  1366,     0,     0,   152,     0,     0,
     118,     0,     0,   118,     0,     0,     0,     0,     0,     0,
    2125,    77,     0,     0,   548,     0,     0,     0,   326,     0,
       0,    80,    81,  1613,  1614,     0,    77,     0,   657,  1801,
    1828,     0,     0,     0,   490,   490,     0,  1655,  2007,     0,
       0,   152,     0,     0,     0,     0,     0,     0,  1636,     0,
       0,     0,     0,     0,     0,  1641,   267,     0,   118,  1642,
       0,     0,     0,     0,     0,     0,     0,  1290,     0,   152,
     152,     0,  2126,   332,     0,     0,  1468,     0,     0,     0,
       0,    58,     0,     0,   118,  1659,     0,   150,     0,     0,
     490,    65,    66,    67,    68,    69,    70,    71,    72,  1364,
     269,     0,   152,  1365,   118,  1366,     0,     0,     0,     0,
       0,   454,     0,     0,   267,   150,     0,   232,   233,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
    2126,  2126,   156,     0,     0,     0,     0,    77,   118,     0,
    1803,  1469,   118,     0,     0,    75,     0,     0,   118,     0,
    1276,   118,     0,     0,     0,   269,     0,     0,     0,     0,
       0,     0,   454,     0,  1398,    77,   349,   118,   381,     0,
    2126,     0,     0,     0,     0,     0,   322,     0,     0,     0,
       0,     0,     0,  1276,     0,  1771,     0,     0,     0,     0,
       0,   455,  1775,     0,  1777,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,   455,     0,     0,  1527,   269,
     454,     0,     0,     0,   150,   355,   203,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   150,   322,   174,   175,
      65,    66,    67,    68,    69,    70,    71,    72,   655,     0,
       0,     0,     0,   118,     0,     0,     0,   571,     0,     0,
       0,     0,     0,     0,     0,     0,   460,     0,     0,     0,
     118,     0,   118,     0,    77,     0,     0,   857,     0,  1813,
     355,   118,     0,     0,     0,   322,   269,     0,     0,     0,
       0,     0,   118,   639,     0,     0,     0,     0,   267,     0,
       0,     0,     0,     0,     0,     0,     0,   604,     0,     0,
     118,     0,     0,     0,     0,   118,     0,   118,     0,     0,
     269,   118,     0,     0,     0,   269,     0,     0,     0,     0,
       0,   269,     0,  1468,  1468,  1468,   155,  1665,  1666,  1670,
       0,   118,     0,     0,     0,     0,     0,     0,     0,   490,
       0,     0,   490,   490,     0,     0,     0,     0,     0,     0,
       0,     0,   118,   150,   269,   118,     0,    65,    66,    67,
      68,    69,    70,    71,    72,  1364,     0,     0,   118,  1365,
       0,  1366,   118,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1469,  1469,
    1469,   156,   573,   322,   322,     0,     0,     0,     0,     0,
     336,     0,     0,    77,     0,  1913,  1914,     0,   337,   338,
     339,   340,     0,  1701,     0,     0,   455,  1701,  1701,  1923,
     784,   785,   786,   787,   788,   789,   790,   791,   792,   793,
     794,  1701,    58,     0,   219,     0,     0,     0,     0,     0,
       0,  1290,     0,     0,   267,     0,     0,     0,     0,   150,
     455,   174,   175,    65,    66,    67,    68,    69,    70,    71,
      72,   795,   736,     0,     0,     0,   150,     0,   232,   233,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   118,
     355,     0,     0,   455,   429,     0,     0,     0,     0,   269,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
     118,     0,   341,     0,     0,     0,   156,    58,     0,     0,
       0,     0,     0,     0,     0,   832,    77,     0,     0,   664,
     342,    58,     0,     0,     0,     0,    80,   833,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   454,     0,
       0,   150,     0,   232,   233,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   150,   118,   232,   233,    65,
      66,    67,    68,    69,    70,    71,    72,   455,   455,    74,
       0,    75,   269,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,   118,
     234,    77,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,    81,   322,   323,    77,     0,     0,   269,  1845,
       0,     0,     0,   911,   913,    80,    81,     0,     0,     0,
       0,   269,     0,     0,     0,     0,     0,  1290,     0,     0,
       0,   118,   118,  1857,   118,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,   381,
       0,   118,   455,     0,   269,   267,     0,     0,     0,   454,
       0,   150,   118,   601,   602,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   118,     0,     0,   269,     0,
       0,     0,   604,     0,     0,   269,     0,     0,   118,     0,
     990,     0,   156,     0,     0,     0,     0,     0,  2124,     0,
     118,   710,    58,     0,     0,   429,   716,     0,   455,     0,
       0,   118,   118,    78,   455,   725,   726,     0,     0,     0,
       0,     0,     0,   455,   322,     0,   118,   118,   118,     0,
     429,   429,     0,     0,     0,     0,   150,     0,   232,   233,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   429,     0,     0,   736,  2162,     0,     0,     0,  1946,
     736,     0,     0,     0,    74,     0,    75,     0,     0,   736,
       0,     0,     0,     0,     0,     0,     0,     0,  2178,     0,
       0,     0,   455,     0,   429,  2125,    77,     0,   736,   548,
       0,     0,     0,  2187,     0,     0,    80,    81,   118,     0,
       0,     0,     0,     0,     0,  1701,   455,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,   118,     0,
       0,     0,     0,     0,  1076,     0,   118,   455,     0,     0,
     150,   118,   174,   175,    65,    66,    67,    68,    69,    70,
      71,    72,   112,     0,     0,     0,   118,     0,   118,     0,
       0,     0,   118,   118,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,   118,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     112,     0,  1189,     0,    46,     0,    47,     0,   150,     0,
     232,   233,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,  2068,     0,    58,     0,     0,     0,     0,
       0,     0,     0,   118,     0,     0,    74,   455,    75,     0,
       0,     0,   118,     0,     0,     0,   278,     0,     0,   118,
     455,     0,     0,     0,     0,     0,     0,  1676,    77,     0,
       0,     0,   118,     0,  1309,   455,     0,  1701,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     112,     0,     0,     0,     0,  1701,  2068,   150,     0,   599,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   358,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,   455,     0,     0,     0,     0,     0,  1701,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   491,     0,     0,
    1079,     0,     0,     0,     0,     0,     0,  2165,     0,     0,
     429,   429,   429,   429,   429,   429,   429,   429,   429,   429,
     429,   429,   429,   429,   429,   429,   429,   429,   429,     0,
       0,     0,     0,     0,     0,   112,     0,     0,     0,     0,
       0,     0,    58,   118,     0,     0,     0,   118,     0,     0,
       0,     0,   118,   118,     0,     0,   118,     0,     0,     0,
       0,     0,     0,   112,     0,     0,   118,     0,     0,     0,
       0,     0,     0,   118,   112,     0,   150,   596,   232,   233,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,   358,     0,     0,   429,     0,     0,     0,   112,
       0,     0,     0,   278,    74,     0,    75,     0,   118,     0,
       0,    14,    15,    16,    17,    18,     0,     0,     0,     0,
     118,     0,     0,     0,   118,   234,    77,     0,   118,     0,
       0,     0,     0,     0,   117,     0,    80,    81,     0,     0,
       0,     0,     0,     0,   656,     0,     0,   278,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,   118,
     656,     0,     0,     0,   656,     0,     0,     0,   455,   150,
      58,   232,   233,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   117,     0,   455,     0,     0,    74,     0,     0,
       0,   455,     0,     0,   150,     0,   232,   233,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   234,    77,
      14,    15,    16,    17,    18,   269,   118,     0,     0,    80,
      81,     0,    74,   118,    75,     0,     0,     0,   280,     0,
     736,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,   323,    77,     0,   455,     0,     0,     0,
    1309,     0,     0,     0,    80,    81,     0,     0,   429,     0,
       0,     0,   117,  1567,   429,   656,     0,     0,     0,    58,
       0,     0,   117,     0,     0,   429,     0,   118,   455,     0,
       0,     0,     0,   118,     0,     0,     0,     0,     0,     0,
     150,   362,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   150,     0,   232,   233,    65,    66,    67,
      68,    69,    70,    71,    72,   429,   150,     0,   174,   175,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   493,
       0,    74,     0,    75,     0,     0,     0,     0,   358,     0,
       0,     0,    78,     0,     0,     0,     0,   118,     0,     0,
     118,   118,  1676,    77,     0,     0,   491,     0,     0,     0,
       0,     0,     0,    80,    81,   502,     0,   117,   118,     0,
       0,   112,   118,     0,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1656,
       0,     0,     0,     0,     0,   117,   118,   118,   118,   118,
     118,   118,   118,     0,     0,     0,   117,     0,   269,     0,
       0,     0,     0,   358,   491,     0,   112,     0,     0,     0,
       0,   455,     0,     0,   362,   455,   455,     0,     0,     0,
       0,   117,     0,   656,   491,   280,     0,     0,     0,   455,
       0,     0,     0,     0,   358,   150,     0,   232,   233,    65,
      66,    67,    68,    69,    70,    71,    72,   656,     0,   429,
       0,     0,     0,     0,     0,     0,   269,  1695,  1703,     0,
     656,  1695,  1713,    74,     0,     0,   658,  1720,     0,   280,
       0,  1724,     0,  1726,     0,  1713,     0,     0,   455,     0,
       0,     0,   658,   118,   832,    77,   658,     0,   664,     0,
       0,     0,     0,     0,     0,    80,   833,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,   666,     0,
       0,     0,    14,    15,    16,    17,    18,     0,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,   118,   429,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,     0,   118,
       0,     0,     0,     0,   491,     0,     0,   429,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   743,
     358,    58,    78,   418,     0,     0,     0,   429,   429,   429,
       0,     0,     0,     0,   429,   429,   358,     0,     0,     0,
     358,     0,     0,     0,     0,     0,     0,   658,   656,   491,
       0,   455,     0,   358,     0,   150,     0,   429,     0,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   358,     0,
     358,     0,     0,     0,   358,   358,   358,  1819,     0,     0,
     269,   118,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,   358,     0,     0,   429,   429,     0,
       0,     0,     0,     0,    76,    77,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,    81,     0,     0,     0,
     362,   150,  1856,   174,   175,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,   493,     0,
     118,  1875,  1877,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   117,     0,   358,     0,     0,     0,   112,
       0,     0,     0,     0,   358,     0,     0,     0,     0,     0,
     506,  1895,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   656,     0,     0,   278,     0,     0,
       0,     0,     0,     0,     0,   362,   493,   150,   117,   232,
     233,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     123,     0,     0,   123,     0,   658,   493,     0,     0,     0,
       0,     0,     0,     0,     0,    74,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   269,     0,     0,   658,
       0,     0,     0,     0,   491,     0,  2125,    77,     0,     0,
     548,     0,   658,   455,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,   150,     0,   232,   233,    65,    66,    67,    68,
      69,    70,    71,    72,  1965,     0,     0,     0,     0,     0,
       0,  1968,     0,  1970,   123,     0,  1975,  1979,     0,  1713,
      74,     0,     0,     0,  1985,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,   358,     0,     0,     0,   358,
       0,   323,    77,   293,   358,   358,     0,     0,   358,     0,
       0,     0,    80,    81,     0,     0,     0,     0,   358,     0,
       0,     0,     0,     0,  1485,   358,   493,     0,   123,     0,
       0,     0,   123,     0,     0,     0,  1499,     0,   123,     0,
       0,   123,   362,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,   362,     0,
     358,     0,   362,     0,     0,     0,     0,     0,     0,     0,
     658,   493,   358,     0,     0,   362,   358,     0,     0,     0,
     358,   123,  2053,     0,     0,     0,     0,  2058,  2060,     0,
     362,     0,   362,     0,   123,   118,   362,   362,   362,     0,
       0,     0,     0,     0,     0,   429,     0,  2079,     0,     0,
       0,     0,     0,     0,     0,     0,   362,     0,     0,     0,
     112,     0,     0,   118,   118,     0,     0,   269,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,  2103,
     123,  2106,   123,   112,  2108,  2110,   118,   123,     0,  2113,
    2115,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,     0,   362,   278,     0,
       0,   117,     0,     0,     0,   358,   362,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,   123,     0,     0,
       0,   123,     0,     0,     0,     0,   658,     0,   656,   280,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,  2156,  2158,  2160,   150,     0,   232,   233,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   358,
     491,     0,     0,     0,   122,     0,     0,   122,     0,     0,
       0,     0,     0,     0,    74,     0,  2182,  2184,  2186,     0,
       0,     0,     0,     0,     0,     0,   493,     0,     0,     0,
       0,     0,     0,     0,     0,  1676,    77,     0,     0,     0,
       0,     0,  1677,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,   123,     0,     0,   358,
       0,     0,   358,   358,     0,     0,     0,     0,   429,  1747,
    1748,     0,     0,     0,     0,     0,     0,     0,   122,     0,
     358,     0,     0,     0,   358,     0,     0,   362,   358,     0,
     123,   362,     0,     0,     0,     0,   362,   362,   122,     0,
     362,     0,     0,     0,     0,     0,     0,     0,   429,     0,
     362,     0,     0,     0,     0,     0,     0,   362,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,   112,     0,     0,   122,   112,   112,     0,
     123,     0,   122,     0,     0,   122,     0,     0,     0,     0,
       0,   112,   362,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,     0,     0,     0,   362,     0,
       0,     0,   362,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
     491,   429,     0,   429,     0,   358,     0,   123,   123,     0,
       0,     0,   117,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,   122,     0,     0,
       0,     0,   429,     0,     0,   117,     0,     0,     0,     0,
       0,     0,     0,     0,   122,     0,   122,  1860,   358,     0,
       0,   122,     0,  1869,     0,   122,     0,   358,     0,     0,
     280,   358,     0,   429,   123,     0,   122,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   122,
     658,   122,     0,     0,     0,   122,     0,  1902,     0,     0,
       0,   127,     0,     0,   127,     0,   429,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,   362,   493,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,   123,   123,   278,   123,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,   123,   123,   123,   127,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   127,     0,     0,     0,     0,
       0,   362,     0,     0,   362,   362,  1956,  1957,     0,     0,
     122,     0,     0,  1967,     0,   127,     0,     0,     0,     0,
       0,     0,   362,     0,     0,  1980,   362,     0,     0,     0,
     362,     0,     0,     0,     0,  1989,     0,  1990,     0,     0,
       0,     0,     0,     0,   122,     0,   123,     0,  2001,   127,
    2003,  2004,  2005,   127,     0,     0,     0,     0,     0,   127,
       0,     0,   127,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   122,     0,   117,     0,     0,     0,   117,
     117,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   117,   122,     0,     0,     0,     0,     0,
       0,     0,   127,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2037,   127,     0,     0,  2042,     0,
       0,     0,     0,  2047,     0,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   493,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,     0,   127,     0,     0,     0,     0,     0,
       0,   122,   122,     0,     0,     0,     0,     0,     0,     0,
       0,   127,     0,   127,     0,     0,     0,   123,   127,  2090,
       0,     0,   127,   122,     0,     0,     0,     0,     0,   123,
     123,  2099,     0,   127,     0,  2102,     0,     0,     0,     0,
     362,     0,     0,     0,     0,     0,     0,     0,  2116,   362,
       0,     0,     0,   362,     0,     0,   127,     0,   127,     0,
       0,     0,   127,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   127,   656,     0,     0,     0,     0,     0,     0,
       0,     0,  2149,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
    2172,     0,     0,     0,   122,   280,     0,  2176,     0,     0,
       0,     0,   122,     0,     0,   122,   122,     0,   122,     0,
       0,     0,     0,     0,     0,   112,   656,   122,     0,     0,
     122,   122,   122,     0,     0,  2196,     0,   127,     0,  2198,
       0,  2176,     0,     0,     0,     0,   358,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,     0,  2198,     0,     0,     0,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     127,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,     0,     0,     0,   171,     0,     0,     0,     0,
       0,   127,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   171,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,   149,     0,     0,     0,     0,     0,   117,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,   127,
     171,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,   123,     0,   171,     0,   171,     0,     0,     0,     0,
     127,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   383,     0,     0,
       0,   122,     0,     0,     0,   123,     0,   206,     0,     0,
       0,     0,     0,   122,   122,   127,     0,     0,     0,     0,
       0,     0,   383,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,   658,     0,     0,     0,     0,
       0,   171,     0,     0,     0,   171,     0,     0,   171,   171,
       0,     0,   171,     0,     0,   171,   171,     0,   171,     0,
     171,   127,     0,     0,     0,   122,     0,     0,     0,   127,
       0,     0,   127,   127,     0,   127,     0,     0,     0,   117,
       0,     0,     0,     0,   127,     0,     0,   127,   127,   127,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   117,   658,     0,
       0,     0,     0,     0,     0,   206,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,     0,
     171,     0,     0,   171,     0,     0,     0,     0,     0,     0,
     117,     0,     0,     0,     0,     0,   123,   123,   123,   123,
     123,   123,   123,     0,     0,     0,     0,   171,   171,     0,
       0,     0,     0,     0,     0,     0,     0,   127,     0,     0,
     575,   123,   171,     0,     0,   123,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     616,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   623,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   633,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,   122,     0,     0,     0,     0,     0,     0,
     653,     0,   122,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,     0,   122,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     127,   127,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,     0,     0,     0,     0,   742,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   383,     0,     0,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   171,     0,     0,     0,     0,   783,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,   369,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   127,     0,     0,   823,     0,   122,     0,     0,
     828,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   480,   369,     0,     0,
     854,     0,     0,     0,   855,   856,     0,     0,   859,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   873,     0,     0,   383,     0,     0,   551,
       0,     0,     0,     0,     0,     0,   551,     0,     0,     0,
     123,     0,     0,     0,     0,     0,   903,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,   171,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,   171,     0,     0,     0,     0,     0,     0,     0,     0,
     122,   122,   122,   122,   122,   122,   122,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,   551,     0,   122,
     122,     0,     0,   943,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   122,     0,     0,     0,   950,     0,     0,
       0,     0,     0,     0,   369,   668,     0,     0,     0,     0,
       0,   127,     0,   123,     0,     0,     0,     0,     0,     0,
     127,     0,   973,     0,   689,     0,     0,     0,     0,   127,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   127,     0,     0,     0,     0,
     171,   171,   127,     0,     0,     0,     0,   171,   122,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   127,  1023,     0,
       0,     0,   171,     0,     0,   171,   171,     0,   171,     0,
     171,   171,     0,     0,     0,   551,   127,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   551,   818,     0,   551,   821,     0,     0,     0,
       0,     0,     0,     0,     0,   369,     0,     0,     0,   668,
       0,     0,     0,   171,     0,     0,     0,   171,     0,     0,
       0,   171,   480,     0,   127,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
    1103,   551,  1104,     0,     0,   551,     0,     0,   828,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1147,     0,   171,   171,     0,
       0,     0,     0,     0,  1156,     0,   369,     0,     0,     0,
    1159,   171,     0,     0,     0,     0,   123,     0,     0,     0,
    2028,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   127,   127,   127,
     127,   127,   127,   127,     0,   653,     0,     0,     0,     0,
    1201,   551,     0,     0,   369,     0,   122,     0,   401,     0,
       0,   402,   127,   403,     0,   404,   127,   127,     0,     0,
       0,     0,     0,   963,   369,     0,     0,     0,     0,     0,
     127,     0,   405,     0,   668,     0,     0,     0,   668,     0,
       0,     0,     0,     0,     0,   981,     0,   369,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   406,   407,     0,   408,   409,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   410,   411,   398,
       0,   412,   413,   414,     0,   415,   416,   122,     0,  1339,
     171,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   169,   127,     0,     0,     0,     0,
       0,     0,     0,     0,   417,     0,     0,    78,   418,     0,
       0,     0,     0,     0,   419,    80,    81,   420,   421,   422,
     423,     0,     0,     0,   171,     0,     0,     0,     0,   171,
       0,     0,   171,     0,     0,     0,   171,     0,     0,     0,
       0,     0,     0,     0,   369,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     551,   551,     0,     0,     0,     0,     0,     0,     0,   298,
       0,   551,  1097,     0,   551,  1100,     0,     0,     0,     0,
       0,     0,   304,     0,   305,     0,     0,     0,   963,   369,
       0,     0,   127,   668,     0,   668,   668,     0,     0,     0,
       0,     0,   668,     0,     0,     0,     0,     0,   369,     0,
     369,     0,     0,     0,   369,   369,   369,     0,     0,     0,
       0,     0,   127,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   369,     0,   551,     0,     0,   122,
     551,     0,     0,     0,     0,     0,     0,   551,  1174,     0,
       0,   551,  1178,     0,     0,   551,  1182,     0,     0,     0,
       0,     0,     0,  1186,     0,   149,     0,   122,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,   127,     0,     0,     0,     0,     0,   553,   554,   171,
     171,   558,     0,     0,   561,   562,     0,   564,     0,   565,
     122,     0,     0,     0,   369,   551,     0,   215,     0,     0,
       0,     0,     0,   127,   783,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   668,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   171,     0,     0,   171,     0,   171,   171,     0,     0,
       0,     0,     0,  1549,   215,     0,     0,     0,   333,     0,
       0,     0,     0,   480,   369,     0,     0,     0,     0,     0,
     374,     0,     0,     0,   127,     0,   650,   651,     0,     0,
    1575,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   683,   171,     0,   215,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   500,     0,
       0,     0,   505,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   551,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   369,   369,     0,     0,   668,   668,
       0,     0,     0,     0,     0,   668,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   215,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   171,     0,     0,     0,   273,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   816,     0,     0,
     369,     0,     0,   551,  1432,     0,   551,  1436,     0,     0,
     551,  1440,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   505,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   215,     0,   127,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   661,     0,   678,     0,     0,
       0,     0,   171,     0,   127,     0,     0,     0,     0,     0,
       0,   899,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1761,     0,     0,     0,     0,     0,
     171,     0,     0,     0,     0,     0,     0,   127,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   740,
       0,     0,     0,   171,     0,   369,     0,     0,     0,   171,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,     0,     0,   215,   668,  1557,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,     0,     0,     0,   661,     0,     0,     0,
       0,     0,   846,     0,     0,     0,     0,   979,   980,     0,
       0,     0,     0,     0,   171,     0,     0,  1761,     0,     0,
     987,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   551,  1610,     0,     0,     0,   453,
       0,     0,     0,     0,     0,   551,  1619,     0,   668,     0,
       0,     0,     0,   485,     0,     0,     0,     0,     0,   369,
       0,     0,   369,   369,     0,   171,   171,   514,     0,   514,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     215,   215,     0,     0,     0,     0,     0,   500,     0,     0,
       0,   171,   171,     0,     0,     0,     0,     0,     0,   383,
       0,     0,     0,     0,   171,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1906,  1907,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1090,
    1091,     0,     0,     0,     0,   374,  1095,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   500,     0,   967,   628,     0,
       0,  1118,     0,     0,  1121,  1122,     0,  1125,     0,  1127,
    1128,     0,     0,     0,     0,     0,     0,     0,   661,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,     0,     0,   171,     0,     0,     0,     0,
       0,     0,     0,   215,     0,     0,     0,     0,   208,     0,
       0,   215,  1172,     0,   740,   215,  1176,   215,   668,     0,
    1180,     0,     0,     0,     0,     0,   740,     0,     0,   740,
     740,   740,     0,     0,     0,     0,     0,  1992,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,     0,
       0,  1761,     0,     0,     0,   208,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   500,  1301,  1302,     0,     0,
       0,   208,     0,     0,     0,     0,     0,     0,     0,     0,
    1318,     0,     0,     0,     0,     0,     0,   551,  2036,   215,
       0,     0,     0,     0,     0,   208,     0,     0,     0,     0,
       0,     0,     0,   551,     0,     0,     0,     0,     0,   487,
     500,     0,     0,     0,     0,  2064,     0,     0,     0,  2065,
       0,     0,     0,     0,     0,     0,     0,     0,   514,   500,
       0,   500,     0,     0,   514,   500,   500,   500,     0,   870,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   171,     0,     0,   500,   185,     6,     7,     8,
       9,    10,    11,    12,    13,     0,   208,     0,     0,     0,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,   256,
     257,     0,   258,    46,     0,    47,     0,   259,     0,     0,
      49,    50,    51,    52,    53,    54,    55,     0,     0,  1318,
       0,     0,     0,     0,     0,   500,     0,     0,     0,     0,
       0,     0,     0,   215,     0,   208,     0,     0,     0,     0,
     942,     0,     0,     0,     0,     0,     0,   846,   551,   551,
       0,     0,     0,     0,     0,     0,   208,     0,     0,     0,
       0,     0,     0,  1424,   551,     0,     0,     0,  1430,     0,
     485,  1434,     0,     0,     0,  1438,     0,     0,     0,     0,
       0,     0,     0,   975,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,     0,     0,  -464,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1006,     0,     0,     0,     0,     0,     0,
       0,     0,  -464,     0,  1016,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   208,     0,     0,     0,   870,  1037,
       0,     0,  1039,     0,  1041,     0,     0,   551,     0,     0,
    1006,     0,  1053,  1006,     0,   551,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   500,   500,   208,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1081,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1083,     0,     0,     0,     0,     0,  1555,
       0,     0,     0,     0,  1092,     0,     0,     0,  1565,  1566,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     485,   500,   551,  2088,     0,  1081,   551,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   208,   208,     0,     0,     0,  1150,     0,   487,   514,
       0,     0,     0,     0,     0,     0,     0,     0,   551,  1608,
       0,  1161,   740,     0,     0,     0,     0,     0,     0,     0,
    1617,     0,     0,  1621,     0,  1624,  1625,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1187,     0,     0,     0,     0,     0,     0,   740,     0,     0,
       0,     0,     0,     0,     0,     0,   208,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   551,   551,     0,     0,
       0,  1651,     0,     0,     0,     0,   487,     0,   273,     0,
       0,     0,     0,     0,     0,     0,   374,     0,   453,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   215,   208,
       0,     0,  1308,  1310,     0,     0,   551,     0,     0,   661,
     485,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   208,     0,   401,     0,     0,   402,
       0,   403,   208,   404,     0,     0,   208,     0,   208,     0,
       0,   374,     0,     0,     0,     0,   740,     0,     0,     0,
     405,     0,     0,     0,  1755,     0,     0,     0,     0,     0,
    1081,     0,     0,     0,     0,     0,     0,     0,  1353,     0,
       0,     0,     0,     0,     0,  1006,     0,     0,     0,     0,
     406,   407,     0,   408,   409,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   410,   411,   398,     0,   412,
     413,   414,     0,   415,   416,     0,   487,     0,     0,     0,
     500,    74,     0,   500,   500,     0,     0,     0,     0,   514,
       0,     0,     0,     0,     0,     0,  1707,  1708,  1709,     0,
     208,     0,   417,  1876,     0,    78,   418,     0,     0,     0,
       0,  1621,   419,    80,    81,   420,   421,   422,   423,     0,
       0,   487,     0,     0,     0,     0,     0,     0,     0,   740,
     740,   740,     0,     0,   740,   740,     0,     0,     0,  1815,
     487,   505,   487,     0,     0,     0,   487,   487,   487,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   514,     0,  1423,     0,  1426,   487,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   273,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   487,     0,     0,     0,
       0,     0,     0,     0,   208,     0,  1502,  1502,     0,     0,
       0,     0,  1460,  1903,  1461,     0,     0,     0,     0,  1462,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,  1935,  1936,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,   208,     0,  1553,     0,
       0,    58,  1463,     0,  1562,     0,     0,     0,     0,     0,
    1950,  1951,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1006,  1955,   215,     0,   485,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    63,    64,     0,
       0,     0,     0,     0,   514,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,     0,     0,     0,     0,     0,
       0,   870,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   487,   487,     0,     0,
       0,     0,     0,  1464,     0,     0,     0,    78,  1011,     0,
       0,     0,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2025,     0,     0,     0,     0,     0,
       0,     0,   487,     0,     0,     0,  1653,  1654,     0,     0,
       0,     0,     0,     0,     0,   740,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1006,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   514,     0,     0,   870,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2086,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   273,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     401,     0,     0,   402,     0,   403,  1037,   404,     0,     0,
       0,     0,     0,     0,     0,  1773,  1774,     0,     0,     0,
       0,     0,     0,     0,   405,     0,   514,   208,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   208,
       0,     0,     0,     0,     0,   514,     0,   870,     0,     0,
     208,     0,     0,     0,   406,   407,     0,   408,   409,  1973,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   410,
     411,   398,     0,   412,   413,   414,     0,   415,   416,     0,
       0,     0,   208,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1707,  1708,  1709,     0,     0,     0,   417,  1974,     0,    78,
     418,     0,     0,     0,   740,     0,   419,    80,    81,   420,
     421,   422,   423,     0,     0,     0,     0,     0,     0,   453,
       0,     0,     0,     0,  1844,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2197,     0,     0,
       0,   487,     0,     0,   487,   487,     0,     0,     0,     0,
       0,     0,     0,     0,  1484,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   740,     0,     0,
     505,     0,     0,     0,  1890,   401,     0,     0,   402,     0,
     403,     0,   404,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1218,     0,   405,
    1220,     0,  1221,  -249,  -249,  1222,  1223,  1224,  1225,  1226,
    1227,  1228,  1229,  1230,  1231,  1232,  1233,  -343,  -343,  1234,
    1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,  1925,   406,
     407,  1927,   508,   409,  1242,  1243,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,  1244,   412,   413,
     414,     0,   415,   416,  2197,     0,     0,  1941,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1484,   208,     0,     0,     0,     0,     0,     0,     0,
    -249,  1245,     0,     0,    78,   418,     0,     0,     0,   302,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,   401,     0,     0,   402,  -189,   403,     0,   404,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1218,     0,   405,  1220,     0,  1221,
    -250,  -250,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    1230,  1231,  1232,  1233,  -343,  -343,  1234,  1235,  1236,  1237,
    1238,  1239,  1240,     0,  1241,     0,   406,   407,     0,   508,
     409,  1242,  1243,    65,    66,    67,    68,    69,    70,    71,
      72,   410,   411,   398,  1244,   412,   413,   414,     0,   415,
     416,     0,     0,     0,  1901,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,   208,     0,     0,     0,     0,
       0,  1484,     0,     0,     0,     0,     0,  -250,  1245,     0,
       0,    78,   418,     0,     0,     0,   302,     0,   419,    80,
      81,   420,   421,   422,   423,     0,     0,     0,     0,     0,
       0,     0,   401,  -189,     0,   402,  1006,   403,     0,   404,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1218,     0,   405,  1220,     0,  1221,
       0,     0,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    1230,  1231,  1232,  1233,  -343,  -343,  1234,  1235,  1236,  1237,
    1238,  1239,  1240,     0,  1241,     0,   406,   407,     0,   508,
     409,  1242,  1243,    65,    66,    67,    68,    69,    70,    71,
      72,   410,   411,   398,  1244,   412,   413,   414,     0,   415,
     416,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1245,     0,
       0,    78,   418,     0,     0,     0,   302,     0,   419,    80,
      81,   420,   421,   422,   423,     0,     0,     0,     0,     0,
       0,     0,     0,  -189,     4,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,  1217,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     401,     0,    46,   402,    47,   403,     0,   404,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,  1218,    58,  1219,  1220,     0,  1221,     0,     0,
    1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,  1231,
    1232,  1233,  -343,  -343,  1234,  1235,  1236,  1237,  1238,  1239,
    1240,     0,  1241,     0,   406,   407,    61,   508,   409,  1242,
    1243,    65,    66,    67,    68,    69,    70,    71,    72,   410,
     411,   398,  1244,   412,   413,   414,     0,   415,   416,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -3,  1245,     0,     0,    78,
    1246,     0,     0,     0,   302,     0,   419,    80,    81,   420,
     421,   422,   423,     0,     0,     0,     0,     0,     0,     0,
       0,  -189,     4,   185,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,  1217,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   401,     0,
      46,   402,    47,   403,     0,   404,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
    1218,    58,  1219,  1220,     0,  1221,     0,     0,  1222,  1223,
    1224,  1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,
    -343,  -343,  1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,
    1241,     0,   406,   407,    61,   508,   409,  1242,  1243,    65,
      66,    67,    68,    69,    70,    71,    72,   410,   411,   398,
    1244,   412,   413,   414,     0,   415,   416,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1245,     0,     0,    78,  1246,     0,
       0,     0,   302,     0,   419,    80,    81,   420,   421,   422,
     423,     0,     0,     0,     0,     0,     0,     0,     0,  -189,
       4,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   401,     0,    46,   402,
      47,   403,     0,   404,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
     405,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     406,   407,    61,   408,   409,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   410,   411,   398,     0,   412,
     413,   414,     0,   415,   416,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1707,  1708,  1709,     0,
       0,     0,   417,  1710,  1711,    78,  1246,     0,     0,     0,
       0,     0,   419,    80,    81,   420,   421,   422,   423,     0,
       0,     0,     0,     0,     0,     0,     0,  1712,     4,   185,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   401,     0,    46,   402,    47,   403,
       0,   404,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,   405,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   406,   407,
      61,   408,   409,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   410,   411,   398,     0,   412,   413,   414,
       0,   415,   416,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1707,  1708,  1709,     0,     0,     0,
     417,  1710,     0,    78,  1246,     0,     0,     0,     0,     0,
     419,    80,    81,   420,   421,   422,   423,     0,     0,     0,
       0,     0,     0,     0,     0,  1712,     4,   185,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   401,     0,    46,   402,    47,   403,     0,   404,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   405,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   406,   407,    61,   408,
     409,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   410,   411,   398,     0,   412,   413,   414,     0,   415,
     416,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   417,     0,
    1705,    78,  1246,     0,     0,     0,     0,     0,   419,    80,
      81,   420,   421,   422,   423,     4,   185,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   401,     0,    46,   402,    47,   403,     0,   404,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   405,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   406,   407,    61,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,     0,   415,   416,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   417,     0,     0,
      78,  1246,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,   185,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   401,
       0,    46,   402,    47,   403,     0,   404,   350,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   405,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,     0,   415,   416,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   417,     0,     0,    78,   482,
       0,     0,     0,     0,     0,   419,   483,    81,   420,   421,
     422,   423,   185,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   401,     0,    46,
     402,    47,   403,     0,   404,   350,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   405,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,     0,   415,   416,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   417,     0,     0,    78,  1305,     0,     0,
       0,     0,     0,   419,  1306,    81,   420,   421,   422,   423,
     185,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   401,     0,    46,   402,    47,
     403,     0,   404,   350,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   405,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,     0,   415,   416,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   417,     0,     0,    78,   830,     0,     0,     0,     0,
       0,   419,   483,    81,   420,   421,   422,   423,   185,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   401,     0,    46,   402,    47,   403,     0,
     404,   350,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   405,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,     0,
     415,   416,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   417,
       0,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,   185,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   401,     0,    46,   402,    47,   403,     0,   404,   350,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   405,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,     0,   415,   416,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   417,     0,     0,
      78,   830,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,  2035,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,    -2,     0,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
       0,     0,    -2,     0,     0,    -2,     0,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,    -2,
      -2,  2063,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,
       0,  1316,    -2,     0,     0,     0,     0,    -2,    -2,    14,
      15,    16,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,   401,     0,     0,   402,     0,
     403,     0,   404,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,    -2,     0,    58,   405,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1559,     0,    -2,     0,     0,     0,    -2,    -2,
      14,    15,    16,    17,    18,     0,    -2,    -2,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,     0,   415,   416,     0,     0,   401,     0,     0,   402,
      74,   403,    75,   404,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
     405,   417,     0,     0,    78,   418,     0,     0,     0,     0,
       0,   419,   483,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     406,   407,     0,   408,   409,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   410,   411,   398,     0,   412,
     413,   414,     0,   415,   416,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   417,     0,     0,    78,   418,     0,     0,     0,
       0,     0,   419,  1560,    81,   420,   421,   422,   423,     4,
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
       0,     0,    80,    81,   261,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -489,  -489,
       0,  -489,    46,     0,    47,     0,  -489,     0,     0,     0,
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
     262,     0,     0,     0,  -823,     0,     0,    80,    81,   261,
     185,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -489,  -489,     0,  -489,    46,     0,    47,
       0,  -489,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   150,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,    78,   262,     0,     0,     0,     0,
       0,     0,    80,    81,     4,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,     0,     0,     0,     0,  -410,  -410,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -410,     0,     0,     0,    78,
      79,     0,     0,     0,     0,     0,     0,    80,    81,     4,
     185,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,     0,
       0,     0,     0,  -411,  -411,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -411,     0,     0,     0,    78,    79,     0,  1460,     0,  1461,
       0,     0,    80,    81,  1462,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1463,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1662,     0,
       0,     0,    78,  1011,     0,  1460,     0,  1461,     0,     0,
      80,    81,  1462,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1463,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1663,     0,     0,     0,
      78,  1011,     0,  1460,     0,  1461,     0,     0,    80,    81,
    1462,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1463,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1664,     0,     0,     0,    78,  1011,
       0,     0,     0,     0,     0,     0,    80,    81,   261,   185,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -489,  -489,     0,  -489,    46,     0,    47,     0,
    -489,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   261,     0,     0,     0,
       0,     0,     0,    78,   262,     0,    14,    15,    16,    17,
      18,    80,    81,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -489,  -489,     0,  -489,    46,     0,    47,     0,  -489,     0,
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
       0,    78,   331,     0,     0,     0,     0,     0,     0,    80,
      81,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   350,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,   607,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1107,    77,  -686,    78,   664,     0,     0,     0,
       0,     0,     0,    80,    81,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -489,  -489,
       0,  -489,    46,     0,    47,     0,  -489,     0,     0,     0,
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
     262,     0,     0,     0,  -827,     0,     0,    80,    81,   185,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -489,  -489,     0,  -489,    46,     0,    47,     0,
    -489,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   262,     0,     0,     0,     0,     0,
       0,    80,    81,   185,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   350,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,   607,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   663,     0,  -686,    78,   664,     0,
       0,     0,     0,     0,     0,    80,    81,   185,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     350,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
     607,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   799,     0,
    -686,    78,   548,     0,     0,     0,     0,     0,     0,    80,
      81,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   350,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,  1140,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -694,    78,   917,     0,     0,     0,
       0,     0,     0,    80,    81,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   350,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   351,    78,
     352,     0,     0,     0,     0,     0,     0,    80,    81,   185,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   350,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,  1631,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   917,     0,     0,     0,     0,     0,
       0,    80,    81,   185,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   350,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,  1633,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   917,     0,
       0,     0,     0,     0,     0,    80,    81,   185,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     350,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   331,     0,     0,     0,     0,     0,     0,    80,
      81,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   350,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   917,     0,     0,     0,
       0,     0,     0,    80,    81,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   350,    49,
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
     352,     0,     0,     0,     0,     0,     0,    80,    81,   185,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -489,  -489,     0,  -489,    46,     0,    47,     0,
    -489,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
    1484,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   401,     0,     0,   402,     0,   403,     0,   404,     0,
       0,     0,     0,    78,   262,     0,     0,     0,     0,     0,
       0,    80,    81,  1218,     0,   405,  1220,     0,  1221,  1958,
    1959,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,
    1231,  1232,  1233,     0,     0,  1234,  1235,  1236,  1237,  1238,
    1239,  1240,     0,  1241,     0,   406,   407,     0,   508,   409,
    1242,  1243,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,  1244,   412,   413,   414,     0,   415,   416,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,  1484,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1245,     0,     0,
      78,   418,     0,     0,     0,   302,     0,   419,    80,    81,
     420,   421,   422,   423,   401,     0,     0,   402,     0,   403,
       0,   404,  -189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1218,     0,   405,  1220,
       0,  1221,     0,     0,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  1229,  1230,  1231,  1232,  1233,     0,     0,  1234,  1235,
    1236,  1237,  1238,  1239,  1240,     0,  1241,     0,   406,   407,
       0,   508,   409,  1242,  1243,    65,    66,    67,    68,    69,
      70,    71,    72,   410,   411,   398,  1244,   412,   413,   414,
       0,   415,   416,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1245,     0,     0,    78,   418,     0,     0,     0,   302,     0,
     419,    80,    81,   420,   421,   422,   423,     0,     0,     0,
       0,     0,     0,     0,     0,  -189,   306,   185,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -414,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,     0,     0,  -414,   306,   185,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -415,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,     0,     0,  -415,   306,   185,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,    14,
      15,    16,    17,    18,    19,   727,    20,   728,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    63,    64,   401,     0,    46,   402,    47,
     403,     0,   404,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   405,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   729,     0,     0,     0,     0,  1233,     0,  -343,     0,
       0,     0,     0,    78,     0,     0,     0,     0,  -414,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,     0,   415,   416,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1245,     0,     0,    78,   730,     0,     0,     0,   302,
       0,   419,    80,    81,   731,   732,   422,   423,    14,    15,
      16,    17,    18,    19,   727,    20,   728,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   401,     0,    46,   402,    47,   403,
       0,   404,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   405,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     729,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   406,   407,
       0,   408,   409,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   410,   411,   398,     0,   412,   413,   414,
       0,   415,   416,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     417,     0,     0,    78,   730,     0,     0,     0,   302,     0,
     419,    80,    81,   731,   732,   422,   423,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   401,     0,    46,   402,    47,   403,     0,
     404,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   405,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,     0,
     415,   416,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   417,
       0,   448,    78,   449,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   401,     0,    46,   402,    47,   403,     0,   404,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   405,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   406,   407,     0,   408,
     409,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   410,   411,   398,     0,   412,   413,   414,     0,   415,
     416,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   417,     0,
       0,    78,   449,     0,     0,     0,   302,     0,   419,    80,
      81,   420,   421,   422,   423,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   401,     0,    46,   402,    47,   403,     0,   404,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   405,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,     0,   415,   416,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   417,     0,     0,
      78,   730,     0,     0,     0,   302,     0,   419,    80,    81,
     420,   421,   422,   423,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     401,     0,    46,   402,    47,   403,     0,   404,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   405,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   406,   407,     0,   408,   409,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   410,
     411,   398,     0,   412,   413,   414,     0,   415,   416,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   417,     0,     0,    78,
     449,     0,     0,     0,     0,     0,   419,    80,    81,   420,
     421,   422,   423,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   401,
       0,    46,   402,    47,   403,     0,   404,   350,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   405,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,     0,   415,   416,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   417,     0,     0,    78,   830,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   401,     0,
      46,   402,    47,   403,     0,   404,   350,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   405,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   406,   407,     0,   408,   409,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   410,   411,   398,
       0,   412,   413,   414,     0,   415,   416,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   417,     0,     0,    78,   418,     0,
       0,     0,     0,     0,   419,    80,    81,   420,   421,   422,
     423,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   718,     0,   719,   720,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,   261,   185,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,   -17,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -489,  -489,     0,  -489,    46,
       0,    47,     0,  -489,     0,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,   350,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   607,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -686,    78,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,    79,     0,     0,     0,
    -825,     0,     0,    80,    81,    14,    15,    16,    17,    18,
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
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,   207,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   350,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
     476,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   853,     0,     0,
      78,   477,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
     476,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   477,     0,     0,     0,     0,     0,     0,    80,    81,
     185,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   350,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   607,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,  -686,    78,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -489,  -489,     0,  -489,    46,     0,    47,     0,
    -489,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   331,     0,     0,     0,     0,     0,
       0,    80,    81,   185,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   350,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,  1211,     0,     0,     0,
     185,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,    78,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   350,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   853,     0,     0,    78,   477,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   350,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   853,
       0,     0,    78,   477,     0,    63,    64,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1018,    78,  1011,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
    1579,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,  1011,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   314,     0,    63,    64,
       0,     0,     0,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   207,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   350,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,   350,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   352,     0,    63,    64,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   314,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     350,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   477,     0,     0,     0,     0,     0,     0,    80,
      81,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -489,  -489,     0,  -489,    46,     0,
      47,     0,  -489,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,   350,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   331,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,  1011,     0,    63,    64,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,    79,     0,     0,
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
       0,    63,    64,   350,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   477,     0,    63,    64,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,  1011,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   350,    49,
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
       0,     0,    14,    15,    16,    17,    18,    80,    81,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -489,  -489,     0,  -489,
      46,     0,    47,     0,  -489,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   331,     0,
      14,    15,    16,    17,    18,    80,    81,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -489,  -489,     0,  -489,    46,     0,
      47,     0,  -489,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,     0,     0,     0,     0,
       0,     0,     0,    80,    81,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   401,     0,    46,   402,    47,   403,
       0,   404,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   405,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   406,   407,
       0,   408,   409,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   410,   411,   398,     0,   412,   413,   414,
       0,   415,   416,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     417,     0,     0,    78,   418,     0,     0,     0,     0,     0,
     419,   483,    81,   420,   421,   422,   423,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   401,     0,    46,   402,
      47,   403,     0,   404,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     405,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     406,   407,     0,   408,   409,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   410,   411,   398,     0,   412,
     413,   414,     0,   415,   416,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   417,     0,     0,    78,   418,     0,     0,     0,
       0,     0,   419,    80,    81,   420,   421,   422,   423,    14,
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
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,    78,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,   350,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    78,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -489,  -489,     0,  -489,    46,     0,    47,     0,  -489,     0,
     185,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   186,     0,   187,   188,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,    75,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   718,     0,   719,
     720,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,    20,    75,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -488,  -488,     0,  -488,    46,     0,    47,
       0,  -488,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -489,  -489,     0,  -489,    46,     0,
      47,   401,  -489,     0,   402,     0,   403,     0,   404,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   508,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,    75,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,    77,     0,
     509,   510,     0,     0,     0,   511,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,  1356,     0,    78,   418,     0,     0,
       0,  1357,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,   401,
     415,   416,   402,     0,   403,     0,   404,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   417,
       0,     0,    78,   418,     0,     0,     0,   511,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,   401,   415,   416,   402,     0,
     403,     0,   404,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
       0,     0,     0,     0,     0,   417,   869,     0,    78,   418,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,   401,   415,   416,   402,     0,   403,     0,   404,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,   417,  1002,     0,    78,   418,     0,     0,     0,     0,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,     0,     0,
      78,   418,     0,     0,     0,   302,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,     0,     0,    78,   418,     0,     0,
    1075,     0,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,   401,
     415,   416,   402,     0,   403,     0,   404,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   417,
    1425,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,   401,   415,   416,   402,     0,
     403,     0,   404,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
       0,     0,     0,     0,     0,   417,     0,     0,    78,   418,
       0,     0,     0,  1494,     0,   419,    80,    81,   420,   421,
     422,   423,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,   401,   415,   416,   402,     0,   403,     0,   404,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,   417,     0,     0,    78,   418,     0,     0,     0,  1569,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,     0,  1964,
      78,   418,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,  1969,     0,    78,   418,     0,     0,
       0,     0,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,   401,
     415,   416,   402,     0,   403,     0,   404,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   417,
    1978,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,   401,   415,   416,   402,     0,
     403,     0,   404,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
       0,     0,     0,     0,     0,   417,  2057,     0,    78,   418,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,   401,   415,   416,   402,     0,   403,     0,   404,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,   417,  2059,     0,    78,   418,     0,     0,     0,     0,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,  2105,     0,
      78,   418,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,  2107,     0,    78,   418,     0,     0,
       0,     0,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,   401,
     415,   416,   402,     0,   403,     0,   404,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   417,
    2109,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,   401,   415,   416,   402,     0,
     403,     0,   404,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
       0,     0,     0,     0,     0,   417,  2112,     0,    78,   418,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,   401,   415,   416,   402,     0,   403,     0,   404,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,   417,  2114,     0,    78,   418,     0,     0,     0,     0,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,  2155,     0,
      78,   418,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,  2157,     0,    78,   418,     0,     0,
       0,     0,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,   401,
     415,   416,   402,     0,   403,     0,   404,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   417,
    2159,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,   401,   415,   416,   402,     0,
     403,     0,   404,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
       0,     0,     0,     0,     0,   417,  2181,     0,    78,   418,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,   401,   415,   416,   402,     0,   403,     0,   404,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,   417,  2183,     0,    78,   418,     0,     0,     0,     0,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,  2185,     0,
      78,   418,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,     0,     0,    78,   418,     0,     0,
       0,     0,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,   401,
     415,   416,   402,     0,   403,     0,   404,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   709,
       0,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,   401,   415,   416,   402,     0,
     403,     0,   404,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
       0,     0,     0,     0,     0,   715,     0,     0,    78,   418,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,   401,   415,   416,   402,     0,   403,     0,   404,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,   724,     0,     0,    78,   418,     0,     0,     0,     0,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,     0,     0,
      78,   418,     0,     0,     0,     0,     0,   419,   941,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,     0,     0,    78,   418,     0,     0,
       0,     0,     0,   419,   483,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,  2052,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,     0,
     415,   416,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   417,
       0,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423
};

static const yytype_int16 yycheck[] =
{
       1,    76,    76,     4,     1,   179,   417,     4,   382,     1,
     229,    76,   260,   222,   353,   954,    76,   234,    85,  1263,
     234,   511,   741,     1,   206,   183,   234,   143,  1245,   168,
     369,   168,   676,   663,   373,    98,   255,   237,     4,   239,
     275,   951,   753,    59,   926,   756,   246,  1297,  1298,   355,
    1201,   244,     1,     1,   832,    56,    57,   663,    59,  1827,
     800,   938,    59,    78,   234,   947,     1,    59,  1827,   667,
    1962,   663,   184,  1827,   834,    76,  1245,   419,    98,   272,
      98,    59,     1,     1,    85,   152,  1228,   234,   734,   205,
     283,     1,    93,   193,     4,    75,  1717,    98,   206,   695,
     101,    72,  1958,    89,   105,   100,   323,   135,   105,   323,
     834,    59,   568,   569,    72,   323,    76,   120,   318,   319,
     195,   195,   938,   101,    59,   832,  1050,   234,   697,   832,
     195,   234,   832,   149,    90,    75,   835,    75,  2096,   105,
      59,    75,   841,   144,    89,   484,   147,   169,   149,    59,
     178,    91,   149,   323,   155,  1079,   159,   149,    92,   234,
     234,   162,   234,     1,  2122,   136,    85,  1713,   169,   234,
     110,   149,   235,   161,   234,   161,   323,   163,   136,    98,
     181,   234,   101,   171,   490,   180,   105,     0,   159,  2147,
     234,   101,   193,   194,   195,   105,   263,   155,     1,   155,
     180,   149,   832,     0,   205,   669,   405,   369,   847,   210,
     849,   373,   161,   162,   149,   235,   323,   235,   163,   220,
     323,    59,     1,  1147,   225,   137,   832,   228,   229,   230,
     149,   430,   431,   234,   235,   195,   834,  2093,   125,   149,
     832,   206,   180,    72,   162,   701,   168,   310,   323,   323,
    1871,   323,   451,  2145,   255,    76,    59,   713,   323,   171,
      76,   324,   263,   323,   860,   332,   155,   105,   157,   163,
     323,   158,   273,   274,   234,   194,   277,   135,   580,   323,
      59,   101,   297,   284,  1365,   484,   180,  2179,    10,   591,
     310,   210,   310,   393,    84,     0,  1828,   298,   299,   277,
     301,   139,  1210,   609,   324,   306,   324,   136,   517,   310,
       4,   149,   529,   652,   139,   529,   235,   552,    61,    62,
    1202,   529,   323,   324,   698,   560,   161,  1038,   163,  1107,
    1876,  1877,   333,   311,   636,   155,   139,   676,   112,  1210,
     341,   342,  1219,   754,   263,   346,   149,   566,  1088,   655,
     689,   807,  1521,   572,  1114,  1524,  1525,  2125,   277,   529,
     139,   135,    56,    57,   480,   155,  2125,   277,   226,   711,
     149,  2125,   836,   679,   195,  1252,   840,   355,   976,   195,
     686,  1027,   529,   384,   451,   501,   387,   851,   852,   958,
    1114,   310,   393,   575,   347,    77,    78,   590,   994,    93,
    1107,   108,   109,  1219,  1107,   324,   244,  1107,   137,  1108,
     120,   155,   629,   234,  1946,   629,   155,   112,   234,   244,
     163,   629,   529,   111,  1575,  1198,   529,   996,  1974,  1975,
    1210,   353,  1205,   386,   272,   179,  1252,   180,    77,    78,
     135,   244,   171,   165,   155,   283,   134,   272,   170,   307,
     144,   158,   152,   147,   529,   529,   157,   529,   283,   629,
     161,   462,   157,   163,   529,   244,   488,   575,   162,   272,
     308,   653,    10,    72,   312,   169,   529,  1107,   541,   179,
     283,   163,   629,   308,   149,   529,   487,   488,   827,   157,
     652,   159,   155,   272,   509,   161,   633,   417,   499,   500,
     163,  1107,   243,    20,   283,   308,   171,   170,   509,   250,
     511,   205,   490,  1286,   180,  1107,  1114,  1598,  1599,  1600,
       1,   541,   629,   541,   163,   633,   629,   689,   529,   308,
     271,   225,   154,  1750,   228,  2067,   230,   136,   161,   161,
     541,   282,   158,   462,   155,   653,   155,   161,   171,  1457,
    1458,  1459,   163,   161,   629,   629,   155,   629,   180,   417,
     159,   760,   484,   179,   629,   566,   180,   137,   631,   529,
    1712,   572,   180,   574,   161,  1717,   629,    72,    59,   273,
     274,  1750,    85,   163,   184,   629,  1457,  1458,  1459,   161,
     284,   155,  1082,   180,  2126,    98,   166,   167,   101,   161,
     180,   783,   105,   155,   298,   299,   161,   301,   180,    72,
     575,   631,   306,   631,  1313,   159,   154,   160,   180,   635,
     164,   923,   541,   781,   963,   180,   155,   165,   629,   651,
     631,   609,   170,  2165,   635,    72,   637,   161,   635,   333,
     155,   136,   161,   635,   622,   646,   828,   341,   342,   650,
     651,   161,   346,   862,  1904,  1374,   180,   635,   139,   965,
     155,   180,   161,  1307,   159,   827,  1482,  1131,   149,  1593,
    1486,  1487,   171,   136,    75,   783,   161,  1457,  1458,  1459,
     899,  1454,   683,   161,  1500,   161,   171,   635,   653,    90,
     384,   194,   155,   387,   284,   696,   159,   555,  1397,   136,
     635,   679,   180,  1299,   705,   563,   180,   210,   529,   299,
     849,   633,   631,   529,   384,   161,   635,   387,   155,   163,
     828,   161,   159,   581,    58,   635,   170,    61,    62,  1871,
      64,  1300,   235,   845,   592,  1331,   149,   150,   151,   847,
     180,   163,   900,   154,   161,   746,   180,   748,   170,   750,
     161,  1320,   590,   754,   676,     3,   757,   161,   171,  2003,
     263,  1330,  1672,   244,  1475,   590,    69,   782,   157,  1408,
     155,    72,  1411,  1412,   277,     3,   180,   162,  1497,   168,
     169,   782,    72,   130,   131,  1381,   161,   590,   161,   709,
     158,   272,   712,   487,   159,   715,   171,   635,   171,   164,
     161,   963,   283,   156,   724,   499,   500,   727,   728,   729,
     163,   590,   834,   155,    72,   556,   179,    72,   783,  1018,
    1759,   155,   149,   150,   151,  1024,   827,   308,   175,   176,
    1972,   832,   635,   834,  1245,   136,  1035,   149,   150,   151,
    1296,   582,  1984,   157,   171,   846,   136,   161,   589,   161,
     653,   709,   593,   180,   155,   856,   635,   715,   159,   171,
     155,   862,   177,   828,   865,   155,   724,  1357,   180,   159,
     157,  1590,   854,   855,   163,   162,   154,  1801,   136,  1803,
     574,   136,   847,   161,   849,   743,  1023,   163,   853,   854,
     855,   873,    13,    14,    15,    16,    17,   155,   899,   155,
     155,   159,    47,    48,   159,    50,  1022,   120,   873,  2051,
      55,  1146,     3,   166,   155,  1023,   157,   157,   159,    62,
     173,   174,    13,    14,    15,    16,    17,  1301,  1302,    13,
      14,    15,    16,    17,    18,   936,   937,   938,   155,   157,
     157,   938,   159,   637,   162,  1154,   938,    72,    72,   157,
    1546,    72,   646,   950,    72,   157,   650,   651,   101,   462,
     938,   943,   157,    72,   157,   987,   157,  1563,  1307,   934,
     113,   162,   115,   157,   117,   157,  1572,   157,   943,  1577,
     162,    72,   162,  1183,   950,   160,   987,   965,    58,   683,
     938,    61,    62,  1589,    64,  1564,     3,   113,   114,   115,
     116,   117,   696,   938,  1494,  1311,    13,    14,    15,    16,
      17,   136,   136,   156,  1665,   136,   159,   160,   136,  1670,
    1219,   155,   158,   159,   155,   828,   162,   136,   938,   161,
     155,   155,    72,    72,   159,   159,   155,   155,   541,   161,
     950,   159,    22,  1044,   155,   136,   155,   101,   906,  1050,
     159,  1462,   746,   155,   748,   157,   750,   159,  1874,   155,
     754,   155,   920,   757,   163,    72,   924,   210,  1316,    89,
     928,  1290,    13,    14,    15,    16,    17,   154,  1079,  1569,
      72,  1082,    13,    14,    15,    16,    17,    72,   782,    72,
     157,  1113,  1114,   180,   161,   157,   136,   136,  1588,   161,
     938,  1023,    72,   155,   160,   157,  1107,   159,  1307,   590,
     157,   155,  1113,  1114,   161,   155,   155,   108,   109,   159,
     159,  1103,  1104,  1297,  1298,   163,  1722,   163,   155,   136,
     160,    72,   159,   827,   277,   938,   279,   280,  1103,  1104,
     157,    72,   163,  1210,   136,  1284,  1147,  1284,  1347,  1348,
    1349,   136,   846,   136,   635,  1354,  1355,   157,   170,   938,
     157,   161,   856,   155,   161,   155,   136,   159,   311,   159,
     155,   157,   155,   316,   159,   161,   159,  1159,   157,   322,
     108,   109,   161,  1779,   925,   155,   157,   155,  1784,   159,
     161,   159,  1566,   155,  1159,   136,   137,  1766,  1767,  1795,
     157,  1338,  1339,   179,  1201,   136,   137,   134,   120,  1444,
     157,   155,   355,   155,   161,  1327,   155,   360,   155,   362,
    2036,  1222,  1219,   161,  1225,  1226,  1227,  1219,   155,   134,
     155,  1339,   159,  1234,   159,  1201,   761,   762,   763,   166,
     167,  1219,   936,   937,   938,   845,   157,   158,  2064,   157,
     155,  1252,   179,   161,   159,  1252,   172,  1258,   167,   859,
    1252,   166,   167,   157,   155,   408,  2130,   161,   159,  1408,
    2134,  1219,  1273,  1412,  1252,  1276,  1277,   165,   157,  1280,
    1277,  2097,   161,   157,  1219,   177,  1287,   161,  1999,  1290,
     157,  1201,   157,   987,   161,   134,   161,   134,  1276,   768,
     769,   770,   771,  1411,  1252,  1417,  1418,   134,   157,  1219,
     158,  1277,   161,  1909,  1910,   157,   155,  1252,   155,   462,
     159,   157,   159,   157,  1325,  1245,   157,   166,   167,   166,
     167,   157,   157,  1311,   157,   161,   161,   157,   161,  1340,
      78,   161,  1252,   157,   157,   157,  1204,   490,   161,   492,
     493,  1920,  1662,  1663,  1664,   157,  1357,  1276,  1277,   161,
    1218,   159,  1284,   155,  1365,   508,  1276,  1277,   106,   155,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
    1238,  1219,   149,   150,   151,  1307,   160,  1245,   137,   157,
    1457,  1458,  1459,   161,   161,  1462,  1463,  1398,   541,   157,
    1207,  1208,  1209,   161,   171,  1468,  1780,   157,  1683,  1684,
    1685,   161,  1549,   180,  1252,   137,  1219,  1339,   157,  2015,
     161,   157,   161,   566,  1344,   161,   161,   106,   571,   162,
     573,   110,   111,   112,   113,   114,   115,   116,   117,  1277,
    1219,  1549,   180,  1408,   162,  2014,  1411,  1412,  1468,  1252,
    1468,   161,   595,   155,   597,   598,    18,   938,   179,  1676,
     157,  1482,  1676,  1460,   157,  1486,   609,  1468,  1676,   168,
     169,  1472,  1473,  1252,   157,  1075,   157,   156,   155,   622,
     159,   128,   129,   132,   133,  1482,   157,  2083,   631,  1486,
    1487,   160,   161,  1494,  1460,    57,    58,    59,    60,    61,
      62,    63,    64,  1500,  1482,   157,  1676,   179,  1486,  1487,
     160,   161,   655,   157,   657,   658,  1517,  1518,   160,   161,
     166,   167,  1500,   160,   161,   157,  1527,   157,  1222,  1676,
     159,  1225,  1226,  1227,  1482,   160,   679,   680,  1486,  1487,
    1234,   160,   161,   686,   160,   161,  1745,   161,   162,  1527,
    1460,   163,  1500,   160,   161,   163,  1156,   163,  1252,   160,
     161,    91,    92,   163,  1258,   160,   161,   163,  1569,  1676,
     160,   161,  1482,  1676,   160,   161,  1486,  1487,  1575,  1273,
     160,   161,  1740,   160,   161,   161,  1280,  1588,   160,   161,
    1500,    70,  1593,  1287,   160,   161,   157,  1598,  1599,  1600,
     160,   161,  1665,   160,  1676,   160,   161,  1670,  1527,  1575,
     160,   161,   161,   162,   155,  1678,  1676,  1527,  1476,  1477,
     160,   161,   180,  1676,    78,  1842,   160,  1549,  1842,   160,
     161,  1325,  1676,    18,  1842,     4,     5,     6,     7,     8,
       9,    10,    11,    12,  1482,  1665,  1340,  1665,  1486,  1487,
    1670,   179,  1670,  1992,    77,    78,   161,  1482,  1678,   163,
    1678,  1486,  1500,   157,  1665,  1575,   161,   162,  1526,  1670,
    1366,  1367,  1842,   766,   767,  1676,   157,  1678,   180,  1482,
     163,   764,   765,  1486,  1487,  1686,   772,   773,  1907,    13,
      14,    15,    16,    17,    18,  1842,    65,  1500,  1524,  1525,
    1684,  1685,   180,  1482,  1705,  1702,   163,  1486,  1487,   160,
    1711,   160,    13,    13,    14,    15,    16,    17,  1567,  1568,
      18,  1500,   160,  1701,   160,   154,  2065,  1327,    22,   157,
    1904,   157,   157,  1891,    70,  1842,  1702,   157,  1219,  1842,
      13,    14,    15,    16,    17,    18,   157,    13,    14,    15,
      16,    17,   157,   157,  1755,    13,    14,    15,    16,    17,
      18,   157,  2068,   157,  1761,   157,   157,   157,   154,   157,
    1842,  1252,    72,  1276,  1277,   154,   160,  1377,  1472,  1473,
     163,   163,  1845,   163,   301,   161,   245,    88,   163,  1842,
     179,  1701,  1702,   157,   157,  1761,   157,   157,  1842,  1998,
    1801,   154,  1803,   157,   161,   106,    72,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,  1417,  1418,   163,
     179,   163,   965,  1517,  1518,  1845,   157,  1845,   157,   161,
     160,   157,   157,   976,   134,   157,   136,   157,  1996,   157,
     161,  1842,   985,   157,  1845,   157,  1958,   157,  2067,   157,
     157,  1761,   157,  1854,  1855,   155,   160,   160,   157,   159,
    1861,   157,   157,   160,  1702,   157,   166,   167,   134,   157,
     136,   157,  1873,  1793,  1732,   157,   157,  1874,    62,  1857,
     157,   161,  1883,   123,  1885,   125,   126,   127,   157,   155,
     161,   161,  1955,   159,   154,  1896,  1874,  1898,  1899,  1900,
     166,   167,   157,   154,   161,  1906,  1907,   155,  2125,   155,
     155,  2125,   155,   155,   155,   155,   155,  2125,   158,   159,
     155,    14,   106,   163,   164,  1992,  1874,   111,   162,   161,
     114,   180,   116,   162,   160,  1955,   160,  1955,   163,   154,
     154,   163,   180,   180,   161,  1088,   157,  1857,   157,   180,
     157,   157,   157,   161,  1955,  2125,   160,   160,   157,   157,
     160,  1962,   157,   157,  1874,  1966,   157,   157,   160,    76,
    1971,  1114,  2130,   154,   154,    80,  2134,  2135,  2125,  2091,
     155,  2093,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    98,  1686,   155,   180,   454,    92,   180,  2065,   220,
     154,  1482,   180,   180,   180,  1486,  1487,   180,   180,  2167,
     155,  1705,   471,   155,    90,   474,   157,  1711,  2125,  1500,
    2132,   154,  2125,   154,  1527,   161,  2027,   161,   154,   123,
     160,   163,   160,  2191,   163,   160,  1874,  2195,  2039,  2036,
     557,   160,  2043,   154,   157,   162,     1,   162,   155,     4,
    2125,  2125,  2210,  2125,   154,  2056,   157,   160,  2036,   157,
    2125,  1755,   157,  2126,   154,  2125,  2067,  2064,  2069,   160,
     157,  1874,  2125,   157,   180,   534,   157,   157,   157,   154,
     162,  2125,   266,   161,   155,   157,  2064,   155,  2036,   155,
     154,   160,   157,   160,  2206,  1874,   154,   160,   154,  2100,
    2097,   163,  2165,   157,    59,   154,  2126,   157,  2126,   157,
     157,    75,    75,   154,   160,   632,  2064,   155,   157,  2097,
     180,    76,   229,   180,  2125,  2126,  2036,   234,   235,   313,
      85,   155,   160,  1276,   160,   180,   154,  2138,   163,   154,
     154,   154,   159,    98,  2145,  2165,   101,  2165,   255,  2097,
     105,   157,   157,   157,  2064,    75,    75,   158,    75,   171,
    1854,  1855,   171,   162,  2165,   154,   154,  1861,  1311,   180,
     180,   154,  2173,   357,  1317,   359,  2177,   361,  2179,  1873,
     180,   156,   154,   162,   171,   171,   155,  2097,   143,  1883,
     106,  1885,   154,   161,   149,   171,   180,   152,  2036,  2200,
     155,   156,  1896,   310,  1898,  1899,  1900,  2126,   156,   206,
    2211,   171,  1906,   168,   160,   180,   323,   324,   301,  2220,
      75,   154,   157,   157,   408,   156,  2064,   157,    56,    57,
     162,   154,   154,  2036,  1793,   180,   157,   192,   180,   194,
     195,   155,   157,   180,   733,  1335,  2165,   774,   777,   775,
     205,   206,   776,   778,   450,   210,  1240,  2036,  1252,  2097,
    1486,  2064,  2179,  2093,  1882,    93,  2122,  1500,  1962,  1874,
    2080,  2162,  1966,  1733,   229,  1733,  1749,  1971,  2065,   234,
     235,  2135,   799,  2064,  2195,  2064,    49,  1280,   509,   114,
     511,   268,  2025,  1955,  2097,  1463,   991,   862,   815,  1273,
     255,   646,   819,  1534,  1761,   518,     0,   705,   263,   799,
    1654,   799,   799,    -1,    -1,    -1,   144,    -1,  2097,   147,
      -1,    -1,   277,    -1,   508,    -1,  1469,    13,    14,    15,
      16,    17,  2190,  2027,   162,    -1,    -1,    -1,    -1,    -1,
     799,   800,    -1,    -1,  2202,  2039,    -1,    -1,    -1,  2043,
      -1,   810,    -1,    -1,   813,   310,   353,    -1,  1958,   356,
      -1,   316,  2056,    -1,    -1,    -1,    -1,   322,   323,   324,
      -1,    -1,   369,    -1,    -1,  2069,   373,   332,    -1,    -1,
      -1,   488,    -1,    -1,  1527,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,  1874,    -1,    -1,    -1,   225,   353,   354,
     355,    -1,    -1,   587,    -1,    -1,  2100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   369,    -1,    -1,    -1,   373,    -1,
     879,    -1,   529,    -1,    -1,    -1,    -1,   886,    -1,    -1,
      -1,   890,   106,    -1,   541,   894,   110,   111,   112,   113,
     114,   115,   116,   117,  2138,   273,   274,    -1,   134,    -1,
     136,  2145,    -1,    -1,    -1,    -1,   284,    -1,    -1,   566,
      -1,    -1,   417,    -1,    -1,   572,    -1,    -1,    -1,   155,
      -1,   299,    -1,   159,   557,    -1,    -1,    -1,    -1,  2173,
     166,   167,    -1,  2177,    -1,  2179,    -1,   484,    -1,    -1,
      -1,  2091,    13,  2093,    -1,    -1,   451,    -1,    -1,   454,
      -1,    -1,    -1,    -1,    -1,   333,  2200,   462,    -1,    -1,
      -1,    -1,    -1,   341,   342,    -1,    -1,  2211,   346,    -1,
      -1,    -1,   629,    -1,   631,   480,  2220,    -1,    -1,   484,
      -1,    -1,  2132,   488,   106,   490,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   501,    -1,    -1,   632,
      -1,    -1,    -1,    -1,    -1,  2036,   384,    -1,  1701,   387,
      -1,    -1,    -1,  2163,    -1,    -1,    -1,    88,    -1,    -1,
      -1,    -1,    -1,    -1,   529,    -1,    -1,  1094,   575,    -1,
     663,  1098,    -1,  2064,    -1,   106,   541,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
    1117,    -1,    -1,    -1,    -1,    -1,  2206,  1124,   180,    -1,
      -1,   566,    -1,   568,   569,    -1,  2097,   572,    -1,    -1,
     575,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1088,
      -1,    -1,    -1,    -1,   155,    -1,   633,    -1,    -1,    -1,
      -1,   862,    -1,    -1,   865,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1171,   652,   653,    -1,  1175,   487,
      -1,    -1,  1179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   499,   500,    -1,   629,    -1,   631,    -1,   633,   676,
     635,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    62,
      -1,    -1,   689,    -1,    -1,    -1,    -1,   652,   653,    -1,
     655,    -1,  1845,  1162,    -1,    -1,  1165,    -1,   663,    85,
    1169,    -1,   667,    -1,  1857,    -1,   799,    -1,    -1,    -1,
      -1,   676,    -1,    -1,    -1,   832,    -1,   834,   101,    -1,
      -1,   686,   815,    -1,   689,    -1,   819,    -1,    -1,    -1,
     113,   114,    -1,    -1,    -1,    -1,   701,    -1,    -1,   832,
      -1,    -1,    -1,    -1,   709,    72,    -1,   712,   713,    -1,
     715,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   724,
      -1,    -1,   727,   728,   729,    -1,   152,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,    -1,   783,    -1,    -1,   106,
      -1,    -1,   899,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   130,  1946,    -1,    -1,    -1,    -1,    -1,   637,
      -1,    -1,    -1,    -1,    -1,    -1,   192,   134,   646,   136,
      -1,    -1,    -1,  1044,    -1,    -1,    -1,    -1,   783,  1050,
     827,   828,    -1,    -1,    -1,    -1,    -1,   210,   155,   156,
      -1,    -1,    -1,    -1,   799,   800,    -1,    -1,    -1,   166,
     167,   934,   807,    -1,    -1,    -1,    -1,    -1,  1079,    -1,
      -1,  1082,    -1,    -1,    -1,    -1,    -1,    -1,   696,    -1,
      -1,    -1,   827,   828,    -1,    -1,    -1,   832,    -1,   834,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   263,    -1,    -1,
      -1,    -1,   847,    -1,   849,    -1,    -1,    -1,   853,   854,
     855,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
       4,    -1,  1429,    -1,    -1,    -1,  1433,    -1,   873,    -1,
    1437,    -1,    -1,    -1,  1383,  2068,  1147,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1394,    -1,    -1,   311,    -1,
      -1,    -1,    -1,   316,   899,    -1,    -1,    -1,    -1,   322,
      -1,    -1,    -1,   106,    -1,    -1,   332,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   963,    -1,    -1,   966,
      -1,    -1,     1,    -1,    -1,     4,    -1,   353,    -1,    -1,
      -1,    -1,   355,   938,    -1,    -1,    -1,    -1,   943,    -1,
      -1,    85,   320,    -1,    -1,   950,   951,    -1,    -1,    -1,
    1107,    -1,   155,   156,    -1,    -1,  1113,  1114,   963,    -1,
     965,  1094,    -1,    -1,    -1,  1098,    -1,    -1,   846,    -1,
      -1,   976,  2165,    -1,  1107,    -1,  1023,    -1,   856,    -1,
      59,    -1,    -1,    -1,  1117,   408,    -1,  1554,    -1,    -1,
      -1,  1124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
      -1,    -1,    -1,    -1,    -1,   106,    85,   108,   152,   110,
     111,   112,   113,   114,   115,   116,   117,  1022,  1023,    -1,
      -1,    -1,    -1,   106,    -1,   451,   105,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,  1171,    -1,
    1607,    -1,  1175,    -1,    -1,    -1,  1179,    -1,   192,  1616,
      -1,    -1,    -1,  1620,    -1,    -1,    -1,    -1,   484,   937,
     139,   205,    -1,    -1,   143,    -1,    -1,   490,    -1,    -1,
     149,    -1,    -1,   152,    -1,    -1,   159,    -1,    -1,    -1,
      -1,    -1,    -1,  1088,    -1,   508,  1357,    -1,    -1,   168,
      -1,    -1,    -1,    -1,  1365,    -1,    -1,    -1,  1103,  1104,
      -1,    -1,  1107,    -1,    -1,   483,    -1,   485,    -1,  1114,
      -1,    -1,    -1,   192,    -1,   194,   494,   261,    -1,   263,
      -1,    -1,    -1,    -1,    -1,    -1,   205,   206,    -1,    -1,
      -1,    -1,    -1,  1290,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   568,   569,    -1,    -1,    -1,    -1,    -1,    -1,
     573,    -1,    -1,    -1,  1159,    -1,   235,  1666,    -1,    -1,
      -1,    -1,   306,    -1,    -1,   244,    -1,    -1,    -1,    -1,
      -1,    -1,   595,    -1,    -1,    -1,   255,    -1,    -1,    -1,
      -1,   260,   261,    -1,   263,    -1,   609,    -1,   332,    -1,
      -1,    -1,    -1,   272,    -1,    -1,  1201,    -1,    -1,   622,
      -1,    -1,    -1,    -1,   283,  1210,    -1,   286,    -1,    -1,
      -1,   290,    -1,    -1,  1219,    -1,   295,    -1,    -1,    -1,
      -1,    -1,    -1,  1494,    -1,    -1,    -1,   306,    -1,   308,
      -1,    -1,   655,   312,    -1,    -1,    -1,    -1,    -1,    -1,
    1245,  1398,    -1,    -1,    -1,   324,    -1,  1252,    -1,    -1,
     676,    -1,    -1,   332,    -1,    -1,   679,    -1,    -1,    -1,
    1307,    -1,    -1,   686,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1276,  1277,   417,   353,   701,    -1,   356,    -1,  1284,
       3,    -1,    -1,   709,    -1,  1290,    -1,   713,    -1,   715,
     369,  1296,  1339,    -1,   373,    -1,  1429,    -1,  1569,    -1,
    1433,    -1,  1307,    -1,  1437,    -1,    -1,   451,    -1,    -1,
      -1,  1468,    -1,    -1,    -1,    -1,    -1,  1588,  1827,  1828,
      -1,    -1,  1593,    -1,    -1,    -1,    -1,  1598,  1599,  1600,
      -1,    -1,    -1,    -1,  1339,    -1,   480,    -1,   417,  1344,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1225,  1226,  1227,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   501,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,   451,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1258,   807,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,   480,  1280,  1408,    -1,   484,  1411,  1412,    -1,  1287,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1554,   501,    -1,   568,   569,    -1,    -1,    -1,    -1,
     180,    -1,   155,   156,    -1,    -1,    -1,  1946,    -1,   162,
      -1,    -1,    -1,   166,   167,    -1,    -1,  1325,    -1,    -1,
      -1,    -1,  1457,  1458,  1459,  1460,    -1,  1462,  1463,    -1,
      -1,    -1,    -1,  1468,  1469,    -1,    -1,   845,    -1,    -1,
      -1,    -1,    -1,    -1,  1607,    -1,    -1,  1482,    -1,    -1,
     903,  1486,  1487,  1616,    -1,    -1,    -1,  1620,    -1,   568,
     569,    -1,    -1,    -1,    -1,  1500,   575,    -1,    -1,    -1,
      -1,    -1,  1549,    -1,    -1,    -1,    -1,    -1,  1665,    -1,
      -1,   590,  2021,  1670,    -1,    -1,  2025,    -1,  2085,  1676,
      -1,  1678,  1527,    -1,    -1,   951,    -1,    -1,    -1,   106,
    1801,    -1,  1803,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   965,    -1,  1549,   122,    -1,   124,    -1,    -1,
      -1,    -1,    -1,    -1,   633,    -1,   635,   701,  2067,    -1,
      -1,    -1,    -1,   941,   942,   709,    -1,    -1,   712,   713,
    1575,   715,  1577,   652,   653,    -1,    -1,    -1,    -1,   156,
     724,    -1,   159,   727,   728,   729,    -1,    -1,   667,    -1,
      -1,    -1,    -1,    -1,  1472,  1473,    -1,   676,    -1,    -1,
      -1,    -1,   681,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     689,    -1,    -1,    -1,    -1,    -1,  2125,  2126,    -1,    -1,
      -1,    -1,   701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     709,    -1,    -1,   712,   713,    -1,   715,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   724,    18,    -1,   727,   728,
     729,    -1,    -1,    -1,    -1,    -1,  2165,    -1,    -1,  1037,
    1665,  1666,    -1,   807,    -1,  1670,    -1,  1672,    -1,    -1,
      -1,  1676,   106,  1678,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,  1842,    -1,    -1,  1845,    61,
      62,    63,    64,    -1,    -1,    -1,  1701,  1702,    -1,    -1,
      -1,    -1,    -1,  1081,   783,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,   800,    -1,    -1,    -1,    -1,    -1,    -1,   807,    -1,
      -1,    -1,   166,    -1,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   827,   828,
    1907,    -1,  1130,    -1,  1132,   834,  1761,   158,    -1,    -1,
      -1,    -1,   163,    -1,    -1,    -1,  1144,  1145,   847,   170,
     849,    -1,  1150,  1151,   853,   854,   855,    -1,    -1,    -1,
      -1,    -1,  1160,    -1,    -1,    -1,    -1,   159,  1793,    -1,
      -1,    -1,    -1,    -1,   873,    -1,    -1,    -1,  1955,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   950,   951,  1686,  1187,
      -1,    -1,  1190,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1827,  1828,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,  1842,    -1,   106,
    1845,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,  1857,  1276,    -1,    -1,    -1,    -1,    -1,   938,
      -1,    -1,    -1,    -1,   943,    -1,    -1,    -1,    -1,  1874,
    1296,   950,   951,    -1,  1252,    -1,    -1,    -1,  1022,    -1,
      -1,  1307,   163,    -1,   963,    -1,    -1,   966,  1311,   170,
      -1,    -1,    -1,    -1,   973,    -1,   163,    -1,    -1,    -1,
      -1,    -1,  1907,   170,    -1,  1283,    -1,    -1,    -1,    -1,
    2067,    -1,    -1,  1291,    -1,  1293,  1294,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1306,    -1,
    1308,    -1,  1310,    -1,    -1,    -1,    -1,    -1,    -1,  1317,
      -1,  1946,    -1,  1022,  1023,  1992,    -1,    -1,    -1,    -1,
    1955,   106,  2085,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,   106,    -1,  2125,  2126,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   134,
      -1,   136,   122,    -1,   124,    -1,    -1,  1992,    -1,    -1,
       1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,    -1,   159,    -1,    -1,    -1,  2165,    -1,
      -1,   166,   167,  1391,  1392,    -1,   156,    -1,  2065,   159,
    2025,    -1,    -1,    -1,  1103,  1104,    -1,  1450,  1906,    -1,
      -1,  2036,    -1,    -1,    -1,    -1,    -1,    -1,  1416,    -1,
      -1,    -1,    -1,    -1,    -1,  1423,  1469,    -1,    59,  1427,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1201,    -1,  2064,
    2065,    -1,  2067,  2068,    -1,    -1,  1210,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    85,  1453,    -1,   106,    -1,    -1,
    1159,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     101,    -1,  2097,   122,   105,   124,    -1,    -1,    -1,    -1,
      -1,  1245,    -1,    -1,  1527,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
    2125,  2126,  1201,    -1,    -1,    -1,    -1,   156,   139,    -1,
     159,  1210,   143,    -1,    -1,   136,    -1,    -1,   149,    -1,
    1219,   152,    -1,    -1,    -1,   156,    -1,    -1,    -1,    -1,
      -1,    -1,  1296,    -1,   155,   156,   167,   168,   169,    -1,
    2165,    -1,    -1,    -1,    -1,    -1,  1245,    -1,    -1,    -1,
      -1,    -1,    -1,  1252,    -1,  1553,    -1,    -1,    -1,    -1,
      -1,   192,  1560,    -1,  1562,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   205,   206,    -1,    -1,  1277,   210,
    1344,    -1,    -1,    -1,   106,  1284,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   106,  1296,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,  1307,    -1,
      -1,    -1,    -1,   244,    -1,    -1,    -1,  1316,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1672,    -1,    -1,    -1,
     261,    -1,   263,    -1,   156,    -1,    -1,   159,    -1,  1637,
    1339,   272,    -1,    -1,    -1,  1344,   277,    -1,    -1,    -1,
      -1,    -1,   283,   163,    -1,    -1,    -1,    -1,  1701,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   298,    -1,    -1,
     301,    -1,    -1,    -1,    -1,   306,    -1,   308,    -1,    -1,
     311,   312,    -1,    -1,    -1,   316,    -1,    -1,    -1,    -1,
      -1,   322,    -1,  1457,  1458,  1459,  1460,  1461,  1462,  1463,
      -1,   332,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1408,
      -1,    -1,  1411,  1412,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   353,   106,   355,   356,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,   369,   122,
      -1,   124,   373,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1457,  1458,
    1459,  1460,  1461,  1462,  1463,    -1,    -1,    -1,    -1,    -1,
      57,    -1,    -1,   156,    -1,  1773,  1774,    -1,    65,    66,
      67,    68,    -1,  1482,    -1,    -1,   417,  1486,  1487,  1787,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,  1500,    72,    -1,   152,    -1,    -1,    -1,    -1,    -1,
      -1,  1575,    -1,    -1,  1857,    -1,    -1,    -1,    -1,   106,
     451,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   179,   417,    -1,    -1,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   480,
    1549,    -1,    -1,   484,   184,    -1,    -1,    -1,    -1,   490,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
     501,    -1,   159,    -1,    -1,    -1,  1575,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,   159,
     177,    72,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1672,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,   106,   557,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   568,   569,   134,
      -1,   136,   573,    -1,   575,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,   590,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,  1672,   155,   156,    -1,    -1,   609,  1678,
      -1,    -1,    -1,   568,   569,   166,   167,    -1,    -1,    -1,
      -1,   622,    -1,    -1,    -1,    -1,    -1,  1761,    -1,    -1,
      -1,   632,   633,  1702,   635,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,   650,
      -1,   652,   653,    -1,   655,  2068,    -1,    -1,    -1,  1793,
      -1,   106,   663,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,   676,    -1,    -1,   679,    -1,
      -1,    -1,   683,    -1,    -1,   686,    -1,    -1,   689,    -1,
     691,    -1,  1761,    -1,    -1,    -1,    -1,    -1,  2066,    -1,
     701,   401,    72,    -1,    -1,   405,   406,    -1,   709,    -1,
      -1,   712,   713,   158,   715,   415,   416,    -1,    -1,    -1,
      -1,    -1,    -1,   724,  1793,    -1,   727,   728,   729,    -1,
     430,   431,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,   451,    -1,    -1,   709,  2123,    -1,    -1,    -1,  1828,
     715,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,   724,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2146,    -1,
      -1,    -1,   783,    -1,   484,   155,   156,    -1,   743,   159,
      -1,    -1,    -1,  2161,    -1,    -1,   166,   167,   799,    -1,
      -1,    -1,    -1,    -1,    -1,  1874,   807,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   815,    -1,    -1,    -1,   819,    -1,
      -1,    -1,    -1,    -1,   779,    -1,   827,   828,    -1,    -1,
     106,   832,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,     1,    -1,    -1,    -1,   847,    -1,   849,    -1,
      -1,    -1,   853,   854,   855,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,   873,    20,    72,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      59,    -1,   903,    -1,    51,    -1,    53,    -1,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,  1992,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   934,    -1,    -1,   134,   938,   136,    -1,
      -1,    -1,   943,    -1,    -1,    -1,   105,    -1,    -1,   950,
     951,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,   963,    -1,   965,   966,    -1,  2036,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
     149,    -1,    -1,    -1,    -1,  2064,  2065,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1022,  1023,    -1,    -1,    -1,    -1,    -1,  2097,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   206,    -1,    -1,
     159,    -1,    -1,    -1,    -1,    -1,    -1,  2126,    -1,    -1,
     760,   761,   762,   763,   764,   765,   766,   767,   768,   769,
     770,   771,   772,   773,   774,   775,   776,   777,   778,    -1,
      -1,    -1,    -1,    -1,    -1,   244,    -1,    -1,    -1,    -1,
      -1,    -1,    72,  1094,    -1,    -1,    -1,  1098,    -1,    -1,
      -1,    -1,  1103,  1104,    -1,    -1,  1107,    -1,    -1,    -1,
      -1,    -1,    -1,   272,    -1,    -1,  1117,    -1,    -1,    -1,
      -1,    -1,    -1,  1124,   283,    -1,   106,   286,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,   301,    -1,    -1,   845,    -1,    -1,    -1,   308,
      -1,    -1,    -1,   312,   134,    -1,   136,    -1,  1159,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
    1171,    -1,    -1,    -1,  1175,   155,   156,    -1,  1179,    -1,
      -1,    -1,    -1,    -1,     1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,   353,    -1,    -1,   356,    -1,    -1,
    1201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1210,
     369,    -1,    -1,    -1,   373,    -1,    -1,    -1,  1219,   106,
      72,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,  1245,    -1,    -1,   134,    -1,    -1,
      -1,  1252,    -1,    -1,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,   155,   156,
      13,    14,    15,    16,    17,  1276,  1277,    -1,    -1,   166,
     167,    -1,   134,  1284,   136,    -1,    -1,    -1,   105,    -1,
    1245,    -1,    -1,    -1,    -1,  1296,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,  1307,    -1,    -1,    -1,
    1311,    -1,    -1,    -1,   166,   167,    -1,    -1,  1018,    -1,
      -1,    -1,   139,  1324,  1024,   484,    -1,    -1,    -1,    72,
      -1,    -1,   149,    -1,    -1,  1035,    -1,  1338,  1339,    -1,
      -1,    -1,    -1,  1344,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   168,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,  1075,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   206,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,   557,    -1,
      -1,    -1,   158,    -1,    -1,    -1,    -1,  1408,    -1,    -1,
    1411,  1412,   155,   156,    -1,    -1,   575,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,   155,    -1,   244,  1429,    -1,
      -1,   590,  1433,    -1,    -1,    -1,  1437,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1450,
      -1,    -1,    -1,    -1,    -1,   272,  1457,  1458,  1459,  1460,
    1461,  1462,  1463,    -1,    -1,    -1,   283,    -1,  1469,    -1,
      -1,    -1,    -1,   632,   633,    -1,   635,    -1,    -1,    -1,
      -1,  1482,    -1,    -1,   301,  1486,  1487,    -1,    -1,    -1,
      -1,   308,    -1,   652,   653,   312,    -1,    -1,    -1,  1500,
      -1,    -1,    -1,    -1,   663,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   676,    -1,  1219,
      -1,    -1,    -1,    -1,    -1,    -1,  1527,  1482,  1483,    -1,
     689,  1486,  1487,   134,    -1,    -1,   353,  1492,    -1,   356,
      -1,  1496,    -1,  1498,    -1,  1500,    -1,    -1,  1549,    -1,
      -1,    -1,   369,  1554,   155,   156,   373,    -1,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1575,    -1,    -1,    -1,   179,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,  1607,  1307,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1616,    -1,    -1,    -1,  1620,
      -1,    -1,    -1,    -1,   783,    -1,    -1,  1327,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     799,    72,   158,   159,    -1,    -1,    -1,  1347,  1348,  1349,
      -1,    -1,    -1,    -1,  1354,  1355,   815,    -1,    -1,    -1,
     819,    -1,    -1,    -1,    -1,    -1,    -1,   484,   827,   828,
      -1,  1672,    -1,   832,    -1,   106,    -1,  1377,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   847,    -1,
     849,    -1,    -1,    -1,   853,   854,   855,  1652,    -1,    -1,
    1701,  1702,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   873,    -1,    -1,  1417,  1418,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
     557,   106,  1697,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,   575,    -1,
    1761,  1716,  1717,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   590,    -1,   934,    -1,    -1,    -1,   938,
      -1,    -1,    -1,    -1,   943,    -1,    -1,    -1,    -1,    -1,
     155,  1746,  1793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   963,    -1,    -1,   966,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   632,   633,   106,   635,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
       1,    -1,    -1,     4,    -1,   652,   653,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,   663,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1857,    -1,    -1,   676,
      -1,    -1,    -1,    -1,  1023,    -1,   155,   156,    -1,    -1,
     159,    -1,   689,  1874,    -1,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,  1859,    -1,    -1,    -1,    -1,    -1,
      -1,  1866,    -1,  1868,    85,    -1,  1871,  1872,    -1,  1874,
     134,    -1,    -1,    -1,  1879,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,  1094,    -1,    -1,    -1,  1098,
      -1,   155,   156,   114,  1103,  1104,    -1,    -1,  1107,    -1,
      -1,    -1,   166,   167,    -1,    -1,    -1,    -1,  1117,    -1,
      -1,    -1,    -1,    -1,  1222,  1124,   783,    -1,   139,    -1,
      -1,    -1,   143,    -1,    -1,    -1,  1234,    -1,   149,    -1,
      -1,   152,   799,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1992,    -1,    -1,    -1,    -1,    -1,    -1,   815,    -1,
    1159,    -1,   819,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     827,   828,  1171,    -1,    -1,   832,  1175,    -1,    -1,    -1,
    1179,   192,  1977,    -1,    -1,    -1,    -1,  1982,  1983,    -1,
     847,    -1,   849,    -1,   205,  2036,   853,   854,   855,    -1,
      -1,    -1,    -1,    -1,    -1,  1745,    -1,  2002,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   873,    -1,    -1,    -1,
    1219,    -1,    -1,  2064,  2065,    -1,    -1,  2068,    -1,    -1,
      -1,    -1,    -1,   244,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2085,    -1,    -1,    -1,    -1,  2044,
     261,  2046,   263,  1252,  2049,  2050,  2097,   268,    -1,  2054,
    2055,   272,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   283,    -1,    -1,    -1,    -1,   934,  1277,    -1,
      -1,   938,    -1,    -1,    -1,  1284,   943,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   306,    -1,   308,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,   963,    -1,  1307,   966,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   332,    -1,  2118,  2119,  2120,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,  1338,
    1339,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,  2151,  2152,  2153,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1023,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,    -1,    -1,    -1,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,   417,    -1,    -1,  1408,
      -1,    -1,  1411,  1412,    -1,    -1,    -1,    -1,  1958,  1517,
    1518,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,
    1429,    -1,    -1,    -1,  1433,    -1,    -1,  1094,  1437,    -1,
     451,  1098,    -1,    -1,    -1,    -1,  1103,  1104,   105,    -1,
    1107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1998,    -1,
    1117,    -1,    -1,    -1,    -1,    -1,    -1,  1124,    -1,   480,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   139,  1482,    -1,    -1,   143,  1486,  1487,    -1,
     501,    -1,   149,    -1,    -1,   152,    -1,    -1,    -1,    -1,
      -1,  1500,  1159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1171,    -1,    -1,    -1,  1175,    -1,
      -1,    -1,  1179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   205,    -1,
    1549,  2091,    -1,  2093,    -1,  1554,    -1,   568,   569,    -1,
      -1,    -1,  1219,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   590,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   244,    -1,    -1,
      -1,    -1,  2132,    -1,    -1,  1252,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   261,    -1,   263,  1705,  1607,    -1,
      -1,   268,    -1,  1711,    -1,   272,    -1,  1616,    -1,    -1,
    1277,  1620,    -1,  2163,   635,    -1,   283,  1284,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   306,
    1307,   308,    -1,    -1,    -1,   312,    -1,  1755,    -1,    -1,
      -1,     1,    -1,    -1,     4,    -1,  2206,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   332,    -1,    -1,    -1,    -1,
      -1,  1338,  1339,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   709,    -1,
      -1,   712,   713,  1702,   715,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   724,    -1,    -1,   727,   728,   729,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,
      -1,  1408,    -1,    -1,  1411,  1412,  1854,  1855,    -1,    -1,
     417,    -1,    -1,  1861,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,  1429,    -1,    -1,  1873,  1433,    -1,    -1,    -1,
    1437,    -1,    -1,    -1,    -1,  1883,    -1,  1885,    -1,    -1,
      -1,    -1,    -1,    -1,   451,    -1,   807,    -1,  1896,   139,
    1898,  1899,  1900,   143,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   480,    -1,  1482,    -1,    -1,    -1,  1486,
    1487,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1500,   501,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1962,   205,    -1,    -1,  1966,    -1,
      -1,    -1,    -1,  1971,    -1,  1874,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1549,    -1,    -1,    -1,    -1,  1554,    -1,    -1,
      -1,    -1,    -1,    -1,   244,    -1,    -1,    -1,    -1,    -1,
      -1,   568,   569,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   261,    -1,   263,    -1,    -1,    -1,   938,   268,  2027,
      -1,    -1,   272,   590,    -1,    -1,    -1,    -1,    -1,   950,
     951,  2039,    -1,   283,    -1,  2043,    -1,    -1,    -1,    -1,
    1607,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2056,  1616,
      -1,    -1,    -1,  1620,    -1,    -1,   306,    -1,   308,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,   635,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   332,  1992,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1022,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2036,    -1,    -1,
    2138,    -1,    -1,    -1,   701,  1702,    -1,  2145,    -1,    -1,
      -1,    -1,   709,    -1,    -1,   712,   713,    -1,   715,    -1,
      -1,    -1,    -1,    -1,    -1,  2064,  2065,   724,    -1,    -1,
     727,   728,   729,    -1,    -1,  2173,    -1,   417,    -1,  2177,
      -1,  2179,    -1,    -1,    -1,    -1,  2085,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2097,    -1,
      -1,    -1,  2200,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   451,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     480,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     807,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    -1,
      -1,   501,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    -1,
    1201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1210,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1219,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,  1874,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   568,   569,
     123,    -1,    -1,    -1,  1245,    -1,    -1,    -1,    -1,    -1,
      -1,  1252,    -1,   136,    -1,   138,    -1,    -1,    -1,    -1,
     590,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1277,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,   938,    -1,    -1,    -1,  1296,    -1,    79,    -1,    -1,
      -1,    -1,    -1,   950,   951,   635,    -1,    -1,    -1,    -1,
      -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1344,    -1,  1992,    -1,    -1,    -1,    -1,
      -1,   234,    -1,    -1,    -1,   238,    -1,    -1,   241,   242,
      -1,    -1,   245,    -1,    -1,   248,   249,    -1,   251,    -1,
     253,   701,    -1,    -1,    -1,  1022,    -1,    -1,    -1,   709,
      -1,    -1,   712,   713,    -1,   715,    -1,    -1,    -1,  2036,
      -1,    -1,    -1,    -1,   724,    -1,    -1,   727,   728,   729,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2064,  2065,    -1,
      -1,    -1,    -1,    -1,    -1,   207,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2085,    -1,
     323,    -1,    -1,   326,    -1,    -1,    -1,    -1,    -1,    -1,
    2097,    -1,    -1,    -1,    -1,    -1,  1457,  1458,  1459,  1460,
    1461,  1462,  1463,    -1,    -1,    -1,    -1,   350,   351,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   807,    -1,    -1,
     262,  1482,   365,    -1,    -1,  1486,  1487,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1500,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   314,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   331,
      -1,    -1,    -1,    -1,  1201,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1210,    -1,    -1,    -1,    -1,    -1,    -1,
     352,    -1,  1219,    -1,  1575,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   471,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1245,    -1,
      -1,    -1,    -1,    -1,    -1,  1252,    -1,    -1,   938,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     950,   951,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1277,    -1,    -1,    -1,    -1,    -1,   418,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   529,    -1,    -1,  1296,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   545,    -1,    -1,    -1,    -1,   449,    -1,    -1,
      -1,  1672,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1022,    -1,    -1,   477,    -1,  1344,    -1,    -1,
     482,  1702,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   205,   206,    -1,    -1,
     502,    -1,    -1,    -1,   506,   507,    -1,    -1,   510,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   525,    -1,    -1,   629,    -1,    -1,   238,
      -1,    -1,    -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,
    1761,    -1,    -1,    -1,    -1,    -1,   548,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   671,   672,
      -1,    -1,  1793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   684,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1457,  1458,  1459,  1460,  1461,  1462,  1463,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1482,    -1,   326,    -1,  1486,
    1487,    -1,    -1,   625,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1500,    -1,    -1,    -1,   639,    -1,    -1,
      -1,    -1,    -1,    -1,   353,   354,    -1,    -1,    -1,    -1,
      -1,  1201,    -1,  1874,    -1,    -1,    -1,    -1,    -1,    -1,
    1210,    -1,   664,    -1,   373,    -1,    -1,    -1,    -1,  1219,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1245,    -1,    -1,    -1,    -1,
     803,   804,  1252,    -1,    -1,    -1,    -1,   810,  1575,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1277,   730,    -1,
      -1,    -1,   835,    -1,    -1,   838,   839,    -1,   841,    -1,
     843,   844,    -1,    -1,    -1,   454,  1296,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   471,   472,    -1,   474,   475,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,   488,
      -1,    -1,    -1,   886,    -1,    -1,    -1,   890,    -1,    -1,
      -1,   894,   501,    -1,  1344,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1672,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2036,    -1,    -1,    -1,    -1,
     822,   530,   824,    -1,    -1,   534,    -1,    -1,   830,    -1,
      -1,    -1,    -1,    -1,    -1,  1702,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2064,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   857,    -1,   960,   961,    -1,
      -1,    -1,    -1,    -1,   866,    -1,   575,    -1,    -1,    -1,
     872,   974,    -1,    -1,    -1,    -1,  2097,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1761,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1457,  1458,  1459,
    1460,  1461,  1462,  1463,    -1,   917,    -1,    -1,    -1,    -1,
     922,   630,    -1,    -1,   633,    -1,  1793,    -1,    49,    -1,
      -1,    52,  1482,    54,    -1,    56,  1486,  1487,    -1,    -1,
      -1,    -1,    -1,   652,   653,    -1,    -1,    -1,    -1,    -1,
    1500,    -1,    73,    -1,   663,    -1,    -1,    -1,   667,    -1,
      -1,    -1,    -1,    -1,    -1,   674,    -1,   676,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,  1874,    -1,  1011,
    1113,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,  1575,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,  1157,    -1,    -1,    -1,    -1,  1162,
      -1,    -1,  1165,    -1,    -1,    -1,  1169,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   783,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     799,   800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,
      -1,   810,   811,    -1,   813,   814,    -1,    -1,    -1,    -1,
      -1,    -1,   136,    -1,   138,    -1,    -1,    -1,   827,   828,
      -1,    -1,  1672,   832,    -1,   834,   835,    -1,    -1,    -1,
      -1,    -1,   841,    -1,    -1,    -1,    -1,    -1,   847,    -1,
     849,    -1,    -1,    -1,   853,   854,   855,    -1,    -1,    -1,
      -1,    -1,  1702,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   873,    -1,   875,    -1,    -1,  2036,
     879,    -1,    -1,    -1,    -1,    -1,    -1,   886,   887,    -1,
      -1,   890,   891,    -1,    -1,   894,   895,    -1,    -1,    -1,
      -1,    -1,    -1,   902,    -1,  1197,    -1,  2064,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1313,  1761,    -1,    -1,    -1,    -1,    -1,   241,   242,  1322,
    1323,   245,    -1,    -1,   248,   249,    -1,   251,    -1,   253,
    2097,    -1,    -1,    -1,   943,   944,    -1,    85,    -1,    -1,
      -1,    -1,    -1,  1793,  1246,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   976,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1383,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1394,    -1,    -1,  1397,    -1,  1399,  1400,    -1,    -1,
      -1,    -1,    -1,  1305,   152,    -1,    -1,    -1,   156,    -1,
      -1,    -1,    -1,  1022,  1023,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,    -1,  1874,    -1,   350,   351,    -1,    -1,
    1332,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   365,  1445,    -1,   192,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   206,    -1,
      -1,    -1,   210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1088,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1103,  1104,    -1,    -1,  1107,  1108,
      -1,    -1,    -1,    -1,    -1,  1114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   263,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1528,    -1,    -1,    -1,   277,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   471,    -1,    -1,
    1159,    -1,    -1,  1162,  1163,    -1,  1165,  1166,    -1,    -1,
    1169,  1170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   322,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   332,    -1,  2036,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   353,    -1,   355,    -1,    -1,
      -1,    -1,  1615,    -1,  2064,    -1,    -1,    -1,    -1,    -1,
      -1,   545,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1536,    -1,    -1,    -1,    -1,    -1,
    1643,    -1,    -1,    -1,    -1,    -1,    -1,  2097,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   417,
      -1,    -1,    -1,  1676,    -1,  1284,    -1,    -1,    -1,  1682,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1307,    -1,
      -1,    -1,    -1,   451,  1313,  1314,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1339,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,
      -1,    -1,   490,    -1,    -1,    -1,    -1,   671,   672,    -1,
      -1,    -1,    -1,    -1,  1757,    -1,    -1,  1659,    -1,    -1,
     684,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1383,  1384,    -1,    -1,    -1,   192,
      -1,    -1,    -1,    -1,    -1,  1394,  1395,    -1,  1397,    -1,
      -1,    -1,    -1,   206,    -1,    -1,    -1,    -1,    -1,  1408,
      -1,    -1,  1411,  1412,    -1,  1808,  1809,   220,    -1,   222,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     568,   569,    -1,    -1,    -1,    -1,    -1,   575,    -1,    -1,
      -1,  1834,  1835,    -1,    -1,    -1,    -1,    -1,    -1,  1842,
      -1,    -1,    -1,    -1,  1847,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1762,  1763,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   803,
     804,    -1,    -1,    -1,    -1,   633,   810,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   653,    -1,   655,   321,    -1,
      -1,   835,    -1,    -1,   838,   839,    -1,   841,    -1,   843,
     844,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   676,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1549,    -1,    -1,    -1,    -1,  1948,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   701,    -1,    -1,    -1,    -1,    85,    -1,
      -1,   709,   886,    -1,   712,   713,   890,   715,  1577,    -1,
     894,    -1,    -1,    -1,    -1,    -1,   724,    -1,    -1,   727,
     728,   729,    -1,    -1,    -1,    -1,    -1,  1889,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2021,    -1,
      -1,  1923,    -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   783,   960,   961,    -1,    -1,
      -1,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     974,    -1,    -1,    -1,    -1,    -1,    -1,  1666,  1960,   807,
      -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1682,    -1,    -1,    -1,    -1,    -1,   206,
     828,    -1,    -1,    -1,    -1,  1987,    -1,    -1,    -1,  1991,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   511,   847,
      -1,   849,    -1,    -1,   517,   853,   854,   855,    -1,   522,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2125,    -1,    -1,   873,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,   263,    -1,    -1,    -1,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,  1113,
      -1,    -1,    -1,    -1,    -1,   943,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   951,    -1,   332,    -1,    -1,    -1,    -1,
     623,    -1,    -1,    -1,    -1,    -1,    -1,   965,  1827,  1828,
      -1,    -1,    -1,    -1,    -1,    -1,   353,    -1,    -1,    -1,
      -1,    -1,    -1,  1157,  1843,    -1,    -1,    -1,  1162,    -1,
     653,  1165,    -1,    -1,    -1,  1169,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   666,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1023,    -1,    -1,    -1,   157,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   706,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,    -1,   717,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   451,    -1,    -1,    -1,   741,   742,
      -1,    -1,   745,    -1,   747,    -1,    -1,  1946,    -1,    -1,
     753,    -1,   755,   756,    -1,  1954,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1103,  1104,   484,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     783,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   796,    -1,    -1,    -1,    -1,    -1,  1313,
      -1,    -1,    -1,    -1,   807,    -1,    -1,    -1,  1322,  1323,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     823,  1159,  2021,  2022,    -1,   828,  2025,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   568,   569,    -1,    -1,    -1,   859,    -1,   575,   862,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2067,  1383,
      -1,   874,  1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1394,    -1,    -1,  1397,    -1,  1399,  1400,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     903,    -1,    -1,    -1,    -1,    -1,    -1,  1245,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   633,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2125,  2126,    -1,    -1,
      -1,  1445,    -1,    -1,    -1,    -1,   653,    -1,  1276,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1284,    -1,   951,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1296,   676,
      -1,    -1,   965,   966,    -1,    -1,  2165,    -1,    -1,  1307,
     973,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   701,    -1,    49,    -1,    -1,    52,
      -1,    54,   709,    56,    -1,    -1,   713,    -1,   715,    -1,
      -1,  1339,    -1,    -1,    -1,    -1,  1344,    -1,    -1,    -1,
      73,    -1,    -1,    -1,  1528,    -1,    -1,    -1,    -1,    -1,
    1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1031,    -1,
      -1,    -1,    -1,    -1,    -1,  1038,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,   783,    -1,    -1,    -1,
    1408,   134,    -1,  1411,  1412,    -1,    -1,    -1,    -1,  1082,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,   151,    -1,
     807,    -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,
      -1,  1615,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,   828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1457,
    1458,  1459,    -1,    -1,  1462,  1463,    -1,    -1,    -1,  1643,
     847,  1469,   849,    -1,    -1,    -1,   853,   854,   855,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1154,    -1,  1156,    -1,  1158,   873,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1527,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1549,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   943,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   951,    -1,  1239,  1240,    -1,    -1,
      -1,    -1,     3,  1757,     5,    -1,    -1,    -1,    -1,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,  1808,  1809,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,  1023,    -1,  1311,    -1,
      -1,    72,    73,    -1,  1317,    -1,    -1,    -1,    -1,    -1,
    1834,  1835,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1335,  1847,  1672,    -1,  1339,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,  1357,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1701,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1374,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1103,  1104,    -1,    -1,
      -1,    -1,    -1,   154,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1948,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1159,    -1,    -1,    -1,  1449,  1450,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1793,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1475,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1494,    -1,    -1,  1497,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2021,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1857,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,  1549,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1558,  1559,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,  1569,  1284,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1296,
      -1,    -1,    -1,    -1,    -1,  1588,    -1,  1590,    -1,    -1,
    1307,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,  1339,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,   151,    -1,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,  1992,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,  1672,
      -1,    -1,    -1,    -1,  1677,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,  1408,    -1,    -1,  1411,  1412,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2065,    -1,    -1,
    2068,    -1,    -1,    -1,  1737,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    73,
      74,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,    -1,   101,  1791,   103,
     104,  1794,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   126,   127,     1,    -1,    -1,  1820,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    18,  1549,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,   180,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,    -1,   101,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,     1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1672,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
      -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,   180,    -1,    52,  1999,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,    -1,   101,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   180,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    71,    72,    73,    74,    -1,    76,    -1,    -1,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   180,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      71,    72,    73,    74,    -1,    76,    -1,    -1,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,   163,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,   151,    -1,
      -1,    -1,   155,   156,   157,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,   151,    -1,    -1,    -1,
     155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   180,     3,     4,     5,     6,
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
     157,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,     3,     4,     5,     6,     7,
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
     168,   169,   170,   171,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      77,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,     5,    72,    -1,    -1,    -1,    -1,    77,    78,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     5,    -1,   154,    -1,    -1,    -1,   158,   159,
      13,    14,    15,    16,    17,    -1,   166,   167,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    49,    -1,    -1,    52,
     134,    54,   136,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,     3,
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
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     3,     4,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,    -1,    -1,    -1,    -1,   163,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,   108,   109,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    -1,    -1,    -1,    -1,    90,    -1,    92,    -1,
      -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,   163,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,
      -1,   165,   166,   167,   168,   169,   170,   171,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,   157,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
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
      -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,
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
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
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
     171,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,   162,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,   108,   109,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
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
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,
     163,    -1,    -1,   166,   167,    13,    14,    15,    16,    17,
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
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
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
      -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    13,    14,    15,    16,    17,
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
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
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
      -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   136,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,   157,   158,    20,    -1,    22,    23,    24,
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
      -1,    -1,    -1,    -1,    -1,   136,   137,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,   158,    22,    23,
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
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      13,    14,    15,    16,    17,    18,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   108,   109,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,   108,   109,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      63,    64,    -1,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   108,   109,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,   108,   109,
      -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
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
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,   108,   109,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,    13,    14,    15,    16,
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
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   108,   109,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
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
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,   108,   109,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    13,    14,    15,    16,    17,   166,   167,    20,
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
      13,    14,    15,    16,    17,   166,   167,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    13,
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
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
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
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,   158,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   106,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,   136,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,   136,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,    72,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    49,    55,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,   136,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,
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
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
     162,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
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
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,    -1,   157,
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
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   182,   406,   407,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
     101,   105,   106,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   121,   134,   136,   155,   156,   158,   159,
     166,   167,   185,   186,   187,   202,   293,   294,   295,   296,
     297,   298,   299,   300,   301,   302,   303,   304,   307,   310,
     312,   313,   314,   315,   316,   317,   318,   319,   320,   321,
     323,   325,   326,   327,   329,   330,   334,   335,   336,   337,
     338,   340,   346,   347,   348,   349,   360,   364,   398,   401,
     411,   417,   419,   425,   429,   434,   435,   436,   437,   438,
     439,   440,   441,   467,   485,   486,   487,   488,     0,   182,
     106,   186,   202,   297,   299,   310,   313,   316,   326,   330,
     335,   120,   155,    58,    61,    62,    64,   155,   155,   423,
     424,   425,   322,   323,   108,   109,   186,   378,   399,   400,
     378,   155,   411,   155,   155,     4,   106,   108,   109,   314,
     319,   320,   155,   155,   202,   424,   429,   435,   436,   437,
     439,   440,   441,   108,   337,   160,   182,   159,   300,   310,
     313,   434,   438,   484,   485,   488,   489,   180,   183,   152,
     163,   179,   223,   381,    89,   161,   418,   378,   161,   161,
     161,   180,   108,   109,   155,   202,   305,   306,   429,   430,
     431,   432,   433,   434,   438,   442,   443,   444,   445,   446,
     447,   448,   449,   450,   456,     3,    47,    48,    50,    55,
     328,     3,   159,   202,   299,   314,   318,   320,   331,   336,
     414,   434,   438,   488,    69,   297,   299,   313,   326,   330,
     335,   415,   434,   438,    65,   319,   319,   314,   320,   308,
     319,   320,   328,   347,   314,   319,   314,   158,   423,   161,
     183,   155,   163,   231,   423,   423,     3,   288,   289,   304,
     307,   313,   317,   318,   159,   310,   313,   486,   378,   378,
     411,   179,   313,   155,   202,   420,   429,   430,   434,   443,
     447,   159,   202,   488,   412,   413,    57,    65,    66,    67,
      68,   159,   177,   378,   387,   389,   393,   395,   396,   336,
      57,   157,   159,   202,   309,   313,   317,   325,   326,   332,
     333,   334,   335,   339,   346,   347,   364,   374,   376,   467,
     480,   481,   482,   483,   488,   489,   108,   109,   163,   170,
     186,   336,   456,   425,   155,   394,   395,   155,    13,    88,
     106,   108,   109,   155,   185,   426,   427,   428,   120,   188,
     189,    49,    52,    54,    56,    73,   103,   104,   106,   107,
     118,   119,   122,   123,   124,   126,   127,   155,   159,   165,
     168,   169,   170,   171,   184,   185,   188,   190,   193,   201,
     202,   203,   204,   207,   208,   209,   210,   211,   212,   213,
     214,   215,   216,   217,   218,   219,   225,   336,   157,   159,
     201,   202,   218,   220,   310,   336,   379,   380,   397,   484,
     489,   426,   313,   435,   436,   437,   439,   440,   441,   157,
     157,   157,   157,   157,   157,   157,   108,   159,   186,   310,
     467,   486,   159,   166,   202,   220,   299,   300,   309,   311,
     313,   326,   333,   335,   371,   372,   373,   375,   376,   480,
     488,   160,   155,   434,   438,   488,   155,   161,   106,   158,
     159,   163,   185,   187,   220,   382,   383,   384,   385,   386,
      22,   382,   155,   378,   231,   155,   186,   420,   186,   424,
     429,   431,   432,   433,   442,   444,   445,   446,   448,   449,
     450,   313,   430,   443,   447,   161,   101,   422,   159,   423,
     464,   467,   422,   423,   423,   418,   288,   155,   423,   464,
     422,   423,   423,   418,   423,   423,   313,   420,   155,   155,
     312,   313,   310,   313,   160,   182,   310,   484,   489,   338,
     163,   418,   288,   378,   378,   381,   299,   318,   416,   434,
     438,   163,   418,   288,   399,   313,   326,   313,   313,   108,
     337,   108,   109,   186,   336,   341,   399,   137,   186,   313,
     368,   369,   373,   374,   377,   154,   182,   231,   304,   180,
     434,   447,   313,   182,   422,   155,   422,   183,   220,   424,
     429,   313,   155,   182,   378,   409,   163,   155,   378,   163,
     378,   137,   166,   167,   392,   157,   161,   378,   396,   157,
     423,   423,   160,   182,   311,   313,   326,   333,   335,   479,
     480,   488,   489,   155,   159,   167,   179,   202,   467,   469,
     470,   471,   472,   473,   474,   491,   202,   339,   488,   313,
     333,   319,   314,   423,   157,   311,   313,   481,   311,   467,
     481,    10,   165,   170,   363,   365,   366,   163,   361,   363,
     387,   179,   387,   426,   157,   161,   155,   157,   120,   155,
     201,   155,   155,   155,   204,   155,   201,   155,   106,   108,
     109,   314,   319,   320,   155,   201,   201,    19,    21,    85,
     159,   168,   169,   205,   206,   220,   227,   231,   349,   379,
     488,   161,   182,   155,   190,   159,   164,   159,   164,   123,
     125,   126,   127,   155,   158,   159,   163,   164,   204,   204,
     172,   166,   173,   174,   168,   169,   128,   129,   130,   131,
     175,   176,   132,   133,   167,   165,   177,   134,   135,   178,
     157,   161,   158,   182,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   179,   222,   223,   224,   155,
     202,   460,   461,   462,   463,   464,   157,   161,   157,   157,
     157,   157,   157,   157,   157,   155,   423,   464,   467,   155,
     464,   467,   155,   182,   155,   310,   486,   160,   182,   183,
     159,   183,   155,   167,   202,   429,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   137,   488,   161,   183,   161,
     183,   378,   378,   155,   182,   182,   182,   159,   187,   182,
     383,   162,   161,   490,   382,   158,   159,   162,   386,   156,
     220,   226,   155,   182,   179,   429,   431,   432,   433,   442,
     444,   445,   446,   448,   449,   450,   157,   157,   157,   157,
     157,   157,   157,   157,   157,   157,   430,   443,   447,   423,
     155,   179,   160,   182,   381,   231,   418,   368,   381,   231,
     420,   227,   380,   227,   380,   420,   108,   159,   409,   231,
     418,   422,   163,   163,   418,   288,   409,   231,   418,   343,
     344,   342,   163,   157,   161,   157,   161,    70,   290,   291,
     180,   166,   220,   182,   429,   372,   411,   409,   378,   160,
     182,   155,   391,   389,   390,    78,   324,   186,   163,   170,
     186,   456,   311,   467,   481,   313,   317,   488,   368,   470,
     471,   472,   160,   182,    18,   220,   313,   469,   491,   423,
     423,   467,   311,   479,   489,   313,   186,   423,   311,   481,
     336,   161,   490,   378,   365,   363,   163,   157,   380,   157,
     157,   427,   156,   194,   195,   196,   220,   180,   379,   489,
     190,   159,   379,   380,   379,   489,   220,   379,   157,   379,
     379,   379,   160,   182,   157,   168,   169,   206,    18,   315,
     157,   161,   157,   166,   167,   157,   226,   220,   163,   220,
     186,   220,   186,   118,   159,   186,   194,   108,   109,   118,
     159,   186,   349,   220,   194,   186,   204,   207,   207,   207,
     208,   208,   209,   209,   210,   210,   210,   210,   211,   211,
     212,   213,   214,   215,   216,   162,   227,   180,   188,   159,
     186,   220,   163,   220,   368,   461,   462,   463,   313,   460,
     423,   423,   220,   380,   155,   423,   464,   467,   155,   464,
     467,   368,   368,   182,   182,   160,   160,   155,   429,   452,
     453,   454,   457,    18,   313,   451,   455,   155,   423,   473,
     491,   423,   423,   491,   155,   423,   473,   423,   423,   183,
     219,   378,   372,   375,   160,   375,   376,   160,   491,   491,
     137,   370,   371,   372,   370,   370,   378,   182,   218,   219,
     220,   421,   490,   382,   384,   154,   182,   157,   161,   182,
     370,   220,   157,   157,   157,   157,   157,   157,   157,   157,
     157,   155,   423,   464,   467,   155,   423,   464,   467,   155,
     423,   464,   467,   420,   188,    22,   467,   220,   320,   336,
     465,   231,   157,   157,   157,   157,   157,   407,   408,   231,
     154,   182,   409,   231,   418,   408,   231,   163,   163,   163,
     350,   137,   373,   374,   186,   292,   378,    18,    71,    73,
      74,    76,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    93,    94,    95,    96,    97,    98,
      99,   101,   108,   109,   121,   155,   159,   227,   228,   229,
     230,   231,   232,   233,   235,   236,   245,   251,   252,   253,
     254,   255,   256,   261,   262,   265,   266,   267,   268,   269,
     270,   271,   277,   278,   279,   293,   313,   317,   378,   419,
      70,   183,   183,   370,   161,   410,   408,   157,   297,   299,
     310,   402,   403,   404,   405,   397,   179,   388,   388,   365,
     163,   423,   423,   311,   481,   159,   166,   202,   220,   336,
     220,   313,   157,   157,   157,   157,     5,   313,   423,   469,
     163,   170,   186,   456,    10,   366,   154,   179,   367,   490,
     163,   365,   163,   157,   157,   161,   157,   157,   161,   182,
     161,   157,   157,   157,   161,   157,   204,   157,   157,   157,
     204,    18,   315,   220,   157,   157,   156,   163,   204,   160,
     183,   194,   160,   160,   118,   122,   124,   187,   197,   198,
     199,   157,   197,   160,   161,   154,   218,   162,   157,   197,
     183,   383,   157,   157,   157,   157,   460,   368,   368,   157,
     157,   370,   370,   457,   157,   157,   157,   157,   155,   429,
     456,   451,   455,   368,   368,   160,   183,   491,   161,   183,
     157,   161,   161,   183,   183,   381,   197,   137,   171,   183,
     183,   154,   382,   220,   423,   156,   220,   370,   183,   155,
     423,   464,   467,   155,   423,   464,   467,   155,   423,   464,
     467,   368,   368,   368,   422,   157,   149,   171,   183,   466,
     161,   183,   410,   402,   408,   231,   410,   350,   350,   350,
       3,     5,    10,    73,   154,   294,   301,   302,   310,   313,
     351,   356,   484,   161,   180,   155,    61,    62,   180,   231,
     293,   419,   155,   155,    18,   229,   155,   155,   180,   378,
     180,   378,   166,   378,   163,   228,   155,   155,   155,   229,
     155,   231,   220,   221,   221,    14,   280,   256,   267,   180,
     183,   233,    78,   180,   378,    91,    92,   260,   264,   112,
     135,   259,   111,   134,   263,   259,   377,   313,   162,   292,
     160,   160,   183,   410,   378,   420,   183,   180,   183,   180,
     183,   157,   380,   394,   394,   490,   365,   363,   363,   182,
     183,   183,   183,   220,   155,   423,   473,   467,   312,     5,
     166,   183,   220,   365,   163,   423,   423,   336,   378,   163,
     219,   154,   365,   490,   154,   182,   196,   309,   186,    78,
     191,   192,   379,   204,   204,   204,   204,   204,   163,   383,
     161,   154,   200,   159,   198,   200,   200,   160,   161,   125,
     158,   160,   226,   218,   180,   160,   490,   155,   423,   464,
     467,   157,   157,   183,   183,   157,   155,   423,   464,   467,
     155,   423,   473,   429,   423,   423,   157,   157,   160,   375,
     160,   137,   372,   137,   157,   157,   183,   219,   219,   160,
     160,   183,   183,   157,   368,   368,   368,   157,   157,   157,
     381,   423,   161,   220,   220,   320,   336,   160,   154,   183,
     410,   154,   154,   154,   154,   310,   310,   349,   357,   484,
     310,   356,   155,   345,   180,   180,   155,   162,   202,   352,
     353,   359,   429,   430,   443,   447,   161,   180,   378,   378,
     194,   180,   231,   180,   231,   227,   237,   293,   295,   298,
     304,   313,   317,   227,    80,   157,   237,   149,   150,   151,
     156,   157,   180,   227,   246,   247,   248,   293,   180,   180,
     227,   180,   383,   180,   227,   226,   227,   246,   113,   114,
     115,   116,   117,   272,   274,   275,   180,   100,   180,    84,
     155,   157,   154,   180,   180,   155,   155,   229,   229,   256,
     155,   266,   256,   266,   231,   423,   180,   157,   154,   392,
     154,   182,   161,   161,   154,   490,   163,   163,   160,   160,
     160,   183,   368,   220,   220,   183,   160,   183,   490,   365,
     362,   363,   367,   367,   383,   490,   154,   402,   468,   469,
     157,   162,   157,   161,   162,   383,   490,   226,   123,   197,
     198,   159,   198,   159,   198,   160,   154,   368,   157,   157,
     368,   368,   160,   183,   157,   423,   157,   157,   157,   227,
     466,   154,   154,   345,   345,   345,   352,   155,   202,   354,
     355,   464,   475,   476,   477,   478,   180,   161,   180,   352,
     180,   397,   424,   429,   220,   313,   154,   161,   180,   358,
     359,   358,   358,   378,   157,   157,   227,   313,   157,   155,
     229,   157,   149,   150,   151,   171,   180,   249,   250,   229,
     228,   180,   250,   157,   162,   227,   156,   227,   228,   248,
     180,   490,   157,   157,   157,   157,   231,   274,   275,   155,
     220,   155,   188,   204,   257,   227,    75,   110,   258,   260,
      75,     1,   229,   423,   388,   403,   182,   182,   154,   365,
     365,   160,   157,   183,   183,   160,   160,   154,   490,   363,
     163,   490,   154,   183,   157,   220,   192,   220,   490,   154,
     160,   160,   197,   197,   157,   423,   423,   157,   157,   160,
     160,   220,   180,   476,   477,   478,   313,   475,   161,   180,
     423,   423,   180,   157,   429,   423,   229,   229,    77,    78,
     163,   240,   241,   242,   157,   227,    75,   229,   227,   156,
     227,    75,   180,   108,   156,   227,   228,   248,   156,   227,
     229,   247,   250,   250,   180,   227,   154,   163,   242,   229,
     229,   155,   182,   180,   188,   157,   162,   157,   161,   162,
     157,   229,   155,   229,   229,   229,   394,   378,   420,   490,
     490,   160,   160,   154,   163,   365,   154,   154,   154,   160,
     160,   157,   157,   157,   475,   423,   353,    75,     1,   219,
     238,   239,   421,     1,   162,     1,   182,   229,   240,    75,
     180,   157,   229,    75,   180,   171,   171,   229,   228,   250,
     250,   180,   108,   227,   171,   171,    75,   156,   227,   156,
     227,   228,   180,     1,   182,   182,   276,   311,   313,   484,
     162,   180,   159,   188,   281,   282,   283,   204,   194,   227,
     259,   154,   154,   365,   490,   155,   423,   464,   467,   355,
     229,   137,     1,   161,   162,   154,   286,   287,   293,   229,
      75,   180,   229,   227,   156,   156,   227,   156,   227,   156,
     227,   228,   156,   227,   156,   227,   229,   171,   171,   171,
     171,   154,   286,   276,   183,   155,   202,   420,   475,   186,
     162,   106,   155,   157,   162,   161,   157,   157,    75,   255,
     490,   154,   368,   219,   238,   241,   243,   244,   293,   229,
     171,   171,   171,   171,   156,   156,   227,   156,   227,   156,
     227,   243,   183,   180,   273,   313,   281,   160,   219,   180,
     281,   283,   229,    75,   154,   157,   229,   234,   183,   241,
     156,   156,   227,   156,   227,   156,   227,   183,   273,   218,
     157,   162,   188,   157,   157,   162,   229,     1,   229,   154,
     234,   154,   157,   231,   188,   284,   155,   180,   284,   231,
     161,   162,   219,   157,   188,   186,   285,   157,   180,   157,
     161,   180,   186
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
     248,   248,   248,   249,   249,   250,   250,   250,   251,   251,
     251,   251,   251,   251,   251,   251,   251,   251,   251,   251,
     251,   251,   251,   251,   251,   251,   251,   251,   252,   252,
     253,   254,   255,   256,   256,   257,   257,   258,   259,   259,
     260,   260,   261,   261,   261,   261,   261,   261,   262,   263,
     263,   264,   265,   265,   266,   266,   267,   267,   267,   268,
     269,   270,   271,   271,   271,   272,   272,   273,   273,   274,
     274,   274,   274,   275,   276,   276,   276,   276,   276,   277,
     278,   278,   279,   279,   279,   279,   279,   280,   280,   281,
     281,   282,   282,   283,   283,   284,   284,   284,   285,   285,
     286,   286,   287,   287,   288,   288,   289,   289,   290,   290,
     291,   291,   292,   292,   293,   293,   293,   294,   294,   295,
     295,   295,   295,   295,   296,   296,   296,   297,   297,   297,
     298,   298,   298,   298,   298,   299,   299,   299,   299,   300,
     300,   301,   301,   301,   302,   302,   302,   302,   302,   303,
     303,   304,   304,   304,   304,   305,   305,   305,   305,   305,
     306,   306,   307,   307,   307,   307,   308,   308,   308,   309,
     309,   309,   310,   310,   310,   311,   311,   311,   312,   312,
     313,   313,   314,   314,   315,   315,   315,   315,   315,   316,
     317,   317,   317,   318,   318,   319,   319,   319,   319,   319,
     319,   319,   319,   319,   320,   321,   321,   321,   321,   321,
     321,   321,   321,   321,   321,   321,   321,   321,   321,   321,
     321,   321,   321,   321,   321,   321,   321,   321,   321,   321,
     321,   321,   321,   322,   322,   323,   324,   324,   325,   325,
     325,   325,   325,   326,   326,   327,   327,   327,   327,   328,
     328,   328,   328,   328,   328,   329,   329,   329,   329,   330,
     331,   330,   330,   332,   332,   332,   332,   333,   333,   333,
     334,   334,   334,   334,   335,   335,   335,   336,   336,   336,
     336,   336,   336,   337,   337,   337,   338,   338,   339,   339,
     341,   340,   342,   340,   343,   340,   344,   340,   340,   345,
     345,   346,   346,   347,   347,   348,   348,   348,   349,   349,
     349,   349,   349,   349,   349,   349,   350,   350,   351,   351,
     351,   351,   351,   351,   351,   351,   351,   351,   351,   351,
     352,   352,   352,   353,   353,   353,   353,   354,   354,   354,
     355,   356,   356,   357,   357,   358,   358,   359,   360,   360,
     361,   360,   360,   360,   360,   360,   360,   362,   360,   360,
     360,   360,   360,   363,   363,   364,   364,   365,   365,   365,
     365,   366,   366,   367,   367,   367,   368,   368,   368,   368,
     369,   369,   369,   369,   370,   370,   370,   370,   370,   370,
     370,   371,   371,   371,   371,   372,   372,   373,   373,   374,
     374,   375,   375,   375,   375,   375,   376,   376,   376,   376,
     376,   377,   377,   378,   378,   378,   379,   379,   379,   380,
     380,   381,   381,   381,   381,   382,   382,   383,   383,   383,
     383,   383,   384,   384,   385,   385,   386,   386,   386,   386,
     386,   387,   387,   388,   388,   390,   389,   391,   389,   389,
     389,   389,   392,   392,   392,   392,   393,   393,   393,   393,
     394,   394,   395,   395,   396,   396,   397,   397,   397,   397,
     398,   398,   398,   399,   399,   400,   400,   401,   401,   401,
     401,   402,   402,   403,   403,   404,   404,   404,   405,   405,
     406,   406,   407,   407,   408,   408,   409,   410,   411,   411,
     411,   411,   411,   411,   411,   411,   411,   411,   411,   412,
     411,   413,   411,   414,   411,   415,   411,   416,   411,   417,
     417,   417,   418,   418,   419,   419,   419,   419,   419,   419,
     419,   419,   419,   419,   420,   420,   420,   420,   421,   422,
     422,   423,   423,   424,   424,   425,   425,   425,   426,   426,
     427,   427,   427,   428,   428,   428,   428,   428,   428,   429,
     429,   430,   430,   430,   430,   431,   431,   431,   431,   432,
     432,   432,   432,   432,   432,   432,   433,   433,   433,   433,
     434,   434,   434,   435,   435,   435,   435,   435,   436,   436,
     436,   436,   437,   437,   437,   437,   437,   437,   438,   438,
     438,   439,   439,   439,   439,   439,   440,   440,   440,   440,
     441,   441,   441,   441,   441,   441,   442,   442,   443,   443,
     443,   443,   444,   444,   444,   444,   445,   445,   445,   445,
     445,   445,   445,   446,   446,   446,   446,   447,   447,   447,
     448,   448,   448,   448,   448,   449,   449,   449,   449,   450,
     450,   450,   450,   450,   450,   451,   451,   451,   451,   451,
     452,   452,   452,   453,   453,   453,   453,   454,   454,   454,
     455,   455,   455,   455,   455,   456,   456,   457,   457,   457,
     458,   458,   459,   459,   460,   460,   460,   461,   461,   461,
     461,   461,   462,   462,   462,   462,   463,   463,   463,   464,
     464,   464,   464,   464,   465,   465,   465,   465,   465,   465,
     466,   466,   467,   467,   467,   467,   468,   468,   469,   469,
     469,   469,   470,   470,   470,   470,   470,   471,   471,   471,
     471,   472,   472,   472,   473,   473,   473,   474,   474,   474,
     474,   474,   474,   475,   475,   475,   476,   476,   476,   476,
     476,   477,   477,   477,   477,   478,   478,   479,   479,   479,
     480,   480,   481,   481,   481,   481,   481,   481,   482,   482,
     482,   482,   482,   482,   482,   482,   482,   482,   483,   483,
     483,   483,   484,   484,   484,   485,   485,   486,   486,   486,
     486,   486,   486,   487,   487,   487,   487,   487,   487,   488,
     488,   488,   489,   489,   489,   490,   490,   491,   491
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
       1,     1,     1,     1,     1,     1,     1,     1,     3,     4,
       2,     3,     3,     2,     3,     2,     3,     3,     6,     2,
       2,     3,     3,     3,     3,     3,     3,     5,     1,     1,
       5,     5,     4,     0,     1,     1,     3,     4,     1,     1,
       4,     6,     3,     5,     5,     5,     8,     9,     1,     1,
       1,     4,     3,     3,     1,     3,     1,     3,     5,     1,
       2,     5,     3,     3,     4,     8,     9,     0,     2,     1,
       1,     1,     1,     2,     1,     2,     2,     2,     1,     3,
       1,     1,     6,     8,    10,    12,    14,     0,     1,     0,
       1,     1,     3,     4,     7,     0,     1,     3,     1,     3,
       0,     1,     1,     2,     0,     1,     2,     3,     0,     1,
       3,     4,     1,     3,     2,     2,     1,     7,     5,     1,
       1,     1,     1,     1,     2,     3,     6,     3,     3,     4,
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
       1,     2,     3,     1,     2,     0,     1,     2,     6,     7,
       0,     9,     8,     9,    10,     8,     9,     0,    13,    11,
      12,    11,     1,     0,     1,     3,     3,     3,     2,     5,
       5,     1,     1,     0,     2,     5,     0,     1,     1,     3,
       1,     1,     3,     3,     0,     1,     1,     1,     3,     3,
       3,     1,     3,     3,     5,     1,     3,     3,     3,     2,
       3,     1,     3,     3,     4,     1,     1,     1,     1,     2,
       1,     1,     3,     1,     1,     1,     1,     1,     2,     1,
       1,     0,     2,     2,     4,     1,     4,     0,     1,     2,
       3,     4,     2,     2,     1,     2,     2,     5,     5,     7,
       6,     1,     3,     0,     2,     0,     5,     0,     5,     3,
       1,     8,     0,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     5,     6,     1,     1,     3,     3,
       2,     3,     3,     2,     4,     1,     4,     7,     5,    10,
       8,     1,     4,     2,     2,     1,     1,     5,     2,     5,
       0,     1,     3,     4,     0,     1,     0,     0,     1,     1,
       2,     2,     2,     2,     2,     2,     1,     2,     5,     0,
       6,     0,     8,     0,     7,     0,     7,     0,     8,     1,
       2,     3,     0,     5,     3,     4,     4,     4,     4,     5,
       5,     5,     5,     6,     1,     1,     1,     1,     3,     0,
       5,     0,     1,     1,     2,     6,     4,     4,     1,     3,
       0,     1,     4,     1,     1,     1,     1,     1,     1,     1,
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
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 8482 "Parser/parser.cc"
    break;

  case 3:
#line 645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8488 "Parser/parser.cc"
    break;

  case 4:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8494 "Parser/parser.cc"
    break;

  case 5:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8500 "Parser/parser.cc"
    break;

  case 6:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8506 "Parser/parser.cc"
    break;

  case 7:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8512 "Parser/parser.cc"
    break;

  case 8:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8518 "Parser/parser.cc"
    break;

  case 20:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8524 "Parser/parser.cc"
    break;

  case 21:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8530 "Parser/parser.cc"
    break;

  case 22:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8536 "Parser/parser.cc"
    break;

  case 23:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8546 "Parser/parser.cc"
    break;

  case 24:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8552 "Parser/parser.cc"
    break;

  case 25:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8558 "Parser/parser.cc"
    break;

  case 26:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8564 "Parser/parser.cc"
    break;

  case 28:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8570 "Parser/parser.cc"
    break;

  case 29:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8576 "Parser/parser.cc"
    break;

  case 30:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8582 "Parser/parser.cc"
    break;

  case 31:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8588 "Parser/parser.cc"
    break;

  case 32:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8598 "Parser/parser.cc"
    break;

  case 33:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8604 "Parser/parser.cc"
    break;

  case 34:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8610 "Parser/parser.cc"
    break;

  case 35:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8616 "Parser/parser.cc"
    break;

  case 36:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8622 "Parser/parser.cc"
    break;

  case 37:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8628 "Parser/parser.cc"
    break;

  case 38:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8634 "Parser/parser.cc"
    break;

  case 40:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8646 "Parser/parser.cc"
    break;

  case 41:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8655 "Parser/parser.cc"
    break;

  case 42:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8661 "Parser/parser.cc"
    break;

  case 44:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8667 "Parser/parser.cc"
    break;

  case 45:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8673 "Parser/parser.cc"
    break;

  case 46:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8679 "Parser/parser.cc"
    break;

  case 47:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8685 "Parser/parser.cc"
    break;

  case 48:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8695 "Parser/parser.cc"
    break;

  case 49:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8701 "Parser/parser.cc"
    break;

  case 50:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8708 "Parser/parser.cc"
    break;

  case 51:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8714 "Parser/parser.cc"
    break;

  case 52:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8720 "Parser/parser.cc"
    break;

  case 53:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8726 "Parser/parser.cc"
    break;

  case 54:
#line 813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8732 "Parser/parser.cc"
    break;

  case 55:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8738 "Parser/parser.cc"
    break;

  case 56:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8744 "Parser/parser.cc"
    break;

  case 57:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8750 "Parser/parser.cc"
    break;

  case 58:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8756 "Parser/parser.cc"
    break;

  case 59:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8762 "Parser/parser.cc"
    break;

  case 60:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8768 "Parser/parser.cc"
    break;

  case 61:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8774 "Parser/parser.cc"
    break;

  case 62:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8780 "Parser/parser.cc"
    break;

  case 63:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8786 "Parser/parser.cc"
    break;

  case 64:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8792 "Parser/parser.cc"
    break;

  case 65:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8798 "Parser/parser.cc"
    break;

  case 66:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8804 "Parser/parser.cc"
    break;

  case 67:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8810 "Parser/parser.cc"
    break;

  case 68:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8820 "Parser/parser.cc"
    break;

  case 69:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8826 "Parser/parser.cc"
    break;

  case 72:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8832 "Parser/parser.cc"
    break;

  case 73:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8838 "Parser/parser.cc"
    break;

  case 76:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8844 "Parser/parser.cc"
    break;

  case 78:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8850 "Parser/parser.cc"
    break;

  case 79:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8856 "Parser/parser.cc"
    break;

  case 80:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8862 "Parser/parser.cc"
    break;

  case 81:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8868 "Parser/parser.cc"
    break;

  case 82:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8874 "Parser/parser.cc"
    break;

  case 83:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8880 "Parser/parser.cc"
    break;

  case 84:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8886 "Parser/parser.cc"
    break;

  case 85:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8892 "Parser/parser.cc"
    break;

  case 86:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8900 "Parser/parser.cc"
    break;

  case 87:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8906 "Parser/parser.cc"
    break;

  case 88:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8915 "Parser/parser.cc"
    break;

  case 91:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8921 "Parser/parser.cc"
    break;

  case 92:
#line 918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8927 "Parser/parser.cc"
    break;

  case 93:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8947 "Parser/parser.cc"
    break;

  case 94:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8953 "Parser/parser.cc"
    break;

  case 95:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8959 "Parser/parser.cc"
    break;

  case 96:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8965 "Parser/parser.cc"
    break;

  case 97:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8971 "Parser/parser.cc"
    break;

  case 98:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8977 "Parser/parser.cc"
    break;

  case 99:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8983 "Parser/parser.cc"
    break;

  case 100:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8989 "Parser/parser.cc"
    break;

  case 101:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8995 "Parser/parser.cc"
    break;

  case 102:
#line 958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9001 "Parser/parser.cc"
    break;

  case 103:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9007 "Parser/parser.cc"
    break;

  case 104:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9016 "Parser/parser.cc"
    break;

  case 105:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9022 "Parser/parser.cc"
    break;

  case 106:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9028 "Parser/parser.cc"
    break;

  case 107:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9034 "Parser/parser.cc"
    break;

  case 108:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9040 "Parser/parser.cc"
    break;

  case 109:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9046 "Parser/parser.cc"
    break;

  case 110:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9052 "Parser/parser.cc"
    break;

  case 111:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9058 "Parser/parser.cc"
    break;

  case 113:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9064 "Parser/parser.cc"
    break;

  case 114:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9070 "Parser/parser.cc"
    break;

  case 115:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9076 "Parser/parser.cc"
    break;

  case 116:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9082 "Parser/parser.cc"
    break;

  case 117:
#line 994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9088 "Parser/parser.cc"
    break;

  case 118:
#line 996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9094 "Parser/parser.cc"
    break;

  case 119:
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9100 "Parser/parser.cc"
    break;

  case 120:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9106 "Parser/parser.cc"
    break;

  case 128:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9112 "Parser/parser.cc"
    break;

  case 130:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9118 "Parser/parser.cc"
    break;

  case 131:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9124 "Parser/parser.cc"
    break;

  case 132:
#line 1030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9130 "Parser/parser.cc"
    break;

  case 134:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9136 "Parser/parser.cc"
    break;

  case 135:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9142 "Parser/parser.cc"
    break;

  case 137:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9148 "Parser/parser.cc"
    break;

  case 138:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9154 "Parser/parser.cc"
    break;

  case 140:
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9160 "Parser/parser.cc"
    break;

  case 141:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9166 "Parser/parser.cc"
    break;

  case 142:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9172 "Parser/parser.cc"
    break;

  case 143:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9178 "Parser/parser.cc"
    break;

  case 145:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9184 "Parser/parser.cc"
    break;

  case 146:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9190 "Parser/parser.cc"
    break;

  case 148:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9196 "Parser/parser.cc"
    break;

  case 150:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9202 "Parser/parser.cc"
    break;

  case 152:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9208 "Parser/parser.cc"
    break;

  case 154:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9214 "Parser/parser.cc"
    break;

  case 156:
#line 1096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9220 "Parser/parser.cc"
    break;

  case 158:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9226 "Parser/parser.cc"
    break;

  case 159:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9232 "Parser/parser.cc"
    break;

  case 162:
#line 1115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 9244 "Parser/parser.cc"
    break;

  case 163:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9250 "Parser/parser.cc"
    break;

  case 164:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9256 "Parser/parser.cc"
    break;

  case 168:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9262 "Parser/parser.cc"
    break;

  case 169:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9268 "Parser/parser.cc"
    break;

  case 170:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9274 "Parser/parser.cc"
    break;

  case 171:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9280 "Parser/parser.cc"
    break;

  case 172:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9286 "Parser/parser.cc"
    break;

  case 173:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9292 "Parser/parser.cc"
    break;

  case 174:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9298 "Parser/parser.cc"
    break;

  case 175:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9304 "Parser/parser.cc"
    break;

  case 176:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9310 "Parser/parser.cc"
    break;

  case 177:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9316 "Parser/parser.cc"
    break;

  case 178:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9322 "Parser/parser.cc"
    break;

  case 179:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9328 "Parser/parser.cc"
    break;

  case 180:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9334 "Parser/parser.cc"
    break;

  case 181:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9340 "Parser/parser.cc"
    break;

  case 182:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9346 "Parser/parser.cc"
    break;

  case 184:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9352 "Parser/parser.cc"
    break;

  case 185:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9358 "Parser/parser.cc"
    break;

  case 186:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9364 "Parser/parser.cc"
    break;

  case 188:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9370 "Parser/parser.cc"
    break;

  case 189:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9376 "Parser/parser.cc"
    break;

  case 204:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9382 "Parser/parser.cc"
    break;

  case 206:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9388 "Parser/parser.cc"
    break;

  case 207:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9394 "Parser/parser.cc"
    break;

  case 208:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9405 "Parser/parser.cc"
    break;

  case 209:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9411 "Parser/parser.cc"
    break;

  case 210:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9417 "Parser/parser.cc"
    break;

  case 212:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9423 "Parser/parser.cc"
    break;

  case 213:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9429 "Parser/parser.cc"
    break;

  case 214:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9435 "Parser/parser.cc"
    break;

  case 215:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9441 "Parser/parser.cc"
    break;

  case 216:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9447 "Parser/parser.cc"
    break;

  case 219:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9453 "Parser/parser.cc"
    break;

  case 220:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9460 "Parser/parser.cc"
    break;

  case 221:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9466 "Parser/parser.cc"
    break;

  case 222:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9472 "Parser/parser.cc"
    break;

  case 223:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9478 "Parser/parser.cc"
    break;

  case 224:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9484 "Parser/parser.cc"
    break;

  case 225:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9498 "Parser/parser.cc"
    break;

  case 226:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9504 "Parser/parser.cc"
    break;

  case 227:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9510 "Parser/parser.cc"
    break;

  case 228:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9519 "Parser/parser.cc"
    break;

  case 229:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9525 "Parser/parser.cc"
    break;

  case 230:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9531 "Parser/parser.cc"
    break;

  case 231:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9537 "Parser/parser.cc"
    break;

  case 232:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9543 "Parser/parser.cc"
    break;

  case 233:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9549 "Parser/parser.cc"
    break;

  case 234:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9555 "Parser/parser.cc"
    break;

  case 235:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9561 "Parser/parser.cc"
    break;

  case 237:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9567 "Parser/parser.cc"
    break;

  case 238:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9573 "Parser/parser.cc"
    break;

  case 239:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9579 "Parser/parser.cc"
    break;

  case 240:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9585 "Parser/parser.cc"
    break;

  case 241:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9591 "Parser/parser.cc"
    break;

  case 242:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9597 "Parser/parser.cc"
    break;

  case 243:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9603 "Parser/parser.cc"
    break;

  case 245:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9609 "Parser/parser.cc"
    break;

  case 246:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9615 "Parser/parser.cc"
    break;

  case 247:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9621 "Parser/parser.cc"
    break;

  case 249:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9627 "Parser/parser.cc"
    break;

  case 250:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9633 "Parser/parser.cc"
    break;

  case 251:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9639 "Parser/parser.cc"
    break;

  case 252:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9648 "Parser/parser.cc"
    break;

  case 253:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9654 "Parser/parser.cc"
    break;

  case 254:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9660 "Parser/parser.cc"
    break;

  case 255:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9666 "Parser/parser.cc"
    break;

  case 256:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9675 "Parser/parser.cc"
    break;

  case 257:
#line 1405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9681 "Parser/parser.cc"
    break;

  case 258:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9687 "Parser/parser.cc"
    break;

  case 259:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9693 "Parser/parser.cc"
    break;

  case 260:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9702 "Parser/parser.cc"
    break;

  case 261:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9708 "Parser/parser.cc"
    break;

  case 262:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9714 "Parser/parser.cc"
    break;

  case 264:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9733 "Parser/parser.cc"
    break;

  case 265:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9739 "Parser/parser.cc"
    break;

  case 266:
#line 1448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9748 "Parser/parser.cc"
    break;

  case 267:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9754 "Parser/parser.cc"
    break;

  case 268:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9760 "Parser/parser.cc"
    break;

  case 269:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9766 "Parser/parser.cc"
    break;

  case 270:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9772 "Parser/parser.cc"
    break;

  case 271:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9778 "Parser/parser.cc"
    break;

  case 272:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9784 "Parser/parser.cc"
    break;

  case 273:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9793 "Parser/parser.cc"
    break;

  case 274:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9802 "Parser/parser.cc"
    break;

  case 275:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9808 "Parser/parser.cc"
    break;

  case 276:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9817 "Parser/parser.cc"
    break;

  case 277:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9826 "Parser/parser.cc"
    break;

  case 278:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9832 "Parser/parser.cc"
    break;

  case 279:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9838 "Parser/parser.cc"
    break;

  case 280:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9844 "Parser/parser.cc"
    break;

  case 281:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9850 "Parser/parser.cc"
    break;

  case 282:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9856 "Parser/parser.cc"
    break;

  case 283:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9862 "Parser/parser.cc"
    break;

  case 284:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9868 "Parser/parser.cc"
    break;

  case 285:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9874 "Parser/parser.cc"
    break;

  case 286:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9883 "Parser/parser.cc"
    break;

  case 287:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9893 "Parser/parser.cc"
    break;

  case 288:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9899 "Parser/parser.cc"
    break;

  case 289:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9905 "Parser/parser.cc"
    break;

  case 290:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9914 "Parser/parser.cc"
    break;

  case 291:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9924 "Parser/parser.cc"
    break;

  case 292:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9930 "Parser/parser.cc"
    break;

  case 293:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9939 "Parser/parser.cc"
    break;

  case 294:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9949 "Parser/parser.cc"
    break;

  case 295:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9955 "Parser/parser.cc"
    break;

  case 296:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9961 "Parser/parser.cc"
    break;

  case 297:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9967 "Parser/parser.cc"
    break;

  case 298:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9973 "Parser/parser.cc"
    break;

  case 299:
#line 1558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9982 "Parser/parser.cc"
    break;

  case 300:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9992 "Parser/parser.cc"
    break;

  case 301:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9998 "Parser/parser.cc"
    break;

  case 302:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10007 "Parser/parser.cc"
    break;

  case 303:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10017 "Parser/parser.cc"
    break;

  case 304:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10023 "Parser/parser.cc"
    break;

  case 305:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10032 "Parser/parser.cc"
    break;

  case 306:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10042 "Parser/parser.cc"
    break;

  case 307:
#line 1596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10048 "Parser/parser.cc"
    break;

  case 308:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 10057 "Parser/parser.cc"
    break;

  case 309:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 10068 "Parser/parser.cc"
    break;

  case 310:
#line 1614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10074 "Parser/parser.cc"
    break;

  case 311:
#line 1616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10080 "Parser/parser.cc"
    break;

  case 312:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10086 "Parser/parser.cc"
    break;

  case 313:
#line 1623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10092 "Parser/parser.cc"
    break;

  case 314:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10098 "Parser/parser.cc"
    break;

  case 316:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10104 "Parser/parser.cc"
    break;

  case 317:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10110 "Parser/parser.cc"
    break;

  case 318:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10116 "Parser/parser.cc"
    break;

  case 319:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10122 "Parser/parser.cc"
    break;

  case 320:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10128 "Parser/parser.cc"
    break;

  case 321:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10134 "Parser/parser.cc"
    break;

  case 322:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10140 "Parser/parser.cc"
    break;

  case 323:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10146 "Parser/parser.cc"
    break;

  case 324:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10152 "Parser/parser.cc"
    break;

  case 325:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10158 "Parser/parser.cc"
    break;

  case 326:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10164 "Parser/parser.cc"
    break;

  case 327:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10170 "Parser/parser.cc"
    break;

  case 328:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10176 "Parser/parser.cc"
    break;

  case 329:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10182 "Parser/parser.cc"
    break;

  case 330:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10188 "Parser/parser.cc"
    break;

  case 331:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10194 "Parser/parser.cc"
    break;

  case 332:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10200 "Parser/parser.cc"
    break;

  case 333:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10206 "Parser/parser.cc"
    break;

  case 334:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10212 "Parser/parser.cc"
    break;

  case 335:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10218 "Parser/parser.cc"
    break;

  case 336:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10224 "Parser/parser.cc"
    break;

  case 337:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10230 "Parser/parser.cc"
    break;

  case 340:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10236 "Parser/parser.cc"
    break;

  case 341:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10245 "Parser/parser.cc"
    break;

  case 342:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10251 "Parser/parser.cc"
    break;

  case 343:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10257 "Parser/parser.cc"
    break;

  case 346:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10263 "Parser/parser.cc"
    break;

  case 347:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10269 "Parser/parser.cc"
    break;

  case 350:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10275 "Parser/parser.cc"
    break;

  case 351:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10281 "Parser/parser.cc"
    break;

  case 352:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10287 "Parser/parser.cc"
    break;

  case 353:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10293 "Parser/parser.cc"
    break;

  case 354:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10299 "Parser/parser.cc"
    break;

  case 355:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10305 "Parser/parser.cc"
    break;

  case 356:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10311 "Parser/parser.cc"
    break;

  case 357:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10317 "Parser/parser.cc"
    break;

  case 358:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10323 "Parser/parser.cc"
    break;

  case 361:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10329 "Parser/parser.cc"
    break;

  case 362:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10335 "Parser/parser.cc"
    break;

  case 363:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10341 "Parser/parser.cc"
    break;

  case 364:
#line 1779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10347 "Parser/parser.cc"
    break;

  case 365:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10353 "Parser/parser.cc"
    break;

  case 366:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10359 "Parser/parser.cc"
    break;

  case 367:
#line 1788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10365 "Parser/parser.cc"
    break;

  case 368:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10371 "Parser/parser.cc"
    break;

  case 369:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10377 "Parser/parser.cc"
    break;

  case 370:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10383 "Parser/parser.cc"
    break;

  case 371:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10389 "Parser/parser.cc"
    break;

  case 372:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10395 "Parser/parser.cc"
    break;

  case 373:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10401 "Parser/parser.cc"
    break;

  case 374:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10407 "Parser/parser.cc"
    break;

  case 375:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10413 "Parser/parser.cc"
    break;

  case 376:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10419 "Parser/parser.cc"
    break;

  case 377:
#line 1826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10425 "Parser/parser.cc"
    break;

  case 378:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10431 "Parser/parser.cc"
    break;

  case 379:
#line 1831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10437 "Parser/parser.cc"
    break;

  case 380:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10443 "Parser/parser.cc"
    break;

  case 381:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10449 "Parser/parser.cc"
    break;

  case 382:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10455 "Parser/parser.cc"
    break;

  case 383:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10461 "Parser/parser.cc"
    break;

  case 385:
#line 1845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10467 "Parser/parser.cc"
    break;

  case 386:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10473 "Parser/parser.cc"
    break;

  case 387:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10479 "Parser/parser.cc"
    break;

  case 392:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10485 "Parser/parser.cc"
    break;

  case 393:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10491 "Parser/parser.cc"
    break;

  case 394:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10497 "Parser/parser.cc"
    break;

  case 395:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10503 "Parser/parser.cc"
    break;

  case 396:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10509 "Parser/parser.cc"
    break;

  case 397:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10515 "Parser/parser.cc"
    break;

  case 398:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10521 "Parser/parser.cc"
    break;

  case 399:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10527 "Parser/parser.cc"
    break;

  case 402:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10533 "Parser/parser.cc"
    break;

  case 403:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10539 "Parser/parser.cc"
    break;

  case 404:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10548 "Parser/parser.cc"
    break;

  case 405:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10554 "Parser/parser.cc"
    break;

  case 406:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10560 "Parser/parser.cc"
    break;

  case 407:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10566 "Parser/parser.cc"
    break;

  case 408:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10575 "Parser/parser.cc"
    break;

  case 409:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10584 "Parser/parser.cc"
    break;

  case 410:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10590 "Parser/parser.cc"
    break;

  case 413:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10596 "Parser/parser.cc"
    break;

  case 414:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10602 "Parser/parser.cc"
    break;

  case 416:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10608 "Parser/parser.cc"
    break;

  case 417:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10614 "Parser/parser.cc"
    break;

  case 427:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10620 "Parser/parser.cc"
    break;

  case 428:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10626 "Parser/parser.cc"
    break;

  case 432:
#line 1996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10632 "Parser/parser.cc"
    break;

  case 434:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10638 "Parser/parser.cc"
    break;

  case 435:
#line 2006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10644 "Parser/parser.cc"
    break;

  case 436:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10650 "Parser/parser.cc"
    break;

  case 437:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10656 "Parser/parser.cc"
    break;

  case 438:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10662 "Parser/parser.cc"
    break;

  case 439:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10668 "Parser/parser.cc"
    break;

  case 441:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10674 "Parser/parser.cc"
    break;

  case 442:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10680 "Parser/parser.cc"
    break;

  case 443:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10686 "Parser/parser.cc"
    break;

  case 444:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10697 "Parser/parser.cc"
    break;

  case 445:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10703 "Parser/parser.cc"
    break;

  case 446:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10709 "Parser/parser.cc"
    break;

  case 447:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10715 "Parser/parser.cc"
    break;

  case 448:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10721 "Parser/parser.cc"
    break;

  case 449:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10727 "Parser/parser.cc"
    break;

  case 450:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10733 "Parser/parser.cc"
    break;

  case 451:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10742 "Parser/parser.cc"
    break;

  case 452:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10751 "Parser/parser.cc"
    break;

  case 453:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10760 "Parser/parser.cc"
    break;

  case 454:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10771 "Parser/parser.cc"
    break;

  case 455:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10780 "Parser/parser.cc"
    break;

  case 456:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10786 "Parser/parser.cc"
    break;

  case 457:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10792 "Parser/parser.cc"
    break;

  case 458:
#line 2108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10798 "Parser/parser.cc"
    break;

  case 459:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10806 "Parser/parser.cc"
    break;

  case 460:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10814 "Parser/parser.cc"
    break;

  case 461:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10820 "Parser/parser.cc"
    break;

  case 464:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10835 "Parser/parser.cc"
    break;

  case 465:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10841 "Parser/parser.cc"
    break;

  case 466:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10847 "Parser/parser.cc"
    break;

  case 467:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10853 "Parser/parser.cc"
    break;

  case 468:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10859 "Parser/parser.cc"
    break;

  case 469:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10865 "Parser/parser.cc"
    break;

  case 475:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   (yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10875 "Parser/parser.cc"
    break;

  case 488:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10881 "Parser/parser.cc"
    break;

  case 491:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10887 "Parser/parser.cc"
    break;

  case 492:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10893 "Parser/parser.cc"
    break;

  case 494:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10899 "Parser/parser.cc"
    break;

  case 495:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10905 "Parser/parser.cc"
    break;

  case 496:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10911 "Parser/parser.cc"
    break;

  case 497:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10917 "Parser/parser.cc"
    break;

  case 498:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10923 "Parser/parser.cc"
    break;

  case 499:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10929 "Parser/parser.cc"
    break;

  case 501:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10935 "Parser/parser.cc"
    break;

  case 502:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10941 "Parser/parser.cc"
    break;

  case 504:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10947 "Parser/parser.cc"
    break;

  case 505:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10953 "Parser/parser.cc"
    break;

  case 506:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10959 "Parser/parser.cc"
    break;

  case 507:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10965 "Parser/parser.cc"
    break;

  case 508:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 10971 "Parser/parser.cc"
    break;

  case 509:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 10977 "Parser/parser.cc"
    break;

  case 510:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 10983 "Parser/parser.cc"
    break;

  case 511:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 10989 "Parser/parser.cc"
    break;

  case 512:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 10995 "Parser/parser.cc"
    break;

  case 513:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11001 "Parser/parser.cc"
    break;

  case 514:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11007 "Parser/parser.cc"
    break;

  case 515:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::Void ); }
#line 11013 "Parser/parser.cc"
    break;

  case 516:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::Bool ); }
#line 11019 "Parser/parser.cc"
    break;

  case 517:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::Char ); }
#line 11025 "Parser/parser.cc"
    break;

  case 518:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::Int ); }
#line 11031 "Parser/parser.cc"
    break;

  case 519:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::Int128 ); }
#line 11037 "Parser/parser.cc"
    break;

  case 520:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( DeclarationNode::Int128 ), build_signedness( DeclarationNode::Unsigned ) ); }
#line 11043 "Parser/parser.cc"
    break;

  case 521:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::Float ); }
#line 11049 "Parser/parser.cc"
    break;

  case 522:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::Double ); }
#line 11055 "Parser/parser.cc"
    break;

  case 523:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::uuFloat80 ); }
#line 11061 "Parser/parser.cc"
    break;

  case 524:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::uuFloat128 ); }
#line 11067 "Parser/parser.cc"
    break;

  case 525:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::uFloat16 ); }
#line 11073 "Parser/parser.cc"
    break;

  case 526:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::uFloat32 ); }
#line 11079 "Parser/parser.cc"
    break;

  case 527:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::uFloat32x ); }
#line 11085 "Parser/parser.cc"
    break;

  case 528:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::uFloat64 ); }
#line 11091 "Parser/parser.cc"
    break;

  case 529:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::uFloat64x ); }
#line 11097 "Parser/parser.cc"
    break;

  case 530:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( DeclarationNode::uFloat128 ); }
#line 11103 "Parser/parser.cc"
    break;

  case 531:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11109 "Parser/parser.cc"
    break;

  case 532:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11115 "Parser/parser.cc"
    break;

  case 533:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11121 "Parser/parser.cc"
    break;

  case 534:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( DeclarationNode::Complex ); }
#line 11127 "Parser/parser.cc"
    break;

  case 535:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( DeclarationNode::Imaginary ); }
#line 11133 "Parser/parser.cc"
    break;

  case 536:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( DeclarationNode::Signed ); }
#line 11139 "Parser/parser.cc"
    break;

  case 537:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( DeclarationNode::Unsigned ); }
#line 11145 "Parser/parser.cc"
    break;

  case 538:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( DeclarationNode::Short ); }
#line 11151 "Parser/parser.cc"
    break;

  case 539:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( DeclarationNode::Long ); }
#line 11157 "Parser/parser.cc"
    break;

  case 540:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( DeclarationNode::Valist ); }
#line 11163 "Parser/parser.cc"
    break;

  case 541:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( DeclarationNode::AutoType ); }
#line 11169 "Parser/parser.cc"
    break;

  case 543:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11175 "Parser/parser.cc"
    break;

  case 545:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11181 "Parser/parser.cc"
    break;

  case 546:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11187 "Parser/parser.cc"
    break;

  case 547:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11193 "Parser/parser.cc"
    break;

  case 549:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11199 "Parser/parser.cc"
    break;

  case 550:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11205 "Parser/parser.cc"
    break;

  case 551:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11211 "Parser/parser.cc"
    break;

  case 552:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11217 "Parser/parser.cc"
    break;

  case 554:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11223 "Parser/parser.cc"
    break;

  case 556:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11229 "Parser/parser.cc"
    break;

  case 557:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11235 "Parser/parser.cc"
    break;

  case 558:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11241 "Parser/parser.cc"
    break;

  case 559:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11247 "Parser/parser.cc"
    break;

  case 560:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11253 "Parser/parser.cc"
    break;

  case 561:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11259 "Parser/parser.cc"
    break;

  case 562:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11265 "Parser/parser.cc"
    break;

  case 563:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( DeclarationNode::Zero ) ); }
#line 11271 "Parser/parser.cc"
    break;

  case 564:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( DeclarationNode::One ) ); }
#line 11277 "Parser/parser.cc"
    break;

  case 566:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11283 "Parser/parser.cc"
    break;

  case 567:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11289 "Parser/parser.cc"
    break;

  case 568:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11295 "Parser/parser.cc"
    break;

  case 570:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11301 "Parser/parser.cc"
    break;

  case 571:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11307 "Parser/parser.cc"
    break;

  case 572:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11316 "Parser/parser.cc"
    break;

  case 574:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11322 "Parser/parser.cc"
    break;

  case 575:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11328 "Parser/parser.cc"
    break;

  case 576:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11334 "Parser/parser.cc"
    break;

  case 578:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 579:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11346 "Parser/parser.cc"
    break;

  case 581:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11352 "Parser/parser.cc"
    break;

  case 582:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11358 "Parser/parser.cc"
    break;

  case 583:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11364 "Parser/parser.cc"
    break;

  case 584:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11370 "Parser/parser.cc"
    break;

  case 585:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11376 "Parser/parser.cc"
    break;

  case 586:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11382 "Parser/parser.cc"
    break;

  case 587:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11388 "Parser/parser.cc"
    break;

  case 588:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11394 "Parser/parser.cc"
    break;

  case 589:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11400 "Parser/parser.cc"
    break;

  case 591:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11406 "Parser/parser.cc"
    break;

  case 592:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11412 "Parser/parser.cc"
    break;

  case 593:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11418 "Parser/parser.cc"
    break;

  case 594:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11424 "Parser/parser.cc"
    break;

  case 595:
#line 2503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11430 "Parser/parser.cc"
    break;

  case 600:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11436 "Parser/parser.cc"
    break;

  case 601:
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11442 "Parser/parser.cc"
    break;

  case 602:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11451 "Parser/parser.cc"
    break;

  case 603:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11459 "Parser/parser.cc"
    break;

  case 604:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11468 "Parser/parser.cc"
    break;

  case 605:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11477 "Parser/parser.cc"
    break;

  case 606:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11486 "Parser/parser.cc"
    break;

  case 607:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11495 "Parser/parser.cc"
    break;

  case 609:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11501 "Parser/parser.cc"
    break;

  case 610:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11507 "Parser/parser.cc"
    break;

  case 611:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11517 "Parser/parser.cc"
    break;

  case 612:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			forall = false;								// reset
			// Create new generic declaration with same name as previous forward declaration, where the IDENTIFIER is
			// switched to a TYPEGENname. Link any generic arguments from typegen_name to new generic declaration and
			if ( (yyvsp[0].type)->kind == TypeData::SymbolicInst && ! (yyvsp[0].type)->symbolic.isTypedef ) {
				(yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) );
			} else {
				(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].type)->symbolic.name, (yyvsp[0].type)->symbolic.actuals, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
				(yyvsp[0].type)->symbolic.name = nullptr;			// copied to $$
				(yyvsp[0].type)->symbolic.actuals = nullptr;
				delete (yyvsp[0].type);
			}
		}
#line 11535 "Parser/parser.cc"
    break;

  case 615:
#line 2592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11541 "Parser/parser.cc"
    break;

  case 616:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11547 "Parser/parser.cc"
    break;

  case 617:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11553 "Parser/parser.cc"
    break;

  case 618:
#line 2601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11559 "Parser/parser.cc"
    break;

  case 619:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11565 "Parser/parser.cc"
    break;

  case 620:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11571 "Parser/parser.cc"
    break;

  case 621:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11580 "Parser/parser.cc"
    break;

  case 622:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11586 "Parser/parser.cc"
    break;

  case 623:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11595 "Parser/parser.cc"
    break;

  case 624:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11601 "Parser/parser.cc"
    break;

  case 625:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11610 "Parser/parser.cc"
    break;

  case 626:
#line 2629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11616 "Parser/parser.cc"
    break;

  case 627:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11622 "Parser/parser.cc"
    break;

  case 628:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11635 "Parser/parser.cc"
    break;

  case 629:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11644 "Parser/parser.cc"
    break;

  case 630:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11650 "Parser/parser.cc"
    break;

  case 631:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11656 "Parser/parser.cc"
    break;

  case 632:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11669 "Parser/parser.cc"
    break;

  case 633:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11675 "Parser/parser.cc"
    break;

  case 636:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11681 "Parser/parser.cc"
    break;

  case 637:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11687 "Parser/parser.cc"
    break;

  case 640:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11693 "Parser/parser.cc"
    break;

  case 642:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11699 "Parser/parser.cc"
    break;

  case 643:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11705 "Parser/parser.cc"
    break;

  case 644:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11711 "Parser/parser.cc"
    break;

  case 645:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11717 "Parser/parser.cc"
    break;

  case 646:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11723 "Parser/parser.cc"
    break;

  case 647:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11729 "Parser/parser.cc"
    break;

  case 649:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11735 "Parser/parser.cc"
    break;

  case 651:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11741 "Parser/parser.cc"
    break;

  case 652:
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11747 "Parser/parser.cc"
    break;

  case 654:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11753 "Parser/parser.cc"
    break;

  case 655:
#line 2726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11759 "Parser/parser.cc"
    break;

  case 657:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11765 "Parser/parser.cc"
    break;

  case 658:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11771 "Parser/parser.cc"
    break;

  case 659:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11777 "Parser/parser.cc"
    break;

  case 660:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11783 "Parser/parser.cc"
    break;

  case 661:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11789 "Parser/parser.cc"
    break;

  case 662:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11795 "Parser/parser.cc"
    break;

  case 663:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11806 "Parser/parser.cc"
    break;

  case 664:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11812 "Parser/parser.cc"
    break;

  case 665:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11820 "Parser/parser.cc"
    break;

  case 666:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11826 "Parser/parser.cc"
    break;

  case 667:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl) && ((yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11837 "Parser/parser.cc"
    break;

  case 668:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11845 "Parser/parser.cc"
    break;

  case 669:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11853 "Parser/parser.cc"
    break;

  case 670:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11861 "Parser/parser.cc"
    break;

  case 671:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11869 "Parser/parser.cc"
    break;

  case 673:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11875 "Parser/parser.cc"
    break;

  case 674:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11881 "Parser/parser.cc"
    break;

  case 675:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11890 "Parser/parser.cc"
    break;

  case 676:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11899 "Parser/parser.cc"
    break;

  case 677:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11905 "Parser/parser.cc"
    break;

  case 678:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].type)->symbolic.name ); }
#line 11911 "Parser/parser.cc"
    break;

  case 679:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11917 "Parser/parser.cc"
    break;

  case 680:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11923 "Parser/parser.cc"
    break;

  case 682:
#line 2823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11929 "Parser/parser.cc"
    break;

  case 683:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11935 "Parser/parser.cc"
    break;

  case 684:
#line 2829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11941 "Parser/parser.cc"
    break;

  case 685:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11947 "Parser/parser.cc"
    break;

  case 686:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11953 "Parser/parser.cc"
    break;

  case 687:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11959 "Parser/parser.cc"
    break;

  case 689:
#line 2844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11965 "Parser/parser.cc"
    break;

  case 692:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11971 "Parser/parser.cc"
    break;

  case 693:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11977 "Parser/parser.cc"
    break;

  case 694:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( DeclarationNode::Void ) ); }
#line 11983 "Parser/parser.cc"
    break;

  case 695:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11989 "Parser/parser.cc"
    break;

  case 698:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11995 "Parser/parser.cc"
    break;

  case 699:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12001 "Parser/parser.cc"
    break;

  case 700:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12007 "Parser/parser.cc"
    break;

  case 702:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12013 "Parser/parser.cc"
    break;

  case 703:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12019 "Parser/parser.cc"
    break;

  case 704:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12025 "Parser/parser.cc"
    break;

  case 706:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12031 "Parser/parser.cc"
    break;

  case 707:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12037 "Parser/parser.cc"
    break;

  case 708:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12043 "Parser/parser.cc"
    break;

  case 709:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12049 "Parser/parser.cc"
    break;

  case 710:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12055 "Parser/parser.cc"
    break;

  case 712:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12061 "Parser/parser.cc"
    break;

  case 713:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12067 "Parser/parser.cc"
    break;

  case 714:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12073 "Parser/parser.cc"
    break;

  case 719:
#line 2925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12079 "Parser/parser.cc"
    break;

  case 721:
#line 2935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12085 "Parser/parser.cc"
    break;

  case 722:
#line 2937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12091 "Parser/parser.cc"
    break;

  case 728:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12097 "Parser/parser.cc"
    break;

  case 731:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12103 "Parser/parser.cc"
    break;

  case 732:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12109 "Parser/parser.cc"
    break;

  case 733:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12115 "Parser/parser.cc"
    break;

  case 734:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12121 "Parser/parser.cc"
    break;

  case 735:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12127 "Parser/parser.cc"
    break;

  case 736:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12133 "Parser/parser.cc"
    break;

  case 737:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12139 "Parser/parser.cc"
    break;

  case 739:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12145 "Parser/parser.cc"
    break;

  case 740:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12151 "Parser/parser.cc"
    break;

  case 741:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12157 "Parser/parser.cc"
    break;

  case 743:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12163 "Parser/parser.cc"
    break;

  case 745:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12169 "Parser/parser.cc"
    break;

  case 746:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12175 "Parser/parser.cc"
    break;

  case 747:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12181 "Parser/parser.cc"
    break;

  case 748:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12187 "Parser/parser.cc"
    break;

  case 749:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12193 "Parser/parser.cc"
    break;

  case 750:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12199 "Parser/parser.cc"
    break;

  case 752:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12205 "Parser/parser.cc"
    break;

  case 753:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12211 "Parser/parser.cc"
    break;

  case 754:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12217 "Parser/parser.cc"
    break;

  case 755:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12228 "Parser/parser.cc"
    break;

  case 756:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12234 "Parser/parser.cc"
    break;

  case 757:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12240 "Parser/parser.cc"
    break;

  case 758:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12246 "Parser/parser.cc"
    break;

  case 759:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12255 "Parser/parser.cc"
    break;

  case 760:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12261 "Parser/parser.cc"
    break;

  case 761:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12271 "Parser/parser.cc"
    break;

  case 762:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12277 "Parser/parser.cc"
    break;

  case 763:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12283 "Parser/parser.cc"
    break;

  case 764:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12289 "Parser/parser.cc"
    break;

  case 765:
#line 3088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12295 "Parser/parser.cc"
    break;

  case 766:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12301 "Parser/parser.cc"
    break;

  case 767:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12307 "Parser/parser.cc"
    break;

  case 768:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12313 "Parser/parser.cc"
    break;

  case 769:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12319 "Parser/parser.cc"
    break;

  case 770:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12325 "Parser/parser.cc"
    break;

  case 773:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12331 "Parser/parser.cc"
    break;

  case 774:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12337 "Parser/parser.cc"
    break;

  case 775:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12343 "Parser/parser.cc"
    break;

  case 776:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12349 "Parser/parser.cc"
    break;

  case 778:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12355 "Parser/parser.cc"
    break;

  case 779:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12361 "Parser/parser.cc"
    break;

  case 780:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12367 "Parser/parser.cc"
    break;

  case 781:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12373 "Parser/parser.cc"
    break;

  case 782:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12379 "Parser/parser.cc"
    break;

  case 783:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12385 "Parser/parser.cc"
    break;

  case 784:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12391 "Parser/parser.cc"
    break;

  case 785:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12400 "Parser/parser.cc"
    break;

  case 786:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12409 "Parser/parser.cc"
    break;

  case 787:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12418 "Parser/parser.cc"
    break;

  case 788:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12424 "Parser/parser.cc"
    break;

  case 789:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12433 "Parser/parser.cc"
    break;

  case 790:
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12439 "Parser/parser.cc"
    break;

  case 792:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12445 "Parser/parser.cc"
    break;

  case 797:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12451 "Parser/parser.cc"
    break;

  case 798:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12457 "Parser/parser.cc"
    break;

  case 799:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12463 "Parser/parser.cc"
    break;

  case 801:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 12469 "Parser/parser.cc"
    break;

  case 802:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12475 "Parser/parser.cc"
    break;

  case 803:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12481 "Parser/parser.cc"
    break;

  case 804:
#line 3221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12487 "Parser/parser.cc"
    break;

  case 806:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12493 "Parser/parser.cc"
    break;

  case 807:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12499 "Parser/parser.cc"
    break;

  case 808:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12505 "Parser/parser.cc"
    break;

  case 809:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Variable declarations of anonymous types requires creating a unique type-name across multiple translation
			// unit, which is a dubious task, especially because C uses name rather than structural typing; hence it is
			// disallowed at the moment.
			if ( (yyvsp[0].decl)->linkage == ast::Linkage::Cforall && ! (yyvsp[0].decl)->storageClasses.is_static &&
				 (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->kind == TypeData::AggregateInst ) {
				if ( (yyvsp[0].decl)->type->aggInst.aggregate->kind == TypeData::Enum && (yyvsp[0].decl)->type->aggInst.aggregate->enumeration.anon ) {
					SemanticError( yylloc, "extern anonymous enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
				} else if ( (yyvsp[0].decl)->type->aggInst.aggregate->aggregate.anon ) { // handles struct or union
					SemanticError( yylloc, "extern anonymous struct/union is currently unimplemented." ); (yyval.decl) = nullptr;
				}
			}
		}
#line 12523 "Parser/parser.cc"
    break;

  case 810:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12529 "Parser/parser.cc"
    break;

  case 811:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12535 "Parser/parser.cc"
    break;

  case 812:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12541 "Parser/parser.cc"
    break;

  case 813:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12547 "Parser/parser.cc"
    break;

  case 814:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12553 "Parser/parser.cc"
    break;

  case 815:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12559 "Parser/parser.cc"
    break;

  case 817:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12568 "Parser/parser.cc"
    break;

  case 818:
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12574 "Parser/parser.cc"
    break;

  case 819:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12583 "Parser/parser.cc"
    break;

  case 820:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12593 "Parser/parser.cc"
    break;

  case 821:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12602 "Parser/parser.cc"
    break;

  case 822:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12612 "Parser/parser.cc"
    break;

  case 823:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12623 "Parser/parser.cc"
    break;

  case 824:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12633 "Parser/parser.cc"
    break;

  case 825:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12644 "Parser/parser.cc"
    break;

  case 826:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12654 "Parser/parser.cc"
    break;

  case 827:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12665 "Parser/parser.cc"
    break;

  case 828:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12675 "Parser/parser.cc"
    break;

  case 830:
#line 3342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12681 "Parser/parser.cc"
    break;

  case 831:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12687 "Parser/parser.cc"
    break;

  case 832:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12693 "Parser/parser.cc"
    break;

  case 833:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12705 "Parser/parser.cc"
    break;

  case 834:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12716 "Parser/parser.cc"
    break;

  case 835:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12725 "Parser/parser.cc"
    break;

  case 836:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12734 "Parser/parser.cc"
    break;

  case 837:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12740 "Parser/parser.cc"
    break;

  case 838:
#line 3383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12746 "Parser/parser.cc"
    break;

  case 839:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12752 "Parser/parser.cc"
    break;

  case 840:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12761 "Parser/parser.cc"
    break;

  case 841:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12767 "Parser/parser.cc"
    break;

  case 842:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12773 "Parser/parser.cc"
    break;

  case 843:
#line 3402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12779 "Parser/parser.cc"
    break;

  case 848:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12785 "Parser/parser.cc"
    break;

  case 849:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12791 "Parser/parser.cc"
    break;

  case 850:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12801 "Parser/parser.cc"
    break;

  case 851:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12807 "Parser/parser.cc"
    break;

  case 854:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12813 "Parser/parser.cc"
    break;

  case 855:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12819 "Parser/parser.cc"
    break;

  case 856:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12825 "Parser/parser.cc"
    break;

  case 857:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12831 "Parser/parser.cc"
    break;

  case 859:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12837 "Parser/parser.cc"
    break;

  case 860:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12843 "Parser/parser.cc"
    break;

  case 861:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12849 "Parser/parser.cc"
    break;

  case 862:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12855 "Parser/parser.cc"
    break;

  case 867:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12861 "Parser/parser.cc"
    break;

  case 868:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12867 "Parser/parser.cc"
    break;

  case 869:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12873 "Parser/parser.cc"
    break;

  case 870:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12879 "Parser/parser.cc"
    break;

  case 871:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12885 "Parser/parser.cc"
    break;

  case 873:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12891 "Parser/parser.cc"
    break;

  case 874:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12897 "Parser/parser.cc"
    break;

  case 875:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12903 "Parser/parser.cc"
    break;

  case 876:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12909 "Parser/parser.cc"
    break;

  case 877:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12915 "Parser/parser.cc"
    break;

  case 878:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12921 "Parser/parser.cc"
    break;

  case 879:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12927 "Parser/parser.cc"
    break;

  case 880:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12933 "Parser/parser.cc"
    break;

  case 881:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12939 "Parser/parser.cc"
    break;

  case 882:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12945 "Parser/parser.cc"
    break;

  case 883:
#line 3547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12951 "Parser/parser.cc"
    break;

  case 884:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12957 "Parser/parser.cc"
    break;

  case 885:
#line 3551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12963 "Parser/parser.cc"
    break;

  case 886:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12969 "Parser/parser.cc"
    break;

  case 887:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12975 "Parser/parser.cc"
    break;

  case 888:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12981 "Parser/parser.cc"
    break;

  case 889:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12987 "Parser/parser.cc"
    break;

  case 890:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12993 "Parser/parser.cc"
    break;

  case 892:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12999 "Parser/parser.cc"
    break;

  case 893:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13005 "Parser/parser.cc"
    break;

  case 894:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13011 "Parser/parser.cc"
    break;

  case 895:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13017 "Parser/parser.cc"
    break;

  case 896:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13023 "Parser/parser.cc"
    break;

  case 897:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13029 "Parser/parser.cc"
    break;

  case 898:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13035 "Parser/parser.cc"
    break;

  case 899:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13041 "Parser/parser.cc"
    break;

  case 900:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13047 "Parser/parser.cc"
    break;

  case 901:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13053 "Parser/parser.cc"
    break;

  case 902:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13059 "Parser/parser.cc"
    break;

  case 903:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13065 "Parser/parser.cc"
    break;

  case 904:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13071 "Parser/parser.cc"
    break;

  case 905:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13077 "Parser/parser.cc"
    break;

  case 906:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13083 "Parser/parser.cc"
    break;

  case 907:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13089 "Parser/parser.cc"
    break;

  case 911:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13095 "Parser/parser.cc"
    break;

  case 912:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13101 "Parser/parser.cc"
    break;

  case 913:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13107 "Parser/parser.cc"
    break;

  case 914:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13113 "Parser/parser.cc"
    break;

  case 915:
#line 3639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13119 "Parser/parser.cc"
    break;

  case 916:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13125 "Parser/parser.cc"
    break;

  case 917:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13131 "Parser/parser.cc"
    break;

  case 918:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13137 "Parser/parser.cc"
    break;

  case 919:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13143 "Parser/parser.cc"
    break;

  case 920:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13149 "Parser/parser.cc"
    break;

  case 921:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13155 "Parser/parser.cc"
    break;

  case 922:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13161 "Parser/parser.cc"
    break;

  case 923:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13167 "Parser/parser.cc"
    break;

  case 924:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13173 "Parser/parser.cc"
    break;

  case 925:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13179 "Parser/parser.cc"
    break;

  case 926:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13188 "Parser/parser.cc"
    break;

  case 927:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13194 "Parser/parser.cc"
    break;

  case 928:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13200 "Parser/parser.cc"
    break;

  case 930:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13206 "Parser/parser.cc"
    break;

  case 931:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13212 "Parser/parser.cc"
    break;

  case 932:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13218 "Parser/parser.cc"
    break;

  case 933:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13224 "Parser/parser.cc"
    break;

  case 934:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13230 "Parser/parser.cc"
    break;

  case 935:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13236 "Parser/parser.cc"
    break;

  case 936:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13242 "Parser/parser.cc"
    break;

  case 937:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13248 "Parser/parser.cc"
    break;

  case 938:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13254 "Parser/parser.cc"
    break;

  case 939:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13260 "Parser/parser.cc"
    break;

  case 940:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13266 "Parser/parser.cc"
    break;

  case 941:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13272 "Parser/parser.cc"
    break;

  case 942:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13278 "Parser/parser.cc"
    break;

  case 943:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13284 "Parser/parser.cc"
    break;

  case 944:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13290 "Parser/parser.cc"
    break;

  case 945:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13296 "Parser/parser.cc"
    break;

  case 946:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13302 "Parser/parser.cc"
    break;

  case 947:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13308 "Parser/parser.cc"
    break;

  case 949:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13314 "Parser/parser.cc"
    break;

  case 950:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13320 "Parser/parser.cc"
    break;

  case 951:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13326 "Parser/parser.cc"
    break;

  case 952:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13332 "Parser/parser.cc"
    break;

  case 953:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13338 "Parser/parser.cc"
    break;

  case 954:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13344 "Parser/parser.cc"
    break;

  case 955:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13350 "Parser/parser.cc"
    break;

  case 956:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13356 "Parser/parser.cc"
    break;

  case 957:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13362 "Parser/parser.cc"
    break;

  case 958:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13368 "Parser/parser.cc"
    break;

  case 959:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13374 "Parser/parser.cc"
    break;

  case 960:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13380 "Parser/parser.cc"
    break;

  case 961:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13386 "Parser/parser.cc"
    break;

  case 962:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13392 "Parser/parser.cc"
    break;

  case 963:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13398 "Parser/parser.cc"
    break;

  case 964:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13404 "Parser/parser.cc"
    break;

  case 965:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13410 "Parser/parser.cc"
    break;

  case 966:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13417 "Parser/parser.cc"
    break;

  case 968:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13423 "Parser/parser.cc"
    break;

  case 969:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13429 "Parser/parser.cc"
    break;

  case 970:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13435 "Parser/parser.cc"
    break;

  case 971:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13441 "Parser/parser.cc"
    break;

  case 972:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13447 "Parser/parser.cc"
    break;

  case 973:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13453 "Parser/parser.cc"
    break;

  case 974:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13459 "Parser/parser.cc"
    break;

  case 975:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13465 "Parser/parser.cc"
    break;

  case 976:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13471 "Parser/parser.cc"
    break;

  case 977:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13477 "Parser/parser.cc"
    break;

  case 978:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13483 "Parser/parser.cc"
    break;

  case 979:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13489 "Parser/parser.cc"
    break;

  case 980:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13495 "Parser/parser.cc"
    break;

  case 981:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13502 "Parser/parser.cc"
    break;

  case 983:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13508 "Parser/parser.cc"
    break;

  case 984:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13514 "Parser/parser.cc"
    break;

  case 985:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13520 "Parser/parser.cc"
    break;

  case 986:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13526 "Parser/parser.cc"
    break;

  case 987:
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13532 "Parser/parser.cc"
    break;

  case 988:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13538 "Parser/parser.cc"
    break;

  case 989:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13544 "Parser/parser.cc"
    break;

  case 990:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13550 "Parser/parser.cc"
    break;

  case 991:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13556 "Parser/parser.cc"
    break;

  case 992:
#line 3879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13562 "Parser/parser.cc"
    break;

  case 993:
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13568 "Parser/parser.cc"
    break;

  case 995:
#line 3899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13574 "Parser/parser.cc"
    break;

  case 996:
#line 3901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13580 "Parser/parser.cc"
    break;

  case 997:
#line 3906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13586 "Parser/parser.cc"
    break;

  case 998:
#line 3908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13592 "Parser/parser.cc"
    break;

  case 999:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13598 "Parser/parser.cc"
    break;

  case 1000:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13604 "Parser/parser.cc"
    break;

  case 1001:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13610 "Parser/parser.cc"
    break;

  case 1003:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13616 "Parser/parser.cc"
    break;

  case 1004:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13622 "Parser/parser.cc"
    break;

  case 1005:
#line 3924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13628 "Parser/parser.cc"
    break;

  case 1006:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13634 "Parser/parser.cc"
    break;

  case 1007:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13640 "Parser/parser.cc"
    break;

  case 1008:
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13646 "Parser/parser.cc"
    break;

  case 1009:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13652 "Parser/parser.cc"
    break;

  case 1010:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13658 "Parser/parser.cc"
    break;

  case 1011:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13664 "Parser/parser.cc"
    break;

  case 1012:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13670 "Parser/parser.cc"
    break;

  case 1014:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13676 "Parser/parser.cc"
    break;

  case 1015:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13682 "Parser/parser.cc"
    break;

  case 1017:
#line 3967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13688 "Parser/parser.cc"
    break;

  case 1018:
#line 3969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13694 "Parser/parser.cc"
    break;

  case 1020:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13700 "Parser/parser.cc"
    break;

  case 1021:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13706 "Parser/parser.cc"
    break;

  case 1022:
#line 3982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13712 "Parser/parser.cc"
    break;

  case 1023:
#line 3984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13718 "Parser/parser.cc"
    break;

  case 1024:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13724 "Parser/parser.cc"
    break;

  case 1025:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13730 "Parser/parser.cc"
    break;

  case 1026:
#line 4022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13736 "Parser/parser.cc"
    break;

  case 1029:
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13742 "Parser/parser.cc"
    break;

  case 1030:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13748 "Parser/parser.cc"
    break;

  case 1031:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13754 "Parser/parser.cc"
    break;

  case 1032:
#line 4038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13760 "Parser/parser.cc"
    break;

  case 1033:
#line 4040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13766 "Parser/parser.cc"
    break;

  case 1034:
#line 4042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13772 "Parser/parser.cc"
    break;

  case 1035:
#line 4044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13778 "Parser/parser.cc"
    break;

  case 1036:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13784 "Parser/parser.cc"
    break;

  case 1038:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13790 "Parser/parser.cc"
    break;

  case 1039:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13796 "Parser/parser.cc"
    break;

  case 1040:
#line 4056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13802 "Parser/parser.cc"
    break;

  case 1041:
#line 4061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13808 "Parser/parser.cc"
    break;

  case 1042:
#line 4063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13814 "Parser/parser.cc"
    break;

  case 1043:
#line 4065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13820 "Parser/parser.cc"
    break;

  case 1045:
#line 4072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13826 "Parser/parser.cc"
    break;

  case 1047:
#line 4083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13832 "Parser/parser.cc"
    break;

  case 1048:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13838 "Parser/parser.cc"
    break;

  case 1049:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13844 "Parser/parser.cc"
    break;

  case 1050:
#line 4091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13850 "Parser/parser.cc"
    break;

  case 1051:
#line 4093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13856 "Parser/parser.cc"
    break;

  case 1052:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13862 "Parser/parser.cc"
    break;

  case 1054:
#line 4110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13868 "Parser/parser.cc"
    break;

  case 1055:
#line 4112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13874 "Parser/parser.cc"
    break;

  case 1056:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13880 "Parser/parser.cc"
    break;

  case 1057:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13886 "Parser/parser.cc"
    break;

  case 1058:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13892 "Parser/parser.cc"
    break;

  case 1059:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13898 "Parser/parser.cc"
    break;

  case 1060:
#line 4125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13904 "Parser/parser.cc"
    break;

  case 1062:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13910 "Parser/parser.cc"
    break;

  case 1063:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13916 "Parser/parser.cc"
    break;

  case 1064:
#line 4135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13922 "Parser/parser.cc"
    break;

  case 1065:
#line 4140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13928 "Parser/parser.cc"
    break;

  case 1066:
#line 4142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13934 "Parser/parser.cc"
    break;

  case 1069:
#line 4152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13940 "Parser/parser.cc"
    break;

  case 1072:
#line 4163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13946 "Parser/parser.cc"
    break;

  case 1073:
#line 4165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13952 "Parser/parser.cc"
    break;

  case 1074:
#line 4167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13958 "Parser/parser.cc"
    break;

  case 1075:
#line 4169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13964 "Parser/parser.cc"
    break;

  case 1076:
#line 4171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13970 "Parser/parser.cc"
    break;

  case 1077:
#line 4173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13976 "Parser/parser.cc"
    break;

  case 1078:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13982 "Parser/parser.cc"
    break;

  case 1079:
#line 4182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13988 "Parser/parser.cc"
    break;

  case 1080:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13994 "Parser/parser.cc"
    break;

  case 1081:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14000 "Parser/parser.cc"
    break;

  case 1082:
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14006 "Parser/parser.cc"
    break;

  case 1083:
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14012 "Parser/parser.cc"
    break;

  case 1084:
#line 4193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14018 "Parser/parser.cc"
    break;

  case 1085:
#line 4195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14024 "Parser/parser.cc"
    break;

  case 1086:
#line 4197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14030 "Parser/parser.cc"
    break;

  case 1087:
#line 4199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14036 "Parser/parser.cc"
    break;

  case 1088:
#line 4204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14042 "Parser/parser.cc"
    break;

  case 1089:
#line 4206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14048 "Parser/parser.cc"
    break;

  case 1090:
#line 4211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14054 "Parser/parser.cc"
    break;

  case 1091:
#line 4213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14060 "Parser/parser.cc"
    break;

  case 1093:
#line 4240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14066 "Parser/parser.cc"
    break;

  case 1097:
#line 4251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14072 "Parser/parser.cc"
    break;

  case 1098:
#line 4253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14078 "Parser/parser.cc"
    break;

  case 1099:
#line 4255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14084 "Parser/parser.cc"
    break;

  case 1100:
#line 4257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14090 "Parser/parser.cc"
    break;

  case 1101:
#line 4259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14096 "Parser/parser.cc"
    break;

  case 1102:
#line 4261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14102 "Parser/parser.cc"
    break;

  case 1103:
#line 4268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14108 "Parser/parser.cc"
    break;

  case 1104:
#line 4270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14114 "Parser/parser.cc"
    break;

  case 1105:
#line 4272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14120 "Parser/parser.cc"
    break;

  case 1106:
#line 4274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14126 "Parser/parser.cc"
    break;

  case 1107:
#line 4276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14132 "Parser/parser.cc"
    break;

  case 1108:
#line 4278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14138 "Parser/parser.cc"
    break;

  case 1109:
#line 4283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14144 "Parser/parser.cc"
    break;

  case 1110:
#line 4285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14150 "Parser/parser.cc"
    break;

  case 1111:
#line 4287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14156 "Parser/parser.cc"
    break;

  case 1112:
#line 4292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14162 "Parser/parser.cc"
    break;

  case 1113:
#line 4294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14168 "Parser/parser.cc"
    break;

  case 1114:
#line 4296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14174 "Parser/parser.cc"
    break;

  case 1117:
#line 4320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14180 "Parser/parser.cc"
    break;

  case 1118:
#line 4322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14186 "Parser/parser.cc"
    break;


#line 14190 "Parser/parser.cc"

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
#line 4325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
