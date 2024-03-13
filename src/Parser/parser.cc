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
#define YYLAST   26145

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  181
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  312
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1120
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2226

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
    1613,  1614,  1618,  1620,  1622,  1627,  1629,  1634,  1635,  1637,
    1642,  1644,  1649,  1651,  1653,  1655,  1658,  1662,  1665,  1669,
    1671,  1673,  1675,  1677,  1679,  1681,  1683,  1685,  1687,  1689,
    1694,  1695,  1699,  1705,  1713,  1718,  1719,  1723,  1724,  1729,
    1733,  1734,  1737,  1739,  1744,  1747,  1749,  1751,  1754,  1756,
    1761,  1766,  1767,  1771,  1776,  1778,  1783,  1785,  1790,  1792,
    1794,  1799,  1804,  1809,  1814,  1816,  1818,  1823,  1825,  1831,
    1832,  1836,  1837,  1838,  1839,  1843,  1848,  1849,  1851,  1853,
    1855,  1859,  1863,  1864,  1868,  1870,  1872,  1874,  1876,  1882,
    1883,  1889,  1890,  1894,  1895,  1900,  1902,  1911,  1912,  1914,
    1919,  1924,  1935,  1936,  1940,  1941,  1947,  1948,  1952,  1954,
    1958,  1960,  1964,  1965,  1969,  1970,  1974,  1975,  1976,  1980,
    1982,  1997,  1998,  1999,  2000,  2002,  2006,  2008,  2012,  2019,
    2021,  2023,  2028,  2029,  2031,  2033,  2035,  2045,  2047,  2059,
    2062,  2067,  2069,  2075,  2080,  2085,  2096,  2103,  2108,  2110,
    2112,  2118,  2122,  2129,  2131,  2132,  2133,  2149,  2151,  2154,
    2156,  2159,  2164,  2165,  2169,  2170,  2171,  2172,  2181,  2182,
    2183,  2192,  2193,  2194,  2198,  2199,  2200,  2209,  2210,  2211,
    2216,  2217,  2226,  2227,  2232,  2234,  2238,  2240,  2242,  2244,
    2251,  2256,  2261,  2262,  2264,  2274,  2275,  2280,  2282,  2284,
    2286,  2288,  2290,  2293,  2295,  2297,  2302,  2308,  2310,  2312,
    2314,  2316,  2318,  2320,  2322,  2324,  2326,  2328,  2330,  2332,
    2334,  2336,  2338,  2340,  2342,  2344,  2346,  2348,  2350,  2352,
    2354,  2356,  2358,  2360,  2362,  2367,  2368,  2372,  2378,  2379,
    2385,  2386,  2388,  2390,  2392,  2397,  2399,  2404,  2405,  2407,
    2409,  2414,  2416,  2418,  2420,  2422,  2424,  2429,  2430,  2432,
    2434,  2439,  2441,  2440,  2444,  2452,  2453,  2455,  2457,  2462,
    2463,  2465,  2470,  2471,  2473,  2475,  2480,  2482,  2484,  2489,
    2491,  2493,  2495,  2496,  2498,  2503,  2505,  2507,  2512,  2513,
    2517,  2518,  2525,  2524,  2529,  2528,  2538,  2537,  2548,  2547,
    2557,  2562,  2563,  2568,  2574,  2592,  2593,  2597,  2599,  2601,
    2606,  2608,  2610,  2612,  2617,  2619,  2624,  2626,  2635,  2636,
    2641,  2650,  2655,  2657,  2659,  2668,  2670,  2671,  2672,  2674,
    2676,  2677,  2682,  2683,  2684,  2689,  2691,  2694,  2697,  2704,
    2705,  2706,  2712,  2717,  2719,  2725,  2726,  2732,  2733,  2737,
    2744,  2746,  2749,  2748,  2752,  2754,  2761,  2763,  2767,  2770,
    2769,  2780,  2784,  2788,  2792,  2797,  2798,  2803,  2808,  2816,
    2818,  2820,  2822,  2827,  2828,  2834,  2835,  2836,  2845,  2846,
    2848,  2849,  2854,  2855,  2856,  2858,  2864,  2865,  2867,  2868,
    2869,  2871,  2873,  2880,  2881,  2883,  2885,  2890,  2891,  2900,
    2902,  2907,  2909,  2914,  2915,  2917,  2920,  2922,  2926,  2927,
    2928,  2930,  2932,  2940,  2942,  2947,  2948,  2949,  2953,  2954,
    2955,  2960,  2961,  2966,  2967,  2968,  2969,  2973,  2974,  2979,
    2980,  2981,  2982,  2983,  2997,  2998,  3003,  3004,  3010,  3012,
    3015,  3017,  3019,  3042,  3043,  3049,  3050,  3056,  3055,  3065,
    3064,  3068,  3074,  3076,  3086,  3087,  3089,  3093,  3098,  3100,
    3102,  3104,  3110,  3111,  3115,  3116,  3121,  3123,  3130,  3132,
    3133,  3135,  3140,  3142,  3144,  3149,  3151,  3156,  3161,  3169,
    3174,  3176,  3181,  3186,  3187,  3192,  3193,  3197,  3198,  3199,
    3204,  3206,  3212,  3214,  3219,  3221,  3227,  3228,  3232,  3236,
    3240,  3242,  3256,  3258,  3260,  3262,  3264,  3266,  3268,  3269,
    3274,  3277,  3276,  3288,  3287,  3300,  3299,  3313,  3312,  3326,
    3325,  3341,  3347,  3349,  3355,  3356,  3367,  3374,  3379,  3385,
    3388,  3391,  3395,  3401,  3404,  3407,  3412,  3413,  3414,  3415,
    3419,  3427,  3428,  3440,  3441,  3445,  3446,  3451,  3453,  3455,
    3460,  3461,  3467,  3468,  3470,  3475,  3476,  3477,  3478,  3479,
    3481,  3516,  3518,  3523,  3525,  3526,  3528,  3533,  3535,  3537,
    3539,  3544,  3546,  3548,  3550,  3552,  3554,  3556,  3561,  3563,
    3565,  3567,  3576,  3578,  3579,  3584,  3586,  3588,  3590,  3592,
    3597,  3599,  3601,  3603,  3608,  3610,  3612,  3614,  3616,  3618,
    3630,  3631,  3632,  3636,  3638,  3640,  3642,  3644,  3649,  3651,
    3653,  3655,  3660,  3662,  3664,  3666,  3668,  3670,  3682,  3687,
    3692,  3694,  3695,  3697,  3702,  3704,  3706,  3708,  3713,  3715,
    3717,  3719,  3721,  3723,  3725,  3730,  3732,  3734,  3736,  3745,
    3747,  3748,  3753,  3755,  3757,  3759,  3761,  3766,  3768,  3770,
    3772,  3777,  3779,  3781,  3783,  3785,  3787,  3797,  3799,  3802,
    3803,  3805,  3810,  3812,  3814,  3819,  3821,  3823,  3825,  3830,
    3832,  3834,  3848,  3850,  3853,  3854,  3856,  3861,  3863,  3868,
    3870,  3872,  3877,  3879,  3884,  3886,  3903,  3904,  3906,  3911,
    3913,  3915,  3917,  3919,  3924,  3925,  3927,  3929,  3934,  3936,
    3938,  3944,  3946,  3949,  3956,  3958,  3967,  3969,  3971,  3972,
    3974,  3976,  3980,  3982,  3987,  3989,  3991,  3993,  4028,  4029,
    4033,  4034,  4037,  4039,  4044,  4046,  4048,  4050,  4052,  4057,
    4058,  4060,  4062,  4067,  4069,  4071,  4077,  4078,  4080,  4089,
    4092,  4094,  4097,  4099,  4101,  4115,  4116,  4118,  4123,  4125,
    4127,  4129,  4131,  4136,  4137,  4139,  4141,  4146,  4148,  4156,
    4157,  4158,  4163,  4164,  4169,  4171,  4173,  4175,  4177,  4179,
    4186,  4188,  4190,  4192,  4194,  4197,  4199,  4201,  4203,  4205,
    4210,  4212,  4214,  4219,  4245,  4246,  4248,  4252,  4253,  4257,
    4259,  4261,  4263,  4265,  4267,  4274,  4276,  4278,  4280,  4282,
    4284,  4289,  4291,  4293,  4298,  4300,  4302,  4320,  4322,  4327,
    4328
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

#define YYPACT_NINF (-1881)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1119)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     135, 13202,   141,   170, 19813,    66, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881,    45,   984,
     138, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881, -1881, -1881, -1881,   406,   264,
   -1881, -1881, -1881, -1881, -1881, -1881,  5477,  5477,   151, 13202,
     172,   178, 23172, -1881,   184, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881,   229,  4266, -1881,   819,   240,
   -1881, -1881, -1881, -1881, -1881, 19348, -1881, -1881,   275,   306,
     477,   155, -1881,  5477,   368,   398,   433,   425,  5735,   614,
     958, 13367, -1881, -1881,   562, 19193,  2602, -1881, -1881, -1881,
   -1881,  3494,   657,  8599, 11612,   930,  3494,   975,   543, -1881,
   -1881, -1881, -1881,   259, -1881, -1881, -1881, -1881,   508, -1881,
   -1881, -1881, -1881, -1881,   561,   570,   259, -1881,   259, 17511,
   -1881, -1881, -1881, 20988,  5477, -1881, -1881,  5477, -1881, 13202,
   -1881,   571, 21041, -1881, -1881,  5749, 22185, -1881, -1881,  1378,
    1378,   673,  3136, -1881, -1881, -1881, -1881,   479, 15807,  3989,
     259, -1881, -1881, -1881, -1881, -1881, -1881,   675, -1881,   645,
     713,  2998, -1881,   733, 25594, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881, 17890,  3439,  5152,  4266,   761,   785,   811,   828,
     855,   869,   878, -1881, -1881, 19968, 12027,   730,   749, -1881,
   20271, -1881, -1881, -1881, -1881,   824, -1881, -1881,   839, -1881,
    8102,  1029, 23694, -1881,   899,  5477,   570,   907,  1345,  5749,
    1345, -1881, -1881, -1881,  3727,  4168,   910,   976,   193,   976,
   -1881,   259,   259,    92, 17189,   309,   976, -1881,   259,   259,
      92,   259, -1881,   259, -1881,  4592, -1881, -1881,   937,   950,
    1378, 22929,   956, 19348, -1881, -1881,  3494, -1881,  1664,   543,
     945,  1052, 17189,  5477,  5477,   477, -1881, 14987, -1881,  1378,
    1378,   971,  1052, 17189,  5477, -1881, 23402, -1881, -1881, -1881,
    1378, -1881, -1881, -1881, -1881,  1378, -1881,  1045,  3715,  5477,
   -1881, 19047,  1011, -1881, -1881, -1881, 22792,   570, 17350,  1010,
    5749, 18994, 22929,  3494, -1881, -1881, 22333, -1881,   976,    62,
   -1881, 25594, 22185,  4372,  4592, -1881,   603, -1881, -1881, -1881,
   -1881, -1881, 21041,  5477, -1881,  1040,  1050, -1881, -1881, -1881,
   -1881,  5477,  3579,   598,   693, -1881,  5477,   645, -1881,  1055,
     259,   259,  1060, 21196,   742, 16299, 22982,  3494, -1881,  3494,
    1378,  3494,  1378, -1881, -1881,   259, -1881, -1881,  1053, 21249,
   -1881, -1881, -1881, 21404,   824, -1881,   380,   747,   129,  1059,
     426,   543,  1083, -1881,  3136,  1048,   645,  3136, -1881, -1881,
   -1881, -1881, -1881,  3439, -1881,   723, -1881,  1074, -1881,  1103,
    1148, 25670,  1116,  1153,  1164, 25594, 25746,  1179, 23287, -1881,
   -1881, -1881, -1881, -1881, -1881, 25822, 25822, 17731,  1175,  5916,
   -1881, -1881, -1881, -1881,   611, -1881,   660, -1881,  1632, -1881,
   25594, 25594, -1881,  1169,   775,  1097,  1163,   393,  1193,  1199,
    1207,  1212,  1218,   114, -1881,   908, -1881,  1238, -1881,  1156,
    5132, 18367, -1881, -1881,   447,  1238, -1881, -1881,   925, -1881,
   -1881,   926,  5152,  1242,  1245,  1250,  1252,  1255,  1270, -1881,
   -1881,   627,  1272, -1881,   646,  1272,  1278, -1881,  1297, -1881,
   20988, -1881,  1257,  1306, 18526, -1881, -1881,  4699,  5372,  1331,
   16299,  1355,  1184,  1201,  1327,  1333, -1881, -1881, -1881,  5477,
    4884, 20625, -1881, -1881, -1881, -1881, -1881, -1881, 18853,  4401,
    1175,  8102,  1314,  1343, -1881, -1881,  1348, 23694,   894, -1881,
   -1881, -1881, 23770,  1358, -1881, -1881, -1881, -1881,  1336,  3727,
     940,  1382,  1385,  1415,   960,  1419,  1426,  1434,  1439,  1443,
    1448,  4168, -1881, -1881, -1881,   259,  1366,  1365,  1392, -1881,
   -1881,  1453,   477, -1881, -1881,   570,  1052, 20132, -1881, -1881,
     477, -1881, -1881,   570, -1881, -1881,  4592, -1881, 18367, 18367,
   -1881,  1378,  5749, 23119,  3254, 16463, -1881, -1881, -1881, -1881,
   -1881,   570,  1052,    62,  1454, -1881, -1881,  3494,  1467,  1052,
   17189, -1881,   570,  1052, -1881, 23453, -1881,  1378,  1378, -1881,
   -1881,  1468,   326,  1470,   543,  1478, -1881, -1881, -1881, 20572,
    1480,  1417, -1881, -1881,   943, -1881,  1574, -1881,  1466, -1881,
   -1881, -1881, 21568, 25898, -1881, -1881, -1881, -1881, -1881,  4372,
     973,  4592, 20132, 16627,   976, 13202, -1881,  5477,  1494, -1881,
    1501, -1881, -1881, -1881, -1881, -1881,  3136, -1881, -1881,  1580,
    4541,  3974, 20780, 12027, -1881, 21621, -1881,  1378,  1378, -1881,
   -1881,   824, -1881, 15315,  1500,  1648, 25594,  2411,  1453,  1488,
   -1881,   259,   259, -1881,  1272, -1881, 21196, -1881, -1881, 20572,
    1378,  1378, -1881,  4541,   259, -1881,  8405, -1881, -1881, 21249,
   -1881,   479, -1881, -1881, -1881,  1509,  5477,   129,  1083,  1510,
     953, 21041,   987,   990, -1881,  3439, 23846,  1492, -1881, 18049,
   -1881,  5916, 21776, 21041, -1881, 18049, -1881, 25594, -1881, -1881,
   -1881, -1881, -1881, -1881, 18208, -1881, -1881,  3958, 21776, 21776,
    1281,  1315,  1547,   738,  1834, -1881,  1017,  1517,  1308,  1519,
   -1881, 23770, 25594, 23922,  1516, 25594,  1345, 25594,  1345, -1881,
    2753, -1881, -1881, 23846,  2254, 25594, 23846,  1345, -1881, -1881,
   25594, 25594, 25594, 25594, 25594, 25594, 25594, 25594, 25594, 25594,
   25594, 25594, 25594, 25594, 25594, 25594, 25594, 25594, 25594, 23998,
    1503,   733,  3818, 12027, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881, -1881, -1881,  1518, 25594, -1881, -1881, 15479,
    2484, -1881, -1881,   259,   259, -1881, -1881, 18367, -1881, -1881,
     636,  1272, -1881,  1030,  1272, 20132, -1881, -1881,  1453, 20132,
   -1881,  1453, -1881, 25974, -1881, -1881, -1881, 19658, 12027,  1520,
    1320,  1527, 14823,  1672,  3497,   658,  1488, -1881,   259,   259,
    1488,   734, -1881,   259,   259, 25594,  5477, 16463,  1531, 16463,
    1532,  1488,   242, 15643, 15643, 15643,  5477, -1881, -1881, 25594,
    1348, -1881,  8102,  1540, -1881,  2365, -1881, -1881, -1881, -1881,
   -1881,  1035, -1881, 15643, 25594,  1064,  1538,  1542,  1546,  1079,
    1550,  1554,  1557,  1560,  1561,  1562,   756,  1272, -1881, -1881,
     767,  1272, -1881, -1881,   802,  1272, -1881, -1881, -1881,  5749,
     733,  1675,  1272, 22481, -1881, -1881,   570,  1573, -1881, -1881,
   -1881,  1086,  1575,  1088,  1579, -1881,  1278,  1571,  1583, -1881,
     570, -1881,  1586, -1881,   570,  1052,  1583, -1881,   570,  1593,
    1598,  1599, -1881, -1881, 20435, -1881,  1345,  5477, 11079,  1693,
   -1881,  1306, -1881, 15643,  1091,  1581, -1881,  1583,  1607, -1881,
   21829, 18367,  1587, -1881,  1587, -1881, -1881, -1881,   129,  1605,
     259,   259, -1881, 21249, -1881, 12195, 18685, -1881,  1612,  1613,
    1615,  1621, -1881,  5289,   259, -1881,  2411, -1881, -1881, -1881,
   -1881,  1453, -1881, -1881, -1881,  1378, -1881,  4095, -1881, -1881,
     543,   548,  1625,  1603,  1509,  1622,   129, -1881, -1881,  1623,
    1627, -1881, -1881,  1101, -1881, -1881, -1881, -1881,  1631,  1636,
    1628,  1634,  1637,  1640,  1644,  1646,  1643,  1649, 25594,  1650,
    1653,  1654, 21984, 12363, 25594, -1881, -1881,  1942, -1881, -1881,
   -1881, 25594, -1881,  1655,  1656, 23618,  1323, -1881, 23846,  1645,
   -1881,  1657, -1881, -1881,  3226, -1881,  1106, -1881, -1881, -1881,
    3226, -1881, -1881,  1326,   698, -1881, -1881,  1169,  1169,  1169,
     775,   775,  1097,  1097,  1163,  1163,  1163,  1163,   393,   393,
    1193,  1199,  1207,  1212,  1218, 25594,  1329, -1881,  1659,  3226,
   -1881, -1881,  8102, -1881,  1662,  1673,  1674,  1679,  2484, -1881,
   -1881, -1881, -1881, -1881, 20132, -1881, -1881,  1453, 20132, -1881,
    1453,  1680,  1682, 15643, 15643, -1881, -1881, 14823,  1027,  1683,
    1686,  1688,  1700,  2040,  3497, -1881, -1881, 20132, -1881, -1881,
   -1881, -1881, -1881, -1881, 20132, -1881, -1881, -1881, -1881,  1698,
   -1881,  1488,  1671, -1881, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881,  1704,  1701,  1702, -1881, -1881,   477,  3226,  1337,    40,
   -1881, -1881,  1660, -1881, 23694, -1881, 25594,   259, 24074, 15643,
   -1881, -1881,   822,  1272, -1881,   862,  1272, -1881, -1881,   882,
    1272, 20132, -1881, -1881,  1453, 20132, -1881, -1881,  1453, 20132,
   -1881, -1881,  1453,   976,  1707, -1881,  1453,   -16, -1881,  1238,
    1709, -1881, -1881, -1881, -1881, -1881, -1881,  1711, -1881, -1881,
   -1881, 21829,  1583, -1881,   570, -1881, -1881, -1881, -1881, -1881,
   14020, -1881, -1881, -1881, -1881,   116, -1881,   659,   441, 11859,
    1716,  1718, 17011,  1720,  1721,  3470,  3606,  3155, 24150,  1722,
   -1881, -1881,  1725,  1726, 17011,  1728, -1881, -1881,   570, 25594,
   25594,  1874,  1727,   769, -1881, 17572,  1339,  1729,  1713, -1881,
   -1881, -1881, 10901, -1881, -1881, -1881, -1881, -1881,  1635, -1881,
   -1881, -1881,  1412,    30, -1881,   329, -1881,    30, -1881, -1881,
   -1881, -1881, -1881,  1345, -1881, -1881, 13532, 19503,  1730, -1881,
    5477,  1734,  1736, -1881, 16627, -1881, -1881,  5477, -1881, -1881,
    5749, -1881, -1881,  1731,  1737,  1152, 21041,   645,   645,  1509,
     129,  1083,  1083, -1881, -1881,  1175,  1306, 18526, -1881,  1238,
   -1881, 12531, -1881,   921,  1272, -1881,  1378, 10176, -1881, -1881,
     129,  1739,   259,   259,   479,  5477, -1881, 24226, -1881,  1746,
     129,  1509,  1753, -1881, -1881, 23846,   809, -1881, 20572, 12363,
    1345, -1881,   809, -1881, 20833,   809, -1881, 25594, 25594, 25594,
   -1881, -1881, -1881, -1881, 25594, 25594,  1745,  8102, -1881, -1881,
    1757,   815, -1881, -1881, -1881,  2586, -1881, -1881,  1347, -1881,
     146, -1881,  1351, -1881, 23770, -1881, -1881, 25594,  1741,  1359,
    1364,  1348, -1881,   948,  1272, -1881, -1881,  1765,  1768, -1881,
   -1881, -1881, -1881,  1769,  1001,  1272, -1881,  1003,  2941,   259,
     259, -1881, -1881,  1770,  1771, -1881,  1778, -1881, 16463,  1779,
   -1881, 15971, 16135,  1786,  1787, -1881,  1788, 25594, 25594,  1368,
    1791, -1881, -1881, -1881, -1881, -1881, -1881, -1881,  1790, 20132,
   -1881, -1881,  1453, 20132, -1881, -1881,  1453, 20132, -1881, -1881,
    1453,  1795,  1805,  1807,   477,   259, -1881, -1881,  1373, 25594,
   22633,  1806,  1794, -1881, -1881, -1881,  1814, 14178, 14336, 14494,
   21829, 22929, 21776, 21776,  1817, -1881,   311,   418,  1712,  6489,
   -1881,   436,  5477,  5477, -1881, 23846,   334,   346, -1881, -1881,
   -1881, -1881, 11859, 25594,  1819,  1889, 11690, 11257, -1881,  1796,
   -1881,  1798, 25594,  1799,  8102,  1800, 25594, 23770, 25594, -1881,
   11435,  1189, -1881,  1803,   -14, -1881,    88,  1893,   299, -1881,
    1832, -1881,  1808, -1881,  1811,  1838,  1841, 17011, 17011, -1881,
   -1881,  1908, -1881, -1881,    21,    21,  1005, 15151,   259,   511,
   -1881, -1881,  1843,  1849,   598, -1881,  1850, -1881,  1844, -1881,
    1846, -1881, -1881, -1881, -1881,  1854,  1509,  1848,  1853, 12699,
    1857,  1858,  1859, -1881, 20132, -1881, -1881,  1453, 25594, 25594,
    1306,  1861, -1881,  1509,   129, -1881,  1083,   100,  1603,  8102,
   -1881, -1881,  1509,  1855, -1881, 21829, -1881,   644,  1856,  1860,
    1161, -1881,  1862, -1881, -1881, -1881, -1881, -1881,  8102,  1348,
   23770, -1881,  1900,  3226, -1881,  1900,  1900, -1881,  3226,  2812,
    2848, -1881,  1377, -1881, -1881, -1881,  1871, 20132, -1881, -1881,
    1453, -1881, -1881,  1869,  1872,   259, 20132, -1881, -1881,  1453,
   20132, -1881, -1881,  1875, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881,  1671, -1881, -1881, -1881,  1873, -1881, -1881, -1881,
   -1881,  1876,  1877,   259,  1878,  1882,  1884, -1881, -1881, -1881,
   -1881, -1881, 25594, -1881,   -16, -1881,  1238, -1881, -1881,  1891,
    1892, -1881,  1817,  1817,  1817,  4989,   873,  1867,   549, -1881,
    4989,   616, 18367, -1881, -1881, -1881,  4762, 25594,  4670,   392,
   -1881, -1881,   287,  1886,  1886,  1886,  5477, -1881, -1881, -1881,
    1185, -1881, -1881, -1881, -1881,  1729,  1895, 25594,   275,  1888,
     425, 14659, 21829,  1186,  1899, 17011,  1898, -1881, -1881, -1881,
    1057, 17011, 25594,  1022,   757, -1881, 25594, 23539, -1881, -1881,
     622, -1881,  1348, -1881,  1187,  1196,  1197,   837, -1881, -1881,
   -1881, -1881,   570,  1189,  1905, -1881, -1881, 25594, -1881,  1907,
     733, -1881, -1881, -1881, -1881, 25594, 25594, -1881, -1881,   460,
      21, -1881,   464, -1881, -1881, 10703, -1881,   259, -1881,  1587,
   -1881, 21829, -1881, -1881, -1881,  1896,   129,   129, -1881, -1881,
   -1881,  1903,  1909, -1881, -1881,  1904, -1881,  1910,  1913,  1509,
    1083,  1912, -1881, -1881,  1348,  1918, -1881, -1881,  1916, -1881,
   -1881, 25594, -1881, 20833, 25594,  1348,  1924,  1387, -1881,  1389,
   -1881,  3226, -1881,  3226, -1881, -1881, -1881,  1922,   259,   259,
    1923,  1925, -1881,  1921, -1881, -1881, -1881, -1881, -1881,  1395,
   25594, -1881, -1881, -1881, -1881, -1881,   677,   873,  2644,   685,
   -1881, -1881, -1881, -1881,   259,   259, -1881, -1881, -1881,   771,
   -1881,  1202,  4762,  1018, -1881,  4670, -1881,   259, -1881, -1881,
   -1881, -1881, -1881, -1881, 17011, 17011,  1729, 16791,   359, 24302,
    2010, 17011, -1881, -1881, -1881, -1881, 25594, -1881, 24378,  2012,
    1914, 23458, 24454, 17011, 11435,  1729,   692,  1081,  1915, 25594,
   -1881,  1934,   440, 17011, -1881, 17011, -1881,  1937, -1881, -1881,
    1920,   733,   853,  1939,  1408,  1216, 17011,  1943, 17011, 17011,
   17011, -1881, -1881, -1881,   645, -1881,  5477,  5749, -1881,  1509,
    1509, -1881, -1881,  1941,  1945, -1881, -1881, -1881,  1953,  1947,
     129,  1954, -1881,  1957, -1881, -1881, -1881, -1881,  1959, -1881,
   -1881, -1881,  1413,  1421, -1881, -1881, -1881, -1881, -1881, -1881,
   -1881, -1881, -1881,  1958,  1962,  1963,  2644, -1881,   259, -1881,
   -1881, -1881, -1881, -1881,  1955,  4989, -1881,  2041,  4903,    68,
   12870, -1881, 16888, -1881,     8,  1224, 17011,  2046,   779,  1952,
     430, 17011, 25594,  2017,  1971,   692,  1081,  1950, -1881, 24530,
    1964,   513,  2059, -1881, 24606, 24682, 25594,  1729,  1961, 13037,
   -1881, -1881, -1881, -1881, 22037, -1881,  1975,  1965,    24, -1881,
   25594, 23846, -1881, -1881, 25594,    30, -1881, -1881, -1881, -1881,
   -1881,  1985,  1989, -1881, -1881, -1881,   129,  1509, -1881, -1881,
   -1881, -1881, -1881,  1021,  1272, -1881, -1881,   873, -1881, 17011,
   -1881,   153, -1881,    75, -1881, -1881, -1881,  2007, 13697, -1881,
   -1881, 17011, -1881,    46, -1881, 17011, 25594,  2006, 24758, -1881,
   -1881, -1881, 24834, 24910, 25594,  1729, -1881, 24986, 25062, 17011,
    1992,   529,  1994,   554, -1881, -1881,  2015, 13697, 22037, -1881,
    5500, 21621,  1345,  2008, -1881,  2067,  2022,   854,  2019, -1881,
   -1881,  1229,  1244,   383, -1881, -1881,  1509,  2024, 20132, -1881,
   -1881,  1453, -1881, -1881, 25594, -1881, 25594, -1881, -1881,  1511,
   13862, -1881, -1881, 17011, -1881, -1881,  1729, -1881, -1881,  1729,
    2013,   566,  2014,   605, -1881, -1881,  1729, -1881,  1729, -1881,
    2027, 25138, 25214, 25290, -1881,  1511, -1881,  2009,  3593,  4062,
   -1881, -1881, -1881,    24,  2028, 25594,  2011,    24,    24, -1881,
   -1881, 17011,  2117,  2050, -1881,  2037, -1881, -1881, 16888, -1881,
    1511, -1881, -1881,  2049, 25366, 25442, 25518, -1881, -1881,  1729,
   -1881,  1729, -1881,  1729, -1881,  2009, 25594,  2051,  4062,  2044,
     733,  2054, -1881,   865, -1881, -1881, 17011, -1881, -1881, -1881,
   10364,  2053, 16888, -1881, -1881,  1729, -1881,  1729, -1881,  1729,
    2058,  2057, -1881,   570,   733,  2060, -1881,  2036,   733, -1881,
   -1881, -1881, -1881, 10571, -1881,   570, -1881, -1881,  1433, 25594,
   -1881,  1262, -1881,   733,  1345,  2066,  2047, -1881, -1881,  1263,
   -1881, -1881,  2048,  1345, -1881, -1881
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
     530,   531,   532,   533,   534,   535,   542,   543,   853,   545,
     618,   619,   622,   624,   620,   626,     0,     0,     0,   490,
       0,     0,    17,   589,   595,     9,    10,    11,    12,    13,
      14,    15,    16,   810,   107,     0,     0,    20,     0,     2,
     105,   106,    18,    19,   871,   490,   811,   428,     0,   431,
     733,   433,   442,     0,   432,   464,   465,     0,     0,     0,
       0,   572,   492,   494,   500,   490,   502,   505,   557,   516,
     544,   474,   550,   555,   476,   567,   475,   582,   586,   592,
     571,   598,   610,   853,   615,   616,   599,   674,   434,   435,
       3,   818,   831,   495,     0,     0,   853,   893,   853,   490,
     910,   911,   912,   490,     0,  1097,  1098,     0,     1,   490,
      17,     0,   490,   453,   454,     0,   572,   500,   484,   485,
     486,   821,     0,   621,   623,   625,   627,     0,   490,     0,
     854,   855,   617,   546,   726,   727,   725,   787,   782,   772,
       0,   862,   819,     0,     0,   507,   812,   816,   817,   813,
     814,   815,   490,   862,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   590,   593,   490,   490,     2,     0,  1099,
     572,   900,   918,  1103,  1096,  1094,  1101,   427,     0,   169,
     739,   168,     0,   436,     0,     0,     0,     0,     0,     0,
       0,   426,   987,   988,     0,     0,   463,   851,   853,   851,
     874,   853,   853,   473,   490,   853,   851,   931,   853,   853,
     472,   853,   950,   853,   928,     0,   565,   566,     0,     0,
     490,   490,     2,   490,   443,   493,   503,   558,     0,   587,
       0,   834,   490,     0,     0,   733,   444,   572,   551,   568,
     583,     0,   834,   490,     0,   506,   552,   559,   560,   477,
     569,   479,   480,   478,   574,   584,   588,     0,   602,     0,
     804,   490,     2,   832,   892,   894,   490,     0,   490,     0,
       0,   572,   490,   502,     2,  1107,   572,  1110,   851,   851,
       3,     0,   572,     0,     0,   456,   853,   846,   848,   847,
     849,     2,   490,     0,   808,     0,     0,   768,   770,   769,
     771,     0,     0,   764,     0,   753,     0,   762,   774,     0,
     853,   853,     2,   490,  1119,   491,   490,   481,   550,   482,
     575,   483,   582,   579,   600,   853,   601,   718,     0,   490,
     719,  1072,  1073,   490,   720,   722,   589,   595,   675,     0,
     677,   678,   675,   856,     0,   785,   773,     0,   870,   869,
     865,   867,   868,   862,   866,     0,   860,   863,    22,     0,
      21,     0,     0,     0,     0,     0,     0,     0,    24,    26,
       4,     8,     5,     6,     7,     0,     0,   490,     2,     0,
     108,   109,   110,   111,    90,    25,    91,    43,    89,   112,
       0,     0,   127,   129,   133,   136,   139,   144,   147,   149,
     151,   153,   155,   157,   160,     0,    27,     0,   596,     2,
     112,   490,   161,   779,   729,   586,   731,   778,     0,   728,
     732,     0,     0,     0,     0,     0,     0,     0,     0,   872,
     898,   853,   908,   916,   920,   926,   589,     2,     0,  1105,
     490,  1108,     2,   105,   490,     3,   717,     0,  1119,     0,
     491,   550,   575,   582,     3,     3,   713,   703,   707,   719,
     720,   490,     2,   901,   919,  1095,     2,     2,    24,     0,
       2,   739,    25,     0,   737,   740,  1117,     0,     0,   746,
     735,   734,     0,     0,   836,     2,   455,   457,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   877,   934,   957,   853,     0,   469,     2,   873,
     881,  1015,   733,   875,   876,     0,   834,   490,   930,   938,
     733,   932,   933,     0,   949,   951,     0,   459,   490,   490,
     556,   491,     0,   572,     0,   490,  1100,  1104,  1102,   573,
     808,     0,   834,   851,     0,   437,   445,   504,     0,   834,
     490,   808,     0,   834,   783,   553,   554,   570,   585,   591,
     594,   589,   595,   613,   614,     0,   784,   689,   723,   491,
       0,   690,   692,   693,     0,   209,   420,   833,     0,   418,
     473,   472,   572,     0,   439,     2,   440,   805,   461,     0,
       0,     0,   490,   490,   851,   490,   808,     0,     0,     2,
       0,   767,   766,   765,   759,   501,     0,   757,   775,   548,
       0,     0,   490,   490,  1074,   491,   487,   488,   489,  1078,
    1069,  1070,  1076,   490,     2,   106,     0,  1034,  1048,  1119,
    1030,   853,   853,  1039,  1046,   711,   490,   580,   721,   491,
     576,   577,   581,     0,   853,  1084,   491,  1089,  1081,   490,
    1086,     0,   684,   676,   683,  1117,     0,   675,   675,     0,
       0,   490,     0,     0,   858,   862,    69,     0,    23,   490,
      97,     0,   490,   490,    92,   490,    99,     0,    33,    37,
      38,    34,    35,    36,   490,    95,    96,   490,   490,   490,
       2,   108,   109,     0,     0,   187,     0,     0,   616,     0,
    1094,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,    64,    65,    69,     0,     0,    69,     0,    93,    94,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   490,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   168,     0,   166,   167,   490,
     999,   730,   996,   853,   853,  1004,   597,   490,   859,   899,
     853,   909,   917,   921,   927,   490,   902,   904,   906,   490,
     922,   924,     2,     0,     2,  1106,  1109,   490,   490,     0,
       2,     0,   490,   106,  1034,   853,  1119,   969,   853,   853,
    1119,   853,   984,   853,   853,     3,   721,   490,     0,   490,
       0,  1119,  1119,   490,   490,   490,     0,     2,   748,     0,
    1117,   745,  1118,     0,   741,     0,     2,   744,   747,   184,
     183,     0,     2,   490,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   853,   886,   890,   929,
     853,   943,   947,   955,   853,   965,   878,   935,   958,     0,
       0,     0,  1011,     0,   467,   837,     0,     0,   468,   838,
     460,     0,     0,     0,     0,   458,     0,     2,     2,   839,
       0,   441,     2,   808,     0,   834,     2,   840,     0,     0,
       0,     0,   628,   895,   490,   913,     0,     0,   490,   421,
     419,   105,     3,   490,     0,     3,   809,     2,     0,   761,
     490,   490,   755,   754,   755,   549,   547,   677,   675,     0,
     853,   853,  1080,   490,  1085,   491,   490,  1071,     0,     0,
       0,     0,  1049,     0,   853,  1120,  1035,  1036,   712,  1032,
    1033,  1047,  1075,  1079,  1077,   578,   613,     0,  1083,  1088,
     680,   675,     0,   685,  1117,     0,   675,   788,   786,     0,
       0,   861,    73,     0,    70,    71,    74,   820,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   490,   490,     0,   126,   125,     0,   122,   121,
      28,     0,    29,     0,     0,     0,     0,     3,    69,     0,
      52,     0,    53,    62,     0,    61,     0,    55,    56,    57,
       0,    54,    60,     0,     0,    51,   128,   130,   131,   132,
     134,   135,   137,   138,   142,   143,   140,   141,   145,   146,
     148,   150,   152,   154,   156,     0,     0,   430,     0,     0,
      30,     3,   739,   162,     0,     0,     0,     0,  1000,  1001,
     997,   998,   781,   780,   490,   903,   905,   907,   490,   923,
     925,     0,     0,   490,   490,  1025,  1024,   490,     0,     0,
       0,     0,     0,   853,  1035,   972,   989,   490,   967,   975,
     709,   970,   971,   710,   490,   982,   992,   985,   986,     0,
       3,  1119,     3,   705,   451,   704,   708,  1111,   714,   715,
     697,     0,   698,   699,     3,     3,   733,     0,   160,     0,
       3,     3,     0,   742,     0,   736,     0,   853,     0,   490,
       3,   462,   853,   887,   891,   853,   944,   948,   956,   853,
     966,   490,   879,   882,   884,   490,   936,   939,   941,   490,
     959,   961,   963,   851,     0,   470,  1012,     3,  1016,  1017,
       3,   842,   952,   562,   561,   564,   563,     2,   809,   843,
     790,   490,     2,   841,     0,   809,   844,   628,   628,   628,
     490,   691,   694,   695,   724,     0,   424,     0,     0,   490,
       0,     0,   345,     0,     0,     0,     0,     0,   189,     0,
     340,   341,     0,     0,   345,     0,   393,   392,     0,   164,
     164,   399,   589,   595,   206,   490,     2,   190,     0,   217,
     191,   192,   490,   211,   193,   194,   195,   196,     0,   197,
     198,   346,     0,   360,   199,   366,   368,   371,   200,   201,
     202,   203,   204,     0,   205,   213,   572,   490,     0,   215,
       0,     0,     0,     3,   490,   822,   809,     0,   797,   798,
       0,     3,   793,     3,     3,     0,   490,   772,   772,  1117,
     675,   675,   675,  1082,  1087,     2,   105,   490,     3,   587,
       3,   491,  1043,   853,  1042,  1045,   490,     3,  1031,  1037,
     675,     0,   853,   853,     0,     0,   660,     0,   679,     0,
     675,  1117,     2,   857,   864,     0,    98,   101,   490,   490,
       0,   104,   100,   102,   490,     0,   116,     0,     0,     0,
     120,   124,   123,   188,     0,     0,     0,   739,   113,   181,
       0,     0,    46,    47,    87,     0,    87,    87,     0,    75,
      77,    49,     0,    45,     0,    48,   159,     0,     0,     0,
       0,  1117,  1008,   853,  1007,  1010,  1002,     0,     0,   896,
     914,     3,     3,     0,   853,   978,   981,   853,     0,   853,
     853,   973,   990,     0,     0,  1112,     0,   716,   490,     0,
    1114,   490,   490,     0,     0,   438,     3,     0,     0,     0,
       0,   738,   743,     3,   835,   186,   185,     3,     0,   490,
     880,   883,   885,   490,   937,   940,   942,   490,   960,   962,
     964,     0,     0,     0,   733,   853,  1023,  1022,     0,     0,
       0,     0,     0,     3,   809,   845,     0,   490,   490,   490,
     490,   490,   490,   490,   611,   641,     0,     0,   642,   572,
     629,     0,     0,     0,   422,    69,     0,     0,   331,   332,
     214,   216,   490,     0,     0,     0,   490,   490,   327,     0,
     325,     0,     0,     0,   739,     0,     0,     0,     0,   372,
     490,     0,   165,     0,     0,   400,     0,     0,     0,   221,
       0,   212,     0,   322,     0,     0,     0,   345,   345,   351,
     350,   345,   362,   361,   345,   345,     0,   572,   853,     0,
    1027,  1026,     0,     0,   764,   800,     2,   795,     0,   796,
       0,   776,   756,   760,   758,     0,  1117,     0,     0,   490,
       0,     0,     0,     3,   490,  1038,  1040,  1041,     0,     0,
     105,     0,     3,  1117,   675,   669,   675,   685,   685,   739,
     686,   661,  1117,     0,   789,   490,    72,  1028,     0,     0,
       0,    39,     0,   117,   119,   118,   115,   114,   739,  1117,
       0,    68,    84,     0,    78,    85,    86,    63,     0,     0,
       0,    59,     0,   158,   429,    31,     0,   490,  1003,  1005,
    1006,   897,   915,     0,     0,   853,   490,   974,   976,   977,
     490,   991,   993,     0,   968,   983,   979,   994,  1113,   706,
     452,   701,   700,   702,  1116,  1115,     0,     3,   850,   749,
     750,     0,     0,   853,     0,     0,     0,   888,   945,   953,
     471,   852,     0,  1018,     0,  1019,  1020,  1014,   826,     2,
       0,   828,   611,   611,   611,   642,   649,   616,     0,   655,
     642,     0,   490,   603,   640,   636,     0,     0,     0,     0,
     643,   645,   853,   657,   657,   657,     0,   637,   653,   425,
       0,   335,   336,   333,   334,   230,     0,     0,   232,   433,
     231,   572,   490,     0,     0,   345,     0,   313,   312,   314,
       0,   345,   189,   270,     0,   263,     0,   189,   328,   326,
       0,   320,  1117,   329,     0,     0,     0,     0,   381,   382,
     383,   384,     0,   374,     0,   375,   337,     0,   338,     0,
       0,   365,   210,   324,   323,     0,     0,   354,   364,     0,
     345,   367,     0,   369,   391,     0,   423,   853,   824,   755,
     777,   490,     2,     2,   667,     0,   675,   675,  1090,  1091,
    1092,     0,     0,     3,     3,     0,  1051,     0,     0,  1117,
     675,     0,   682,   681,  1117,     0,   664,     3,     0,  1029,
     103,     0,    32,   490,     0,  1117,     0,     0,    88,     0,
      76,     0,    82,     0,    80,    44,   163,     0,   853,   853,
       0,     0,   752,     0,   446,   450,   889,   946,   954,     0,
       0,   792,   830,   607,   609,   605,     0,     0,  1058,     0,
     650,  1063,   652,  1055,   853,   853,   635,   656,   639,     0,
     638,     0,     0,     0,   659,     0,   631,   853,   630,   646,
     658,   647,   648,   654,   345,   345,   233,   572,     0,     0,
     251,   345,   318,   316,   319,   315,     0,   317,     0,   259,
       0,   189,     0,   345,   490,   271,     0,   296,     0,     0,
     321,     0,     0,   345,   344,   345,   385,     0,   376,     2,
       0,     0,     0,   347,     0,     0,   345,     0,   345,   345,
     345,   208,   207,   449,   772,   794,     0,     0,   668,  1117,
    1117,  1093,  1044,     0,     0,  1050,  1052,   665,     0,     0,
     675,     0,   663,     2,    50,    42,    40,    41,     0,    66,
     182,    79,     0,     0,  1009,   448,   447,   980,   995,   751,
    1013,  1021,   633,     0,     0,     0,  1059,  1060,   853,   634,
    1056,  1057,   632,   612,     0,     0,   343,   222,     0,     0,
       0,   244,   345,   224,     0,     0,   345,   253,   268,   279,
     273,   345,   189,     0,   310,     0,   283,     0,   308,     0,
     274,   272,   261,   264,     0,     0,   189,   297,     0,     0,
     227,   342,   373,     2,   490,   339,     0,     0,   401,   352,
       0,    69,   363,   356,     0,   357,   355,   370,   763,   799,
     801,     0,     0,  1053,  1054,   666,   675,  1117,   687,   791,
      67,    83,    81,   853,  1066,  1068,  1061,     0,   644,   345,
     239,   234,   237,     0,   236,   243,   242,     0,   490,   246,
     245,   345,   255,     0,   252,   345,     0,     0,     0,   260,
     265,   311,     0,     0,   189,   284,   309,     0,     0,   345,
       0,   299,   300,   298,   267,   330,     0,   490,   490,     3,
     386,   491,   390,     0,   394,     0,     0,     0,   402,   403,
     348,     0,     0,     0,   671,   673,  1117,     0,   490,  1062,
    1064,  1065,   651,   223,     0,   241,     0,   240,   226,   247,
     490,   414,   256,   345,   257,   254,   269,   282,   280,   276,
     288,   286,   287,   285,   266,   281,   277,   278,   275,   262,
       0,     0,     0,     0,   229,   247,     3,   379,     0,  1058,
     387,   388,   389,   401,     0,     0,     0,   401,     0,   353,
     349,   345,     0,     0,   672,     0,   235,   238,   345,     3,
     248,   415,   258,     0,     0,     0,     0,   307,   305,   302,
     306,   303,   304,   301,     3,   379,     0,     0,  1059,     0,
       0,     0,   395,     0,   404,   358,   345,   670,  1067,   218,
       0,     0,   345,   295,   293,   290,   294,   291,   292,   289,
       0,     0,   380,     0,   407,     0,   405,     0,   407,   359,
     220,   219,   225,     0,   228,     0,   377,   408,     0,     0,
     396,     0,   378,     0,     0,     0,     0,   409,   410,     0,
     406,   397,     0,     0,   398,   411
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1881,  4317,  3304, -1881,    -1,   413,  1897,  -156, -1881,  -337,
   -1881,   431, -1881,  -727, -1881,   891,  -934,  -988, -1881,   236,
    6823,  2186, -1881,    20, -1881,  1496,   362,   843,   844,   667,
     848,  1457,  1458,  1456,  1460,  1461, -1881,  -123,  -182,  8375,
     994, -1881,  1785, -1881, -1881,  -728,  8100, -1179,  1177, -1881,
    -116, -1881,   986,    60, -1881, -1881,   755,   148, -1881, -1835,
   -1650,   364,   122, -1881, -1881,   748,   375,   271, -1636, -1881,
   -1444, -1881, -1881, -1881, -1881,   168,  -977, -1881, -1881, -1248,
     504, -1881, -1881, -1881, -1881, -1881,   101, -1214, -1881, -1881,
   -1881, -1881, -1881,    89,   522,   524,   190, -1881, -1881, -1881,
   -1881,  -708, -1881,   121,    72, -1881,   194, -1881,  -164, -1881,
   -1881, -1881,   980,  -604, -1054,   -56, -1881,    10,    58,   202,
    9307,  -792,  -779, -1881,   -13, -1881, -1881,    63, -1881,  -151,
    8967,     0,  -249,  3091,   463,  -677,   163,    71,    59,  1300,
    2096, -1881, -1881,  2215, -1881,    36,  4944, -1881,  2152, -1881,
     136, -1881, -1881,  1567,    93,  5628,  4260,   -50,  2020,  -288,
   -1881, -1881, -1881, -1881, -1881,  -379,  6490,  5681, -1881,  -394,
      91, -1881,  -640,   320, -1881,   250,   816, -1881,   -57,  -238,
   -1881, -1881, -1881,  -372,  6616,  -676,  1289,    85,  1474, -1881,
    -343,  -132,   156,    32,  1367,  -624,  -115,  1008,  2683,   120,
    -544,  -196,  -192,  -499,  1420, -1881,  1767,   583,  -938,  1651,
   -1881, -1881,   758, -1881, -1247,  -171,    43,  -931, -1881,   125,
   -1881, -1881, -1169,   533, -1881, -1881, -1881,  2283,  -813,  -440,
    -745,   -26, -1881, -1881, -1881, -1881, -1881, -1881,   284,  -832,
    -211, -1880,  -169,  8252,   -72,  7165,  -127,  1585, -1881,  2202,
     -64,  -227,  -199,  -194,     4,   -75,   -67,   -37,   557,    23,
      55,   176,  -191,   -46,  -187,  -144,  -141,   -36,  -136,  -133,
    -122,  -746,  -790,  -725,  -694,  -720,  -131,  -685, -1881, -1881,
    -713,  1504,  1505,  1508,  1056, -1881,   641,  7877, -1881,  -613,
    -627,  -617,  -592,  -750, -1881, -1655, -1782, -1779, -1762,  -654,
     -43,  -195, -1881, -1881,   -79,   515,  -102, -1881,  7452,  2113,
    -408,  -424
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   850,   424,   425,    83,    84,   426,   400,   427,
    1580,  1581,   428,  1003,  1004,  1005,  1368,  1369,  1370,  1592,
     450,   430,   431,   432,   733,   734,   433,   434,   435,   436,
     437,   438,   439,   440,   441,   442,   443,   452,  1149,   735,
    1503,   796,   222,   798,   446,   871,  1247,  1248,  1249,  1250,
    1251,  1252,  1253,  2180,  1254,  1255,  1696,  2032,  2033,  1961,
    1962,  1963,  2149,  2150,  1256,  1714,  1715,  1978,  1716,  1867,
    1868,  1257,  1258,  1259,  1260,  1261,  1262,  1894,  1898,  1525,
    1517,  1263,  1264,  1524,  1518,  1265,  1266,  1267,  1268,  1269,
    1270,  1271,  1733,  2167,  1734,  1735,  2069,  1272,  1273,  1274,
    1506,  2077,  2078,  2079,  2208,  2219,  2099,  2100,   307,   308,
     938,   939,  1215,    86,    87,    88,    89,    90,  1699,   486,
      93,    94,    95,    96,    97,   236,   237,   310,   289,   488,
      99,   489,   100,   609,   102,   103,   157,   356,   313,   107,
     108,   109,   172,   110,   956,   357,   158,   113,   260,   114,
     159,   268,   359,   360,   361,   160,   447,   119,   120,   363,
     121,   605,   931,   929,   930,  1673,   364,   365,   124,   125,
    1210,  1470,  1679,  1680,  1829,  1830,  1471,  1668,  1849,  1681,
     126,   698,  1780,   694,   366,   695,   696,  1328,   968,   611,
    1141,  1142,  1143,   612,   367,   497,   498,   614,  1278,   456,
     457,   223,   515,   516,   517,   518,   519,   344,  1297,   345,
     954,   952,   644,   346,   385,   347,   348,   458,   128,   178,
     179,   129,  1291,  1292,  1293,  1294,     2,  1197,  1198,   635,
    1285,   130,   334,   335,   270,   281,   588,   131,   226,   132,
     325,  1151,   921,   549,   170,   133,   395,   396,   397,   134,
     327,   240,   241,   242,   328,   136,   137,   138,   139,   140,
     141,   142,   245,   329,   247,   248,   249,   330,   251,   252,
     253,   836,   837,   838,   839,   840,   254,   842,   843,   844,
     801,   802,   803,   804,   550,  1190,  1449,   143,  1788,   669,
     670,   671,   672,   673,   674,  1832,  1833,  1834,  1835,   659,
     499,   371,   372,   373,   459,   214,   145,   146,   147,   375,
     863,   675
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   197,   445,    82,   195,   135,   213,   531,   386,   198,
     699,   570,   860,  1036,   153,  1521,  1298,   354,   527,   303,
    1295,   994,   983,   738,   912,   914,  1046,   399,   204,  1054,
     521,  1508,  1453,   182,   239,   532,   969,   111,   382,   199,
     533,   317,  1109,   534,   567,  1943,   970,   535,  1944,  1495,
    1543,  1544,   246,   368,   977,    82,    82,  1029,    82,    91,
     106,   444,   250,   135,    98,  1945,   461,   677,   547,  2035,
     552,   971,   105,   213,   494,    82,  2095,   560,  2034,   585,
     556,  1879,   744,  2041,    82,  1119,  1737,  1089,  1115,   211,
     536,  1126,    82,   537,   116,   111,   531,    82,   538,   200,
      82,   539,   243,   481,    82,   271,  1279,  1110,   582,   282,
     524,  1233,   540,  1205,  1116,   275,  1372,    91,   106,   593,
     463,  2103,    98,   320,   532,   370,   309,  2040,   464,   533,
     105,   201,   534,  1446,  1286,  -802,   535,   115,  1111,   691,
     918,   148,  1519,    82,   398,  1379,    82,  1112,    82,   624,
     626,   926,   116,   135,    82,  1447,  1465,   998,   465,   197,
     266,    82,   529,   546,   104,  1520,  1738,   198,    82,  1013,
    -803,   542,  1739,  1947,   687,   111,  1750,  1417,   690,   536,
     394,   224,   537,  2075,   577,   111,   161,   538,  2042,   543,
     539,   617,   394,    82,    82,   115,   947,   199,   211,   544,
     162,   540,   677,    92,    82,   969,   154,    91,   106,    82,
     312,  1418,    98,  1416,   503,   970,  2034,   625,   466,   512,
     105,   977,   104,  1133,    82,  1135,  2104,    82,    82,    82,
    2036,   309,   116,    82,    82,  1979,  2096,  2097,   496,   211,
     971,   279,   116,  1740,   224,   978,   239,   600,   197,   778,
     467,   629,   202,   577,    82,  -834,   198,   200,   297,   309,
     542,    92,    82,  1093,   246,    58,   703,   211,  1507,  1872,
     309,  1599,    82,    82,   621,   115,    82,  1473,   543,  1327,
     111,   589,  1299,    82,    20,   115,   199,   992,   544,   201,
    2094,  2026,   779,   167,   692,   618,  1474,    82,    82,   693,
      82,   737,   876,   264,  1600,    82,   181,   276,   111,    82,
     660,  1361,   104,  2040,   620,   312,   225,  1109,  -834,   111,
    1331,   677,    82,    82,  1418,   864,   995,   183,   211,    75,
     877,    58,    82,   184,  1275,   878,   587,   116,   879,   192,
      82,    82,   880,   312,   111,    82,  1943,  2040,   301,  1944,
    1352,    92,   548,   654,   312,   910,   904,   841,  1119,    58,
    1052,   915,   495,  1319,   908,   116,  1945,   677,  1401,   685,
     266,   468,   213,   688,  1010,  1386,   116,  1594,   826,   312,
     115,    58,  1110,    82,   193,   881,    82,  1481,   882,  1454,
     648,   677,   394,   883,  1402,    75,   884,   625,   677,   481,
     205,   116,   876,  1465,  1465,  1465,  1787,   885,   115,   594,
     202,  1519,  1120,  1111,   266,  2131,  1123,   151,  1466,   115,
    1279,   666,  1393,    75,   606,   714,   925,  1138,  1139,   648,
     877,  1467,  1984,  1985,  1520,   878,  1958,  1959,   879,   905,
    1522,   660,   880,   494,   115,    75,   548,   909,   279,  2148,
     758,   759,  1152,  1452,   463,   217,  1741,   964,  2141,   204,
    1456,    82,   464,  1523,   557,   919,   503,    -3,   548,   176,
     176,   758,   228,  1233,  1947,  2148,   927,   896,    58,   586,
     969,   192,   354,  1202,   654,   881,    82,    82,   882,  -608,
     970,  1674,   465,   883,   989,   897,   884,   302,    82,    82,
    2182,  1319,  1476,  1477,   758,   898,   176,   885,    82,   302,
     512,  1144,  1145,  2026,  1691,   971,   144,  1958,  1959,   144,
     961,   494,  1960,   768,   769,   189,  1693,   555,    82,   228,
    1160,  2052,  2053,  1870,   563,  1896,  1508,   739,  1878,  1900,
      82,  1533,    75,  -987,  1749,   503,  1846,  1507,  1752,   266,
    -987,  1515,   466,  1847,   463,   581,  1516,   176,  1324,   229,
     176,   168,   464,  1556,   265,    82,   592,   896,   770,   771,
    1897,    82,  1848,    82,   144,   176,   287,   309,   294,   229,
     296,    74,   380,  1381,   467,   897,  1329,    63,    64,  -662,
     370,  1031,   465,   737,   230,   898,  -662,  1686,  1675,   737,
    1283,  2048,   799,  1989,   302,   231,   548,   496,   737,   946,
    1800,  1802,  1804,    80,    81,  1480,  1687,   255,   478,   265,
     144,  1478,   294,   296,  1546,  1078,   111,   737,    82,   219,
      82,   274,   964,   660,    82,   503,    82,    78,   176,   135,
     220,   526,   212,   528,  1563,    82,  1602,  1622,  1275,    82,
      82,   494,   962,   983,  1572,   244,   221,  2008,   272,  1799,
    -484,   312,   283,  1130,   144,  1466,  1466,  1466,   266,   299,
    1153,   111,  1473,   265,  1031,    58,   982,   677,  1467,  1467,
    1467,   587,    82,   116,  2058,   496,   176,   176,  1183,   988,
    1031,  1756,  1977,    91,   106,    82,   494,   176,    98,    58,
    2121,   297, -1118,   841,   394,   468,   105,  1407,    58,  1660,
    1837,   603,   176,   692,   608,  1031,   301,   163,   693,   151,
     164,   165,   444,   166,   966,  2123,   115,  1031,   116,  1838,
      58,   495,   600,   302,  1136,   641,  1148,  2154,   587,    75,
     265,  1841,   294,   296,  1184,    82,   176,    82,  1690,    82,
     321,   212,  1542,    82,   176,   176,    82,  2083,   632,   176,
    1391,  1392,   548,    75,   642,   643,  1031,   504,  1304,  1725,
     745,   115,    75,  1507,   265,   746,  2156,  1686,    74,   265,
    1056,    82,   815,  1031,  1629,   265,   548,  1133,  1135,   945,
    1191,  1094,   212,  2050,    75,   548,  1840,   176,   104,   663,
     176,   819,  1880,   664,  1199,   548,    58,  2064,  1203,   495,
      80,   665,  1206,  1117,  1475,   496,  1427,   664,   265,   747,
     212,  1904,   342,   682,   748,   296,    82,   962,    58,  1008,
     384,    82,  1012,    82,   590,  1014,  -823,    92,  1847,    58,
     906,  1862,  1863,  1864,  1017,    82,  1948,  1019,  1020,  1021,
     645,   738,  1375,   398,   646,    82,  1323,  1942,  1589,  1335,
     496,   512,  1797,  1865,    82,  1949,   920,  1932,   387,  1933,
      75,   721,   354,   924,    58,  2114,    74,   928,  1697,   496,
     704,   496,  1697,  1717,   705,   496,   496,   496,  1779,  1124,
     501,  1545,    75,   664,    58,  1024,  1717,   663,    82,   966,
     176,   664,   192,    75,   502,   496,  1025,  1026,    80,   665,
    -988,  1171,   176,   176,  1873,   548,   301,  -988,   469,  1874,
     481,   666,  1175,  1573,   192,   265,   548,   203,    64,  1547,
    1548,  -727,  1847,  -485,    58,    82,    82,   512,    75,   495,
    1031,   761,   470,    14,    15,    16,    17,    18,   762,   763,
    1415,  1952,  1376,   265,    58,   682,   296,  1179,    75,  2046,
    1288,   548,  1422,  1303,  1789,  1356,  1212,   700,   471,  1591,
     702,   721,  1357,  1606,   111,   496,  1335,  1429,  -486,   506,
     370,   548,  1400,   841,   495,   472,    82,   478,    14,    15,
      16,    17,    18,    58,  1885,  1722,    91,   106,    75,  1874,
     507,    98,    58,  1132,   265,   256,   257,    74,   258,  1277,
    1997,  2136,   473,   259,  1444,  1998,  2137,  1433,    75,   504,
      58,   548,  2197,   677,   266,  1826,   474,  2198,  1827,   265,
    1839,   116,   548,  1148,   265,   475,   265,  1437,  1346,    80,
      81,   548,   163,    82,  1350,   164,   165,    58,   166,    82,
     176,   520,   865,   866,   522,  1358,   867,    75,   265,   176,
     265,   265,   525,   957,   960,   780,    75,  1558,  1667,   781,
    1784,   545,   265,    58,   115,    58,  1554,   546,    82,  1535,
     664,   512,   806,   808,    75,   265,   807,   705,  1455,  1795,
    1909,  1910,   568,    58,   265,   301,   986,   469,   504,   548,
     935,   104,  1479,  1607,   936,   569,    82,   548,   580,   176,
     997,    75,    82,    82,   646,   557,   574,   889,   265,   548,
     682,   296,  1501,  1057,  1058,  1059,   386,   386,   632,   737,
     469,  1472,   548,   354,   591,   496,   496,    75,  1765,    75,
      92,   224,   265,   682,   999,  1570,    82,  1000,   646,   265,
     144,   705,  1289,   599,    64,  1778,  1616,    75,  1620,  1040,
     548,  1042,   664,  1045,  1785,   615,   936,  1051,   302,  1136,
    1055,  1862,  1863,  1864,  1030,   469,  2088,   548,  1031,   945,
     548,  1796,  1117,  1031,   469,  1098,   664,  1577,   354,   548,
     619,   496,  1157,  1865,  1781,  1080,  1158,    14,    15,    16,
      17,    18,  1871,   636,   444,   637,  1862,  1863,  1864,  1204,
     684,  1288,   649,   297,    14,    15,    16,    17,    18,   301,
     652,   512,   697,   548,    82,    82,    82,   701,  1865,   706,
    1862,  1863,  1864,   512,   557,  1637,  1638,  1866,   548,   714,
     478,   370,  1031,  1193,  2017,  1195,   632,  1031,  1650,  1031,
     548,   512,  1865,   693,  1603,   111,    58,    82,  1334,   176,
     707,  -190,  1335,  1371,   660,   764,   765,  1335,   708,   176,
    1717,   711,    82,    58,  2081,    82,    82,    91,   106,    82,
     271,   282,    98,  1823,  1824,  1825,    82,   275,   111,    82,
    1277,   766,   767,  1136,   444,   444,   370,  1136,  1457,  1458,
    1459,   559,  1728,  1729,  1730,  1731,  1732,   982,   712,  1541,
      91,   106,   116,   807,  1881,    98,   501,   741,  1792,   713,
      75,  -488,  1793,  1277,    82,   772,   773,   758,  -126,  -126,
    -126,  -126,  -126,  -126,   717,   266,   741,    75,  -489,    82,
    2086,   760,  1854,  1858,  1882,   116,  1335,  1031,  1031,  1214,
     176,   176,   777,  1883,  1884,   115,   512,  1158,  1031,  1953,
    1692,  1694,   190,   807,    82,   739,   774,  1583,  1584,  1585,
     587,  1918,   775,  2002,  1586,  1587,  1921,  1031,  1472,  1472,
    1472,  2043,   104,  1669,  1472,  1031,  2139,  1928,   115,   776,
    1335,    14,    15,    16,    17,    18,   782,    82,   354,   809,
    1322,  2140,   810,  1289,  1683,  1031,   285,   811,  1919,   812,
    1754,   286,   813,   279,   290,   104,   295,   827,   741,  2216,
    2222,    92,  1684,  2213,  2223,  2169,  1698,   814,   265,  2173,
    1698,   477,  1685,   822,  2101,  1064,  1065,  1066,  1067,   265,
     496,  1022,   741,   496,   496,  1850,  1850,  1850,   265,   531,
      58,   150,   824,   144,    92,    65,    66,    67,    68,    69,
      70,    71,    72,  2101,  1582,   144,    -3,    82,   845,  1700,
     153,    82,    82,  1700,  1033,  1034,   -18,   532,   264,   276,
     652,   741,   533,  1359,  1158,   534,  1373,  1374,   847,   535,
    1031,  1377,  -487,   512,   849,   945,  2151,  -161,  -161,   205,
     741,  2011,  2012,  1515,  1516,   861,   370,  1597,  1598,   862,
     805,  1601,  1598,   872,    75,   874,   512,   512,   111,  1605,
    1598,   900,   111,   111,  1106,  1590,    82,   817,  1639,  1590,
     820,   589,   536,  1106,  1652,   537,   111,  1805,  1158,   886,
     538,   106,   887,   539,   901,   106,   106,  1930,  1158,  1931,
    1598,   265,   902,  1702,   540,  1940,  1031,  1702,  1702,   106,
    -125,  -125,  -125,  -125,  -125,  -125,   285,  1632,   512,  2000,
    2001,  1702,   888,  2021,  1598,   116,   890,   265,   934,   116,
     116,  2022,  1598,   891,  1892,  1288,   587,   512,  1958,  1959,
     559,   892,    82,   116,  2213,  2214,   893,    82,    82,    82,
     894,  1683,  1595,  1596,  1842,   895,  1683,  1060,  1061,  2087,
    1062,  1063,   314,   285,   542,   876,  1886,   922,   115,  1684,
    1068,  1069,   115,   115,  1684,  1751,  1753,  1851,  1852,  1685,
     923,  -606,   543,  -604,  1685,   176,   115,   933,   176,   176,
     176,   932,   544,   877,   937,   104,   940,   176,   878,   104,
     104,   879,  1782,  1783,   949,   880,   951,   286,   955,   681,
     972,   295,   154,   104,    82,   176,   974,   666,   613,    82,
     991,   176,  1007,   996,  1032,    82,  1035,    82,  2143,  1038,
    1105,  1082,    19,  1077,    92,    82,   608,  1106,    92,    92,
    1113,  1134,  1137,   176,  1155,  1162,  2010,  1185,   881,  1163,
     176,   882,    92,  1164,   512,   945,   883,  1165,   722,   884,
     512,  1166,   275,  1512,  1167,   255,   144,  1168,  1169,  1170,
     885,    48,    49,    50,    51,    52,    53,    54,    55,   586,
    1192,   827,  1194,   386,   144,  1996,  1196,  -806,   176,   265,
    1200,   150,  1284,   174,   175,    65,    66,    67,    68,    69,
      70,    71,    72,  1578,   512,   749,  1207,   750,   751,   752,
     266,  1208,  1209,  1280,  1287,  1893,  1296,   144,  1300,  1312,
    1313,  1288,  1314,   492,   265,   610,  2031,  1289,  1315,  1326,
     265,   896,  1327,   677,  1333,  1330,  1332,   753,  1336,  1338,
     754,   755,   144,  1337,  1022,   756,   757,  1341,  1340,   897,
      82,  1342,    82,  1343,  1344,  1362,  1345,  1347,   722,   898,
    1348,  1349,  1354,  1355,  1421,  1513,  1378,  1363,   150,  1382,
     232,   233,    65,    66,    67,    68,    69,    70,    71,    72,
    1383,  1384,  1408,   272,   283,   444,  1385,  1389,   279,  1390,
    1394,    82,  2076,  1395,    82,  1396,    74,    14,    15,    16,
      17,    18,  1028,   512,   512,   805,   805,  1397,  1405,  2130,
     512,  1410,  1411,  1412,  1445,  -807,  1096,  1676,    77,  1099,
    1450,  1482,   512,  1483,  1677,  1486,  1487,  1496,    80,    81,
    1497,  1498,   512,  1500,   512,   176,   176,   285,  1505,  -726,
    1031,  1683,  1528,  1509,  1530,   512,  1531,   512,   512,   512,
    1571,   531,  1564,   264,   276,    82,    82,  1574,  1588,  1684,
     111,  1537,  2146,  1582,  2031,  2072,   587,  1539,  1590,  1685,
     657,  1604,  1611,   680,   613,  1612,  1615,  1626,  1627,   532,
     176,   176,   265,   106,   533,   559,   657,   534,  1628,  1630,
     657,   535,  1173,  1634,  1635,  1702,  1177,  1643,  1658,  1598,
    1181,  1640,  1647,  2171,    82,    14,    15,    16,    17,    18,
    1351,   512,  1648,  1289,  1649,   512,  1657,   116,  1661,  1704,
     512,   444,  1672,   444,  1475,   144,  1718,  2076,  1719,  1721,
    1723,  2076,  2076,  1736,   536,  1516,  1742,   537,  1743,  2072,
     265,  1744,   538,  1745,  2070,   539,  1746,   144,  1233,   613,
    1757,   144,   144,  1758,  1760,  1762,   540,  1763,  1764,  1786,
     115,  1766,   444,  1790,  2195,   144,  1767,  1768,  1769,  1770,
    2080,  1776,  1791,  1798,  1794,  1806,  1808,  2215,   512,  1809,
     613,   907,   469,  1812,  1814,  1816,  1639,   104,  2207,  1817,
     512,  1818,  2207,  2192,   512,  1821,  1822,  1836,  1677,   225,
    1908,   657,  1855,   197,  1859,  1861,   629,  2217,   512,   586,
    1889,   198,  1891,  1911,  1915,   542,  1912,  1917,  2070,    82,
    1916,    82,  1922,  1924,   111,  1920,    92,  2206,  1929,  1934,
    1937,  1939,  1938,   543,   590,  1966,   444,  1971,  1988,  2212,
     144,   199,  1993,   544,  1972,  1986,  1999,   106,  2004,   176,
    1995,  2013,   512,   111,   896,  2014,   610,  2015,  2018,  1702,
    2016,  2019,    58,  2020,   548,  2023,  2029,   513,   176,  2024,
    2025,  2045,   897,  2047,   176,  2051,   106,    82,    82,  -589,
    2054,   116,   898,   211,  2059,  2057,   111,  2073,  1702,  2084,
     512,  2065,   492,  2085,   805,  2074,   150,   512,   232,   233,
      65,    66,    67,    68,    69,    70,    71,    72,   191,   106,
     116,  2098,  2107,  2120,   265,  2122,   613,    82,   176,  2124,
    2133,  1702,   503,  2134,   115,   512,    75,  2135,  2144,   512,
    2138,   512,   613,  2157,  2153,  2155,   613,    85,  2170,  2166,
     152,  2172,  2176,   116,  2178,  1398,    77,   267,   216,   613,
     492,   104,   512,   115,  2177,  2183,  2194,  2202,  2193,   288,
     291,  2196,  2204,    82,  2205,  2209,  2210,   144,  1431,   657,
     492,  1435,    82,  2220,  1926,  1439,  1576,  2221,  2224,  1027,
     104,  1070,  1072,  1071,  1504,   797,   115,  1073,  1511,  1074,
      92,  1706,  2203,   657,  2147,    85,  1990,  2164,  1727,  1983,
    2056,  2142,   267,  1899,  2191,  1887,   657,  1888,  2126,  2174,
    1529,  2125,   194,   104,   173,   216,   292,   176,   176,    92,
    2211,    85,    19,  1084,   176,  2028,   144,  2092,   196,  1671,
    1325,  1526,  1154,     3,   235,   868,   176,   263,   579,  1101,
    1001,    85,  1759,  1102,  1905,  1820,   176,   953,   176,     0,
     238,  1213,    92,  1085,  1086,   460,   267,  1087,   265,   176,
       0,   176,   176,   176,     0,    52,    53,    54,    55,   176,
     265,     0,     0,     0,     0,     0,     0,     0,     0,   152,
       0,     0,     0,     0,     0,    85,     0,     0,   152,     0,
       0,   324,   332,     0,     0,     0,     0,     0,     0,     0,
     492,     0,     0,     0,   353,     0,     0,   326,     0,     0,
     150,     0,  1047,  1048,    65,    66,    67,    68,    69,    70,
      71,    72,  1049,   267,     0,   176,   578,     0,   451,   176,
     194,   194,     0,     0,   176,     0,     0,     0,     0,   144,
       0,   152,   484,     0,   657,   492,   263,     0,     0,  1485,
       0,     0,     0,     0,     0,     0,   858,   267,   513,   265,
       0,  1499,   267,  1050,     0,   324,     0,     0,   267,     0,
     235,   235,     0,     0,    14,    15,    16,    17,    18,     0,
       0,   326,     0,     0,     0,     0,   530,   238,     0,  1609,
       0,   324,   176,     0,     0,   578,     0,     0,     0,    85,
    1618,   267,     0,     0,   176,     0,     0,   326,   176,     0,
       0,   613,     0,   263,     0,   613,   662,     0,     0,     0,
       0,   150,   176,     0,   613,    65,    66,    67,    68,    69,
      70,    71,    72,    58,   613,  2132,     0,     0,     0,     0,
       0,   613,     0,     0,     0,     0,   324,    14,    15,    16,
      17,    18,   332,     0,   723,     0,     0,     0,   332,   324,
     324,     0,   326,     0,     0,     0,   176,     0,   152,     0,
       0,    77,     0,     0,   857,   630,   326,     0,     0,     0,
     657,     0,     0,   680,   265,     0,     0,     0,   613,   353,
     667,   676,   613,     0,     0,    74,   613,    75,     0,     0,
       0,     0,     0,   144,   176,   353,    58,     0,     0,   353,
       0,   176,     0,     0,   216,     0,   663,     0,  1387,     0,
     664,     0,  1388,     0,     0,     0,     0,    80,   665,     0,
       0,     0,   144,     0,     0,     0,   267,     0,     0,   176,
     492,  1403,     0,   176,     0,   176,     0,   662,  1404,     0,
       0,     0,     0,   451,   723,     0,   185,     6,     7,     8,
       9,    10,    11,    12,    13,   144,   176,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,  2218,     0,     0,
       0,   265,     0,     0,     0,     0,  2225,   451,     0,   799,
     800,     0,     0,   548,     0,  1441,     0,     0,   194,  1442,
      80,    81,     0,  1443,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,     0,   152,   284,     0,   267,
     484,     0,     0,     0,   834,     0,   676,     0,     0,     0,
       0,   460,   460,     0,     0,     0,     0,   152,     0,     0,
     835,   267,   150,     0,  1747,  1748,    65,    66,    67,    68,
      69,    70,    71,    72,  1364,   267,     0,     0,  1365,     0,
    1366,     0,     0,     0,     0,   235,    58,     0,   267,     0,
       0,     0,  1831,     0,     0,     0,     0,   235,     0,     0,
       0,   875,     0,     0,     0,     0,     0,     0,     0,   177,
     180,     0,    77,   238,     0,  1593,     0,     0,     0,     0,
       0,   267,   324,     0,   451,   451,     0,     0,   324,   513,
       0,   353,   858,     0,     0,     0,     0,     0,   326,     0,
       0,     0,     0,     0,   326,   267,   227,     0,    74,     0,
      75,     0,   267,     0,     0,     0,     0,     0,     0,   984,
       0,     0,     0,     0,     0,     0,   613,     0,     0,  1827,
     613,     0,     0,   548,   613,     0,     0,     0,     0,     0,
      80,    81,     0,     0,   460,   324,     0,   324,     0,   353,
       0,    85,  1009,     0,     0,     0,   460,   318,  1015,     0,
     319,   944,     0,   326,     0,     0,     0,     0,   353,   484,
       0,   676,     0,     0,     0,   343,     0,     0,     0,   667,
       0,     0,     0,   667,     0,     0,     0,     0,     0,   150,
       0,     0,   353,    65,    66,    67,    68,    69,    70,    71,
      72,  1043,   676,     0,   657,   353,     0,     0,     0,     0,
       0,     0,  1860,  1831,  1831,     0,     0,   152,  1869,     0,
       0,     0,     0,     0,     0,   451,     0,     0,   152,   152,
       0,   451,     0,  1644,     0,     0,   492,  1645,   523,     0,
     451,  1646,  1044,   152,   152,   152,     0,     0,   150,     0,
     460,   613,    65,    66,    67,    68,    69,    70,    71,    72,
    1364,     0,  1902,     0,  1365,     0,  1366,     0,     0,     0,
       0,  1367,     0,     0,     0,     0,     0,  1367,     0,     0,
       0,     0,     0,     0,   150,     0,   583,   584,    65,    66,
      67,    68,    69,    70,    71,    72,  1364,   177,    77,   484,
    1365,  1801,  1366,     0,   613,     0,  1367,     0,     0,   513,
       0,     0,   177,   613,     0,   800,   800,   613,     0,     0,
       0,     0,     0,   451,     0,     0,     0,     0,     0,  1188,
       0,     0,  1831,     0,    77,     0,     0,  1803,     0,     0,
       0,   388,     0,   353,   484,     0,   634,     0,   834,     0,
     834,     0,     0,     0,   638,   640,     0,     0,  1772,   647,
       0,  1956,  1957,   353,  1108,   353,   835,     0,  1967,   353,
     353,   353,     0,     0,  1367,     0,     0,   150,     0,     0,
    1982,    65,    66,    67,    68,    69,    70,    71,    72,   353,
    1991,   267,  1992,     0,   460,     0,     0,   343,     0,     0,
     343,     0,     0,  2003,     0,  2005,  2006,  2007,     0,  2090,
       0,  1807,     0,  1831,     0,   324,   389,     0,     0,     0,
    1810,     0,   101,     0,  1811,   156,  1398,    77,     0,     0,
       0,   326,     0,     0,   390,     0,   391,   392,    65,    66,
      67,    68,    69,    70,    71,    72,   492,     0,     0,     0,
       0,     0,     0,     0,   451,     0,  1831,     0,     0,   353,
       0,     0,     0,     0,     0,     0,   152,   451,     0,  2039,
       0,     0,     0,  2044,     0,     0,     0,     0,  2049,   353,
     101,  1307,     0,   393,     0,     0,     0,     0,     0,     0,
       0,     0,   667,     0,     0,     0,     0,     0,     0,     0,
     227,     0,     0,     0,     0,     0,   210,     0,     0,     0,
       0,     0,   851,   852,  1831,  1831,     0,     0,     0,     0,
       0,     0,     0,   336,     0,     0,   277,     0,     0,     0,
       0,   337,   338,   339,   340,     0,  2093,     0,   152,   484,
       0,     0,     0,     0,     0,     0,     0,     0,  2102,     0,
       0,     0,  2105,     0,  1831,     0,     0,     0,     0,     0,
     311,     0,     0,     0,   316,     0,  2119,     0,     0,     0,
     101,     0,   150,   322,   174,   175,    65,    66,    67,    68,
      69,    70,    71,    72,   513,     0,     0,     0,     0,   355,
       0,   150,  1367,   174,   175,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   800,     0,     0,     0,     0,     0,
    2152,     0,     0,   322,     0,   462,     0,     0,     0,   353,
     353,     0,     0,   834,     0,   341,   316,   490,     0,     0,
     834,     0,     0,     0,     0,     0,     0,     0,     0,  1108,
       0,     0,     0,   342,     0,  1399,   835,     0,  2175,     0,
     948,  1492,     0,     0,     0,  2179,   541,     0,     0,   343,
       0,     0,   150,     0,     0,   311,    65,    66,    67,    68,
      69,    70,    71,    72,  1364,   353,   566,     0,  1365,     0,
    1366,   571,   573,  2199,   210,     0,     0,  2201,     0,  2179,
     150,     0,   916,   311,    65,    66,    67,    68,    69,    70,
      71,    72,   267,     0,   311,     0,     0,   595,     0,   993,
    2201,   597,    77,     0,     0,     0,   598,   152,     0,     0,
       0,   513,     0,   218,     0,     0,   152,   573,     0,   311,
       0,     0,     0,   622,     0,   451,     0,   267,     0,   460,
       0,     0,     0,     0,     0,   631,     0,     0,     0,     0,
     984,     0,     0,   322,     0,     0,     0,     0,     0,     0,
       0,   451,     0,     0,   300,     0,     0,     0,   451,     0,
       0,     0,     0,     0,   655,     0,     0,   679,     0,     0,
       0,     0,   388,     0,     0,   613,     0,     0,     0,     0,
     686,     0,   263,    85,   686,     0,   513,     0,     0,     0,
     353,     0,     0,     0,     0,     0,   324,     0,     0,     0,
       0,     0,   152,     0,     0,   513,     0,     0,     0,     0,
    1367,     0,   326,   484,     0,  1367,  1367,  1367,   185,     6,
       7,     8,     9,    10,    11,    12,    13,     0,   322,     0,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   484,     0,   389,     0,  1131,
     152,     0,     0,     0,     0,     0,     0,     0,     0,  1146,
       0,     0,   322,     0,     0,   390,  1655,   391,   392,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,   657,  2145,     0,     0,   267,     0,     0,     0,    58,
       0,   316,     0,     0,     0,   655,   150,     0,   174,   175,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,   316,     0,   353,     0,     0,   353,   353,     0,
    1623,     0,     0,   150,     0,   232,   233,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
    1216,     0,     0,   267,   627,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,   657,     0,     0,     0,     0,
       0,     0,     0,   152,   152,   152,   152,     0,   152,   152,
    1488,     0,   832,    77,  1678,   332,   664,     0,     0,   322,
     322,     0,     0,    80,   833,    58,   490,     0,   451,     0,
    1682,     0,   451,   451,     0,     0,     0,     0,     0,     0,
       0,   311,     0,     0,     0,   150,   451,   174,   175,    65,
      66,    67,    68,    69,    70,    71,    72,     0,  1367,   150,
    1367,   232,   233,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   150,   263,   174,   175,    65,    66,    67,    68,
      69,    70,    71,    72,   355,     0,   101,    74,     0,    75,
       0,     0,     0,     0,     0,   484,     0,     0,     0,     0,
       0,     0,   639,   686,   965,     0,     0,     0,  2128,    77,
       0,     0,   548,     0,     0,     0,     0,     0,   976,    80,
      81,   152,     0,   667,     0,     0,     0,   655,     0,     0,
       0,     0,   985,     0,     0,     0,     0,     0,     0,     0,
     686,     0,     0,     0,     0,   460,  1490,   829,     0,   831,
       0,     0,   322,     0,     0,     0,     0,   267,   848,    58,
     322,     0,     0,   322,   322,     0,   322,     0,     0,     0,
       0,     0,     0,     0,     0,   322,     0,     0,   322,   322,
     322,   150,     0,   601,   602,    65,    66,    67,    68,    69,
      70,    71,    72,   150,     0,   232,   233,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,  1678,  1828,     0,     0,     0,  1678,     0,   451,     0,
       0,    74,  1678,    75,  1678,     0,     0,  1682,     0,     0,
       0,     0,  1682,    78,   490,     0,     0,     0,  1843,     0,
    1682,     0,   234,    77,     0,     0,     0,   332,   152,     0,
       0,  1088,     0,    80,    81,     0,     0,     0,   322,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1489,  1491,
    1493,     0,     0,     0,     0,     0,     0,     0,   686,   965,
       0,     0,     0,     0,   150,  1114,   599,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   490,     0,
     490,  1514,     0,     0,   490,   490,   490,   152,     0,     0,
       0,     0,     0,   267,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1216,   490,     0,     0,     0,     0,     0,
    1534,    14,    15,    16,    17,    18,    19,  1079,    20,   152,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,  1568,    46,
       0,    47,     0,  1828,  1828,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,  1678,  1276,
      58,  1678,     0,     0,   490,     0,     0,     0,     0,     0,
       0,   156,   322,   332,  1954,     0,     0,  1682,     0,     0,
       0,     0,     0,     0,   686,     0,     0,  1311,     0,     0,
     451,     0,     0,     0,  1317,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
     150,     0,   232,   233,    65,    66,    67,    68,    69,    70,
      71,    72,    74,   324,    75,   150,     0,   376,   377,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   326,
       0,     0,     0,   316,   355,  1018,    78,  1011,     0,     0,
       0,     0,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,  1828,     0,    58,     0,     0,   958,     0,     0,
       0,  1678,     0,     0,   959,     0,     0,    78,     0,  1129,
       0,     0,   378,     0,     0,  1688,  1689,  1682,     0,   379,
       0,     0,     0,     0,     0,     0,     0,   267,   150,     0,
     232,   233,    65,    66,    67,    68,    69,    70,    71,    72,
     152,    14,    15,    16,    17,    18,     0,     0,     0,     0,
       0,     0,     0,     0,   490,   490,    74,     0,    75,     0,
       0,   150,     0,   232,   233,    65,    66,    67,    68,    69,
      70,    71,    72,  1828,     0,     0,     0,  2128,    77,     0,
       0,   548,     0,     0,   152,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,  1281,  1282,     0,     0,     0,
     490,     0,     0,   152,   152,     0,  2129,   332,  1320,     0,
       0,   118,     0,     0,   118,  1321,     0,     0,     0,     0,
       0,     0,   326,     0,   150,     0,   232,   233,    65,    66,
      67,    68,    69,    70,    71,    72,   152,     0,     0,     0,
       0,     0,   156,     0,     0,     0,     0,     0,     0,     0,
       0,  1469,    74,     0,    75,     0,     0,     0,     0,     0,
    1276,     0,     0,     0,  2129,  2129,     0,     0,     0,   118,
     149,     0,     0,   234,    77,     0,     0,     0,     0,     0,
     630,   326,     0,     0,    80,    81,   322,     0,    58,     0,
       0,  1360,     0,  1276,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,  2129,     0,     0,     0,     0,     0,
       0,   269,     0,     0,     0,   118,     0,     0,  1527,  1853,
     326,     0,   150,     0,     0,   355,    65,    66,    67,    68,
      69,    70,    71,    72,     0,  1380,     0,   322,     0,     0,
       0,     0,     0,     0,     0,     0,   206,     0,   655,   118,
      74,     0,    75,   118,     0,     0,     0,   571,     0,   118,
       0,     0,   118,     0,     0,     0,   269,     0,     0,     0,
       0,    76,    77,     0,     0,     0,     0,   349,   118,   381,
     355,     0,    80,    81,  1406,   322,  1409,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,  1413,  1414,
       0,     0,   455,     0,  1419,  1420,     0,     0,     0,     0,
       0,     0,     0,     0,  1428,   118,   455,     0,     0,     0,
     269,     0,     0,     0,     0,     0,     0,     0,   150,     0,
     232,   233,    65,    66,    67,    68,    69,    70,    71,    72,
       0,  1448,     0,     0,  1451,     0,     0,     0,     0,   490,
       0,     0,   490,   490,   118,     0,    74,   150,    75,   203,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,   118,     0,   118,   206,     0,     0,   323,    77,     0,
       0,     0,   118,     0,     0,     0,     0,   269,    80,    81,
       0,     0,     0,   118,     0,     0,     0,     0,  1469,  1469,
    1469,   156,   573,   322,   322,     0,  1510,    77,   604,     0,
     857,   118,     0,     0,     0,     0,   118,     0,   118,     0,
       0,   269,   118,  1701,     0,     0,   269,  1701,  1701,   575,
       0,     0,   269,     0,     0,     0,     0,  1532,     0,  2009,
       0,  1701,   118,     0,     0,  1536,     0,  1538,  1540,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
    1550,     0,  1551,   118,  1552,   269,   118,     0,     0,   616,
       0,  1561,     0,     0,     0,     0,     0,     0,     0,   118,
       0,   623,     0,   118,     0,     0,     0,     0,     0,     0,
     355,     0,     0,     0,     0,     0,     0,   150,   633,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,    58,     0,   156,     0,     0,   653,
       0,     0,     0,     0,     0,     0,     0,   455,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,  1613,  1614,     0,   150,    78,
     232,   233,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   455,     0,     0,     0,     0,     0,     0,     0,     0,
    1636,     0,     0,     0,     0,     0,    74,  1641,    75,     0,
       0,  1642,     0,     0,     0,   742,     0,     0,     0,     0,
     118,     0,    58,     0,   455,     0,     0,   323,    77,     0,
     269,     0,     0,     0,     0,     0,     0,  1659,    80,    81,
       0,   118,     0,   322,     0,     0,   783,     0,     0,  1845,
       0,     0,     0,     0,     0,     0,   150,     0,   232,   233,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,  1857,   823,     0,     0,     0,     0,   828,
       0,     0,     0,     0,    74,   150,    75,   174,   175,    65,
      66,    67,    68,    69,    70,    71,    72,   118,     0,   854,
       0,     0,     0,   855,   856,  1676,    77,   859,   455,   455,
       0,     0,     0,   269,    58,   118,    80,    81,     0,     0,
       0,     0,   873,     0,     0,     0,     0,     0,     0,     0,
     118,     0,   156,     0,   502,     0,     0,  1771,     0,     0,
       0,     0,     0,     0,  1775,   903,  1777,     0,   150,   269,
     232,   233,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,   269,     0,   322,     0,     0,     0,     0,     0,
       0,     0,   118,   118,     0,   118,    74,     0,    75,     0,
       0,     0,     0,     0,  2030,     0,     0,     0,     0,     0,
     381,     0,   118,   455,     0,   269,     0,  1676,    77,  1946,
       0,     0,     0,   118,     0,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,   118,     0,     0,   269,
       0,  1813,   943,   604,     0,   112,   269,     0,     0,   118,
       0,   990,   401,     0,     0,   402,   950,   403,     0,   404,
       0,   118,     0,     0,     0,  1701,     0,     0,     0,   455,
       0,     0,   118,   118,     0,   455,   405,     0,     0,     0,
       0,   973,     0,     0,   455,     0,     0,   118,   118,   118,
     150,     0,   174,   175,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   112,     0,     0,   406,   407,     0,   408,
     409,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   410,   411,   398,     0,   412,   413,   414,     0,   415,
     416,     0,     0,     0,     0,     0,     0,    74,     0,   506,
       0,     0,     0,   455,     0,     0,     0,  1023,     0,   278,
       0,     0,     0,     0,     0,     0,     0,     0,   417,   118,
       0,    78,   418,     0,     0,     0,     0,   455,   419,    80,
      81,   420,   421,   422,   423,   118,     0,  1913,  1914,   118,
       0,     0,     0,   112,     0,  2071,     0,   118,   455,     0,
       0,  1923,   118,   112,     0,   150,     0,   232,   233,    65,
      66,    67,    68,    69,    70,    71,    72,   118,     0,   118,
       0,     0,   358,   118,   118,   118,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,  1701,
       0,     0,     0,   118,     0,     0,     0,     0,     0,  1103,
       0,  1104,     0,     0,  1676,    77,     0,   828,     0,     0,
     491,  1677,     0,     0,     0,    80,    81,     0,  1701,  2071,
       0,     0,     0,  1189,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,  1147,     0,     0,     0,     0,     0,
       0,     0,     0,  1156,     0,     0,     0,     0,   112,  1159,
       0,  1701,     0,     0,   118,     0,     0,     0,   455,     0,
       0,     0,     0,   118,     0,     0,     0,     0,     0,     0,
     118,   455,     0,     0,     0,     0,   112,     0,     0,     0,
    2168,     0,     0,   118,    58,  1309,   455,   112,     0,     0,
     596,     0,     0,     0,   653,     0,     0,     0,     0,  1201,
       0,     0,     0,     0,     0,   358,     0,     0,     0,     0,
       0,     0,   112,     0,     0,     0,   278,     0,   150,     0,
       0,     0,    65,    66,    67,    68,    69,    70,    71,    72,
     784,   785,   786,   787,   788,   789,   790,   791,   792,   793,
     794,     0,   118,   455,   219,     0,    74,     0,    75,     0,
       0,     0,     0,     0,  1316,     0,     0,   656,     0,     0,
     278,     0,    14,    15,    16,    17,    18,    76,    77,     0,
       0,   795,     0,   656,     0,     0,     0,   656,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,  1339,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   401,     0,
       0,   402,     0,   403,     0,   404,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,   118,     0,
       0,    58,   405,   118,   118,     0,     0,   118,     0,     0,
       0,     0,     0,  2127,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,   406,   407,     0,   408,   409,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   410,   411,   398,
       0,   412,   413,   414,     0,   415,   416,     0,     0,   118,
       0,     0,     0,    74,     0,    75,     0,     0,   656,     0,
    2165,   118,     0,     0,     0,   118,     0,     0,     0,   118,
       0,     0,     0,     0,   417,     0,     0,    78,   418,     0,
       0,     0,     0,  2181,   419,   483,    81,   420,   421,   422,
     423,   118,     0,     0,     0,     0,     0,     0,  2190,     0,
     118,     0,     0,     0,     0,     0,     0,     0,   150,   455,
     232,   233,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   358,     0,     0,     0,   455,    74,     0,     0,     0,
       0,     0,   455,     0,   149,     0,     0,     0,     0,   491,
       0,     0,     0,     0,     0,     0,     0,   832,    77,     0,
       0,   664,     0,     0,   112,     0,   269,   118,    80,   833,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,   666,     0,     0,     0,     0,   118,     0,     0,     0,
       0,     0,     0,   783,     0,     0,     0,   455,     0,     0,
       0,  1309,     0,     0,     0,     0,   358,   491,     0,   112,
       0,     0,     0,   150,  1567,   174,   175,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   656,   491,   118,   455,
       0,     0,     0,     0,   118,     0,   150,   358,   232,   233,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
     656,     0,  1549,     0,     0,     0,     0,     0,     0,   117,
       0,     0,     0,   656,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1575,
       0,     0,     0,     0,     0,  2128,    77,     0,     0,   548,
       0,     0,     0,     0,     0,     0,    80,    81,   118,     0,
       0,   118,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,   123,     0,   117,     0,   118,
       0,     0,     0,   118,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1656,     0,     0,     0,     0,     0,     0,   118,   118,   118,
     118,   118,   118,   118,     0,     0,     0,   491,     0,   269,
       0,     0,     0,   280,     0,     0,     0,     0,     0,     0,
     123,     0,   455,   358,     0,     0,   455,   455,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   358,
     455,     0,     0,   358,     0,     0,   123,   117,     0,     0,
       0,   656,   491,     0,     0,     0,   358,   117,     0,     0,
       0,     0,     0,     0,     0,     0,   123,   269,     0,     0,
       0,   358,     0,   358,     0,   293,   362,   358,   358,   358,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   455,
       0,     0,     0,     0,   118,     0,     0,   358,     0,     0,
     123,     0,     0,     0,   123,     0,     0,     0,     0,     0,
     123,     0,     0,   123,   493,   118,     0,     0,     0,     0,
       0,   150,     0,   232,   233,    65,    66,    67,    68,    69,
      70,    71,    72,  1761,     0,   150,     0,   232,   233,    65,
      66,    67,    68,    69,    70,    71,    72,   118,     0,    74,
       0,     0,   117,   123,     0,     0,   118,     0,   358,     0,
     118,     0,   112,    74,     0,     0,   123,   358,     0,     0,
     234,    77,     0,     0,     0,     0,     0,     0,     0,     0,
     117,    80,    81,     0,   323,    77,     0,   656,     0,     0,
     278,   117,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,   362,
       0,     0,   455,     0,     0,     0,   117,     0,     0,     0,
     280,     0,   123,     0,   123,     0,     0,     0,     0,   123,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,   269,   118,     0,   123,     0,     0,   491,     0,     0,
       0,     0,     0,     0,     0,     0,  1761,     0,     0,     0,
       0,   658,     0,     0,   280,     0,     0,   123,     0,   123,
       0,     0,     0,   123,     0,     0,     0,   658,     0,     0,
       0,   658,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
     407,   118,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,   358,     0,
       0,     0,   358,     0,     0,     0,     0,   358,   358,     0,
       0,   358,     0,   118,     0,     0,     0,     0,     0,     0,
       0,   358,     0,     0,     0,     0,     0,     0,   358,     0,
       0,   743,     0,     0,    78,   418,     0,     0,     0,  1906,
    1907,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,   358,     0,     0,     0,     0,     0,     0,
       0,     0,   658,     0,     0,   358,     0,   269,     0,   358,
       0,     0,     0,   358,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,   455,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
       0,     0,     0,   493,     0,     0,  1994,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   117,     0,
       0,   278,     0,     0,     0,     0,     0,     0,   358,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1761,     0,     0,     0,     0,     0,     0,     0,     0,   123,
     123,   656,     0,     0,   118,     0,     0,     0,     0,     0,
     362,   493,     0,   117,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,  2038,     0,     0,
     658,   493,   358,   491,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,     0,   118,     0,
       0,     0,     0,     0,   658,     0,  2067,     0,     0,     0,
    2068,     0,     0,     0,     0,     0,   123,   658,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   118,   118,     0,
       0,   269,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,     0,   358,     0,     0,   358,   358,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   358,     0,     0,     0,   358,     0,     0,
       0,   358,   123,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,   123,   123,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,   123,   123,
     123,   493,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,   362,     0,     0,
     112,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,   112,     0,     0,   362,     0,     0,
       0,     0,     0,     0,     0,   658,   493,     0,     0,     0,
     362,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,   362,     0,     0,
       0,   362,   362,   362,     0,     0,     0,     0,   123,     0,
       0,   122,   261,   491,   122,     0,     0,     0,   358,     0,
       0,   362,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -491,  -491,     0,  -491,
      46,     0,    47,     0,  -491,     0,     0,     0,     0,   122,
       0,   358,     0,     0,     0,     0,     0,     0,     0,     0,
     358,    58,   362,     0,   358,     0,   117,     0,     0,     0,
       0,   362,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   658,     0,     0,   280,   122,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   127,     0,   123,
     127,     0,     0,    74,     0,    75,     0,     0,     0,   122,
       0,   123,   123,   122,     0,     0,     0,     0,     0,   122,
       0,     0,   122,     0,     0,     0,   278,    78,   331,     0,
       0,   493,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   127,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,   127,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   127,   362,     0,     0,     0,   362,     0,     0,     0,
       0,   362,   362,     0,   122,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   122,   362,   122,     0,   127,     0,     0,   122,   127,
       0,     0,   122,     0,     0,   127,     0,     0,   127,     0,
       0,     0,     0,   122,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,   122,   362,
       0,     0,   122,   362,     0,     0,     0,   362,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,   127,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   117,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     127,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   127,     0,   127,
     117,     0,   123,     0,   127,     0,     0,     0,   127,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,   127,
     123,     0,     0,     0,     0,   280,     0,   122,     0,     0,
       0,     0,   362,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   127,     0,   127,     0,   123,     0,   127,     0,
       0,     0,     0,   123,     0,   658,     0,     0,   656,     0,
       0,   122,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,   362,   493,     0,     0,
     122,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,   112,     0,     0,     0,     0,     0,     0,     0,
       0,   122,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   429,     0,     0,
       0,   112,   656,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,   358,   127,     0,     0,   362,     0,     0,   362,
     362,     0,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,   122,   122,
       0,   362,     0,     0,     0,   362,     0,   127,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   127,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     117,     0,     0,     0,   117,   117,     0,   127,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,   117,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,   123,
     123,   123,   123,   123,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,   123,   123,     0,
       0,     0,     0,     0,     0,     0,     0,   493,     0,     0,
       0,   123,   362,     0,   127,   127,     0,     0,     0,     0,
       0,   122,     0,     0,     0,     0,     0,     0,     0,   122,
       0,     0,   122,   122,     0,   122,   127,     0,     0,     0,
       0,     0,     0,   171,   122,     0,     0,   122,   122,   122,
       0,     0,     0,     0,   710,     0,     0,     0,   429,   716,
       0,     0,     0,     0,     0,   362,     0,     0,   725,   726,
       0,   171,     0,     0,   362,     0,     0,     0,   362,     0,
       0,   127,     0,   429,   429,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   429,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,     0,
       0,     0,     0,     0,     0,     0,     0,   122,     0,     0,
       0,   171,     0,   171,     0,     0,     0,   429,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   127,     0,     0,
       0,     0,     0,     0,     0,   127,     0,     0,   127,   127,
     280,   127,     0,     0,     0,   383,     0,     0,     0,     0,
     127,     0,     0,   127,   127,   127,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
     383,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   171,
       0,     0,     0,   171,     0,     0,   171,   171,     0,     0,
     171,     0,     0,   171,   171,     0,   171,     0,   171,     0,
       0,     0,     0,   127,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     122,   122,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,     0,
       0,   171,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   117,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,     0,     0,   171,   171,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,     0,     0,     0,     0,     0,     0,   215,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   273,   127,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   127,   127,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   429,   429,   429,   429,   429,   429,   429,
     429,   429,   429,   429,   429,   429,   429,   429,   429,   429,
     429,   429,     0,     0,   215,     0,     0,     0,   333,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,     0,   658,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   171,     0,   127,     0,
       0,     0,     0,     0,   215,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   500,     0,
       0,     0,   505,     0,     0,     0,   117,     0,   429,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   122,     0,     0,   383,   117,   658,     0,     0,     0,
     122,     0,     0,     0,     0,     0,     0,     0,     0,   122,
     171,     0,     0,     0,     0,   215,   362,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,   117,   273,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   122,     0,     0,
       0,     0,     0,     0,   505,     0,     0,     0,     0,     0,
       0,   123,     0,     0,   215,     0,   122,     0,     0,     0,
       0,     0,     0,     0,   383,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   661,     0,   678,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   127,     0,     0,
       0,     0,     0,     0,     0,     0,   127,     0,     0,     0,
       0,     0,     0,     0,   122,   127,   171,   171,     0,     0,
       0,   429,     0,     0,     0,     0,     0,   429,     0,   171,
       0,     0,     0,     0,     0,     0,     0,     0,   429,     0,
       0,   127,     0,     0,     0,     0,     0,     0,   127,   740,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   127,     0,     0,     0,     0,   429,     0,
       0,     0,     0,   215,     0,     0,     0,     0,     0,     0,
       0,     0,   127,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   661,     0,     0,     0,
       0,     0,   846,     0,     0,     0,     0,   122,   122,   122,
     122,   122,   122,   122,     0,     0,     0,     0,     0,     0,
     127,     0,     0,     0,     0,     0,     0,     0,   171,   171,
       0,     0,   122,     0,     0,   171,   122,   122,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,     0,     0,   171,   171,     0,   171,     0,   171,   171,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     215,   215,     0,     0,     0,     0,     0,   500,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   429,     0,     0,   369,     0,     0,     0,     0,
       0,   171,     0,     0,     0,   171,     0,     0,     0,   171,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,   127,   127,   127,   127,   127,   127,   127,
       0,     0,   480,   369,     0,   374,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,   127,   127,     0,   500,     0,   967,     0,     0,
       0,     0,     0,     0,     0,   551,   127,     0,     0,     0,
       0,     0,   551,     0,     0,   171,   171,     0,   661,     0,
     429,     0,     0,     0,     0,     0,     0,     0,     0,   171,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     429,   401,     0,   215,   402,     0,   403,     0,   404,     0,
       0,   215,   122,     0,   740,   215,     0,   215,     0,     0,
     429,   429,   429,     0,     0,   405,   740,   429,   429,   740,
     740,   740,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   127,   122,     0,     0,     0,     0,     0,     0,     0,
     429,     0,     0,   551,     0,   406,   407,     0,   508,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,     0,   415,   416,
     369,   668,     0,     0,     0,   500,    74,     0,     0,     0,
     429,   429,     0,     0,     0,     0,     0,     0,     0,     0,
     689,   122,     0,     0,     0,     0,     0,   417,    77,   215,
     509,   510,     0,     0,     0,   511,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,   171,     0,
     500,     0,     0,   122,     0,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   500,
     169,   500,     0,     0,     0,   500,   500,   500,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   127,     0,
       0,     0,   171,     0,     0,   500,     0,   171,     0,     0,
     171,   551,     0,     0,   171,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   551,   818,
       0,   551,   821,     0,     0,     0,     0,     0,     0,     0,
       0,   369,     0,     0,   122,   668,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   298,     0,   127,   480,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   304,     0,
     305,     0,     0,     0,     0,   500,     0,     0,     0,     0,
       0,     0,     0,   215,     0,     0,     0,   551,     0,   127,
       0,   551,     0,     0,     0,     0,     0,   846,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,   369,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   350,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,   374,     0,    58,   171,     0,
       0,     0,     0,     0,     0,     0,     0,   171,   171,     0,
     127,     0,     0,   553,   554,     0,     0,   558,     0,     0,
     561,   562,     0,   564,     0,   565,     0,   551,     0,     0,
     369,     0,     0,    63,    64,     0,     0,   736,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,   963,
     369,     0,     0,     0,     0,     0,     0,     0,     0,    74,
     668,    75,     0,     0,   668,     0,     0,     0,   171,     0,
       0,   981,     0,   369,     0,   500,   500,   122,     0,   171,
       0,     0,   171,    78,   171,   171,     0,   453,   429,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,   485,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,     0,     0,     0,   514,     0,   514,     0,     0,
       0,     0,   650,   651,     0,     0,     0,     0,     0,     0,
     171,   500,    14,    15,    16,    17,    18,   683,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,   127,     0,     0,     0,     0,     0,
     369,     0,   740,     0,     0,     0,     0,     0,   911,   913,
       0,    58,     0,     0,     0,     0,   551,   551,     0,     0,
       0,     0,     0,   127,     0,     0,     0,   551,  1097,     0,
     551,  1100,     0,   171,     0,     0,   628,   740,     0,     0,
       0,     0,     0,     0,   963,   369,     0,     0,     0,   668,
       0,   668,   668,     0,     0,     0,   127,     0,   668,     0,
       0,     0,     0,   816,   369,     0,   369,     0,   273,     0,
     369,   369,   369,     0,     0,    75,   374,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   215,     0,
     369,     0,   551,     0,     0,     0,   551,     0,     0,   661,
       0,     0,     0,   551,  1174,     0,     0,   551,  1178,     0,
       0,   551,  1182,     0,     0,     0,     0,     0,     0,  1186,
     171,   429,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,     0,     0,     0,     0,   740,   899,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,   736,
       0,     0,     0,     0,     0,   736,     0,     0,     0,     0,
     369,   551,     0,   429,   736,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   171,     0,   736,     0,     0,     0,   171,     0,     0,
       0,     0,     0,   668,     0,     0,     0,     0,     0,     0,
     500,     0,     0,   500,   500,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1076,
       0,     0,     0,     0,     0,     0,   514,     0,     0,     0,
       0,     0,   514,     0,     0,     0,     0,   870,     0,   480,
     369,     0,     0,     0,     0,     0,     0,     0,     0,   740,
     740,   740,     0,     0,   740,   740,     0,   429,     0,   429,
       0,   505,   171,   979,   980,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   987,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   429,     0,
       0,     0,     0,     0,     0,   551,     0,     0,     0,     0,
       0,   155,     0,   171,   171,     0,     0,     0,     0,   273,
     369,   369,     0,     0,   668,   668,     0,     0,     0,   429,
       0,   668,     0,     0,     0,     0,     0,     0,   942,   171,
     171,   374,     0,     0,     0,     0,     0,   383,     0,     0,
       0,     0,   171,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   485,     0,
       0,     0,   429,     0,     0,     0,   369,     0,     0,   551,
    1432,   975,   551,  1436,     0,     0,   551,  1440,     0,     0,
       0,     0,   209,     0,     0,  1090,  1091,     0,     0,     0,
       0,     0,  1095,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1006,     0,     0,     0,     0,     0,  1118,     0,     0,
    1121,  1122,  1016,  1125,     0,  1127,  1128,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     315,     0,     0,   171,     0,     0,   870,  1037,     0,   209,
    1039,     0,  1041,     0,   215,     0,     0,     0,  1006,     0,
    1053,  1006,     0,     0,     0,     0,     0,     0,  1172,     0,
       0,     0,  1176,     0,     0,     0,  1180,     0,     0,     0,
       0,     0,     0,   273,     0,     0,     0,     0,  1081,   454,
       0,   369,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1083,   479,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1092,     0,   369,     0,     0,     0,   171,     0,
     668,  1557,     0,     0,     0,     0,     0,     0,   485,     0,
       0,     0,     0,  1081,     0,     0,     0,     0,     0,     0,
       0,     0,  1301,  1302,     0,     0,   369,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1318,     0,   572,     0,
     576,     0,     0,     0,  1150,     0,     0,   514,     0,     0,
       0,     0,     0,     0,     0,   740,     0,     0,     0,  1161,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     551,  1610,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   551,  1619,   155,   668,     0,     0,     0,  1187,     0,
       0,     0,     0,     0,     0,   369,     0,     0,   369,   369,
       0,     0,     0,   171,     0,     0,     0,     0,     0,   576,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   273,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   453,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1308,  1310,     0,     0,     0,   736,     0,     0,   485,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1318,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   454,     0,     0,     0,     0,     0,
       0,     0,   208,     0,     0,     0,     0,     0,  1081,     0,
       0,     0,     0,     0,     0,     0,  1353,     0,     0,  1424,
       0,     0,     0,  1006,  1430,     0,     0,  1434,   209,     0,
       0,  1438,     0,     0,     0,     0,   369,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   740,   825,     0,     0,
       0,     0,     0,     0,   668,     0,     0,   514,     0,   208,
       0,     0,     0,     0,     0,     0,     0,     0,   479,     0,
       0,     0,     0,     0,     0,   208,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   208,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   487,     0,     0,     0,     0,     0,     0,
     740,     0,     0,   505,     0,     0,     0,     0,     0,   514,
       0,  1423,     0,  1426,     0,   454,   454,     0,     0,     0,
       0,     0,     0,   551,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   551,
       0,     0,     0,     0,     0,  1555,     0,     0,     0,     0,
     208,     0,     0,     0,  1565,  1566,     0,     0,     0,     0,
       0,     0,  1695,  1703,     0,     0,  1695,  1713,     0,     0,
       0,     0,  1720,     0,     0,     0,  1724,     0,  1726,     0,
    1713,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1502,  1502,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1608,     0,     0,     0,   208,
       0,     0,     0,     0,     0,     0,  1617,     0,     0,  1621,
       0,  1624,  1625,     0,     0,     0,     0,     0,     0,     0,
     208,     0,     0,     0,     0,     0,     0,     0,   454,     0,
       0,     0,     0,     0,     0,     0,   454,     0,     0,   454,
     454,     0,   454,     0,     0,     0,  1553,     0,     0,     0,
       0,   454,  1562,     0,   454,   454,   454,  1651,     0,     0,
       0,     0,     0,     0,   551,   551,     0,     0,     0,     0,
    1006,     0,     0,     0,   485,     0,     0,     0,     0,     0,
     551,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   514,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   870,
       0,     0,  1819,     0,     0,     0,     0,     0,   208,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   454,     0,     0,     0,     0,     0,
    1755,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   208,     0,     0,     0,     0,     0,  1856,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1875,  1877,     0,     0,
       0,     0,     0,   551,  1653,  1654,     0,     0,     0,     0,
       0,   551,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1895,     0,     0,     0,
    1006,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1621,     0,   514,
       0,     0,   870,     0,     0,   208,   208,     0,     0,     0,
       0,     0,   487,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1815,     0,     0,     0,     0,
     551,  2091,     0,     0,   551,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1290,   454,     0,
       0,     0,     0,     0,  1037,     0,     0,     0,     0,     0,
       0,     0,     0,  1773,  1774,     0,     0,     0,     0,     0,
     208,     0,     0,     0,   514,     0,     0,   551,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1965,
     487,     0,     0,   514,     0,   870,  1968,     0,  1970,     0,
       0,  1976,  1981,     0,  1713,     0,     0,     0,     0,  1987,
       0,     0,     0,   208,     0,     0,     0,     0,     0,   479,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   551,   551,     0,   208,  1903,
       0,     0,     0,     0,     0,     0,   208,     0,     0,     0,
     208,     0,   208,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   551,     0,   453,     0,     0,
       0,     0,  1844,     0,     0,     0,     0,     0,     0,     0,
    1935,  1936,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2055,
       0,     0,     0,     0,  2061,  2063,  1950,  1951,     0,     0,
     487,     0,     0,     0,     0,     0,     0,     0,     0,  1955,
       0,     0,     0,     0,  2082,     0,     0,     0,     0,     0,
       0,     0,  1890,     0,   208,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   487,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2106,     0,  2109,     0,
       0,     0,  2111,  2113,   487,     0,   487,  2116,  2118,     0,
     487,   487,   487,     0,     0,     0,  1925,     0,  1290,  1927,
       0,     0,     0,     0,     0,     0,     0,  1468,     0,     0,
     487,  1559,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,  1941,     0,     0,     0,     0,
    2027,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   454,     0,     0,     0,     0,     0,     0,     0,
       0,  2159,  2161,  2163,     0,   401,     0,     0,   402,     0,
     403,     0,   404,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,   405,
     487,     0,     0,     0,  2185,  2187,  2189,     0,   208,     0,
       0,     0,     0,   454,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2089,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,     0,   415,   416,     0,     0,     0,     0,     0,     0,
      74,   454,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     208,   417,     0,     0,    78,   418,     0,     0,     0,     0,
       0,   419,  1560,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2200,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1006,     0,     0,     0,
       0,     0,  1484,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     487,   487,     0,   401,     0,     0,   402,     0,   403,     0,
     404,     0,     0,     0,  1468,  1468,  1468,   155,  1665,  1666,
    1670,     0,     0,     0,     0,  1218,     0,   405,  1220,     0,
    1221,  -249,  -249,  1222,  1223,  1224,  1225,  1226,  1227,  1228,
    1229,  1230,  1231,  1232,  1233,  -345,  -345,  1234,  1235,  1236,
    1237,  1238,  1239,  1240,     0,  1241,   487,   406,   407,     0,
     508,   409,  1242,  1243,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,  1244,   412,   413,   414,     0,
     415,   416,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -249,  1245,
       0,     0,    78,   418,     0,     0,     0,   302,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,  1290,     0,  -189,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2200,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1484,
       0,   208,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   208,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   208,     0,     0,     0,     0,     0,
     401,     0,     0,   402,     0,   403,     0,   404,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   454,
       0,     0,  1218,     0,   405,  1220,   208,  1221,  -250,  -250,
    1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,  1231,
    1232,  1233,  -345,  -345,  1234,  1235,  1236,  1237,  1238,  1239,
    1240,     0,  1241,     0,   406,   407,     0,   508,   409,  1242,
    1243,    65,    66,    67,    68,    69,    70,    71,    72,   410,
     411,   398,  1244,   412,   413,   414,     0,   415,   416,     0,
       0,     0,     0,     0,  1901,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   487,     0,     0,   487,   487,
       0,  1484,     0,     0,     0,  -250,  1245,     0,  1290,    78,
     418,     0,     0,     0,   302,     0,   419,    80,    81,   420,
     421,   422,   423,     0,     0,     0,     0,     0,     0,     0,
       0,  -189,   401,     0,     0,   402,     0,   403,     0,   404,
     454,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1218,     0,   405,  1220,     0,  1221,
       0,     0,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    1230,  1231,  1232,  1233,  -345,  -345,  1234,  1235,  1236,  1237,
    1238,  1239,  1240,     0,  1241,     0,   406,   407,     0,   508,
     409,  1242,  1243,    65,    66,    67,    68,    69,    70,    71,
      72,   410,   411,   398,  1244,   412,   413,   414,     0,   415,
     416,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   208,     0,  1245,     0,
       0,    78,   418,     0,     0,     0,   302,     0,   419,    80,
      81,   420,   421,   422,   423,     0,     0,     0,     0,     0,
       0,     0,     0,  -189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     4,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,  1217,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     401,     0,    46,   402,    47,   403,     0,   404,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,  1218,    58,  1219,  1220,     0,  1221,     0,   208,
    1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,  1231,
    1232,  1233,  -345,  -345,  1234,  1235,  1236,  1237,  1238,  1239,
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
    -345,  -345,  1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,
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
       0,     0,     0,     0,     0,  1712,   185,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,   256,
     257,     0,   258,    46,     0,    47,     0,   259,     0,     0,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     4,   185,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   401,
       0,    46,   402,    47,   403,     0,   404,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   405,     0,     0,     0,     0,     0,  -466,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -466,   406,   407,    61,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,     0,   415,   416,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   417,     0,  1705,    78,  1246,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,     4,   185,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   401,     0,
      46,   402,    47,   403,     0,   404,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   405,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   406,   407,    61,   408,   409,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   410,   411,   398,
       0,   412,   413,   414,     0,   415,   416,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   417,     0,     0,    78,  1246,     0,
       0,     0,     0,     0,   419,    80,    81,   420,   421,   422,
     423,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   401,     0,    46,   402,
      47,   403,     0,   404,   350,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     405,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     406,   407,     0,   408,   409,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   410,   411,   398,     0,   412,
     413,   414,     0,   415,   416,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   417,     0,     0,    78,   482,     0,     0,     0,
       0,     0,   419,   483,    81,   420,   421,   422,   423,   185,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   401,     0,    46,   402,    47,   403,
       0,   404,   350,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   405,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   406,   407,
       0,   408,   409,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   410,   411,   398,     0,   412,   413,   414,
       0,   415,   416,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     417,     0,     0,    78,  1305,     0,     0,     0,     0,     0,
     419,  1306,    81,   420,   421,   422,   423,   185,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   401,     0,    46,   402,    47,   403,     0,   404,
     350,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   405,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   406,   407,     0,   408,
     409,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   410,   411,   398,     0,   412,   413,   414,     0,   415,
     416,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   417,     0,
       0,    78,   830,     0,     0,     0,     0,     0,   419,   483,
      81,   420,   421,   422,   423,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     401,     0,    46,   402,    47,   403,     0,   404,   350,    49,
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
     418,     0,     0,     0,     0,     0,   419,    80,    81,   420,
     421,   422,   423,   185,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
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
       0,     0,     0,     0,   417,     0,     0,    78,   830,     0,
       0,     0,     0,     0,   419,    80,    81,   420,   421,   422,
     423,  2037,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,
       0,     0,    -2,     0,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,    -2,    -2,  2066,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,
       0,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,    -2,    -2,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,    59,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    61,    62,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,    79,     0,     0,     0,     0,     0,     0,    80,    81,
     261,   185,     6,     7,     8,     9,    10,    11,    12,    13,
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
       0,     0,    76,    77,     0,    78,   262,     0,     0,     0,
    -825,     0,     0,    80,    81,   261,   185,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
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
      78,   262,     0,     0,     0,     0,     0,     0,    80,    81,
       4,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
       0,     0,     0,     0,  -412,  -412,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -412,     0,     0,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,     4,   185,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,     0,     0,     0,     0,  -413,
    -413,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -413,     0,     0,     0,
      78,    79,     0,  1460,     0,  1461,     0,     0,    80,    81,
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
       0,     0,     0,     0,  1464,     0,     0,     0,    78,  1011,
       0,  1460,     0,  1461,     0,     0,    80,    81,  1462,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1463,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1662,     0,     0,     0,    78,  1011,     0,  1460,
       0,  1461,     0,     0,    80,    81,  1462,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1463,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1663,     0,     0,     0,    78,  1011,     0,  1460,     0,  1461,
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
       0,     0,     0,     0,     0,     0,     0,     0,  1664,     0,
       0,     0,    78,  1011,     0,     0,     0,     0,     0,     0,
      80,    81,   261,   185,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
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
       0,     0,     0,     0,     0,     0,     0,    78,   262,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
     607,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1107,    77,
    -688,    78,   664,     0,     0,     0,     0,     0,     0,    80,
      81,   185,     6,     7,     8,     9,    10,    11,    12,    13,
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
       0,     0,    76,    77,     0,    78,   262,     0,     0,     0,
    -829,     0,     0,    80,    81,   185,     6,     7,     8,     9,
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
     262,     0,     0,     0,     0,     0,     0,    80,    81,   185,
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
       0,    75,   607,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     663,     0,  -688,    78,   664,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,   799,     0,  -688,    78,   548,     0,
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
    1140,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -696,    78,   917,     0,     0,     0,     0,     0,     0,    80,
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
       0,     0,     0,     0,   351,    78,   352,     0,     0,     0,
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
       0,     0,     0,     0,     0,    74,     0,    75,  1631,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     917,     0,     0,     0,     0,     0,     0,    80,    81,   185,
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
       0,    75,  1633,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   331,     0,
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
       0,    78,   917,     0,     0,     0,     0,     0,     0,    80,
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
       0,     0,     0,     0,     0,    78,   352,     0,     0,     0,
       0,     0,     0,    80,    81,   185,     6,     7,     8,     9,
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
      64,     0,     0,     0,     0,     0,  1484,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   401,     0,     0,
     402,     0,   403,     0,   404,     0,     0,     0,     0,    78,
     262,     0,     0,     0,     0,     0,     0,    80,    81,  1218,
       0,   405,  1220,     0,  1221,  1958,  1959,  1222,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,     0,
       0,  1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,
       0,   406,   407,     0,   508,   409,  1242,  1243,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,  1244,
     412,   413,   414,     0,   415,   416,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,  1484,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1245,     0,     0,    78,   418,     0,     0,
       0,   302,     0,   419,    80,    81,   420,   421,   422,   423,
     401,     0,     0,   402,     0,   403,     0,   404,  -189,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1218,     0,   405,  1220,     0,  1221,     0,     0,
    1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,  1231,
    1232,  1233,     0,     0,  1234,  1235,  1236,  1237,  1238,  1239,
    1240,     0,  1241,     0,   406,   407,     0,   508,   409,  1242,
    1243,    65,    66,    67,    68,    69,    70,    71,    72,   410,
     411,   398,  1244,   412,   413,   414,     0,   415,   416,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1245,     0,     0,    78,
     418,     0,     0,     0,   302,     0,   419,    80,    81,   420,
     421,   422,   423,     0,     0,     0,     0,     0,     0,     0,
       0,  -189,   306,   185,     6,     7,     8,     9,    10,    11,
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
       0,     0,  -416,   306,   185,     6,     7,     8,     9,    10,
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
       0,     0,     0,  -417,   306,   185,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,    14,    15,    16,    17,    18,
      19,   727,    20,   728,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    63,
      64,   401,     0,    46,   402,    47,   403,     0,   404,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   405,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   729,     0,     0,
       0,     0,  1233,     0,  -345,     0,     0,     0,     0,    78,
       0,     0,     0,     0,  -416,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,     0,   415,   416,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1245,     0,     0,
      78,   730,     0,     0,     0,   302,     0,   419,    80,    81,
     731,   732,   422,   423,    14,    15,    16,    17,    18,    19,
     727,    20,   728,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     401,     0,    46,   402,    47,   403,     0,   404,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   405,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   729,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   406,   407,     0,   408,   409,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   410,
     411,   398,     0,   412,   413,   414,     0,   415,   416,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   417,     0,     0,    78,
     730,     0,     0,     0,   302,     0,   419,    80,    81,   731,
     732,   422,   423,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   401,
       0,    46,   402,    47,   403,     0,   404,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   405,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,     0,   415,   416,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   417,     0,   448,    78,   449,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   401,     0,
      46,   402,    47,   403,     0,   404,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   405,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   406,   407,     0,   408,   409,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   410,   411,   398,
       0,   412,   413,   414,     0,   415,   416,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   417,     0,     0,    78,   449,     0,
       0,     0,   302,     0,   419,    80,    81,   420,   421,   422,
     423,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   401,     0,    46,
     402,    47,   403,     0,   404,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   405,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,     0,   415,   416,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   417,     0,     0,    78,   730,     0,     0,
       0,   302,     0,   419,    80,    81,   420,   421,   422,   423,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   401,     0,    46,   402,
      47,   403,     0,   404,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     405,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     406,   407,     0,   408,   409,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   410,   411,   398,     0,   412,
     413,   414,     0,   415,   416,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   417,     0,     0,    78,   449,     0,     0,     0,
       0,     0,   419,    80,    81,   420,   421,   422,   423,    14,
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
       0,   419,    80,    81,   420,   421,   422,   423,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   401,     0,    46,   402,    47,   403,
       0,   404,   350,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   405,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   406,   407,
       0,   408,   409,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   410,   411,   398,     0,   412,   413,   414,
       0,   415,   416,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     417,     0,     0,    78,   418,     0,     0,     0,     0,     0,
     419,    80,    81,   420,   421,   422,   423,   185,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   718,
       0,   719,   720,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,   261,   185,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,   -17,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -491,  -491,     0,  -491,    46,     0,    47,     0,  -491,
       0,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   350,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   607,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -688,    78,    14,    15,    16,    17,
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
       0,    78,    79,     0,     0,     0,  -827,     0,     0,    80,
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
       0,     0,     0,    76,    77,     0,    78,   207,     0,     0,
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
       0,    47,     0,     0,     0,   350,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,   476,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   853,     0,     0,    78,   477,     0,     0,
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
       0,     0,     0,     0,   150,     0,   476,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   477,     0,     0,
       0,     0,     0,     0,    80,    81,   185,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   350,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   607,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,  -688,
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
     331,     0,     0,     0,     0,     0,     0,    80,    81,   185,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,  1211,     0,     0,     0,   185,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,    78,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   350,
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
     853,     0,     0,    78,   477,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   350,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,  1579,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   853,     0,     0,    78,   477,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,  1011,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   314,     0,    63,
      64,     0,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     207,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   350,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,   350,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   352,     0,    63,    64,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   314,     0,
       0,     0,     0,     0,     0,    80,    81,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   350,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   477,     0,     0,     0,     0,     0,     0,
      80,    81,   185,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -491,  -491,     0,  -491,    46,
       0,    47,     0,  -491,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,   350,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     331,     0,     0,     0,     0,     0,     0,    80,    81,    14,
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
       0,     0,     0,     0,    78,  1011,     0,    63,    64,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,    79,     0,
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
      47,     0,    63,    64,   350,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   477,     0,    63,    64,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,  1011,     0,    14,    15,
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
       0,     0,     0,    78,   331,     0,    14,    15,    16,    17,
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
     401,     0,    46,   402,    47,   403,     0,   404,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   405,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   406,   407,     0,   408,   409,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   410,
     411,   398,     0,   412,   413,   414,     0,   415,   416,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   417,     0,     0,    78,
     418,     0,     0,     0,     0,     0,   419,   483,    81,   420,
     421,   422,   423,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   401,     0,    46,   402,    47,   403,     0,   404,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   405,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   406,   407,     0,   408,
     409,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   410,   411,   398,     0,   412,   413,   414,     0,   415,
     416,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   417,     0,
       0,    78,   418,     0,     0,     0,     0,     0,   419,    80,
      81,   420,   421,   422,   423,    14,    15,    16,    17,    18,
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
       0,     0,     0,    46,     0,    47,     0,    63,    64,   350,
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
      46,     0,    47,     0,  -491,     0,   185,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   186,     0,
     187,   188,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   185,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,    75,    21,
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
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,    20,    75,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -490,
    -490,     0,  -490,    46,     0,    47,     0,  -490,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -491,  -491,     0,  -491,    46,     0,    47,   401,  -491,     0,
     402,     0,   403,     0,   404,  1973,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,  1974,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,     0,   415,   416,     0,     0,   401,    75,
       0,   402,    74,   403,     0,   404,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1707,  1708,  1709,
       0,     0,   405,   417,  1975,     0,    78,   418,     0,     0,
       0,     0,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   406,   407,     0,   408,   409,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   410,   411,   398,
       0,   412,   413,   414,     0,   415,   416,   401,     0,     0,
     402,     0,   403,    74,   404,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1707,  1708,
    1709,   405,     0,     0,   417,  1876,     0,    78,   418,     0,
       0,     0,     0,     0,   419,    80,    81,   420,   421,   422,
     423,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
     415,   416,   402,     0,   403,     0,   404,  1973,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   417,
    1980,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,  1974,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,   401,   415,   416,   402,     0,
     403,     0,   404,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
       0,     0,     0,     0,     0,   417,     0,     0,    78,   418,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,   401,   415,   416,   402,     0,   403,     0,   404,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,   417,  2060,     0,    78,   418,     0,     0,     0,     0,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,  2062,     0,
      78,   418,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,  2108,     0,    78,   418,     0,     0,
       0,     0,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,   401,
     415,   416,   402,     0,   403,     0,   404,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   417,
    2110,     0,    78,   418,     0,     0,     0,     0,     0,   419,
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
       0,   417,  2115,     0,    78,   418,     0,     0,     0,     0,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,  2117,     0,
      78,   418,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,  2158,     0,    78,   418,     0,     0,
       0,     0,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,   401,
     415,   416,   402,     0,   403,     0,   404,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   417,
    2160,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,   401,   415,   416,   402,     0,
     403,     0,   404,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
       0,     0,     0,     0,     0,   417,  2162,     0,    78,   418,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,   401,   415,   416,   402,     0,   403,     0,   404,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,   417,  2184,     0,    78,   418,     0,     0,     0,     0,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   417,  2186,     0,
      78,   418,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,  2188,     0,    78,   418,     0,     0,
       0,     0,     0,   419,    80,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,   401,
     415,   416,   402,     0,   403,     0,   404,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,     0,     0,     0,     0,     0,   417,
       0,     0,    78,   418,     0,     0,     0,     0,     0,   419,
      80,    81,   420,   421,   422,   423,     0,     0,     0,     0,
       0,     0,     0,   406,   407,     0,   408,   409,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   410,   411,
     398,     0,   412,   413,   414,   401,   415,   416,   402,     0,
     403,     0,   404,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
       0,     0,     0,     0,     0,   709,     0,     0,    78,   418,
       0,     0,     0,     0,     0,   419,    80,    81,   420,   421,
     422,   423,     0,     0,     0,     0,     0,     0,     0,   406,
     407,     0,   408,   409,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   410,   411,   398,     0,   412,   413,
     414,   401,   415,   416,   402,     0,   403,     0,   404,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,     0,     0,     0,     0,
       0,   715,     0,     0,    78,   418,     0,     0,     0,     0,
       0,   419,    80,    81,   420,   421,   422,   423,     0,     0,
       0,     0,     0,     0,     0,   406,   407,     0,   408,   409,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     410,   411,   398,     0,   412,   413,   414,   401,   415,   416,
     402,     0,   403,     0,   404,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,     0,     0,     0,     0,     0,   724,     0,     0,
      78,   418,     0,     0,     0,     0,     0,   419,    80,    81,
     420,   421,   422,   423,     0,     0,     0,     0,     0,     0,
       0,   406,   407,     0,   408,   409,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   410,   411,   398,     0,
     412,   413,   414,   401,   415,   416,   402,     0,   403,     0,
     404,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,     0,     0,
       0,     0,     0,   417,     0,     0,    78,   418,     0,     0,
       0,     0,     0,   419,   941,    81,   420,   421,   422,   423,
       0,     0,     0,     0,     0,     0,     0,   406,   407,     0,
     408,   409,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   410,   411,   398,     0,   412,   413,   414,     0,
     415,   416,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   417,
       0,     0,    78,   418,     0,     0,     0,     0,     0,   419,
     483,    81,   420,   421,   422,   423
};

static const yytype_int16 yycheck[] =
{
       1,    76,   184,     4,    76,     1,    85,   234,   179,    76,
     382,   260,   511,   741,     4,  1263,   954,   168,   229,   135,
     951,   697,   676,   417,   568,   569,   753,   183,    78,   756,
     222,  1245,  1201,    59,    98,   234,   663,     1,   169,    76,
     234,   143,   832,   234,   255,  1827,   663,   234,  1827,  1228,
    1297,  1298,    98,   168,   667,    56,    57,   734,    59,     1,
       1,   184,    98,    59,     1,  1827,   193,   355,   237,     1,
     239,   663,     1,   152,   206,    76,     1,   246,  1958,   275,
     244,  1717,   419,    75,    85,   835,   100,   800,   834,    85,
     234,   841,    93,   234,     1,    59,   323,    98,   234,    76,
     101,   234,    98,   205,   105,   101,   938,   832,   272,   105,
     226,    90,   234,   926,   834,   105,  1050,    59,    59,   283,
     195,    75,    59,   149,   323,   168,   139,  1962,   195,   323,
      59,    76,   323,   149,   947,     0,   323,     1,   832,    10,
     580,     0,   112,   144,   120,  1079,   147,   832,   149,   318,
     319,   591,    59,   149,   155,   171,  1210,   701,   195,   234,
     101,   162,   234,   101,     1,   135,   180,   234,   169,   713,
       0,   235,    84,  1828,   369,   139,   155,   137,   373,   323,
     181,    89,   323,   159,   263,   149,   120,   323,   180,   235,
     323,   307,   193,   194,   195,    59,   636,   234,   194,   235,
     155,   323,   490,     1,   205,   832,     4,   149,   149,   210,
     139,   171,   149,  1147,   210,   832,  2096,   155,   195,   220,
     149,   834,    59,   847,   225,   849,   180,   228,   229,   230,
     162,   244,   139,   234,   235,  1871,   161,   162,   206,   235,
     832,   105,   149,   155,    89,   669,   310,   297,   323,   135,
     195,   323,    76,   332,   255,   163,   323,   234,   158,   272,
     324,    59,   263,   807,   310,    72,   393,   263,  1245,  1713,
     283,   125,   273,   274,   310,   139,   277,   161,   324,   179,
     244,   277,   958,   284,    20,   149,   323,   695,   324,   234,
     137,  1946,   178,   155,   165,   308,   180,   298,   299,   170,
     301,   417,   529,   101,   158,   306,   155,   105,   272,   310,
     353,  1038,   149,  2148,   310,   244,   161,  1107,   163,   283,
     996,   609,   323,   324,   171,   517,   698,   155,   324,   136,
     529,    72,   333,   155,   938,   529,   277,   244,   529,   155,
     341,   342,   529,   272,   308,   346,  2128,  2182,   155,  2128,
    1027,   149,   159,   353,   283,   566,   552,   488,  1108,    72,
     754,   572,   206,   976,   560,   272,  2128,   655,  1114,   369,
     311,   195,   451,   373,   711,  1088,   283,  1365,   480,   308,
     244,    72,  1107,   384,   155,   529,   387,  1219,   529,  1202,
     347,   679,   393,   529,  1114,   136,   529,   155,   686,   501,
     160,   308,   629,  1457,  1458,  1459,  1575,   529,   272,   284,
     234,   112,   836,  1107,   355,  2070,   840,     4,  1210,   283,
    1252,   179,  1107,   136,   299,   405,   590,   851,   852,   386,
     629,  1210,  1876,  1877,   135,   629,    77,    78,   629,   555,
     111,   484,   629,   575,   308,   136,   159,   563,   312,  2099,
     430,   431,   860,  1198,   529,   180,   157,   652,    75,   509,
    1205,   462,   529,   134,   155,   581,   462,   161,   159,    56,
      57,   451,   161,    90,  2129,  2125,   592,   541,    72,   277,
    1107,   155,   633,   923,   484,   629,   487,   488,   629,   163,
    1107,   180,   529,   629,   689,   541,   629,   163,   499,   500,
    2150,  1114,    61,    62,   484,   541,    93,   629,   509,   163,
     511,   854,   855,  2168,   180,  1107,     1,    77,    78,     4,
     651,   653,   163,   130,   131,    62,   180,   243,   529,   161,
     873,  1975,  1976,  1712,   250,    75,  1750,   417,  1717,    75,
     541,  1286,   136,   163,  1521,   541,   154,  1524,  1525,   490,
     170,    91,   529,   161,   629,   271,    92,   144,    10,   161,
     147,   155,   629,  1313,   101,   566,   282,   631,   175,   176,
     110,   572,   180,   574,    59,   162,   113,   590,   115,   161,
     117,   134,   169,  1082,   529,   631,   994,   108,   109,   163,
     633,   161,   629,   709,   161,   631,   170,   161,   180,   715,
     943,   171,   155,   163,   163,   180,   159,   575,   724,   635,
    1598,  1599,  1600,   166,   167,  1219,   180,     3,   205,   156,
     105,   180,   159,   160,  1300,   781,   590,   743,   629,   152,
     631,    69,   827,   676,   635,   631,   637,   158,   225,   635,
     163,   228,    85,   230,  1320,   646,  1374,  1397,  1252,   650,
     651,   783,   652,  1307,  1330,    98,   179,  1904,   101,  1593,
       3,   590,   105,   845,   149,  1457,  1458,  1459,   609,   161,
     862,   635,   161,   210,   161,    72,   676,   965,  1457,  1458,
    1459,   622,   683,   590,   171,   653,   273,   274,   899,   689,
     161,   180,  1871,   635,   635,   696,   828,   284,   635,    72,
     171,   158,   154,   834,   705,   529,   635,  1131,    72,  1454,
     161,   298,   299,   165,   301,   161,   155,    58,   170,   306,
      61,    62,   845,    64,   653,   171,   590,   161,   635,   180,
      72,   575,   782,   163,   849,   137,   859,   171,   679,   136,
     277,  1672,   279,   280,   900,   746,   333,   748,  1475,   750,
     179,   194,  1296,   754,   341,   342,   757,  2005,   155,   346,
    1103,  1104,   159,   136,   166,   167,   161,   210,   963,  1497,
     159,   635,   136,  1750,   311,   164,   171,   161,   134,   316,
     760,   782,   155,   161,  1408,   322,   159,  1411,  1412,   633,
     906,   155,   235,  1972,   136,   159,   180,   384,   635,   155,
     387,   155,   180,   159,   920,   159,    72,  1986,   924,   653,
     166,   167,   928,   155,   155,   783,  1159,   159,   355,   159,
     263,  1759,   177,   360,   164,   362,   827,   827,    72,   709,
     155,   832,   712,   834,   277,   715,   163,   635,   161,    72,
     556,   149,   150,   151,   724,   846,   161,   727,   728,   729,
     157,  1245,   154,   120,   161,   856,   987,   180,  1357,   161,
     828,   862,  1590,   171,   865,   180,   582,  1801,   155,  1803,
     136,   408,  1023,   589,    72,  2054,   134,   593,  1482,   847,
     157,   849,  1486,  1487,   161,   853,   854,   855,  1564,   155,
     160,  1299,   136,   159,    72,   157,  1500,   155,   899,   828,
     487,   159,   155,   136,   155,   873,   168,   169,   166,   167,
     163,   155,   499,   500,   157,   159,   155,   170,   157,   162,
    1022,   179,   155,  1331,   155,   462,   159,   108,   109,  1301,
    1302,   162,   161,     3,    72,   936,   937,   938,   136,   783,
     161,   166,   157,    13,    14,    15,    16,    17,   173,   174,
    1146,   180,  1075,   490,    72,   492,   493,   155,   136,   180,
     950,   159,  1154,   963,  1577,   156,   934,   384,   157,   154,
     387,   508,   163,  1381,   938,   943,   161,   155,     3,   155,
    1023,   159,  1113,  1114,   828,   157,   987,   574,    13,    14,
      15,    16,    17,    72,   157,  1494,   938,   938,   136,   162,
     161,   938,    72,   847,   541,    47,    48,   134,    50,   938,
     157,   157,   157,    55,  1183,   162,   162,   155,   136,   462,
      72,   159,   157,  1311,   965,  1665,   157,   162,   155,   566,
    1670,   938,   159,  1156,   571,   157,   573,   155,  1018,   166,
     167,   159,    58,  1044,  1024,    61,    62,    72,    64,  1050,
     637,    22,   158,   159,   155,  1035,   162,   136,   595,   646,
     597,   598,   155,   650,   651,   157,   136,  1316,  1462,   161,
    1569,   161,   609,    72,   938,    72,   155,   101,  1079,  1290,
     159,  1082,   157,   157,   136,   622,   161,   161,  1204,  1588,
    1766,  1767,   155,    72,   631,   155,   683,   157,   541,   159,
     157,   938,  1218,   155,   161,   155,  1107,   159,   163,   696,
     157,   136,  1113,  1114,   161,   155,   160,   157,   655,   159,
     657,   658,  1238,   761,   762,   763,  1297,  1298,   155,  1245,
     157,  1210,   159,  1284,   163,  1103,  1104,   136,  1546,   136,
     938,    89,   679,   680,   157,  1327,  1147,   157,   161,   686,
     635,   161,   950,   108,   109,  1563,   155,   136,   155,   746,
     159,   748,   159,   750,  1572,   154,   161,   754,   163,  1284,
     757,   149,   150,   151,   157,   157,   155,   159,   161,  1023,
     159,  1589,   155,   161,   157,   155,   159,  1338,  1339,   159,
     180,  1159,   157,   171,  1566,   782,   161,    13,    14,    15,
      16,    17,   180,   163,  1327,   155,   149,   150,   151,   925,
     157,  1201,   157,   158,    13,    14,    15,    16,    17,   155,
     160,  1222,   163,   159,  1225,  1226,  1227,   179,   171,   155,
     149,   150,   151,  1234,   155,  1417,  1418,   180,   159,  1219,
     827,  1284,   161,   157,  1920,   157,   155,   161,  1444,   161,
     159,  1252,   171,   170,  1377,  1219,    72,  1258,   157,   846,
     157,   180,   161,   157,  1307,   168,   169,   161,   120,   856,
    1874,   155,  1273,    72,  2001,  1276,  1277,  1219,  1219,  1280,
    1276,  1277,  1219,  1662,  1663,  1664,  1287,  1277,  1252,  1290,
    1219,   128,   129,  1408,  1417,  1418,  1339,  1412,  1207,  1208,
    1209,   245,   113,   114,   115,   116,   117,  1307,   155,   157,
    1252,  1252,  1219,   161,  1722,  1252,   160,   161,   157,   155,
     136,   137,   161,  1252,  1325,   132,   133,  1307,    13,    14,
      15,    16,    17,    18,   155,  1276,   161,   136,   137,  1340,
    2016,   172,   157,   157,   157,  1252,   161,   161,   161,   936,
     937,   938,   134,   157,   157,  1219,  1357,   161,   161,   157,
    1476,  1477,    62,   161,  1365,  1245,   167,  1347,  1348,  1349,
    1311,  1779,   165,   157,  1354,  1355,  1784,   161,  1457,  1458,
    1459,   157,  1219,  1462,  1463,   161,   157,  1795,  1252,   177,
     161,    13,    14,    15,    16,    17,   158,  1398,  1549,   157,
     987,   157,   157,  1201,  1468,   161,   106,   157,  1780,   157,
    1526,   111,   157,  1277,   114,  1252,   116,   160,   161,   157,
     157,  1219,  1468,   161,   161,  2133,  1482,   157,   965,  2137,
    1486,   159,  1468,   155,  2038,   768,   769,   770,   771,   976,
    1408,   160,   161,  1411,  1412,  1683,  1684,  1685,   985,  1676,
      72,   106,   155,   938,  1252,   110,   111,   112,   113,   114,
     115,   116,   117,  2067,  1344,   950,   160,  1468,   137,  1482,
    1460,  1472,  1473,  1486,   166,   167,   162,  1676,  1276,  1277,
     160,   161,  1676,   160,   161,  1676,   160,   161,   161,  1676,
     161,   162,   137,  1494,   161,  1339,  2100,   160,   161,   160,
     161,  1909,  1910,    91,    92,   162,  1549,   160,   161,   161,
     454,   160,   161,   155,   136,   179,  1517,  1518,  1482,   160,
     161,   155,  1486,  1487,   160,   161,  1527,   471,   160,   161,
     474,  1527,  1676,   160,   161,  1676,  1500,   160,   161,   157,
    1676,  1482,   157,  1676,   179,  1486,  1487,   160,   161,   160,
     161,  1088,   160,  1482,  1676,   160,   161,  1486,  1487,  1500,
      13,    14,    15,    16,    17,    18,   266,  1411,  1569,   161,
     162,  1500,   157,   160,   161,  1482,   157,  1114,   161,  1486,
    1487,   160,   161,   157,  1740,  1575,  1527,  1588,    77,    78,
     534,   157,  1593,  1500,   161,   162,   157,  1598,  1599,  1600,
     157,  1665,  1366,  1367,  1676,   157,  1670,   764,   765,  2017,
     766,   767,   159,   313,  1678,  1842,  1732,   163,  1482,  1665,
     772,   773,  1486,  1487,  1670,  1524,  1525,  1684,  1685,  1665,
     163,   163,  1678,   163,  1670,  1222,  1500,   157,  1225,  1226,
    1227,   163,  1678,  1842,    70,  1482,   180,  1234,  1842,  1486,
    1487,  1842,  1567,  1568,   160,  1842,   155,   357,    78,   359,
     160,   361,  1460,  1500,  1665,  1252,    18,   179,   301,  1670,
     161,  1258,   180,   163,   157,  1676,   157,  1678,  2086,   163,
     160,   163,    18,   180,  1482,  1686,  1273,   160,  1486,  1487,
      18,   160,   160,  1280,   154,   157,  1907,    22,  1842,   157,
    1287,  1842,  1500,   157,  1705,  1549,  1842,   157,   408,  1842,
    1711,   157,  1702,    78,   157,     3,  1201,   157,   157,   157,
    1842,    57,    58,    59,    60,    61,    62,    63,    64,  1527,
     157,   160,   157,  1904,  1219,  1891,   157,   154,  1325,  1276,
     154,   106,   161,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,  1340,  1755,   123,   163,   125,   126,   127,
    1701,   163,   163,    70,   157,  1745,   179,  1252,   163,   157,
     157,  1761,   157,   206,  1311,   301,  1958,  1575,   157,   154,
    1317,  1845,   179,  2071,   157,   163,   163,   155,   157,   161,
     158,   159,  1277,   157,   160,   163,   164,   157,   161,  1845,
    1801,   157,  1803,   157,   161,   160,   157,   157,   508,  1845,
     157,   157,   157,   157,   154,   180,   157,   160,   106,   157,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     157,   157,   161,  1276,  1277,  1958,   157,   157,  1702,   157,
     157,  1842,  1998,   157,  1845,   157,   134,    13,    14,    15,
      16,    17,    18,  1854,  1855,   799,   800,   157,   160,  2070,
    1861,   157,   161,   161,   157,   154,   810,   155,   156,   813,
     161,   155,  1873,   155,   162,   155,   155,   155,   166,   167,
     155,   155,  1883,   155,  1885,  1472,  1473,   587,    14,   162,
     161,  1955,   162,   180,   160,  1896,   160,  1898,  1899,  1900,
     154,  2128,   163,  1701,  1702,  1906,  1907,   154,   163,  1955,
    1874,   180,  2094,  1793,  2096,  1994,  1857,   180,   161,  1955,
     353,   180,   157,   356,   557,   157,   157,   157,   157,  2128,
    1517,  1518,  1469,  1874,  2128,   879,   369,  2128,   160,   160,
     373,  2128,   886,   157,   157,  1874,   890,   157,   154,   161,
     894,   160,   157,  2135,  1955,    13,    14,    15,    16,    17,
      18,  1962,   157,  1761,   157,  1966,   160,  1874,   154,    80,
    1971,  2094,   155,  2096,   155,  1460,   180,  2133,   180,   180,
     180,  2137,  2138,   180,  2128,    92,   154,  2128,   180,  2068,
    1527,   180,  2128,   155,  1994,  2128,   155,  1482,    90,   632,
     157,  1486,  1487,   154,   154,   161,  2128,   161,   154,   154,
    1874,   163,  2135,   157,  2170,  1500,   163,   160,   160,   160,
    2000,   160,   162,   123,   162,   154,   157,  2209,  2029,   157,
     663,   557,   157,   160,   157,   157,   160,  1874,  2194,   157,
    2041,   157,  2198,  2166,  2045,   154,   154,   180,   162,   161,
     154,   484,   157,  2128,   155,   157,  2128,  2213,  2059,  1857,
     155,  2128,   155,   160,   160,  2129,   157,   154,  2068,  2070,
     160,  2072,   154,   157,  2038,   163,  1874,  2193,   154,   157,
     157,   160,   157,  2129,  1527,    75,  2209,    75,   154,  2205,
    1575,  2128,   155,  2129,   180,   180,   157,  2038,   155,  1686,
     180,   160,  2103,  2067,  2168,   160,   632,   154,   154,  2038,
     163,   154,    72,   154,   159,   157,    75,   220,  1705,   157,
     157,    75,  2168,   171,  1711,   108,  2067,  2128,  2129,   158,
     180,  2038,  2168,  2129,    75,   171,  2100,   162,  2067,   154,
    2141,   180,   575,   154,  1088,   180,   106,  2148,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    62,  2100,
    2067,   154,   156,   171,  1701,   171,   799,  2168,  1755,   154,
     162,  2100,  2168,   106,  2038,  2176,   136,   155,   154,  2180,
     161,  2182,   815,   156,   171,   171,   819,     1,   160,   180,
       4,   180,    75,  2100,   157,   155,   156,   101,    85,   832,
     633,  2038,  2203,  2067,   154,   156,   162,   154,   157,   113,
     114,   157,   154,  2214,   157,   155,   180,  1702,  1162,   652,
     653,  1165,  2223,   157,  1793,  1169,  1335,   180,   180,   733,
    2067,   774,   776,   775,  1240,   450,  2100,   777,  1252,   778,
    2038,  1486,  2182,   676,  2096,    59,  1882,  2125,  1500,  1874,
    1979,  2083,   156,  1749,  2165,  1733,   689,  1733,  2068,  2138,
    1280,  2067,    76,  2100,    49,   152,   114,  1854,  1855,  2067,
    2198,    85,    18,   799,  1861,  1955,  1761,  2027,    76,  1463,
     991,  1273,   862,     0,    98,   518,  1873,   101,   268,   815,
     705,   105,  1534,   819,  1761,  1654,  1883,   646,  1885,    -1,
      98,   934,  2100,   799,   799,   192,   210,   799,  1845,  1896,
      -1,  1898,  1899,  1900,    -1,    61,    62,    63,    64,  1906,
    1857,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     783,    -1,    -1,    -1,   168,    -1,    -1,   155,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   277,    -1,  1962,   263,    -1,   192,  1966,
     194,   195,    -1,    -1,  1971,    -1,    -1,    -1,    -1,  1874,
      -1,   205,   206,    -1,   827,   828,   210,    -1,    -1,  1222,
      -1,    -1,    -1,    -1,    -1,    -1,   509,   311,   511,  1946,
      -1,  1234,   316,   159,    -1,   229,    -1,    -1,   322,    -1,
     234,   235,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,   229,    -1,    -1,    -1,    -1,   234,   235,    -1,  1383,
      -1,   255,  2029,    -1,    -1,   332,    -1,    -1,    -1,   263,
    1394,   355,    -1,    -1,  2041,    -1,    -1,   255,  2045,    -1,
      -1,  1094,    -1,   277,    -1,  1098,   353,    -1,    -1,    -1,
      -1,   106,  2059,    -1,  1107,   110,   111,   112,   113,   114,
     115,   116,   117,    72,  1117,  2072,    -1,    -1,    -1,    -1,
      -1,  1124,    -1,    -1,    -1,    -1,   310,    13,    14,    15,
      16,    17,   316,    -1,   408,    -1,    -1,    -1,   322,   323,
     324,    -1,   310,    -1,    -1,    -1,  2103,    -1,   332,    -1,
      -1,   156,    -1,    -1,   159,   323,   324,    -1,    -1,    -1,
     963,    -1,    -1,   966,  2071,    -1,    -1,    -1,  1171,   353,
     354,   355,  1175,    -1,    -1,   134,  1179,   136,    -1,    -1,
      -1,    -1,    -1,  2038,  2141,   369,    72,    -1,    -1,   373,
      -1,  2148,    -1,    -1,   451,    -1,   155,    -1,  1094,    -1,
     159,    -1,  1098,    -1,    -1,    -1,    -1,   166,   167,    -1,
      -1,    -1,  2067,    -1,    -1,    -1,   490,    -1,    -1,  2176,
    1023,  1117,    -1,  2180,    -1,  2182,    -1,   484,  1124,    -1,
      -1,    -1,    -1,   417,   508,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,  2100,  2203,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,  2214,    -1,    -1,
      -1,  2168,    -1,    -1,    -1,    -1,  2223,   451,    -1,   155,
     454,    -1,    -1,   159,    -1,  1171,    -1,    -1,   462,  1175,
     166,   167,    -1,  1179,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,   480,    65,    -1,   573,
     484,    -1,    -1,    -1,   488,    -1,   490,    -1,    -1,    -1,
      -1,   568,   569,    -1,    -1,    -1,    -1,   501,    -1,    -1,
     488,   595,   106,    -1,  1517,  1518,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   609,    -1,    -1,   122,    -1,
     124,    -1,    -1,    -1,    -1,   529,    72,    -1,   622,    -1,
      -1,    -1,  1666,    -1,    -1,    -1,    -1,   541,    -1,    -1,
      -1,   529,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,
      57,    -1,   156,   541,    -1,   159,    -1,    -1,    -1,    -1,
      -1,   655,   566,    -1,   568,   569,    -1,    -1,   572,   862,
      -1,   575,   865,    -1,    -1,    -1,    -1,    -1,   566,    -1,
      -1,    -1,    -1,    -1,   572,   679,    93,    -1,   134,    -1,
     136,    -1,   686,    -1,    -1,    -1,    -1,    -1,    -1,   676,
      -1,    -1,    -1,    -1,    -1,    -1,  1429,    -1,    -1,   155,
    1433,    -1,    -1,   159,  1437,    -1,    -1,    -1,    -1,    -1,
     166,   167,    -1,    -1,   701,   629,    -1,   631,    -1,   633,
      -1,   635,   709,    -1,    -1,    -1,   713,   144,   715,    -1,
     147,   629,    -1,   631,    -1,    -1,    -1,    -1,   652,   653,
      -1,   655,    -1,    -1,    -1,   162,    -1,    -1,    -1,   663,
      -1,    -1,    -1,   667,    -1,    -1,    -1,    -1,    -1,   106,
      -1,    -1,   676,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   686,    -1,  1307,   689,    -1,    -1,    -1,    -1,
      -1,    -1,  1705,  1827,  1828,    -1,    -1,   701,  1711,    -1,
      -1,    -1,    -1,    -1,    -1,   709,    -1,    -1,   712,   713,
      -1,   715,    -1,  1429,    -1,    -1,  1339,  1433,   225,    -1,
     724,  1437,   159,   727,   728,   729,    -1,    -1,   106,    -1,
     807,  1554,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,  1755,    -1,   122,    -1,   124,    -1,    -1,    -1,
      -1,  1044,    -1,    -1,    -1,    -1,    -1,  1050,    -1,    -1,
      -1,    -1,    -1,    -1,   106,    -1,   273,   274,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   284,   156,   783,
     122,   159,   124,    -1,  1607,    -1,  1079,    -1,    -1,  1082,
      -1,    -1,   299,  1616,    -1,   799,   800,  1620,    -1,    -1,
      -1,    -1,    -1,   807,    -1,    -1,    -1,    -1,    -1,   903,
      -1,    -1,  1946,    -1,   156,    -1,    -1,   159,    -1,    -1,
      -1,    13,    -1,   827,   828,    -1,   333,    -1,   832,    -1,
     834,    -1,    -1,    -1,   341,   342,    -1,    -1,  1554,   346,
      -1,  1854,  1855,   847,   832,   849,   834,    -1,  1861,   853,
     854,   855,    -1,    -1,  1147,    -1,    -1,   106,    -1,    -1,
    1873,   110,   111,   112,   113,   114,   115,   116,   117,   873,
    1883,   965,  1885,    -1,   951,    -1,    -1,   384,    -1,    -1,
     387,    -1,    -1,  1896,    -1,  1898,  1899,  1900,    -1,  2023,
      -1,  1607,    -1,  2027,    -1,   899,    88,    -1,    -1,    -1,
    1616,    -1,     1,    -1,  1620,     4,   155,   156,    -1,    -1,
      -1,   899,    -1,    -1,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,  1549,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   938,    -1,  2070,    -1,    -1,   943,
      -1,    -1,    -1,    -1,    -1,    -1,   950,   951,    -1,  1962,
      -1,    -1,    -1,  1966,    -1,    -1,    -1,    -1,  1971,   963,
      59,   965,    -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   976,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     487,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    -1,   499,   500,  2128,  2129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    57,    -1,    -1,   105,    -1,    -1,    -1,
      -1,    65,    66,    67,    68,    -1,  2029,    -1,  1022,  1023,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2041,    -1,
      -1,    -1,  2045,    -1,  2168,    -1,    -1,    -1,    -1,    -1,
     139,    -1,    -1,    -1,   143,    -1,  2059,    -1,    -1,    -1,
     149,    -1,   106,   152,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,  1357,    -1,    -1,    -1,    -1,   168,
      -1,   106,  1365,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,  1088,    -1,    -1,    -1,    -1,    -1,
    2103,    -1,    -1,   192,    -1,   194,    -1,    -1,    -1,  1103,
    1104,    -1,    -1,  1107,    -1,   159,   205,   206,    -1,    -1,
    1114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1107,
      -1,    -1,    -1,   177,    -1,  1113,  1114,    -1,  2141,    -1,
     637,   166,    -1,    -1,    -1,  2148,   235,    -1,    -1,   646,
      -1,    -1,   106,    -1,    -1,   244,   110,   111,   112,   113,
     114,   115,   116,   117,   118,  1159,   255,    -1,   122,    -1,
     124,   260,   261,  2176,   263,    -1,    -1,  2180,    -1,  2182,
     106,    -1,   108,   272,   110,   111,   112,   113,   114,   115,
     116,   117,  1276,    -1,   283,    -1,    -1,   286,    -1,   696,
    2203,   290,   156,    -1,    -1,    -1,   295,  1201,    -1,    -1,
      -1,  1494,    -1,    89,    -1,    -1,  1210,   306,    -1,   308,
      -1,    -1,    -1,   312,    -1,  1219,    -1,  1311,    -1,  1296,
      -1,    -1,    -1,    -1,    -1,   324,    -1,    -1,    -1,    -1,
    1307,    -1,    -1,   332,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1245,    -1,    -1,   130,    -1,    -1,    -1,  1252,    -1,
      -1,    -1,    -1,    -1,   353,    -1,    -1,   356,    -1,    -1,
      -1,    -1,    13,    -1,    -1,  2088,    -1,    -1,    -1,    -1,
     369,    -1,  1276,  1277,   373,    -1,  1569,    -1,    -1,    -1,
    1284,    -1,    -1,    -1,    -1,    -1,  1290,    -1,    -1,    -1,
      -1,    -1,  1296,    -1,    -1,  1588,    -1,    -1,    -1,    -1,
    1593,    -1,  1290,  1307,    -1,  1598,  1599,  1600,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,   417,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1339,    -1,    88,    -1,   846,
    1344,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   856,
      -1,    -1,   451,    -1,    -1,   106,  1450,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,  1994,  2088,    -1,    -1,  1469,    -1,    -1,    -1,    72,
      -1,   480,    -1,    -1,    -1,   484,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,   501,    -1,  1408,    -1,    -1,  1411,  1412,    -1,
    1398,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
     937,    -1,    -1,  1527,   320,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,  2068,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1457,  1458,  1459,  1460,    -1,  1462,  1463,
     180,    -1,   155,   156,  1468,  1469,   159,    -1,    -1,   568,
     569,    -1,    -1,   166,   167,    72,   575,    -1,  1482,    -1,
    1468,    -1,  1486,  1487,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   590,    -1,    -1,    -1,   106,  1500,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,  1801,   106,
    1803,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   106,  1527,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   633,    -1,   635,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,  1549,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   652,   653,    -1,    -1,    -1,   155,   156,
      -1,    -1,   159,    -1,    -1,    -1,    -1,    -1,   667,   166,
     167,  1575,    -1,  1577,    -1,    -1,    -1,   676,    -1,    -1,
      -1,    -1,   681,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     689,    -1,    -1,    -1,    -1,  1672,   180,   483,    -1,   485,
      -1,    -1,   701,    -1,    -1,    -1,    -1,  1701,   494,    72,
     709,    -1,    -1,   712,   713,    -1,   715,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   724,    -1,    -1,   727,   728,
     729,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,  1665,  1666,    -1,    -1,    -1,  1670,    -1,  1672,    -1,
      -1,   134,  1676,   136,  1678,    -1,    -1,  1665,    -1,    -1,
      -1,    -1,  1670,   158,   783,    -1,    -1,    -1,  1676,    -1,
    1678,    -1,   155,   156,    -1,    -1,    -1,  1701,  1702,    -1,
      -1,   800,    -1,   166,   167,    -1,    -1,    -1,   807,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1225,  1226,
    1227,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   827,   828,
      -1,    -1,    -1,    -1,   106,   834,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,   847,    -1,
     849,  1258,    -1,    -1,   853,   854,   855,  1761,    -1,    -1,
      -1,    -1,    -1,  1857,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1280,   873,    -1,    -1,    -1,    -1,    -1,
    1287,    13,    14,    15,    16,    17,    18,   159,    20,  1793,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,  1325,    51,
      -1,    53,    -1,  1827,  1828,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,  1842,   938,
      72,  1845,    -1,    -1,   943,    -1,    -1,    -1,    -1,    -1,
      -1,   950,   951,  1857,  1842,    -1,    -1,  1845,    -1,    -1,
      -1,    -1,    -1,    -1,   963,    -1,    -1,   966,    -1,    -1,
    1874,    -1,    -1,    -1,   973,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   134,  1907,   136,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,  1907,
      -1,    -1,    -1,  1022,  1023,   157,   158,   159,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,  1946,    -1,    72,    -1,    -1,   163,    -1,    -1,
      -1,  1955,    -1,    -1,   170,    -1,    -1,   158,    -1,   845,
      -1,    -1,   163,    -1,    -1,  1472,  1473,  1955,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2071,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
    1994,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1103,  1104,   134,    -1,   136,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,  2027,    -1,    -1,    -1,   155,   156,    -1,
      -1,   159,    -1,    -1,  2038,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,   941,   942,    -1,    -1,    -1,
    1159,    -1,    -1,  2067,  2068,    -1,  2070,  2071,   163,    -1,
      -1,     1,    -1,    -1,     4,   170,    -1,    -1,    -1,    -1,
      -1,    -1,  2070,    -1,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,  2100,    -1,    -1,    -1,
      -1,    -1,  1201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1210,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
    1219,    -1,    -1,    -1,  2128,  2129,    -1,    -1,    -1,    59,
       3,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
    2128,  2129,    -1,    -1,   166,   167,  1245,    -1,    72,    -1,
      -1,  1037,    -1,  1252,    -1,    85,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   105,    -1,    -1,  1277,  1686,
    2168,    -1,   106,    -1,    -1,  1284,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,  1081,    -1,  1296,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,  1307,   139,
     134,    -1,   136,   143,    -1,    -1,    -1,  1316,    -1,   149,
      -1,    -1,   152,    -1,    -1,    -1,   156,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,   167,   168,   169,
    1339,    -1,   166,   167,  1130,  1344,  1132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,  1144,  1145,
      -1,    -1,   192,    -1,  1150,  1151,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1160,   205,   206,    -1,    -1,    -1,
     210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,  1187,    -1,    -1,  1190,    -1,    -1,    -1,    -1,  1408,
      -1,    -1,  1411,  1412,   244,    -1,   134,   106,   136,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,   261,    -1,   263,   207,    -1,    -1,   155,   156,    -1,
      -1,    -1,   272,    -1,    -1,    -1,    -1,   277,   166,   167,
      -1,    -1,    -1,   283,    -1,    -1,    -1,    -1,  1457,  1458,
    1459,  1460,  1461,  1462,  1463,    -1,  1252,   156,   298,    -1,
     159,   301,    -1,    -1,    -1,    -1,   306,    -1,   308,    -1,
      -1,   311,   312,  1482,    -1,    -1,   316,  1486,  1487,   262,
      -1,    -1,   322,    -1,    -1,    -1,    -1,  1283,    -1,  1906,
      -1,  1500,   332,    -1,    -1,  1291,    -1,  1293,  1294,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
    1306,    -1,  1308,   353,  1310,   355,   356,    -1,    -1,   302,
      -1,  1317,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   369,
      -1,   314,    -1,   373,    -1,    -1,    -1,    -1,    -1,    -1,
    1549,    -1,    -1,    -1,    -1,    -1,    -1,   106,   331,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    72,    -1,  1575,    -1,    -1,   352,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   417,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1391,  1392,    -1,   106,   158,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   451,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1416,    -1,    -1,    -1,    -1,    -1,   134,  1423,   136,    -1,
      -1,  1427,    -1,    -1,    -1,   418,    -1,    -1,    -1,    -1,
     480,    -1,    72,    -1,   484,    -1,    -1,   155,   156,    -1,
     490,    -1,    -1,    -1,    -1,    -1,    -1,  1453,   166,   167,
      -1,   501,    -1,  1672,    -1,    -1,   449,    -1,    -1,  1678,
      -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,    -1,  1702,   477,    -1,    -1,    -1,    -1,   482,
      -1,    -1,    -1,    -1,   134,   106,   136,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   557,    -1,   502,
      -1,    -1,    -1,   506,   507,   155,   156,   510,   568,   569,
      -1,    -1,    -1,   573,    72,   575,   166,   167,    -1,    -1,
      -1,    -1,   525,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     590,    -1,  1761,    -1,   155,    -1,    -1,  1553,    -1,    -1,
      -1,    -1,    -1,    -1,  1560,   548,  1562,    -1,   106,   609,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,   622,    -1,  1793,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   632,   633,    -1,   635,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
     650,    -1,   652,   653,    -1,   655,    -1,   155,   156,  1828,
      -1,    -1,    -1,   663,    -1,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,   676,    -1,    -1,   679,
      -1,  1637,   625,   683,    -1,     1,   686,    -1,    -1,   689,
      -1,   691,    49,    -1,    -1,    52,   639,    54,    -1,    56,
      -1,   701,    -1,    -1,    -1,  1874,    -1,    -1,    -1,   709,
      -1,    -1,   712,   713,    -1,   715,    73,    -1,    -1,    -1,
      -1,   664,    -1,    -1,   724,    -1,    -1,   727,   728,   729,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    59,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   155,
      -1,    -1,    -1,   783,    -1,    -1,    -1,   730,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   799,
      -1,   158,   159,    -1,    -1,    -1,    -1,   807,   165,   166,
     167,   168,   169,   170,   171,   815,    -1,  1773,  1774,   819,
      -1,    -1,    -1,   139,    -1,  1994,    -1,   827,   828,    -1,
      -1,  1787,   832,   149,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   847,    -1,   849,
      -1,    -1,   168,   853,   854,   855,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,  2038,
      -1,    -1,    -1,   873,    -1,    -1,    -1,    -1,    -1,   822,
      -1,   824,    -1,    -1,   155,   156,    -1,   830,    -1,    -1,
     206,   162,    -1,    -1,    -1,   166,   167,    -1,  2067,  2068,
      -1,    -1,    -1,   903,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,   857,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   866,    -1,    -1,    -1,    -1,   244,   872,
      -1,  2100,    -1,    -1,   934,    -1,    -1,    -1,   938,    -1,
      -1,    -1,    -1,   943,    -1,    -1,    -1,    -1,    -1,    -1,
     950,   951,    -1,    -1,    -1,    -1,   272,    -1,    -1,    -1,
    2129,    -1,    -1,   963,    72,   965,   966,   283,    -1,    -1,
     286,    -1,    -1,    -1,   917,    -1,    -1,    -1,    -1,   922,
      -1,    -1,    -1,    -1,    -1,   301,    -1,    -1,    -1,    -1,
      -1,    -1,   308,    -1,    -1,    -1,   312,    -1,   106,    -1,
      -1,    -1,   110,   111,   112,   113,   114,   115,   116,   117,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,    -1,  1022,  1023,   152,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,     5,    -1,    -1,   353,    -1,    -1,
     356,    -1,    13,    14,    15,    16,    17,   155,   156,    -1,
      -1,   179,    -1,   369,    -1,    -1,    -1,   373,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1011,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1094,    -1,    -1,    -1,  1098,    -1,
      -1,    72,    73,  1103,  1104,    -1,    -1,  1107,    -1,    -1,
      -1,    -1,    -1,  2069,    -1,    -1,    -1,  1117,    -1,    -1,
      -1,    -1,    -1,    -1,  1124,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,  1159,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,   484,    -1,
    2126,  1171,    -1,    -1,    -1,  1175,    -1,    -1,    -1,  1179,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,  2149,   165,   166,   167,   168,   169,   170,
     171,  1201,    -1,    -1,    -1,    -1,    -1,    -1,  2164,    -1,
    1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,  1219,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   557,    -1,    -1,    -1,  1245,   134,    -1,    -1,    -1,
      -1,    -1,  1252,    -1,  1197,    -1,    -1,    -1,    -1,   575,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,   159,    -1,    -1,   590,    -1,  1276,  1277,   166,   167,
      -1,    -1,    -1,    -1,  1284,    -1,    -1,    -1,    -1,    -1,
      -1,   179,    -1,    -1,    -1,    -1,  1296,    -1,    -1,    -1,
      -1,    -1,    -1,  1246,    -1,    -1,    -1,  1307,    -1,    -1,
      -1,  1311,    -1,    -1,    -1,    -1,   632,   633,    -1,   635,
      -1,    -1,    -1,   106,  1324,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   652,   653,  1338,  1339,
      -1,    -1,    -1,    -1,  1344,    -1,   106,   663,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
     676,    -1,  1305,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,   689,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1332,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,   159,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,  1408,    -1,
      -1,  1411,  1412,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    59,    -1,  1429,
      -1,    -1,    -1,  1433,    -1,    -1,    -1,  1437,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1450,    -1,    -1,    -1,    -1,    -1,    -1,  1457,  1458,  1459,
    1460,  1461,  1462,  1463,    -1,    -1,    -1,   783,    -1,  1469,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      59,    -1,  1482,   799,    -1,    -1,  1486,  1487,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   815,
    1500,    -1,    -1,   819,    -1,    -1,    85,   139,    -1,    -1,
      -1,   827,   828,    -1,    -1,    -1,   832,   149,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,  1527,    -1,    -1,
      -1,   847,    -1,   849,    -1,   114,   168,   853,   854,   855,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1549,
      -1,    -1,    -1,    -1,  1554,    -1,    -1,   873,    -1,    -1,
     139,    -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   206,  1575,    -1,    -1,    -1,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,  1536,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,  1607,    -1,   134,
      -1,    -1,   244,   192,    -1,    -1,  1616,    -1,   934,    -1,
    1620,    -1,   938,   134,    -1,    -1,   205,   943,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     272,   166,   167,    -1,   155,   156,    -1,   963,    -1,    -1,
     966,   283,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   244,    -1,    -1,    -1,   301,
      -1,    -1,  1672,    -1,    -1,    -1,   308,    -1,    -1,    -1,
     312,    -1,   261,    -1,   263,    -1,    -1,    -1,    -1,   268,
      -1,    -1,    -1,   272,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1701,  1702,    -1,   283,    -1,    -1,  1023,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1659,    -1,    -1,    -1,
      -1,   353,    -1,    -1,   356,    -1,    -1,   306,    -1,   308,
      -1,    -1,    -1,   312,    -1,    -1,    -1,   369,    -1,    -1,
      -1,   373,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   332,    -1,    -1,    -1,    -1,    -1,    -1,
     104,  1761,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,  1094,    -1,
      -1,    -1,  1098,    -1,    -1,    -1,    -1,  1103,  1104,    -1,
      -1,  1107,    -1,  1793,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1117,    -1,    -1,    -1,    -1,    -1,    -1,  1124,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,  1762,
    1763,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   417,    -1,
      -1,    -1,    -1,  1159,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   484,    -1,    -1,  1171,    -1,  1857,    -1,  1175,
      -1,    -1,    -1,  1179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   451,    -1,  1874,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   480,    -1,  1219,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   501,    -1,    -1,   557,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1252,    -1,    -1,    -1,
      -1,    -1,    -1,   575,    -1,    -1,  1889,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   590,    -1,
      -1,  1277,    -1,    -1,    -1,    -1,    -1,    -1,  1284,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1923,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   568,
     569,  1307,    -1,    -1,  1994,    -1,    -1,    -1,    -1,    -1,
     632,   633,    -1,   635,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   590,    -1,    -1,    -1,    -1,    -1,  1960,    -1,    -1,
     652,   653,  1338,  1339,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   663,    -1,    -1,    -1,    -1,    -1,    -1,  2038,    -1,
      -1,    -1,    -1,    -1,   676,    -1,  1989,    -1,    -1,    -1,
    1993,    -1,    -1,    -1,    -1,    -1,   635,   689,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2067,  2068,    -1,
      -1,  2071,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2088,    -1,
      -1,    -1,  1408,    -1,    -1,  1411,  1412,    -1,    -1,    -1,
    2100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1429,    -1,    -1,    -1,  1433,    -1,    -1,
      -1,  1437,   701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     709,    -1,    -1,   712,   713,    -1,   715,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   724,    -1,    -1,   727,   728,
     729,   783,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1482,   799,    -1,    -1,
    1486,  1487,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   815,  1500,    -1,    -1,   819,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   827,   828,    -1,    -1,    -1,
     832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   847,    -1,   849,    -1,    -1,
      -1,   853,   854,   855,    -1,    -1,    -1,    -1,   807,    -1,
      -1,     1,     3,  1549,     4,    -1,    -1,    -1,  1554,    -1,
      -1,   873,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    59,
      -1,  1607,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1616,    72,   934,    -1,  1620,    -1,   938,    -1,    -1,    -1,
      -1,   943,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   963,    -1,    -1,   966,   105,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,   938,
       4,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,   139,
      -1,   950,   951,   143,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,    -1,    -1,    -1,  1702,   158,   159,    -1,
      -1,  1023,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,    -1,
      -1,    -1,   192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   205,    -1,    -1,    -1,    -1,
      -1,    85,    -1,  1022,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,  1094,    -1,    -1,    -1,  1098,    -1,    -1,    -1,
      -1,  1103,  1104,    -1,   244,  1107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1117,    -1,    -1,    -1,    -1,
      -1,   261,  1124,   263,    -1,   139,    -1,    -1,   268,   143,
      -1,    -1,   272,    -1,    -1,   149,    -1,    -1,   152,    -1,
      -1,    -1,    -1,   283,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1159,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   306,    -1,   308,  1171,
      -1,    -1,   312,  1175,    -1,    -1,    -1,  1179,   192,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1874,    -1,
      -1,   205,   332,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1219,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   261,    -1,   263,
    1252,    -1,  1201,    -1,   268,    -1,    -1,    -1,   272,    -1,
      -1,  1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   283,
    1219,    -1,    -1,    -1,    -1,  1277,    -1,   417,    -1,    -1,
      -1,    -1,  1284,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   306,    -1,   308,    -1,  1245,    -1,   312,    -1,
      -1,    -1,    -1,  1252,    -1,  1307,    -1,    -1,  1994,    -1,
      -1,   451,    -1,    -1,    -1,    -1,    -1,    -1,   332,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1277,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1338,  1339,    -1,    -1,
     480,    -1,    -1,    -1,    -1,    -1,    -1,  1296,    -1,    -1,
      -1,    -1,  2038,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   501,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,    -1,    -1,
      -1,  2067,  2068,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1344,    -1,    -1,    -1,    -1,
      -1,    -1,  2088,   417,    -1,    -1,  1408,    -1,    -1,  1411,
    1412,    -1,    -1,    -1,  2100,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1429,   568,   569,
      -1,  1433,    -1,    -1,    -1,  1437,    -1,   451,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     590,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   480,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1482,    -1,    -1,    -1,  1486,  1487,    -1,   501,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,  1500,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1457,  1458,
    1459,  1460,  1461,  1462,  1463,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1482,    -1,    -1,    -1,  1486,  1487,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1549,    -1,    -1,
      -1,  1500,  1554,    -1,   568,   569,    -1,    -1,    -1,    -1,
      -1,   701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   709,
      -1,    -1,   712,   713,    -1,   715,   590,    -1,    -1,    -1,
      -1,    -1,    -1,    48,   724,    -1,    -1,   727,   728,   729,
      -1,    -1,    -1,    -1,   401,    -1,    -1,    -1,   405,   406,
      -1,    -1,    -1,    -1,    -1,  1607,    -1,    -1,   415,   416,
      -1,    76,    -1,    -1,  1616,    -1,    -1,    -1,  1620,    -1,
      -1,   635,    -1,   430,   431,    -1,  1575,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   451,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   807,    -1,    -1,
      -1,   136,    -1,   138,    -1,    -1,    -1,   484,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   701,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   709,    -1,    -1,   712,   713,
    1702,   715,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
     724,    -1,    -1,   727,   728,   729,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1672,    -1,    -1,    -1,    -1,    -1,    -1,
     195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1702,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   234,
      -1,    -1,    -1,   238,    -1,    -1,   241,   242,    -1,    -1,
     245,    -1,    -1,   248,   249,    -1,   251,    -1,   253,    -1,
      -1,    -1,    -1,   807,    -1,    -1,    -1,    -1,   938,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     950,   951,  1761,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1793,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   323,    -1,
      -1,   326,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1874,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1022,    -1,    -1,   350,   351,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     365,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   938,  1874,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   950,   951,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   760,   761,   762,   763,   764,   765,   766,
     767,   768,   769,   770,   771,   772,   773,   774,   775,   776,
     777,   778,    -1,    -1,   152,    -1,    -1,    -1,   156,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,    -1,  1994,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   471,    -1,  1022,    -1,
      -1,    -1,    -1,    -1,   192,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   206,    -1,
      -1,    -1,   210,    -1,    -1,    -1,  2038,    -1,   845,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1201,    -1,    -1,   529,  2067,  2068,    -1,    -1,    -1,
    1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1219,
     545,    -1,    -1,    -1,    -1,   263,  2088,    -1,    -1,  2038,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2100,   277,
      -1,    -1,    -1,    -1,    -1,  1245,    -1,    -1,    -1,    -1,
      -1,    -1,  1252,    -1,    -1,    -1,    -1,    -1,  2067,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1277,    -1,    -1,
      -1,    -1,    -1,    -1,   322,    -1,    -1,    -1,    -1,    -1,
      -1,  2100,    -1,    -1,   332,    -1,  1296,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   629,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   353,    -1,   355,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1201,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1210,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1344,  1219,   671,   672,    -1,    -1,
      -1,  1018,    -1,    -1,    -1,    -1,    -1,  1024,    -1,   684,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1035,    -1,
      -1,  1245,    -1,    -1,    -1,    -1,    -1,    -1,  1252,   417,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1277,    -1,    -1,    -1,    -1,  1075,    -1,
      -1,    -1,    -1,   451,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1296,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,
      -1,    -1,   490,    -1,    -1,    -1,    -1,  1457,  1458,  1459,
    1460,  1461,  1462,  1463,    -1,    -1,    -1,    -1,    -1,    -1,
    1344,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   803,   804,
      -1,    -1,  1482,    -1,    -1,   810,  1486,  1487,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1500,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     835,    -1,    -1,   838,   839,    -1,   841,    -1,   843,   844,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     568,   569,    -1,    -1,    -1,    -1,    -1,   575,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1219,    -1,    -1,   168,    -1,    -1,    -1,    -1,
      -1,   886,    -1,    -1,    -1,   890,    -1,    -1,    -1,   894,
      -1,    -1,    -1,    -1,    -1,  1575,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1457,  1458,  1459,  1460,  1461,  1462,  1463,
      -1,    -1,   205,   206,    -1,   633,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1482,    -1,
      -1,    -1,  1486,  1487,    -1,   653,    -1,   655,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   238,  1500,    -1,    -1,    -1,
      -1,    -1,   245,    -1,    -1,   960,   961,    -1,   676,    -1,
    1307,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   974,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1327,    49,    -1,   701,    52,    -1,    54,    -1,    56,    -1,
      -1,   709,  1672,    -1,   712,   713,    -1,   715,    -1,    -1,
    1347,  1348,  1349,    -1,    -1,    73,   724,  1354,  1355,   727,
     728,   729,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1575,  1702,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1377,    -1,    -1,   326,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
     353,   354,    -1,    -1,    -1,   783,   134,    -1,    -1,    -1,
    1417,  1418,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     373,  1761,    -1,    -1,    -1,    -1,    -1,   155,   156,   807,
     158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,  1113,    -1,
     828,    -1,    -1,  1793,    -1,    -1,    -1,    -1,  1672,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   847,
      48,   849,    -1,    -1,    -1,   853,   854,   855,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1702,    -1,
      -1,    -1,  1157,    -1,    -1,   873,    -1,  1162,    -1,    -1,
    1165,   454,    -1,    -1,  1169,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   471,   472,
      -1,   474,   475,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   484,    -1,    -1,  1874,   488,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   123,    -1,  1761,   501,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,
     138,    -1,    -1,    -1,    -1,   943,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   951,    -1,    -1,    -1,   530,    -1,  1793,
      -1,   534,    -1,    -1,    -1,    -1,    -1,   965,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,   575,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,  1023,    -1,    72,  1313,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1322,  1323,    -1,
    1874,    -1,    -1,   241,   242,    -1,    -1,   245,    -1,    -1,
     248,   249,    -1,   251,    -1,   253,    -1,   630,    -1,    -1,
     633,    -1,    -1,   108,   109,    -1,    -1,   417,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2038,   652,
     653,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
     663,   136,    -1,    -1,   667,    -1,    -1,    -1,  1383,    -1,
      -1,   674,    -1,   676,    -1,  1103,  1104,  2067,    -1,  1394,
      -1,    -1,  1397,   158,  1399,  1400,    -1,   192,  1745,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   206,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2100,    -1,    -1,    -1,    -1,   220,    -1,   222,    -1,    -1,
      -1,    -1,   350,   351,    -1,    -1,    -1,    -1,    -1,    -1,
    1445,  1159,    13,    14,    15,    16,    17,   365,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,  2038,    -1,    -1,    -1,    -1,    -1,
     783,    -1,  1210,    -1,    -1,    -1,    -1,    -1,   568,   569,
      -1,    72,    -1,    -1,    -1,    -1,   799,   800,    -1,    -1,
      -1,    -1,    -1,  2067,    -1,    -1,    -1,   810,   811,    -1,
     813,   814,    -1,  1528,    -1,    -1,   321,  1245,    -1,    -1,
      -1,    -1,    -1,    -1,   827,   828,    -1,    -1,    -1,   832,
      -1,   834,   835,    -1,    -1,    -1,  2100,    -1,   841,    -1,
      -1,    -1,    -1,   471,   847,    -1,   849,    -1,  1276,    -1,
     853,   854,   855,    -1,    -1,   136,  1284,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1296,    -1,
     873,    -1,   875,    -1,    -1,    -1,   879,    -1,    -1,  1307,
      -1,    -1,    -1,   886,   887,    -1,    -1,   890,   891,    -1,
      -1,   894,   895,    -1,    -1,    -1,    -1,    -1,    -1,   902,
    1615,  1958,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1339,    -1,    -1,    -1,    -1,  1344,   545,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1643,   709,
      -1,    -1,    -1,    -1,    -1,   715,    -1,    -1,    -1,    -1,
     943,   944,    -1,  2000,   724,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1676,    -1,   743,    -1,    -1,    -1,  1682,    -1,    -1,
      -1,    -1,    -1,   976,    -1,    -1,    -1,    -1,    -1,    -1,
    1408,    -1,    -1,  1411,  1412,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   779,
      -1,    -1,    -1,    -1,    -1,    -1,   511,    -1,    -1,    -1,
      -1,    -1,   517,    -1,    -1,    -1,    -1,   522,    -1,  1022,
    1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1457,
    1458,  1459,    -1,    -1,  1462,  1463,    -1,  2094,    -1,  2096,
      -1,  1469,  1757,   671,   672,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2135,    -1,
      -1,    -1,    -1,    -1,    -1,  1088,    -1,    -1,    -1,    -1,
      -1,     4,    -1,  1808,  1809,    -1,    -1,    -1,    -1,  1527,
    1103,  1104,    -1,    -1,  1107,  1108,    -1,    -1,    -1,  2166,
      -1,  1114,    -1,    -1,    -1,    -1,    -1,    -1,   623,  1834,
    1835,  1549,    -1,    -1,    -1,    -1,    -1,  1842,    -1,    -1,
      -1,    -1,  1847,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   653,    -1,
      -1,    -1,  2209,    -1,    -1,    -1,  1159,    -1,    -1,  1162,
    1163,   666,  1165,  1166,    -1,    -1,  1169,  1170,    -1,    -1,
      -1,    -1,    85,    -1,    -1,   803,   804,    -1,    -1,    -1,
      -1,    -1,   810,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   706,    -1,    -1,    -1,    -1,    -1,   835,    -1,    -1,
     838,   839,   717,   841,    -1,   843,   844,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,    -1,    -1,  1948,    -1,    -1,   741,   742,    -1,   152,
     745,    -1,   747,    -1,  1672,    -1,    -1,    -1,   753,    -1,
     755,   756,    -1,    -1,    -1,    -1,    -1,    -1,   886,    -1,
      -1,    -1,   890,    -1,    -1,    -1,   894,    -1,    -1,    -1,
      -1,    -1,    -1,  1701,    -1,    -1,    -1,    -1,   783,   192,
      -1,  1284,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   796,   205,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   807,    -1,  1307,    -1,    -1,    -1,  2023,    -1,
    1313,  1314,    -1,    -1,    -1,    -1,    -1,    -1,   823,    -1,
      -1,    -1,    -1,   828,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   960,   961,    -1,    -1,  1339,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   974,    -1,   261,    -1,
     263,    -1,    -1,    -1,   859,    -1,    -1,   862,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1793,    -1,    -1,    -1,   874,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1383,  1384,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1394,  1395,   306,  1397,    -1,    -1,    -1,   903,    -1,
      -1,    -1,    -1,    -1,    -1,  1408,    -1,    -1,  1411,  1412,
      -1,    -1,    -1,  2128,    -1,    -1,    -1,    -1,    -1,   332,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1857,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   951,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     965,   966,    -1,    -1,    -1,  1245,    -1,    -1,   973,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   417,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,  1023,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1031,    -1,    -1,  1157,
      -1,    -1,    -1,  1038,  1162,    -1,    -1,  1165,   451,    -1,
      -1,  1169,    -1,    -1,    -1,    -1,  1549,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1994,   480,    -1,    -1,
      -1,    -1,    -1,    -1,  1577,    -1,    -1,  1082,    -1,   152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   501,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   206,    -1,    -1,    -1,    -1,    -1,    -1,
    2068,    -1,    -1,  2071,    -1,    -1,    -1,    -1,    -1,  1154,
      -1,  1156,    -1,  1158,    -1,   568,   569,    -1,    -1,    -1,
      -1,    -1,    -1,  1666,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1682,
      -1,    -1,    -1,    -1,    -1,  1313,    -1,    -1,    -1,    -1,
     263,    -1,    -1,    -1,  1322,  1323,    -1,    -1,    -1,    -1,
      -1,    -1,  1482,  1483,    -1,    -1,  1486,  1487,    -1,    -1,
      -1,    -1,  1492,    -1,    -1,    -1,  1496,    -1,  1498,    -1,
    1500,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1239,  1240,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1383,    -1,    -1,    -1,   332,
      -1,    -1,    -1,    -1,    -1,    -1,  1394,    -1,    -1,  1397,
      -1,  1399,  1400,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     353,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   701,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   709,    -1,    -1,   712,
     713,    -1,   715,    -1,    -1,    -1,  1311,    -1,    -1,    -1,
      -1,   724,  1317,    -1,   727,   728,   729,  1445,    -1,    -1,
      -1,    -1,    -1,    -1,  1827,  1828,    -1,    -1,    -1,    -1,
    1335,    -1,    -1,    -1,  1339,    -1,    -1,    -1,    -1,    -1,
    1843,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1357,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1374,
      -1,    -1,  1652,    -1,    -1,    -1,    -1,    -1,   451,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   807,    -1,    -1,    -1,    -1,    -1,
    1528,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   484,    -1,    -1,    -1,    -1,    -1,  1697,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1716,  1717,    -1,    -1,
      -1,    -1,    -1,  1946,  1449,  1450,    -1,    -1,    -1,    -1,
      -1,  1954,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1746,    -1,    -1,    -1,
    1475,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1615,    -1,  1494,
      -1,    -1,  1497,    -1,    -1,   568,   569,    -1,    -1,    -1,
      -1,    -1,   575,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1643,    -1,    -1,    -1,    -1,
    2023,  2024,    -1,    -1,  2027,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   950,   951,    -1,
      -1,    -1,    -1,    -1,  1549,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1558,  1559,    -1,    -1,    -1,    -1,    -1,
     633,    -1,    -1,    -1,  1569,    -1,    -1,  2070,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1859,
     653,    -1,    -1,  1588,    -1,  1590,  1866,    -1,  1868,    -1,
      -1,  1871,  1872,    -1,  1874,    -1,    -1,    -1,    -1,  1879,
      -1,    -1,    -1,   676,    -1,    -1,    -1,    -1,    -1,  1022,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2128,  2129,    -1,   701,  1757,
      -1,    -1,    -1,    -1,    -1,    -1,   709,    -1,    -1,    -1,
     713,    -1,   715,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2168,    -1,  1672,    -1,    -1,
      -1,    -1,  1677,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1808,  1809,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1979,
      -1,    -1,    -1,    -1,  1984,  1985,  1834,  1835,    -1,    -1,
     783,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1847,
      -1,    -1,    -1,    -1,  2004,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1737,    -1,   807,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   828,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2046,    -1,  2048,    -1,
      -1,    -1,  2052,  2053,   847,    -1,   849,  2057,  2058,    -1,
     853,   854,   855,    -1,    -1,    -1,  1791,    -1,  1201,  1794,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1210,    -1,    -1,
     873,     5,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,  1820,    -1,    -1,    -1,    -1,
    1948,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1245,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2121,  2122,  2123,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
     943,    -1,    -1,    -1,  2154,  2155,  2156,    -1,   951,    -1,
      -1,    -1,    -1,  1296,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2023,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,  1344,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1023,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2001,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1103,  1104,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,  1457,  1458,  1459,  1460,  1461,  1462,
    1463,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,    -1,   101,  1159,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,  1575,    -1,   180,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,
      -1,  1284,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1296,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1307,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1672,
      -1,    -1,    71,    -1,    73,    74,  1339,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,    -1,   101,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,     1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1408,    -1,    -1,  1411,  1412,
      -1,    18,    -1,    -1,    -1,   154,   155,    -1,  1761,   158,
     159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   180,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
    1793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,    -1,   101,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1549,    -1,   155,    -1,
      -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    71,    72,    73,    74,    -1,    76,    -1,  1672,
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
      -1,    -1,    -1,    -1,    -1,   180,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,   157,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
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
     171,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,    -1,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
     163,    -1,    -1,   166,   167,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
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
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
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
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    78,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
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
      62,    63,    64,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,   108,   109,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,   108,
     109,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,   108,   109,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,   108,
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
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    13,    14,
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
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,   136,    22,
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
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    20,   136,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,    72,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    49,    55,    -1,
      52,    -1,    54,    -1,    56,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    49,   136,
      -1,    52,   134,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,   151,
      -1,    -1,    73,   155,   156,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    49,    -1,    -1,
      52,    -1,    54,   134,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
     151,    73,    -1,    -1,   155,   156,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     126,   127,    52,    -1,    54,    -1,    56,    57,   134,    -1,
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
      -1,    -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,
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
       0,   182,   407,   408,     3,     4,     5,     6,     7,     8,
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
     339,   341,   347,   348,   349,   350,   361,   365,   399,   402,
     412,   418,   420,   426,   430,   435,   436,   437,   438,   439,
     440,   441,   442,   468,   486,   487,   488,   489,     0,   182,
     106,   186,   202,   298,   300,   311,   314,   317,   327,   331,
     336,   120,   155,    58,    61,    62,    64,   155,   155,   424,
     425,   426,   323,   324,   108,   109,   186,   379,   400,   401,
     379,   155,   412,   155,   155,     4,   106,   108,   109,   315,
     320,   321,   155,   155,   202,   425,   430,   436,   437,   438,
     440,   441,   442,   108,   338,   160,   182,   159,   301,   311,
     314,   435,   439,   485,   486,   489,   490,   180,   183,   152,
     163,   179,   223,   382,    89,   161,   419,   379,   161,   161,
     161,   180,   108,   109,   155,   202,   306,   307,   430,   431,
     432,   433,   434,   435,   439,   443,   444,   445,   446,   447,
     448,   449,   450,   451,   457,     3,    47,    48,    50,    55,
     329,     3,   159,   202,   300,   315,   319,   321,   332,   337,
     415,   435,   439,   489,    69,   298,   300,   314,   327,   331,
     336,   416,   435,   439,    65,   320,   320,   315,   321,   309,
     320,   321,   329,   348,   315,   320,   315,   158,   424,   161,
     183,   155,   163,   231,   424,   424,     3,   289,   290,   305,
     308,   314,   318,   319,   159,   311,   314,   487,   379,   379,
     412,   179,   314,   155,   202,   421,   430,   431,   435,   444,
     448,   159,   202,   489,   413,   414,    57,    65,    66,    67,
      68,   159,   177,   379,   388,   390,   394,   396,   397,   337,
      57,   157,   159,   202,   310,   314,   318,   326,   327,   333,
     334,   335,   336,   340,   347,   348,   365,   375,   377,   468,
     481,   482,   483,   484,   489,   490,   108,   109,   163,   170,
     186,   337,   457,   426,   155,   395,   396,   155,    13,    88,
     106,   108,   109,   155,   185,   427,   428,   429,   120,   188,
     189,    49,    52,    54,    56,    73,   103,   104,   106,   107,
     118,   119,   122,   123,   124,   126,   127,   155,   159,   165,
     168,   169,   170,   171,   184,   185,   188,   190,   193,   201,
     202,   203,   204,   207,   208,   209,   210,   211,   212,   213,
     214,   215,   216,   217,   218,   219,   225,   337,   157,   159,
     201,   202,   218,   220,   311,   337,   380,   381,   398,   485,
     490,   427,   314,   436,   437,   438,   440,   441,   442,   157,
     157,   157,   157,   157,   157,   157,   108,   159,   186,   311,
     468,   487,   159,   166,   202,   220,   300,   301,   310,   312,
     314,   327,   334,   336,   372,   373,   374,   376,   377,   481,
     489,   160,   155,   435,   439,   489,   155,   161,   106,   158,
     159,   163,   185,   187,   220,   383,   384,   385,   386,   387,
      22,   383,   155,   379,   231,   155,   186,   421,   186,   425,
     430,   432,   433,   434,   443,   445,   446,   447,   449,   450,
     451,   314,   431,   444,   448,   161,   101,   423,   159,   424,
     465,   468,   423,   424,   424,   419,   289,   155,   424,   465,
     423,   424,   424,   419,   424,   424,   314,   421,   155,   155,
     313,   314,   311,   314,   160,   182,   311,   485,   490,   339,
     163,   419,   289,   379,   379,   382,   300,   319,   417,   435,
     439,   163,   419,   289,   400,   314,   327,   314,   314,   108,
     338,   108,   109,   186,   337,   342,   400,   137,   186,   314,
     369,   370,   374,   375,   378,   154,   182,   231,   305,   180,
     435,   448,   314,   182,   423,   155,   423,   183,   220,   425,
     430,   314,   155,   182,   379,   410,   163,   155,   379,   163,
     379,   137,   166,   167,   393,   157,   161,   379,   397,   157,
     424,   424,   160,   182,   312,   314,   327,   334,   336,   480,
     481,   489,   490,   155,   159,   167,   179,   202,   468,   470,
     471,   472,   473,   474,   475,   492,   202,   340,   489,   314,
     334,   320,   315,   424,   157,   312,   314,   482,   312,   468,
     482,    10,   165,   170,   364,   366,   367,   163,   362,   364,
     388,   179,   388,   427,   157,   161,   155,   157,   120,   155,
     201,   155,   155,   155,   204,   155,   201,   155,   106,   108,
     109,   315,   320,   321,   155,   201,   201,    19,    21,    85,
     159,   168,   169,   205,   206,   220,   227,   231,   350,   380,
     489,   161,   182,   155,   190,   159,   164,   159,   164,   123,
     125,   126,   127,   155,   158,   159,   163,   164,   204,   204,
     172,   166,   173,   174,   168,   169,   128,   129,   130,   131,
     175,   176,   132,   133,   167,   165,   177,   134,   135,   178,
     157,   161,   158,   182,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   179,   222,   223,   224,   155,
     202,   461,   462,   463,   464,   465,   157,   161,   157,   157,
     157,   157,   157,   157,   157,   155,   424,   465,   468,   155,
     465,   468,   155,   182,   155,   311,   487,   160,   182,   183,
     159,   183,   155,   167,   202,   430,   452,   453,   454,   455,
     456,   457,   458,   459,   460,   137,   489,   161,   183,   161,
     183,   379,   379,   155,   182,   182,   182,   159,   187,   182,
     384,   162,   161,   491,   383,   158,   159,   162,   387,   156,
     220,   226,   155,   182,   179,   430,   432,   433,   434,   443,
     445,   446,   447,   449,   450,   451,   157,   157,   157,   157,
     157,   157,   157,   157,   157,   157,   431,   444,   448,   424,
     155,   179,   160,   182,   382,   231,   419,   369,   382,   231,
     421,   227,   381,   227,   381,   421,   108,   159,   410,   231,
     419,   423,   163,   163,   419,   289,   410,   231,   419,   344,
     345,   343,   163,   157,   161,   157,   161,    70,   291,   292,
     180,   166,   220,   182,   430,   373,   412,   410,   379,   160,
     182,   155,   392,   390,   391,    78,   325,   186,   163,   170,
     186,   457,   312,   468,   482,   314,   318,   489,   369,   471,
     472,   473,   160,   182,    18,   220,   314,   470,   492,   424,
     424,   468,   312,   480,   490,   314,   186,   424,   312,   482,
     337,   161,   491,   379,   366,   364,   163,   157,   381,   157,
     157,   428,   156,   194,   195,   196,   220,   180,   380,   490,
     190,   159,   380,   381,   380,   490,   220,   380,   157,   380,
     380,   380,   160,   182,   157,   168,   169,   206,    18,   316,
     157,   161,   157,   166,   167,   157,   226,   220,   163,   220,
     186,   220,   186,   118,   159,   186,   194,   108,   109,   118,
     159,   186,   350,   220,   194,   186,   204,   207,   207,   207,
     208,   208,   209,   209,   210,   210,   210,   210,   211,   211,
     212,   213,   214,   215,   216,   162,   227,   180,   188,   159,
     186,   220,   163,   220,   369,   462,   463,   464,   314,   461,
     424,   424,   220,   381,   155,   424,   465,   468,   155,   465,
     468,   369,   369,   182,   182,   160,   160,   155,   430,   453,
     454,   455,   458,    18,   314,   452,   456,   155,   424,   474,
     492,   424,   424,   492,   155,   424,   474,   424,   424,   183,
     219,   379,   373,   376,   160,   376,   377,   160,   492,   492,
     137,   371,   372,   373,   371,   371,   379,   182,   218,   219,
     220,   422,   491,   383,   385,   154,   182,   157,   161,   182,
     371,   220,   157,   157,   157,   157,   157,   157,   157,   157,
     157,   155,   424,   465,   468,   155,   424,   465,   468,   155,
     424,   465,   468,   421,   188,    22,   468,   220,   321,   337,
     466,   231,   157,   157,   157,   157,   157,   408,   409,   231,
     154,   182,   410,   231,   419,   409,   231,   163,   163,   163,
     351,   137,   374,   375,   186,   293,   379,    18,    71,    73,
      74,    76,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    93,    94,    95,    96,    97,    98,
      99,   101,   108,   109,   121,   155,   159,   227,   228,   229,
     230,   231,   232,   233,   235,   236,   245,   252,   253,   254,
     255,   256,   257,   262,   263,   266,   267,   268,   269,   270,
     271,   272,   278,   279,   280,   294,   314,   318,   379,   420,
      70,   183,   183,   371,   161,   411,   409,   157,   298,   300,
     311,   403,   404,   405,   406,   398,   179,   389,   389,   366,
     163,   424,   424,   312,   482,   159,   166,   202,   220,   337,
     220,   314,   157,   157,   157,   157,     5,   314,   424,   470,
     163,   170,   186,   457,    10,   367,   154,   179,   368,   491,
     163,   366,   163,   157,   157,   161,   157,   157,   161,   182,
     161,   157,   157,   157,   161,   157,   204,   157,   157,   157,
     204,    18,   316,   220,   157,   157,   156,   163,   204,   160,
     183,   194,   160,   160,   118,   122,   124,   187,   197,   198,
     199,   157,   197,   160,   161,   154,   218,   162,   157,   197,
     183,   384,   157,   157,   157,   157,   461,   369,   369,   157,
     157,   371,   371,   458,   157,   157,   157,   157,   155,   430,
     457,   452,   456,   369,   369,   160,   183,   492,   161,   183,
     157,   161,   161,   183,   183,   382,   197,   137,   171,   183,
     183,   154,   383,   220,   424,   156,   220,   371,   183,   155,
     424,   465,   468,   155,   424,   465,   468,   155,   424,   465,
     468,   369,   369,   369,   423,   157,   149,   171,   183,   467,
     161,   183,   411,   403,   409,   231,   411,   351,   351,   351,
       3,     5,    10,    73,   154,   295,   302,   303,   311,   314,
     352,   357,   485,   161,   180,   155,    61,    62,   180,   231,
     294,   420,   155,   155,    18,   229,   155,   155,   180,   379,
     180,   379,   166,   379,   163,   228,   155,   155,   155,   229,
     155,   231,   220,   221,   221,    14,   281,   257,   268,   180,
     183,   233,    78,   180,   379,    91,    92,   261,   265,   112,
     135,   260,   111,   134,   264,   260,   378,   314,   162,   293,
     160,   160,   183,   411,   379,   421,   183,   180,   183,   180,
     183,   157,   381,   395,   395,   491,   366,   364,   364,   182,
     183,   183,   183,   220,   155,   424,   474,   468,   313,     5,
     166,   183,   220,   366,   163,   424,   424,   337,   379,   163,
     219,   154,   366,   491,   154,   182,   196,   310,   186,    78,
     191,   192,   380,   204,   204,   204,   204,   204,   163,   384,
     161,   154,   200,   159,   198,   200,   200,   160,   161,   125,
     158,   160,   226,   218,   180,   160,   491,   155,   424,   465,
     468,   157,   157,   183,   183,   157,   155,   424,   465,   468,
     155,   424,   474,   430,   424,   424,   157,   157,   160,   376,
     160,   137,   373,   137,   157,   157,   183,   219,   219,   160,
     160,   183,   183,   157,   369,   369,   369,   157,   157,   157,
     382,   424,   161,   220,   220,   321,   337,   160,   154,   183,
     411,   154,   154,   154,   154,   311,   311,   350,   358,   485,
     311,   357,   155,   346,   180,   180,   155,   162,   202,   353,
     354,   360,   430,   431,   444,   448,   161,   180,   379,   379,
     194,   180,   231,   180,   231,   227,   237,   294,   296,   299,
     305,   314,   318,   227,    80,   157,   237,   149,   150,   151,
     156,   157,   180,   227,   246,   247,   249,   294,   180,   180,
     227,   180,   384,   180,   227,   226,   227,   246,   113,   114,
     115,   116,   117,   273,   275,   276,   180,   100,   180,    84,
     155,   157,   154,   180,   180,   155,   155,   229,   229,   257,
     155,   267,   257,   267,   231,   424,   180,   157,   154,   393,
     154,   182,   161,   161,   154,   491,   163,   163,   160,   160,
     160,   183,   369,   220,   220,   183,   160,   183,   491,   366,
     363,   364,   368,   368,   384,   491,   154,   403,   469,   470,
     157,   162,   157,   161,   162,   384,   491,   226,   123,   197,
     198,   159,   198,   159,   198,   160,   154,   369,   157,   157,
     369,   369,   160,   183,   157,   424,   157,   157,   157,   227,
     467,   154,   154,   346,   346,   346,   353,   155,   202,   355,
     356,   465,   476,   477,   478,   479,   180,   161,   180,   353,
     180,   398,   425,   430,   220,   314,   154,   161,   180,   359,
     360,   359,   359,   379,   157,   157,   227,   314,   157,   155,
     229,   157,   149,   150,   151,   171,   180,   250,   251,   229,
     228,   180,   251,   157,   162,   227,   156,   227,   228,   249,
     180,   491,   157,   157,   157,   157,   231,   275,   276,   155,
     220,   155,   188,   204,   258,   227,    75,   110,   259,   261,
      75,     1,   229,   424,   389,   404,   182,   182,   154,   366,
     366,   160,   157,   183,   183,   160,   160,   154,   491,   364,
     163,   491,   154,   183,   157,   220,   192,   220,   491,   154,
     160,   160,   197,   197,   157,   424,   424,   157,   157,   160,
     160,   220,   180,   477,   478,   479,   314,   476,   161,   180,
     424,   424,   180,   157,   430,   424,   229,   229,    77,    78,
     163,   240,   241,   242,   157,   227,    75,   229,   227,   156,
     227,    75,   180,    57,   108,   156,   227,   228,   248,   249,
     156,   227,   229,   247,   251,   251,   180,   227,   154,   163,
     242,   229,   229,   155,   182,   180,   188,   157,   162,   157,
     161,   162,   157,   229,   155,   229,   229,   229,   395,   379,
     421,   491,   491,   160,   160,   154,   163,   366,   154,   154,
     154,   160,   160,   157,   157,   157,   476,   424,   354,    75,
       1,   219,   238,   239,   422,     1,   162,     1,   182,   229,
     240,    75,   180,   157,   229,    75,   180,   171,   171,   229,
     228,   108,   251,   251,   180,   227,   248,   171,   171,    75,
     156,   227,   156,   227,   228,   180,     1,   182,   182,   277,
     312,   314,   485,   162,   180,   159,   188,   282,   283,   284,
     204,   194,   227,   260,   154,   154,   366,   491,   155,   424,
     465,   468,   356,   229,   137,     1,   161,   162,   154,   287,
     288,   294,   229,    75,   180,   229,   227,   156,   156,   227,
     156,   227,   156,   227,   228,   156,   227,   156,   227,   229,
     171,   171,   171,   171,   154,   287,   277,   183,   155,   202,
     421,   476,   186,   162,   106,   155,   157,   162,   161,   157,
     157,    75,   256,   491,   154,   369,   219,   238,   241,   243,
     244,   294,   229,   171,   171,   171,   171,   156,   156,   227,
     156,   227,   156,   227,   243,   183,   180,   274,   314,   282,
     160,   219,   180,   282,   284,   229,    75,   154,   157,   229,
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
     352,   352,   353,   353,   353,   354,   354,   354,   354,   355,
     355,   355,   356,   357,   357,   358,   358,   359,   359,   360,
     361,   361,   362,   361,   361,   361,   361,   361,   361,   363,
     361,   361,   361,   361,   361,   364,   364,   365,   365,   366,
     366,   366,   366,   367,   367,   368,   368,   368,   369,   369,
     369,   369,   370,   370,   370,   370,   371,   371,   371,   371,
     371,   371,   371,   372,   372,   372,   372,   373,   373,   374,
     374,   375,   375,   376,   376,   376,   376,   376,   377,   377,
     377,   377,   377,   378,   378,   379,   379,   379,   380,   380,
     380,   381,   381,   382,   382,   382,   382,   383,   383,   384,
     384,   384,   384,   384,   385,   385,   386,   386,   387,   387,
     387,   387,   387,   388,   388,   389,   389,   391,   390,   392,
     390,   390,   390,   390,   393,   393,   393,   393,   394,   394,
     394,   394,   395,   395,   396,   396,   397,   397,   398,   398,
     398,   398,   399,   399,   399,   400,   400,   401,   401,   402,
     402,   402,   402,   403,   403,   404,   404,   405,   405,   405,
     406,   406,   407,   407,   408,   408,   409,   409,   410,   411,
     412,   412,   412,   412,   412,   412,   412,   412,   412,   412,
     412,   413,   412,   414,   412,   415,   412,   416,   412,   417,
     412,   418,   418,   418,   419,   419,   420,   420,   420,   420,
     420,   420,   420,   420,   420,   420,   421,   421,   421,   421,
     422,   423,   423,   424,   424,   425,   425,   426,   426,   426,
     427,   427,   428,   428,   428,   429,   429,   429,   429,   429,
     429,   430,   430,   431,   431,   431,   431,   432,   432,   432,
     432,   433,   433,   433,   433,   433,   433,   433,   434,   434,
     434,   434,   435,   435,   435,   436,   436,   436,   436,   436,
     437,   437,   437,   437,   438,   438,   438,   438,   438,   438,
     439,   439,   439,   440,   440,   440,   440,   440,   441,   441,
     441,   441,   442,   442,   442,   442,   442,   442,   443,   443,
     444,   444,   444,   444,   445,   445,   445,   445,   446,   446,
     446,   446,   446,   446,   446,   447,   447,   447,   447,   448,
     448,   448,   449,   449,   449,   449,   449,   450,   450,   450,
     450,   451,   451,   451,   451,   451,   451,   452,   452,   452,
     452,   452,   453,   453,   453,   454,   454,   454,   454,   455,
     455,   455,   456,   456,   456,   456,   456,   457,   457,   458,
     458,   458,   459,   459,   460,   460,   461,   461,   461,   462,
     462,   462,   462,   462,   463,   463,   463,   463,   464,   464,
     464,   465,   465,   465,   465,   465,   466,   466,   466,   466,
     466,   466,   467,   467,   468,   468,   468,   468,   469,   469,
     470,   470,   470,   470,   471,   471,   471,   471,   471,   472,
     472,   472,   472,   473,   473,   473,   474,   474,   474,   475,
     475,   475,   475,   475,   475,   476,   476,   476,   477,   477,
     477,   477,   477,   478,   478,   478,   478,   479,   479,   480,
     480,   480,   481,   481,   482,   482,   482,   482,   482,   482,
     483,   483,   483,   483,   483,   483,   483,   483,   483,   483,
     484,   484,   484,   484,   485,   485,   485,   486,   486,   487,
     487,   487,   487,   487,   487,   488,   488,   488,   488,   488,
     488,   489,   489,   489,   490,   490,   490,   491,   491,   492,
     492
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
       6,     7,     0,     9,     8,     9,    10,     8,     9,     0,
      13,    11,    12,    11,     1,     0,     1,     3,     3,     3,
       2,     5,     5,     1,     1,     0,     2,     5,     0,     1,
       1,     3,     1,     1,     3,     3,     0,     1,     1,     1,
       3,     3,     3,     1,     3,     3,     5,     1,     3,     3,
       3,     2,     3,     1,     3,     3,     4,     1,     1,     1,
       1,     2,     1,     1,     3,     1,     1,     1,     1,     1,
       2,     1,     1,     0,     2,     2,     4,     1,     4,     0,
       1,     2,     3,     4,     2,     2,     1,     2,     2,     5,
       5,     7,     6,     1,     3,     0,     2,     0,     5,     0,
       5,     3,     1,     8,     0,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     5,     6,     1,     1,
       3,     3,     2,     3,     3,     2,     4,     1,     4,     7,
       5,    10,     8,     1,     4,     2,     2,     1,     1,     5,
       2,     5,     0,     1,     3,     4,     0,     1,     0,     0,
       1,     1,     2,     2,     2,     2,     2,     2,     1,     2,
       5,     0,     6,     0,     8,     0,     7,     0,     7,     0,
       8,     1,     2,     3,     0,     5,     3,     4,     4,     4,
       4,     5,     5,     5,     5,     6,     1,     1,     1,     1,
       3,     0,     5,     0,     1,     1,     2,     6,     4,     4,
       1,     3,     0,     1,     4,     1,     1,     1,     1,     1,
       1,     1,     3,     2,     1,     2,     2,     2,     3,     4,
       5,     2,     4,     5,     4,     5,     3,     4,     6,     7,
       3,     4,     2,     1,     2,     4,     6,     7,     3,     4,
       2,     3,     4,     5,     4,     5,     4,     5,     3,     4,
       1,     1,     1,     4,     6,     7,     3,     4,     2,     3,
       3,     4,     4,     5,     4,     5,     3,     4,     1,     3,
       2,     1,     2,     2,     2,     3,     4,     5,     2,     4,
       5,     4,     5,     3,     4,     6,     7,     3,     4,     2,
       1,     2,     4,     6,     7,     3,     4,     2,     3,     4,
       5,     4,     5,     4,     5,     3,     4,     2,     4,     1,
       2,     2,     2,     3,     4,     2,     4,     4,     3,     4,
       6,     3,     2,     4,     1,     2,     2,     1,     1,     2,
       3,     4,     2,     4,     4,     6,     1,     2,     2,     1,
       2,     2,     3,     4,     1,     4,     4,     3,     3,     6,
       3,     2,     3,     7,     5,     1,     1,     1,     3,     3,
       3,     5,     1,     1,     5,     5,     6,     6,     0,     1,
       1,     3,     2,     2,     1,     2,     2,     3,     4,     1,
       4,     4,     3,     3,     6,     3,     1,     2,     1,     2,
       6,     5,     6,     7,     7,     1,     2,     2,     1,     2,
       2,     3,     4,     1,     4,     4,     3,     6,     3,     1,
       1,     2,     1,     1,     2,     3,     2,     3,     2,     3,
       3,     2,     4,     3,     2,     3,     2,     4,     3,     2,
       6,     6,     6,     7,     1,     2,     1,     1,     1,     2,
       3,     2,     3,     2,     3,     3,     4,     2,     3,     4,
       2,     5,     6,     7,     5,     6,     6,     0,     1,     0,
       2
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
#line 8467 "Parser/parser.cc"
    break;

  case 3:
#line 645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8473 "Parser/parser.cc"
    break;

  case 4:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8479 "Parser/parser.cc"
    break;

  case 5:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8485 "Parser/parser.cc"
    break;

  case 6:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8491 "Parser/parser.cc"
    break;

  case 7:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8497 "Parser/parser.cc"
    break;

  case 8:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8503 "Parser/parser.cc"
    break;

  case 20:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8509 "Parser/parser.cc"
    break;

  case 21:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8515 "Parser/parser.cc"
    break;

  case 22:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8521 "Parser/parser.cc"
    break;

  case 23:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8531 "Parser/parser.cc"
    break;

  case 24:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8537 "Parser/parser.cc"
    break;

  case 25:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8543 "Parser/parser.cc"
    break;

  case 26:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8549 "Parser/parser.cc"
    break;

  case 28:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8555 "Parser/parser.cc"
    break;

  case 29:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8561 "Parser/parser.cc"
    break;

  case 30:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8567 "Parser/parser.cc"
    break;

  case 31:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8573 "Parser/parser.cc"
    break;

  case 32:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8583 "Parser/parser.cc"
    break;

  case 33:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8589 "Parser/parser.cc"
    break;

  case 34:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8595 "Parser/parser.cc"
    break;

  case 35:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8601 "Parser/parser.cc"
    break;

  case 36:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8607 "Parser/parser.cc"
    break;

  case 37:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8613 "Parser/parser.cc"
    break;

  case 38:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8619 "Parser/parser.cc"
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
#line 8631 "Parser/parser.cc"
    break;

  case 41:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8640 "Parser/parser.cc"
    break;

  case 42:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8646 "Parser/parser.cc"
    break;

  case 44:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8652 "Parser/parser.cc"
    break;

  case 45:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8658 "Parser/parser.cc"
    break;

  case 46:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8664 "Parser/parser.cc"
    break;

  case 47:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8670 "Parser/parser.cc"
    break;

  case 48:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8680 "Parser/parser.cc"
    break;

  case 49:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8686 "Parser/parser.cc"
    break;

  case 50:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8693 "Parser/parser.cc"
    break;

  case 51:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8699 "Parser/parser.cc"
    break;

  case 52:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8705 "Parser/parser.cc"
    break;

  case 53:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8711 "Parser/parser.cc"
    break;

  case 54:
#line 813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8717 "Parser/parser.cc"
    break;

  case 55:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8723 "Parser/parser.cc"
    break;

  case 56:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8729 "Parser/parser.cc"
    break;

  case 57:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8735 "Parser/parser.cc"
    break;

  case 58:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8741 "Parser/parser.cc"
    break;

  case 59:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8747 "Parser/parser.cc"
    break;

  case 60:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8753 "Parser/parser.cc"
    break;

  case 61:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8759 "Parser/parser.cc"
    break;

  case 62:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8765 "Parser/parser.cc"
    break;

  case 63:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8771 "Parser/parser.cc"
    break;

  case 64:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8777 "Parser/parser.cc"
    break;

  case 65:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8783 "Parser/parser.cc"
    break;

  case 66:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8789 "Parser/parser.cc"
    break;

  case 67:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8795 "Parser/parser.cc"
    break;

  case 68:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8805 "Parser/parser.cc"
    break;

  case 69:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8811 "Parser/parser.cc"
    break;

  case 72:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8817 "Parser/parser.cc"
    break;

  case 73:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8823 "Parser/parser.cc"
    break;

  case 76:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8829 "Parser/parser.cc"
    break;

  case 78:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8835 "Parser/parser.cc"
    break;

  case 79:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8841 "Parser/parser.cc"
    break;

  case 80:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8847 "Parser/parser.cc"
    break;

  case 81:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8853 "Parser/parser.cc"
    break;

  case 82:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8859 "Parser/parser.cc"
    break;

  case 83:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8865 "Parser/parser.cc"
    break;

  case 84:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8871 "Parser/parser.cc"
    break;

  case 85:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8877 "Parser/parser.cc"
    break;

  case 86:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8885 "Parser/parser.cc"
    break;

  case 87:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8891 "Parser/parser.cc"
    break;

  case 88:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8900 "Parser/parser.cc"
    break;

  case 91:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8906 "Parser/parser.cc"
    break;

  case 92:
#line 918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8912 "Parser/parser.cc"
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
#line 8932 "Parser/parser.cc"
    break;

  case 94:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8938 "Parser/parser.cc"
    break;

  case 95:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8944 "Parser/parser.cc"
    break;

  case 96:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8950 "Parser/parser.cc"
    break;

  case 97:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8956 "Parser/parser.cc"
    break;

  case 98:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8962 "Parser/parser.cc"
    break;

  case 99:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8968 "Parser/parser.cc"
    break;

  case 100:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8974 "Parser/parser.cc"
    break;

  case 101:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8980 "Parser/parser.cc"
    break;

  case 102:
#line 958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8986 "Parser/parser.cc"
    break;

  case 103:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8992 "Parser/parser.cc"
    break;

  case 104:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9001 "Parser/parser.cc"
    break;

  case 105:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9007 "Parser/parser.cc"
    break;

  case 106:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9013 "Parser/parser.cc"
    break;

  case 107:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9019 "Parser/parser.cc"
    break;

  case 108:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9025 "Parser/parser.cc"
    break;

  case 109:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9031 "Parser/parser.cc"
    break;

  case 110:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9037 "Parser/parser.cc"
    break;

  case 111:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9043 "Parser/parser.cc"
    break;

  case 113:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9049 "Parser/parser.cc"
    break;

  case 114:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9055 "Parser/parser.cc"
    break;

  case 115:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9061 "Parser/parser.cc"
    break;

  case 116:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9067 "Parser/parser.cc"
    break;

  case 117:
#line 994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9073 "Parser/parser.cc"
    break;

  case 118:
#line 996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9079 "Parser/parser.cc"
    break;

  case 119:
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9085 "Parser/parser.cc"
    break;

  case 120:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9091 "Parser/parser.cc"
    break;

  case 128:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9097 "Parser/parser.cc"
    break;

  case 130:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9103 "Parser/parser.cc"
    break;

  case 131:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9109 "Parser/parser.cc"
    break;

  case 132:
#line 1030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9115 "Parser/parser.cc"
    break;

  case 134:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9121 "Parser/parser.cc"
    break;

  case 135:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9127 "Parser/parser.cc"
    break;

  case 137:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9133 "Parser/parser.cc"
    break;

  case 138:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9139 "Parser/parser.cc"
    break;

  case 140:
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9145 "Parser/parser.cc"
    break;

  case 141:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9151 "Parser/parser.cc"
    break;

  case 142:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9157 "Parser/parser.cc"
    break;

  case 143:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9163 "Parser/parser.cc"
    break;

  case 145:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9169 "Parser/parser.cc"
    break;

  case 146:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9175 "Parser/parser.cc"
    break;

  case 148:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9181 "Parser/parser.cc"
    break;

  case 150:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9187 "Parser/parser.cc"
    break;

  case 152:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9193 "Parser/parser.cc"
    break;

  case 154:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9199 "Parser/parser.cc"
    break;

  case 156:
#line 1096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9205 "Parser/parser.cc"
    break;

  case 158:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9211 "Parser/parser.cc"
    break;

  case 159:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9217 "Parser/parser.cc"
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
#line 9229 "Parser/parser.cc"
    break;

  case 163:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9235 "Parser/parser.cc"
    break;

  case 164:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9241 "Parser/parser.cc"
    break;

  case 168:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9247 "Parser/parser.cc"
    break;

  case 169:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9253 "Parser/parser.cc"
    break;

  case 170:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9259 "Parser/parser.cc"
    break;

  case 171:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9265 "Parser/parser.cc"
    break;

  case 172:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9271 "Parser/parser.cc"
    break;

  case 173:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9277 "Parser/parser.cc"
    break;

  case 174:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9283 "Parser/parser.cc"
    break;

  case 175:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9289 "Parser/parser.cc"
    break;

  case 176:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9295 "Parser/parser.cc"
    break;

  case 177:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9301 "Parser/parser.cc"
    break;

  case 178:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9307 "Parser/parser.cc"
    break;

  case 179:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9313 "Parser/parser.cc"
    break;

  case 180:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9319 "Parser/parser.cc"
    break;

  case 181:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9325 "Parser/parser.cc"
    break;

  case 182:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9331 "Parser/parser.cc"
    break;

  case 184:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9337 "Parser/parser.cc"
    break;

  case 185:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9343 "Parser/parser.cc"
    break;

  case 186:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9349 "Parser/parser.cc"
    break;

  case 188:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9355 "Parser/parser.cc"
    break;

  case 189:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9361 "Parser/parser.cc"
    break;

  case 204:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9367 "Parser/parser.cc"
    break;

  case 206:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9373 "Parser/parser.cc"
    break;

  case 207:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9379 "Parser/parser.cc"
    break;

  case 208:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9390 "Parser/parser.cc"
    break;

  case 209:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9396 "Parser/parser.cc"
    break;

  case 210:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9402 "Parser/parser.cc"
    break;

  case 212:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9408 "Parser/parser.cc"
    break;

  case 213:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9414 "Parser/parser.cc"
    break;

  case 214:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9420 "Parser/parser.cc"
    break;

  case 215:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9426 "Parser/parser.cc"
    break;

  case 216:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9432 "Parser/parser.cc"
    break;

  case 219:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9438 "Parser/parser.cc"
    break;

  case 220:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9445 "Parser/parser.cc"
    break;

  case 221:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9451 "Parser/parser.cc"
    break;

  case 222:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9457 "Parser/parser.cc"
    break;

  case 223:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9463 "Parser/parser.cc"
    break;

  case 224:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9469 "Parser/parser.cc"
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
#line 9483 "Parser/parser.cc"
    break;

  case 226:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9489 "Parser/parser.cc"
    break;

  case 227:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9495 "Parser/parser.cc"
    break;

  case 228:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9504 "Parser/parser.cc"
    break;

  case 229:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9510 "Parser/parser.cc"
    break;

  case 230:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9516 "Parser/parser.cc"
    break;

  case 231:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9522 "Parser/parser.cc"
    break;

  case 232:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9528 "Parser/parser.cc"
    break;

  case 233:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9534 "Parser/parser.cc"
    break;

  case 234:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9540 "Parser/parser.cc"
    break;

  case 235:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9546 "Parser/parser.cc"
    break;

  case 237:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9552 "Parser/parser.cc"
    break;

  case 238:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9558 "Parser/parser.cc"
    break;

  case 239:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9564 "Parser/parser.cc"
    break;

  case 240:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9570 "Parser/parser.cc"
    break;

  case 241:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9576 "Parser/parser.cc"
    break;

  case 242:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9582 "Parser/parser.cc"
    break;

  case 243:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9588 "Parser/parser.cc"
    break;

  case 245:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9594 "Parser/parser.cc"
    break;

  case 246:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9600 "Parser/parser.cc"
    break;

  case 247:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9606 "Parser/parser.cc"
    break;

  case 249:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9612 "Parser/parser.cc"
    break;

  case 250:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9618 "Parser/parser.cc"
    break;

  case 251:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9624 "Parser/parser.cc"
    break;

  case 252:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9633 "Parser/parser.cc"
    break;

  case 253:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9639 "Parser/parser.cc"
    break;

  case 254:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9645 "Parser/parser.cc"
    break;

  case 255:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9651 "Parser/parser.cc"
    break;

  case 256:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9660 "Parser/parser.cc"
    break;

  case 257:
#line 1405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9666 "Parser/parser.cc"
    break;

  case 258:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9672 "Parser/parser.cc"
    break;

  case 259:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9678 "Parser/parser.cc"
    break;

  case 260:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9687 "Parser/parser.cc"
    break;

  case 261:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9693 "Parser/parser.cc"
    break;

  case 262:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9699 "Parser/parser.cc"
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
#line 9718 "Parser/parser.cc"
    break;

  case 265:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9724 "Parser/parser.cc"
    break;

  case 266:
#line 1448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9733 "Parser/parser.cc"
    break;

  case 267:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9739 "Parser/parser.cc"
    break;

  case 268:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9745 "Parser/parser.cc"
    break;

  case 269:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9751 "Parser/parser.cc"
    break;

  case 270:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9757 "Parser/parser.cc"
    break;

  case 271:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9763 "Parser/parser.cc"
    break;

  case 272:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9769 "Parser/parser.cc"
    break;

  case 273:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9778 "Parser/parser.cc"
    break;

  case 274:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9787 "Parser/parser.cc"
    break;

  case 275:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9793 "Parser/parser.cc"
    break;

  case 276:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9802 "Parser/parser.cc"
    break;

  case 277:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9811 "Parser/parser.cc"
    break;

  case 278:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9817 "Parser/parser.cc"
    break;

  case 279:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9823 "Parser/parser.cc"
    break;

  case 280:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9829 "Parser/parser.cc"
    break;

  case 281:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9835 "Parser/parser.cc"
    break;

  case 282:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9841 "Parser/parser.cc"
    break;

  case 283:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9847 "Parser/parser.cc"
    break;

  case 284:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9853 "Parser/parser.cc"
    break;

  case 285:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9859 "Parser/parser.cc"
    break;

  case 286:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9868 "Parser/parser.cc"
    break;

  case 287:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9878 "Parser/parser.cc"
    break;

  case 288:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9884 "Parser/parser.cc"
    break;

  case 289:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9890 "Parser/parser.cc"
    break;

  case 290:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9899 "Parser/parser.cc"
    break;

  case 291:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9909 "Parser/parser.cc"
    break;

  case 292:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9915 "Parser/parser.cc"
    break;

  case 293:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9924 "Parser/parser.cc"
    break;

  case 294:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9934 "Parser/parser.cc"
    break;

  case 295:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9940 "Parser/parser.cc"
    break;

  case 296:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9946 "Parser/parser.cc"
    break;

  case 297:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9952 "Parser/parser.cc"
    break;

  case 298:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9958 "Parser/parser.cc"
    break;

  case 299:
#line 1558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9967 "Parser/parser.cc"
    break;

  case 300:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9977 "Parser/parser.cc"
    break;

  case 301:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9983 "Parser/parser.cc"
    break;

  case 302:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9992 "Parser/parser.cc"
    break;

  case 303:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10002 "Parser/parser.cc"
    break;

  case 304:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10008 "Parser/parser.cc"
    break;

  case 305:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10017 "Parser/parser.cc"
    break;

  case 306:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10027 "Parser/parser.cc"
    break;

  case 307:
#line 1596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10033 "Parser/parser.cc"
    break;

  case 308:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 10042 "Parser/parser.cc"
    break;

  case 309:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 10053 "Parser/parser.cc"
    break;

  case 312:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10059 "Parser/parser.cc"
    break;

  case 313:
#line 1621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10065 "Parser/parser.cc"
    break;

  case 314:
#line 1623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10071 "Parser/parser.cc"
    break;

  case 315:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10077 "Parser/parser.cc"
    break;

  case 316:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10083 "Parser/parser.cc"
    break;

  case 318:
#line 1636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10089 "Parser/parser.cc"
    break;

  case 319:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10095 "Parser/parser.cc"
    break;

  case 320:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10101 "Parser/parser.cc"
    break;

  case 321:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10107 "Parser/parser.cc"
    break;

  case 322:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10113 "Parser/parser.cc"
    break;

  case 323:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10119 "Parser/parser.cc"
    break;

  case 324:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10125 "Parser/parser.cc"
    break;

  case 325:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10131 "Parser/parser.cc"
    break;

  case 326:
#line 1661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10137 "Parser/parser.cc"
    break;

  case 327:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10143 "Parser/parser.cc"
    break;

  case 328:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10149 "Parser/parser.cc"
    break;

  case 329:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10155 "Parser/parser.cc"
    break;

  case 330:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10161 "Parser/parser.cc"
    break;

  case 331:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10167 "Parser/parser.cc"
    break;

  case 332:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10173 "Parser/parser.cc"
    break;

  case 333:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 334:
#line 1680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 335:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 336:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 337:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10203 "Parser/parser.cc"
    break;

  case 338:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10209 "Parser/parser.cc"
    break;

  case 339:
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10215 "Parser/parser.cc"
    break;

  case 342:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10221 "Parser/parser.cc"
    break;

  case 343:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10230 "Parser/parser.cc"
    break;

  case 344:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10236 "Parser/parser.cc"
    break;

  case 345:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10242 "Parser/parser.cc"
    break;

  case 348:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10248 "Parser/parser.cc"
    break;

  case 349:
#line 1729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10254 "Parser/parser.cc"
    break;

  case 352:
#line 1738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10260 "Parser/parser.cc"
    break;

  case 353:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10266 "Parser/parser.cc"
    break;

  case 354:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10272 "Parser/parser.cc"
    break;

  case 355:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10278 "Parser/parser.cc"
    break;

  case 356:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10284 "Parser/parser.cc"
    break;

  case 357:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10290 "Parser/parser.cc"
    break;

  case 358:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10296 "Parser/parser.cc"
    break;

  case 359:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10302 "Parser/parser.cc"
    break;

  case 360:
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10308 "Parser/parser.cc"
    break;

  case 363:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10314 "Parser/parser.cc"
    break;

  case 364:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10320 "Parser/parser.cc"
    break;

  case 365:
#line 1779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10326 "Parser/parser.cc"
    break;

  case 366:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10332 "Parser/parser.cc"
    break;

  case 367:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10338 "Parser/parser.cc"
    break;

  case 368:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10344 "Parser/parser.cc"
    break;

  case 369:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10350 "Parser/parser.cc"
    break;

  case 370:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10356 "Parser/parser.cc"
    break;

  case 371:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10362 "Parser/parser.cc"
    break;

  case 372:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10368 "Parser/parser.cc"
    break;

  case 373:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10374 "Parser/parser.cc"
    break;

  case 374:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10380 "Parser/parser.cc"
    break;

  case 375:
#line 1817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10386 "Parser/parser.cc"
    break;

  case 376:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10392 "Parser/parser.cc"
    break;

  case 377:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10398 "Parser/parser.cc"
    break;

  case 378:
#line 1826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10404 "Parser/parser.cc"
    break;

  case 379:
#line 1831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10410 "Parser/parser.cc"
    break;

  case 380:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10416 "Parser/parser.cc"
    break;

  case 381:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10422 "Parser/parser.cc"
    break;

  case 382:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10428 "Parser/parser.cc"
    break;

  case 383:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10434 "Parser/parser.cc"
    break;

  case 384:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10440 "Parser/parser.cc"
    break;

  case 385:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10446 "Parser/parser.cc"
    break;

  case 387:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10452 "Parser/parser.cc"
    break;

  case 388:
#line 1852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10458 "Parser/parser.cc"
    break;

  case 389:
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10464 "Parser/parser.cc"
    break;

  case 394:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10470 "Parser/parser.cc"
    break;

  case 395:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10476 "Parser/parser.cc"
    break;

  case 396:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10482 "Parser/parser.cc"
    break;

  case 397:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10488 "Parser/parser.cc"
    break;

  case 398:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10494 "Parser/parser.cc"
    break;

  case 399:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10500 "Parser/parser.cc"
    break;

  case 400:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10506 "Parser/parser.cc"
    break;

  case 401:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10512 "Parser/parser.cc"
    break;

  case 404:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10518 "Parser/parser.cc"
    break;

  case 405:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10524 "Parser/parser.cc"
    break;

  case 406:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10533 "Parser/parser.cc"
    break;

  case 407:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10539 "Parser/parser.cc"
    break;

  case 408:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10545 "Parser/parser.cc"
    break;

  case 409:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10551 "Parser/parser.cc"
    break;

  case 410:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10560 "Parser/parser.cc"
    break;

  case 411:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10569 "Parser/parser.cc"
    break;

  case 412:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10575 "Parser/parser.cc"
    break;

  case 415:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10581 "Parser/parser.cc"
    break;

  case 416:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10587 "Parser/parser.cc"
    break;

  case 418:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10593 "Parser/parser.cc"
    break;

  case 419:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10599 "Parser/parser.cc"
    break;

  case 429:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10605 "Parser/parser.cc"
    break;

  case 430:
#line 1983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10611 "Parser/parser.cc"
    break;

  case 434:
#line 2001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10617 "Parser/parser.cc"
    break;

  case 436:
#line 2007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10623 "Parser/parser.cc"
    break;

  case 437:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10629 "Parser/parser.cc"
    break;

  case 438:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10635 "Parser/parser.cc"
    break;

  case 439:
#line 2020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10641 "Parser/parser.cc"
    break;

  case 440:
#line 2022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10647 "Parser/parser.cc"
    break;

  case 441:
#line 2024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10653 "Parser/parser.cc"
    break;

  case 443:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10659 "Parser/parser.cc"
    break;

  case 444:
#line 2032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10665 "Parser/parser.cc"
    break;

  case 445:
#line 2034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10671 "Parser/parser.cc"
    break;

  case 446:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10682 "Parser/parser.cc"
    break;

  case 447:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10688 "Parser/parser.cc"
    break;

  case 448:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10694 "Parser/parser.cc"
    break;

  case 449:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 450:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10706 "Parser/parser.cc"
    break;

  case 451:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10712 "Parser/parser.cc"
    break;

  case 452:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10718 "Parser/parser.cc"
    break;

  case 453:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10727 "Parser/parser.cc"
    break;

  case 454:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10736 "Parser/parser.cc"
    break;

  case 455:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10745 "Parser/parser.cc"
    break;

  case 456:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10756 "Parser/parser.cc"
    break;

  case 457:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10765 "Parser/parser.cc"
    break;

  case 458:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10771 "Parser/parser.cc"
    break;

  case 459:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10777 "Parser/parser.cc"
    break;

  case 460:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10783 "Parser/parser.cc"
    break;

  case 461:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10791 "Parser/parser.cc"
    break;

  case 462:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10799 "Parser/parser.cc"
    break;

  case 463:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10805 "Parser/parser.cc"
    break;

  case 466:
#line 2134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10820 "Parser/parser.cc"
    break;

  case 467:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10826 "Parser/parser.cc"
    break;

  case 468:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10832 "Parser/parser.cc"
    break;

  case 469:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10838 "Parser/parser.cc"
    break;

  case 470:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10844 "Parser/parser.cc"
    break;

  case 471:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10850 "Parser/parser.cc"
    break;

  case 477:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   (yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10860 "Parser/parser.cc"
    break;

  case 490:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10866 "Parser/parser.cc"
    break;

  case 493:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 494:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10878 "Parser/parser.cc"
    break;

  case 496:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10884 "Parser/parser.cc"
    break;

  case 497:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10890 "Parser/parser.cc"
    break;

  case 498:
#line 2243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10896 "Parser/parser.cc"
    break;

  case 499:
#line 2245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10902 "Parser/parser.cc"
    break;

  case 500:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10908 "Parser/parser.cc"
    break;

  case 501:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10914 "Parser/parser.cc"
    break;

  case 503:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10920 "Parser/parser.cc"
    break;

  case 504:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10926 "Parser/parser.cc"
    break;

  case 506:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10932 "Parser/parser.cc"
    break;

  case 507:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10938 "Parser/parser.cc"
    break;

  case 508:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10944 "Parser/parser.cc"
    break;

  case 509:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10950 "Parser/parser.cc"
    break;

  case 510:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 10956 "Parser/parser.cc"
    break;

  case 511:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 10962 "Parser/parser.cc"
    break;

  case 512:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 10968 "Parser/parser.cc"
    break;

  case 513:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 10974 "Parser/parser.cc"
    break;

  case 514:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 10980 "Parser/parser.cc"
    break;

  case 515:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 10986 "Parser/parser.cc"
    break;

  case 516:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 517:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 10998 "Parser/parser.cc"
    break;

  case 518:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11004 "Parser/parser.cc"
    break;

  case 519:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11010 "Parser/parser.cc"
    break;

  case 520:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11016 "Parser/parser.cc"
    break;

  case 521:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11022 "Parser/parser.cc"
    break;

  case 522:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11028 "Parser/parser.cc"
    break;

  case 523:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11034 "Parser/parser.cc"
    break;

  case 524:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11040 "Parser/parser.cc"
    break;

  case 525:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11046 "Parser/parser.cc"
    break;

  case 526:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11052 "Parser/parser.cc"
    break;

  case 527:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11058 "Parser/parser.cc"
    break;

  case 528:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11064 "Parser/parser.cc"
    break;

  case 529:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11070 "Parser/parser.cc"
    break;

  case 530:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11076 "Parser/parser.cc"
    break;

  case 531:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11082 "Parser/parser.cc"
    break;

  case 532:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11088 "Parser/parser.cc"
    break;

  case 533:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11094 "Parser/parser.cc"
    break;

  case 534:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11100 "Parser/parser.cc"
    break;

  case 535:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11106 "Parser/parser.cc"
    break;

  case 536:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11112 "Parser/parser.cc"
    break;

  case 537:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11118 "Parser/parser.cc"
    break;

  case 538:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11124 "Parser/parser.cc"
    break;

  case 539:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11130 "Parser/parser.cc"
    break;

  case 540:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11136 "Parser/parser.cc"
    break;

  case 541:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11142 "Parser/parser.cc"
    break;

  case 542:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11148 "Parser/parser.cc"
    break;

  case 543:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11154 "Parser/parser.cc"
    break;

  case 545:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11160 "Parser/parser.cc"
    break;

  case 547:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 548:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11172 "Parser/parser.cc"
    break;

  case 549:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11178 "Parser/parser.cc"
    break;

  case 551:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11184 "Parser/parser.cc"
    break;

  case 552:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11190 "Parser/parser.cc"
    break;

  case 553:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11196 "Parser/parser.cc"
    break;

  case 554:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11202 "Parser/parser.cc"
    break;

  case 556:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11208 "Parser/parser.cc"
    break;

  case 558:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11214 "Parser/parser.cc"
    break;

  case 559:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11220 "Parser/parser.cc"
    break;

  case 560:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11226 "Parser/parser.cc"
    break;

  case 561:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11232 "Parser/parser.cc"
    break;

  case 562:
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11238 "Parser/parser.cc"
    break;

  case 563:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11244 "Parser/parser.cc"
    break;

  case 564:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11250 "Parser/parser.cc"
    break;

  case 565:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11256 "Parser/parser.cc"
    break;

  case 566:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11262 "Parser/parser.cc"
    break;

  case 568:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11268 "Parser/parser.cc"
    break;

  case 569:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11274 "Parser/parser.cc"
    break;

  case 570:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11280 "Parser/parser.cc"
    break;

  case 572:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11286 "Parser/parser.cc"
    break;

  case 573:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11292 "Parser/parser.cc"
    break;

  case 574:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11301 "Parser/parser.cc"
    break;

  case 576:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11307 "Parser/parser.cc"
    break;

  case 577:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11313 "Parser/parser.cc"
    break;

  case 578:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11319 "Parser/parser.cc"
    break;

  case 580:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11325 "Parser/parser.cc"
    break;

  case 581:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11331 "Parser/parser.cc"
    break;

  case 583:
#line 2472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11337 "Parser/parser.cc"
    break;

  case 584:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11343 "Parser/parser.cc"
    break;

  case 585:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11349 "Parser/parser.cc"
    break;

  case 586:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11355 "Parser/parser.cc"
    break;

  case 587:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11361 "Parser/parser.cc"
    break;

  case 588:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11367 "Parser/parser.cc"
    break;

  case 589:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11373 "Parser/parser.cc"
    break;

  case 590:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11379 "Parser/parser.cc"
    break;

  case 591:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11385 "Parser/parser.cc"
    break;

  case 593:
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11391 "Parser/parser.cc"
    break;

  case 594:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11397 "Parser/parser.cc"
    break;

  case 595:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11403 "Parser/parser.cc"
    break;

  case 596:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11409 "Parser/parser.cc"
    break;

  case 597:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11415 "Parser/parser.cc"
    break;

  case 602:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11421 "Parser/parser.cc"
    break;

  case 603:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11427 "Parser/parser.cc"
    break;

  case 604:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11436 "Parser/parser.cc"
    break;

  case 605:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11444 "Parser/parser.cc"
    break;

  case 606:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11453 "Parser/parser.cc"
    break;

  case 607:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11462 "Parser/parser.cc"
    break;

  case 608:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11471 "Parser/parser.cc"
    break;

  case 609:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11480 "Parser/parser.cc"
    break;

  case 611:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11486 "Parser/parser.cc"
    break;

  case 612:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11492 "Parser/parser.cc"
    break;

  case 613:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11502 "Parser/parser.cc"
    break;

  case 614:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11521 "Parser/parser.cc"
    break;

  case 617:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11527 "Parser/parser.cc"
    break;

  case 618:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11533 "Parser/parser.cc"
    break;

  case 619:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11539 "Parser/parser.cc"
    break;

  case 620:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11545 "Parser/parser.cc"
    break;

  case 621:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11551 "Parser/parser.cc"
    break;

  case 622:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11557 "Parser/parser.cc"
    break;

  case 623:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11566 "Parser/parser.cc"
    break;

  case 624:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11572 "Parser/parser.cc"
    break;

  case 625:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11581 "Parser/parser.cc"
    break;

  case 626:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11587 "Parser/parser.cc"
    break;

  case 627:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11596 "Parser/parser.cc"
    break;

  case 628:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11602 "Parser/parser.cc"
    break;

  case 629:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11608 "Parser/parser.cc"
    break;

  case 630:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11621 "Parser/parser.cc"
    break;

  case 631:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11630 "Parser/parser.cc"
    break;

  case 632:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11636 "Parser/parser.cc"
    break;

  case 633:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11642 "Parser/parser.cc"
    break;

  case 634:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11655 "Parser/parser.cc"
    break;

  case 635:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11661 "Parser/parser.cc"
    break;

  case 638:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11667 "Parser/parser.cc"
    break;

  case 639:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11673 "Parser/parser.cc"
    break;

  case 642:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11679 "Parser/parser.cc"
    break;

  case 644:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11685 "Parser/parser.cc"
    break;

  case 645:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11691 "Parser/parser.cc"
    break;

  case 646:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11697 "Parser/parser.cc"
    break;

  case 647:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11703 "Parser/parser.cc"
    break;

  case 648:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11709 "Parser/parser.cc"
    break;

  case 649:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11715 "Parser/parser.cc"
    break;

  case 651:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11721 "Parser/parser.cc"
    break;

  case 653:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11727 "Parser/parser.cc"
    break;

  case 654:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11733 "Parser/parser.cc"
    break;

  case 656:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11739 "Parser/parser.cc"
    break;

  case 657:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11745 "Parser/parser.cc"
    break;

  case 659:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11751 "Parser/parser.cc"
    break;

  case 660:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11757 "Parser/parser.cc"
    break;

  case 661:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11763 "Parser/parser.cc"
    break;

  case 662:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11769 "Parser/parser.cc"
    break;

  case 663:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11775 "Parser/parser.cc"
    break;

  case 664:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11781 "Parser/parser.cc"
    break;

  case 665:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11792 "Parser/parser.cc"
    break;

  case 666:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11798 "Parser/parser.cc"
    break;

  case 667:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11806 "Parser/parser.cc"
    break;

  case 668:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11812 "Parser/parser.cc"
    break;

  case 669:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl) && ((yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11823 "Parser/parser.cc"
    break;

  case 670:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11831 "Parser/parser.cc"
    break;

  case 671:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11839 "Parser/parser.cc"
    break;

  case 672:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11847 "Parser/parser.cc"
    break;

  case 673:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11855 "Parser/parser.cc"
    break;

  case 675:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11861 "Parser/parser.cc"
    break;

  case 676:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11867 "Parser/parser.cc"
    break;

  case 677:
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11876 "Parser/parser.cc"
    break;

  case 678:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11885 "Parser/parser.cc"
    break;

  case 679:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11891 "Parser/parser.cc"
    break;

  case 680:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].type)->symbolic.name ); }
#line 11897 "Parser/parser.cc"
    break;

  case 681:
#line 2821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11903 "Parser/parser.cc"
    break;

  case 682:
#line 2823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11909 "Parser/parser.cc"
    break;

  case 684:
#line 2829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11915 "Parser/parser.cc"
    break;

  case 685:
#line 2834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11921 "Parser/parser.cc"
    break;

  case 686:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 687:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11933 "Parser/parser.cc"
    break;

  case 688:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11939 "Parser/parser.cc"
    break;

  case 689:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11945 "Parser/parser.cc"
    break;

  case 691:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11951 "Parser/parser.cc"
    break;

  case 694:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11957 "Parser/parser.cc"
    break;

  case 695:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11963 "Parser/parser.cc"
    break;

  case 696:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 11969 "Parser/parser.cc"
    break;

  case 697:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11975 "Parser/parser.cc"
    break;

  case 700:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11981 "Parser/parser.cc"
    break;

  case 701:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11987 "Parser/parser.cc"
    break;

  case 702:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11993 "Parser/parser.cc"
    break;

  case 704:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 705:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12005 "Parser/parser.cc"
    break;

  case 706:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12011 "Parser/parser.cc"
    break;

  case 708:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12017 "Parser/parser.cc"
    break;

  case 709:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12023 "Parser/parser.cc"
    break;

  case 710:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12029 "Parser/parser.cc"
    break;

  case 711:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12035 "Parser/parser.cc"
    break;

  case 712:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12041 "Parser/parser.cc"
    break;

  case 714:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 715:
#line 2919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 716:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 721:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 723:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 724:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 730:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12083 "Parser/parser.cc"
    break;

  case 733:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12089 "Parser/parser.cc"
    break;

  case 734:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12095 "Parser/parser.cc"
    break;

  case 735:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12101 "Parser/parser.cc"
    break;

  case 736:
#line 2969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12107 "Parser/parser.cc"
    break;

  case 737:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12113 "Parser/parser.cc"
    break;

  case 738:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12119 "Parser/parser.cc"
    break;

  case 739:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12125 "Parser/parser.cc"
    break;

  case 741:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12131 "Parser/parser.cc"
    break;

  case 742:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12137 "Parser/parser.cc"
    break;

  case 743:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12143 "Parser/parser.cc"
    break;

  case 745:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12149 "Parser/parser.cc"
    break;

  case 747:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12155 "Parser/parser.cc"
    break;

  case 748:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 749:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12167 "Parser/parser.cc"
    break;

  case 750:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12173 "Parser/parser.cc"
    break;

  case 751:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 752:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12185 "Parser/parser.cc"
    break;

  case 754:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12191 "Parser/parser.cc"
    break;

  case 755:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12197 "Parser/parser.cc"
    break;

  case 756:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12203 "Parser/parser.cc"
    break;

  case 757:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12214 "Parser/parser.cc"
    break;

  case 758:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12220 "Parser/parser.cc"
    break;

  case 759:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12226 "Parser/parser.cc"
    break;

  case 760:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12232 "Parser/parser.cc"
    break;

  case 761:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12241 "Parser/parser.cc"
    break;

  case 762:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12247 "Parser/parser.cc"
    break;

  case 763:
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12257 "Parser/parser.cc"
    break;

  case 764:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12263 "Parser/parser.cc"
    break;

  case 765:
#line 3088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12269 "Parser/parser.cc"
    break;

  case 766:
#line 3090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12275 "Parser/parser.cc"
    break;

  case 767:
#line 3094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12281 "Parser/parser.cc"
    break;

  case 768:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12287 "Parser/parser.cc"
    break;

  case 769:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12293 "Parser/parser.cc"
    break;

  case 770:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12299 "Parser/parser.cc"
    break;

  case 771:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12305 "Parser/parser.cc"
    break;

  case 772:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12311 "Parser/parser.cc"
    break;

  case 775:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12317 "Parser/parser.cc"
    break;

  case 776:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12323 "Parser/parser.cc"
    break;

  case 777:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12329 "Parser/parser.cc"
    break;

  case 778:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12335 "Parser/parser.cc"
    break;

  case 780:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12341 "Parser/parser.cc"
    break;

  case 781:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 782:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12353 "Parser/parser.cc"
    break;

  case 783:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12359 "Parser/parser.cc"
    break;

  case 784:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 785:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 786:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 787:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12386 "Parser/parser.cc"
    break;

  case 788:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12395 "Parser/parser.cc"
    break;

  case 789:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12404 "Parser/parser.cc"
    break;

  case 790:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12410 "Parser/parser.cc"
    break;

  case 791:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12419 "Parser/parser.cc"
    break;

  case 792:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 794:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12431 "Parser/parser.cc"
    break;

  case 799:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12437 "Parser/parser.cc"
    break;

  case 800:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12443 "Parser/parser.cc"
    break;

  case 801:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12449 "Parser/parser.cc"
    break;

  case 803:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 12455 "Parser/parser.cc"
    break;

  case 804:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12461 "Parser/parser.cc"
    break;

  case 805:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12467 "Parser/parser.cc"
    break;

  case 806:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12473 "Parser/parser.cc"
    break;

  case 808:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12479 "Parser/parser.cc"
    break;

  case 809:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12485 "Parser/parser.cc"
    break;

  case 810:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12491 "Parser/parser.cc"
    break;

  case 811:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12509 "Parser/parser.cc"
    break;

  case 812:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12515 "Parser/parser.cc"
    break;

  case 813:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12521 "Parser/parser.cc"
    break;

  case 814:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12527 "Parser/parser.cc"
    break;

  case 815:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12533 "Parser/parser.cc"
    break;

  case 816:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12539 "Parser/parser.cc"
    break;

  case 817:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12545 "Parser/parser.cc"
    break;

  case 819:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12554 "Parser/parser.cc"
    break;

  case 820:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12560 "Parser/parser.cc"
    break;

  case 821:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12569 "Parser/parser.cc"
    break;

  case 822:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12579 "Parser/parser.cc"
    break;

  case 823:
#line 3288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12588 "Parser/parser.cc"
    break;

  case 824:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12598 "Parser/parser.cc"
    break;

  case 825:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12609 "Parser/parser.cc"
    break;

  case 826:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12619 "Parser/parser.cc"
    break;

  case 827:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12630 "Parser/parser.cc"
    break;

  case 828:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12640 "Parser/parser.cc"
    break;

  case 829:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12651 "Parser/parser.cc"
    break;

  case 830:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12661 "Parser/parser.cc"
    break;

  case 832:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12667 "Parser/parser.cc"
    break;

  case 833:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12673 "Parser/parser.cc"
    break;

  case 834:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12679 "Parser/parser.cc"
    break;

  case 835:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12691 "Parser/parser.cc"
    break;

  case 836:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12702 "Parser/parser.cc"
    break;

  case 837:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12711 "Parser/parser.cc"
    break;

  case 838:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12720 "Parser/parser.cc"
    break;

  case 839:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12726 "Parser/parser.cc"
    break;

  case 840:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12732 "Parser/parser.cc"
    break;

  case 841:
#line 3392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12738 "Parser/parser.cc"
    break;

  case 842:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12747 "Parser/parser.cc"
    break;

  case 843:
#line 3402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12753 "Parser/parser.cc"
    break;

  case 844:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12759 "Parser/parser.cc"
    break;

  case 845:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12765 "Parser/parser.cc"
    break;

  case 850:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12771 "Parser/parser.cc"
    break;

  case 851:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12777 "Parser/parser.cc"
    break;

  case 852:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12787 "Parser/parser.cc"
    break;

  case 853:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12793 "Parser/parser.cc"
    break;

  case 856:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12799 "Parser/parser.cc"
    break;

  case 857:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12805 "Parser/parser.cc"
    break;

  case 858:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12811 "Parser/parser.cc"
    break;

  case 859:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12817 "Parser/parser.cc"
    break;

  case 861:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12823 "Parser/parser.cc"
    break;

  case 862:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12829 "Parser/parser.cc"
    break;

  case 863:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12835 "Parser/parser.cc"
    break;

  case 864:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12841 "Parser/parser.cc"
    break;

  case 869:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12847 "Parser/parser.cc"
    break;

  case 870:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12853 "Parser/parser.cc"
    break;

  case 871:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 872:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12865 "Parser/parser.cc"
    break;

  case 873:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12871 "Parser/parser.cc"
    break;

  case 875:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 876:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12883 "Parser/parser.cc"
    break;

  case 877:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12889 "Parser/parser.cc"
    break;

  case 878:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12895 "Parser/parser.cc"
    break;

  case 879:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12901 "Parser/parser.cc"
    break;

  case 880:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12907 "Parser/parser.cc"
    break;

  case 881:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12913 "Parser/parser.cc"
    break;

  case 882:
#line 3547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12919 "Parser/parser.cc"
    break;

  case 883:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12925 "Parser/parser.cc"
    break;

  case 884:
#line 3551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12931 "Parser/parser.cc"
    break;

  case 885:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12937 "Parser/parser.cc"
    break;

  case 886:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12943 "Parser/parser.cc"
    break;

  case 887:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12949 "Parser/parser.cc"
    break;

  case 888:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12955 "Parser/parser.cc"
    break;

  case 889:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12961 "Parser/parser.cc"
    break;

  case 890:
#line 3566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12967 "Parser/parser.cc"
    break;

  case 891:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12973 "Parser/parser.cc"
    break;

  case 892:
#line 3577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12979 "Parser/parser.cc"
    break;

  case 894:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12985 "Parser/parser.cc"
    break;

  case 895:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12991 "Parser/parser.cc"
    break;

  case 896:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12997 "Parser/parser.cc"
    break;

  case 897:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13003 "Parser/parser.cc"
    break;

  case 898:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13009 "Parser/parser.cc"
    break;

  case 899:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13015 "Parser/parser.cc"
    break;

  case 900:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13021 "Parser/parser.cc"
    break;

  case 901:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13027 "Parser/parser.cc"
    break;

  case 902:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13033 "Parser/parser.cc"
    break;

  case 903:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13039 "Parser/parser.cc"
    break;

  case 904:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13045 "Parser/parser.cc"
    break;

  case 905:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13051 "Parser/parser.cc"
    break;

  case 906:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13057 "Parser/parser.cc"
    break;

  case 907:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13063 "Parser/parser.cc"
    break;

  case 908:
#line 3617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13069 "Parser/parser.cc"
    break;

  case 909:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13075 "Parser/parser.cc"
    break;

  case 913:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13081 "Parser/parser.cc"
    break;

  case 914:
#line 3639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13087 "Parser/parser.cc"
    break;

  case 915:
#line 3641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13093 "Parser/parser.cc"
    break;

  case 916:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13099 "Parser/parser.cc"
    break;

  case 917:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13105 "Parser/parser.cc"
    break;

  case 918:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13111 "Parser/parser.cc"
    break;

  case 919:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13117 "Parser/parser.cc"
    break;

  case 920:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13123 "Parser/parser.cc"
    break;

  case 921:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13129 "Parser/parser.cc"
    break;

  case 922:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13135 "Parser/parser.cc"
    break;

  case 923:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13141 "Parser/parser.cc"
    break;

  case 924:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13147 "Parser/parser.cc"
    break;

  case 925:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13153 "Parser/parser.cc"
    break;

  case 926:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13159 "Parser/parser.cc"
    break;

  case 927:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13165 "Parser/parser.cc"
    break;

  case 928:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13174 "Parser/parser.cc"
    break;

  case 929:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13180 "Parser/parser.cc"
    break;

  case 930:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13186 "Parser/parser.cc"
    break;

  case 932:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13192 "Parser/parser.cc"
    break;

  case 933:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13198 "Parser/parser.cc"
    break;

  case 934:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13204 "Parser/parser.cc"
    break;

  case 935:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13210 "Parser/parser.cc"
    break;

  case 936:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13216 "Parser/parser.cc"
    break;

  case 937:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13222 "Parser/parser.cc"
    break;

  case 938:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13228 "Parser/parser.cc"
    break;

  case 939:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13234 "Parser/parser.cc"
    break;

  case 940:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13240 "Parser/parser.cc"
    break;

  case 941:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13246 "Parser/parser.cc"
    break;

  case 942:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13252 "Parser/parser.cc"
    break;

  case 943:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13258 "Parser/parser.cc"
    break;

  case 944:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13264 "Parser/parser.cc"
    break;

  case 945:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13270 "Parser/parser.cc"
    break;

  case 946:
#line 3733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13276 "Parser/parser.cc"
    break;

  case 947:
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13282 "Parser/parser.cc"
    break;

  case 948:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13288 "Parser/parser.cc"
    break;

  case 949:
#line 3746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13294 "Parser/parser.cc"
    break;

  case 951:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13300 "Parser/parser.cc"
    break;

  case 952:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13306 "Parser/parser.cc"
    break;

  case 953:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13312 "Parser/parser.cc"
    break;

  case 954:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13318 "Parser/parser.cc"
    break;

  case 955:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13324 "Parser/parser.cc"
    break;

  case 956:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13330 "Parser/parser.cc"
    break;

  case 957:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13336 "Parser/parser.cc"
    break;

  case 958:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13342 "Parser/parser.cc"
    break;

  case 959:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13348 "Parser/parser.cc"
    break;

  case 960:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13354 "Parser/parser.cc"
    break;

  case 961:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13360 "Parser/parser.cc"
    break;

  case 962:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13366 "Parser/parser.cc"
    break;

  case 963:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13372 "Parser/parser.cc"
    break;

  case 964:
#line 3784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13378 "Parser/parser.cc"
    break;

  case 965:
#line 3786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13384 "Parser/parser.cc"
    break;

  case 966:
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13390 "Parser/parser.cc"
    break;

  case 967:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13396 "Parser/parser.cc"
    break;

  case 968:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13403 "Parser/parser.cc"
    break;

  case 970:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13409 "Parser/parser.cc"
    break;

  case 971:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13415 "Parser/parser.cc"
    break;

  case 972:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13421 "Parser/parser.cc"
    break;

  case 973:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13427 "Parser/parser.cc"
    break;

  case 974:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13433 "Parser/parser.cc"
    break;

  case 975:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13439 "Parser/parser.cc"
    break;

  case 976:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13445 "Parser/parser.cc"
    break;

  case 977:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13451 "Parser/parser.cc"
    break;

  case 978:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13457 "Parser/parser.cc"
    break;

  case 979:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13463 "Parser/parser.cc"
    break;

  case 980:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13469 "Parser/parser.cc"
    break;

  case 981:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13475 "Parser/parser.cc"
    break;

  case 982:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13481 "Parser/parser.cc"
    break;

  case 983:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13488 "Parser/parser.cc"
    break;

  case 985:
#line 3855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13494 "Parser/parser.cc"
    break;

  case 986:
#line 3857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13500 "Parser/parser.cc"
    break;

  case 987:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13506 "Parser/parser.cc"
    break;

  case 988:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13512 "Parser/parser.cc"
    break;

  case 989:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13518 "Parser/parser.cc"
    break;

  case 990:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13524 "Parser/parser.cc"
    break;

  case 991:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13530 "Parser/parser.cc"
    break;

  case 992:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13536 "Parser/parser.cc"
    break;

  case 993:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13542 "Parser/parser.cc"
    break;

  case 994:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13548 "Parser/parser.cc"
    break;

  case 995:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13554 "Parser/parser.cc"
    break;

  case 997:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13560 "Parser/parser.cc"
    break;

  case 998:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13566 "Parser/parser.cc"
    break;

  case 999:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13572 "Parser/parser.cc"
    break;

  case 1000:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13578 "Parser/parser.cc"
    break;

  case 1001:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13584 "Parser/parser.cc"
    break;

  case 1002:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13590 "Parser/parser.cc"
    break;

  case 1003:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13596 "Parser/parser.cc"
    break;

  case 1005:
#line 3926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13602 "Parser/parser.cc"
    break;

  case 1006:
#line 3928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13608 "Parser/parser.cc"
    break;

  case 1007:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13614 "Parser/parser.cc"
    break;

  case 1008:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13620 "Parser/parser.cc"
    break;

  case 1009:
#line 3937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13626 "Parser/parser.cc"
    break;

  case 1010:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13632 "Parser/parser.cc"
    break;

  case 1011:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13638 "Parser/parser.cc"
    break;

  case 1012:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13644 "Parser/parser.cc"
    break;

  case 1013:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13650 "Parser/parser.cc"
    break;

  case 1014:
#line 3957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13656 "Parser/parser.cc"
    break;

  case 1016:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13662 "Parser/parser.cc"
    break;

  case 1017:
#line 3970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13668 "Parser/parser.cc"
    break;

  case 1019:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13674 "Parser/parser.cc"
    break;

  case 1020:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13680 "Parser/parser.cc"
    break;

  case 1022:
#line 3981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13686 "Parser/parser.cc"
    break;

  case 1023:
#line 3983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13692 "Parser/parser.cc"
    break;

  case 1024:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13698 "Parser/parser.cc"
    break;

  case 1025:
#line 3990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13704 "Parser/parser.cc"
    break;

  case 1026:
#line 3992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13710 "Parser/parser.cc"
    break;

  case 1027:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13716 "Parser/parser.cc"
    break;

  case 1028:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13722 "Parser/parser.cc"
    break;

  case 1031:
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13729 "Parser/parser.cc"
    break;

  case 1032:
#line 4038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13735 "Parser/parser.cc"
    break;

  case 1033:
#line 4040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13741 "Parser/parser.cc"
    break;

  case 1034:
#line 4045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13747 "Parser/parser.cc"
    break;

  case 1035:
#line 4047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13753 "Parser/parser.cc"
    break;

  case 1036:
#line 4049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13759 "Parser/parser.cc"
    break;

  case 1037:
#line 4051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13765 "Parser/parser.cc"
    break;

  case 1038:
#line 4053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13771 "Parser/parser.cc"
    break;

  case 1040:
#line 4059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13777 "Parser/parser.cc"
    break;

  case 1041:
#line 4061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13783 "Parser/parser.cc"
    break;

  case 1042:
#line 4063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13789 "Parser/parser.cc"
    break;

  case 1043:
#line 4068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13795 "Parser/parser.cc"
    break;

  case 1044:
#line 4070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13801 "Parser/parser.cc"
    break;

  case 1045:
#line 4072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13807 "Parser/parser.cc"
    break;

  case 1047:
#line 4079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13813 "Parser/parser.cc"
    break;

  case 1049:
#line 4090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13819 "Parser/parser.cc"
    break;

  case 1050:
#line 4093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13825 "Parser/parser.cc"
    break;

  case 1051:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13831 "Parser/parser.cc"
    break;

  case 1052:
#line 4098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13837 "Parser/parser.cc"
    break;

  case 1053:
#line 4100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13843 "Parser/parser.cc"
    break;

  case 1054:
#line 4102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13849 "Parser/parser.cc"
    break;

  case 1056:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13855 "Parser/parser.cc"
    break;

  case 1057:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13861 "Parser/parser.cc"
    break;

  case 1058:
#line 4124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13867 "Parser/parser.cc"
    break;

  case 1059:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13873 "Parser/parser.cc"
    break;

  case 1060:
#line 4128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13879 "Parser/parser.cc"
    break;

  case 1061:
#line 4130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13885 "Parser/parser.cc"
    break;

  case 1062:
#line 4132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13891 "Parser/parser.cc"
    break;

  case 1064:
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13897 "Parser/parser.cc"
    break;

  case 1065:
#line 4140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13903 "Parser/parser.cc"
    break;

  case 1066:
#line 4142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13909 "Parser/parser.cc"
    break;

  case 1067:
#line 4147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13915 "Parser/parser.cc"
    break;

  case 1068:
#line 4149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13921 "Parser/parser.cc"
    break;

  case 1071:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13927 "Parser/parser.cc"
    break;

  case 1074:
#line 4170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13933 "Parser/parser.cc"
    break;

  case 1075:
#line 4172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13939 "Parser/parser.cc"
    break;

  case 1076:
#line 4174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13945 "Parser/parser.cc"
    break;

  case 1077:
#line 4176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13951 "Parser/parser.cc"
    break;

  case 1078:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13957 "Parser/parser.cc"
    break;

  case 1079:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13963 "Parser/parser.cc"
    break;

  case 1080:
#line 4187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13969 "Parser/parser.cc"
    break;

  case 1081:
#line 4189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13975 "Parser/parser.cc"
    break;

  case 1082:
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13981 "Parser/parser.cc"
    break;

  case 1083:
#line 4193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13987 "Parser/parser.cc"
    break;

  case 1084:
#line 4195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13993 "Parser/parser.cc"
    break;

  case 1085:
#line 4198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13999 "Parser/parser.cc"
    break;

  case 1086:
#line 4200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14005 "Parser/parser.cc"
    break;

  case 1087:
#line 4202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14011 "Parser/parser.cc"
    break;

  case 1088:
#line 4204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14017 "Parser/parser.cc"
    break;

  case 1089:
#line 4206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14023 "Parser/parser.cc"
    break;

  case 1090:
#line 4211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14029 "Parser/parser.cc"
    break;

  case 1091:
#line 4213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14035 "Parser/parser.cc"
    break;

  case 1092:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14041 "Parser/parser.cc"
    break;

  case 1093:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14047 "Parser/parser.cc"
    break;

  case 1095:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14053 "Parser/parser.cc"
    break;

  case 1099:
#line 4258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14059 "Parser/parser.cc"
    break;

  case 1100:
#line 4260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14065 "Parser/parser.cc"
    break;

  case 1101:
#line 4262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14071 "Parser/parser.cc"
    break;

  case 1102:
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14077 "Parser/parser.cc"
    break;

  case 1103:
#line 4266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14083 "Parser/parser.cc"
    break;

  case 1104:
#line 4268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14089 "Parser/parser.cc"
    break;

  case 1105:
#line 4275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14095 "Parser/parser.cc"
    break;

  case 1106:
#line 4277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14101 "Parser/parser.cc"
    break;

  case 1107:
#line 4279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14107 "Parser/parser.cc"
    break;

  case 1108:
#line 4281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14113 "Parser/parser.cc"
    break;

  case 1109:
#line 4283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14119 "Parser/parser.cc"
    break;

  case 1110:
#line 4285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14125 "Parser/parser.cc"
    break;

  case 1111:
#line 4290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14131 "Parser/parser.cc"
    break;

  case 1112:
#line 4292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14137 "Parser/parser.cc"
    break;

  case 1113:
#line 4294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14143 "Parser/parser.cc"
    break;

  case 1114:
#line 4299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14149 "Parser/parser.cc"
    break;

  case 1115:
#line 4301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14155 "Parser/parser.cc"
    break;

  case 1116:
#line 4303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14161 "Parser/parser.cc"
    break;

  case 1119:
#line 4327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14167 "Parser/parser.cc"
    break;

  case 1120:
#line 4329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14173 "Parser/parser.cc"
    break;


#line 14177 "Parser/parser.cc"

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
#line 4332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
