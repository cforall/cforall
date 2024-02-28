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

	// Start at second variable in declaration list and clone the type specifiers for each variable..
	for ( DeclarationNode * cur = dynamic_cast<DeclarationNode *>( declList->get_next() ); cur != nullptr; cur = dynamic_cast<DeclarationNode *>( cur->get_next() ) ) {
		cl->cloneBaseType( cur, copyattr );				// cur is modified
	} // for

	// Add first variable in declaration list with hidden type information in aggInst.aggregate, which is used by
	// extractType to recover the type for the aggregate instances.
	declList->addType( cl, copyattr );					// cl IS DELETED!!!
	return declList;
} // distAttr

void distExt( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
	for ( DeclarationNode *iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
		iter->set_extension( true );
	} // for
} // distExt

void distInl( DeclarationNode * declaration ) {
	// distribute INLINE across all declarations
	for ( DeclarationNode *iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
		iter->set_inLine( true );
	} // for
} // distInl

void distQual( DeclarationNode * declaration, DeclarationNode * qualifiers ) {
	// distribute qualifiers across all non-variable declarations in a distribution statemement
	for ( DeclarationNode * iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
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
    ELLIPSIS = 391,
    EXPassign = 392,
    MULTassign = 393,
    DIVassign = 394,
    MODassign = 395,
    PLUSassign = 396,
    MINUSassign = 397,
    LSassign = 398,
    RSassign = 399,
    ANDassign = 400,
    ERassign = 401,
    ORassign = 402,
    ErangeUpEq = 403,
    ErangeDown = 404,
    ErangeDownEq = 405,
    ATassign = 406,
    THEN = 407
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
#define ELLIPSIS 391
#define EXPassign 392
#define MULTassign 393
#define DIVassign 394
#define MODassign 395
#define PLUSassign 396
#define MINUSassign 397
#define LSassign 398
#define RSassign 399
#define ANDassign 400
#define ERassign 401
#define ORassign 402
#define ErangeUpEq 403
#define ErangeDown 404
#define ErangeDownEq 405
#define ATassign 406
#define THEN 407

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

	Token tok;
	ExpressionNode * expr;
	DeclarationNode * decl;
	ast::AggregateDecl::Aggregate aggKey;
	ast::TypeDecl::Kind tclass;
	StatementNode * stmt;
	ClauseNode * clause;
	ast::WaitForStmt * wfs;
    ast::WaitUntilStmt::ClauseNode * wucn;
	CondCtl * ifctl;
	ForCtrl * forctl;
	LabelNode * labels;
	InitializerNode * init;
	OperKinds oper;
	std::string * str;
	bool is_volatile;
	EnumHiding enum_hiding;
	ast::ExceptionKind except_kind;
	ast::GenericExpr * genexpr;

#line 724 "Parser/parser.cc"

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
#define YYFINAL  146
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   24067

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  180
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  311
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1111
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2201

#define YYUNDEFTOK  2
#define YYMAXUTOK   407


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
       2,     2,     2,   169,     2,     2,     2,   173,   166,     2,
     154,   156,   165,   167,   160,   168,   157,   172,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   161,   179,
     174,   178,   175,   177,   155,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   158,   171,   159,   164,     2,   163,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   162,   176,   153,   170,     2,     2,     2,
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
     145,   146,   147,   148,   149,   150,   151,   152
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   627,   627,   631,   638,   639,   640,   641,   642,   646,
     647,   648,   649,   650,   651,   652,   653,   657,   658,   662,
     663,   668,   672,   673,   684,   686,   688,   692,   693,   695,
     697,   699,   701,   711,   713,   715,   717,   719,   721,   726,
     727,   738,   743,   748,   749,   754,   760,   762,   764,   770,
     772,   776,   778,   780,   800,   802,   804,   807,   809,   811,
     813,   815,   817,   819,   821,   823,   825,   827,   829,   839,
     840,   844,   845,   850,   853,   857,   858,   862,   863,   865,
     867,   869,   871,   873,   878,   880,   882,   890,   891,   899,
     902,   903,   905,   910,   926,   928,   930,   932,   934,   936,
     938,   940,   942,   950,   951,   953,   957,   958,   959,   960,
     964,   965,   967,   969,   971,   973,   975,   977,   979,   986,
     987,   988,   989,   993,   994,   998,   999,  1004,  1005,  1007,
    1009,  1014,  1015,  1017,  1022,  1023,  1025,  1030,  1031,  1033,
    1035,  1037,  1042,  1043,  1045,  1050,  1051,  1056,  1057,  1062,
    1063,  1068,  1069,  1074,  1075,  1080,  1081,  1083,  1088,  1093,
    1094,  1102,  1108,  1109,  1113,  1114,  1118,  1119,  1123,  1124,
    1125,  1126,  1127,  1128,  1129,  1130,  1131,  1132,  1133,  1143,
    1145,  1150,  1151,  1153,  1155,  1160,  1161,  1167,  1168,  1174,
    1175,  1176,  1177,  1178,  1179,  1180,  1181,  1182,  1183,  1184,
    1185,  1186,  1187,  1189,  1190,  1196,  1198,  1208,  1210,  1218,
    1219,  1224,  1226,  1228,  1230,  1232,  1236,  1237,  1239,  1244,
    1251,  1253,  1255,  1265,  1267,  1269,  1274,  1279,  1282,  1287,
    1289,  1291,  1293,  1301,  1302,  1304,  1308,  1310,  1314,  1316,
    1317,  1319,  1321,  1326,  1327,  1331,  1336,  1337,  1341,  1343,
    1348,  1350,  1355,  1357,  1359,  1361,  1366,  1368,  1370,  1372,
    1377,  1379,  1384,  1385,  1407,  1409,  1414,  1417,  1419,  1422,
    1424,  1427,  1429,  1434,  1439,  1441,  1446,  1451,  1453,  1455,
    1457,  1459,  1462,  1464,  1467,  1469,  1474,  1480,  1483,  1485,
    1490,  1496,  1498,  1503,  1509,  1512,  1514,  1517,  1519,  1524,
    1531,  1533,  1538,  1544,  1546,  1551,  1557,  1560,  1565,  1575,
    1577,  1579,  1584,  1586,  1591,  1592,  1594,  1599,  1601,  1606,
    1608,  1610,  1612,  1615,  1619,  1622,  1626,  1628,  1630,  1632,
    1634,  1636,  1638,  1640,  1642,  1644,  1646,  1651,  1652,  1656,
    1662,  1670,  1675,  1676,  1680,  1681,  1687,  1691,  1692,  1695,
    1697,  1702,  1705,  1707,  1709,  1712,  1714,  1719,  1724,  1725,
    1729,  1734,  1736,  1741,  1743,  1748,  1750,  1752,  1757,  1762,
    1767,  1772,  1774,  1776,  1781,  1783,  1789,  1790,  1794,  1795,
    1796,  1797,  1801,  1806,  1807,  1809,  1811,  1813,  1817,  1821,
    1822,  1826,  1828,  1830,  1832,  1834,  1840,  1841,  1847,  1848,
    1852,  1853,  1858,  1860,  1869,  1870,  1872,  1877,  1882,  1893,
    1894,  1898,  1899,  1904,  1905,  1909,  1911,  1915,  1917,  1921,
    1922,  1926,  1927,  1931,  1932,  1933,  1937,  1939,  1954,  1955,
    1956,  1957,  1959,  1963,  1965,  1969,  1976,  1978,  1980,  1985,
    1986,  1988,  1990,  1992,  2024,  2027,  2032,  2034,  2040,  2045,
    2050,  2061,  2068,  2073,  2075,  2077,  2083,  2087,  2094,  2096,
    2097,  2098,  2114,  2116,  2119,  2121,  2124,  2129,  2130,  2134,
    2135,  2136,  2137,  2146,  2147,  2148,  2157,  2158,  2159,  2163,
    2164,  2165,  2174,  2175,  2176,  2181,  2182,  2191,  2192,  2197,
    2198,  2202,  2204,  2206,  2208,  2210,  2215,  2220,  2221,  2223,
    2233,  2234,  2239,  2241,  2243,  2245,  2247,  2249,  2252,  2254,
    2256,  2261,  2263,  2265,  2267,  2269,  2271,  2273,  2275,  2277,
    2279,  2281,  2283,  2285,  2287,  2289,  2291,  2293,  2295,  2297,
    2299,  2301,  2303,  2305,  2307,  2309,  2311,  2313,  2315,  2320,
    2321,  2325,  2332,  2333,  2339,  2340,  2342,  2344,  2346,  2351,
    2353,  2358,  2359,  2361,  2363,  2368,  2370,  2372,  2374,  2376,
    2378,  2383,  2384,  2386,  2388,  2393,  2395,  2394,  2398,  2406,
    2407,  2409,  2411,  2416,  2417,  2419,  2424,  2425,  2427,  2429,
    2434,  2435,  2437,  2442,  2444,  2446,  2448,  2449,  2451,  2456,
    2458,  2460,  2465,  2466,  2470,  2471,  2476,  2475,  2480,  2479,
    2489,  2488,  2499,  2498,  2508,  2513,  2514,  2519,  2525,  2543,
    2544,  2548,  2550,  2552,  2558,  2560,  2562,  2564,  2569,  2571,
    2576,  2578,  2587,  2588,  2593,  2602,  2607,  2609,  2611,  2620,
    2622,  2623,  2624,  2626,  2628,  2629,  2634,  2635,  2636,  2641,
    2643,  2646,  2649,  2656,  2657,  2658,  2664,  2669,  2671,  2677,
    2678,  2684,  2685,  2689,  2694,  2696,  2699,  2698,  2702,  2704,
    2711,  2713,  2717,  2720,  2719,  2730,  2734,  2738,  2742,  2747,
    2748,  2753,  2758,  2766,  2768,  2770,  2772,  2777,  2778,  2784,
    2785,  2786,  2793,  2794,  2796,  2797,  2798,  2800,  2802,  2809,
    2810,  2812,  2814,  2819,  2820,  2826,  2827,  2829,  2830,  2835,
    2836,  2837,  2839,  2847,  2848,  2850,  2853,  2855,  2859,  2860,
    2861,  2863,  2865,  2870,  2872,  2877,  2879,  2888,  2890,  2895,
    2896,  2897,  2901,  2902,  2903,  2908,  2909,  2914,  2915,  2916,
    2917,  2921,  2922,  2927,  2928,  2929,  2930,  2931,  2945,  2946,
    2951,  2952,  2958,  2960,  2963,  2965,  2967,  2990,  2991,  2997,
    2998,  3004,  3003,  3013,  3012,  3016,  3022,  3024,  3034,  3035,
    3037,  3041,  3046,  3048,  3050,  3052,  3058,  3059,  3063,  3064,
    3069,  3071,  3078,  3080,  3081,  3083,  3088,  3090,  3092,  3097,
    3099,  3104,  3109,  3117,  3122,  3124,  3129,  3134,  3135,  3140,
    3141,  3145,  3146,  3147,  3152,  3154,  3160,  3162,  3167,  3169,
    3175,  3176,  3180,  3184,  3188,  3190,  3204,  3206,  3208,  3210,
    3212,  3214,  3216,  3217,  3222,  3225,  3224,  3236,  3235,  3248,
    3247,  3261,  3260,  3274,  3273,  3289,  3295,  3297,  3303,  3304,
    3315,  3322,  3327,  3333,  3336,  3339,  3343,  3349,  3352,  3355,
    3360,  3361,  3362,  3363,  3367,  3373,  3374,  3384,  3385,  3389,
    3390,  3395,  3400,  3401,  3407,  3408,  3410,  3415,  3416,  3417,
    3418,  3419,  3421,  3456,  3458,  3463,  3465,  3466,  3468,  3473,
    3475,  3477,  3479,  3484,  3486,  3488,  3490,  3492,  3494,  3496,
    3501,  3503,  3505,  3507,  3516,  3518,  3519,  3524,  3526,  3528,
    3530,  3532,  3537,  3539,  3541,  3543,  3548,  3550,  3552,  3554,
    3556,  3558,  3570,  3571,  3572,  3576,  3578,  3580,  3582,  3584,
    3589,  3591,  3593,  3595,  3600,  3602,  3604,  3606,  3608,  3610,
    3622,  3627,  3632,  3634,  3635,  3637,  3642,  3644,  3646,  3648,
    3653,  3655,  3657,  3659,  3661,  3663,  3665,  3670,  3672,  3674,
    3676,  3685,  3687,  3688,  3693,  3695,  3697,  3699,  3701,  3706,
    3708,  3710,  3712,  3717,  3719,  3721,  3723,  3725,  3727,  3737,
    3739,  3741,  3742,  3744,  3749,  3751,  3753,  3758,  3760,  3762,
    3764,  3769,  3771,  3773,  3787,  3789,  3791,  3792,  3794,  3799,
    3801,  3806,  3808,  3810,  3815,  3817,  3822,  3824,  3841,  3842,
    3844,  3849,  3851,  3853,  3855,  3857,  3862,  3863,  3865,  3867,
    3872,  3874,  3876,  3882,  3884,  3887,  3890,  3892,  3896,  3898,
    3900,  3901,  3903,  3905,  3909,  3911,  3916,  3918,  3920,  3922,
    3957,  3958,  3962,  3963,  3965,  3967,  3972,  3974,  3976,  3978,
    3980,  3985,  3986,  3988,  3990,  3995,  3997,  3999,  4005,  4006,
    4008,  4017,  4020,  4022,  4025,  4027,  4029,  4043,  4044,  4046,
    4051,  4053,  4055,  4057,  4059,  4064,  4065,  4067,  4069,  4074,
    4076,  4084,  4085,  4086,  4091,  4092,  4097,  4099,  4101,  4103,
    4105,  4107,  4114,  4116,  4118,  4120,  4122,  4125,  4127,  4129,
    4131,  4133,  4138,  4140,  4142,  4147,  4173,  4174,  4176,  4180,
    4181,  4185,  4187,  4189,  4191,  4193,  4195,  4202,  4204,  4206,
    4208,  4210,  4212,  4217,  4219,  4221,  4228,  4230,  4248,  4250,
    4255,  4256
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
  "NE", "ANDAND", "OROR", "ELLIPSIS", "EXPassign", "MULTassign",
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
  "selection_statement", "if_statement", "conditional_declaration",
  "case_value", "case_value_list", "case_label", "case_label_list",
  "case_clause", "switch_clause_list_opt", "switch_clause_list",
  "iteration_statement", "for_control_expression_list",
  "for_control_expression", "downupdowneq", "updown", "updowneq",
  "jump_statement", "fall_through_name", "with_statement",
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
  "basic_type_name", "vtable_opt", "vtable", "default_opt",
  "basic_declaration_specifier", "basic_type_specifier", "direct_type",
  "indirect_type", "sue_declaration_specifier", "sue_type_specifier",
  "$@1", "sue_declaration_specifier_nobody", "sue_type_specifier_nobody",
  "type_declaration_specifier", "type_type_specifier", "type_name",
  "typegen_name", "elaborated_type", "elaborated_type_nobody",
  "aggregate_type", "$@2", "$@3", "$@4", "$@5", "type_parameters_opt",
  "aggregate_type_nobody", "aggregate_key", "aggregate_data",
  "aggregate_control", "field_declaration_list_opt", "field_declaration",
  "field_declaring_list_opt", "field_declarator",
  "field_abstract_list_opt", "field_abstract", "cfa_field_declaring_list",
  "cfa_field_abstract_list", "bit_subrange_size_opt", "bit_subrange_size",
  "enum_type", "$@6", "$@7", "hide_opt", "enum_type_nobody",
  "enumerator_list", "visible_hide_opt", "enumerator_value_opt",
  "cfa_parameter_ellipsis_list_opt", "cfa_parameter_list",
  "cfa_abstract_parameter_list", "parameter_type_list_opt",
  "parameter_list", "cfa_parameter_declaration",
  "cfa_abstract_parameter_declaration", "parameter_declaration",
  "abstract_parameter_declaration", "identifier_list",
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
     405,   406,   407,   125,    40,    64,    41,    46,    91,    93,
      44,    58,   123,    96,    94,    42,    38,    43,    45,    33,
     126,    92,    47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1803)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1110)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     164, 12994,   172,   178, 18726,    72, -1803, -1803, -1803, -1803,
   -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803,    69,   993,
      83, -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803,
   -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803,
   -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803,   107,   253,
   -1803, -1803, -1803, -1803, -1803, -1803,  5154,  5154,   216, 12994,
     244,   296, 13376, -1803,   355, -1803, -1803, -1803, -1803, -1803,
   -1803, -1803, -1803, -1803, -1803,  2672, -1803,   486,   401, -1803,
   -1803, -1803, -1803, -1803, 18572, -1803, -1803,   444,   480,   300,
      79, -1803,  5154,   480,   498,   569,   559,  4819,   764,   862,
   13158, -1803, -1803,   648, 18418,  1564, -1803, -1803, -1803,  3920,
     773,  9304,  9809,   888,  3920,  1002,   625, -1803, -1803, -1803,
   -1803,   738, -1803, -1803, -1803, -1803,   697, -1803, -1803, -1803,
   -1803, -1803,   684,   698,   738, -1803,   738, 17043, -1803, -1803,
   -1803, 19687,  5154, -1803, -1803,  5154, -1803, 12994, -1803,   740,
   19740, -1803, -1803,  4933, 20878, -1803, -1803,   966,   966,   765,
    2435, -1803, -1803, -1803, -1803,   543, 15634,  3600,   738, -1803,
   -1803, -1803, -1803, -1803, -1803,   782, -1803,   790,   844,   857,
   -1803,   875, 23330, -1803, -1803, -1803, -1803, -1803, -1803, -1803,
   17420,  3665,  2672,   820,   864,   876,   885,   891,   893,   912,
   -1803, -1803, 19894, 11923,   872, -1803, 19027, -1803, -1803, -1803,
   -1803,   926, -1803, -1803,   913, -1803, 21430,  1091,  9401, -1803,
     968,  5154,   698,   984,   922,  4933,  2324, -1803, -1803, -1803,
    2646,  3384,   960,  1039,   318,  1039, -1803,   738,   738,    50,
   16829,   403,  1039, -1803,   738,   738,    50,   738, -1803,   738,
   -1803,  3933, -1803, -1803,   994,  1012,   966, 15198, -1803, 18572,
   -1803, -1803,  3920, -1803,  2790,   625,  1026,  1080, 16829,  5154,
    5154,   300, -1803, 14819, -1803,   966,   966,  1045,  1080, 16829,
    5154, -1803, 23944, -1803, -1803, -1803,   966, -1803, -1803, -1803,
   -1803,   966, -1803,  1038,  4461,  5154, -1803, 18273,  1021, -1803,
   -1803, -1803, 21334,   698, 16936,  1033,  4933, 18220, 15198,  3920,
   -1803, -1803,  4609, -1803,  1039,   179, -1803, 23330, 20878,  3411,
    3933, -1803,   433, -1803, -1803, -1803, -1803, -1803, 19740,  5154,
   -1803,  1058,  1046, -1803, -1803, -1803, -1803,  5154,  2173,   434,
     534, -1803,  5154,   790, -1803,   549,   738,   738,  1071, 19947,
     792, 16123, 15361,  3920, -1803,  3920,   966,  3920,   966, -1803,
   -1803,   738, -1803,  1079, -1803, 20101, -1803, -1803, -1803, 20154,
     926, -1803,   421,   778,   407,  1082,   649,   625,  1086, -1803,
    2435,  1063,   790,  2435,  1017, -1803,  1107,  1152, 23406,  1116,
    1126,  1128, 23330, 23482,  1136, 16502, -1803, -1803, -1803, -1803,
   -1803, -1803, 23558, 23558, 17262,  1133,  4999, -1803, -1803, -1803,
   -1803,   411, -1803,   607, -1803,  1576, -1803, 23330, 23330, -1803,
    1162,   710,  1019,  1073,   733,  1120,  1157,  1177,  1178,  1222,
      25, -1803,   779, -1803,  1202, -1803,  1171,  4228, 17736, -1803,
   -1803,   807,  1202, -1803, -1803,   789, -1803, -1803,  3665,  1207,
    1219,  1224,  1226,  1234,  1251, -1803, -1803,   443,  1256, -1803,
     131,  1256, -1803, -1803, 19687, -1803,  1193,  1264, 17894, -1803,
   -1803,  5025,  4427,  1293, 16123,  1299,   550,   775, -1803, -1803,
   -1803, -1803, -1803,  5154,  5053, -1803, -1803, -1803, -1803, -1803,
   -1803, 15036,  3872,  1133, 21430,  1278,  1281, -1803, -1803,  1287,
    9401,   870, -1803, -1803, -1803, 21582,  1302, -1803, -1803, -1803,
   -1803,  1275,  2646,   905,  1308,  1313,  1316,   956,  1321,  1324,
    1326,  1330,  1339,  1347,  3384, -1803, -1803, -1803,   738,  1334,
    1346,  1349, -1803, -1803,  1359,   300, -1803, -1803,   698,  1080,
   18889, -1803, -1803,   300, -1803, -1803,   698, -1803, -1803,  3933,
   -1803, 17736, 17736, -1803,   966,  4933, 20929, 16286, -1803, -1803,
   -1803, -1803, -1803,   698,  1080,   179,  1358, -1803, -1803,  3920,
    1365,  1080, 16829, -1803,   698,  1080, -1803, 23995, -1803,   966,
     966, -1803, -1803,  1386,   368,  1389,   625,  1397, -1803, -1803,
   -1803, 19326,  1390,  1380, -1803, -1803,   796, -1803,  1491, -1803,
    1414, -1803, -1803, -1803, 20317, 23634, -1803, -1803, -1803, -1803,
   -1803,  3411,   963,  3933, 18889, 16286,  1039, 12994, -1803,  5154,
    1404, -1803,  1444, -1803, -1803, -1803, -1803, -1803,  2435, -1803,
   -1803,  1526,  4729,  3533, 20154, 11923, -1803, 20370, -1803,   966,
     966, -1803, -1803,   926, -1803, 15145,  1452,  1598, 23330,  1761,
    1359,  1445, -1803,   738,   738, -1803,  1256, -1803, 19947, -1803,
   -1803, 19326,   966,   966, -1803,  4729,   738, -1803, 20731, -1803,
   -1803, 20101, -1803,   543, -1803, -1803, -1803,  1464,  5154,   407,
    1086,  1469,   803, 19740,   877, -1803, -1803, -1803, -1803, -1803,
   -1803,   927, -1803,  1482,  1454, -1803, 17578, -1803,  4999, 20524,
   20524, -1803, 17578, -1803, 23330, -1803, -1803, -1803, -1803, -1803,
   -1803, 17578, -1803, -1803, 19379, 20524, 20524,  1171,  1322,  1354,
     705,  1676, -1803,   944,  1484,  1212,  1486, -1803, 21582, 23330,
   21658,  1476, 23330,  2324, 23330,  2324, -1803,  2422, -1803, -1803,
   21734,  2336, 23330, 21734,  2324, -1803, -1803, 23330, 23330, 23330,
   23330, 23330, 23330, 23330, 23330, 23330, 23330, 23330, 23330, 23330,
   23330, 23330, 23330, 23330, 23330, 23330, 21810,  1465,   875,  4200,
   11923, -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803,
   -1803, -1803,  1485, 23330, -1803, -1803, 15308,  1944, -1803, -1803,
     738,   738, -1803, -1803, 17736, -1803,   479,  1256, -1803,   981,
    1256, 18889, -1803, -1803,  1359, 18889, -1803,  1359, 23710, -1803,
   -1803, 11923,  1487,  1492, 14656,  1632,  3903,   487,  1445, -1803,
     738,   738,  1445,   520, -1803,   738,   738, 23330,  5154,  1225,
    1229,  1445,   218, 15471, 15471,  5154, -1803, -1803, 23330,  1287,
   -1803, 21430,  1499, -1803,  2785, -1803, -1803, -1803, -1803, -1803,
     985, -1803, 15471,  2324, 23330,  1010,  1501,  1502,  1506,  1022,
    1509,  1510,  1511,  1512,  1515,  1516,   587,  1256, -1803, -1803,
     597,  1256, -1803, -1803,   599,  1256, -1803, -1803, -1803,  4933,
     875,  1633,  1256, 21025, -1803, -1803,   698,  1517, -1803, -1803,
   -1803,  1025,  1518,  1057,  1520, -1803,  1527, -1803,   698, -1803,
    1528, -1803,   698,  1080,  1527, -1803,   698,  1522,  1523,  1524,
   -1803, -1803, 19190, -1803,  2324,  5154, 11057,  1609, -1803,  1264,
   -1803, 15471,  1060, -1803, -1803,  1527,  1531, -1803, 19740, 17736,
    1504, -1803,  1504, -1803, -1803, -1803,   407,  1533,   738,   738,
   -1803, 20101, -1803, 12090, 18052, -1803,  1541,  1542,  1544,  1549,
   -1803, 10012,   738, -1803,  1761, -1803, -1803, -1803, -1803,  1359,
   -1803, -1803, -1803,   966, -1803,  3735, -1803, -1803,   625,   268,
    1553,  1529,  1464,  1546,   407, -1803, -1803,  1547,  1555,  1017,
   21734, -1803,  1559,  1556,   401,  1557,  1563,  1566,  1560,  1568,
   23330,  1572,  1573,  1575, 11923, 23330, -1803, -1803,  1736, -1803,
   -1803, -1803, 23330, -1803,  1581,  1584, 21506,  1257, -1803, 21734,
    1583, -1803,  1585, -1803, -1803,  4905, -1803, -1803,  1059, -1803,
   -1803, -1803, -1803, -1803, -1803,  4905, -1803, -1803,  1259,   691,
   -1803, -1803,  1162,  1162,  1162,   710,   710,  1019,  1019,  1073,
    1073,  1073,  1073,   733,   733,  1120,  1157,  1177,  1178,  1222,
   23330,  1261, -1803,  1589,  4905, -1803, -1803, 21430, -1803,  1600,
    1601,  1603,  1604,  1944, -1803, -1803, -1803, -1803, -1803, 18889,
   -1803, -1803,  1359, 18889, -1803,  1359,  1605,  1607, -1803, -1803,
   14656,  1007,  1608,  1610,  1611,  1612,  2098,  3903, -1803, -1803,
   18889, -1803, -1803, -1803, -1803, -1803, -1803, 18889, -1803, -1803,
   -1803, -1803,  1606, -1803,  1445, -1803, -1803, -1803, -1803, -1803,
   -1803, -1803, -1803,  1613,  1624, -1803,   300,  4905,  1266,   258,
   -1803, -1803,  1628, -1803,  9401, -1803, 23330,   738, 21886, 15471,
   -1803, -1803, -1803,   650,  1256, -1803,   651,  1256, -1803, -1803,
     665,  1256, 18889, -1803, -1803,  1359, 18889, -1803, -1803,  1359,
   18889, -1803, -1803,  1359,  1039,  1626, -1803,  1359,   350, -1803,
    1202,  1625, -1803, -1803, -1803, -1803, -1803, -1803,  1631, -1803,
   -1803, -1803, 19740,  1527, -1803,   698, -1803, -1803, -1803, -1803,
   -1803, 13807, -1803, -1803, -1803, -1803,   393, -1803,   663,   419,
   11756,  1634, 16652,  1635,  1636,  2805,  2892,  3521, 21962,  1638,
   -1803, -1803,  1639,  1641, 16652,  1642, -1803, -1803,   698, 23330,
   23330,  1772,  1637,   762, -1803, 17104,  1723,  1643,  1620, -1803,
   -1803, -1803, 10876, -1803, -1803, -1803, -1803, -1803,  2230, -1803,
   -1803, -1803,  1336,   180, -1803,   223, -1803,   180, -1803, -1803,
   -1803, -1803, -1803,  2324, -1803, -1803, 13322, 18572,  1645, -1803,
    5154,  1648,  1651, -1803,  1272, -1803, -1803,  5154, -1803, -1803,
    4933, -1803, -1803,  1623,  1640,  1066, 19740,   790,   790,  1464,
     407,  1086,  1086, -1803, -1803,  1133,  1264, 17894, -1803,  1202,
   -1803, 12257, -1803,   678,  1256, -1803,   966, 12826, -1803, -1803,
     407,  1649,   738,   738,   543,  5154, -1803, 22038, -1803,  1660,
     407,  1464,  1661, -1803, -1803,  1072,   732, 19326, 11923,  2324,
   -1803,   732, 19533,   732, -1803, 23330, 23330, 23330, -1803, -1803,
   -1803, -1803, 23330, 23330,  1654, 21430, -1803, -1803,  1657,   768,
   -1803, -1803, -1803,  4561, -1803, -1803,  1290, -1803,   229, -1803,
   21734,  1303, -1803, 21582, -1803, -1803, 23330,  1647,  1332,  1337,
    1287, -1803,   701,  1256, -1803, -1803,  1664,  1665, -1803, -1803,
    1666,   711,  1256, -1803,   712,  3028,   738,   738, -1803, -1803,
    1673,  1674, -1803,  1672, -1803, 16286, 16286,  1678,  1675,  1677,
    1684, -1803,  1681, 23330, 23330,  1340,  1685, -1803, -1803, -1803,
   -1803, -1803, -1803, -1803,  1687, 18889, -1803, -1803,  1359, 18889,
   -1803, -1803,  1359, 18889, -1803, -1803,  1359,  1693,  1695,  1699,
     300,   738, -1803, -1803,  1342, 23330, 21176,  1698,  1679, -1803,
   -1803, -1803,  1705, 13964, 14121, 14278, 19740, 15198, 20524, 20524,
    1707, -1803,  1680,   513,  1043, 14493, -1803,   537,  5154,  5154,
   -1803, 21734,   338,   383, -1803, -1803, -1803, -1803, 23330,  1708,
    1783, 11588, 11234, -1803,  1686, -1803,  1688, 23330,  1692, 21430,
    1696, 23330, 21582, 23330, -1803, 11411,  1231, -1803,  1700,   -30,
   -1803,   105,  1780,   329,  1710, -1803, -1803,  1725, -1803,  1701,
   -1803,  1702,  1728,  1729, 16652, 16652, -1803, -1803,  1795, -1803,
   -1803,    21,    21,   848, 14982,   738,   552, -1803, -1803,  1730,
   -1803,  1734,   434, -1803,  1737, -1803,  1733, -1803,  1738, -1803,
   -1803, -1803, -1803,  1746,  1464,  1727,  1739, 12424,  1744,  1747,
    1749, -1803, 18889, -1803, -1803,  1359, 23330, 23330,  1264,  1751,
   -1803,  1464,   407, -1803,  1086,   312,  1529, 21430, -1803, -1803,
    1464,  1758, -1803, 19740, -1803,   906,  1756,  1752,  1105, -1803,
    1759, -1803, -1803, -1803, -1803, -1803, 21430,  1287, 21582, -1803,
    1794,  4905, -1803,  1794,  1794, -1803,  4905,  4636,  4760, -1803,
   -1803,  1353, -1803, -1803, -1803,  1768, 18889, -1803, -1803,  1359,
   -1803, -1803,   738, 18889, -1803, -1803,  1359, 18889, -1803, -1803,
    1766, -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803, -1803,
   -1803, -1803, -1803, -1803,  1764, -1803, -1803, -1803, -1803,  1765,
    1769,   738,  1773,  1774,  1775, -1803, -1803, -1803, -1803, -1803,
   23330, -1803,   350, -1803,  1202, -1803, -1803,  1779,  1784, -1803,
    1707,  1707,  1707,  3997,   936,  1754,   568, -1803,  3997,   582,
   17736, -1803, -1803, -1803,  4170, 23330,  4400,    54, -1803, -1803,
      76,  1778,  1778,  1778,  5154, -1803, -1803, -1803,  1106, -1803,
   -1803, -1803, -1803,  1113,  1782, 16652,  1643,  1786, 23330,   444,
    1789,   559, 14442, 19740, -1803, -1803, -1803,   646, 16652, 23330,
     928,   739, -1803, 23330,  8505, -1803, -1803,   592, -1803,  1287,
   -1803,  1127,  1129,  1142,   832, -1803, -1803, -1803, -1803,   698,
    1231,  1790, -1803, -1803, 23330, -1803,  1796,   875, -1803, 11756,
   -1803, -1803, -1803, -1803, 23330, 23330, -1803, -1803,   427,    21,
   -1803,   310, -1803, -1803, 10651, -1803,   738, 16286, -1803,  1504,
   -1803, 19740, -1803, -1803, -1803,  1787,   407,   407, -1803, -1803,
   -1803,  1797,  1798, -1803, -1803,  1803, -1803,  1806,  1800,  1464,
    1086,  1793, -1803, -1803,  1287,  1813, -1803, -1803,  1812, -1803,
   -1803, 23330, -1803, 19533, 23330,  1287,  1819,  1355, -1803,  1363,
   -1803,  4905, -1803,  4905, -1803, -1803, -1803,  1822,  1824,  1825,
    1370, 15797, 15960, -1803,  1823, -1803, -1803, -1803, -1803, -1803,
    1372, 23330, -1803, -1803, -1803, -1803, -1803,   614,   936,  2158,
     662, -1803, -1803, -1803, -1803,   738,   738, -1803, -1803, -1803,
     670, -1803,  1144,  4170,   865, -1803,  4400, -1803,   738, -1803,
   -1803, -1803, -1803, -1803, -1803, 16652,   388, 22114,  1910, 16652,
    1643, 16449, -1803, -1803, -1803, -1803, 23330, -1803, 22190,  1911,
    1808, 21351, 22266, 16652, 11411,  1643,   560,  1088,  1809, 23330,
   -1803,  1838,   557, 16652, -1803, 16652, -1803,  1839, -1803, -1803,
    1815,   875,   833,  1836,  1840,  1374,  1147, 16652,  1841, 16652,
   16652, 16652, -1803, -1803, -1803,   790, -1803,  5154,  4933, -1803,
    1464,  1464, -1803, -1803,  1842,  1843, -1803, -1803, -1803,  1844,
    1837,   407,  1847, -1803,  1850, -1803, -1803, -1803, -1803,  1852,
   -1803, -1803, -1803,  1378,  1384, -1803, -1803, -1803, -1803, -1803,
   -1803,  1854, -1803, -1803, -1803, -1803, -1803,  1856,  1859,  1863,
    2158, -1803,   738, -1803, -1803, -1803, -1803, -1803,  1848,  3997,
   -1803,  6394,    88, 12594, -1803, 16545, -1803,    38,  1150, 16652,
    1946,   702,  1855,   -76, 16652, 23330,  1865,   560,  1088,  1853,
   23786,  1861,   283,  1951, -1803, 22342, 22418, 23330,  1643,  1858,
   12760, -1803, -1803, -1803, -1803, 20577, -1803,  1868,  1860,    51,
   16652, -1803, 23330, 21734, -1803, -1803, 23330,   180, -1803, -1803,
   -1803, -1803, -1803,  1885,  1887, -1803, -1803, -1803,   407,  1464,
   -1803, -1803, -1803, -1803, -1803, 16286,  1886,   713,  1256, -1803,
   -1803,   936, -1803, -1803,   324, -1803,   379, -1803, -1803, -1803,
    1892, 13486, -1803, -1803, 16652, -1803,   118, -1803, 16652, 23330,
    1894, 22494, -1803, -1803, 22570, 22646, 23330,  1865,  1643, 22722,
   22798, 16652,  1877,   354,  1880,   493, -1803, -1803,  1900, 13486,
   20577, -1803,  4790, 20370,  2324,  1893, -1803,  1949,  1903,   841,
    1899, -1803,  1985, -1803,  1153,  1156,   538, -1803, -1803,  1464,
    1909, -1803, 18889, -1803, -1803,  1359, -1803, 23330, -1803, 23330,
   -1803, -1803,  1479, 13650, -1803, -1803, 16652, -1803, -1803,  1643,
   -1803, -1803,  1643,  1895,   637,  1896,   714, -1803, -1803,  1643,
   -1803,  1643, -1803,  1908, 22874, 22950, 23026, -1803,  1479, -1803,
    1888,  1286,  4325, -1803, -1803, -1803,    51,  1912, 23330,  1890,
      51,    51, 16652, -1803, -1803, 16652,  1995,  1919, -1803,  1918,
   -1803, -1803, 16545, -1803,  1479, -1803, -1803,  1924, 23102, 23178,
   23254, -1803, -1803,  1643, -1803,  1643, -1803,  1643, -1803,  1888,
   23330,  1925,  4325,  1922,   875,  1928, -1803,   845, -1803, -1803,
   -1803, 16652, -1803, -1803, -1803, 10220,  1933, 16545, -1803, -1803,
    1643, -1803,  1643, -1803,  1643,  1934,  1932, -1803,   698,   875,
    1936, -1803,  1914,   875, -1803, -1803, -1803, -1803, 10440, -1803,
     698, -1803, -1803,  1423, 23330, -1803,  1158, -1803,   875,  2324,
    1935,  1915, -1803, -1803,  1165, -1803, -1803,  1920,  2324, -1803,
   -1803
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   485,     0,     2,   485,   502,   503,   504,   505,   506,
     507,   508,   509,   510,   491,   493,   492,   494,     0,     0,
       0,   511,   513,   534,   514,   535,   517,   518,   532,   533,
     512,   530,   531,   515,   516,   519,   520,   521,   522,   523,
     524,   525,   526,   527,   528,   529,   536,   537,   847,   539,
     612,   613,   616,   618,   614,   620,     0,     0,     0,   485,
       0,     0,    17,   583,   589,     9,    10,    11,    12,    13,
      14,    15,    16,   804,   105,     0,    20,     0,     2,   103,
     104,    18,    19,   863,   485,   805,   425,     0,   428,   727,
     430,   439,     0,   429,   459,   460,     0,     0,     0,     0,
     566,   487,   489,   495,   485,   497,   500,   551,   538,   469,
     544,   549,   471,   561,   470,   576,   580,   586,   565,   592,
     604,   847,   609,   610,   593,   668,   431,   432,     3,   812,
     825,   490,     0,     0,   847,   885,   847,   485,   902,   903,
     904,   485,     0,  1089,  1090,     0,     1,   485,    17,     0,
     485,   448,   449,     0,   566,   495,   479,   480,   481,   815,
       0,   615,   617,   619,   621,     0,   485,     0,   848,   849,
     611,   540,   720,   721,   719,   781,   776,   766,     0,     0,
     813,     0,     0,   502,   806,   810,   811,   807,   808,   809,
     485,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     584,   587,   485,   485,     0,  1091,   566,   892,   910,  1095,
    1088,  1086,  1093,   424,     0,   167,   733,   166,     0,   433,
       0,     0,     0,     0,     0,     0,     0,   423,   979,   980,
       0,     0,   458,   845,   847,   845,   866,   847,   847,   468,
     485,   847,   845,   923,   847,   847,   467,   847,   942,   847,
     920,     0,   559,   560,     0,     0,   485,   485,     2,   485,
     440,   488,   498,   552,     0,   581,     0,   828,   485,     0,
       0,   727,   441,   566,   545,   562,   577,     0,   828,   485,
       0,   501,   546,   553,   554,   472,   563,   474,   475,   473,
     568,   578,   582,     0,   596,     0,   798,   485,     2,   826,
     884,   886,   485,     0,   485,     0,     0,   566,   485,   497,
       2,  1099,   566,  1102,   845,   845,     3,     0,   566,     0,
       0,   451,   847,   840,   842,   841,   843,     2,   485,     0,
     802,     0,     0,   762,   764,   763,   765,     0,     0,   758,
       0,   747,     0,   756,   768,     0,   847,   847,     2,   485,
    1110,   486,   485,   476,   544,   477,   569,   478,   576,   573,
     594,   847,   595,     0,   708,   485,   709,  1064,  1065,   485,
     710,   712,   583,   589,   669,     0,   671,   672,   669,   850,
       0,   779,   767,     0,   854,    22,     0,    21,     0,     0,
       0,     0,     0,     0,     0,    24,    26,     4,     8,     5,
       6,     7,     0,     0,   485,     2,     0,   106,   107,   108,
     109,    90,    25,    91,    43,    89,   110,     0,     0,   125,
     127,   131,   134,   137,   142,   145,   147,   149,   151,   153,
     155,   158,     0,    27,     0,   590,     2,   110,   485,   159,
     773,   723,   580,   725,   772,     0,   722,   726,     0,     0,
       0,     0,     0,     0,     0,   864,   890,   847,   900,   908,
     912,   918,     2,  1097,   485,  1100,     2,   103,   485,     3,
     707,     0,  1110,     0,   486,   544,   569,   576,     3,     3,
     689,   693,   703,   709,   710,     2,   893,   911,  1087,     2,
       2,    24,     0,     2,   733,    25,     0,   731,   734,  1108,
       0,     0,   740,   729,   728,     0,     0,   830,     2,     2,
     452,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   869,   926,   949,   847,     0,
     464,     2,   865,   873,  1007,   727,   867,   868,     0,   828,
     485,   922,   930,   727,   924,   925,     0,   941,   943,     0,
     454,   485,   485,   550,   486,     0,   566,   485,  1092,  1096,
    1094,   567,   802,     0,   828,   845,     0,   434,   442,   499,
       0,   828,   485,   802,     0,   828,   777,   547,   548,   564,
     579,   585,   588,   583,   589,   607,   608,     0,   778,   696,
     717,   486,     0,   697,   700,   699,     0,   207,   417,   827,
       0,   415,   468,   467,   566,     0,   436,     2,   437,   799,
     456,     0,     0,     0,   485,   485,   845,   485,   802,     0,
       0,     2,     0,   761,   760,   759,   753,   496,     0,   751,
     769,   542,     0,     0,   485,   485,  1066,   486,   482,   483,
     484,  1070,  1061,  1062,  1068,   485,     2,   104,     0,  1026,
    1040,  1110,  1022,   847,   847,  1031,  1038,   715,   485,   574,
     711,   486,   570,   571,   575,     0,   847,  1076,   486,  1081,
    1073,   485,  1078,     0,   678,   670,   677,  1108,     0,   669,
     669,     0,     0,   485,     0,   862,   861,   857,   859,   860,
     858,     0,   852,   855,     0,    23,   485,    97,     0,   485,
     485,    92,   485,    99,     0,    33,    37,    38,    34,    35,
      36,   485,    95,    96,   485,   485,   485,     2,   106,   107,
       0,     0,   185,     0,     0,   610,     0,  1086,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,    64,    65,
      69,     0,     0,    69,     0,    93,    94,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     485,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     177,   178,   166,     0,   164,   165,   485,   991,   724,   988,
     847,   847,   996,   591,   485,   891,   847,   901,   909,   913,
     919,   485,   894,   896,   898,   485,   914,   916,     0,  1098,
    1101,   485,     0,     0,   485,   104,  1026,   847,  1110,   961,
     847,   847,  1110,   847,   976,   847,   847,     3,   711,     0,
       0,  1110,  1110,   485,   485,     0,     2,   742,     0,  1108,
     739,  1109,     0,   735,     0,     2,   738,   741,   182,   181,
       0,     2,   485,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   847,   878,   882,   921,
     847,   935,   939,   947,   847,   957,   870,   927,   950,     0,
       0,     0,  1003,     0,   462,   831,     0,     0,   463,   832,
     455,     0,     0,     0,     0,   453,     2,   833,     0,   438,
       2,   802,     0,   828,     2,   834,     0,     0,     0,     0,
     622,   887,   485,   905,     0,     0,   485,   418,   416,   103,
       3,   485,     0,     3,   803,     2,     0,   755,   485,   485,
     749,   748,   749,   543,   541,   671,   669,     0,   847,   847,
    1072,   485,  1077,   486,   485,  1063,     0,     0,     0,     0,
    1041,     0,   847,  1111,  1027,  1028,   716,  1024,  1025,  1039,
    1067,  1071,  1069,   572,   607,     0,  1075,  1080,   674,   669,
       0,   679,  1108,     0,   669,   782,   780,     0,     0,   854,
      69,   814,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   485,     0,   124,   123,     0,   120,
     119,    28,     0,    29,     0,     0,     0,     0,     3,    69,
       0,    52,     0,    53,    62,     0,    61,    73,     0,    70,
      71,    74,    55,    56,    57,     0,    54,    60,     0,     0,
      51,   126,   128,   129,   130,   132,   133,   135,   136,   140,
     141,   138,   139,   143,   144,   146,   148,   150,   152,   154,
       0,     0,   427,     0,     0,    30,     3,   733,   160,     0,
       0,     0,     0,   992,   993,   989,   990,   775,   774,   485,
     895,   897,   899,   485,   915,   917,     0,     0,  1017,  1016,
     485,     0,     0,     0,     0,     0,   847,  1027,   964,   981,
     485,   959,   967,   713,   962,   963,   714,   485,   974,   984,
     977,   978,     0,     3,  1110,   446,     2,  1103,     2,   704,
     705,   683,     3,     3,     3,     3,   727,     0,   158,     0,
       3,     3,     0,   736,     0,   730,     0,   847,     0,   485,
       3,   450,   457,   847,   879,   883,   847,   936,   940,   948,
     847,   958,   485,   871,   874,   876,   485,   928,   931,   933,
     485,   951,   953,   955,   845,     0,   465,  1004,     3,  1008,
    1009,     3,   836,   944,   556,   555,   558,   557,     2,   803,
     837,   784,   485,     2,   835,     0,   803,   838,   622,   622,
     622,   485,   698,   702,   701,   718,     0,   421,     0,     0,
     485,     0,   342,     0,     0,     0,     0,     0,   187,     0,
     337,   338,     0,     0,   342,     0,   390,   389,     0,   162,
     162,   396,   583,   589,   204,   485,     0,   188,     0,   215,
     189,   190,   485,   209,   191,   192,   193,   194,     0,   195,
     196,   343,     0,   357,   197,   363,   365,   368,   198,   199,
     200,   201,   202,     0,   203,   211,   566,   485,     0,   213,
       0,     0,     0,     3,     0,   816,   803,     0,   791,   792,
       0,     3,   787,     3,     3,     0,   485,   766,   766,  1108,
     669,   669,   669,  1074,  1079,     2,   103,   485,     3,   581,
       3,   486,  1035,   847,  1034,  1037,   485,     3,  1023,  1029,
     669,     0,   847,   847,     0,     0,   654,     0,   673,     0,
     669,  1108,     2,   851,   853,     0,    98,   485,   485,     0,
     102,   100,   485,     0,   114,     0,     0,     0,   118,   122,
     121,   186,     0,     0,     0,   733,   111,   179,     0,     0,
      46,    47,    87,     0,    87,    87,     0,    75,    77,    49,
       0,     0,    45,     0,    48,   157,     0,     0,     0,     0,
    1108,  1000,   847,   999,  1002,   994,     0,     0,   888,   906,
       0,   847,   970,   973,   847,     0,   847,   847,   965,   982,
       0,     0,  1104,     0,   706,   485,   485,     0,     0,     0,
       0,   435,     3,     0,     0,     0,     0,   732,   737,     3,
     829,   184,   183,     3,     0,   485,   872,   875,   877,   485,
     929,   932,   934,   485,   952,   954,   956,     0,     0,     0,
     727,   847,  1015,  1014,     0,     0,     0,     0,     0,     3,
     803,   839,     0,   485,   485,   485,   485,   485,   485,   485,
     605,   635,     3,     0,   636,   566,   623,     0,     0,     0,
     419,    69,     0,     0,   328,   329,   212,   214,     0,     0,
       0,   485,   485,   324,     0,   322,     0,     0,     0,   733,
       0,     0,     0,     0,   369,   485,     0,   163,     0,     0,
     397,     0,     0,     0,     0,     3,   219,     0,   210,     0,
     319,     0,     0,     0,   342,   342,   348,   347,   342,   359,
     358,   342,   342,     0,   566,   847,     0,  1019,  1018,     0,
       2,     0,   758,   794,     2,   789,     0,   790,     0,   770,
     750,   754,   752,     0,  1108,     0,     0,   485,     0,     0,
       0,     3,   485,  1030,  1032,  1033,     0,     0,   103,     0,
       3,  1108,   669,   663,   669,   679,   679,   733,   680,   655,
    1108,     0,   783,   485,   856,  1020,     0,     0,     0,    39,
       0,   115,   117,   116,   113,   112,   733,  1108,     0,    68,
      84,     0,    78,    85,    86,    63,     0,     0,     0,    72,
      59,     0,   156,   426,    31,     0,   485,   995,   997,   998,
     889,   907,   847,   485,   966,   968,   969,   485,   983,   985,
       0,   960,   975,   971,   986,  1105,     3,   691,   690,   694,
    1107,     2,     2,  1106,     0,     3,   844,   743,   744,     0,
       0,   847,     0,     0,     0,   880,   937,   945,   466,   846,
       0,  1010,     0,  1011,  1012,  1006,   820,     2,     0,   822,
     605,   605,   605,   636,   643,   610,     0,   649,   636,     0,
     485,   597,   634,   630,     0,     0,     0,     0,   637,   639,
     847,   651,   651,   651,     0,   631,   647,   422,     0,   332,
     333,   330,   331,     0,     0,   342,   229,     0,     0,   231,
     430,   230,   566,   485,   310,   309,   311,     0,   342,   187,
     269,     0,   262,     0,   187,   325,   323,     0,   317,  1108,
     326,     0,     0,     0,     0,   378,   379,   380,   381,     0,
     371,     0,   372,   334,     0,   335,     0,     0,   362,   485,
     220,   208,   321,   320,     0,     0,   351,   361,     0,   342,
     364,     0,   366,   388,     0,   420,   847,   485,   818,   749,
     771,   485,     2,     2,   661,     0,   669,   669,  1082,  1083,
    1084,     0,     0,     3,     3,     0,  1043,     0,     0,  1108,
     669,     0,   676,   675,  1108,     0,   658,     3,     0,  1021,
     101,     0,    32,   485,     0,  1108,     0,     0,    88,     0,
      76,     0,    82,     0,    80,    44,   161,     0,     0,     0,
       0,   485,   485,   746,     0,   443,   445,   881,   938,   946,
       0,     0,   786,   824,   601,   603,   599,     0,     0,  1050,
       0,   644,  1055,   646,  1047,   847,   847,   629,   650,   633,
       0,   632,     0,     0,     0,   653,     0,   625,   847,   624,
     640,   652,   641,   642,   648,   342,     0,     0,   250,   342,
     232,   566,   315,   313,   316,   312,     0,   314,     0,   258,
       0,   187,     0,   342,   485,   270,     0,   295,     0,     0,
     318,     0,     0,   342,   341,   342,   382,     0,   373,     2,
       0,     0,     0,     0,   344,     0,     0,   342,     0,   342,
     342,   342,   206,   205,   444,   766,   788,     0,     0,   662,
    1108,  1108,  1085,  1036,     0,     0,  1042,  1044,   659,     0,
       0,   669,     0,   657,     2,    50,    42,    40,    41,     0,
      66,   180,    79,     0,     0,  1001,   972,   987,   447,     2,
     688,     3,   687,   745,  1005,  1013,   627,     0,     0,     0,
    1051,  1052,   847,   628,  1048,  1049,   626,   606,     0,     0,
     340,     0,     0,     0,   243,   342,   221,     0,     0,   342,
     252,   267,   278,   272,   342,   187,   307,     0,   282,     0,
       0,   273,   271,   260,   263,     0,     0,   187,   296,     0,
       0,   224,   339,   370,     2,   485,   336,     0,     0,   398,
     342,   349,     0,    69,   360,   353,     0,   354,   352,   367,
     757,   793,   795,     0,     0,  1045,  1046,   660,   669,  1108,
     681,   785,    67,    83,    81,   485,     0,   847,  1058,  1060,
    1053,     0,   638,   238,   233,   236,     0,   235,   242,   241,
       0,   485,   245,   244,   342,   254,     0,   251,   342,     0,
       0,     0,   259,   264,     0,     0,   187,   308,   283,     0,
       0,   342,     0,   298,   299,   297,   266,   327,     0,   485,
     485,     3,   383,   486,   387,     0,   391,     0,     0,     0,
     399,   400,   227,   345,     0,     0,     0,   665,   667,  1108,
       0,   692,   485,  1054,  1056,  1057,   645,     0,   240,     0,
     239,   223,   246,   485,   411,   255,   342,   256,   253,   268,
     281,   279,   275,   287,   285,   286,   284,   265,   280,   276,
     277,   274,   261,     0,     0,     0,     0,   226,   246,     3,
     376,     0,  1050,   384,   385,   386,   398,     0,     0,     0,
     398,     0,   342,   350,   346,   342,     0,     0,   666,     0,
     234,   237,   342,     3,   247,   412,   257,     0,     0,     0,
       0,   306,   304,   301,   305,   302,   303,   300,     3,   376,
       0,     0,  1051,     0,     0,     0,   392,     0,   401,   228,
     355,   342,   664,  1059,   216,     0,     0,   342,   294,   292,
     289,   293,   290,   291,   288,     0,     0,   377,     0,   404,
       0,   402,     0,   404,   356,   218,   217,   222,     0,   225,
       0,   374,   405,     0,     0,   393,     0,   375,     0,     0,
       0,     0,   406,   407,     0,   403,   394,     0,     0,   395,
     408
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1803,  6573,  5522, -1803,    -1,   551,  2434,  -165, -1803,  -346,
   -1803,   334, -1803,  -692, -1803,   763,  -947,  -989, -1803,   251,
    5953,  2160, -1803,  1715, -1803,  1385,   148,   838,   847,   655,
     855,  1343,  1350,  1345,  1351,  1357, -1803,  -173,  -160,  9162,
     908, -1803,  1682, -1803, -1803,  -704,  4183, -1158,  3185, -1803,
    -100, -1803,   894,   -37, -1803, -1803, -1803,   414,    55, -1803,
   -1758, -1566,   275,    30, -1803, -1803,   664,   286, -1613, -1803,
   -1525, -1803, -1803, -1803, -1803,    75, -1115, -1803, -1803, -1208,
     416, -1803, -1803, -1803, -1803, -1803,   115, -1178, -1803, -1803,
   -1803, -1803, -1803,    -4,   436,   437,    99, -1803, -1803, -1803,
   -1803,  -784, -1803,    31,   -29, -1803,   106, -1803,  -158, -1803,
   -1803, -1803,   898,  -566,  -936, -1284, -1803,    10,   289,  1870,
    7123,  -928,  -890, -1803,   -60, -1803, -1803,   360, -1803,  -136,
    2762,  -326,  -244,  2957,  2779,  -642,     9,     3,    53,   567,
    2419, -1803,  2100, -1803,    74,  4822, -1803,  2039, -1803,   127,
   -1803, -1803,   333,   129,  5147,  4153,   -36,  1891,  -256, -1803,
   -1803, -1803, -1803, -1803,  -645,  8225,  7420, -1803,  -378,  -113,
   -1803,  -626,   228, -1803,   151,   729, -1803,   -34,  -560, -1803,
   -1803, -1803,  -342,  8635,  -261,  1190,    91,  -736,  -487,  -375,
     -98, -1803, -1327,  -122,  -186,   676,   917,  2925,    64,  -485,
    -260,  -180,  -475,  1328, -1803,  1662,   376,  -917,  1534, -1803,
   -1803,   674, -1803, -1215,  -171,   -43,  -901, -1803,   409, -1803,
   -1803, -1130,   447, -1803, -1803, -1803,  2167,  -743,  -512,  -972,
     -12, -1803, -1803, -1803, -1803, -1803, -1803,   264,  -831,  -224,
   -1802,   -59,  8077,   -57,  5872, -1803,  1200, -1803,  1009,   -84,
    -210,  -201,  -196,     4,   -73,   -68,   -67,   489,    40,    56,
      68,  -185,   -25,  -154,  -150,  -138,    23,  -125,  -103,   -92,
    -699,  -712,  -677,  -669,  -680,  -132,  -656, -1803, -1803,  -714,
    1394,  1395,  1396,  1820, -1803,   562,  6710, -1803,  -603,  -613,
    -588,  -576,  -758, -1803, -1493, -1717, -1701, -1672,  -627,  -102,
    -279, -1803, -1803,   -63,   183,   -48, -1803,  7452,   919,   706,
    -519
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1216,   224,   411,   412,    82,    83,   413,   387,   414,
    1548,  1549,   415,  1018,  1019,  1020,  1336,  1337,  1338,  1560,
     437,   417,   418,   419,   720,   721,   420,   421,   422,   423,
     424,   425,   426,   427,   428,   429,   430,   439,  1119,   722,
    1468,   783,   218,   785,   433,   850,  1217,  1218,  1219,  1220,
    1221,  1222,  1223,  2155,  1224,  1225,  1475,  1667,  2005,  2006,
    1934,  1935,  1936,  2123,  2124,  1226,  1681,  1682,  1683,  1837,
    1838,  1227,  1228,  1229,  1230,  1231,  1232,  1865,  1869,  1492,
    1484,  1233,  1234,  1491,  1485,  1235,  1236,  1237,  1238,  1239,
    1240,  1241,  1700,  2141,  1701,  1702,  2041,  1242,  1243,  1244,
    1471,  2049,  2050,  2051,  2183,  2194,  2072,  2073,   303,   304,
     916,   917,  1186,    85,    86,    87,    88,    89,  1670,    91,
      92,    93,    94,    95,    96,   232,   233,   306,   285,   472,
      98,   473,    99,   591,   101,   102,   155,   352,   309,   106,
     107,   170,   108,   934,   353,   156,   111,   256,   112,   157,
     264,   355,   356,   357,   158,   434,   117,   118,   359,   119,
     587,   909,   907,   908,  1641,   120,   121,   122,   123,  1181,
    1436,  1647,  1648,  1800,  1801,  1437,  1636,  1820,  1649,   124,
     680,  1750,   676,   125,   677,   678,  1298,  1112,   478,   479,
     946,   593,   480,   481,   594,   595,   596,  1248,   443,   444,
     219,   498,   499,   500,   501,   502,   340,  1267,   341,   932,
     930,   626,   342,   381,   343,   344,   445,   126,   176,   177,
     127,  1261,  1262,  1263,  1264,     2,  1168,  1169,   617,  1255,
     128,   330,   331,   266,   277,   570,   129,   222,   130,   321,
    1121,   899,   532,   168,   131,   691,   692,   693,   132,   323,
     236,   237,   238,   324,   134,   135,   136,   137,   138,   139,
     140,   241,   325,   243,   244,   245,   326,   247,   248,   249,
     818,   819,   820,   821,   822,   250,   824,   825,   826,   788,
     789,   790,   791,   533,  1161,  1415,   141,  1758,   651,   652,
     653,   654,   655,   656,  1803,  1804,  1805,  1806,   641,   483,
     367,   368,   369,   446,   210,   143,   144,   145,   371,   842,
     657
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      81,   510,   194,    81,   104,   133,   382,   195,   196,   431,
     103,   567,   553,   235,   151,  1268,   386,   482,   192,   839,
     514,   209,   432,   636,  1007,  1488,   725,   550,  1265,   515,
     350,   961,   947,   299,   516,   378,   681,  1473,   504,   667,
    1460,   201,  1419,   670,   363,   517,   955,   180,  1597,  1598,
     896,  1029,  1511,  1512,   105,    81,    81,   948,    81,  1092,
     731,   904,   104,   133,   366,  1099,   892,   894,   103,   949,
    1704,  1849,   242,  1064,    81,   109,   518,   305,  1341,  1000,
     519,  1917,   539,    81,  1002,  1249,   669,   209,   207,  2008,
     672,    81,   520,   313,  2021,   659,    81,  1918,  1115,    81,
    1472,   239,  1082,    81,   267,   521,   925,  1348,   278,   514,
     564,  1203,   105,  2014,   271,   197,  1130,  1088,   515,   449,
     246,   575,   507,   516,   450,   451,  1919,   522,   113,  2007,
     114,   198,   956,   109,   517,   316,  1089,  1083,   523,   220,
     308,    81,   636,   199,    81,  1084,    81,   525,    58,  1705,
     104,   133,    81,   262,   465,  1842,   103,   194,  1085,    81,
     765,  1176,   195,   196,  -796,   518,    81,  1669,   220,   519,
    1382,   385,   146,   512,   530,  1719,   535,  2013,  -797,    58,
     305,   520,  1256,   543,   142,  1253,   113,   142,   114,  1706,
      81,    81,   159,  2076,   521,   207,   559,  1418,   976,   592,
     105,   947,   766,   599,  1422,    81,   526,  1817,   305,  2047,
     486,   109,  -828,   955,  1818,   495,   522,  2015,   659,   305,
      81,   109,   235,   160,    81,    81,   948,   523,  1950,    81,
      81,   275,   452,  1819,   531,   207,   525,   165,   949,   221,
     923,  -828,   142,   308,   600,  1431,   194,   642,   453,  2009,
      81,   195,   196,  1432,   527,   606,   608,   582,    81,  1707,
     454,   166,   611,   207,   113,   559,   114,  2007,    81,    81,
     197,   308,    81,    20,   113,   884,   114,   571,  1294,    81,
     529,   242,   308,   888,  1501,   805,   198,   142,  1305,   531,
      90,  1433,  1486,    81,    81,   526,    81,  2077,   199,  1093,
     630,    81,   856,  1096,   724,    81,  1921,   308,   940,  1068,
     602,   857,  1109,  1110,   109,  1487,   858,  1329,    81,    81,
     843,  1955,  1956,  1092,   207,   890,   569,   859,    81,   603,
     142,   895,   960,   607,  1489,   659,    81,    81,   973,   630,
     823,    81,   109,   527,  1562,   966,  1113,  1113,    90,  1355,
    1245,  1289,   983,   109,  1567,   942,  1320,  1490,   860,  1447,
     262,    97,   861,  1027,  2013,  1113,   642,   113,  1082,   114,
     179,   482,   607,  1718,   862,   209,  1472,  1721,   109,    81,
    2068,   659,    81,   690,  1917,  1871,  1568,   863,  1368,  1173,
      58,  1249,   967,  1393,  1383,   113,   648,   114,   181,  2013,
    1918,   856,  1483,  1083,   262,   659,   113,  1369,   114,   864,
     857,  1084,   659,  1757,   903,   858,   810,   673,   972,    97,
     865, -1109,  2024,  2025,  1360,  1669,   859,  2000,  1384,  1919,
    1420,   113,   674,   114,  1113,   275,    90,   675,   885,   449,
     876,  1486,   887,  1002,   450,   451,   889,    81,  1628,   482,
     182,   215,   486,  2030,  1597,  1598,   201,   860,  1114,  1114,
    2067,   861,   216,   897,  1487,  1931,  1932,   947,   726,   293,
      81,    81,   297,   862,   905,    58,   531,  1114,   217,   350,
    1442,  1443,    81,    81,  1289,  1708,   863,  1431,  1431,  1431,
    1297,    81,   948,   495,  1384,  1432,  1432,  1432,  1412,   877,
     298,   939,  1867,   538,   949,    58,  2122,    97,   864,   190,
     546,    81,   305,   366,  1002,    58,   592,  1659,  1482,   865,
    1413,  1840,   190,    81,  2094,  1524,  1848,   262,   486,   876,
    -602,   563,  2122,  1433,  1433,  1433,   476,  1868,   449,  2069,
    2070,  1473,   574,   450,   451,   298,  1114,   878,    81,  2104,
    1933,    58,   452,  1439,    81,   149,   642,   540,  2157,    58,
     202,   531,  1661,    14,    15,    16,    17,    18,   453,   732,
     623,   674,  1440,   208,   733,   308,   675,  1770,  1772,  1774,
     454,   298,  1350,  -979,   482,  1374,   240,   614,   877,   268,
    -979,   531,    58,   279,   200,    64,   724,   801,  1444,   624,
     625,   531,   724,  1053,  1472,   924,  1589,   174,   174,  1921,
      81,   724,    81,  2115,  1769,  1273,    81,   486,    81,   923,
     104,   133,    58,   213,  1446,   482,   103,    81,  1203,   188,
     724,    81,    81,  1069,  1931,  1932,   878,   531,   944,  1571,
      -3,  1090,  1113,   174,   262,   646,   109,   482,   482,  2000,
     961,    63,    64,  1002,   431,  1154,  1245,   569,   225,    58,
    1980,  1123,  1274,  2096,    81,  1118,   482,  1103,  2061,    58,
     105,    58,   281,   225,  1097,  1269,   282,    81,   646,   286,
     208,   291,   639,  1949,   823,   662,  -483,   659,  1059,   576,
     627,   109,  1643,   174,   628,   487,   174,  1654,   639,   113,
      77,   114,   639,  1076,   588,   631,   293,  1077,  1832,  1833,
    1834,   174,  1439,  1301,   569,  1155,  1655,   270,   376,  1960,
     208,   161,    58,    58,   162,   163,  1183,   164,  1808,   226,
    1835,  1725,    81,   582,    81,   482,    81,    58,   227,  1812,
      81,  1142,  1654,    81,   113,   531,   114,  1809,   208,  1658,
      58,  1146,  1002,  1150,  1114,   531,   682,   531,  1692,   684,
     982,  1811,   572,   985,   986,   734,   987,   251,    81,  2056,
     735,  1850,   174,    58,  1818,   989,  -479,   511,   991,   992,
     993,  1510,   293,    58,    58,    58,  1162,  2023,    14,    15,
      16,    17,    18,  1916,  1832,  1833,  1834,  1002,  1170,  2036,
     142,   639,  1174,   886,  1395,  1399,  1177,  2128,   531,   531,
      58,  -656,  1875,    81,   944,    81,  1835,  1441,  -656,  1403,
     174,   174,  1922,   531,  1903,  1836,  1904,    81,   898,   281,
    1818,   174,  1522,  1293,    81,   902,   646,   725,   297,   906,
     495,  1923,   364,    81,  1344,   585,   174,    58,   590,  1926,
    1557,  1340,    81,   149,  1767,  1576,  1381,   295,   350,   531,
     298,   995,  1002,   755,   756,  1583,  1587,  2062,  2087,   531,
     646,   531,   996,   997,  1002,   748,   281,  1345,    81,   364,
     174,  2019,   749,   750,  2130,  1668,  1684,  1324,   174,   174,
     476,  -480,   366,   174,  1325,  1843,  1032,  1033,  1034,  1684,
    1844,    14,    15,    16,    17,    18,    90,   757,   758,   252,
     253,  -484,   254,    81,    81,   495,   190,   255,   317,  1247,
     282,  1559,   663,  -721,   291,   103,    74,  -817,  1340,  1515,
    1516,   174,   190,   923,   174,   767,   380,   487,  1258,   768,
    -980,    74,  1759,   482,  1388,   793,   645,  -980,   476,   794,
     646,   960,   913,  1118,  1367,   823,   914,    79,   647,   975,
      58,   786,   709,   628,    81,   531,   338,   639,   476,   105,
     648,  1356,    79,    80,   297,  1357,   455,    97,   690,    14,
      15,    16,    17,    18,  1689,  1794,  1795,  1796,  1855,  1968,
     109,   639,  1370,  1844,  1969,   385,   262,  2109,   383,  1371,
    1596,  2172,  2110,   212,   639,  -481,  2173,  1797,   914,  1514,
     298,   384,  1810,   487,    81,    14,    15,    16,    17,    18,
     456,   455,   174,   531,    81,   659,   485,   844,   845,  1531,
     685,   846,   457,   977,   174,   174,  1503,   628,    58,  1540,
      74,   458,  1526,   113,  1407,   114,   251,   459,  1408,   460,
    1635,   161,  1409,    81,   162,   163,   495,   164,   709,   297,
     645,   455,  1754,   531,   646,  1423,  1424,  1425,   461,   212,
      74,    79,   647,   490,    58,  1421,  1832,  1833,  1834,    81,
     489,  1765,   509,   978,   193,    81,    81,   979,  1002,  1445,
    1798,  1821,  1821,  1821,   531,  1410,   382,   382,  1835,   142,
    1001,    79,    80,   476,  1002,   686,   234,  1841,  1466,   447,
     540,   142,   869,   503,   531,   724,    81,   614,  1438,   455,
     528,   531,   505,   687,   431,   688,   689,    65,    66,    67,
      68,    69,    70,    71,    72,  1073,   281,  1538,   508,   531,
     529,  1127,   923,  1668,   476,  1128,   581,    64,   551,   148,
    1618,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,  1090,   322,   455,   297,   646,   552,  1175,   531,   220,
     174,  1545,   350,  1572,   597,   642,   540,    74,   560,   174,
     531,  1164,  1258,   935,   938,  1002,   751,   752,   562,   482,
     482,   495,  1751,  1247,    81,    81,    81,  1644,    76,   103,
     619,   753,   754,   495,  1645,    90,   366,   573,    79,    80,
     431,   431,   601,  1166,   614,  1339,   964,  1002,   531,  1340,
     618,   495,  1509,  1605,  1606,  1247,   794,    81,  1544,   174,
     634,   103,  1340,   364,   322,   666,  1832,  1833,  1834,   513,
     234,   683,    81,   105,   679,    81,    81,   560,  1002,    81,
     267,   278,   759,   760,  1599,   675,    81,   271,  1835,    81,
     322,  1762,  1825,   694,   109,  1763,  1340,  -188,   644,  1826,
     698,  1749,   695,  1002,   639,   105,    97,   662,  1684,   726,
     699,  2054,   700,  1852,  1011,  1853,  1013,  1002,  1016,  1128,
     704,   364,  1026,   728,    81,  1030,   109,  1612,  1854,   262,
    1927,  1613,  1002,  1974,   794,  1614,  2016,  1002,    81,  2113,
    1002,   364,  2114,  1340,  2191,   322,  1002,   113,  2188,   114,
    1055,  2197,  2143,   761,   495,  2198,  2147,   476,   612,   322,
     202,   728,    81,   747,   569,  -124,  -124,  -124,  -124,  -124,
    -124,   762,  1660,  1662,  1695,  1696,  1697,  1698,  1699,   113,
    1651,   114,   634,   728,   763,   142,   764,   212,    58,   769,
    1438,  1438,  1438,   795,    81,  1637,  1438,  -123,  -123,  -123,
    -123,  -123,  -123,   142,   275,   796,  1550,  1004,  1005,   174,
     797,   350,   798,   970,  1105,  1106,   174,   644,  1107,  1108,
     799,  1671,   148,  1723,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,  1131,   142,  1911,   800,  1890,  1652,
    1039,  1040,  1041,  1042,   462,   366,  1327,  1128,  1342,  1343,
      74,  1002,  1346,    -3,  1742,  -159,  -159,  1482,  1483,   827,
     142,  1107,  1500,    81,   514,  -482,   151,    81,    81,   -18,
    2101,    76,   840,   515,   531,  2074,   364,   841,   516,  1565,
    1566,    79,    80,   854,  1673,  1673,   851,  1653,   495,   517,
     103,   103,  1570,  1566,   866,  1185,   174,   174,  1673,   867,
     447,   447,   868,  2074,   103,  1880,  1881,   870,  1777,    90,
     871,   817,   872,   495,   495,  1778,   873,   364,   880,  1779,
     518,  1574,  1566,    81,   519,   874,  1079,  1558,   571,  1607,
    1558,  1079,  1620,   875,   105,   105,   520,  2125,   882,   364,
     364,    90,  1775,  1128,  1901,  1128,  1292,   310,   105,   521,
     900,   855,  1902,  1566,   881,   109,   109,   901,   364,  1908,
    1909,  1914,  1002,   234,  1972,  1973,   495,  1993,  1566,   109,
     912,   522,  1862,  1994,  1566,  1122,   911,   569,  -600,  1651,
      97,  -598,   523,  1258,  1651,   495,  1931,  1932,   322,   910,
      81,   915,   525,   927,   322,    81,    81,    81,   183,     6,
       7,     8,     9,    10,    11,    12,    13,   962,   113,   113,
     114,   114,    97,  2188,  2189,  1563,  1564,  1813,  1184,  1035,
    1036,   350,   113,   918,   114,   482,   482,   364,   929,  1856,
    1037,  1038,   447,   856,   933,  1599,  1720,  1722,  1652,   142,
     639,   950,   857,  1652,  1043,  1044,   952,   858,  1822,  1823,
     922,   526,   322,   648,   969,   366,  1752,  1753,   859,   280,
    1989,   974,    81,   981,   142,   142,   980,    81,  1009,  2042,
    1003,   476,  1006,    81,  1052,    81,  1078,  1057,   142,  1671,
    1086,  1079,  1125,    81,  1982,  1156,  1653,  1133,  1134,   860,
    1599,  1653,  1135,   861,   495,  1136,  1137,  1138,  1139,   527,
     364,  1140,  1141,  1163,  1165,   862,  1167,   495,  1299,  1250,
    -800,  1171,  1266,   271,  1178,  1179,  1180,  1257,   863,    14,
      15,    16,    17,    18,   999,  1270,  1967,  1282,  1283,   736,
    1284,   737,   738,   739,   382,  1285,  1296,  1297,  1300,  1302,
     864,  1303,  1673,   447,  2042,  1306,  1307,  1309,   103,  1310,
    1312,   865,  1311,   495,  1313,   262,   142,  2059,  1315,  1316,
     740,  1317,   876,   741,   742,   268,   279,  1322,   743,   744,
    1323,  1258,  1330,   174,  1331,  1347,   174,   174,   174,    14,
      15,    16,    17,    18,  1319,   174,  1351,  1352,   431,  1353,
    1354,  1358,   105,  1359,  1361,  1372,  1362,  1363,  1364,  -685,
      81,  2004,    81,   174,    14,    15,    16,    17,    18,   174,
    -684,  1387,  1411,   109,  -801,  1416,  1470,   659,  1448,  1451,
    1452,   877,  1461,  1462,   590,  1463,  1465,  1474,  -720,  1476,
     275,   174,  1505,  1002,  2048,   364,  1495,  1497,   174,   482,
    1498,  1532,    81,  1539,  1542,    81,  1556,  1558,  2103,  1507,
    1580,  1581,  1582,  1081,   495,   817,  1573,  1550,   495,  1593,
    1594,  1595,  1626,    58,  1600,  1601,   113,  1602,   114,   878,
    1603,  1566,   495,  1611,  1608,  1651,   174,  1673,   447,  1615,
     476,  1616,   495,   103,   495,  1617,   142,  1625,  1629,  1642,
    1546,  1640,  1441,  1664,  1709,  1685,   495,  1686,   495,   495,
     495,  1688,  1483,  1599,   152,  1690,    81,    81,  1711,  1703,
    1712,  1713,  1714,  1715,   569,  1203,  1726,  1728,   322,  1736,
    1730,   514,   142,  1732,   431,    74,   431,   105,  1733,  1734,
     515,  1737,  2044,  1738,  1652,   516,  1739,  2120,  1740,  2004,
    1746,  1756,  1760,  1761,   142,   645,   517,  1768,   109,   646,
    1764,  1776,   455,  1783,  1607,  1785,    79,   647,    81,  1787,
    1788,  1789,  1792,  1807,   495,   431,  1827,  1793,   495,  1645,
    1879,  2048,  1829,   495,  1859,  2048,  2048,   518,  2145,   221,
    1861,   519,  1653,  1888,  1883,  1891,  1882,    14,    15,    16,
      17,    18,  1886,   520,  2119,  1887,  1893,  2167,  1895,   495,
     260,   113,  1900,   114,   272,  1513,   521,  2044,  1905,  2170,
    1906,  1907,  1913,   572,   364,  1939,  1944,  1945,  1957,   174,
     174,  1959,  1970,  1964,  1966,  1976,  1971,  1987,   522,  1988,
    1990,  1985,  1986,  1991,  2182,  1992,   531,  1541,  2182,   523,
    -686,   431,  1997,   495,  1673,  1998,    58,   495,   525,  1999,
     103,  2018,  -583,  2192,  2190,  2020,  2031,   142,   194,  2045,
     495,  2029,  2026,   195,   196,   174,   174,  2037,  2057,  2046,
    2058,    81,  1673,    81,   611,  2071,  1909,  2093,   103,  2080,
    2095,   364,   364,  2097,  2106,  2107,  1575,  2108,   876,  2111,
    2112,   542,  2118,  2131,   105,  2127,  2129,  2140,  2181,  2146,
    2151,  2144,  2152,   470,  2153,   495,  1673,   526,    74,  2158,
    2187,  2168,   103,  2169,  2171,   109,  2177,  2179,  2180,  1081,
    2184,  2195,   105,  2185,  2196,  1366,   817,  1897,   786,  2199,
      81,    81,   531,  1569,  1045,   998,   207,   701,  1047,    79,
      80,   495,  1046,   109,   495,  1048,  1478,   877,  1469,   784,
    2178,   495,  1049,  1863,  2121,   527,   105,  1961,  2138,  1694,
    1954,  2116,   745,   746,  1870,  2166,  1857,  1858,   113,  2099,
     114,    81,  2148,   568,  2186,  2098,   486,   109,  1496,   171,
     495,   288,  2066,   745,   495,   561,   495,  2002,  1639,  1295,
    1493,    84,   931,   847,   150,   878,   113,     3,   114,  1124,
      58,    14,    15,    16,    17,    18,  1729,   495,  1876,  1304,
    1060,  1061,  1062,   745,  1791,   447,     0,     0,    81,     0,
       0,     0,     0,   364,   142,     0,   962,    81,     0,     0,
     113,     0,   114,     0,   148,   174,   228,   229,    65,    66,
      67,    68,    69,    70,    71,    72,   174,     0,     0,    84,
    1735,     0,   142,     0,     0,     0,     0,     0,     0,   174,
      58,     0,     0,     0,     0,   191,     0,  1748,     0,     0,
       0,     0,     0,     0,    84,     0,  1755,     0,     0,     0,
       0,     0,  1365,    76,     0,     0,   142,   231,     0,     0,
     259,   792,     0,  1766,    84,     0,     0,     0,     0,   322,
       0,     0,     0,     0,     0,   174,     0,   803,     0,   148,
     806,   172,   173,    65,    66,    67,    68,    69,    70,    71,
      72,     0,    74,     0,     0,     0,     0,     0,   639,     0,
       0,   150,     0,     0,     0,     0,     0,    84,  1479,     0,
     150,     0,  1798,   320,   328,     0,   531,     0,     0,     0,
       0,     0,     0,    79,    80,     0,   349,     0,     0,     0,
       0,     0,     0,     0,     0,   621,   148,   542,   172,   173,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
     438,   191,   191,     0,    19,     0,     0,     0,     0,     0,
       0,     0,   150,   468,     0,     0,   259,     0,     0,     0,
       0,     0,     0,   639,  1590,     0,   174,     0,     0,     0,
     174,     0,     0,     0,     0,   320,     0,     0,     0,     0,
     231,   231,     0,     0,   174,  1851,     0,    52,    53,    54,
      55,     0,     0,   364,   174,     0,   174,     0,     0,  1480,
       0,   320,     0,     0,     0,     0,     0,     0,   174,    84,
     174,   174,   174,     0,     0,     0,     0,   470,   174,     0,
     148,     0,     0,   259,    65,    66,    67,    68,    69,    70,
      71,    72,   148,  1650,  1022,  1023,    65,    66,    67,    68,
      69,    70,    71,    72,  1024,  1889,     0,   364,   364,     0,
    1892,     0,  1031,     0,     0,     0,   320,     0,     0,     0,
       0,  1899,   328,     0,     0,     0,     0,     0,   328,   320,
     320,   189,     0,     0,     0,     0,   174,     0,   150,     0,
     174,     0,   332,     0,  1025,   174,     0,     0,     0,     0,
     333,   334,   335,   336,     0,   470,     0,     0,     0,   349,
     649,   658,     0,     0,     0,     0,     0,     0,     0,   263,
       0,   174,     0,     0,     0,   349,     0,     0,   148,   349,
     284,   287,    65,    66,    67,    68,    69,    70,    71,    72,
    1014,   148,     0,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,   447,
       0,     0,     0,     0,   438,   174,     0,     0,     0,   174,
       0,     0,     0,   263,     0,     0,     0,     0,     0,     0,
    1015,     0,   174,     0,     0,     0,  1983,  1984,     0,     0,
       0,     0,     0,   337,     0,  2105,     0,     0,   438,     0,
       0,   787,     0,     0,     0,     0,   792,   792,   191,     0,
       0,   338,     0,     0,     0,     0,  1071,     0,     0,  1074,
       0,     0,     0,     0,   150,   263,     0,   174,   468,     0,
       0,     0,   816,     0,   658,     0,     0,     0,     0,     0,
     470,     0,  1650,     0,     0,     0,     0,  1650,     0,     0,
     496,     0,     0,  1814,     0,  1650,     0,     0,     0,     0,
       0,     0,     0,   174,     0,     0,   174,     0,     0,     0,
       0,   364,   231,   174,     0,     0,     0,     0,     0,   542,
       0,   470,     0,     0,   231,     0,  1144,     0,     0,     0,
    1148,     0,   263,     0,  1152,  2060,     0,     0,     0,     0,
       0,     0,   174,   470,   470,  1314,   174,     0,   174,   320,
    1318,   438,   438,     0,     0,   320,     0,   349,    58,     0,
       0,  1326,   470,     0,     0,     0,   263,     0,     0,   174,
       0,   263,     0,     0,     0,     0,     0,   263,     0,     0,
    2193,     0,     0,     0,    58,     0,     0,     0,     0,  2200,
       0,     0,   148,     0,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,  2117,   153,     0,     0,     0,
     263,   320,     0,   320,     0,   349,     0,    84,   148,     0,
      74,     0,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   470,     0,     0,   349,   468,     0,   658,  1259,     0,
     230,    76,     0,     0,     0,   649,    74,     0,    19,   649,
       0,    79,    80,     0,   710,     0,     0,     0,   349,     0,
       0,     0,  1928,     0,     0,  1650,    75,    76,   658,     0,
       0,   349,     0,     0,     0,     0,     0,    79,    80,     0,
       0,   187,     0,   150,     0,     0,   205,    48,    49,    50,
      51,    52,    53,    54,    55,     0,   438,     0,     0,   150,
     150,     0,   438,     0,     0,     0,     0,     0,     0,     0,
       0,   438,     0,     0,   150,   150,   150,     0,     0,   261,
       0,     0,     0,   792,     0,     0,     0,   322,     0,     0,
     283,   148,   290,   263,   292,    65,    66,    67,    68,    69,
      70,    71,    72,   311,     0,   701,     0,     0,     0,     0,
     710,   148,   205,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,   837,     0,   496,     0,
     468,     0,     0,   261,     0,     0,   290,   292,  1650,     0,
      76,     0,     0,   836,     0,     0,   787,   787,     0,     0,
       0,     0,   441,  1397,   438,     0,  1401,     0,   100,     0,
    1405,   154,     0,     0,   463,     0,     0,     0,     0,     0,
       0,   468,     0,     0,   816,   263,   816,     0,     0,     0,
       0,   175,   178,     0,  1453,   261,     0,     0,     0,     0,
       0,     0,   745,   349,   349,     0,   263,     0,   148,   470,
     172,   173,    65,    66,    67,    68,    69,    70,    71,    72,
     263,     0,   349,     0,     0,     0,   100,   223,     0,   555,
       0,   558,     0,   263,     0,     0,     0,     0,     0,     0,
    1551,  1552,  1553,     0,     0,     0,     0,  1554,  1555,   320,
       0,   206,  1259,     0,     0,     0,     0,     0,     0,     0,
       0,   322,   261,     0,   290,   292,   263,     0,     0,     0,
       0,   273,     0,     0,   153,     0,     0,   314,     0,     0,
     315,  1455,     0,     0,     0,     0,   438,     0,     0,     0,
     263,   349,     0,     0,     0,   339,   261,   263,   150,   438,
     558,   261,     0,     0,   307,     0,     0,   261,   312,     0,
       0,   349,     0,  1277,   100,     0,     0,   318,     0,     0,
     612,   322,     0,     0,   649,     0,   260,   272,     0,     0,
       0,     0,     0,   351,     0,     0,     0,     0,     0,     0,
     261,     0,     0,     0,   148,   664,     0,   292,    65,    66,
      67,    68,    69,    70,    71,    72,   506,   318,   448,     0,
       0,   322,     0,     0,   468,     0,     0,     0,     0,   312,
     474,     0,     0,     0,     0,     0,   441,     0,     0,     0,
       0,     0,  1578,     0,   708,     0,     0,     0,     0,     0,
       0,  1585,  1365,    76,     0,     0,     0,     0,   524,     0,
       0,     0,     0,     0,   565,   566,     0,   307,     0,     0,
     205,     0,     0,     0,     0,   175,     0,     0,   549,     0,
       0,     0,     0,   554,   556,     0,   206,     0,     0,     0,
     175,     0,     0,   787,     0,   307,   809,   261,     0,     0,
       0,     0,     0,     0,     0,     0,   307,     0,     0,   577,
     816,     0,     0,   579,     0,   470,   470,   816,   580,     0,
       0,     0,     0,   261,   616,   664,   292,     0,     0,   556,
       0,   307,   620,   622,     0,   604,     0,   629,     0,     0,
     708,     0,     0,     0,     0,   496,     0,   613,   837,     0,
       0,     0,     0,     0,     0,   318,     0,     0,     0,   349,
       0,     0,     0,     0,     0,     0,   152,     0,     0,     0,
       0,     0,  1159,   261,     0,   339,   637,     0,   339,   661,
       0,     0,     0,   441,   441,     0,     0,     0,     0,     0,
       0,     0,   668,     0,     0,     0,   668,     0,   261,     0,
       0,     0,   150,   261,     0,   261,     0,     0,     0,     0,
       0,   150,     0,     0,     0,     0,     0,     0,     0,     0,
     438,     0,     0,     0,     0,     0,   261,     0,   261,   261,
       0,   318,   263,     0,   568,     0,     0,     0,     0,     0,
     261,     0,     0,     0,     0,   438,     0,     0,     0,     0,
       0,     0,   438,   261,     0,     0,     0,     0,     0,     0,
       0,     0,   261,     0,     0,   318,   223,    14,    15,    16,
      17,    18,     0,     0,     0,     0,   259,    84,   831,   832,
       0,     0,     0,  1259,     0,     0,   261,     0,   664,   292,
     320,   312,     0,     0,     0,   637,   150,     0,     0,  1864,
       0,     0,     0,     0,     0,     0,     0,   468,     0,     0,
     261,   664,     0,     0,     0,   441,     0,   261,     0,  1335,
       0,     0,     0,     0,  1802,     0,    58,     0,   441,  1335,
       0,   441,   441,     0,   441,     0,     0,     0,   468,     0,
       0,     0,   150,   441,     0,     0,   441,   441,   441,     0,
       0,     0,     0,    58,     0,     0,     0,     0,  1335,     0,
     148,   496,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,   318,   318,
       0,     0,     0,     0,   474,     0,     0,   148,    74,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,   307,
       0,     0,     0,     0,     0,   349,   349,     0,   230,    76,
       0,     0,   260,   272,   926,    74,     0,     0,     0,    79,
      80,  1335,     0,   339,     0,     0,   441,     0,     0,     0,
       0,     0,     0,     0,     0,   319,    76,     0,     0,     0,
       0,     0,   351,     0,   100,     0,    79,    80,     0,     0,
       0,     0,     0,   150,   150,   150,   150,     0,   150,   150,
       0,   668,   943,     0,  1646,   328,     0,     0,     0,     0,
       0,  1259,     0,   971,     0,     0,   954,     0,     0,     0,
       0,   438,   438,     0,     0,   637,     0,     0,  1802,  1802,
     963,     0,     0,     0,     0,   438,     0,   148,   668,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,   148,
     318,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,   470,   470,   318,   259,     0,   318,   318,     0,   318,
       0,     0,     0,     0,     0,   263,     0,     0,   318,     0,
       0,   318,   318,   318,     0,     0,     0,   468,    14,    15,
      16,    17,    18,     0,     0,     0,  1457,  2053,     0,     0,
    1260,   441,     0,     0,     0,   936,     0,     0,     0,     0,
     263,   568,   937,   150,     0,   649,   148,     0,   372,   373,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,   261,     0,     0,     0,     0,   474,     0,     0,
       0,     0,     0,   261,     0,     0,     0,    58,     0,     0,
    1802,     0,   261,     0,  1063,     0,     0,     0,     0,     0,
       0,   318,     0,  1104,     0,     0,     0,    77,     0,   496,
    1116,     0,   374,     0,     0,     0,     0,  1335,   943,   375,
       0,   148,     0,  1087,     0,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
     474,   474,     0,  1646,  1799,     0,     0,     0,  1646,    74,
     438,     0,     0,     0,  1646,     0,  1646,     0,     0,   474,
       0,     0,     0,     0,     0,     0,     0,  2064,     0,    75,
      76,  1802,     0,     0,     0,     0,     0,     0,     0,     0,
      79,    80,   328,   150,     0,  1623,     0,     0,     0,     0,
    1187,   148,   261,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   263,     0,     0,     0,     0,     0,
       0,     0,  1802,     0,     0,   470,   261,     0,     0,   438,
       0,     0,     0,  1246,     0,     0,     0,     0,   474,     0,
       0,     0,     0,     0,     0,   154,   318,   349,     0,     0,
       0,   150,     0,   496,     0,     0,     0,  1290,   668,     0,
       0,  1281,     0,     0,  1291,     0,     0,     0,  1287,     0,
       0,     0,     0,   263,     0,     0,    14,    15,    16,    17,
      18,  1802,  1802,   150,   183,     6,     7,     8,     9,    10,
      11,    12,    13,     0,  1260,     0,     0,     0,     0,     0,
       0,   349,   349,  1434,     0,     0,    14,    15,    16,    17,
      18,   351,     0,     0,     0,     0,     0,     0,  1799,  1799,
       0,     0,  1802,     0,     0,     0,     0,     0,     0,     0,
       0,   496,     0,  1646,     0,    58,  1646,   441,   148,     0,
     200,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     496,   328,     0,     0,     0,  1335,     0,     0,     0,     0,
    1335,  1335,  1335,     0,   438,    58,     0,     0,     0,   148,
       0,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,   261,     0,    76,   441,     0,
     836,     0,     0,     0,     0,     0,     0,    74,   320,   148,
       0,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,   814,    76,     0,
     261,   646,     0,     0,     0,     0,   261,    74,    79,   815,
       0,     0,     0,     0,   441,     0,     0,     0,     0,     0,
    1799,     0,     0,     0,     0,     0,   474,   319,    76,  1646,
       0,   263,     0,     0,     0,     0,     0,     0,    79,    80,
       0,     0,     0,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
    1454,  1456,  1458,     0,     0,   150,     0,     0,     0,   154,
       0,    74,     0,     0,     0,     0,     0,     0,  1435,     0,
       0,     0,     0,     0,     0,     0,     0,  1246,     0,     0,
       0,  1644,    76,  1481,   116,   349,     0,   116,  1645,     0,
       0,  1799,    79,    80,     0,     0,     0,     0,     0,     0,
       0,   150,   318,     0,     0,  1187,     0,     0,     0,  1246,
       0,     0,  1502,     0,     0,  1434,  1434,  1434,   153,  1633,
    1634,  1638,     0,     0,     0,     0,     0,     0,     0,   150,
     150,     0,  2102,   328,  1494,  1335,     0,  1335,     0,     0,
       0,     0,   116,     0,   261,     0,     0,     0,     0,     0,
    1536,     0,     0,   318,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,   637,     0,     0,   116,     0,     0,
       0,     0,    58,   554,     0,     0,     0,     0,     0,     0,
     263,     0,     0,   265,     0,     0,     0,   116,     0,     0,
       0,  2102,  2102,     0,     0,   351,     0,     0,     0,   318,
       0,     0,     0,   261,     0,     0,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
     116,     0,     0,     0,   116,     0,     0,     0,     0,     0,
     116,     0,  2102,   116,    74,  1260,   148,   265,   581,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   345,   116,
     377,     0,     0,     0,  1644,    76,     0,     0,     0,     0,
       0,     0,   474,   474,     0,    79,    80,     0,    14,    15,
      16,    17,    18,   442,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,   442,     0,  1054,   265,
       0,     0,     0,  1656,  1657,   771,   772,   773,   774,   775,
     776,   777,   778,   779,   780,   781,     0,  1450,     0,   215,
    1435,  1435,  1435,   154,   556,   318,   318,     0,     0,  1464,
       0,     0,     0,   116,     0,     0,     0,    58,     0,     0,
       0,     0,   441,     0,     0,     0,   782,     0,  1672,  1672,
     116,     0,   116,    14,    15,    16,    17,    18,     0,     0,
       0,   116,  1672,     0,     0,     0,   265,     0,     0,     0,
       0,   148,   116,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,   586,     0,     0,
     116,   261,     0,     0,     0,   116,     0,   116,     0,    74,
     265,   116,   263,     0,     0,   265,     0,     0,     0,     0,
       0,   265,    58,     0,   351,     0,     0,     0,     0,  2101,
      76,   116,     0,   531,     0,     0,     0,     0,     0,     0,
      79,    80,     0,  1260,     0,     0,     0,     0,     0,     0,
     154,     0,   116,     0,   265,   116,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,   116,     0,
       0,     0,   116,     0,     0,   441,     0,     0,     0,     0,
       0,     0,     0,   148,    74,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1644,    76,     0,   442,     0,     0,
       0,    74,     0,     0,     0,    79,    80,   148,     0,   583,
     584,    65,    66,    67,    68,    69,    70,    71,    72,  1824,
       0,   814,    76,     0,     0,   646,     0,   723,     0,     0,
       0,   442,    79,   815,     0,   261,     0,   318,     0,     0,
       0,     0,     0,  1816,     0,   648,     0,     0,     0,     0,
     261,     0,     0,     0,     0,     0,     0,   116,    77,     0,
       0,   442,    14,    15,    16,    17,    18,   265,     0,    20,
    1831,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -486,  -486,     0,  -486,
      46,     0,    47,     0,  -486,     0,  1672,   148,     0,  1716,
    1717,    65,    66,    67,    68,    69,    70,    71,    72,  1332,
       0,    58,     0,  1333,   351,  1334,     0,     0,   154,     0,
       0,     0,     0,   116,     0,     0,     0,     0,     0,   261,
       0,     0,     0,     0,   442,   442,     0,     0,     0,   265,
     116,     0,     0,     0,     0,     0,    76,    63,    64,  1561,
     318,     0,     0,     0,     0,   116,     0,     0,     0,     0,
       0,     0,     0,     0,   891,   893,     0,     0,   474,   474,
       0,     0,   148,    74,   265,     0,    65,    66,    67,    68,
      69,    70,    71,    72,  1332,     0,  1920,   265,  1333,     0,
    1334,     0,     0,     0,     0,     0,    77,   116,   116,     0,
     116,     0,     0,     0,    79,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   377,     0,   116,   442,     0,
     265,    76,     0,     0,  1771,     0,     0,     0,   116,     0,
       0,  1672,  1981,     0,     0,     0,     0,     0,     0,     0,
       0,   116,     0,     0,   265,     0,     0,     0,   586,     0,
       0,   265,   261,   110,   116,     0,   968,     0,     0,     0,
       0,     0,     0,     0,     0,   148,   116,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   442,
    1828,     0,   116,   116,     0,   442,     0,     0,     0,     0,
       0,     0,     0,  1839,   442,     0,   148,   116,   116,   116,
      65,    66,    67,    68,    69,    70,    71,    72,  1332,   723,
       0,   110,  1333,     0,  1334,   723,    77,     0,     0,     0,
       0,     0,     0,     0,   723,     0,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,  1873,
       0,     0,     0,   723,     0,    76,     0,     0,  1773,     0,
       0,   261,  2043,   442,    74,   148,   274,   228,   229,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   116,
       0,     0,     0,     0,  2101,    76,     0,   442,   531,  1051,
       0,     0,   474,    74,   116,    79,    80,     0,   116,   110,
       0,     0,     0,     0,   442,     0,     0,   116,  1672,   110,
       0,     0,     0,   230,    76,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    79,    80,   116,   116,   354,     0,
       0,     0,     0,     0,     0,     0,  1672,  2043,     0,     0,
       0,     0,     0,     0,     0,   116,     0,     0,     0,     0,
    1930,   148,     0,     0,  1940,    65,    66,    67,    68,    69,
      70,    71,    72,  1332,     0,   475,     0,  1333,  1953,  1334,
    1672,     0,     0,     0,     0,     0,  1160,     0,  1962,   148,
    1963,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,  1975,     0,  1977,  1978,  1979,     0,     0,  2142,
      76,     0,   110,     0,     0,   116,     0,    74,     0,   442,
       0,     0,     0,     0,   116,     0,     0,     0,     0,     0,
       0,   116,   442,     0,     0,     0,     0,   319,    76,     0,
     110,     0,     0,     0,   116,     0,  1279,   442,    79,    80,
       0,   110,     0,   394,   578,   395,   396,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   354,
    2012,     0,     0,     0,  2017,     0,   110,     0,     0,  2022,
     274,   148,     0,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,   442,   115,     0,
       0,     0,     0,   730,     0,  2052,    77,   405,     0,   148,
       0,   172,   173,    65,    66,    67,    68,    69,    70,    71,
      72,   638,     0,     0,   274,     0,     0,     0,     0,   485,
       0,     0,     0,     0,     0,     0,     0,   638,     0,     0,
       0,   638,     0,     0,     0,     0,     0,     0,     0,  2075,
       0,     0,     0,  2078,     0,     0,   115,   489,     0,     0,
       0,     0,     0,     0,     0,     0,  2092,     0,     0,     0,
       0,     0,   116,     0,     0,     0,   116,     0,     0,     0,
       0,     0,     0,   116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   116,     0,     0,     0,     0,     0,     0,
     116,   276,     0,     0,     0,     0,     0,     0,     0,     0,
     148,  2126,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   116,     0,   115,     0,     0,     0,     0,     0,
     638,     0,     0,     0,   115,   116,     0,  2149,     0,   116,
    2150,     0,     0,   116,     0,     0,     0,  2154,     0,     0,
       0,     0,     0,   358,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,     0,     0,     0,     0,
       0,     0,     0,     0,   116,     0,  2174,     0,     0,     0,
    2176,     0,  2154,   442,     0,     0,     0,     0,     0,     0,
     477,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   354,  2176,     0,     0,     0,     0,   442,     0,
       0,     0,     0,     0,     0,   442,     0,     0,     0,   475,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   723,   265,
     116,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,   116,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
     442,     0,     0,     0,  1279,     0,   354,   475,     0,   110,
       0,     0,     0,     0,   358,     0,     0,  1535,     0,     0,
       0,   115,     0,     0,     0,   276,   638,   475,     0,     0,
     116,   442,     0,     0,     0,   116,     0,   354,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     638,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   638,     0,     0,   640,     0,     0,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   640,     0,     0,     0,   640,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   116,   116,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   116,     0,
       0,     0,   116,     0,     0,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1624,
       0,     0,     0,     0,     0,     0,   116,   116,   116,   116,
     116,   116,   116,     0,     0,     0,     0,     0,   265,     0,
       0,     0,   475,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   442,   442,     0,     0,   354,     0,
     214,     0,     0,     0,     0,   640,     0,     0,   442,     0,
       0,     0,     0,   354,     0,     0,     0,   354,     0,     0,
       0,  1663,     0,   475,  1666,  1680,   354,     0,     0,     0,
    1687,     0,     0,     0,  1691,     0,  1693,   265,  1680,     0,
     296,     0,     0,     0,     0,   354,   354,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     442,     0,     0,     0,   354,   116,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   358,     0,     0,
       0,     0,     0,     0,     0,     0,   116,     0,     0,     0,
       0,     0,     0,     0,   477,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   116,
       0,     0,     0,     0,   354,     0,   116,     0,   110,     0,
     116,     0,     0,   354,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   358,   477,   638,   115,     0,   274,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   640,   477,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   358,   442,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1790,     0,   640,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   475,     0,   640,     0,
       0,     0,     0,     0,     0,   265,   116,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   609,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1830,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   442,     0,     0,     0,  1845,  1847,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     116,     0,     0,     0,   116,     0,     0,     0,     0,     0,
       0,   354,  1666,     0,     0,   354,     0,     0,  1866,     0,
       0,     0,   354,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   354,     0,     0,     0,   116,   477,     0,   354,
     169,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   358,   116,   116,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   169,   358,     0,
       0,   354,   358,     0,     0,     0,     0,     0,   477,     0,
       0,   358,     0,     0,   354,     0,     0,     0,   354,     0,
       0,     0,   354,     0,     0,     0,     0,     0,     0,     0,
     358,   358,     0,     0,   265,     0,     0,     0,     0,   812,
       0,   813,     0,   169,     0,     0,     0,   442,     0,   358,
     829,   830,     0,     0,     0,     0,   169,     0,   169,     0,
    1938,     0,   110,     0,     0,     0,     0,     0,     0,  1941,
       0,  1943,     0,     0,  1948,  1952,     0,  1680,     0,     0,
       0,     0,  1958,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   358,
       0,     0,     0,   115,   379,     0,     0,     0,   358,   274,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   640,     0,
       0,   276,     0,     0,     0,     0,     0,     0,     0,   638,
       0,     0,   169,     0,     0,     0,   169,     0,     0,   169,
     169,     0,     0,   169,     0,     0,   169,   169,   116,   169,
       0,   169,     0,     0,     0,     0,     0,     0,     0,   354,
     475,     0,     0,  2028,     0,   416,     0,     0,  2033,  2035,
       0,   477,     0,     0,     0,     0,     0,     0,   116,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2055,
       0,     0,     0,     0,   116,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   169,   116,   116,   169,     0,   265,   354,   354,     0,
       0,     0,  2079,     0,  2082,     0,     0,  2084,  2086,     0,
       0,     0,  2089,  2091,     0,   116,   358,   354,   169,   169,
     358,   354,     0,     0,     0,   354,   116,   358,     0,     0,
       0,     0,     0,   169,     0,     0,     0,   358,     0,     0,
       0,     0,     0,     0,   358,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,   110,     0,   358,  2133,  2135,  2137,
       0,     0,     0,     0,     0,     0,     0,   110,     0,   358,
       0,     0,     0,   358,     0,     0,     0,   358,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2160,  2162,  2164,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,   115,     0,   475,
       0,   697,     0,     0,   354,   416,   703,     0,     0,  1102,
       0,     0,     0,     0,     0,   712,   713,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   115,
     416,   416,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,     0,
       0,   416,     0,     0,   276,  2003,     0,     0,   354,     0,
     169,     0,     0,     0,     0,   354,     0,     0,     0,   354,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   416,     0,     0,   640,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1251,  1252,   388,     0,  1254,   389,     0,   390,     0,
     391,     0,     0,     0,   358,   477,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   379,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   274,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,     0,
     402,   403,   358,   358,     0,   169,   169,     0,    74,     0,
    1328,   110,     0,     0,     0,     0,     0,     0,   169,     0,
       0,     0,   358,     0,     0,     0,   358,     0,   404,   354,
     358,    77,   405,     0,     0,     0,     0,     0,   406,    79,
      80,   407,   408,   409,   410,     0,     0,     0,     0,     0,
       0,     0,     0,     1,     0,     0,   147,     0,  1349,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,   115,
       0,     0,     0,   354,   354,     0,     0,     0,     0,     0,
       0,     0,   115,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1373,     0,     0,     0,     0,
       0,     0,     0,     0,  1377,  1378,  1379,  1380,     0,     0,
       0,     0,  1385,  1386,     0,     0,     0,     0,     0,     0,
       0,   203,  1394,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   169,   169,   477,     0,   110,     0,   169,   358,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1414,     0,     0,  1417,     0,     0,     0,     0,     0,   169,
       0,     0,   169,   169,     0,   169,     0,   169,   169,     0,
     416,   416,   416,   416,   416,   416,   416,   416,   416,   416,
     416,   416,   416,   416,   416,   416,   416,   416,   416,     0,
       0,     0,     0,   358,     0,     0,     0,     0,     0,     0,
     358,     0,     0,     0,   358,     0,     0,     0,   169,     0,
       0,     0,   169,     0,  1477,     0,   169,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1499,     0,     0,     0,     0,
     416,     0,     0,  1504,     0,  1506,  1508,   638,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1518,     0,
    1519,     0,  1520,     0,     0,     0,     0,     0,     0,  1529,
     169,   169,     0,     0,     0,     0,     0,   354,     0,     0,
     276,     0,     0,     0,   169,     0,     0,     0,     0,     0,
       0,   557,     0,   110,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,   110,   638,     0,     0,     0,     0,     0,     0,     0,
       0,   598,     0,     0,   358,     0,   365,     0,     0,     0,
       0,     0,     0,   605,   354,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
     615,     0,     0,     0,  1604,     0,     0,     0,     0,     0,
       0,  1609,   464,   365,     0,  1610,     0,     0,     0,     0,
       0,   635,     0,     0,     0,     0,     0,     0,   358,   358,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1627,     0,   416,   534,     0,     0,     0,   416,     0,
       0,   534,     0,     0,     0,     0,     0,     0,   169,   416,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   729,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   115,     0,     0,     0,     0,     0,  1710,     0,   169,
       0,     0,     0,   416,     0,   169,     0,     0,   169,   770,
       0,     0,   169,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   534,     0,     0,   808,     0,     0,     0,   811,
       0,     0,     0,  1741,     0,     0,     0,     0,     0,     0,
    1745,     0,  1747,     0,     0,     0,     0,     0,   833,   365,
     650,     0,   834,   835,     0,     0,   838,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   671,
       0,   852,   853,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   883,     0,     0,     0,     0,     0,
       0,     0,   640,     0,     0,     0,     0,     0,  1780,     0,
       0,     0,     0,     0,     0,     0,     0,  1784,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   358,   416,     0,     0,     0,     0,     0,     0,
       0,   534,     0,     0,     0,   169,     0,     0,   115,     0,
       0,     0,     0,     0,   169,   169,     0,   534,   804,     0,
     534,   807,     0,     0,     0,     0,     0,     0,   365,     0,
     921,     0,   650,     0,     0,     0,   115,   640,     0,     0,
       0,     0,     0,     0,   928,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   204,     0,   358,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   951,
     115,     0,     0,   534,   169,     0,     0,   534,     0,     0,
     416,     0,     0,   169,     0,     0,   169,     0,   169,   169,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     416,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1884,  1885,   365,   416,   416,
     416,     0,     0,   204,     0,   416,   416,     0,     0,  1894,
       0,     0,     0,   169,     0,     0,     0,     0,     0,   204,
     994,     0,     0,     0,     0,     0,     0,     0,     0,   416,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   204,     0,     0,     0,     0,     0,     0,
       0,     0,   534,     0,     0,   365,   471,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   416,   416,     0,     0,
       0,     0,     0,     0,   941,   365,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   650,     0,     0,     0,   650,
       0,     0,     0,     0,     0,     0,   959,   169,   365,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   204,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1117,
       0,     0,     0,     0,     0,     0,     0,     0,  1126,     0,
       0,     0,     0,     0,  1129,     0,     0,     0,     0,     0,
       0,     0,     0,  1996,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   204,     0,     0,   169,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,   204,  1172,     0,     0,     0,     1,     0,     0,
     365,     0,     0,   169,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   534,   534,     1,     0,
       0,     0,     0,     0,     0,     0,   534,  1072,     0,   534,
    1075,     0,     0,     0,     0,     0,   169,     0,     0,     0,
       0,   365,   169,     0,   650,     0,   650,   650,     0,     0,
       0,     0,   289,   650,     0,     0,   211,     0,     0,     0,
       0,     0,     0,   365,   365,     0,     0,     0,     0,     0,
       0,     0,   269,     0,     0,     0,     0,  1308,     0,     0,
       0,   204,   365,  2100,     0,   534,     0,     0,     0,   534,
       0,     0,     0,     0,     0,     0,   534,  1145,     0,     0,
     534,  1149,     0,     0,   534,  1153,   361,     0,     0,     0,
       0,   204,  1157,     0,     0,     0,     0,     0,   169,     0,
       0,     0,   211,     0,     0,     0,   329,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   370,     0,
       0,  2139,     0,   361,     0,     0,     0,     0,     0,     0,
       0,   365,   534,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   211,     0,     0,  2156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   484,     0,     0,   488,     0,
    2165,     0,     0,     0,   650,     0,     0,   416,     0,     0,
       0,     0,     0,     0,   204,   204,     0,   169,   169,  1375,
     471,  1376,     0,     0,     0,   379,     0,     0,     0,     0,
     169,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,     0,     0,     0,     0,     0,
       0,   211,     0,     0,     0,     0,     0,   361,     0,     0,
       0,     0,     0,     0,     0,   269,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   204,     0,
       0,   147,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   471,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   361,
     488,   361,   361,   534,     0,     0,     0,     0,     0,     0,
     211,   204,     0,     0,     0,   361,     0,     0,     0,   361,
     650,   650,     0,     0,   169,     0,     0,   650,     0,     0,
       0,   643,     0,   660,     0,     0,   204,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,   534,  1398,     0,   534,  1402,  1517,     0,
     534,  1406,     0,     0,     0,     0,   727,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   169,
       0,     0,     0,     0,     0,  1543,     0,     0,     0,     0,
       0,     0,     0,     0,   416,     0,     0,     0,   361,     0,
     211,     0,     0,   471,   361,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   204,     0,     0,
     643,     0,     0,     0,     0,   416,   828,     0,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   471,   471,     0,     0,
     361,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   169,     0,   471,     0,   361,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
       0,     0,     0,   650,  1525,     0,     0,     0,     0,     0,
       0,     0,     0,   211,   211,     0,     0,     0,     0,   484,
       0,   361,     0,     0,     0,     0,     0,     0,   365,     0,
     416,     0,   416,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   361,   361,     0,     0,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,     0,     0,
       0,     0,   204,     0,   361,   361,     0,   361,     0,     0,
       0,   416,   534,  1579,     0,   361,     0,   370,     0,     0,
       0,   534,  1586,  1727,   650,     0,     0,  1731,   361,     0,
       0,   361,     0,     0,     0,   365,   365,   484,   361,   945,
       0,   361,     0,   416,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     643,     0,     0,     0,     0,     0,     0,   204,     0,     0,
       0,     0,     0,     0,     0,   167,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   211,     0,   416,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   727,     0,
       0,   727,   727,     0,   727,     0,     0,     0,     0,     0,
       0,     0,     0,   727,     0,     0,   727,   727,   727,     0,
       0,     0,     0,     0,  1781,  1782,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     361,     0,     0,     0,     0,     0,     0,     0,   294,     0,
    1731,     0,     0,     0,     0,     0,   361,     0,     0,     0,
       0,   300,     0,   301,     0,     0,     0,     0,     0,     0,
       0,   361,   484,     0,     0,   361,     0,   365,     0,     0,
       0,   361,     0,     0,   361,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   211,     0,     0,     0,
       0,     0,   471,   361,   361,   650,     0,     0,     0,     0,
       0,     0,     0,   484,     0,     0,     0,     0,     0,     0,
       0,     0,   361,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   484,   484,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   484,  1877,  1878,     0,     0,     0,
       0,     0,     0,     0,   536,   537,     0,     0,   541,     0,
       0,   544,   545,     0,   547,     0,   548,     0,     0,     0,
       0,     0,   361,     0,     0,     0,     0,     0,     0,     0,
       0,   361,     0,     0,   534,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     534,   361,     0,   361,   361,     0,     0,     0,     0,     0,
       0,     0,     0,   484,     0,     0,     0,     0,     0,     0,
       0,   211,     0,     0,     0,     0,     0,     0,     0,   204,
       0,   360,     0,     0,     0,   828,     0,     0,     0,     0,
     204,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   361,     0,     0,     0,     0,     0,
       0,     0,     0,   632,   633,     0,     0,     0,   360,     0,
       0,   204,  1965,     0,     0,     0,     0,   365,   665,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1731,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1995,     0,     0,     0,     0,     0,     0,   361,
       0,   365,   365,   361,     0,     0,     0,     0,   471,   471,
     361,     0,     0,     0,     0,     0,  2011,     0,   534,   534,
     361,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,     0,   360,     0,   534,     0,     0,     0,     0,     0,
       0,     0,     0,  2039,   802,     0,     0,  2040,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   361,
       0,     0,     0,     0,   388,     0,     0,   389,     0,   390,
       0,   391,   361,     0,     0,     0,   361,     0,     0,     0,
     361,     0,     0,     0,   360,     0,   360,   360,   392,     0,
       0,   484,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,   360,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   879,     0,     0,   393,   394,
       0,   395,   396,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   397,   398,   385,     0,   399,   400,   401,
     534,   402,   403,   727,     0,     0,     0,     0,   534,    74,
     204,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1674,  1675,  1676,     0,     0,     0,   404,
    1846,     0,    77,   405,     0,     0,     0,   727,     0,   406,
      79,    80,   407,   408,   409,   410,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   360,     0,     0,     0,   361,   269,   360,
       0,   361,     0,     0,     0,   365,     0,   534,  2065,     0,
       0,   534,     0,     0,     0,     0,     0,     0,   211,     0,
       0,     0,     0,     0,     0,     0,     0,   361,   361,   643,
     957,   958,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   965,     0,     0,     0,     0,     0,     0,
       0,     0,   534,     0,     0,     0,     0,     0,     0,     0,
     370,     0,     0,   204,   727,   360,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   361,   361,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   534,   534,     0,     0,   361,   360,     0,     0,   361,
       0,     0,     0,   361,     0,     0,     0,   484,   484,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,   360,
     360,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     204,     0,   534,     0,     0,     0,     0,     0,     0,   360,
     360,     0,   360,     0,     0,     0,     0,  1065,  1066,     0,
     360,     0,     0,  1070,     0,   727,   727,   727,     0,     0,
     727,   727,     0,   360,     0,     0,   360,   488,     0,     0,
       0,     0,     0,   360,  1091,     0,   360,  1094,  1095,     0,
    1098,     0,  1100,  1101,   471,   471,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,     0,   361,     0,     0,
       0,     0,   361,  1143,     0,     0,   269,  1147,     0,     0,
       0,  1151,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,     0,   362,   362,     0,     0,
       0,     0,     0,     0,     0,   360,   361,     0,     0,     0,
     362,     0,     0,   361,   362,     0,     0,   361,     0,     0,
       0,   360,     0,     0,     0,  1271,  1272,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,  1288,
     360,     0,     0,     0,     0,     0,   360,     0,     0,   360,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   360,   360,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   360,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   211,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,     0,     0,     0,   471,     0,
       0,     0,     0,     0,   269,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   360,     0,     0,
       0,     0,     0,     0,     0,     0,   360,   361,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1288,     0,     0,   360,     0,   360,   360,
       0,     0,     0,     0,     0,   362,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,     0,     0,     0,     0,
       0,   361,   361,     0,  1390,     0,     0,     0,     0,     0,
    1396,     0,     0,  1400,     0,   727,     0,  1404,     0,   360,
       0,     0,     0,     0,     0,     0,   362,     0,     0,     0,
       0,     0,     0,   484,   484,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
     362,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
     362,     0,   362,     0,     0,     0,     0,     0,     0,     0,
     362,     0,     0,   269,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,   360,     0,   362,     0,   360,     0,
       0,     0,     0,   362,     0,   360,   362,     0,     0,     0,
       0,     0,     0,     0,     0,   360,     0,    14,    15,    16,
      17,    18,   360,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,   440,     0,   360,    46,     0,    47,     0,     0,
    1523,     0,     0,     0,     0,   469,     0,   360,     0,  1533,
    1534,   360,     0,     0,     0,   360,    58,     0,   497,     0,
     497,     0,     0,     0,     0,   361,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   361,     0,   727,     0,     0,
       0,   362,     0,     0,     0,     0,     0,     0,     0,  1577,
       0,     0,     0,     0,     0,     0,   362,     0,  1584,     0,
     362,  1588,     0,  1591,  1592,     0,   362,   484,     0,   362,
     388,     0,     0,   389,     0,   390,     0,   391,     0,     0,
     361,     0,     0,   361,     0,     0,     0,     0,   362,   362,
       0,     0,     0,     0,   392,     0,     0,     0,     0,   610,
       0,     0,   361,     0,     0,     0,     0,   362,  1619,     0,
       0,     0,   727,     0,     0,   488,     0,     0,     0,     0,
       0,     0,   360,     0,   393,   394,   360,   395,   396,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   397,
     398,   385,     0,   399,   400,   401,     0,   402,   403,     0,
       0,     0,   360,   360,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,     0,     0,   404,   362,     0,    77,   405,
       0,     0,     0,   494,     0,   406,    79,    80,   407,   408,
     409,   410,  1724,     0,     0,     0,   362,     0,   362,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,   360,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,   360,     0,     0,     0,   360,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   497,     0,     0,  1588,
       0,     0,   497,     0,     0,     0,     0,   849,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1786,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,     0,     0,     0,   362,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,     0,   360,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,     0,     0,   920,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,   362,     0,     0,     0,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   469,     0,     0,
       0,   360,     0,  1874,     0,     0,     0,     0,   360,     0,
     953,     0,   360,   183,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,   252,   253,     0,   254,
      46,     0,    47,     0,   255,     0,   988,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,     0,  1924,  1925,     0,     0,     0,     0,     0,     0,
     849,  1008,     0,     0,  1010,  1929,  1012,     0,     0,     0,
       0,     0,  1021,     0,  1028,  1021,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1056,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   362,   362,     0,  1058,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,  1067,     0,     0,     0,
       0,     0,     0,     0,     0,  -461,     0,     0,     0,     0,
     469,     0,     0,  1056,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -461,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2001,
    1120,     0,     0,   497,     0,     0,   360,   360,     0,     0,
     362,   362,     0,     0,     0,     0,  1132,  1286,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,    18,
     362,     0,     0,     0,   362,     0,     0,     0,   362,     0,
       0,     0,     0,     0,     0,  1158,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   388,     0,     0,   389,     0,   390,     0,   391,     0,
       0,     0,     0,     0,  2063,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,   392,     0,     0,     0,     0,
       0,   440,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1278,  1280,     0,     0,     0,
       0,     0,     0,   469,     0,   393,   394,     0,   395,   396,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,     0,   399,   400,   401,     0,   402,   403,
       0,     0,  1021,     0,     0,     0,    74,     0,     0,     0,
       0,     0,   362,     0,     0,     0,  1056,   362,     0,     0,
       0,     0,     0,     0,  1321,     0,   404,     0,     0,    77,
     405,  1021,     0,     0,     0,     0,   406,   467,    80,   407,
     408,   409,   410,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,     0,   362,   497,
     360,  2175,   362,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1449,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   360,     0,     0,   360,   388,
       0,     0,   389,     0,   390,     0,   391,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   497,   360,  1389,     0,
    1392,  1189,     0,   392,    -2,     0,  1191,  -248,  -248,  1192,
    1193,  1194,  1195,  1196,  1197,  1198,  1199,  1200,  1201,  1202,
    1203,  -342,  -342,  1204,  1205,  1206,  1207,  1208,  1209,  1210,
       0,  1211,     0,   393,   394,     0,   491,   396,  1212,  1213,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,  1214,   399,   400,   401,     0,   402,   403,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,     0,     0,     0,     0,
       0,  1467,  1467,  -248,  1215,     0,     0,    77,   405,     0,
       0,     0,   298,     0,   406,    79,    80,   407,   408,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,  -187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   362,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2175,     0,  1521,     0,     0,     0,     0,     0,  1530,
       0,     0,     0,     0,     0,     0,     0,     0,  1449,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     469,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   497,     0,   388,
       0,     0,   389,     0,   390,     0,   391,     0,     0,     0,
       0,     0,  1021,     0,     0,   849,     0,     0,     0,     0,
       0,  1189,     0,   392,    -2,     0,  1191,  -249,  -249,  1192,
    1193,  1194,  1195,  1196,  1197,  1198,  1199,  1200,  1201,  1202,
    1203,  -342,  -342,  1204,  1205,  1206,  1207,  1208,  1209,  1210,
       0,  1211,     0,   393,   394,     0,   491,   396,  1212,  1213,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,  1214,   399,   400,   401,     0,   402,   403,     0,     0,
       0,     0,     0,     0,    74,     0,     0,  1621,  1622,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -249,  1215,     0,     0,    77,   405,     0,
     362,     0,   298,  1021,   406,    79,    80,   407,   408,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,  -187,
       0,   497,     0,     0,   849,     0,     0,     0,     0,     0,
     362,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1872,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1449,
       0,     0,     0,     0,     0,   362,     0,     0,   362,  1008,
       0,     0,     0,     0,     0,     0,     0,     0,  1743,  1744,
       0,     0,     0,     0,     0,     0,     0,   362,     0,   497,
     388,     0,     0,   389,     0,   390,     0,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   497,     0,
     849,     0,  1189,     0,   392,    -2,     0,  1191,     0,     0,
    1192,  1193,  1194,  1195,  1196,  1197,  1198,  1199,  1200,  1201,
    1202,  1203,  -342,  -342,  1204,  1205,  1206,  1207,  1208,  1209,
    1210,     0,  1211,     0,   393,   394,     0,   491,   396,  1212,
    1213,    65,    66,    67,    68,    69,    70,    71,    72,   397,
     398,   385,  1214,   399,   400,   401,     0,   402,   403,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   440,     0,     0,  1215,     0,  1815,    77,   405,
       0,     0,     0,   298,     0,   406,    79,    80,   407,   408,
     409,   410,     0,     0,     0,     0,     0,     0,     0,     0,
    -187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1860,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  1188,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  1896,     0,   388,  1898,    46,   389,    47,
     390,     0,   391,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,  1189,    58,  1190,
      -2,     0,  1191,  1915,     0,  1192,  1193,  1194,  1195,  1196,
    1197,  1198,  1199,  1200,  1201,  1202,  1203,  -342,  -342,  1204,
    1205,  1206,  1207,  1208,  1209,  1210,     0,  1211,     0,   393,
     394,    61,   491,   396,  1212,  1213,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,  1214,   399,   400,
     401,     0,   402,   403,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -3,
    1215,     0,     0,    77,   436,     0,     0,     0,   298,     0,
     406,    79,    80,   407,   408,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,  -187,     0,     0,     0,     0,
       4,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1188,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   388,     0,    46,   389,
      47,   390,     0,   391,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,  1189,    58,
    1190,    -2,     0,  1191,     0,  1021,  1192,  1193,  1194,  1195,
    1196,  1197,  1198,  1199,  1200,  1201,  1202,  1203,  -342,  -342,
    1204,  1205,  1206,  1207,  1208,  1209,  1210,     0,  1211,     0,
     393,   394,    61,   491,   396,  1212,  1213,    65,    66,    67,
      68,    69,    70,    71,    72,   397,   398,   385,  1214,   399,
     400,   401,     0,   402,   403,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1215,     0,     0,    77,   436,     0,     0,     0,   298,
       0,   406,    79,    80,   407,   408,   409,   410,     0,     0,
       0,     0,     0,     0,     0,     0,  -187,     4,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   388,     0,    46,   389,    47,   390,     0,
     391,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   392,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   394,    61,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,     0,
     402,   403,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1674,  1675,  1676,     0,     0,     0,   404,  1677,
    1678,    77,   436,     0,     0,     0,     0,     0,   406,    79,
      80,   407,   408,   409,   410,     0,     0,     0,     0,     0,
       0,     0,     0,  1679,     4,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     388,     0,    46,   389,    47,   390,     0,   391,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,   392,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   393,   394,    61,   395,   396,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   397,
     398,   385,     0,   399,   400,   401,     0,   402,   403,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1674,
    1675,  1676,     0,     0,     0,   404,  1677,     0,    77,   436,
       0,     0,     0,     0,     0,   406,    79,    80,   407,   408,
     409,   410,     0,     0,     0,     0,     0,     0,     0,     0,
    1679,     4,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   388,     0,    46,
     389,    47,   390,     0,   391,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   392,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   393,   394,    61,   395,   396,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   397,   398,   385,     0,
     399,   400,   401,     0,   402,   403,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   404,     0,  1665,    77,   436,     0,     0,     0,
       0,     0,   406,    79,    80,   407,   408,   409,   410,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   388,     0,    46,   389,    47,
     390,     0,   391,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   392,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,    61,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,     0,   402,   403,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     404,     0,     0,    77,   436,     0,     0,     0,     0,     0,
     406,    79,    80,   407,   408,   409,   410,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   388,     0,    46,   389,    47,   390,     0,   391,
     346,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   392,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   393,   394,     0,   395,
     396,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   397,   398,   385,     0,   399,   400,   401,     0,   402,
     403,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,     0,     0,
      77,   466,     0,     0,     0,     0,     0,   406,   467,    80,
     407,   408,   409,   410,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   388,
       0,    46,   389,    47,   390,     0,   391,   346,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   392,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   393,   394,     0,   395,   396,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,     0,   399,   400,   401,     0,   402,   403,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   404,     0,     0,    77,  1275,     0,
       0,     0,     0,     0,   406,  1276,    80,   407,   408,   409,
     410,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   388,     0,    46,   389,
      47,   390,     0,   391,   346,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     392,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     393,   394,     0,   395,   396,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   397,   398,   385,     0,   399,
     400,   401,     0,   402,   403,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,     0,     0,    77,   405,     0,     0,     0,     0,
       0,   406,    79,    80,   407,   408,   409,   410,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   388,     0,    46,   389,    47,   390,     0,
     391,   346,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   392,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,     0,
     402,   403,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   404,     0,
       0,    77,   466,     0,     0,     0,     0,     0,   406,    79,
      80,   407,   408,   409,   410,  2010,     0,    -2,    -2,    -2,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,    -2,
      -2,  2038,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,
       0,  1527,    -2,     0,     0,     0,     0,    -2,    -2,    14,
      15,    16,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,   388,     0,     0,   389,     0,
     390,     0,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    58,   392,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,    -2,    -2,     0,     0,   393,
     394,     0,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,     0,   402,   403,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     404,     0,     0,    77,   405,     0,     0,     0,     0,     0,
     406,  1528,    80,   407,   408,   409,   410,     4,     5,     6,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,    76,
       0,    77,    78,     0,     0,     0,     0,     0,     0,    79,
      80,   257,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -486,  -486,     0,  -486,    46,
       0,    47,     0,  -486,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   148,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,    76,     0,    77,   258,     0,     0,     0,
    -819,     0,     0,    79,    80,   257,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -486,
    -486,     0,  -486,    46,     0,    47,     0,  -486,     0,     0,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    58,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,   148,    47,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,    76,     0,    77,
     258,     0,   184,     0,   185,   186,     0,    79,    80,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,     0,
       0,     0,     0,  -409,  -409,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -409,
       0,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,    79,    80,     4,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,     0,     0,     0,     0,  -410,  -410,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -410,     0,     0,     0,    77,    78,     0,
    1426,     0,  1427,     0,     0,    79,    80,  1428,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1429,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1430,     0,     0,     0,    77,   984,     0,  1426,     0,  1427,
       0,     0,    79,    80,  1428,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1429,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1630,     0,     0,
       0,    77,   984,     0,  1426,     0,  1427,     0,     0,    79,
      80,  1428,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1429,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1631,     0,     0,     0,    77,   984,
       0,  1426,     0,  1427,     0,     0,    79,    80,  1428,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1429,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1632,     0,     0,     0,    77,   984,     0,     0,     0,
       0,     0,     0,    79,    80,   257,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -486,
    -486,     0,  -486,    46,     0,    47,   257,  -486,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -486,  -486,     0,  -486,    46,     0,    47,     0,  -486,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
     258,    63,    64,     0,     0,     0,     0,    79,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,   327,     0,     0,     0,     0,     0,     0,    79,    80,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   346,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   148,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,   589,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1080,    76,  -695,    77,   646,     0,     0,     0,     0,     0,
       0,    79,    80,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -486,  -486,     0,  -486,
      46,     0,    47,     0,  -486,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   148,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,    76,     0,    77,   258,     0,     0,
       0,  -823,     0,     0,    79,    80,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -486,
    -486,     0,  -486,    46,     0,    47,     0,  -486,     0,     0,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    58,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,   148,    47,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,    76,     0,    77,
     258,     0,   705,     0,   706,   707,     0,    79,    80,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,   -17,    47,     0,
       0,     0,   346,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,   589,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   645,
       0,  -695,    77,   646,     0,     0,    63,    64,     0,     0,
      79,    80,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    77,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   346,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,   346,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,   589,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   786,     0,  -695,    77,   531,     0,     0,    63,
      64,     0,     0,    79,    80,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    77,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   346,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,  1111,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -682,    77,   348,
       0,     0,     0,     0,     0,     0,    79,    80,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   346,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     347,    77,   348,     0,     0,     0,     0,     0,     0,    79,
      80,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   346,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,  1910,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,   348,     0,     0,     0,     0,
       0,     0,    79,    80,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   346,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,  1912,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   348,     0,
       0,     0,     0,     0,     0,    79,    80,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     346,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,   327,     0,     0,     0,     0,     0,     0,    79,    80,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   346,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   348,     0,     0,     0,     0,     0,
       0,    79,    80,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -486,  -486,     0,  -486,
      46,     0,    47,     0,  -486,     0,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,     0,
       0,     0,     0,  1449,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   388,     0,     0,   389,     0,   390,
       0,   391,     0,     0,     0,     0,    77,   258,   705,     0,
     706,   707,     0,     0,    79,    80,  1189,     0,   392,    -2,
       0,  1191,  1931,  1932,  1192,  1193,  1194,  1195,  1196,  1197,
    1198,  1199,  1200,  1201,  1202,  1203,     0,     0,  1204,  1205,
    1206,  1207,  1208,  1209,  1210,     0,  1211,     0,   393,   394,
       0,   491,   396,  1212,  1213,    65,    66,    67,    68,    69,
      70,    71,    72,   397,   398,   385,  1214,   399,   400,   401,
    1449,   402,   403,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1215,
       0,   388,    77,   405,   389,     0,   390,   298,   391,   406,
      79,    80,   407,   408,   409,   410,     0,     0,     0,     0,
       0,     0,     0,  1189,  -187,   392,    -2,     0,  1191,     0,
       0,  1192,  1193,  1194,  1195,  1196,  1197,  1198,  1199,  1200,
    1201,  1202,  1203,     0,     0,  1204,  1205,  1206,  1207,  1208,
    1209,  1210,     0,  1211,     0,   393,   394,     0,   491,   396,
    1212,  1213,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,  1214,   399,   400,   401,     0,   402,   403,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1215,     0,     0,    77,
     405,     0,     0,     0,   298,     0,   406,    79,    80,   407,
     408,   409,   410,     0,     0,     0,     0,     0,     0,     0,
       0,  -187,   302,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -413,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,   302,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,    77,    46,     0,    47,
       0,  -413,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -414,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,   302,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,    77,    46,     0,    47,     0,  -414,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,    14,    15,    16,
      17,    18,    19,   714,    20,   715,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    63,    64,   388,     0,    46,   389,    47,   390,     0,
     391,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   392,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   716,
       0,     0,     0,     0,  1203,     0,  -342,     0,     0,     0,
      77,     0,     0,     0,     0,  -413,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,     0,
     402,   403,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1215,     0,
       0,    77,   717,     0,     0,     0,   298,     0,   406,    79,
      80,   718,   719,   409,   410,    14,    15,    16,    17,    18,
      19,   714,    20,   715,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   388,     0,    46,   389,    47,   390,     0,   391,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   392,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   716,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   393,   394,     0,   395,   396,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,     0,   399,   400,   401,     0,   402,   403,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   404,     0,     0,    77,
     717,     0,     0,     0,   298,     0,   406,    79,    80,   718,
     719,   409,   410,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   388,
       0,    46,   389,    47,   390,     0,   391,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   392,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   393,   394,     0,   395,   396,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,     0,   399,   400,   401,     0,   402,   403,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   404,     0,   435,    77,   436,     0,
       0,     0,     0,     0,   406,    79,    80,   407,   408,   409,
     410,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   388,     0,    46,
     389,    47,   390,     0,   391,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   392,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   393,   394,     0,   395,   396,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   397,   398,   385,     0,
     399,   400,   401,     0,   402,   403,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   404,     0,     0,    77,   717,     0,     0,     0,
     298,     0,   406,    79,    80,   407,   408,   409,   410,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   388,     0,    46,   389,    47,
     390,     0,   391,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   392,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,     0,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,     0,   402,   403,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     404,     0,     0,    77,   436,     0,     0,     0,     0,     0,
     406,    79,    80,   407,   408,   409,   410,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   388,     0,    46,   389,    47,   390,     0,
     391,   346,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   392,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,     0,
     402,   403,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   404,     0,
       0,    77,   466,     0,     0,     0,     0,     0,   406,    79,
      80,   407,   408,   409,   410,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   388,     0,    46,   389,    47,   390,     0,   391,   346,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   392,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   393,   394,     0,   395,   396,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,     0,   399,   400,   401,     0,   402,   403,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   404,     0,     0,    77,
     405,     0,     0,     0,     0,     0,   406,    79,    80,   407,
     408,   409,   410,   257,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -486,  -486,     0,
    -486,    46,     0,    47,     0,  -486,     0,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
     346,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,     0,   148,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   589,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -695,
      77,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   148,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,    76,     0,    77,    78,     0,     0,     0,
    -821,     0,     0,    79,    80,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   148,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,    76,     0,    77,
      78,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   148,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,    79,    80,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   346,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   589,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,  -695,    77,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -486,  -486,     0,  -486,    46,     0,
      47,     0,  -486,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   148,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,    76,     0,    77,   327,     0,     0,     0,     0,
       0,     0,    79,    80,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   346,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1182,     0,     0,     0,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,    77,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   346,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   990,    77,   984,     0,     0,
       0,     0,     0,     0,    79,    80,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,  1547,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,   984,     0,     0,     0,     0,     0,     0,    79,    80,
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
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,   310,     0,     0,    63,    64,
       0,     0,    79,    80,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,    79,    80,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   346,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   462,     0,     0,    63,    64,     0,     0,    79,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,   348,     0,     0,     0,     0,
       0,     0,    79,    80,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   346,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,   346,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,   310,
       0,     0,    63,    64,     0,     0,    79,    80,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   462,     0,     0,     0,     0,     0,     0,    79,
      80,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -486,  -486,     0,  -486,    46,     0,
      47,     0,  -486,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,   346,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   327,     0,
       0,     0,     0,     0,     0,    79,    80,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   346,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   984,     0,     0,    63,    64,     0,     0,    79,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,   984,     0,     0,     0,     0,
       0,     0,    79,    80,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   346,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,     0,
       0,    14,    15,    16,    17,    18,    79,    80,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -486,  -486,     0,  -486,    46,
       0,    47,     0,  -486,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      58,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -486,  -486,     0,  -486,
      46,     0,    47,     0,  -486,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   327,    63,    64,     0,
       0,     0,     0,    79,    80,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   388,     0,    46,   389,    47,   390,
       0,   391,     0,     0,     0,     0,    77,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   392,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   393,   394,
       0,   395,   396,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   397,   398,   385,     0,   399,   400,   401,
       0,   402,   403,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   404,
       0,     0,    77,   405,     0,     0,     0,     0,     0,   406,
     467,    80,   407,   408,   409,   410,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   388,     0,    46,   389,    47,
     390,     0,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,     0,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,     0,   402,   403,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     404,     0,     0,    77,   405,     0,     0,     0,     0,     0,
     406,    79,    80,   407,   408,   409,   410,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
     388,     0,     0,   389,     0,   390,    58,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   392,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,   393,   394,     0,   395,   396,  1946,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   397,
     398,   385,     0,   399,   400,   401,     0,   402,   403,   388,
       0,     0,   389,     0,   390,    74,   391,     0,     0,     0,
       0,    77,     0,     0,     0,     0,     0,     0,     0,  1674,
    1675,  1676,     0,   392,     0,   404,  1947,     0,    77,   405,
       0,     0,     0,     0,     0,   406,    79,    80,   407,   408,
     409,   410,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   393,   394,     0,   491,   396,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,     0,   399,   400,   401,   388,   402,   403,   389,     0,
     390,     0,   391,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
       0,     0,     0,     0,   404,    76,     0,   492,   493,     0,
       0,     0,   494,     0,   406,    79,    80,   407,   408,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,     0,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,   388,   402,   403,   389,     0,   390,     0,   391,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   392,     0,     0,     0,     0,
     404,  1324,     0,    77,   405,     0,     0,     0,  1325,     0,
     406,    79,    80,   407,   408,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,   393,   394,     0,   395,   396,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,     0,   399,   400,   401,   388,   402,   403,
     389,     0,   390,     0,   391,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   392,     0,     0,     0,     0,   404,   848,     0,    77,
     405,     0,     0,     0,     0,     0,   406,    79,    80,   407,
     408,   409,   410,     0,     0,     0,     0,     0,     0,     0,
       0,   393,   394,     0,   395,   396,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   397,   398,   385,     0,
     399,   400,   401,   388,   402,   403,   389,     0,   390,     0,
     391,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,     0,     0,
       0,     0,   404,     0,     0,    77,   405,     0,     0,     0,
     298,     0,   406,    79,    80,   407,   408,   409,   410,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,   388,
     402,   403,   389,     0,   390,     0,   391,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,     0,     0,     0,     0,   404,  1017,
       0,    77,   405,     0,     0,     0,     0,     0,   406,    79,
      80,   407,   408,   409,   410,     0,     0,     0,     0,     0,
       0,     0,     0,   393,   394,     0,   395,   396,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,     0,   399,   400,   401,   388,   402,   403,   389,     0,
     390,     0,   391,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
       0,     0,     0,     0,   404,     0,     0,    77,   405,     0,
       0,  1050,     0,     0,   406,    79,    80,   407,   408,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,     0,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,   388,   402,   403,   389,     0,   390,     0,   391,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   392,     0,     0,     0,     0,
     404,  1391,     0,    77,   405,     0,     0,     0,     0,     0,
     406,    79,    80,   407,   408,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,   393,   394,     0,   395,   396,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,     0,   399,   400,   401,   388,   402,   403,
     389,     0,   390,     0,   391,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   392,     0,     0,     0,     0,   404,     0,     0,    77,
     405,     0,     0,     0,  1459,     0,   406,    79,    80,   407,
     408,   409,   410,     0,     0,     0,     0,     0,     0,     0,
       0,   393,   394,     0,   395,   396,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   397,   398,   385,     0,
     399,   400,   401,   388,   402,   403,   389,     0,   390,     0,
     391,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,     0,     0,
       0,     0,   404,     0,     0,    77,   405,     0,     0,     0,
    1537,     0,   406,    79,    80,   407,   408,   409,   410,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,   388,
     402,   403,   389,     0,   390,     0,   391,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,     0,     0,     0,     0,   404,     0,
    1937,    77,   405,     0,     0,     0,     0,     0,   406,    79,
      80,   407,   408,   409,   410,     0,     0,     0,     0,     0,
       0,     0,     0,   393,   394,     0,   395,   396,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,     0,   399,   400,   401,   388,   402,   403,   389,     0,
     390,     0,   391,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
       0,     0,     0,     0,   404,  1942,     0,    77,   405,     0,
       0,     0,     0,     0,   406,    79,    80,   407,   408,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,     0,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,   388,   402,   403,   389,     0,   390,     0,   391,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   392,     0,     0,     0,     0,
     404,  1951,     0,    77,   405,     0,     0,     0,     0,     0,
     406,    79,    80,   407,   408,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,   393,   394,     0,   395,   396,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,     0,   399,   400,   401,   388,   402,   403,
     389,     0,   390,     0,   391,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   392,     0,     0,     0,     0,   404,  2032,     0,    77,
     405,     0,     0,     0,     0,     0,   406,    79,    80,   407,
     408,   409,   410,     0,     0,     0,     0,     0,     0,     0,
       0,   393,   394,     0,   395,   396,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   397,   398,   385,     0,
     399,   400,   401,   388,   402,   403,   389,     0,   390,     0,
     391,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,     0,     0,
       0,     0,   404,  2034,     0,    77,   405,     0,     0,     0,
       0,     0,   406,    79,    80,   407,   408,   409,   410,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,   388,
     402,   403,   389,     0,   390,     0,   391,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,     0,     0,     0,     0,   404,  2081,
       0,    77,   405,     0,     0,     0,     0,     0,   406,    79,
      80,   407,   408,   409,   410,     0,     0,     0,     0,     0,
       0,     0,     0,   393,   394,     0,   395,   396,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,     0,   399,   400,   401,   388,   402,   403,   389,     0,
     390,     0,   391,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
       0,     0,     0,     0,   404,  2083,     0,    77,   405,     0,
       0,     0,     0,     0,   406,    79,    80,   407,   408,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,     0,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,   388,   402,   403,   389,     0,   390,     0,   391,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   392,     0,     0,     0,     0,
     404,  2085,     0,    77,   405,     0,     0,     0,     0,     0,
     406,    79,    80,   407,   408,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,   393,   394,     0,   395,   396,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,     0,   399,   400,   401,   388,   402,   403,
     389,     0,   390,     0,   391,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   392,     0,     0,     0,     0,   404,  2088,     0,    77,
     405,     0,     0,     0,     0,     0,   406,    79,    80,   407,
     408,   409,   410,     0,     0,     0,     0,     0,     0,     0,
       0,   393,   394,     0,   395,   396,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   397,   398,   385,     0,
     399,   400,   401,   388,   402,   403,   389,     0,   390,     0,
     391,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,     0,     0,
       0,     0,   404,  2090,     0,    77,   405,     0,     0,     0,
       0,     0,   406,    79,    80,   407,   408,   409,   410,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,   388,
     402,   403,   389,     0,   390,     0,   391,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,     0,     0,     0,     0,   404,  2132,
       0,    77,   405,     0,     0,     0,     0,     0,   406,    79,
      80,   407,   408,   409,   410,     0,     0,     0,     0,     0,
       0,     0,     0,   393,   394,     0,   395,   396,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,     0,   399,   400,   401,   388,   402,   403,   389,     0,
     390,     0,   391,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
       0,     0,     0,     0,   404,  2134,     0,    77,   405,     0,
       0,     0,     0,     0,   406,    79,    80,   407,   408,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,     0,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,   388,   402,   403,   389,     0,   390,     0,   391,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   392,     0,     0,     0,     0,
     404,  2136,     0,    77,   405,     0,     0,     0,     0,     0,
     406,    79,    80,   407,   408,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,   393,   394,     0,   395,   396,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,     0,   399,   400,   401,   388,   402,   403,
     389,     0,   390,     0,   391,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   392,     0,     0,     0,     0,   404,  2159,     0,    77,
     405,     0,     0,     0,     0,     0,   406,    79,    80,   407,
     408,   409,   410,     0,     0,     0,     0,     0,     0,     0,
       0,   393,   394,     0,   395,   396,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   397,   398,   385,     0,
     399,   400,   401,   388,   402,   403,   389,     0,   390,     0,
     391,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,     0,     0,
       0,     0,   404,  2161,     0,    77,   405,     0,     0,     0,
       0,     0,   406,    79,    80,   407,   408,   409,   410,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,   388,
     402,   403,   389,     0,   390,     0,   391,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,     0,     0,     0,     0,   404,  2163,
       0,    77,   405,     0,     0,     0,     0,     0,   406,    79,
      80,   407,   408,   409,   410,     0,     0,     0,     0,     0,
       0,     0,     0,   393,   394,     0,   395,   396,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,     0,   399,   400,   401,   388,   402,   403,   389,     0,
     390,     0,   391,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
       0,     0,     0,     0,   404,     0,     0,    77,   405,     0,
       0,     0,     0,     0,   406,    79,    80,   407,   408,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,     0,   395,   396,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,   388,   402,   403,   389,     0,   390,     0,   391,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   392,     0,     0,     0,     0,
     696,     0,     0,    77,   405,     0,     0,     0,     0,     0,
     406,    79,    80,   407,   408,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,   393,   394,     0,   395,   396,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     397,   398,   385,     0,   399,   400,   401,   388,   402,   403,
     389,     0,   390,     0,   391,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   392,     0,     0,     0,     0,   702,     0,     0,    77,
     405,     0,     0,     0,     0,     0,   406,    79,    80,   407,
     408,   409,   410,     0,     0,     0,     0,     0,     0,     0,
       0,   393,   394,     0,   395,   396,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   397,   398,   385,     0,
     399,   400,   401,   388,   402,   403,   389,     0,   390,     0,
     391,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   392,     0,     0,
       0,     0,   711,     0,     0,    77,   405,     0,     0,     0,
       0,     0,   406,    79,    80,   407,   408,   409,   410,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   394,     0,
     395,   396,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   397,   398,   385,     0,   399,   400,   401,   388,
     402,   403,   389,     0,   390,     0,   391,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   392,     0,     0,     0,     0,   404,     0,
       0,    77,   405,     0,     0,     0,     0,     0,   406,   919,
      80,   407,   408,   409,   410,     0,     0,     0,     0,     0,
       0,     0,     0,   393,   394,     0,   395,   396,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   397,   398,
     385,     0,   399,   400,   401,   388,   402,   403,   389,     0,
     390,     0,   391,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   392,
       0,     0,     0,     0,   404,     0,     0,    77,   405,     0,
       0,     0,     0,     0,   406,   467,    80,   407,   408,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,   393,
     394,     0,   395,   396,  2027,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   397,   398,   385,     0,   399,   400,
     401,     0,   402,   403,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     404,     0,     0,    77,   405,     0,     0,     0,     0,     0,
     406,    79,    80,   407,   408,   409,   410,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -485,  -485,     0,  -485,    46,     0,    47,     0,  -485,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,    20,    58,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -486,  -486,     0,  -486,    46,     0,    47,     0,
    -486,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,   225,    75,     4,     1,     1,   177,    75,    75,   182,
       1,   271,   256,    97,     4,   932,   181,   203,    75,   494,
     230,    84,   182,   349,   728,  1233,   404,   251,   929,   230,
     166,   658,   645,   133,   230,   167,   378,  1215,   218,   365,
    1198,    77,  1172,   369,   166,   230,   649,    59,  1375,  1376,
     562,   743,  1267,  1268,     1,    56,    57,   645,    59,   817,
     406,   573,    59,    59,   166,   823,   551,   552,    59,   645,
     100,  1684,    97,   787,    75,     1,   230,   137,  1025,   721,
     230,  1798,   240,    84,   160,   916,   365,   150,    84,     1,
     369,    92,   230,   141,   170,   351,    97,  1798,   834,   100,
    1215,    97,   814,   104,   100,   230,   618,  1054,   104,   319,
     268,    90,    59,    75,   104,    75,   852,   816,   319,   192,
      97,   279,   222,   319,   192,   192,  1798,   230,     1,  1931,
       1,    75,   651,    59,   319,   147,   816,   814,   230,    89,
     137,   142,   468,    75,   145,   814,   147,   231,    72,   179,
     147,   147,   153,   100,   202,  1680,   147,   230,   814,   160,
     135,   904,   230,   230,     0,   319,   167,  1451,    89,   319,
    1117,   120,     0,   230,   233,   154,   235,  1935,     0,    72,
     240,   319,   925,   242,     1,   921,    59,     4,    59,    84,
     191,   192,   120,    75,   319,   191,   259,  1169,   683,   297,
     147,   814,   177,   303,  1176,   206,   231,   153,   268,   158,
     206,   137,   162,   816,   160,   216,   319,   179,   474,   279,
     221,   147,   306,   154,   225,   226,   814,   319,  1841,   230,
     231,   104,   192,   179,   158,   231,   320,   154,   814,   160,
     615,   162,    59,   240,   304,  1181,   319,   349,   192,   161,
     251,   319,   319,  1181,   231,   314,   315,   293,   259,   154,
     192,   154,   319,   259,   137,   328,   137,  2069,   269,   270,
     230,   268,   273,    20,   147,   535,   147,   273,    10,   280,
     101,   306,   279,   543,  1256,   154,   230,   104,   980,   158,
       1,  1181,   112,   294,   295,   320,   297,   179,   230,   818,
     343,   302,   512,   822,   404,   306,  1799,   304,   634,   794,
     306,   512,   831,   832,   240,   135,   512,  1009,   319,   320,
     500,  1846,  1847,  1081,   320,   549,   273,   512,   329,   306,
     147,   555,   658,   154,   111,   591,   337,   338,   680,   382,
     472,   342,   268,   320,  1333,   671,   833,   834,    59,  1063,
     916,   954,   698,   279,   125,   634,   998,   134,   512,  1190,
     307,     1,   512,   741,  2122,   852,   468,   240,  1080,   240,
     154,   557,   154,  1488,   512,   438,  1491,  1492,   304,   380,
       1,   637,   383,   384,  2101,    75,   157,   512,  1087,   901,
      72,  1222,   671,  1129,   136,   268,   178,   268,   154,  2157,
    2101,   611,    92,  1080,   351,   661,   279,  1087,   279,   512,
     611,  1080,   668,  1543,   572,   611,   464,    10,   679,    59,
     512,   153,  1947,  1948,  1080,  1709,   611,  1920,   170,  2101,
    1173,   304,   164,   304,   921,   308,   147,   169,   538,   512,
     524,   112,   540,   160,   512,   512,   546,   448,  1420,   635,
     154,   151,   448,   170,  1781,  1782,   492,   611,   833,   834,
     136,   611,   162,   563,   135,    77,    78,  1080,   404,   157,
     471,   472,   154,   611,   574,    72,   158,   852,   178,   615,
      61,    62,   483,   484,  1087,   156,   611,  1423,  1424,  1425,
     178,   492,  1080,   494,   170,  1423,  1424,  1425,   148,   524,
     162,   633,    75,   239,  1080,    72,  2072,   147,   611,   154,
     246,   512,   572,   615,   160,    72,   614,   179,    91,   611,
     170,  1679,   154,   524,   170,  1283,  1684,   474,   524,   613,
     162,   267,  2098,  1423,  1424,  1425,   203,   110,   611,   160,
     161,  1719,   278,   611,   611,   162,   921,   524,   549,  2042,
     162,    72,   512,   160,   555,     4,   658,   154,  2124,    72,
     159,   158,   179,    13,    14,    15,    16,    17,   512,   158,
     136,   164,   179,    84,   163,   572,   169,  1566,  1567,  1568,
     512,   162,  1057,   162,   770,  1104,    97,   154,   613,   100,
     169,   158,    72,   104,   108,   109,   696,   154,   179,   165,
     166,   158,   702,   768,  1719,   617,  1364,    56,    57,  2102,
     611,   711,   613,    75,  1561,   941,   617,   613,   619,   994,
     617,   617,    72,   179,  1190,   811,   617,   628,    90,    62,
     730,   632,   633,   154,    77,    78,   613,   158,   635,  1343,
     160,   154,  1129,    92,   591,   158,   572,   833,   834,  2142,
    1277,   108,   109,   160,   827,   879,  1222,   604,   160,    72,
    1875,   841,   941,   170,   665,   838,   852,   827,  1995,    72,
     617,    72,   105,   160,   154,   936,   109,   678,   158,   112,
     191,   114,   349,  1841,   816,   352,   136,   943,   786,   280,
     156,   617,   179,   142,   160,   206,   145,   160,   365,   572,
     157,   572,   369,   801,   295,   156,   157,   805,   148,   149,
     150,   160,   160,   974,   661,   880,   179,    69,   167,   162,
     231,    58,    72,    72,    61,    62,   912,    64,   160,   160,
     170,   179,   733,   769,   735,   921,   737,    72,   179,  1640,
     741,   154,   160,   744,   617,   158,   617,   179,   259,  1441,
      72,   154,   160,   154,  1129,   158,   380,   158,  1462,   383,
     696,   179,   273,   699,   700,   158,   702,     3,   769,  1977,
     163,   179,   221,    72,   160,   711,     3,   226,   714,   715,
     716,  1266,   157,    72,    72,    72,   886,  1945,    13,    14,
      15,    16,    17,   179,   148,   149,   150,   160,   898,  1957,
     617,   468,   902,   539,   154,   154,   906,   170,   158,   158,
      72,   162,  1729,   814,   811,   816,   170,   154,   169,   154,
     269,   270,   160,   158,  1771,   179,  1773,   828,   564,   262,
     160,   280,   154,   965,   835,   571,   158,  1215,   154,   575,
     841,   179,   166,   844,   153,   294,   295,    72,   297,   179,
    1325,   160,   853,   302,  1558,   154,  1116,   160,   994,   158,
     162,   156,   160,   130,   131,   154,   154,   154,  2026,   158,
     158,   158,   167,   168,   160,   165,   309,  1050,   879,   203,
     329,   179,   172,   173,   170,  1451,  1452,   155,   337,   338,
     557,     3,   994,   342,   162,   156,   748,   749,   750,  1465,
     161,    13,    14,    15,    16,    17,   617,   174,   175,    47,
      48,   136,    50,   914,   915,   916,   154,    55,   178,   916,
     353,   153,   355,   161,   357,   916,   134,   162,   160,  1271,
    1272,   380,   154,  1308,   383,   156,   154,   448,   928,   160,
     162,   134,  1545,  1129,  1124,   156,   154,   169,   615,   160,
     158,  1277,   156,  1126,  1086,  1087,   160,   165,   166,   156,
      72,   154,   395,   160,   965,   158,   176,   634,   635,   916,
     178,  1069,   165,   166,   154,  1073,   156,   617,   979,    13,
      14,    15,    16,    17,  1459,  1630,  1631,  1632,   156,   156,
     916,   658,  1090,   161,   161,   120,   943,   156,   154,  1097,
    1375,   156,   161,    84,   671,     3,   161,  1633,   160,  1270,
     162,   154,  1638,   524,  1015,    13,    14,    15,    16,    17,
     156,   156,   471,   158,  1025,  1281,   154,   157,   158,  1290,
      13,   161,   156,   156,   483,   484,  1260,   160,    72,  1300,
     134,   156,  1286,   916,  1142,   916,     3,   156,  1146,   156,
    1428,    58,  1150,  1054,    61,    62,  1057,    64,   491,   154,
     154,   156,  1537,   158,   158,  1178,  1179,  1180,   156,   150,
     134,   165,   166,   160,    72,  1175,   148,   149,   150,  1080,
     154,  1556,   160,   156,    75,  1086,  1087,   160,   160,  1189,
     154,  1651,  1652,  1653,   158,  1154,  1267,  1268,   170,   916,
     156,   165,   166,   770,   160,    88,    97,   179,  1208,   190,
     154,   928,   156,    22,   158,  1215,  1117,   154,  1181,   156,
     160,   158,   154,   106,  1297,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   154,   569,  1297,   154,   158,
     101,   156,  1517,  1709,   811,   160,   108,   109,   154,   106,
    1410,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   154,   153,   156,   154,   158,   154,   903,   158,    89,
     619,  1307,  1308,  1346,   153,  1277,   154,   134,   259,   628,
     158,   156,  1172,   632,   633,   160,   167,   168,   162,  1375,
    1376,  1192,  1534,  1190,  1195,  1196,  1197,   154,   155,  1190,
     154,   128,   129,  1204,   161,   916,  1308,   162,   165,   166,
    1383,  1384,   179,   156,   154,   156,   665,   160,   158,   160,
     162,  1222,   156,  1383,  1384,  1222,   160,  1228,   156,   678,
     159,  1222,   160,   557,   225,   156,   148,   149,   150,   230,
     231,   178,  1243,  1190,   162,  1246,  1247,   328,   160,  1250,
    1246,  1247,   132,   133,  1376,   169,  1257,  1247,   170,  1260,
     251,   156,   156,   156,  1190,   160,   160,   179,   349,   156,
     154,  1532,   120,   160,   941,  1222,   916,   944,  1844,  1215,
     154,  1973,   154,   156,   733,   156,   735,   160,   737,   160,
     154,   615,   741,   160,  1295,   744,  1222,  1395,   156,  1246,
     156,  1399,   160,   156,   160,  1403,   156,   160,  1309,   156,
     160,   635,   156,   160,   156,   306,   160,  1190,   160,  1190,
     769,   156,  2106,   166,  1325,   160,  2110,   994,   319,   320,
     159,   160,  1333,   171,  1281,    13,    14,    15,    16,    17,
      18,   164,  1442,  1443,   113,   114,   115,   116,   117,  1222,
    1434,  1222,   159,   160,   176,  1172,   134,   438,    72,   157,
    1423,  1424,  1425,   156,  1365,  1428,  1429,    13,    14,    15,
      16,    17,    18,  1190,  1247,   156,  1312,   165,   166,   828,
     156,  1517,   156,   677,   159,   160,   835,   468,   159,   160,
     156,  1451,   106,  1493,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   853,  1222,  1781,   156,  1750,  1434,
     755,   756,   757,   758,   158,  1517,   159,   160,   159,   160,
     134,   160,   161,   159,  1522,   159,   160,    91,    92,   136,
    1247,   159,   160,  1434,  1644,   136,  1426,  1438,  1439,   161,
     154,   155,   161,  1644,   158,  2011,   770,   160,  1644,   159,
     160,   165,   166,   178,  1451,  1452,   154,  1434,  1459,  1644,
    1451,  1452,   159,   160,   156,   914,   915,   916,  1465,   156,
     551,   552,   156,  2039,  1465,  1736,  1737,   156,  1576,  1190,
     156,   472,   156,  1484,  1485,  1583,   156,   811,   154,  1587,
    1644,   159,   160,  1494,  1644,   156,   159,   160,  1494,   159,
     160,   159,   160,   156,  1451,  1452,  1644,  2073,   159,   833,
     834,  1222,   159,   160,   159,   160,   965,   158,  1465,  1644,
     162,   512,   159,   160,   178,  1451,  1452,   162,   852,   159,
     160,   159,   160,   524,   160,   161,  1537,   159,   160,  1465,
     160,  1644,  1707,   159,   160,   839,   156,  1494,   162,  1633,
    1190,   162,  1644,  1543,  1638,  1556,    77,    78,   549,   162,
    1561,    70,  1646,   159,   555,  1566,  1567,  1568,     4,     5,
       6,     7,     8,     9,    10,    11,    12,   658,  1451,  1452,
    1451,  1452,  1222,   160,   161,  1334,  1335,  1644,   912,   751,
     752,  1727,  1465,   179,  1465,  1781,  1782,   921,   154,  1699,
     753,   754,   683,  1813,    78,  1727,  1491,  1492,  1633,  1426,
    1277,   159,  1813,  1638,   759,   760,    18,  1813,  1652,  1653,
     611,  1646,   613,   178,   160,  1727,  1535,  1536,  1813,    65,
    1891,   162,  1633,   179,  1451,  1452,   154,  1638,   162,  1965,
     156,  1308,   156,  1644,   179,  1646,   159,   162,  1465,  1709,
      18,   159,   153,  1654,  1878,    22,  1633,   156,   156,  1813,
    1782,  1638,   156,  1813,  1665,   156,   156,   156,   156,  1646,
     994,   156,   156,   156,   156,  1813,   156,  1678,   972,    70,
     153,   153,   178,  1673,   162,   162,   162,   156,  1813,    13,
      14,    15,    16,    17,    18,   162,  1861,   156,   156,   123,
     156,   125,   126,   127,  1875,   156,   153,   178,   162,   162,
    1813,   156,  1709,   794,  2040,   156,   160,   160,  1709,   156,
     160,  1813,   156,  1724,   156,  1672,  1543,  1988,   156,   156,
     154,   156,  1816,   157,   158,  1246,  1247,   156,   162,   163,
     156,  1731,   159,  1192,   159,   156,  1195,  1196,  1197,    13,
      14,    15,    16,    17,    18,  1204,   156,   156,  1931,   156,
     156,   156,  1709,   156,   156,   159,   156,   156,   156,   156,
    1771,  1931,  1773,  1222,    13,    14,    15,    16,    17,  1228,
     156,   153,   156,  1709,   153,   160,    14,  2043,   154,   154,
     154,  1816,   154,   154,  1243,   154,   154,    74,   161,   179,
    1673,  1250,   179,   160,  1969,  1129,   161,   159,  1257,  1995,
     159,   162,  1813,   153,   153,  1816,   162,   160,  2042,   179,
     156,   156,   156,   814,  1825,   816,   179,  1763,  1829,   156,
     156,   159,   153,    72,   156,   160,  1709,   160,  1709,  1816,
     156,   160,  1843,   156,   159,  1929,  1295,  1844,   929,   156,
    1517,   156,  1853,  1844,  1855,   156,  1673,   159,   153,   179,
    1309,   154,   154,    80,   154,   179,  1867,   179,  1869,  1870,
    1871,   179,    92,  1995,     4,   179,  1877,  1878,   153,   179,
     179,   179,   154,   154,  1831,    90,   156,   153,   879,   162,
     153,  2101,  1709,   160,  2067,   134,  2069,  1844,   160,   153,
    2101,   162,  1965,   159,  1929,  2101,   159,  2067,   159,  2069,
     159,   153,   156,   161,  1731,   154,  2101,   123,  1844,   158,
     161,   153,   156,   159,   159,   156,   165,   166,  1929,   156,
     156,   156,   153,   179,  1935,  2108,   154,   153,  1939,   161,
     153,  2106,   156,  1944,   154,  2110,  2111,  2101,  2108,   160,
     154,  2101,  1929,   153,   156,   162,   159,    13,    14,    15,
      16,    17,   159,  2101,  2062,   159,   153,  2140,   156,  1970,
     100,  1844,   153,  1844,   104,  1269,  2101,  2040,   156,  2144,
     156,   156,   159,  1494,  1308,    75,    75,   179,   179,  1438,
    1439,   153,   156,   154,   179,   154,   156,   153,  2101,   162,
     153,   159,   159,   153,  2169,   153,   158,  1301,  2173,  2101,
     156,  2184,   156,  2014,  2011,   156,    72,  2018,  2102,   156,
    2011,    75,   157,  2188,  2184,   170,    75,  1844,  2101,   161,
    2031,   170,   179,  2101,  2101,  1484,  1485,   179,   153,   179,
     153,  2042,  2039,  2044,  2101,   153,   160,   170,  2039,   155,
     170,  1375,  1376,   153,   161,   106,  1350,   154,  2142,   160,
      75,   241,   153,   155,  2011,   170,   170,   179,  2168,   179,
      75,   159,   153,   203,   156,  2076,  2073,  2102,   134,   155,
    2180,   156,  2073,   161,   156,  2011,   153,   153,   156,  1080,
     154,   156,  2039,   179,   179,  1086,  1087,  1763,   154,   179,
    2101,  2102,   158,  1340,   761,   720,  2102,   392,   763,   165,
     166,  2112,   762,  2039,  2115,   764,  1222,  2142,  1210,   437,
    2157,  2122,   765,  1709,  2069,  2102,  2073,  1852,  2098,  1465,
    1844,  2056,   417,   418,  1718,  2139,  1700,  1700,  2011,  2040,
    2011,  2142,  2111,   273,  2173,  2039,  2142,  2073,  1250,    49,
    2151,   112,  2001,   438,  2155,   264,  2157,  1929,  1429,   969,
    1243,     1,   628,   501,     4,  2142,  2039,     0,  2039,   841,
      72,    13,    14,    15,    16,    17,  1502,  2178,  1731,   979,
     786,   786,   786,   468,  1622,  1266,    -1,    -1,  2189,    -1,
      -1,    -1,    -1,  1517,  2011,    -1,  1277,  2198,    -1,    -1,
    2073,    -1,  2073,    -1,   106,  1654,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,  1665,    -1,    -1,    59,
    1514,    -1,  2039,    -1,    -1,    -1,    -1,    -1,    -1,  1678,
      72,    -1,    -1,    -1,    -1,    75,    -1,  1531,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    -1,  1540,    -1,    -1,    -1,
      -1,    -1,   154,   155,    -1,    -1,  2073,    97,    -1,    -1,
     100,   441,    -1,  1557,   104,    -1,    -1,    -1,    -1,  1260,
      -1,    -1,    -1,    -1,    -1,  1724,    -1,   457,    -1,   106,
     460,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   134,    -1,    -1,    -1,    -1,    -1,  1965,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,   147,    78,    -1,
     150,    -1,   154,   153,   154,    -1,   158,    -1,    -1,    -1,
      -1,    -1,    -1,   165,   166,    -1,   166,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   106,   517,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
     190,   191,   192,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   202,   203,    -1,    -1,   206,    -1,    -1,    -1,
      -1,    -1,    -1,  2040,  1365,    -1,  1825,    -1,    -1,    -1,
    1829,    -1,    -1,    -1,    -1,   225,    -1,    -1,    -1,    -1,
     230,   231,    -1,    -1,  1843,  1689,    -1,    61,    62,    63,
      64,    -1,    -1,  1727,  1853,    -1,  1855,    -1,    -1,   179,
      -1,   251,    -1,    -1,    -1,    -1,    -1,    -1,  1867,   259,
    1869,  1870,  1871,    -1,    -1,    -1,    -1,   557,  1877,    -1,
     106,    -1,    -1,   273,   110,   111,   112,   113,   114,   115,
     116,   117,   106,  1434,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,  1749,    -1,  1781,  1782,    -1,
    1754,    -1,   747,    -1,    -1,    -1,   306,    -1,    -1,    -1,
      -1,  1765,   312,    -1,    -1,    -1,    -1,    -1,   318,   319,
     320,    62,    -1,    -1,    -1,    -1,  1935,    -1,   328,    -1,
    1939,    -1,    57,    -1,   158,  1944,    -1,    -1,    -1,    -1,
      65,    66,    67,    68,    -1,   635,    -1,    -1,    -1,   349,
     350,   351,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,  1970,    -1,    -1,    -1,   365,    -1,    -1,   106,   369,
     111,   112,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,  1640,
      -1,    -1,    -1,    -1,   404,  2014,    -1,    -1,    -1,  2018,
      -1,    -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,
     158,    -1,  2031,    -1,    -1,    -1,  1880,  1881,    -1,    -1,
      -1,    -1,    -1,   158,    -1,  2044,    -1,    -1,   438,    -1,
      -1,   441,    -1,    -1,    -1,    -1,   786,   787,   448,    -1,
      -1,   176,    -1,    -1,    -1,    -1,   796,    -1,    -1,   799,
      -1,    -1,    -1,    -1,   464,   206,    -1,  2076,   468,    -1,
      -1,    -1,   472,    -1,   474,    -1,    -1,    -1,    -1,    -1,
     770,    -1,  1633,    -1,    -1,    -1,    -1,  1638,    -1,    -1,
     216,    -1,    -1,  1644,    -1,  1646,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2112,    -1,    -1,  2115,    -1,    -1,    -1,
      -1,  1995,   512,  2122,    -1,    -1,    -1,    -1,    -1,   859,
      -1,   811,    -1,    -1,   524,    -1,   866,    -1,    -1,    -1,
     870,    -1,   273,    -1,   874,  1989,    -1,    -1,    -1,    -1,
      -1,    -1,  2151,   833,   834,   990,  2155,    -1,  2157,   549,
     995,   551,   552,    -1,    -1,   555,    -1,   557,    72,    -1,
      -1,  1006,   852,    -1,    -1,    -1,   307,    -1,    -1,  2178,
      -1,   312,    -1,    -1,    -1,    -1,    -1,   318,    -1,    -1,
    2189,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,  2198,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,  2059,     4,    -1,    -1,    -1,
     351,   611,    -1,   613,    -1,   615,    -1,   617,   106,    -1,
     134,    -1,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   921,    -1,    -1,   634,   635,    -1,   637,   928,    -1,
     154,   155,    -1,    -1,    -1,   645,   134,    -1,    18,   649,
      -1,   165,   166,    -1,   395,    -1,    -1,    -1,   658,    -1,
      -1,    -1,  1813,    -1,    -1,  1816,   154,   155,   668,    -1,
      -1,   671,    -1,    -1,    -1,    -1,    -1,   165,   166,    -1,
      -1,    62,    -1,   683,    -1,    -1,    84,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,   696,    -1,    -1,   699,
     700,    -1,   702,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   711,    -1,    -1,   714,   715,   716,    -1,    -1,   100,
      -1,    -1,    -1,  1063,    -1,    -1,    -1,  1878,    -1,    -1,
     111,   106,   113,   474,   115,   110,   111,   112,   113,   114,
     115,   116,   117,   141,    -1,  1190,    -1,    -1,    -1,    -1,
     491,   106,   150,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,   492,    -1,   494,    -1,
     770,    -1,    -1,   154,    -1,    -1,   157,   158,  1929,    -1,
     155,    -1,    -1,   158,    -1,    -1,   786,   787,    -1,    -1,
      -1,    -1,   190,  1133,   794,    -1,  1136,    -1,     1,    -1,
    1140,     4,    -1,    -1,   202,    -1,    -1,    -1,    -1,    -1,
      -1,   811,    -1,    -1,   814,   556,   816,    -1,    -1,    -1,
      -1,    56,    57,    -1,   179,   206,    -1,    -1,    -1,    -1,
      -1,    -1,  1277,   833,   834,    -1,   577,    -1,   106,  1129,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     591,    -1,   852,    -1,    -1,    -1,    59,    92,    -1,   257,
      -1,   259,    -1,   604,    -1,    -1,    -1,    -1,    -1,    -1,
    1315,  1316,  1317,    -1,    -1,    -1,    -1,  1322,  1323,   879,
      -1,    84,  1172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2042,   273,    -1,   275,   276,   637,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   302,    -1,    -1,   142,    -1,    -1,
     145,   179,    -1,    -1,    -1,    -1,   916,    -1,    -1,    -1,
     661,   921,    -1,    -1,    -1,   160,   307,   668,   928,   929,
     328,   312,    -1,    -1,   137,    -1,    -1,   318,   141,    -1,
      -1,   941,    -1,   943,   147,    -1,    -1,   150,    -1,    -1,
    2101,  2102,    -1,    -1,   954,    -1,  1246,  1247,    -1,    -1,
      -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,    -1,    -1,
     351,    -1,    -1,    -1,   106,   356,    -1,   358,   110,   111,
     112,   113,   114,   115,   116,   117,   221,   190,   191,    -1,
      -1,  2142,    -1,    -1,   994,    -1,    -1,    -1,    -1,   202,
     203,    -1,    -1,    -1,    -1,    -1,   404,    -1,    -1,    -1,
      -1,    -1,  1352,    -1,   395,    -1,    -1,    -1,    -1,    -1,
      -1,  1361,   154,   155,    -1,    -1,    -1,    -1,   231,    -1,
      -1,    -1,    -1,    -1,   269,   270,    -1,   240,    -1,    -1,
     438,    -1,    -1,    -1,    -1,   280,    -1,    -1,   251,    -1,
      -1,    -1,    -1,   256,   257,    -1,   259,    -1,    -1,    -1,
     295,    -1,    -1,  1063,    -1,   268,   464,   448,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   279,    -1,    -1,   282,
    1080,    -1,    -1,   286,    -1,  1375,  1376,  1087,   291,    -1,
      -1,    -1,    -1,   474,   329,   476,   477,    -1,    -1,   302,
      -1,   304,   337,   338,    -1,   308,    -1,   342,    -1,    -1,
     491,    -1,    -1,    -1,    -1,   841,    -1,   320,   844,    -1,
      -1,    -1,    -1,    -1,    -1,   328,    -1,    -1,    -1,  1129,
      -1,    -1,    -1,    -1,    -1,    -1,  1426,    -1,    -1,    -1,
      -1,    -1,   883,   524,    -1,   380,   349,    -1,   383,   352,
      -1,    -1,    -1,   551,   552,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   365,    -1,    -1,    -1,   369,    -1,   549,    -1,
      -1,    -1,  1172,   554,    -1,   556,    -1,    -1,    -1,    -1,
      -1,  1181,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1190,    -1,    -1,    -1,    -1,    -1,   577,    -1,   579,   580,
      -1,   404,   943,    -1,  1494,    -1,    -1,    -1,    -1,    -1,
     591,    -1,    -1,    -1,    -1,  1215,    -1,    -1,    -1,    -1,
      -1,    -1,  1222,   604,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   613,    -1,    -1,   438,   471,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,  1246,  1247,   483,   484,
      -1,    -1,    -1,  1543,    -1,    -1,   637,    -1,   639,   640,
    1260,   464,    -1,    -1,    -1,   468,  1266,    -1,    -1,  1714,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1277,    -1,    -1,
     661,   662,    -1,    -1,    -1,   683,    -1,   668,    -1,  1015,
      -1,    -1,    -1,    -1,  1634,    -1,    72,    -1,   696,  1025,
      -1,   699,   700,    -1,   702,    -1,    -1,    -1,  1308,    -1,
      -1,    -1,  1312,   711,    -1,    -1,   714,   715,   716,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,  1054,    -1,
     106,  1057,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,    -1,   551,   552,
      -1,    -1,    -1,    -1,   557,    -1,    -1,   106,   134,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   572,
      -1,    -1,    -1,    -1,    -1,  1375,  1376,    -1,   154,   155,
      -1,    -1,  1672,  1673,   619,   134,    -1,    -1,    -1,   165,
     166,  1117,    -1,   628,    -1,    -1,   794,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,   615,    -1,   617,    -1,   165,   166,    -1,    -1,
      -1,    -1,    -1,  1423,  1424,  1425,  1426,    -1,  1428,  1429,
      -1,   634,   635,    -1,  1434,  1435,    -1,    -1,    -1,    -1,
      -1,  1731,    -1,   678,    -1,    -1,   649,    -1,    -1,    -1,
      -1,  1451,  1452,    -1,    -1,   658,    -1,    -1,  1798,  1799,
     663,    -1,    -1,    -1,    -1,  1465,    -1,   106,   671,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   106,
     683,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,  1781,  1782,   696,  1494,    -1,   699,   700,    -1,   702,
      -1,    -1,    -1,    -1,    -1,  1246,    -1,    -1,   711,    -1,
      -1,   714,   715,   716,    -1,    -1,    -1,  1517,    13,    14,
      15,    16,    17,    -1,    -1,    -1,   165,  1972,    -1,    -1,
     928,   929,    -1,    -1,    -1,   162,    -1,    -1,    -1,    -1,
    1281,  1831,   169,  1543,    -1,  1545,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,   943,    -1,    -1,    -1,    -1,   770,    -1,    -1,
      -1,    -1,    -1,   954,    -1,    -1,    -1,    72,    -1,    -1,
    1920,    -1,   963,    -1,   787,    -1,    -1,    -1,    -1,    -1,
      -1,   794,    -1,   828,    -1,    -1,    -1,   157,    -1,  1325,
     835,    -1,   162,    -1,    -1,    -1,    -1,  1333,   811,   169,
      -1,   106,    -1,   816,    -1,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     833,   834,    -1,  1633,  1634,    -1,    -1,    -1,  1638,   134,
    1640,    -1,    -1,    -1,  1644,    -1,  1646,    -1,    -1,   852,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1997,    -1,   154,
     155,  2001,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     165,   166,  1672,  1673,    -1,  1416,    -1,    -1,    -1,    -1,
     915,   106,  1063,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,  1435,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2042,    -1,    -1,  1995,  1087,    -1,    -1,  1709,
      -1,    -1,    -1,   916,    -1,    -1,    -1,    -1,   921,    -1,
      -1,    -1,    -1,    -1,    -1,   928,   929,  1727,    -1,    -1,
      -1,  1731,    -1,  1459,    -1,    -1,    -1,   162,   941,    -1,
      -1,   944,    -1,    -1,   169,    -1,    -1,    -1,   951,    -1,
      -1,    -1,    -1,  1494,    -1,    -1,    13,    14,    15,    16,
      17,  2101,  2102,  1763,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,  1172,    -1,    -1,    -1,    -1,    -1,
      -1,  1781,  1782,  1181,    -1,    -1,    13,    14,    15,    16,
      17,   994,    -1,    -1,    -1,    -1,    -1,    -1,  1798,  1799,
      -1,    -1,  2142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1537,    -1,  1813,    -1,    72,  1816,  1215,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
    1556,  1831,    -1,    -1,    -1,  1561,    -1,    -1,    -1,    -1,
    1566,  1567,  1568,    -1,  1844,    72,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,  1246,    -1,   155,  1266,    -1,
     158,    -1,    -1,    -1,    -1,    -1,    -1,   134,  1878,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
    1281,   158,    -1,    -1,    -1,    -1,  1287,   134,   165,   166,
      -1,    -1,    -1,    -1,  1312,    -1,    -1,    -1,    -1,    -1,
    1920,    -1,    -1,    -1,    -1,    -1,  1129,   154,   155,  1929,
      -1,  1672,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
      -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
    1195,  1196,  1197,    -1,    -1,  1965,    -1,    -1,    -1,  1172,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,  1181,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1190,    -1,    -1,
      -1,   154,   155,  1228,     1,  1995,    -1,     4,   161,    -1,
      -1,  2001,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2011,  1215,    -1,    -1,  1250,    -1,    -1,    -1,  1222,
      -1,    -1,  1257,    -1,    -1,  1423,  1424,  1425,  1426,  1427,
    1428,  1429,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2039,
    2040,    -1,  2042,  2043,  1247,  1771,    -1,  1773,    -1,    -1,
      -1,    -1,    59,    -1,  1435,    -1,    -1,    -1,    -1,    -1,
    1295,    -1,    -1,  1266,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2073,  1277,    -1,    -1,    84,    -1,    -1,
      -1,    -1,    72,  1286,    -1,    -1,    -1,    -1,    -1,    -1,
    1831,    -1,    -1,   100,    -1,    -1,    -1,   104,    -1,    -1,
      -1,  2101,  2102,    -1,    -1,  1308,    -1,    -1,    -1,  1312,
      -1,    -1,    -1,  1494,    -1,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
     137,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
     147,    -1,  2142,   150,   134,  1543,   106,   154,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   165,   166,
     167,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,    -1,  1375,  1376,    -1,   165,   166,    -1,    13,    14,
      15,    16,    17,   190,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   202,   203,    -1,   158,   206,
      -1,    -1,    -1,  1438,  1439,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,    -1,  1192,    -1,   151,
    1423,  1424,  1425,  1426,  1427,  1428,  1429,    -1,    -1,  1204,
      -1,    -1,    -1,   240,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,  1640,    -1,    -1,    -1,   178,    -1,  1451,  1452,
     257,    -1,   259,    13,    14,    15,    16,    17,    -1,    -1,
      -1,   268,  1465,    -1,    -1,    -1,   273,    -1,    -1,    -1,
      -1,   106,   279,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,   294,    -1,    -1,
     297,  1672,    -1,    -1,    -1,   302,    -1,   304,    -1,   134,
     307,   308,  2043,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,   318,    72,    -1,  1517,    -1,    -1,    -1,    -1,   154,
     155,   328,    -1,   158,    -1,    -1,    -1,    -1,    -1,    -1,
     165,   166,    -1,  1731,    -1,    -1,    -1,    -1,    -1,    -1,
    1543,    -1,   349,    -1,   351,   352,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   365,    -1,
      -1,    -1,   369,    -1,    -1,  1763,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   134,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,   155,    -1,   404,    -1,    -1,
      -1,   134,    -1,    -1,    -1,   165,   166,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,  1654,
      -1,   154,   155,    -1,    -1,   158,    -1,   404,    -1,    -1,
      -1,   438,   165,   166,    -1,  1816,    -1,  1640,    -1,    -1,
      -1,    -1,    -1,  1646,    -1,   178,    -1,    -1,    -1,    -1,
    1831,    -1,    -1,    -1,    -1,    -1,    -1,   464,   157,    -1,
      -1,   468,    13,    14,    15,    16,    17,   474,    -1,    20,
    1673,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,  1709,   106,    -1,  1484,
    1485,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    72,    -1,   122,  1727,   124,    -1,    -1,  1731,    -1,
      -1,    -1,    -1,   540,    -1,    -1,    -1,    -1,    -1,  1920,
      -1,    -1,    -1,    -1,   551,   552,    -1,    -1,    -1,   556,
     557,    -1,    -1,    -1,    -1,    -1,   155,   108,   109,   158,
    1763,    -1,    -1,    -1,    -1,   572,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   551,   552,    -1,    -1,  1781,  1782,
      -1,    -1,   106,   134,   591,    -1,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,  1799,   604,   122,    -1,
     124,    -1,    -1,    -1,    -1,    -1,   157,   614,   615,    -1,
     617,    -1,    -1,    -1,   165,   166,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   632,    -1,   634,   635,    -1,
     637,   155,    -1,    -1,   158,    -1,    -1,    -1,   645,    -1,
      -1,  1844,  1877,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   658,    -1,    -1,   661,    -1,    -1,    -1,   665,    -1,
      -1,   668,  2043,     1,   671,    -1,   673,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   683,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,   696,
    1665,    -1,   699,   700,    -1,   702,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1678,   711,    -1,   106,   714,   715,   716,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   696,
      -1,    59,   122,    -1,   124,   702,   157,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   711,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,  1724,
      -1,    -1,    -1,   730,    -1,   155,    -1,    -1,   158,    -1,
      -1,  2142,  1965,   770,   134,   106,   104,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,   786,
      -1,    -1,    -1,    -1,   154,   155,    -1,   794,   158,   766,
      -1,    -1,  1995,   134,   801,   165,   166,    -1,   805,   137,
      -1,    -1,    -1,    -1,   811,    -1,    -1,   814,  2011,   147,
      -1,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   165,   166,   833,   834,   166,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2039,  2040,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   852,    -1,    -1,    -1,    -1,
    1825,   106,    -1,    -1,  1829,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   203,    -1,   122,  1843,   124,
    2073,    -1,    -1,    -1,    -1,    -1,   883,    -1,  1853,   106,
    1855,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,  1867,    -1,  1869,  1870,  1871,    -1,    -1,  2102,
     155,    -1,   240,    -1,    -1,   912,    -1,   134,    -1,   916,
      -1,    -1,    -1,    -1,   921,    -1,    -1,    -1,    -1,    -1,
      -1,   928,   929,    -1,    -1,    -1,    -1,   154,   155,    -1,
     268,    -1,    -1,    -1,   941,    -1,   943,   944,   165,   166,
      -1,   279,    -1,   104,   282,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,   297,
    1935,    -1,    -1,    -1,  1939,    -1,   304,    -1,    -1,  1944,
     308,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,   994,     1,    -1,
      -1,    -1,    -1,   154,    -1,  1970,   157,   158,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   349,    -1,    -1,   352,    -1,    -1,    -1,    -1,   154,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   365,    -1,    -1,
      -1,   369,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2014,
      -1,    -1,    -1,  2018,    -1,    -1,    59,   154,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2031,    -1,    -1,    -1,
      -1,    -1,  1069,    -1,    -1,    -1,  1073,    -1,    -1,    -1,
      -1,    -1,    -1,  1080,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1090,    -1,    -1,    -1,    -1,    -1,    -1,
    1097,   104,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,  2076,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1129,    -1,   137,    -1,    -1,    -1,    -1,    -1,
     468,    -1,    -1,    -1,   147,  1142,    -1,  2112,    -1,  1146,
    2115,    -1,    -1,  1150,    -1,    -1,    -1,  2122,    -1,    -1,
      -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1172,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1181,    -1,  2151,    -1,    -1,    -1,
    2155,    -1,  2157,  1190,    -1,    -1,    -1,    -1,    -1,    -1,
     203,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   540,  2178,    -1,    -1,    -1,    -1,  1215,    -1,
      -1,    -1,    -1,    -1,    -1,  1222,    -1,    -1,    -1,   557,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   240,    -1,    -1,
      -1,    -1,    -1,    -1,   572,    -1,    -1,    -1,  1215,  1246,
    1247,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   268,    -1,    -1,    -1,  1266,
      -1,    -1,    -1,    -1,    -1,    -1,   279,    -1,    -1,    -1,
    1277,    -1,    -1,    -1,  1281,    -1,   614,   615,    -1,   617,
      -1,    -1,    -1,    -1,   297,    -1,    -1,  1294,    -1,    -1,
      -1,   304,    -1,    -1,    -1,   308,   634,   635,    -1,    -1,
    1307,  1308,    -1,    -1,    -1,  1312,    -1,   645,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     658,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   671,    -1,    -1,   349,    -1,    -1,   352,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   365,    -1,    -1,    -1,   369,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1375,  1376,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1395,    -1,
      -1,    -1,  1399,    -1,    -1,    -1,  1403,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1416,
      -1,    -1,    -1,    -1,    -1,    -1,  1423,  1424,  1425,  1426,
    1427,  1428,  1429,    -1,    -1,    -1,    -1,    -1,  1435,    -1,
      -1,    -1,   770,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1451,  1452,    -1,    -1,   786,    -1,
      88,    -1,    -1,    -1,    -1,   468,    -1,    -1,  1465,    -1,
      -1,    -1,    -1,   801,    -1,    -1,    -1,   805,    -1,    -1,
      -1,  1448,    -1,   811,  1451,  1452,   814,    -1,    -1,    -1,
    1457,    -1,    -1,    -1,  1461,    -1,  1463,  1494,  1465,    -1,
     128,    -1,    -1,    -1,    -1,   833,   834,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1517,    -1,    -1,    -1,   852,  1522,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   540,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1543,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   572,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1576,
      -1,    -1,    -1,    -1,   912,    -1,  1583,    -1,   916,    -1,
    1587,    -1,    -1,   921,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   614,   615,   941,   617,    -1,   944,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   634,   635,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   645,  1640,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1620,    -1,   658,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   994,    -1,   671,    -1,
      -1,    -1,    -1,    -1,    -1,  1672,  1673,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   316,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1668,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1709,    -1,    -1,    -1,  1683,  1684,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1727,    -1,    -1,    -1,  1731,    -1,    -1,    -1,    -1,    -1,
      -1,  1069,  1709,    -1,    -1,  1073,    -1,    -1,  1715,    -1,
      -1,    -1,  1080,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1090,    -1,    -1,    -1,  1763,   770,    -1,  1097,
      48,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   786,  1781,  1782,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,   801,    -1,
      -1,  1129,   805,    -1,    -1,    -1,    -1,    -1,   811,    -1,
      -1,   814,    -1,    -1,  1142,    -1,    -1,    -1,  1146,    -1,
      -1,    -1,  1150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     833,   834,    -1,    -1,  1831,    -1,    -1,    -1,    -1,   467,
      -1,   469,    -1,   121,    -1,    -1,    -1,  1844,    -1,   852,
     478,   479,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
    1827,    -1,  1190,    -1,    -1,    -1,    -1,    -1,    -1,  1836,
      -1,  1838,    -1,    -1,  1841,  1842,    -1,  1844,    -1,    -1,
      -1,    -1,  1849,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,    -1,  1222,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   912,
      -1,    -1,    -1,   916,   192,    -1,    -1,    -1,   921,  1247,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   941,    -1,
      -1,   944,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1277,
      -1,    -1,   230,    -1,    -1,    -1,   234,    -1,    -1,   237,
     238,    -1,    -1,   241,    -1,    -1,   244,   245,  1965,   247,
      -1,   249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1307,
    1308,    -1,    -1,  1950,    -1,   182,    -1,    -1,  1955,  1956,
      -1,   994,    -1,    -1,    -1,    -1,    -1,    -1,  1995,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1976,
      -1,    -1,    -1,    -1,  2011,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   319,  2039,  2040,   322,    -1,  2043,  1375,  1376,    -1,
      -1,    -1,  2019,    -1,  2021,    -1,    -1,  2024,  2025,    -1,
      -1,    -1,  2029,  2030,    -1,  2062,  1069,  1395,   346,   347,
    1073,  1399,    -1,    -1,    -1,  1403,  2073,  1080,    -1,    -1,
      -1,    -1,    -1,   361,    -1,    -1,    -1,  1090,    -1,    -1,
      -1,    -1,    -1,    -1,  1097,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1451,  1452,    -1,  1129,  2094,  2095,  2096,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1465,    -1,  1142,
      -1,    -1,    -1,  1146,    -1,    -1,    -1,  1150,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2128,  2129,  2130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   457,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1190,    -1,  1517,
      -1,   388,    -1,    -1,  1522,   392,   393,    -1,    -1,   827,
      -1,    -1,    -1,    -1,    -1,   402,   403,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1222,
     417,   418,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   512,    -1,    -1,    -1,    -1,    -1,
      -1,   438,    -1,    -1,  1247,     1,    -1,    -1,  1576,    -1,
     528,    -1,    -1,    -1,    -1,  1583,    -1,    -1,    -1,  1587,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   468,    -1,    -1,  1277,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   919,   920,    49,    -1,   923,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,  1307,  1308,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   611,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1673,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,  1375,  1376,    -1,   653,   654,    -1,   134,    -1,
    1008,  1709,    -1,    -1,    -1,    -1,    -1,    -1,   666,    -1,
      -1,    -1,  1395,    -1,    -1,    -1,  1399,    -1,   154,  1727,
    1403,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     0,    -1,    -1,     3,    -1,  1056,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1451,  1452,
      -1,    -1,    -1,  1781,  1782,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1465,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1103,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1112,  1113,  1114,  1115,    -1,    -1,
      -1,    -1,  1120,  1121,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    78,  1130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   790,   791,  1517,    -1,  1844,    -1,   796,  1522,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1158,    -1,    -1,  1161,    -1,    -1,    -1,    -1,    -1,   817,
      -1,    -1,   820,   821,    -1,   823,    -1,   825,   826,    -1,
     747,   748,   749,   750,   751,   752,   753,   754,   755,   756,
     757,   758,   759,   760,   761,   762,   763,   764,   765,    -1,
      -1,    -1,    -1,  1576,    -1,    -1,    -1,    -1,    -1,    -1,
    1583,    -1,    -1,    -1,  1587,    -1,    -1,    -1,   866,    -1,
      -1,    -1,   870,    -1,  1222,    -1,   874,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1253,    -1,    -1,    -1,    -1,
     827,    -1,    -1,  1261,    -1,  1263,  1264,  1965,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1276,    -1,
    1278,    -1,  1280,    -1,    -1,    -1,    -1,    -1,    -1,  1287,
     938,   939,    -1,    -1,    -1,    -1,    -1,  1995,    -1,    -1,
    1673,    -1,    -1,    -1,   952,    -1,    -1,    -1,    -1,    -1,
      -1,   258,    -1,  2011,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1709,    -1,    -1,    -1,
      -1,  2039,  2040,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   298,    -1,    -1,  1727,    -1,   166,    -1,    -1,    -1,
      -1,    -1,    -1,   310,  2062,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2073,    -1,    -1,    -1,    -1,
     327,    -1,    -1,    -1,  1382,    -1,    -1,    -1,    -1,    -1,
      -1,  1389,   202,   203,    -1,  1393,    -1,    -1,    -1,    -1,
      -1,   348,    -1,    -1,    -1,    -1,    -1,    -1,  1781,  1782,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1419,    -1,   990,   234,    -1,    -1,    -1,   995,    -1,
      -1,   241,    -1,    -1,    -1,    -1,    -1,    -1,  1086,  1006,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   405,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1844,    -1,    -1,    -1,    -1,    -1,  1475,    -1,  1127,
      -1,    -1,    -1,  1050,    -1,  1133,    -1,    -1,  1136,   436,
      -1,    -1,  1140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   322,    -1,    -1,   462,    -1,    -1,    -1,   466,
      -1,    -1,    -1,  1521,    -1,    -1,    -1,    -1,    -1,    -1,
    1528,    -1,  1530,    -1,    -1,    -1,    -1,    -1,   485,   349,
     350,    -1,   489,   490,    -1,    -1,   493,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   369,
      -1,   508,   509,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   531,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1965,    -1,    -1,    -1,    -1,    -1,  1596,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1605,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1995,  1190,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   441,    -1,    -1,    -1,  1283,    -1,    -1,  2011,    -1,
      -1,    -1,    -1,    -1,  1292,  1293,    -1,   457,   458,    -1,
     460,   461,    -1,    -1,    -1,    -1,    -1,    -1,   468,    -1,
     607,    -1,   472,    -1,    -1,    -1,  2039,  2040,    -1,    -1,
      -1,    -1,    -1,    -1,   621,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    -1,  2062,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   646,
    2073,    -1,    -1,   513,  1352,    -1,    -1,   517,    -1,    -1,
    1277,    -1,    -1,  1361,    -1,    -1,  1364,    -1,  1366,  1367,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1297,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1743,  1744,   557,  1315,  1316,
    1317,    -1,    -1,   150,    -1,  1322,  1323,    -1,    -1,  1757,
      -1,    -1,    -1,  1411,    -1,    -1,    -1,    -1,    -1,   166,
     717,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1346,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   190,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   612,    -1,    -1,   615,   203,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1383,  1384,    -1,    -1,
      -1,    -1,    -1,    -1,   634,   635,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   645,    -1,    -1,    -1,   649,
      -1,    -1,    -1,    -1,    -1,    -1,   656,  1495,   658,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   259,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   836,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,
      -1,    -1,    -1,    -1,   851,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1911,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   328,    -1,    -1,  1582,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   896,
      -1,    -1,   349,   900,    -1,    -1,    -1,   904,    -1,    -1,
     770,    -1,    -1,  1611,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   786,   787,   925,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   796,   797,    -1,   799,
     800,    -1,    -1,    -1,    -1,    -1,  1644,    -1,    -1,    -1,
      -1,   811,  1650,    -1,   814,    -1,   816,   817,    -1,    -1,
      -1,    -1,   112,   823,    -1,    -1,    84,    -1,    -1,    -1,
      -1,    -1,    -1,   833,   834,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,    -1,   984,    -1,    -1,
      -1,   438,   852,  2041,    -1,   855,    -1,    -1,    -1,   859,
      -1,    -1,    -1,    -1,    -1,    -1,   866,   867,    -1,    -1,
     870,   871,    -1,    -1,   874,   875,   166,    -1,    -1,    -1,
      -1,   468,   882,    -1,    -1,    -1,    -1,    -1,  1726,    -1,
      -1,    -1,   150,    -1,    -1,    -1,   154,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,
      -1,  2099,    -1,   203,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   921,   922,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   190,    -1,    -1,  2123,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   203,    -1,    -1,   206,    -1,
    2138,    -1,    -1,    -1,   954,    -1,    -1,  1714,    -1,    -1,
      -1,    -1,    -1,    -1,   551,   552,    -1,  1805,  1806,  1106,
     557,  1108,    -1,    -1,    -1,  1813,    -1,    -1,    -1,    -1,
    1818,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   994,    -1,    -1,    -1,    -1,    -1,
      -1,   259,    -1,    -1,    -1,    -1,    -1,   297,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   615,    -1,
      -1,  1168,    -1,    -1,    -1,    -1,  1173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   635,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   349,
     318,   351,   352,  1063,    -1,    -1,    -1,    -1,    -1,    -1,
     328,   658,    -1,    -1,    -1,   365,    -1,    -1,    -1,   369,
    1080,  1081,    -1,    -1,  1922,    -1,    -1,  1087,    -1,    -1,
      -1,   349,    -1,   351,    -1,    -1,   683,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1129,
      -1,    -1,    -1,  1133,  1134,    -1,  1136,  1137,  1275,    -1,
    1140,  1141,    -1,    -1,    -1,    -1,   404,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1997,
      -1,    -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1931,    -1,    -1,    -1,   468,    -1,
     438,    -1,    -1,   770,   474,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   794,    -1,    -1,
     468,    -1,    -1,    -1,    -1,  1972,   474,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   811,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   833,   834,    -1,    -1,
     540,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2101,    -1,   852,    -1,   557,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1277,    -1,    -1,
      -1,    -1,    -1,  1283,  1284,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   551,   552,    -1,    -1,    -1,    -1,   557,
      -1,   591,    -1,    -1,    -1,    -1,    -1,    -1,  1308,    -1,
    2067,    -1,  2069,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   614,   615,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   921,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   929,    -1,   634,   635,    -1,   637,    -1,    -1,
      -1,  2108,  1352,  1353,    -1,   645,    -1,   615,    -1,    -1,
      -1,  1361,  1362,  1500,  1364,    -1,    -1,  1504,   658,    -1,
      -1,   661,    -1,    -1,    -1,  1375,  1376,   635,   668,   637,
      -1,   671,    -1,  2140,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     658,    -1,    -1,    -1,    -1,    -1,    -1,   994,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   683,    -1,  2184,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   696,    -1,
      -1,   699,   700,    -1,   702,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   711,    -1,    -1,   714,   715,   716,    -1,
      -1,    -1,    -1,    -1,  1601,  1602,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     770,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,
    1627,    -1,    -1,    -1,    -1,    -1,   786,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   801,   770,    -1,    -1,   805,    -1,  1517,    -1,    -1,
      -1,   811,    -1,    -1,   814,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   794,    -1,    -1,    -1,
      -1,    -1,  1129,   833,   834,  1545,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   811,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   852,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   833,   834,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   852,  1732,  1733,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   237,   238,    -1,    -1,   241,    -1,
      -1,   244,   245,    -1,   247,    -1,   249,    -1,    -1,    -1,
      -1,    -1,   912,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   921,    -1,    -1,  1634,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1650,   941,    -1,   943,   944,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   921,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   929,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1266,
      -1,   166,    -1,    -1,    -1,   943,    -1,    -1,    -1,    -1,
    1277,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   994,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   346,   347,    -1,    -1,    -1,   203,    -1,
      -1,  1308,  1859,    -1,    -1,    -1,    -1,  1727,   361,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   994,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1894,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1909,    -1,    -1,    -1,    -1,    -1,    -1,  1069,
      -1,  1781,  1782,  1073,    -1,    -1,    -1,    -1,  1375,  1376,
    1080,    -1,    -1,    -1,    -1,    -1,  1933,    -1,  1798,  1799,
    1090,    -1,    -1,    -1,    -1,    -1,    -1,  1097,    -1,    -1,
      -1,    -1,   297,    -1,  1814,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1960,   457,    -1,    -1,  1964,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1129,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      -1,    56,  1142,    -1,    -1,    -1,  1146,    -1,    -1,    -1,
    1150,    -1,    -1,    -1,   349,    -1,   351,   352,    73,    -1,
      -1,  1129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     365,    -1,    -1,    -1,   369,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   528,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
    1920,   126,   127,  1181,    -1,    -1,    -1,    -1,  1928,   134,
    1517,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,   149,   150,    -1,    -1,    -1,   154,
     155,    -1,   157,   158,    -1,    -1,    -1,  1215,    -1,   164,
     165,   166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   468,    -1,    -1,    -1,  1277,  1246,   474,
      -1,  1281,    -1,    -1,    -1,  1995,    -1,  1997,  1998,    -1,
      -1,  2001,    -1,    -1,    -1,    -1,    -1,    -1,  1266,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1307,  1308,  1277,
     653,   654,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   666,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2042,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1308,    -1,    -1,  1640,  1312,   540,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1375,  1376,    -1,    -1,    -1,
      -1,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2101,  2102,    -1,    -1,  1395,   591,    -1,    -1,  1399,
      -1,    -1,    -1,  1403,    -1,    -1,    -1,  1375,  1376,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   203,   614,
     615,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1727,    -1,  2142,    -1,    -1,    -1,    -1,    -1,    -1,   634,
     635,    -1,   637,    -1,    -1,    -1,    -1,   790,   791,    -1,
     645,    -1,    -1,   796,    -1,  1423,  1424,  1425,    -1,    -1,
    1428,  1429,    -1,   658,    -1,    -1,   661,  1435,    -1,    -1,
      -1,    -1,    -1,   668,   817,    -1,   671,   820,   821,    -1,
     823,    -1,   825,   826,  1781,  1782,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   297,    -1,    -1,    -1,    -1,  1517,    -1,    -1,
      -1,    -1,  1522,   866,    -1,    -1,  1494,   870,    -1,    -1,
      -1,   874,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1517,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   349,    -1,   351,   352,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   770,  1576,    -1,    -1,    -1,
     365,    -1,    -1,  1583,   369,    -1,    -1,  1587,    -1,    -1,
      -1,   786,    -1,    -1,    -1,   938,   939,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   801,    -1,    -1,   952,
     805,    -1,    -1,    -1,    -1,    -1,   811,    -1,    -1,   814,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   833,   834,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   852,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   468,    -1,    -1,    -1,    -1,    -1,   474,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1995,    -1,
      -1,    -1,    -1,    -1,  1672,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   912,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   921,  1727,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1086,    -1,    -1,   941,    -1,   943,   944,
      -1,    -1,    -1,    -1,    -1,   540,    -1,    -1,    -1,  1727,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1781,  1782,    -1,  1127,    -1,    -1,    -1,    -1,    -1,
    1133,    -1,    -1,  1136,    -1,  1763,    -1,  1140,    -1,   994,
      -1,    -1,    -1,    -1,    -1,    -1,   591,    -1,    -1,    -1,
      -1,    -1,    -1,  1781,  1782,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   614,
     615,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   634,
     635,    -1,   637,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     645,    -1,    -1,  1831,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   658,  1069,    -1,   661,    -1,  1073,    -1,
      -1,    -1,    -1,   668,    -1,  1080,   671,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1090,    -1,    13,    14,    15,
      16,    17,  1097,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,   190,    -1,  1129,    51,    -1,    53,    -1,    -1,
    1283,    -1,    -1,    -1,    -1,   203,    -1,  1142,    -1,  1292,
    1293,  1146,    -1,    -1,    -1,  1150,    72,    -1,   216,    -1,
     218,    -1,    -1,    -1,    -1,  1965,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   770,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1995,    -1,  1965,    -1,    -1,
      -1,   786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1352,
      -1,    -1,    -1,    -1,    -1,    -1,   801,    -1,  1361,    -1,
     805,  1364,    -1,  1366,  1367,    -1,   811,  1995,    -1,   814,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,
    2040,    -1,    -1,  2043,    -1,    -1,    -1,    -1,   833,   834,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   317,
      -1,    -1,  2062,    -1,    -1,    -1,    -1,   852,  1411,    -1,
      -1,    -1,  2040,    -1,    -1,  2043,    -1,    -1,    -1,    -1,
      -1,    -1,  1277,    -1,   103,   104,  1281,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,  1307,  1308,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   912,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,   921,    -1,   157,   158,
      -1,    -1,    -1,   162,    -1,   164,   165,   166,   167,   168,
     169,   170,  1495,    -1,    -1,    -1,   941,    -1,   943,   944,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1375,  1376,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1395,    -1,    -1,    -1,  1399,    -1,    -1,    -1,  1403,   994,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   494,    -1,    -1,  1582,
      -1,    -1,   500,    -1,    -1,    -1,    -1,   505,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1611,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1069,    -1,    -1,    -1,  1073,    -1,
      -1,    -1,    -1,    -1,    -1,  1080,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1090,    -1,    -1,    -1,    -1,
      -1,    -1,  1097,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1517,    -1,    -1,    -1,    -1,  1522,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1129,    -1,    -1,   605,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1142,    -1,    -1,
      -1,  1146,    -1,    -1,    -1,  1150,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,
      -1,  1576,    -1,  1726,    -1,    -1,    -1,    -1,  1583,    -1,
     648,    -1,  1587,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    -1,    -1,    -1,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,   704,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1805,  1806,    -1,    -1,    -1,    -1,    -1,    -1,
     728,   729,    -1,    -1,   732,  1818,   734,    -1,    -1,    -1,
      -1,    -1,   740,    -1,   742,   743,    -1,    -1,    -1,    -1,
      -1,    -1,  1277,    -1,    -1,    -1,  1281,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   770,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1307,  1308,    -1,   783,    -1,    -1,    -1,    -1,
      -1,    -1,  1727,    -1,    -1,    -1,   794,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,    -1,
     808,    -1,    -1,   811,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1922,
     838,    -1,    -1,   841,    -1,    -1,  1781,  1782,    -1,    -1,
    1375,  1376,    -1,    -1,    -1,    -1,   854,     5,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
    1395,    -1,    -1,    -1,  1399,    -1,    -1,    -1,  1403,    -1,
      -1,    -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,  1997,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,   929,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   943,   944,    -1,    -1,    -1,
      -1,    -1,    -1,   951,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,   980,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,  1517,    -1,    -1,    -1,   994,  1522,    -1,    -1,
      -1,    -1,    -1,    -1,  1002,    -1,   154,    -1,    -1,   157,
     158,  1009,    -1,    -1,    -1,    -1,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1965,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1576,    -1,    -1,    -1,    -1,    -1,    -1,  1583,  1057,
    1995,     1,  1587,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2040,    -1,    -1,  2043,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1124,  2062,  1126,    -1,
    1128,    71,    -1,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
      -1,   101,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1727,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1209,  1210,   153,   154,    -1,    -1,   157,   158,    -1,
      -1,    -1,   162,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1781,  1782,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,  1281,    -1,    -1,    -1,    -1,    -1,  1287,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1308,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1325,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,  1340,    -1,    -1,  1343,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
      -1,   101,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,  1415,  1416,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,   154,    -1,    -1,   157,   158,    -1,
    1965,    -1,   162,  1441,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,
      -1,  1459,    -1,    -1,  1462,    -1,    -1,    -1,    -1,    -1,
    1995,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,
      -1,    -1,    -1,    -1,    -1,  2040,    -1,    -1,  2043,  1517,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1526,  1527,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2062,    -1,  1537,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1556,    -1,
    1558,    -1,    71,    -1,    73,    74,    -1,    76,    -1,    -1,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,    -1,   101,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1640,    -1,    -1,   154,    -1,  1645,   157,   158,
      -1,    -1,    -1,   162,    -1,   164,   165,   166,   167,   168,
     169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1704,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,  1761,    -1,    49,  1764,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    71,    72,    73,
      74,    -1,    76,  1791,    -1,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
     154,    -1,    -1,   157,   158,    -1,    -1,    -1,   162,    -1,
     164,   165,   166,   167,   168,   169,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    71,    72,
      73,    74,    -1,    76,    -1,  1973,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,    -1,    -1,   157,   158,    -1,    -1,    -1,   162,
      -1,   164,   165,   166,   167,   168,   169,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   179,     3,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,   149,   150,    -1,    -1,    -1,   154,   155,
     156,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   179,     3,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,   150,    -1,    -1,    -1,   154,   155,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,
     169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     179,     3,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,    -1,   156,   157,   158,    -1,    -1,    -1,
      -1,    -1,   164,   165,   166,   167,   168,   169,   170,     3,
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
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,     4,     5,     6,
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
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,
     167,   168,   169,   170,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,   169,
     170,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,   164,   165,   166,   167,   168,   169,   170,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,     1,    -1,     3,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,    -1,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,    -1,   165,   166,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,     3,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,
     162,    -1,    -1,   165,   166,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    72,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,   106,    53,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,
     158,    -1,   106,    -1,   108,   109,    -1,   165,   166,     3,
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
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
      -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,    -1,    -1,    -1,   157,   158,    -1,
       3,    -1,     5,    -1,    -1,   165,   166,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,    -1,    -1,    -1,   157,   158,    -1,     3,    -1,     5,
      -1,    -1,   165,   166,    10,    -1,    -1,    13,    14,    15,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,    -1,
      -1,   157,   158,    -1,     3,    -1,     5,    -1,    -1,   165,
     166,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,    -1,    -1,    -1,   157,   158,
      -1,     3,    -1,     5,    -1,    -1,   165,   166,    10,    -1,
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
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   153,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,
      -1,    -1,    -1,   165,   166,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,     3,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,    72,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,   108,   109,    -1,    -1,    -1,    -1,   165,   166,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
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
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,   155,   156,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,
      -1,   162,    -1,    -1,   165,   166,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    72,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,   106,    53,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,
     158,    -1,   106,    -1,   108,   109,    -1,   165,   166,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,   161,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    13,    14,    15,    16,    17,    18,    72,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,   108,   109,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
      -1,   156,   157,   158,    -1,    -1,   108,   109,    -1,    -1,
     165,   166,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,   157,    -1,    -1,    -1,    51,
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
      -1,    -1,   154,    -1,   156,   157,   158,    -1,    -1,   108,
     109,    -1,    -1,   165,   166,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,   157,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,   158,
      -1,    -1,    -1,    -1,    -1,    -1,   165,   166,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,    -1,   165,   166,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
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
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    72,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,   108,   109,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      -1,    56,    -1,    -1,    -1,    -1,   157,   158,   106,    -1,
     108,   109,    -1,    -1,   165,   166,    71,    -1,    73,    74,
      -1,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    -1,    -1,    93,    94,
      95,    96,    97,    98,    99,    -1,   101,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      18,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
      -1,    49,   157,   158,    52,    -1,    54,   162,    56,   164,
     165,   166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,   179,    73,    74,    -1,    76,    -1,
      -1,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    -1,    -1,    93,    94,    95,    96,    97,
      98,    99,    -1,   101,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,
     158,    -1,    -1,    -1,   162,    -1,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   179,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,   157,    51,    -1,    53,
      -1,   162,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,   157,    51,    -1,    53,    -1,   162,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,   108,   109,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      -1,    -1,    -1,    -1,    90,    -1,    92,    -1,    -1,    -1,
     157,    -1,    -1,    -1,    -1,   162,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,
      -1,   157,   158,    -1,    -1,    -1,   162,    -1,   164,   165,
     166,   167,   168,   169,   170,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,
     158,    -1,    -1,    -1,   162,    -1,   164,   165,   166,   167,
     168,   169,   170,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,    -1,   156,   157,   158,    -1,
      -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,   169,
     170,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,    -1,    -1,   157,   158,    -1,    -1,    -1,
     162,    -1,   164,   165,   166,   167,   168,   169,   170,    13,
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
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,    13,    14,    15,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    13,    14,    15,    16,    17,
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
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,
     158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,
     168,   169,   170,     3,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,
     162,    -1,    -1,   165,   166,    13,    14,    15,    16,    17,
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
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,
     158,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,    13,
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
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,   156,   157,    20,    -1,    22,
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
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,   157,    22,    23,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,   157,   158,    -1,    -1,
      -1,    -1,    -1,    -1,   165,   166,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
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
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,   108,   109,
      -1,    -1,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,    -1,   165,   166,    13,    14,    15,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,   108,   109,    -1,    -1,   165,
     166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,    -1,   108,   109,    -1,    -1,   165,   166,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,    -1,   165,   166,    13,    14,    15,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,   108,   109,    -1,    -1,   165,
     166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,
      -1,    -1,   165,   166,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
      -1,    13,    14,    15,    16,    17,   165,   166,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      72,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,   108,   109,    -1,
      -1,    -1,    -1,   165,   166,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    -1,    -1,    -1,    -1,   157,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
      -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,
     165,   166,   167,   168,   169,   170,    20,    -1,    22,    23,
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
     154,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      49,    -1,    -1,    52,    -1,    54,    72,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    49,
      -1,    -1,    52,    -1,    54,   134,    56,    -1,    -1,    -1,
      -1,   157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,   150,    -1,    73,    -1,   154,   155,    -1,   157,   158,
      -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,
     169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,
      -1,    -1,   162,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     154,   155,    -1,   157,   158,    -1,    -1,    -1,   162,    -1,
     164,   165,   166,   167,   168,   169,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,
     158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   154,    -1,    -1,   157,   158,    -1,    -1,    -1,
     162,    -1,   164,   165,   166,   167,   168,   169,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,   155,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,
      -1,   161,    -1,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,
     158,    -1,    -1,    -1,   162,    -1,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   154,    -1,    -1,   157,   158,    -1,    -1,    -1,
     162,    -1,   164,   165,   166,   167,   168,   169,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,    -1,
     156,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,
     158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,
      -1,    -1,   164,   165,   166,   167,   168,   169,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,   155,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,
     158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,
      -1,    -1,   164,   165,   166,   167,   168,   169,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,   155,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   154,   155,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     154,   155,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   154,   155,    -1,   157,
     158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   154,   155,    -1,   157,   158,    -1,    -1,    -1,
      -1,    -1,   164,   165,   166,   167,   168,   169,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,   155,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   154,    -1,    -1,   157,
     158,    -1,    -1,    -1,    -1,    -1,   164,   165,   166,   167,
     168,   169,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    49,   126,   127,    52,    -1,    54,    -1,
      56,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   154,    -1,    -1,   157,   158,    -1,    -1,    -1,
      -1,    -1,   164,   165,   166,   167,   168,   169,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   154,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   164,   165,
     166,   167,   168,   169,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    49,   126,   127,    52,    -1,
      54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   154,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,    -1,   164,   165,   166,   167,   168,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
     164,   165,   166,   167,   168,   169,   170,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,    72,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   181,   405,   406,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
     101,   105,   106,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   121,   134,   154,   155,   157,   158,   165,
     166,   184,   185,   186,   201,   293,   294,   295,   296,   297,
     298,   299,   300,   301,   302,   303,   304,   307,   310,   312,
     313,   314,   315,   316,   317,   318,   319,   320,   322,   324,
     325,   326,   328,   329,   333,   334,   335,   336,   337,   339,
     345,   346,   347,   348,   359,   363,   397,   400,   410,   416,
     418,   424,   428,   433,   434,   435,   436,   437,   438,   439,
     440,   466,   484,   485,   486,   487,     0,   181,   106,   185,
     201,   297,   299,   310,   313,   316,   325,   329,   334,   120,
     154,    58,    61,    62,    64,   154,   154,   422,   423,   424,
     321,   322,   108,   109,   185,   377,   398,   399,   377,   154,
     410,   154,   154,     4,   106,   108,   109,   314,   319,   320,
     154,   201,   423,   428,   434,   435,   436,   438,   439,   440,
     108,   336,   159,   181,   300,   310,   313,   433,   437,   483,
     484,   487,   488,   179,   182,   151,   162,   178,   222,   380,
      89,   160,   417,   377,   182,   160,   160,   179,   108,   109,
     154,   201,   305,   306,   428,   429,   430,   431,   432,   433,
     437,   441,   442,   443,   444,   445,   446,   447,   448,   449,
     455,     3,    47,    48,    50,    55,   327,     3,   158,   201,
     299,   314,   318,   320,   330,   335,   413,   433,   437,   487,
      69,   297,   299,   313,   325,   329,   334,   414,   433,   437,
      65,   319,   319,   314,   320,   308,   319,   320,   327,   346,
     314,   319,   314,   157,   422,   160,   182,   154,   162,   230,
     422,   422,     3,   288,   289,   304,   307,   313,   317,   318,
     158,   310,   313,   485,   377,   377,   410,   178,   313,   154,
     201,   419,   428,   429,   433,   442,   446,   158,   201,   487,
     411,   412,    57,    65,    66,    67,    68,   158,   176,   377,
     386,   388,   392,   394,   395,   335,    57,   156,   158,   201,
     309,   313,   317,   324,   325,   331,   332,   333,   334,   338,
     345,   346,   363,   373,   375,   466,   479,   480,   481,   482,
     487,   488,   108,   109,   162,   169,   185,   335,   455,   424,
     154,   393,   394,   154,   154,   120,   187,   188,    49,    52,
      54,    56,    73,   103,   104,   106,   107,   118,   119,   122,
     123,   124,   126,   127,   154,   158,   164,   167,   168,   169,
     170,   183,   184,   187,   189,   192,   200,   201,   202,   203,
     206,   207,   208,   209,   210,   211,   212,   213,   214,   215,
     216,   217,   218,   224,   335,   156,   158,   200,   201,   217,
     219,   310,   335,   378,   379,   396,   483,   488,   313,   434,
     435,   436,   438,   439,   440,   156,   156,   156,   156,   156,
     156,   156,   158,   310,   466,   485,   158,   165,   201,   219,
     299,   300,   309,   311,   313,   325,   332,   334,   368,   369,
     372,   373,   374,   479,   487,   154,   433,   437,   487,   154,
     160,   106,   157,   158,   162,   184,   186,   219,   381,   382,
     383,   384,   385,    22,   381,   154,   377,   230,   154,   160,
     419,   185,   423,   428,   430,   431,   432,   441,   443,   444,
     445,   447,   448,   449,   313,   429,   442,   446,   160,   101,
     421,   158,   422,   463,   466,   421,   422,   422,   417,   288,
     154,   422,   463,   421,   422,   422,   417,   422,   422,   313,
     419,   154,   154,   312,   313,   310,   313,   181,   310,   483,
     488,   337,   162,   417,   288,   377,   377,   380,   299,   318,
     415,   433,   437,   162,   417,   288,   398,   313,   325,   313,
     313,   108,   336,   108,   109,   185,   335,   340,   398,   136,
     185,   313,   370,   371,   374,   375,   376,   153,   181,   230,
     304,   179,   433,   446,   313,   181,   421,   154,   421,   182,
     219,   423,   428,   313,   154,   181,   377,   408,   162,   154,
     377,   162,   377,   136,   165,   166,   391,   156,   160,   377,
     395,   156,   422,   422,   159,   181,   311,   313,   325,   332,
     334,   478,   479,   487,   488,   154,   158,   166,   178,   201,
     466,   468,   469,   470,   471,   472,   473,   490,   201,   338,
     487,   313,   332,   319,   314,   422,   156,   311,   313,   480,
     311,   466,   480,    10,   164,   169,   362,   364,   365,   162,
     360,   362,   386,   178,   386,    13,    88,   106,   108,   109,
     184,   425,   426,   427,   156,   120,   154,   200,   154,   154,
     154,   203,   154,   200,   154,   106,   108,   109,   314,   319,
     320,   154,   200,   200,    19,    21,    85,   158,   167,   168,
     204,   205,   219,   226,   230,   348,   378,   487,   160,   181,
     154,   189,   158,   163,   158,   163,   123,   125,   126,   127,
     154,   157,   158,   162,   163,   203,   203,   171,   165,   172,
     173,   167,   168,   128,   129,   130,   131,   174,   175,   132,
     133,   166,   164,   176,   134,   135,   177,   156,   160,   157,
     181,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   178,   221,   222,   223,   154,   201,   459,   460,
     461,   462,   463,   156,   160,   156,   156,   156,   156,   156,
     156,   154,   422,   463,   466,   154,   463,   466,   181,   310,
     485,   181,   182,   182,   154,   166,   201,   428,   450,   451,
     452,   453,   454,   455,   456,   457,   458,   136,   487,   182,
     182,   377,   377,   181,   181,   181,   158,   186,   181,   382,
     161,   160,   489,   381,   157,   158,   161,   385,   155,   219,
     225,   154,   181,   181,   178,   428,   430,   431,   432,   441,
     443,   444,   445,   447,   448,   449,   156,   156,   156,   156,
     156,   156,   156,   156,   156,   156,   429,   442,   446,   422,
     154,   178,   159,   181,   380,   230,   417,   370,   380,   230,
     419,   226,   379,   226,   379,   419,   408,   230,   417,   421,
     162,   162,   417,   288,   408,   230,   417,   342,   343,   341,
     162,   156,   160,   156,   160,    70,   290,   291,   179,   165,
     219,   181,   428,   369,   410,   408,   377,   159,   181,   154,
     390,   388,   389,    78,   323,   185,   162,   169,   185,   455,
     311,   466,   480,   313,   317,   487,   370,   469,   470,   471,
     159,   181,    18,   219,   313,   468,   490,   422,   422,   466,
     311,   478,   488,   313,   185,   422,   311,   480,   335,   160,
     489,   377,   364,   362,   162,   156,   379,   156,   156,   160,
     154,   179,   378,   189,   158,   378,   378,   378,   219,   378,
     156,   378,   378,   378,   181,   156,   167,   168,   205,    18,
     315,   156,   160,   156,   165,   166,   156,   225,   219,   162,
     219,   185,   219,   185,   118,   158,   185,   155,   193,   194,
     195,   219,   108,   109,   118,   158,   185,   348,   219,   193,
     185,   203,   206,   206,   206,   207,   207,   208,   208,   209,
     209,   209,   209,   210,   210,   211,   212,   213,   214,   215,
     161,   226,   179,   187,   158,   185,   219,   162,   219,   370,
     460,   461,   462,   313,   459,   422,   422,   219,   379,   154,
     422,   463,   466,   154,   463,   466,   370,   370,   159,   159,
     154,   428,   451,   452,   453,   456,    18,   313,   450,   454,
     154,   422,   472,   490,   422,   422,   490,   154,   422,   472,
     422,   422,   182,   218,   377,   159,   160,   159,   160,   490,
     490,   136,   367,   368,   369,   367,   377,   181,   217,   218,
     219,   420,   489,   381,   383,   153,   181,   156,   160,   181,
     367,   185,   219,   156,   156,   156,   156,   156,   156,   156,
     156,   156,   154,   422,   463,   466,   154,   422,   463,   466,
     154,   422,   463,   466,   419,   187,    22,   466,   219,   320,
     335,   464,   230,   156,   156,   156,   156,   156,   406,   407,
     230,   153,   181,   408,   230,   417,   407,   230,   162,   162,
     162,   349,   136,   374,   375,   185,   292,   377,    18,    71,
      73,    76,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    93,    94,    95,    96,    97,    98,
      99,   101,   108,   109,   121,   154,   181,   226,   227,   228,
     229,   230,   231,   232,   234,   235,   245,   251,   252,   253,
     254,   255,   256,   261,   262,   265,   266,   267,   268,   269,
     270,   271,   277,   278,   279,   293,   313,   317,   377,   418,
      70,   182,   182,   367,   182,   409,   407,   156,   297,   299,
     310,   401,   402,   403,   404,   396,   178,   387,   387,   364,
     162,   422,   422,   311,   480,   158,   165,   201,   219,   335,
     219,   313,   156,   156,   156,   156,     5,   313,   422,   468,
     162,   169,   185,   455,    10,   365,   153,   178,   366,   489,
     162,   364,   162,   156,   426,   193,   156,   160,   181,   160,
     156,   156,   160,   156,   203,   156,   156,   156,   203,    18,
     315,   219,   156,   156,   155,   162,   203,   159,   182,   193,
     159,   159,   118,   122,   124,   186,   196,   197,   198,   156,
     160,   196,   159,   160,   153,   217,   161,   156,   196,   182,
     382,   156,   156,   156,   156,   459,   370,   370,   156,   156,
     456,   156,   156,   156,   156,   154,   428,   455,   450,   454,
     370,   370,   159,   182,   490,   181,   181,   182,   182,   182,
     182,   380,   196,   136,   170,   182,   182,   153,   381,   219,
     422,   155,   219,   367,   182,   154,   422,   463,   466,   154,
     422,   463,   466,   154,   422,   463,   466,   370,   370,   370,
     421,   156,   148,   170,   182,   465,   160,   182,   409,   401,
     407,   230,   409,   349,   349,   349,     3,     5,    10,    73,
     153,   294,   301,   302,   310,   313,   350,   355,   483,   160,
     179,   154,    61,    62,   179,   230,   293,   418,   154,    18,
     228,   154,   154,   179,   377,   179,   377,   165,   377,   162,
     227,   154,   154,   154,   228,   154,   230,   219,   220,   220,
      14,   280,   256,   267,    74,   236,   179,   182,   232,    78,
     179,   377,    91,    92,   260,   264,   112,   135,   259,   111,
     134,   263,   259,   376,   313,   161,   292,   159,   159,   182,
     160,   409,   377,   419,   182,   179,   182,   179,   182,   156,
     379,   393,   393,   489,   364,   362,   362,   181,   182,   182,
     182,   219,   154,   422,   472,   466,   312,     5,   165,   182,
     219,   364,   162,   422,   422,   335,   377,   162,   218,   153,
     364,   489,   153,   181,   156,   309,   185,    78,   190,   191,
     378,   203,   203,   203,   203,   203,   162,   382,   160,   153,
     199,   158,   197,   199,   199,   159,   160,   125,   157,   195,
     159,   225,   217,   179,   159,   489,   154,   422,   463,   466,
     156,   156,   156,   154,   422,   463,   466,   154,   422,   472,
     428,   422,   422,   156,   156,   159,   369,   372,   372,   373,
     156,   160,   160,   156,   182,   218,   218,   159,   159,   182,
     182,   156,   370,   370,   370,   156,   156,   156,   380,   422,
     160,   219,   219,   320,   335,   159,   153,   182,   409,   153,
     153,   153,   153,   310,   310,   348,   356,   483,   310,   355,
     154,   344,   179,   179,   154,   161,   201,   351,   352,   358,
     428,   429,   442,   446,   160,   179,   377,   377,   193,   179,
     230,   179,   230,   226,    80,   156,   226,   237,   293,   295,
     298,   304,   313,   317,   148,   149,   150,   155,   156,   179,
     226,   246,   247,   248,   293,   179,   179,   226,   179,   382,
     179,   226,   225,   226,   246,   113,   114,   115,   116,   117,
     272,   274,   275,   179,   100,   179,    84,   154,   156,   154,
     182,   153,   179,   179,   154,   154,   228,   228,   256,   154,
     266,   256,   266,   230,   422,   179,   156,   181,   153,   391,
     153,   181,   160,   160,   153,   489,   162,   162,   159,   159,
     159,   182,   370,   219,   219,   182,   159,   182,   489,   364,
     361,   362,   366,   366,   382,   489,   153,   401,   467,   468,
     156,   161,   156,   160,   161,   382,   489,   225,   123,   196,
     197,   158,   197,   158,   197,   159,   153,   370,   370,   370,
     182,   181,   181,   159,   182,   156,   422,   156,   156,   156,
     226,   465,   153,   153,   344,   344,   344,   351,   154,   201,
     353,   354,   463,   474,   475,   476,   477,   179,   160,   179,
     351,   179,   396,   423,   428,   219,   313,   153,   160,   179,
     357,   358,   357,   357,   377,   156,   156,   154,   228,   156,
     226,   313,   148,   149,   150,   170,   179,   249,   250,   228,
     227,   179,   250,   156,   161,   226,   155,   226,   227,   248,
     179,   489,   156,   156,   156,   156,   230,   274,   275,   154,
     219,   154,   187,   237,   203,   257,   226,    75,   110,   258,
     260,    75,     1,   228,   422,   387,   402,   181,   181,   153,
     364,   364,   159,   156,   182,   182,   159,   159,   153,   489,
     362,   162,   489,   153,   182,   156,   219,   191,   219,   489,
     153,   159,   159,   196,   196,   156,   156,   156,   159,   160,
     136,   369,   136,   159,   159,   219,   179,   475,   476,   477,
     313,   474,   160,   179,   422,   422,   179,   156,   428,   422,
     228,    77,    78,   162,   240,   241,   242,   156,   226,    75,
     228,   226,   155,   226,    75,   179,   108,   155,   226,   227,
     248,   155,   226,   228,   247,   250,   250,   179,   226,   153,
     162,   242,   228,   228,   154,   181,   179,   187,   156,   161,
     156,   156,   160,   161,   156,   228,   154,   228,   228,   228,
     393,   377,   419,   489,   489,   159,   159,   153,   162,   364,
     153,   153,   153,   159,   159,   181,   182,   156,   156,   156,
     474,   422,   352,     1,   218,   238,   239,   420,     1,   161,
       1,   181,   228,   240,    75,   179,   156,   228,    75,   179,
     170,   170,   228,   227,   250,   250,   179,   108,   226,   170,
     170,    75,   155,   226,   155,   226,   227,   179,     1,   181,
     181,   276,   311,   313,   483,   161,   179,   158,   187,   281,
     282,   283,   228,   203,   193,   226,   259,   153,   153,   364,
     489,   372,   154,   422,   463,   466,   354,   136,     1,   160,
     161,   153,   286,   287,   293,   228,    75,   179,   228,   226,
     155,   155,   226,   155,   226,   155,   226,   227,   155,   226,
     155,   226,   228,   170,   170,   170,   170,   153,   286,   276,
     182,   154,   201,   419,   474,   185,   161,   106,   154,   156,
     161,   160,    75,   156,   156,    75,   255,   489,   153,   370,
     218,   238,   241,   243,   244,   293,   228,   170,   170,   170,
     170,   155,   155,   226,   155,   226,   155,   226,   243,   182,
     179,   273,   313,   281,   159,   218,   179,   281,   283,   228,
     228,    75,   153,   156,   228,   233,   182,   241,   155,   155,
     226,   155,   226,   155,   226,   182,   273,   217,   156,   161,
     187,   156,   156,   161,   228,     1,   228,   153,   233,   153,
     156,   230,   187,   284,   154,   179,   284,   230,   160,   161,
     218,   156,   187,   185,   285,   156,   179,   156,   160,   179,
     185
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   180,   181,   182,   183,   183,   183,   183,   183,   184,
     184,   184,   184,   184,   184,   184,   184,   185,   185,   186,
     186,   187,   188,   188,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   190,
     190,   191,   191,   192,   192,   192,   192,   192,   192,   192,
     192,   192,   192,   192,   192,   192,   192,   192,   192,   192,
     192,   192,   192,   192,   192,   192,   192,   192,   192,   193,
     193,   194,   194,   195,   195,   196,   196,   197,   197,   197,
     197,   197,   197,   197,   198,   198,   198,   199,   199,   200,
     200,   200,   200,   200,   200,   200,   200,   200,   200,   200,
     200,   200,   200,   201,   201,   201,   202,   202,   202,   202,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   204,
     204,   204,   204,   205,   205,   206,   206,   207,   207,   207,
     207,   208,   208,   208,   209,   209,   209,   210,   210,   210,
     210,   210,   211,   211,   211,   212,   212,   213,   213,   214,
     214,   215,   215,   216,   216,   217,   217,   217,   218,   219,
     219,   219,   220,   220,   221,   221,   222,   222,   223,   223,
     223,   223,   223,   223,   223,   223,   223,   223,   223,   224,
     224,   225,   225,   225,   225,   226,   226,   227,   227,   228,
     228,   228,   228,   228,   228,   228,   228,   228,   228,   228,
     228,   228,   228,   228,   228,   229,   229,   230,   230,   231,
     231,   232,   232,   232,   232,   232,   233,   233,   233,   234,
     235,   235,   235,   235,   235,   235,   235,   236,   236,   237,
     237,   237,   237,   238,   238,   238,   239,   239,   240,   240,
     240,   240,   240,   241,   241,   242,   243,   243,   244,   244,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   246,   246,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   248,
     248,   248,   249,   249,   250,   250,   250,   251,   251,   251,
     251,   251,   251,   251,   251,   251,   251,   251,   251,   251,
     251,   251,   251,   251,   251,   251,   251,   252,   252,   253,
     254,   255,   256,   256,   257,   257,   258,   259,   259,   260,
     260,   261,   261,   261,   261,   261,   261,   262,   263,   263,
     264,   265,   265,   266,   266,   267,   267,   267,   268,   269,
     270,   271,   271,   271,   272,   272,   273,   273,   274,   274,
     274,   274,   275,   276,   276,   276,   276,   276,   277,   278,
     278,   279,   279,   279,   279,   279,   280,   280,   281,   281,
     282,   282,   283,   283,   284,   284,   284,   285,   285,   286,
     286,   287,   287,   288,   288,   289,   289,   290,   290,   291,
     291,   292,   292,   293,   293,   293,   294,   294,   295,   295,
     295,   295,   295,   296,   296,   296,   297,   297,   297,   298,
     298,   298,   298,   298,   299,   299,   300,   300,   301,   301,
     301,   302,   302,   302,   302,   302,   303,   303,   304,   304,
     304,   304,   305,   305,   305,   305,   305,   306,   306,   307,
     307,   307,   307,   308,   308,   308,   309,   309,   309,   310,
     310,   310,   311,   311,   311,   312,   312,   313,   313,   314,
     314,   315,   315,   315,   315,   315,   316,   317,   317,   317,
     318,   318,   319,   319,   319,   319,   319,   319,   319,   319,
     319,   320,   320,   320,   320,   320,   320,   320,   320,   320,
     320,   320,   320,   320,   320,   320,   320,   320,   320,   320,
     320,   320,   320,   320,   320,   320,   320,   320,   320,   321,
     321,   322,   323,   323,   324,   324,   324,   324,   324,   325,
     325,   326,   326,   326,   326,   327,   327,   327,   327,   327,
     327,   328,   328,   328,   328,   329,   330,   329,   329,   331,
     331,   331,   331,   332,   332,   332,   333,   333,   333,   333,
     334,   334,   334,   335,   335,   335,   335,   335,   335,   336,
     336,   336,   337,   337,   338,   338,   340,   339,   341,   339,
     342,   339,   343,   339,   339,   344,   344,   345,   345,   346,
     346,   347,   347,   347,   348,   348,   348,   348,   348,   348,
     348,   348,   349,   349,   350,   350,   350,   350,   350,   350,
     350,   350,   350,   350,   350,   350,   351,   351,   351,   352,
     352,   352,   352,   353,   353,   353,   354,   355,   355,   356,
     356,   357,   357,   358,   359,   359,   360,   359,   359,   359,
     359,   359,   359,   361,   359,   359,   359,   359,   359,   362,
     362,   363,   363,   364,   364,   364,   364,   365,   365,   366,
     366,   366,   367,   367,   367,   367,   367,   367,   367,   368,
     368,   368,   368,   369,   369,   370,   370,   370,   370,   371,
     371,   371,   371,   372,   372,   372,   372,   372,   373,   373,
     373,   373,   373,   374,   374,   375,   375,   376,   376,   377,
     377,   377,   378,   378,   378,   379,   379,   380,   380,   380,
     380,   381,   381,   382,   382,   382,   382,   382,   383,   383,
     384,   384,   385,   385,   385,   385,   385,   386,   386,   387,
     387,   389,   388,   390,   388,   388,   388,   388,   391,   391,
     391,   391,   392,   392,   392,   392,   393,   393,   394,   394,
     395,   395,   396,   396,   396,   396,   397,   397,   397,   398,
     398,   399,   399,   400,   400,   400,   400,   401,   401,   402,
     402,   403,   403,   403,   404,   404,   405,   405,   406,   406,
     407,   407,   408,   409,   410,   410,   410,   410,   410,   410,
     410,   410,   410,   410,   410,   411,   410,   412,   410,   413,
     410,   414,   410,   415,   410,   416,   416,   416,   417,   417,
     418,   418,   418,   418,   418,   418,   418,   418,   418,   418,
     419,   419,   419,   419,   420,   421,   421,   422,   422,   423,
     423,   424,   425,   425,   426,   426,   426,   427,   427,   427,
     427,   427,   427,   428,   428,   429,   429,   429,   429,   430,
     430,   430,   430,   431,   431,   431,   431,   431,   431,   431,
     432,   432,   432,   432,   433,   433,   433,   434,   434,   434,
     434,   434,   435,   435,   435,   435,   436,   436,   436,   436,
     436,   436,   437,   437,   437,   438,   438,   438,   438,   438,
     439,   439,   439,   439,   440,   440,   440,   440,   440,   440,
     441,   441,   442,   442,   442,   442,   443,   443,   443,   443,
     444,   444,   444,   444,   444,   444,   444,   445,   445,   445,
     445,   446,   446,   446,   447,   447,   447,   447,   447,   448,
     448,   448,   448,   449,   449,   449,   449,   449,   449,   450,
     450,   450,   450,   450,   451,   451,   451,   452,   452,   452,
     452,   453,   453,   453,   454,   454,   454,   454,   454,   455,
     455,   456,   456,   456,   457,   457,   458,   458,   459,   459,
     459,   460,   460,   460,   460,   460,   461,   461,   461,   461,
     462,   462,   462,   463,   463,   463,   463,   463,   464,   464,
     464,   464,   464,   464,   465,   465,   466,   466,   466,   466,
     467,   467,   468,   468,   468,   468,   469,   469,   469,   469,
     469,   470,   470,   470,   470,   471,   471,   471,   472,   472,
     472,   473,   473,   473,   473,   473,   473,   474,   474,   474,
     475,   475,   475,   475,   475,   476,   476,   476,   476,   477,
     477,   478,   478,   478,   479,   479,   480,   480,   480,   480,
     480,   480,   481,   481,   481,   481,   481,   481,   481,   481,
     481,   481,   482,   482,   482,   482,   483,   483,   483,   484,
     484,   485,   485,   485,   485,   485,   485,   486,   486,   486,
     486,   486,   486,   487,   487,   487,   488,   488,   489,   489,
     490,   490
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
       4,     6,     4,     1,     1,     1,     1,     1,     1,     1,
       1,     4,     5,     5,     4,     5,     5,     5,     4,     2,
       2,     3,     3,     1,     1,     1,     3,     1,     3,     3,
       3,     1,     3,     3,     1,     3,     3,     1,     3,     3,
       3,     3,     1,     3,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     5,     4,     1,     1,
       3,     6,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       7,     1,     1,     3,     3,     1,     3,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     4,     2,     6,     1,
       2,     1,     2,     1,     2,     1,     1,     2,     2,     2,
       3,     5,    10,     7,     5,    10,     7,     5,     7,     1,
       1,     1,     2,     1,     3,     1,     1,     3,     2,     3,
       3,     2,     2,     1,     2,     2,     0,     1,     2,     3,
       4,     6,     5,     7,     6,     7,     7,     8,     4,     6,
       5,     7,     1,     3,     4,     5,     4,     3,     5,     1,
       2,     3,     3,     3,     5,     5,     5,     5,     3,     5,
       5,     5,     3,     4,     5,     5,     5,     5,     7,     7,
       7,     7,     7,     7,     7,     2,     3,     4,     4,     4,
       6,     6,     6,     6,     6,     6,     6,     3,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     4,     2,
       3,     3,     2,     3,     2,     3,     3,     6,     2,     2,
       3,     3,     3,     3,     3,     3,     5,     1,     1,     5,
       5,     4,     0,     1,     1,     3,     4,     1,     1,     4,
       6,     3,     5,     5,     5,     8,     9,     1,     1,     1,
       4,     3,     3,     1,     3,     1,     3,     5,     1,     2,
       5,     3,     3,     4,     8,     9,     0,     2,     1,     1,
       1,     1,     2,     1,     2,     2,     2,     1,     3,     1,
       1,     6,     8,    10,    12,    14,     0,     1,     0,     1,
       1,     3,     4,     7,     0,     1,     3,     1,     3,     0,
       1,     1,     2,     0,     1,     2,     3,     0,     1,     3,
       4,     1,     3,     2,     2,     1,     7,     5,     1,     1,
       1,     1,     1,     2,     3,     6,     3,     3,     4,     1,
       2,     2,     3,     8,     8,     8,     5,     9,     2,     2,
       5,     3,     3,     4,     3,     4,     4,     5,     2,     1,
       1,     1,     3,     3,     2,     4,     6,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     4,     1,     2,     3,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     5,     0,     1,     1,     2,     2,     3,     3,     1,
       3,     1,     2,     2,     2,     4,     4,     4,     4,     1,
       1,     1,     2,     2,     3,     1,     0,     3,     2,     1,
       2,     2,     3,     1,     2,     2,     1,     2,     2,     3,
       1,     2,     2,     1,     2,     3,     1,     2,     3,     1,
       3,     4,     1,     1,     1,     1,     0,     7,     0,     8,
       0,     8,     0,     8,     1,     0,     3,     3,     3,     1,
       1,     2,     1,     1,     1,     2,     1,     2,     1,     2,
       1,     2,     0,     2,     3,     3,     4,     4,     4,     3,
       2,     2,     3,     3,     2,     1,     0,     1,     4,     1,
       2,     2,     2,     0,     1,     4,     1,     2,     3,     1,
       2,     0,     1,     2,     6,     7,     0,     9,     8,     9,
      10,     8,     9,     0,    13,    11,    12,    11,     1,     0,
       1,     3,     3,     3,     2,     5,     5,     1,     1,     0,
       2,     5,     0,     1,     1,     1,     5,     5,     5,     1,
       5,     5,     9,     1,     5,     0,     1,     1,     3,     1,
       1,     3,     3,     1,     3,     3,     4,     1,     1,     1,
       1,     2,     1,     3,     3,     2,     3,     1,     3,     1,
       1,     1,     1,     1,     2,     1,     1,     0,     2,     2,
       4,     1,     4,     0,     1,     2,     3,     4,     2,     2,
       1,     2,     2,     5,     5,     7,     6,     1,     3,     0,
       2,     0,     5,     0,     5,     3,     1,     8,     0,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     2,
       5,     6,     1,     1,     3,     3,     2,     3,     3,     2,
       4,     1,     4,     7,     5,    10,     8,     1,     4,     2,
       2,     1,     1,     5,     2,     5,     0,     1,     3,     4,
       0,     1,     0,     0,     1,     1,     2,     2,     2,     2,
       2,     2,     1,     2,     5,     0,     6,     0,     8,     0,
       7,     0,     7,     0,     8,     1,     2,     3,     0,     5,
       3,     4,     4,     4,     4,     5,     5,     5,     5,     6,
       1,     1,     1,     1,     3,     0,     5,     0,     1,     1,
       2,     6,     1,     3,     0,     1,     4,     1,     1,     1,
       1,     1,     1,     1,     3,     2,     1,     2,     2,     2,
       3,     4,     5,     2,     4,     5,     4,     5,     3,     4,
       6,     7,     3,     4,     2,     1,     2,     4,     6,     7,
       3,     4,     2,     3,     4,     5,     4,     5,     4,     5,
       3,     4,     1,     1,     1,     4,     6,     7,     3,     4,
       2,     3,     3,     4,     4,     5,     4,     5,     3,     4,
       1,     3,     2,     1,     2,     2,     2,     3,     4,     5,
       2,     4,     5,     4,     5,     3,     4,     6,     7,     3,
       4,     2,     1,     2,     4,     6,     7,     3,     4,     2,
       3,     4,     5,     4,     5,     4,     5,     3,     4,     2,
       4,     1,     2,     2,     2,     3,     4,     2,     4,     4,
       3,     4,     6,     3,     2,     4,     1,     2,     2,     1,
       1,     2,     3,     4,     2,     4,     4,     6,     1,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       3,     6,     3,     2,     3,     7,     5,     1,     1,     1,
       3,     3,     3,     5,     1,     1,     5,     5,     6,     6,
       0,     1,     1,     3,     2,     2,     1,     2,     2,     3,
       4,     1,     4,     4,     3,     3,     6,     3,     1,     2,
       1,     2,     6,     5,     6,     7,     7,     1,     2,     2,
       1,     2,     2,     3,     4,     1,     4,     4,     3,     6,
       3,     1,     1,     2,     1,     1,     2,     3,     2,     3,
       2,     3,     3,     2,     4,     3,     2,     3,     2,     4,
       3,     2,     6,     6,     6,     7,     1,     2,     1,     1,
       1,     2,     3,     2,     3,     2,     3,     3,     4,     2,
       3,     4,     2,     5,     6,     7,     6,     6,     0,     1,
       0,     2
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
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 8029 "Parser/parser.cc"
    break;

  case 3:
#line 631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8035 "Parser/parser.cc"
    break;

  case 4:
#line 638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8041 "Parser/parser.cc"
    break;

  case 5:
#line 639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8047 "Parser/parser.cc"
    break;

  case 6:
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8053 "Parser/parser.cc"
    break;

  case 7:
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8059 "Parser/parser.cc"
    break;

  case 8:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8065 "Parser/parser.cc"
    break;

  case 20:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8071 "Parser/parser.cc"
    break;

  case 21:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8077 "Parser/parser.cc"
    break;

  case 22:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8083 "Parser/parser.cc"
    break;

  case 23:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8093 "Parser/parser.cc"
    break;

  case 24:
#line 685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8099 "Parser/parser.cc"
    break;

  case 25:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8105 "Parser/parser.cc"
    break;

  case 26:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8111 "Parser/parser.cc"
    break;

  case 28:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8117 "Parser/parser.cc"
    break;

  case 29:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8123 "Parser/parser.cc"
    break;

  case 30:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, (yyvsp[-2].decl), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8129 "Parser/parser.cc"
    break;

  case 31:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8135 "Parser/parser.cc"
    break;

  case 32:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8145 "Parser/parser.cc"
    break;

  case 33:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8151 "Parser/parser.cc"
    break;

  case 34:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8157 "Parser/parser.cc"
    break;

  case 35:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8163 "Parser/parser.cc"
    break;

  case 36:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8169 "Parser/parser.cc"
    break;

  case 37:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8175 "Parser/parser.cc"
    break;

  case 38:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8181 "Parser/parser.cc"
    break;

  case 40:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8193 "Parser/parser.cc"
    break;

  case 41:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8202 "Parser/parser.cc"
    break;

  case 42:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8208 "Parser/parser.cc"
    break;

  case 44:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) )) ) ); }
#line 8214 "Parser/parser.cc"
    break;

  case 45:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8220 "Parser/parser.cc"
    break;

  case 46:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8226 "Parser/parser.cc"
    break;

  case 47:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8232 "Parser/parser.cc"
    break;

  case 48:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8242 "Parser/parser.cc"
    break;

  case 49:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8248 "Parser/parser.cc"
    break;

  case 50:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8255 "Parser/parser.cc"
    break;

  case 51:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8261 "Parser/parser.cc"
    break;

  case 52:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8267 "Parser/parser.cc"
    break;

  case 53:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8273 "Parser/parser.cc"
    break;

  case 54:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8279 "Parser/parser.cc"
    break;

  case 55:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8285 "Parser/parser.cc"
    break;

  case 56:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8291 "Parser/parser.cc"
    break;

  case 57:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8297 "Parser/parser.cc"
    break;

  case 58:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8303 "Parser/parser.cc"
    break;

  case 59:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8309 "Parser/parser.cc"
    break;

  case 60:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8315 "Parser/parser.cc"
    break;

  case 61:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8321 "Parser/parser.cc"
    break;

  case 62:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8327 "Parser/parser.cc"
    break;

  case 63:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8333 "Parser/parser.cc"
    break;

  case 64:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8339 "Parser/parser.cc"
    break;

  case 65:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8345 "Parser/parser.cc"
    break;

  case 66:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8351 "Parser/parser.cc"
    break;

  case 67:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8357 "Parser/parser.cc"
    break;

  case 68:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8367 "Parser/parser.cc"
    break;

  case 69:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8373 "Parser/parser.cc"
    break;

  case 72:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8379 "Parser/parser.cc"
    break;

  case 73:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8385 "Parser/parser.cc"
    break;

  case 76:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8391 "Parser/parser.cc"
    break;

  case 78:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8397 "Parser/parser.cc"
    break;

  case 79:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8403 "Parser/parser.cc"
    break;

  case 80:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8409 "Parser/parser.cc"
    break;

  case 81:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8415 "Parser/parser.cc"
    break;

  case 82:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8421 "Parser/parser.cc"
    break;

  case 83:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8427 "Parser/parser.cc"
    break;

  case 84:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8433 "Parser/parser.cc"
    break;

  case 85:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8439 "Parser/parser.cc"
    break;

  case 86:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8447 "Parser/parser.cc"
    break;

  case 87:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8453 "Parser/parser.cc"
    break;

  case 88:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8462 "Parser/parser.cc"
    break;

  case 91:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8468 "Parser/parser.cc"
    break;

  case 92:
#line 906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8474 "Parser/parser.cc"
    break;

  case 93:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8494 "Parser/parser.cc"
    break;

  case 94:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8500 "Parser/parser.cc"
    break;

  case 95:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8506 "Parser/parser.cc"
    break;

  case 96:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8512 "Parser/parser.cc"
    break;

  case 97:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8518 "Parser/parser.cc"
    break;

  case 98:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8524 "Parser/parser.cc"
    break;

  case 99:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8530 "Parser/parser.cc"
    break;

  case 100:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8536 "Parser/parser.cc"
    break;

  case 101:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8542 "Parser/parser.cc"
    break;

  case 102:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8551 "Parser/parser.cc"
    break;

  case 103:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8557 "Parser/parser.cc"
    break;

  case 104:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 8563 "Parser/parser.cc"
    break;

  case 105:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 8569 "Parser/parser.cc"
    break;

  case 106:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 8575 "Parser/parser.cc"
    break;

  case 107:
#line 958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 8581 "Parser/parser.cc"
    break;

  case 108:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 8587 "Parser/parser.cc"
    break;

  case 109:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 8593 "Parser/parser.cc"
    break;

  case 111:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 8599 "Parser/parser.cc"
    break;

  case 112:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8605 "Parser/parser.cc"
    break;

  case 113:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8611 "Parser/parser.cc"
    break;

  case 114:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8617 "Parser/parser.cc"
    break;

  case 115:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8623 "Parser/parser.cc"
    break;

  case 116:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 8629 "Parser/parser.cc"
    break;

  case 117:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8635 "Parser/parser.cc"
    break;

  case 118:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8641 "Parser/parser.cc"
    break;

  case 126:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8647 "Parser/parser.cc"
    break;

  case 128:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8653 "Parser/parser.cc"
    break;

  case 129:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8659 "Parser/parser.cc"
    break;

  case 130:
#line 1010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8665 "Parser/parser.cc"
    break;

  case 132:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8671 "Parser/parser.cc"
    break;

  case 133:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8677 "Parser/parser.cc"
    break;

  case 135:
#line 1024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8683 "Parser/parser.cc"
    break;

  case 136:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8689 "Parser/parser.cc"
    break;

  case 138:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8695 "Parser/parser.cc"
    break;

  case 139:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8701 "Parser/parser.cc"
    break;

  case 140:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8707 "Parser/parser.cc"
    break;

  case 141:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8713 "Parser/parser.cc"
    break;

  case 143:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8719 "Parser/parser.cc"
    break;

  case 144:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8725 "Parser/parser.cc"
    break;

  case 146:
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8731 "Parser/parser.cc"
    break;

  case 148:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8737 "Parser/parser.cc"
    break;

  case 150:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8743 "Parser/parser.cc"
    break;

  case 152:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 8749 "Parser/parser.cc"
    break;

  case 154:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 8755 "Parser/parser.cc"
    break;

  case 156:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8761 "Parser/parser.cc"
    break;

  case 157:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 8767 "Parser/parser.cc"
    break;

  case 160:
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 8779 "Parser/parser.cc"
    break;

  case 161:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8785 "Parser/parser.cc"
    break;

  case 162:
#line 1108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8791 "Parser/parser.cc"
    break;

  case 166:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 8797 "Parser/parser.cc"
    break;

  case 167:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 8803 "Parser/parser.cc"
    break;

  case 168:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 8809 "Parser/parser.cc"
    break;

  case 169:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 8815 "Parser/parser.cc"
    break;

  case 170:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 8821 "Parser/parser.cc"
    break;

  case 171:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 8827 "Parser/parser.cc"
    break;

  case 172:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 8833 "Parser/parser.cc"
    break;

  case 173:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 8839 "Parser/parser.cc"
    break;

  case 174:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 8845 "Parser/parser.cc"
    break;

  case 175:
#line 1130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 8851 "Parser/parser.cc"
    break;

  case 176:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 8857 "Parser/parser.cc"
    break;

  case 177:
#line 1132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 8863 "Parser/parser.cc"
    break;

  case 178:
#line 1133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 8869 "Parser/parser.cc"
    break;

  case 179:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].expr) ) ) ); }
#line 8875 "Parser/parser.cc"
    break;

  case 180:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) )); }
#line 8881 "Parser/parser.cc"
    break;

  case 182:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8887 "Parser/parser.cc"
    break;

  case 183:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8893 "Parser/parser.cc"
    break;

  case 184:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8899 "Parser/parser.cc"
    break;

  case 186:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8905 "Parser/parser.cc"
    break;

  case 187:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8911 "Parser/parser.cc"
    break;

  case 202:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 8917 "Parser/parser.cc"
    break;

  case 204:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 8923 "Parser/parser.cc"
    break;

  case 205:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8929 "Parser/parser.cc"
    break;

  case 206:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 8940 "Parser/parser.cc"
    break;

  case 207:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 8946 "Parser/parser.cc"
    break;

  case 208:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 8952 "Parser/parser.cc"
    break;

  case 210:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8958 "Parser/parser.cc"
    break;

  case 211:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8964 "Parser/parser.cc"
    break;

  case 212:
#line 1227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8970 "Parser/parser.cc"
    break;

  case 213:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8976 "Parser/parser.cc"
    break;

  case 214:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8982 "Parser/parser.cc"
    break;

  case 217:
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8988 "Parser/parser.cc"
    break;

  case 218:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 8994 "Parser/parser.cc"
    break;

  case 219:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9000 "Parser/parser.cc"
    break;

  case 220:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9006 "Parser/parser.cc"
    break;

  case 221:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9012 "Parser/parser.cc"
    break;

  case 222:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 9026 "Parser/parser.cc"
    break;

  case 223:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9032 "Parser/parser.cc"
    break;

  case 224:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9038 "Parser/parser.cc"
    break;

  case 225:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 9047 "Parser/parser.cc"
    break;

  case 226:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9053 "Parser/parser.cc"
    break;

  case 227:
#line 1281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9059 "Parser/parser.cc"
    break;

  case 228:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9065 "Parser/parser.cc"
    break;

  case 229:
#line 1288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9071 "Parser/parser.cc"
    break;

  case 230:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9077 "Parser/parser.cc"
    break;

  case 231:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9083 "Parser/parser.cc"
    break;

  case 232:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9089 "Parser/parser.cc"
    break;

  case 233:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9095 "Parser/parser.cc"
    break;

  case 234:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9101 "Parser/parser.cc"
    break;

  case 236:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 237:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9113 "Parser/parser.cc"
    break;

  case 238:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9119 "Parser/parser.cc"
    break;

  case 239:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9125 "Parser/parser.cc"
    break;

  case 240:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9131 "Parser/parser.cc"
    break;

  case 241:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9137 "Parser/parser.cc"
    break;

  case 242:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9143 "Parser/parser.cc"
    break;

  case 244:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9149 "Parser/parser.cc"
    break;

  case 245:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9155 "Parser/parser.cc"
    break;

  case 246:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9161 "Parser/parser.cc"
    break;

  case 248:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9167 "Parser/parser.cc"
    break;

  case 249:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9173 "Parser/parser.cc"
    break;

  case 250:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9179 "Parser/parser.cc"
    break;

  case 251:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9188 "Parser/parser.cc"
    break;

  case 252:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9194 "Parser/parser.cc"
    break;

  case 253:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9200 "Parser/parser.cc"
    break;

  case 254:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9206 "Parser/parser.cc"
    break;

  case 255:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9215 "Parser/parser.cc"
    break;

  case 256:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9221 "Parser/parser.cc"
    break;

  case 257:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9227 "Parser/parser.cc"
    break;

  case 258:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9233 "Parser/parser.cc"
    break;

  case 259:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9242 "Parser/parser.cc"
    break;

  case 260:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9248 "Parser/parser.cc"
    break;

  case 261:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9254 "Parser/parser.cc"
    break;

  case 263:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9273 "Parser/parser.cc"
    break;

  case 264:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9279 "Parser/parser.cc"
    break;

  case 265:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9288 "Parser/parser.cc"
    break;

  case 266:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9294 "Parser/parser.cc"
    break;

  case 267:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9300 "Parser/parser.cc"
    break;

  case 268:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9306 "Parser/parser.cc"
    break;

  case 269:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9312 "Parser/parser.cc"
    break;

  case 270:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9318 "Parser/parser.cc"
    break;

  case 271:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9324 "Parser/parser.cc"
    break;

  case 272:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9333 "Parser/parser.cc"
    break;

  case 273:
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9342 "Parser/parser.cc"
    break;

  case 274:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9348 "Parser/parser.cc"
    break;

  case 275:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9357 "Parser/parser.cc"
    break;

  case 276:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9366 "Parser/parser.cc"
    break;

  case 277:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9372 "Parser/parser.cc"
    break;

  case 278:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9378 "Parser/parser.cc"
    break;

  case 279:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9384 "Parser/parser.cc"
    break;

  case 280:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9390 "Parser/parser.cc"
    break;

  case 281:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9396 "Parser/parser.cc"
    break;

  case 282:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9402 "Parser/parser.cc"
    break;

  case 283:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9408 "Parser/parser.cc"
    break;

  case 284:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9414 "Parser/parser.cc"
    break;

  case 285:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9423 "Parser/parser.cc"
    break;

  case 286:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9433 "Parser/parser.cc"
    break;

  case 287:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9439 "Parser/parser.cc"
    break;

  case 288:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9445 "Parser/parser.cc"
    break;

  case 289:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9454 "Parser/parser.cc"
    break;

  case 290:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9464 "Parser/parser.cc"
    break;

  case 291:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9470 "Parser/parser.cc"
    break;

  case 292:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9479 "Parser/parser.cc"
    break;

  case 293:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9489 "Parser/parser.cc"
    break;

  case 294:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9495 "Parser/parser.cc"
    break;

  case 295:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9501 "Parser/parser.cc"
    break;

  case 296:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9507 "Parser/parser.cc"
    break;

  case 297:
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9513 "Parser/parser.cc"
    break;

  case 298:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9522 "Parser/parser.cc"
    break;

  case 299:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9532 "Parser/parser.cc"
    break;

  case 300:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9538 "Parser/parser.cc"
    break;

  case 301:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9547 "Parser/parser.cc"
    break;

  case 302:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9557 "Parser/parser.cc"
    break;

  case 303:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9563 "Parser/parser.cc"
    break;

  case 304:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9572 "Parser/parser.cc"
    break;

  case 305:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9582 "Parser/parser.cc"
    break;

  case 306:
#line 1558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9588 "Parser/parser.cc"
    break;

  case 307:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9597 "Parser/parser.cc"
    break;

  case 308:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 9608 "Parser/parser.cc"
    break;

  case 309:
#line 1576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9614 "Parser/parser.cc"
    break;

  case 310:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9620 "Parser/parser.cc"
    break;

  case 311:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9626 "Parser/parser.cc"
    break;

  case 312:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 9632 "Parser/parser.cc"
    break;

  case 313:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9638 "Parser/parser.cc"
    break;

  case 315:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9644 "Parser/parser.cc"
    break;

  case 316:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9650 "Parser/parser.cc"
    break;

  case 317:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 9656 "Parser/parser.cc"
    break;

  case 318:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 9662 "Parser/parser.cc"
    break;

  case 319:
#line 1607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 9668 "Parser/parser.cc"
    break;

  case 320:
#line 1609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 9674 "Parser/parser.cc"
    break;

  case 321:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 9680 "Parser/parser.cc"
    break;

  case 322:
#line 1614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 9686 "Parser/parser.cc"
    break;

  case 323:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 9692 "Parser/parser.cc"
    break;

  case 324:
#line 1621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 9698 "Parser/parser.cc"
    break;

  case 325:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 9704 "Parser/parser.cc"
    break;

  case 326:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 9710 "Parser/parser.cc"
    break;

  case 327:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9716 "Parser/parser.cc"
    break;

  case 328:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 9722 "Parser/parser.cc"
    break;

  case 329:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 9728 "Parser/parser.cc"
    break;

  case 330:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 9734 "Parser/parser.cc"
    break;

  case 331:
#line 1637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 9740 "Parser/parser.cc"
    break;

  case 332:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 9746 "Parser/parser.cc"
    break;

  case 333:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 9752 "Parser/parser.cc"
    break;

  case 334:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 9758 "Parser/parser.cc"
    break;

  case 335:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 9764 "Parser/parser.cc"
    break;

  case 336:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9770 "Parser/parser.cc"
    break;

  case 339:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9776 "Parser/parser.cc"
    break;

  case 340:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 9785 "Parser/parser.cc"
    break;

  case 341:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9791 "Parser/parser.cc"
    break;

  case 342:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9797 "Parser/parser.cc"
    break;

  case 345:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9803 "Parser/parser.cc"
    break;

  case 346:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9809 "Parser/parser.cc"
    break;

  case 349:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9815 "Parser/parser.cc"
    break;

  case 350:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) )); }
#line 9821 "Parser/parser.cc"
    break;

  case 351:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9827 "Parser/parser.cc"
    break;

  case 352:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9833 "Parser/parser.cc"
    break;

  case 353:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9839 "Parser/parser.cc"
    break;

  case 354:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9845 "Parser/parser.cc"
    break;

  case 355:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9851 "Parser/parser.cc"
    break;

  case 356:
#line 1715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9857 "Parser/parser.cc"
    break;

  case 357:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 9863 "Parser/parser.cc"
    break;

  case 360:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9869 "Parser/parser.cc"
    break;

  case 361:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9875 "Parser/parser.cc"
    break;

  case 362:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 9881 "Parser/parser.cc"
    break;

  case 363:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9887 "Parser/parser.cc"
    break;

  case 364:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9893 "Parser/parser.cc"
    break;

  case 365:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9899 "Parser/parser.cc"
    break;

  case 366:
#line 1751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9905 "Parser/parser.cc"
    break;

  case 367:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9911 "Parser/parser.cc"
    break;

  case 368:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 9917 "Parser/parser.cc"
    break;

  case 369:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 9923 "Parser/parser.cc"
    break;

  case 370:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9929 "Parser/parser.cc"
    break;

  case 371:
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 9935 "Parser/parser.cc"
    break;

  case 372:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 9941 "Parser/parser.cc"
    break;

  case 373:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 9947 "Parser/parser.cc"
    break;

  case 374:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9953 "Parser/parser.cc"
    break;

  case 375:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 9959 "Parser/parser.cc"
    break;

  case 376:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9965 "Parser/parser.cc"
    break;

  case 377:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9971 "Parser/parser.cc"
    break;

  case 378:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9977 "Parser/parser.cc"
    break;

  case 379:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9983 "Parser/parser.cc"
    break;

  case 380:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 9989 "Parser/parser.cc"
    break;

  case 381:
#line 1797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 9995 "Parser/parser.cc"
    break;

  case 382:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10001 "Parser/parser.cc"
    break;

  case 384:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10007 "Parser/parser.cc"
    break;

  case 385:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10013 "Parser/parser.cc"
    break;

  case 386:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10019 "Parser/parser.cc"
    break;

  case 391:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10025 "Parser/parser.cc"
    break;

  case 392:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10031 "Parser/parser.cc"
    break;

  case 393:
#line 1831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10037 "Parser/parser.cc"
    break;

  case 394:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10043 "Parser/parser.cc"
    break;

  case 395:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10049 "Parser/parser.cc"
    break;

  case 396:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10055 "Parser/parser.cc"
    break;

  case 397:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10061 "Parser/parser.cc"
    break;

  case 398:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10067 "Parser/parser.cc"
    break;

  case 401:
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 10073 "Parser/parser.cc"
    break;

  case 402:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10079 "Parser/parser.cc"
    break;

  case 403:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10088 "Parser/parser.cc"
    break;

  case 404:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10094 "Parser/parser.cc"
    break;

  case 405:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10100 "Parser/parser.cc"
    break;

  case 406:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ) ); }
#line 10106 "Parser/parser.cc"
    break;

  case 407:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10115 "Parser/parser.cc"
    break;

  case 408:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10124 "Parser/parser.cc"
    break;

  case 409:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10130 "Parser/parser.cc"
    break;

  case 412:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10136 "Parser/parser.cc"
    break;

  case 413:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10142 "Parser/parser.cc"
    break;

  case 415:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10148 "Parser/parser.cc"
    break;

  case 416:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10154 "Parser/parser.cc"
    break;

  case 426:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10160 "Parser/parser.cc"
    break;

  case 427:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10166 "Parser/parser.cc"
    break;

  case 431:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10172 "Parser/parser.cc"
    break;

  case 433:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10178 "Parser/parser.cc"
    break;

  case 434:
#line 1968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10184 "Parser/parser.cc"
    break;

  case 435:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10190 "Parser/parser.cc"
    break;

  case 436:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10196 "Parser/parser.cc"
    break;

  case 437:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10202 "Parser/parser.cc"
    break;

  case 438:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10208 "Parser/parser.cc"
    break;

  case 440:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10214 "Parser/parser.cc"
    break;

  case 441:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10220 "Parser/parser.cc"
    break;

  case 442:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10226 "Parser/parser.cc"
    break;

  case 443:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10237 "Parser/parser.cc"
    break;

  case 444:
#line 2026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10243 "Parser/parser.cc"
    break;

  case 445:
#line 2028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10249 "Parser/parser.cc"
    break;

  case 446:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10255 "Parser/parser.cc"
    break;

  case 447:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10261 "Parser/parser.cc"
    break;

  case 448:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10270 "Parser/parser.cc"
    break;

  case 449:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10279 "Parser/parser.cc"
    break;

  case 450:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10288 "Parser/parser.cc"
    break;

  case 451:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10299 "Parser/parser.cc"
    break;

  case 452:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10308 "Parser/parser.cc"
    break;

  case 453:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10314 "Parser/parser.cc"
    break;

  case 454:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10320 "Parser/parser.cc"
    break;

  case 455:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10326 "Parser/parser.cc"
    break;

  case 456:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10334 "Parser/parser.cc"
    break;

  case 457:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10342 "Parser/parser.cc"
    break;

  case 458:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10348 "Parser/parser.cc"
    break;

  case 461:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10363 "Parser/parser.cc"
    break;

  case 462:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10369 "Parser/parser.cc"
    break;

  case 463:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10375 "Parser/parser.cc"
    break;

  case 464:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10381 "Parser/parser.cc"
    break;

  case 465:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10387 "Parser/parser.cc"
    break;

  case 466:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10393 "Parser/parser.cc"
    break;

  case 472:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   (yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10403 "Parser/parser.cc"
    break;

  case 485:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10409 "Parser/parser.cc"
    break;

  case 488:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 491:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Const ); }
#line 10421 "Parser/parser.cc"
    break;

  case 492:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Restrict ); }
#line 10427 "Parser/parser.cc"
    break;

  case 493:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Volatile ); }
#line 10433 "Parser/parser.cc"
    break;

  case 494:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Atomic ); }
#line 10439 "Parser/parser.cc"
    break;

  case 495:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10445 "Parser/parser.cc"
    break;

  case 496:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10451 "Parser/parser.cc"
    break;

  case 498:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10457 "Parser/parser.cc"
    break;

  case 499:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10463 "Parser/parser.cc"
    break;

  case 501:
#line 2235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10469 "Parser/parser.cc"
    break;

  case 502:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10475 "Parser/parser.cc"
    break;

  case 503:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10481 "Parser/parser.cc"
    break;

  case 504:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10487 "Parser/parser.cc"
    break;

  case 505:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 10493 "Parser/parser.cc"
    break;

  case 506:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 10499 "Parser/parser.cc"
    break;

  case 507:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 10505 "Parser/parser.cc"
    break;

  case 508:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 10511 "Parser/parser.cc"
    break;

  case 509:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 10517 "Parser/parser.cc"
    break;

  case 510:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 10523 "Parser/parser.cc"
    break;

  case 511:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10529 "Parser/parser.cc"
    break;

  case 512:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10535 "Parser/parser.cc"
    break;

  case 513:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10541 "Parser/parser.cc"
    break;

  case 514:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10547 "Parser/parser.cc"
    break;

  case 515:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10553 "Parser/parser.cc"
    break;

  case 516:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10559 "Parser/parser.cc"
    break;

  case 517:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10565 "Parser/parser.cc"
    break;

  case 518:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10571 "Parser/parser.cc"
    break;

  case 519:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10577 "Parser/parser.cc"
    break;

  case 520:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10583 "Parser/parser.cc"
    break;

  case 521:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10589 "Parser/parser.cc"
    break;

  case 522:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10595 "Parser/parser.cc"
    break;

  case 523:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10601 "Parser/parser.cc"
    break;

  case 524:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10607 "Parser/parser.cc"
    break;

  case 525:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10613 "Parser/parser.cc"
    break;

  case 526:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10619 "Parser/parser.cc"
    break;

  case 527:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10625 "Parser/parser.cc"
    break;

  case 528:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10631 "Parser/parser.cc"
    break;

  case 529:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10637 "Parser/parser.cc"
    break;

  case 530:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10643 "Parser/parser.cc"
    break;

  case 531:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10649 "Parser/parser.cc"
    break;

  case 532:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10655 "Parser/parser.cc"
    break;

  case 533:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10661 "Parser/parser.cc"
    break;

  case 534:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10667 "Parser/parser.cc"
    break;

  case 535:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10673 "Parser/parser.cc"
    break;

  case 536:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10679 "Parser/parser.cc"
    break;

  case 537:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10685 "Parser/parser.cc"
    break;

  case 539:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10691 "Parser/parser.cc"
    break;

  case 541:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10697 "Parser/parser.cc"
    break;

  case 542:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10703 "Parser/parser.cc"
    break;

  case 543:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10709 "Parser/parser.cc"
    break;

  case 545:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10715 "Parser/parser.cc"
    break;

  case 546:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10721 "Parser/parser.cc"
    break;

  case 547:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10727 "Parser/parser.cc"
    break;

  case 548:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10733 "Parser/parser.cc"
    break;

  case 550:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10739 "Parser/parser.cc"
    break;

  case 552:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10745 "Parser/parser.cc"
    break;

  case 553:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10751 "Parser/parser.cc"
    break;

  case 554:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10757 "Parser/parser.cc"
    break;

  case 555:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10763 "Parser/parser.cc"
    break;

  case 556:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 10769 "Parser/parser.cc"
    break;

  case 557:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10775 "Parser/parser.cc"
    break;

  case 558:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 10781 "Parser/parser.cc"
    break;

  case 559:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10787 "Parser/parser.cc"
    break;

  case 560:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10793 "Parser/parser.cc"
    break;

  case 562:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10799 "Parser/parser.cc"
    break;

  case 563:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10805 "Parser/parser.cc"
    break;

  case 564:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10811 "Parser/parser.cc"
    break;

  case 566:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10817 "Parser/parser.cc"
    break;

  case 567:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10823 "Parser/parser.cc"
    break;

  case 568:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10832 "Parser/parser.cc"
    break;

  case 570:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10838 "Parser/parser.cc"
    break;

  case 571:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10844 "Parser/parser.cc"
    break;

  case 572:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10850 "Parser/parser.cc"
    break;

  case 574:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10856 "Parser/parser.cc"
    break;

  case 575:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10862 "Parser/parser.cc"
    break;

  case 577:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10868 "Parser/parser.cc"
    break;

  case 578:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10874 "Parser/parser.cc"
    break;

  case 579:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10880 "Parser/parser.cc"
    break;

  case 581:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10886 "Parser/parser.cc"
    break;

  case 582:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10892 "Parser/parser.cc"
    break;

  case 583:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10898 "Parser/parser.cc"
    break;

  case 584:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10904 "Parser/parser.cc"
    break;

  case 585:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10910 "Parser/parser.cc"
    break;

  case 587:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10916 "Parser/parser.cc"
    break;

  case 588:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10922 "Parser/parser.cc"
    break;

  case 589:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10928 "Parser/parser.cc"
    break;

  case 590:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10934 "Parser/parser.cc"
    break;

  case 591:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 10940 "Parser/parser.cc"
    break;

  case 596:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10946 "Parser/parser.cc"
    break;

  case 597:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10952 "Parser/parser.cc"
    break;

  case 598:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 10961 "Parser/parser.cc"
    break;

  case 599:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10969 "Parser/parser.cc"
    break;

  case 600:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 10978 "Parser/parser.cc"
    break;

  case 601:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10987 "Parser/parser.cc"
    break;

  case 602:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 10996 "Parser/parser.cc"
    break;

  case 603:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11005 "Parser/parser.cc"
    break;

  case 605:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11011 "Parser/parser.cc"
    break;

  case 606:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11017 "Parser/parser.cc"
    break;

  case 607:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11027 "Parser/parser.cc"
    break;

  case 608:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			forall = false;								// reset
			// Create new generic declaration with same name as previous forward declaration, where the IDENTIFIER is
			// switched to a TYPEGENname. Link any generic arguments from typegen_name to new generic declaration and
			// delete newFromTypeGen.
			if ( (yyvsp[0].decl)->type->kind == TypeData::SymbolicInst && ! (yyvsp[0].decl)->type->symbolic.isTypedef ) {
				(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) );
			} else {
				(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].decl)->type->symbolic.name, (yyvsp[0].decl)->type->symbolic.actuals, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
				(yyvsp[0].decl)->type->symbolic.name = nullptr;			// copied to $$
				(yyvsp[0].decl)->type->symbolic.actuals = nullptr;
				delete (yyvsp[0].decl);
			}
		}
#line 11046 "Parser/parser.cc"
    break;

  case 611:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11052 "Parser/parser.cc"
    break;

  case 612:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11058 "Parser/parser.cc"
    break;

  case 613:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11064 "Parser/parser.cc"
    break;

  case 614:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11070 "Parser/parser.cc"
    break;

  case 615:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11076 "Parser/parser.cc"
    break;

  case 616:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11082 "Parser/parser.cc"
    break;

  case 617:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11091 "Parser/parser.cc"
    break;

  case 618:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11097 "Parser/parser.cc"
    break;

  case 619:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11106 "Parser/parser.cc"
    break;

  case 620:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11112 "Parser/parser.cc"
    break;

  case 621:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11121 "Parser/parser.cc"
    break;

  case 622:
#line 2587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11127 "Parser/parser.cc"
    break;

  case 623:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11133 "Parser/parser.cc"
    break;

  case 624:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11146 "Parser/parser.cc"
    break;

  case 625:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11155 "Parser/parser.cc"
    break;

  case 626:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11161 "Parser/parser.cc"
    break;

  case 627:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11167 "Parser/parser.cc"
    break;

  case 628:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11180 "Parser/parser.cc"
    break;

  case 629:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11186 "Parser/parser.cc"
    break;

  case 632:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11192 "Parser/parser.cc"
    break;

  case 633:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11198 "Parser/parser.cc"
    break;

  case 636:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11204 "Parser/parser.cc"
    break;

  case 638:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11210 "Parser/parser.cc"
    break;

  case 639:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11216 "Parser/parser.cc"
    break;

  case 640:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11222 "Parser/parser.cc"
    break;

  case 641:
#line 2648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11228 "Parser/parser.cc"
    break;

  case 642:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11234 "Parser/parser.cc"
    break;

  case 643:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11240 "Parser/parser.cc"
    break;

  case 645:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11246 "Parser/parser.cc"
    break;

  case 647:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11252 "Parser/parser.cc"
    break;

  case 648:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11258 "Parser/parser.cc"
    break;

  case 650:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11264 "Parser/parser.cc"
    break;

  case 651:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11270 "Parser/parser.cc"
    break;

  case 653:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11276 "Parser/parser.cc"
    break;

  case 654:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11282 "Parser/parser.cc"
    break;

  case 655:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11288 "Parser/parser.cc"
    break;

  case 656:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11294 "Parser/parser.cc"
    break;

  case 657:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11300 "Parser/parser.cc"
    break;

  case 658:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11306 "Parser/parser.cc"
    break;

  case 659:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11317 "Parser/parser.cc"
    break;

  case 660:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11323 "Parser/parser.cc"
    break;

  case 661:
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11331 "Parser/parser.cc"
    break;

  case 662:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11337 "Parser/parser.cc"
    break;

  case 663:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl) && ((yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 )) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11348 "Parser/parser.cc"
    break;

  case 664:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11356 "Parser/parser.cc"
    break;

  case 665:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11364 "Parser/parser.cc"
    break;

  case 666:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11372 "Parser/parser.cc"
    break;

  case 667:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11380 "Parser/parser.cc"
    break;

  case 669:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11386 "Parser/parser.cc"
    break;

  case 670:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11392 "Parser/parser.cc"
    break;

  case 671:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11401 "Parser/parser.cc"
    break;

  case 672:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11410 "Parser/parser.cc"
    break;

  case 673:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11416 "Parser/parser.cc"
    break;

  case 674:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11422 "Parser/parser.cc"
    break;

  case 675:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11428 "Parser/parser.cc"
    break;

  case 676:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11434 "Parser/parser.cc"
    break;

  case 678:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11440 "Parser/parser.cc"
    break;

  case 679:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11446 "Parser/parser.cc"
    break;

  case 680:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11452 "Parser/parser.cc"
    break;

  case 681:
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11458 "Parser/parser.cc"
    break;

  case 682:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11464 "Parser/parser.cc"
    break;

  case 683:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11470 "Parser/parser.cc"
    break;

  case 686:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[0].decl) ); }
#line 11476 "Parser/parser.cc"
    break;

  case 687:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11482 "Parser/parser.cc"
    break;

  case 688:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11488 "Parser/parser.cc"
    break;

  case 690:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[0].decl) ); }
#line 11494 "Parser/parser.cc"
    break;

  case 691:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[0].decl) ); }
#line 11500 "Parser/parser.cc"
    break;

  case 692:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->set_last( (yyvsp[-4].decl) )->set_last( (yyvsp[0].decl) ); }
#line 11506 "Parser/parser.cc"
    break;

  case 694:
#line 2821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[0].decl) ); }
#line 11512 "Parser/parser.cc"
    break;

  case 695:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11518 "Parser/parser.cc"
    break;

  case 696:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11524 "Parser/parser.cc"
    break;

  case 698:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11530 "Parser/parser.cc"
    break;

  case 701:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11536 "Parser/parser.cc"
    break;

  case 702:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11542 "Parser/parser.cc"
    break;

  case 704:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11548 "Parser/parser.cc"
    break;

  case 705:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11554 "Parser/parser.cc"
    break;

  case 706:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11560 "Parser/parser.cc"
    break;

  case 711:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11566 "Parser/parser.cc"
    break;

  case 713:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11572 "Parser/parser.cc"
    break;

  case 714:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11578 "Parser/parser.cc"
    break;

  case 715:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11584 "Parser/parser.cc"
    break;

  case 716:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11590 "Parser/parser.cc"
    break;

  case 717:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11596 "Parser/parser.cc"
    break;

  case 718:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11602 "Parser/parser.cc"
    break;

  case 724:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11608 "Parser/parser.cc"
    break;

  case 727:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11614 "Parser/parser.cc"
    break;

  case 728:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 11620 "Parser/parser.cc"
    break;

  case 729:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 11626 "Parser/parser.cc"
    break;

  case 730:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11632 "Parser/parser.cc"
    break;

  case 731:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11638 "Parser/parser.cc"
    break;

  case 732:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11644 "Parser/parser.cc"
    break;

  case 733:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11650 "Parser/parser.cc"
    break;

  case 735:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 11656 "Parser/parser.cc"
    break;

  case 736:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (InitializerNode *)( (yyvsp[-2].init)->set_last( (yyvsp[0].init) ) ); }
#line 11662 "Parser/parser.cc"
    break;

  case 737:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (InitializerNode *)((yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) )); }
#line 11668 "Parser/parser.cc"
    break;

  case 739:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 11674 "Parser/parser.cc"
    break;

  case 741:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-1].expr)->set_last( (yyvsp[0].expr) )); }
#line 11680 "Parser/parser.cc"
    break;

  case 742:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 11686 "Parser/parser.cc"
    break;

  case 743:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11692 "Parser/parser.cc"
    break;

  case 744:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11698 "Parser/parser.cc"
    break;

  case 745:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 11704 "Parser/parser.cc"
    break;

  case 746:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11710 "Parser/parser.cc"
    break;

  case 748:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11716 "Parser/parser.cc"
    break;

  case 749:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11722 "Parser/parser.cc"
    break;

  case 750:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11728 "Parser/parser.cc"
    break;

  case 751:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11739 "Parser/parser.cc"
    break;

  case 752:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11745 "Parser/parser.cc"
    break;

  case 753:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 11751 "Parser/parser.cc"
    break;

  case 754:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11757 "Parser/parser.cc"
    break;

  case 755:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11766 "Parser/parser.cc"
    break;

  case 756:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11772 "Parser/parser.cc"
    break;

  case 757:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 11782 "Parser/parser.cc"
    break;

  case 758:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11788 "Parser/parser.cc"
    break;

  case 759:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11794 "Parser/parser.cc"
    break;

  case 760:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 11800 "Parser/parser.cc"
    break;

  case 761:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11806 "Parser/parser.cc"
    break;

  case 762:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11812 "Parser/parser.cc"
    break;

  case 763:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11818 "Parser/parser.cc"
    break;

  case 764:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 11824 "Parser/parser.cc"
    break;

  case 765:
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11830 "Parser/parser.cc"
    break;

  case 766:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11836 "Parser/parser.cc"
    break;

  case 769:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 11842 "Parser/parser.cc"
    break;

  case 770:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11848 "Parser/parser.cc"
    break;

  case 771:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11854 "Parser/parser.cc"
    break;

  case 772:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11860 "Parser/parser.cc"
    break;

  case 774:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11866 "Parser/parser.cc"
    break;

  case 775:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 11872 "Parser/parser.cc"
    break;

  case 776:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11878 "Parser/parser.cc"
    break;

  case 777:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11884 "Parser/parser.cc"
    break;

  case 778:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11890 "Parser/parser.cc"
    break;

  case 779:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11896 "Parser/parser.cc"
    break;

  case 780:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11902 "Parser/parser.cc"
    break;

  case 781:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11911 "Parser/parser.cc"
    break;

  case 782:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11920 "Parser/parser.cc"
    break;

  case 783:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11929 "Parser/parser.cc"
    break;

  case 784:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11935 "Parser/parser.cc"
    break;

  case 785:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11944 "Parser/parser.cc"
    break;

  case 786:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11950 "Parser/parser.cc"
    break;

  case 788:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 11956 "Parser/parser.cc"
    break;

  case 793:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11962 "Parser/parser.cc"
    break;

  case 794:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11968 "Parser/parser.cc"
    break;

  case 795:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11974 "Parser/parser.cc"
    break;

  case 797:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11980 "Parser/parser.cc"
    break;

  case 798:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11986 "Parser/parser.cc"
    break;

  case 799:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11992 "Parser/parser.cc"
    break;

  case 800:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11998 "Parser/parser.cc"
    break;

  case 802:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12004 "Parser/parser.cc"
    break;

  case 803:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12010 "Parser/parser.cc"
    break;

  case 804:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12016 "Parser/parser.cc"
    break;

  case 805:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12034 "Parser/parser.cc"
    break;

  case 806:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12040 "Parser/parser.cc"
    break;

  case 807:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12046 "Parser/parser.cc"
    break;

  case 808:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12052 "Parser/parser.cc"
    break;

  case 809:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12058 "Parser/parser.cc"
    break;

  case 810:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12064 "Parser/parser.cc"
    break;

  case 811:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12070 "Parser/parser.cc"
    break;

  case 813:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12079 "Parser/parser.cc"
    break;

  case 814:
#line 3223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12085 "Parser/parser.cc"
    break;

  case 815:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12094 "Parser/parser.cc"
    break;

  case 816:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12104 "Parser/parser.cc"
    break;

  case 817:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12113 "Parser/parser.cc"
    break;

  case 818:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12123 "Parser/parser.cc"
    break;

  case 819:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12134 "Parser/parser.cc"
    break;

  case 820:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12144 "Parser/parser.cc"
    break;

  case 821:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12155 "Parser/parser.cc"
    break;

  case 822:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12165 "Parser/parser.cc"
    break;

  case 823:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12176 "Parser/parser.cc"
    break;

  case 824:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12186 "Parser/parser.cc"
    break;

  case 826:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12192 "Parser/parser.cc"
    break;

  case 827:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12198 "Parser/parser.cc"
    break;

  case 828:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12204 "Parser/parser.cc"
    break;

  case 829:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12216 "Parser/parser.cc"
    break;

  case 830:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12227 "Parser/parser.cc"
    break;

  case 831:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12236 "Parser/parser.cc"
    break;

  case 832:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12245 "Parser/parser.cc"
    break;

  case 833:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12251 "Parser/parser.cc"
    break;

  case 834:
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12257 "Parser/parser.cc"
    break;

  case 835:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12263 "Parser/parser.cc"
    break;

  case 836:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12272 "Parser/parser.cc"
    break;

  case 837:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12278 "Parser/parser.cc"
    break;

  case 838:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12284 "Parser/parser.cc"
    break;

  case 839:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12290 "Parser/parser.cc"
    break;

  case 844:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12296 "Parser/parser.cc"
    break;

  case 845:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12302 "Parser/parser.cc"
    break;

  case 846:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12312 "Parser/parser.cc"
    break;

  case 847:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12318 "Parser/parser.cc"
    break;

  case 850:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12324 "Parser/parser.cc"
    break;

  case 851:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12330 "Parser/parser.cc"
    break;

  case 853:
#line 3402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12336 "Parser/parser.cc"
    break;

  case 854:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12342 "Parser/parser.cc"
    break;

  case 855:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12348 "Parser/parser.cc"
    break;

  case 856:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12354 "Parser/parser.cc"
    break;

  case 861:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12360 "Parser/parser.cc"
    break;

  case 862:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12366 "Parser/parser.cc"
    break;

  case 863:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12372 "Parser/parser.cc"
    break;

  case 864:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12378 "Parser/parser.cc"
    break;

  case 865:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12384 "Parser/parser.cc"
    break;

  case 867:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12390 "Parser/parser.cc"
    break;

  case 868:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12396 "Parser/parser.cc"
    break;

  case 869:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12402 "Parser/parser.cc"
    break;

  case 870:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12408 "Parser/parser.cc"
    break;

  case 871:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12414 "Parser/parser.cc"
    break;

  case 872:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12420 "Parser/parser.cc"
    break;

  case 873:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12426 "Parser/parser.cc"
    break;

  case 874:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12432 "Parser/parser.cc"
    break;

  case 875:
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12438 "Parser/parser.cc"
    break;

  case 876:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12444 "Parser/parser.cc"
    break;

  case 877:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12450 "Parser/parser.cc"
    break;

  case 878:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12456 "Parser/parser.cc"
    break;

  case 879:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12462 "Parser/parser.cc"
    break;

  case 880:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12468 "Parser/parser.cc"
    break;

  case 881:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12474 "Parser/parser.cc"
    break;

  case 882:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12480 "Parser/parser.cc"
    break;

  case 883:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12486 "Parser/parser.cc"
    break;

  case 884:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12492 "Parser/parser.cc"
    break;

  case 886:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12498 "Parser/parser.cc"
    break;

  case 887:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12504 "Parser/parser.cc"
    break;

  case 888:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12510 "Parser/parser.cc"
    break;

  case 889:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12516 "Parser/parser.cc"
    break;

  case 890:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12522 "Parser/parser.cc"
    break;

  case 891:
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12528 "Parser/parser.cc"
    break;

  case 892:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12534 "Parser/parser.cc"
    break;

  case 893:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12540 "Parser/parser.cc"
    break;

  case 894:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12546 "Parser/parser.cc"
    break;

  case 895:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12552 "Parser/parser.cc"
    break;

  case 896:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12558 "Parser/parser.cc"
    break;

  case 897:
#line 3551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12564 "Parser/parser.cc"
    break;

  case 898:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12570 "Parser/parser.cc"
    break;

  case 899:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12576 "Parser/parser.cc"
    break;

  case 900:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12582 "Parser/parser.cc"
    break;

  case 901:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12588 "Parser/parser.cc"
    break;

  case 905:
#line 3577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12594 "Parser/parser.cc"
    break;

  case 906:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12600 "Parser/parser.cc"
    break;

  case 907:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12606 "Parser/parser.cc"
    break;

  case 908:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12612 "Parser/parser.cc"
    break;

  case 909:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12618 "Parser/parser.cc"
    break;

  case 910:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12624 "Parser/parser.cc"
    break;

  case 911:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12630 "Parser/parser.cc"
    break;

  case 912:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12636 "Parser/parser.cc"
    break;

  case 913:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12642 "Parser/parser.cc"
    break;

  case 914:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12648 "Parser/parser.cc"
    break;

  case 915:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12654 "Parser/parser.cc"
    break;

  case 916:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12660 "Parser/parser.cc"
    break;

  case 917:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12666 "Parser/parser.cc"
    break;

  case 918:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12672 "Parser/parser.cc"
    break;

  case 919:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12678 "Parser/parser.cc"
    break;

  case 920:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 12687 "Parser/parser.cc"
    break;

  case 921:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12693 "Parser/parser.cc"
    break;

  case 922:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12699 "Parser/parser.cc"
    break;

  case 924:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12705 "Parser/parser.cc"
    break;

  case 925:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12711 "Parser/parser.cc"
    break;

  case 926:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12717 "Parser/parser.cc"
    break;

  case 927:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12723 "Parser/parser.cc"
    break;

  case 928:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12729 "Parser/parser.cc"
    break;

  case 929:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12735 "Parser/parser.cc"
    break;

  case 930:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12741 "Parser/parser.cc"
    break;

  case 931:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12747 "Parser/parser.cc"
    break;

  case 932:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12753 "Parser/parser.cc"
    break;

  case 933:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12759 "Parser/parser.cc"
    break;

  case 934:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12765 "Parser/parser.cc"
    break;

  case 935:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12771 "Parser/parser.cc"
    break;

  case 936:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12777 "Parser/parser.cc"
    break;

  case 937:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12783 "Parser/parser.cc"
    break;

  case 938:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12789 "Parser/parser.cc"
    break;

  case 939:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12795 "Parser/parser.cc"
    break;

  case 940:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12801 "Parser/parser.cc"
    break;

  case 941:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12807 "Parser/parser.cc"
    break;

  case 943:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12813 "Parser/parser.cc"
    break;

  case 944:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12819 "Parser/parser.cc"
    break;

  case 945:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12825 "Parser/parser.cc"
    break;

  case 946:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12831 "Parser/parser.cc"
    break;

  case 947:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12837 "Parser/parser.cc"
    break;

  case 948:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12843 "Parser/parser.cc"
    break;

  case 949:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12849 "Parser/parser.cc"
    break;

  case 950:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12855 "Parser/parser.cc"
    break;

  case 951:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12861 "Parser/parser.cc"
    break;

  case 952:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12867 "Parser/parser.cc"
    break;

  case 953:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12873 "Parser/parser.cc"
    break;

  case 954:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12879 "Parser/parser.cc"
    break;

  case 955:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12885 "Parser/parser.cc"
    break;

  case 956:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12891 "Parser/parser.cc"
    break;

  case 957:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12897 "Parser/parser.cc"
    break;

  case 958:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12903 "Parser/parser.cc"
    break;

  case 959:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12909 "Parser/parser.cc"
    break;

  case 960:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( ast::CV::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12915 "Parser/parser.cc"
    break;

  case 962:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12921 "Parser/parser.cc"
    break;

  case 963:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12927 "Parser/parser.cc"
    break;

  case 964:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12933 "Parser/parser.cc"
    break;

  case 965:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12939 "Parser/parser.cc"
    break;

  case 966:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12945 "Parser/parser.cc"
    break;

  case 967:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12951 "Parser/parser.cc"
    break;

  case 968:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12957 "Parser/parser.cc"
    break;

  case 969:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12963 "Parser/parser.cc"
    break;

  case 970:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12969 "Parser/parser.cc"
    break;

  case 971:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12975 "Parser/parser.cc"
    break;

  case 972:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12981 "Parser/parser.cc"
    break;

  case 973:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12987 "Parser/parser.cc"
    break;

  case 974:
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12993 "Parser/parser.cc"
    break;

  case 975:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( ast::CV::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12999 "Parser/parser.cc"
    break;

  case 977:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13005 "Parser/parser.cc"
    break;

  case 978:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13011 "Parser/parser.cc"
    break;

  case 979:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13017 "Parser/parser.cc"
    break;

  case 980:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13023 "Parser/parser.cc"
    break;

  case 981:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13029 "Parser/parser.cc"
    break;

  case 982:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13035 "Parser/parser.cc"
    break;

  case 983:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13041 "Parser/parser.cc"
    break;

  case 984:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13047 "Parser/parser.cc"
    break;

  case 985:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13053 "Parser/parser.cc"
    break;

  case 986:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13059 "Parser/parser.cc"
    break;

  case 987:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13065 "Parser/parser.cc"
    break;

  case 989:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13071 "Parser/parser.cc"
    break;

  case 990:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13077 "Parser/parser.cc"
    break;

  case 991:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13083 "Parser/parser.cc"
    break;

  case 992:
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13089 "Parser/parser.cc"
    break;

  case 993:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13095 "Parser/parser.cc"
    break;

  case 994:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13101 "Parser/parser.cc"
    break;

  case 995:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13107 "Parser/parser.cc"
    break;

  case 997:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13113 "Parser/parser.cc"
    break;

  case 998:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13119 "Parser/parser.cc"
    break;

  case 999:
#line 3868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13125 "Parser/parser.cc"
    break;

  case 1000:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13131 "Parser/parser.cc"
    break;

  case 1001:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13137 "Parser/parser.cc"
    break;

  case 1002:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13143 "Parser/parser.cc"
    break;

  case 1003:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13149 "Parser/parser.cc"
    break;

  case 1004:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13155 "Parser/parser.cc"
    break;

  case 1005:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13161 "Parser/parser.cc"
    break;

  case 1006:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13167 "Parser/parser.cc"
    break;

  case 1008:
#line 3897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13173 "Parser/parser.cc"
    break;

  case 1009:
#line 3899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13179 "Parser/parser.cc"
    break;

  case 1011:
#line 3902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13185 "Parser/parser.cc"
    break;

  case 1012:
#line 3904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13191 "Parser/parser.cc"
    break;

  case 1014:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13197 "Parser/parser.cc"
    break;

  case 1015:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13203 "Parser/parser.cc"
    break;

  case 1016:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13209 "Parser/parser.cc"
    break;

  case 1017:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13215 "Parser/parser.cc"
    break;

  case 1018:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13221 "Parser/parser.cc"
    break;

  case 1019:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13227 "Parser/parser.cc"
    break;

  case 1020:
#line 3957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13233 "Parser/parser.cc"
    break;

  case 1023:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( ast::CV::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13239 "Parser/parser.cc"
    break;

  case 1024:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13245 "Parser/parser.cc"
    break;

  case 1025:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13251 "Parser/parser.cc"
    break;

  case 1026:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13257 "Parser/parser.cc"
    break;

  case 1027:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13263 "Parser/parser.cc"
    break;

  case 1028:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13269 "Parser/parser.cc"
    break;

  case 1029:
#line 3979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13275 "Parser/parser.cc"
    break;

  case 1030:
#line 3981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13281 "Parser/parser.cc"
    break;

  case 1032:
#line 3987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13287 "Parser/parser.cc"
    break;

  case 1033:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13293 "Parser/parser.cc"
    break;

  case 1034:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13299 "Parser/parser.cc"
    break;

  case 1035:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13305 "Parser/parser.cc"
    break;

  case 1036:
#line 3998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13311 "Parser/parser.cc"
    break;

  case 1037:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13317 "Parser/parser.cc"
    break;

  case 1039:
#line 4007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13323 "Parser/parser.cc"
    break;

  case 1041:
#line 4018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13329 "Parser/parser.cc"
    break;

  case 1042:
#line 4021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13335 "Parser/parser.cc"
    break;

  case 1043:
#line 4023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13341 "Parser/parser.cc"
    break;

  case 1044:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13347 "Parser/parser.cc"
    break;

  case 1045:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13353 "Parser/parser.cc"
    break;

  case 1046:
#line 4030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13359 "Parser/parser.cc"
    break;

  case 1048:
#line 4045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13365 "Parser/parser.cc"
    break;

  case 1049:
#line 4047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13371 "Parser/parser.cc"
    break;

  case 1050:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13377 "Parser/parser.cc"
    break;

  case 1051:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13383 "Parser/parser.cc"
    break;

  case 1052:
#line 4056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13389 "Parser/parser.cc"
    break;

  case 1053:
#line 4058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13395 "Parser/parser.cc"
    break;

  case 1054:
#line 4060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13401 "Parser/parser.cc"
    break;

  case 1056:
#line 4066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13407 "Parser/parser.cc"
    break;

  case 1057:
#line 4068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13413 "Parser/parser.cc"
    break;

  case 1058:
#line 4070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13419 "Parser/parser.cc"
    break;

  case 1059:
#line 4075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13425 "Parser/parser.cc"
    break;

  case 1060:
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13431 "Parser/parser.cc"
    break;

  case 1063:
#line 4087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13437 "Parser/parser.cc"
    break;

  case 1066:
#line 4098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13443 "Parser/parser.cc"
    break;

  case 1067:
#line 4100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13449 "Parser/parser.cc"
    break;

  case 1068:
#line 4102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13455 "Parser/parser.cc"
    break;

  case 1069:
#line 4104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13461 "Parser/parser.cc"
    break;

  case 1070:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13467 "Parser/parser.cc"
    break;

  case 1071:
#line 4108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13473 "Parser/parser.cc"
    break;

  case 1072:
#line 4115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13479 "Parser/parser.cc"
    break;

  case 1073:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13485 "Parser/parser.cc"
    break;

  case 1074:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13491 "Parser/parser.cc"
    break;

  case 1075:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13497 "Parser/parser.cc"
    break;

  case 1076:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13503 "Parser/parser.cc"
    break;

  case 1077:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13509 "Parser/parser.cc"
    break;

  case 1078:
#line 4128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13515 "Parser/parser.cc"
    break;

  case 1079:
#line 4130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13521 "Parser/parser.cc"
    break;

  case 1080:
#line 4132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13527 "Parser/parser.cc"
    break;

  case 1081:
#line 4134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13533 "Parser/parser.cc"
    break;

  case 1082:
#line 4139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13539 "Parser/parser.cc"
    break;

  case 1083:
#line 4141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13545 "Parser/parser.cc"
    break;

  case 1084:
#line 4146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13551 "Parser/parser.cc"
    break;

  case 1085:
#line 4148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13557 "Parser/parser.cc"
    break;

  case 1087:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13563 "Parser/parser.cc"
    break;

  case 1091:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13569 "Parser/parser.cc"
    break;

  case 1092:
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13575 "Parser/parser.cc"
    break;

  case 1093:
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13581 "Parser/parser.cc"
    break;

  case 1094:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13587 "Parser/parser.cc"
    break;

  case 1095:
#line 4194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13593 "Parser/parser.cc"
    break;

  case 1096:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13599 "Parser/parser.cc"
    break;

  case 1097:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13605 "Parser/parser.cc"
    break;

  case 1098:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13611 "Parser/parser.cc"
    break;

  case 1099:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13617 "Parser/parser.cc"
    break;

  case 1100:
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13623 "Parser/parser.cc"
    break;

  case 1101:
#line 4211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13629 "Parser/parser.cc"
    break;

  case 1102:
#line 4213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13635 "Parser/parser.cc"
    break;

  case 1103:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13641 "Parser/parser.cc"
    break;

  case 1104:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13647 "Parser/parser.cc"
    break;

  case 1105:
#line 4222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13653 "Parser/parser.cc"
    break;

  case 1106:
#line 4229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13659 "Parser/parser.cc"
    break;

  case 1107:
#line 4231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13665 "Parser/parser.cc"
    break;

  case 1110:
#line 4255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13671 "Parser/parser.cc"
    break;

  case 1111:
#line 4257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13677 "Parser/parser.cc"
    break;


#line 13681 "Parser/parser.cc"

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
#line 4260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
