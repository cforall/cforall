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

#line 726 "Parser/parser.cc"

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
#define YYFINAL  147
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   26239

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  181
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  310
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1115
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2218

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
    1255,  1258,  1260,  1262,  1272,  1274,  1276,  1281,  1286,  1288,
    1290,  1292,  1300,  1301,  1303,  1307,  1309,  1313,  1315,  1316,
    1318,  1320,  1325,  1326,  1330,  1335,  1336,  1340,  1342,  1347,
    1349,  1354,  1356,  1358,  1360,  1365,  1367,  1369,  1371,  1376,
    1378,  1383,  1384,  1406,  1408,  1413,  1416,  1418,  1421,  1423,
    1426,  1428,  1433,  1438,  1440,  1445,  1450,  1452,  1454,  1456,
    1458,  1461,  1463,  1466,  1468,  1473,  1479,  1482,  1484,  1489,
    1495,  1497,  1502,  1508,  1511,  1513,  1516,  1518,  1523,  1530,
    1532,  1537,  1543,  1545,  1550,  1556,  1559,  1564,  1574,  1576,
    1578,  1583,  1585,  1590,  1591,  1593,  1598,  1600,  1605,  1607,
    1609,  1611,  1614,  1618,  1621,  1625,  1627,  1629,  1631,  1633,
    1635,  1637,  1639,  1641,  1643,  1645,  1650,  1651,  1655,  1661,
    1669,  1674,  1675,  1679,  1680,  1686,  1690,  1691,  1694,  1696,
    1701,  1704,  1706,  1708,  1711,  1713,  1718,  1723,  1724,  1728,
    1733,  1735,  1740,  1742,  1747,  1749,  1751,  1756,  1761,  1766,
    1771,  1773,  1775,  1780,  1782,  1788,  1789,  1793,  1794,  1795,
    1796,  1800,  1805,  1806,  1808,  1810,  1812,  1816,  1820,  1821,
    1825,  1827,  1829,  1831,  1833,  1839,  1840,  1846,  1847,  1851,
    1852,  1857,  1859,  1868,  1869,  1871,  1876,  1881,  1892,  1893,
    1897,  1898,  1904,  1905,  1909,  1911,  1915,  1917,  1921,  1922,
    1926,  1927,  1931,  1932,  1933,  1937,  1939,  1954,  1955,  1956,
    1957,  1959,  1963,  1965,  1969,  1976,  1978,  1980,  1985,  1986,
    1988,  1990,  1992,  2002,  2004,  2016,  2019,  2024,  2026,  2032,
    2037,  2042,  2053,  2060,  2065,  2067,  2069,  2075,  2079,  2086,
    2088,  2089,  2090,  2106,  2108,  2111,  2113,  2116,  2121,  2122,
    2126,  2127,  2128,  2129,  2138,  2139,  2140,  2149,  2150,  2151,
    2155,  2156,  2157,  2166,  2167,  2168,  2173,  2174,  2183,  2184,
    2189,  2190,  2194,  2196,  2198,  2200,  2202,  2207,  2212,  2213,
    2215,  2225,  2226,  2231,  2233,  2235,  2237,  2239,  2241,  2244,
    2246,  2248,  2253,  2255,  2257,  2259,  2261,  2263,  2265,  2267,
    2269,  2271,  2273,  2275,  2277,  2279,  2281,  2283,  2285,  2287,
    2289,  2291,  2293,  2295,  2297,  2299,  2301,  2303,  2305,  2307,
    2312,  2313,  2317,  2324,  2325,  2331,  2332,  2334,  2336,  2338,
    2343,  2345,  2350,  2351,  2353,  2355,  2360,  2362,  2364,  2366,
    2368,  2370,  2375,  2376,  2378,  2380,  2385,  2387,  2386,  2390,
    2398,  2399,  2401,  2403,  2408,  2409,  2411,  2416,  2417,  2419,
    2421,  2426,  2427,  2429,  2434,  2436,  2438,  2440,  2441,  2443,
    2448,  2450,  2452,  2457,  2458,  2462,  2463,  2468,  2467,  2472,
    2471,  2481,  2480,  2491,  2490,  2500,  2505,  2506,  2511,  2517,
    2535,  2536,  2540,  2542,  2544,  2550,  2552,  2554,  2556,  2561,
    2563,  2568,  2570,  2579,  2580,  2585,  2594,  2599,  2601,  2603,
    2612,  2614,  2615,  2616,  2618,  2620,  2621,  2626,  2627,  2628,
    2633,  2635,  2638,  2641,  2648,  2649,  2650,  2656,  2661,  2663,
    2669,  2670,  2676,  2677,  2681,  2690,  2692,  2695,  2694,  2698,
    2700,  2707,  2709,  2713,  2716,  2715,  2726,  2730,  2734,  2738,
    2743,  2744,  2749,  2754,  2762,  2764,  2766,  2768,  2773,  2774,
    2780,  2781,  2782,  2793,  2794,  2796,  2797,  2802,  2803,  2804,
    2806,  2812,  2813,  2815,  2816,  2817,  2819,  2821,  2828,  2829,
    2831,  2833,  2838,  2839,  2848,  2850,  2855,  2857,  2862,  2863,
    2865,  2868,  2870,  2874,  2875,  2876,  2878,  2880,  2888,  2890,
    2895,  2896,  2897,  2901,  2902,  2903,  2908,  2909,  2914,  2915,
    2916,  2917,  2921,  2922,  2927,  2928,  2929,  2930,  2931,  2945,
    2946,  2951,  2952,  2958,  2960,  2963,  2965,  2967,  2990,  2991,
    2997,  2998,  3004,  3003,  3013,  3012,  3016,  3022,  3024,  3034,
    3035,  3037,  3041,  3046,  3048,  3050,  3052,  3058,  3059,  3063,
    3064,  3069,  3071,  3078,  3080,  3081,  3083,  3088,  3090,  3092,
    3097,  3099,  3104,  3109,  3117,  3122,  3124,  3129,  3134,  3135,
    3140,  3141,  3145,  3146,  3147,  3152,  3154,  3160,  3162,  3167,
    3169,  3175,  3176,  3180,  3184,  3188,  3190,  3204,  3206,  3208,
    3210,  3212,  3214,  3216,  3217,  3222,  3225,  3224,  3236,  3235,
    3248,  3247,  3261,  3260,  3274,  3273,  3289,  3295,  3297,  3303,
    3304,  3315,  3322,  3327,  3333,  3336,  3339,  3343,  3349,  3352,
    3355,  3360,  3361,  3362,  3363,  3367,  3373,  3374,  3384,  3385,
    3389,  3390,  3395,  3397,  3399,  3404,  3405,  3411,  3412,  3414,
    3419,  3420,  3421,  3422,  3423,  3425,  3460,  3462,  3467,  3469,
    3470,  3472,  3477,  3479,  3481,  3483,  3488,  3490,  3492,  3494,
    3496,  3498,  3500,  3505,  3507,  3509,  3511,  3520,  3522,  3523,
    3528,  3530,  3532,  3534,  3536,  3541,  3543,  3545,  3547,  3552,
    3554,  3556,  3558,  3560,  3562,  3574,  3575,  3576,  3580,  3582,
    3584,  3586,  3588,  3593,  3595,  3597,  3599,  3604,  3606,  3608,
    3610,  3612,  3614,  3626,  3631,  3636,  3638,  3639,  3641,  3646,
    3648,  3650,  3652,  3657,  3659,  3661,  3663,  3665,  3667,  3669,
    3674,  3676,  3678,  3680,  3689,  3691,  3692,  3697,  3699,  3701,
    3703,  3705,  3710,  3712,  3714,  3716,  3721,  3723,  3725,  3727,
    3729,  3731,  3741,  3743,  3746,  3747,  3749,  3754,  3756,  3758,
    3763,  3765,  3767,  3769,  3774,  3776,  3778,  3792,  3794,  3797,
    3798,  3800,  3805,  3807,  3812,  3814,  3816,  3821,  3823,  3828,
    3830,  3847,  3848,  3850,  3855,  3857,  3859,  3861,  3863,  3868,
    3869,  3871,  3873,  3878,  3880,  3882,  3888,  3890,  3893,  3900,
    3902,  3911,  3913,  3915,  3916,  3918,  3920,  3924,  3926,  3931,
    3933,  3935,  3937,  3972,  3973,  3977,  3978,  3980,  3982,  3987,
    3989,  3991,  3993,  3995,  4000,  4001,  4003,  4005,  4010,  4012,
    4014,  4020,  4021,  4023,  4032,  4035,  4037,  4040,  4042,  4044,
    4058,  4059,  4061,  4066,  4068,  4070,  4072,  4074,  4079,  4080,
    4082,  4084,  4089,  4091,  4099,  4100,  4101,  4106,  4107,  4112,
    4114,  4116,  4118,  4120,  4122,  4129,  4131,  4133,  4135,  4137,
    4140,  4142,  4144,  4146,  4148,  4153,  4155,  4157,  4162,  4188,
    4189,  4191,  4195,  4196,  4200,  4202,  4204,  4206,  4208,  4210,
    4217,  4219,  4221,  4223,  4225,  4227,  4232,  4234,  4236,  4241,
    4243,  4245,  4263,  4265,  4270,  4271
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

#define YYPACT_NINF (-1882)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1114)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      90, 13122,   120,   236, 19572,   189, -1882, -1882, -1882, -1882,
   -1882, -1882, -1882, -1882, -1882, -1882, -1882, -1882,   -16,  1155,
     249, -1882, -1882, -1882, -1882, -1882, -1882, -1882, -1882, -1882,
   -1882, -1882, -1882, -1882, -1882, -1882, -1882, -1882, -1882, -1882,
   -1882, -1882, -1882, -1882, -1882, -1882, -1882, -1882,   130,   400,
   -1882, -1882, -1882, -1882, -1882, -1882,  3749,  3749,   291, 13122,
     303,   344, 23241, -1882,   390, -1882, -1882, -1882, -1882, -1882,
   -1882, -1882, -1882, -1882, -1882,   399,  2712, -1882,   362,   -30,
   -1882, -1882, -1882, -1882, -1882, 19107, -1882, -1882,   406,   453,
     256,    44, -1882,  3749,   485,   494,   497,   491,  4862,   680,
     918, 13287, -1882, -1882,   631, 18952,  1864, -1882, -1882, -1882,
    2606,   744,  6630,  5233,   824,  2606,   903,   604, -1882, -1882,
   -1882, -1882,   184, -1882, -1882, -1882, -1882,   615, -1882, -1882,
   -1882, -1882, -1882,   638,   639,   184, -1882,   184,  5546, -1882,
   -1882, -1882, 20902,  3749, -1882, -1882,  3749, -1882, 13122, -1882,
     635, 20955, -1882, -1882,  4977, 22254, -1882, -1882,  1115,  1115,
     660,  3424, -1882, -1882, -1882, -1882,   170, 15875,  3686,   184,
   -1882, -1882, -1882, -1882, -1882, -1882,   676, -1882,   656,   696,
    2000, -1882,   733, 25612, -1882, -1882, -1882, -1882, -1882, -1882,
   -1882, 17887,  2447,  3084,  2712,   377,   742,   765,   801,   807,
     820,   830, -1882, -1882, 19727, 11947,   751,   838, -1882, 20030,
   -1882, -1882, -1882, -1882,   845, -1882, -1882,   842, -1882, 23636,
     988, 23788, -1882,   865,  3749,   639,   867,  2363,  4977,  2363,
   -1882, -1882, -1882,  3657,  3924,   881,   931,   502,   931, -1882,
     184,   184,   128, 17257,   530,   931, -1882,   184,   184,   128,
     184, -1882,   184, -1882,  4831, -1882, -1882,   892,   901,  1115,
   22998,   928, 19107, -1882, -1882,  2606, -1882,  1965,   604,   919,
    1011, 17257,  3749,  3749,   256, -1882, 15055, -1882,  1115,  1115,
     942,  1011, 17257,  3749, -1882,  8396, -1882, -1882, -1882,  1115,
   -1882, -1882, -1882, -1882,  1115, -1882,   839,  4256,  3749, -1882,
   18806,   969, -1882, -1882, -1882, 22861,   639, 17418,   940,  4977,
   10380, 22998,  2606, -1882, -1882, 22402, -1882,   931,    35, -1882,
   25612, 22254,  4188,  4831, -1882,   540, -1882, -1882, -1882, -1882,
   -1882, 20955,  3749, -1882,   963,   978, -1882, -1882, -1882, -1882,
    3749,  4036,   410,   271, -1882,  3749,   656, -1882,   772,   184,
     184,   982, 21110,   608, 16367, 23051,  2606, -1882,  2606,  1115,
    2606,  1115, -1882, -1882,   184, -1882, -1882,   990, 21163, -1882,
   -1882, -1882, 21318,   845, -1882,   538,   517,   172,   976,   559,
     604,  1000, -1882,  3424,  1004,   656,  3424, -1882, -1882, -1882,
   -1882, -1882,  2447, -1882,   716, -1882,  1035, -1882,  1036,  1078,
   25688,  1052,  1057,  1060, 25612, 25764,  1098, 23356, -1882, -1882,
   -1882, -1882, -1882, -1882, 25840, 25840, 17728,  1077,  4434, -1882,
   -1882, -1882, -1882,   182, -1882,   446, -1882,  1530, -1882, 25612,
   25612, -1882,  1086,   702,   803,   945,   467,  1012,  1103,  1112,
    1102,  1154,    19, -1882,   752, -1882,  1144, -1882,   953,  3834,
   18205, -1882, -1882,  1014,  1144, -1882, -1882,   837, -1882, -1882,
     854,  3084,  1136,  1159,  1169,  1172,  1192,  1198, -1882, -1882,
     662,  1214, -1882,   895,  1214,  1221, -1882,  1235, -1882, 20902,
   -1882,  1135,  1237, 18364, -1882, -1882,  3605,  3464,  1263, 16367,
    1268,  1020,  1364,  1250,  1261, -1882, -1882, -1882,  3749,  5006,
   20384, -1882, -1882, -1882, -1882, -1882, -1882, 18691,  4645,  1077,
   23636,  1264,  1269, -1882, -1882,  1284, 23788,   721, -1882, -1882,
   -1882, 23864,  1297, -1882, -1882, -1882, -1882,  1275,  3657,   110,
    1300,  1312,  1314,   785,  1321,  1325,  1330,  1336,  1339,  1341,
    3924, -1882, -1882, -1882,   184,  1305,  1323,  1344, -1882, -1882,
    1350,   256, -1882, -1882,   639,  1011, 19891, -1882, -1882,   256,
   -1882, -1882,   639, -1882, -1882,  4831, -1882, 18205, 18205, -1882,
    1115,  4977, 23188,  3451, 16531, -1882, -1882, -1882, -1882, -1882,
     639,  1011,    35,  1353, -1882, -1882,  2606,  1362,  1011, 17257,
   -1882,   639,  1011, -1882, 23471, -1882,  1115,  1115, -1882, -1882,
    1381,   229,  1389,   604,  1393, -1882, -1882, -1882, 20331,  1392,
    1404, -1882, -1882,   908, -1882,  1499, -1882,  1411, -1882, -1882,
   -1882, 21482, 25916, -1882, -1882, -1882, -1882, -1882,  4188,   796,
    4831, 19891, 16695,   931, 13122, -1882,  3749,  1412, -1882,  1426,
   -1882, -1882, -1882, -1882, -1882,  3424, -1882, -1882,  1529,  4709,
    3766, 20539, 11947, -1882, 21535, -1882,  1115,  1115, -1882, -1882,
     845, -1882, 15383,  1453,  1597, 25612,  1371,  1350,  1442, -1882,
     184,   184, -1882,  1214, -1882, 21110, -1882, -1882, 20331,  1115,
    1115, -1882,  4709,   184, -1882, 22106, -1882, -1882, 21163, -1882,
     170, -1882, -1882, -1882,  1456,  3749,   172,  1000,  1460,   915,
   20955,   922,   950, -1882,  2447, 23940,  1455, -1882, 18046, -1882,
    4434, 21690, 21690, -1882, 18046, -1882, 25612, -1882, -1882, -1882,
   -1882, -1882, -1882, 18046, -1882, -1882, 20592, 21690, 21690,  1177,
    1146,  1561,   643,  1743, -1882,  1018,  1480,  1074,  1486, -1882,
   23864, 25612, 24016,  1481, 25612,  2363, 25612,  2363, -1882,  2567,
   -1882, -1882, 23940,  2167, 25612, 23940,  2363, -1882, -1882, 25612,
   25612, 25612, 25612, 25612, 25612, 25612, 25612, 25612, 25612, 25612,
   25612, 25612, 25612, 25612, 25612, 25612, 25612, 25612, 24092,  1468,
     733,  4001, 11947, -1882, -1882, -1882, -1882, -1882, -1882, -1882,
   -1882, -1882, -1882, -1882,  1487, 25612, -1882, -1882, 15547,  1662,
   -1882, -1882,   184,   184, -1882, -1882, 18205, -1882, -1882,   719,
    1214, -1882,  1037,  1214, 19891, -1882, -1882,  1350, 19891, -1882,
    1350, -1882, 25992, -1882, -1882, -1882, 19417, 11947,  1489,  1186,
    1491, 14891,  1636,  5324,   729,  1442, -1882,   184,   184,  1442,
     821, -1882,   184,   184, 25612,  3749, 16531,  1498, 16531,  1500,
    1442,   207, 15711, 15711, 15711,  3749, -1882, -1882, 25612,  1284,
   -1882, 23636,  1507, -1882,  2854, -1882, -1882, -1882, -1882, -1882,
    1044, -1882, 15711, 25612,  1065,  1505,  1506,  1509,  1101,  1510,
    1511,  1514,  1516,  1524,  1535,   833,  1214, -1882, -1882,   853,
    1214, -1882, -1882,   871,  1214, -1882, -1882, -1882,  4977,   733,
    1665,  1214, 22550, -1882, -1882,   639,  1538, -1882, -1882, -1882,
    1106,  1539,  1107,  1540, -1882,  1221,  1541,  1544, -1882,   639,
   -1882,  1546, -1882,   639,  1011,  1544, -1882,   639,  1521,  1543,
    1545, -1882, -1882, 20194, -1882,  2363,  3749, 11076,  1633, -1882,
    1237, -1882, 15711,  1116,  1552, -1882,  1544,  1557, -1882, 21743,
   18205,  1537, -1882,  1537, -1882, -1882, -1882,   172,  1555,   184,
     184, -1882, 21163, -1882, 12115, 18523, -1882,  1562,  1564,  1565,
    1567, -1882,  7316,   184, -1882,  1371, -1882, -1882, -1882, -1882,
    1350, -1882, -1882, -1882,  1115, -1882,  3798, -1882, -1882,   604,
     370,  1572,  1548,  1456,  1570,   172, -1882, -1882,  1573,  1571,
   -1882, -1882,  1123, -1882, -1882, -1882, -1882,  1578,  1569,  1579,
    1577,  1583,  1584,  1586,  1591, 25612,  1595,  1596,  1605, 21898,
   12283, 25612, -1882, -1882,  1797, -1882, -1882, -1882, 25612, -1882,
    1609,  1610, 23712,  1222, -1882, 23940,  1612, -1882,  1614, -1882,
   -1882,  4953, -1882,  1147, -1882, -1882, -1882,  4953, -1882, -1882,
    1274,   218, -1882, -1882,  1086,  1086,  1086,   702,   702,   803,
     803,   945,   945,   945,   945,   467,   467,  1012,  1103,  1112,
    1102,  1154, 25612,  1303, -1882,  1613,  4953, -1882, -1882, 23636,
   -1882,  1619,  1623,  1625,  1627,  1662, -1882, -1882, -1882, -1882,
   -1882, 19891, -1882, -1882,  1350, 19891, -1882,  1350,  1628,  1629,
   15711, 15711, -1882, -1882, 14891,   872,  1631,  1637,  1642,  1645,
    3340,  5324, -1882, -1882, 19891, -1882, -1882, -1882, -1882, -1882,
   -1882, 19891, -1882, -1882, -1882, -1882,  1630, -1882,  1442,  1632,
   -1882, -1882, -1882, -1882, -1882, -1882, -1882, -1882,  1646,  1643,
    1655, -1882, -1882,   256,  4953,  1315,   139, -1882, -1882,  1653,
   -1882, 23788, -1882, 25612,   184, 24168, 15711, -1882, -1882,   889,
    1214, -1882,   932,  1214, -1882, -1882,   966,  1214, 19891, -1882,
   -1882,  1350, 19891, -1882, -1882,  1350, 19891, -1882, -1882,  1350,
     931,  1661, -1882,  1350,   339, -1882,  1144,  1664, -1882, -1882,
   -1882, -1882, -1882, -1882,  1668, -1882, -1882, -1882, 21743,  1544,
   -1882,   639, -1882, -1882, -1882, -1882, -1882, 13940, -1882, -1882,
   -1882, -1882,   272, -1882,   520,   452, 11779,  1671,  1672, 17079,
    1676,  1677,  3026,  3118,  3210, 24244,  1678, -1882, -1882,  1679,
    1680, 17079,  1682, -1882, -1882,   639, 25612, 25612,  1809,  1683,
     694, -1882, 17569,  1331,  1686,  1658, -1882, -1882, -1882, 10898,
   -1882, -1882, -1882, -1882, -1882,  2549, -1882, -1882, -1882,  1422,
     259, -1882,   484, -1882,   259, -1882, -1882, -1882, -1882, -1882,
    2363, -1882, -1882, 13452, 19262,  1688, -1882,  3749,  1681,  1691,
   -1882, 16695, -1882, -1882,  3749, -1882, -1882,  4977, -1882, -1882,
    1660,  1673,  1148, 20955,   656,   656,  1456,   172,  1000,  1000,
   -1882, -1882,  1077,  1237, 18364, -1882,  1144, -1882, 12451, -1882,
     979,  1214, -1882,  1115,  7867, -1882, -1882,   172,  1689,   184,
     184,   170,  3749, -1882, 24320, -1882,  1700,   172,  1456,  1703,
   -1882, -1882, 23940,   673, 20331, 12283,  2363, -1882,   673, 20747,
     673, -1882, 25612, 25612, 25612, -1882, -1882, -1882, -1882, 25612,
   25612,  1695, 23636, -1882, -1882,  1698,   754, -1882, -1882, -1882,
    2645, -1882, -1882,  1363, -1882,    97, -1882,  1368, -1882, 23864,
   -1882, -1882, 25612,  1701,  1380,  1382,  1284, -1882,   981,  1214,
   -1882, -1882,  1704,  1705, -1882, -1882, -1882, -1882,  1709,  1013,
    1214, -1882,  1029,  3002,   184,   184, -1882, -1882,  1721,  1723,
   -1882,  1726, -1882, 16531,  1727, -1882, 16039, 16203,  1740,  1741,
   -1882,  1699, 25612, 25612,  1386,  1744, -1882, -1882, -1882, -1882,
   -1882, -1882, -1882,  1751, 19891, -1882, -1882,  1350, 19891, -1882,
   -1882,  1350, 19891, -1882, -1882,  1350,  1752,  1754,  1755,   256,
     184, -1882, -1882,  1394, 25612, 22702,  1753,  1760, -1882, -1882,
   -1882,  1761, 14098, 14256, 14414, 21743, 22998, 21690, 21690,  1728,
   -1882,   302,   341,  2940, 14727, -1882,   369,  3749,  3749, -1882,
   23940,   135,   436, -1882, -1882, -1882, -1882, 11779, 25612,  1767,
    1843, 11610, 11254, -1882,  1745, -1882,  1746, 25612,  1747, 23636,
    1748, 25612, 23864, 25612, -1882, 11432,  1418, -1882,  1756,    66,
   -1882,   102,  1832,   393, -1882,  1777, -1882,  1757, -1882,  1758,
    1778,  1779, 17079, 17079, -1882, -1882,  1842, -1882, -1882,    56,
      56,   445, 15219,   184,   459, -1882, -1882,  1783,  1789,   410,
   -1882,  1790, -1882,  1785, -1882,  1786, -1882, -1882, -1882, -1882,
    1796,  1456,  1788,  1792, 12619,  1793,  1801,  1803, -1882, 19891,
   -1882, -1882,  1350, 25612, 25612,  1237,  1804, -1882,  1456,   172,
   -1882,  1000,   350,  1548, 23636, -1882, -1882,  1456,  1798, -1882,
   21743, -1882,  1019,  1811,  1807,  1161, -1882,  1808, -1882, -1882,
   -1882, -1882, -1882, 23636,  1284, 23864, -1882,  1848,  4953, -1882,
    1848,  1848, -1882,  4953,  4133,  4530, -1882,  1399, -1882, -1882,
   -1882,  1818, 19891, -1882, -1882,  1350, -1882, -1882,  1817,  1819,
     184, 19891, -1882, -1882,  1350, 19891, -1882, -1882,  1824, -1882,
   -1882, -1882, -1882, -1882, -1882, -1882, -1882,  1632, -1882, -1882,
   -1882,  1822, -1882, -1882, -1882, -1882,  1826,  1827,   184,  1830,
    1833,  1835, -1882, -1882, -1882, -1882, -1882, 25612, -1882,   339,
   -1882,  1144, -1882, -1882,  1840,  1841, -1882,  1728,  1728,  1728,
    4294,  1080,  1820,   537, -1882,  4294,   550, 18205, -1882, -1882,
   -1882,  4633, 25612,  5970,   342, -1882, -1882,    21,  1839,  1839,
    1839,  3749, -1882, -1882, -1882,  1171, -1882, -1882, -1882, -1882,
    1686,  1846, 25612,   406,  1844,   491, 14579, 21743,  1179,  1834,
   17079,  1847, -1882, -1882, -1882,   741, 17079, 25612,  1140,   594,
   -1882, 25612, 23476, -1882, -1882,   584, -1882,  1284, -1882,  1187,
    1195,  1197,   603, -1882, -1882, -1882, -1882,   639,  1418,  1851,
   -1882, -1882, 25612, -1882,  1854,   733, -1882, -1882, -1882, -1882,
   25612, 25612, -1882, -1882,   513,    56, -1882,   570, -1882, -1882,
   10720, -1882,   184, -1882,  1537, -1882, 21743, -1882, -1882, -1882,
    1856,   172,   172, -1882, -1882, -1882,  1852,  1857, -1882, -1882,
    1860, -1882,  1870,  1863,  1456,  1000,  1873, -1882, -1882,  1284,
    1885, -1882, -1882,  1883, -1882, -1882, 25612, -1882, 20747, 25612,
    1284,  1887,  1401, -1882,  1407, -1882,  4953, -1882,  4953, -1882,
   -1882, -1882,  1886,   184,   184,  1891,  1894, -1882,  1882, -1882,
   -1882, -1882, -1882, -1882,  1424, 25612, -1882, -1882, -1882, -1882,
   -1882,   588,  1080,  1729,   592, -1882, -1882, -1882, -1882,   184,
     184, -1882, -1882, -1882,   597, -1882,  1203,  4633,   825, -1882,
    5970, -1882,   184, -1882, -1882, -1882, -1882, -1882, -1882, 17079,
   17079,  1686, 16859,   107, 24396,  1980, 17079, -1882, -1882, -1882,
   -1882, 25612, -1882, 24472,  1985,  1884, 23557, 24548, 17079, 11432,
    1686,   541,  1163,  1888, 25612, -1882,  1911,   123, 17079, -1882,
   17079, -1882,  1912, -1882, -1882,  1890,   733,   616,  1914,  1427,
    1210, 17079,  1918, 17079, 17079, 17079, -1882, -1882, -1882,   656,
   -1882,  3749,  4977, -1882,  1456,  1456, -1882, -1882,  1919,  1920,
   -1882, -1882, -1882,  1928,  1922,   172,  1929, -1882,  1932, -1882,
   -1882, -1882, -1882,  1933, -1882, -1882, -1882,  1436,  1438, -1882,
   -1882, -1882, -1882, -1882, -1882, -1882, -1882, -1882,  1917,  1934,
    1935,  1729, -1882,   184, -1882, -1882, -1882, -1882, -1882,  1930,
    4294, -1882,  2015,  4558,    87, 12790, -1882, 16956, -1882,    33,
    1213, 17079,  2018,   624,  1925,   553, 17079, 25612,  1939,   541,
    1163,  1923, 26068,  1927,   579,  2027, -1882, 24624, 24700, 25612,
    1686,  1938, 12957, -1882, -1882, -1882, -1882, 21951, -1882,  1959,
    1942,   142, -1882, 25612, 23940, -1882, -1882, 25612,   259, -1882,
   -1882, -1882, -1882, -1882,  1969,  1971, -1882, -1882, -1882,   172,
    1456, -1882, -1882, -1882, -1882, -1882,  1040,  1214, -1882, -1882,
    1080, -1882, 17079, -1882,   199, -1882,   162, -1882, -1882, -1882,
    1972, 13617, -1882, -1882, 17079, -1882,    54, -1882, 17079, 25612,
    1973, 24776, -1882, -1882, 24852, 24928, 25612,  1939,  1686, 25004,
   25080, 17079,  1956,   628,  1957,   647, -1882, -1882,  1976, 13617,
   21951, -1882,  4062, 21535,  2363,  1974, -1882,  2025,  1978,   658,
    1977, -1882, -1882,  1232,  1234,   544, -1882, -1882,  1456,  1983,
   19891, -1882, -1882,  1350, -1882, -1882, 25612, -1882, 25612, -1882,
   -1882,  1527, 13782, -1882, -1882, 17079, -1882, -1882,  1686, -1882,
   -1882,  1686,  1963,   674,  1982,   710, -1882, -1882,  1686, -1882,
    1686, -1882,  1984, 25156, 25232, 25308, -1882,  1527, -1882,  1964,
    3271,  5733, -1882, -1882, -1882,   142,  1988, 25612,  1981,   142,
     142, -1882, -1882, 17079,  2068,  1995, -1882,  1997, -1882, -1882,
   16956, -1882,  1527, -1882, -1882,  2002, 25384, 25460, 25536, -1882,
   -1882,  1686, -1882,  1686, -1882,  1686, -1882,  1964, 25612,  1999,
    5733,  1998,   733,  2005, -1882,   690, -1882, -1882, 17079, -1882,
   -1882, -1882, 10466,  2011, 16956, -1882, -1882,  1686, -1882,  1686,
   -1882,  1686,  2021,  2012, -1882,   639,   733,  2017, -1882,  2001,
     733, -1882, -1882, -1882, -1882, 10593, -1882,   639, -1882, -1882,
    1448, 25612, -1882,  1249, -1882,   733,  2363,  2020,  2003, -1882,
   -1882,  1255, -1882, -1882,  2004,  2363, -1882, -1882
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   486,     0,     2,   486,   503,   504,   505,   506,   507,
     508,   509,   510,   511,   492,   494,   493,   495,     0,     0,
       0,   512,   514,   535,   515,   536,   518,   519,   533,   534,
     513,   531,   532,   516,   517,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   537,   538,   848,   540,
     613,   614,   617,   619,   615,   621,     0,     0,     0,   486,
       0,     0,    17,   584,   590,     9,    10,    11,    12,    13,
      14,    15,    16,   805,   105,     0,     0,    20,     0,     2,
     103,   104,    18,    19,   866,   486,   806,   424,     0,   427,
     728,   429,   438,     0,   428,   460,   461,     0,     0,     0,
       0,   567,   488,   490,   496,   486,   498,   501,   552,   539,
     470,   545,   550,   472,   562,   471,   577,   581,   587,   566,
     593,   605,   848,   610,   611,   594,   669,   430,   431,     3,
     813,   826,   491,     0,     0,   848,   888,   848,   486,   905,
     906,   907,   486,     0,  1092,  1093,     0,     1,   486,    17,
       0,   486,   449,   450,     0,   567,   496,   480,   481,   482,
     816,     0,   616,   618,   620,   622,     0,   486,     0,   849,
     850,   612,   541,   721,   722,   720,   782,   777,   767,     0,
     857,   814,     0,     0,   503,   807,   811,   812,   808,   809,
     810,   486,   857,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   585,   588,   486,   486,     2,     0,  1094,   567,
     895,   913,  1098,  1091,  1089,  1096,   423,     0,   167,   734,
     166,     0,   432,     0,     0,     0,     0,     0,     0,     0,
     422,   982,   983,     0,     0,   459,   846,   848,   846,   869,
     848,   848,   469,   486,   848,   846,   926,   848,   848,   468,
     848,   945,   848,   923,     0,   560,   561,     0,     0,   486,
     486,     2,   486,   439,   489,   499,   553,     0,   582,     0,
     829,   486,     0,     0,   728,   440,   567,   546,   563,   578,
       0,   829,   486,     0,   502,   547,   554,   555,   473,   564,
     475,   476,   474,   569,   579,   583,     0,   597,     0,   799,
     486,     2,   827,   887,   889,   486,     0,   486,     0,     0,
     567,   486,   498,     2,  1102,   567,  1105,   846,   846,     3,
       0,   567,     0,     0,   452,   848,   841,   843,   842,   844,
       2,   486,     0,   803,     0,     0,   763,   765,   764,   766,
       0,     0,   759,     0,   748,     0,   757,   769,     0,   848,
     848,     2,   486,  1114,   487,   486,   477,   545,   478,   570,
     479,   577,   574,   595,   848,   596,   713,     0,   486,   714,
    1067,  1068,   486,   715,   717,   584,   590,   670,     0,   672,
     673,   670,   851,     0,   780,   768,     0,   865,   864,   860,
     862,   863,   857,   861,     0,   855,   858,    22,     0,    21,
       0,     0,     0,     0,     0,     0,     0,    24,    26,     4,
       8,     5,     6,     7,     0,     0,   486,     2,     0,   106,
     107,   108,   109,    90,    25,    91,    43,    89,   110,     0,
       0,   125,   127,   131,   134,   137,   142,   145,   147,   149,
     151,   153,   155,   158,     0,    27,     0,   591,     2,   110,
     486,   159,   774,   724,   581,   726,   773,     0,   723,   727,
       0,     0,     0,     0,     0,     0,     0,     0,   867,   893,
     848,   903,   911,   915,   921,   584,     2,     0,  1100,   486,
    1103,     2,   103,   486,     3,   712,     0,  1114,     0,   487,
     545,   570,   577,     3,     3,   708,   698,   702,   714,   715,
     486,     2,   896,   914,  1090,     2,     2,    24,     0,     2,
     734,    25,     0,   732,   735,  1112,     0,     0,   741,   730,
     729,     0,     0,   831,     2,   451,   453,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   872,   929,   952,   848,     0,   465,     2,   868,   876,
    1010,   728,   870,   871,     0,   829,   486,   925,   933,   728,
     927,   928,     0,   944,   946,     0,   455,   486,   486,   551,
     487,     0,   567,     0,   486,  1095,  1099,  1097,   568,   803,
       0,   829,   846,     0,   433,   441,   500,     0,   829,   486,
     803,     0,   829,   778,   548,   549,   565,   580,   586,   589,
     584,   590,   608,   609,     0,   779,   684,   718,   487,     0,
     685,   687,   688,     0,   207,   416,   828,     0,   414,   469,
     468,   567,     0,   435,     2,   436,   800,   457,     0,     0,
       0,   486,   486,   846,   486,   803,     0,     0,     2,     0,
     762,   761,   760,   754,   497,     0,   752,   770,   543,     0,
       0,   486,   486,  1069,   487,   483,   484,   485,  1073,  1064,
    1065,  1071,   486,     2,   104,     0,  1029,  1043,  1114,  1025,
     848,   848,  1034,  1041,   706,   486,   575,   716,   487,   571,
     572,   576,     0,   848,  1079,   487,  1084,  1076,   486,  1081,
       0,   679,   671,   678,  1112,     0,   670,   670,     0,     0,
     486,     0,     0,   853,   857,    69,     0,    23,   486,    97,
       0,   486,   486,    92,   486,    99,     0,    33,    37,    38,
      34,    35,    36,   486,    95,    96,   486,   486,   486,     2,
     106,   107,     0,     0,   185,     0,     0,   611,     0,  1089,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
      64,    65,    69,     0,     0,    69,     0,    93,    94,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   486,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   166,     0,   164,   165,   486,   994,
     725,   991,   848,   848,   999,   592,   486,   854,   894,   848,
     904,   912,   916,   922,   486,   897,   899,   901,   486,   917,
     919,     2,     0,     2,  1101,  1104,   486,   486,     0,     2,
       0,   486,   104,  1029,   848,  1114,   964,   848,   848,  1114,
     848,   979,   848,   848,     3,   716,   486,     0,   486,     0,
    1114,  1114,   486,   486,   486,     0,     2,   743,     0,  1112,
     740,  1113,     0,   736,     0,     2,   739,   742,   182,   181,
       0,     2,   486,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   848,   881,   885,   924,   848,
     938,   942,   950,   848,   960,   873,   930,   953,     0,     0,
       0,  1006,     0,   463,   832,     0,     0,   464,   833,   456,
       0,     0,     0,     0,   454,     0,     2,     2,   834,     0,
     437,     2,   803,     0,   829,     2,   835,     0,     0,     0,
       0,   623,   890,   486,   908,     0,     0,   486,   417,   415,
     103,     3,   486,     0,     3,   804,     2,     0,   756,   486,
     486,   750,   749,   750,   544,   542,   672,   670,     0,   848,
     848,  1075,   486,  1080,   487,   486,  1066,     0,     0,     0,
       0,  1044,     0,   848,  1115,  1030,  1031,   707,  1027,  1028,
    1042,  1070,  1074,  1072,   573,   608,     0,  1078,  1083,   675,
     670,     0,   680,  1112,     0,   670,   783,   781,     0,     0,
     856,    73,     0,    70,    71,    74,   815,     0,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   486,
     486,     0,   124,   123,     0,   120,   119,    28,     0,    29,
       0,     0,     0,     0,     3,    69,     0,    52,     0,    53,
      62,     0,    61,     0,    55,    56,    57,     0,    54,    60,
       0,     0,    51,   126,   128,   129,   130,   132,   133,   135,
     136,   140,   141,   138,   139,   143,   144,   146,   148,   150,
     152,   154,     0,     0,   426,     0,     0,    30,     3,   734,
     160,     0,     0,     0,     0,   995,   996,   992,   993,   776,
     775,   486,   898,   900,   902,   486,   918,   920,     0,     0,
     486,   486,  1020,  1019,   486,     0,     0,     0,     0,     0,
     848,  1030,   967,   984,   486,   962,   970,   704,   965,   966,
     705,   486,   977,   987,   980,   981,     0,     3,  1114,     3,
     700,   447,   699,   703,  1106,   709,   710,   692,     0,   693,
     694,     3,     3,   728,     0,   158,     0,     3,     3,     0,
     737,     0,   731,     0,   848,     0,   486,     3,   458,   848,
     882,   886,   848,   939,   943,   951,   848,   961,   486,   874,
     877,   879,   486,   931,   934,   936,   486,   954,   956,   958,
     846,     0,   466,  1007,     3,  1011,  1012,     3,   837,   947,
     557,   556,   559,   558,     2,   804,   838,   785,   486,     2,
     836,     0,   804,   839,   623,   623,   623,   486,   686,   689,
     690,   719,     0,   420,     0,     0,   486,     0,     0,   341,
       0,     0,     0,     0,     0,   187,     0,   336,   337,     0,
       0,   341,     0,   389,   388,     0,   162,   162,   395,   584,
     590,   204,   486,     2,   188,     0,   215,   189,   190,   486,
     209,   191,   192,   193,   194,     0,   195,   196,   342,     0,
     356,   197,   362,   364,   367,   198,   199,   200,   201,   202,
       0,   203,   211,   567,   486,     0,   213,     0,     0,     0,
       3,   486,   817,   804,     0,   792,   793,     0,     3,   788,
       3,     3,     0,   486,   767,   767,  1112,   670,   670,   670,
    1077,  1082,     2,   103,   486,     3,   582,     3,   487,  1038,
     848,  1037,  1040,   486,     3,  1026,  1032,   670,     0,   848,
     848,     0,     0,   655,     0,   674,     0,   670,  1112,     2,
     852,   859,     0,    98,   486,   486,     0,   102,   100,   486,
       0,   114,     0,     0,     0,   118,   122,   121,   186,     0,
       0,     0,   734,   111,   179,     0,     0,    46,    47,    87,
       0,    87,    87,     0,    75,    77,    49,     0,    45,     0,
      48,   157,     0,     0,     0,     0,  1112,  1003,   848,  1002,
    1005,   997,     0,     0,   891,   909,     3,     3,     0,   848,
     973,   976,   848,     0,   848,   848,   968,   985,     0,     0,
    1107,     0,   711,   486,     0,  1109,   486,   486,     0,     0,
     434,     3,     0,     0,     0,     0,   733,   738,     3,   830,
     184,   183,     3,     0,   486,   875,   878,   880,   486,   932,
     935,   937,   486,   955,   957,   959,     0,     0,     0,   728,
     848,  1018,  1017,     0,     0,     0,     0,     0,     3,   804,
     840,     0,   486,   486,   486,   486,   486,   486,   486,   606,
     636,     0,     0,   637,   567,   624,     0,     0,     0,   418,
      69,     0,     0,   327,   328,   212,   214,   486,     0,     0,
       0,   486,   486,   323,     0,   321,     0,     0,     0,   734,
       0,     0,     0,     0,   368,   486,     0,   163,     0,     0,
     396,     0,     0,     0,   219,     0,   210,     0,   318,     0,
       0,     0,   341,   341,   347,   346,   341,   358,   357,   341,
     341,     0,   567,   848,     0,  1022,  1021,     0,     0,   759,
     795,     2,   790,     0,   791,     0,   771,   751,   755,   753,
       0,  1112,     0,     0,   486,     0,     0,     0,     3,   486,
    1033,  1035,  1036,     0,     0,   103,     0,     3,  1112,   670,
     664,   670,   680,   680,   734,   681,   656,  1112,     0,   784,
     486,    72,  1023,     0,     0,     0,    39,     0,   115,   117,
     116,   113,   112,   734,  1112,     0,    68,    84,     0,    78,
      85,    86,    63,     0,     0,     0,    59,     0,   156,   425,
      31,     0,   486,   998,  1000,  1001,   892,   910,     0,     0,
     848,   486,   969,   971,   972,   486,   986,   988,     0,   963,
     978,   974,   989,  1108,   701,   448,   696,   695,   697,  1111,
    1110,     0,     3,   845,   744,   745,     0,     0,   848,     0,
       0,     0,   883,   940,   948,   467,   847,     0,  1013,     0,
    1014,  1015,  1009,   821,     2,     0,   823,   606,   606,   606,
     637,   644,   611,     0,   650,   637,     0,   486,   598,   635,
     631,     0,     0,     0,     0,   638,   640,   848,   652,   652,
     652,     0,   632,   648,   421,     0,   331,   332,   329,   330,
     228,     0,     0,   230,   429,   229,   567,   486,     0,     0,
     341,     0,   309,   308,   310,     0,   341,   187,   268,     0,
     261,     0,   187,   324,   322,     0,   316,  1112,   325,     0,
       0,     0,     0,   377,   378,   379,   380,     0,   370,     0,
     371,   333,     0,   334,     0,     0,   361,   208,   320,   319,
       0,     0,   350,   360,     0,   341,   363,     0,   365,   387,
       0,   419,   848,   819,   750,   772,   486,     2,     2,   662,
       0,   670,   670,  1085,  1086,  1087,     0,     0,     3,     3,
       0,  1046,     0,     0,  1112,   670,     0,   677,   676,  1112,
       0,   659,     3,     0,  1024,   101,     0,    32,   486,     0,
    1112,     0,     0,    88,     0,    76,     0,    82,     0,    80,
      44,   161,     0,   848,   848,     0,     0,   747,     0,   442,
     446,   884,   941,   949,     0,     0,   787,   825,   602,   604,
     600,     0,     0,  1053,     0,   645,  1058,   647,  1050,   848,
     848,   630,   651,   634,     0,   633,     0,     0,     0,   654,
       0,   626,   848,   625,   641,   653,   642,   643,   649,   341,
     341,   231,   567,     0,     0,   249,   341,   314,   312,   315,
     311,     0,   313,     0,   257,     0,   187,     0,   341,   486,
     269,     0,   294,     0,     0,   317,     0,     0,   341,   340,
     341,   381,     0,   372,     2,     0,     0,     0,   343,     0,
       0,   341,     0,   341,   341,   341,   206,   205,   445,   767,
     789,     0,     0,   663,  1112,  1112,  1088,  1039,     0,     0,
    1045,  1047,   660,     0,     0,   670,     0,   658,     2,    50,
      42,    40,    41,     0,    66,   180,    79,     0,     0,  1004,
     444,   443,   975,   990,   746,  1008,  1016,   628,     0,     0,
       0,  1054,  1055,   848,   629,  1051,  1052,   627,   607,     0,
       0,   339,   220,     0,     0,     0,   242,   341,   222,     0,
       0,   341,   251,   266,   277,   271,   341,   187,   306,     0,
     281,     0,     0,   272,   270,   259,   262,     0,     0,   187,
     295,     0,     0,   225,   338,   369,     2,   486,   335,     0,
       0,   397,   348,     0,    69,   359,   352,     0,   353,   351,
     366,   758,   794,   796,     0,     0,  1048,  1049,   661,   670,
    1112,   682,   786,    67,    83,    81,   848,  1061,  1063,  1056,
       0,   639,   341,   237,   232,   235,     0,   234,   241,   240,
       0,   486,   244,   243,   341,   253,     0,   250,   341,     0,
       0,     0,   258,   263,     0,     0,   187,   307,   282,     0,
       0,   341,     0,   297,   298,   296,   265,   326,     0,   486,
     486,     3,   382,   487,   386,     0,   390,     0,     0,     0,
     398,   399,   344,     0,     0,     0,   666,   668,  1112,     0,
     486,  1057,  1059,  1060,   646,   221,     0,   239,     0,   238,
     224,   245,   486,   410,   254,   341,   255,   252,   267,   280,
     278,   274,   286,   284,   285,   283,   264,   279,   275,   276,
     273,   260,     0,     0,     0,     0,   227,   245,     3,   375,
       0,  1053,   383,   384,   385,   397,     0,     0,     0,   397,
       0,   349,   345,   341,     0,     0,   667,     0,   233,   236,
     341,     3,   246,   411,   256,     0,     0,     0,     0,   305,
     303,   300,   304,   301,   302,   299,     3,   375,     0,     0,
    1054,     0,     0,     0,   391,     0,   400,   354,   341,   665,
    1062,   216,     0,     0,   341,   293,   291,   288,   292,   289,
     290,   287,     0,     0,   376,     0,   403,     0,   401,     0,
     403,   355,   218,   217,   223,     0,   226,     0,   373,   404,
       0,     0,   392,     0,   374,     0,     0,     0,     0,   405,
     406,     0,   402,   393,     0,     0,   394,   407
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1882,  4873,  2337, -1882,    -1,   296,  1828,  -171, -1882,  -350,
   -1882,   392, -1882,  -745, -1882,   850,  -948, -1034, -1882,   250,
    5964,  2065, -1882,    65, -1882,  1461,   333,   856,   860,   670,
     859,  1415,  1429,  1417,  1420,  1421, -1882,  -170,  -147,  8482,
     964, -1882,  1759, -1882, -1882,  -696,  8331, -1145,  2458, -1882,
     536, -1882,   951,    36, -1882, -1882,   730,   129, -1882, -1881,
   -1695,   338,   101, -1882, -1882,   726,   354, -1646, -1882, -1427,
   -1882, -1882, -1882, -1882,   149, -1153, -1882, -1882, -1255,   482,
   -1882, -1882, -1882, -1882, -1882,   114, -1214, -1882, -1882, -1882,
   -1882, -1882,    77,   505,   508,   177, -1882, -1882, -1882, -1882,
    -822, -1882,   108,    51, -1882,   183, -1882,  -165, -1882, -1882,
   -1882,   967,  -698,  -986,   -64, -1882,   276,    81,   147,  9206,
    -748,  -716, -1882,  -109, -1882, -1882,    82, -1882,  -126,  6437,
    -196,  -255,  2982,   745,  -670,   209,   230,   516,  2089,  2426,
   -1882,  2194, -1882,   273,  4853, -1882,  2132, -1882,    76, -1882,
   -1882,   235,   425,  5658,  4208,   -54,  1979,  -244, -1882, -1882,
   -1882, -1882, -1882,  -431,  6301,  5828, -1882,  -376,   243, -1882,
    -727,   297, -1882,   231,   791, -1882,   -40,  -195, -1882, -1882,
   -1882,  -372,  6812,  -558,  1260,    83,  1286, -1882,  -423,   -96,
     -41,  2102,  -105,  -781,  -146,   984,  3932,  -311,  -522,  -248,
    -202,  -494,  1391, -1882,  1738,   573,  -938,  1615, -1882, -1882,
     735, -1882, -1245,  -176,   -73,  -944, -1882,   386, -1882, -1882,
   -1156,   509, -1882, -1882, -1882,  2266,  -851,  -520, -1043,   -34,
   -1882, -1882, -1882, -1882, -1882, -1882,   382,  -899,   -13, -1872,
    -174,  8496,   -68,  6977,   -76,  1568, -1882,  2081,   -65,  -221,
    -215,  -210,    26,   -75,   -45,   -39,   705,   -24,    10,    18,
    -201,   -59,  -199,  -190,  -185,   127,  -172,  -154,  -135,  -746,
    -729,  -728,  -709,  -742,  -114,  -706, -1882, -1882,  -726,  1473,
    1488,  1490,  2046, -1882,   648,  7671, -1882,  -570,  -642,  -632,
    -593,  -657, -1882, -1770, -1787, -1775, -1765,  -653,   138,   -38,
   -1882, -1882,   -25,    13,   -91, -1882,  8305,  2140,  1083,  -374
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   849,   423,   424,    83,    84,   425,   399,   426,
    1575,  1576,   427,  1002,  1003,  1004,  1363,  1364,  1365,  1587,
     449,   429,   430,   431,   732,   733,   432,   433,   434,   435,
     436,   437,   438,   439,   440,   441,   442,   451,  1146,   734,
    1498,   795,   221,   797,   445,   870,  1244,  1245,  1246,  1247,
    1248,  1249,  1250,  2172,  1251,  1252,  1691,  2025,  2026,  1956,
    1957,  1958,  2141,  2142,  1253,  1709,  1710,  1711,  1862,  1863,
    1254,  1255,  1256,  1257,  1258,  1259,  1889,  1893,  1520,  1512,
    1260,  1261,  1519,  1513,  1262,  1263,  1264,  1265,  1266,  1267,
    1268,  1728,  2159,  1729,  1730,  2061,  1269,  1270,  1271,  1501,
    2069,  2070,  2071,  2200,  2211,  2091,  2092,   306,   307,   937,
     938,  1212,    86,    87,    88,    89,    90,  1694,   485,    93,
      94,    95,    96,    97,   235,   236,   309,   288,   487,    99,
     488,   100,   608,   102,   103,   156,   355,   312,   107,   108,
     171,   109,   955,   356,   157,   112,   259,   113,   158,   267,
     358,   359,   360,   159,   446,   118,   119,   362,   120,   604,
     930,   928,   929,  1668,   363,   364,   123,   124,  1207,  1465,
    1674,  1675,  1824,  1825,  1466,  1663,  1844,  1676,   125,   697,
    1775,   693,   365,   694,   695,  1325,   967,   610,  1138,  1139,
    1140,   611,   366,   496,   497,   613,  1275,   455,   456,   222,
     514,   515,   516,   517,   518,   343,  1294,   344,   953,   951,
     643,   345,   384,   346,   347,   457,   127,   177,   178,   128,
    1288,  1289,  1290,  1291,     2,  1194,  1195,   634,  1282,   129,
     333,   334,   269,   280,   587,   130,   225,   131,   324,  1148,
     920,   548,   169,   132,   394,   395,   396,   133,   326,   239,
     240,   241,   327,   135,   136,   137,   138,   139,   140,   141,
     244,   328,   246,   247,   248,   329,   250,   251,   252,   835,
     836,   837,   838,   839,   253,   841,   842,   843,   800,   801,
     802,   803,   549,  1187,  1444,   142,  1783,   668,   669,   670,
     671,   672,   673,  1827,  1828,  1829,  1830,   658,   498,   370,
     371,   372,   458,   213,   144,   145,   146,   374,   862,   674
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   196,   385,    82,   569,  1516,  1292,  1043,   194,   698,
    1051,   398,   530,   443,   143,  1295,   859,   143,   531,   520,
     968,   367,   982,   532,   203,   181,   584,   134,  1503,   308,
     969,   197,   533,   238,   534,  1938,   444,   198,  1276,   245,
     737,   353,  1448,   535,  1033,   911,   913,  1939,   536,  1538,
    1539,   316,   199,  1942,   381,    82,    82,  1940,    82,   917,
     212,   537,   546,  1026,   551,  1130,  1874,  1132,   743,   970,
     925,   559,   143,  1086,  1202,    82,  2033,   114,   555,   538,
    1490,  2027,    91,    98,    82,   134,   200,  1112,  2028,  1502,
    -797,  1113,    82,    58,   201,  1283,   976,    82,   539,  1367,
      82,   530,  1106,  1107,    82,   738,   581,   531,  2034,   493,
     676,   210,   532,   480,   319,   946,   460,   592,   143,   462,
     147,   533,  1108,   534,   242,  1109,   212,   270,  1374,  2095,
     204,   281,   535,   223,   308,   114,   545,   536,   993,   161,
      91,    98,    82,   623,   625,    82,  1230,    82,    92,   463,
     537,   153,  1447,    82,   777,   464,   653,    75,   196,  1451,
      82,   143,   308,  2087,   494,   528,  1732,    82,   538,   541,
     465,  2019,   684,   308,   134,   542,   687,  1116,   997,   393,
     547,   278,   690,  1123,  1953,  1954,  1734,   539,   197,   968,
     624,   393,    82,    82,   198,   612,  1411,   778,   617,   969,
    1953,  1954,    58,    82,   466,   224,    92,  -829,    82,   199,
     104,  1745,   467,  2035,   114,   526,  2027,   223,   511,   210,
    1972,  1460,  1594,    82,   114,   249,    82,    82,    82,    91,
      98,   105,    82,    82,  2096,   502,  -798,   576,   970,  1272,
    1528,   566,   599,   200,   238,   676,  1733,   196,   263,  2029,
     245,   201,   275,    82,   628,  1595,    58,  1735,   541,  2033,
     210,    82,   397,   976,   542,   300,    75,   468,   104,   547,
    1955,    82,    82,   647,   110,    82,  1412,   197,    63,    64,
     152,  1867,    82,   198,  1090,   167,  1982,   653,   210,   105,
    1356,  -829,  2123,  2033,   977,    92,    82,    82,   301,    82,
     150,  2067,   588,   903,    82,   369,   576,   875,    82,   160,
    1413,   907,   647,   876,   863,  1686,   702,  1476,   877,   114,
      75,    82,    82,  2088,  2089,   994,  1589,   878,    78,   879,
     686,    82,   110,  1938,   689,   619,  2086,   691,   880,    82,
      82,   744,   692,   881,    82,  1939,   745,   114,  1449,   210,
    1276,  1942,   175,   175,  1347,  1940,   882,   104,   114,  1381,
    1008,   543,   624,  1744,   676,  1396,  1502,  1747,   311,  1397,
    1413,  1514,  1370,   840,   883,  1106,  1107,  1049,   105,  1332,
    1321,   274,    82,   114,   191,    82,   665,   278,   825,   175,
    2019,   393,  -603,   884,  1515,  1108,  2140,  1007,  1388,  1296,
    1010,  1011,  1199,  1012,   166,  1316,  1655,   875,   218,   480,
     676,   110,  1014,   876,  1782,  1016,  1017,  1018,   877,   219,
      20,   110,  2140,   585,   924,   212,   115,   878,   644,   879,
    1141,  1142,   645,  1468,   676,   220,   620,  1328,   880,   175,
     491,   676,   175,   881,  1977,  1978,   180,  2174,  1116,  1157,
     543,   612,  1469,   462,   203,   961,   882,   175,   182,  1461,
      82,  1117,   968,   227,   379,  1120,  1460,  1460,  1460,   713,
     202,    64,   969,   311,   883,   895,  1135,  1136,   493,   981,
     308,   896,  1669,   463,   115,    82,    82,   502,  1441,   464,
     659,  1462,   987,   884,   757,   758,  1841,    82,    82,   183,
     477,   311,   228,  1842,   465,  1514,   353,    82,   296,   511,
    1442,   970,   311,  1471,  1472,   757,   110,   106,  1475,  1280,
     175,  1670,  1843,   525, -1113,   527,   612,    82,  1515,  1324,
    1681,  1503,   300,   494,   468,   691,   960,   311,   466,    82,
     692,  1316,  2044,  2045,   110,   191,   467,   640,   757,  1682,
    1736,  1272,   909,   462,   192,   110,   493,   612,   914,  1795,
    1797,  1799,  1865,   115,    82,   895,   502,  1873,   175,   175,
      82,   896,    82,   115,    58,   106,   641,   642,   162,   175,
     110,   163,   164,   463,   165,  1376,   216,   656,  1891,   464,
     679,   944,  1502,   602,   175,  1517,   607,   767,   768,   301,
     945,   150,    58,   656,  1510,   746,   935,   656,   301,  1075,
     747,   494,    58,   963,    -3,   301,  1688,   265,  1518,  2133,
    1468,   659,  1624,  1892,   554,  1130,  1132,    82,   175,    82,
     961,   562,  1473,    82,  1230,    82,   175,   175,    75,  1751,
    1794,   175,   769,   770,    82,  1895,   227,   143,    82,    82,
     988,   982,   580,  1551,  2001,   228,   502,   300,   229,  1150,
     134,   547,  1511,   591,   106,   114,    75,   897,   115,   593,
     302,   230,   191,  1597,   443,  1470,    75,  1386,  1387,   175,
    -983,    82,   175,   254,   605,   556,   493,  -983,  1145,   547,
    1857,  1858,  1859,   612,    82,   631,   115,  1127,  1832,   547,
     273,  -982,  1133,   393,  1461,  1461,  1461,   115,  -982,   612,
     114,  1681,  1860,   612,  1028,    91,    98,  1833,   656,   840,
     676,  1971,  -657,  1836,  2041,  1685,   612,   599,  1181,  -657,
    1835,   493,   115,  1422,    58,  1617,  1462,  1462,  1462,  1541,
    1028,   494,    74,  2075,    82,  1028,    82,  -480,    82,  1842,
    2050,  1868,    82,  1943,  1402,    82,  1869,   897,  1842,  1558,
    1880,   523,   296,   662,  1875,  1869,  1300,   663,  1937,  1567,
     369,  1537,  1944,  1990,    80,   664,   298,  1947,  1991,  1692,
      82,    92,   175,  1692,  1712,  1028,   494,   665,   963,  1028,
     211,    58,   586,   300,   175,   175,  1720,  1712,    75,  2113,
    1021,    58,   301,   243,  2039,  1129,   271,   188,  1028,   491,
     282,  1022,  1023,   659,   320,  2128,  1899,   814,  2115,   311,
    2129,   547,  2043,  -818,  1053,    82,   265,  -481,  1210,  1351,
      82,   383,    82,   341,  2056,  1028,  1352,    14,    15,    16,
      17,    18,   616,   104,    82,  2146,   264,  2189,  1927,   191,
    1928,   386,  2190,   397,    82,    75,  -722,   286,  1584,   293,
     511,   295,   110,    82,   105,    75,   737,   491,   760,   477,
     265,  1028,  1320,   703,  1091,   761,   762,   704,   547,   864,
     865,  2148,   965,   866,  1114,  1180,   656,   491,   663,  1792,
    1857,  1858,  1859,    58,   353,  1410,    58,    82,   211,   469,
     264,  2106,  1371,   293,   295,    58,  -482,   110,  1586,   779,
     656,   500,  1860,   780,   503,  1332,    14,    15,    16,    17,
      18,  1861,   470,   656,  1301,    58,  1542,  1543,   480,   648,
     296,   738,   175,  1821,    82,    82,   511,   905,  1834,   211,
     556,   175,   888,    58,   547,   956,   959,   598,    64,  1417,
     143,   631,   736,   468,   264,   547,   699,    75,   471,   701,
      75,    58,   143,   919,   472,   255,   256,   211,   257,    75,
     923,   763,   764,   258,   927,    58,  1121,   473,   985,   944,
     663,   589,   468,  1145,   547,    82,   612,   474,  1168,    75,
     612,   175,   547,   501,   805,  1717,  1395,   840,   806,   612,
     505,  1774,  1784,   506,    58,   265,  1439,    75,  1172,   612,
     519,   807,   547,   114,   115,   704,   612,   491,    91,    98,
     521,   264,   524,   293,   295,    75,  1176,  1114,  1577,   468,
     547,   663,   545,    14,    15,    16,    17,    18,    58,    75,
      82,  1037,   544,  1039,  1424,  1042,    82,   567,   547,  1048,
     818,    58,  1052,    58,   547,   264,   568,   965,  1553,   115,
     264,   656,   491,   612,   676,   934,   264,   612,    75,   935,
    1779,   612,   996,   765,   766,    82,   645,  1077,   511,   998,
    1341,  1662,   579,   645,    92,    58,  1345,  1428,   573,  1790,
     904,   547,    58,  1054,  1055,  1056,  1286,  1353,   908,   264,
     223,    58,    75,    82,   681,   590,   295,   999,   981,    82,
      82,   704,    58,   500,   740,    75,   918,    75,   385,   385,
     618,  1432,   477,   614,   265,   547,   635,   926,    14,    15,
      16,    17,    18,   636,  1549,  1133,  1602,   586,   663,   696,
     547,   175,   651,    82,   771,   772,   104,   683,    74,    75,
     106,   175,   720,    74,   443,   353,    75,  -484,   369,  -124,
    -124,  -124,  -124,  -124,  -124,    75,   503,  1274,  1611,   798,
     692,  1712,   547,   547,   662,  1027,    75,  1565,   663,  1028,
      80,    81,  1467,   700,  1615,    80,   664,    58,   663,  1776,
     705,  1645,  1095,   706,   586,  2080,   547,   656,   707,   547,
     679,  1154,  1598,  1904,  1905,  1155,   264,   710,  1572,   353,
     110,   143,   711,   162,    74,   712,   163,   164,   511,   165,
     300,    82,    82,    82,   547,  1285,  1818,  1819,  1820,   143,
     511,  1211,   175,   175,   264,  1822,   681,   295,   740,   547,
    1030,  1031,   443,   443,   736,   503,    80,    81,   511,  2073,
     736,    75,   720,   716,    82,   491,   556,  1133,   759,   736,
     547,  1133,   143,  1190,  1192,  1632,  1633,  1028,  1028,    82,
     773,   631,    82,    82,  1530,   547,    82,   774,   736,   775,
    1331,   713,  1319,    82,  1332,   264,    82,   143,   776,  1857,
    1858,  1859,   114,   808,   944,   826,   740,    91,    98,   270,
     281,  1028,   781,  2161,  1366,  1536,  1201,  2165,  1332,   806,
     264,  1860,  1857,  1858,  1859,   264,   809,   264,  1787,   612,
    1866,    82,  1788,   612,  1028,   114,   810,   612,  1849,   811,
      91,    98,  1332,  2093,  1860,    82,  1853,  1019,   740,   264,
    1028,   264,   264,  -188,  1877,  1286,   651,   740,  1028,   812,
     278,   511,  1878,   264,  1879,   813,  1155,  2010,  1028,    82,
    1948,  2093,   115,    92,   806,  1627,   264,  1995,  1695,   757,
    2036,  1028,  1695,   476,  1028,   264,   821,    14,    15,    16,
      17,    18,  1354,  1155,    14,    15,    16,    17,    18,  2131,
     823,  2132,    82,  1332,  2143,  1028,    92,    -3,  1678,   264,
     844,   681,   295,  1914,  1679,  -483,  2208,  1578,  1579,  1580,
    2205,   846,  2214,  1693,  1581,  1582,  2215,  1693,   353,   369,
     263,   275,   848,   264,   681,   104,   -18,  1467,  1467,  1467,
     264,   860,  1664,  1467,  1368,  1369,    58,  1061,  1062,  1063,
    1064,  1188,   659,    58,   612,   861,  1274,  1452,  1453,  1454,
     530,  2078,   871,   106,   873,  1196,   531,   885,   104,  1200,
     899,   532,    82,  1203,  1028,  1372,    82,    82,   143,   886,
     533,   887,   534,   369,  1285,  -159,  -159,  1577,   889,  1274,
     265,   535,   890,  1845,  1845,  1845,   536,   891,   511,   110,
     143,   204,   740,   892,   143,   143,   893,   612,   894,   537,
      75,  -485,   900,   944,   901,    74,   612,    75,   143,   313,
     612,   511,   511,  1510,  1511,   175,   921,   538,   175,   175,
     175,    82,   110,  1592,  1593,   922,   662,   175,  1596,  1593,
     663,  1723,  1724,  1725,  1726,  1727,   539,    80,   664,   656,
    1600,  1593,  1103,  1585,  -601,   175,  1634,  1585,   588,   932,
     274,   175,  -599,   114,  1103,  1647,   931,   114,   114,  1800,
    1155,  1925,  1155,   511,  1887,   933,   607,  1926,  1593,   936,
     491,   114,   948,   175,  -123,  -123,  -123,  -123,  -123,  -123,
     175,   950,   511,   143,  1935,  1028,   609,    82,  1993,  1994,
    1680,   939,    82,    82,    82,  1678,  2014,  1593,  2015,  1593,
    1678,  1679,   153,  1837,  1953,  1954,  1679,   954,   541,  2205,
    2206,  1590,  1591,   971,   542,   973,   875,   990,   175,  1057,
    1058,   665,   876,   995,    92,  1059,  1060,   877,    92,    92,
    1065,  1066,  1573,  1746,  1748,  1006,   878,  1029,   879,  1846,
    1847,   115,    92,  1032,  1035,  1777,  1778,   880,  1074,  1102,
    1079,  1103,   881,   748,  1110,   749,   750,   751,  1131,    82,
    1134,  1152,  1159,  1160,    82,   882,  1161,  1162,  1163,   585,
      82,  1164,    82,  1165,   115,    14,    15,    16,    17,    18,
      82,  1166,   369,   883,  1204,   752,   104,  1182,   753,   754,
     104,   104,  1167,   755,   756,  1189,  1191,  1193,  -801,   511,
    1197,   826,   884,  1277,   104,   511,  1205,  1697,  1206,   264,
     143,  1697,  1697,  1281,  1284,  1989,  1293,  1286,  1297,  1309,
     264,  1310,  1311,   385,  1312,  1697,  1323,  1324,  1330,   264,
    1334,   152,   106,  1327,    58,  1333,  1329,  1450,  1336,  1019,
    1337,  1338,    14,    15,    16,    17,    18,  1339,  1340,   511,
     110,  1474,  1342,  1343,   110,   110,    14,    15,    16,    17,
      18,  1025,  1344,   175,   175,   106,  1349,  1350,   110,   143,
    1373,  1496,  1357,   278,  1358,   895,  1377,   991,   736,   491,
    1378,   896,  1379,   443,  1380,  1384,  1385,  1680,  1389,   265,
    1400,  2062,  1680,  1403,  1390,    82,    74,    82,    75,  1391,
     543,    58,  1392,  1405,  1406,  1888,  2024,  1416,   175,   175,
      14,    15,    16,    17,    18,  1346,  1407,   798,  1440,   676,
    2068,   547,  -802,  1500,   586,  1445,  1477,  1478,    80,    81,
     264,  1481,  1482,  1491,  1492,  1493,    82,  1495,  1504,    82,
    1532,  1525,   906,   263,   275,  -721,  1285,  1028,   511,   511,
    1523,  1526,  1559,  1534,  1566,   511,   264,  1569,  1583,  1585,
    1593,  1606,  1607,    74,  2062,    75,  1610,   511,   184,     6,
       7,     8,     9,    10,    11,    12,    13,   511,  1621,   511,
    1622,  1599,   143,  1667,  1822,  1678,  1623,  1625,   547,  2003,
     511,  1679,   511,   511,   511,    80,    81,  1629,  1630,   530,
      82,    82,   115,  1286,  1635,   531,   115,   115,  1638,  1642,
     532,  1643,  1644,  1652,  1653,  1656,   443,   609,   443,   533,
     115,   534,  1470,  1699,  1511,  1713,  1714,  1716,  1718,   283,
     535,  1737,  1230,  1740,  1741,   536,  1731,  1738,  1739,  2138,
    1752,  2024,  1149,  1753,  1755,   114,  1757,  1758,   537,    82,
    1759,  1761,  1781,  1763,  2068,  1762,   511,   443,  2068,  2068,
     511,  1764,  2064,  1765,  1771,   511,   538,   897,  1785,  1786,
    1789,  1793,  1801,   274,  1803,   612,  1804,   175,   271,   282,
    2163,   468,  1807,    19,  1809,   539,  1634,  1811,  2184,  1854,
    1812,  2187,  1813,   106,  1816,  1817,   175,   106,   106,   585,
    1831,  1672,   175,  1850,  1856,   224,  1884,  1687,  1689,  1886,
    1903,   106,  1906,   387,  1907,  2199,    92,  1912,   264,  2199,
    1910,   511,    48,    49,    50,    51,    52,    53,    54,    55,
    1911,   443,  1285,   511,  2209,  2064,  1915,   511,   586,  1917,
    1919,  1924,  1934,  1929,   143,   196,   175,   512,  1932,  2122,
     511,  1933,   628,   264,  2207,  1961,   541,  1749,  2072,   264,
    1966,    82,   542,    82,  1967,  1981,    85,  1986,  1979,   151,
    1988,  1992,   143,  1997,  2016,   197,  1326,  1680,   104,  2006,
    2007,   198,  2008,  2011,  1081,  2009,  2012,  2013,   388,   547,
    2022,  2017,  2018,  2038,   511,   895,  2040,  -584,  2049,  1697,
    1098,   896,  2051,  2046,  1099,   143,   389,   114,   390,   391,
      65,    66,    67,    68,    69,    70,    71,    72,  2057,    82,
      82,  2065,  2066,  2076,    85,  2077,  2090,  2112,  2114,  2099,
    2116,  2126,   511,  2127,  2145,   114,  2125,  2136,  2130,   511,
    2149,   193,   110,  2168,  2158,   175,   175,   210,  2162,  2169,
      85,   189,   175,  2147,  2170,   392,  2185,   195,  2175,    82,
    2186,  2164,  2188,   234,   175,  2194,   262,   511,   114,  2197,
      85,   511,  2201,   511,   175,  2196,   175,  2212,    92,   237,
    1921,  2202,  1571,  2213,  2216,    19,   502,   175,  1067,   175,
     175,   175,  1069,  1024,   511,   284,  1070,   175,  1071,   285,
    1506,  1499,   289,  1068,   294,    82,    92,   151,   796,   264,
    2195,  1701,   265,    85,    82,  1983,   151,  2139,  2156,   323,
     331,  1722,   656,  1976,  2134,   215,  1894,   589,    52,    53,
      54,    55,   352,  1882,  2183,   325,  1883,  2118,  2166,    92,
     104,  2203,  2117,   172,  1524,   291,   578,  2021,   543,  1666,
    1322,  2084,  1151,   175,  1521,   867,   450,   175,   193,   193,
     952,  1697,   175,  1881,  1754,  1900,     3,   264,   104,   151,
     483,  1082,  1000,   149,   262,  1044,  1045,    65,    66,    67,
      68,    69,    70,    71,    72,  1046,  1083,   897,  1084,  1697,
     558,   215,     0,   323,   115,   656,     0,  1815,   234,   234,
       0,   104,     0,     0,   110,     0,     0,   495,     0,   325,
       0,     0,     0,     0,   529,   237,     0,     0,   175,   323,
       0,     0,  1697,     0,     0,     0,  1047,    85,     0,     0,
     175,   459,   110,     0,   175,   325,   857,     0,   512,     0,
       0,   262,     0,     0,     0,     0,     0,   175,     0,     0,
       0,     0,     0,     0,   284,     0,     0,     0,     0,     0,
    2124,     0,     0,     0,     0,   110,     0,     0,   586,     0,
       0,     0,     0,     0,   323,     0,     0,  1382,     0,  1540,
     331,  1383,     0,     0,     0,   106,   331,   323,   323,     0,
     325,   175,     0,     0,     0,     0,   151,     0,     0,     0,
    1398,   284,   577,   629,   325,     0,     0,  1399,     0,     0,
       0,  1568,     0,     0,     0,     0,     0,   352,   666,   675,
       0,     0,     0,     0,     0,     0,   217,     0,     0,   175,
       0,     0,     0,   352,     0,     0,   175,   352,     0,     0,
       0,   264,     0,     0,     0,   285,     0,   680,     0,   294,
       0,     0,     0,     0,  1436,     0,   115,     0,  1437,  1601,
     387,     0,  1438,     0,   175,     0,   299,     0,   175,   149,
     175,   577,     0,    65,    66,    67,    68,    69,    70,    71,
      72,   450,     0,     0,   115,     0,     0,     0,   190,     0,
       0,   175,   661,     0,     0,     0,   721,     0,     0,   804,
       0,     0,  2210,     0,     0,     0,     0,     0,     0,     0,
       0,  2217,     0,     0,     0,   450,   816,   115,   799,   819,
       0,     0,     0,     0,     0,     0,   193,   266,     0,     0,
       0,     0,     0,     0,     0,   388,     0,     0,   287,   290,
       0,     0,     0,     0,   151,     0,     0,   106,   483,     0,
       0,     0,   833,   389,   675,   390,   391,    65,    66,    67,
      68,    69,    70,    71,    72,   151,     0,     0,   834,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,   558,
       0,   266,     0,     0,     0,   264,     0,     0,     0,     0,
     215,     0,     0,   234,     0,     0,   721,   264,     0,     0,
       0,     0,     0,     0,     0,   234,     0,     0,   106,   874,
     184,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,   237,     0,   661,  1760,     0,     0,  1507,     0,     0,
     323,     0,   450,   450,     0,   266,   323,     0,     0,   352,
       0,  1773,     0,     0,     0,     0,   325,     0,     0,     0,
    1780,     0,   325,     0,     0,   149,   626,   173,   174,    65,
      66,    67,    68,    69,    70,    71,    72,  1791,     0,     0,
       0,     0,     0,   149,     0,   284,   495,    65,    66,    67,
      68,    69,    70,    71,    72,  1040,   264,     0,     0,   512,
       0,     0,   857,   323,     0,   323,     0,   352,     0,    85,
       0,     0,   266,     0,     0,     0,     0,   459,   459,   943,
    1639,   325,     0,     0,  1640,     0,   352,   483,  1641,   675,
       0,  2198,     0,     0,     0,     0,  1041,   666,     0,  1508,
       0,   666,     0,  2204,     0,     0,   266,     0,     0,     0,
     352,   266,     0,     0,     0,     0,     0,   266,     0,     0,
     675,   149,     0,   352,   495,    65,    66,    67,    68,    69,
      70,    71,    72,  1359,     0,   151,     0,  1360,     0,  1361,
       0,     0,     0,   450,     0,     0,   151,   151,     0,   450,
     266,     0,     0,     0,    58,     0,     0,     0,   450,     0,
       0,   151,   151,   151,     0,     0,     0,     0,     0,     0,
    1876,    77,     0,     0,  1588,     0,     0,     0,   264,     0,
       0,     0,     0,     0,     0,   983,     0,     0,   149,   828,
       0,   830,    65,    66,    67,    68,    69,    70,    71,    72,
     847,     0,     0,   722,     0,  1767,     0,     0,     0,     0,
     459,     0,     0,     0,   804,   804,    74,   483,    75,     0,
       0,     0,     0,     0,     0,  1093,     0,  1913,  1096,     0,
       0,     0,  1916,   799,   799,     0,     0,    76,    77,  1362,
       0,   450,     0,  1923,     0,  1362,     0,     0,    80,    81,
       0,     0,     0,     0,   495,     0,     0,     0,  1802,     0,
       0,   352,   483,     0,     0,     0,   833,  1805,   833,     0,
       0,  1806,     0,     0,  1362,   264,     0,   512,     0,     0,
       0,   352,  1105,   352,   834,   266,     0,   352,   352,   352,
       0,     0,     0,     0,   558,     0,     0,     0,     0,   495,
       0,  1170,     0,   722,     0,  1174,     0,   352,     0,  1178,
       0,     0,     0,   254,     0,     0,   459,     0,   495,     0,
     495,     0,     0,     0,   495,   495,   495,     0,     0,     0,
     149,     0,     0,   323,    65,    66,    67,    68,    69,    70,
      71,    72,  1362,     0,   495,     0,     0,     0,     0,   325,
       0,     0,     0,   101,     0,     0,   155,  2004,  2005,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   266,     0,
       0,     0,   450,     0,     0,     0,     0,   352,     0,     0,
      77,     0,     0,   856,   151,   450,     0,     0,     0,     0,
     266,     0,     0,     0,     0,     0,     0,   352,     0,  1304,
       0,     0,     0,     0,   266,  1209,     0,     0,     0,     0,
     666,   101,     0,     0,   495,     0,   149,   266,   231,   232,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   209,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
     266,     0,     0,     0,   151,   483,     0,   276,     0,     0,
     459,     0,     0,  2079,     0,  1671,    77,    14,    15,    16,
      17,    18,  1672,     0,   266,     0,    80,    81,   149,     0,
       0,   266,    65,    66,    67,    68,    69,    70,    71,    72,
     310,     0,     0,     0,   315,     0,     0,     0,     0,     0,
     101,   804,   149,   321,   173,   174,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,   354,
     799,     0,     0,     0,     0,     0,    58,  1393,    77,     0,
       0,  2135,     0,     0,     0,   352,   352,     0,     0,   833,
       0,     0,     0,   321,     0,   461,   833,     0,     0,     0,
     512,  1126,     0,     0,     0,  1105,   315,   489,  1362,     0,
     149,  1394,   834,     0,    65,    66,    67,    68,    69,    70,
      71,    72,   495,   495,     0,  1426,  1483,     0,  1430,     0,
       0,     0,  1434,     0,     0,     0,   540,     0,    74,     0,
      75,   352,     0,     0,   149,   310,   173,   174,    65,    66,
      67,    68,    69,    70,    71,    72,   565,     0,     0,    76,
      77,   570,   572,     0,   209,     0,     0,     0,     0,     0,
      80,    81,     0,   310,     0,     0,     0,     0,   495,     0,
       0,     0,     0,   151,   310,     0,     0,   594,     0,     0,
       0,   596,   151,     0,     0,     0,   597,  1278,  1279,     0,
       0,   450,     0,     0,     0,     0,     0,   572,     0,   310,
       0,     0,     0,   621,     0,     0,     0,     0,  1485,     0,
       0,     0,     0,     0,     0,   630,     0,   450,     0,     0,
       0,     0,     0,   321,   450,     0,   149,   512,   173,   174,
      65,    66,    67,    68,    69,    70,    71,    72,  1185,     0,
       0,     0,     0,     0,   654,     0,     0,   678,   262,    85,
       0,     0,     0,    58,     0,     0,   352,     0,     0,     0,
     685,     0,   323,     0,   685,     0,     0,     0,   151,     0,
       0,     0,     0,     0,     0,     0,  2137,     0,   325,   483,
       0,  1355,     0,     0,     0,     0,  1487,   149,     0,   231,
     232,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     266,     0,   512,     0,     0,     0,     0,     0,   321,     0,
     483,     0,     0,     0,   151,    74,     0,    75,     0,     0,
       0,   512,    58,     0,     0,  1375,  1362,     0,     0,     0,
       0,  1362,  1362,  1362,  1604,     0,  2120,    77,     0,     0,
     547,     0,   321,   459,     0,  1613,     0,    80,    81,     0,
       0,     0,     0,     0,   983,     0,   149,     0,   231,   232,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   315,     0,     0,  1401,   654,  1404,     0,   352,     0,
       0,   352,   352,     0,  1618,     0,    75,     0,  1408,  1409,
       0,   335,   315,     0,  1414,  1415,     0,     0,     0,   336,
     337,   338,   339,     0,  1423,  1393,    77,     0,     0,     0,
       0,     0,     0,     0,     0,   495,     0,     0,   495,   495,
       0,     0,     0,     0,     0,     0,     0,   151,   151,   151,
     151,  1443,   151,   151,  1446,     0,     0,     0,  1673,   331,
     149,     0,   173,   174,    65,    66,    67,    68,    69,    70,
      71,    72,   450,     0,  1677,     0,   450,   450,     0,   321,
     321,     0,     0,     0,     0,     0,   489,   149,     0,   915,
     450,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     149,   310,   231,   232,    65,    66,    67,    68,    69,    70,
      71,    72,     0,   340,     0,     0,  1505,   262,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,   341,     0,     0,     0,     0,     0,     0,     0,   483,
       0,     0,     0,     0,   354,     0,   101,  1527,     0,   831,
      77,     0,     0,   663,  1362,  1531,  1362,  1533,  1535,     0,
      80,   832,     0,   685,   964,   151,     0,   666,     0,     0,
    1545,     0,  1546,   665,  1547,     0,     0,     0,   975,     0,
       0,  1556,     0,     0,     0,     0,     0,   654,     0,     0,
       0,     0,   984,     0,     0,     0,     0,     0,     0,     0,
     685,     0,     0,     0,     0,     0,     0,  1480,     0,     0,
       0,     0,   321,     0,     0,     0,     0,     0,     0,  1494,
     321,     0,     0,   321,   321,     0,   321,     0,     0,   266,
       0,     0,     0,     0,     0,   321,     0,  1826,   321,   321,
     321,   149,     0,   173,   174,    65,    66,    67,    68,    69,
      70,    71,    72,  1608,  1609,  1673,  1823,     0,     0,    58,
    1673,     0,   450,     0,   266,     0,  1673,     0,  1673,     0,
       0,  1677,     0,     0,     0,     0,  1677,     0,  1631,     0,
       0,     0,  1838,     0,  1677,  1636,     0,     0,     0,  1637,
     501,   331,   151,   149,   489,   231,   232,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,  1085,     0,     0,     0,  1654,     0,     0,   321,     0,
       0,    74,   149,    75,   375,   376,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,   459,   685,   964,
       0,     0,   233,    77,     0,  1111,     0,     0,     0,     0,
       0,   151,     0,    80,    81,     0,     0,     0,   489,     0,
     489,     0,     0,     0,   489,   489,   489,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,     0,     0,   377,
       0,     0,     0,   151,   489,   149,   378,   173,   174,    65,
      66,    67,    68,    69,    70,    71,    72,     0,  1826,  1826,
       0,  1650,   149,     0,   231,   232,    65,    66,    67,    68,
      69,    70,    71,    72,     0,  1766,     0,  1823,  1823,     0,
     266,     0,  1770,     0,  1772,     0,     0,     0,     0,     0,
       0,     0,  1673,     0,   149,  1673,   231,   232,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   331,  1949,  1273,
       0,  1677,     0,     0,   489,     0,     0,     0,     0,   957,
       0,   155,   321,     0,   450,     0,   958,    14,    15,    16,
      17,    18,     0,     0,   685,     0,     0,  1308,   266,     0,
       0,     0,     0,     0,  1314,     0,     0,     0,     0,     0,
       0,  1317,     0,     0,     0,     0,     0,   323,  1318,  1808,
    1742,  1743,   783,   784,   785,   786,   787,   788,   789,   790,
     791,   792,   793,   325,     0,     0,   218,  1826,   176,   179,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,   315,   354,     0,     0,     0,  1823,     0,     0,     0,
       0,     0,     0,   794,     0,  1673,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   226,     0,     0,     0,     0,
     149,  1677,   231,   232,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   151,     0,     0,     0,     0,     0,    74,     0,
      75,     0,  2082,     0,     0,     0,  1826,     0,     0,     0,
       0,     0,     0,     0,     0,   317,     0,     0,   318,   233,
      77,     0,   489,   489,     0,  1823,     0,     0,     0,     0,
      80,    81,     0,   342,     0,     0,   151,     0,     0,     0,
       0,     0,     0,     0,     0,  1908,  1909,   149,  1826,   598,
      64,    65,    66,    67,    68,    69,    70,    71,    72,  1918,
       0,     0,   266,     0,   151,   151,     0,  2121,   331,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   489,     0,
       0,     0,   149,   325,   173,   174,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,   522,   151,  1855,     0,
    1076,     0,     0,     0,  1864,     0,  1826,  1826,   149,     0,
     231,   232,    65,    66,    67,    68,    69,    70,    71,    72,
     155,     0,     0,     0,     0,  2121,  2121,     0,     0,  1464,
       0,     0,     0,     0,     0,     0,    74,     0,  1273,   638,
       0,   629,   325,     0,   582,   583,  1826,     0,  1897,   117,
       0,     0,   117,     0,     0,   176,     0,  2120,    77,     0,
       0,   547,     0,     0,   321,  2121,     0,     0,    80,    81,
     176,  1273,     0,     0,     0,     0,     0,     0,     0,   149,
       0,   325,     0,    65,    66,    67,    68,    69,    70,    71,
      72,  1359,     0,     0,     0,  1360,  1522,  1361,     0,     0,
      58,     0,     0,   354,   633,     0,     0,   117,     0,     0,
       0,     0,   637,   639,     0,   321,     0,   646,   266,     0,
       0,     0,     0,     0,     0,     0,   654,     0,     0,    77,
       0,     0,  1796,   117,   149,   570,   231,   232,    65,    66,
      67,    68,    69,    70,    71,    72,     0,  1951,  1952,   268,
       0,     0,     0,   117,  1962,   342,     0,   354,   342,     0,
       0,   321,    74,     0,    75,     0,  1975,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1984,     0,  1985,     0,
       0,     0,     0,   322,    77,     0,   117,     0,     0,  1996,
     117,  1998,  1999,  2000,    80,    81,   117,     0,     0,   117,
       0,     0,   149,   268,   600,   601,    65,    66,    67,    68,
      69,    70,    71,    72,   348,   117,   380,     0,     0,     0,
       0,     0,     0,     0,     0,   489,     0,     0,   489,   489,
       0,     0,     0,     0,     0,     0,     0,     0,  2119,   454,
     149,     0,   231,   232,    65,    66,    67,    68,    69,    70,
      71,    72,   117,   454,    78,  2032,     0,   268,   226,  2037,
       0,     0,     0,     0,  2042,     0,     0,     0,    74,     0,
     850,   851,     0,     0,  1464,  1464,  1464,   155,   572,   321,
     321,     0,     0,     0,     0,     0,     0,     0,     0,  1671,
      77,   117,     0,     0,     0,  2157,  1672,     0,     0,  1696,
      80,    81,     0,  1696,  1696,     0,     0,     0,   117,     0,
     117,     0,     0,     0,     0,     0,     0,  1696,  2173,   117,
    2085,     0,     0,     0,   268,     0,     0,     0,     0,   266,
     117,     0,  2094,  2182,     0,     0,  2097,     0,     0,     0,
       0,     0,     0,     0,     0,   603,     0,     0,   117,  2111,
       0,     0,     0,   117,     0,   117,     0,     0,   268,   117,
       0,     0,     0,   268,     0,     0,   354,     0,     0,   268,
       0,     0,     0,     0,     0,     0,     0,     0,   406,   117,
     407,   408,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   155,  2144,     0,     0,     0,     0,     0,  2023,
     117,     0,   268,   117,     0,     0,     0,     0,   947,     0,
       0,     0,     0,     0,     0,     0,   117,   342,     0,     0,
     117,     0,     0,     0,     0,     0,     0,     0,     0,   742,
       0,  2167,    78,   417,     0,     0,     0,     0,  2171,     0,
       0,     0,     0,     0,     0,     0,     0,   400,     0,     0,
     401,     0,   402,     0,   403,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   454,     0,  2191,   992,     0,     0,
    2193,   404,  2171,     0,     0,     0,   149,     0,     0,     0,
      65,    66,    67,    68,    69,    70,    71,    72,  1359,   321,
       0,     0,  1360,  2193,  1361,  1840,     0,     0,   454,     0,
       0,   405,   406,     0,   407,   408,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,  1852,
     411,   412,   413,     0,   414,   415,    77,   117,     0,  1798,
       0,   454,    74,     0,     0,     0,     0,   268,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,   117,     0,
       0,     0,     0,   416,     0,     0,    78,   417,     0,     0,
       0,     0,     0,   418,    80,    81,   419,   420,   421,   422,
       0,     0,     0,     0,     0,     0,     0,     0,   155,   149,
       0,   231,   232,    65,    66,    67,    68,    69,    70,    71,
      72,   149,     0,   202,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   117,     0,     0,    74,     0,    75,
     321,     0,     0,     0,     0,   454,   454,  1128,     0,     0,
     268,     0,   117,     0,     0,     0,     0,  1143,  1671,    77,
       0,     0,     0,     0,     0,     0,     0,   117,     0,    80,
      81,    77,     0,     0,   856,  1941,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   149,   268,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   268,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   117,
     117,     0,   117,     0,    14,    15,    16,    17,    18,     0,
       0,  1696,     0,     0,   111,     0,     0,   380,     0,   117,
     454,     0,   268,     0,     0,     0,     0,    78,  1213,     0,
     117,     0,     0,     0,     0,     0,   148,     0,     0,     0,
       0,     0,     0,   117,     0,     0,   268,     0,     0,     0,
     603,     0,     0,   268,     0,     0,   117,     0,   989,     0,
       0,     0,     0,    58,     0,     0,     0,     0,   117,     0,
       0,     0,   111,     0,     0,     0,   454,     0,     0,   117,
     117,     0,   454,     0,     0,     0,     0,     0,     0,     0,
       0,   454,     0,     0,   117,   117,   117,   149,     0,   231,
     232,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   205,     0,     0,     0,     0,     0,   277,     0,
       0,     0,     0,     0,     0,    74,     0,    75,   149,  2063,
     231,   232,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,   322,    77,     0,     0,
     454,   111,     0,     0,     0,     0,    74,    80,    81,     0,
       0,   111,     0,     0,     0,     0,   117,     0,     0,     0,
       0,     0,     0,  1696,   454,     0,     0,   233,    77,     0,
     357,     0,   117,     0,     0,     0,   117,     0,    80,    81,
       0,     0,     0,     0,   117,   454,     0,     0,     0,   117,
       0,  1696,  2063,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   117,     0,   117,     0,   490,   149,
     117,   117,   117,    65,    66,    67,    68,    69,    70,    71,
      72,  1359,     0,     0,  1696,  1360,     0,  1361,     0,   205,
     117,     0,     0,   149,     0,   231,   232,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   111,     0,     0,     0,
       0,     0,     0,  2160,     0,     0,     0,     0,     0,    77,
    1186,    74,   149,     0,   173,   174,    65,    66,    67,    68,
      69,    70,    71,    72,   111,     0,     0,     0,     0,     0,
       0,     0,   322,    77,   574,   111,     0,     0,   595,     0,
       0,   117,     0,    80,    81,   454,     0,     0,     0,     0,
     117,     0,     0,   357,  1484,  1486,  1488,   117,   454,     0,
     111,   505,     0,     0,   277,     0,     0,     0,     0,     0,
     117,     0,  1306,   454,   615,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   622,  1509,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   632,     0,   655,     0,     0,   277,  1213,
       0,     0,     0,     0,     0,     0,  1529,     0,     0,     0,
       0,   655,     0,     0,   652,   655,     0,   117,   454,     0,
       0,     0,     0,     0,     0,     0,     0,   184,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,    19,     0,    20,  1563,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     255,   256,     0,   257,    46,     0,    47,     0,   258,     0,
     741,    49,    50,    51,    52,    53,    54,    55,     0,   117,
       0,     0,     0,   117,     0,     0,     0,     0,   117,   117,
       0,     0,   117,     0,     0,     0,     0,     0,     0,     0,
       0,   782,   117,     0,     0,     0,     0,     0,     0,   117,
       0,     0,     0,     0,     0,     0,   655,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,   822,
       0,     0,     0,     0,   827,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   117,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   853,     0,   117,     0,   854,   855,
     117,     0,   858,     0,   117,     0,     0,     0,     0,     0,
    -462,     0,     0,     0,     0,     0,    58,   872,     0,  1683,
    1684,     0,     0,     0,     0,     0,   117,     0,     0,   357,
       0,     0,     0,  -462,     0,   117,     0,     0,     0,     0,
     902,     0,     0,     0,   454,     0,     0,   490,     0,     0,
     149,     0,   231,   232,    65,    66,    67,    68,    69,    70,
      71,    72,   111,     0,     0,     0,     0,     0,     0,     0,
     454,     0,     0,     0,     0,     0,     0,   454,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   831,
      77,   268,   117,   663,   357,   490,     0,   111,     0,   117,
      80,   832,     0,     0,     0,     0,     0,   942,     0,     0,
       0,   117,     0,     0,   655,   490,     0,     0,     0,     0,
       0,   949,   454,     0,     0,   357,  1306,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   655,  1562,
       0,     0,     0,     0,     0,     0,   972,     0,     0,     0,
       0,   655,   117,   454,     0,     0,     0,   117,     0,   305,
     184,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,  1020,    48,    49,    50,    51,    52,    53,    54,
      55,   117,     0,  1848,   117,   117,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   117,     0,     0,   490,   117,     0,     0,     0,
     117,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   357,     0,  1651,    63,    64,     0,     0,     0,   116,
     117,   117,   117,   117,   117,   117,   117,   357,     0,     0,
       0,   357,   268,     0,     0,     0,     0,     0,     0,   655,
     490,     0,    75,     0,   357,   454,     0,     0,     0,   454,
     454,     0,     0,     0,  1100,     0,  1101,     0,     0,   357,
       0,   357,   827,   454,    78,   357,   357,   357,     0,  -412,
       0,     0,     0,     0,     0,     0,     0,   116,     0,     0,
       0,     0,     0,     0,     0,   357,     0,     0,     0,  1144,
     268,     0,     0,     0,     0,     0,     0,     0,  1153,     0,
       0,     0,     0,     0,  1156,     0,    14,    15,    16,    17,
      18,     0,   454,     0,     0,     0,     0,   117,     0,     0,
       0,     0,     0,   279,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   117,     0,
       0,     0,     0,     0,     0,     0,   357,     0,     0,   652,
     111,     0,     0,     0,  1198,   357,   116,     0,     0,     0,
       0,     0,     0,     0,     0,    58,   116,     0,     0,     0,
     117,     0,     0,     0,     0,   655,     0,     0,   277,   117,
       0,     0,     0,   117,     0,   361,     0,     0,     0,   122,
       0,     0,   122,  2002,     0,     0,     0,     0,     0,   149,
       0,   231,   232,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   492,     0,     0,     0,    74,     0,    75,
       0,     0,     0,   490,     0,   454,     0,     0,     0,     0,
       0,     0,  1335,     0,     0,     0,     0,   122,  2120,    77,
       0,     0,   547,     0,     0,     0,     0,     0,     0,    80,
      81,   116,     0,     0,   268,   117,     0,     0,     0,     0,
       0,     0,     0,   122,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   116,
       0,     0,     0,   122,     0,     0,     0,     0,     0,     0,
     116,   292,     0,     0,   357,     0,     0,     0,   357,     0,
       0,     0,     0,   357,   357,     0,     0,   357,   361,     0,
       0,     0,     0,     0,   117,   116,   122,   357,     0,   279,
     122,     0,     0,     0,   357,     0,   122,     0,     0,   122,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     0,   117,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   357,
     657,     0,     0,   279,     0,     0,     0,     0,     0,   122,
       0,   357,     0,     0,     0,   357,   657,     0,     0,   357,
     657,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     268,     0,     0,     0,     0,     0,     0,   148,     0,   111,
       0,   122,     0,     0,     0,     0,   149,   454,   231,   232,
      65,    66,    67,    68,    69,    70,    71,    72,   122,     0,
     122,     0,     0,     0,     0,   122,     0,     0,     0,   122,
       0,     0,   111,     0,    74,     0,    75,     0,     0,     0,
     122,     0,     0,     0,     0,     0,   782,     0,     0,     0,
       0,     0,     0,     0,     0,  1671,    77,   277,     0,     0,
       0,     0,     0,   122,   357,   122,    80,    81,     0,   122,
       0,   657,     0,     0,     0,     0,     0,   428,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   655,     0,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1544,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   357,   490,     0,
       0,     0,     0,     0,     0,   117,     0,     0,     0,     0,
       0,     0,  1570,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   361,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   492,     0,     0,     0,     0,     0,     0,   117,
       0,     0,     0,     0,   122,     0,     0,   116,     0,     0,
       0,     0,     0,     0,     0,     0,   357,     0,     0,   357,
     357,     0,     0,     0,     0,     0,     0,   117,   117,     0,
       0,   268,     0,     0,     0,     0,     0,   357,   122,     0,
       0,   357,     0,     0,     0,   357,     0,     0,   117,   361,
     492,     0,   116,     0,     0,     0,     0,     0,     0,     0,
     117,     0,   121,     0,     0,   121,     0,   122,     0,   657,
     492,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     361,     0,     0,     0,     0,     0,     0,     0,   122,     0,
     111,     0,     0,   657,   111,   111,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   657,     0,   111,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     121,     0,     0,     0,   709,     0,     0,     0,   428,   715,
       0,     0,     0,     0,     0,     0,     0,     0,   724,   725,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,   428,   428,   122,   122,   490,     0,     0,
       0,     0,   357,     0,  1756,     0,   121,     0,     0,     0,
       0,     0,     0,     0,   428,     0,     0,   122,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   121,
     492,   154,     0,   121,     0,     0,     0,   428,     0,   121,
       0,     0,   121,     0,     0,   357,   361,     0,     0,     0,
       0,     0,   122,     0,   357,     0,     0,     0,   357,     0,
       0,     0,   361,     0,     0,     0,   361,     0,     0,     0,
       0,     0,     0,     0,   657,   492,     0,     0,     0,   361,
       0,     0,   121,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   361,   121,   361,     0,     0,     0,
     361,   361,   361,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   208,     0,     0,     0,     0,  1756,   122,     0,
     361,     0,     0,     0,     0,     0,   122,     0,     0,   122,
     122,     0,   122,     0,   121,     0,     0,     0,     0,     0,
     277,   122,     0,     0,   122,   122,   122,     0,     0,     0,
       0,   121,     0,   121,     0,     0,     0,     0,   121,     0,
       0,     0,   121,     0,     0,     0,     0,     0,     0,   314,
       0,     0,     0,   121,     0,     0,     0,     0,   208,     0,
       0,   361,     0,     0,     0,   116,     0,     0,     0,     0,
     361,     0,     0,     0,     0,     0,   121,     0,   121,     0,
       0,     0,   121,     0,     0,     0,     0,     0,     0,     0,
     657,     0,     0,   279,     0,     0,     0,     0,   453,     0,
    1901,  1902,   121,     0,   122,     0,     0,     0,     0,     0,
       0,   478,     0,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,   492,     0,
       0,    46,     0,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   571,     0,   575,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   121,     0,     0,
       0,     0,   111,   428,   428,   428,   428,   428,   428,   428,
     428,   428,   428,   428,   428,   428,   428,   428,   428,   428,
     428,   428,   154,     0,     0,     0,     0,     0,     0,   361,
       0,   121,     0,   361,     0,     0,     0,  1987,   361,   361,
       0,     0,   361,     0,     0,   122,    75,     0,   575,     0,
       0,     0,   361,     0,     0,     0,     0,   122,   122,   361,
     121,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1756,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   121,     0,     0,     0,     0,     0,     0,   428,     0,
       0,     0,     0,   126,   361,     0,   126,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   361,     0,  2031,     0,
     361,     0,     0,     0,   361,     0,     0,     0,     0,     0,
     655,     0,     0,     0,     0,     0,     0,   122,     0,     0,
       0,     0,     0,   453,     0,  2059,     0,     0,     0,  2060,
       0,     0,     0,     0,     0,     0,     0,     0,   121,   121,
       0,   126,     0,     0,   116,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,   208,     0,     0,
     121,     0,     0,     0,     0,     0,     0,   126,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   116,     0,     0,
       0,     0,   111,   655,     0,     0,   824,   126,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   279,   357,     0,   121,     0,   478,     0,   361,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
     126,     0,     0,     0,   126,     0,     0,     0,     0,     0,
     126,     0,   657,   126,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   428,
       0,     0,     0,     0,     0,   428,     0,     0,     0,     0,
       0,     0,   361,   492,     0,     0,   428,     0,     0,     0,
       0,   121,     0,   126,   453,   453,     0,     0,     0,   121,
       0,     0,   121,   121,     0,   121,   126,     0,     0,     0,
       0,     0,     0,     0,   121,   170,   122,   121,   121,   121,
       0,     0,     0,     0,     0,   122,   428,     0,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,   170,     0,   126,     0,     0,     0,     0,
       0,   361,     0,     0,   361,   361,     0,     0,     0,     0,
     122,     0,   126,     0,   126,     0,     0,   122,     0,   126,
       0,     0,   361,   126,     0,     0,   361,     0,     0,     0,
     361,     0,     0,     0,   126,     0,     0,     0,     0,   170,
       0,     0,   122,     0,     0,     0,     0,   121,     0,     0,
       0,     0,   170,     0,   170,     0,     0,   126,     0,   126,
       0,   122,     0,   126,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,     0,   453,     0,   116,
     116,     0,     0,   126,     0,   453,   382,     0,   453,   453,
       0,   453,     0,   116,     0,     0,     0,     0,     0,     0,
     453,     0,     0,   453,   453,   453,     0,   122,     0,     0,
       0,   382,     0,     0,     0,     0,     0,     0,     0,     0,
     428,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   492,     0,     0,     0,     0,   361,     0,     0,
     170,     0,     0,     0,   170,     0,     0,   170,   170,     0,
       0,   170,     0,     0,   170,   170,     0,   170,   126,   170,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,   453,     0,     0,     0,     0,     0,     0,
     121,   121,     0,     0,     0,     0,     0,     0,     0,     0,
     361,     0,   126,     0,     0,     0,     0,     0,   428,   361,
       0,     0,     0,   361,     0,     0,     0,     0,     0,     0,
     122,   122,   122,   122,   122,   122,   122,     0,   428,     0,
       0,   126,     0,     0,     0,     0,     0,     0,     0,   170,
       0,     0,   170,     0,     0,   122,   428,   428,   428,   122,
     122,     0,   126,   428,   428,     0,     0,     0,     0,     0,
     121,  1313,     0,   122,     0,     0,   170,   170,     0,    14,
      15,    16,    17,    18,     0,     0,   428,     0,     0,     0,
       0,   170,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   279,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   400,     0,     0,   401,     0,
     402,     0,   403,     0,     0,     0,   428,   428,     0,   126,
     126,     0,     0,     0,     0,     0,  1287,   453,    58,   404,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
       0,   126,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   407,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,     0,   414,   415,     0,     0,   126,   170,     0,     0,
      74,     0,    75,     0,     0,     0,   478,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   416,     0,     0,    78,   417,     0,     0,     0,     0,
       0,   418,   482,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,   121,
       0,     0,     0,     0,     0,   382,     0,     0,   121,     0,
       0,     0,   126,     0,     0,     0,     0,   121,     0,     0,
     126,   170,     0,   126,   126,   122,   126,   116,     0,     0,
       0,     0,     0,     0,     0,   126,     0,     0,   126,   126,
     126,     0,     0,   121,     0,     0,     0,     0,     0,     0,
     121,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,     0,     0,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   121,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   382,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,   126,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1287,     0,     0,     0,     0,
     121,     0,     0,     0,  1463,   657,     0,   170,   170,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     170,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   453,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   116,
       0,     0,     0,     0,     0,     0,     0,   122,     0,     0,
       0,     0,     0,     0,   428,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   116,   657,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     453,     0,     0,     0,     0,     0,     0,     0,   361,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   126,
     116,     0,     0,   121,   121,   121,   121,   121,   121,   121,
       0,   126,   126,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   453,     0,   121,   170,
     170,     0,   121,   121,     0,     0,   170,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   170,     0,     0,   170,   170,     0,   170,     0,   170,
     170,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   126,     0,     0,     0,     0,     0,     0,   368,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   122,
       0,     0,   170,     0,     0,     0,   170,     0,     0,     0,
     170,   121,  1554,     0,     0,   479,   368,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,   122,     0,  1463,
    1463,  1463,   154,  1660,  1661,  1665,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   550,     0,
       0,     0,     0,     0,     0,   550,   400,   428,     0,   401,
     122,   402,     0,   403,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   170,   170,     0,    58,
     404,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     170,     0,     0,     0,     0,     0,     0,   428,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
     405,   406,     0,   407,   408,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   409,   410,   397,     0,   411,
     412,   413,     0,   414,   415,     0,   550,     0,   121,     0,
       0,    74,     0,    75,     0,     0,     0,  1287,     0,     0,
     126,     0,     0,     0,     0,     0,     0,     0,     0,   126,
       0,     0,   416,   368,   667,    78,   417,     0,   126,     0,
       0,     0,   418,  1555,    81,   419,   420,   421,   422,     0,
       0,     0,     0,   688,     0,     0,     0,     0,     0,     0,
     428,     0,   428,     0,   126,     0,     0,   121,     0,     0,
       0,   126,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   126,   170,     0,   121,
       0,   428,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   453,   126,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   428,     0,   550,     0,     0,     0,     0,     0,
       0,   170,     0,     0,     0,     0,   170,     0,     0,   170,
       0,   550,   817,   170,   550,   820,     0,     0,     0,     0,
       0,   126,     0,     0,   368,     0,     0,     0,   667,     0,
       0,     0,     0,     0,     0,   428,     0,     0,     0,     0,
     121,   479,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1287,     0,     0,     0,     0,     0,     0,
     550,     0,     0,     0,   550,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   453,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   368,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   126,   126,   126,   126,   126,   126,
     126,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   170,     0,   126,
       0,     0,     0,   126,   126,     0,   170,   170,     0,     0,
     550,     0,     0,   368,     0,     0,     0,   126,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   962,   368,     0,     0,     0,     0,     0,     0,
       0,     0,   121,   667,     0,     0,     0,   667,     0,     0,
       0,     0,     0,     0,   980,     0,   368,     0,     0,     0,
       0,     0,     0,     0,     0,   170,     0,     0,     0,     0,
     121,     0,     0,     0,     0,     0,   170,     0,     0,   170,
       0,   170,   170,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   126,     0,     0,     0,     0,     0,     0,     0,
     214,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   272,     0,     0,    14,
      15,    16,    17,    18,     0,     0,    20,   170,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -486,  -486,     0,  -486,    46,     0,    47,
       0,  -486,     0,   368,     0,     0,   214,     0,     0,     0,
     332,     0,     0,     0,     0,     0,     0,     0,    58,   550,
     550,     0,   373,     0,     0,     0,     0,     0,     0,   126,
     550,  1094,     0,   550,  1097,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   214,   962,   368,     0,
     170,     0,   667,     0,   667,   667,     0,     0,     0,   126,
     499,   667,     0,     0,   504,     0,     0,   368,     0,   368,
       0,     0,     0,   368,   368,   368,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   368,   168,   550,     0,     0,     0,   550,
       0,     0,     0,     0,     0,     0,   550,  1171,     0,     0,
     550,  1175,     0,     0,   550,  1179,     0,   214,   126,     0,
       0,     0,  1183,     0,     0,     0,     0,     0,     0,     0,
       0,   272,     0,     0,     0,     0,     0,   170,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     126,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   368,   550,   170,     0,     0,   297,     0,
       0,     0,     0,     0,     0,     0,   504,     0,     0,     0,
       0,   303,     0,   304,     0,     0,   214,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   667,     0,   170,     0,
       0,     0,     0,     0,   170,     0,     0,   660,     0,   677,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   452,     0,     0,     0,     0,     0,     0,
       0,   126,     0,     0,     0,     0,     0,   484,     0,     0,
     479,   368,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   513,     0,   513,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   739,     0,     0,     0,     0,     0,     0,     0,   170,
       0,     0,     0,     0,     0,     0,   552,   553,     0,     0,
     557,     0,     0,   560,   561,     0,   563,   735,   564,     0,
       0,     0,     0,     0,     0,   214,   550,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   368,   368,     0,     0,   667,   667,     0,     0,     0,
     170,   170,   667,     0,     0,     0,     0,     0,   660,     0,
       0,     0,     0,     0,   845,     0,     0,     0,     0,     0,
       0,     0,   627,     0,     0,     0,   170,   170,     0,     0,
       0,     0,     0,     0,   382,     0,     0,     0,     0,   170,
       0,     0,     0,     0,     0,     0,     0,   368,     0,     0,
     550,  1427,     0,   550,  1431,     0,     0,   550,  1435,     0,
       0,     0,     0,   126,     0,   649,   650,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     682,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   126,   214,   214,     0,     0,     0,     0,     0,   499,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   910,   912,
       0,     0,     0,     0,   126,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     170,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   368,     0,     0,     0,     0,   499,     0,   966,
       0,     0,     0,     0,     0,     0,   815,     0,     0,     0,
       0,     0,     0,     0,     0,   368,     0,     0,     0,     0,
     660,   667,  1552,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   513,   170,     0,     0,     0,     0,   513,     0,
       0,     0,     0,   869,     0,   214,   368,     0,     0,     0,
       0,     0,     0,   739,     0,     0,   739,   739,     0,   739,
       0,     0,     0,     0,     0,     0,     0,     0,   739,     0,
       0,   739,   739,   739,     0,     0,     0,     0,     0,   735,
     898,     0,     0,     0,     0,   735,     0,     0,     0,   550,
    1605,     0,     0,     0,   735,     0,     0,     0,     0,     0,
     550,  1614,     0,   667,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   735,   368,     0,     0,   368,   368,     0,
       0,     0,     0,     0,     0,     0,     0,   499,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   170,     0,     0,
       0,     0,     0,     0,   941,     0,     0,     0,     0,  1073,
       0,   214,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   499,     0,   484,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   974,     0,     0,
       0,   499,     0,   499,     0,     0,     0,   499,   499,   499,
       0,     0,     0,     0,     0,     0,   978,   979,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   499,     0,   986,
       0,     0,     0,     0,     0,     0,     0,  1005,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1013,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   368,     0,     0,     0,     0,
       0,     0,   869,  1034,     0,     0,  1036,     0,  1038,     0,
       0,     0,     0,     0,  1005,     0,  1050,  1005,     0,     0,
       0,     0,     0,   667,     0,     0,     0,   499,     0,     0,
       0,     0,     0,     0,     0,   214,     0,     0,     0,     0,
       0,     0,     0,     0,  1078,     0,     0,     0,     0,   845,
       0,     0,     0,     0,     0,     0,     0,  1080,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1089,     0,
       0,   207,     0,     0,     0,     0,     0,     0,  1087,  1088,
       0,     0,     0,     0,   484,  1092,     0,     0,     0,  1078,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
    1115,     0,   550,  1118,  1119,     0,  1122,     0,  1124,  1125,
    1147,     0,     0,   513,     0,     0,     0,     0,   550,     0,
       0,     0,     0,     0,     0,  1158,     0,   207,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   207,     0,     0,     0,     0,     0,     0,
       0,  1169,     0,     0,  1184,  1173,     0,     0,     0,  1177,
       0,     0,     0,     0,     0,     0,     0,   207,     0,     0,
       0,     0,     0,     0,     0,   499,   499,     0,     0,     0,
       0,   486,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   452,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1305,  1307,     0,     0,
       0,     0,     0,     0,   484,  1298,  1299,     0,     0,     0,
       0,   499,     0,     0,     0,     0,     0,     0,   207,  1315,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   550,   550,     0,     0,     0,     0,     0,
       0,     0,  1078,     0,     0,     0,     0,     0,     0,   550,
    1348,     0,   739,     0,     0,     0,     0,  1005,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   207,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   739,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   207,     0,
       0,   513,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   735,     0,     0,     0,     0,   272,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   214,     0,
       0,     0,     0,     0,     0,     0,  1315,     0,     0,   660,
       0,     0,   550,     0,     0,     0,     0,     0,     0,     0,
     550,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   513,     0,  1418,     0,  1421,     0,     0,
     373,     0,     0,     0,   739,     0,     0,     0,     0,     0,
    1419,     0,     0,     0,     0,  1425,   207,     0,  1429,     0,
       0,     0,  1433,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   550,  2083,   207,
       0,   550,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   499,     0,
       0,   499,   499,     0,     0,     0,     0,     0,  1497,  1497,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   550,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   739,   739,   739,
       0,     0,   739,   739,     0,     0,     0,     0,     0,   504,
       0,     0,     0,   207,   207,     0,     0,     0,     0,     0,
     486,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1548,   550,   550,     0,     0,     0,  1557,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1550,     0,  1690,  1698,
       0,     0,  1690,  1708,  1005,  1560,  1561,   484,  1715,     0,
       0,     0,  1719,     0,  1721,     0,  1708,   272,     0,     0,
       0,   550,     0,     0,   513,     0,     0,     0,   207,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,   869,     0,     0,     0,     0,     0,     0,   486,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1603,     0,     0,     0,     0,     0,
       0,   207,     0,     0,     0,  1612,     0,     0,  1616,     0,
    1619,  1620,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   207,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1648,  1649,     0,     0,
       0,     0,     0,     0,     0,     0,  1646,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1005,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   513,   214,     0,   869,     0,     0,     0,  1814,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   486,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   272,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   207,     0,     0,     0,     0,     0,     0,  1750,
       0,     0,     0,  1851,     0,     0,  1034,     0,     0,     0,
       0,     0,     0,   486,     0,  1768,  1769,     0,     0,     0,
       0,     0,  1870,  1872,     0,     0,   513,     0,     0,     0,
       0,     0,   486,     0,   486,     0,     0,     0,   486,   486,
     486,     0,     0,     0,     0,   513,     0,   869,     0,     0,
       0,     0,  1890,     0,     0,     0,     0,     0,   486,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   739,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1616,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1810,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   486,   452,
       0,     0,     0,     0,  1839,     0,   207,   272,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1960,     0,     0,     0,     0,
       0,     0,  1963,     0,  1965,     0,     0,  1970,  1974,     0,
    1708,     0,     0,     0,     0,  1980,     0,     0,     0,     0,
       0,     0,     0,     0,  1885,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   207,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1898,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1920,     0,
       0,  1922,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   739,     0,     0,     0,     0,  1936,     0,  1930,
    1931,     0,     0,  2048,     0,     0,   486,   486,  2053,  2055,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1945,  1946,     0,  2074,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1950,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   486,     0,     0,   739,     0,     0,   504,     0,
    2098,     0,  2101,     0,     0,  2103,  2105,     0,     0,     0,
    2108,  2110,     0,   260,   184,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -487,  -487,     0,
    -487,    46,     0,    47,     0,  -487,     0,     0,     0,  2020,
       0,     0,     0,     0,  2151,  2153,  2155,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2192,     0,     0,
       0,     0,     0,     0,     0,     0,  1005,  2177,  2179,  2181,
       0,     0,     0,     0,  1479,     0,     0,   207,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   207,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     207,     0,  2081,     0,     0,   400,    75,     0,   401,     0,
     402,     0,   403,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1215,    78,   404,
    1217,   207,  1218,  -247,  -247,  1219,  1220,  1221,  1222,  1223,
    1224,  1225,  1226,  1227,  1228,  1229,  1230,  -341,  -341,  1231,
    1232,  1233,  1234,  1235,  1236,  1237,     0,  1238,     0,   405,
     406,     0,   507,   408,  1239,  1240,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,  1241,   411,   412,
     413,     0,   414,   415,  2192,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,   486,
       0,  1479,   486,   486,     0,     0,     0,     0,     0,     0,
    -247,  1242,     0,     0,    78,   417,     0,     0,     0,   301,
       0,   418,    80,    81,   419,   420,   421,   422,     0,     0,
       0,     0,   400,     0,     0,   401,  -187,   402,     0,   403,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1215,     0,   404,  1217,     0,  1218,
    -248,  -248,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,
    1227,  1228,  1229,  1230,  -341,  -341,  1231,  1232,  1233,  1234,
    1235,  1236,  1237,     0,  1238,     0,   405,   406,     0,   507,
     408,  1239,  1240,    65,    66,    67,    68,    69,    70,    71,
      72,   409,   410,   397,  1241,   411,   412,   413,     0,   414,
     415,  1896,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1479,     0,
       0,     0,     0,     0,     0,     0,     0,  -248,  1242,     0,
     207,    78,   417,     0,     0,     0,   301,     0,   418,    80,
      81,   419,   420,   421,   422,     0,     0,     0,     0,   400,
       0,     0,   401,  -187,   402,     0,   403,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1215,     0,   404,  1217,     0,  1218,     0,     0,  1219,
    1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    1230,  -341,  -341,  1231,  1232,  1233,  1234,  1235,  1236,  1237,
       0,  1238,     0,   405,   406,     0,   507,   408,  1239,  1240,
      65,    66,    67,    68,    69,    70,    71,    72,   409,   410,
     397,  1241,   411,   412,   413,     0,   414,   415,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   207,     0,  1242,     0,     0,    78,   417,
       0,     0,     0,   301,     0,   418,    80,    81,   419,   420,
     421,   422,     0,     0,     0,     0,     0,     0,     0,     0,
    -187,     4,   184,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1214,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   400,     0,    46,
     401,    47,   402,     0,   403,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1215,
      58,  1216,  1217,     0,  1218,     0,     0,  1219,  1220,  1221,
    1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,  -341,
    -341,  1231,  1232,  1233,  1234,  1235,  1236,  1237,     0,  1238,
       0,   405,   406,    61,   507,   408,  1239,  1240,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,  1241,
     411,   412,   413,     0,   414,   415,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -3,  1242,     0,     0,    78,  1243,     0,     0,
       0,   301,     0,   418,    80,    81,   419,   420,   421,   422,
       0,     0,     0,     0,     0,     0,     0,     0,  -187,     4,
     184,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  1214,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   400,     0,    46,   401,    47,
     402,     0,   403,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,  1215,    58,  1216,
    1217,     0,  1218,     0,     0,  1219,  1220,  1221,  1222,  1223,
    1224,  1225,  1226,  1227,  1228,  1229,  1230,  -341,  -341,  1231,
    1232,  1233,  1234,  1235,  1236,  1237,     0,  1238,     0,   405,
     406,    61,   507,   408,  1239,  1240,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,  1241,   411,   412,
     413,     0,   414,   415,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1242,     0,     0,    78,  1243,     0,     0,     0,   301,
       0,   418,    80,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,     0,  -187,     4,   184,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   400,     0,    46,   401,    47,   402,     0,
     403,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   404,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,   406,    61,
     407,   408,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   409,   410,   397,     0,   411,   412,   413,     0,
     414,   415,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1702,  1703,  1704,     0,     0,     0,   416,
    1705,  1706,    78,  1243,     0,     0,     0,     0,     0,   418,
      80,    81,   419,   420,   421,   422,     0,     0,     0,     0,
       0,     0,     0,     0,  1707,     4,   184,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   400,     0,    46,   401,    47,   402,     0,   403,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   404,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,   406,    61,   407,   408,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   410,   397,     0,   411,   412,   413,     0,   414,   415,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1702,  1703,  1704,     0,     0,     0,   416,  1705,     0,
      78,  1243,     0,     0,     0,     0,     0,   418,    80,    81,
     419,   420,   421,   422,     0,     0,     0,     0,     0,     0,
       0,     0,  1707,     4,   184,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   400,
       0,    46,   401,    47,   402,     0,   403,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   404,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,   406,    61,   407,   408,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   409,   410,
     397,     0,   411,   412,   413,     0,   414,   415,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   416,     0,  1700,    78,  1243,
       0,     0,     0,     0,     0,   418,    80,    81,   419,   420,
     421,   422,     4,   184,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   400,     0,
      46,   401,    47,   402,     0,   403,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   404,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   405,   406,    61,   407,   408,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   409,   410,   397,
       0,   411,   412,   413,     0,   414,   415,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   416,     0,     0,    78,  1243,     0,
       0,     0,     0,     0,   418,    80,    81,   419,   420,   421,
     422,   184,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   400,     0,    46,   401,
      47,   402,     0,   403,   349,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     404,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     405,   406,     0,   407,   408,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   409,   410,   397,     0,   411,
     412,   413,     0,   414,   415,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   416,     0,     0,    78,   481,     0,     0,     0,
       0,     0,   418,   482,    81,   419,   420,   421,   422,   184,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   400,     0,    46,   401,    47,   402,
       0,   403,   349,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   404,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   405,   406,
       0,   407,   408,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   409,   410,   397,     0,   411,   412,   413,
       0,   414,   415,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     416,     0,     0,    78,  1302,     0,     0,     0,     0,     0,
     418,  1303,    81,   419,   420,   421,   422,   184,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   400,     0,    46,   401,    47,   402,     0,   403,
     349,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   404,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   405,   406,     0,   407,
     408,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   409,   410,   397,     0,   411,   412,   413,     0,   414,
     415,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   416,     0,
       0,    78,   829,     0,     0,     0,     0,     0,   418,   482,
      81,   419,   420,   421,   422,   184,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     400,     0,    46,   401,    47,   402,     0,   403,   349,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   404,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   405,   406,     0,   407,   408,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   409,
     410,   397,     0,   411,   412,   413,     0,   414,   415,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   416,     0,     0,    78,
     417,     0,     0,     0,     0,     0,   418,    80,    81,   419,
     420,   421,   422,   184,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   400,     0,
      46,   401,    47,   402,     0,   403,   349,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   404,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   405,   406,     0,   407,   408,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   409,   410,   397,
       0,   411,   412,   413,     0,   414,   415,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   416,     0,     0,    78,   829,     0,
       0,     0,     0,     0,   418,    80,    81,   419,   420,   421,
     422,  2030,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
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
       0,     0,     0,     0,     0,     0,    -2,    -2,  2058,     0,
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
     260,   184,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -487,  -487,     0,  -487,    46,     0,
      47,     0,  -487,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   149,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,   261,     0,     0,     0,
    -820,     0,     0,    80,    81,   260,   184,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -487,
    -487,     0,  -487,    46,     0,    47,     0,  -487,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   149,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,   261,     0,     0,     0,     0,     0,     0,    80,    81,
       4,   184,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
       0,     0,     0,     0,  -408,  -408,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -408,     0,     0,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,     4,   184,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,     0,     0,     0,     0,  -409,
    -409,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -409,     0,     0,     0,
      78,    79,     0,  1455,     0,  1456,     0,     0,    80,    81,
    1457,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1458,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1459,     0,     0,     0,    78,  1009,
       0,  1455,     0,  1456,     0,     0,    80,    81,  1457,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1458,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1657,     0,     0,     0,    78,  1009,     0,  1455,
       0,  1456,     0,     0,    80,    81,  1457,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1658,     0,     0,     0,    78,  1009,     0,  1455,     0,  1456,
       0,     0,    80,    81,  1457,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1458,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1659,     0,
       0,     0,    78,  1009,     0,     0,     0,     0,     0,     0,
      80,    81,   260,   184,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -487,  -487,     0,  -487,
      46,     0,    47,     0,  -487,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     260,     0,     0,     0,     0,     0,     0,    78,   261,     0,
      14,    15,    16,    17,    18,    80,    81,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -487,  -487,     0,  -487,    46,     0,
      47,     0,  -487,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   330,     0,     0,     0,
       0,     0,     0,    80,    81,   184,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   349,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   149,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,   606,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1104,    77,  -683,    78,
     663,     0,     0,     0,     0,     0,     0,    80,    81,   184,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -487,  -487,     0,  -487,    46,     0,    47,     0,
    -487,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   149,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   261,     0,     0,     0,  -824,     0,
       0,    80,    81,   184,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -487,  -487,     0,  -487,
      46,     0,    47,     0,  -487,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   149,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,   261,     0,
       0,     0,     0,     0,     0,    80,    81,   184,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     349,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
     606,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   662,     0,
    -683,    78,   663,     0,     0,     0,     0,     0,     0,    80,
      81,   184,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   349,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,   606,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   798,     0,  -683,    78,   547,     0,     0,     0,
       0,     0,     0,    80,    81,   184,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   349,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,  1137,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -691,    78,
     916,     0,     0,     0,     0,     0,     0,    80,    81,   184,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   349,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   350,    78,   351,     0,     0,     0,     0,     0,
       0,    80,    81,   184,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   349,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,  1626,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   916,     0,
       0,     0,     0,     0,     0,    80,    81,   184,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     349,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
    1628,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   916,     0,     0,     0,     0,     0,     0,    80,
      81,   184,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   349,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   330,     0,     0,     0,
       0,     0,     0,    80,    81,   184,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   349,    49,
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
     916,     0,     0,     0,     0,     0,     0,    80,    81,   184,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   349,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   351,     0,     0,     0,     0,     0,
       0,    80,    81,   184,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -487,  -487,     0,  -487,
      46,     0,    47,     0,  -487,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,  1479,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   400,     0,     0,   401,     0,
     402,     0,   403,     0,     0,     0,     0,    78,   261,     0,
       0,     0,     0,     0,     0,    80,    81,  1215,     0,   404,
    1217,     0,  1218,  1953,  1954,  1219,  1220,  1221,  1222,  1223,
    1224,  1225,  1226,  1227,  1228,  1229,  1230,     0,     0,  1231,
    1232,  1233,  1234,  1235,  1236,  1237,     0,  1238,     0,   405,
     406,     0,   507,   408,  1239,  1240,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,  1241,   411,   412,
     413,     0,   414,   415,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,  1479,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1242,     0,     0,    78,   417,     0,     0,     0,   301,
       0,   418,    80,    81,   419,   420,   421,   422,   400,     0,
       0,   401,     0,   402,     0,   403,  -187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1215,     0,   404,  1217,     0,  1218,     0,     0,  1219,  1220,
    1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,
       0,     0,  1231,  1232,  1233,  1234,  1235,  1236,  1237,     0,
    1238,     0,   405,   406,     0,   507,   408,  1239,  1240,    65,
      66,    67,    68,    69,    70,    71,    72,   409,   410,   397,
    1241,   411,   412,   413,     0,   414,   415,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1242,     0,     0,    78,   417,     0,
       0,     0,   301,     0,   418,    80,    81,   419,   420,   421,
     422,     0,     0,     0,     0,     0,     0,     0,     0,  -187,
     305,   184,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -412,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,     0,     0,     0,     0,
    -412,   305,   184,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -413,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,     0,
       0,  -413,    14,    15,    16,    17,    18,    19,   726,    20,
     727,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   400,     0,
      46,   401,    47,   402,     0,   403,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   404,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   728,     0,     0,     0,     0,  1230,
       0,  -341,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   405,   406,     0,   407,   408,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   409,   410,   397,
       0,   411,   412,   413,     0,   414,   415,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1242,     0,     0,    78,   729,     0,
       0,     0,   301,     0,   418,    80,    81,   730,   731,   421,
     422,    14,    15,    16,    17,    18,    19,   726,    20,   727,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   400,     0,    46,
     401,    47,   402,     0,   403,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   404,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   728,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,   406,     0,   407,   408,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,     0,
     411,   412,   413,     0,   414,   415,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   416,     0,     0,    78,   729,     0,     0,
       0,   301,     0,   418,    80,    81,   730,   731,   421,   422,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   400,     0,    46,   401,
      47,   402,     0,   403,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     404,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     405,   406,     0,   407,   408,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   409,   410,   397,     0,   411,
     412,   413,     0,   414,   415,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   416,     0,   447,    78,   448,     0,     0,     0,
       0,     0,   418,    80,    81,   419,   420,   421,   422,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   400,     0,    46,   401,    47,
     402,     0,   403,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   404,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   407,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,     0,   414,   415,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   416,     0,     0,    78,   729,     0,     0,     0,   301,
       0,   418,    80,    81,   419,   420,   421,   422,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   400,     0,    46,   401,    47,   402,
       0,   403,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   404,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   405,   406,
       0,   407,   408,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   409,   410,   397,     0,   411,   412,   413,
       0,   414,   415,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     416,     0,     0,    78,   448,     0,     0,     0,     0,     0,
     418,    80,    81,   419,   420,   421,   422,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   400,     0,    46,   401,    47,   402,     0,
     403,   349,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   404,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   405,   406,     0,
     407,   408,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   409,   410,   397,     0,   411,   412,   413,     0,
     414,   415,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   416,
       0,     0,    78,   829,     0,     0,     0,     0,     0,   418,
      80,    81,   419,   420,   421,   422,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   400,     0,    46,   401,    47,   402,     0,   403,
     349,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   404,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   405,   406,     0,   407,
     408,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   409,   410,   397,     0,   411,   412,   413,     0,   414,
     415,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   416,     0,
       0,    78,   417,     0,     0,     0,     0,     0,   418,    80,
      81,   419,   420,   421,   422,   184,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   717,     0,   718,
     719,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,    75,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,   -17,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   349,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   149,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   606,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -683,    78,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   149,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,    79,     0,     0,     0,  -822,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   149,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,   206,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   149,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
      78,    79,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   349,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   149,     0,   475,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   852,     0,     0,    78,   476,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   149,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   149,     0,   475,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   476,     0,     0,     0,
       0,     0,     0,    80,    81,   184,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   349,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   606,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,  -683,    78,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -487,  -487,     0,
    -487,    46,     0,    47,     0,  -487,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   149,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,     0,    78,   330,
       0,     0,     0,     0,     0,     0,    80,    81,   184,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   349,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,  1208,     0,     0,     0,   184,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,    78,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   349,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   852,
       0,     0,    78,   476,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   349,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   852,     0,     0,    78,   476,     0,
      63,    64,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1015,
      78,  1009,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,  1574,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,  1009,     0,     0,     0,
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
      78,   313,     0,    63,    64,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   206,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   349,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,    63,    64,
     349,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   351,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   313,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   349,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   476,     0,     0,
       0,     0,     0,     0,    80,    81,   184,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -487,
    -487,     0,  -487,    46,     0,    47,     0,  -487,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,   349,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   330,     0,     0,     0,     0,     0,
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
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,  1009,
       0,    63,    64,     0,     0,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,    80,
      81,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,   349,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   476,     0,    63,
      64,     0,     0,     0,    80,    81,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
    1009,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   349,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,    14,    15,    16,
      17,    18,    80,    81,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -487,  -487,     0,  -487,    46,     0,    47,     0,  -487,
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
       0,     0,    78,   330,     0,    14,    15,    16,    17,    18,
      80,    81,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -487,
    -487,     0,  -487,    46,     0,    47,     0,  -487,     0,     0,
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
      78,     0,     0,     0,     0,     0,     0,     0,    80,    81,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   400,
       0,    46,   401,    47,   402,     0,   403,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   405,   406,     0,   407,   408,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   409,   410,
     397,     0,   411,   412,   413,     0,   414,   415,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   416,     0,     0,    78,   417,
       0,     0,     0,     0,     0,   418,   482,    81,   419,   420,
     421,   422,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   400,     0,    46,   401,    47,   402,     0,   403,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   404,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,   406,     0,   407,   408,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   410,   397,     0,   411,   412,   413,     0,   414,   415,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   416,     0,     0,
      78,   417,     0,     0,     0,     0,     0,   418,    80,    81,
     419,   420,   421,   422,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   149,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,    78,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,   349,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    20,    78,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -487,  -487,     0,  -487,    46,
       0,    47,     0,  -487,     0,   184,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   185,     0,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,    75,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   717,     0,   718,   719,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,    75,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -487,  -487,
       0,  -487,    46,     0,    47,   400,  -487,     0,   401,     0,
     402,     0,   403,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,   404,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   407,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,     0,   414,   415,     0,     0,   400,    75,     0,   401,
      74,   402,     0,   403,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1702,  1703,  1704,     0,     0,
     404,   416,  1871,     0,    78,   417,     0,     0,     0,     0,
       0,   418,    80,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     405,   406,     0,   407,   408,  1968,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   409,   410,   397,     0,   411,
     412,   413,     0,   414,   415,   400,     0,     0,   401,     0,
     402,    74,   403,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1702,  1703,  1704,   404,
       0,     0,   416,  1969,     0,    78,   417,     0,     0,     0,
       0,     0,   418,    80,    81,   419,   420,   421,   422,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   507,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,   400,   414,   415,   401,     0,   402,     0,   403,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   404,     0,     0,     0,     0,
       0,   416,    77,     0,   508,   509,     0,     0,     0,   510,
       0,   418,    80,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,   405,   406,     0,   407,   408,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   410,   397,     0,   411,   412,   413,   400,   414,   415,
     401,     0,   402,     0,   403,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,     0,     0,     0,     0,     0,   416,  1351,     0,
      78,   417,     0,     0,     0,  1352,     0,   418,    80,    81,
     419,   420,   421,   422,     0,     0,     0,     0,     0,     0,
       0,   405,   406,     0,   407,   408,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,     0,
     411,   412,   413,   400,   414,   415,   401,     0,   402,     0,
     403,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,     0,     0,
       0,     0,     0,   416,     0,     0,    78,   417,     0,     0,
       0,   510,     0,   418,    80,    81,   419,   420,   421,   422,
       0,     0,     0,     0,     0,     0,     0,   405,   406,     0,
     407,   408,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   409,   410,   397,     0,   411,   412,   413,   400,
     414,   415,   401,     0,   402,     0,   403,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,     0,     0,     0,   416,
     868,     0,    78,   417,     0,     0,     0,     0,     0,   418,
      80,    81,   419,   420,   421,   422,     0,     0,     0,     0,
       0,     0,     0,   405,   406,     0,   407,   408,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   409,   410,
     397,     0,   411,   412,   413,   400,   414,   415,   401,     0,
     402,     0,   403,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   404,
       0,     0,     0,     0,     0,   416,  1001,     0,    78,   417,
       0,     0,     0,     0,     0,   418,    80,    81,   419,   420,
     421,   422,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   407,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,   400,   414,   415,   401,     0,   402,     0,   403,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   404,     0,     0,     0,     0,
       0,   416,     0,     0,    78,   417,     0,     0,     0,   301,
       0,   418,    80,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,   405,   406,     0,   407,   408,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   410,   397,     0,   411,   412,   413,   400,   414,   415,
     401,     0,   402,     0,   403,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,     0,     0,     0,     0,     0,   416,     0,     0,
      78,   417,     0,     0,  1072,     0,     0,   418,    80,    81,
     419,   420,   421,   422,     0,     0,     0,     0,     0,     0,
       0,   405,   406,     0,   407,   408,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,     0,
     411,   412,   413,   400,   414,   415,   401,     0,   402,     0,
     403,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,     0,     0,
       0,     0,     0,   416,  1420,     0,    78,   417,     0,     0,
       0,     0,     0,   418,    80,    81,   419,   420,   421,   422,
       0,     0,     0,     0,     0,     0,     0,   405,   406,     0,
     407,   408,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   409,   410,   397,     0,   411,   412,   413,   400,
     414,   415,   401,     0,   402,     0,   403,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,     0,     0,     0,   416,
       0,     0,    78,   417,     0,     0,     0,  1489,     0,   418,
      80,    81,   419,   420,   421,   422,     0,     0,     0,     0,
       0,     0,     0,   405,   406,     0,   407,   408,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   409,   410,
     397,     0,   411,   412,   413,   400,   414,   415,   401,     0,
     402,     0,   403,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   404,
       0,     0,     0,     0,     0,   416,     0,     0,    78,   417,
       0,     0,     0,  1564,     0,   418,    80,    81,   419,   420,
     421,   422,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   407,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,   400,   414,   415,   401,     0,   402,     0,   403,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   404,     0,     0,     0,     0,
       0,   416,     0,  1959,    78,   417,     0,     0,     0,     0,
       0,   418,    80,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,   405,   406,     0,   407,   408,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   410,   397,     0,   411,   412,   413,   400,   414,   415,
     401,     0,   402,     0,   403,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,     0,     0,     0,     0,     0,   416,  1964,     0,
      78,   417,     0,     0,     0,     0,     0,   418,    80,    81,
     419,   420,   421,   422,     0,     0,     0,     0,     0,     0,
       0,   405,   406,     0,   407,   408,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,     0,
     411,   412,   413,   400,   414,   415,   401,     0,   402,     0,
     403,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,     0,     0,
       0,     0,     0,   416,  1973,     0,    78,   417,     0,     0,
       0,     0,     0,   418,    80,    81,   419,   420,   421,   422,
       0,     0,     0,     0,     0,     0,     0,   405,   406,     0,
     407,   408,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   409,   410,   397,     0,   411,   412,   413,   400,
     414,   415,   401,     0,   402,     0,   403,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,     0,     0,     0,   416,
    2052,     0,    78,   417,     0,     0,     0,     0,     0,   418,
      80,    81,   419,   420,   421,   422,     0,     0,     0,     0,
       0,     0,     0,   405,   406,     0,   407,   408,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   409,   410,
     397,     0,   411,   412,   413,   400,   414,   415,   401,     0,
     402,     0,   403,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   404,
       0,     0,     0,     0,     0,   416,  2054,     0,    78,   417,
       0,     0,     0,     0,     0,   418,    80,    81,   419,   420,
     421,   422,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   407,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,   400,   414,   415,   401,     0,   402,     0,   403,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   404,     0,     0,     0,     0,
       0,   416,  2100,     0,    78,   417,     0,     0,     0,     0,
       0,   418,    80,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,   405,   406,     0,   407,   408,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   410,   397,     0,   411,   412,   413,   400,   414,   415,
     401,     0,   402,     0,   403,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,     0,     0,     0,     0,     0,   416,  2102,     0,
      78,   417,     0,     0,     0,     0,     0,   418,    80,    81,
     419,   420,   421,   422,     0,     0,     0,     0,     0,     0,
       0,   405,   406,     0,   407,   408,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,     0,
     411,   412,   413,   400,   414,   415,   401,     0,   402,     0,
     403,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,     0,     0,
       0,     0,     0,   416,  2104,     0,    78,   417,     0,     0,
       0,     0,     0,   418,    80,    81,   419,   420,   421,   422,
       0,     0,     0,     0,     0,     0,     0,   405,   406,     0,
     407,   408,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   409,   410,   397,     0,   411,   412,   413,   400,
     414,   415,   401,     0,   402,     0,   403,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,     0,     0,     0,   416,
    2107,     0,    78,   417,     0,     0,     0,     0,     0,   418,
      80,    81,   419,   420,   421,   422,     0,     0,     0,     0,
       0,     0,     0,   405,   406,     0,   407,   408,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   409,   410,
     397,     0,   411,   412,   413,   400,   414,   415,   401,     0,
     402,     0,   403,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   404,
       0,     0,     0,     0,     0,   416,  2109,     0,    78,   417,
       0,     0,     0,     0,     0,   418,    80,    81,   419,   420,
     421,   422,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   407,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,   400,   414,   415,   401,     0,   402,     0,   403,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   404,     0,     0,     0,     0,
       0,   416,  2150,     0,    78,   417,     0,     0,     0,     0,
       0,   418,    80,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,   405,   406,     0,   407,   408,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   410,   397,     0,   411,   412,   413,   400,   414,   415,
     401,     0,   402,     0,   403,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,     0,     0,     0,     0,     0,   416,  2152,     0,
      78,   417,     0,     0,     0,     0,     0,   418,    80,    81,
     419,   420,   421,   422,     0,     0,     0,     0,     0,     0,
       0,   405,   406,     0,   407,   408,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,     0,
     411,   412,   413,   400,   414,   415,   401,     0,   402,     0,
     403,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,     0,     0,
       0,     0,     0,   416,  2154,     0,    78,   417,     0,     0,
       0,     0,     0,   418,    80,    81,   419,   420,   421,   422,
       0,     0,     0,     0,     0,     0,     0,   405,   406,     0,
     407,   408,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   409,   410,   397,     0,   411,   412,   413,   400,
     414,   415,   401,     0,   402,     0,   403,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,     0,     0,     0,   416,
    2176,     0,    78,   417,     0,     0,     0,     0,     0,   418,
      80,    81,   419,   420,   421,   422,     0,     0,     0,     0,
       0,     0,     0,   405,   406,     0,   407,   408,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   409,   410,
     397,     0,   411,   412,   413,   400,   414,   415,   401,     0,
     402,     0,   403,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   404,
       0,     0,     0,     0,     0,   416,  2178,     0,    78,   417,
       0,     0,     0,     0,     0,   418,    80,    81,   419,   420,
     421,   422,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   407,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,   400,   414,   415,   401,     0,   402,     0,   403,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   404,     0,     0,     0,     0,
       0,   416,  2180,     0,    78,   417,     0,     0,     0,     0,
       0,   418,    80,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,   405,   406,     0,   407,   408,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   410,   397,     0,   411,   412,   413,   400,   414,   415,
     401,     0,   402,     0,   403,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,     0,     0,     0,     0,     0,   416,     0,     0,
      78,   417,     0,     0,     0,     0,     0,   418,    80,    81,
     419,   420,   421,   422,     0,     0,     0,     0,     0,     0,
       0,   405,   406,     0,   407,   408,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,     0,
     411,   412,   413,   400,   414,   415,   401,     0,   402,     0,
     403,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,     0,     0,
       0,     0,     0,   708,     0,     0,    78,   417,     0,     0,
       0,     0,     0,   418,    80,    81,   419,   420,   421,   422,
       0,     0,     0,     0,     0,     0,     0,   405,   406,     0,
     407,   408,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   409,   410,   397,     0,   411,   412,   413,   400,
     414,   415,   401,     0,   402,     0,   403,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,     0,     0,     0,   714,
       0,     0,    78,   417,     0,     0,     0,     0,     0,   418,
      80,    81,   419,   420,   421,   422,     0,     0,     0,     0,
       0,     0,     0,   405,   406,     0,   407,   408,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   409,   410,
     397,     0,   411,   412,   413,   400,   414,   415,   401,     0,
     402,     0,   403,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   404,
       0,     0,     0,     0,     0,   723,     0,     0,    78,   417,
       0,     0,     0,     0,     0,   418,    80,    81,   419,   420,
     421,   422,     0,     0,     0,     0,     0,     0,     0,   405,
     406,     0,   407,   408,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   409,   410,   397,     0,   411,   412,
     413,   400,   414,   415,   401,     0,   402,     0,   403,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   404,     0,     0,     0,     0,
       0,   416,     0,     0,    78,   417,     0,     0,     0,     0,
       0,   418,   940,    81,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,   405,   406,     0,   407,   408,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     409,   410,   397,     0,   411,   412,   413,   400,   414,   415,
     401,     0,   402,     0,   403,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,     0,     0,     0,     0,     0,   416,     0,     0,
      78,   417,     0,     0,     0,     0,     0,   418,   482,    81,
     419,   420,   421,   422,     0,     0,     0,     0,     0,     0,
       0,   405,   406,     0,   407,   408,  2047,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   409,   410,   397,     0,
     411,   412,   413,     0,   414,   415,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   416,     0,     0,    78,   417,     0,     0,
       0,     0,     0,   418,    80,    81,   419,   420,   421,   422
};

static const yytype_int16 yycheck[] =
{
       1,    76,   178,     4,   259,  1260,   950,   752,    76,   381,
     755,   182,   233,   183,     1,   953,   510,     4,   233,   221,
     662,   167,   675,   233,    78,    59,   274,     1,  1242,   138,
     662,    76,   233,    98,   233,  1822,   183,    76,   937,    98,
     416,   167,  1198,   233,   740,   567,   568,  1822,   233,  1294,
    1295,   142,    76,  1823,   168,    56,    57,  1822,    59,   579,
      85,   233,   236,   733,   238,   846,  1712,   848,   418,   662,
     590,   245,    59,   799,   925,    76,  1957,     1,   243,   233,
    1225,  1953,     1,     1,    85,    59,    76,   833,     1,  1242,
       0,   833,    93,    72,    76,   946,   666,    98,   233,  1047,
     101,   322,   831,   831,   105,   416,   271,   322,    75,   205,
     354,    85,   322,   204,   148,   635,   192,   282,   105,   194,
       0,   322,   831,   322,    98,   831,   151,   101,  1076,    75,
     160,   105,   322,    89,   243,    59,   101,   322,   696,   155,
      59,    59,   143,   317,   318,   146,    90,   148,     1,   194,
     322,     4,  1195,   154,   135,   194,   352,   136,   233,  1202,
     161,   148,   271,     1,   205,   233,   100,   168,   322,   234,
     194,  1941,   368,   282,   148,   234,   372,   834,   700,   180,
     159,   105,    10,   840,    77,    78,    84,   322,   233,   831,
     155,   192,   193,   194,   233,   300,  1144,   178,   307,   831,
      77,    78,    72,   204,   194,   161,    59,   163,   209,   233,
       1,   155,   194,   180,   138,   228,  2088,    89,   219,   193,
    1866,  1207,   125,   224,   148,    98,   227,   228,   229,   148,
     148,     1,   233,   234,   180,   209,     0,   262,   831,   937,
    1283,   254,   296,   233,   309,   489,   180,   322,   101,   162,
     309,   233,   105,   254,   322,   158,    72,   155,   323,  2140,
     234,   262,   120,   833,   323,   155,   136,   157,    59,   159,
     163,   272,   273,   346,     1,   276,   137,   322,   108,   109,
       4,  1708,   283,   322,   806,   155,   163,   483,   262,    59,
    1035,   163,  2062,  2174,   668,   148,   297,   298,   163,   300,
       4,   159,   276,   551,   305,   167,   331,   528,   309,   120,
     171,   559,   385,   528,   516,   180,   392,  1216,   528,   243,
     136,   322,   323,   161,   162,   697,  1360,   528,   158,   528,
     368,   332,    59,  2120,   372,   309,   137,   165,   528,   340,
     341,   159,   170,   528,   345,  2120,   164,   271,  1199,   323,
    1249,  2121,    56,    57,  1024,  2120,   528,   148,   282,  1085,
     710,   234,   155,  1516,   608,  1111,  1519,  1520,   138,  1111,
     171,   112,   154,   487,   528,  1104,  1104,   753,   148,   161,
      10,   105,   383,   307,   155,   386,   179,   311,   479,    93,
    2160,   392,   163,   528,   135,  1104,  2091,   708,  1104,   957,
     711,   712,   922,   714,   155,   975,  1449,   628,   152,   500,
     654,   138,   723,   628,  1570,   726,   727,   728,   628,   163,
      20,   148,  2117,   276,   589,   450,     1,   628,   157,   628,
     853,   854,   161,   161,   678,   179,   309,   995,   628,   143,
     205,   685,   146,   628,  1871,  1872,   155,  2142,  1105,   872,
     323,   556,   180,   528,   508,   651,   628,   161,   155,  1207,
     461,   835,  1104,   161,   168,   839,  1452,  1453,  1454,   404,
     108,   109,  1104,   243,   628,   540,   850,   851,   574,   675,
     589,   540,   180,   528,    59,   486,   487,   461,   149,   528,
     352,  1207,   688,   628,   429,   430,   154,   498,   499,   155,
     204,   271,   161,   161,   528,   112,   632,   508,   158,   510,
     171,  1104,   282,    61,    62,   450,   243,     1,  1216,   942,
     224,   180,   180,   227,   154,   229,   631,   528,   135,   179,
     161,  1745,   155,   574,   157,   165,   650,   307,   528,   540,
     170,  1111,  1969,  1970,   271,   155,   528,   137,   483,   180,
     157,  1249,   565,   628,   155,   282,   652,   662,   571,  1593,
    1594,  1595,  1707,   138,   565,   630,   540,  1712,   272,   273,
     571,   630,   573,   148,    72,    59,   166,   167,    58,   283,
     307,    61,    62,   628,    64,  1079,   180,   352,    75,   628,
     355,   632,  1745,   297,   298,   111,   300,   130,   131,   163,
     634,   305,    72,   368,    91,   159,   161,   372,   163,   780,
     164,   652,    72,   651,   161,   163,   180,   101,   134,    75,
     161,   483,  1403,   110,   242,  1406,  1407,   628,   332,   630,
     826,   249,   180,   634,    90,   636,   340,   341,   136,   180,
    1588,   345,   175,   176,   645,    75,   161,   634,   649,   650,
     688,  1304,   270,  1310,  1899,   161,   630,   155,   161,   861,
     634,   159,    92,   281,   148,   589,   136,   540,   243,   283,
     134,   180,   155,  1369,   844,   155,   136,  1100,  1101,   383,
     163,   682,   386,     3,   298,   155,   782,   170,   858,   159,
     149,   150,   151,   798,   695,   155,   271,   844,   161,   159,
      69,   163,   848,   704,  1452,  1453,  1454,   282,   170,   814,
     634,   161,   171,   818,   161,   634,   634,   180,   483,   833,
     964,  1866,   163,  1667,   171,  1470,   831,   781,   899,   170,
     180,   827,   307,  1156,    72,  1392,  1452,  1453,  1454,  1297,
     161,   782,   134,  1998,   745,   161,   747,     3,   749,   161,
     171,   157,   753,   161,  1128,   756,   162,   630,   161,  1317,
     157,   225,   158,   155,   180,   162,   962,   159,   180,  1327,
     632,  1293,   180,   157,   166,   167,   161,   180,   162,  1477,
     781,   634,   486,  1481,  1482,   161,   827,   179,   826,   161,
      85,    72,   276,   155,   498,   499,  1492,  1495,   136,   171,
     157,    72,   163,    98,   180,   846,   101,    62,   161,   574,
     105,   168,   169,   675,   179,   157,  1754,   155,   171,   589,
     162,   159,  1967,   163,   759,   826,   310,     3,   933,   156,
     831,   155,   833,   177,  1979,   161,   163,    13,    14,    15,
      16,    17,   306,   634,   845,   171,   101,   157,  1796,   155,
    1798,   155,   162,   120,   855,   136,   162,   112,  1352,   114,
     861,   116,   589,   864,   634,   136,  1242,   632,   166,   573,
     354,   161,   986,   157,   155,   173,   174,   161,   159,   158,
     159,   171,   652,   162,   155,   898,   651,   652,   159,  1585,
     149,   150,   151,    72,  1020,  1143,    72,   898,   193,   157,
     155,  2046,  1072,   158,   159,    72,     3,   634,   154,   157,
     675,   160,   171,   161,   209,   161,    13,    14,    15,    16,
      17,   180,   157,   688,   962,    72,  1298,  1299,  1019,   157,
     158,  1242,   636,  1660,   935,   936,   937,   555,  1665,   234,
     155,   645,   157,    72,   159,   649,   650,   108,   109,  1151,
     937,   155,   416,   157,   209,   159,   383,   136,   157,   386,
     136,    72,   949,   581,   157,    47,    48,   262,    50,   136,
     588,   168,   169,    55,   592,    72,   155,   157,   682,  1020,
     159,   276,   157,  1153,   159,   986,  1091,   157,   155,   136,
    1095,   695,   159,   155,   157,  1489,  1110,  1111,   161,  1104,
     155,  1559,  1572,   161,    72,   489,  1180,   136,   155,  1114,
      22,   157,   159,   937,   589,   161,  1121,   782,   937,   937,
     155,   276,   155,   278,   279,   136,   155,   155,  1339,   157,
     159,   159,   101,    13,    14,    15,    16,    17,    72,   136,
    1041,   745,   161,   747,   155,   749,  1047,   155,   159,   753,
     155,    72,   756,    72,   159,   310,   155,   827,  1313,   634,
     315,   826,   827,  1168,  1308,   157,   321,  1172,   136,   161,
    1564,  1176,   157,   128,   129,  1076,   161,   781,  1079,   157,
    1015,  1457,   163,   161,   937,    72,  1021,   155,   160,  1583,
     554,   159,    72,   760,   761,   762,   949,  1032,   562,   354,
      89,    72,   136,  1104,   359,   163,   361,   157,  1304,  1110,
    1111,   161,    72,   160,   161,   136,   580,   136,  1294,  1295,
     180,   155,   826,   154,   608,   159,   163,   591,    13,    14,
      15,    16,    17,   155,   155,  1281,   155,   621,   159,   163,
     159,   845,   160,  1144,   132,   133,   937,   157,   134,   136,
     634,   855,   407,   134,  1324,  1281,   136,   137,  1020,    13,
      14,    15,    16,    17,    18,   136,   461,   937,   155,   155,
     170,  1869,   159,   159,   155,   157,   136,  1324,   159,   161,
     166,   167,  1207,   179,   155,   166,   167,    72,   159,  1561,
     155,  1439,   155,   157,   678,   155,   159,   962,   120,   159,
     965,   157,  1372,  1761,  1762,   161,   461,   155,  1334,  1335,
     937,  1198,   155,    58,   134,   155,    61,    62,  1219,    64,
     155,  1222,  1223,  1224,   159,   949,  1657,  1658,  1659,  1216,
    1231,   935,   936,   937,   489,   155,   491,   492,   161,   159,
     166,   167,  1412,  1413,   708,   540,   166,   167,  1249,  1994,
     714,   136,   507,   155,  1255,  1020,   155,  1403,   172,   723,
     159,  1407,  1249,   157,   157,  1412,  1413,   161,   161,  1270,
     167,   155,  1273,  1274,  1287,   159,  1277,   165,   742,   177,
     157,  1216,   986,  1284,   161,   540,  1287,  1274,   134,   149,
     150,   151,  1216,   157,  1335,   160,   161,  1216,  1216,  1273,
    1274,   161,   158,  2125,   157,   157,   924,  2129,   161,   161,
     565,   171,   149,   150,   151,   570,   157,   572,   157,  1424,
     180,  1322,   161,  1428,   161,  1249,   157,  1432,   157,   157,
    1249,  1249,   161,  2031,   171,  1336,   157,   160,   161,   594,
     161,   596,   597,   180,   157,  1198,   160,   161,   161,   157,
    1274,  1352,   157,   608,   157,   157,   161,  1915,   161,  1360,
     157,  2059,   937,  1216,   161,  1406,   621,   157,  1477,  1304,
     157,   161,  1481,   159,   161,   630,   155,    13,    14,    15,
      16,    17,   160,   161,    13,    14,    15,    16,    17,   157,
     155,   157,  1393,   161,  2092,   161,  1249,   160,  1463,   654,
     137,   656,   657,  1775,  1463,   137,   157,  1342,  1343,  1344,
     161,   161,   157,  1477,  1349,  1350,   161,  1481,  1544,  1281,
    1273,  1274,   161,   678,   679,  1216,   162,  1452,  1453,  1454,
     685,   162,  1457,  1458,   160,   161,    72,   767,   768,   769,
     770,   905,  1304,    72,  1549,   161,  1216,  1204,  1205,  1206,
    1671,  2009,   155,   937,   179,   919,  1671,   157,  1249,   923,
     155,  1671,  1463,   927,   161,   162,  1467,  1468,  1455,   157,
    1671,   157,  1671,  1335,  1198,   160,   161,  1788,   157,  1249,
     964,  1671,   157,  1678,  1679,  1680,  1671,   157,  1489,  1216,
    1477,   160,   161,   157,  1481,  1482,   157,  1602,   157,  1671,
     136,   137,   179,  1544,   160,   134,  1611,   136,  1495,   159,
    1615,  1512,  1513,    91,    92,  1219,   163,  1671,  1222,  1223,
    1224,  1522,  1249,   160,   161,   163,   155,  1231,   160,   161,
     159,   113,   114,   115,   116,   117,  1671,   166,   167,  1304,
     160,   161,   160,   161,   163,  1249,   160,   161,  1522,   157,
    1274,  1255,   163,  1477,   160,   161,   163,  1481,  1482,   160,
     161,   160,   161,  1564,  1735,   161,  1270,   160,   161,    70,
    1335,  1495,   160,  1277,    13,    14,    15,    16,    17,    18,
    1284,   155,  1583,  1570,   160,   161,   300,  1588,   161,   162,
    1463,   180,  1593,  1594,  1595,  1660,   160,   161,   160,   161,
    1665,  1660,  1455,  1671,    77,    78,  1665,    78,  1673,   161,
     162,  1361,  1362,   160,  1673,    18,  1837,   161,  1322,   763,
     764,   179,  1837,   163,  1477,   765,   766,  1837,  1481,  1482,
     771,   772,  1336,  1519,  1520,   180,  1837,   157,  1837,  1679,
    1680,  1216,  1495,   157,   163,  1562,  1563,  1837,   180,   160,
     163,   160,  1837,   123,    18,   125,   126,   127,   160,  1660,
     160,   154,   157,   157,  1665,  1837,   157,   157,   157,  1522,
    1671,   157,  1673,   157,  1249,    13,    14,    15,    16,    17,
    1681,   157,  1544,  1837,   163,   155,  1477,    22,   158,   159,
    1481,  1482,   157,   163,   164,   157,   157,   157,   154,  1700,
     154,   160,  1837,    70,  1495,  1706,   163,  1477,   163,   964,
    1697,  1481,  1482,   161,   157,  1886,   179,  1570,   163,   157,
     975,   157,   157,  1899,   157,  1495,   154,   179,   157,   984,
     161,  1455,  1216,   163,    72,   157,   163,  1201,   161,   160,
     157,   157,    13,    14,    15,    16,    17,   161,   157,  1750,
    1477,  1215,   157,   157,  1481,  1482,    13,    14,    15,    16,
      17,    18,   157,  1467,  1468,  1249,   157,   157,  1495,  1756,
     157,  1235,   160,  1697,   160,  1840,   157,   694,  1242,  1544,
     157,  1840,   157,  1953,   157,   157,   157,  1660,   157,  1273,
     160,  1987,  1665,   161,   157,  1796,   134,  1798,   136,   157,
    1673,    72,   157,   157,   161,  1740,  1953,   154,  1512,  1513,
      13,    14,    15,    16,    17,    18,   161,   155,   157,  2063,
    1991,   159,   154,    14,  1308,   161,   155,   155,   166,   167,
    1085,   155,   155,   155,   155,   155,  1837,   155,   180,  1840,
     180,   160,   556,  1696,  1697,   162,  1570,   161,  1849,  1850,
     162,   160,   163,   180,   154,  1856,  1111,   154,   163,   161,
     161,   157,   157,   134,  2060,   136,   157,  1868,     4,     5,
       6,     7,     8,     9,    10,    11,    12,  1878,   157,  1880,
     157,   180,  1869,   155,   155,  1950,   160,   160,   159,  1902,
    1891,  1950,  1893,  1894,  1895,   166,   167,   157,   157,  2120,
    1901,  1902,  1477,  1756,   160,  2120,  1481,  1482,   157,   157,
    2120,   157,   157,   160,   154,   154,  2086,   631,  2088,  2120,
    1495,  2120,   155,    80,    92,   180,   180,   180,   180,    65,
    2120,   154,    90,   155,   155,  2120,   180,   180,   180,  2086,
     157,  2088,   859,   154,   154,  1869,   161,   161,  2120,  1950,
     154,   163,   154,   160,  2125,   163,  1957,  2127,  2129,  2130,
    1961,   160,  1987,   160,   160,  1966,  2120,  1840,   157,   162,
     162,   123,   154,  1697,   157,  2080,   157,  1681,  1273,  1274,
    2127,   157,   160,    18,   157,  2120,   160,   157,  2158,   155,
     157,  2162,   157,  1477,   154,   154,  1700,  1481,  1482,  1852,
     180,   162,  1706,   157,   157,   161,   155,  1471,  1472,   155,
     154,  1495,   160,    13,   157,  2186,  1869,   154,  1273,  2190,
     160,  2022,    57,    58,    59,    60,    61,    62,    63,    64,
     160,  2201,  1756,  2034,  2205,  2060,   163,  2038,  1522,   154,
     157,   154,   160,   157,  2031,  2120,  1750,   219,   157,  2062,
    2051,   157,  2120,  1308,  2201,    75,  2121,  1521,  1993,  1314,
      75,  2062,  2121,  2064,   180,   154,     1,   155,   180,     4,
     180,   157,  2059,   155,   157,  2120,   993,  1950,  1869,   160,
     160,  2120,   154,   154,   798,   163,   154,   154,    88,   159,
      75,   157,   157,    75,  2095,  2160,   171,   158,   171,  1869,
     814,  2160,    75,   180,   818,  2092,   106,  2031,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   180,  2120,
    2121,   162,   180,   154,    59,   154,   154,   171,   171,   156,
     154,   106,  2133,   155,   171,  2059,   162,   154,   161,  2140,
     156,    76,  1869,    75,   180,  1849,  1850,  2121,   160,   154,
      85,    62,  1856,   171,   157,   155,   157,    76,   156,  2160,
     162,   180,   157,    98,  1868,   154,   101,  2168,  2092,   157,
     105,  2172,   155,  2174,  1878,   154,  1880,   157,  2031,    98,
    1788,   180,  1332,   180,   180,    18,  2160,  1891,   773,  1893,
    1894,  1895,   775,   732,  2195,   106,   776,  1901,   777,   110,
    1249,  1237,   113,   774,   115,  2206,  2059,   142,   449,  1464,
    2174,  1481,  1696,   148,  2215,  1877,   151,  2088,  2117,   154,
     155,  1495,  1987,  1869,  2075,    85,  1744,  1522,    61,    62,
      63,    64,   167,  1728,  2157,   154,  1728,  2060,  2130,  2092,
    2031,  2190,  2059,    49,  1277,   113,   267,  1950,  2121,  1458,
     990,  2020,   861,  1957,  1270,   517,   191,  1961,   193,   194,
     645,  2031,  1966,  1727,  1529,  1756,     0,  1522,  2059,   204,
     205,   798,   704,   106,   209,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   798,  2160,   798,  2059,
     244,   151,    -1,   228,  1869,  2060,    -1,  1649,   233,   234,
      -1,  2092,    -1,    -1,  2031,    -1,    -1,   205,    -1,   228,
      -1,    -1,    -1,    -1,   233,   234,    -1,    -1,  2022,   254,
      -1,    -1,  2092,    -1,    -1,    -1,   159,   262,    -1,    -1,
    2034,   191,  2059,    -1,  2038,   254,   508,    -1,   510,    -1,
      -1,   276,    -1,    -1,    -1,    -1,    -1,  2051,    -1,    -1,
      -1,    -1,    -1,    -1,   265,    -1,    -1,    -1,    -1,    -1,
    2064,    -1,    -1,    -1,    -1,  2092,    -1,    -1,  1852,    -1,
      -1,    -1,    -1,    -1,   309,    -1,    -1,  1091,    -1,  1296,
     315,  1095,    -1,    -1,    -1,  1869,   321,   322,   323,    -1,
     309,  2095,    -1,    -1,    -1,    -1,   331,    -1,    -1,    -1,
    1114,   312,   262,   322,   323,    -1,    -1,  1121,    -1,    -1,
      -1,  1328,    -1,    -1,    -1,    -1,    -1,   352,   353,   354,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,  2133,
      -1,    -1,    -1,   368,    -1,    -1,  2140,   372,    -1,    -1,
      -1,  1696,    -1,    -1,    -1,   356,    -1,   358,    -1,   360,
      -1,    -1,    -1,    -1,  1168,    -1,  2031,    -1,  1172,  1376,
      13,    -1,  1176,    -1,  2168,    -1,   129,    -1,  2172,   106,
    2174,   331,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   416,    -1,    -1,  2059,    -1,    -1,    -1,    62,    -1,
      -1,  2195,   352,    -1,    -1,    -1,   407,    -1,    -1,   453,
      -1,    -1,  2206,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2215,    -1,    -1,    -1,   450,   470,  2092,   453,   473,
      -1,    -1,    -1,    -1,    -1,    -1,   461,   101,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,   112,   113,
      -1,    -1,    -1,    -1,   479,    -1,    -1,  2031,   483,    -1,
      -1,    -1,   487,   106,   489,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   500,    -1,    -1,   487,    -1,
      -1,    -1,    -1,    -1,    -1,  2059,    -1,    -1,    -1,   533,
      -1,   155,    -1,    -1,    -1,  1840,    -1,    -1,    -1,    -1,
     450,    -1,    -1,   528,    -1,    -1,   507,  1852,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   540,    -1,    -1,  2092,   528,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,   540,    -1,   483,  1541,    -1,    -1,    78,    -1,    -1,
     565,    -1,   567,   568,    -1,   209,   571,    -1,    -1,   574,
      -1,  1558,    -1,    -1,    -1,    -1,   565,    -1,    -1,    -1,
    1567,    -1,   571,    -1,    -1,   106,   319,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,  1584,    -1,    -1,
      -1,    -1,    -1,   106,    -1,   586,   574,   110,   111,   112,
     113,   114,   115,   116,   117,   118,  1941,    -1,    -1,   861,
      -1,    -1,   864,   628,    -1,   630,    -1,   632,    -1,   634,
      -1,    -1,   276,    -1,    -1,    -1,    -1,   567,   568,   628,
    1424,   630,    -1,    -1,  1428,    -1,   651,   652,  1432,   654,
      -1,  2185,    -1,    -1,    -1,    -1,   159,   662,    -1,   180,
      -1,   666,    -1,  2197,    -1,    -1,   310,    -1,    -1,    -1,
     675,   315,    -1,    -1,    -1,    -1,    -1,   321,    -1,    -1,
     685,   106,    -1,   688,   652,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   700,    -1,   122,    -1,   124,
      -1,    -1,    -1,   708,    -1,    -1,   711,   712,    -1,   714,
     354,    -1,    -1,    -1,    72,    -1,    -1,    -1,   723,    -1,
      -1,   726,   727,   728,    -1,    -1,    -1,    -1,    -1,    -1,
    1717,   156,    -1,    -1,   159,    -1,    -1,    -1,  2063,    -1,
      -1,    -1,    -1,    -1,    -1,   675,    -1,    -1,   106,   482,
      -1,   484,   110,   111,   112,   113,   114,   115,   116,   117,
     493,    -1,    -1,   407,    -1,  1549,    -1,    -1,    -1,    -1,
     700,    -1,    -1,    -1,   798,   799,   134,   782,   136,    -1,
      -1,    -1,    -1,    -1,    -1,   809,    -1,  1774,   812,    -1,
      -1,    -1,  1779,   798,   799,    -1,    -1,   155,   156,  1041,
      -1,   806,    -1,  1790,    -1,  1047,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,   782,    -1,    -1,    -1,  1602,    -1,
      -1,   826,   827,    -1,    -1,    -1,   831,  1611,   833,    -1,
      -1,  1615,    -1,    -1,  1076,  2160,    -1,  1079,    -1,    -1,
      -1,   846,   831,   848,   833,   489,    -1,   852,   853,   854,
      -1,    -1,    -1,    -1,   878,    -1,    -1,    -1,    -1,   827,
      -1,   885,    -1,   507,    -1,   889,    -1,   872,    -1,   893,
      -1,    -1,    -1,     3,    -1,    -1,   806,    -1,   846,    -1,
     848,    -1,    -1,    -1,   852,   853,   854,    -1,    -1,    -1,
     106,    -1,    -1,   898,   110,   111,   112,   113,   114,   115,
     116,   117,  1144,    -1,   872,    -1,    -1,    -1,    -1,   898,
      -1,    -1,    -1,     1,    -1,    -1,     4,  1904,  1905,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   572,    -1,
      -1,    -1,   937,    -1,    -1,    -1,    -1,   942,    -1,    -1,
     156,    -1,    -1,   159,   949,   950,    -1,    -1,    -1,    -1,
     594,    -1,    -1,    -1,    -1,    -1,    -1,   962,    -1,   964,
      -1,    -1,    -1,    -1,   608,   933,    -1,    -1,    -1,    -1,
     975,    59,    -1,    -1,   942,    -1,   106,   621,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
     654,    -1,    -1,    -1,  1019,  1020,    -1,   105,    -1,    -1,
     950,    -1,    -1,  2010,    -1,   155,   156,    13,    14,    15,
      16,    17,   162,    -1,   678,    -1,   166,   167,   106,    -1,
      -1,   685,   110,   111,   112,   113,   114,   115,   116,   117,
     138,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
     148,  1085,   106,   151,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,   167,
    1085,    -1,    -1,    -1,    -1,    -1,    72,   155,   156,    -1,
      -1,  2078,    -1,    -1,    -1,  1100,  1101,    -1,    -1,  1104,
      -1,    -1,    -1,   191,    -1,   193,  1111,    -1,    -1,    -1,
    1352,   844,    -1,    -1,    -1,  1104,   204,   205,  1360,    -1,
     106,  1110,  1111,    -1,   110,   111,   112,   113,   114,   115,
     116,   117,  1100,  1101,    -1,  1159,   180,    -1,  1162,    -1,
      -1,    -1,  1166,    -1,    -1,    -1,   234,    -1,   134,    -1,
     136,  1156,    -1,    -1,   106,   243,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   254,    -1,    -1,   155,
     156,   259,   260,    -1,   262,    -1,    -1,    -1,    -1,    -1,
     166,   167,    -1,   271,    -1,    -1,    -1,    -1,  1156,    -1,
      -1,    -1,    -1,  1198,   282,    -1,    -1,   285,    -1,    -1,
      -1,   289,  1207,    -1,    -1,    -1,   294,   940,   941,    -1,
      -1,  1216,    -1,    -1,    -1,    -1,    -1,   305,    -1,   307,
      -1,    -1,    -1,   311,    -1,    -1,    -1,    -1,   180,    -1,
      -1,    -1,    -1,    -1,    -1,   323,    -1,  1242,    -1,    -1,
      -1,    -1,    -1,   331,  1249,    -1,   106,  1489,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   902,    -1,
      -1,    -1,    -1,    -1,   352,    -1,    -1,   355,  1273,  1274,
      -1,    -1,    -1,    72,    -1,    -1,  1281,    -1,    -1,    -1,
     368,    -1,  1287,    -1,   372,    -1,    -1,    -1,  1293,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2080,    -1,  1287,  1304,
      -1,  1034,    -1,    -1,    -1,    -1,   166,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     964,    -1,  1564,    -1,    -1,    -1,    -1,    -1,   416,    -1,
    1335,    -1,    -1,    -1,  1339,   134,    -1,   136,    -1,    -1,
      -1,  1583,    72,    -1,    -1,  1078,  1588,    -1,    -1,    -1,
      -1,  1593,  1594,  1595,  1378,    -1,   155,   156,    -1,    -1,
     159,    -1,   450,  1293,    -1,  1389,    -1,   166,   167,    -1,
      -1,    -1,    -1,    -1,  1304,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,   479,    -1,    -1,  1127,   483,  1129,    -1,  1403,    -1,
      -1,  1406,  1407,    -1,  1393,    -1,   136,    -1,  1141,  1142,
      -1,    57,   500,    -1,  1147,  1148,    -1,    -1,    -1,    65,
      66,    67,    68,    -1,  1157,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1403,    -1,    -1,  1406,  1407,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1452,  1453,  1454,
    1455,  1184,  1457,  1458,  1187,    -1,    -1,    -1,  1463,  1464,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,  1477,    -1,  1463,    -1,  1481,  1482,    -1,   567,
     568,    -1,    -1,    -1,    -1,    -1,   574,   106,    -1,   108,
    1495,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     106,   589,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   159,    -1,    -1,  1249,  1522,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,   177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1544,
      -1,    -1,    -1,    -1,   632,    -1,   634,  1280,    -1,   155,
     156,    -1,    -1,   159,  1796,  1288,  1798,  1290,  1291,    -1,
     166,   167,    -1,   651,   652,  1570,    -1,  1572,    -1,    -1,
    1303,    -1,  1305,   179,  1307,    -1,    -1,    -1,   666,    -1,
      -1,  1314,    -1,    -1,    -1,    -1,    -1,   675,    -1,    -1,
      -1,    -1,   680,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     688,    -1,    -1,    -1,    -1,    -1,    -1,  1219,    -1,    -1,
      -1,    -1,   700,    -1,    -1,    -1,    -1,    -1,    -1,  1231,
     708,    -1,    -1,   711,   712,    -1,   714,    -1,    -1,  1273,
      -1,    -1,    -1,    -1,    -1,   723,    -1,  1661,   726,   727,
     728,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,  1386,  1387,  1660,  1661,    -1,    -1,    72,
    1665,    -1,  1667,    -1,  1308,    -1,  1671,    -1,  1673,    -1,
      -1,  1660,    -1,    -1,    -1,    -1,  1665,    -1,  1411,    -1,
      -1,    -1,  1671,    -1,  1673,  1418,    -1,    -1,    -1,  1422,
     155,  1696,  1697,   106,   782,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,   799,    -1,    -1,    -1,  1448,    -1,    -1,   806,    -1,
      -1,   134,   106,   136,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,  1667,   826,   827,
      -1,    -1,   155,   156,    -1,   833,    -1,    -1,    -1,    -1,
      -1,  1756,    -1,   166,   167,    -1,    -1,    -1,   846,    -1,
     848,    -1,    -1,    -1,   852,   853,   854,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,   163,
      -1,    -1,    -1,  1788,   872,   106,   170,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,  1822,  1823,
      -1,  1445,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,  1548,    -1,  1822,  1823,    -1,
    1464,    -1,  1555,    -1,  1557,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1837,    -1,   106,  1840,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,  1852,  1837,   937,
      -1,  1840,    -1,    -1,   942,    -1,    -1,    -1,    -1,   163,
      -1,   949,   950,    -1,  1869,    -1,   170,    13,    14,    15,
      16,    17,    -1,    -1,   962,    -1,    -1,   965,  1522,    -1,
      -1,    -1,    -1,    -1,   972,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,    -1,    -1,    -1,  1902,   170,  1632,
    1512,  1513,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,  1902,    -1,    -1,   152,  1941,    56,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,  1019,  1020,    -1,    -1,    -1,  1941,    -1,    -1,    -1,
      -1,    -1,    -1,   179,    -1,  1950,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,
     106,  1950,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1987,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,  2016,    -1,    -1,    -1,  2020,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,   146,   155,
     156,    -1,  1100,  1101,    -1,  2020,    -1,    -1,    -1,    -1,
     166,   167,    -1,   161,    -1,    -1,  2031,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1768,  1769,   106,  2062,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,  1782,
      -1,    -1,  1696,    -1,  2059,  2060,    -1,  2062,  2063,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1156,    -1,
      -1,    -1,   106,  2062,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,   224,  2092,  1700,    -1,
     159,    -1,    -1,    -1,  1706,    -1,  2120,  2121,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
    1198,    -1,    -1,    -1,    -1,  2120,  2121,    -1,    -1,  1207,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,  1216,   163,
      -1,  2120,  2121,    -1,   272,   273,  2160,    -1,  1750,     1,
      -1,    -1,     4,    -1,    -1,   283,    -1,   155,   156,    -1,
      -1,   159,    -1,    -1,  1242,  2160,    -1,    -1,   166,   167,
     298,  1249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,  2160,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,   122,  1274,   124,    -1,    -1,
      72,    -1,    -1,  1281,   332,    -1,    -1,    59,    -1,    -1,
      -1,    -1,   340,   341,    -1,  1293,    -1,   345,  1852,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1304,    -1,    -1,   156,
      -1,    -1,   159,    85,   106,  1313,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,  1849,  1850,   101,
      -1,    -1,    -1,   105,  1856,   383,    -1,  1335,   386,    -1,
      -1,  1339,   134,    -1,   136,    -1,  1868,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1878,    -1,  1880,    -1,
      -1,    -1,    -1,   155,   156,    -1,   138,    -1,    -1,  1891,
     142,  1893,  1894,  1895,   166,   167,   148,    -1,    -1,   151,
      -1,    -1,   106,   155,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1403,    -1,    -1,  1406,  1407,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2061,   191,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   204,   205,   158,  1957,    -1,   209,   486,  1961,
      -1,    -1,    -1,    -1,  1966,    -1,    -1,    -1,   134,    -1,
     498,   499,    -1,    -1,  1452,  1453,  1454,  1455,  1456,  1457,
    1458,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,   243,    -1,    -1,    -1,  2118,   162,    -1,    -1,  1477,
     166,   167,    -1,  1481,  1482,    -1,    -1,    -1,   260,    -1,
     262,    -1,    -1,    -1,    -1,    -1,    -1,  1495,  2141,   271,
    2022,    -1,    -1,    -1,   276,    -1,    -1,    -1,    -1,  2063,
     282,    -1,  2034,  2156,    -1,    -1,  2038,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   297,    -1,    -1,   300,  2051,
      -1,    -1,    -1,   305,    -1,   307,    -1,    -1,   310,   311,
      -1,    -1,    -1,   315,    -1,    -1,  1544,    -1,    -1,   321,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   331,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,  1570,  2095,    -1,    -1,    -1,    -1,    -1,     1,
     352,    -1,   354,   355,    -1,    -1,    -1,    -1,   636,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   368,   645,    -1,    -1,
     372,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,  2133,   158,   159,    -1,    -1,    -1,    -1,  2140,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   416,    -1,  2168,   695,    -1,    -1,
    2172,    73,  2174,    -1,    -1,    -1,   106,    -1,    -1,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,  1667,
      -1,    -1,   122,  2195,   124,  1673,    -1,    -1,   450,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,  1697,
     122,   123,   124,    -1,   126,   127,   156,   479,    -1,   159,
      -1,   483,   134,    -1,    -1,    -1,    -1,   489,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,   500,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1756,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   556,    -1,    -1,   134,    -1,   136,
    1788,    -1,    -1,    -1,    -1,   567,   568,   845,    -1,    -1,
     572,    -1,   574,    -1,    -1,    -1,    -1,   855,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   589,    -1,   166,
     167,   156,    -1,    -1,   159,  1823,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   608,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,   621,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   631,
     632,    -1,   634,    -1,    13,    14,    15,    16,    17,    -1,
      -1,  1869,    -1,    -1,     1,    -1,    -1,   649,    -1,   651,
     652,    -1,   654,    -1,    -1,    -1,    -1,   158,   936,    -1,
     662,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,   675,    -1,    -1,   678,    -1,    -1,    -1,
     682,    -1,    -1,   685,    -1,    -1,   688,    -1,   690,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,   700,    -1,
      -1,    -1,    59,    -1,    -1,    -1,   708,    -1,    -1,   711,
     712,    -1,   714,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   723,    -1,    -1,   726,   727,   728,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,   106,  1987,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
     782,   138,    -1,    -1,    -1,    -1,   134,   166,   167,    -1,
      -1,   148,    -1,    -1,    -1,    -1,   798,    -1,    -1,    -1,
      -1,    -1,    -1,  2031,   806,    -1,    -1,   155,   156,    -1,
     167,    -1,   814,    -1,    -1,    -1,   818,    -1,   166,   167,
      -1,    -1,    -1,    -1,   826,   827,    -1,    -1,    -1,   831,
      -1,  2059,  2060,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   846,    -1,   848,    -1,   205,   106,
     852,   853,   854,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,  2092,   122,    -1,   124,    -1,   206,
     872,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   243,    -1,    -1,    -1,
      -1,    -1,    -1,  2121,    -1,    -1,    -1,    -1,    -1,   156,
     902,   134,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   271,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,   261,   282,    -1,    -1,   285,    -1,
      -1,   933,    -1,   166,   167,   937,    -1,    -1,    -1,    -1,
     942,    -1,    -1,   300,  1222,  1223,  1224,   949,   950,    -1,
     307,   155,    -1,    -1,   311,    -1,    -1,    -1,    -1,    -1,
     962,    -1,   964,   965,   301,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   313,  1255,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   330,    -1,   352,    -1,    -1,   355,  1277,
      -1,    -1,    -1,    -1,    -1,    -1,  1284,    -1,    -1,    -1,
      -1,   368,    -1,    -1,   351,   372,    -1,  1019,  1020,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    20,  1322,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
     417,    58,    59,    60,    61,    62,    63,    64,    -1,  1091,
      -1,    -1,    -1,  1095,    -1,    -1,    -1,    -1,  1100,  1101,
      -1,    -1,  1104,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   448,  1114,    -1,    -1,    -1,    -1,    -1,    -1,  1121,
      -1,    -1,    -1,    -1,    -1,    -1,   483,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   476,
      -1,    -1,    -1,    -1,   481,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1156,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   501,    -1,  1168,    -1,   505,   506,
    1172,    -1,   509,    -1,  1176,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,    -1,    -1,    -1,    72,   524,    -1,  1467,
    1468,    -1,    -1,    -1,    -1,    -1,  1198,    -1,    -1,   556,
      -1,    -1,    -1,   180,    -1,  1207,    -1,    -1,    -1,    -1,
     547,    -1,    -1,    -1,  1216,    -1,    -1,   574,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   589,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1242,    -1,    -1,    -1,    -1,    -1,    -1,  1249,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,  1273,  1274,   159,   631,   632,    -1,   634,    -1,  1281,
     166,   167,    -1,    -1,    -1,    -1,    -1,   624,    -1,    -1,
      -1,  1293,    -1,    -1,   651,   652,    -1,    -1,    -1,    -1,
      -1,   638,  1304,    -1,    -1,   662,  1308,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   675,  1321,
      -1,    -1,    -1,    -1,    -1,    -1,   663,    -1,    -1,    -1,
      -1,   688,  1334,  1335,    -1,    -1,    -1,  1339,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,   729,    57,    58,    59,    60,    61,    62,    63,
      64,  1403,    -1,  1681,  1406,  1407,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1424,    -1,    -1,   782,  1428,    -1,    -1,    -1,
    1432,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   798,    -1,  1445,   108,   109,    -1,    -1,    -1,     1,
    1452,  1453,  1454,  1455,  1456,  1457,  1458,   814,    -1,    -1,
      -1,   818,  1464,    -1,    -1,    -1,    -1,    -1,    -1,   826,
     827,    -1,   136,    -1,   831,  1477,    -1,    -1,    -1,  1481,
    1482,    -1,    -1,    -1,   821,    -1,   823,    -1,    -1,   846,
      -1,   848,   829,  1495,   158,   852,   853,   854,    -1,   163,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   872,    -1,    -1,    -1,   856,
    1522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   865,    -1,
      -1,    -1,    -1,    -1,   871,    -1,    13,    14,    15,    16,
      17,    -1,  1544,    -1,    -1,    -1,    -1,  1549,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1570,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   933,    -1,    -1,   916,
     937,    -1,    -1,    -1,   921,   942,   138,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,   148,    -1,    -1,    -1,
    1602,    -1,    -1,    -1,    -1,   962,    -1,    -1,   965,  1611,
      -1,    -1,    -1,  1615,    -1,   167,    -1,    -1,    -1,     1,
      -1,    -1,     4,  1901,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   205,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,  1020,    -1,  1667,    -1,    -1,    -1,    -1,
      -1,    -1,  1009,    -1,    -1,    -1,    -1,    59,   155,   156,
      -1,    -1,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,
     167,   243,    -1,    -1,  1696,  1697,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   271,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
     282,   113,    -1,    -1,  1091,    -1,    -1,    -1,  1095,    -1,
      -1,    -1,    -1,  1100,  1101,    -1,    -1,  1104,   300,    -1,
      -1,    -1,    -1,    -1,  1756,   307,   138,  1114,    -1,   311,
     142,    -1,    -1,    -1,  1121,    -1,   148,    -1,    -1,   151,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1788,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1156,
     352,    -1,    -1,   355,    -1,    -1,    -1,    -1,    -1,   191,
      -1,  1168,    -1,    -1,    -1,  1172,   368,    -1,    -1,  1176,
     372,    -1,   204,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1852,    -1,    -1,    -1,    -1,    -1,    -1,  1194,    -1,  1216,
      -1,   243,    -1,    -1,    -1,    -1,   106,  1869,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   260,    -1,
     262,    -1,    -1,    -1,    -1,   267,    -1,    -1,    -1,   271,
      -1,    -1,  1249,    -1,   134,    -1,   136,    -1,    -1,    -1,
     282,    -1,    -1,    -1,    -1,    -1,  1243,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,  1274,    -1,    -1,
      -1,    -1,    -1,   305,  1281,   307,   166,   167,    -1,   311,
      -1,   483,    -1,    -1,    -1,    -1,    -1,   183,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1304,    -1,   331,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1334,  1335,    -1,
      -1,    -1,    -1,    -1,    -1,  1987,    -1,    -1,    -1,    -1,
      -1,    -1,  1329,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   556,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   574,    -1,    -1,    -1,    -1,    -1,    -1,  2031,
      -1,    -1,    -1,    -1,   416,    -1,    -1,   589,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1403,    -1,    -1,  1406,
    1407,    -1,    -1,    -1,    -1,    -1,    -1,  2059,  2060,    -1,
      -1,  2063,    -1,    -1,    -1,    -1,    -1,  1424,   450,    -1,
      -1,  1428,    -1,    -1,    -1,  1432,    -1,    -1,  2080,   631,
     632,    -1,   634,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2092,    -1,     1,    -1,    -1,     4,    -1,   479,    -1,   651,
     652,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     662,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   500,    -1,
    1477,    -1,    -1,   675,  1481,  1482,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   688,    -1,  1495,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      59,    -1,    -1,    -1,   400,    -1,    -1,    -1,   404,   405,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   414,   415,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    -1,    -1,   429,   430,   567,   568,  1544,    -1,    -1,
      -1,    -1,  1549,    -1,  1531,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   450,    -1,    -1,   589,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,
     782,     4,    -1,   142,    -1,    -1,    -1,   483,    -1,   148,
      -1,    -1,   151,    -1,    -1,  1602,   798,    -1,    -1,    -1,
      -1,    -1,   634,    -1,  1611,    -1,    -1,    -1,  1615,    -1,
      -1,    -1,   814,    -1,    -1,    -1,   818,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   826,   827,    -1,    -1,    -1,   831,
      -1,    -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   846,   204,   848,    -1,    -1,    -1,
     852,   853,   854,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    -1,    -1,    -1,    -1,  1654,   700,    -1,
     872,    -1,    -1,    -1,    -1,    -1,   708,    -1,    -1,   711,
     712,    -1,   714,    -1,   243,    -1,    -1,    -1,    -1,    -1,
    1697,   723,    -1,    -1,   726,   727,   728,    -1,    -1,    -1,
      -1,   260,    -1,   262,    -1,    -1,    -1,    -1,   267,    -1,
      -1,    -1,   271,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    -1,   282,    -1,    -1,    -1,    -1,   151,    -1,
      -1,   933,    -1,    -1,    -1,   937,    -1,    -1,    -1,    -1,
     942,    -1,    -1,    -1,    -1,    -1,   305,    -1,   307,    -1,
      -1,    -1,   311,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     962,    -1,    -1,   965,    -1,    -1,    -1,    -1,   191,    -1,
    1757,  1758,   331,    -1,   806,    -1,    -1,    -1,    -1,    -1,
      -1,   204,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,  1020,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   260,    -1,   262,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   416,    -1,    -1,
      -1,    -1,  1869,   759,   760,   761,   762,   763,   764,   765,
     766,   767,   768,   769,   770,   771,   772,   773,   774,   775,
     776,   777,   305,    -1,    -1,    -1,    -1,    -1,    -1,  1091,
      -1,   450,    -1,  1095,    -1,    -1,    -1,  1884,  1100,  1101,
      -1,    -1,  1104,    -1,    -1,   937,   136,    -1,   331,    -1,
      -1,    -1,  1114,    -1,    -1,    -1,    -1,   949,   950,  1121,
     479,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1918,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   500,    -1,    -1,    -1,    -1,    -1,    -1,   844,    -1,
      -1,    -1,    -1,     1,  1156,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1168,    -1,  1955,    -1,
    1172,    -1,    -1,    -1,  1176,    -1,    -1,    -1,    -1,    -1,
    1987,    -1,    -1,    -1,    -1,    -1,    -1,  1019,    -1,    -1,
      -1,    -1,    -1,   416,    -1,  1982,    -1,    -1,    -1,  1986,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   567,   568,
      -1,    59,    -1,    -1,  1216,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2031,    -1,    -1,   450,    -1,    -1,
     589,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1249,    -1,    -1,
      -1,    -1,  2059,  2060,    -1,    -1,   479,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1274,  2080,    -1,   634,    -1,   500,    -1,  1281,
      -1,    -1,    -1,    -1,    -1,  2092,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
     148,    -1,  1304,   151,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1015,
      -1,    -1,    -1,    -1,    -1,  1021,    -1,    -1,    -1,    -1,
      -1,    -1,  1334,  1335,    -1,    -1,  1032,    -1,    -1,    -1,
      -1,   700,    -1,   191,   567,   568,    -1,    -1,    -1,   708,
      -1,    -1,   711,   712,    -1,   714,   204,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   723,    48,  1198,   726,   727,   728,
      -1,    -1,    -1,    -1,    -1,  1207,  1072,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1216,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    -1,   243,    -1,    -1,    -1,    -1,
      -1,  1403,    -1,    -1,  1406,  1407,    -1,    -1,    -1,    -1,
    1242,    -1,   260,    -1,   262,    -1,    -1,  1249,    -1,   267,
      -1,    -1,  1424,   271,    -1,    -1,  1428,    -1,    -1,    -1,
    1432,    -1,    -1,    -1,   282,    -1,    -1,    -1,    -1,   122,
      -1,    -1,  1274,    -1,    -1,    -1,    -1,   806,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,   305,    -1,   307,
      -1,  1293,    -1,   311,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1477,    -1,   700,    -1,  1481,
    1482,    -1,    -1,   331,    -1,   708,   169,    -1,   711,   712,
      -1,   714,    -1,  1495,    -1,    -1,    -1,    -1,    -1,    -1,
     723,    -1,    -1,   726,   727,   728,    -1,  1339,    -1,    -1,
      -1,   194,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1216,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1544,    -1,    -1,    -1,    -1,  1549,    -1,    -1,
     233,    -1,    -1,    -1,   237,    -1,    -1,   240,   241,    -1,
      -1,   244,    -1,    -1,   247,   248,    -1,   250,   416,   252,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   937,    -1,
      -1,    -1,    -1,   806,    -1,    -1,    -1,    -1,    -1,    -1,
     949,   950,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1602,    -1,   450,    -1,    -1,    -1,    -1,    -1,  1304,  1611,
      -1,    -1,    -1,  1615,    -1,    -1,    -1,    -1,    -1,    -1,
    1452,  1453,  1454,  1455,  1456,  1457,  1458,    -1,  1324,    -1,
      -1,   479,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   322,
      -1,    -1,   325,    -1,    -1,  1477,  1342,  1343,  1344,  1481,
    1482,    -1,   500,  1349,  1350,    -1,    -1,    -1,    -1,    -1,
    1019,     5,    -1,  1495,    -1,    -1,   349,   350,    -1,    13,
      14,    15,    16,    17,    -1,    -1,  1372,    -1,    -1,    -1,
      -1,   364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1697,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,  1412,  1413,    -1,   567,
     568,    -1,    -1,    -1,    -1,    -1,   949,   950,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1570,    -1,
      -1,   589,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,   634,   470,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,  1019,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1667,    -1,    -1,    -1,  1198,
      -1,    -1,    -1,    -1,    -1,   528,    -1,    -1,  1207,    -1,
      -1,    -1,   700,    -1,    -1,    -1,    -1,  1216,    -1,    -1,
     708,   544,    -1,   711,   712,  1697,   714,  1869,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   723,    -1,    -1,   726,   727,
     728,    -1,    -1,  1242,    -1,    -1,    -1,    -1,    -1,    -1,
    1249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1274,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1756,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1293,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   628,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1788,    -1,   806,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1198,    -1,    -1,    -1,    -1,
    1339,    -1,    -1,    -1,  1207,  1987,    -1,   670,   671,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     683,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1242,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2031,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1869,    -1,    -1,
      -1,    -1,    -1,    -1,  1740,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2059,  2060,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1293,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2080,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   937,
    2092,    -1,    -1,  1452,  1453,  1454,  1455,  1456,  1457,  1458,
      -1,   949,   950,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1339,    -1,  1477,   802,
     803,    -1,  1481,  1482,    -1,    -1,   809,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1495,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   834,    -1,    -1,   837,   838,    -1,   840,    -1,   842,
     843,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1019,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2031,
      -1,    -1,   885,    -1,    -1,    -1,   889,    -1,    -1,    -1,
     893,  1570,     5,    -1,    -1,   204,   205,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,  2059,    -1,  1452,
    1453,  1454,  1455,  1456,  1457,  1458,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   237,    -1,
      -1,    -1,    -1,    -1,    -1,   244,    49,  1953,    -1,    52,
    2092,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   959,   960,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     973,    -1,    -1,    -1,    -1,    -1,    -1,  1993,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1667,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    -1,   325,    -1,  1697,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,  1570,    -1,    -1,
    1198,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1207,
      -1,    -1,   155,   352,   353,   158,   159,    -1,  1216,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,   372,    -1,    -1,    -1,    -1,    -1,    -1,
    2086,    -1,  2088,    -1,  1242,    -1,    -1,  1756,    -1,    -1,
      -1,  1249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1274,  1110,    -1,  1788,
      -1,  2127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1667,  1293,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2158,    -1,   453,    -1,    -1,    -1,    -1,    -1,
      -1,  1154,    -1,    -1,    -1,    -1,  1159,    -1,    -1,  1162,
      -1,   470,   471,  1166,   473,   474,    -1,    -1,    -1,    -1,
      -1,  1339,    -1,    -1,   483,    -1,    -1,    -1,   487,    -1,
      -1,    -1,    -1,    -1,    -1,  2201,    -1,    -1,    -1,    -1,
    1869,   500,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1756,    -1,    -1,    -1,    -1,    -1,    -1,
     529,    -1,    -1,    -1,   533,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1788,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   574,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1452,  1453,  1454,  1455,  1456,  1457,
    1458,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1310,    -1,  1477,
      -1,    -1,    -1,  1481,  1482,    -1,  1319,  1320,    -1,    -1,
     629,    -1,    -1,   632,    -1,    -1,    -1,  1495,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   651,   652,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2031,   662,    -1,    -1,    -1,   666,    -1,    -1,
      -1,    -1,    -1,    -1,   673,    -1,   675,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1378,    -1,    -1,    -1,    -1,
    2059,    -1,    -1,    -1,    -1,    -1,  1389,    -1,    -1,  1392,
      -1,  1394,  1395,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1570,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    -1,    -1,  2092,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,  1440,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,   782,    -1,    -1,   151,    -1,    -1,    -1,
     155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,   798,
     799,    -1,   167,    -1,    -1,    -1,    -1,    -1,    -1,  1667,
     809,   810,    -1,   812,   813,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   191,   826,   827,    -1,
    1523,    -1,   831,    -1,   833,   834,    -1,    -1,    -1,  1697,
     205,   840,    -1,    -1,   209,    -1,    -1,   846,    -1,   848,
      -1,    -1,    -1,   852,   853,   854,    -1,    -1,    -1,    -1,
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   872,    48,   874,    -1,    -1,    -1,   878,
      -1,    -1,    -1,    -1,    -1,    -1,   885,   886,    -1,    -1,
     889,   890,    -1,    -1,   893,   894,    -1,   262,  1756,    -1,
      -1,    -1,   901,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   276,    -1,    -1,    -1,    -1,    -1,  1610,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1788,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   942,   943,  1638,    -1,    -1,   122,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   321,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,   331,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   975,    -1,  1671,    -1,
      -1,    -1,    -1,    -1,  1677,    -1,    -1,   352,    -1,   354,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1869,    -1,    -1,    -1,    -1,    -1,   205,    -1,    -1,
    1019,  1020,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   219,    -1,   221,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   416,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1752,
      -1,    -1,    -1,    -1,    -1,    -1,   240,   241,    -1,    -1,
     244,    -1,    -1,   247,   248,    -1,   250,   416,   252,    -1,
      -1,    -1,    -1,    -1,    -1,   450,  1085,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1100,  1101,    -1,    -1,  1104,  1105,    -1,    -1,    -1,
    1803,  1804,  1111,    -1,    -1,    -1,    -1,    -1,   483,    -1,
      -1,    -1,    -1,    -1,   489,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   320,    -1,    -1,    -1,  1829,  1830,    -1,    -1,
      -1,    -1,    -1,    -1,  1837,    -1,    -1,    -1,    -1,  1842,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1156,    -1,    -1,
    1159,  1160,    -1,  1162,  1163,    -1,    -1,  1166,  1167,    -1,
      -1,    -1,    -1,  2031,    -1,   349,   350,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2059,   567,   568,    -1,    -1,    -1,    -1,    -1,   574,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   567,   568,
      -1,    -1,    -1,    -1,  2092,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1943,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   632,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1281,    -1,    -1,    -1,    -1,   652,    -1,   654,
      -1,    -1,    -1,    -1,    -1,    -1,   470,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1304,    -1,    -1,    -1,    -1,
     675,  1310,  1311,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   510,  2016,    -1,    -1,    -1,    -1,   516,    -1,
      -1,    -1,    -1,   521,    -1,   700,  1335,    -1,    -1,    -1,
      -1,    -1,    -1,   708,    -1,    -1,   711,   712,    -1,   714,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   723,    -1,
      -1,   726,   727,   728,    -1,    -1,    -1,    -1,    -1,   708,
     544,    -1,    -1,    -1,    -1,   714,    -1,    -1,    -1,  1378,
    1379,    -1,    -1,    -1,   723,    -1,    -1,    -1,    -1,    -1,
    1389,  1390,    -1,  1392,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   742,  1403,    -1,    -1,  1406,  1407,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   782,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2120,    -1,    -1,
      -1,    -1,    -1,    -1,   622,    -1,    -1,    -1,    -1,   778,
      -1,   806,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   827,    -1,   652,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   665,    -1,    -1,
      -1,   846,    -1,   848,    -1,    -1,    -1,   852,   853,   854,
      -1,    -1,    -1,    -1,    -1,    -1,   670,   671,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   872,    -1,   683,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   705,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   716,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1544,    -1,    -1,    -1,    -1,
      -1,    -1,   740,   741,    -1,    -1,   744,    -1,   746,    -1,
      -1,    -1,    -1,    -1,   752,    -1,   754,   755,    -1,    -1,
      -1,    -1,    -1,  1572,    -1,    -1,    -1,   942,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   950,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   782,    -1,    -1,    -1,    -1,   964,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   795,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   806,    -1,
      -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,   802,   803,
      -1,    -1,    -1,    -1,   822,   809,    -1,    -1,    -1,   827,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1020,    -1,    -1,    -1,    -1,
     834,    -1,  1661,   837,   838,    -1,   840,    -1,   842,   843,
     858,    -1,    -1,   861,    -1,    -1,    -1,    -1,  1677,    -1,
      -1,    -1,    -1,    -1,    -1,   873,    -1,   151,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   885,    -1,    -1,   902,   889,    -1,    -1,    -1,   893,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1100,  1101,    -1,    -1,    -1,
      -1,   205,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   950,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   964,   965,    -1,    -1,
      -1,    -1,    -1,    -1,   972,   959,   960,    -1,    -1,    -1,
      -1,  1156,    -1,    -1,    -1,    -1,    -1,    -1,   262,   973,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1822,  1823,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1020,    -1,    -1,    -1,    -1,    -1,    -1,  1838,
    1028,    -1,  1207,    -1,    -1,    -1,    -1,  1035,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   331,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1242,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   352,    -1,
      -1,  1079,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1242,    -1,    -1,    -1,    -1,  1273,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1281,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1293,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1110,    -1,    -1,  1304,
      -1,    -1,  1941,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1949,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1151,    -1,  1153,    -1,  1155,    -1,    -1,
    1335,    -1,    -1,    -1,  1339,    -1,    -1,    -1,    -1,    -1,
    1154,    -1,    -1,    -1,    -1,  1159,   450,    -1,  1162,    -1,
      -1,    -1,  1166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2016,  2017,   483,
      -1,  2020,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1403,    -1,
      -1,  1406,  1407,    -1,    -1,    -1,    -1,    -1,  1236,  1237,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2062,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1452,  1453,  1454,
      -1,    -1,  1457,  1458,    -1,    -1,    -1,    -1,    -1,  1464,
      -1,    -1,    -1,   567,   568,    -1,    -1,    -1,    -1,    -1,
     574,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1308,  2120,  2121,    -1,    -1,    -1,  1314,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1310,    -1,  1477,  1478,
      -1,    -1,  1481,  1482,  1332,  1319,  1320,  1335,  1487,    -1,
      -1,    -1,  1491,    -1,  1493,    -1,  1495,  1522,    -1,    -1,
      -1,  2160,    -1,    -1,  1352,    -1,    -1,    -1,   632,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1544,
      -1,  1369,    -1,    -1,    -1,    -1,    -1,    -1,   652,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1378,    -1,    -1,    -1,    -1,    -1,
      -1,   675,    -1,    -1,    -1,  1389,    -1,    -1,  1392,    -1,
    1394,  1395,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   700,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1444,  1445,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1440,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1470,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1489,  1667,    -1,  1492,    -1,    -1,    -1,  1647,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   782,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1696,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   806,    -1,    -1,    -1,    -1,    -1,    -1,  1523,
      -1,    -1,    -1,  1692,    -1,    -1,  1544,    -1,    -1,    -1,
      -1,    -1,    -1,   827,    -1,  1553,  1554,    -1,    -1,    -1,
      -1,    -1,  1711,  1712,    -1,    -1,  1564,    -1,    -1,    -1,
      -1,    -1,   846,    -1,   848,    -1,    -1,    -1,   852,   853,
     854,    -1,    -1,    -1,    -1,  1583,    -1,  1585,    -1,    -1,
      -1,    -1,  1741,    -1,    -1,    -1,    -1,    -1,   872,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1788,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1610,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1638,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   942,  1667,
      -1,    -1,    -1,    -1,  1672,    -1,   950,  1852,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1854,    -1,    -1,    -1,    -1,
      -1,    -1,  1861,    -1,  1863,    -1,    -1,  1866,  1867,    -1,
    1869,    -1,    -1,    -1,    -1,  1874,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1732,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1020,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1752,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1786,    -1,
      -1,  1789,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1987,    -1,    -1,    -1,    -1,  1815,    -1,  1803,
    1804,    -1,    -1,  1972,    -1,    -1,  1100,  1101,  1977,  1978,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1829,  1830,    -1,  1997,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1842,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1156,    -1,    -1,  2060,    -1,    -1,  2063,    -1,
    2039,    -1,  2041,    -1,    -1,  2044,  2045,    -1,    -1,    -1,
    2049,  2050,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,  1943,
      -1,    -1,    -1,    -1,  2113,  2114,  2115,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1994,  2146,  2147,  2148,
      -1,    -1,    -1,    -1,    18,    -1,    -1,  1281,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1293,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1304,    -1,  2016,    -1,    -1,    49,   136,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,   158,    73,
      74,  1335,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,    -1,   101,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   126,   127,     1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1403,
      -1,    18,  1406,  1407,    -1,    -1,    -1,    -1,    -1,    -1,
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
     127,     1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
    1544,   158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    52,   180,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    73,    74,    -1,    76,    -1,    -1,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
      -1,   101,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1667,    -1,   155,    -1,    -1,   158,   159,
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
      -1,    -1,   154,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,   163,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    71,    72,    73,
      74,    -1,    76,    -1,    -1,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,
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
     156,   157,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
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
      -1,   149,   150,   151,    -1,    -1,    -1,   155,   156,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,     3,     4,     5,     6,     7,     8,     9,
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
       3,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,   136,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,   157,   158,
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
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,
      -1,   166,   167,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,   155,    -1,   157,   158,   159,    -1,    -1,    -1,
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
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,   159,    -1,    -1,    -1,    -1,    -1,
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
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    71,    -1,    73,
      74,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    -1,    -1,    93,
      94,    95,    96,    97,    98,    99,    -1,   101,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,
      -1,   165,   166,   167,   168,   169,   170,   171,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    73,    74,    -1,    76,    -1,    -1,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      -1,    -1,    93,    94,    95,    96,    97,    98,    99,    -1,
     101,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,   163,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,
     163,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,
      -1,   163,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    90,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,   163,    -1,   165,   166,   167,   168,   169,   170,
     171,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   155,    -1,   157,   158,   159,    -1,    -1,    -1,
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
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,
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
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
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
     167,   168,   169,   170,   171,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,   136,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,   162,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   136,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,   158,    13,    14,    15,    16,    17,
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
     158,   159,    -1,    -1,    -1,   163,    -1,    -1,   166,   167,
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
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,   157,   158,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     136,   137,    -1,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,   158,    22,    23,    24,    25,    26,    27,    28,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
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
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
     108,   109,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
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
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   158,    -1,    -1,    13,    14,    15,
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
      -1,    -1,   158,   159,    -1,    13,    14,    15,    16,    17,
     166,   167,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,    20,    -1,    22,    23,    24,    25,    26,    27,
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
     168,   169,   170,   171,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,   158,
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
      -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    20,   158,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   106,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,   136,    22,    23,
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
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,   136,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    49,    55,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    -1,   126,   127,    -1,    -1,    49,   136,    -1,    52,
     134,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,   151,    -1,    -1,
      73,   155,   156,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,   124,    -1,   126,   127,    49,    -1,    -1,    52,    -1,
      54,   134,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,   151,    73,
      -1,    -1,   155,   156,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,   165,   166,   167,   168,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
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
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
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
      -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,   163,
      -1,   165,   166,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,   162,    -1,    -1,   165,   166,   167,
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
      -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,   122,   123,
     124,    49,   126,   127,    52,    -1,    54,    -1,    56,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,   155,    -1,   157,   158,   159,    -1,    -1,    -1,    -1,
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
     118,   119,   120,    -1,   122,   123,   124,    49,   126,   127,
      52,    -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,
     168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,   165,   166,   167,   168,   169,   170,   171
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   182,   405,   406,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
     101,   105,   106,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   121,   134,   136,   155,   156,   158,   159,
     166,   167,   185,   186,   187,   202,   293,   294,   295,   296,
     297,   298,   299,   300,   301,   302,   303,   304,   307,   310,
     312,   313,   314,   315,   316,   317,   318,   319,   320,   322,
     324,   325,   326,   328,   329,   333,   334,   335,   336,   337,
     339,   345,   346,   347,   348,   359,   363,   397,   400,   410,
     416,   418,   424,   428,   433,   434,   435,   436,   437,   438,
     439,   440,   466,   484,   485,   486,   487,     0,   182,   106,
     186,   202,   297,   299,   310,   313,   316,   325,   329,   334,
     120,   155,    58,    61,    62,    64,   155,   155,   422,   423,
     424,   321,   322,   108,   109,   186,   377,   398,   399,   377,
     155,   410,   155,   155,     4,   106,   108,   109,   314,   319,
     320,   155,   155,   202,   423,   428,   434,   435,   436,   438,
     439,   440,   108,   336,   160,   182,   159,   300,   310,   313,
     433,   437,   483,   484,   487,   488,   180,   183,   152,   163,
     179,   223,   380,    89,   161,   417,   377,   161,   161,   161,
     180,   108,   109,   155,   202,   305,   306,   428,   429,   430,
     431,   432,   433,   437,   441,   442,   443,   444,   445,   446,
     447,   448,   449,   455,     3,    47,    48,    50,    55,   327,
       3,   159,   202,   299,   314,   318,   320,   330,   335,   413,
     433,   437,   487,    69,   297,   299,   313,   325,   329,   334,
     414,   433,   437,    65,   319,   319,   314,   320,   308,   319,
     320,   327,   346,   314,   319,   314,   158,   422,   161,   183,
     155,   163,   231,   422,   422,     3,   288,   289,   304,   307,
     313,   317,   318,   159,   310,   313,   485,   377,   377,   410,
     179,   313,   155,   202,   419,   428,   429,   433,   442,   446,
     159,   202,   487,   411,   412,    57,    65,    66,    67,    68,
     159,   177,   377,   386,   388,   392,   394,   395,   335,    57,
     157,   159,   202,   309,   313,   317,   324,   325,   331,   332,
     333,   334,   338,   345,   346,   363,   373,   375,   466,   479,
     480,   481,   482,   487,   488,   108,   109,   163,   170,   186,
     335,   455,   424,   155,   393,   394,   155,    13,    88,   106,
     108,   109,   155,   185,   425,   426,   427,   120,   188,   189,
      49,    52,    54,    56,    73,   103,   104,   106,   107,   118,
     119,   122,   123,   124,   126,   127,   155,   159,   165,   168,
     169,   170,   171,   184,   185,   188,   190,   193,   201,   202,
     203,   204,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   225,   335,   157,   159,   201,
     202,   218,   220,   310,   335,   378,   379,   396,   483,   488,
     425,   313,   434,   435,   436,   438,   439,   440,   157,   157,
     157,   157,   157,   157,   157,   108,   159,   186,   310,   466,
     485,   159,   166,   202,   220,   299,   300,   309,   311,   313,
     325,   332,   334,   370,   371,   372,   374,   375,   479,   487,
     160,   155,   433,   437,   487,   155,   161,   106,   158,   159,
     163,   185,   187,   220,   381,   382,   383,   384,   385,    22,
     381,   155,   377,   231,   155,   186,   419,   186,   423,   428,
     430,   431,   432,   441,   443,   444,   445,   447,   448,   449,
     313,   429,   442,   446,   161,   101,   421,   159,   422,   463,
     466,   421,   422,   422,   417,   288,   155,   422,   463,   421,
     422,   422,   417,   422,   422,   313,   419,   155,   155,   312,
     313,   310,   313,   160,   182,   310,   483,   488,   337,   163,
     417,   288,   377,   377,   380,   299,   318,   415,   433,   437,
     163,   417,   288,   398,   313,   325,   313,   313,   108,   336,
     108,   109,   186,   335,   340,   398,   137,   186,   313,   367,
     368,   372,   373,   376,   154,   182,   231,   304,   180,   433,
     446,   313,   182,   421,   155,   421,   183,   220,   423,   428,
     313,   155,   182,   377,   408,   163,   155,   377,   163,   377,
     137,   166,   167,   391,   157,   161,   377,   395,   157,   422,
     422,   160,   182,   311,   313,   325,   332,   334,   478,   479,
     487,   488,   155,   159,   167,   179,   202,   466,   468,   469,
     470,   471,   472,   473,   490,   202,   338,   487,   313,   332,
     319,   314,   422,   157,   311,   313,   480,   311,   466,   480,
      10,   165,   170,   362,   364,   365,   163,   360,   362,   386,
     179,   386,   425,   157,   161,   155,   157,   120,   155,   201,
     155,   155,   155,   204,   155,   201,   155,   106,   108,   109,
     314,   319,   320,   155,   201,   201,    19,    21,    85,   159,
     168,   169,   205,   206,   220,   227,   231,   348,   378,   487,
     161,   182,   155,   190,   159,   164,   159,   164,   123,   125,
     126,   127,   155,   158,   159,   163,   164,   204,   204,   172,
     166,   173,   174,   168,   169,   128,   129,   130,   131,   175,
     176,   132,   133,   167,   165,   177,   134,   135,   178,   157,
     161,   158,   182,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   179,   222,   223,   224,   155,   202,
     459,   460,   461,   462,   463,   157,   161,   157,   157,   157,
     157,   157,   157,   157,   155,   422,   463,   466,   155,   463,
     466,   155,   182,   155,   310,   485,   160,   182,   183,   159,
     183,   155,   167,   202,   428,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   137,   487,   161,   183,   161,   183,
     377,   377,   155,   182,   182,   182,   159,   187,   182,   382,
     162,   161,   489,   381,   158,   159,   162,   385,   156,   220,
     226,   155,   182,   179,   428,   430,   431,   432,   441,   443,
     444,   445,   447,   448,   449,   157,   157,   157,   157,   157,
     157,   157,   157,   157,   157,   429,   442,   446,   422,   155,
     179,   160,   182,   380,   231,   417,   367,   380,   231,   419,
     227,   379,   227,   379,   419,   108,   159,   408,   231,   417,
     421,   163,   163,   417,   288,   408,   231,   417,   342,   343,
     341,   163,   157,   161,   157,   161,    70,   290,   291,   180,
     166,   220,   182,   428,   371,   410,   408,   377,   160,   182,
     155,   390,   388,   389,    78,   323,   186,   163,   170,   186,
     455,   311,   466,   480,   313,   317,   487,   367,   469,   470,
     471,   160,   182,    18,   220,   313,   468,   490,   422,   422,
     466,   311,   478,   488,   313,   186,   422,   311,   480,   335,
     161,   489,   377,   364,   362,   163,   157,   379,   157,   157,
     426,   156,   194,   195,   196,   220,   180,   378,   190,   159,
     378,   378,   378,   220,   378,   157,   378,   378,   378,   160,
     182,   157,   168,   169,   206,    18,   315,   157,   161,   157,
     166,   167,   157,   226,   220,   163,   220,   186,   220,   186,
     118,   159,   186,   194,   108,   109,   118,   159,   186,   348,
     220,   194,   186,   204,   207,   207,   207,   208,   208,   209,
     209,   210,   210,   210,   210,   211,   211,   212,   213,   214,
     215,   216,   162,   227,   180,   188,   159,   186,   220,   163,
     220,   367,   460,   461,   462,   313,   459,   422,   422,   220,
     379,   155,   422,   463,   466,   155,   463,   466,   367,   367,
     182,   182,   160,   160,   155,   428,   451,   452,   453,   456,
      18,   313,   450,   454,   155,   422,   472,   490,   422,   422,
     490,   155,   422,   472,   422,   422,   183,   219,   377,   371,
     374,   160,   374,   375,   160,   490,   490,   137,   369,   370,
     371,   369,   369,   377,   182,   218,   219,   220,   420,   489,
     381,   383,   154,   182,   157,   161,   182,   369,   220,   157,
     157,   157,   157,   157,   157,   157,   157,   157,   155,   422,
     463,   466,   155,   422,   463,   466,   155,   422,   463,   466,
     419,   188,    22,   466,   220,   320,   335,   464,   231,   157,
     157,   157,   157,   157,   406,   407,   231,   154,   182,   408,
     231,   417,   407,   231,   163,   163,   163,   349,   137,   372,
     373,   186,   292,   377,    18,    71,    73,    74,    76,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    93,    94,    95,    96,    97,    98,    99,   101,   108,
     109,   121,   155,   159,   227,   228,   229,   230,   231,   232,
     233,   235,   236,   245,   251,   252,   253,   254,   255,   256,
     261,   262,   265,   266,   267,   268,   269,   270,   271,   277,
     278,   279,   293,   313,   317,   377,   418,    70,   183,   183,
     369,   161,   409,   407,   157,   297,   299,   310,   401,   402,
     403,   404,   396,   179,   387,   387,   364,   163,   422,   422,
     311,   480,   159,   166,   202,   220,   335,   220,   313,   157,
     157,   157,   157,     5,   313,   422,   468,   163,   170,   186,
     455,    10,   365,   154,   179,   366,   489,   163,   364,   163,
     157,   157,   161,   157,   161,   182,   161,   157,   157,   161,
     157,   204,   157,   157,   157,   204,    18,   315,   220,   157,
     157,   156,   163,   204,   160,   183,   194,   160,   160,   118,
     122,   124,   187,   197,   198,   199,   157,   197,   160,   161,
     154,   218,   162,   157,   197,   183,   382,   157,   157,   157,
     157,   459,   367,   367,   157,   157,   369,   369,   456,   157,
     157,   157,   157,   155,   428,   455,   450,   454,   367,   367,
     160,   183,   490,   161,   183,   157,   161,   161,   183,   183,
     380,   197,   137,   171,   183,   183,   154,   381,   220,   422,
     156,   220,   369,   183,   155,   422,   463,   466,   155,   422,
     463,   466,   155,   422,   463,   466,   367,   367,   367,   421,
     157,   149,   171,   183,   465,   161,   183,   409,   401,   407,
     231,   409,   349,   349,   349,     3,     5,    10,    73,   154,
     294,   301,   302,   310,   313,   350,   355,   483,   161,   180,
     155,    61,    62,   180,   231,   293,   418,   155,   155,    18,
     229,   155,   155,   180,   377,   180,   377,   166,   377,   163,
     228,   155,   155,   155,   229,   155,   231,   220,   221,   221,
      14,   280,   256,   267,   180,   183,   233,    78,   180,   377,
      91,    92,   260,   264,   112,   135,   259,   111,   134,   263,
     259,   376,   313,   162,   292,   160,   160,   183,   409,   377,
     419,   183,   180,   183,   180,   183,   157,   379,   393,   393,
     489,   364,   362,   362,   182,   183,   183,   183,   220,   155,
     422,   472,   466,   312,     5,   166,   183,   220,   364,   163,
     422,   422,   335,   377,   163,   219,   154,   364,   489,   154,
     182,   196,   309,   186,    78,   191,   192,   378,   204,   204,
     204,   204,   204,   163,   382,   161,   154,   200,   159,   198,
     200,   200,   160,   161,   125,   158,   160,   226,   218,   180,
     160,   489,   155,   422,   463,   466,   157,   157,   183,   183,
     157,   155,   422,   463,   466,   155,   422,   472,   428,   422,
     422,   157,   157,   160,   374,   160,   137,   371,   137,   157,
     157,   183,   219,   219,   160,   160,   183,   183,   157,   367,
     367,   367,   157,   157,   157,   380,   422,   161,   220,   220,
     320,   335,   160,   154,   183,   409,   154,   154,   154,   154,
     310,   310,   348,   356,   483,   310,   355,   155,   344,   180,
     180,   155,   162,   202,   351,   352,   358,   428,   429,   442,
     446,   161,   180,   377,   377,   194,   180,   231,   180,   231,
     227,   237,   293,   295,   298,   304,   313,   317,   227,    80,
     157,   237,   149,   150,   151,   156,   157,   180,   227,   246,
     247,   248,   293,   180,   180,   227,   180,   382,   180,   227,
     226,   227,   246,   113,   114,   115,   116,   117,   272,   274,
     275,   180,   100,   180,    84,   155,   157,   154,   180,   180,
     155,   155,   229,   229,   256,   155,   266,   256,   266,   231,
     422,   180,   157,   154,   391,   154,   182,   161,   161,   154,
     489,   163,   163,   160,   160,   160,   183,   367,   220,   220,
     183,   160,   183,   489,   364,   361,   362,   366,   366,   382,
     489,   154,   401,   467,   468,   157,   162,   157,   161,   162,
     382,   489,   226,   123,   197,   198,   159,   198,   159,   198,
     160,   154,   367,   157,   157,   367,   367,   160,   183,   157,
     422,   157,   157,   157,   227,   465,   154,   154,   344,   344,
     344,   351,   155,   202,   353,   354,   463,   474,   475,   476,
     477,   180,   161,   180,   351,   180,   396,   423,   428,   220,
     313,   154,   161,   180,   357,   358,   357,   357,   377,   157,
     157,   227,   313,   157,   155,   229,   157,   149,   150,   151,
     171,   180,   249,   250,   229,   228,   180,   250,   157,   162,
     227,   156,   227,   228,   248,   180,   489,   157,   157,   157,
     157,   231,   274,   275,   155,   220,   155,   188,   204,   257,
     227,    75,   110,   258,   260,    75,     1,   229,   422,   387,
     402,   182,   182,   154,   364,   364,   160,   157,   183,   183,
     160,   160,   154,   489,   362,   163,   489,   154,   183,   157,
     220,   192,   220,   489,   154,   160,   160,   197,   197,   157,
     422,   422,   157,   157,   160,   160,   220,   180,   475,   476,
     477,   313,   474,   161,   180,   422,   422,   180,   157,   428,
     422,   229,   229,    77,    78,   163,   240,   241,   242,   157,
     227,    75,   229,   227,   156,   227,    75,   180,   108,   156,
     227,   228,   248,   156,   227,   229,   247,   250,   250,   180,
     227,   154,   163,   242,   229,   229,   155,   182,   180,   188,
     157,   162,   157,   161,   162,   157,   229,   155,   229,   229,
     229,   393,   377,   419,   489,   489,   160,   160,   154,   163,
     364,   154,   154,   154,   160,   160,   157,   157,   157,   474,
     422,   352,    75,     1,   219,   238,   239,   420,     1,   162,
       1,   182,   229,   240,    75,   180,   157,   229,    75,   180,
     171,   171,   229,   228,   250,   250,   180,   108,   227,   171,
     171,    75,   156,   227,   156,   227,   228,   180,     1,   182,
     182,   276,   311,   313,   483,   162,   180,   159,   188,   281,
     282,   283,   204,   194,   227,   259,   154,   154,   364,   489,
     155,   422,   463,   466,   354,   229,   137,     1,   161,   162,
     154,   286,   287,   293,   229,    75,   180,   229,   227,   156,
     156,   227,   156,   227,   156,   227,   228,   156,   227,   156,
     227,   229,   171,   171,   171,   171,   154,   286,   276,   183,
     155,   202,   419,   474,   186,   162,   106,   155,   157,   162,
     161,   157,   157,    75,   255,   489,   154,   367,   219,   238,
     241,   243,   244,   293,   229,   171,   171,   171,   171,   156,
     156,   227,   156,   227,   156,   227,   243,   183,   180,   273,
     313,   281,   160,   219,   180,   281,   283,   229,    75,   154,
     157,   229,   234,   183,   241,   156,   156,   227,   156,   227,
     156,   227,   183,   273,   218,   157,   162,   188,   157,   157,
     162,   229,     1,   229,   154,   234,   154,   157,   231,   188,
     284,   155,   180,   284,   231,   161,   162,   219,   157,   188,
     186,   285,   157,   180,   157,   161,   180,   186
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
     201,   201,   201,   202,   202,   202,   203,   203,   203,   203,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   205,
     205,   205,   205,   206,   206,   207,   207,   208,   208,   208,
     208,   209,   209,   209,   210,   210,   210,   211,   211,   211,
     211,   211,   212,   212,   212,   213,   213,   214,   214,   215,
     215,   216,   216,   217,   217,   218,   218,   218,   219,   220,
     220,   220,   221,   221,   222,   222,   223,   223,   224,   224,
     224,   224,   224,   224,   224,   224,   224,   224,   224,   225,
     225,   226,   226,   226,   226,   227,   227,   228,   228,   229,
     229,   229,   229,   229,   229,   229,   229,   229,   229,   229,
     229,   229,   229,   229,   229,   230,   230,   231,   231,   232,
     232,   233,   233,   233,   233,   233,   234,   234,   234,   235,
     236,   236,   236,   236,   236,   236,   236,   236,   237,   237,
     237,   237,   238,   238,   238,   239,   239,   240,   240,   240,
     240,   240,   241,   241,   242,   243,   243,   244,   244,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   246,   246,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   248,   248,
     248,   249,   249,   250,   250,   250,   251,   251,   251,   251,
     251,   251,   251,   251,   251,   251,   251,   251,   251,   251,
     251,   251,   251,   251,   251,   251,   252,   252,   253,   254,
     255,   256,   256,   257,   257,   258,   259,   259,   260,   260,
     261,   261,   261,   261,   261,   261,   262,   263,   263,   264,
     265,   265,   266,   266,   267,   267,   267,   268,   269,   270,
     271,   271,   271,   272,   272,   273,   273,   274,   274,   274,
     274,   275,   276,   276,   276,   276,   276,   277,   278,   278,
     279,   279,   279,   279,   279,   280,   280,   281,   281,   282,
     282,   283,   283,   284,   284,   284,   285,   285,   286,   286,
     287,   287,   288,   288,   289,   289,   290,   290,   291,   291,
     292,   292,   293,   293,   293,   294,   294,   295,   295,   295,
     295,   295,   296,   296,   296,   297,   297,   297,   298,   298,
     298,   298,   298,   299,   299,   299,   299,   300,   300,   301,
     301,   301,   302,   302,   302,   302,   302,   303,   303,   304,
     304,   304,   304,   305,   305,   305,   305,   305,   306,   306,
     307,   307,   307,   307,   308,   308,   308,   309,   309,   309,
     310,   310,   310,   311,   311,   311,   312,   312,   313,   313,
     314,   314,   315,   315,   315,   315,   315,   316,   317,   317,
     317,   318,   318,   319,   319,   319,   319,   319,   319,   319,
     319,   319,   320,   320,   320,   320,   320,   320,   320,   320,
     320,   320,   320,   320,   320,   320,   320,   320,   320,   320,
     320,   320,   320,   320,   320,   320,   320,   320,   320,   320,
     321,   321,   322,   323,   323,   324,   324,   324,   324,   324,
     325,   325,   326,   326,   326,   326,   327,   327,   327,   327,
     327,   327,   328,   328,   328,   328,   329,   330,   329,   329,
     331,   331,   331,   331,   332,   332,   332,   333,   333,   333,
     333,   334,   334,   334,   335,   335,   335,   335,   335,   335,
     336,   336,   336,   337,   337,   338,   338,   340,   339,   341,
     339,   342,   339,   343,   339,   339,   344,   344,   345,   345,
     346,   346,   347,   347,   347,   348,   348,   348,   348,   348,
     348,   348,   348,   349,   349,   350,   350,   350,   350,   350,
     350,   350,   350,   350,   350,   350,   350,   351,   351,   351,
     352,   352,   352,   352,   353,   353,   353,   354,   355,   355,
     356,   356,   357,   357,   358,   359,   359,   360,   359,   359,
     359,   359,   359,   359,   361,   359,   359,   359,   359,   359,
     362,   362,   363,   363,   364,   364,   364,   364,   365,   365,
     366,   366,   366,   367,   367,   367,   367,   368,   368,   368,
     368,   369,   369,   369,   369,   369,   369,   369,   370,   370,
     370,   370,   371,   371,   372,   372,   373,   373,   374,   374,
     374,   374,   374,   375,   375,   375,   375,   375,   376,   376,
     377,   377,   377,   378,   378,   378,   379,   379,   380,   380,
     380,   380,   381,   381,   382,   382,   382,   382,   382,   383,
     383,   384,   384,   385,   385,   385,   385,   385,   386,   386,
     387,   387,   389,   388,   390,   388,   388,   388,   388,   391,
     391,   391,   391,   392,   392,   392,   392,   393,   393,   394,
     394,   395,   395,   396,   396,   396,   396,   397,   397,   397,
     398,   398,   399,   399,   400,   400,   400,   400,   401,   401,
     402,   402,   403,   403,   403,   404,   404,   405,   405,   406,
     406,   407,   407,   408,   409,   410,   410,   410,   410,   410,
     410,   410,   410,   410,   410,   410,   411,   410,   412,   410,
     413,   410,   414,   410,   415,   410,   416,   416,   416,   417,
     417,   418,   418,   418,   418,   418,   418,   418,   418,   418,
     418,   419,   419,   419,   419,   420,   421,   421,   422,   422,
     423,   423,   424,   424,   424,   425,   425,   426,   426,   426,
     427,   427,   427,   427,   427,   427,   428,   428,   429,   429,
     429,   429,   430,   430,   430,   430,   431,   431,   431,   431,
     431,   431,   431,   432,   432,   432,   432,   433,   433,   433,
     434,   434,   434,   434,   434,   435,   435,   435,   435,   436,
     436,   436,   436,   436,   436,   437,   437,   437,   438,   438,
     438,   438,   438,   439,   439,   439,   439,   440,   440,   440,
     440,   440,   440,   441,   441,   442,   442,   442,   442,   443,
     443,   443,   443,   444,   444,   444,   444,   444,   444,   444,
     445,   445,   445,   445,   446,   446,   446,   447,   447,   447,
     447,   447,   448,   448,   448,   448,   449,   449,   449,   449,
     449,   449,   450,   450,   450,   450,   450,   451,   451,   451,
     452,   452,   452,   452,   453,   453,   453,   454,   454,   454,
     454,   454,   455,   455,   456,   456,   456,   457,   457,   458,
     458,   459,   459,   459,   460,   460,   460,   460,   460,   461,
     461,   461,   461,   462,   462,   462,   463,   463,   463,   463,
     463,   464,   464,   464,   464,   464,   464,   465,   465,   466,
     466,   466,   466,   467,   467,   468,   468,   468,   468,   469,
     469,   469,   469,   469,   470,   470,   470,   470,   471,   471,
     471,   472,   472,   472,   473,   473,   473,   473,   473,   473,
     474,   474,   474,   475,   475,   475,   475,   475,   476,   476,
     476,   476,   477,   477,   478,   478,   478,   479,   479,   480,
     480,   480,   480,   480,   480,   481,   481,   481,   481,   481,
     481,   481,   481,   481,   481,   482,   482,   482,   482,   483,
     483,   483,   484,   484,   485,   485,   485,   485,   485,   485,
     486,   486,   486,   486,   486,   486,   487,   487,   487,   488,
     488,   488,   489,   489,   490,   490
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
       5,     7,     5,    10,     7,     5,    10,     7,     1,     1,
       1,     2,     1,     3,     1,     1,     3,     2,     3,     3,
       2,     2,     1,     2,     2,     0,     1,     2,     3,     4,
       6,     5,     7,     6,     7,     7,     8,     4,     6,     5,
       7,     1,     3,     4,     5,     4,     3,     5,     1,     2,
       3,     3,     3,     5,     5,     5,     5,     3,     5,     5,
       5,     3,     4,     5,     5,     5,     5,     7,     7,     7,
       7,     7,     7,     7,     2,     3,     4,     4,     4,     6,
       6,     6,     6,     6,     6,     6,     3,     4,     1,     1,
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
       1,     1,     2,     3,     6,     3,     3,     4,     1,     2,
       2,     3,     8,     9,     9,     8,     8,     5,     7,     2,
       2,     3,     3,     3,     4,     3,     4,     4,     5,     2,
       1,     1,     1,     3,     3,     2,     4,     6,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     4,     1,     2,
       3,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     5,     0,     1,     1,     2,     2,     3,     3,
       1,     3,     1,     2,     2,     2,     4,     4,     4,     4,
       1,     1,     1,     2,     2,     3,     1,     0,     3,     2,
       1,     2,     2,     3,     1,     2,     2,     1,     2,     2,
       3,     1,     2,     2,     1,     2,     3,     1,     2,     3,
       1,     3,     4,     1,     1,     1,     1,     0,     7,     0,
       8,     0,     8,     0,     8,     1,     0,     3,     3,     3,
       1,     1,     2,     1,     1,     1,     2,     1,     2,     1,
       2,     1,     2,     0,     2,     3,     3,     4,     4,     4,
       3,     2,     2,     3,     3,     2,     1,     0,     1,     4,
       1,     2,     2,     2,     0,     1,     4,     1,     2,     3,
       1,     2,     0,     1,     2,     6,     7,     0,     9,     8,
       9,    10,     8,     9,     0,    13,    11,    12,    11,     1,
       0,     1,     3,     3,     3,     2,     5,     5,     1,     1,
       0,     2,     5,     0,     1,     1,     3,     1,     1,     3,
       3,     0,     1,     1,     1,     3,     3,     3,     1,     3,
       3,     5,     1,     3,     3,     3,     2,     3,     1,     3,
       3,     4,     1,     1,     1,     1,     2,     1,     1,     3,
       1,     1,     1,     1,     1,     2,     1,     1,     0,     2,
       2,     4,     1,     4,     0,     1,     2,     3,     4,     2,
       2,     1,     2,     2,     5,     5,     7,     6,     1,     3,
       0,     2,     0,     5,     0,     5,     3,     1,     8,     0,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     1,
       2,     5,     6,     1,     1,     3,     3,     2,     3,     3,
       2,     4,     1,     4,     7,     5,    10,     8,     1,     4,
       2,     2,     1,     1,     5,     2,     5,     0,     1,     3,
       4,     0,     1,     0,     0,     1,     1,     2,     2,     2,
       2,     2,     2,     1,     2,     5,     0,     6,     0,     8,
       0,     7,     0,     7,     0,     8,     1,     2,     3,     0,
       5,     3,     4,     4,     4,     4,     5,     5,     5,     5,
       6,     1,     1,     1,     1,     3,     0,     5,     0,     1,
       1,     2,     6,     4,     4,     1,     3,     0,     1,     4,
       1,     1,     1,     1,     1,     1,     1,     3,     2,     1,
       2,     2,     2,     3,     4,     5,     2,     4,     5,     4,
       5,     3,     4,     6,     7,     3,     4,     2,     1,     2,
       4,     6,     7,     3,     4,     2,     3,     4,     5,     4,
       5,     4,     5,     3,     4,     1,     1,     1,     4,     6,
       7,     3,     4,     2,     3,     3,     4,     4,     5,     4,
       5,     3,     4,     1,     3,     2,     1,     2,     2,     2,
       3,     4,     5,     2,     4,     5,     4,     5,     3,     4,
       6,     7,     3,     4,     2,     1,     2,     4,     6,     7,
       3,     4,     2,     3,     4,     5,     4,     5,     4,     5,
       3,     4,     2,     4,     1,     2,     2,     2,     3,     4,
       2,     4,     4,     3,     4,     6,     3,     2,     4,     1,
       2,     2,     1,     1,     2,     3,     4,     2,     4,     4,
       6,     1,     2,     2,     1,     2,     2,     3,     4,     1,
       4,     4,     3,     3,     6,     3,     2,     3,     7,     5,
       1,     1,     1,     3,     3,     3,     5,     1,     1,     5,
       5,     6,     6,     0,     1,     1,     3,     2,     2,     1,
       2,     2,     3,     4,     1,     4,     4,     3,     3,     6,
       3,     1,     2,     1,     2,     6,     5,     6,     7,     7,
       1,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     6,     3,     1,     1,     2,     1,     1,     2,
       3,     2,     3,     2,     3,     3,     2,     4,     3,     2,
       3,     2,     4,     3,     2,     6,     6,     6,     7,     1,
       2,     1,     1,     1,     2,     3,     2,     3,     2,     3,
       3,     4,     2,     3,     4,     2,     5,     6,     7,     5,
       6,     6,     0,     1,     0,     2
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
#line 8466 "Parser/parser.cc"
    break;

  case 3:
#line 631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8472 "Parser/parser.cc"
    break;

  case 4:
#line 638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8478 "Parser/parser.cc"
    break;

  case 5:
#line 639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8484 "Parser/parser.cc"
    break;

  case 6:
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8490 "Parser/parser.cc"
    break;

  case 7:
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8496 "Parser/parser.cc"
    break;

  case 8:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8502 "Parser/parser.cc"
    break;

  case 20:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8508 "Parser/parser.cc"
    break;

  case 21:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8514 "Parser/parser.cc"
    break;

  case 22:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8520 "Parser/parser.cc"
    break;

  case 23:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8530 "Parser/parser.cc"
    break;

  case 24:
#line 685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8536 "Parser/parser.cc"
    break;

  case 25:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8542 "Parser/parser.cc"
    break;

  case 26:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8548 "Parser/parser.cc"
    break;

  case 28:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8554 "Parser/parser.cc"
    break;

  case 29:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8560 "Parser/parser.cc"
    break;

  case 30:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, (yyvsp[-2].decl), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8566 "Parser/parser.cc"
    break;

  case 31:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8572 "Parser/parser.cc"
    break;

  case 32:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8582 "Parser/parser.cc"
    break;

  case 33:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8588 "Parser/parser.cc"
    break;

  case 34:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8594 "Parser/parser.cc"
    break;

  case 35:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8600 "Parser/parser.cc"
    break;

  case 36:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8606 "Parser/parser.cc"
    break;

  case 37:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8612 "Parser/parser.cc"
    break;

  case 38:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8618 "Parser/parser.cc"
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
#line 8630 "Parser/parser.cc"
    break;

  case 41:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8639 "Parser/parser.cc"
    break;

  case 42:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8645 "Parser/parser.cc"
    break;

  case 44:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) )) ) ); }
#line 8651 "Parser/parser.cc"
    break;

  case 45:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8657 "Parser/parser.cc"
    break;

  case 46:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8663 "Parser/parser.cc"
    break;

  case 47:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8669 "Parser/parser.cc"
    break;

  case 48:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8679 "Parser/parser.cc"
    break;

  case 49:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8685 "Parser/parser.cc"
    break;

  case 50:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8692 "Parser/parser.cc"
    break;

  case 51:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8698 "Parser/parser.cc"
    break;

  case 52:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8704 "Parser/parser.cc"
    break;

  case 53:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8710 "Parser/parser.cc"
    break;

  case 54:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8716 "Parser/parser.cc"
    break;

  case 55:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8722 "Parser/parser.cc"
    break;

  case 56:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8728 "Parser/parser.cc"
    break;

  case 57:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8734 "Parser/parser.cc"
    break;

  case 58:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8740 "Parser/parser.cc"
    break;

  case 59:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8746 "Parser/parser.cc"
    break;

  case 60:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8752 "Parser/parser.cc"
    break;

  case 61:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8758 "Parser/parser.cc"
    break;

  case 62:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8764 "Parser/parser.cc"
    break;

  case 63:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8770 "Parser/parser.cc"
    break;

  case 64:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8776 "Parser/parser.cc"
    break;

  case 65:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8782 "Parser/parser.cc"
    break;

  case 66:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8788 "Parser/parser.cc"
    break;

  case 67:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8794 "Parser/parser.cc"
    break;

  case 68:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8804 "Parser/parser.cc"
    break;

  case 69:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8810 "Parser/parser.cc"
    break;

  case 72:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8816 "Parser/parser.cc"
    break;

  case 73:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8822 "Parser/parser.cc"
    break;

  case 76:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8828 "Parser/parser.cc"
    break;

  case 78:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8834 "Parser/parser.cc"
    break;

  case 79:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8840 "Parser/parser.cc"
    break;

  case 80:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8846 "Parser/parser.cc"
    break;

  case 81:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8852 "Parser/parser.cc"
    break;

  case 82:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8858 "Parser/parser.cc"
    break;

  case 83:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8864 "Parser/parser.cc"
    break;

  case 84:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8870 "Parser/parser.cc"
    break;

  case 85:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8876 "Parser/parser.cc"
    break;

  case 86:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8884 "Parser/parser.cc"
    break;

  case 87:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8890 "Parser/parser.cc"
    break;

  case 88:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8899 "Parser/parser.cc"
    break;

  case 91:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8905 "Parser/parser.cc"
    break;

  case 92:
#line 906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8911 "Parser/parser.cc"
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
#line 8931 "Parser/parser.cc"
    break;

  case 94:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8937 "Parser/parser.cc"
    break;

  case 95:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8943 "Parser/parser.cc"
    break;

  case 96:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8949 "Parser/parser.cc"
    break;

  case 97:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8955 "Parser/parser.cc"
    break;

  case 98:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8961 "Parser/parser.cc"
    break;

  case 99:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8967 "Parser/parser.cc"
    break;

  case 100:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8973 "Parser/parser.cc"
    break;

  case 101:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8979 "Parser/parser.cc"
    break;

  case 102:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8988 "Parser/parser.cc"
    break;

  case 103:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8994 "Parser/parser.cc"
    break;

  case 104:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9000 "Parser/parser.cc"
    break;

  case 105:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9006 "Parser/parser.cc"
    break;

  case 106:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9012 "Parser/parser.cc"
    break;

  case 107:
#line 958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9018 "Parser/parser.cc"
    break;

  case 108:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9024 "Parser/parser.cc"
    break;

  case 109:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9030 "Parser/parser.cc"
    break;

  case 111:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9036 "Parser/parser.cc"
    break;

  case 112:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9042 "Parser/parser.cc"
    break;

  case 113:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 114:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( nullptr ) ) ); }
#line 9054 "Parser/parser.cc"
    break;

  case 115:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9060 "Parser/parser.cc"
    break;

  case 116:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9066 "Parser/parser.cc"
    break;

  case 117:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9072 "Parser/parser.cc"
    break;

  case 118:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9078 "Parser/parser.cc"
    break;

  case 126:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9084 "Parser/parser.cc"
    break;

  case 128:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9090 "Parser/parser.cc"
    break;

  case 129:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9096 "Parser/parser.cc"
    break;

  case 130:
#line 1010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9102 "Parser/parser.cc"
    break;

  case 132:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9108 "Parser/parser.cc"
    break;

  case 133:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9114 "Parser/parser.cc"
    break;

  case 135:
#line 1024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9120 "Parser/parser.cc"
    break;

  case 136:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9126 "Parser/parser.cc"
    break;

  case 138:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9132 "Parser/parser.cc"
    break;

  case 139:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9138 "Parser/parser.cc"
    break;

  case 140:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9144 "Parser/parser.cc"
    break;

  case 141:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9150 "Parser/parser.cc"
    break;

  case 143:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9156 "Parser/parser.cc"
    break;

  case 144:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9162 "Parser/parser.cc"
    break;

  case 146:
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9168 "Parser/parser.cc"
    break;

  case 148:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9174 "Parser/parser.cc"
    break;

  case 150:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9180 "Parser/parser.cc"
    break;

  case 152:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9186 "Parser/parser.cc"
    break;

  case 154:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9192 "Parser/parser.cc"
    break;

  case 156:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9198 "Parser/parser.cc"
    break;

  case 157:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9204 "Parser/parser.cc"
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
#line 9216 "Parser/parser.cc"
    break;

  case 161:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9222 "Parser/parser.cc"
    break;

  case 162:
#line 1108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9228 "Parser/parser.cc"
    break;

  case 166:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9234 "Parser/parser.cc"
    break;

  case 167:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9240 "Parser/parser.cc"
    break;

  case 168:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9246 "Parser/parser.cc"
    break;

  case 169:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9252 "Parser/parser.cc"
    break;

  case 170:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9258 "Parser/parser.cc"
    break;

  case 171:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9264 "Parser/parser.cc"
    break;

  case 172:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9270 "Parser/parser.cc"
    break;

  case 173:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9276 "Parser/parser.cc"
    break;

  case 174:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9282 "Parser/parser.cc"
    break;

  case 175:
#line 1130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9288 "Parser/parser.cc"
    break;

  case 176:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9294 "Parser/parser.cc"
    break;

  case 177:
#line 1132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9300 "Parser/parser.cc"
    break;

  case 178:
#line 1133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9306 "Parser/parser.cc"
    break;

  case 179:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9312 "Parser/parser.cc"
    break;

  case 180:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) )); }
#line 9318 "Parser/parser.cc"
    break;

  case 182:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9324 "Parser/parser.cc"
    break;

  case 183:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 9330 "Parser/parser.cc"
    break;

  case 184:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9336 "Parser/parser.cc"
    break;

  case 186:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9342 "Parser/parser.cc"
    break;

  case 187:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9348 "Parser/parser.cc"
    break;

  case 202:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9354 "Parser/parser.cc"
    break;

  case 204:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9360 "Parser/parser.cc"
    break;

  case 205:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9366 "Parser/parser.cc"
    break;

  case 206:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9377 "Parser/parser.cc"
    break;

  case 207:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9383 "Parser/parser.cc"
    break;

  case 208:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9389 "Parser/parser.cc"
    break;

  case 210:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9395 "Parser/parser.cc"
    break;

  case 211:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9401 "Parser/parser.cc"
    break;

  case 212:
#line 1227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9407 "Parser/parser.cc"
    break;

  case 213:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9413 "Parser/parser.cc"
    break;

  case 214:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9419 "Parser/parser.cc"
    break;

  case 217:
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9425 "Parser/parser.cc"
    break;

  case 218:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9431 "Parser/parser.cc"
    break;

  case 219:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9437 "Parser/parser.cc"
    break;

  case 220:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9443 "Parser/parser.cc"
    break;

  case 221:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9449 "Parser/parser.cc"
    break;

  case 222:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9455 "Parser/parser.cc"
    break;

  case 223:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 9469 "Parser/parser.cc"
    break;

  case 224:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9475 "Parser/parser.cc"
    break;

  case 225:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9481 "Parser/parser.cc"
    break;

  case 226:
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 9490 "Parser/parser.cc"
    break;

  case 227:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9496 "Parser/parser.cc"
    break;

  case 228:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9502 "Parser/parser.cc"
    break;

  case 229:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9508 "Parser/parser.cc"
    break;

  case 230:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9514 "Parser/parser.cc"
    break;

  case 231:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9520 "Parser/parser.cc"
    break;

  case 232:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9526 "Parser/parser.cc"
    break;

  case 233:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9532 "Parser/parser.cc"
    break;

  case 235:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9538 "Parser/parser.cc"
    break;

  case 236:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9544 "Parser/parser.cc"
    break;

  case 237:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9550 "Parser/parser.cc"
    break;

  case 238:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9556 "Parser/parser.cc"
    break;

  case 239:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9562 "Parser/parser.cc"
    break;

  case 240:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9568 "Parser/parser.cc"
    break;

  case 241:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9574 "Parser/parser.cc"
    break;

  case 243:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9580 "Parser/parser.cc"
    break;

  case 244:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9586 "Parser/parser.cc"
    break;

  case 245:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9592 "Parser/parser.cc"
    break;

  case 247:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9598 "Parser/parser.cc"
    break;

  case 248:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9604 "Parser/parser.cc"
    break;

  case 249:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9610 "Parser/parser.cc"
    break;

  case 250:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9619 "Parser/parser.cc"
    break;

  case 251:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 252:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 253:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 254:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9646 "Parser/parser.cc"
    break;

  case 255:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9652 "Parser/parser.cc"
    break;

  case 256:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9658 "Parser/parser.cc"
    break;

  case 257:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9664 "Parser/parser.cc"
    break;

  case 258:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9673 "Parser/parser.cc"
    break;

  case 259:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 260:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 262:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9704 "Parser/parser.cc"
    break;

  case 263:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9710 "Parser/parser.cc"
    break;

  case 264:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9719 "Parser/parser.cc"
    break;

  case 265:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9725 "Parser/parser.cc"
    break;

  case 266:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9731 "Parser/parser.cc"
    break;

  case 267:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9737 "Parser/parser.cc"
    break;

  case 268:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9743 "Parser/parser.cc"
    break;

  case 269:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9749 "Parser/parser.cc"
    break;

  case 270:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9755 "Parser/parser.cc"
    break;

  case 271:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9764 "Parser/parser.cc"
    break;

  case 272:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9773 "Parser/parser.cc"
    break;

  case 273:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9779 "Parser/parser.cc"
    break;

  case 274:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9788 "Parser/parser.cc"
    break;

  case 275:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9797 "Parser/parser.cc"
    break;

  case 276:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9803 "Parser/parser.cc"
    break;

  case 277:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9809 "Parser/parser.cc"
    break;

  case 278:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9815 "Parser/parser.cc"
    break;

  case 279:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9821 "Parser/parser.cc"
    break;

  case 280:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9827 "Parser/parser.cc"
    break;

  case 281:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9833 "Parser/parser.cc"
    break;

  case 282:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9839 "Parser/parser.cc"
    break;

  case 283:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9845 "Parser/parser.cc"
    break;

  case 284:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9854 "Parser/parser.cc"
    break;

  case 285:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9864 "Parser/parser.cc"
    break;

  case 286:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9870 "Parser/parser.cc"
    break;

  case 287:
#line 1483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9876 "Parser/parser.cc"
    break;

  case 288:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9885 "Parser/parser.cc"
    break;

  case 289:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9895 "Parser/parser.cc"
    break;

  case 290:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9901 "Parser/parser.cc"
    break;

  case 291:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9910 "Parser/parser.cc"
    break;

  case 292:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9920 "Parser/parser.cc"
    break;

  case 293:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9926 "Parser/parser.cc"
    break;

  case 294:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9932 "Parser/parser.cc"
    break;

  case 295:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9938 "Parser/parser.cc"
    break;

  case 296:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9944 "Parser/parser.cc"
    break;

  case 297:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9953 "Parser/parser.cc"
    break;

  case 298:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9963 "Parser/parser.cc"
    break;

  case 299:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9969 "Parser/parser.cc"
    break;

  case 300:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9978 "Parser/parser.cc"
    break;

  case 301:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9988 "Parser/parser.cc"
    break;

  case 302:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9994 "Parser/parser.cc"
    break;

  case 303:
#line 1546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10003 "Parser/parser.cc"
    break;

  case 304:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10013 "Parser/parser.cc"
    break;

  case 305:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10019 "Parser/parser.cc"
    break;

  case 306:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 10028 "Parser/parser.cc"
    break;

  case 307:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 10039 "Parser/parser.cc"
    break;

  case 308:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10045 "Parser/parser.cc"
    break;

  case 309:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10051 "Parser/parser.cc"
    break;

  case 310:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10057 "Parser/parser.cc"
    break;

  case 311:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10063 "Parser/parser.cc"
    break;

  case 312:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10069 "Parser/parser.cc"
    break;

  case 314:
#line 1592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10075 "Parser/parser.cc"
    break;

  case 315:
#line 1594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10081 "Parser/parser.cc"
    break;

  case 316:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10087 "Parser/parser.cc"
    break;

  case 317:
#line 1603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10093 "Parser/parser.cc"
    break;

  case 318:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10099 "Parser/parser.cc"
    break;

  case 319:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10105 "Parser/parser.cc"
    break;

  case 320:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10111 "Parser/parser.cc"
    break;

  case 321:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10117 "Parser/parser.cc"
    break;

  case 322:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10123 "Parser/parser.cc"
    break;

  case 323:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10129 "Parser/parser.cc"
    break;

  case 324:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10135 "Parser/parser.cc"
    break;

  case 325:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10141 "Parser/parser.cc"
    break;

  case 326:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10147 "Parser/parser.cc"
    break;

  case 327:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10153 "Parser/parser.cc"
    break;

  case 328:
#line 1632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10159 "Parser/parser.cc"
    break;

  case 329:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10165 "Parser/parser.cc"
    break;

  case 330:
#line 1636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10171 "Parser/parser.cc"
    break;

  case 331:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10177 "Parser/parser.cc"
    break;

  case 332:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10183 "Parser/parser.cc"
    break;

  case 333:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10189 "Parser/parser.cc"
    break;

  case 334:
#line 1644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10195 "Parser/parser.cc"
    break;

  case 335:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10201 "Parser/parser.cc"
    break;

  case 338:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10207 "Parser/parser.cc"
    break;

  case 339:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10216 "Parser/parser.cc"
    break;

  case 340:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10222 "Parser/parser.cc"
    break;

  case 341:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10228 "Parser/parser.cc"
    break;

  case 344:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10234 "Parser/parser.cc"
    break;

  case 345:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10240 "Parser/parser.cc"
    break;

  case 348:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10246 "Parser/parser.cc"
    break;

  case 349:
#line 1697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) )); }
#line 10252 "Parser/parser.cc"
    break;

  case 350:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10258 "Parser/parser.cc"
    break;

  case 351:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10264 "Parser/parser.cc"
    break;

  case 352:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10270 "Parser/parser.cc"
    break;

  case 353:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10276 "Parser/parser.cc"
    break;

  case 354:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10282 "Parser/parser.cc"
    break;

  case 355:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10288 "Parser/parser.cc"
    break;

  case 356:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10294 "Parser/parser.cc"
    break;

  case 359:
#line 1729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10300 "Parser/parser.cc"
    break;

  case 360:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10306 "Parser/parser.cc"
    break;

  case 361:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10312 "Parser/parser.cc"
    break;

  case 362:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10318 "Parser/parser.cc"
    break;

  case 363:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10324 "Parser/parser.cc"
    break;

  case 364:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10330 "Parser/parser.cc"
    break;

  case 365:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10336 "Parser/parser.cc"
    break;

  case 366:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10342 "Parser/parser.cc"
    break;

  case 367:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10348 "Parser/parser.cc"
    break;

  case 368:
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10354 "Parser/parser.cc"
    break;

  case 369:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10360 "Parser/parser.cc"
    break;

  case 370:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10366 "Parser/parser.cc"
    break;

  case 371:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10372 "Parser/parser.cc"
    break;

  case 372:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10378 "Parser/parser.cc"
    break;

  case 373:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10384 "Parser/parser.cc"
    break;

  case 374:
#line 1783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10390 "Parser/parser.cc"
    break;

  case 375:
#line 1788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10396 "Parser/parser.cc"
    break;

  case 376:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10402 "Parser/parser.cc"
    break;

  case 377:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10408 "Parser/parser.cc"
    break;

  case 378:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10414 "Parser/parser.cc"
    break;

  case 379:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10420 "Parser/parser.cc"
    break;

  case 380:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10426 "Parser/parser.cc"
    break;

  case 381:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10432 "Parser/parser.cc"
    break;

  case 383:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 384:
#line 1809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10444 "Parser/parser.cc"
    break;

  case 385:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 390:
#line 1826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10456 "Parser/parser.cc"
    break;

  case 391:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10462 "Parser/parser.cc"
    break;

  case 392:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 393:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10474 "Parser/parser.cc"
    break;

  case 394:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10480 "Parser/parser.cc"
    break;

  case 395:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10486 "Parser/parser.cc"
    break;

  case 396:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10492 "Parser/parser.cc"
    break;

  case 397:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10498 "Parser/parser.cc"
    break;

  case 400:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 10504 "Parser/parser.cc"
    break;

  case 401:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 402:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10519 "Parser/parser.cc"
    break;

  case 403:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10525 "Parser/parser.cc"
    break;

  case 404:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10531 "Parser/parser.cc"
    break;

  case 405:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ) ); }
#line 10537 "Parser/parser.cc"
    break;

  case 406:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10546 "Parser/parser.cc"
    break;

  case 407:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10555 "Parser/parser.cc"
    break;

  case 408:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10561 "Parser/parser.cc"
    break;

  case 411:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10567 "Parser/parser.cc"
    break;

  case 412:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10573 "Parser/parser.cc"
    break;

  case 414:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10579 "Parser/parser.cc"
    break;

  case 415:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10585 "Parser/parser.cc"
    break;

  case 425:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10591 "Parser/parser.cc"
    break;

  case 426:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10597 "Parser/parser.cc"
    break;

  case 430:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10603 "Parser/parser.cc"
    break;

  case 432:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10609 "Parser/parser.cc"
    break;

  case 433:
#line 1968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10615 "Parser/parser.cc"
    break;

  case 434:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10621 "Parser/parser.cc"
    break;

  case 435:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10627 "Parser/parser.cc"
    break;

  case 436:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10633 "Parser/parser.cc"
    break;

  case 437:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10639 "Parser/parser.cc"
    break;

  case 439:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10645 "Parser/parser.cc"
    break;

  case 440:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10651 "Parser/parser.cc"
    break;

  case 441:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10657 "Parser/parser.cc"
    break;

  case 442:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10668 "Parser/parser.cc"
    break;

  case 443:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10674 "Parser/parser.cc"
    break;

  case 444:
#line 2005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10680 "Parser/parser.cc"
    break;

  case 445:
#line 2018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10686 "Parser/parser.cc"
    break;

  case 446:
#line 2020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10692 "Parser/parser.cc"
    break;

  case 447:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10698 "Parser/parser.cc"
    break;

  case 448:
#line 2028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10704 "Parser/parser.cc"
    break;

  case 449:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10713 "Parser/parser.cc"
    break;

  case 450:
#line 2038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10722 "Parser/parser.cc"
    break;

  case 451:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10731 "Parser/parser.cc"
    break;

  case 452:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10742 "Parser/parser.cc"
    break;

  case 453:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10751 "Parser/parser.cc"
    break;

  case 454:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10757 "Parser/parser.cc"
    break;

  case 455:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10763 "Parser/parser.cc"
    break;

  case 456:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10769 "Parser/parser.cc"
    break;

  case 457:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10777 "Parser/parser.cc"
    break;

  case 458:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10785 "Parser/parser.cc"
    break;

  case 459:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10791 "Parser/parser.cc"
    break;

  case 462:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10806 "Parser/parser.cc"
    break;

  case 463:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 464:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 465:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10824 "Parser/parser.cc"
    break;

  case 466:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 467:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 473:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   (yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10846 "Parser/parser.cc"
    break;

  case 486:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10852 "Parser/parser.cc"
    break;

  case 489:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10858 "Parser/parser.cc"
    break;

  case 492:
#line 2195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Const ); }
#line 10864 "Parser/parser.cc"
    break;

  case 493:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Restrict ); }
#line 10870 "Parser/parser.cc"
    break;

  case 494:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Volatile ); }
#line 10876 "Parser/parser.cc"
    break;

  case 495:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( ast::CV::Atomic ); }
#line 10882 "Parser/parser.cc"
    break;

  case 496:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10888 "Parser/parser.cc"
    break;

  case 497:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10894 "Parser/parser.cc"
    break;

  case 499:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10900 "Parser/parser.cc"
    break;

  case 500:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10906 "Parser/parser.cc"
    break;

  case 502:
#line 2227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10912 "Parser/parser.cc"
    break;

  case 503:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10918 "Parser/parser.cc"
    break;

  case 504:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10924 "Parser/parser.cc"
    break;

  case 505:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10930 "Parser/parser.cc"
    break;

  case 506:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 10936 "Parser/parser.cc"
    break;

  case 507:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 10942 "Parser/parser.cc"
    break;

  case 508:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 10948 "Parser/parser.cc"
    break;

  case 509:
#line 2245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 10954 "Parser/parser.cc"
    break;

  case 510:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 10960 "Parser/parser.cc"
    break;

  case 511:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 10966 "Parser/parser.cc"
    break;

  case 512:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10972 "Parser/parser.cc"
    break;

  case 513:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10978 "Parser/parser.cc"
    break;

  case 514:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10984 "Parser/parser.cc"
    break;

  case 515:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10990 "Parser/parser.cc"
    break;

  case 516:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10996 "Parser/parser.cc"
    break;

  case 517:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 11002 "Parser/parser.cc"
    break;

  case 518:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 11008 "Parser/parser.cc"
    break;

  case 519:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 11014 "Parser/parser.cc"
    break;

  case 520:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 11020 "Parser/parser.cc"
    break;

  case 521:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 11026 "Parser/parser.cc"
    break;

  case 522:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 11032 "Parser/parser.cc"
    break;

  case 523:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 11038 "Parser/parser.cc"
    break;

  case 524:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 11044 "Parser/parser.cc"
    break;

  case 525:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 11050 "Parser/parser.cc"
    break;

  case 526:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 11056 "Parser/parser.cc"
    break;

  case 527:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 11062 "Parser/parser.cc"
    break;

  case 528:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11068 "Parser/parser.cc"
    break;

  case 529:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11074 "Parser/parser.cc"
    break;

  case 530:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11080 "Parser/parser.cc"
    break;

  case 531:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 11086 "Parser/parser.cc"
    break;

  case 532:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 11092 "Parser/parser.cc"
    break;

  case 533:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 11098 "Parser/parser.cc"
    break;

  case 534:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 11104 "Parser/parser.cc"
    break;

  case 535:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 11110 "Parser/parser.cc"
    break;

  case 536:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 11116 "Parser/parser.cc"
    break;

  case 537:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 11122 "Parser/parser.cc"
    break;

  case 538:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 11128 "Parser/parser.cc"
    break;

  case 540:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11134 "Parser/parser.cc"
    break;

  case 542:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 11140 "Parser/parser.cc"
    break;

  case 543:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11146 "Parser/parser.cc"
    break;

  case 544:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11152 "Parser/parser.cc"
    break;

  case 546:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11158 "Parser/parser.cc"
    break;

  case 547:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11164 "Parser/parser.cc"
    break;

  case 548:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11170 "Parser/parser.cc"
    break;

  case 549:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11176 "Parser/parser.cc"
    break;

  case 551:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11182 "Parser/parser.cc"
    break;

  case 553:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11188 "Parser/parser.cc"
    break;

  case 554:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11194 "Parser/parser.cc"
    break;

  case 555:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11200 "Parser/parser.cc"
    break;

  case 556:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11206 "Parser/parser.cc"
    break;

  case 557:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11212 "Parser/parser.cc"
    break;

  case 558:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11218 "Parser/parser.cc"
    break;

  case 559:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11224 "Parser/parser.cc"
    break;

  case 560:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 11230 "Parser/parser.cc"
    break;

  case 561:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 11236 "Parser/parser.cc"
    break;

  case 563:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11242 "Parser/parser.cc"
    break;

  case 564:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11248 "Parser/parser.cc"
    break;

  case 565:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11254 "Parser/parser.cc"
    break;

  case 567:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11260 "Parser/parser.cc"
    break;

  case 568:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 569:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11275 "Parser/parser.cc"
    break;

  case 571:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 572:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11287 "Parser/parser.cc"
    break;

  case 573:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11293 "Parser/parser.cc"
    break;

  case 575:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11299 "Parser/parser.cc"
    break;

  case 576:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11305 "Parser/parser.cc"
    break;

  case 578:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11311 "Parser/parser.cc"
    break;

  case 579:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11317 "Parser/parser.cc"
    break;

  case 580:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11323 "Parser/parser.cc"
    break;

  case 582:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11329 "Parser/parser.cc"
    break;

  case 583:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 584:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 11341 "Parser/parser.cc"
    break;

  case 585:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 11347 "Parser/parser.cc"
    break;

  case 586:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 11353 "Parser/parser.cc"
    break;

  case 588:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 589:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 590:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 11371 "Parser/parser.cc"
    break;

  case 591:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 11377 "Parser/parser.cc"
    break;

  case 592:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11383 "Parser/parser.cc"
    break;

  case 597:
#line 2468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11389 "Parser/parser.cc"
    break;

  case 598:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11395 "Parser/parser.cc"
    break;

  case 599:
#line 2472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11404 "Parser/parser.cc"
    break;

  case 600:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11412 "Parser/parser.cc"
    break;

  case 601:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11421 "Parser/parser.cc"
    break;

  case 602:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11430 "Parser/parser.cc"
    break;

  case 603:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11439 "Parser/parser.cc"
    break;

  case 604:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11448 "Parser/parser.cc"
    break;

  case 606:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11454 "Parser/parser.cc"
    break;

  case 607:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11460 "Parser/parser.cc"
    break;

  case 608:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11470 "Parser/parser.cc"
    break;

  case 609:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11489 "Parser/parser.cc"
    break;

  case 612:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11495 "Parser/parser.cc"
    break;

  case 613:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11501 "Parser/parser.cc"
    break;

  case 614:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11507 "Parser/parser.cc"
    break;

  case 615:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11513 "Parser/parser.cc"
    break;

  case 616:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11519 "Parser/parser.cc"
    break;

  case 617:
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11525 "Parser/parser.cc"
    break;

  case 618:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11534 "Parser/parser.cc"
    break;

  case 619:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11540 "Parser/parser.cc"
    break;

  case 620:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11549 "Parser/parser.cc"
    break;

  case 621:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11555 "Parser/parser.cc"
    break;

  case 622:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11564 "Parser/parser.cc"
    break;

  case 623:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11570 "Parser/parser.cc"
    break;

  case 624:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11576 "Parser/parser.cc"
    break;

  case 625:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11589 "Parser/parser.cc"
    break;

  case 626:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11598 "Parser/parser.cc"
    break;

  case 627:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11604 "Parser/parser.cc"
    break;

  case 628:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11610 "Parser/parser.cc"
    break;

  case 629:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11623 "Parser/parser.cc"
    break;

  case 630:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11629 "Parser/parser.cc"
    break;

  case 633:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11635 "Parser/parser.cc"
    break;

  case 634:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11641 "Parser/parser.cc"
    break;

  case 637:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11647 "Parser/parser.cc"
    break;

  case 639:
#line 2629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11653 "Parser/parser.cc"
    break;

  case 640:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11659 "Parser/parser.cc"
    break;

  case 641:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11665 "Parser/parser.cc"
    break;

  case 642:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11671 "Parser/parser.cc"
    break;

  case 643:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11677 "Parser/parser.cc"
    break;

  case 644:
#line 2648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11683 "Parser/parser.cc"
    break;

  case 646:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11689 "Parser/parser.cc"
    break;

  case 648:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11695 "Parser/parser.cc"
    break;

  case 649:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11701 "Parser/parser.cc"
    break;

  case 651:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11707 "Parser/parser.cc"
    break;

  case 652:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11713 "Parser/parser.cc"
    break;

  case 654:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11719 "Parser/parser.cc"
    break;

  case 655:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11725 "Parser/parser.cc"
    break;

  case 656:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11731 "Parser/parser.cc"
    break;

  case 657:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11737 "Parser/parser.cc"
    break;

  case 658:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11743 "Parser/parser.cc"
    break;

  case 659:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11749 "Parser/parser.cc"
    break;

  case 660:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11760 "Parser/parser.cc"
    break;

  case 661:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11766 "Parser/parser.cc"
    break;

  case 662:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11774 "Parser/parser.cc"
    break;

  case 663:
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr; }
#line 11780 "Parser/parser.cc"
    break;

  case 664:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl) && ((yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11791 "Parser/parser.cc"
    break;

  case 665:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11799 "Parser/parser.cc"
    break;

  case 666:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11807 "Parser/parser.cc"
    break;

  case 667:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11815 "Parser/parser.cc"
    break;

  case 668:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11823 "Parser/parser.cc"
    break;

  case 670:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11829 "Parser/parser.cc"
    break;

  case 671:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11835 "Parser/parser.cc"
    break;

  case 672:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11844 "Parser/parser.cc"
    break;

  case 673:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11853 "Parser/parser.cc"
    break;

  case 674:
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11859 "Parser/parser.cc"
    break;

  case 675:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11865 "Parser/parser.cc"
    break;

  case 676:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11871 "Parser/parser.cc"
    break;

  case 677:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11877 "Parser/parser.cc"
    break;

  case 679:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11883 "Parser/parser.cc"
    break;

  case 680:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11889 "Parser/parser.cc"
    break;

  case 681:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11895 "Parser/parser.cc"
    break;

  case 682:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11901 "Parser/parser.cc"
    break;

  case 683:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11907 "Parser/parser.cc"
    break;

  case 684:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11913 "Parser/parser.cc"
    break;

  case 686:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11919 "Parser/parser.cc"
    break;

  case 689:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11925 "Parser/parser.cc"
    break;

  case 690:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11931 "Parser/parser.cc"
    break;

  case 691:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11937 "Parser/parser.cc"
    break;

  case 692:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11943 "Parser/parser.cc"
    break;

  case 695:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11949 "Parser/parser.cc"
    break;

  case 696:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11955 "Parser/parser.cc"
    break;

  case 697:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11961 "Parser/parser.cc"
    break;

  case 699:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11967 "Parser/parser.cc"
    break;

  case 700:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11973 "Parser/parser.cc"
    break;

  case 701:
#line 2834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 11979 "Parser/parser.cc"
    break;

  case 703:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11985 "Parser/parser.cc"
    break;

  case 704:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11991 "Parser/parser.cc"
    break;

  case 705:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11997 "Parser/parser.cc"
    break;

  case 706:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12003 "Parser/parser.cc"
    break;

  case 707:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12009 "Parser/parser.cc"
    break;

  case 709:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12015 "Parser/parser.cc"
    break;

  case 710:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12021 "Parser/parser.cc"
    break;

  case 711:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12027 "Parser/parser.cc"
    break;

  case 716:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12033 "Parser/parser.cc"
    break;

  case 718:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12039 "Parser/parser.cc"
    break;

  case 719:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12045 "Parser/parser.cc"
    break;

  case 725:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12051 "Parser/parser.cc"
    break;

  case 728:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12057 "Parser/parser.cc"
    break;

  case 729:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12063 "Parser/parser.cc"
    break;

  case 730:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12069 "Parser/parser.cc"
    break;

  case 731:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12075 "Parser/parser.cc"
    break;

  case 732:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12081 "Parser/parser.cc"
    break;

  case 733:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12087 "Parser/parser.cc"
    break;

  case 734:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12093 "Parser/parser.cc"
    break;

  case 736:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12099 "Parser/parser.cc"
    break;

  case 737:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (InitializerNode *)( (yyvsp[-2].init)->set_last( (yyvsp[0].init) ) ); }
#line 12105 "Parser/parser.cc"
    break;

  case 738:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (InitializerNode *)((yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) )); }
#line 12111 "Parser/parser.cc"
    break;

  case 740:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12117 "Parser/parser.cc"
    break;

  case 742:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-1].expr)->set_last( (yyvsp[0].expr) )); }
#line 12123 "Parser/parser.cc"
    break;

  case 743:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12129 "Parser/parser.cc"
    break;

  case 744:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12135 "Parser/parser.cc"
    break;

  case 745:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12141 "Parser/parser.cc"
    break;

  case 746:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12147 "Parser/parser.cc"
    break;

  case 747:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12153 "Parser/parser.cc"
    break;

  case 749:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12159 "Parser/parser.cc"
    break;

  case 750:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12165 "Parser/parser.cc"
    break;

  case 751:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12171 "Parser/parser.cc"
    break;

  case 752:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12182 "Parser/parser.cc"
    break;

  case 753:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12188 "Parser/parser.cc"
    break;

  case 754:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12194 "Parser/parser.cc"
    break;

  case 755:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12200 "Parser/parser.cc"
    break;

  case 756:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12209 "Parser/parser.cc"
    break;

  case 757:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12215 "Parser/parser.cc"
    break;

  case 758:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12225 "Parser/parser.cc"
    break;

  case 759:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12231 "Parser/parser.cc"
    break;

  case 760:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12237 "Parser/parser.cc"
    break;

  case 761:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12243 "Parser/parser.cc"
    break;

  case 762:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12249 "Parser/parser.cc"
    break;

  case 763:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12255 "Parser/parser.cc"
    break;

  case 764:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12261 "Parser/parser.cc"
    break;

  case 765:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12267 "Parser/parser.cc"
    break;

  case 766:
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12273 "Parser/parser.cc"
    break;

  case 767:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12279 "Parser/parser.cc"
    break;

  case 770:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12285 "Parser/parser.cc"
    break;

  case 771:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12291 "Parser/parser.cc"
    break;

  case 772:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12297 "Parser/parser.cc"
    break;

  case 773:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12303 "Parser/parser.cc"
    break;

  case 775:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12309 "Parser/parser.cc"
    break;

  case 776:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 12315 "Parser/parser.cc"
    break;

  case 777:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12321 "Parser/parser.cc"
    break;

  case 778:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12327 "Parser/parser.cc"
    break;

  case 779:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12333 "Parser/parser.cc"
    break;

  case 780:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12339 "Parser/parser.cc"
    break;

  case 781:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12345 "Parser/parser.cc"
    break;

  case 782:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12354 "Parser/parser.cc"
    break;

  case 783:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12363 "Parser/parser.cc"
    break;

  case 784:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12372 "Parser/parser.cc"
    break;

  case 785:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12378 "Parser/parser.cc"
    break;

  case 786:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12387 "Parser/parser.cc"
    break;

  case 787:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12393 "Parser/parser.cc"
    break;

  case 789:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12399 "Parser/parser.cc"
    break;

  case 794:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12405 "Parser/parser.cc"
    break;

  case 795:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12411 "Parser/parser.cc"
    break;

  case 796:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12417 "Parser/parser.cc"
    break;

  case 798:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 12423 "Parser/parser.cc"
    break;

  case 799:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12429 "Parser/parser.cc"
    break;

  case 800:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12435 "Parser/parser.cc"
    break;

  case 801:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12441 "Parser/parser.cc"
    break;

  case 803:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12447 "Parser/parser.cc"
    break;

  case 804:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12453 "Parser/parser.cc"
    break;

  case 805:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12459 "Parser/parser.cc"
    break;

  case 806:
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
#line 12477 "Parser/parser.cc"
    break;

  case 807:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12483 "Parser/parser.cc"
    break;

  case 808:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12489 "Parser/parser.cc"
    break;

  case 809:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12495 "Parser/parser.cc"
    break;

  case 810:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12501 "Parser/parser.cc"
    break;

  case 811:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12507 "Parser/parser.cc"
    break;

  case 812:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12513 "Parser/parser.cc"
    break;

  case 814:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12522 "Parser/parser.cc"
    break;

  case 815:
#line 3223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12528 "Parser/parser.cc"
    break;

  case 816:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12537 "Parser/parser.cc"
    break;

  case 817:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12547 "Parser/parser.cc"
    break;

  case 818:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12556 "Parser/parser.cc"
    break;

  case 819:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12566 "Parser/parser.cc"
    break;

  case 820:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12577 "Parser/parser.cc"
    break;

  case 821:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12587 "Parser/parser.cc"
    break;

  case 822:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12598 "Parser/parser.cc"
    break;

  case 823:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12608 "Parser/parser.cc"
    break;

  case 824:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12619 "Parser/parser.cc"
    break;

  case 825:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12629 "Parser/parser.cc"
    break;

  case 827:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12635 "Parser/parser.cc"
    break;

  case 828:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12641 "Parser/parser.cc"
    break;

  case 829:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12647 "Parser/parser.cc"
    break;

  case 830:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12659 "Parser/parser.cc"
    break;

  case 831:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12670 "Parser/parser.cc"
    break;

  case 832:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12679 "Parser/parser.cc"
    break;

  case 833:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12688 "Parser/parser.cc"
    break;

  case 834:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12694 "Parser/parser.cc"
    break;

  case 835:
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12700 "Parser/parser.cc"
    break;

  case 836:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12706 "Parser/parser.cc"
    break;

  case 837:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12715 "Parser/parser.cc"
    break;

  case 838:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12721 "Parser/parser.cc"
    break;

  case 839:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12727 "Parser/parser.cc"
    break;

  case 840:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12733 "Parser/parser.cc"
    break;

  case 845:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12739 "Parser/parser.cc"
    break;

  case 846:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12745 "Parser/parser.cc"
    break;

  case 847:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12755 "Parser/parser.cc"
    break;

  case 848:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12761 "Parser/parser.cc"
    break;

  case 851:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12767 "Parser/parser.cc"
    break;

  case 852:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12773 "Parser/parser.cc"
    break;

  case 853:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12779 "Parser/parser.cc"
    break;

  case 854:
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12785 "Parser/parser.cc"
    break;

  case 856:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12791 "Parser/parser.cc"
    break;

  case 857:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12797 "Parser/parser.cc"
    break;

  case 858:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12803 "Parser/parser.cc"
    break;

  case 859:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12809 "Parser/parser.cc"
    break;

  case 864:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12815 "Parser/parser.cc"
    break;

  case 865:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12821 "Parser/parser.cc"
    break;

  case 866:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12827 "Parser/parser.cc"
    break;

  case 867:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12833 "Parser/parser.cc"
    break;

  case 868:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12839 "Parser/parser.cc"
    break;

  case 870:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12845 "Parser/parser.cc"
    break;

  case 871:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12851 "Parser/parser.cc"
    break;

  case 872:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12857 "Parser/parser.cc"
    break;

  case 873:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12863 "Parser/parser.cc"
    break;

  case 874:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12869 "Parser/parser.cc"
    break;

  case 875:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12875 "Parser/parser.cc"
    break;

  case 876:
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12881 "Parser/parser.cc"
    break;

  case 877:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12887 "Parser/parser.cc"
    break;

  case 878:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12893 "Parser/parser.cc"
    break;

  case 879:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12899 "Parser/parser.cc"
    break;

  case 880:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12905 "Parser/parser.cc"
    break;

  case 881:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12911 "Parser/parser.cc"
    break;

  case 882:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12917 "Parser/parser.cc"
    break;

  case 883:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12923 "Parser/parser.cc"
    break;

  case 884:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12929 "Parser/parser.cc"
    break;

  case 885:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12935 "Parser/parser.cc"
    break;

  case 886:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12941 "Parser/parser.cc"
    break;

  case 887:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12947 "Parser/parser.cc"
    break;

  case 889:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12953 "Parser/parser.cc"
    break;

  case 890:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12959 "Parser/parser.cc"
    break;

  case 891:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12965 "Parser/parser.cc"
    break;

  case 892:
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12971 "Parser/parser.cc"
    break;

  case 893:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12977 "Parser/parser.cc"
    break;

  case 894:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12983 "Parser/parser.cc"
    break;

  case 895:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12989 "Parser/parser.cc"
    break;

  case 896:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12995 "Parser/parser.cc"
    break;

  case 897:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13001 "Parser/parser.cc"
    break;

  case 898:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13007 "Parser/parser.cc"
    break;

  case 899:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13013 "Parser/parser.cc"
    break;

  case 900:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13019 "Parser/parser.cc"
    break;

  case 901:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13025 "Parser/parser.cc"
    break;

  case 902:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13031 "Parser/parser.cc"
    break;

  case 903:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13037 "Parser/parser.cc"
    break;

  case 904:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13043 "Parser/parser.cc"
    break;

  case 908:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13049 "Parser/parser.cc"
    break;

  case 909:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13055 "Parser/parser.cc"
    break;

  case 910:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13061 "Parser/parser.cc"
    break;

  case 911:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13067 "Parser/parser.cc"
    break;

  case 912:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13073 "Parser/parser.cc"
    break;

  case 913:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13079 "Parser/parser.cc"
    break;

  case 914:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13085 "Parser/parser.cc"
    break;

  case 915:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13091 "Parser/parser.cc"
    break;

  case 916:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13097 "Parser/parser.cc"
    break;

  case 917:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13103 "Parser/parser.cc"
    break;

  case 918:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13109 "Parser/parser.cc"
    break;

  case 919:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13115 "Parser/parser.cc"
    break;

  case 920:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13121 "Parser/parser.cc"
    break;

  case 921:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13127 "Parser/parser.cc"
    break;

  case 922:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13133 "Parser/parser.cc"
    break;

  case 923:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13142 "Parser/parser.cc"
    break;

  case 924:
#line 3632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13148 "Parser/parser.cc"
    break;

  case 925:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13154 "Parser/parser.cc"
    break;

  case 927:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13160 "Parser/parser.cc"
    break;

  case 928:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13166 "Parser/parser.cc"
    break;

  case 929:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13172 "Parser/parser.cc"
    break;

  case 930:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13178 "Parser/parser.cc"
    break;

  case 931:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13184 "Parser/parser.cc"
    break;

  case 932:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13190 "Parser/parser.cc"
    break;

  case 933:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13196 "Parser/parser.cc"
    break;

  case 934:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13202 "Parser/parser.cc"
    break;

  case 935:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13208 "Parser/parser.cc"
    break;

  case 936:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13214 "Parser/parser.cc"
    break;

  case 937:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13220 "Parser/parser.cc"
    break;

  case 938:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13226 "Parser/parser.cc"
    break;

  case 939:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13232 "Parser/parser.cc"
    break;

  case 940:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13238 "Parser/parser.cc"
    break;

  case 941:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13244 "Parser/parser.cc"
    break;

  case 942:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13250 "Parser/parser.cc"
    break;

  case 943:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13256 "Parser/parser.cc"
    break;

  case 944:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13262 "Parser/parser.cc"
    break;

  case 946:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13268 "Parser/parser.cc"
    break;

  case 947:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13274 "Parser/parser.cc"
    break;

  case 948:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13280 "Parser/parser.cc"
    break;

  case 949:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13286 "Parser/parser.cc"
    break;

  case 950:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13292 "Parser/parser.cc"
    break;

  case 951:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13298 "Parser/parser.cc"
    break;

  case 952:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13304 "Parser/parser.cc"
    break;

  case 953:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13310 "Parser/parser.cc"
    break;

  case 954:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13316 "Parser/parser.cc"
    break;

  case 955:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13322 "Parser/parser.cc"
    break;

  case 956:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13328 "Parser/parser.cc"
    break;

  case 957:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13334 "Parser/parser.cc"
    break;

  case 958:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13340 "Parser/parser.cc"
    break;

  case 959:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13346 "Parser/parser.cc"
    break;

  case 960:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13352 "Parser/parser.cc"
    break;

  case 961:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13358 "Parser/parser.cc"
    break;

  case 962:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13364 "Parser/parser.cc"
    break;

  case 963:
#line 3744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( ast::CV::Mutex ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13371 "Parser/parser.cc"
    break;

  case 965:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13377 "Parser/parser.cc"
    break;

  case 966:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13383 "Parser/parser.cc"
    break;

  case 967:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13389 "Parser/parser.cc"
    break;

  case 968:
#line 3757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13395 "Parser/parser.cc"
    break;

  case 969:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13401 "Parser/parser.cc"
    break;

  case 970:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13407 "Parser/parser.cc"
    break;

  case 971:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13413 "Parser/parser.cc"
    break;

  case 972:
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13419 "Parser/parser.cc"
    break;

  case 973:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13425 "Parser/parser.cc"
    break;

  case 974:
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13431 "Parser/parser.cc"
    break;

  case 975:
#line 3777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13437 "Parser/parser.cc"
    break;

  case 976:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13443 "Parser/parser.cc"
    break;

  case 977:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13449 "Parser/parser.cc"
    break;

  case 978:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( ast::CV::Mutex ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13456 "Parser/parser.cc"
    break;

  case 980:
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13462 "Parser/parser.cc"
    break;

  case 981:
#line 3801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13468 "Parser/parser.cc"
    break;

  case 982:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13474 "Parser/parser.cc"
    break;

  case 983:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13480 "Parser/parser.cc"
    break;

  case 984:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13486 "Parser/parser.cc"
    break;

  case 985:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13492 "Parser/parser.cc"
    break;

  case 986:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13498 "Parser/parser.cc"
    break;

  case 987:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13504 "Parser/parser.cc"
    break;

  case 988:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13510 "Parser/parser.cc"
    break;

  case 989:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13516 "Parser/parser.cc"
    break;

  case 990:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13522 "Parser/parser.cc"
    break;

  case 992:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13528 "Parser/parser.cc"
    break;

  case 993:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13534 "Parser/parser.cc"
    break;

  case 994:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13540 "Parser/parser.cc"
    break;

  case 995:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13546 "Parser/parser.cc"
    break;

  case 996:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13552 "Parser/parser.cc"
    break;

  case 997:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13558 "Parser/parser.cc"
    break;

  case 998:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13564 "Parser/parser.cc"
    break;

  case 1000:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13570 "Parser/parser.cc"
    break;

  case 1001:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13576 "Parser/parser.cc"
    break;

  case 1002:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13582 "Parser/parser.cc"
    break;

  case 1003:
#line 3879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13588 "Parser/parser.cc"
    break;

  case 1004:
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13594 "Parser/parser.cc"
    break;

  case 1005:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13600 "Parser/parser.cc"
    break;

  case 1006:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13606 "Parser/parser.cc"
    break;

  case 1007:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13612 "Parser/parser.cc"
    break;

  case 1008:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13618 "Parser/parser.cc"
    break;

  case 1009:
#line 3901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13624 "Parser/parser.cc"
    break;

  case 1011:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13630 "Parser/parser.cc"
    break;

  case 1012:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13636 "Parser/parser.cc"
    break;

  case 1014:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13642 "Parser/parser.cc"
    break;

  case 1015:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13648 "Parser/parser.cc"
    break;

  case 1017:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13654 "Parser/parser.cc"
    break;

  case 1018:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13660 "Parser/parser.cc"
    break;

  case 1019:
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13666 "Parser/parser.cc"
    break;

  case 1020:
#line 3934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13672 "Parser/parser.cc"
    break;

  case 1021:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13678 "Parser/parser.cc"
    break;

  case 1022:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13684 "Parser/parser.cc"
    break;

  case 1023:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13690 "Parser/parser.cc"
    break;

  case 1026:
#line 3979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( ast::CV::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13696 "Parser/parser.cc"
    break;

  case 1027:
#line 3981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13702 "Parser/parser.cc"
    break;

  case 1028:
#line 3983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13708 "Parser/parser.cc"
    break;

  case 1029:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13714 "Parser/parser.cc"
    break;

  case 1030:
#line 3990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13720 "Parser/parser.cc"
    break;

  case 1031:
#line 3992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13726 "Parser/parser.cc"
    break;

  case 1032:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13732 "Parser/parser.cc"
    break;

  case 1033:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13738 "Parser/parser.cc"
    break;

  case 1035:
#line 4002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13744 "Parser/parser.cc"
    break;

  case 1036:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13750 "Parser/parser.cc"
    break;

  case 1037:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13756 "Parser/parser.cc"
    break;

  case 1038:
#line 4011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13762 "Parser/parser.cc"
    break;

  case 1039:
#line 4013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13768 "Parser/parser.cc"
    break;

  case 1040:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13774 "Parser/parser.cc"
    break;

  case 1042:
#line 4022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13780 "Parser/parser.cc"
    break;

  case 1044:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13786 "Parser/parser.cc"
    break;

  case 1045:
#line 4036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13792 "Parser/parser.cc"
    break;

  case 1046:
#line 4038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13798 "Parser/parser.cc"
    break;

  case 1047:
#line 4041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13804 "Parser/parser.cc"
    break;

  case 1048:
#line 4043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13810 "Parser/parser.cc"
    break;

  case 1049:
#line 4045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13816 "Parser/parser.cc"
    break;

  case 1051:
#line 4060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13822 "Parser/parser.cc"
    break;

  case 1052:
#line 4062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13828 "Parser/parser.cc"
    break;

  case 1053:
#line 4067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13834 "Parser/parser.cc"
    break;

  case 1054:
#line 4069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13840 "Parser/parser.cc"
    break;

  case 1055:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13846 "Parser/parser.cc"
    break;

  case 1056:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13852 "Parser/parser.cc"
    break;

  case 1057:
#line 4075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13858 "Parser/parser.cc"
    break;

  case 1059:
#line 4081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13864 "Parser/parser.cc"
    break;

  case 1060:
#line 4083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13870 "Parser/parser.cc"
    break;

  case 1061:
#line 4085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13876 "Parser/parser.cc"
    break;

  case 1062:
#line 4090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13882 "Parser/parser.cc"
    break;

  case 1063:
#line 4092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13888 "Parser/parser.cc"
    break;

  case 1066:
#line 4102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13894 "Parser/parser.cc"
    break;

  case 1069:
#line 4113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13900 "Parser/parser.cc"
    break;

  case 1070:
#line 4115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13906 "Parser/parser.cc"
    break;

  case 1071:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13912 "Parser/parser.cc"
    break;

  case 1072:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13918 "Parser/parser.cc"
    break;

  case 1073:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13924 "Parser/parser.cc"
    break;

  case 1074:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13930 "Parser/parser.cc"
    break;

  case 1075:
#line 4130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13936 "Parser/parser.cc"
    break;

  case 1076:
#line 4132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13942 "Parser/parser.cc"
    break;

  case 1077:
#line 4134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13948 "Parser/parser.cc"
    break;

  case 1078:
#line 4136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13954 "Parser/parser.cc"
    break;

  case 1079:
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13960 "Parser/parser.cc"
    break;

  case 1080:
#line 4141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13966 "Parser/parser.cc"
    break;

  case 1081:
#line 4143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13972 "Parser/parser.cc"
    break;

  case 1082:
#line 4145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13978 "Parser/parser.cc"
    break;

  case 1083:
#line 4147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13984 "Parser/parser.cc"
    break;

  case 1084:
#line 4149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13990 "Parser/parser.cc"
    break;

  case 1085:
#line 4154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13996 "Parser/parser.cc"
    break;

  case 1086:
#line 4156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14002 "Parser/parser.cc"
    break;

  case 1087:
#line 4161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14008 "Parser/parser.cc"
    break;

  case 1088:
#line 4163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14014 "Parser/parser.cc"
    break;

  case 1090:
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14020 "Parser/parser.cc"
    break;

  case 1094:
#line 4201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14026 "Parser/parser.cc"
    break;

  case 1095:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14032 "Parser/parser.cc"
    break;

  case 1096:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14038 "Parser/parser.cc"
    break;

  case 1097:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14044 "Parser/parser.cc"
    break;

  case 1098:
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14050 "Parser/parser.cc"
    break;

  case 1099:
#line 4211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14056 "Parser/parser.cc"
    break;

  case 1100:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14062 "Parser/parser.cc"
    break;

  case 1101:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14068 "Parser/parser.cc"
    break;

  case 1102:
#line 4222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14074 "Parser/parser.cc"
    break;

  case 1103:
#line 4224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14080 "Parser/parser.cc"
    break;

  case 1104:
#line 4226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14086 "Parser/parser.cc"
    break;

  case 1105:
#line 4228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14092 "Parser/parser.cc"
    break;

  case 1106:
#line 4233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14098 "Parser/parser.cc"
    break;

  case 1107:
#line 4235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14104 "Parser/parser.cc"
    break;

  case 1108:
#line 4237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14110 "Parser/parser.cc"
    break;

  case 1109:
#line 4242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14116 "Parser/parser.cc"
    break;

  case 1110:
#line 4244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14122 "Parser/parser.cc"
    break;

  case 1111:
#line 4246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14128 "Parser/parser.cc"
    break;

  case 1114:
#line 4270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14134 "Parser/parser.cc"
    break;

  case 1115:
#line 4272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14140 "Parser/parser.cc"
    break;


#line 14144 "Parser/parser.cc"

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
#line 4275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
