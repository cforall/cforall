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
#define YYLAST   26242

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  181
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  314
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1117
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

#define YYPACT_NINF (-1913)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1116)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     171, 13336,   216,   237, 19807,     9, -1913, -1913, -1913, -1913,
   -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913,    88,   969,
     109, -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913,
   -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913,
   -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913,   125,   317,
   -1913, -1913, -1913, -1913, -1913, -1913,  4240,  4240,   140, 13336,
     193,   244, 23423, -1913,   264, -1913, -1913, -1913, -1913, -1913,
   -1913, -1913, -1913, -1913, -1913,   298,  3131, -1913,   738,   348,
   -1913, -1913, -1913, -1913, -1913, 19342, -1913, -1913,   390,   387,
     497,   368, -1913,  4240,   435,   487,   509,   532,  4271,   773,
     856, 13501, -1913, -1913,   515, 19187,  2307, -1913, -1913, -1913,
   -1913,  3492,   797,  5651,  8270,  1159,  3492,  1394,   627, -1913,
   -1913, -1913, -1913,   279, -1913, -1913, -1913, -1913,   648, -1913,
   -1913, -1913, -1913, -1913,   657,   675,   279, -1913,   279, 17645,
   -1913, -1913, -1913, 21137,  4240, -1913, -1913,  4240, -1913, 13336,
   -1913,   687, 21190, -1913, -1913,  4457, 22489, -1913, -1913,  1460,
    1460,   717,  2928, -1913, -1913, -1913, -1913,   367, 15941,   279,
    3337,   279, -1913, -1913, -1913, -1913, -1913, -1913,   746, -1913,
     737,   790,  1312, -1913,   836, 25691, -1913, -1913, -1913, -1913,
   -1913, -1913, -1913, 18024,  2553,  2573,  3131,   454,   806,   809,
     816,   833,   838,   840, -1913, -1913, 19962, 12161,   843,   855,
   -1913, 20265, -1913, -1913, -1913, -1913,   860, -1913, -1913,   857,
   -1913, 23639,  1004, 23791, -1913,   869,  4240,   675,   877,  3163,
    4457,  3163, -1913, -1913, -1913,  2491,  4287,   883,   946,   120,
     946, -1913,   279,   279,    41, 17323,   288,   946, -1913,   279,
     279,    41,   279, -1913,   279, -1913,  5079, -1913, -1913,   895,
     898,  1460, 23233,   900, 19342, -1913, -1913,  3492, -1913,  2418,
     627,   906,   986, 17323,  4240,  4240,   497, -1913, 15121, -1913,
    1460,  1460,   935,   986, 17323,  4240, -1913,  6981, -1913, -1913,
   -1913,  1460, -1913, -1913, -1913, -1913,  1460, -1913,   904,  4470,
    4240, -1913, 19041,   933, -1913, -1913, -1913, 23096,   675, 17484,
     923,  4457, 18988, 23233,  3492, -1913, -1913, 22637, -1913,   946,
     107, -1913, 25691, 22489,  3524,  5079, -1913,   520, -1913, -1913,
   -1913, -1913, -1913, 21190,  4240, -1913,   947,   958, -1913, -1913,
   -1913, -1913,  4240,  3371,   284,   493, -1913,  4240,   737, -1913,
     927,   279, -1913,   971, 21345,   792, 16433, 23286,  3492, -1913,
    3492,  1460,  3492,  1460, -1913, -1913,   279, -1913, -1913,   972,
   21398, -1913, -1913, -1913, 21553,   860, -1913,  3203,   376,   394,
   -1913,   442,   627,   976,   964, -1913,  2928,   967,   737,  2928,
   -1913, -1913, -1913, -1913, -1913,  2553, -1913,   580, -1913,   999,
   -1913,  1000,  1045, 25767,  1024,  1035,  1038, 25691, 25843,  1058,
   23474, -1913, -1913, -1913, -1913, -1913, -1913, 25919, 25919, 17865,
    1054,  3655, -1913, -1913, -1913, -1913,   535, -1913,   544, -1913,
    1615, -1913, 25691, 25691, -1913,  1034,   694,  1002,  1366,   586,
    1368,  1060,  1073,  1063,  1120,   -19, -1913,   683, -1913,  1105,
   -1913,  1351,  4701, 18501, -1913, -1913,   655,  1105, -1913, -1913,
     716, -1913, -1913,   726,  2573,  1109,  1115,  1128,  1130,  1141,
    1143, -1913, -1913,   542,  1144, -1913,   791,  1144,  1156, -1913,
    1164, -1913, 21137, -1913,  1388,  1154, 18660, -1913, -1913,  3354,
    3977,  1199, 16433,  1202,  1730,  1782,  1165,  1193, -1913, -1913,
   -1913,  4240,  4676, 20619, -1913, -1913, -1913, -1913, -1913, -1913,
   10311,  3051,  1054, 23639,  1208,  1211, -1913, -1913,  1221, 23791,
     897, -1913, -1913, -1913, 18501,  1230, -1913, -1913, -1913, -1913,
    1214,  2491,   779,  1233,  1241,  1247,   849,  1257,  1259,  1262,
    1276,  1279,  1287,  4287, -1913, -1913, -1913,   279,  1232,  1267,
    1300, -1913, -1913,  1293,   497, -1913, -1913,   675,   986, 20126,
   -1913, -1913,   497, -1913, -1913,   675, -1913, -1913,  5079, -1913,
   18501, 18501, -1913,  1460,  4457,  7200,  2688, 16597, -1913, -1913,
   -1913, -1913, -1913,   675,   986,   107,  1299, -1913, -1913,  3492,
    1329,   986, 17323, -1913,   675,   986, -1913,  7637, -1913,  1460,
    1460, -1913, -1913,  1335,   479,  1339,   627,  1345, -1913, -1913,
   -1913, 20566,  1359,  1353, -1913, -1913,   919, -1913,  1452, -1913,
    1356, -1913, -1913, -1913, 21717, 25995, -1913, -1913, -1913, -1913,
   -1913,  3524,   924,  5079, 20126, 16761,   946, 13336, -1913,  4240,
    1378, -1913,  1385, -1913, -1913, -1913, -1913, -1913,  2928, -1913,
   -1913,  1466,  4620, 20774, 12161, -1913, 21770, -1913,  1460,  1460,
   -1913, -1913,   860, -1913, 15449,  1386,  1536, 25691,   782,  1293,
    1379, -1913,   279,   279, -1913,  1144, -1913, 21345, -1913, -1913,
   20566,  1460,  1460, -1913,  4620, -1913, -1913, 22341, -1913, -1913,
   21398, -1913,   279,  1403,   279,   964,   222,  1405,   955, 21190,
     991,   994, -1913,  2553, 23867,  1390, -1913, 18183, -1913,  3655,
   21925, 21190, -1913, 18183, -1913, 25691, -1913, -1913, -1913, -1913,
   -1913, -1913, 18342, -1913, -1913, 20827, 21925, 21925,  1391,  1244,
    1292,   636,  1511, -1913,  1051,  1415,  1367,  1422, -1913, 23943,
   25691, 24019,  1440, 25691,  3163, 25691,  3163, -1913,  2783, -1913,
   -1913, 23867,  2052, 25691, 23867,  3163, -1913, -1913, 25691, 25691,
   25691, 25691, 25691, 25691, 25691, 25691, 25691, 25691, 25691, 25691,
   25691, 25691, 25691, 25691, 25691, 25691, 25691, 24095,  1421,   836,
    4004, 12161, -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913,
   -1913, -1913, -1913,  1442, 25691, -1913, -1913, 15613,  1075, -1913,
   -1913,   279,   279, -1913, -1913, 18501, -1913, -1913,   545,  1144,
   -1913,   990,  1144, 20126, -1913, -1913,  1293, 20126, -1913,  1293,
   -1913, 26071, -1913, -1913, -1913, 19652, 12161,  1447,  1414,  1468,
   14957,  1602,  2891,   574,  1379, -1913,   279,   279,  1379,   614,
   -1913,   279,   279, 25691,  4240, 16597,  1475, 16597,  1477,  1379,
     134, 15777, 15777, 15777,  4240, -1913, -1913, 25691,  1221, -1913,
   23639,  1493, -1913,  2661, -1913, -1913, -1913,  1065, -1913, 15777,
   25691,  1070,  1469,  1501,  1503,  1077,  1515,  1529,  1537,  1543,
    1552,  1568,   764,  1144, -1913, -1913,   769,  1144, -1913, -1913,
     785,  1144, -1913, -1913, -1913,  4457,   836,  1629,  1144, 22785,
   -1913, -1913,   675,  1572, -1913, -1913, -1913,  1108,  1596,  1131,
    1599, -1913,  1156,  1556,  1509, -1913,   675, -1913,  1611, -1913,
     675,   986,  1509, -1913,   675,  1614,  1617,  1619, -1913, -1913,
   20429, -1913,  3163,  4240, 11290,  1715, -1913,  1154, -1913, 15777,
    1090,  1625, -1913,  1509,  1630, -1913, 21978, 18501,  1609, -1913,
    1609, -1913, -1913, -1913, -1913, 21398, -1913, 12329, 18819, -1913,
    1632,  1634,  1635,  1636, -1913, 10115,   279, -1913,   782, -1913,
   -1913, -1913, -1913,  1293, -1913, -1913, -1913,  1460, -1913, -1913,
   -1913, -1913,   222,   964,  1631,   367, -1913, -1913,  1639,  4240,
     222, -1913, -1913,  1638,  1647, -1913, -1913,  1133, -1913, -1913,
   -1913, -1913,  1648,  1650,  1652,  1649,  1658,  1651,  1664,  1667,
    1662,  1669, 25691,  1670,  1671,  1674, 22133, 12497, 25691, -1913,
   -1913,  1666, -1913, -1913, -1913, 25691, -1913,  1675,  1676, 23715,
   -1913, -1913,  1417, -1913, 23867,  1655, -1913,  1677, -1913, -1913,
    4653, -1913,  1136, -1913, -1913, -1913,  4653, -1913, -1913,  1433,
      85, -1913, -1913,  1034,  1034,  1034,   694,   694,  1002,  1002,
    1366,  1366,  1366,  1366,   586,   586,  1368,  1060,  1073,  1063,
    1120, 25691,  1438, -1913,  1679,  4653, -1913, -1913, 23639, -1913,
    1681,  1682,  1683,  1685,  1075, -1913, -1913, -1913, -1913, -1913,
   20126, -1913, -1913,  1293, 20126, -1913,  1293,  1687,  1688, 15777,
   15777, -1913, -1913, 14957,  1025,  1689,  1690,  1693,  1694,  3063,
    2891, -1913, -1913, 20126, -1913, -1913, -1913, -1913, -1913, -1913,
   20126, -1913, -1913, -1913, -1913,  1695, -1913,  1379,  1697, -1913,
   -1913, -1913, -1913, -1913, -1913, -1913, -1913,  1702,  1699,  1700,
   -1913, -1913,   497,  4653,  1453,   123, -1913, -1913,  1657, -1913,
   23791, -1913, 25691,   279, 15777, -1913, -1913,   850,  1144, -1913,
     881,  1144, -1913, -1913,   902,  1144, 20126, -1913, -1913,  1293,
   20126, -1913, -1913,  1293, 20126, -1913, -1913,  1293,   946,  1705,
   -1913,  1293,   265, -1913,  1105,  1703, -1913, -1913, -1913, -1913,
   -1913, -1913,  1714, -1913, -1913, -1913, 21978,  1509, -1913,   675,
   -1913, -1913, -1913, -1913, -1913, 14154, -1913, -1913, -1913, -1913,
     402, -1913,   116,   260, 11993,  1716,  1720, 17145,  1722,  1727,
    1373,  2606,  2751, 24171,  1729, -1913, -1913,  1733,  1734, 17145,
    1737, -1913, -1913,   675, 25691, 25691,  1855,  1723,   610, -1913,
   17706,  1456,  1735,  1718, -1913, -1913, -1913, 11112, -1913, -1913,
   -1913, -1913, -1913,  1088, -1913, -1913, -1913,  1531,     0, -1913,
      38, -1913,     0, -1913, -1913, -1913, -1913, -1913,  3163, -1913,
   -1913, 13666, 19497,  1739, -1913,  4240,  1742,  1743, -1913, 16761,
   -1913, -1913,  4240, -1913, -1913,  4457, -1913, -1913,  1725,  1731,
    1171, 21190,   737,   737, -1913, -1913,  1054,  1154, 18660, -1913,
    1105, -1913, 12665, -1913,   907,  1144, -1913,  1460, 10440, -1913,
   -1913,   964,  1639,  1732,   222,   627,   339,  1758,  1741,  1639,
    1759, -1913, -1913, 23867,   624, -1913, 20566, 12497,  3163, -1913,
     624, -1913, 20982,   624, -1913, 25691, 25691, 25691, -1913, -1913,
   -1913, -1913, 25691, 25691,  1751, 23639, -1913, -1913, 24247,  1755,
     681, -1913, -1913, -1913,  3281, -1913, -1913,  1472, -1913,    33,
   -1913,  1480, -1913, 23943, -1913, -1913, 25691,  1745,  1489,  1506,
    1221, -1913,   912,  1144, -1913, -1913,  1764,  1765, -1913, -1913,
   -1913, -1913,  1770,   942,  1144, -1913,  1022,  2192,   279,   279,
   -1913, -1913,  1771,  1773, -1913,  1772, -1913, 16597,  1781, -1913,
   16105, 16269,  1786,  1787, -1913,  1784, 25691, 25691,  1508,  1789,
   -1913, -1913, -1913, -1913, -1913,  1793, 20126, -1913, -1913,  1293,
   20126, -1913, -1913,  1293, 20126, -1913, -1913,  1293,  1794,  1795,
    1796,   497,   279, -1913, -1913,  1530, 25691, 22937,  1797,  1792,
   -1913, -1913, -1913,  1800, 14312, 14470, 14628, 21978, 23233, 21925,
   21925,  1803, -1913,   426,   441,  2357,  8469, -1913,   455,  4240,
    4240, -1913, 23867,   242,   310, -1913, -1913, -1913, -1913, 11993,
   25691,  1805,  1882, 11824, 11468, -1913,  1790, -1913,  1791, 25691,
    1798, 23639,  1799, 25691, 18501, 25691, -1913, 11646,  1005, -1913,
    1801,    21, -1913,    58,  1880,   226, -1913,  1819, -1913,  1806,
   -1913,  1809,  1820,  1821, 17145, 17145, -1913, -1913,  1884, -1913,
   -1913,   262,   262,   304, 15285,   279,   529, -1913, -1913,  1823,
    1829,   284, -1913,  1836, -1913,  1830, -1913,  1833, -1913, -1913,
   -1913, -1913, 12833,  1837,  1838,  1839, -1913, 20126, -1913, -1913,
    1293, 25691, 25691,  1154,  1840, -1913,  1841,  1842,   222,  1639,
     367,  4240, -1913, 24323, -1913,  1847, -1913, 21978, -1913,  1217,
    1848,  1845,  1174, -1913,  1846, -1913, -1913, -1913, -1913, -1913,
   23639,  1221, -1913, -1913, 23943, -1913,  1887,  4653, -1913,  1887,
    1887, -1913,  4653,  3553,  3863, -1913,  1532, -1913, -1913, -1913,
    1859, 20126, -1913, -1913,  1293, -1913, -1913,  1858,  1860,   279,
   20126, -1913, -1913,  1293, 20126, -1913, -1913,  1864, -1913, -1913,
   -1913, -1913, -1913, -1913, -1913, -1913,  1697, -1913, -1913, -1913,
    1856, -1913, -1913, -1913, -1913,  1862,  1867,   279,  1869,  1871,
    1874, -1913, -1913, -1913, -1913, -1913, 25691, -1913,   265, -1913,
    1105, -1913, -1913,  1879,  1883, -1913,  1803,  1803,  1803,  4078,
    1222,  1854,   546, -1913,  4078,   587, 18501, -1913, -1913, -1913,
    3538, 25691,  5280,   326,  1875, -1913, -1913,   369,  1878,  1878,
    1878,  4240, -1913, -1913, -1913,  1176, -1913, -1913, -1913, -1913,
    1735,  1885, 25691,   390,  1886,   532, 14793, 21978,  1181,  1889,
   17145,  1893, -1913, -1913, -1913,   956, 17145, 25691,  1494,   578,
   -1913, 25691, 23560, -1913, -1913,   588, -1913,  1221, -1913,  1184,
    1189,  1191,   670, -1913, -1913, -1913, -1913,   675,  1005,  1891,
   -1913, -1913, 25691, -1913,  1896,   836, -1913, -1913, -1913, -1913,
   25691, 25691, -1913, -1913,   404,   262, -1913,   451, -1913, -1913,
   10934, -1913,   279, -1913,  1609, -1913, 21978, -1913, -1913, -1913,
   -1913, -1913,  1895,  1899, -1913, -1913,  1901, -1913,  1902,   222,
   -1913,  1639,  1909,   627,  1741, 23639, -1913, -1913, -1913,  1907,
   -1913, -1913, 25691, -1913, 20982, 25691,  1221,  1911,  1538, -1913,
    1542, -1913,  4653, -1913,  4653, -1913, -1913, -1913,  1910,   279,
     279,  1914,  1915, -1913,  1906, -1913, -1913, -1913, -1913, -1913,
    1544, 25691, -1913, -1913, -1913, -1913, -1913,  1873,  1222,  1425,
     594, -1913, -1913, -1913, -1913,   279,   279, -1913, -1913, -1913,
    1898, -1913,  1210,  3538,   830, -1913,  5280, -1913, -1913,   279,
   -1913, -1913, -1913, -1913, -1913, 17145, 17145,  1735, 16925,   192,
   24399,  2000, 17145, -1913, -1913, -1913, -1913, 25691, -1913, 24475,
    2007,  1903, 23481, 24551, 17145, 11646,  1735,   516,  1526,  1904,
   25691, -1913,  1932,   208, 17145, -1913, 17145, -1913,  1935, -1913,
   -1913,  1912,   836,   693,  1936,  1550,  1235, 17145,  1940, 17145,
   17145, 17145, -1913, -1913, -1913,   737, -1913,  4240,  4457, -1913,
   -1913,  1942,  1943, -1913, -1913,  1639,  1952, -1913, -1913,  1221,
    1953, -1913, -1913, -1913, -1913,  1956, -1913, -1913, -1913,  1559,
    1567, -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913, -1913,
    1939,  1954,  1961,  1425, -1913,   279, -1913, -1913, -1913, -1913,
   -1913,  1960,  4078, -1913,  2045,  5353,   154, 13004, -1913, 17022,
   -1913,     6,  1238, 17145,  2046,   597,  1955,   400, 17145, 25691,
    2015,  1966,   516,  1526,  1945, -1913, 24627,  1957,   511,  2056,
   -1913, 24703, 24779, 25691,  1735,  1967, 13171, -1913, -1913, -1913,
   -1913, 22186, -1913,  1970,  1969,    99, -1913, 25691, 23867, -1913,
   -1913, 25691,     0, -1913, -1913, -1913, -1913, -1913, -1913, -1913,
    1981, -1913,  1982, -1913, -1913, -1913, -1913,  1032,  1144, -1913,
   -1913,  1222, -1913, 17145, -1913,   129, -1913,   164, -1913, -1913,
   -1913,  1987, 13831, -1913, -1913, 17145, -1913,    16, -1913, 17145,
   25691,  1990, 24855, -1913, -1913, -1913, 24931, 25007, 25691,  1735,
   -1913, 25083, 25159, 17145,  1980,   563,  1983,   567, -1913, -1913,
    1998, 13831, 22186, -1913,  4140, 21770,  3163,  1991, -1913,  2049,
    2001,   740,  1996, -1913, -1913,  1256,  1273,   423, -1913, -1913,
   20126, -1913, -1913,  1293, -1913, -1913, 25691, -1913, 25691, -1913,
   -1913,  1654, 13996, -1913, -1913, 17145, -1913, -1913,  1735, -1913,
   -1913,  1735,  2002,   581,  2004,   654, -1913, -1913,  1735, -1913,
    1735, -1913,  2016, 25235, 25311, 25387, -1913,  1654, -1913,  1999,
    3404,  4209, -1913, -1913, -1913,    99,  2020, 25691,  2009,    99,
      99, -1913, -1913, 17145,  2101,  2021, -1913, -1913, 17022, -1913,
    1654, -1913, -1913,  2028, 25463, 25539, 25615, -1913, -1913,  1735,
   -1913,  1735, -1913,  1735, -1913,  1999, 25691,  2030,  4209,  2031,
     836,  2033, -1913,   753, -1913, -1913, 17145, -1913, -1913, 10570,
    2038, 17022, -1913, -1913,  1735, -1913,  1735, -1913,  1735,  2040,
    2039, -1913,   675,   836,  2048, -1913,  2032,   836, -1913, -1913,
   -1913, -1913, 10790, -1913,   675, -1913, -1913,  1587, 25691, -1913,
    1274, -1913,   836,  3163,  2043,  2035, -1913, -1913,  1286, -1913,
   -1913,  2037,  3163, -1913, -1913
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
     662,  1114,     0,   679,   682,   736,   683,   665,     3,     0,
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
    1041,     0,     0,  1047,  1049,  1114,     0,   664,   678,  1114,
       2,    50,    42,    40,    41,     0,    66,   182,    79,     0,
       0,  1006,   448,   447,   977,   992,   748,  1010,  1018,   633,
       0,     0,     0,  1056,  1057,   850,   634,  1053,  1054,   632,
     612,     0,     0,   343,   222,     0,     0,     0,   244,   345,
     224,     0,     0,   345,   253,   268,   279,   273,   345,   189,
       0,   310,     0,   283,     0,   308,     0,   274,   272,   261,
     264,     0,     0,   189,   297,     0,     0,   227,   342,   373,
       2,   490,   339,     0,     0,   401,   352,     0,    69,   363,
     356,     0,   357,   355,   370,   760,   796,   798,  1050,  1051,
       0,   668,     0,   788,    67,    83,    81,   850,  1063,  1065,
    1058,     0,   645,   345,   239,   234,   237,     0,   236,   243,
     242,     0,   490,   246,   245,   345,   255,     0,   252,   345,
       0,     0,     0,   260,   265,   311,     0,     0,   189,   284,
     309,     0,     0,   345,     0,   299,   300,   298,   267,   330,
       0,   490,   490,     3,   386,   491,   390,     0,   394,     0,
       0,     0,   402,   403,   348,     0,     0,     0,   667,   684,
     490,  1059,  1061,  1062,   652,   223,     0,   241,     0,   240,
     226,   247,   490,   414,   256,   345,   257,   254,   269,   282,
     280,   276,   288,   286,   287,   285,   266,   281,   277,   278,
     275,   262,     0,     0,     0,     0,   229,   247,     3,   379,
       0,  1055,   387,   388,   389,   401,     0,     0,     0,   401,
       0,   353,   349,   345,     0,     0,   235,   238,   345,     3,
     248,   415,   258,     0,     0,     0,     0,   307,   305,   302,
     306,   303,   304,   301,     3,   379,     0,     0,  1056,     0,
       0,     0,   395,     0,   404,   358,   345,  1064,   218,     0,
       0,   345,   295,   293,   290,   294,   291,   292,   289,     0,
       0,   380,     0,   407,     0,   405,     0,   407,   359,   220,
     219,   225,     0,   228,     0,   377,   408,     0,     0,   396,
       0,   378,     0,     0,     0,     0,   409,   410,     0,   406,
     397,     0,     0,   398,   411
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1913,  4211,  2662, -1913,    -1,   389,  2893,    34, -1913,  -381,
   -1913,   421, -1913,  -741, -1913,   890,  -998,  -985, -1913,   395,
    5365,  2129, -1913,  1281, -1913,  1491,   600,   992,   993,   820,
     998,  1448,  1450,  1454,  1458,  1459, -1913,  -159,  -151,  9061,
    1006, -1913,  1777, -1913, -1913, -1269,  4561,  -982,  1988, -1913,
    1316, -1913,   995,    93, -1913, -1913,   766,   178, -1913, -1821,
   -1575,   384,   151, -1913, -1913,   762,   396,   294, -1595, -1913,
   -1450, -1913, -1913, -1913, -1913,   195, -1220, -1913, -1913, -1236,
     522, -1913, -1913, -1913, -1913, -1913,   250, -1198, -1913, -1913,
   -1913, -1913, -1913,   119,   539,   540,   217, -1913, -1913, -1913,
   -1913,  -789, -1913,   150,    95, -1913,   230, -1913,  -186, -1913,
   -1913, -1913,  1007,  -880, -1017,   -68, -1913,    14,    77,   249,
    9110,  -984,  -801, -1913,  -111, -1913, -1913,    82, -1913,  -141,
     519,   117,  -249,  3065,   296,  -653,    68,   165,   383,   768,
    2612, -1913, -1913,  2214, -1913,    36,  5071, -1913,  2153, -1913,
     712, -1913, -1913,  2057,   880,  5788,  4330,   -53,  2005,  -324,
   -1913, -1913, -1913, -1913, -1913,  -198,  8440,  8066, -1913,  -395,
     253, -1913,  -838, -1913,   345, -1913,   274,   829, -1913,   102,
    -199, -1913, -1913, -1913, -1913,   -94,  8775,  -925,   970,   524,
    -257, -1913,    -4,  -157,   -25,  1357,  2018,  -775,  -162,  1012,
    4661,  -172,  -509,  -243,  -207,  -447,  1423, -1913,  1769,   575,
    -936,  1643, -1913, -1913,   771, -1913, -1246,  -178,  -181,  -516,
   -1913,   159, -1913, -1913, -1175,   548, -1913, -1913, -1913,  2295,
    -819,  -494, -1067,   -10, -1913, -1913, -1913, -1913, -1913, -1913,
    -158,  -852,  -215, -1912,  -209,  7572,   -75,  5980,   -88,  1597,
   -1913,   988,   -89,  -228,  -196,  -192,     4,   -65,   -59,   -46,
     787,   -32,   -12,    37,  -167,   275,  -161,  -145,  -115,   419,
    -113,   -97,   -48,  -779,  -763,  -722,  -713,  -731,  -110,  -707,
   -1913, -1913,  -718,  1533,  1534,  1541,  2455, -1913,   685,  6617,
   -1913,  -632,  -579,  -513,  -512,  -627, -1913, -1669, -1773, -1757,
   -1735,  -658,    76,  -132, -1913, -1913,   -81,   343,   -91, -1913,
    7380,  2416,  -270,  -269
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   848,   426,   427,    83,    84,   428,   402,   429,
    1562,  1563,   430,   997,   998,   999,  1357,  1358,  1359,  1576,
     452,   432,   433,   434,   731,   732,   435,   436,   437,   438,
     439,   440,   441,   442,   443,   444,   445,   454,  1145,   733,
    1490,   794,   223,   796,   448,  1032,  1242,  1243,  1244,  1245,
    1246,  1247,  1248,  2149,  1249,  1250,  1681,  2006,  2007,  1938,
    1939,  1940,  2119,  2120,  1251,  1699,  1700,  1955,  1701,  1848,
    1849,  1252,  1253,  1254,  1255,  1256,  1257,  1875,  1879,  1512,
    1504,  1258,  1259,  1511,  1505,  1260,  1261,  1262,  1263,  1264,
    1265,  1266,  1718,  2137,  1719,  1720,  2043,  1267,  1268,  1269,
    1493,  2051,  2052,  2053,  2177,  2188,  2071,  2072,   308,   309,
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
      82,   196,   388,    82,   214,   135,   369,   533,   867,   240,
    1042,   198,   572,  1050,  1293,   529,   523,   199,   153,   975,
    1494,  1440,  1508,  2008,   736,   205,   446,   355,   310,   549,
     200,   554,   678,   587,   447,  1920,   969,   111,   562,   534,
     742,   569,  1495,   535,   201,   612,  1530,  1531,  1361,   183,
     496,  1921,   318,  1111,  1270,    82,    82,  1312,    82,   558,
     384,   908,   910,   135,   202,  1319,   858,  1105,   536,   104,
    1129,   214,  1131,  1922,   537,    82,   383,  1368,    91,  1023,
    1085,  2015,  1274,    98,    82,   961,   557,   584,   914,   212,
     538,  2075,    82,   565,  1586,   111,   533,    82,   595,   922,
      82,  1112,   244,  1200,    82,   272,   463,  1860,  1106,   283,
     626,   628,  1506,   203,   583,   483,   776,  1107,  2014,   276,
     539,  1722,   540,  1108,  1281,   594,  1439,   104,   534,   161,
     225,   465,   535,  1443,   310,  1507,    91,   466,   541,   321,
    1924,    98,  1724,    82,   943,  1405,    82,   544,    82,  1509,
     467,   962,   963,   135,    82,  2009,  2008,   536,  1583,   777,
     531,    82,   310,   537,   468,  2067,   105,   650,   678,    82,
     198,  -799,  1510,   310,   163,   111,   199,   164,   165,   538,
     166,   396,   497,   579,   469,   111,  2016,   542,  1452,   200,
     992,  1584,    58,   396,    82,    82,  2076,    58,   620,   212,
     969,  1723,  1007,   201,  -831,    82,  1115,   650,   548,   539,
      82,   540,  1122,  1725,  1520,   505,   148,   104,   401,   400,
     514,  1453,   240,   202,   105,    82,    91,   541,    82,    82,
      82,    98,   985,   470,    82,    82,   544,  -800,   688,  1364,
     212,  1482,   691,   162,   371,   602,  1323,   737,  1853,   631,
      92,   961,   579,   154,  2000,    82,    75,  1956,  2049,   198,
    1406,    75,   627,    82,   167,   199,  2066,   694,   212,  1935,
    1936,  1462,   203,    82,    82,   302,   542,    82,   200,   550,
     168,   111,   591,   693,    82,  1935,  1936,   678,  1734,   627,
     697,  1494,  1737,  1350,  1407,   182,  1089,  2014,    82,    82,
    1407,    82,   903,   872,   313,  1778,    82,   701,    92,   111,
      82,   900,   862,   667,   105,   622,  2010,   962,   963,   904,
     111,  1463,  1464,    82,    82,  2068,  2069,  1920,  1004,   212,
    2014,  1390,   678,    82,  1467,   873,  1310,    20,  1506,   874,
    1105,    82,    82,  1921,   144,   111,    82,   144,   184,  1550,
     265,    58,  1228,   906,   277,  1937,   678,  1048,   190,   911,
      58,  1507,  1468,   678,   875,  1922,  1375,  1270,  1340,  1578,
     876,  1966,   214,   247,  1644,  2103,    82,   612,  1441,  1391,
     839,  1106,  1768,  1726,   106,    82,   877,   986,    82,  1549,
    1107,   824,   380,   151,   396,  1274,  1382,   266,    92,   185,
     902,   970,   144,   872,  1454,   303,   921,  1961,  1962,   288,
     313,   295,   483,   297,  1433,    75,   878,  1735,   879,   193,
     496,   643,  1676,   303,    75,  1197,   916,  1452,  1452,  1452,
     661,  1290,  1924,   920,   880,   873,  1434,   924,   313,   874,
    1465,    58,   106,   559,   596,   177,   177,   550,   144,   313,
     644,   645,   266,   194,   892,   295,   297,   225,   205,   608,
    1453,  1453,  1453,    82,   875,   932,   465,   303,   505,  2000,
     876,   655,   466,   303,   313,    63,    64,  1115,  1310,  1877,
    1827,   310,   177,   881,   267,   467,   877,   686,    82,    82,
    1678,   689,   144, -1115,   355,  1502,  2118,   496,  2113,   468,
      82,    82,  2026,  2027,   986,    75,  1828,   266,   206,   380,
      82,   545,   514,  1228,  1878,  1494,   878,   251,   879,   469,
      99,   956,  2118,   155,   961,    78,  1881,   588,   550,   226,
      82,  -831,   106,   177,   880,  1002,   177,  1495,  1006,  -984,
    1080,  1008,    82,  1503,   892,  2151,  -984,   505,    -3,   193,
    1011,   177,   497,  1013,  1014,  1015,  1097,  -985,   980,   381,
    1098,  1025,   661,  1460,  -985,  1116,   465,    82,   470,  1119,
     218,  2022,   466,    82,   266,    82,   295,   297,    99,  1780,
    1134,  1135,  1461,   881,   275,   467,   247,   229,  1148,  1682,
     962,   963,    58,  1682,  1702,   480,   229,  1781,  1783,  1785,
     545,   984,   230,   655,   210,  -663,  1658,  1702,   266,   302,
     941,   471,  -663,   266,    58,   177,  1671,    58,   528,   266,
     530,  1659,  1613,  1761,   496,  1129,  1131,   942,   111,   497,
      82,  1370,    82,   678,   193,  1672,    82,   505,    82,  1985,
     975,   135,  -608,  1454,  1454,  1454,    58,    82,   230,   220,
     647,    82,   266,  1149,   648,   546,    75,   683,    99,   297,
     221,   589,   316,   177,   177,  1843,  1844,  1845,    99,   496,
     231,   210,  1025,   111,   177,   634,   222,  1539,    75,   550,
    1178,    75,  2032,    82,   446,  1132,    58,  1846,   605,   177,
    1460,   610,  1126,   956,   743,   267,   151,   813,  1144,   744,
    1090,   550,   396,   745,   550,   104,   719,  1818,   746,  1741,
      75,   371,   232,   115,    91,  1851,   766,   767,  1317,    98,
    1859,  1675,   839,   177,  1025,   481,  1819,   602,  1025,  1113,
     623,   177,   177,   665,  2093,  1854,   177,   702,  2095,   267,
    1855,   703,  1025,    82,   546,    82,  2057,    82,  1671,  1025,
      75,    82,  2124,   661,    82,  1925,   497,   313,  1025,  1606,
     266,   768,   769,  1199,    99,   193,   692,  1821,  1861,  1120,
     954,   115,  -724,   665,  1926,   177,   256,  2020,   177,    82,
    1344,   574,  1529,   578,  1909,   298,  1910,  1345,   266,    74,
     683,   297,    99,  1018,   974,    14,    15,    16,    17,    18,
    -484,   497,   105,    99,  1019,  1020,   719,   979,  1885,   300,
     797,  1807,   302,  1074,   550,  1025,  1820,   280,   893,   958,
    1128,    80,    81,  1295,    82,  2126,   155,  1866,    99,    82,
     191,    82,  1855,  1376,  1895,  1575,    58,  1377,   303,   266,
     778,    58,  1323,    82,   779,   736,   204,    64,  1140,  1141,
    1974,   115,   578,    82,    58,  1975,  1392,    58,  1396,   514,
     759,   115,    82,  1393,   266,  1155,   322,   760,   761,   266,
    1954,   266,   213,   804,   286,   267,   355,   805,   177,   287,
    -820,   116,   291,   806,   296,   245,    92,   703,   273,  1313,
     177,   177,   284,   266,    82,   266,   266,  2108,  1571,  1404,
      75,   386,  2109,   257,   258,    75,   259,   266,   893,  1428,
    2166,   260,  1365,  1429,   343,  2167,    74,  1430,    75,  1166,
     266,    75,    58,   550,  1170,   483,    74,  1770,   550,   266,
    1179,    82,    82,   514,   302,  1278,   471,   664,   550,   116,
    1174,   665,   954,  1411,   550,   389,   817,   664,    80,   666,
     550,   665,   266,    58,   683,   297,   400,   115,    80,   666,
    1283,   698,   894,   472,   700,   480,   473,  2024,  1710,  1431,
     111,   667,   210,   474,    58,  1702,   266,   683,   678,    58,
     144,  2038,   213,   266,    58,   115,    75,   471,    82,   550,
     475,   958,   941,  1144,   267,   476,   115,   477,   506,  1389,
     839,   823,   104,   503,   559,  1416,   885,   589,   550,   550,
     504,    91,   601,    64,    58,   508,    98,    75,   509,   116,
     106,   115,   481,   213,   524,   280,   522,   163,   177,   116,
     164,   165,   527,   166,  1707,   286,  1420,   177,    75,    82,
     550,   953,  1547,    75,   547,    82,  2086,   548,    75,  1555,
     570,   213,   894,   571,  1651,   863,   864,  1424,  1541,   865,
     576,   550,  1537,   589,   197,   592,   665,  1591,   737,   582,
    1522,   550,  1294,   978,    82,   225,   931,   514,    75,   634,
     932,   471,   286,   550,   651,   298,   239,   617,    14,    15,
      16,    17,    18,   371,    58,  1380,  1381,  1600,   593,  1272,
    1590,   550,    82,   621,    58,  1843,  1844,  1845,    82,    82,
     638,    99,   991,   639,   388,   388,   648,  1132,  1713,  1714,
    1715,  1716,  1717,  1776,  1459,   116,   287,  1846,   682,   685,
     296,   653,  2073,  1036,   380,  1038,  1847,  1041,   355,   696,
    1822,  1047,    82,   327,  1051,  1094,   699,    58,   993,   550,
    1414,   994,   648,   116,   704,   703,    99,   705,    75,  1628,
    1564,  2073,  -485,  1629,   116,   706,  1499,  1630,    75,  1076,
     762,   763,    14,    15,    16,    17,    18,  1604,   720,   709,
    1113,   665,   471,    92,   665,  1559,   355,  2060,  1634,   116,
     710,   550,  2121,   711,   150,  1284,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,   758,  1587,  1024,    74,
    1283,    75,  1025,   715,   480,   739,   514,  1546,   327,    82,
      82,    82,  1153,   532,   239,   302,   805,   772,   514,   550,
     797,    58,   559,   177,   550,  1132,   550,  2055,   773,  1132,
     774,    80,    81,   177,   327,   634,   514,   446,   446,   550,
     111,   506,    82,   266,   775,  1621,  1622,  -126,  -126,  -126,
    -126,  -126,  -126,   780,   266,  1188,   807,    82,  1500,  1025,
      82,    82,   808,   266,    82,   272,   283,   144,   720,  1762,
    1753,    82,   104,   111,    82,   809,   276,   810,  1190,   144,
    1322,    91,  1025,  1360,  1323,    75,    98,  1323,   811,   327,
     812,  1777,   941,   479,   115,  -125,  -125,  -125,  -125,  -125,
    -125,   820,   632,   327,    -3,   104,  2139,   106,  1899,   822,
    2143,  1209,   177,   177,    91,   390,   845,    82,  1528,    98,
     506,  1773,   805,  1835,  1788,  1774,   843,  1323,  1839,  -487,
     267,  1863,  1025,  1791,   514,  1025,  1864,  1792,  1865,   115,
     805,    74,  1025,    82,   847,   371,    74,   286,  1685,  1053,
    1054,  1055,  1685,  1459,  1459,  1459,  1668,  1930,  1653,  1459,
     -18,   805,   664,   859,   661,  1616,   665,  1808,   177,  1272,
     266,   550,   860,    80,   666,   868,    82,   896,    80,    81,
     882,   355,  1979,   870,   446,  2017,  1025,  -486,   883,  1025,
     391,  1683,  1766,   371,   884,  1683,   266,    14,    15,    16,
      17,    18,  1272,  2111,   886,   974,   887,  1323,   392,   888,
     393,   394,    65,    66,    67,    68,    69,    70,    71,    72,
    2112,  2185,   533,   889,  1025,  2182,   890,  1862,    14,    15,
      16,    17,    18,  2191,   891,  1284,   897,  2192,  1804,  1805,
    1806,   304,   315,    99,    82,  1444,  1445,  1446,    82,    82,
     898,   153,   918,    92,   534,  1285,    58,   395,   535,  1831,
    1831,  1831,   116,    14,    15,    16,    17,    18,   833,   150,
     514,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,  1896,   919,   536,   764,   765,    92,    58,  -606,   537,
     770,   771,  -604,   514,   514,   111,  1905,   941,   928,   111,
     111,   503,   739,    82,   930,   538,   929,   116,   591,   871,
     265,   277,   933,   111,    14,    15,    16,    17,    18,  1022,
      75,   239,    58,  1027,  1028,   481,   936,   104,   945,   144,
     947,   104,   104,   526,   951,   539,   964,   540,   825,   739,
      82,  1016,   739,  1475,   966,   104,   327,   144,   667,    74,
    1668,    75,   327,   541,   498,  1668,   982,   266,   990,   514,
    1001,  1283,  1026,   544,   653,   739,    82,  1347,  1348,  1029,
    1808,    82,    82,    82,   550,  1823,  1060,  1061,  1062,  1063,
     144,    80,    81,  1362,  1363,   872,    75,   106,   266,  1025,
    1366,  1073,  1564,  1034,   266,  1078,   177,  1101,   371,   177,
     177,   177,   542,  -161,  -161,   144,   206,   739,   177,   940,
    1109,   327,  1502,  1503,   619,  1990,  1157,   873,  1102,  1992,
     106,   874,  1581,  1582,  1687,  1130,   177,  1133,  1687,  1687,
    1585,  1582,   177,  1843,  1844,  1845,   115,  1151,    82,  1589,
    1582,  1180,  1687,    82,   267,  1025,   875,   610,  1158,    82,
    1159,    82,   876,  -803,   177,  1846,  1102,  1574,  1623,  1574,
      82,   177,  1160,  1987,  1852,  1843,  1844,  1845,   877,    14,
      15,    16,    17,    18,  1339,   589,  1161,  1025,   712,   514,
    1102,  1636,  1786,  1348,  1162,   514,   154,  1846,  1907,  1348,
    1163,   276,  1908,  1582,  1917,  1025,  -190,   388,   878,  1164,
     879,  1977,  1978,   756,   757,  1285,   825,  1560,    92,  1995,
    1582,   678,    92,    92,  1455,  1165,   880,  1996,  1582,  1187,
    1669,  1935,  1936,    99,   756,   735,    92,   892,   747,   514,
     748,   749,   750,    14,    15,    16,    17,    18,  2182,  2183,
    1579,  1580,   266,  1189,  1056,  1057,  1191,  1058,  1059,  1873,
    1283,  1736,  1738,   588,   514,  1195,    99,   756,  1064,  1065,
     751,  1832,  1833,   752,   753,   881,   446,  1202,   754,   755,
    1203,    82,  1204,    82,  2005,  1275,  1279,  1282,  1291,  1303,
     144,  1304,  1305,  1306,  1314,    14,    15,    16,    17,    18,
    1316,  1320,    58,  2115,  1321,  1324,  1284,  1325,  1329,  1016,
     266,  1410,   144,  1326,   116,  1351,   144,   144,  1104,  1328,
     833,  1330,    82,  1332,  1331,    82,  1333,  1335,  1336,  2102,
     144,  1337,  1342,  1343,   514,   514,  1367,  1352,  1371,  1372,
    1373,   514,  1374,  1668,  1378,  1379,  1383,  1384,   177,   177,
    1385,  1386,   106,   514,    58,  1394,   106,   106,  1397,  1399,
    1400,  1401,  1432,   514,  1437,   514,    75,  -488,  -804,  1492,
     106,  1469,   533,   901,  1670,  1470,   514,  1473,   514,   514,
     514,   905,  1474,   327,  1483,  -723,    82,    82,  1484,  1485,
    2046,   111,  1487,   177,   177,  1548,  1025,   589,  1496,   915,
     144,  1515,  1517,  1518,   534,  1524,  1973,   446,   535,   446,
     923,  1526,  1552,  1556,  1570,  2116,  1574,  2005,    75,  -489,
    1553,  1595,  1596,   104,  1669,  1588,   115,  1599,  1610,  1669,
    1611,    82,  1612,   536,   498,   265,   277,   545,   514,   537,
     177,  1614,   514,  1618,  1619,  1582,  1642,   514,   446,  1624,
    1627,  1631,  1632,  1633,  1645,   538,  2141,  1641,  1656,   115,
    1462,  2046,  1689,  1455,  1455,  1455,   155,  1649,  1650,  1654,
    1703,  1704,  1503,  1727,  1228,  1730,  1731,  2161,  1706,  1708,
    1742,  1721,   266,  1743,   280,   539,  1728,   540,    99,  1729,
    1745,  1747,    99,    99,  1748,  1284,  1760,  1749,  1750,  1751,
    1757,  1767,   514,   541,  1759,  1771,    99,  1772,  1775,  2050,
    1779,   498,   544,  1787,   514,  1789,  1793,  1790,   514,   446,
    1687,   471,  1623,   735,  1795,   631,  1797,  2184,  1798,   735,
     144,  1799,   514,  1802,  1817,   198,  1829,  1803,   735,  1052,
    1661,   199,  1836,    82,  1840,    82,  1870,   226,   111,   892,
    1842,  1872,   542,  1919,   200,  1889,  1890,   735,   273,   284,
     177,  1893,  1894,  1897,  1901,  1906,  1916,  1911,  1670,   267,
      19,  1914,  1915,  1670,   514,  1943,  1285,   111,  1929,   177,
     104,   546,  1948,  1949,  1963,   177,  1965,   588,  2044,   144,
    1970,  1104,  1972,  1976,   116,  1981,  1997,  1388,   833,    82,
      82,   893,  1988,  1989,    92,   212,  1991,  1993,   111,   104,
    1994,  1998,   514,    52,    53,    54,    55,   514,  1999,   550,
    2003,  2019,   266,  2025,  -589,  2028,  2021,   116,  2031,   177,
      85,  2033,  2047,   152,   266,  2058,  2059,    82,   498,  2050,
     104,  2070,   505,  2050,  2050,   514,  2079,  2039,   514,  2048,
     514,  2092,  2096,  2105,  2094,  2106,  2107,  2110,   150,  2044,
    1043,  1044,    65,    66,    67,    68,    69,    70,    71,    72,
    1045,   514,  2127,  2123,  2164,  2125,  2146,  1687,  2147,  2136,
    2140,   115,    82,   498,  2152,   115,   115,  2162,    85,  2142,
    2165,    82,  2171,  2163,  2173,  1903,  2174,  2176,   144,   115,
    2189,  2176,   498,  2178,   498,   195,  1687,  1669,   498,   498,
     498,  1046,  2179,  1558,    85,  2190,  2186,  2193,  1186,   266,
    1066,   589,  1021,  1067,   177,   177,   498,   236,  1068,   795,
     264,   177,  1194,  1069,    85,  1070,  1198,  1687,   106,  1691,
    1201,  1491,  1498,   177,  2172,   894,  2117,  1967,  2134,  1712,
    2030,  1960,  2114,   177,  2160,   177,  1880,  1868,  1869,  2098,
    2144,    92,  2180,   174,   494,  1285,   177,   293,   177,   177,
     177,  2097,   152,   327,   581,  2064,   177,  2002,    85,  1655,
    1513,   152,  1516,  1150,   325,   333,  1551,  1207,  1898,   866,
      92,   949,  1744,  1334,  1886,     3,   498,   354,   150,  1338,
     995,   592,    65,    66,    67,    68,    69,    70,    71,    72,
    1346,   186,     6,     7,     8,     9,    10,    11,    12,    13,
     615,    92,   453,  1801,   195,   195,     0,     0,   177,     0,
    1081,  1082,   177,     0,     0,   152,   486,   177,  1083,     0,
     264,   266,     0,     0,     0,     0,     0,  1387,    77,   116,
       0,  1670,     0,   116,   116,   144,     0,     0,     0,   325,
     256,     0,     0,     0,   236,   236,     0,   116,     0,     0,
       0,     0,   285,     0,    99,  1607,   545,     0,     0,     0,
       0,     0,     0,     0,   144,   325,     0,     0,     0,     0,
       0,     0,   177,    85,     0,   106,     0,     0,     0,   280,
       0,     0,     0,     0,   177,     0,     0,   264,   177,     0,
       0,   658,     0,   893,   681,   144,     0,     0,     0,     0,
       0,     0,   177,     0,   106,     0,     0,   658,     0,     0,
       0,   658,     0,     0,   266,  2104,    19,     0,     0,     0,
     325,     0,     0,  1667,     0,     0,   333,     0,     0,     0,
       0,     0,   333,   325,   325,   106,   498,   498,     0,     0,
       0,     0,   152,   150,   177,   233,   234,    65,    66,    67,
      68,    69,    70,    71,    72,    48,    49,    50,    51,    52,
      53,    54,    55,   354,   668,   677,     0,     0,     0,     0,
       0,    74,     0,     0,     0,   712,     0,     0,     0,   354,
       0,   217,   177,   354,     0,     0,     0,   177,     0,     0,
       0,   498,  1660,    77,     0,  1442,     0,     0,     0,  1661,
     546,     0,     0,    80,    81,     0,     0,     0,     0,  1466,
       0,    99,     0,     0,     0,   177,     0,     0,   177,     0,
     177,     0,     0,   658,     0,     0,     0,     0,   453,  1488,
       0,     0,     0,     0,     0,     0,   735,   894,     0,     0,
      99,   177,     0,    58,     0,     0,   390,   115,   217,     0,
       0,     0,  2187,     0,     0,     0,     0,   615,     0,   756,
       0,  2194,   453,     0,     0,   798,    14,    15,    16,    17,
      18,    99,     0,   195,     0,     0,     0,   150,     0,   233,
     234,    65,    66,    67,    68,    69,    70,    71,    72,   462,
       0,   152,     0,     0,     0,   486,  1565,  1566,  1567,   832,
       0,   677,     0,  1568,  1569,    74,     0,    75,     0,     0,
       0,     0,   152,     0,   494,     0,     0,  1667,     0,     0,
       0,   391,  1667,     0,     0,    58,   235,    77,  1824,     0,
    1667,     0,   615,   453,     0,     0,     0,    80,    81,   392,
     236,   393,   394,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   236,     0,   192,     0,     0,     0,     0,   150,
     580,     0,   615,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   494,     0,     0,     0,     0,   325,     0,   453,
     453,   561,     0,   325,     0,     0,   354,    74,     0,    75,
     658,   494,   150,   268,   175,   176,    65,    66,    67,    68,
      69,    70,    71,    72,   115,   289,   292,     0,    76,    77,
       0,     0,     0,     0,   658,   116,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,   658,     0,   580,
       0,   219,     0,   115,   498,     0,     0,   498,   498,     0,
     325,     0,   325,     0,   354,     0,    85,   150,   268,     0,
     663,    65,    66,    67,    68,    69,    70,    71,    72,  1677,
    1679,     0,   354,   486,   115,   677,  1477,     0,     0,     0,
       0,     0,   301,   668,   150,     0,   912,   668,    65,    66,
      67,    68,    69,    70,    71,    72,   354,     0,     0,     0,
       0,  1931,     0,     0,  1667,   615,   677,    77,     0,   354,
     855,     0,     0,   268,     0,     0,     0,     0,   152,  1739,
       0,   615,     0,     0,     0,   615,   453,     0,   494,   152,
     152,     0,   453,     0,     0,     0,     0,     0,   615,     0,
       0,   453,     0,     0,   152,   152,   152,   150,     0,   175,
     176,    65,    66,    67,    68,    69,    70,    71,    72,   217,
       0,     0,     0,     0,     0,     0,   327,     0,     0,     0,
       0,     0,   658,   494,     0,     0,     0,     0,     0,   150,
     268,     0,   116,    65,    66,    67,    68,    69,    70,    71,
      72,  1039,   663,     0,    14,    15,    16,    17,    18,     0,
     486,   803,     0,     0,     0,     0,     0,  1479,     0,     0,
    1667,   116,     0,     0,   268,     0,   798,   798,   815,   268,
       0,   818,     0,     0,   453,   268,     0,     0,     0,     0,
     462,     0,  1040,     0,     0,     0,     0,     0,  1208,     0,
       0,     0,   116,     0,   354,   486,     0,     0,     0,   832,
       0,   832,     0,    58,     0,     0,     0,     0,   268,     0,
       0,     0,     0,     0,   354,     0,   354,     0,     0,     0,
     354,   354,   354,   629,     0,   337,   462,   462,     0,     0,
       0,   561,     0,   338,   339,   340,   341,   150,   354,   233,
     234,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,  1874,   658,     0,     0,   681,     0,     0,     0,     0,
       0,     0,   721,     0,   325,    74,     0,    75,     0,     0,
       0,     0,   327,  1867,   150,     0,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,   830,    77,     0,     0,
     665,     0,     0,     0,     0,     0,     0,    80,   831,     0,
       0,     0,     0,   453,     0,     0,   101,     0,   354,   156,
       0,     0,     0,     0,   494,   152,   453,     0,     0,     0,
       0,     0,     0,     0,   354,     0,  1298,   342,   632,   327,
       0,     0,     0,   976,     0,     0,     0,   668,     0,     0,
       0,     0,     0,     0,   268,   343,     0,     0,   615,     0,
       0,     0,   615,     0,   515,   462,     0,     0,     0,     0,
       0,   615,   721,  1003,   101,     0,   327,   462,     0,  1009,
       0,   615,     0,     0,     0,    58,     0,     0,   615,     0,
       0,     0,     0,     0,     0,   152,   486,   827,     0,   829,
     211,     0,     0,     0,     0,     0,     0,   150,   846,   204,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   150,
     278,   233,   234,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,   615,     0,     0,   268,   615,     0,
       0,     0,   615,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,    58,   312,  1472,     0,    77,   317,   268,
     855,     0,     0,   798,   101,     0,     0,  1486,  1387,    77,
       0,   462,     0,   268,     0,     0,     0,     0,   354,   354,
       0,     0,   832,   356,     0,     0,   268,   150,     0,   832,
       0,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   803,   803,     0,     0,     0,     0,  2054,     0,
     464,     0,     0,  1092,     0,    74,  1095,    75,   268,   150,
       0,   317,   492,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   354,     0,     0,    76,    77,     0,     0,
       0,     0,   268,     0,     0,     0,     0,    80,    81,   268,
       0,   543,     0,     0,     0,     0,     0,     0,     0,   150,
     312,   233,   234,    65,    66,    67,    68,    69,    70,    71,
      72,   568,     0,     0,     0,   152,   573,   575,     0,   211,
     561,     0,     0,     0,   152,     0,     0,  1168,   312,     0,
       0,  1172,     0,   453,     0,  1176,     0,     0,     0,   312,
       0,     0,   597,     0,     0,   658,   599,     0,     0,     0,
       0,   600,     0,   462,     0,     0,     0,   611,     0,   453,
       0,     0,   575,   380,   312,     0,   453,     0,   624,     0,
       0,     0,     0,     0,   494,     0,     0,   150,     0,     0,
     633,    65,    66,    67,    68,    69,    70,    71,    72,  1353,
     264,    85,     0,  1354,   856,  1355,   515,     0,   354,     0,
       0,     0,     0,     0,   325,     0,     0,     0,     0,   656,
     152,     0,   680,     0,     0,     0,     0,   486,     0,     0,
       0,     0,     0,     0,   615,   687,     0,    77,   615,   687,
    1577,     0,   615,   150,     0,   378,   379,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   486,     0,     0,     0,
     150,   152,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,    58,   150,  2175,   175,
     176,    65,    66,    67,    68,    69,    70,    71,    72,     0,
    2181,     0,  1732,  1733,     0,    78,   186,     6,     7,     8,
       9,    10,    11,    12,    13,  1125,     0,   380,     0,   504,
     150,  1183,   233,   234,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,   354,     0,     0,   354,
     354,     0,     0,     0,   641,     0,     0,     0,    74,   803,
      75,     0,     0,     0,     0,     0,     0,   317,     0,     0,
       0,   656,     0,     0,     0,   615,     0,     0,     0,  2100,
      77,     0,     0,   550,     0,     0,     0,     0,   317,   268,
      80,    81,     0,   152,   152,   152,   152,     0,   152,   152,
       0,     0,     0,     0,  1662,   333,     0,     0,     0,   494,
       0,     0,     0,     0,     0,     0,    58,     0,   453,  1276,
    1277,     0,   453,   453,     0,     0,     0,     0,     0,   615,
      58,     0,  1418,   453,     0,  1422,   453,     0,   615,  1426,
       0,     0,   615,     0,   611,     0,     0,     0,     0,     0,
     150,     0,   233,   234,    65,    66,    67,    68,    69,    70,
      71,    72,   492,   264,   150,     0,   233,   234,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   312,    74,   150,
      75,   486,     0,    65,    66,    67,    68,    69,    70,    71,
      72,  1353,    74,     0,    75,  1354,     0,  1355,  1841,   324,
      77,     0,     0,     0,  1850,     0,   152,     0,   668,     0,
      80,    81,     0,  1660,    77,  1349,     0,     0,     0,   611,
     356,     0,   101,     0,    80,    81,     0,   462,     0,    77,
       0,     0,  1782,     0,   976,     0,     0,     0,   687,   957,
       0,     0,     0,     0,     0,     0,     0,     0,  1883,   611,
       0,     0,     0,   968,     0,     0,     0,     0,     0,  1369,
       0,     0,   656,     0,     0,     0,     0,   977,     0,     0,
       0,     0,     0,   515,     0,   687,   856,     0,     0,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,  1662,  1809,
       0,     0,     0,  1662,     0,   453,     0,     0,  1395,  1662,
    1398,  1662,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1402,  1403,     0,     0,     0,     0,  1408,  1409,
     741,     0,     0,    78,   420,   333,   152,  1415,     0,     0,
       0,     0,     0,  1933,  1934,     0,     0,  1593,     0,     0,
    1944,     0,     0,     0,     0,     0,     0,     0,  1602,     0,
       0,     0,  1959,     0,  1435,     0,   492,  1438,     0,     0,
       0,     0,  1968,     0,  1969,     0,     0,     0,     0,     0,
       0,     0,   611,  1084,     0,  1980,     0,  1982,  1983,  1984,
       0,     0,     0,     0,     0,   152,     0,     0,   611,     0,
       0,     0,   611,   268,     0,     0,     0,     0,     0,     0,
     687,   957,     0,     0,     0,   611,     0,  1110,     0,     0,
     462,     0,     0,   152,     0,     0,     0,     0,     0,  1497,
     492,     0,   492,     0,   268,     0,   492,   492,   492,     0,
       0,     0,     0,     0,     0,     0,     0,  2013,     0,     0,
       0,  2018,     0,  1356,   492,     0,  2023,  1809,  1809,  1356,
    1519,     0,     0,     0,     0,     0,     0,     0,  1523,     0,
    1525,  1527,  1662,     0,     0,  1662,     0,     0,     0,  1533,
       0,  1534,     0,  1535,     0,     0,     0,   333,  1356,   150,
    1544,   515,     0,    65,    66,    67,    68,    69,    70,    71,
      72,  1353,     0,     0,   453,  1354,     0,  1355,     0,     0,
       0,  2065,     0,     0,     0,   611,     0,     0,     0,  1271,
       0,     0,     0,  2074,   492,     0,     0,  2077,     0,     0,
       0,   156,     0,     0,     0,     0,     0,   325,     0,    77,
     687,  2091,  1784,  1302,     0,     0,     0,     0,   658,     0,
    1308,     0,     0,     0,     0,     0,  1356,     0,     0,     0,
       0,     0,  1597,  1598,     0,     0,     0,     0,     0,  1639,
       0,     0,  1809,     0,     0,     0,     0,     0,     0,     0,
       0,  1662,     0,  2122,     0,     0,     0,  1620,   268,     0,
       0,     0,   462,     0,  1625,     0,  1626,     0,   615,     0,
       0,   317,   356,   150,     0,   233,   234,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,   658,
     152,  2145,  1643,     0,     0,  1812,  2148,     0,     0,     0,
     150,    74,   601,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,   268,     0,     0,     0,
    1809,     0,   830,    77,  2168,     0,   665,  2170,     0,  2148,
       0,   152,     0,    80,   831,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   611,   667,     0,     0,   611,
    2170,     0,     0,  1075,   492,   492,     0,     0,   611,     0,
     152,   152,     0,  2101,   333,     0,     0,     0,   611,     0,
       0,     0,     0,     0,   150,   611,   233,   234,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,  1752,     0,
       0,   152,     0,     0,     0,  1756,     0,  1758,     0,     0,
       0,     0,    74,     0,   149,     0,     0,     0,     0,   492,
       0,     0,    14,    15,    16,    17,    18,     0,     0,  2101,
    2101,   611,     0,  1660,    77,   611,     0,     0,   515,   611,
    1661,     0,     0,     0,    80,    81,   150,  1356,   233,   234,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   156,     0,  1812,  1812,     0,     0,  2101,     0,     0,
    1456,     0,     0,     0,    74,     0,     0,     0,     0,  1271,
       0,    58,     0,  1794,     0,     0,     0,     0,     0,     0,
     207,     0,     0,     0,     0,  2100,    77,     0,   268,   550,
      14,    15,    16,    17,    18,     0,    80,    81,     0,     0,
       0,     0,  1271,     0,     0,   150,     0,   233,   234,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,   118,     0,     0,   118,     0,     0,  1514,     0,     0,
       0,     0,     0,    74,   356,    75,   150,     0,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,     0,    58,
       0,     0,     0,   656,  2100,    77,     0,     0,   550,     0,
       0,     0,   573,     0,   515,    80,    81,   150,  1812,   233,
     234,    65,    66,    67,    68,    69,    70,    71,    72,   118,
       0,   611,   356,   150,     0,   233,   234,    65,    66,    67,
      68,    69,    70,    71,    72,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,  1891,  1892,     0,   207,
       0,    74,     0,    75,     0,     0,   235,    77,     0,     0,
    1900,   270,     0,     0,     0,   118,     0,    80,    81,     0,
       0,     0,   235,    77,     0,     0,     0,     0,     0,     0,
     268,     0,  2062,    80,    81,     0,  1812,     0,     0,     0,
       0,     0,   492,   515,     0,   492,   492,     0,     0,   118,
    1356,     0,     0,   118,   577,  1356,  1356,  1356,     0,   118,
       0,   611,   118,     0,     0,   611,   270,     0,     0,   611,
       0,     0,     0,     0,     0,     0,     0,   350,   118,  1812,
     382,     0,     0,     0,     0,     0,     0,     0,     0,  1456,
    1456,  1456,   156,   575,   618,     0,     0,     0,     0,     0,
       0,     0,     0,   457,     0,     0,   625,     0,     0,     0,
       0,     0,     0,     0,  1686,     0,   118,   457,  1686,  1686,
       0,   270,     0,   635,     0,     0,     0,     0,     0,     0,
       0,     0,  1686,     0,     0,  1812,  1812,     0,     0,     0,
       0,     0,     0,   150,   654,   233,   234,    65,    66,    67,
      68,    69,    70,    71,    72,   118,   150,     0,   603,   604,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,    74,   118,  1812,   118,     0,     0,   356,     0,     0,
       0,     0,   611,   118,     0,     0,     0,     0,   270,     0,
       0,     0,   324,    77,   118,     0,     0,     0,     0,     0,
       0,     0,   156,    80,    81,     0,     0,     0,    78,   606,
       0,   740,   118,     0,     0,     0,     0,   118,     0,   118,
       0,     0,   270,   118,     0,     0,     0,   270,     0,     0,
       0,     0,     0,   270,     0,     0,   611,   268,   515,     0,
       0,     0,   781,   118,     0,   611,     0,     0,     0,   611,
       0,     0,     0,     0,     0,  1356,     0,  1356,     0,     0,
       0,     0,     0,     0,   118,     0,   270,   118,     0,     0,
     821,     0,     0,     0,     0,   826,     0,     0,     0,     0,
     118,     0,     0,     0,   118,  2099,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   852,     0,   178,   181,   853,
     854,     0,     0,   857,     0,     0,   150,  1826,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   869,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   457,
       0,     0,  1838,     0,   228,     0,     0,     0,     0,   150,
    2135,   899,     0,    65,    66,    67,    68,    69,    70,    71,
      72,  1353,     0,     0,     0,  1354,     0,  1355,    78,     0,
       0,  2150,   150,   457,   175,   176,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,  2159,     0,     0,     0,
       0,     0,     0,     0,     0,   319,     0,     0,   320,    77,
       0,   156,   118,     0,     0,     0,   457,     0,     0,     0,
       0,     0,   270,   344,     0,     0,     0,     0,     0,     0,
       0,   508,     0,   118,     0,     0,     0,     0,   939,   782,
     783,   784,   785,   786,   787,   788,   789,   790,   791,   792,
       0,     0,   946,   220,   457,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1923,     0,   965,     0,     0,     0,
     793,     0,     0,     0,     0,     0,     0,   525,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     457,   457,     0,     0,     0,   270,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1686,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   585,   586,     0,     0,  1017,
       0,   270,     0,     0,     0,     0,   178,     0,     0,     0,
       0,     0,     0,     0,   270,     0,     0,     0,     0,     0,
       0,   178,     0,     0,   118,   118,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     734,     0,   382,   118,   457,     0,   270,     0,     0,     0,
       0,     0,     0,     0,   118,   636,     0,     0,     0,     0,
       0,     0,     0,   640,   642,     0,     0,   118,   649,     0,
     270,     0,     0,     0,   606,     0,     0,   270,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,  1099,     0,  1100,     0,     0,  2045,   457,     0,   826,
     118,   118,     0,   457,     0,     0,     0,   344,     0,     0,
     344,     0,   457,     0,     0,   118,   118,   118,     0,     0,
       0,     0,     0,     0,     0,     0,  1143,     0,     0,     0,
       0,     0,   112,     0,     0,  1152,     0,  1686,     0,  1154,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1686,  2045,     0,     0,
       0,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   654,   611,     0,   118,     0,  1196,
     112,   907,   909,     0,     0,   457,     0,  1686,     0,     0,
       0,     0,     0,   118,     0,     0,     0,   118,     0,     0,
     228,    58,     0,     0,     0,   118,   457,     0,     0,     0,
     118,     0,   849,   850,     0,     0,  2138,     0,     0,     0,
       0,     0,     0,     0,     0,   118,   279,   118,     0,     0,
       0,   118,   118,   118,     0,   150,     0,   233,   234,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,    74,     0,    75,  1327,     0,     0,     0,
     112,     0,     0,     0,     0,     0,     0,     0,     0,  1184,
       0,     0,     0,     0,   324,    77,     0,     0,     0,   359,
       0,     0,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,   457,     0,     0,     0,   734,   118,
       0,     0,     0,     0,   734,     0,   118,   457,   493,     0,
       0,     0,     0,   734,     0,   118,     0,  1300,   457,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
     944,     0,   734,     0,     0,     0,     0,     0,     0,   344,
       0,     0,     0,     0,     0,  1315,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1072,     0,
       0,     0,     0,     0,   112,     0,   118,   457,     0,     0,
       0,     0,    58,     0,  2004,   112,     0,     0,   598,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   359,     0,     0,     0,     0,     0,     0,
     112,     0,     0,     0,   279,     0,   150,     0,   233,   234,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,   403,   149,     0,   404,     0,   405,     0,   406,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
     118,     0,     0,     0,   118,   657,   407,     0,   279,   118,
     118,     0,     0,   118,     0,  1660,    77,     0,     0,     0,
       0,   657,     0,   118,     0,   657,    80,    81,     0,     0,
     118,     0,   781,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,     0,   417,
     418,     0,     0,     0,   118,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,     0,
     118,     0,     0,     0,   118,  1127,     0,  1532,   419,     0,
       0,    78,   420,     0,     0,  1142,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,   118,     0,     0,     0,
       0,  1557,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,   457,     0,     0,     0,     0,     0,
     431,     0,     0,     0,     0,     0,     0,   657,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     457,     0,     0,     0,     0,     0,     0,   457,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1211,     0,     0,     0,     0,     0,
       0,   270,   118,     0,     0,     0,     0,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,   457,     0,
     359,     0,  1300,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   493,     0,
    1318,     0,     0,     0,     0,     0,   118,   457,     0,     0,
       0,     0,   118,   112,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,   359,   493,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,   657,   493,     0,   118,     0,     0,
     118,   118,     0,     0,  1746,   359,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,   657,     0,
     118,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,   657,     0,     0,     0,     0,     0,  1640,   708,     0,
       0,     0,   431,   714,   118,   118,   118,   118,   118,   118,
     118,     0,   723,   724,     0,     0,   270,    75,     0,   117,
       0,     0,     0,     0,     0,     0,     0,   431,   431,   457,
       0,   734,     0,   457,   457,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   457,     0,     0,   457,   431,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   270,     0,     0,   117,     0,     0,
       0,   431,   493,     0,  1746,     0,     0,     0,     0,     0,
       0,     0,   457,     0,     0,     0,     0,   118,   359,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1763,  1476,  1478,  1480,   359,     0,     0,   118,   359,     0,
       0,     0,     0,   281,     0,     0,   657,   493,     0,     0,
       0,   359,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1501,     0,   359,     0,   359,     0,
       0,   118,   359,   359,   359,     0,     0,   117,     0,     0,
     118,     0,     0,     0,   118,     0,  1211,   117,     0,     0,
     359,     0,     0,  1521,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,  1887,  1888,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   457,     0,     0,     0,
       0,     0,     0,     0,     0,   495,     0,     0,     0,     0,
       0,   359,     0,     0,     0,   112,     0,     0,     0,     0,
     359,     0,     0,     0,     0,     0,   270,   118,     0,     0,
       0,     0,     0,     0,     0,     0,   657,     0,   172,   279,
    1680,  1688,     0,   117,  1680,  1698,     0,     0,     0,     0,
    1705,     0,     0,     0,  1709,     0,  1711,     0,  1698,     0,
       0,     0,     0,     0,     0,     0,   172,     0,     0,     0,
       0,   117,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   117,     0,     0,     0,   118,     0,     0,     0,
       0,  1971,     0,     0,     0,     0,     0,     0,   493,     0,
     363,     0,     0,     0,     0,     0,     0,   117,     0,     0,
       0,   281,     0,   172,   118,     0,     0,     0,     0,     0,
       0,  1746,     0,     0,     0,     0,   172,     0,   172,     0,
    1673,  1674,     0,   431,   431,   431,   431,   431,   431,   431,
     431,   431,   431,   431,   431,   431,   431,   431,   431,   431,
     431,   431,   659,     0,     0,   281,     0,     0,  2012,   172,
       0,   385,     0,     0,     0,     0,     0,     0,   659,     0,
       0,   359,   659,     0,     0,   359,     0,     0,   270,     0,
     359,   359,     0,     0,   359,     0,   385,  2041,     0,     0,
       0,  2042,     0,     0,   359,   457,     0,     0,     0,     0,
       0,   359,     0,     0,     0,     0,     0,  1800,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   431,     0,
       0,     0,  1764,     0,     0,   172,     0,     0,     0,   172,
       0,     0,   172,   172,     0,   359,   172,     0,     0,   172,
     172,     0,   172,     0,   172,     0,     0,   359,     0,     0,
       0,   359,     0,  1837,     0,   359,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1856,  1858,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   659,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   112,     0,     0,     0,     0,
       0,     0,  1876,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,   172,     0,     0,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   172,  1834,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   118,   279,     0,     0,   172,   363,     0,     0,
     359,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   495,     0,     0,     0,   657,
       0,   118,   118,     0,     0,   270,     0,   431,     0,     0,
     117,     0,     0,   431,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,   431,     0,     0,   359,   493,     0,
       0,  1942,   118,     0,     0,     0,     0,     0,  1945,     0,
    1947,     0,     0,  1953,  1958,     0,  1698,     0,     0,     0,
       0,  1964,   363,   495,     0,   117,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   431,     0,     0,     0,
       0,   659,   495,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,   172,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   659,     0,     0,   359,     0,
       0,   359,   359,     0,     0,     0,     0,     0,   659,     0,
       0,     0,     0,     0,     0,     0,     0,   359,     0,     0,
       0,   359,     0,     0,     0,   359,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,     0,  2029,     0,     0,
       0,     0,  2035,  2037,     0,     0,     0,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,  2056,     0,   112,   112,     0,     0,  1986,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   495,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   431,
       0,  2078,     0,  2081,     0,   363,     0,  2083,  2085,     0,
       0,     0,  2088,  2090,     0,     0,     0,     0,     0,     0,
       0,   363,     0,   493,     0,   363,     0,     0,   359,     0,
       0,   385,     0,   659,   495,     0,     0,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,   363,     0,     0,     0,   363,
     363,   363,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   172,   172,  2129,  2131,  2133,   363,     0,     0,
       0,     0,   359,   431,     0,     0,     0,     0,     0,     0,
       0,   359,   172,     0,   172,   359,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2154,  2156,  2158,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     431,   431,   431,     0,     0,     0,     0,   431,   431,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,   117,     0,     0,     0,     0,   363,     0,     0,
       0,   431,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   659,     0,     0,   281,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   279,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   431,   431,     0,     0,     0,     0,     0,     0,     0,
       0,   172,   172,     0,     0,   370,     0,     0,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   495,     0,     0,     0,     0,
       0,     0,     0,   172,     0,     0,   172,   172,     0,   172,
       0,   172,   172,   482,   370,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   553,     0,     0,     0,
       0,     0,   172,   553,     0,     0,   172,     0,     0,     0,
     172,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,   363,     0,     0,     0,     0,   363,   363,     0,
       0,   363,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   431,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,   553,     0,   172,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,   363,     0,
       0,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,   370,   669,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   690,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,   117,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -490,  -490,
       0,  -490,    46,     0,    47,   117,  -490,     0,     0,     0,
       0,     0,   657,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
     281,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,   553,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   112,     0,     0,   659,     0,     0,   172,
     553,   816,     0,   553,   819,   431,     0,     0,     0,     0,
       0,     0,     0,   370,     0,     0,     0,   669,     0,     0,
       0,     0,   112,   657,   363,   495,     0,    75,     0,     0,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   359,     0,   172,     0,     0,     0,   172,     0,     0,
     172,     0,     0,   112,   172,     0,     0,     0,     0,   553,
       0,     0,     0,   553,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,   363,   363,
       0,     0,     0,     0,   370,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,   363,     0,
       0,     0,   363,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -491,  -491,   553,
    -491,    46,   370,    47,     0,  -491,     0,   117,     0,     0,
       0,   117,   117,     0,     0,     0,     0,     0,     0,     0,
     955,   370,    58,     0,     0,   117,     0,     0,     0,     0,
       0,   669,     0,     0,   172,   669,     0,     0,     0,     0,
       0,     0,   973,     0,   370,     0,     0,     0,     0,     0,
     431,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     495,     0,     0,     0,     0,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,   431,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   172,     0,     0,     0,     0,     0,    78,     0,
       0,     0,     0,   172,     0,     0,   172,     0,   172,   172,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   370,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   172,     0,   553,   553,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   553,  1093,     0,   553,  1096,
       0,   431,     0,   431,     0,     0,     0,     0,     0,     0,
       0,     0,   955,   370,     0,     0,     0,   669,     0,   669,
     669,     0,     0,     0,     0,     0,   669,     0,     0,     0,
       0,     0,   370,     0,   370,   216,     0,     0,   370,   370,
     370,     0,   431,     0,     0,   281,     0,     0,     0,     0,
       0,   274,     0,     0,     0,     0,   370,     0,   553,     0,
       0,     0,   553,     0,     0,   172,     0,     0,     0,   553,
    1169,   431,     0,   553,  1173,     0,     0,   553,  1177,     0,
       0,     0,     0,     0,     0,  1181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,     0,     0,     0,   334,     0,     0,     0,
       0,     0,     0,   431,     0,     0,     0,     0,   375,     0,
       0,     0,     0,     0,     0,     0,   370,   553,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   216,     0,     0,     0,     0,     0,   172,
       0,     0,     0,     0,     0,   669,     0,   502,     0,     0,
       0,   507,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     170,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   482,   370,     0,     0,     0,     0,     0,
     172,     0,     0,   117,   216,     0,     0,   172,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,   274,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -491,  -491,     0,  -491,    46,     0,
      47,     0,  -491,     0,     0,   299,     0,     0,     0,     0,
       0,   553,     0,   507,     0,     0,     0,     0,   305,    58,
     306,     0,     0,   216,     0,     0,   370,   370,     0,     0,
     669,   669,   172,     0,     0,     0,     0,   669,     0,     0,
       0,     0,     0,     0,   662,     0,   679,     0,     0,     0,
       0,   377,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   659,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   172,
     172,   370,     0,    75,   553,  1419,     0,   553,  1423,     0,
       0,   553,  1427,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   172,   172,     0,     0,   738,
     117,     0,     0,   385,     0,     0,     0,     0,     0,   172,
       0,     0,     0,     0,   555,   556,     0,     0,   560,     0,
       0,   563,   564,     0,   566,     0,   567,     0,     0,   117,
     659,     0,     0,   216,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     117,     0,     0,     0,     0,     0,   662,     0,     0,     0,
       0,     0,   844,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,   216,   172,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,     0,
       0,   669,  1540,   652,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   684,     0,
       0,     0,     0,     0,   370,     0,     0,     0,     0,     0,
     216,   216,     0,     0,     0,     0,     0,   502,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   553,
    1594,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     553,  1603,     0,   669,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   370,   375,     0,   370,   370,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   502,     0,   959,     0,     0,     0,
       0,     0,     0,     0,     0,   814,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   662,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   216,
     172,     0,     0,     0,     0,     0,     0,   216,     0,     0,
     738,   216,     0,   216,     0,     0,     0,     0,     0,     0,
       0,     0,   738,     0,     0,   738,   738,   738,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   895,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   502,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   669,     0,     0,     0,
     294,     0,     0,     0,     0,   216,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   502,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   502,     0,   502,     0,     0,
       0,   502,   502,   502,   366,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   971,   972,     0,     0,     0,   502,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   981,     0,   983,   553,     0,     0,
       0,     0,     0,   366,   186,     6,     7,     8,     9,    10,
      11,    12,    13,     0,   553,     0,     0,     0,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,   257,   258,   502,
     259,    46,     0,    47,     0,   260,     0,   216,    49,    50,
      51,    52,    53,    54,    55,     0,     0,   844,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   366,     0,
       0,     0,     0,  1086,  1087,     0,     0,     0,     0,     0,
    1091,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
       0,     0,     0,     0,     0,  1114,     0,     0,  1117,  1118,
       0,  1121,     0,  1123,  1124,     0,     0,     0,     0,     0,
     366,     0,   366,   366,     0,   553,   553,  -466,     0,     0,
       0,     0,     0,     0,     0,     0,   366,     0,     0,     0,
     366,   553,     0,     0,     0,     0,     0,     0,     0,     0,
    -466,     0,     0,     0,  1167,     0,     0,     0,  1171,     0,
       0,     0,  1175,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   262,     0,     0,     0,     0,     0,     0,   502,
     502,     0,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -491,  -491,     0,  -491,
      46,     0,    47,     0,  -491,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   502,     0,     0,     0,  1309,     0,
     553,    58,     0,     0,     0,     0,     0,     0,   553,     0,
       0,     0,   366,     0,     0,     0,     0,     0,   366,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,   738,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,   365,     0,
       0,     0,     0,     0,   553,  2063,     0,     0,   553,     0,
     738,     0,     0,     0,     0,   366,     0,    78,   332,     0,
       0,     0,     0,     0,     0,    80,    81,     0,     0,     0,
       0,     0,     0,   366,     0,     0,     0,   365,     0,     0,
       0,   274,     0,     0,     0,     0,     0,     0,     0,   375,
       0,   553,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   216,     0,     0,     0,     0,     0,   366,   662,     0,
       0,  1309,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     366,   366,     0,     0,     0,     0,     0,   375,     0,     0,
       0,     0,   738,     0,     0,     0,     0,   553,   553,   366,
     366,     0,   366,     0,     0,  1413,     0,     0,     0,  1417,
     366,     0,  1421,     0,     0,     0,  1425,     0,     0,     0,
       0,     0,   365,   366,     0,     0,   366,     0,     0,     0,
       0,     0,     0,   366,     0,   553,   366,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   502,     0,     0,
     502,   502,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,     0,   365,   365,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,   365,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   738,   738,   738,     0,     0,   738,
     738,     0,     0,     0,     0,     0,   507,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   366,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   366,   216,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1538,     0,     0,   366,
       0,     0,     0,   366,     0,     0,     0,     0,     0,     0,
       0,   366,   366,     0,   274,     0,   366,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   366,   375,   366,     0,     0,     0,   366,   366,   366,
       0,     0,     0,     0,     0,     0,   365,     0,     0,     0,
       0,     0,   365,     0,     0,   366,     0,     0,     0,     0,
       0,     0,     0,   367,  1592,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1601,     0,     0,  1605,     0,
    1608,  1609,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   366,     0,     0,   365,
       0,     0,     0,     0,  1635,   366,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
       0,   366,     0,   366,   366,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   216,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   274,     0,     0,     0,
       0,     0,     0,     0,   365,   365,     0,   367,     0,     0,
       0,     0,     0,   366,     0,     0,     0,  1740,     0,     0,
       0,     0,     0,   365,   365,     0,   365,     0,     0,     0,
       0,     0,     0,     0,   365,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
     365,     0,     0,     0,     0,     0,     0,   365,     0,   367,
     365,   367,   367,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   367,     0,     0,     0,   367,
       0,     0,     0,     0,   738,     0,   366,     0,     0,     0,
     366,     0,     0,     0,     0,   366,   366,     0,     0,   366,
       0,  1605,     0,     0,     0,     0,     0,     0,     0,   366,
       0,     0,     0,     0,     0,     0,   366,     0,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,     0,  1796,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   274,     0,
     366,   365,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   366,     0,     0,     0,   366,   365,     0,     0,
     366,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,   455,     0,     0,   365,     0,     0,
       0,   367,   209,     0,     0,   365,   365,   367,   487,     0,
     365,     0,     0,     0,     0,     0,     0,     0,   209,     0,
       0,     0,   516,     0,   516,   365,     0,   365,     0,     0,
       0,   365,   365,   365,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   209,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,  1884,     0,     0,   489,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   367,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   366,     0,     0,     0,     0,
       0,   738,   367,     0,     0,     0,     0,     0,     0,     0,
       0,  1912,  1913,     0,   366,     0,     0,     0,   366,     0,
     365,     0,     0,     0,   209,     0,     0,     0,     0,   365,
       0,     0,     0,   630,     0,     0,   367,  1927,  1928,     0,
       0,     0,   366,   366,     0,   365,     0,   365,   365,     0,
       0,  1932,     0,     0,     0,     0,     0,     0,     0,   367,
     367,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   738,     0,     0,   507,     0,     0,   367,   367,
       0,   367,     0,     0,     0,     0,     0,     0,     0,   367,
       0,     0,     0,   209,     0,     0,     0,     0,     0,     0,
       0,     0,   367,     0,     0,   367,     0,   365,     0,     0,
       0,     0,   367,   366,   209,   367,   366,   366,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   366,     0,     0,     0,   366,     0,     0,     0,
     366,     0,     0,     0,     0,     0,     0,  2001,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,   365,     0,     0,     0,     0,   365,
     365,     0,     0,   365,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,   367,     0,     0,     0,
     365,     0,     0,   209,     0,     0,     0,     0,     0,  2061,
       0,     0,   367,     0,   516,     0,     0,     0,     0,     0,
     516,     0,     0,     0,     0,   455,     0,     0,   367,     0,
       0,     0,   367,     0,   365,     0,   209,     0,   366,     0,
     367,   367,     0,   366,     0,   367,   365,     0,     0,     0,
     365,     0,     0,     0,   365,     0,     0,     0,     0,     0,
     367,     0,   367,     0,     0,     0,   367,   367,   367,     0,
       0,     0,     0,     0,   209,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   367,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   366,     0,     0,
       0,     0,     0,     0,     0,     0,   366,     0,     0,     0,
     366,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     209,   209,     0,     0,     0,     0,   938,   489,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   367,     0,     0,     0,     0,
       0,     0,     0,     0,   367,   487,     0,     0,     0,   365,
       0,     0,     0,     0,     0,     0,     0,     0,   967,     0,
     367,     0,   367,   367,     0,     0,     0,     0,   365,     0,
       0,     0,   365,     0,     0,   209,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   489,  1000,   365,   365,     0,     0,
       0,     0,     0,     0,     0,     0,  1010,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   209,     0,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
    1031,  1033,     0,     0,  1035,     0,  1037,     0,     0,   209,
       0,     0,  1000,     0,  1049,  1000,     0,   209,     0,     0,
       0,   209,     0,   209,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
     365,   365,  1077,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1079,   365,     0,     0,     0,
     365,     0,     0,     0,   365,   367,  1088,     0,     0,   367,
       0,     0,     0,     0,   367,   367,     0,     0,   367,     0,
       0,     0,   487,     0,     0,     0,     0,  1077,   367,     0,
       0,   489,     0,     0,     0,   367,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,  1146,     0,
       0,   516,     0,     0,     0,     0,     0,     0,     0,   367,
       0,  1156,     0,     0,     0,     0,   489,     0,     0,     0,
       0,   367,     0,     0,     0,   367,     0,     0,     0,   367,
       0,     0,     0,     0,     0,   489,     0,   489,     0,     0,
    1182,   489,   489,   489,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,   365,     0,   489,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   455,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1299,  1301,
       0,     0,     0,     0,     0,     0,   487,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,   366,     0,     0,
     365,     0,     0,     0,   365,     0,     0,     0,     0,   489,
       0,     0,     0,     0,   367,     0,     0,   209,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   367,     0,     0,     0,   367,  1077,     0,
       0,     0,     0,     0,     0,     0,  1341,     0,     0,     0,
       0,     0,     0,     0,     0,  1000,     0,     0,     0,     0,
       0,   367,   367,     0,     0,     0,     0,     0,   366,     0,
       0,   366,     0,     0,     0,     0,     0,     0,     0,     0,
    1307,     0,     0,     0,     0,     0,   366,   209,    14,    15,
      16,    17,    18,     0,     0,     0,     0,     0,     0,   516,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   403,     0,     0,   404,     0,   405,
       0,   406,   367,     0,     0,   367,   367,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,   407,     0,
       0,   367,     0,     0,     0,   367,     0,     0,     0,   367,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   489,
     489,   516,     0,  1412,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   489,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,     0,     0,     0,
     421,   485,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,     0,  1489,  1489,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   367,     0,     0,
       0,     0,   367,     0,     0,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,  1536,    47,     0,   367,     0,     0,  1545,
       0,     0,     0,     0,     0,   367,     0,     0,     0,   367,
       0,     0,     0,    58,  1000,     0,     0,     0,   487,   209,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   209,     0,     0,     0,     0,   516,     0,   209,  1573,
       0,   365,     0,     0,     0,     0,     0,   716,     0,   717,
     718,     0,     0,     0,  1031,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   209,     0,     0,
       0,     0,     0,     0,     0,  1542,     0,    75,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   -17,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,   365,     0,     0,     0,   403,
       0,     0,   404,     0,   405,     0,   406,  1637,  1638,     0,
     365,     0,     0,     0,     0,     0,     0,   489,     0,     0,
     489,   489,    58,   407,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1000,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   516,   408,   409,   455,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,     0,   417,   418,     0,     0,
       0,  2169,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1471,     0,
       0,     0,     0,  1033,   209,   419,     0,     0,    78,   420,
       0,     0,  1754,  1755,     0,   421,  1543,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,   403,
       0,     0,   404,     0,   405,     0,   406,     0,     0,     0,
       0,   516,     0,     0,     0,  1031,     0,     0,     0,     0,
       0,  1213,   209,   407,  1215,     0,  1216,  -249,  -249,  1217,
    1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  -345,  -345,  1229,  1230,  1231,  1232,  1233,  1234,  1235,
       0,  1236,     0,   408,   409,     0,   510,   411,  1237,  1238,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,  1239,   414,   415,   416,     0,   417,   418,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   455,     0,     0,
       0,     0,  1825,     0,  -249,  1240,     0,     0,    78,   420,
       0,     0,     0,   303,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,   367,     0,     0,     0,
    -189,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   209,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1871,     0,     0,     0,     0,     0,     0,
       0,  2169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1471,     0,
       0,     0,     0,     0,     0,     0,     0,   367,     0,     0,
     367,     0,     0,     0,     0,     0,   516,     0,     0,     0,
       0,     0,     0,  1902,     0,   367,  1904,     0,     0,   403,
       0,     0,   404,     0,   405,     0,   406,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1213,  1918,   407,  1215,     0,  1216,  -250,  -250,  1217,
    1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  -345,  -345,  1229,  1230,  1231,  1232,  1233,  1234,  1235,
       0,  1236,     0,   408,   409,     0,   510,   411,  1237,  1238,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,  1239,   414,   415,   416,     0,   417,   418,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1882,     0,     0,     0,     0,
       0,     0,     0,     0,  -250,  1240,     0,     0,    78,   420,
       0,     0,  1471,   303,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,     0,
    -189,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,   404,     0,   405,     0,
     406,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1213,     0,   407,  1215,     0,
    1216,     0,     0,  1217,  1218,  1219,  1220,  1221,  1222,  1223,
    1224,  1225,  1226,  1227,  1228,  -345,  -345,  1229,  1230,  1231,
    1232,  1233,  1234,  1235,     0,  1236,     0,   408,   409,  1000,
     510,   411,  1237,  1238,    65,    66,    67,    68,    69,    70,
      71,    72,   412,   413,   400,  1239,   414,   415,   416,     0,
     417,   418,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1240,
       0,     0,    78,   420,     0,     0,     0,   303,     0,   421,
      80,    81,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,     0,  -189,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    1212,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   403,     0,    46,   404,    47,   405,     0,   406,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,  1213,    58,  1214,  1215,     0,  1216,     0,
       0,  1217,  1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,
    1226,  1227,  1228,  -345,  -345,  1229,  1230,  1231,  1232,  1233,
    1234,  1235,     0,  1236,     0,   408,   409,    61,   510,   411,
    1237,  1238,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,  1239,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -3,  1240,     0,     0,
      78,  1241,     0,     0,     0,   303,     0,   421,    80,    81,
     422,   423,   424,   425,     0,     0,     0,     0,     0,     0,
       0,     0,  -189,     4,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,  1212,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   403,
       0,    46,   404,    47,   405,     0,   406,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,  1213,    58,  1214,  1215,     0,  1216,     0,     0,  1217,
    1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  -345,  -345,  1229,  1230,  1231,  1232,  1233,  1234,  1235,
       0,  1236,     0,   408,   409,    61,   510,   411,  1237,  1238,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,  1239,   414,   415,   416,     0,   417,   418,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1240,     0,     0,    78,  1241,
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
       0,     0,     0,   419,  1695,  1696,    78,  1241,     0,     0,
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
       0,   419,  1695,     0,    78,  1241,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,  1690,    78,  1241,     0,     0,     0,     0,     0,   421,
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
       0,    78,  1241,     0,     0,     0,     0,     0,   421,    80,
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
       0,     0,     0,     0,   419,     0,     0,    78,  1296,     0,
       0,     0,     0,     0,   421,  1297,    81,   422,   423,   424,
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
     263,     0,     0,     0,  -822,     0,     0,    80,    81,   262,
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
    -413,     0,     0,     0,    78,    79,     0,  1447,     0,  1448,
       0,     0,    80,    81,  1449,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1450,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1451,     0,
       0,     0,    78,  1005,     0,  1447,     0,  1448,     0,     0,
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
       0,     0,     0,     0,     0,     0,  1646,     0,     0,     0,
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
       0,     0,     0,     0,  1647,     0,     0,     0,    78,  1005,
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
       0,     0,  1648,     0,     0,     0,    78,  1005,     0,     0,
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
       0,     0,  1103,    77,  -685,    78,   665,     0,     0,     0,
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
     263,     0,     0,     0,  -826,     0,     0,    80,    81,   186,
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
       0,     0,     0,     0,   664,     0,  -685,    78,   665,     0,
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
    -685,    78,   550,     0,     0,     0,     0,     0,     0,    80,
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
       0,     0,     0,     0,  -693,    78,   913,     0,     0,     0,
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
       0,     0,     0,    74,     0,    75,  1617,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   913,     0,
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
       0,     0,     0,     0,     0,    78,   913,     0,     0,     0,
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
    1471,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,     0,   404,     0,   405,     0,   406,     0,
       0,     0,     0,    78,   263,     0,     0,     0,     0,     0,
       0,    80,    81,  1213,     0,   407,  1215,     0,  1216,  1935,
    1936,  1217,  1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,
    1226,  1227,  1228,     0,     0,  1229,  1230,  1231,  1232,  1233,
    1234,  1235,     0,  1236,     0,   408,   409,     0,   510,   411,
    1237,  1238,    65,    66,    67,    68,    69,    70,    71,    72,
     412,   413,   400,  1239,   414,   415,   416,     0,   417,   418,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,  1471,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1240,     0,     0,
      78,   420,     0,     0,     0,   303,     0,   421,    80,    81,
     422,   423,   424,   425,   403,     0,     0,   404,     0,   405,
       0,   406,  -189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1213,     0,   407,  1215,
       0,  1216,     0,     0,  1217,  1218,  1219,  1220,  1221,  1222,
    1223,  1224,  1225,  1226,  1227,  1228,     0,     0,  1229,  1230,
    1231,  1232,  1233,  1234,  1235,     0,  1236,     0,   408,   409,
       0,   510,   411,  1237,  1238,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,  1239,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1240,     0,     0,    78,   420,     0,     0,     0,   303,     0,
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
       0,     0,  -416,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,     0,     0,  -416,   307,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -417,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,     0,     0,  -417,   307,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,    14,
      15,    16,    17,    18,    19,   725,    20,   726,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    63,    64,   403,     0,    46,   404,    47,
     405,     0,   406,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   407,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   727,     0,     0,     0,     0,  1228,     0,  -345,     0,
       0,     0,     0,    78,     0,     0,     0,     0,  -416,   408,
     409,     0,   410,   411,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   412,   413,   400,     0,   414,   415,
     416,     0,   417,   418,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1240,     0,     0,    78,   728,     0,     0,     0,   303,
       0,   421,    80,    81,   729,   730,   424,   425,    14,    15,
      16,    17,    18,    19,   725,    20,   726,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   403,     0,    46,   404,    47,   405,
       0,   406,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   407,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     727,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   728,     0,     0,     0,   303,     0,
     421,    80,    81,   729,   730,   424,   425,    14,    15,    16,
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
       0,   450,    78,   451,     0,     0,     0,     0,     0,   421,
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
       0,    78,   451,     0,     0,     0,   303,     0,   421,    80,
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
      78,   728,     0,     0,     0,   303,     0,   421,    80,    81,
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
     451,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,    14,    15,    16,    17,    18,    19,     0,
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
       0,     0,     0,     0,   419,     0,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,   262,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -491,  -491,     0,  -491,    46,
       0,    47,     0,  -491,     0,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,   351,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   609,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -685,    78,
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
    -824,     0,     0,    80,    81,    14,    15,    16,    17,    18,
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
      78,   208,     0,     0,     0,     0,     0,     0,    80,    81,
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
       0,     0,     0,    46,     0,    47,     0,     0,     0,   351,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
     478,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   851,     0,     0,
      78,   479,     0,     0,     0,     0,     0,     0,    80,    81,
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
     478,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   479,     0,     0,     0,     0,     0,     0,    80,    81,
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
       0,     0,    75,   609,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,  -685,    78,    20,     0,    21,    22,    23,
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
      76,    77,     0,    78,   332,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    75,  1206,     0,     0,     0,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,    78,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   351,    49,    50,    51,    52,    53,    54,
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
       0,     0,     0,     0,   851,     0,     0,    78,   479,     0,
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
      47,     0,    63,    64,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   851,
       0,     0,    78,   479,     0,    63,    64,     0,     0,     0,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1012,    78,  1005,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
    1561,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,  1005,     0,     0,     0,     0,     0,     0,    80,    81,
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
       0,     0,     0,     0,     0,    78,   315,     0,    63,    64,
       0,     0,     0,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   208,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   351,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,   351,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   353,     0,    63,    64,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   315,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
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
       0,    78,   479,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -491,  -491,     0,  -491,    46,     0,
      47,     0,  -491,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,   351,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   332,
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
       0,     0,     0,    78,  1005,     0,    63,    64,     0,     0,
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
       0,    63,    64,   351,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   479,     0,    63,    64,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,  1005,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
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
       0,     0,    14,    15,    16,    17,    18,    80,    81,    20,
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
      14,    15,    16,    17,    18,    80,    81,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -491,  -491,     0,  -491,    46,     0,
      47,     0,  -491,     0,     0,     0,     0,     0,     0,     0,
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
      44,    45,     0,     0,   403,     0,    46,   404,    47,   405,
       0,   406,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
       0,   417,   418,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,     0,     0,     0,
     421,   485,    81,   422,   423,   424,   425,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   403,     0,    46,   404,
      47,   405,     0,   406,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,     0,   417,   418,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,    14,
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
       0,    63,    64,   351,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,    78,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,   187,
     403,   188,   189,   404,     0,   405,     0,   406,  1950,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     716,     0,   717,   718,   408,   409,     0,   410,   411,  1951,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,     0,   417,   418,   403,
      75,     0,   404,     0,   405,    74,   406,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1692,  1693,  1694,   407,     0,     0,   419,  1952,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,   409,     0,   410,   411,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   412,   413,
     400,     0,   414,   415,   416,     0,   417,   418,   403,     0,
       0,   404,     0,   405,    74,   406,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1692,
    1693,  1694,   407,     0,     0,   419,  1857,     0,    78,   420,
       0,     0,     0,     0,     0,   421,    80,    81,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   510,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,    77,     0,   511,   512,     0,
       0,     0,   513,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,  1344,     0,    78,   420,     0,     0,     0,  1345,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,     0,     0,    78,
     420,     0,     0,     0,   513,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,   996,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,  1030,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,     0,     0,    78,   420,     0,
       0,     0,   303,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,  1071,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,     0,     0,    78,
     420,     0,     0,     0,  1481,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,  1572,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,     0,
       0,    78,   420,     0,     0,     0,  1765,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,     0,  1941,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,  1946,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,  1950,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,  1957,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,  1951,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,     0,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,  2034,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,  2036,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,  2080,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,  2082,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,  2084,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,  2087,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,  2089,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,  2128,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,   403,   417,   418,   404,
       0,   405,     0,   406,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     407,     0,     0,     0,     0,     0,   419,  2130,     0,    78,
     420,     0,     0,     0,     0,     0,   421,    80,    81,   422,
     423,   424,   425,     0,     0,     0,     0,     0,     0,     0,
     408,   409,     0,   410,   411,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   412,   413,   400,     0,   414,
     415,   416,   403,   417,   418,   404,     0,   405,     0,   406,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,     0,     0,
       0,     0,   419,  2132,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   419,  2153,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   419,  2155,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,  2157,     0,    78,   420,     0,     0,     0,     0,     0,
     421,    80,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
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
       0,     0,   707,     0,     0,    78,   420,     0,     0,     0,
       0,     0,   421,    80,    81,   422,   423,   424,   425,     0,
       0,     0,     0,     0,     0,     0,   408,   409,     0,   410,
     411,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   412,   413,   400,     0,   414,   415,   416,   403,   417,
     418,   404,     0,   405,     0,   406,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   407,     0,     0,     0,     0,     0,   713,     0,
       0,    78,   420,     0,     0,     0,     0,     0,   421,    80,
      81,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,   408,   409,     0,   410,   411,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   412,   413,   400,
       0,   414,   415,   416,   403,   417,   418,   404,     0,   405,
       0,   406,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   407,     0,
       0,     0,     0,     0,   722,     0,     0,    78,   420,     0,
       0,     0,     0,     0,   421,    80,    81,   422,   423,   424,
     425,     0,     0,     0,     0,     0,     0,     0,   408,   409,
       0,   410,   411,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   412,   413,   400,     0,   414,   415,   416,
     403,   417,   418,   404,     0,   405,     0,   406,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,     0,     0,     0,     0,
     419,     0,     0,    78,   420,     0,     0,     0,     0,     0,
     421,   937,    81,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,   408,   409,     0,   410,   411,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   412,
     413,   400,     0,   414,   415,   416,     0,   417,   418,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,    78,
     420,     0,     0,     0,     0,     0,   421,   485,    81,   422,
     423,   424,   425
};

static const yytype_int16 yycheck[] =
{
       1,    76,   180,     4,    85,     1,   168,   235,   524,    98,
     751,    76,   261,   754,   950,   230,   223,    76,     4,   677,
    1240,  1196,  1258,  1935,   419,    78,   185,   168,   139,   238,
      76,   240,   356,   276,   185,  1808,   668,     1,   247,   235,
     421,   256,  1240,   235,    76,   302,  1292,  1293,  1046,    59,
     207,  1808,   143,   832,   934,    56,    57,   982,    59,   245,
     170,   570,   571,    59,    76,   990,   513,   830,   235,     1,
     845,   152,   847,  1808,   235,    76,   170,  1075,     1,   732,
     798,    75,   934,     1,    85,   664,   244,   273,   582,    85,
     235,    75,    93,   251,  1363,    59,   324,    98,   284,   593,
     101,   832,    98,   922,   105,   101,   194,  1702,   830,   105,
     319,   320,   112,    76,   272,   206,   135,   830,  1939,   105,
     235,   100,   235,   830,   943,   283,  1193,    59,   324,   120,
      89,   196,   324,  1200,   245,   135,    59,   196,   235,   149,
    1809,    59,    84,   144,   638,  1143,   147,   236,   149,   111,
     196,   664,   664,   149,   155,     1,  2068,   324,   125,   178,
     235,   162,   273,   324,   196,     1,     1,   348,   492,   170,
     235,     0,   134,   284,    58,   139,   235,    61,    62,   324,
      64,   182,   207,   264,   196,   149,   180,   235,  1205,   235,
     699,   158,    72,   194,   195,   196,   180,    72,   309,   195,
     832,   180,   711,   235,   163,   206,   833,   388,   101,   324,
     211,   324,   839,   155,  1281,   211,     0,   149,   184,   120,
     221,  1205,   311,   235,    59,   226,   149,   324,   229,   230,
     231,   149,    10,   196,   235,   236,   325,     0,   370,   154,
     236,  1223,   374,   155,   168,   298,   161,   419,  1698,   324,
       1,   830,   333,     4,  1923,   256,   136,  1852,   159,   324,
     137,   136,   155,   264,   155,   324,   137,   377,   264,    77,
      78,   155,   235,   274,   275,   155,   324,   278,   324,   159,
     155,   245,   278,   377,   285,    77,    78,   611,  1508,   155,
     384,  1511,  1512,  1034,   171,   155,   805,  2118,   299,   300,
     171,   302,   559,   531,   139,  1574,   307,   395,    59,   273,
     311,   554,   519,   179,   149,   311,   162,   830,   830,   562,
     284,    61,    62,   324,   325,   161,   162,  2100,   709,   325,
    2151,  1110,   656,   334,  1214,   531,   968,    20,   112,   531,
    1103,   342,   343,  2100,     1,   309,   347,     4,   155,    10,
     101,    72,    90,   568,   105,   163,   680,   752,    62,   574,
      72,   135,  1214,   687,   531,  2100,  1084,  1247,  1021,  1354,
     531,   163,   453,    98,  1441,  2044,   377,   634,  1197,  1110,
     490,  1103,  1557,   157,     1,   386,   531,   165,   389,  1314,
    1103,   482,   170,     4,   395,  1247,  1103,   101,   149,   155,
     558,   670,    59,   631,  1205,   163,   592,  1857,  1858,   113,
     245,   115,   503,   117,   149,   136,   531,   155,   531,   155,
     577,   137,   180,   163,   136,   919,   584,  1444,  1445,  1446,
     354,   947,  2101,   591,   531,   631,   171,   595,   273,   631,
     180,    72,    59,   155,   285,    56,    57,   159,   105,   284,
     166,   167,   156,   155,   543,   159,   160,    89,   511,   300,
    1444,  1445,  1446,   464,   631,   161,   531,   163,   464,  2138,
     631,   354,   531,   163,   309,   108,   109,  1104,  1110,    75,
     154,   592,    93,   531,   101,   531,   631,   370,   489,   490,
     180,   374,   149,   154,   635,    91,  2071,   654,    75,   531,
     501,   502,  1952,  1953,   165,   136,   180,   211,   160,   170,
     511,   236,   513,    90,   110,  1735,   631,    98,   631,   531,
       1,   653,  2097,     4,  1103,   158,    75,   278,   159,   161,
     531,   163,   149,   144,   631,   707,   147,  1735,   710,   163,
     797,   713,   543,    92,   633,  2120,   170,   543,   161,   155,
     722,   162,   577,   725,   726,   727,   813,   163,   690,   170,
     817,   161,   486,   161,   170,   834,   631,   568,   531,   838,
     180,   171,   631,   574,   278,   576,   280,   281,    59,  1577,
     849,   850,   180,   631,    69,   631,   311,   161,   858,  1469,
    1103,  1103,    72,  1473,  1474,   206,   161,  1582,  1583,  1584,
     325,   695,   161,   486,    85,   163,   180,  1487,   312,   155,
     635,   157,   170,   317,    72,   226,   161,    72,   229,   323,
     231,   180,  1397,  1548,   781,  1400,  1401,   637,   592,   654,
     631,  1078,   633,   957,   155,   180,   637,   633,   639,  1885,
    1298,   637,   163,  1444,  1445,  1446,    72,   648,   161,   152,
     157,   652,   356,   860,   161,   236,   136,   361,   139,   363,
     163,   278,   143,   274,   275,   149,   150,   151,   149,   826,
     161,   152,   161,   637,   285,   155,   179,  1304,   136,   159,
     895,   136,   171,   684,   843,   847,    72,   171,   299,   300,
     161,   302,   843,   825,   159,   312,   307,   155,   857,   164,
     155,   159,   703,   159,   159,   637,   410,   161,   164,   180,
     136,   635,   180,     1,   637,  1697,   130,   131,   988,   637,
    1702,  1462,   832,   334,   161,   206,   180,   780,   161,   155,
     311,   342,   343,   159,   171,   157,   347,   157,   171,   356,
     162,   161,   161,   744,   325,   746,  1982,   748,   161,   161,
     136,   752,   171,   677,   755,   161,   781,   592,   161,  1386,
     464,   175,   176,   921,   245,   155,   377,   180,   180,   155,
     653,    59,   162,   159,   180,   386,     3,   180,   389,   780,
     156,   262,  1291,   264,  1782,   158,  1784,   163,   492,   134,
     494,   495,   273,   157,   677,    13,    14,    15,    16,    17,
       3,   826,   637,   284,   168,   169,   510,   690,  1744,   161,
     155,  1649,   155,   779,   159,   161,  1654,   105,   543,   654,
     845,   166,   167,   955,   825,   171,   307,   157,   309,   830,
      62,   832,   162,  1090,  1759,   154,    72,  1094,   163,   543,
     157,    72,   161,   844,   161,  1240,   108,   109,   852,   853,
     157,   139,   333,   854,    72,   162,  1113,    72,  1127,   860,
     166,   149,   863,  1120,   568,   869,   179,   173,   174,   573,
    1852,   575,    85,   157,   106,   492,  1017,   161,   489,   111,
     163,     1,   114,   157,   116,    98,   637,   161,   101,   983,
     501,   502,   105,   597,   895,   599,   600,   157,  1345,  1142,
     136,   155,   162,    47,    48,   136,    50,   611,   633,  1166,
     157,    55,  1071,  1170,   177,   162,   134,  1174,   136,   155,
     624,   136,    72,   159,   155,  1016,   134,  1559,   159,   633,
     896,   932,   933,   934,   155,   939,   157,   155,   159,    59,
     155,   159,   825,  1150,   159,   155,   155,   155,   166,   167,
     159,   159,   656,    72,   658,   659,   120,   245,   166,   167,
     946,   386,   543,   157,   389,   576,   157,  1949,  1484,  1178,
     934,   179,   453,   157,    72,  1855,   680,   681,  1302,    72,
     637,  1963,   195,   687,    72,   273,   136,   157,   989,   159,
     157,   826,  1017,  1152,   611,   157,   284,   157,   211,  1109,
    1110,   482,   934,   160,   155,   155,   157,   624,   159,   159,
     155,   934,   108,   109,    72,   155,   934,   136,   161,   139,
     637,   309,   503,   236,   155,   313,    22,    58,   639,   149,
      61,    62,   155,    64,  1481,   267,   155,   648,   136,  1040,
     159,   652,  1312,   136,   161,  1046,  2028,   101,   136,  1319,
     155,   264,   633,   155,  1449,   158,   159,   155,  1307,   162,
     160,   159,   155,   680,    76,   278,   159,   155,  1240,   163,
    1285,   159,   955,   684,  1075,    89,   157,  1078,   136,   155,
     161,   157,   314,   159,   157,   158,    98,   154,    13,    14,
      15,    16,    17,  1017,    72,  1099,  1100,   155,   163,   934,
    1370,   159,  1103,   180,    72,   149,   150,   151,  1109,  1110,
     163,   592,   157,   155,  1292,  1293,   161,  1279,   113,   114,
     115,   116,   117,  1570,  1205,   245,   358,   171,   360,   157,
     362,   160,  2012,   744,   170,   746,   180,   748,  1279,   163,
    1656,   752,  1143,   155,   755,   155,   179,    72,   157,   159,
    1154,   157,   161,   273,   155,   161,   637,   157,   136,  1416,
    1332,  2041,     3,  1420,   284,   120,    78,  1424,   136,   780,
     168,   169,    13,    14,    15,    16,    17,   155,   410,   155,
     155,   159,   157,   934,   159,  1326,  1327,   155,  1431,   309,
     155,   159,  2072,   155,   106,   946,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   172,  1366,   157,   134,
    1196,   136,   161,   155,   825,   161,  1217,  1311,   230,  1220,
    1221,  1222,   157,   235,   236,   155,   161,   167,  1229,   159,
     155,    72,   155,   844,   159,  1397,   159,  1978,   165,  1401,
     177,   166,   167,   854,   256,   155,  1247,  1406,  1407,   159,
    1214,   464,  1253,   957,   134,  1406,  1407,    13,    14,    15,
      16,    17,    18,   158,   968,   157,   157,  1268,   180,   161,
    1271,  1272,   157,   977,  1275,  1271,  1272,   934,   510,  1549,
    1537,  1282,  1214,  1247,  1285,   157,  1272,   157,   157,   946,
     157,  1214,   161,   157,   161,   136,  1214,   161,   157,   311,
     157,  1571,  1327,   159,   592,    13,    14,    15,    16,    17,
      18,   155,   324,   325,   160,  1247,  2105,   934,  1765,   155,
    2109,   932,   933,   934,  1247,    13,   161,  1328,   157,  1247,
     543,   157,   161,   157,  1591,   161,   137,   161,   157,   137,
     957,   157,   161,  1600,  1345,   161,   157,  1604,   157,   637,
     161,   134,   161,  1354,   161,  1279,   134,   589,  1469,   759,
     760,   761,  1473,  1444,  1445,  1446,  1455,   157,  1449,  1450,
     162,   161,   155,   162,  1298,  1400,   159,   155,   989,  1214,
    1084,   159,   161,   166,   167,   155,  1387,   155,   166,   167,
     157,  1532,   157,   179,  1553,   157,   161,     3,   157,   161,
      88,  1469,  1553,  1327,   157,  1473,  1110,    13,    14,    15,
      16,    17,  1247,   157,   157,  1298,   157,   161,   106,   157,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     157,   157,  1660,   157,   161,   161,   157,  1707,    13,    14,
      15,    16,    17,   157,   157,  1196,   179,   161,  1646,  1647,
    1648,   135,   159,   934,  1455,  1202,  1203,  1204,  1459,  1460,
     160,  1447,   163,  1214,  1660,   946,    72,   155,  1660,  1668,
    1669,  1670,   592,    13,    14,    15,    16,    17,   490,   106,
    1481,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,  1761,   163,  1660,   128,   129,  1247,    72,   163,  1660,
     132,   133,   163,  1504,  1505,  1469,  1776,  1532,   163,  1473,
    1474,   160,   161,  1514,   161,  1660,   157,   637,  1514,   531,
    1271,  1272,    70,  1487,    13,    14,    15,    16,    17,    18,
     136,   543,    72,   166,   167,  1016,   180,  1469,   160,  1196,
     155,  1473,  1474,   227,    78,  1660,   160,  1660,   160,   161,
    1551,   160,   161,   180,    18,  1487,   568,  1214,   179,   134,
    1649,   136,   574,  1660,   207,  1654,   163,  1271,   163,  1570,
     180,  1557,   157,  1662,   160,   161,  1577,   160,   161,   157,
     155,  1582,  1583,  1584,   159,  1660,   766,   767,   768,   769,
    1247,   166,   167,   160,   161,  1823,   136,  1214,  1302,   161,
     162,   180,  1774,   163,  1308,   163,  1217,   160,  1532,  1220,
    1221,  1222,  1660,   160,   161,  1272,   160,   161,  1229,   631,
      18,   633,    91,    92,   308,  1895,   157,  1823,   160,  1899,
    1247,  1823,   160,   161,  1469,   160,  1247,   160,  1473,  1474,
     160,   161,  1253,   149,   150,   151,   934,   154,  1649,   160,
     161,    22,  1487,  1654,  1271,   161,  1823,  1268,   157,  1660,
     157,  1662,  1823,   154,  1275,   171,   160,   161,   160,   161,
    1671,  1282,   157,  1888,   180,   149,   150,   151,  1823,    13,
      14,    15,    16,    17,    18,  1302,   157,   161,   407,  1690,
     160,   161,   160,   161,   157,  1696,  1447,   171,   160,   161,
     157,  1687,   160,   161,   160,   161,   180,  1885,  1823,   157,
    1823,   161,   162,   432,   433,  1196,   160,  1328,  1469,   160,
     161,  2045,  1473,  1474,  1205,   157,  1823,   160,   161,   157,
    1455,    77,    78,  1214,   453,   419,  1487,  1826,   123,  1740,
     125,   126,   127,    13,    14,    15,    16,    17,   161,   162,
    1355,  1356,  1456,   157,   762,   763,   157,   764,   765,  1725,
    1746,  1511,  1512,  1514,  1765,   154,  1247,   486,   770,   771,
     155,  1669,  1670,   158,   159,  1823,  1935,   163,   163,   164,
     163,  1782,   163,  1784,  1935,    70,   161,   157,   179,   157,
    1447,   157,   157,   157,   163,    13,    14,    15,    16,    17,
     161,   163,    72,  2060,   157,   157,  1557,   157,   157,   160,
    1514,   154,  1469,   161,   934,   160,  1473,  1474,   830,   161,
     832,   157,  1823,   161,   157,  1826,   157,   157,   157,  2044,
    1487,   157,   157,   157,  1835,  1836,   157,   160,   157,   157,
     157,  1842,   157,  1932,   157,   157,   157,   157,  1459,  1460,
     157,   157,  1469,  1854,    72,   160,  1473,  1474,   161,   157,
     161,   161,   157,  1864,   161,  1866,   136,   137,   154,    14,
    1487,   155,  2100,   557,  1455,   155,  1877,   155,  1879,  1880,
    1881,   565,   155,   895,   155,   162,  1887,  1888,   155,   155,
    1971,  1855,   155,  1504,  1505,   163,   161,  1514,   180,   583,
    1557,   162,   160,   160,  2100,   180,  1872,  2066,  2100,  2068,
     594,   180,   154,   154,   163,  2066,   161,  2068,   136,   137,
     179,   157,   157,  1855,  1649,   180,  1214,   157,   157,  1654,
     157,  1932,   160,  2100,   577,  1686,  1687,  1662,  1939,  2100,
    1551,   160,  1943,   157,   157,   161,   154,  1948,  2107,   160,
     157,   157,   157,   157,   154,  2100,  2107,   160,   155,  1247,
     155,  2042,    80,  1444,  1445,  1446,  1447,  1448,  1449,  1450,
     180,   180,    92,   154,    90,   155,   155,  2136,   180,   180,
     157,   180,  1686,   154,  1272,  2100,   180,  2100,  1469,   180,
     154,   161,  1473,  1474,   161,  1746,   154,   160,   160,   160,
     160,   154,  2003,  2100,   163,   157,  1487,   162,   162,  1975,
     123,   654,  2101,   154,  2015,   157,   160,   157,  2019,  2178,
    1855,   157,   160,   707,   157,  2100,   157,  2178,   157,   713,
    1687,   157,  2033,   154,   180,  2100,   161,   154,   722,   758,
     162,  2100,   157,  2044,   155,  2046,   155,   161,  2012,  2138,
     157,   155,  2100,   180,  2100,   160,   157,   741,  1271,  1272,
    1671,   160,   160,   154,   157,   154,   160,   157,  1649,  1686,
      18,   157,   157,  1654,  2075,    75,  1557,  2041,   180,  1690,
    2012,  1662,    75,   180,   180,  1696,   154,  1838,  1971,  1746,
     155,  1103,   180,   157,  1214,   155,   157,  1109,  1110,  2100,
    2101,  1826,   160,   160,  1855,  2101,   154,   154,  2072,  2041,
     154,   157,  2113,    61,    62,    63,    64,  2118,   157,   159,
      75,    75,  1826,   108,   158,   180,   171,  1247,   171,  1740,
       1,    75,   162,     4,  1838,   154,   154,  2138,   781,  2105,
    2072,   154,  2138,  2109,  2110,  2146,   156,   180,  2149,   180,
    2151,   171,   154,   162,   171,   106,   155,   161,   106,  2042,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,  2172,   156,   171,  2140,   171,    75,  2012,   157,   180,
     160,  1469,  2183,   826,   156,  1473,  1474,   157,    59,   180,
     157,  2192,   154,   162,   154,  1774,   157,  2163,  1855,  1487,
     157,  2167,   845,   155,   847,    76,  2041,  1932,   851,   852,
     853,   159,   180,  1323,    85,   180,  2182,   180,   902,  1923,
     772,  1838,   731,   773,  1835,  1836,   869,    98,   774,   452,
     101,  1842,   916,   775,   105,   776,   920,  2072,  1855,  1473,
     924,  1235,  1247,  1854,  2151,  1826,  2068,  1863,  2097,  1487,
    1956,  1855,  2057,  1864,  2135,  1866,  1734,  1718,  1718,  2042,
    2110,  2012,  2167,    49,   207,  1746,  1877,   114,  1879,  1880,
    1881,  2041,   143,  1285,   269,  2001,  1887,  1932,   149,  1450,
    1268,   152,  1275,   860,   155,   156,  1316,   930,  1764,   520,
    2041,   648,  1521,  1012,  1746,     0,   939,   168,   106,  1018,
     703,  1514,   110,   111,   112,   113,   114,   115,   116,   117,
    1029,     4,     5,     6,     7,     8,     9,    10,    11,    12,
     302,  2072,   193,  1638,   195,   196,    -1,    -1,  1939,    -1,
     797,   797,  1943,    -1,    -1,   206,   207,  1948,   797,    -1,
     211,  2045,    -1,    -1,    -1,    -1,    -1,   155,   156,  1469,
      -1,  1932,    -1,  1473,  1474,  2012,    -1,    -1,    -1,   230,
       3,    -1,    -1,    -1,   235,   236,    -1,  1487,    -1,    -1,
      -1,    -1,    65,    -1,  1855,  1387,  2101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2041,   256,    -1,    -1,    -1,    -1,
      -1,    -1,  2003,   264,    -1,  2012,    -1,    -1,    -1,  1687,
      -1,    -1,    -1,    -1,  2015,    -1,    -1,   278,  2019,    -1,
      -1,   354,    -1,  2138,   357,  2072,    -1,    -1,    -1,    -1,
      -1,    -1,  2033,    -1,  2041,    -1,    -1,   370,    -1,    -1,
      -1,   374,    -1,    -1,  2138,  2046,    18,    -1,    -1,    -1,
     311,    -1,    -1,  1455,    -1,    -1,   317,    -1,    -1,    -1,
      -1,    -1,   323,   324,   325,  2072,  1099,  1100,    -1,    -1,
      -1,    -1,   333,   106,  2075,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    57,    58,    59,    60,    61,
      62,    63,    64,   354,   355,   356,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,  1214,    -1,    -1,    -1,   370,
      -1,    85,  2113,   374,    -1,    -1,    -1,  2118,    -1,    -1,
      -1,  1154,   155,   156,    -1,  1199,    -1,    -1,    -1,   162,
    2101,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,  1213,
      -1,  2012,    -1,    -1,    -1,  2146,    -1,    -1,  2149,    -1,
    2151,    -1,    -1,   486,    -1,    -1,    -1,    -1,   419,  1233,
      -1,    -1,    -1,    -1,    -1,    -1,  1240,  2138,    -1,    -1,
    2041,  2172,    -1,    72,    -1,    -1,    13,  1855,   152,    -1,
      -1,    -1,  2183,    -1,    -1,    -1,    -1,   559,    -1,  1298,
      -1,  2192,   453,    -1,    -1,   456,    13,    14,    15,    16,
      17,  2072,    -1,   464,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   193,
      -1,   482,    -1,    -1,    -1,   486,  1335,  1336,  1337,   490,
      -1,   492,    -1,  1342,  1343,   134,    -1,   136,    -1,    -1,
      -1,    -1,   503,    -1,   577,    -1,    -1,  1649,    -1,    -1,
      -1,    88,  1654,    -1,    -1,    72,   155,   156,  1660,    -1,
    1662,    -1,   634,   524,    -1,    -1,    -1,   166,   167,   106,
     531,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   543,    -1,    62,    -1,    -1,    -1,    -1,   106,
     264,    -1,   664,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   635,    -1,    -1,    -1,    -1,   568,    -1,   570,
     571,   246,    -1,   574,    -1,    -1,   577,   134,    -1,   136,
     653,   654,   106,   101,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,  2012,   113,   114,    -1,   155,   156,
      -1,    -1,    -1,    -1,   677,  1855,    -1,    -1,    -1,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,   690,    -1,   333,
      -1,    89,    -1,  2041,  1397,    -1,    -1,  1400,  1401,    -1,
     631,    -1,   633,    -1,   635,    -1,   637,   106,   156,    -1,
     354,   110,   111,   112,   113,   114,   115,   116,   117,  1463,
    1464,    -1,   653,   654,  2072,   656,   180,    -1,    -1,    -1,
      -1,    -1,   130,   664,   106,    -1,   108,   668,   110,   111,
     112,   113,   114,   115,   116,   117,   677,    -1,    -1,    -1,
      -1,  1823,    -1,    -1,  1826,   797,   687,   156,    -1,   690,
     159,    -1,    -1,   211,    -1,    -1,    -1,    -1,   699,  1513,
      -1,   813,    -1,    -1,    -1,   817,   707,    -1,   781,   710,
     711,    -1,   713,    -1,    -1,    -1,    -1,    -1,   830,    -1,
      -1,   722,    -1,    -1,   725,   726,   727,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   453,
      -1,    -1,    -1,    -1,    -1,    -1,  1888,    -1,    -1,    -1,
      -1,    -1,   825,   826,    -1,    -1,    -1,    -1,    -1,   106,
     278,    -1,  2012,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   486,    -1,    13,    14,    15,    16,    17,    -1,
     781,   456,    -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,
    1932,  2041,    -1,    -1,   312,    -1,   797,   798,   473,   317,
      -1,   476,    -1,    -1,   805,   323,    -1,    -1,    -1,    -1,
     524,    -1,   159,    -1,    -1,    -1,    -1,    -1,   930,    -1,
      -1,    -1,  2072,    -1,   825,   826,    -1,    -1,    -1,   830,
      -1,   832,    -1,    72,    -1,    -1,    -1,    -1,   356,    -1,
      -1,    -1,    -1,    -1,   845,    -1,   847,    -1,    -1,    -1,
     851,   852,   853,   321,    -1,    57,   570,   571,    -1,    -1,
      -1,   536,    -1,    65,    66,    67,    68,   106,   869,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,  1730,   955,    -1,    -1,   958,    -1,    -1,    -1,    -1,
      -1,    -1,   410,    -1,   895,   134,    -1,   136,    -1,    -1,
      -1,    -1,  2044,  1717,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   155,   156,    -1,    -1,
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,
      -1,    -1,    -1,   934,    -1,    -1,     1,    -1,   939,     4,
      -1,    -1,    -1,    -1,  1017,   946,   947,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   955,    -1,   957,   159,  2100,  2101,
      -1,    -1,    -1,   677,    -1,    -1,    -1,   968,    -1,    -1,
      -1,    -1,    -1,    -1,   492,   177,    -1,    -1,  1090,    -1,
      -1,    -1,  1094,    -1,   221,   699,    -1,    -1,    -1,    -1,
      -1,  1103,   510,   707,    59,    -1,  2138,   711,    -1,   713,
      -1,  1113,    -1,    -1,    -1,    72,    -1,    -1,  1120,    -1,
      -1,    -1,    -1,    -1,    -1,  1016,  1017,   485,    -1,   487,
      85,    -1,    -1,    -1,    -1,    -1,    -1,   106,   496,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   106,
     105,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,    -1,  1166,    -1,    -1,   575,  1170,    -1,
      -1,    -1,  1174,    -1,    -1,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    72,   139,  1217,    -1,   156,   143,   597,
     159,    -1,    -1,  1084,   149,    -1,    -1,  1229,   155,   156,
      -1,   805,    -1,   611,    -1,    -1,    -1,    -1,  1099,  1100,
      -1,    -1,  1103,   168,    -1,    -1,   624,   106,    -1,  1110,
      -1,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,   797,   798,    -1,    -1,    -1,    -1,  1977,    -1,
     195,    -1,    -1,   808,    -1,   134,   811,   136,   656,   106,
      -1,   206,   207,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,  1154,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,   680,    -1,    -1,    -1,    -1,   166,   167,   687,
      -1,   236,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
     245,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   256,    -1,    -1,    -1,  1196,   261,   262,    -1,   264,
     875,    -1,    -1,    -1,  1205,    -1,    -1,   882,   273,    -1,
      -1,   886,    -1,  1214,    -1,   890,    -1,    -1,    -1,   284,
      -1,    -1,   287,    -1,    -1,  1298,   291,    -1,    -1,    -1,
      -1,   296,    -1,   947,    -1,    -1,    -1,   302,    -1,  1240,
      -1,    -1,   307,   170,   309,    -1,  1247,    -1,   313,    -1,
      -1,    -1,    -1,    -1,  1327,    -1,    -1,   106,    -1,    -1,
     325,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    1271,  1272,    -1,   122,   511,   124,   513,    -1,  1279,    -1,
      -1,    -1,    -1,    -1,  1285,    -1,    -1,    -1,    -1,   354,
    1291,    -1,   357,    -1,    -1,    -1,    -1,  1298,    -1,    -1,
      -1,    -1,    -1,    -1,  1416,   370,    -1,   156,  1420,   374,
     159,    -1,  1424,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,  1327,    -1,    -1,    -1,
     106,  1332,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,    72,   106,  2162,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
    2174,    -1,  1504,  1505,    -1,   158,     4,     5,     6,     7,
       8,     9,    10,    11,    12,   843,    -1,   170,    -1,   155,
     106,   899,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,  1397,    -1,    -1,  1400,
    1401,    -1,    -1,    -1,   163,    -1,    -1,    -1,   134,  1084,
     136,    -1,    -1,    -1,    -1,    -1,    -1,   482,    -1,    -1,
      -1,   486,    -1,    -1,    -1,  1537,    -1,    -1,    -1,   155,
     156,    -1,    -1,   159,    -1,    -1,    -1,    -1,   503,   957,
     166,   167,    -1,  1444,  1445,  1446,  1447,    -1,  1449,  1450,
      -1,    -1,    -1,    -1,  1455,  1456,    -1,    -1,    -1,  1532,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,  1469,   937,
     938,    -1,  1473,  1474,    -1,    -1,    -1,    -1,    -1,  1591,
      72,    -1,  1157,  1484,    -1,  1160,  1487,    -1,  1600,  1164,
      -1,    -1,  1604,    -1,   559,    -1,    -1,    -1,    -1,    -1,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   577,  1514,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   592,   134,   106,
     136,  1532,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   134,    -1,   136,   122,    -1,   124,  1690,   155,
     156,    -1,    -1,    -1,  1696,    -1,  1557,    -1,  1559,    -1,
     166,   167,    -1,   155,   156,  1033,    -1,    -1,    -1,   634,
     635,    -1,   637,    -1,   166,   167,    -1,  1291,    -1,   156,
      -1,    -1,   159,    -1,  1298,    -1,    -1,    -1,   653,   654,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1740,   664,
      -1,    -1,    -1,   668,    -1,    -1,    -1,    -1,    -1,  1077,
      -1,    -1,   677,    -1,    -1,    -1,    -1,   682,    -1,    -1,
      -1,    -1,    -1,   860,    -1,   690,   863,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,  1649,  1650,
      -1,    -1,    -1,  1654,    -1,  1656,    -1,    -1,  1126,  1660,
    1128,  1662,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1140,  1141,    -1,    -1,    -1,    -1,  1146,  1147,
     155,    -1,    -1,   158,   159,  1686,  1687,  1155,    -1,    -1,
      -1,    -1,    -1,  1835,  1836,    -1,    -1,  1372,    -1,    -1,
    1842,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1383,    -1,
      -1,    -1,  1854,    -1,  1182,    -1,   781,  1185,    -1,    -1,
      -1,    -1,  1864,    -1,  1866,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   797,   798,    -1,  1877,    -1,  1879,  1880,  1881,
      -1,    -1,    -1,    -1,    -1,  1746,    -1,    -1,   813,    -1,
      -1,    -1,   817,  1271,    -1,    -1,    -1,    -1,    -1,    -1,
     825,   826,    -1,    -1,    -1,   830,    -1,   832,    -1,    -1,
    1484,    -1,    -1,  1774,    -1,    -1,    -1,    -1,    -1,  1247,
     845,    -1,   847,    -1,  1302,    -1,   851,   852,   853,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1939,    -1,    -1,
      -1,  1943,    -1,  1040,   869,    -1,  1948,  1808,  1809,  1046,
    1278,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1286,    -1,
    1288,  1289,  1823,    -1,    -1,  1826,    -1,    -1,    -1,  1297,
      -1,  1299,    -1,  1301,    -1,    -1,    -1,  1838,  1075,   106,
    1308,  1078,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,  1855,   122,    -1,   124,    -1,    -1,
      -1,  2003,    -1,    -1,    -1,   930,    -1,    -1,    -1,   934,
      -1,    -1,    -1,  2015,   939,    -1,    -1,  2019,    -1,    -1,
      -1,   946,    -1,    -1,    -1,    -1,    -1,  1888,    -1,   156,
     955,  2033,   159,   958,    -1,    -1,    -1,    -1,  1971,    -1,
     965,    -1,    -1,    -1,    -1,    -1,  1143,    -1,    -1,    -1,
      -1,    -1,  1380,  1381,    -1,    -1,    -1,    -1,    -1,  1437,
      -1,    -1,  1923,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1932,    -1,  2075,    -1,    -1,    -1,  1405,  1456,    -1,
      -1,    -1,  1656,    -1,  1412,    -1,  1414,    -1,  2060,    -1,
      -1,  1016,  1017,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,  2042,
    1971,  2113,  1440,    -1,    -1,  1650,  2118,    -1,    -1,    -1,
     106,   134,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,  1514,    -1,    -1,    -1,
    2001,    -1,   155,   156,  2146,    -1,   159,  2149,    -1,  2151,
      -1,  2012,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1090,   179,    -1,    -1,  1094,
    2172,    -1,    -1,   159,  1099,  1100,    -1,    -1,  1103,    -1,
    2041,  2042,    -1,  2044,  2045,    -1,    -1,    -1,  1113,    -1,
      -1,    -1,    -1,    -1,   106,  1120,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,  1536,    -1,
      -1,  2072,    -1,    -1,    -1,  1543,    -1,  1545,    -1,    -1,
      -1,    -1,   134,    -1,     3,    -1,    -1,    -1,    -1,  1154,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,  2100,
    2101,  1166,    -1,   155,   156,  1170,    -1,    -1,  1345,  1174,
     162,    -1,    -1,    -1,   166,   167,   106,  1354,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,  1196,    -1,  1808,  1809,    -1,    -1,  2138,    -1,    -1,
    1205,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,  1214,
      -1,    72,    -1,  1621,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,   155,   156,    -1,  1686,   159,
      13,    14,    15,    16,    17,    -1,   166,   167,    -1,    -1,
      -1,    -1,  1247,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,     1,    -1,    -1,     4,    -1,    -1,  1272,    -1,    -1,
      -1,    -1,    -1,   134,  1279,   136,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    72,
      -1,    -1,    -1,  1298,   155,   156,    -1,    -1,   159,    -1,
      -1,    -1,  1307,    -1,  1481,   166,   167,   106,  1923,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    59,
      -1,  1326,  1327,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,  1754,  1755,    -1,   208,
      -1,   134,    -1,   136,    -1,    -1,   155,   156,    -1,    -1,
    1768,   101,    -1,    -1,    -1,   105,    -1,   166,   167,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
    1838,    -1,  1997,   166,   167,    -1,  2001,    -1,    -1,    -1,
      -1,    -1,  1397,  1570,    -1,  1400,  1401,    -1,    -1,   139,
    1577,    -1,    -1,   143,   263,  1582,  1583,  1584,    -1,   149,
      -1,  1416,   152,    -1,    -1,  1420,   156,    -1,    -1,  1424,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,  2044,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1444,
    1445,  1446,  1447,  1448,   303,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   193,    -1,    -1,   315,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1469,    -1,   206,   207,  1473,  1474,
      -1,   211,    -1,   332,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1487,    -1,    -1,  2100,  2101,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   353,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   245,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,   134,   262,  2138,   264,    -1,    -1,  1532,    -1,    -1,
      -1,    -1,  1537,   273,    -1,    -1,    -1,    -1,   278,    -1,
      -1,    -1,   155,   156,   284,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1557,   166,   167,    -1,    -1,    -1,   158,   299,
      -1,   420,   302,    -1,    -1,    -1,    -1,   307,    -1,   309,
      -1,    -1,   312,   313,    -1,    -1,    -1,   317,    -1,    -1,
      -1,    -1,    -1,   323,    -1,    -1,  1591,  2045,  1765,    -1,
      -1,    -1,   451,   333,    -1,  1600,    -1,    -1,    -1,  1604,
      -1,    -1,    -1,    -1,    -1,  1782,    -1,  1784,    -1,    -1,
      -1,    -1,    -1,    -1,   354,    -1,   356,   357,    -1,    -1,
     479,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,    -1,
     370,    -1,    -1,    -1,   374,  2043,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   504,    -1,    56,    57,   508,
     509,    -1,    -1,   512,    -1,    -1,   106,  1662,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   527,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   419,
      -1,    -1,  1687,    -1,    93,    -1,    -1,    -1,    -1,   106,
    2098,   550,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,   122,    -1,   124,   158,    -1,
      -1,  2119,   106,   453,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,  2134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,   147,   156,
      -1,  1746,   482,    -1,    -1,    -1,   486,    -1,    -1,    -1,
      -1,    -1,   492,   162,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,   503,    -1,    -1,    -1,    -1,   627,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
      -1,    -1,   641,   152,   524,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1809,    -1,   665,    -1,    -1,    -1,
     179,    -1,    -1,    -1,    -1,    -1,    -1,   226,    -1,   559,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     570,   571,    -1,    -1,    -1,   575,    -1,   577,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1855,    -1,   592,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   274,   275,    -1,    -1,   728,
      -1,   611,    -1,    -1,    -1,    -1,   285,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   624,    -1,    -1,    -1,    -1,    -1,
      -1,   300,    -1,    -1,   634,   635,    -1,   637,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     419,    -1,   652,   653,   654,    -1,   656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   664,   334,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   342,   343,    -1,    -1,   677,   347,    -1,
     680,    -1,    -1,    -1,   684,    -1,    -1,   687,    -1,    -1,
     690,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   699,
      -1,   820,    -1,   822,    -1,    -1,  1971,   707,    -1,   828,
     710,   711,    -1,   713,    -1,    -1,    -1,   386,    -1,    -1,
     389,    -1,   722,    -1,    -1,   725,   726,   727,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   855,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,   864,    -1,  2012,    -1,   868,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2041,  2042,    -1,    -1,
      -1,   781,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   913,  2060,    -1,   797,    -1,   918,
      59,   570,   571,    -1,    -1,   805,    -1,  2072,    -1,    -1,
      -1,    -1,    -1,   813,    -1,    -1,    -1,   817,    -1,    -1,
     489,    72,    -1,    -1,    -1,   825,   826,    -1,    -1,    -1,
     830,    -1,   501,   502,    -1,    -1,  2101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   845,   105,   847,    -1,    -1,
      -1,   851,   852,   853,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,   869,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     139,    -1,    -1,   134,    -1,   136,  1005,    -1,    -1,    -1,
     149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   899,
      -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,   168,
      -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     930,    -1,    -1,    -1,   934,    -1,    -1,    -1,   707,   939,
      -1,    -1,    -1,    -1,   713,    -1,   946,   947,   207,    -1,
      -1,    -1,    -1,   722,    -1,   955,    -1,   957,   958,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
     639,    -1,   741,    -1,    -1,    -1,    -1,    -1,    -1,   648,
      -1,    -1,    -1,    -1,    -1,   985,   245,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   777,    -1,
      -1,    -1,    -1,    -1,   273,    -1,  1016,  1017,    -1,    -1,
      -1,    -1,    72,    -1,     1,   284,    -1,    -1,   287,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
     309,    -1,    -1,    -1,   313,    -1,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,    49,  1192,    -1,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
    1090,    -1,    -1,    -1,  1094,   354,    73,    -1,   357,  1099,
    1100,    -1,    -1,  1103,    -1,   155,   156,    -1,    -1,    -1,
      -1,   370,    -1,  1113,    -1,   374,   166,   167,    -1,    -1,
    1120,    -1,  1241,    -1,    -1,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,  1154,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1166,    -1,    -1,    -1,
    1170,    -1,    -1,    -1,  1174,   844,    -1,  1296,   155,    -1,
      -1,   158,   159,    -1,    -1,   854,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,    -1,  1196,    -1,    -1,    -1,
      -1,  1320,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1214,    -1,    -1,    -1,    -1,    -1,
     185,    -1,    -1,    -1,    -1,    -1,    -1,   486,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1240,    -1,    -1,    -1,    -1,    -1,    -1,  1247,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   933,    -1,    -1,    -1,    -1,    -1,
      -1,  1271,  1272,    -1,    -1,    -1,    -1,    -1,    -1,  1279,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1291,    -1,    -1,    -1,    -1,    -1,    -1,  1298,    -1,
     559,    -1,  1302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   577,    -1,
     989,    -1,    -1,    -1,    -1,    -1,  1326,  1327,    -1,    -1,
      -1,    -1,  1332,   592,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,   634,   635,    -1,   637,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,   653,   654,    -1,  1397,    -1,    -1,
    1400,  1401,    -1,    -1,  1523,   664,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1416,    -1,   677,    -1,
    1420,    -1,    -1,    -1,  1424,    -1,    -1,    -1,    -1,    -1,
      -1,   690,    -1,    -1,    -1,    -1,    -1,  1437,   403,    -1,
      -1,    -1,   407,   408,  1444,  1445,  1446,  1447,  1448,  1449,
    1450,    -1,   417,   418,    -1,    -1,  1456,   136,    -1,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   432,   433,  1469,
      -1,  1240,    -1,  1473,  1474,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1484,    -1,    -1,  1487,   453,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1514,    -1,    -1,    59,    -1,    -1,
      -1,   486,   781,    -1,  1643,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1532,    -1,    -1,    -1,    -1,  1537,   797,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1550,  1220,  1221,  1222,   813,    -1,    -1,  1557,   817,    -1,
      -1,    -1,    -1,   105,    -1,    -1,   825,   826,    -1,    -1,
      -1,   830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1253,    -1,   845,    -1,   847,    -1,
      -1,  1591,   851,   852,   853,    -1,    -1,   139,    -1,    -1,
    1600,    -1,    -1,    -1,  1604,    -1,  1275,   149,    -1,    -1,
     869,    -1,    -1,  1282,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,  1747,  1748,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   207,    -1,    -1,    -1,    -1,
      -1,   930,    -1,    -1,    -1,   934,    -1,    -1,    -1,    -1,
     939,    -1,    -1,    -1,    -1,    -1,  1686,  1687,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   955,    -1,    48,   958,
    1469,  1470,    -1,   245,  1473,  1474,    -1,    -1,    -1,    -1,
    1479,    -1,    -1,    -1,  1483,    -1,  1485,    -1,  1487,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,
      -1,   273,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,    -1,    -1,  1746,    -1,    -1,    -1,
      -1,  1870,    -1,    -1,    -1,    -1,    -1,    -1,  1017,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,   309,    -1,    -1,
      -1,   313,    -1,   123,  1774,    -1,    -1,    -1,    -1,    -1,
      -1,  1900,    -1,    -1,    -1,    -1,   136,    -1,   138,    -1,
    1459,  1460,    -1,   758,   759,   760,   761,   762,   763,   764,
     765,   766,   767,   768,   769,   770,   771,   772,   773,   774,
     775,   776,   354,    -1,    -1,   357,    -1,    -1,  1937,   169,
      -1,   171,    -1,    -1,    -1,    -1,    -1,    -1,   370,    -1,
      -1,  1090,   374,    -1,    -1,  1094,    -1,    -1,  1838,    -1,
    1099,  1100,    -1,    -1,  1103,    -1,   196,  1966,    -1,    -1,
      -1,  1970,    -1,    -1,  1113,  1855,    -1,    -1,    -1,    -1,
      -1,  1120,    -1,    -1,    -1,    -1,    -1,  1636,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   843,    -1,
      -1,    -1,  1551,    -1,    -1,   235,    -1,    -1,    -1,   239,
      -1,    -1,   242,   243,    -1,  1154,   246,    -1,    -1,   249,
     250,    -1,   252,    -1,   254,    -1,    -1,  1166,    -1,    -1,
      -1,  1170,    -1,  1682,    -1,  1174,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1701,  1702,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   486,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1214,    -1,    -1,    -1,    -1,
      -1,    -1,  1731,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1971,    -1,    -1,   324,    -1,    -1,   327,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1247,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   351,  1671,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2012,  1272,    -1,    -1,   366,   559,    -1,    -1,
    1279,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   577,    -1,    -1,    -1,  1298,
      -1,  2041,  2042,    -1,    -1,  2045,    -1,  1012,    -1,    -1,
     592,    -1,    -1,  1018,    -1,    -1,    -1,    -1,    -1,    -1,
    2060,    -1,    -1,    -1,  1029,    -1,    -1,  1326,  1327,    -1,
      -1,  1840,  2072,    -1,    -1,    -1,    -1,    -1,  1847,    -1,
    1849,    -1,    -1,  1852,  1853,    -1,  1855,    -1,    -1,    -1,
      -1,  1860,   634,   635,    -1,   637,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1071,    -1,    -1,    -1,
      -1,   653,   654,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   664,   473,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   677,    -1,    -1,  1397,    -1,
      -1,  1400,  1401,    -1,    -1,    -1,    -1,    -1,   690,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1416,    -1,    -1,
      -1,  1420,    -1,    -1,    -1,  1424,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   531,    -1,    -1,    -1,    -1,    -1,  1956,    -1,    -1,
      -1,    -1,  1961,  1962,    -1,    -1,    -1,   547,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1469,    -1,  1981,    -1,  1473,  1474,    -1,    -1,  1887,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1487,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   781,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1214,
      -1,  2020,    -1,  2022,    -1,   797,    -1,  2026,  2027,    -1,
      -1,    -1,  2031,  2032,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   813,    -1,  1532,    -1,   817,    -1,    -1,  1537,    -1,
      -1,   631,    -1,   825,   826,    -1,    -1,    -1,   830,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   845,    -1,   847,    -1,    -1,    -1,   851,
     852,   853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   672,   673,  2093,  2094,  2095,   869,    -1,    -1,
      -1,    -1,  1591,  1298,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1600,   692,    -1,   694,  1604,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2124,  2125,  2126,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1335,  1336,  1337,    -1,    -1,    -1,    -1,  1342,  1343,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   930,    -1,
      -1,    -1,   934,    -1,    -1,    -1,    -1,   939,    -1,    -1,
      -1,  1366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   955,    -1,    -1,   958,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1687,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1406,  1407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   801,   802,    -1,    -1,   168,    -1,    -1,   808,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1017,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   833,    -1,    -1,   836,   837,    -1,   839,
      -1,   841,   842,   206,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   239,    -1,    -1,    -1,
      -1,    -1,   882,   246,    -1,    -1,   886,    -1,    -1,    -1,
     890,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1090,    -1,
      -1,    -1,  1094,    -1,    -1,    -1,    -1,  1099,  1100,    -1,
      -1,  1103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1113,    -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1553,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1855,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1154,    -1,   327,    -1,   966,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1166,    -1,    -1,    -1,  1170,    -1,
      -1,    -1,  1174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   354,   355,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   374,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,  1214,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,  1247,    55,    -1,    -1,    -1,
      -1,    -1,  1971,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
    1272,    -1,    -1,    -1,    -1,    -1,    -1,  1279,    -1,    -1,
      -1,    -1,    -1,   456,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2012,    -1,    -1,  1298,    -1,    -1,  1109,
     473,   474,    -1,   476,   477,  1730,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   486,    -1,    -1,    -1,   490,    -1,    -1,
      -1,    -1,  2041,  2042,  1326,  1327,    -1,   136,    -1,    -1,
     503,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2060,    -1,  1153,    -1,    -1,    -1,  1157,    -1,    -1,
    1160,    -1,    -1,  2072,  1164,    -1,    -1,    -1,    -1,   532,
      -1,    -1,    -1,   536,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1397,    -1,    -1,  1400,  1401,
      -1,    -1,    -1,    -1,   577,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1416,    -1,    -1,    -1,  1420,    -1,
      -1,    -1,  1424,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,   632,
      50,    51,   635,    53,    -1,    55,    -1,  1469,    -1,    -1,
      -1,  1473,  1474,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     653,   654,    72,    -1,    -1,  1487,    -1,    -1,    -1,    -1,
      -1,   664,    -1,    -1,  1304,   668,    -1,    -1,    -1,    -1,
      -1,    -1,   675,    -1,   677,    -1,    -1,    -1,    -1,    -1,
    1935,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1532,    -1,    -1,    -1,    -1,  1537,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,
      -1,    -1,  1977,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1372,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,    -1,    -1,  1383,    -1,    -1,  1386,    -1,  1388,  1389,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1591,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,
      -1,    -1,  1604,    -1,    -1,    -1,    -1,    -1,   781,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1432,    -1,   797,   798,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   808,   809,    -1,   811,   812,
      -1,  2066,    -1,  2068,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   825,   826,    -1,    -1,    -1,   830,    -1,   832,
     833,    -1,    -1,    -1,    -1,    -1,   839,    -1,    -1,    -1,
      -1,    -1,   845,    -1,   847,    85,    -1,    -1,   851,   852,
     853,    -1,  2107,    -1,    -1,  1687,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,    -1,   869,    -1,   871,    -1,
      -1,    -1,   875,    -1,    -1,  1515,    -1,    -1,    -1,   882,
     883,  2136,    -1,   886,   887,    -1,    -1,   890,   891,    -1,
      -1,    -1,    -1,    -1,    -1,   898,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,    -1,   156,    -1,    -1,    -1,
      -1,    -1,    -1,  2178,    -1,    -1,    -1,    -1,   168,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   939,   940,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   193,    -1,    -1,    -1,    -1,    -1,  1599,
      -1,    -1,    -1,    -1,    -1,   968,    -1,   207,    -1,    -1,
      -1,   211,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1627,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1016,  1017,    -1,    -1,    -1,    -1,    -1,
    1660,    -1,    -1,  1855,   264,    -1,    -1,  1667,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,   278,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,   123,    -1,    -1,    -1,    -1,
      -1,  1084,    -1,   323,    -1,    -1,    -1,    -1,   136,    72,
     138,    -1,    -1,   333,    -1,    -1,  1099,  1100,    -1,    -1,
    1103,  1104,  1742,    -1,    -1,    -1,    -1,  1110,    -1,    -1,
      -1,    -1,    -1,    -1,   354,    -1,   356,    -1,    -1,    -1,
      -1,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1971,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1789,
    1790,  1154,    -1,   136,  1157,  1158,    -1,  1160,  1161,    -1,
      -1,  1164,  1165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1815,  1816,    -1,    -1,   419,
    2012,    -1,    -1,  1823,    -1,    -1,    -1,    -1,    -1,  1829,
      -1,    -1,    -1,    -1,   242,   243,    -1,    -1,   246,    -1,
      -1,   249,   250,    -1,   252,    -1,   254,    -1,    -1,  2041,
    2042,    -1,    -1,   453,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2060,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2072,    -1,    -1,    -1,    -1,    -1,   486,    -1,    -1,    -1,
      -1,    -1,   492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1279,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   524,  1925,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1298,    -1,    -1,    -1,    -1,
      -1,  1304,  1305,   351,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   366,    -1,
      -1,    -1,    -1,    -1,  1327,    -1,    -1,    -1,    -1,    -1,
     570,   571,    -1,    -1,    -1,    -1,    -1,   577,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1997,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1372,
    1373,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1383,  1384,    -1,  1386,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1397,   635,    -1,  1400,  1401,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   654,    -1,   656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   473,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   699,
    2100,    -1,    -1,    -1,    -1,    -1,    -1,   707,    -1,    -1,
     710,   711,    -1,   713,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   722,    -1,    -1,   725,   726,   727,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   547,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1532,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   781,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1559,    -1,    -1,    -1,
     114,    -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   826,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   845,    -1,   847,    -1,    -1,
      -1,   851,   852,   853,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   672,   673,    -1,    -1,    -1,   869,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   692,    -1,   694,  1650,    -1,    -1,
      -1,    -1,    -1,   207,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,  1667,    -1,    -1,    -1,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,   939,
      50,    51,    -1,    53,    -1,    55,    -1,   947,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,   957,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,
      -1,    -1,    -1,   801,   802,    -1,    -1,    -1,    -1,    -1,
     808,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1017,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   833,    -1,    -1,   836,   837,
      -1,   839,    -1,   841,   842,    -1,    -1,    -1,    -1,    -1,
     354,    -1,   356,   357,    -1,  1808,  1809,   157,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   370,    -1,    -1,    -1,
     374,  1824,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    -1,    -1,   882,    -1,    -1,    -1,   886,    -1,
      -1,    -1,   890,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,  1099,
    1100,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1154,    -1,    -1,    -1,   966,    -1,
    1923,    72,    -1,    -1,    -1,    -1,    -1,    -1,  1931,    -1,
      -1,    -1,   486,    -1,    -1,    -1,    -1,    -1,   492,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,   168,    -1,
      -1,    -1,    -1,    -1,  1997,  1998,    -1,    -1,  2001,    -1,
    1240,    -1,    -1,    -1,    -1,   559,    -1,   158,   159,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,   577,    -1,    -1,    -1,   207,    -1,    -1,
      -1,  1271,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1279,
      -1,  2044,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1291,    -1,    -1,    -1,    -1,    -1,   611,  1298,    -1,
      -1,  1109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     634,   635,    -1,    -1,    -1,    -1,    -1,  1327,    -1,    -1,
      -1,    -1,  1332,    -1,    -1,    -1,    -1,  2100,  2101,   653,
     654,    -1,   656,    -1,    -1,  1153,    -1,    -1,    -1,  1157,
     664,    -1,  1160,    -1,    -1,    -1,  1164,    -1,    -1,    -1,
      -1,    -1,   302,   677,    -1,    -1,   680,    -1,    -1,    -1,
      -1,    -1,    -1,   687,    -1,  2138,   690,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1397,    -1,    -1,
    1400,  1401,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   354,    -1,   356,   357,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     370,    -1,    -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1444,  1445,  1446,    -1,    -1,  1449,
    1450,    -1,    -1,    -1,    -1,    -1,  1456,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   781,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   797,  1484,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1304,    -1,    -1,   813,
      -1,    -1,    -1,   817,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   825,   826,    -1,  1514,    -1,   830,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   845,  1532,   847,    -1,    -1,    -1,   851,   852,   853,
      -1,    -1,    -1,    -1,    -1,    -1,   486,    -1,    -1,    -1,
      -1,    -1,   492,    -1,    -1,   869,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,  1372,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1383,    -1,    -1,  1386,    -1,
    1388,  1389,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   930,    -1,    -1,   559,
      -1,    -1,    -1,    -1,  1432,   939,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   577,    -1,    -1,
      -1,   955,    -1,   957,   958,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   611,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1686,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   634,   635,    -1,   302,    -1,    -1,
      -1,    -1,    -1,  1017,    -1,    -1,    -1,  1515,    -1,    -1,
      -1,    -1,    -1,   653,   654,    -1,   656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   664,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,    -1,
     680,    -1,    -1,    -1,    -1,    -1,    -1,   687,    -1,   354,
     690,   356,   357,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   370,    -1,    -1,    -1,   374,
      -1,    -1,    -1,    -1,  1774,    -1,  1090,    -1,    -1,    -1,
    1094,    -1,    -1,    -1,    -1,  1099,  1100,    -1,    -1,  1103,
      -1,  1599,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1113,
      -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,  1627,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1838,    -1,
    1154,   781,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1166,    -1,    -1,    -1,  1170,   797,    -1,    -1,
    1174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   813,   193,    -1,    -1,   817,    -1,    -1,
      -1,   486,   152,    -1,    -1,   825,   826,   492,   207,    -1,
     830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,
      -1,    -1,   221,    -1,   223,   845,    -1,   847,    -1,    -1,
      -1,   851,   852,   853,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   193,    -1,    -1,    -1,    -1,    -1,   869,
      -1,    -1,    -1,    -1,  1742,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   559,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1279,    -1,    -1,    -1,    -1,
      -1,  1971,   577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1789,  1790,    -1,  1298,    -1,    -1,    -1,  1302,    -1,
     930,    -1,    -1,    -1,   264,    -1,    -1,    -1,    -1,   939,
      -1,    -1,    -1,   322,    -1,    -1,   611,  1815,  1816,    -1,
      -1,    -1,  1326,  1327,    -1,   955,    -1,   957,   958,    -1,
      -1,  1829,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   634,
     635,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2042,    -1,    -1,  2045,    -1,    -1,   653,   654,
      -1,   656,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   664,
      -1,    -1,    -1,   333,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   677,    -1,    -1,   680,    -1,  1017,    -1,    -1,
      -1,    -1,   687,  1397,   354,   690,  1400,  1401,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1416,    -1,    -1,    -1,  1420,    -1,    -1,    -1,
    1424,    -1,    -1,    -1,    -1,    -1,    -1,  1925,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1090,    -1,    -1,    -1,  1094,    -1,    -1,    -1,    -1,  1099,
    1100,    -1,    -1,  1103,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1113,    -1,    -1,   781,    -1,    -1,    -1,
    1120,    -1,    -1,   453,    -1,    -1,    -1,    -1,    -1,  1997,
      -1,    -1,   797,    -1,   513,    -1,    -1,    -1,    -1,    -1,
     519,    -1,    -1,    -1,    -1,   524,    -1,    -1,   813,    -1,
      -1,    -1,   817,    -1,  1154,    -1,   486,    -1,  1532,    -1,
     825,   826,    -1,  1537,    -1,   830,  1166,    -1,    -1,    -1,
    1170,    -1,    -1,    -1,  1174,    -1,    -1,    -1,    -1,    -1,
     845,    -1,   847,    -1,    -1,    -1,   851,   852,   853,    -1,
      -1,    -1,    -1,    -1,   524,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   869,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1591,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,    -1,    -1,
    1604,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     570,   571,    -1,    -1,    -1,    -1,   625,   577,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   930,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   939,   654,    -1,    -1,    -1,  1279,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   667,    -1,
     955,    -1,   957,   958,    -1,    -1,    -1,    -1,  1298,    -1,
      -1,    -1,  1302,    -1,    -1,   635,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   654,   704,  1326,  1327,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   715,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,    -1,    -1,
      -1,    -1,  1017,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     739,   740,    -1,    -1,   743,    -1,   745,    -1,    -1,   699,
      -1,    -1,   751,    -1,   753,   754,    -1,   707,    -1,    -1,
      -1,   711,    -1,   713,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1397,    -1,    -1,
    1400,  1401,   781,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   794,  1416,    -1,    -1,    -1,
    1420,    -1,    -1,    -1,  1424,  1090,   805,    -1,    -1,  1094,
      -1,    -1,    -1,    -1,  1099,  1100,    -1,    -1,  1103,    -1,
      -1,    -1,   821,    -1,    -1,    -1,    -1,   826,  1113,    -1,
      -1,   781,    -1,    -1,    -1,  1120,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   805,    -1,    -1,   857,    -1,
      -1,   860,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1154,
      -1,   870,    -1,    -1,    -1,    -1,   826,    -1,    -1,    -1,
      -1,  1166,    -1,    -1,    -1,  1170,    -1,    -1,    -1,  1174,
      -1,    -1,    -1,    -1,    -1,   845,    -1,   847,    -1,    -1,
     899,   851,   852,   853,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1532,    -1,    -1,    -1,    -1,  1537,    -1,   869,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   947,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   957,   958,
      -1,    -1,    -1,    -1,    -1,    -1,   965,    -1,    -1,    -1,
      -1,  1591,    -1,    -1,    -1,    -1,    -1,  1971,    -1,    -1,
    1600,    -1,    -1,    -1,  1604,    -1,    -1,    -1,    -1,   939,
      -1,    -1,    -1,    -1,  1279,    -1,    -1,   947,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1298,    -1,    -1,    -1,  1302,  1017,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1025,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1034,    -1,    -1,    -1,    -1,
      -1,  1326,  1327,    -1,    -1,    -1,    -1,    -1,  2042,    -1,
      -1,  2045,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       5,    -1,    -1,    -1,    -1,    -1,  2060,  1017,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,  1078,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      -1,    56,  1397,    -1,    -1,  1400,  1401,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,  1416,    -1,    -1,    -1,  1420,    -1,    -1,    -1,  1424,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1099,
    1100,  1150,    -1,  1152,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1154,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1234,  1235,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1532,    -1,    -1,
      -1,    -1,  1537,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,  1302,    53,    -1,  1591,    -1,    -1,  1308,
      -1,    -1,    -1,    -1,    -1,  1600,    -1,    -1,    -1,  1604,
      -1,    -1,    -1,    72,  1323,    -1,    -1,    -1,  1327,  1279,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1291,    -1,    -1,    -1,    -1,  1345,    -1,  1298,  1348,
      -1,  1971,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,    -1,    -1,    -1,  1363,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1327,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     5,    -1,   136,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   162,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2042,    -1,    -1,  2045,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,  1436,  1437,    -1,
    2060,    -1,    -1,    -1,    -1,    -1,    -1,  1397,    -1,    -1,
    1400,  1401,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1462,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1481,   103,   104,  1484,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,     1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,  1532,  1484,   155,    -1,    -1,   158,   159,
      -1,    -1,  1541,  1542,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,  1570,    -1,    -1,    -1,  1574,    -1,    -1,    -1,    -1,
      -1,    71,  1532,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
      -1,   101,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1656,    -1,    -1,
      -1,    -1,  1661,    -1,   154,   155,    -1,    -1,   158,   159,
      -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,  1971,    -1,    -1,    -1,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1722,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2042,    -1,    -1,
    2045,    -1,    -1,    -1,    -1,    -1,  1765,    -1,    -1,    -1,
      -1,    -1,    -1,  1772,    -1,  2060,  1775,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,  1801,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
      -1,   101,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,   155,    -1,    -1,   158,   159,
      -1,    -1,    18,   163,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,
      76,    -1,    -1,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,    -1,   101,    -1,   103,   104,  1978,
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
     171,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
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
      -1,    -1,   136,    -1,    -1,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,   158,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    72,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   106,
      49,   108,   109,    52,    -1,    54,    -1,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,   108,   109,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    49,
     136,    -1,    52,    -1,    54,   134,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,   151,    73,    -1,    -1,   155,   156,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    -1,   126,   127,    49,    -1,
      -1,    52,    -1,    54,   134,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,   151,    73,    -1,    -1,   155,   156,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     155,   156,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,
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
      -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,    -1,
      -1,    -1,   163,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,   124,
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,   162,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    49,   126,   127,    52,
      -1,    54,    -1,    56,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,   163,    -1,   165,   166,   167,   168,
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
      -1,    -1,    -1,    -1,   155,    -1,   157,   158,   159,    -1,
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
      -1,    54,    -1,    56,    57,   134,    -1,    -1,    -1,    -1,
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
      49,   126,   127,    52,    -1,    54,    -1,    56,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     165,   166,   167,   168,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,   124,    -1,   126,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,
     169,   170,   171
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
     157,   183,   183,   160,   160,   368,   493,   154,   370,   386,
     183,   157,   220,   192,   220,   493,   154,   160,   160,   197,
     197,   157,   426,   426,   157,   157,   160,   160,   220,   180,
     479,   480,   481,   314,   478,   161,   180,   426,   426,   180,
     157,   432,   426,   229,   229,    77,    78,   163,   240,   241,
     242,   157,   227,    75,   229,   227,   156,   227,    75,   180,
      57,   108,   156,   227,   228,   248,   249,   156,   227,   229,
     247,   251,   251,   180,   227,   154,   163,   242,   229,   229,
     155,   182,   180,   188,   157,   162,   157,   161,   162,   157,
     229,   155,   229,   229,   229,   397,   381,   423,   160,   160,
     493,   154,   493,   154,   154,   160,   160,   157,   157,   157,
     478,   426,   355,    75,     1,   219,   238,   239,   424,     1,
     162,     1,   182,   229,   240,    75,   180,   157,   229,    75,
     180,   171,   171,   229,   228,   108,   251,   251,   180,   227,
     248,   171,   171,    75,   156,   227,   156,   227,   228,   180,
       1,   182,   182,   277,   312,   314,   487,   162,   180,   159,
     188,   282,   283,   284,   204,   194,   227,   260,   154,   154,
     155,   426,   467,   470,   357,   229,   137,     1,   161,   162,
     154,   287,   288,   294,   229,    75,   180,   229,   227,   156,
     156,   227,   156,   227,   156,   227,   228,   156,   227,   156,
     227,   229,   171,   171,   171,   171,   154,   287,   277,   183,
     155,   202,   423,   478,   186,   162,   106,   155,   157,   162,
     161,   157,   157,    75,   256,   371,   219,   238,   241,   243,
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
       2,     3,     0,     1,     3,     3,     3,     2,     5,     4,
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
#line 8477 "Parser/parser.cc"
    break;

  case 3:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8483 "Parser/parser.cc"
    break;

  case 4:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8489 "Parser/parser.cc"
    break;

  case 5:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8495 "Parser/parser.cc"
    break;

  case 6:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8501 "Parser/parser.cc"
    break;

  case 7:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8507 "Parser/parser.cc"
    break;

  case 8:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8513 "Parser/parser.cc"
    break;

  case 20:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8519 "Parser/parser.cc"
    break;

  case 21:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8525 "Parser/parser.cc"
    break;

  case 22:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8531 "Parser/parser.cc"
    break;

  case 23:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8541 "Parser/parser.cc"
    break;

  case 24:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8547 "Parser/parser.cc"
    break;

  case 25:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8553 "Parser/parser.cc"
    break;

  case 26:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8559 "Parser/parser.cc"
    break;

  case 28:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8565 "Parser/parser.cc"
    break;

  case 29:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8571 "Parser/parser.cc"
    break;

  case 30:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8577 "Parser/parser.cc"
    break;

  case 31:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8583 "Parser/parser.cc"
    break;

  case 32:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8593 "Parser/parser.cc"
    break;

  case 33:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8599 "Parser/parser.cc"
    break;

  case 34:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8605 "Parser/parser.cc"
    break;

  case 35:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8611 "Parser/parser.cc"
    break;

  case 36:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8617 "Parser/parser.cc"
    break;

  case 37:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8623 "Parser/parser.cc"
    break;

  case 38:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8629 "Parser/parser.cc"
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
#line 8641 "Parser/parser.cc"
    break;

  case 41:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8650 "Parser/parser.cc"
    break;

  case 42:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8656 "Parser/parser.cc"
    break;

  case 44:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8662 "Parser/parser.cc"
    break;

  case 45:
#line 772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8668 "Parser/parser.cc"
    break;

  case 46:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8674 "Parser/parser.cc"
    break;

  case 47:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8680 "Parser/parser.cc"
    break;

  case 48:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8690 "Parser/parser.cc"
    break;

  case 49:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8696 "Parser/parser.cc"
    break;

  case 50:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8703 "Parser/parser.cc"
    break;

  case 51:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8709 "Parser/parser.cc"
    break;

  case 52:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8715 "Parser/parser.cc"
    break;

  case 53:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8721 "Parser/parser.cc"
    break;

  case 54:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8727 "Parser/parser.cc"
    break;

  case 55:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8733 "Parser/parser.cc"
    break;

  case 56:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8739 "Parser/parser.cc"
    break;

  case 57:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8745 "Parser/parser.cc"
    break;

  case 58:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 59:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8757 "Parser/parser.cc"
    break;

  case 60:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8763 "Parser/parser.cc"
    break;

  case 61:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8769 "Parser/parser.cc"
    break;

  case 62:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8775 "Parser/parser.cc"
    break;

  case 63:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8781 "Parser/parser.cc"
    break;

  case 64:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8787 "Parser/parser.cc"
    break;

  case 65:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8793 "Parser/parser.cc"
    break;

  case 66:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8799 "Parser/parser.cc"
    break;

  case 67:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8805 "Parser/parser.cc"
    break;

  case 68:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8815 "Parser/parser.cc"
    break;

  case 69:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8821 "Parser/parser.cc"
    break;

  case 72:
#line 859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8827 "Parser/parser.cc"
    break;

  case 73:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8833 "Parser/parser.cc"
    break;

  case 76:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8839 "Parser/parser.cc"
    break;

  case 78:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8845 "Parser/parser.cc"
    break;

  case 79:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8851 "Parser/parser.cc"
    break;

  case 80:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8857 "Parser/parser.cc"
    break;

  case 81:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8863 "Parser/parser.cc"
    break;

  case 82:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8869 "Parser/parser.cc"
    break;

  case 83:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8875 "Parser/parser.cc"
    break;

  case 84:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8881 "Parser/parser.cc"
    break;

  case 85:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8887 "Parser/parser.cc"
    break;

  case 86:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8895 "Parser/parser.cc"
    break;

  case 87:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8901 "Parser/parser.cc"
    break;

  case 88:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8910 "Parser/parser.cc"
    break;

  case 91:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8916 "Parser/parser.cc"
    break;

  case 92:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8922 "Parser/parser.cc"
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
#line 8942 "Parser/parser.cc"
    break;

  case 94:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8948 "Parser/parser.cc"
    break;

  case 95:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8954 "Parser/parser.cc"
    break;

  case 96:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8960 "Parser/parser.cc"
    break;

  case 97:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8966 "Parser/parser.cc"
    break;

  case 98:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8972 "Parser/parser.cc"
    break;

  case 99:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8978 "Parser/parser.cc"
    break;

  case 100:
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8984 "Parser/parser.cc"
    break;

  case 101:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8990 "Parser/parser.cc"
    break;

  case 102:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8996 "Parser/parser.cc"
    break;

  case 103:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9002 "Parser/parser.cc"
    break;

  case 104:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9011 "Parser/parser.cc"
    break;

  case 105:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9017 "Parser/parser.cc"
    break;

  case 106:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9023 "Parser/parser.cc"
    break;

  case 107:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9029 "Parser/parser.cc"
    break;

  case 108:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9035 "Parser/parser.cc"
    break;

  case 109:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9041 "Parser/parser.cc"
    break;

  case 110:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9047 "Parser/parser.cc"
    break;

  case 111:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9053 "Parser/parser.cc"
    break;

  case 113:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9059 "Parser/parser.cc"
    break;

  case 114:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9065 "Parser/parser.cc"
    break;

  case 115:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9071 "Parser/parser.cc"
    break;

  case 116:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9077 "Parser/parser.cc"
    break;

  case 117:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9083 "Parser/parser.cc"
    break;

  case 118:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9089 "Parser/parser.cc"
    break;

  case 119:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9095 "Parser/parser.cc"
    break;

  case 120:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9101 "Parser/parser.cc"
    break;

  case 128:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 130:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9113 "Parser/parser.cc"
    break;

  case 131:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9119 "Parser/parser.cc"
    break;

  case 132:
#line 1031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9125 "Parser/parser.cc"
    break;

  case 134:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9131 "Parser/parser.cc"
    break;

  case 135:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9137 "Parser/parser.cc"
    break;

  case 137:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9143 "Parser/parser.cc"
    break;

  case 138:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9149 "Parser/parser.cc"
    break;

  case 140:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9155 "Parser/parser.cc"
    break;

  case 141:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9161 "Parser/parser.cc"
    break;

  case 142:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9167 "Parser/parser.cc"
    break;

  case 143:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9173 "Parser/parser.cc"
    break;

  case 145:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9179 "Parser/parser.cc"
    break;

  case 146:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9185 "Parser/parser.cc"
    break;

  case 148:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9191 "Parser/parser.cc"
    break;

  case 150:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9197 "Parser/parser.cc"
    break;

  case 152:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9203 "Parser/parser.cc"
    break;

  case 154:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9209 "Parser/parser.cc"
    break;

  case 156:
#line 1097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9215 "Parser/parser.cc"
    break;

  case 158:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9221 "Parser/parser.cc"
    break;

  case 159:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9227 "Parser/parser.cc"
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
#line 9239 "Parser/parser.cc"
    break;

  case 163:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9245 "Parser/parser.cc"
    break;

  case 164:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9251 "Parser/parser.cc"
    break;

  case 168:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9257 "Parser/parser.cc"
    break;

  case 169:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9263 "Parser/parser.cc"
    break;

  case 170:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9269 "Parser/parser.cc"
    break;

  case 171:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9275 "Parser/parser.cc"
    break;

  case 172:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9281 "Parser/parser.cc"
    break;

  case 173:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9287 "Parser/parser.cc"
    break;

  case 174:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9293 "Parser/parser.cc"
    break;

  case 175:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9299 "Parser/parser.cc"
    break;

  case 176:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9305 "Parser/parser.cc"
    break;

  case 177:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9311 "Parser/parser.cc"
    break;

  case 178:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9317 "Parser/parser.cc"
    break;

  case 179:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9323 "Parser/parser.cc"
    break;

  case 180:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9329 "Parser/parser.cc"
    break;

  case 181:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9335 "Parser/parser.cc"
    break;

  case 182:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9341 "Parser/parser.cc"
    break;

  case 184:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9347 "Parser/parser.cc"
    break;

  case 185:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9353 "Parser/parser.cc"
    break;

  case 186:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9359 "Parser/parser.cc"
    break;

  case 188:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9365 "Parser/parser.cc"
    break;

  case 189:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9371 "Parser/parser.cc"
    break;

  case 204:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9377 "Parser/parser.cc"
    break;

  case 206:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9383 "Parser/parser.cc"
    break;

  case 207:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9389 "Parser/parser.cc"
    break;

  case 208:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9400 "Parser/parser.cc"
    break;

  case 209:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9406 "Parser/parser.cc"
    break;

  case 210:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9412 "Parser/parser.cc"
    break;

  case 212:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9418 "Parser/parser.cc"
    break;

  case 213:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9424 "Parser/parser.cc"
    break;

  case 214:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9430 "Parser/parser.cc"
    break;

  case 215:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9436 "Parser/parser.cc"
    break;

  case 216:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9442 "Parser/parser.cc"
    break;

  case 219:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9448 "Parser/parser.cc"
    break;

  case 220:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9455 "Parser/parser.cc"
    break;

  case 221:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9461 "Parser/parser.cc"
    break;

  case 222:
#line 1297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9467 "Parser/parser.cc"
    break;

  case 223:
#line 1299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9473 "Parser/parser.cc"
    break;

  case 224:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9479 "Parser/parser.cc"
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
#line 9493 "Parser/parser.cc"
    break;

  case 226:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9499 "Parser/parser.cc"
    break;

  case 227:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9505 "Parser/parser.cc"
    break;

  case 228:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9514 "Parser/parser.cc"
    break;

  case 229:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9520 "Parser/parser.cc"
    break;

  case 230:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9526 "Parser/parser.cc"
    break;

  case 231:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9532 "Parser/parser.cc"
    break;

  case 232:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9538 "Parser/parser.cc"
    break;

  case 233:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9544 "Parser/parser.cc"
    break;

  case 234:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9550 "Parser/parser.cc"
    break;

  case 235:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9556 "Parser/parser.cc"
    break;

  case 237:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9562 "Parser/parser.cc"
    break;

  case 238:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9568 "Parser/parser.cc"
    break;

  case 239:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9574 "Parser/parser.cc"
    break;

  case 240:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9580 "Parser/parser.cc"
    break;

  case 241:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9586 "Parser/parser.cc"
    break;

  case 242:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9592 "Parser/parser.cc"
    break;

  case 243:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9598 "Parser/parser.cc"
    break;

  case 245:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9604 "Parser/parser.cc"
    break;

  case 246:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9610 "Parser/parser.cc"
    break;

  case 247:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9616 "Parser/parser.cc"
    break;

  case 249:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9622 "Parser/parser.cc"
    break;

  case 250:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9628 "Parser/parser.cc"
    break;

  case 251:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9634 "Parser/parser.cc"
    break;

  case 252:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9643 "Parser/parser.cc"
    break;

  case 253:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9649 "Parser/parser.cc"
    break;

  case 254:
#line 1397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9655 "Parser/parser.cc"
    break;

  case 255:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9661 "Parser/parser.cc"
    break;

  case 256:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9670 "Parser/parser.cc"
    break;

  case 257:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9676 "Parser/parser.cc"
    break;

  case 258:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9682 "Parser/parser.cc"
    break;

  case 259:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9688 "Parser/parser.cc"
    break;

  case 260:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9697 "Parser/parser.cc"
    break;

  case 261:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9703 "Parser/parser.cc"
    break;

  case 262:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9709 "Parser/parser.cc"
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
#line 9728 "Parser/parser.cc"
    break;

  case 265:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9734 "Parser/parser.cc"
    break;

  case 266:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9743 "Parser/parser.cc"
    break;

  case 267:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9749 "Parser/parser.cc"
    break;

  case 268:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9755 "Parser/parser.cc"
    break;

  case 269:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9761 "Parser/parser.cc"
    break;

  case 270:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9767 "Parser/parser.cc"
    break;

  case 271:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9773 "Parser/parser.cc"
    break;

  case 272:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9779 "Parser/parser.cc"
    break;

  case 273:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9788 "Parser/parser.cc"
    break;

  case 274:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9797 "Parser/parser.cc"
    break;

  case 275:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9803 "Parser/parser.cc"
    break;

  case 276:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9812 "Parser/parser.cc"
    break;

  case 277:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9821 "Parser/parser.cc"
    break;

  case 278:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9827 "Parser/parser.cc"
    break;

  case 279:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9833 "Parser/parser.cc"
    break;

  case 280:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9839 "Parser/parser.cc"
    break;

  case 281:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9845 "Parser/parser.cc"
    break;

  case 282:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9851 "Parser/parser.cc"
    break;

  case 283:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9857 "Parser/parser.cc"
    break;

  case 284:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9863 "Parser/parser.cc"
    break;

  case 285:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9869 "Parser/parser.cc"
    break;

  case 286:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9878 "Parser/parser.cc"
    break;

  case 287:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9888 "Parser/parser.cc"
    break;

  case 288:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9894 "Parser/parser.cc"
    break;

  case 289:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9900 "Parser/parser.cc"
    break;

  case 290:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9909 "Parser/parser.cc"
    break;

  case 291:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9919 "Parser/parser.cc"
    break;

  case 292:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9925 "Parser/parser.cc"
    break;

  case 293:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9934 "Parser/parser.cc"
    break;

  case 294:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9944 "Parser/parser.cc"
    break;

  case 295:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9950 "Parser/parser.cc"
    break;

  case 296:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9956 "Parser/parser.cc"
    break;

  case 297:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9962 "Parser/parser.cc"
    break;

  case 298:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9968 "Parser/parser.cc"
    break;

  case 299:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9977 "Parser/parser.cc"
    break;

  case 300:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9987 "Parser/parser.cc"
    break;

  case 301:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9993 "Parser/parser.cc"
    break;

  case 302:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10002 "Parser/parser.cc"
    break;

  case 303:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10012 "Parser/parser.cc"
    break;

  case 304:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10018 "Parser/parser.cc"
    break;

  case 305:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10027 "Parser/parser.cc"
    break;

  case 306:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10037 "Parser/parser.cc"
    break;

  case 307:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10043 "Parser/parser.cc"
    break;

  case 308:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 10052 "Parser/parser.cc"
    break;

  case 309:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 10063 "Parser/parser.cc"
    break;

  case 312:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10069 "Parser/parser.cc"
    break;

  case 313:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10075 "Parser/parser.cc"
    break;

  case 314:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10081 "Parser/parser.cc"
    break;

  case 315:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10087 "Parser/parser.cc"
    break;

  case 316:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10093 "Parser/parser.cc"
    break;

  case 318:
#line 1637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10099 "Parser/parser.cc"
    break;

  case 319:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10105 "Parser/parser.cc"
    break;

  case 320:
#line 1644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10111 "Parser/parser.cc"
    break;

  case 321:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10117 "Parser/parser.cc"
    break;

  case 322:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10123 "Parser/parser.cc"
    break;

  case 323:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10129 "Parser/parser.cc"
    break;

  case 324:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10135 "Parser/parser.cc"
    break;

  case 325:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10141 "Parser/parser.cc"
    break;

  case 326:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10147 "Parser/parser.cc"
    break;

  case 327:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10153 "Parser/parser.cc"
    break;

  case 328:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10159 "Parser/parser.cc"
    break;

  case 329:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10165 "Parser/parser.cc"
    break;

  case 330:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10171 "Parser/parser.cc"
    break;

  case 331:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10177 "Parser/parser.cc"
    break;

  case 332:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10183 "Parser/parser.cc"
    break;

  case 333:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10189 "Parser/parser.cc"
    break;

  case 334:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10195 "Parser/parser.cc"
    break;

  case 335:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10201 "Parser/parser.cc"
    break;

  case 336:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10207 "Parser/parser.cc"
    break;

  case 337:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10213 "Parser/parser.cc"
    break;

  case 338:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10219 "Parser/parser.cc"
    break;

  case 339:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10225 "Parser/parser.cc"
    break;

  case 342:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10231 "Parser/parser.cc"
    break;

  case 343:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10240 "Parser/parser.cc"
    break;

  case 344:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10246 "Parser/parser.cc"
    break;

  case 345:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10252 "Parser/parser.cc"
    break;

  case 348:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10258 "Parser/parser.cc"
    break;

  case 349:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10264 "Parser/parser.cc"
    break;

  case 352:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10270 "Parser/parser.cc"
    break;

  case 353:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10276 "Parser/parser.cc"
    break;

  case 354:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10282 "Parser/parser.cc"
    break;

  case 355:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10288 "Parser/parser.cc"
    break;

  case 356:
#line 1751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10294 "Parser/parser.cc"
    break;

  case 357:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10300 "Parser/parser.cc"
    break;

  case 358:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10306 "Parser/parser.cc"
    break;

  case 359:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10312 "Parser/parser.cc"
    break;

  case 360:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10318 "Parser/parser.cc"
    break;

  case 363:
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10324 "Parser/parser.cc"
    break;

  case 364:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10330 "Parser/parser.cc"
    break;

  case 365:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10336 "Parser/parser.cc"
    break;

  case 366:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10342 "Parser/parser.cc"
    break;

  case 367:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10348 "Parser/parser.cc"
    break;

  case 368:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10354 "Parser/parser.cc"
    break;

  case 369:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10360 "Parser/parser.cc"
    break;

  case 370:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10366 "Parser/parser.cc"
    break;

  case 371:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10372 "Parser/parser.cc"
    break;

  case 372:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10378 "Parser/parser.cc"
    break;

  case 373:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10384 "Parser/parser.cc"
    break;

  case 374:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10390 "Parser/parser.cc"
    break;

  case 375:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10396 "Parser/parser.cc"
    break;

  case 376:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10402 "Parser/parser.cc"
    break;

  case 377:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10408 "Parser/parser.cc"
    break;

  case 378:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10414 "Parser/parser.cc"
    break;

  case 379:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10420 "Parser/parser.cc"
    break;

  case 380:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10426 "Parser/parser.cc"
    break;

  case 381:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10432 "Parser/parser.cc"
    break;

  case 382:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10438 "Parser/parser.cc"
    break;

  case 383:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10444 "Parser/parser.cc"
    break;

  case 384:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10450 "Parser/parser.cc"
    break;

  case 385:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10456 "Parser/parser.cc"
    break;

  case 387:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10462 "Parser/parser.cc"
    break;

  case 388:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 389:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10474 "Parser/parser.cc"
    break;

  case 394:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10480 "Parser/parser.cc"
    break;

  case 395:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10486 "Parser/parser.cc"
    break;

  case 396:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10492 "Parser/parser.cc"
    break;

  case 397:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10498 "Parser/parser.cc"
    break;

  case 398:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10504 "Parser/parser.cc"
    break;

  case 399:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10510 "Parser/parser.cc"
    break;

  case 400:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10516 "Parser/parser.cc"
    break;

  case 401:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10522 "Parser/parser.cc"
    break;

  case 404:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10528 "Parser/parser.cc"
    break;

  case 405:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10534 "Parser/parser.cc"
    break;

  case 406:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10543 "Parser/parser.cc"
    break;

  case 407:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10549 "Parser/parser.cc"
    break;

  case 408:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10555 "Parser/parser.cc"
    break;

  case 409:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10561 "Parser/parser.cc"
    break;

  case 410:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10570 "Parser/parser.cc"
    break;

  case 411:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10579 "Parser/parser.cc"
    break;

  case 412:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10585 "Parser/parser.cc"
    break;

  case 415:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10591 "Parser/parser.cc"
    break;

  case 416:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10597 "Parser/parser.cc"
    break;

  case 418:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10603 "Parser/parser.cc"
    break;

  case 419:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10609 "Parser/parser.cc"
    break;

  case 429:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10615 "Parser/parser.cc"
    break;

  case 430:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10621 "Parser/parser.cc"
    break;

  case 434:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10627 "Parser/parser.cc"
    break;

  case 436:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10633 "Parser/parser.cc"
    break;

  case 437:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10639 "Parser/parser.cc"
    break;

  case 438:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10645 "Parser/parser.cc"
    break;

  case 439:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10651 "Parser/parser.cc"
    break;

  case 440:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10657 "Parser/parser.cc"
    break;

  case 441:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10663 "Parser/parser.cc"
    break;

  case 443:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10669 "Parser/parser.cc"
    break;

  case 444:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10675 "Parser/parser.cc"
    break;

  case 445:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10681 "Parser/parser.cc"
    break;

  case 446:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10692 "Parser/parser.cc"
    break;

  case 447:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10698 "Parser/parser.cc"
    break;

  case 448:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10704 "Parser/parser.cc"
    break;

  case 449:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10710 "Parser/parser.cc"
    break;

  case 450:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10716 "Parser/parser.cc"
    break;

  case 451:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10722 "Parser/parser.cc"
    break;

  case 452:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10728 "Parser/parser.cc"
    break;

  case 453:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10737 "Parser/parser.cc"
    break;

  case 454:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10746 "Parser/parser.cc"
    break;

  case 455:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10755 "Parser/parser.cc"
    break;

  case 456:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10766 "Parser/parser.cc"
    break;

  case 457:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10775 "Parser/parser.cc"
    break;

  case 458:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10781 "Parser/parser.cc"
    break;

  case 459:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10787 "Parser/parser.cc"
    break;

  case 460:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10793 "Parser/parser.cc"
    break;

  case 461:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10801 "Parser/parser.cc"
    break;

  case 462:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10809 "Parser/parser.cc"
    break;

  case 463:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10815 "Parser/parser.cc"
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
#line 10830 "Parser/parser.cc"
    break;

  case 467:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 468:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10842 "Parser/parser.cc"
    break;

  case 469:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10848 "Parser/parser.cc"
    break;

  case 470:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10854 "Parser/parser.cc"
    break;

  case 471:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 477:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10870 "Parser/parser.cc"
    break;

  case 490:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10876 "Parser/parser.cc"
    break;

  case 493:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10882 "Parser/parser.cc"
    break;

  case 494:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10888 "Parser/parser.cc"
    break;

  case 496:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10894 "Parser/parser.cc"
    break;

  case 497:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10900 "Parser/parser.cc"
    break;

  case 498:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10906 "Parser/parser.cc"
    break;

  case 499:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10912 "Parser/parser.cc"
    break;

  case 500:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10918 "Parser/parser.cc"
    break;

  case 501:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10924 "Parser/parser.cc"
    break;

  case 503:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10930 "Parser/parser.cc"
    break;

  case 504:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10936 "Parser/parser.cc"
    break;

  case 506:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10942 "Parser/parser.cc"
    break;

  case 507:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10948 "Parser/parser.cc"
    break;

  case 508:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10954 "Parser/parser.cc"
    break;

  case 509:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10960 "Parser/parser.cc"
    break;

  case 510:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 10966 "Parser/parser.cc"
    break;

  case 511:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 10972 "Parser/parser.cc"
    break;

  case 512:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 10978 "Parser/parser.cc"
    break;

  case 513:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 10984 "Parser/parser.cc"
    break;

  case 514:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 10990 "Parser/parser.cc"
    break;

  case 515:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 10996 "Parser/parser.cc"
    break;

  case 516:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11002 "Parser/parser.cc"
    break;

  case 517:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11008 "Parser/parser.cc"
    break;

  case 518:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11014 "Parser/parser.cc"
    break;

  case 519:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11020 "Parser/parser.cc"
    break;

  case 520:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11026 "Parser/parser.cc"
    break;

  case 521:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11032 "Parser/parser.cc"
    break;

  case 522:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11038 "Parser/parser.cc"
    break;

  case 523:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11044 "Parser/parser.cc"
    break;

  case 524:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11050 "Parser/parser.cc"
    break;

  case 525:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11056 "Parser/parser.cc"
    break;

  case 526:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11062 "Parser/parser.cc"
    break;

  case 527:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11068 "Parser/parser.cc"
    break;

  case 528:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11074 "Parser/parser.cc"
    break;

  case 529:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11080 "Parser/parser.cc"
    break;

  case 530:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11086 "Parser/parser.cc"
    break;

  case 531:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11092 "Parser/parser.cc"
    break;

  case 532:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11098 "Parser/parser.cc"
    break;

  case 533:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11104 "Parser/parser.cc"
    break;

  case 534:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11110 "Parser/parser.cc"
    break;

  case 535:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11116 "Parser/parser.cc"
    break;

  case 536:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11122 "Parser/parser.cc"
    break;

  case 537:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11128 "Parser/parser.cc"
    break;

  case 538:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11134 "Parser/parser.cc"
    break;

  case 539:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11140 "Parser/parser.cc"
    break;

  case 540:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11146 "Parser/parser.cc"
    break;

  case 541:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11152 "Parser/parser.cc"
    break;

  case 542:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11158 "Parser/parser.cc"
    break;

  case 543:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11164 "Parser/parser.cc"
    break;

  case 545:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11170 "Parser/parser.cc"
    break;

  case 547:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11176 "Parser/parser.cc"
    break;

  case 548:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11182 "Parser/parser.cc"
    break;

  case 549:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11188 "Parser/parser.cc"
    break;

  case 551:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11194 "Parser/parser.cc"
    break;

  case 552:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11200 "Parser/parser.cc"
    break;

  case 553:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11206 "Parser/parser.cc"
    break;

  case 554:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11212 "Parser/parser.cc"
    break;

  case 556:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11218 "Parser/parser.cc"
    break;

  case 558:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11224 "Parser/parser.cc"
    break;

  case 559:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11230 "Parser/parser.cc"
    break;

  case 560:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11236 "Parser/parser.cc"
    break;

  case 561:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11242 "Parser/parser.cc"
    break;

  case 562:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11248 "Parser/parser.cc"
    break;

  case 563:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11254 "Parser/parser.cc"
    break;

  case 564:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11260 "Parser/parser.cc"
    break;

  case 565:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 566:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11272 "Parser/parser.cc"
    break;

  case 568:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11278 "Parser/parser.cc"
    break;

  case 569:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11284 "Parser/parser.cc"
    break;

  case 570:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11290 "Parser/parser.cc"
    break;

  case 572:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11296 "Parser/parser.cc"
    break;

  case 573:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11302 "Parser/parser.cc"
    break;

  case 574:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11311 "Parser/parser.cc"
    break;

  case 576:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11317 "Parser/parser.cc"
    break;

  case 577:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11323 "Parser/parser.cc"
    break;

  case 578:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11329 "Parser/parser.cc"
    break;

  case 580:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 581:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11341 "Parser/parser.cc"
    break;

  case 583:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11347 "Parser/parser.cc"
    break;

  case 584:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11353 "Parser/parser.cc"
    break;

  case 585:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 586:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 587:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11371 "Parser/parser.cc"
    break;

  case 588:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11377 "Parser/parser.cc"
    break;

  case 589:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11383 "Parser/parser.cc"
    break;

  case 590:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11389 "Parser/parser.cc"
    break;

  case 591:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11395 "Parser/parser.cc"
    break;

  case 593:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 594:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11407 "Parser/parser.cc"
    break;

  case 595:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11413 "Parser/parser.cc"
    break;

  case 596:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11419 "Parser/parser.cc"
    break;

  case 597:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11425 "Parser/parser.cc"
    break;

  case 602:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11431 "Parser/parser.cc"
    break;

  case 603:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 604:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11446 "Parser/parser.cc"
    break;

  case 605:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11454 "Parser/parser.cc"
    break;

  case 606:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11463 "Parser/parser.cc"
    break;

  case 607:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11472 "Parser/parser.cc"
    break;

  case 608:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11481 "Parser/parser.cc"
    break;

  case 609:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11490 "Parser/parser.cc"
    break;

  case 611:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11496 "Parser/parser.cc"
    break;

  case 612:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11502 "Parser/parser.cc"
    break;

  case 613:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11512 "Parser/parser.cc"
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
#line 11531 "Parser/parser.cc"
    break;

  case 617:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11537 "Parser/parser.cc"
    break;

  case 618:
#line 2601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11543 "Parser/parser.cc"
    break;

  case 619:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11549 "Parser/parser.cc"
    break;

  case 620:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11555 "Parser/parser.cc"
    break;

  case 621:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11561 "Parser/parser.cc"
    break;

  case 622:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11567 "Parser/parser.cc"
    break;

  case 623:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11576 "Parser/parser.cc"
    break;

  case 624:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11582 "Parser/parser.cc"
    break;

  case 625:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11591 "Parser/parser.cc"
    break;

  case 626:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11597 "Parser/parser.cc"
    break;

  case 627:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11606 "Parser/parser.cc"
    break;

  case 628:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11612 "Parser/parser.cc"
    break;

  case 629:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11618 "Parser/parser.cc"
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
#line 11631 "Parser/parser.cc"
    break;

  case 631:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11640 "Parser/parser.cc"
    break;

  case 632:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11646 "Parser/parser.cc"
    break;

  case 633:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11652 "Parser/parser.cc"
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
#line 11665 "Parser/parser.cc"
    break;

  case 635:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11671 "Parser/parser.cc"
    break;

  case 638:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11677 "Parser/parser.cc"
    break;

  case 639:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11683 "Parser/parser.cc"
    break;

  case 642:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11689 "Parser/parser.cc"
    break;

  case 645:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11695 "Parser/parser.cc"
    break;

  case 646:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11701 "Parser/parser.cc"
    break;

  case 647:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11707 "Parser/parser.cc"
    break;

  case 648:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11713 "Parser/parser.cc"
    break;

  case 649:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11719 "Parser/parser.cc"
    break;

  case 650:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11725 "Parser/parser.cc"
    break;

  case 652:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11731 "Parser/parser.cc"
    break;

  case 654:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11737 "Parser/parser.cc"
    break;

  case 655:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11743 "Parser/parser.cc"
    break;

  case 657:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11749 "Parser/parser.cc"
    break;

  case 658:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11755 "Parser/parser.cc"
    break;

  case 660:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11761 "Parser/parser.cc"
    break;

  case 661:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11772 "Parser/parser.cc"
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
#line 11786 "Parser/parser.cc"
    break;

  case 663:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11792 "Parser/parser.cc"
    break;

  case 664:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11798 "Parser/parser.cc"
    break;

  case 665:
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11804 "Parser/parser.cc"
    break;

  case 666:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11815 "Parser/parser.cc"
    break;

  case 667:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11821 "Parser/parser.cc"
    break;

  case 668:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11827 "Parser/parser.cc"
    break;

  case 670:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11833 "Parser/parser.cc"
    break;

  case 671:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11839 "Parser/parser.cc"
    break;

  case 672:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11845 "Parser/parser.cc"
    break;

  case 673:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11851 "Parser/parser.cc"
    break;

  case 674:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11860 "Parser/parser.cc"
    break;

  case 675:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11869 "Parser/parser.cc"
    break;

  case 676:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11875 "Parser/parser.cc"
    break;

  case 677:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11885 "Parser/parser.cc"
    break;

  case 678:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11891 "Parser/parser.cc"
    break;

  case 679:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 11897 "Parser/parser.cc"
    break;

  case 681:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11903 "Parser/parser.cc"
    break;

  case 682:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11909 "Parser/parser.cc"
    break;

  case 683:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11915 "Parser/parser.cc"
    break;

  case 684:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11921 "Parser/parser.cc"
    break;

  case 685:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11927 "Parser/parser.cc"
    break;

  case 686:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11933 "Parser/parser.cc"
    break;

  case 688:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11939 "Parser/parser.cc"
    break;

  case 691:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11945 "Parser/parser.cc"
    break;

  case 692:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11951 "Parser/parser.cc"
    break;

  case 693:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 11957 "Parser/parser.cc"
    break;

  case 694:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11963 "Parser/parser.cc"
    break;

  case 697:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11969 "Parser/parser.cc"
    break;

  case 698:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11975 "Parser/parser.cc"
    break;

  case 699:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11981 "Parser/parser.cc"
    break;

  case 701:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 702:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 703:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 705:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12005 "Parser/parser.cc"
    break;

  case 706:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12011 "Parser/parser.cc"
    break;

  case 707:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12017 "Parser/parser.cc"
    break;

  case 708:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12023 "Parser/parser.cc"
    break;

  case 709:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12029 "Parser/parser.cc"
    break;

  case 711:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12035 "Parser/parser.cc"
    break;

  case 712:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 713:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 718:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 720:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 721:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 727:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 730:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12077 "Parser/parser.cc"
    break;

  case 731:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12083 "Parser/parser.cc"
    break;

  case 732:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12089 "Parser/parser.cc"
    break;

  case 733:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12095 "Parser/parser.cc"
    break;

  case 734:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12101 "Parser/parser.cc"
    break;

  case 735:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12107 "Parser/parser.cc"
    break;

  case 736:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12113 "Parser/parser.cc"
    break;

  case 738:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12119 "Parser/parser.cc"
    break;

  case 739:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12125 "Parser/parser.cc"
    break;

  case 740:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12131 "Parser/parser.cc"
    break;

  case 742:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12137 "Parser/parser.cc"
    break;

  case 744:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12143 "Parser/parser.cc"
    break;

  case 745:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12149 "Parser/parser.cc"
    break;

  case 746:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12155 "Parser/parser.cc"
    break;

  case 747:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12161 "Parser/parser.cc"
    break;

  case 748:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 749:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12173 "Parser/parser.cc"
    break;

  case 751:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 752:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12185 "Parser/parser.cc"
    break;

  case 753:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12191 "Parser/parser.cc"
    break;

  case 754:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12202 "Parser/parser.cc"
    break;

  case 755:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12208 "Parser/parser.cc"
    break;

  case 756:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12214 "Parser/parser.cc"
    break;

  case 757:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12220 "Parser/parser.cc"
    break;

  case 758:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12229 "Parser/parser.cc"
    break;

  case 759:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12235 "Parser/parser.cc"
    break;

  case 760:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12245 "Parser/parser.cc"
    break;

  case 761:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12251 "Parser/parser.cc"
    break;

  case 762:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12257 "Parser/parser.cc"
    break;

  case 763:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12263 "Parser/parser.cc"
    break;

  case 764:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12269 "Parser/parser.cc"
    break;

  case 765:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12275 "Parser/parser.cc"
    break;

  case 766:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12281 "Parser/parser.cc"
    break;

  case 767:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12287 "Parser/parser.cc"
    break;

  case 768:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12293 "Parser/parser.cc"
    break;

  case 769:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12299 "Parser/parser.cc"
    break;

  case 772:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12305 "Parser/parser.cc"
    break;

  case 773:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12311 "Parser/parser.cc"
    break;

  case 774:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12317 "Parser/parser.cc"
    break;

  case 775:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12323 "Parser/parser.cc"
    break;

  case 777:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12329 "Parser/parser.cc"
    break;

  case 778:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12335 "Parser/parser.cc"
    break;

  case 779:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12341 "Parser/parser.cc"
    break;

  case 780:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 781:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12353 "Parser/parser.cc"
    break;

  case 782:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12359 "Parser/parser.cc"
    break;

  case 783:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 784:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12374 "Parser/parser.cc"
    break;

  case 785:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12383 "Parser/parser.cc"
    break;

  case 786:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12392 "Parser/parser.cc"
    break;

  case 787:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12398 "Parser/parser.cc"
    break;

  case 788:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12407 "Parser/parser.cc"
    break;

  case 789:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12413 "Parser/parser.cc"
    break;

  case 791:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12419 "Parser/parser.cc"
    break;

  case 796:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 797:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12431 "Parser/parser.cc"
    break;

  case 798:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12437 "Parser/parser.cc"
    break;

  case 800:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 12443 "Parser/parser.cc"
    break;

  case 801:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12449 "Parser/parser.cc"
    break;

  case 802:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12455 "Parser/parser.cc"
    break;

  case 803:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12461 "Parser/parser.cc"
    break;

  case 805:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12467 "Parser/parser.cc"
    break;

  case 806:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12473 "Parser/parser.cc"
    break;

  case 807:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12479 "Parser/parser.cc"
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
#line 12495 "Parser/parser.cc"
    break;

  case 809:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12501 "Parser/parser.cc"
    break;

  case 810:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12507 "Parser/parser.cc"
    break;

  case 811:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12513 "Parser/parser.cc"
    break;

  case 812:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12519 "Parser/parser.cc"
    break;

  case 813:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12525 "Parser/parser.cc"
    break;

  case 814:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12531 "Parser/parser.cc"
    break;

  case 816:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12540 "Parser/parser.cc"
    break;

  case 817:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12546 "Parser/parser.cc"
    break;

  case 818:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12555 "Parser/parser.cc"
    break;

  case 819:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12565 "Parser/parser.cc"
    break;

  case 820:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12574 "Parser/parser.cc"
    break;

  case 821:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12584 "Parser/parser.cc"
    break;

  case 822:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12595 "Parser/parser.cc"
    break;

  case 823:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12605 "Parser/parser.cc"
    break;

  case 824:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12616 "Parser/parser.cc"
    break;

  case 825:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12626 "Parser/parser.cc"
    break;

  case 826:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12637 "Parser/parser.cc"
    break;

  case 827:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12647 "Parser/parser.cc"
    break;

  case 829:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12653 "Parser/parser.cc"
    break;

  case 830:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12659 "Parser/parser.cc"
    break;

  case 831:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12665 "Parser/parser.cc"
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
#line 12677 "Parser/parser.cc"
    break;

  case 833:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12688 "Parser/parser.cc"
    break;

  case 834:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12697 "Parser/parser.cc"
    break;

  case 835:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12706 "Parser/parser.cc"
    break;

  case 836:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12712 "Parser/parser.cc"
    break;

  case 837:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12718 "Parser/parser.cc"
    break;

  case 838:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12724 "Parser/parser.cc"
    break;

  case 839:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12733 "Parser/parser.cc"
    break;

  case 840:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12739 "Parser/parser.cc"
    break;

  case 841:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12745 "Parser/parser.cc"
    break;

  case 842:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12751 "Parser/parser.cc"
    break;

  case 847:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12757 "Parser/parser.cc"
    break;

  case 848:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12763 "Parser/parser.cc"
    break;

  case 849:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12773 "Parser/parser.cc"
    break;

  case 850:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12779 "Parser/parser.cc"
    break;

  case 853:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12785 "Parser/parser.cc"
    break;

  case 854:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12791 "Parser/parser.cc"
    break;

  case 855:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12797 "Parser/parser.cc"
    break;

  case 856:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12803 "Parser/parser.cc"
    break;

  case 858:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12809 "Parser/parser.cc"
    break;

  case 859:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12815 "Parser/parser.cc"
    break;

  case 860:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12821 "Parser/parser.cc"
    break;

  case 861:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12827 "Parser/parser.cc"
    break;

  case 866:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12833 "Parser/parser.cc"
    break;

  case 867:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12839 "Parser/parser.cc"
    break;

  case 868:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12845 "Parser/parser.cc"
    break;

  case 869:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12851 "Parser/parser.cc"
    break;

  case 870:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12857 "Parser/parser.cc"
    break;

  case 872:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12863 "Parser/parser.cc"
    break;

  case 873:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12869 "Parser/parser.cc"
    break;

  case 874:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12875 "Parser/parser.cc"
    break;

  case 875:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12881 "Parser/parser.cc"
    break;

  case 876:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12887 "Parser/parser.cc"
    break;

  case 877:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12893 "Parser/parser.cc"
    break;

  case 878:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12899 "Parser/parser.cc"
    break;

  case 879:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12905 "Parser/parser.cc"
    break;

  case 880:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12911 "Parser/parser.cc"
    break;

  case 881:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12917 "Parser/parser.cc"
    break;

  case 882:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12923 "Parser/parser.cc"
    break;

  case 883:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12929 "Parser/parser.cc"
    break;

  case 884:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12935 "Parser/parser.cc"
    break;

  case 885:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12941 "Parser/parser.cc"
    break;

  case 886:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12947 "Parser/parser.cc"
    break;

  case 887:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12953 "Parser/parser.cc"
    break;

  case 888:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12959 "Parser/parser.cc"
    break;

  case 889:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12965 "Parser/parser.cc"
    break;

  case 891:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12971 "Parser/parser.cc"
    break;

  case 892:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12977 "Parser/parser.cc"
    break;

  case 893:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12983 "Parser/parser.cc"
    break;

  case 894:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12989 "Parser/parser.cc"
    break;

  case 895:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12995 "Parser/parser.cc"
    break;

  case 896:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13001 "Parser/parser.cc"
    break;

  case 897:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13007 "Parser/parser.cc"
    break;

  case 898:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13013 "Parser/parser.cc"
    break;

  case 899:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13019 "Parser/parser.cc"
    break;

  case 900:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13025 "Parser/parser.cc"
    break;

  case 901:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13031 "Parser/parser.cc"
    break;

  case 902:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13037 "Parser/parser.cc"
    break;

  case 903:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13043 "Parser/parser.cc"
    break;

  case 904:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13049 "Parser/parser.cc"
    break;

  case 905:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13055 "Parser/parser.cc"
    break;

  case 906:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13061 "Parser/parser.cc"
    break;

  case 910:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13067 "Parser/parser.cc"
    break;

  case 911:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13073 "Parser/parser.cc"
    break;

  case 912:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13079 "Parser/parser.cc"
    break;

  case 913:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13085 "Parser/parser.cc"
    break;

  case 914:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13091 "Parser/parser.cc"
    break;

  case 915:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13097 "Parser/parser.cc"
    break;

  case 916:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13103 "Parser/parser.cc"
    break;

  case 917:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13109 "Parser/parser.cc"
    break;

  case 918:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13115 "Parser/parser.cc"
    break;

  case 919:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13121 "Parser/parser.cc"
    break;

  case 920:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13127 "Parser/parser.cc"
    break;

  case 921:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13133 "Parser/parser.cc"
    break;

  case 922:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13139 "Parser/parser.cc"
    break;

  case 923:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13145 "Parser/parser.cc"
    break;

  case 924:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13151 "Parser/parser.cc"
    break;

  case 925:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13160 "Parser/parser.cc"
    break;

  case 926:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13166 "Parser/parser.cc"
    break;

  case 927:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13172 "Parser/parser.cc"
    break;

  case 929:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13178 "Parser/parser.cc"
    break;

  case 930:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13184 "Parser/parser.cc"
    break;

  case 931:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13190 "Parser/parser.cc"
    break;

  case 932:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13196 "Parser/parser.cc"
    break;

  case 933:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13202 "Parser/parser.cc"
    break;

  case 934:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13208 "Parser/parser.cc"
    break;

  case 935:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13214 "Parser/parser.cc"
    break;

  case 936:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13220 "Parser/parser.cc"
    break;

  case 937:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13226 "Parser/parser.cc"
    break;

  case 938:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13232 "Parser/parser.cc"
    break;

  case 939:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13238 "Parser/parser.cc"
    break;

  case 940:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13244 "Parser/parser.cc"
    break;

  case 941:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13250 "Parser/parser.cc"
    break;

  case 942:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13256 "Parser/parser.cc"
    break;

  case 943:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13262 "Parser/parser.cc"
    break;

  case 944:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13268 "Parser/parser.cc"
    break;

  case 945:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13274 "Parser/parser.cc"
    break;

  case 946:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13280 "Parser/parser.cc"
    break;

  case 948:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13286 "Parser/parser.cc"
    break;

  case 949:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13292 "Parser/parser.cc"
    break;

  case 950:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13298 "Parser/parser.cc"
    break;

  case 951:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13304 "Parser/parser.cc"
    break;

  case 952:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13310 "Parser/parser.cc"
    break;

  case 953:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13316 "Parser/parser.cc"
    break;

  case 954:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13322 "Parser/parser.cc"
    break;

  case 955:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13328 "Parser/parser.cc"
    break;

  case 956:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13334 "Parser/parser.cc"
    break;

  case 957:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13340 "Parser/parser.cc"
    break;

  case 958:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13346 "Parser/parser.cc"
    break;

  case 959:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13352 "Parser/parser.cc"
    break;

  case 960:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13358 "Parser/parser.cc"
    break;

  case 961:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13364 "Parser/parser.cc"
    break;

  case 962:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13370 "Parser/parser.cc"
    break;

  case 963:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13376 "Parser/parser.cc"
    break;

  case 964:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13382 "Parser/parser.cc"
    break;

  case 965:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13389 "Parser/parser.cc"
    break;

  case 967:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13395 "Parser/parser.cc"
    break;

  case 968:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13401 "Parser/parser.cc"
    break;

  case 969:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13407 "Parser/parser.cc"
    break;

  case 970:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13413 "Parser/parser.cc"
    break;

  case 971:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13419 "Parser/parser.cc"
    break;

  case 972:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13425 "Parser/parser.cc"
    break;

  case 973:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13431 "Parser/parser.cc"
    break;

  case 974:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13437 "Parser/parser.cc"
    break;

  case 975:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13443 "Parser/parser.cc"
    break;

  case 976:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13449 "Parser/parser.cc"
    break;

  case 977:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13455 "Parser/parser.cc"
    break;

  case 978:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13461 "Parser/parser.cc"
    break;

  case 979:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13467 "Parser/parser.cc"
    break;

  case 980:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13474 "Parser/parser.cc"
    break;

  case 982:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13480 "Parser/parser.cc"
    break;

  case 983:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13486 "Parser/parser.cc"
    break;

  case 984:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13492 "Parser/parser.cc"
    break;

  case 985:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13498 "Parser/parser.cc"
    break;

  case 986:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13504 "Parser/parser.cc"
    break;

  case 987:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13510 "Parser/parser.cc"
    break;

  case 988:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13516 "Parser/parser.cc"
    break;

  case 989:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13522 "Parser/parser.cc"
    break;

  case 990:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13528 "Parser/parser.cc"
    break;

  case 991:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13534 "Parser/parser.cc"
    break;

  case 992:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13540 "Parser/parser.cc"
    break;

  case 994:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13546 "Parser/parser.cc"
    break;

  case 995:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13552 "Parser/parser.cc"
    break;

  case 996:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13558 "Parser/parser.cc"
    break;

  case 997:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13564 "Parser/parser.cc"
    break;

  case 998:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13570 "Parser/parser.cc"
    break;

  case 999:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13576 "Parser/parser.cc"
    break;

  case 1000:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13582 "Parser/parser.cc"
    break;

  case 1002:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13588 "Parser/parser.cc"
    break;

  case 1003:
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13594 "Parser/parser.cc"
    break;

  case 1004:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13600 "Parser/parser.cc"
    break;

  case 1005:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13606 "Parser/parser.cc"
    break;

  case 1006:
#line 3942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13612 "Parser/parser.cc"
    break;

  case 1007:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13618 "Parser/parser.cc"
    break;

  case 1008:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13624 "Parser/parser.cc"
    break;

  case 1009:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13630 "Parser/parser.cc"
    break;

  case 1010:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13636 "Parser/parser.cc"
    break;

  case 1011:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13642 "Parser/parser.cc"
    break;

  case 1013:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13648 "Parser/parser.cc"
    break;

  case 1014:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13654 "Parser/parser.cc"
    break;

  case 1016:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13660 "Parser/parser.cc"
    break;

  case 1017:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13666 "Parser/parser.cc"
    break;

  case 1019:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13672 "Parser/parser.cc"
    break;

  case 1020:
#line 3988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13678 "Parser/parser.cc"
    break;

  case 1021:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13684 "Parser/parser.cc"
    break;

  case 1022:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13690 "Parser/parser.cc"
    break;

  case 1023:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13696 "Parser/parser.cc"
    break;

  case 1024:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13702 "Parser/parser.cc"
    break;

  case 1025:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13708 "Parser/parser.cc"
    break;

  case 1028:
#line 4040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13715 "Parser/parser.cc"
    break;

  case 1029:
#line 4043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13721 "Parser/parser.cc"
    break;

  case 1030:
#line 4045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13727 "Parser/parser.cc"
    break;

  case 1031:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13733 "Parser/parser.cc"
    break;

  case 1032:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13739 "Parser/parser.cc"
    break;

  case 1033:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13745 "Parser/parser.cc"
    break;

  case 1034:
#line 4056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13751 "Parser/parser.cc"
    break;

  case 1035:
#line 4058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13757 "Parser/parser.cc"
    break;

  case 1037:
#line 4064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13763 "Parser/parser.cc"
    break;

  case 1038:
#line 4066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13769 "Parser/parser.cc"
    break;

  case 1039:
#line 4068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13775 "Parser/parser.cc"
    break;

  case 1040:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13781 "Parser/parser.cc"
    break;

  case 1041:
#line 4075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13787 "Parser/parser.cc"
    break;

  case 1042:
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13793 "Parser/parser.cc"
    break;

  case 1044:
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13799 "Parser/parser.cc"
    break;

  case 1046:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13805 "Parser/parser.cc"
    break;

  case 1047:
#line 4098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13811 "Parser/parser.cc"
    break;

  case 1048:
#line 4100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13817 "Parser/parser.cc"
    break;

  case 1049:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13823 "Parser/parser.cc"
    break;

  case 1050:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13829 "Parser/parser.cc"
    break;

  case 1051:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13835 "Parser/parser.cc"
    break;

  case 1053:
#line 4122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13841 "Parser/parser.cc"
    break;

  case 1054:
#line 4124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13847 "Parser/parser.cc"
    break;

  case 1055:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13853 "Parser/parser.cc"
    break;

  case 1056:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13859 "Parser/parser.cc"
    break;

  case 1057:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13865 "Parser/parser.cc"
    break;

  case 1058:
#line 4135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13871 "Parser/parser.cc"
    break;

  case 1059:
#line 4137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13877 "Parser/parser.cc"
    break;

  case 1061:
#line 4143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13883 "Parser/parser.cc"
    break;

  case 1062:
#line 4145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13889 "Parser/parser.cc"
    break;

  case 1063:
#line 4147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13895 "Parser/parser.cc"
    break;

  case 1064:
#line 4152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13901 "Parser/parser.cc"
    break;

  case 1065:
#line 4154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13907 "Parser/parser.cc"
    break;

  case 1068:
#line 4164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13913 "Parser/parser.cc"
    break;

  case 1071:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13919 "Parser/parser.cc"
    break;

  case 1072:
#line 4177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13925 "Parser/parser.cc"
    break;

  case 1073:
#line 4179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13931 "Parser/parser.cc"
    break;

  case 1074:
#line 4181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13937 "Parser/parser.cc"
    break;

  case 1075:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13943 "Parser/parser.cc"
    break;

  case 1076:
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13949 "Parser/parser.cc"
    break;

  case 1077:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13955 "Parser/parser.cc"
    break;

  case 1078:
#line 4194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13961 "Parser/parser.cc"
    break;

  case 1079:
#line 4196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13967 "Parser/parser.cc"
    break;

  case 1080:
#line 4198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13973 "Parser/parser.cc"
    break;

  case 1081:
#line 4200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13979 "Parser/parser.cc"
    break;

  case 1082:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13985 "Parser/parser.cc"
    break;

  case 1083:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13991 "Parser/parser.cc"
    break;

  case 1084:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13997 "Parser/parser.cc"
    break;

  case 1085:
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14003 "Parser/parser.cc"
    break;

  case 1086:
#line 4211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14009 "Parser/parser.cc"
    break;

  case 1087:
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14015 "Parser/parser.cc"
    break;

  case 1088:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14021 "Parser/parser.cc"
    break;

  case 1089:
#line 4223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14027 "Parser/parser.cc"
    break;

  case 1090:
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14033 "Parser/parser.cc"
    break;

  case 1092:
#line 4252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14039 "Parser/parser.cc"
    break;

  case 1096:
#line 4263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14045 "Parser/parser.cc"
    break;

  case 1097:
#line 4265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14051 "Parser/parser.cc"
    break;

  case 1098:
#line 4267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14057 "Parser/parser.cc"
    break;

  case 1099:
#line 4269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14063 "Parser/parser.cc"
    break;

  case 1100:
#line 4271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14069 "Parser/parser.cc"
    break;

  case 1101:
#line 4273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14075 "Parser/parser.cc"
    break;

  case 1102:
#line 4280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14081 "Parser/parser.cc"
    break;

  case 1103:
#line 4282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14087 "Parser/parser.cc"
    break;

  case 1104:
#line 4284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14093 "Parser/parser.cc"
    break;

  case 1105:
#line 4286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14099 "Parser/parser.cc"
    break;

  case 1106:
#line 4288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14105 "Parser/parser.cc"
    break;

  case 1107:
#line 4290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14111 "Parser/parser.cc"
    break;

  case 1108:
#line 4295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14117 "Parser/parser.cc"
    break;

  case 1109:
#line 4297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14123 "Parser/parser.cc"
    break;

  case 1110:
#line 4299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14129 "Parser/parser.cc"
    break;

  case 1111:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14135 "Parser/parser.cc"
    break;

  case 1112:
#line 4306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14141 "Parser/parser.cc"
    break;

  case 1113:
#line 4308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14147 "Parser/parser.cc"
    break;

  case 1116:
#line 4332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14153 "Parser/parser.cc"
    break;

  case 1117:
#line 4334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14159 "Parser/parser.cc"
    break;


#line 14163 "Parser/parser.cc"

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
