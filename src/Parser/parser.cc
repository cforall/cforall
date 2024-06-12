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
#define YYLAST   26101

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  181
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1113
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2194

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
     678,   683,   684,   685,   689,   693,   694,   705,   707,   709,
     711,   712,   714,   716,   718,   720,   730,   732,   734,   736,
     738,   740,   745,   746,   757,   762,   767,   768,   773,   779,
     781,   783,   789,   791,   795,   797,   799,   819,   822,   824,
     826,   828,   830,   832,   834,   836,   838,   840,   842,   844,
     854,   855,   859,   860,   865,   868,   872,   873,   877,   878,
     880,   882,   884,   886,   888,   893,   895,   897,   905,   906,
     914,   917,   918,   920,   925,   941,   943,   945,   947,   949,
     951,   953,   958,   960,   963,   965,   973,   974,   976,   980,
     981,   982,   983,   987,   988,   990,   992,   994,   996,   998,
    1000,  1002,  1009,  1010,  1011,  1012,  1016,  1017,  1021,  1022,
    1027,  1028,  1030,  1032,  1037,  1038,  1040,  1045,  1046,  1048,
    1053,  1054,  1056,  1058,  1060,  1065,  1066,  1068,  1073,  1074,
    1079,  1080,  1085,  1086,  1091,  1092,  1097,  1098,  1103,  1104,
    1106,  1111,  1116,  1117,  1125,  1131,  1132,  1136,  1137,  1141,
    1142,  1146,  1147,  1148,  1149,  1150,  1151,  1152,  1153,  1154,
    1155,  1156,  1166,  1168,  1173,  1174,  1176,  1178,  1183,  1184,
    1190,  1191,  1197,  1198,  1199,  1200,  1201,  1202,  1203,  1204,
    1205,  1206,  1207,  1208,  1209,  1210,  1212,  1213,  1219,  1221,
    1231,  1233,  1241,  1242,  1247,  1249,  1251,  1253,  1255,  1259,
    1260,  1262,  1268,  1297,  1300,  1302,  1304,  1314,  1316,  1318,
    1323,  1328,  1330,  1332,  1334,  1342,  1343,  1345,  1349,  1351,
    1355,  1357,  1358,  1360,  1362,  1367,  1368,  1372,  1377,  1378,
    1382,  1384,  1389,  1391,  1396,  1398,  1400,  1402,  1407,  1409,
    1411,  1413,  1418,  1420,  1425,  1426,  1448,  1450,  1455,  1458,
    1460,  1463,  1465,  1468,  1470,  1475,  1480,  1482,  1487,  1492,
    1494,  1496,  1498,  1500,  1503,  1505,  1508,  1510,  1515,  1521,
    1524,  1526,  1531,  1537,  1539,  1544,  1550,  1553,  1555,  1558,
    1560,  1565,  1572,  1574,  1579,  1585,  1587,  1592,  1598,  1601,
    1606,  1616,  1617,  1621,  1623,  1625,  1630,  1632,  1637,  1638,
    1640,  1645,  1647,  1652,  1654,  1656,  1658,  1661,  1665,  1668,
    1672,  1674,  1676,  1678,  1680,  1682,  1684,  1686,  1688,  1690,
    1692,  1697,  1698,  1702,  1708,  1716,  1721,  1722,  1726,  1727,
    1732,  1736,  1737,  1740,  1742,  1747,  1750,  1752,  1754,  1757,
    1759,  1764,  1769,  1770,  1774,  1779,  1781,  1786,  1788,  1793,
    1795,  1797,  1802,  1807,  1812,  1817,  1819,  1821,  1826,  1828,
    1834,  1835,  1839,  1840,  1841,  1842,  1846,  1851,  1852,  1854,
    1856,  1858,  1862,  1866,  1867,  1871,  1873,  1875,  1877,  1879,
    1885,  1886,  1892,  1893,  1897,  1898,  1903,  1905,  1914,  1915,
    1917,  1922,  1927,  1938,  1939,  1943,  1944,  1950,  1951,  1955,
    1957,  1961,  1963,  1967,  1968,  1972,  1973,  1977,  1978,  1979,
    1983,  1985,  2000,  2001,  2002,  2003,  2005,  2009,  2011,  2015,
    2022,  2024,  2026,  2034,  2036,  2041,  2042,  2044,  2046,  2048,
    2058,  2060,  2072,  2075,  2080,  2082,  2088,  2093,  2098,  2109,
    2116,  2121,  2123,  2125,  2131,  2135,  2142,  2144,  2145,  2146,
    2162,  2164,  2167,  2169,  2172,  2177,  2178,  2182,  2183,  2184,
    2185,  2194,  2195,  2196,  2205,  2206,  2207,  2211,  2212,  2213,
    2222,  2223,  2224,  2229,  2230,  2239,  2240,  2245,  2247,  2251,
    2253,  2255,  2257,  2264,  2269,  2274,  2275,  2277,  2287,  2288,
    2293,  2295,  2297,  2299,  2301,  2303,  2306,  2308,  2310,  2315,
    2321,  2323,  2325,  2327,  2329,  2331,  2333,  2335,  2337,  2339,
    2341,  2343,  2345,  2347,  2349,  2351,  2353,  2355,  2357,  2359,
    2361,  2363,  2365,  2367,  2369,  2371,  2373,  2375,  2380,  2381,
    2385,  2391,  2392,  2398,  2399,  2401,  2403,  2405,  2410,  2412,
    2417,  2418,  2420,  2422,  2427,  2429,  2431,  2433,  2435,  2437,
    2442,  2443,  2445,  2447,  2452,  2454,  2453,  2457,  2465,  2466,
    2468,  2470,  2475,  2476,  2478,  2483,  2484,  2486,  2488,  2493,
    2495,  2497,  2502,  2504,  2506,  2508,  2509,  2511,  2516,  2518,
    2520,  2525,  2526,  2530,  2531,  2538,  2537,  2542,  2541,  2551,
    2550,  2561,  2560,  2570,  2575,  2576,  2581,  2587,  2605,  2606,
    2610,  2612,  2614,  2619,  2621,  2623,  2625,  2630,  2632,  2637,
    2639,  2648,  2649,  2654,  2663,  2668,  2670,  2672,  2681,  2683,
    2684,  2685,  2687,  2689,  2690,  2695,  2696,  2697,  2702,  2704,
    2707,  2710,  2717,  2718,  2719,  2725,  2730,  2732,  2738,  2739,
    2745,  2746,  2750,  2758,  2765,  2778,  2777,  2781,  2784,  2783,
    2792,  2796,  2800,  2802,  2808,  2809,  2814,  2819,  2827,  2829,
    2835,  2837,  2842,  2843,  2849,  2850,  2851,  2860,  2861,  2863,
    2864,  2869,  2870,  2871,  2873,  2879,  2880,  2882,  2883,  2884,
    2886,  2888,  2895,  2896,  2898,  2900,  2905,  2906,  2915,  2917,
    2922,  2924,  2929,  2930,  2932,  2935,  2937,  2941,  2942,  2943,
    2945,  2947,  2955,  2957,  2962,  2963,  2964,  2969,  2970,  2975,
    2976,  2977,  2978,  2982,  2983,  2988,  2989,  2990,  2991,  2992,
    3006,  3007,  3012,  3013,  3019,  3021,  3024,  3026,  3028,  3051,
    3052,  3058,  3059,  3065,  3064,  3074,  3073,  3077,  3083,  3085,
    3095,  3096,  3098,  3102,  3107,  3109,  3111,  3113,  3119,  3120,
    3124,  3125,  3130,  3132,  3139,  3141,  3142,  3144,  3149,  3151,
    3153,  3158,  3160,  3165,  3170,  3178,  3183,  3185,  3190,  3195,
    3196,  3201,  3202,  3206,  3207,  3208,  3213,  3215,  3221,  3223,
    3228,  3230,  3236,  3237,  3241,  3245,  3249,  3251,  3263,  3265,
    3267,  3269,  3271,  3273,  3275,  3276,  3281,  3284,  3283,  3295,
    3294,  3307,  3306,  3320,  3319,  3333,  3332,  3348,  3354,  3356,
    3362,  3363,  3374,  3381,  3386,  3392,  3395,  3398,  3402,  3408,
    3411,  3414,  3419,  3420,  3421,  3422,  3426,  3434,  3435,  3447,
    3448,  3452,  3453,  3458,  3460,  3462,  3467,  3468,  3474,  3475,
    3477,  3482,  3483,  3485,  3520,  3522,  3527,  3529,  3530,  3532,
    3537,  3539,  3541,  3543,  3548,  3550,  3552,  3554,  3556,  3558,
    3560,  3565,  3567,  3569,  3571,  3580,  3582,  3583,  3588,  3590,
    3592,  3594,  3596,  3601,  3603,  3605,  3607,  3612,  3614,  3616,
    3618,  3620,  3622,  3634,  3635,  3636,  3640,  3642,  3644,  3646,
    3648,  3653,  3655,  3657,  3659,  3664,  3666,  3668,  3670,  3672,
    3674,  3686,  3691,  3696,  3698,  3699,  3701,  3706,  3708,  3710,
    3712,  3717,  3719,  3721,  3723,  3725,  3727,  3729,  3734,  3736,
    3738,  3740,  3749,  3751,  3752,  3757,  3759,  3761,  3763,  3765,
    3770,  3772,  3774,  3776,  3781,  3783,  3785,  3787,  3789,  3791,
    3801,  3803,  3806,  3807,  3809,  3814,  3816,  3818,  3823,  3825,
    3827,  3829,  3834,  3836,  3838,  3852,  3854,  3857,  3858,  3860,
    3865,  3867,  3872,  3874,  3876,  3881,  3883,  3888,  3890,  3907,
    3908,  3910,  3915,  3917,  3919,  3921,  3923,  3928,  3929,  3931,
    3933,  3938,  3940,  3942,  3948,  3950,  3953,  3960,  3962,  3971,
    3973,  3975,  3976,  3978,  3980,  3984,  3986,  3991,  3993,  3995,
    3997,  4032,  4033,  4037,  4038,  4041,  4043,  4048,  4050,  4052,
    4054,  4056,  4061,  4062,  4064,  4066,  4071,  4073,  4075,  4081,
    4082,  4084,  4093,  4096,  4098,  4101,  4103,  4105,  4119,  4120,
    4122,  4127,  4129,  4131,  4133,  4135,  4140,  4141,  4143,  4145,
    4150,  4152,  4160,  4161,  4162,  4167,  4168,  4173,  4175,  4177,
    4179,  4181,  4183,  4190,  4192,  4194,  4196,  4198,  4201,  4203,
    4205,  4207,  4209,  4214,  4216,  4218,  4223,  4249,  4250,  4252,
    4256,  4257,  4261,  4263,  4265,  4267,  4269,  4271,  4278,  4280,
    4282,  4284,  4286,  4288,  4293,  4295,  4297,  4302,  4304,  4306,
    4324,  4326,  4331,  4332
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
  "quasi_keyword", "identifier", "identifier_at",
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
     405,   406,   407,   408,   125,    40,    64,    41,    46,    91,
      93,    44,    58,   123,    96,    94,    42,    38,    43,    45,
      33,   126,    92,    47,    37,    60,    62,   124,    63,    61,
      59
};
# endif

#define YYPACT_NINF (-1886)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1112)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      57, 12772,   117,   184, 19383,    31, -1886, -1886, -1886, -1886,
   -1886, -1886, -1886, -1886, -1886, -1886, -1886, -1886,    95,  1008,
     115, -1886, -1886, -1886, -1886, -1886, -1886, -1886, -1886, -1886,
   -1886, -1886, -1886, -1886, -1886, -1886, -1886, -1886, -1886, -1886,
   -1886, -1886, -1886, -1886, -1886, -1886, -1886, -1886,   360,   270,
   -1886, -1886, -1886, -1886, -1886, -1886,  4673,  4673,   156, 12772,
     218,   320, 23052, -1886,   340, -1886, -1886, -1886, -1886, -1886,
   -1886, -1886, -1886, -1886, -1886,   351,   978, -1886,   505,   303,
   -1886, -1886, -1886, -1886, -1886, 18918, -1886, -1886,   352,   468,
     374,    41, -1886,  4461,   489,   517,   538,   486,  5577,   551,
    1107, 12937, -1886, -1886,   682, 18763,  1329, -1886, -1886, -1886,
   -1886,  2278,   774,  7158,  9647,   783,  2278,   834,   627, -1886,
   -1886, -1886, -1886,     2, -1886, -1886, -1886, -1886,   604, -1886,
   -1886, -1886, -1886, -1886,   659,   653,     2, -1886,     2, 17081,
   -1886, -1886, -1886, 20565,  4673, -1886, -1886,  4673, -1886, 12772,
   -1886,   687, 20618, -1886, -1886,  5714, 21917, -1886, -1886,   996,
     996,   709,  3760, -1886, -1886, -1886, -1886,    81, 15377,     2,
    3838,     2, -1886, -1886, -1886, -1886, -1886, -1886,   736, -1886,
     716,   749,  1333, -1886,   796, 25550, -1886, -1886, -1886, -1886,
   -1886, -1886, -1886, 17460,  3200,  3129,   978,   553,   765,   771,
     789,   802,   830,   840, -1886, -1886, 19538, 11527,   809,   853,
   -1886,  9449, -1886, -1886, -1886, -1886,   860, -1886, -1886,   864,
   -1886, 23498,  1013, 23650, -1886,   875,  4673,   653,   883,   894,
   -1886,  1654,  5714,  1654, -1886, -1886, -1886,  3489,  5311,   925,
    1003,   262,  1003, -1886,     2,     2,    70, 16759,   584,  1003,
   -1886,     2,     2,    70,     2, -1886,     2, -1886,  5899, -1886,
   -1886,   958,   975,   996, 22809,   986, 18918, -1886,  4461, -1886,
    2278, -1886,  2101,   627,   972,  1049, 16759,  4673,  4673,   374,
   -1886, 14557, -1886,   996,   996,   997,  1049, 16759,  4673, -1886,
   23282, -1886, -1886, -1886,   996, -1886, -1886, -1886, -1886,   996,
   -1886,  1002,  4122,  4673, -1886, 18617,   987, -1886, -1886, -1886,
   22672,   653, 16920,  1001,  5714, 18564, 22809,  2278, -1886, -1886,
   22213, -1886,  1003,   -20, -1886, 25550, 22065,  4399,  5899, -1886,
     704, -1886, -1886, -1886, -1886, -1886, 20618,  1003,  4673, -1886,
    1004,  1034, -1886, -1886, -1886, -1886,  4673,  3947,   527,   250,
   -1886,  4673,   716, -1886,  1015,     2, -1886,  1042, 20773,  1222,
   15869, 22862,  2278, -1886,  2278,   996,  2278,   996, -1886, -1886,
       2, -1886, -1886,  1082, 20826, -1886, -1886, -1886, 20981,   860,
   -1886,  3465,   441,   373, -1886,   631,   627,  1041,  1080, -1886,
    3760,  1053,   716,  3760, -1886, -1886,  3200, -1886,   284, -1886,
    1097, -1886,  1100,  1157, 25626,  1138,  1150,  1166, 25550, 25702,
    1173, 23167, -1886, -1886, -1886, -1886, -1886, -1886, 25778, 25778,
   17301,  1189,  4843, -1886, -1886, -1886, -1886,   344, -1886,   766,
   -1886,  1386, -1886, 25550, 25550, -1886,  1180,   714,  1095,  1202,
     660,  1275,  1199,  1192,  1178,  1234,     5, -1886,   480, -1886,
    1218, -1886,  1213,  4193, 17937, -1886, -1886,   607,  1218, -1886,
   -1886,   724, -1886, -1886,   814,  3129,  1228,  1230,  1233,  1238,
    1243,  1248, -1886, -1886,   782,  1260, -1886,   944,  1260,  1299,
   -1886,  1306, -1886, 20565, -1886,  1264,  1277, 18096, -1886, -1886,
    4568,  5346,  1338, 15869,  1343,   806,   896,  1305,  1324, -1886,
   -1886, -1886,  4673,  4605, 20047, -1886, -1886, -1886, -1886, -1886,
   -1886, -1886, 18423,  4500,  1189, 23498,  1310,  1325, -1886, -1886,
    1328, 23650,   862, -1886, -1886, -1886, 17937,  1345, -1886,   796,
   -1886, -1886, -1886,  1344,  3489,   887,  1351,  1353,  1357,   905,
    1382,  1424,  1428,  1435,  1440,  1458,  5311, -1886, -1886, -1886,
       2,  1389,  1452, -1886, -1886,  1464,   374, -1886, -1886,   653,
    1049, 19702, -1886, -1886,   374, -1886, -1886,   653, -1886, -1886,
    5899, -1886, 17937, 17937, -1886,   996,  5714, 22999,  2258, 16033,
   -1886, -1886, -1886, -1886, -1886, -1886,   653,  1049,   -20,  1462,
   -1886, -1886,  2278,  1468,  1049, 16759, -1886,   653,  1049, -1886,
   23333, -1886,   996,   996, -1886, -1886,  1472,   161,  1483,   627,
    1486, -1886, -1886, -1886, 19994,  1470,  1479, -1886, -1886,   827,
   -1886,  1588, -1886,  1487, -1886, -1886, -1886, 21145, 25854, -1886,
   -1886, -1886, -1886, -1886,  4399,   916,  5899, 19702,  1003, 12772,
   -1886,  4673,  1504, -1886,  1515, -1886, -1886, -1886, -1886, -1886,
    3760, -1886, -1886,  1602,  4343, 20202, 11527, -1886, 21198, -1886,
     996,   996, -1886, -1886,   860, -1886, 14885,  1525,  1668, 25550,
    2312,  1464,  1510, -1886,     2,     2, -1886,  1260, -1886, 20773,
   -1886, -1886, 19994,   996,   996, -1886,  4343, -1886, -1886, 21769,
   -1886, -1886, 20826, -1886,     2,  1532,     2,  1080,   238,  1533,
     962, 20618,   963,   964, -1886,  3200, 23726,  1517, -1886, 17619,
   -1886,  4843, 21353, 20618, -1886, 17619, -1886, 25550, -1886, -1886,
   -1886, -1886, -1886, -1886, 17778, -1886, -1886, 20255, 21353, 21353,
    1297,  1282,  1298,   739,  1397, -1886,   970,  1542,  1311,  1543,
   -1886, 23802, 25550, 23878,  1541, 25550,  1654, 25550,  1654, -1886,
    2204, -1886, -1886, 23726,  3011, 25550, 23726,  1654, -1886, -1886,
   25550, 25550, 25550, 25550, 25550, 25550, 25550, 25550, 25550, 25550,
   25550, 25550, 25550, 25550, 25550, 25550, 25550, 25550, 25550, 23954,
    1526,   796,  4043, 11527, -1886, -1886, -1886, -1886, -1886, -1886,
   -1886, -1886, -1886, -1886, -1886,  1544, 25550, -1886, -1886, 15049,
    2472, -1886, -1886,     2,     2, -1886, -1886, 17937, -1886, -1886,
     797,  1260, -1886,   992,  1260, 19702, -1886, -1886,  1464, 19702,
   -1886,  1464, -1886, 25930, -1886, -1886, -1886, 19228, 11527,  1545,
    1303,  1551, 14393,  1695,  4533,   843,  1510, -1886,     2,     2,
    1510,   867, -1886,     2,     2, 25550,  4673, 16033,  1554, 16033,
    1557,  1510,   258, 15213, 15213, 16197, 15213,  4673, -1886, -1886,
   25550,  1328, -1886, 23498,  1567, -1886,  1317, -1886, -1886, -1886,
    1031, -1886,  1569, 15213, 25550,  1036,  1570,  1571,  1572,  1050,
    1576,  1577,  1579,  1583,  1585,  1595,   868,  1260, -1886, -1886,
     892,  1260, -1886, -1886,   900,  1260, -1886, -1886, -1886,  5714,
    1731,  1260, 22361, -1886, -1886,   653,  1598, -1886, -1886, -1886,
    1055,  1599,  1058,  1600, -1886,  1299,  1601,  1604, -1886,   653,
   -1886,  1605, -1886,   653,  1049,  1604, -1886,   653,  1610,  1611,
    1612, -1886, -1886, 19857, -1886,  1654,  4673, 10656,  1706, -1886,
    1277, -1886, 15213,  1075, -1886,  1604,  1620, -1886, 21406, 17937,
    1608, -1886,  1608, -1886, -1886, -1886, -1886, 20826, -1886, 11695,
   18255, -1886,  1621,  1622,  1624,  1627, -1886,  9774,     2, -1886,
    2312, -1886, -1886, -1886, -1886,  1464, -1886, -1886, -1886,   996,
   -1886, -1886, -1886, -1886,   238,  1080,  1628,    81, -1886, -1886,
    1629,  4673,   238, -1886, -1886,  1630,  1635, -1886, -1886,  1074,
   -1886, -1886, -1886, -1886,  1637,  1639,  1636,  1638,  1640,  1643,
    1645,  1647,  1644,  1649, 25550,  1651,  1652,  1653, 21561, 11863,
   25550, -1886, -1886,  1589, -1886, -1886, -1886, 25550, -1886,  1657,
    1660, 23574, -1886, -1886,  1322, -1886, 23726,  1658, -1886,  1659,
   -1886, -1886,  2339, -1886,  1090, -1886,  2339, -1886, -1886,  1331,
     760, -1886, -1886,  1180,  1180,  1180,   714,   714,  1095,  1095,
    1202,  1202,  1202,  1202,   660,   660,  1275,  1199,  1192,  1178,
    1234, 25550,  1335, -1886,  1665,  2339, -1886, -1886, 23498, -1886,
    1669,  1670,  1672,  1674,  2472, -1886, -1886, -1886, -1886, -1886,
   19702, -1886, -1886,  1464, 19702, -1886,  1464,  1675,  1678, 15213,
   15213, -1886, -1886, 14393,   924,  1680,  1681,  1682,  1684,  4370,
    4533, -1886, -1886, 19702, -1886, -1886, -1886, -1886, -1886, -1886,
   19702, -1886, -1886, -1886, -1886,  1683, -1886,  1510,  1686, -1886,
   -1886, -1886, -1886, -1886, -1886, -1886, -1886,  1692,  1689,  1690,
   -1886,  1693, -1886,   374,  2339,  1368,    37, -1886, -1886,  1701,
   -1886, 23650, -1886, 25550,     2, 15213,     2, -1886, -1886,   921,
    1260, -1886,   946,  1260, -1886, -1886,   981,  1260, 19702, -1886,
   -1886,  1464, 19702, -1886, -1886,  1464, 19702, -1886, -1886,  1464,
    1003, -1886,  1464,    -7, -1886,  1218,  1697, -1886, -1886, -1886,
   -1886, -1886, -1886,  1702, -1886, -1886, -1886, 21406,  1604, -1886,
     653, -1886, -1886, -1886, -1886, -1886, 13590, -1886, -1886, -1886,
   -1886, -1886,   -47,   528,   125, 11359,  1704,  1705, 16581,  1708,
    1710,  3329,  3814,  2689, 24030,  1711, -1886, -1886,  1719,  1721,
   16581,  1732, -1886, -1886,   653, 25550, 25550,  1847,  1726,   762,
   -1886, 17142,  1372,  1727,  1737,  1713, -1886, -1886, -1886, 10478,
   -1886, -1886, -1886, -1886, -1886,  2806, -1886, -1886, -1886,  1455,
     304, -1886,   393, -1886,   304, -1886, -1886, -1886, -1886, -1886,
    1654, -1886, -1886, 13102, 19073, -1886,  4673,  1739,  1742, -1886,
   -1886, -1886,  4673, -1886, -1886,  5714, -1886, -1886,  1724,  1725,
    1098, 20618,   716,   716, -1886, -1886,  1189,  1277, 18096, -1886,
    1218, -1886, 12031, -1886,  1025,  1260, -1886,   996, 12603, -1886,
   -1886,  1080,  1629,  1744,   238,   627,   312,  1754,  1730,  1629,
    1757, -1886, -1886, 23726,   764, -1886, 19994, 11863,  1654, -1886,
     764, -1886, 20410,   764, -1886, 25550, 25550, 25550, -1886, -1886,
   -1886, -1886, 25550, 25550,  1751, 23498, -1886, -1886, 24106,  1755,
     784, -1886, -1886, -1886,  2898, -1886, -1886,  1398, -1886,    19,
   -1886,  1405, -1886, 23802, -1886, -1886, 25550,  1735,  1411,  1418,
    1328, -1886,  1028,  1260, -1886, -1886,  1761,  1762, -1886, -1886,
   -1886, -1886,  1763,  1035,  1260, -1886,  1087,  1633,     2,     2,
   -1886, -1886,  1764,  1767, -1886,  1765, -1886, 16033,  1768, -1886,
   15541, 15705,  1770, 16197,  1774, -1886,  1775, 25550, 25550,  1429,
    1779, -1886, -1886, -1886, -1886, -1886, -1886,  1783, 19702, -1886,
   -1886,  1464, 19702, -1886, -1886,  1464, 19702, -1886, -1886,  1464,
    1785,  1786,  1787,   374, -1886, -1886,  1434, 25550, 22513,  1789,
    1791, -1886, -1886, -1886,  1796, 13748, 13906, 14064, 21406, 22809,
   21353, 21353,  1797, -1886,   380,   411,  2702,  7944, -1886,   464,
    4673,  4673, -1886, 23726,   439,   516, -1886, -1886, -1886, -1886,
   11359, 25550,  1799,  1871, 11190, 10834, -1886,  1776, -1886,  1781,
   25550,  1782, 23498,  1784, 25550, 17937, 25550, -1886, 11012,  1062,
   -1886,  1792,    39, -1886,   119,  1863,   315,     2, -1886,  1809,
   -1886,  1793, -1886,  1795,  1810,  1811, 16581, 16581, -1886, -1886,
    1877, -1886, -1886,     8,     8,   565, 14721,   478, -1886, -1886,
    1812,  1822,   527, -1886,  1823, -1886,  1817, -1886,  1818, -1886,
   -1886, -1886, -1886, 12199,  1821,  1824,  1825, -1886, 19702, -1886,
   -1886,  1464, 25550, 25550,  1277,  1828, -1886,  1819,  1829,   238,
    1629,    81,  4673, -1886, 24182, -1886,  1835, -1886, 21406, -1886,
     658,  1833,  1830,  1105, -1886,  1836, -1886, -1886, -1886, -1886,
   -1886, 23498,  1328, -1886, -1886, 23802, -1886,  1872,  2339, -1886,
    1872,  1872, -1886,  2339,  3365,  3926, -1886,  1439, -1886, -1886,
   -1886,  1843, 19702, -1886, -1886,  1464, -1886, -1886,  1846,  1848,
       2, 19702, -1886, -1886,  1464, 19702, -1886, -1886,  1849, -1886,
   -1886, -1886, -1886, -1886, -1886, -1886, -1886,  1686, -1886, -1886,
   -1886,  1851, -1886, -1886, -1886, -1886,  1854,  1850,     2,  1852,
    1858,  1859, -1886, -1886, -1886, -1886, 25550, -1886,    -7, -1886,
    1218, -1886, -1886,  1864,  1875, -1886,  1797,  1797,  1797,  5234,
    1094,  1845,   523, -1886,  5234,   556, 17937, -1886, -1886, -1886,
    5147, 25550,  6203,   520, -1886, -1886,   329,  1874,  1874,  1874,
    4673, -1886, -1886, -1886,  1119, -1886, -1886, -1886, -1886,  1737,
    1869, 25550,   352,  1873,   486, 14229, 21406,  1125,  1853, 16581,
    1876, -1886, -1886, -1886,  1047, 16581, 25550,   812,   787, -1886,
   25550, 23419, -1886, -1886,   558, -1886,  1328, -1886,  1131,  1145,
    1146,   829, -1886, -1886, -1886, -1886,   653,  1062,  1882, -1886,
   -1886, 25550, -1886,  1883,   796, -1886, 10300, -1886, -1886, -1886,
   25550, 25550, -1886, -1886,   448,     8, -1886,   680, -1886, -1886,
   -1886,     2, -1886,  1608, -1886, 21406, -1886, -1886, -1886, -1886,
   -1886,  1881,  1886, -1886, -1886,  1885, -1886,  1888,   238, -1886,
    1629,  1892,   627,  1730, 23498, -1886, -1886, -1886,  1894, -1886,
   -1886, 25550, -1886, 20410, 25550,  1328,  1895,  1456, -1886,  1460,
   -1886,  2339, -1886,  2339, -1886, -1886, -1886,  1896,     2,     2,
    1898,  1899, -1886,  1897, -1886, -1886, -1886, -1886, -1886,  1469,
   25550, -1886, -1886, -1886, -1886, -1886,   564,  1094,  2687,   589,
   -1886, -1886, -1886, -1886,     2,     2, -1886, -1886, -1886,   666,
   -1886,  1162,  5147,   684, -1886,  6203, -1886,     2, -1886, -1886,
   -1886, -1886, -1886, -1886, 16581, 16581,  1737, 16361,   151, 24258,
    1977, 16581, -1886, -1886, -1886, -1886, 25550, -1886, 24334,  1983,
    1879, 23338, 24410, 16581, 11012,  1737,   657,  1211,  1880, 25550,
   -1886,  1907,   443, 16581, -1886, 16581, -1886,  1910, -1886, -1886,
    1887,   796,   837, -1886, -1886,  1909,  1477,  1188, 16581,  1913,
   16581, 16581, 16581, -1886,   716, -1886,  4673,  5714, -1886, -1886,
    1911,  1912, -1886, -1886,  1629,  1916, -1886, -1886,  1328,  1920,
   -1886, -1886, -1886, -1886,  1921, -1886, -1886, -1886,  1482,  1484,
   -1886, -1886, -1886, -1886, -1886, -1886, -1886, -1886, -1886,  1919,
    1928,  1930,  2687, -1886,     2, -1886, -1886, -1886, -1886, -1886,
    1935,  5234, -1886,  2003,  9290,    60, 12370, -1886, 16458, -1886,
      -8,  1190, 16581,  2018,   703,  1924,   308, 16581, 25550,  4673,
    1938,   657,  1211,  1923, -1886, 24486,  1936,   395,  2026, -1886,
   24562, 24638, 25550,  1737,  1925, 12537, -1886, -1886, -1886, -1886,
   21614, -1886,  1944,  1929,   255, -1886, 25550, 23726, -1886, -1886,
   25550,   304, -1886, -1886, -1886, -1886, -1886, -1886, -1886,  1956,
   -1886,  1958, -1886, -1886, -1886, -1886,  1135,  1260, -1886, -1886,
    1094, -1886, 16581, -1886,    43, -1886,   267, -1886, -1886, -1886,
    1960, 13267, -1886, -1886, 16581, -1886,    -4, -1886, 16581, 25550,
    1959, 24714, -1886, -1886, -1886, 24790, 24866, 25550,  1737, -1886,
   24942, 25018, 16581,  1947,   521,  1949,   552, -1886, -1886,  1967,
   13267, 21614, -1886,  5503, 21198,  1654,  1962, -1886,  2016,  1971,
     844,  1970, -1886, -1886,  1206,  1208,   206, -1886, -1886, 19702,
   -1886, -1886,  1464, -1886, -1886, 25550, -1886, 25550, -1886, -1886,
    1574, 13432, -1886, -1886, 16581, -1886, -1886,  1737, -1886, -1886,
    1737,  1957,   673,  1961,   681, -1886, -1886,  1737, -1886,  1737,
   -1886,  1978, 25094, 25170, 25246, -1886,  1574, -1886,  1953,  3577,
    5005, -1886, -1886, -1886,   255,  1982, 25550,  1968,   255,   255,
   -1886, -1886, 16581,  2062,  1981, -1886, -1886, 16458, -1886,  1574,
   -1886, -1886,  1997, 25322, 25398, 25474, -1886, -1886,  1737, -1886,
    1737, -1886,  1737, -1886,  1953, 25550,  2020,  5005,  2011,   796,
    2021, -1886,   857, -1886, -1886, 16581, -1886, -1886, 10029,  2028,
   16458, -1886, -1886,  1737, -1886,  1737, -1886,  1737,  2029,  2023,
   -1886,   653,   796,  2030, -1886,  1995,   796, -1886, -1886, -1886,
   -1886, 10173, -1886,   653, -1886, -1886,  1495, 25550, -1886,  1236,
   -1886,   796,  1654,  2037,  2006, -1886, -1886,  1241, -1886, -1886,
    2015,  1654, -1886, -1886
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   493,     0,     2,   493,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   499,   501,   500,   502,     0,     0,
       0,   520,   522,   543,   523,   544,   526,   527,   541,   542,
     521,   539,   540,   524,   525,   528,   529,   530,   531,   532,
     533,   534,   535,   536,   537,   538,   545,   546,   849,   548,
     621,   622,   625,   627,   623,   629,     0,     0,     0,   493,
       0,     0,    17,   592,   598,     9,    10,    11,    12,    13,
      14,    15,    16,   806,   108,     0,     0,    20,     0,     2,
     106,   107,    18,    19,   864,   493,   807,   429,     0,   432,
     729,   434,   445,   847,   433,   467,   468,     0,     0,     0,
       0,   575,   495,   497,   503,   493,   505,   508,   560,   519,
     547,   477,   553,   558,   479,   570,   478,   585,   589,   595,
     574,   601,   613,   849,   618,   619,   602,   671,   435,   436,
       3,   814,   827,   498,     0,     0,   849,   886,   849,   493,
     903,   904,   905,   493,     0,  1090,  1091,     0,     1,   493,
      17,     0,   493,   456,   457,     0,   575,   503,   487,   488,
     489,   817,     0,   624,   626,   628,   630,     0,   493,   849,
     674,   850,   851,   620,   549,    22,    23,    21,   783,   778,
     768,     0,   858,   815,     0,     0,   510,   808,   812,   813,
     809,   810,   811,   493,   858,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   593,   596,   493,   493,     2,     0,
    1092,   575,   893,   911,  1096,  1089,  1087,  1094,   428,     0,
     170,   735,   169,     0,   437,     0,     0,     0,     0,     0,
     443,     0,     0,     0,   427,   980,   981,     0,     0,   466,
     847,   849,   847,   867,   849,   849,   476,   493,   849,   847,
     924,   849,   849,   475,   849,   943,   849,   921,     0,   568,
     569,     0,     0,   493,   493,     2,   493,   446,   847,   496,
     506,   561,     0,   590,     0,   830,   493,     0,     0,   729,
     447,   575,   554,   571,   586,     0,   830,   493,     0,   509,
     555,   562,   563,   480,   572,   482,   483,   481,   577,   587,
     591,     0,   605,     0,   800,   493,     2,   828,   885,   887,
     493,     0,   493,     0,     0,   575,   493,   505,     2,  1100,
     575,  1103,   847,   847,     3,     0,   575,     0,     0,   459,
     849,   842,   844,   843,   845,     2,   493,   847,     0,   804,
       0,     0,   764,   766,   765,   767,     0,     0,   760,     0,
     749,     0,   758,   770,     0,   849,   672,     2,   493,  1112,
     494,   493,   484,   553,   485,   578,   486,   585,   582,   603,
     849,   604,   717,     0,   493,   718,  1065,  1066,   493,   719,
     721,   674,   592,   598,   675,   676,   677,     0,   674,   852,
       0,   781,   769,     0,   863,   862,   858,   861,     0,   856,
     859,    25,     0,    24,     0,     0,     0,     0,     0,     0,
       0,    27,    29,     4,     8,     5,     6,     7,     0,     0,
     493,     2,     0,   109,   110,   111,   112,    91,    28,    92,
      46,    90,   113,     0,     0,   128,   130,   134,   137,   140,
     145,   148,   150,   152,   154,   156,   158,   161,     0,    30,
       0,   599,     2,   113,   493,   162,   775,   725,   589,   727,
     774,     0,   724,   728,     0,     0,     0,     0,     0,     0,
       0,     0,   865,   891,   849,   901,   909,   913,   919,   592,
       2,     0,  1098,   493,  1101,     2,   106,   493,     3,   716,
       0,  1112,     0,   494,   553,   578,   585,     3,     3,   712,
     702,   706,   718,   719,   493,     2,     2,   894,   912,  1088,
       2,     2,    27,     0,     2,   735,    28,     0,   733,   736,
    1110,     0,     0,   742,   731,   730,   493,     0,   832,     0,
       2,   458,   460,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   870,   927,   950,
     849,   472,     2,   866,   874,  1008,   729,   868,   869,     0,
     830,   493,   923,   931,   729,   925,   926,     0,   942,   944,
       0,   462,   493,   493,   559,   494,     0,   575,     0,   493,
    1093,  1097,  1095,   444,   576,   804,     0,   830,   847,     0,
     438,   448,   507,     0,   830,   493,   804,     0,   830,   779,
     556,   557,   573,   588,   594,   597,   592,   598,   616,   617,
       0,   780,   688,   722,   494,     0,   689,   691,   692,     0,
     210,   421,   829,     0,   419,   476,   475,   575,     0,   440,
       2,   441,   801,   464,     0,     0,     0,   493,   847,   493,
     804,     0,     0,     2,     0,   763,   762,   761,   755,   504,
       0,   753,   771,   551,     0,   493,   493,  1067,   494,   490,
     491,   492,  1071,  1062,  1063,  1069,   493,     2,   107,     0,
    1027,  1041,  1112,  1023,   849,   849,  1032,  1039,   710,   493,
     583,   720,   494,   579,   580,   584,     0,   673,  1077,   494,
    1082,  1074,   493,  1079,   849,     0,   849,   674,   674,     0,
       0,   493,     0,     0,   854,   858,    70,     0,    26,   493,
      98,     0,   493,   493,    93,   493,   100,     0,    36,    40,
      41,    37,    38,    39,   493,    96,    97,   493,   493,   493,
       2,   109,   110,     0,     0,   188,     0,     0,   619,     0,
    1087,     0,     0,     0,     0,     0,     0,     0,     0,    59,
       0,    65,    66,    70,     0,     0,    70,     0,    94,    95,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   493,   171,   172,   173,   174,   175,   176,
     177,   178,   179,   180,   181,   169,     0,   167,   168,   493,
     992,   726,   989,   849,   849,   997,   600,   493,   855,   892,
     849,   902,   910,   914,   920,   493,   895,   897,   899,   493,
     915,   917,     2,     0,     2,  1099,  1102,   493,   493,     0,
       2,     0,   493,   107,  1027,   849,  1112,   962,   849,   849,
    1112,   849,   977,   849,   849,     3,   720,   493,     0,   493,
       0,  1112,  1112,   493,   493,   493,   493,     0,     2,   744,
       0,  1110,   741,  1111,     0,   737,     0,     2,   740,   743,
       0,     2,     0,   493,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   849,   879,   883,   922,
     849,   936,   940,   948,   849,   958,   871,   928,   951,     0,
       0,  1004,     0,   470,   833,     0,     0,   471,   834,   463,
       0,     0,     0,     0,   461,     0,     2,     2,   835,     0,
     442,     2,   804,     0,   830,     2,   836,     0,     0,     0,
       0,   631,   888,   493,   906,     0,     0,   493,   422,   420,
     106,     3,   493,     0,   805,     2,     0,   757,   493,   493,
     751,   750,   751,   552,   550,   676,  1073,   493,  1078,   494,
     493,  1064,     0,     0,     0,     0,  1042,     0,   849,  1113,
    1028,  1029,   711,  1025,  1026,  1040,  1068,  1072,  1070,   581,
     616,  1076,  1081,   668,   674,   674,     0,     0,   683,   682,
    1110,     0,   674,   784,   782,     0,     0,   857,    74,     0,
      71,    72,    75,   816,     0,     0,     0,     2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   493,   493,
       0,   127,   126,     0,   123,   122,    31,     0,    32,     0,
       0,     0,   185,   184,     0,     3,    70,     0,    55,     0,
      56,    63,     0,    62,     0,    58,     0,    57,    61,     0,
       0,    54,   129,   131,   132,   133,   135,   136,   138,   139,
     143,   144,   141,   142,   146,   147,   149,   151,   153,   155,
     157,     0,     0,   431,     0,     0,    33,     3,   735,   163,
       0,     0,     0,     0,   993,   994,   990,   991,   777,   776,
     493,   896,   898,   900,   493,   916,   918,     0,     0,   493,
     493,  1018,  1017,   493,     0,     0,     0,     0,     0,   849,
    1028,   965,   982,   493,   960,   968,   708,   963,   964,   709,
     493,   975,   985,   978,   979,     0,     3,  1112,     3,   704,
     454,   703,   707,  1104,   713,   714,   696,     0,   697,   698,
       3,     3,     3,   729,     0,   161,     0,     3,     3,     0,
     738,     0,   732,     0,   849,   493,   849,     3,   465,   849,
     880,   884,   849,   937,   941,   949,   849,   959,   493,   872,
     875,   877,   493,   929,   932,   934,   493,   952,   954,   956,
     847,   473,  1005,     3,  1009,  1010,     3,   838,   945,   565,
     564,   567,   566,     2,   805,   839,   786,   493,     2,   837,
       0,   805,   840,   631,   631,   631,   493,   690,   693,   694,
     723,   425,     0,     0,     0,   493,     0,     0,   346,     0,
       0,     0,     0,     0,   190,     0,   341,   342,     0,     0,
     346,     0,   394,   393,     0,   165,   165,   400,   592,   598,
     207,   493,     2,     0,   191,     0,   218,   192,   193,   493,
     212,   194,   195,   196,   197,     0,   198,   199,   347,     0,
     361,   200,   367,   369,   372,   201,   202,   203,   204,   205,
       0,   206,   214,   575,   493,   216,     0,     0,     0,     3,
     818,   805,     0,   793,   794,     0,     3,   789,     3,     3,
       0,   493,   768,   768,  1075,  1080,     2,   106,   493,     3,
     590,     3,   494,  1036,   849,  1035,  1038,   493,     3,  1024,
    1030,   674,  1110,     0,   674,   679,   674,     0,   684,  1110,
       2,   853,   860,     0,    99,   102,   493,   493,     0,   105,
     101,   103,   493,     0,   117,     0,     0,     0,   121,   125,
     124,   189,     0,     0,     0,   735,   114,   182,     0,     0,
       0,    49,    50,    88,     0,    88,    88,     0,    76,    78,
      52,     0,    48,     0,    51,   160,     0,     0,     0,     0,
    1110,  1001,   849,  1000,  1003,   995,     0,     0,   889,   907,
       3,     3,     0,   849,   971,   974,   849,     0,   849,   849,
     966,   983,     0,     0,  1105,     0,   715,   493,     0,  1107,
     493,   493,     0,   493,     0,   439,     3,     0,     0,     0,
       0,   734,   739,     3,   831,     3,   848,     0,   493,   873,
     876,   878,   493,   930,   933,   935,   493,   953,   955,   957,
       0,     0,     0,   729,  1016,  1015,     0,     0,     0,     0,
       0,     3,   805,   841,     0,   493,   493,   493,   493,   493,
     493,   493,   614,   644,     0,     0,   645,   575,   632,     0,
       0,     0,   423,    70,     0,     0,   332,   333,   215,   217,
     493,     0,     0,     0,   493,   493,   328,     0,   326,     0,
       0,     0,   735,     0,     0,   493,     0,   373,   493,     0,
     166,     0,     0,   401,     0,     0,     0,   849,   222,     0,
     213,     0,   323,     0,     0,     0,   346,   346,   352,   351,
     346,   363,   362,   346,   346,     0,   575,     0,  1020,  1019,
       0,     0,   760,   796,     2,   791,     0,   792,     0,   772,
     752,   756,   754,   493,     0,     0,     0,     3,   493,  1031,
    1033,  1034,     0,     0,   106,     0,     3,     0,     0,   674,
    1110,     0,     0,   663,     0,   678,     0,   785,   493,    73,
    1021,     0,     0,     0,    42,     0,   118,   120,   119,   116,
     115,   735,  1110,   187,   186,     0,    69,    85,     0,    79,
      86,    87,    64,     0,     0,     0,    60,     0,   159,   430,
      34,     0,   493,   996,   998,   999,   890,   908,     0,     0,
     849,   493,   967,   969,   970,   493,   984,   986,     0,   961,
     976,   972,   987,  1106,   705,   455,   700,   699,   701,  1109,
    1108,     0,     3,   846,   745,   746,     0,     0,   849,     0,
       0,     0,   881,   938,   946,   474,     0,  1011,     0,  1012,
    1013,  1007,   822,     2,     0,   824,   614,   614,   614,   645,
     652,   619,     0,   658,   645,     0,   493,   606,   643,   639,
       0,     0,     0,     0,   646,   648,   849,   660,   660,   660,
       0,   640,   656,   426,     0,   336,   337,   334,   335,   231,
       0,     0,   233,   434,   232,   575,   493,     0,     0,   346,
       0,   314,   313,   315,     0,   346,   190,   271,     0,   264,
       0,   190,   329,   327,     0,   321,  1110,   330,     0,     0,
       0,     0,   382,   383,   384,   385,     0,   375,     0,   376,
     338,     0,   339,     0,     0,   366,     0,   211,   325,   324,
       0,     0,   355,   365,     0,   346,   368,     0,   370,   392,
     424,   849,   820,   751,   773,   493,     2,     2,  1083,  1084,
    1085,     0,     0,     3,     3,     0,  1044,     0,   674,   664,
    1110,     0,   681,   684,   735,   685,   667,     3,     0,  1022,
     104,     0,    35,   493,     0,  1110,     0,     0,    89,     0,
      77,     0,    83,     0,    81,    47,   164,     0,   849,   849,
       0,     0,   748,     0,   449,   453,   882,   939,   947,     0,
       0,   788,   826,   610,   612,   608,     0,     0,  1051,     0,
     653,  1056,   655,  1048,   849,   849,   638,   659,   642,     0,
     641,     0,     0,     0,   662,     0,   634,   849,   633,   649,
     661,   650,   651,   657,   346,   346,   234,   575,     0,     0,
     252,   346,   319,   317,   320,   316,     0,   318,     0,   260,
       0,   190,     0,   346,   493,   272,     0,   297,     0,     0,
     322,     0,     0,   346,   345,   346,   386,     0,   377,     2,
       0,     0,     0,   209,   208,   348,     0,     0,   346,     0,
     346,   346,   346,   452,   768,   790,     0,     0,  1086,  1037,
       0,     0,  1043,  1045,  1110,     0,   666,   680,  1110,     2,
      53,    45,    43,    44,     0,    67,   183,    80,     0,     0,
    1002,   451,   450,   973,   988,   747,  1006,  1014,   636,     0,
       0,     0,  1052,  1053,   849,   637,  1049,  1050,   635,   615,
       0,     0,   344,   223,     0,     0,     0,   245,   346,   225,
       0,     0,   346,   254,   269,   280,   274,   346,   190,     0,
     311,     0,   284,     0,   309,     0,   275,   273,   262,   265,
       0,     0,   190,   298,     0,     0,   228,   343,   374,     2,
     493,   340,     0,     0,   402,   353,     0,    70,   364,   357,
       0,   358,   356,   371,   759,   795,   797,  1046,  1047,     0,
     670,     0,   787,    68,    84,    82,   849,  1059,  1061,  1054,
       0,   647,   346,   240,   235,   238,     0,   237,   244,   243,
       0,   493,   247,   246,   346,   256,     0,   253,   346,     0,
       0,     0,   261,   266,   312,     0,     0,   190,   285,   310,
       0,     0,   346,     0,   300,   301,   299,   268,   331,     0,
     493,   493,     3,   387,   494,   391,     0,   395,     0,     0,
       0,   403,   404,   349,     0,     0,     0,   669,   686,   493,
    1055,  1057,  1058,   654,   224,     0,   242,     0,   241,   227,
     248,   493,   415,   257,   346,   258,   255,   270,   283,   281,
     277,   289,   287,   288,   286,   267,   282,   278,   279,   276,
     263,     0,     0,     0,     0,   230,   248,     3,   380,     0,
    1051,   388,   389,   390,   402,     0,     0,     0,   402,     0,
     354,   350,   346,     0,     0,   236,   239,   346,     3,   249,
     416,   259,     0,     0,     0,     0,   308,   306,   303,   307,
     304,   305,   302,     3,   380,     0,     0,  1052,     0,     0,
       0,   396,     0,   405,   359,   346,  1060,   219,     0,     0,
     346,   296,   294,   291,   295,   292,   293,   290,     0,     0,
     381,     0,   408,     0,   406,     0,   408,   360,   221,   220,
     226,     0,   229,     0,   378,   409,     0,     0,   397,     0,
     379,     0,     0,     0,     0,   410,   411,     0,   407,   398,
       0,     0,   399,   412
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1886,  5966,  3619, -1886,    -1,   572,  2946,  2909,   452, -1886,
    -359, -1886,   423, -1886,  -739, -1886,   874,  -999, -1300, -1886,
     307,  3860,  1934, -1886,  1200, -1886,  1465,   707,   901,   910,
     750,   906,  1427,  1430,  1423,  1426,  1431, -1886,    87,  -183,
    8301,   966, -1886,  1758, -1886, -1886, -1280,  7894, -1133,  4917,
   -1886,   -48, -1886,   955,    62, -1886, -1886,   733,   143, -1886,
   -1715, -1579,   354,   121, -1886, -1886,   725,   364,   264, -1585,
   -1886, -1236, -1886, -1886, -1886, -1886,   169, -1070, -1886, -1886,
   -1241,   495, -1886, -1886, -1886, -1886, -1886,   170, -1206, -1886,
   -1886, -1886, -1886, -1886,    96,   518,   519,   192, -1886, -1886,
   -1886, -1886,  -686, -1886,   128,    72, -1886,   199, -1886,  -121,
   -1886, -1886, -1886,   967,  -858,  -994,   -18, -1886,    17,    21,
     337,  4993,  -947,  -658, -1886,   106, -1886, -1886,    94, -1886,
    -158,   422,  -246,  -251,  2691,  3040,  -635,    29,   459,  1209,
    2073,  5183, -1886, -1886,  2193, -1886,   483,  1425, -1886,  2131,
   -1886,    52, -1886, -1886,  2876,   753,  4931,  4114,   -45,  1974,
    -335, -1886, -1886, -1886, -1886, -1886,  -111,  6773,  7576, -1886,
    -383,   350, -1886, -1178,   316, -1886,   249,   799, -1886,    23,
    -106, -1886, -1886, -1886, -1886,  -146,  7909,  -890,   935,   492,
    2792, -1886,  -512,  -173,  -201,   101,  2999,  -801,  -160,   988,
    -409,  -243,  -270,  -200,  -475,  1393, -1886,  1741,   233,  -932,
    1609, -1886, -1886,   743, -1886, -1250,  -164,  -287,  -500, -1886,
     290, -1886, -1886, -1138,   522, -1886, -1886, -1886,  2268,  -836,
    -465,  -950,   -30, -1886, -1886, -1886, -1886, -1886, -1886,     7,
    -864,  -227, -1885,   -31,  6984,   -69,  5740,   -98,  1564, -1886,
    3753,   -53,  -224,  -209,  -198,    51,   -75,   -72,   -49,   314,
       1,    10,   137,  -186,    69,  -167,  -129,  -112,   166,  -103,
    -100,   -87,  -798,  -726,  -719,  -717,  -741,  -132,  -709, -1886,
   -1886,  -718,  1474,  1476,  1478,  2404, -1886,   633,  6196, -1886,
    -580,  -625,  -616,  -606,  -734, -1886, -1587, -1743, -1741, -1739,
    -647,  -124,  -182, -1886, -1886,   -67,    68,  -128, -1886,  6759,
     188,  -590,   -88
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   850,   427,   428,   177,    84,  1243,   429,   403,
     430,  1563,  1564,   431,   999,  1000,  1001,  1357,  1358,  1359,
    1577,   453,   433,   434,   435,   733,   734,   436,   437,   438,
     439,   440,   441,   442,   443,   444,   445,   446,   455,  1146,
     735,  1491,   796,   223,   798,   449,  1034,  1244,  1245,  1246,
    1247,  1248,  1249,  1250,  2148,  1251,  1252,  1680,  2005,  2006,
    1937,  1938,  1939,  2118,  2119,  1253,  1698,  1699,  1954,  1700,
    1847,  1848,  1254,  1255,  1256,  1257,  1258,  1259,  1876,  1880,
    1514,  1506,  1260,  1261,  1513,  1507,  1262,  1263,  1264,  1265,
    1266,  1267,  1268,  1717,  2136,  1718,  1719,  2042,  1269,  1270,
    1271,  1494,  2050,  2051,  2052,  2176,  2187,  2070,  2071,   311,
     312,   937,   938,  1212,    86,    87,    88,    89,    90,  1683,
     489,   209,    94,    95,    96,    97,   239,   240,   314,   293,
     491,   457,   492,   100,   326,   102,   103,   157,   361,   317,
     107,   108,   109,   173,   110,   954,   362,   158,   113,   263,
     114,   159,   272,   364,   365,   366,   160,   450,   119,   120,
     368,   121,   610,   930,   928,   929,  1657,   122,   123,   124,
     125,  1206,  1458,  1663,  1664,  1809,  1810,  1459,  1652,  1829,
    1665,   126,   697,  1311,   169,   989,   127,   990,   991,  1555,
     962,   616,  1137,  1138,  1139,   617,   372,   500,   501,   619,
     459,   460,   224,   519,   520,   521,   522,   523,   349,  1292,
     350,   952,   950,   648,   351,   391,   352,   353,   461,   128,
     179,   180,   129,  1286,  1287,  1288,  1289,     2,  1193,  1194,
     639,  1280,   130,   339,   340,   274,   285,   593,   131,   227,
     132,   329,  1148,   583,   553,   171,   133,   398,   399,   400,
     134,   331,   243,   244,   245,   332,   136,   137,   138,   139,
     140,   141,   142,   248,   333,   250,   251,   252,   334,   254,
     255,   256,   836,   837,   838,   839,   840,   257,   842,   843,
     844,   801,   802,   803,   804,   554,  1186,  1437,   143,  1768,
     672,   673,   674,   675,   676,   677,  1812,  1813,  1814,  1815,
     662,   502,   376,   377,   378,   462,   215,   145,   146,   147,
     380,   864,   678
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   198,   448,    82,   199,   532,   498,   196,   373,   590,
     359,   739,   574,   536,  1044,   321,   392,  1050,   214,  1510,
    1293,   153,    91,   525,   387,   680,   870,   200,   537,   183,
     104,   571,   977,   205,   497,  1496,  1111,   738,   388,   538,
     861,   963,  1531,  1532,   375,   242,  1129,  1361,  1131,  2007,
     964,   539,   135,   115,  1579,    82,    82,  -798,    82,  1441,
     965,  2008,   230,   744,  1919,   652,  1920,  2014,  1921,   144,
     540,  2074,   144,  1275,    58,    82,  1368,   201,   484,  1272,
      91,   228,  1085,  1587,    82,   214,   202,   307,   104,  1201,
     971,  1483,    82,  1112,  1312,    98,   464,    82,  1229,  1025,
      82,  1115,  1319,   536,    82,   652,  1105,  1122,   541,  1281,
     135,   115,   657,  1106,  1461,  1107,  1859,   148,   537,   324,
     917,   466,   279,  1108,   467,   542,   560,   144,   688,   538,
     225,   925,   691,  1462,   543,   630,   212,   544,    75,  1721,
     778,   539,  1434,    82,  1584,  1406,    82,   468,    82,   246,
     545,   161,   275,    98,    82,   587,   286,   283,   680,   225,
     540,    82,   198,  1735,  1435,   199,   598,   249,   534,    82,
      91,  1495,  2015,   144,  1407,   945,  2075,  1585,   104,   528,
    2065,    82,  2007,   779,  -799,   547,  1464,  1465,   200,    63,
      64,   115,   690,    82,    82,    82,   693,   469,   541,   581,
     135,   115,   226,  1723,  -830,    82,   470,   963,  1408,   551,
      82,   556,  1453,   203,  1408,   542,   964,   144,   564,  1722,
     516,  1923,  2009,  2013,   543,    82,   965,   544,  1934,  1935,
      82,    82,    82,  -830,   663,   695,    82,    82,   201,    78,
     545,   657,   699,    98,  1440,   313,   212,   202,   987,   696,
     162,  1444,   198,   559,   971,   199,   605,    82,   634,  1454,
     567,   242,   507,   622,   253,    82,  1955,    82,  2066,   581,
     167,  1149,   447,   217,  1724,   547,    82,    82,   200,   680,
      82,  2112,   586,  1780,  1782,  1784,   903,    82,   306,   212,
      20,   629,   631,   597,   907,  1777,  1229,  1350,   703,   115,
    1004,    82,    82,  1008,    82,  1466,  1010,   548,   499,    82,
     876,   182,  1390,    82,  1936,  1013,   193,   212,  1015,  1016,
    1017,   865,  1551,   680,  -611,   877,    82,    82,   115,   911,
     913,  1521,   594,   471,    58,  1999,   878,    82,    92,   115,
     217,   154,  1140,   909,  1142,    82,    82,   680,   879,   914,
      82,  1469,  1006,   313,   680,   826,  1919,  1468,  1920,   841,
    1921,  1157,  1442,   663,   115,   625,  1375,   880,   283,  1391,
    1115,  1048,   737,   184,   203,   401,   484,  1105,   498,   212,
      82,   463,   313,   249,  1106,  1275,  1107,   214,  1340,    82,
    1310,  1272,    82,   313,  1382,    82,    92,   548,    75,   213,
    1317,    58,  2013,   988,   549,   881,   497,   649,   384,   956,
     876,   650,   247,   630,  2048,   276,  1508,   305,   623,   287,
    1767,   552,   882,    99,  1550,   877,   155,  1508,  2067,  2068,
    1279,   883,    58,   976,   884,  2013,   878,   669,   267,  1509,
    1734,   704,   280,  1495,  1737,   705,   981,   885,   879,  1290,
    1509,  1453,  1453,  1453,   582,   498,  2102,  1198,   994,   466,
     105,  1852,   467,   206,    82,    75, -1111,   880,   205,  1027,
    1009,  1806,  1725,   958,   924,   185,  1819,   988,   963,  2021,
     626,    99,   384,   497,   111,   468,    92,   964,   552,    82,
      82,  2117,  1644,   896,   549,   193,    75,   965,  1454,  1454,
    1454,    82,    82,   745,  1511,   881,   194,   210,   746,   213,
     982,   904,    82,  1923,   516,   168,   507,  2117,   105,   908,
    1934,  1935,   882,  1878,   582,   508,   220,  1512,   193,  1496,
    1310,   883,   218,    82,   884,   469,  -981,   221,   918,  1504,
    2150,   231,   111,  -981,   470,    82,   665,   885,  1455,   926,
    1999,   986,   213,   222,   258,   663,  1027,   920,  1879,   466,
    1658,    99,   467,  1850,  1089,   319,  2031,   905,  1858,    82,
    1540,    99,   232,    83,   210,    82,   151,    82,   599,  1779,
     213,   956,   498,   896,   972,   468,   163,  1380,  1381,   164,
     165,  1659,   166,   611,   919,   595,  1614,   507,   316,  1129,
    1131,   923,   306,  1370,  -980,   927,  1965,   920,   105,   944,
     497,  -980,  1681,   204,    64,   897,  1681,  1701,   591,  1675,
    1960,  1961,   111,   700,   680,  1670,   702,   498,   482,    -3,
    1701,    83,   111,    82,  1984,    82,   402,   780,    82,  1461,
      82,   781,   217,  1415,  1671,   958,  1128,   115,    83,    82,
     231,   977,  1607,    82,  1141,   497,    58,    83,  1740,  1760,
      91,   737,  1126,  1150,   645,  1495,   234,   737,   104,    99,
      83,   471,  1180,    83,  1826,   665,   737,    83,   232,   306,
     499,  1827,  1027,  1463,  1817,    82,   576,   507,   580,  1132,
     135,   115,  2092,   646,   647,   737,  1677,   359,    99,   233,
    1828,   313,   841,  1818,    82,   897,   316,   144,   305,    99,
     472,  1294,   898,  1027,   463,  2025,  2026,  1670,  1953,  1027,
      75,    83,  1548,  2094,  1674,  1827,   935,    83,   306,  1556,
     111,   375,   155,    98,    99,   316,  1820,   605,  1860,   561,
    2056,    74,   385,   552,  1918,    82,   316,    82,  1116,    82,
    1924,   278,  1119,    82,   116,  1882,    82,   499,   580,   111,
     463,   463,   799,  1134,  1135,   303,   552,    83,    83,  1925,
     111,   316,  1505,    80,    81,  1295,    58,  -487,   481,   508,
    1591,    82,  1908,    83,  1909,   301,  -488,  1455,  1455,  1455,
     768,   769,    74,    83,  -665,   111,    14,    15,    16,    17,
      18,  -665,   898,   531,    83,   533,  1842,  1843,  1844,    83,
      83,  1884,   116,   666,   305,  2023,   306,   667,  1141,    14,
      15,    16,    17,    18,    80,   668,    82,  1827,  1845,  2037,
      83,    82,   739,    82,  1027,   770,   771,  -489,    83,  1313,
      75,   472,  1027,   552,  2123,    82,  1928,    14,    15,    16,
      17,    18,  2125,    83,    58,    58,    82,  1187,   738,   637,
     508,   359,   516,   552,  1027,    82,   325,   978,  1894,    58,
    1572,  1195,  -819,  1405,   608,  1199,   210,   613,    58,  1202,
     761,   806,   151,  2019,   499,   807,    83,   762,   763,   463,
     484,   390,   116,   347,  2085,   375,  1020,  1005,    82,    83,
      83,   463,   116,  1011,   393,   825,    58,  1021,  1022,    14,
      15,    16,    17,    18,  1364,    58,   401,   193,    75,    75,
    1344,  1323,   473,  1565,   -23,   747,   482,  1345,   474,   499,
     748,  1200,   447,    75,    82,    82,   516,   815,  1576,    58,
      58,   552,    75,  -491,  1853,  1323,   475,  1145,   499,  1854,
     499,  1412,  1090,   694,   499,   499,   552,   499,    91,   476,
    1761,  1842,  1843,  1844,    58,  1283,   104,   680,    58,   504,
      75,   808,    58,  1027,   499,   705,    92,  1389,   841,    75,
    1769,   872,  1776,  1845,   934,  1709,  1865,   477,   935,   115,
      82,  1854,  1851,    58,  1973,   463,  1701,   478,  1113,  1974,
     116,  2107,   667,    75,    75,   144,  2108,  1706,   505,    14,
      15,    16,    17,    18,  2165,   510,   144,    99,    58,  2166,
     866,   867,  1120,  1168,   868,   511,   667,   552,    75,   116,
     526,    98,    75,  -492,  1208,   524,    75,    83,   529,  1396,
     116,    82,   305,   499,   472,    82,   552,  1172,  1530,   530,
      58,   552,   976,    58,   316,  1176,  1542,    75,  1523,   552,
     561,    99,   889,    83,   552,   116,   163,  1651,    58,   164,
     165,   637,   166,   472,    82,   552,  1418,   516,   111,  1113,
     552,   472,    75,   667,   150,    83,   550,    83,    65,    66,
      67,    68,    69,    70,    71,    72,  1775,    58,   105,   819,
      58,  1422,    82,   552,   228,   552,    83,    58,    82,    82,
     604,    64,    74,   572,    75,   960,  1861,    75,    83,   993,
     995,   996,   111,   650,   650,   705,  1141,  1026,   392,   392,
     573,  1027,    75,    76,    77,   585,  1426,   463,   225,  1460,
     552,   620,    83,    82,    80,    81,   578,  1094,    83,  1433,
     481,   552,  1443,  2072,   259,   260,  1821,   261,  1365,    58,
     596,    75,   262,  1635,    75,  1547,  1467,   640,  1560,   359,
    1895,    75,   653,   301,   663,  1712,  1713,  1714,  1715,  1716,
    1538,   624,  2072,  1592,   667,  1904,  1489,   552,  1154,   641,
    1601,   305,   807,   737,   552,   552,  1842,  1843,  1844,  1617,
     499,   499,   655,   375,   698,   561,    83,    58,    83,   552,
     106,    83,  1189,  2120,  1283,  1191,  1027,   516,  1845,  1027,
      82,    82,    82,    75,  1622,  1623,   955,  1846,    74,   516,
     637,  1322,   701,  1074,   552,  1323,    91,  1132,  2054,   687,
    1145,  1132,  1605,  1132,   104,   359,   667,  1360,   516,  1807,
     384,  1323,   706,   552,    82,  1529,   499,   707,   980,   807,
      80,    81,  1772,   764,   765,   144,  1773,   115,   106,    82,
      91,    75,    82,    82,    92,    82,  1834,   708,   104,   375,
    1323,    82,  1838,   144,    82,  1284,  1027,   960,  1862,  1898,
    2059,   279,  1027,   711,   552,  -127,  -127,  -127,  -127,  -127,
    -127,   115,  1863,  1864,  1989,   712,   807,  1027,  1991,    98,
     270,  -126,  -126,  -126,  -126,  -126,  -126,   144,  1038,  1929,
    1040,   713,  1043,   807,   275,   286,   283,    82,   717,  1051,
     766,   767,  1141,   186,     6,     7,     8,     9,    10,    11,
      12,    13,   144,    98,   516,  1978,   394,  2016,   116,  1027,
     741,  1027,   760,    82,  1076,   776,    74,   775,   106,    99,
    1842,  1843,  1844,  2110,  1565,  2111,   774,  1323,   777,  1027,
    1285,  1765,  1027,   504,   741,   359,   782,   666,  1460,  1460,
    1460,   667,  1845,  1653,  1460,   809,    82,   810,    80,   668,
     811,  -191,   116,  2184,   288,   812,  1274,  2181,  2190,   481,
     813,   669,  2191,  1667,    83,   814,    83,   772,   773,   375,
      14,    15,    16,    17,    18,  1024,  1676,  1678,  2138,   480,
     111,   395,  2142,   150,   827,   741,   112,    65,    66,    67,
      68,    69,    70,    71,    72,    83,   536,    -3,    83,   150,
     482,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,   537,  1682,  1588,   822,    82,  1682,  1018,   741,    82,
      82,   824,   538,   655,   741,   153,   847,  1739,  1053,  1054,
    1055,    83,   -18,    77,   539,   845,   858,  1029,  1030,   463,
    -490,   516,  1347,  1348,   112,   849,   978,   862,   396,   863,
     592,  1362,  1363,   540,   447,   447,  1027,  1366,   499,   104,
     871,   499,   499,   104,   104,   516,   516,  1210,   886,   749,
     887,   750,   751,   752,   888,    82,   144,   104,  1060,  1061,
    1062,  1063,   115,   874,   270,  1668,   115,   115,  -162,  -162,
     282,   541,   206,   741,  1284,  1803,  1804,  1805,   144,   890,
     115,   753,   144,   144,   754,   755,  1504,  1505,   542,   756,
     757,    82,    92,  1445,  1446,  1447,   144,   543,  1582,  1583,
     544,  1830,  1830,  1830,   112,  1586,  1583,   594,   900,   270,
     516,  1590,  1583,   545,   112,  1283,  1684,    82,  1102,  1575,
    1684,   891,    82,    82,    82,   892,    92,   276,   287,  1624,
    1575,  1822,   893,   363,  1102,  1636,  1667,   894,   876,  1785,
    1348,  1667,    14,    15,    16,    17,    18,  1339,   714,   547,
     267,   280,   901,   877,    83,   895,  1906,  1348,    83,  1285,
    1907,  1583,  1669,   318,   878,   921,   144,   932,  1456,  1916,
    1027,   922,   494,   758,   759,  -609,   879,    99,  1976,  1977,
     933,   447,  1994,  1583,  1995,  1583,  -607,    83,    82,   931,
      83,  1934,  1935,    82,   758,   880,  2181,  2182,   936,    82,
    1986,    82,  1580,  1581,   947,  1056,  1057,   939,  1866,    82,
     949,    99,   112,   463,  1274,    83,  1058,  1059,  1064,  1065,
     953,    83,    83,  1736,  1738,   966,   968,   758,   516,   669,
     116,  1831,  1832,   881,   516,   984,   992,  1003,   111,  1028,
    1031,   112,   270,   279,  1036,  1101,  1073,  1078,  1274,   680,
     882,  1102,   112,  1109,  1130,   601,    83,  1133,  1668,   883,
     392,  1152,   884,  1668,  2043,   516,  1156,  1159,  1160,  1161,
     363,   548,   111,  1162,  1163,   885,  1164,   112,   283,   150,
    1165,   282,  1166,    65,    66,    67,    68,    69,    70,    71,
      72,  2004,  1167,  1181,   144,  1188,  1190,  1192,  -802,  1196,
     150,   827,  1283,   516,    65,    66,    67,    68,    69,    70,
      71,    72,   896,  1203,  1204,  1205,  1276,  1282,  1303,  1304,
      82,  1305,    82,   659,  1306,   154,   282,  1291,  1387,    77,
    1316,  1314,  1321,  1320,  1324,  2043,  1325,  1326,  1018,   659,
    1329,  1328,  1330,   659,  1331,  1332,  1333,    92,  1335,  1336,
    1337,    92,    92,   144,  1342,  1669,  2101,  1343,  1351,  1352,
    1669,    82,  1367,   270,    82,    92,  1371,  1372,   549,  1373,
     595,  1374,  1378,   516,   516,  1379,   592,  1383,  1384,  1385,
     516,  1386,   613,  1394,   463,    83,    83,  1397,   106,  1399,
    1400,  1401,   516,   591,  1403,  1411,  -803,    83,  1438,  1470,
    1471,  1493,   516,  1474,   516,  1475,  1484,  1456,  1456,  1456,
     155,  1649,  1650,  1654,  1485,   536,  1486,   516,  1667,   516,
     516,   516,  2115,   104,  2004,    82,    82,  1488,   -22,  1497,
     537,   592,    99,  1498,   897,  1284,    99,    99,  1027,  1518,
    1561,   538,  1519,  2045,  1525,  1527,   115,  1549,  1553,  1554,
      99,  1557,   659,   539,  1571,  1589,  1575,    83,  1596,  1597,
    1600,  1611,   144,  2140,  1612,  1613,    83,  1619,  1615,  1686,
      82,  1620,   540,  1686,  1686,    85,  1583,   516,   152,  1625,
    1628,   516,  1632,  1633,  1634,  1642,   516,  1686,    82,  1641,
    1645,  1688,  1656,   111,  1463,  1505,  1702,   111,   111,    83,
    1052,  1703,  1705,  1727,  1707,  1730,  1731,  1229,   116,  1741,
     541,   111,  1720,  1728,  2045,  1729,  1742,  1744,  1746,  1747,
    1285,  1748,  1758,  1759,  1749,  1750,   363,   542,  1756,  1766,
    1770,   898,  1771,    85,  2183,  1778,   543,  1786,  1774,   544,
    1668,   516,   116,  1788,   494,  1789,   472,  1794,  1839,  1796,
     195,  1792,   545,   516,  1624,  1797,  1798,   516,  1801,    85,
     112,   447,   267,   280,   198,  1816,  1835,   199,    83,  1802,
     634,   516,   238,  1841,   226,   266,  1661,  1869,  1871,    85,
     104,  1888,    82,  1889,    82,  1892,  1896,   547,  1893,  1905,
     200,  1900,  1942,  1910,    83,  1913,  1914,  1915,  1947,  1948,
    1962,  1964,   363,   115,   112,  1969,  1975,  1971,  1980,   104,
    1990,  1987,  1988,   516,  1992,  1993,  1996,   152,  2002,   144,
     659,   494,  1284,    85,   896,  1997,   152,  1998,    83,   328,
     336,   363,   115,  2018,   552,  2020,  -592,  1669,    82,    82,
     104,  2032,   358,  2027,   659,  2038,  2046,  2030,   144,  2047,
    2057,   516,  2058,  2174,  2069,  2078,   516,   659,  2091,    19,
    2093,  2095,  2105,   115,  2104,  2180,  2106,   454,  2122,   195,
     195,  2109,  2124,  2135,  2126,   191,    82,  2145,  2146,   144,
     152,   487,  2139,    83,   516,   266,   106,   516,  2141,   516,
      83,   212,   447,  2151,   447,    83,    83,    83,    48,    49,
      50,    51,    52,    53,    54,    55,   328,  1285,   270,   548,
     516,   238,   238,  2162,   591,  2178,  1872,  2161,  2164,   289,
    2173,    82,  2170,  2172,   290,  2177,  2189,   294,   507,   299,
      82,    92,   328,   447,  2188,  2192,  1902,  1559,  1023,  1068,
      85,  1066,  1492,  1069,  1500,  1067,   897,  1690,   494,  1070,
    2116,   797,  2171,  1711,  1334,   266,  1966,  2133,  1959,  2029,
    1338,    83,  2160,   116,   363,  2113,    83,   116,   116,  1881,
    2159,  1346,    83,  2097,    83,  1867,  1868,  2143,  2179,  2096,
     363,   116,   174,  1517,   363,   296,   584,  2001,   328,  2063,
    1655,  1552,   659,   494,   336,  1897,  1151,   363,  1515,   951,
     336,   328,   328,   869,   447,  1743,   549,  1885,     3,   997,
     152,  1800,   363,  1081,   363,  1082,    99,  1083,   363,   363,
     494,   363,   186,     6,     7,     8,     9,    10,    11,    12,
      13,     0,   358,   670,   679,     0,     0,     0,   363,     0,
       0,     0,     0,   898,     0,     0,     0,     0,   358,     0,
     150,     0,   358,  1686,    65,    66,    67,    68,    69,    70,
      71,    72,  1041,  1972,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,    83,   111,     0,     0,
       0,     0,     0,   289,     0,     0,     0,     0,    92,     0,
       0,     0,     0,    83,   454,    83,     0,     0,   363,     0,
       0,     0,   112,  1042,   150,     0,   915,   363,    65,    66,
      67,    68,    69,    70,    71,    72,     0,    92,     0,     0,
       0,     0,   659,     0,    58,   282,     0,     0,   454,     0,
     289,   800,     0,     0,    83,     0,     0,    83,     0,   195,
       0,     0,     0,     0,     0,     0,     0,     0,    92,     0,
       0,     0,     0,     0,     0,   714,     0,   152,     0,     0,
       0,   487,     0,     0,   106,   834,  2049,   679,     0,     0,
       0,     0,     0,    99,     0,   290,     0,   684,   152,   299,
       0,     0,     0,     0,   494,   150,    74,     0,    75,    65,
      66,    67,    68,    69,    70,    71,    72,  1353,   106,    83,
     454,  1354,    99,  1355,     0,     0,     0,   666,   238,     0,
    1686,   667,     0,     0,     0,     0,     0,     0,    80,   668,
     238,     0,   270,     0,   722,    14,    15,    16,    17,    18,
       0,     0,     0,    99,   111,    77,     0,     0,   758,  1686,
       0,     0,     0,    83,   328,     0,   454,   454,     0,     0,
     328,   592,     0,   358,     0,   363,     0,     0,     0,   363,
       0,     0,     0,   111,   363,   363,     0,     0,   363,     0,
    1686,     0,     0,     0,     0,  1566,  1567,  1568,   363,     0,
       0,     0,  1569,  1570,    58,   363,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,  2049,     0,     0,     0,
    2049,  2049,     0,     0,     0,     0,     0,     0,   328,     0,
     328,     0,     0,    85,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,   722,     0,     0,     0,   358,
     487,  2163,   679,   363,     0,     0,     0,   363,     0,     0,
     670,   363,     0,     0,   670,     0,    74,   116,    75,     0,
       0,     0,     0,   358,  2175,    83,     0,  2103,  2175,     0,
       0,     0,     0,   679,     0,     0,   358,   799,     0,     0,
       0,   552,     0,  2185,     0,   152,     0,     0,    80,    81,
     112,     0,     0,   454,     0,     0,   152,   152,     0,   454,
       0,     0,   563,     0,     0,     0,     0,     0,   454,     0,
       0,   152,   152,   152,     0,   289,     0,     0,     0,     0,
       0,    83,    83,     0,   112,     0,     0,     0,     0,   106,
       0,     0,     0,   106,   106,     0,     0,     0,     0,     0,
       0,     0,   101,     0,     0,   156,     0,   106,     0,   282,
      14,    15,    16,    17,    18,   258,     0,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,   487,     0,     0,
       0,     0,     0,   659,     0,   592,     0,     0,     0,     0,
       0,     0,     0,   800,   800,     0,     0,     0,     0,     0,
       0,   454,     0,     0,     0,     0,     0,     0,     0,     0,
     101,   363,   494,     0,  2186,     0,     0,     0,     0,    58,
       0,   358,   487,  2193,   116,     0,   834,     0,   834,     0,
       0,     0,     0,     0,     0,     0,   211,     0,     0,     0,
       0,   358,     0,   358,     0,     0,     0,   358,   358,   358,
     358,     0,     0,   116,     0,   150,   281,   175,   176,    65,
      66,    67,    68,    69,    70,    71,    72,   358,   150,     0,
     235,   236,    65,    66,    67,    68,    69,    70,    71,    72,
       0,    74,   363,    75,   116,   363,   363,     0,   363,     0,
     315,     0,     0,   328,   320,     0,    74,     0,     0,     0,
     101,     0,  1807,   363,     0,     0,   552,   363,     0,     0,
       0,   363,     0,    80,    81,  1480,     0,  1660,    77,   360,
       0,   805,     0,     0,  1661,     0,     0,     0,    80,    81,
       0,   454,     0,     0,     0,     0,   358,     0,   817,     0,
       0,   820,   152,   454,  1501,     0,   465,     0,     0,     0,
       0,   358,     0,  1298,   270,   112,     0,   320,   493,   112,
     112,     0,     0,     0,   670,     0,     0,     0,     0,     0,
       0,     0,   150,   112,   175,   176,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,   546,
    1875,     0,     0,     0,     0,     0,     0,     0,   315,     0,
       0,     0,     0,   563,     0,     0,     0,     0,     0,   570,
       0,     0,   152,   487,   575,   577,     0,   211,   494,     0,
       0,     0,     0,   363,     0,   178,   181,   315,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   315,     0,
       0,   600,     0,     0,     0,   602,  1502,     0,     0,     0,
     603,     0,     0,     0,     0,     0,   614,     0,     0,     0,
       0,   577,   229,   315,   150,     0,     0,   627,    65,    66,
      67,    68,    69,    70,    71,    72,  1353,   363,   800,   636,
    1354,     0,  1355,     0,     0,     0,   363,     0,     0,    19,
     363,     0,     0,   358,   358,     0,     0,   834,     0,     0,
       0,     0,     0,     0,   834,     0,   592,     0,     0,   658,
       0,     0,   682,   322,    77,     0,   323,  1578,     0,     0,
       0,     0,     0,   106,     0,   689,     0,     0,     0,   689,
       0,   348,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,   495,     0,     0,     0,     0,     0,   358,
       0,   397,     0,     0,     0,     0,     0,   615,     0,     0,
       0,     0,   190,   397,     0,     0,     0,     0,     0,     0,
       0,   282,     0,     0,     0,     0,     0,   150,     0,   175,
     176,    65,    66,    67,    68,    69,    70,    71,    72,  1045,
       0,   152,     0,     0,     0,   527,     0,     0,     0,     0,
     152,   269,    14,    15,    16,    17,    18,     0,     0,   454,
       0,     0,     0,   291,     0,   298,     0,   300,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   517,     0,     0,
    1046,     0,     0,     0,   320,   454,  2053,   229,   658,     0,
       0,     0,     0,   454,     0,     0,   588,   589,     0,     0,
       0,     0,     0,     0,     0,   320,   269,   178,     0,   298,
     300,    58,     0,   805,   805,     0,     0,   266,    85,     0,
       0,     0,   178,   394,  1092,     0,     0,  1095,     0,   328,
     106,     0,     0,     0,     0,   152,     0,     0,     0,     0,
       0,     0,   487,     0,   660,   150,     0,   683,     0,    65,
      66,    67,    68,    69,    70,    71,    72,   638,     0,   106,
     660,   269,   614,     0,   660,   642,   644,     0,     0,     0,
     651,   487,     0,    74,     0,    75,   152,     0,     0,     0,
     493,     0,     0,     0,     0,     0,     0,     0,     0,   112,
     106,     0,     0,   563,    76,    77,   315,     0,   395,     0,
    1170,     0,     0,     0,  1174,    80,    81,     0,  1178,   348,
       0,     0,   348,     0,   618,   397,   150,     0,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   269,     0,   298,   300,     0,     0,     0,   614,     0,
     101,   358,     0,     0,   358,   358,     0,   358,     0,     0,
       0,     0,     0,     0,     0,     0,   689,   959,     0,     0,
       0,     0,     0,   906,     0,   269,     0,   614,     0,     0,
     269,   970,     0,   660,     0,     0,   269,     0,     0,     0,
     658,     0,     0,     0,     0,   979,     0,     0,     0,   152,
     152,   152,   152,   689,   152,   152,     0,     0,     0,     0,
    1662,   336,     0,     0,     0,   659,     0,     0,     0,   229,
     269,     0,     0,     0,   454,   685,     0,   300,   454,   454,
       0,   851,   852,     0,     0,     0,     0,     0,     0,   454,
       0,     0,   454,     0,     0,     0,     0,     0,     0,   615,
       0,     0,     0,     0,     0,   150,   112,   175,   176,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
     266,   721,     0,     0,     0,   495,     0,     0,     0,   859,
       0,   517,     0,     0,     0,   112,   659,   487,     0,     0,
       0,   150,     0,     0,   493,    65,    66,    67,    68,    69,
      70,    71,    72,  1353,   363,     0,     0,  1354,   805,  1355,
     614,  1084,   152,     0,   670,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,   269,   614,     0,     0,  1476,
     614,     0,     0,     0,     0,     0,     0,     0,   689,   959,
       0,    77,     0,   614,  1781,  1110,     0,     0,     0,     0,
       0,   660,   495,   269,     0,   685,   300,     0,   493,     0,
     493,     0,     0,     0,   493,   493,   360,   493,     0,     0,
     946,     0,   721,     0,     0,   660,     0,     0,     0,   348,
     618,    58,     0,  1420,   493,     0,  1424,     0,   660,     0,
    1428,   150,     0,   235,   236,    65,    66,    67,    68,    69,
      70,    71,    72,  1662,  1808,     0,   269,     0,  1662,     0,
     454,  1080,     0,     0,  1662,   150,  1662,   235,   236,    65,
      66,    67,    68,    69,    70,    71,    72,  1097,     0,     0,
     269,  1098,     0,     0,   397,   269,     0,   269,     0,   336,
     152,     0,     0,    74,   614,    75,     0,     0,  1273,     0,
       0,     0,     0,   493,     0,   384,   618,     0,     0,   156,
     269,     0,   269,   269,   237,    77,     0,     0,   689,    58,
       0,  1302,     0,     0,   269,    80,    81,     0,  1308,   495,
       0,     0,     0,  1047,     0,   618,     0,   269,     0,     0,
       0,     0,     0,     0,     0,     0,   269,     0,     0,   152,
       0,     0,     0,   150,     0,   235,   236,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,   269,     0,
     685,   300,     0,   660,   495,     0,     0,   152,   219,   320,
     360,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,   269,   685,     0,     0,     0,     0,     0,   269,
       0,   495,  2099,    77,     0,     0,   552,     0,     0,     0,
       0,  1808,  1808,    80,    81,     0,     0,     0,     0,   304,
       0,     0,     0,     0,     0,  1127,  1662,     0,     0,  1662,
       0,     0,     0,     0,     0,     0,  1143,     0,     0,     0,
       0,   336,     0,     0,     0,     0,  1594,     0,     0,     0,
       0,   614,     0,     0,     0,   614,     0,  1603,   454,     0,
     493,   493,     0,     0,   614,     0,     0,     0,   618,     0,
       0,     0,     0,     0,   614,     0,     0,     0,     0,   517,
       0,   614,   859,     0,   618,     0,     0,   341,   618,     0,
       0,   328,     0,     0,     0,   342,   343,   344,   345,   197,
       0,   618,     0,   660,     0,     0,   683,     0,     0,     0,
       0,     0,     0,     0,     0,  1211,   493,     0,     0,     0,
       0,   241,     0,     0,     0,     0,  1808,     0,     0,   614,
       0,     0,     0,   614,     0,  1662,   150,   614,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,  1376,     0,     0,     0,  1377,     0,   156,     0,
       0,     0,     0,     0,     0,   495,     0,  1457,     0,     0,
    1318,     0,     0,     0,   152,  1392,  1273,     0,   330,     0,
       0,     0,  1393,     0,     0,     0,     0,     0,     0,   346,
     150,     0,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,  1209,     0,  1808,     0,     0,   347,     0,     0,
    1273,     0,     0,   632,   150,   152,   382,   383,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
    1430,     0,     0,     0,  1431,  1516,     0,     0,  1432,     0,
       0,     0,     0,     0,   152,   152,     0,  2100,   336,     0,
       0,     0,     0,     0,     0,   330,     0,     0,  1356,   658,
     535,   241,  1356,     0,  1478,     0,    78,     0,   575,   269,
       0,     0,     0,     0,     0,   152,     0,     0,   384,     0,
     269,   330,     0,     0,     0,     0,     0,   614,   360,   269,
       0,  1356,     0,     0,   517,     0,     0,     0,     0,     0,
       0,     0,   150,  2100,  2100,     0,    65,    66,    67,    68,
      69,    70,    71,    72,  1353,   432,     0,     0,  1354,     0,
    1355,     0,     0,   150,  1811,   175,   176,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,   330,     0,     0,
       0,  2100,     0,     0,     0,     0,     0,     0,     0,     0,
     635,   330,    77,     0,     0,  1783,     0,     0,   493,   618,
    1356,   493,   493,   618,   360,     0,     0,     0,     0,     0,
       0,     0,   618,     0,     0,   829,     0,   831,     0,   614,
     643,     0,   618,   614,     0,   118,   848,   614,   118,   618,
       0,     0,     0,     0,   269,     0,     0,     0,     0,     0,
    1477,  1479,  1481,     0,     0,     0,  1457,  1457,  1457,   156,
     577,     0,     0,     0,     0,     0,     0,     0,     0,   150,
     269,   604,    64,    65,    66,    67,    68,    69,    70,    71,
      72,  1685,     0,     0,  1503,  1685,  1685,   618,     0,     0,
       0,   618,     0,   118,   660,   618,     0,     0,     0,  1685,
       0,     0,     0,     0,     0,  1211,     0,     0,     0,     0,
       0,  1522,     0,     0,     0,     0,     0,     0,     0,   118,
       0,     0,  1075,   495,     0,     0,     0,     0,     0,     0,
    1629,  1811,  1811,     0,  1630,   273,     0,     0,  1631,   118,
       0,     0,     0,     0,   360,     0,     0,     0,   150,   614,
     606,   607,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,   835,     0,     0,     0,     0,   156,
       0,     0,     0,   118,     0,     0,     0,   118,     0,     0,
       0,     0,     0,   118,   710,     0,   118,     0,   432,   716,
     273,     0,     0,     0,     0,     0,     0,     0,   725,   726,
      78,   354,   118,   614,   386,     0,     0,   875,     0,     0,
       0,   517,   614,   432,   432,     0,   614,     0,     0,   241,
    1356,     0,     0,     0,     0,     0,     0,   458,     0,     0,
       0,     0,     0,   269,   432,     0,     0,     0,     0,     0,
     118,   458,     0,   330,     0,   273,  1811,     0,     0,   330,
    1752,   784,   785,   786,   787,   788,   789,   790,   791,   792,
     793,   794,   269,     0,     0,   220,     0,   432,   269,     0,
       0,     0,     0,  1825,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,  1672,
    1673,     0,   795,     0,     0,     0,     0,  1837,   118,     0,
     118,     0,     0,     0,  1787,     0,     0,   943,     0,   330,
     118,     0,     0,  1790,     0,   273,     0,  1791,     0,     0,
    2061,   118,     0,     0,  1811,     0,     0,     0,     0,   495,
       0,     0,     0,     0,     0,     0,   609,   618,     0,   118,
       0,   618,     0,     0,   118,   618,   118,     0,   517,   273,
     118,     0,     0,     0,   273,     0,   156,     0,     0,     0,
     273,     0,    58,     0,     0,     0,     0,  1811,     0,   150,
     118,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,  1763,     0,     0,  1125,     0,     0,     0,     0,     0,
       0,    58,   118,     0,   273,   118,   150,     0,   235,   236,
      65,    66,    67,    68,    69,    70,    71,    72,   118,     0,
       0,     0,   118,     0,     0,     0,     0,   269,     0,  1922,
       0,    78,     0,  1811,  1811,   150,    75,   235,   236,    65,
      66,    67,    68,    69,    70,    71,    72,   517,     0,     0,
       0,     0,     0,     0,  1356,  1387,    77,     0,     0,  1356,
    1356,  1356,     0,    74,   458,    75,     0,   618,     0,     0,
       0,  1811,     0,     0,     0,  1685,    14,    15,    16,    17,
      18,     0,     0,     0,   327,    77,   269,     0,     0,  1277,
    1278,     0,   228,     0,     0,    80,    81,   150,   458,   175,
     176,    65,    66,    67,    68,    69,    70,    71,    72,  1833,
       0,     0,     0,     0,     0,  1104,     0,   835,     0,     0,
       0,   618,     0,     0,     0,     0,     0,   118,     0,     0,
     618,   458,     0,     0,   618,    58,   150,   273,   204,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   118,     0,
     432,   432,   432,   432,   432,   432,   432,   432,   432,   432,
     432,   432,   432,   432,   432,   432,   432,   432,   432,   150,
     458,   235,   236,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   330,     0,  1349,     0,    77,     0,     0,   858,
       0,  2044,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,   150,   118,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,   458,   458,   832,    77,
       0,   273,   667,   118,     0,     0,  1369,     0,     0,    80,
     833,     0,  1685,     0,     0,   432,     0,     0,     0,   118,
     517,   150,     0,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,   505,     0,   269,     0,  1356,   273,  1356,
       0,  1685,  2044,     0,     0,     0,     0,     0,     0,     0,
       0,   273,     0,     0,     0,  1395,     0,  1398,     0,     0,
     614,   118,     0,   118,     0,     0,     0,     0,     0,  1402,
     510,  1404,  1685,     0,     0,     0,  1409,  1410,   386,   118,
     458,     0,   273,     0,     0,     0,  1417,     0,     0,   150,
     118,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,  2137,     0,   118,     0,  1985,   273,     0,     0,     0,
     609,     0,  1436,   273,     0,  1439,   118,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,   458,     0,     0,   118,   118,     0,   458,
       0,     0,     0,     0,     0,     0,     0,     0,   458,     0,
       0,   118,   118,   118,     0,     0,   660,     0,     0,     0,
       0,  2114,     0,     0,     0,     0,  1104,     0,  2024,     0,
       0,     0,  1388,   835,     0,   269,     0,     0,  1499,     0,
       0,     0,     0,     0,   432,     0,     0,   269,     0,     0,
     432,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   432,     0,     0,     0,     0,     0,   458,  1520,     0,
       0,     0,     0,     0,     0,  1524,     0,  1526,  1528,     0,
       0,     0,     0,   118,     0,     0,  1534,   660,  1535,     0,
    1536,   458,     0,     0,     0,     0,     0,  1545,     0,   118,
       0,   432,   117,   118,     0,     0,     0,     0,     0,     0,
       0,   118,   458,     0,     0,     0,   118,   410,     0,   411,
     412,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   118,   269,   118,     0,     0,     0,   118,   118,   118,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   118,     0,     0,
     117,     0,     0,     0,    93,     0,     0,    93,   743,  1598,
    1599,    78,   421,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1185,     0,    14,    15,
      16,    17,    18,     0,     0,  1621,     0,     0,     0,     0,
       0,     0,  1626,     0,  1627,     0,   284,     0,   330,     0,
       0,     0,     0,     0,     0,     0,     0,   118,     0,     0,
       0,   458,    93,     0,     0,     0,   118,     0,   618,     0,
    1643,     0,   118,   458,     0,     0,     0,     0,     0,     0,
     117,   118,     0,  1300,   458,   432,     0,    58,     0,     0,
     117,     0,     0,     0,   269,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   268,     0,     0,     0,    93,   367,
       0,  1315,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,     0,   235,   236,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   118,   458,     0,     0,     0,     0,   496,    74,
    1608,    75,    93,     0,     0,     0,     0,     0,     0,   337,
       0,     0,     0,     0,     0,     0,  1751,     0,   432,     0,
    2099,    77,     0,  1755,   552,  1757,     0,     0,     0,     0,
       0,    80,    81,     0,     0,     0,     0,   269,   117,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   432,   432,   432,     0,     0,
     490,     0,   432,   432,   118,     0,     0,   117,   118,  1666,
       0,     0,     0,   118,   118,     0,     0,   118,   117,    58,
       0,     0,     0,     0,     0,     0,   432,   118,     0,     0,
       0,     0,     0,     0,   118,     0,   367,     0,     0,     0,
       0,  1793,     0,   117,     0,   192,     0,   284,     0,     0,
       0,     0,     0,   150,     0,   235,   236,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,   432,   432,   118,
       0,     0,     0,     0,   268,     0,     0,     0,     0,     0,
       0,    74,   118,    75,   271,     0,   118,     0,     0,   661,
     118,     0,   284,     0,     0,     0,   292,   295,     0,     0,
       0,     0,  1660,    77,     0,   661,     0,     0,     0,   661,
       0,   118,     0,    80,    81,     0,     0,     0,     0,     0,
     118,     0,     0,     0,    14,    15,    16,    17,    18,   458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   271,
     150,     0,   235,   236,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,   458,     0,     0,     0,     0,
       0,     0,     0,   458,     0,     0,     0,     0,    74,     0,
       0,     0,  1890,  1891,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,  1899,   273,   118,  1660,
      77,     0,     0,     0,   271,     0,  1661,     0,     0,     0,
      80,    81,  1666,     0,     0,   118,     0,  1666,     0,     0,
       0,     0,   458,  1823,   432,  1666,  1300,   150,   661,   235,
     236,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,   458,     0,     0,     0,    74,   118,    75,     0,     0,
       0,     0,   150,     0,   235,   236,    65,    66,    67,    68,
      69,    70,    71,    72,   271,     0,   237,    77,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,    81,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   367,     0,     0,     0,     0,     0,   271,     0,
       0,   832,    77,   271,     0,   667,     0,     0,     0,   271,
     496,   118,    80,   833,   118,   118,     0,   118,     0,     0,
       0,     0,     0,     0,     0,   669,   117,     0,     0,     0,
       0,     0,   118,     0,     0,     0,   118,     0,     0,     0,
     118,     0,     0,   271,     0,     0,     0,     0,     0,     0,
       0,     0,  1640,     0,     0,     0,     0,     0,     0,   118,
     118,   118,   118,   118,   118,   118,     0,     0,   367,     0,
     117,   273,   490,     0,     0,  1930,     0,     0,  1666,     0,
       0,     0,     0,     0,   458,     0,   661,   496,   458,   458,
     432,     0,     0,     0,   723,     0,     0,   367,     0,   458,
       0,     0,   458,     0,     0,     0,     0,     0,     0,   150,
     661,   235,   236,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   661,     0,     0,     0,     0,     0,     0,
     273,     0,    93,     0,     0,     0,     0,    74,     0,     0,
     330,     0,     0,     0,     0,     0,     0,   458,     0,   490,
       0,     0,   118,     0,     0,     0,     0,     0,  2099,    77,
       0,  2098,   552,     0,     0,  1762,     0,     0,     0,    80,
      81,     0,   118,     0,     0,     0,   271,     0,     0,     0,
       0,     0,     0,   150,  1666,   235,   236,    65,    66,    67,
      68,    69,    70,    71,    72,   723,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,     0,
       0,    74,     0,     0,   496,   118,  2134,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     367,     0,   237,    77,     0,     0,     0,  2149,     0,     0,
       0,     0,     0,    80,    81,     0,   367,     0,     0,     0,
     367,     0,  2158,     0,     0,     0,     0,     0,   661,   496,
     271,     0,     0,   367,     0,     0,     0,     0,     0,     0,
     458,     0,     0,     0,     0,     0,   490,     0,   367,     0,
     367,     0,     0,   271,   367,   367,   496,   367,   172,     0,
       0,     0,     0,     0,   432,     0,   330,   271,     0,   273,
     118,     0,     0,     0,   367,     0,     0,     0,     0,     0,
     271,     0,     0,     0,     0,     0,   172,     0,     0,     0,
     150,   490,   235,   236,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,   432,     0,     0,     0,
     490,   271,   490,     0,     0,     0,   490,   490,    74,   490,
       0,     0,   635,   330,     0,     0,     0,     0,     0,   118,
       0,     0,     0,   172,   367,   271,   490,     0,   117,   327,
      77,     0,   271,   367,     0,     0,   172,     0,   172,     0,
      80,    81,     0,     0,     0,     0,     0,   118,   661,     0,
     330,   284,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   172,
       0,   389,    14,    15,    16,    17,    18,     0,     0,     0,
       0,     0,     0,     0,     0,   432,     0,   432,     0,     0,
      93,     0,     0,     0,     0,   490,   389,     0,     0,     0,
       0,    93,     0,     0,     0,     0,     0,     0,     0,     0,
     496,   273,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   432,     0,   458,   149,
       0,    58,     0,     0,     0,     0,     0,   172,     0,     0,
       0,   172,     0,     0,   172,   172,     0,     0,   172,     0,
       0,   172,   172,     0,   172,   432,   172,     0,     0,     0,
       0,     0,     0,     0,     0,   150,     0,   235,   236,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,   367,     0,     0,     0,   367,     0,     0,     0,     0,
     367,   367,     0,    74,   367,    75,     0,   432,     0,     0,
       0,     0,     0,     0,   367,   207,     0,     0,     0,     0,
       0,   367,     0,     0,   327,    77,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,    81,   172,     0,     0,
     172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,  1184,   367,     0,     0,     0,
       0,     0,   490,   490,     0,   172,     0,     0,     0,   367,
       0,     0,     0,   367,     0,     0,     0,   367,     0,     0,
     172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1473,     0,     0,     0,     0,
       0,     0,   271,     0,     0,     0,   117,  1487,   490,     0,
       0,     0,     0,     0,   118,   118,     0,     0,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,   207,     0,     0,     0,     0,     0,
     117,     0,     0,     0,     0,   118,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   284,     0,     0,    93,     0,
       0,     0,     0,     0,   172,     0,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,   661,
       0,   579,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    93,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   367,   496,     0,
       0,     0,     0,     0,     0,     0,   268,    93,     0,     0,
       0,     0,   621,     0,   389,    58,     0,     0,     0,     0,
       0,     0,     0,     0,   628,     0,     0,     0,     0,     0,
     172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   579,     0,     0,     0,     0,     0,     0,     0,   150,
       0,   235,   236,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   656,     0,     0,     0,     0,   367,     0,
       0,   367,   367,     0,   367,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   367,
       0,     0,     0,   367,     0,     0,     0,   367,  1660,    77,
       0,     0,     0,     0,   374,     0,     0,     0,     0,    80,
      81,     0,     0,     0,   389,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   742,     0,     0,
     490,     0,     0,   490,   490,     0,     0,     0,     0,     0,
       0,   117,   483,   374,     0,   117,   117,     0,     0,     0,
       0,     0,     0,     0,   172,   172,     0,     0,   783,   117,
       0,     0,     0,  1732,  1733,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   172,     0,   172,   555,     0,     0,
       0,    93,     0,     0,   555,     0,   823,     0,     0,     0,
       0,   828,     0,     0,     0,     0,   271,     0,     0,     0,
       0,     0,     0,    93,   496,     0,     0,    93,    93,   367,
       0,   854,   855,     0,     0,     0,   856,   857,     0,     0,
     860,    93,     0,     0,     0,   271,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   873,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   268,
       0,     0,     0,     0,     0,     0,     0,     0,   902,     0,
       0,     0,     0,   367,     0,     0,   555,     0,     0,     0,
       0,     0,   367,     0,     0,     0,   367,     0,     0,     0,
       0,     0,     0,   172,   172,     0,     0,     0,     0,     0,
     172,    93,     0,     0,   374,   671,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   692,   172,     0,     0,   172,   172,
       0,   172,     0,   172,   172,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   942,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1840,     0,     0,   948,
       0,     0,  1849,     0,     0,     0,     0,   284,     0,     0,
       0,  1639,     0,     0,     0,     0,   172,     0,     0,     0,
     172,     0,     0,   967,   172,     0,     0,     0,     0,     0,
     271,     0,     0,  1874,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   555,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     555,   818,     0,   555,   821,     0,     0,     0,   268,    93,
       0,     0,     0,   374,     0,     0,     0,   671,     0,     0,
       0,     0,     0,     0,     0,     0,  1019,     0,     0,   271,
     483,     0,     0,     0,     0,     0,     0,     0,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   555,     0,     0,     0,   555,     0,     0,    93,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1932,  1933,     0,     0,     0,     0,     0,  1943,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1958,     0,     0,     0,     0,   374,     0,     0,     0,     0,
    1967,     0,  1968,     0,     0,   117,     0,     0,  1099,     0,
    1100,     0,     0,     0,     0,  1979,   828,  1981,  1982,  1983,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1144,     0,     0,     0,     0,     0,
     268,   555,     0,  1153,     0,     0,     0,  1155,     0,     0,
       0,     0,     0,     0,   216,     0,     0,    93,     0,   172,
       0,   957,   374,     0,     0,  2012,     0,     0,     0,  2017,
     277,     0,   671,     0,  2022,     0,   671,     0,   271,     0,
       0,     0,     0,   975,     0,   374,     0,     0,     0,     0,
       0,     0,   656,     0,     0,     0,     0,  1197,     0,     0,
       0,     0,     0,     0,   172,     0,   172,     0,     0,   172,
       0,   661,   172,     0,     0,     0,   172,     0,     0,     0,
       0,   216,     0,     0,     0,   338,     0,     0,     0,  2064,
       0,     0,     0,     0,     0,     0,     0,   379,     0,     0,
       0,  2073,     0,     0,     0,  2076,     0,     0,     0,     0,
       0,   369,   117,     0,     0,     0,     0,     0,     0,  2090,
       0,     0,   216,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   503,     0,     0,     0,
     509,   117,   661,  1327,     0,     0,     0,     0,     0,   374,
     369,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     367,  2121,     0,     0,     0,   555,   555,     0,     0,     0,
       0,     0,   117,     0,    93,     0,   555,  1093,     0,   555,
    1096,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     271,     0,     0,   957,   374,   216,     0,     0,   671,  2144,
     671,   671,   170,    93,  2147,     0,     0,   671,     0,     0,
     277,     0,     0,   374,   172,   374,     0,     0,     0,   374,
     374,   374,   374,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2167,     0,    93,  2169,     0,  2147,     0,   374,
       0,   555,     0,     0,     0,   555,     0,     0,   369,     0,
       0,     0,   555,  1171,     0,   509,   555,  1175,  2169,     0,
     555,  1179,     0,     0,     0,   216,     0,  1182,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   302,     0,     0,
       0,     0,   172,     0,     0,     0,     0,   664,     0,   681,
     308,     0,   309,   172,     0,     0,   172,     0,   172,   172,
       0,   369,     0,   369,   369,     0,     0,     0,   374,   555,
       0,     0,     0,     0,     0,     0,     0,   369,     0,     0,
       0,   369,     0,   381,     0,     0,     0,     0,     0,   149,
       0,     0,     0,     0,     0,     0,   671,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    20,   740,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,   783,    46,
       0,    47,     0,   216,   483,   374,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   271,   557,   558,
      58,     0,   562,     0,     0,   565,   566,   172,   568,     0,
     569,     0,     0,     0,     0,     0,   664,     0,     0,     0,
       0,     0,   846,     0,     0,     0,     0,     0,     0,     0,
     369,     0,  1533,     0,     0,     0,   369,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     555,     0,     0,     0,     0,   216,  1558,     0,     0,     0,
       0,     0,     0,     0,    75,   374,   374,     0,     0,   671,
     671,     0,     0,     0,     0,     0,   671,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   216,   216,     0,   369,     0,     0,     0,   503,   654,
     172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,   369,     0,   686,   555,  1421,     0,   555,  1425,
       0,     0,   555,  1429,     0,     0,     0,     0,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   369,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     172,     0,     0,     0,     0,     0,   172,     0,     0,     0,
     369,     0,     0,     0,     0,   503,     0,   961,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   369,   369,
       0,   369,     0,     0,     0,     0,     0,     0,   664,   369,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,     0,   369,     0,     0,   816,     0,
     216,     0,   369,     0,     0,   369,     0,     0,   216,     0,
       0,   740,   216,     0,   216,     0,     0,     0,     0,     0,
       0,   172,     0,   740,     0,     0,   740,   740,   740,     0,
    1745,     0,     0,     0,   374,     0,     0,     0,     0,     0,
     671,  1541,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,     0,     0,     0,     0,   172,   172,
       0,     0,     0,     0,   899,     0,     0,     0,     0,     0,
       0,     0,   503,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   172,   172,   369,     0,     0,     0,
       0,     0,   389,     0,     0,     0,   216,   172,   555,  1595,
       0,     0,   369,     0,     0,     0,     0,     0,     0,   555,
    1604,     0,   671,     0,     0,     0,     0,   503,   369,     0,
       0,     0,   369,   374,     0,     0,   374,   374,     0,   374,
     369,   369,     0,     0,     0,   369,   503,     0,   503,  1745,
       0,     0,   503,   503,   379,   503,     0,     0,     0,     0,
     369,     0,   369,     0,     0,     0,   369,   369,   369,   369,
       0,     0,   503,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   973,   974,
       0,     0,     0,     0,   172,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   983,     0,
     985,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     297,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   503,     0,     0,     0,     0,   369,     0,   216,     0,
       0,     0,  1886,  1887,     0,   369,     0,     0,   846,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
     369,     0,   369,   369,     0,     0,   172,     0,     0,     0,
       0,     0,     0,     0,   370,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   671,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   370,     0,     0,     0,  1086,  1087,     0,
       0,     0,   369,     0,  1091,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1114,
       0,     0,  1117,  1118,     0,  1121,     0,  1123,  1124,     0,
       0,     0,     0,     0,     0,  1970,     0,     0,     0,   172,
       0,     0,     0,     0,     0,     0,   555,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   503,   503,
       0,     0,   555,   369,     0,  1745,     0,   369,     0,     0,
    1169,     0,   369,   369,  1173,     0,   369,     0,  1177,     0,
       0,   370,     0,     0,     0,     0,   369,     0,     0,     0,
       0,     0,     0,   369,     0,     0,     0,     0,     0,     0,
       0,     0,  2011,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   503,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,  2040,     0,     0,   370,  2041,   370,   370,     0,     0,
       0,   369,     0,     0,     0,   369,     0,   264,     0,   369,
     370,     0,  1309,     0,   370,     0,     0,    14,    15,    16,
      17,    18,     0,     0,    20,   740,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -494,  -494,     0,  -494,    46,     0,    47,     0,  -494,
     740,     0,     0,   555,   555,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,   555,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   277,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     216,     0,    63,    64,     0,     0,     0,   664,     0,     0,
       0,     0,     0,   370,     0,     0,     0,     0,     0,   370,
       0,   369,     0,     0,     0,   369,     0,   371,    74,     0,
      75,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   740,     0,  1309,     0,     0,     0,     0,     0,   369,
     369,     0,    78,   506,     0,     0,     0,     0,     0,     0,
      80,    81,     0,     0,     0,     0,   371,     0,   555,     0,
       0,     0,     0,     0,     0,     0,   555,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   370,  1414,     0,
    1416,     0,     0,  1419,     0,     0,  1423,     0,     0,     0,
    1427,     0,     0,     0,     0,   370,   503,     0,     0,   503,
     503,     0,   379,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,   369,   369,     0,   369,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     370,   369,   555,  2062,     0,   369,   555,     0,     0,   369,
       0,     0,     0,     0,   740,   740,   740,     0,     0,   740,
     740,     0,     0,   370,   371,     0,   509,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   370,   370,     0,   370,     0,     0,     0,     0,   555,
       0,     0,   370,     0,   216,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,   370,     0,
       0,     0,     0,     0,     0,   370,     0,   371,   370,   371,
     371,     0,     0,     0,     0,   277,     0,     0,     0,     0,
       0,     0,     0,   371,     0,     0,     0,   371,  1539,     0,
       0,     0,   379,     0,     0,   555,   555,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,     0,     0,
       0,   369,     0,     0,   736,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   555,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1593,     0,     0,   370,
       0,     0,     0,     0,     0,   369,     0,  1602,     0,     0,
    1606,     0,  1609,  1610,   369,   370,     0,     0,   369,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   370,     0,     0,     0,   370,   371,     0,     0,     0,
       0,     0,   371,   370,   370,     0,     0,     0,   370,     0,
       0,     0,     0,     0,     0,   216,     0,     0,     0,     0,
       0,     0,     0,   370,     0,   370,     0,     0,     0,   370,
     370,   370,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   277,     0,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   910,   912,     0,     0,
     371,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1726,     0,     0,     0,     0,     0,     0,   371,     0,
       0,     0,     0,     0,   456,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   488,   370,
       0,     0,     0,     0,     0,     0,     0,     0,   370,     0,
       0,     0,   518,   371,   518,     0,     0,     0,     0,     0,
       0,     0,   740,   370,     0,   370,   370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   371,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   371,   371,     0,   371,     0,     0,
       0,     0,     0,     0,     0,   371,     0,     0,     0,     0,
       0,     0,     0,     0,  1606,     0,     0,     0,   371,     0,
       0,   371,     0,     0,     0,   370,   277,     0,   371,     0,
       0,   371,     0,   736,     0,     0,     0,     0,     0,   736,
       0,     0,  1795,     0,     0,     0,     0,     0,   736,     0,
       0,     0,     0,     0,     0,     0,   633,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   736,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
     370,     0,     0,  1072,     0,   370,   370,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,   371,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   371,  1883,     0,     0,   371,   740,
       0,   370,     0,     0,     0,     0,   371,   371,     0,     0,
       0,   371,     0,   369,   370,     0,     0,     0,   370,     0,
       0,     0,   370,     0,     0,     0,   371,     0,   371,     0,
       0,     0,   371,   371,   371,   371,     0,     0,     0,     0,
       0,     0,  1911,  1912,     0,     0,     0,     0,     0,     0,
       0,     0,   371,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1926,  1927,
     740,     0,     0,   509,     0,     0,     0,     0,     0,     0,
       0,  1931,     0,     0,   369,     0,   518,   369,     0,     0,
       0,     0,   518,     0,     0,     0,     0,   456,     0,     0,
       0,     0,   369,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   371,     0,     0,     0,     0,     0,     0,     0,
       0,   371,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   371,     0,   371,   371,
       0,     0,     0,     0,   370,     0,     0,     0,   370,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   370,   370,     0,     0,     0,     0,  2000,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   371,   941,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   488,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     969,     0,     0,   370,     0,     0,   370,   370,     0,   370,
    2060,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   370,     0,     0,     0,   370,   371,
       0,     0,   370,   371,     0,     0,     0,  1002,   371,   371,
       0,     0,   371,     0,     0,     0,     0,     0,  1012,     0,
       0,     0,   371,     0,     0,     0,     0,     0,     0,   371,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1033,  1035,     0,     0,  1037,     0,  1039,     0,
       0,     0,     0,     0,  1002,     0,  1049,  1002,     0,     0,
       0,     0,     0,     0,   371,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   371,     0,     0,
       0,   371,     0,     0,  1077,   371,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1079,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1088,   370,
       0,     0,     0,     0,   370,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   488,     0,     0,     0,     0,  1077,
       0,     0,     0,     0,     0,   736,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1147,     0,     0,   518,     0,     0,     0,   370,     0,
       0,     0,     0,     0,     0,  1158,     0,   370,     0,     0,
       0,   370,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1183,     0,     0,     0,   371,     0,     0,
       0,   371,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   371,   371,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     456,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1299,  1301,     0,     0,     0,     0,     0,     0,   488,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2003,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   371,     0,     0,   371,
     371,     0,   371,     0,     0,     0,     0,     0,     0,     0,
    1077,     0,     0,     0,     0,     0,     0,   371,  1341,     0,
       0,   371,     0,     0,     0,   371,     0,  1002,     0,   404,
       0,     0,   405,     0,   406,     0,   407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,  1679,  1687,     0,     0,  1679,  1697,
       0,     0,     0,     0,  1704,     0,     0,     0,  1708,   518,
    1710,     0,  1697,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,   410,     0,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,     0,   418,   419,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   371,     0,     0,   420,     0,   371,    78,   421,
       0,     0,   518,     0,  1413,   422,    80,    81,   423,   424,
     425,   426,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -494,  -494,     0,  -494,
      46,   371,    47,     0,  -494,     0,     0,     0,     0,     0,
     371,     0,     0,     0,   371,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
    1799,     0,     0,     0,     0,     0,  1490,  1490,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,   150,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,  1836,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,  1855,  1857,     0,     0,     0,     0,
       0,     0,     0,  1537,    76,    77,     0,    78,   506,  1546,
       0,     0,     0,     0,     0,    80,    81,   370,     0,     0,
     370,     0,     0,     0,  1002,  1877,     0,     0,   488,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   518,     0,     0,  1574,
       0,   186,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,  1033,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,   259,   260,     0,   261,    46,     0,
      47,     0,   262,     0,     0,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1941,     0,     0,     0,     0,  1637,  1638,
    1944,     0,  1946,     0,     0,  1952,  1957,     0,  1697,     0,
       0,     0,     0,  1963,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1002,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1307,
       0,     0,     0,   518,     0,     0,   456,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -469,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,   405,  -469,   406,     0,
     407,     0,     0,     0,  1035,     0,     0,     0,     0,     0,
       0,     0,     0,  1753,  1754,     0,    58,   408,     0,  2028,
       0,     0,     0,     0,  2034,  2036,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   518,     0,  2055,     0,  1033,   409,   410,   371,
     411,   412,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   413,   414,   401,     0,   415,   416,   417,     0,
     418,   419,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,  2077,     0,  2080,     0,     0,     0,  2082,
    2084,     0,     0,     0,  2087,  2089,     0,     0,     0,   420,
       0,     0,    78,   421,     0,     0,     0,     0,     0,   422,
     486,    81,   423,   424,   425,   426,     0,     0,     0,     0,
     371,     0,     0,   371,     0,     0,     0,   456,     0,     0,
       0,     0,  1824,     0,     0,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2128,  2130,  2132,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2153,  2155,  2157,
       0,     0,  1870,     0,     0,     0,     0,     0,     0,     0,
    2168,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1472,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   518,     0,     0,     0,     0,
       0,     0,  1901,     0,     0,  1903,     0,     0,   404,     0,
       0,   405,     0,   406,     0,   407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1214,  1917,   408,  1216,     0,  1217,  -250,  -250,  1218,  1219,
    1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    -346,  -346,  1230,  1231,  1232,  1233,  1234,  1235,  1236,     0,
    1237,     0,   409,   410,     0,   512,   412,  1238,  1239,    65,
      66,    67,    68,    69,    70,    71,    72,   413,   414,   401,
    1240,   415,   416,   417,     0,   418,   419,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2168,     0,     0,     0,     0,     0,
       0,     0,     0,  -250,  1241,     0,     0,    78,   421,     0,
       0,  1472,   306,     0,   422,    80,    81,   423,   424,   425,
     426,     0,     0,     0,     0,     0,     0,     0,     0,  -190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   404,     0,     0,   405,     0,   406,     0,   407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1214,     0,   408,  1216,     0,  1217,
    -251,  -251,  1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,
    1226,  1227,  1228,  1229,  -346,  -346,  1230,  1231,  1232,  1233,
    1234,  1235,  1236,     0,  1237,     0,   409,   410,  1002,   512,
     412,  1238,  1239,    65,    66,    67,    68,    69,    70,    71,
      72,   413,   414,   401,  1240,   415,   416,   417,     0,   418,
     419,  1873,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1472,     0,
       0,     0,     0,     0,     0,     0,     0,  -251,  1241,     0,
       0,    78,   421,     0,     0,     0,   306,     0,   422,    80,
      81,   423,   424,   425,   426,     0,     0,     0,     0,   404,
       0,     0,   405,  -190,   406,     0,   407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1214,     0,   408,  1216,     0,  1217,     0,     0,  1218,
    1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,
    1229,  -346,  -346,  1230,  1231,  1232,  1233,  1234,  1235,  1236,
       0,  1237,     0,   409,   410,     0,   512,   412,  1238,  1239,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,  1240,   415,   416,   417,     0,   418,   419,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1241,     0,     0,    78,   421,
       0,     0,     0,   306,     0,   422,    80,    81,   423,   424,
     425,   426,     0,     0,     0,     0,     0,     0,     0,     0,
    -190,     4,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1213,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,     0,   407,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1214,
      58,  1215,  1216,     0,  1217,     0,     0,  1218,  1219,  1220,
    1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  -346,
    -346,  1230,  1231,  1232,  1233,  1234,  1235,  1236,     0,  1237,
       0,   409,   410,    61,   512,   412,  1238,  1239,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,  1240,
     415,   416,   417,     0,   418,   419,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -3,  1241,     0,     0,    78,  1242,     0,     0,
       0,   306,     0,   422,    80,    81,   423,   424,   425,   426,
       0,     0,     0,     0,     0,     0,     0,     0,  -190,     4,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  1213,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   404,     0,    46,   405,    47,
     406,     0,   407,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,  1214,    58,  1215,
    1216,     0,  1217,     0,     0,  1218,  1219,  1220,  1221,  1222,
    1223,  1224,  1225,  1226,  1227,  1228,  1229,  -346,  -346,  1230,
    1231,  1232,  1233,  1234,  1235,  1236,     0,  1237,     0,   409,
     410,    61,   512,   412,  1238,  1239,    65,    66,    67,    68,
      69,    70,    71,    72,   413,   414,   401,  1240,   415,   416,
     417,     0,   418,   419,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1241,     0,     0,    78,  1242,     0,     0,     0,   306,
       0,   422,    80,    81,   423,   424,   425,   426,     0,     0,
       0,     0,     0,     0,     0,     0,  -190,     4,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   404,     0,    46,   405,    47,   406,     0,
     407,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   408,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,   410,    61,
     411,   412,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   413,   414,   401,     0,   415,   416,   417,     0,
     418,   419,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1691,  1692,  1693,     0,     0,     0,   420,
    1694,  1695,    78,  1242,     0,     0,     0,     0,     0,   422,
      80,    81,   423,   424,   425,   426,     0,     0,     0,     0,
       0,     0,     0,     0,  1696,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   404,     0,    46,   405,    47,   406,     0,   407,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   408,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,   410,    61,   411,   412,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     413,   414,   401,     0,   415,   416,   417,     0,   418,   419,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1691,  1692,  1693,     0,     0,     0,   420,  1694,     0,
      78,  1242,     0,     0,     0,     0,     0,   422,    80,    81,
     423,   424,   425,   426,     0,     0,     0,     0,     0,     0,
       0,     0,  1696,     4,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   404,
       0,    46,   405,    47,   406,     0,   407,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,   410,    61,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,     0,   418,   419,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   420,     0,  1689,    78,  1242,
       0,     0,     0,     0,     0,   422,    80,    81,   423,   424,
     425,   426,     4,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   404,     0,
      46,   405,    47,   406,     0,   407,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   408,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,   410,    61,   411,   412,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   413,   414,   401,
       0,   415,   416,   417,     0,   418,   419,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   420,     0,     0,    78,  1242,     0,
       0,     0,     0,     0,   422,    80,    81,   423,   424,   425,
     426,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,     0,   407,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     408,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,   410,     0,   411,   412,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   413,   414,   401,     0,   415,
     416,   417,     0,   418,   419,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   420,     0,     0,    78,   485,     0,     0,     0,
       0,     0,   422,   486,    81,   423,   424,   425,   426,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   404,     0,    46,   405,    47,   406,
       0,   407,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   408,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,   410,
       0,   411,   412,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   413,   414,   401,     0,   415,   416,   417,
       0,   418,   419,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     420,     0,     0,    78,  1296,     0,     0,     0,     0,     0,
     422,  1297,    81,   423,   424,   425,   426,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,     0,   407,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   408,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,   410,     0,   411,
     412,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   413,   414,   401,     0,   415,   416,   417,     0,   418,
     419,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   420,     0,
       0,    78,   830,     0,     0,     0,     0,     0,   422,   486,
      81,   423,   424,   425,   426,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,     0,   407,   355,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   408,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,   410,     0,   411,   412,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   413,
     414,   401,     0,   415,   416,   417,     0,   418,   419,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   420,     0,     0,    78,
     421,     0,     0,     0,     0,     0,   422,    80,    81,   423,
     424,   425,   426,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   404,     0,
      46,   405,    47,   406,     0,   407,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   408,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,   410,     0,   411,   412,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   413,   414,   401,
       0,   415,   416,   417,     0,   418,   419,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   420,     0,     0,    78,   830,     0,
       0,     0,     0,     0,   422,    80,    81,   423,   424,   425,
     426,  2010,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
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
       0,     0,     0,     0,     0,     0,    -2,    -2,  2039,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,     0,     0,    -2,     0,  1543,    -2,
       0,     0,     0,     0,    -2,    -2,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,   404,     0,     0,   405,     0,   406,     0,   407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,    -2,     0,    58,   408,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,    -2,    -2,     0,   409,   410,     0,   411,
     412,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   413,   414,   401,     0,   415,   416,   417,     0,   418,
     419,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   420,     0,
       0,    78,   421,     0,     0,     0,     0,     0,   422,  1544,
      81,   423,   424,   425,   426,     4,     5,     6,     7,     8,
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
     264,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -494,  -494,     0,  -494,    46,     0,
      47,     0,  -494,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,   265,     0,     0,     0,
    -821,     0,     0,    80,    81,   264,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -494,
    -494,     0,  -494,    46,     0,    47,     0,  -494,     0,     0,
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
      78,   265,     0,     0,     0,     0,     0,     0,    80,    81,
       4,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
       0,     0,     0,     0,  -413,  -413,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -413,     0,     0,     0,    78,    79,     0,     0,     0,
       0,     0,     0,    80,    81,     4,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,     0,     0,     0,     0,  -414,
    -414,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -414,     0,     0,     0,
      78,    79,     0,  1448,     0,  1449,     0,     0,    80,    81,
    1450,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1451,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1452,     0,     0,     0,    78,  1007,
       0,  1448,     0,  1449,     0,     0,    80,    81,  1450,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1451,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1646,     0,     0,     0,    78,  1007,     0,  1448,
       0,  1449,     0,     0,    80,    81,  1450,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1451,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1647,     0,     0,     0,    78,  1007,     0,  1448,     0,  1449,
       0,     0,    80,    81,  1450,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1451,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1648,     0,
       0,     0,    78,  1007,     0,     0,     0,     0,     0,     0,
      80,    81,   264,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -494,  -494,     0,  -494,
      46,     0,    47,     0,  -494,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   265,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
     612,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1103,    77,
    -687,    78,   667,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -494,  -494,     0,  -494,    46,     0,
      47,     0,  -494,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,    78,   265,     0,     0,     0,
    -825,     0,     0,    80,    81,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -494,  -494,
       0,  -494,    46,     0,    47,     0,  -494,     0,     0,     0,
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
     265,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,   612,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     666,     0,  -687,    78,   667,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,   612,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   799,     0,  -687,    78,   552,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
    1136,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -695,    78,   916,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   356,    78,   357,     0,     0,     0,
       0,     0,     0,    80,    81,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   355,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,  1616,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     916,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,  1618,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   916,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   506,     0,
       0,     0,     0,     0,     0,    80,    81,   186,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   916,     0,     0,     0,     0,     0,     0,    80,
      81,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   357,     0,     0,     0,
       0,     0,     0,    80,    81,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -494,  -494,
       0,  -494,    46,     0,    47,     0,  -494,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,  1472,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,     0,     0,
     405,     0,   406,     0,   407,     0,     0,     0,     0,    78,
     265,     0,     0,     0,     0,     0,     0,    80,    81,  1214,
       0,   408,  1216,     0,  1217,  1934,  1935,  1218,  1219,  1220,
    1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,     0,
       0,  1230,  1231,  1232,  1233,  1234,  1235,  1236,     0,  1237,
       0,   409,   410,     0,   512,   412,  1238,  1239,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,  1240,
     415,   416,   417,     0,   418,   419,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,  1472,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1241,     0,     0,    78,   421,     0,     0,
       0,   306,     0,   422,    80,    81,   423,   424,   425,   426,
     404,     0,     0,   405,     0,   406,     0,   407,  -190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1214,     0,   408,  1216,     0,  1217,     0,     0,
    1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  1229,     0,     0,  1230,  1231,  1232,  1233,  1234,  1235,
    1236,     0,  1237,     0,   409,   410,     0,   512,   412,  1238,
    1239,    65,    66,    67,    68,    69,    70,    71,    72,   413,
     414,   401,  1240,   415,   416,   417,     0,   418,   419,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1241,     0,     0,    78,
     421,     0,     0,     0,   306,     0,   422,    80,    81,   423,
     424,   425,   426,     0,     0,     0,     0,     0,     0,     0,
       0,  -190,   310,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -417,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,     0,     0,
       0,     0,  -417,   310,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -418,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,     0,
       0,     0,     0,  -418,   310,   186,     6,     7,     8,     9,
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
      64,   404,     0,    46,   405,    47,   406,     0,   407,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   408,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   729,     0,     0,
       0,     0,  1229,     0,  -346,     0,     0,     0,     0,    78,
       0,     0,     0,     0,  -417,   409,   410,     0,   411,   412,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     413,   414,   401,     0,   415,   416,   417,     0,   418,   419,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1241,     0,     0,
      78,   730,     0,     0,     0,   306,     0,   422,    80,    81,
     731,   732,   425,   426,    14,    15,    16,    17,    18,    19,
     727,    20,   728,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,     0,   407,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   408,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   729,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,   410,     0,   411,   412,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   413,
     414,   401,     0,   415,   416,   417,     0,   418,   419,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   420,     0,     0,    78,
     730,     0,     0,     0,   306,     0,   422,    80,    81,   731,
     732,   425,   426,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   404,
       0,    46,   405,    47,   406,     0,   407,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,   410,     0,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,     0,   418,   419,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   420,     0,   451,    78,   452,
       0,     0,     0,     0,     0,   422,    80,    81,   423,   424,
     425,   426,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   404,     0,
      46,   405,    47,   406,     0,   407,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   408,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,   410,     0,   411,   412,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   413,   414,   401,
       0,   415,   416,   417,     0,   418,   419,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   420,     0,     0,    78,   452,     0,
       0,     0,   306,     0,   422,    80,    81,   423,   424,   425,
     426,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,     0,   407,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   408,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,   410,     0,   411,   412,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,     0,
     415,   416,   417,     0,   418,   419,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   420,     0,     0,    78,   730,     0,     0,
       0,   306,     0,   422,    80,    81,   423,   424,   425,   426,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,     0,   407,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     408,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,   410,     0,   411,   412,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   413,   414,   401,     0,   415,
     416,   417,     0,   418,   419,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   420,     0,     0,    78,   452,     0,     0,     0,
       0,     0,   422,    80,    81,   423,   424,   425,   426,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   404,     0,    46,   405,    47,
     406,     0,   407,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
     410,     0,   411,   412,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   413,   414,   401,     0,   415,   416,
     417,     0,   418,   419,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   420,     0,     0,    78,   830,     0,     0,     0,     0,
       0,   422,    80,    81,   423,   424,   425,   426,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   404,     0,    46,   405,    47,   406,
       0,   407,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   408,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,   410,
       0,   411,   412,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   413,   414,   401,     0,   415,   416,   417,
       0,   418,   419,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     420,     0,     0,    78,   421,     0,     0,     0,     0,     0,
     422,    80,    81,   423,   424,   425,   426,   186,     6,     7,
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
       0,     0,     0,     0,     0,     0,     0,   264,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,   -17,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -494,  -494,     0,  -494,    46,     0,    47,     0,  -494,
       0,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,   150,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   612,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -687,    78,    14,    15,    16,    17,
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
       0,    78,    79,     0,     0,     0,  -823,     0,     0,    80,
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
       0,    47,     0,     0,     0,   355,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,   479,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   853,     0,     0,    78,   480,     0,     0,
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
       0,     0,     0,     0,   150,     0,   479,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   480,     0,     0,
       0,     0,     0,     0,    80,    81,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   355,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   612,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -687,
      78,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,  1207,     0,     0,     0,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,    78,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   355,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   853,     0,     0,    78,   480,     0,     0,     0,
       0,     0,     0,    80,    81,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   355,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   853,     0,     0,
      78,   480,     0,    63,    64,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1014,    78,  1007,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,  1562,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,  1007,
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
       0,     0,     0,    78,   318,     0,    63,    64,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   208,     0,     0,
       0,     0,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    78,   357,     0,    63,    64,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   318,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   355,    49,
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
     480,     0,     0,     0,     0,     0,     0,    80,    81,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -494,  -494,     0,  -494,    46,     0,    47,     0,
    -494,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,   355,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   506,     0,     0,
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
       0,    78,  1007,     0,    63,    64,     0,     0,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,    79,     0,     0,     0,     0,
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
      64,   355,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     480,     0,    63,    64,     0,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,  1007,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,     0,     0,
      14,    15,    16,    17,    18,    80,    81,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -494,  -494,     0,  -494,    46,     0,
      47,     0,  -494,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   335,     0,    14,    15,
      16,    17,    18,    80,    81,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -494,  -494,     0,  -494,    46,     0,    47,     0,
    -494,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   506,     0,    14,    15,    16,    17,
      18,    80,    81,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -494,  -494,     0,  -494,    46,     0,    47,     0,  -494,     0,
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
     404,     0,    46,   405,    47,   406,     0,   407,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   408,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,   410,     0,   411,   412,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   413,
     414,   401,     0,   415,   416,   417,     0,   418,   419,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   420,     0,     0,    78,
     421,     0,     0,     0,     0,     0,   422,   486,    81,   423,
     424,   425,   426,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   404,     0,    46,   405,    47,   406,     0,   407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   408,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,   410,     0,   411,
     412,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   413,   414,   401,     0,   415,   416,   417,     0,   418,
     419,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   420,     0,
       0,    78,   421,     0,     0,     0,     0,     0,   422,    80,
      81,   423,   424,   425,   426,    14,    15,    16,    17,    18,
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
       0,     0,     0,    46,     0,    47,     0,    63,    64,   355,
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
      40,    41,    42,    43,    44,    45,  -494,  -494,     0,  -494,
      46,     0,    47,     0,  -494,     0,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    78,   187,     0,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   186,     6,     7,     8,     9,    10,    11,    12,    13,
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
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -493,
    -493,     0,  -493,    46,     0,    47,     0,  -493,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -494,  -494,     0,  -494,    46,     0,    47,   404,  -494,     0,
     405,     0,   406,     0,   407,  1949,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,   408,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,   410,     0,   411,   412,  1950,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,     0,
     415,   416,   417,     0,   418,   419,     0,     0,   404,    75,
       0,   405,    74,   406,     0,   407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1691,  1692,  1693,
       0,     0,   408,   420,  1951,     0,    78,   421,     0,     0,
       0,     0,     0,   422,    80,    81,   423,   424,   425,   426,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,   410,     0,   411,   412,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   413,   414,   401,
       0,   415,   416,   417,     0,   418,   419,   404,     0,     0,
     405,     0,   406,    74,   407,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1691,  1692,
    1693,   408,     0,     0,   420,  1856,     0,    78,   421,     0,
       0,     0,     0,     0,   422,    80,    81,   423,   424,   425,
     426,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,   410,     0,   512,   412,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,     0,
     415,   416,   417,   404,   418,   419,   405,     0,   406,     0,
     407,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,     0,     0,
       0,     0,     0,   420,    77,     0,   513,   514,     0,     0,
       0,   515,     0,   422,    80,    81,   423,   424,   425,   426,
       0,     0,     0,     0,     0,     0,     0,   409,   410,     0,
     411,   412,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   413,   414,   401,     0,   415,   416,   417,   404,
     418,   419,   405,     0,   406,     0,   407,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,     0,     0,     0,   420,
    1344,     0,    78,   421,     0,     0,     0,  1345,     0,   422,
      80,    81,   423,   424,   425,   426,     0,     0,     0,     0,
       0,     0,     0,   409,   410,     0,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,   404,   418,   419,   405,     0,
     406,     0,   407,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
       0,     0,     0,     0,     0,   420,     0,     0,    78,   421,
       0,     0,     0,   515,     0,   422,    80,    81,   423,   424,
     425,   426,     0,     0,     0,     0,     0,     0,     0,   409,
     410,     0,   411,   412,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   413,   414,   401,     0,   415,   416,
     417,   404,   418,   419,   405,     0,   406,     0,   407,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,     0,     0,     0,     0,
       0,   420,   998,     0,    78,   421,     0,     0,     0,     0,
       0,   422,    80,    81,   423,   424,   425,   426,     0,     0,
       0,     0,     0,     0,     0,   409,   410,     0,   411,   412,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     413,   414,   401,     0,   415,   416,   417,   404,   418,   419,
     405,     0,   406,     0,   407,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,     0,     0,     0,     0,     0,   420,  1032,     0,
      78,   421,     0,     0,     0,     0,     0,   422,    80,    81,
     423,   424,   425,   426,     0,     0,     0,     0,     0,     0,
       0,   409,   410,     0,   411,   412,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,     0,
     415,   416,   417,   404,   418,   419,   405,     0,   406,     0,
     407,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,     0,     0,
       0,     0,     0,   420,     0,     0,    78,   421,     0,     0,
       0,   306,     0,   422,    80,    81,   423,   424,   425,   426,
       0,     0,     0,     0,     0,     0,     0,   409,   410,     0,
     411,   412,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   413,   414,   401,     0,   415,   416,   417,   404,
     418,   419,   405,     0,   406,     0,   407,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,     0,     0,     0,   420,
       0,     0,    78,   421,     0,     0,  1071,     0,     0,   422,
      80,    81,   423,   424,   425,   426,     0,     0,     0,     0,
       0,     0,     0,   409,   410,     0,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,   404,   418,   419,   405,     0,
     406,     0,   407,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
       0,     0,     0,     0,     0,   420,     0,     0,    78,   421,
       0,     0,     0,  1482,     0,   422,    80,    81,   423,   424,
     425,   426,     0,     0,     0,     0,     0,     0,     0,   409,
     410,     0,   411,   412,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   413,   414,   401,     0,   415,   416,
     417,   404,   418,   419,   405,     0,   406,     0,   407,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,     0,     0,     0,     0,
       0,   420,  1573,     0,    78,   421,     0,     0,     0,     0,
       0,   422,    80,    81,   423,   424,   425,   426,     0,     0,
       0,     0,     0,     0,     0,   409,   410,     0,   411,   412,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     413,   414,   401,     0,   415,   416,   417,   404,   418,   419,
     405,     0,   406,     0,   407,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,     0,     0,     0,     0,     0,   420,     0,     0,
      78,   421,     0,     0,     0,  1764,     0,   422,    80,    81,
     423,   424,   425,   426,     0,     0,     0,     0,     0,     0,
       0,   409,   410,     0,   411,   412,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,     0,
     415,   416,   417,   404,   418,   419,   405,     0,   406,     0,
     407,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,     0,     0,
       0,     0,     0,   420,     0,  1940,    78,   421,     0,     0,
       0,     0,     0,   422,    80,    81,   423,   424,   425,   426,
       0,     0,     0,     0,     0,     0,     0,   409,   410,     0,
     411,   412,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   413,   414,   401,     0,   415,   416,   417,   404,
     418,   419,   405,     0,   406,     0,   407,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,     0,     0,     0,   420,
    1945,     0,    78,   421,     0,     0,     0,     0,     0,   422,
      80,    81,   423,   424,   425,   426,     0,     0,     0,     0,
       0,     0,     0,   409,   410,     0,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,   404,   418,   419,   405,     0,
     406,     0,   407,  1949,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
       0,     0,     0,     0,     0,   420,  1956,     0,    78,   421,
       0,     0,     0,     0,     0,   422,    80,    81,   423,   424,
     425,   426,     0,     0,     0,     0,     0,     0,     0,   409,
     410,     0,   411,   412,  1950,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   413,   414,   401,     0,   415,   416,
     417,   404,   418,   419,   405,     0,   406,     0,   407,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,     0,     0,     0,     0,
       0,   420,     0,     0,    78,   421,     0,     0,     0,     0,
       0,   422,    80,    81,   423,   424,   425,   426,     0,     0,
       0,     0,     0,     0,     0,   409,   410,     0,   411,   412,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     413,   414,   401,     0,   415,   416,   417,   404,   418,   419,
     405,     0,   406,     0,   407,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,     0,     0,     0,     0,     0,   420,  2033,     0,
      78,   421,     0,     0,     0,     0,     0,   422,    80,    81,
     423,   424,   425,   426,     0,     0,     0,     0,     0,     0,
       0,   409,   410,     0,   411,   412,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,     0,
     415,   416,   417,   404,   418,   419,   405,     0,   406,     0,
     407,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,     0,     0,
       0,     0,     0,   420,  2035,     0,    78,   421,     0,     0,
       0,     0,     0,   422,    80,    81,   423,   424,   425,   426,
       0,     0,     0,     0,     0,     0,     0,   409,   410,     0,
     411,   412,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   413,   414,   401,     0,   415,   416,   417,   404,
     418,   419,   405,     0,   406,     0,   407,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,     0,     0,     0,   420,
    2079,     0,    78,   421,     0,     0,     0,     0,     0,   422,
      80,    81,   423,   424,   425,   426,     0,     0,     0,     0,
       0,     0,     0,   409,   410,     0,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,   404,   418,   419,   405,     0,
     406,     0,   407,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
       0,     0,     0,     0,     0,   420,  2081,     0,    78,   421,
       0,     0,     0,     0,     0,   422,    80,    81,   423,   424,
     425,   426,     0,     0,     0,     0,     0,     0,     0,   409,
     410,     0,   411,   412,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   413,   414,   401,     0,   415,   416,
     417,   404,   418,   419,   405,     0,   406,     0,   407,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,     0,     0,     0,     0,
       0,   420,  2083,     0,    78,   421,     0,     0,     0,     0,
       0,   422,    80,    81,   423,   424,   425,   426,     0,     0,
       0,     0,     0,     0,     0,   409,   410,     0,   411,   412,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     413,   414,   401,     0,   415,   416,   417,   404,   418,   419,
     405,     0,   406,     0,   407,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,     0,     0,     0,     0,     0,   420,  2086,     0,
      78,   421,     0,     0,     0,     0,     0,   422,    80,    81,
     423,   424,   425,   426,     0,     0,     0,     0,     0,     0,
       0,   409,   410,     0,   411,   412,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,     0,
     415,   416,   417,   404,   418,   419,   405,     0,   406,     0,
     407,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,     0,     0,
       0,     0,     0,   420,  2088,     0,    78,   421,     0,     0,
       0,     0,     0,   422,    80,    81,   423,   424,   425,   426,
       0,     0,     0,     0,     0,     0,     0,   409,   410,     0,
     411,   412,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   413,   414,   401,     0,   415,   416,   417,   404,
     418,   419,   405,     0,   406,     0,   407,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,     0,     0,     0,   420,
    2127,     0,    78,   421,     0,     0,     0,     0,     0,   422,
      80,    81,   423,   424,   425,   426,     0,     0,     0,     0,
       0,     0,     0,   409,   410,     0,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,   404,   418,   419,   405,     0,
     406,     0,   407,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
       0,     0,     0,     0,     0,   420,  2129,     0,    78,   421,
       0,     0,     0,     0,     0,   422,    80,    81,   423,   424,
     425,   426,     0,     0,     0,     0,     0,     0,     0,   409,
     410,     0,   411,   412,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   413,   414,   401,     0,   415,   416,
     417,   404,   418,   419,   405,     0,   406,     0,   407,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,     0,     0,     0,     0,
       0,   420,  2131,     0,    78,   421,     0,     0,     0,     0,
       0,   422,    80,    81,   423,   424,   425,   426,     0,     0,
       0,     0,     0,     0,     0,   409,   410,     0,   411,   412,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     413,   414,   401,     0,   415,   416,   417,   404,   418,   419,
     405,     0,   406,     0,   407,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,     0,     0,     0,     0,     0,   420,  2152,     0,
      78,   421,     0,     0,     0,     0,     0,   422,    80,    81,
     423,   424,   425,   426,     0,     0,     0,     0,     0,     0,
       0,   409,   410,     0,   411,   412,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,     0,
     415,   416,   417,   404,   418,   419,   405,     0,   406,     0,
     407,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,     0,     0,
       0,     0,     0,   420,  2154,     0,    78,   421,     0,     0,
       0,     0,     0,   422,    80,    81,   423,   424,   425,   426,
       0,     0,     0,     0,     0,     0,     0,   409,   410,     0,
     411,   412,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   413,   414,   401,     0,   415,   416,   417,   404,
     418,   419,   405,     0,   406,     0,   407,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,     0,     0,     0,   420,
    2156,     0,    78,   421,     0,     0,     0,     0,     0,   422,
      80,    81,   423,   424,   425,   426,     0,     0,     0,     0,
       0,     0,     0,   409,   410,     0,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,   404,   418,   419,   405,     0,
     406,     0,   407,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   408,
       0,     0,     0,     0,     0,   420,     0,     0,    78,   421,
       0,     0,     0,     0,     0,   422,    80,    81,   423,   424,
     425,   426,     0,     0,     0,     0,     0,     0,     0,   409,
     410,     0,   411,   412,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   413,   414,   401,     0,   415,   416,
     417,   404,   418,   419,   405,     0,   406,     0,   407,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   408,     0,     0,     0,     0,
       0,   709,     0,     0,    78,   421,     0,     0,     0,     0,
       0,   422,    80,    81,   423,   424,   425,   426,     0,     0,
       0,     0,     0,     0,     0,   409,   410,     0,   411,   412,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     413,   414,   401,     0,   415,   416,   417,   404,   418,   419,
     405,     0,   406,     0,   407,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   408,     0,     0,     0,     0,     0,   715,     0,     0,
      78,   421,     0,     0,     0,     0,     0,   422,    80,    81,
     423,   424,   425,   426,     0,     0,     0,     0,     0,     0,
       0,   409,   410,     0,   411,   412,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   413,   414,   401,     0,
     415,   416,   417,   404,   418,   419,   405,     0,   406,     0,
     407,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   408,     0,     0,
       0,     0,     0,   724,     0,     0,    78,   421,     0,     0,
       0,     0,     0,   422,    80,    81,   423,   424,   425,   426,
       0,     0,     0,     0,     0,     0,     0,   409,   410,     0,
     411,   412,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   413,   414,   401,     0,   415,   416,   417,   404,
     418,   419,   405,     0,   406,     0,   407,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   408,     0,     0,     0,     0,     0,   420,
       0,     0,    78,   421,     0,     0,     0,     0,     0,   422,
     940,    81,   423,   424,   425,   426,     0,     0,     0,     0,
       0,     0,     0,   409,   410,     0,   411,   412,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   413,   414,
     401,     0,   415,   416,   417,     0,   418,   419,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   420,     0,     0,    78,   421,
       0,     0,     0,     0,     0,   422,   486,    81,   423,   424,
     425,   426
};

static const yytype_int16 yycheck[] =
{
       1,    76,   185,     4,    76,   232,   207,    76,   168,   279,
     168,   420,   263,   237,   753,   143,   180,   756,    85,  1260,
     952,     4,     1,   223,   170,   360,   526,    76,   237,    59,
       1,   258,   679,    78,   207,  1241,   834,   420,   170,   237,
     515,   666,  1292,  1293,   168,    98,   847,  1046,   849,  1934,
     666,   237,     1,     1,  1354,    56,    57,     0,    59,  1197,
     666,     1,    93,   422,  1807,   352,  1807,    75,  1807,     1,
     237,    75,     4,   937,    72,    76,  1075,    76,   206,   937,
      59,   101,   800,  1363,    85,   152,    76,   135,    59,   925,
     670,  1224,    93,   834,   984,     1,   194,    98,    90,   734,
     101,   835,   992,   327,   105,   392,   832,   841,   237,   945,
      59,    59,   358,   832,   161,   832,  1701,     0,   327,   149,
     585,   196,   105,   832,   196,   237,   247,    59,   374,   327,
      89,   596,   378,   180,   237,   155,    85,   237,   136,   100,
     135,   327,   149,   144,   125,  1144,   147,   196,   149,    98,
     237,   120,   101,    59,   155,   276,   105,   105,   493,    89,
     327,   162,   237,   155,   171,   237,   287,    98,   237,   170,
     149,  1241,   180,   105,   137,   640,   180,   158,   149,   227,
     137,   182,  2067,   178,     0,   238,    61,    62,   237,   108,
     109,   139,   374,   194,   195,   196,   378,   196,   327,   266,
     149,   149,   161,    84,   163,   206,   196,   832,   171,   240,
     211,   242,  1206,    76,   171,   327,   832,   149,   249,   180,
     221,  1808,   162,  1938,   327,   226,   832,   327,    77,    78,
     231,   232,   233,   163,   358,   381,   237,   238,   237,   158,
     327,   487,   388,   149,  1194,   139,   195,   237,    10,   381,
     155,  1201,   327,   246,   834,   327,   301,   258,   327,  1206,
     253,   314,   211,   311,    98,   266,  1851,   268,     1,   336,
     155,   861,   185,    85,   155,   328,   277,   278,   327,   614,
     281,    75,   275,  1583,  1584,  1585,   556,   288,   163,   238,
      20,   322,   323,   286,   564,  1575,    90,  1036,   396,   247,
     709,   302,   303,   712,   305,   180,   715,   238,   207,   310,
     534,   155,  1110,   314,   163,   724,   155,   266,   727,   728,
     729,   521,    10,   658,   163,   534,   327,   328,   276,   572,
     573,  1281,   281,   196,    72,  1922,   534,   338,     1,   287,
     152,     4,   854,   570,   856,   346,   347,   682,   534,   576,
     351,  1215,   711,   247,   689,   483,  2099,  1215,  2099,   491,
    2099,   873,  1198,   487,   312,   314,  1084,   534,   316,  1110,
    1104,   754,   420,   155,   237,   120,   504,  1103,   579,   328,
     381,   193,   276,   314,  1103,  1249,  1103,   454,  1023,   390,
     970,  1249,   393,   287,  1103,   396,    59,   328,   136,    85,
     990,    72,  2117,   165,   238,   534,   579,   157,   170,   655,
     634,   161,    98,   155,   159,   101,   112,   155,   312,   105,
    1558,   159,   534,     1,  1314,   634,     4,   112,   161,   162,
     942,   534,    72,   679,   534,  2150,   634,   179,   101,   135,
    1510,   157,   105,  1513,  1514,   161,   692,   534,   634,   949,
     135,  1445,  1446,  1447,   266,   656,  2043,   922,   701,   534,
       1,  1697,   534,   160,   465,   136,   154,   634,   513,   161,
     713,  1649,   157,   655,   595,   155,  1654,   165,  1103,   171,
     314,    59,   170,   656,     1,   534,   149,  1103,   159,   490,
     491,  2070,  1442,   546,   328,   155,   136,  1103,  1445,  1446,
    1447,   502,   503,   159,   111,   634,   155,    85,   164,   195,
     692,   559,   513,  2100,   515,   155,   465,  2096,    59,   567,
      77,    78,   634,    75,   336,   211,   152,   134,   155,  1735,
    1110,   634,   180,   534,   634,   534,   163,   163,   586,    91,
    2119,   161,    59,   170,   534,   546,   358,   634,  1206,   597,
    2137,   697,   238,   179,     3,   679,   161,   588,   110,   634,
     180,   139,   634,  1696,   807,   143,   171,   560,  1701,   570,
    1304,   149,   161,     1,   152,   576,     4,   578,   288,  1578,
     266,   827,   783,   636,   672,   634,    58,  1099,  1100,    61,
      62,   180,    64,   303,   587,   281,  1397,   546,   139,  1400,
    1401,   594,   163,  1078,   163,   598,   163,   638,   149,   639,
     783,   170,  1470,   108,   109,   546,  1474,  1475,   281,   180,
    1856,  1857,   139,   390,   959,   161,   393,   828,   206,   161,
    1488,    59,   149,   634,  1884,   636,   184,   157,   639,   161,
     641,   161,   454,  1155,   180,   827,   847,   595,    76,   650,
     161,  1298,  1386,   654,   855,   828,    72,    85,   180,  1549,
     639,   709,   845,   863,   137,  1735,   180,   715,   639,   247,
      98,   534,   899,   101,   154,   487,   724,   105,   161,   163,
     579,   161,   161,   155,   161,   686,   264,   636,   266,   849,
     639,   639,   171,   166,   167,   743,   180,   855,   276,   161,
     180,   595,   834,   180,   705,   636,   247,   639,   155,   287,
     157,   957,   546,   161,   526,  1951,  1952,   161,  1851,   161,
     136,   149,  1312,   171,  1463,   161,   161,   155,   163,  1319,
     247,   855,   310,   639,   312,   276,   180,   782,   180,   155,
    1981,   134,   170,   159,   180,   746,   287,   748,   836,   750,
     161,    69,   840,   754,     1,    75,   757,   656,   336,   276,
     572,   573,   155,   851,   852,   161,   159,   195,   196,   180,
     287,   312,    92,   166,   167,   957,    72,     3,   206,   465,
    1370,   782,  1781,   211,  1783,   158,     3,  1445,  1446,  1447,
     130,   131,   134,   221,   163,   312,    13,    14,    15,    16,
      17,   170,   636,   231,   232,   233,   149,   150,   151,   237,
     238,  1743,    59,   155,   155,  1948,   163,   159,  1019,    13,
      14,    15,    16,    17,   166,   167,   827,   161,   171,  1962,
     258,   832,  1241,   834,   161,   175,   176,     3,   266,   985,
     136,   157,   161,   159,   171,   846,   180,    13,    14,    15,
      16,    17,   171,   281,    72,    72,   857,   905,  1241,   155,
     546,  1019,   863,   159,   161,   866,   179,   679,  1758,    72,
    1345,   919,   163,  1143,   302,   923,   454,   305,    72,   927,
     166,   157,   310,   180,   783,   161,   314,   173,   174,   701,
    1018,   155,   139,   177,  2027,  1019,   157,   709,   899,   327,
     328,   713,   149,   715,   155,   483,    72,   168,   169,    13,
      14,    15,    16,    17,   154,    72,   120,   155,   136,   136,
     156,   161,   157,  1332,   162,   159,   504,   163,   157,   828,
     164,   924,   845,   136,   935,   936,   937,   155,   154,    72,
      72,   159,   136,   137,   157,   161,   157,   860,   847,   162,
     849,  1151,   155,   381,   853,   854,   159,   856,   937,   157,
    1550,   149,   150,   151,    72,   948,   937,  1302,    72,   160,
     136,   157,    72,   161,   873,   161,   639,  1109,  1110,   136,
    1560,   529,  1572,   171,   157,  1485,   157,   157,   161,   937,
     991,   162,   180,    72,   157,   807,  1854,   157,   155,   162,
     247,   157,   159,   136,   136,   937,   162,  1482,   155,    13,
      14,    15,    16,    17,   157,   155,   948,   595,    72,   162,
     158,   159,   155,   155,   162,   161,   159,   159,   136,   276,
     155,   937,   136,   137,   933,    22,   136,   465,   155,  1127,
     287,  1042,   155,   942,   157,  1046,   159,   155,  1291,   155,
      72,   159,  1298,    72,   595,   155,  1307,   136,  1285,   159,
     155,   639,   157,   491,   159,   312,    58,  1450,    72,    61,
      62,   155,    64,   157,  1075,   159,   155,  1078,   595,   155,
     159,   157,   136,   159,   106,   513,   161,   515,   110,   111,
     112,   113,   114,   115,   116,   117,  1571,    72,   639,   155,
      72,   155,  1103,   159,   101,   159,   534,    72,  1109,  1110,
     108,   109,   134,   155,   136,   656,  1706,   136,   546,   157,
     157,   157,   639,   161,   161,   161,  1327,   157,  1292,  1293,
     155,   161,   136,   155,   156,   163,   155,   949,    89,  1206,
     159,   154,   570,  1144,   166,   167,   160,   155,   576,  1180,
     578,   159,  1200,  2011,    47,    48,  1656,    50,  1071,    72,
     163,   136,    55,  1433,   136,  1311,  1214,   163,  1326,  1327,
    1760,   136,   157,   158,  1298,   113,   114,   115,   116,   117,
     155,   180,  2040,   155,   159,  1775,  1234,   159,   157,   155,
     155,   155,   161,  1241,   159,   159,   149,   150,   151,  1400,
    1099,  1100,   160,  1327,   163,   155,   634,    72,   636,   159,
       1,   639,   157,  2071,  1197,   157,   161,  1218,   171,   161,
    1221,  1222,  1223,   136,  1407,  1408,   654,   180,   134,  1230,
     155,   157,   179,   781,   159,   161,  1215,  1397,  1977,   157,
    1153,  1401,   155,  1403,  1215,  1403,   159,   157,  1249,   155,
     170,   161,   155,   159,  1255,   157,  1155,   157,   686,   161,
     166,   167,   157,   168,   169,  1197,   161,  1215,    59,  1270,
    1249,   136,  1273,  1274,   937,  1276,   157,   120,  1249,  1403,
     161,  1282,   157,  1215,  1285,   948,   161,   828,   157,  1764,
     155,  1274,   161,   155,   159,    13,    14,    15,    16,    17,
      18,  1249,   157,   157,  1894,   155,   161,   161,  1898,  1215,
     101,    13,    14,    15,    16,    17,    18,  1249,   746,   157,
     748,   155,   750,   161,  1273,  1274,  1274,  1328,   155,   757,
     128,   129,  1533,     4,     5,     6,     7,     8,     9,    10,
      11,    12,  1274,  1249,  1345,   157,    13,   157,   595,   161,
     161,   161,   172,  1354,   782,   177,   134,   165,   149,   937,
     149,   150,   151,   157,  1773,   157,   167,   161,   134,   161,
     948,  1554,   161,   160,   161,  1533,   158,   155,  1445,  1446,
    1447,   159,   171,  1450,  1451,   157,  1387,   157,   166,   167,
     157,   180,   639,   157,    65,   157,   937,   161,   157,   827,
     157,   179,   161,  1456,   832,   157,   834,   132,   133,  1533,
      13,    14,    15,    16,    17,    18,  1464,  1465,  2104,   159,
     937,    88,  2108,   106,   160,   161,     1,   110,   111,   112,
     113,   114,   115,   116,   117,   863,  1660,   160,   866,   106,
    1018,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,  1660,  1470,  1366,   155,  1456,  1474,   160,   161,  1460,
    1461,   155,  1660,   160,   161,  1448,   161,  1515,   761,   762,
     763,   899,   162,   156,  1660,   137,   159,   166,   167,  1291,
     137,  1482,   160,   161,    59,   161,  1298,   162,   155,   161,
     281,   160,   161,  1660,  1407,  1408,   161,   162,  1397,  1470,
     155,  1400,  1401,  1474,  1475,  1506,  1507,   935,   157,   123,
     157,   125,   126,   127,   157,  1516,  1448,  1488,   768,   769,
     770,   771,  1470,   179,   315,  1456,  1474,  1475,   160,   161,
     105,  1660,   160,   161,  1197,  1646,  1647,  1648,  1470,   157,
    1488,   155,  1474,  1475,   158,   159,    91,    92,  1660,   163,
     164,  1552,  1215,  1203,  1204,  1205,  1488,  1660,   160,   161,
    1660,  1667,  1668,  1669,   139,   160,   161,  1516,   179,   360,
    1571,   160,   161,  1660,   149,  1558,  1470,  1578,   160,   161,
    1474,   157,  1583,  1584,  1585,   157,  1249,  1273,  1274,   160,
     161,  1660,   157,   168,   160,   161,  1649,   157,  1822,   160,
     161,  1654,    13,    14,    15,    16,    17,    18,   408,  1662,
    1273,  1274,   160,  1822,  1042,   157,   160,   161,  1046,  1197,
     160,   161,  1456,   159,  1822,   163,  1558,   157,  1206,   160,
     161,   163,   207,   433,   434,   163,  1822,  1215,   161,   162,
     161,  1554,   160,   161,   160,   161,   163,  1075,  1649,   163,
    1078,    77,    78,  1654,   454,  1822,   161,   162,    70,  1660,
    1887,  1662,  1355,  1356,   160,   764,   765,   180,  1716,  1670,
     155,  1249,   247,  1485,  1215,  1103,   766,   767,   772,   773,
      78,  1109,  1110,  1513,  1514,   160,    18,   487,  1689,   179,
     937,  1668,  1669,  1822,  1695,   163,   163,   180,  1215,   157,
     157,   276,   493,  1686,   163,   160,   180,   163,  1249,  2044,
    1822,   160,   287,    18,   160,   290,  1144,   160,  1649,  1822,
    1884,   154,  1822,  1654,  1970,  1726,   157,   157,   157,   157,
     305,  1662,  1249,   157,   157,  1822,   157,   312,  1686,   106,
     157,   316,   157,   110,   111,   112,   113,   114,   115,   116,
     117,  1934,   157,    22,  1686,   157,   157,   157,   154,   154,
     106,   160,  1745,  1764,   110,   111,   112,   113,   114,   115,
     116,   117,  1825,   163,   163,   163,    70,   157,   157,   157,
    1781,   157,  1783,   358,   157,  1448,   361,   179,   155,   156,
     161,   163,   157,   163,   157,  2041,   157,   161,   160,   374,
     157,   161,   157,   378,   157,   161,   157,  1470,   157,   157,
     157,  1474,  1475,  1745,   157,  1649,  2043,   157,   160,   160,
    1654,  1822,   157,   614,  1825,  1488,   157,   157,  1662,   157,
    1516,   157,   157,  1834,  1835,   157,   627,   157,   157,   157,
    1841,   157,  1270,   160,  1656,  1273,  1274,   161,   639,   157,
     161,   161,  1853,  1516,   161,   154,   154,  1285,   161,   155,
     155,    14,  1863,   155,  1865,   155,   155,  1445,  1446,  1447,
    1448,  1449,  1450,  1451,   155,  2099,   155,  1878,  1931,  1880,
    1881,  1882,  2065,  1854,  2067,  1886,  1887,   155,   162,   162,
    2099,   682,  1470,   180,  1825,  1558,  1474,  1475,   161,   160,
    1328,  2099,   160,  1970,   180,   180,  1854,   163,   154,   179,
    1488,   154,   487,  2099,   163,   180,   161,  1345,   157,   157,
     157,   157,  1854,  2106,   157,   160,  1354,   157,   160,  1470,
    1931,   157,  2099,  1474,  1475,     1,   161,  1938,     4,   160,
     157,  1942,   157,   157,   157,   154,  1947,  1488,  1949,   160,
     154,    80,   155,  1470,   155,    92,   180,  1474,  1475,  1387,
     760,   180,   180,   154,   180,   155,   155,    90,  1215,   157,
    2099,  1488,   180,   180,  2041,   180,   154,   154,   161,   161,
    1558,   160,   163,   154,   160,   160,   561,  2099,   160,   154,
     157,  1825,   162,    59,  2177,   123,  2099,   154,   162,  2099,
    1931,  2002,  1249,   157,   579,   157,   157,   157,   155,   157,
      76,   160,  2099,  2014,   160,   157,   157,  2018,   154,    85,
     595,  1934,  1685,  1686,  2099,   180,   157,  2099,  1456,   154,
    2099,  2032,    98,   157,   161,   101,   162,   155,   155,   105,
    2011,   160,  2043,   157,  2045,   160,   154,  2100,   160,   154,
    2099,   157,    75,   157,  1482,   157,   157,   160,    75,   180,
     180,   154,   637,  2011,   639,   155,   157,   180,   155,  2040,
     154,   160,   160,  2074,   154,   154,   157,   143,    75,  2011,
     655,   656,  1745,   149,  2137,   157,   152,   157,  1516,   155,
     156,   666,  2040,    75,   159,   171,   158,  1931,  2099,  2100,
    2071,    75,   168,   180,   679,   180,   162,   171,  2040,   180,
     154,  2112,   154,  2161,   154,   156,  2117,   692,   171,    18,
     171,   154,   106,  2071,   162,  2173,   155,   193,   171,   195,
     196,   161,   171,   180,   156,    62,  2137,    75,   157,  2071,
     206,   207,   160,  1571,  2145,   211,   937,  2148,   180,  2150,
    1578,  2100,  2065,   156,  2067,  1583,  1584,  1585,    57,    58,
      59,    60,    61,    62,    63,    64,   232,  1745,   959,  2100,
    2171,   237,   238,   162,  1837,   180,  1724,   157,   157,   106,
     157,  2182,   154,   154,   111,   155,   180,   114,  2137,   116,
    2191,  1854,   258,  2106,   157,   180,  1773,  1323,   733,   776,
     266,   774,  1236,   777,  1249,   775,  2137,  1474,   783,   778,
    2067,   453,  2150,  1488,  1014,   281,  1862,  2096,  1854,  1955,
    1020,  1649,  2135,  1470,   799,  2056,  1654,  1474,  1475,  1734,
    2134,  1031,  1660,  2041,  1662,  1717,  1717,  2109,  2166,  2040,
     815,  1488,    49,  1276,   819,   114,   272,  1931,   314,  2000,
    1451,  1316,   827,   828,   320,  1763,   863,   832,  1270,   650,
     326,   327,   328,   522,  2177,  1522,  2100,  1745,     0,   705,
     336,  1638,   847,   799,   849,   799,  1854,   799,   853,   854,
     855,   856,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,   358,   359,   360,    -1,    -1,    -1,   873,    -1,
      -1,    -1,    -1,  2137,    -1,    -1,    -1,    -1,   374,    -1,
     106,    -1,   378,  1854,   110,   111,   112,   113,   114,   115,
     116,   117,   118,  1871,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,  1764,  1854,    -1,    -1,
      -1,    -1,    -1,   270,    -1,    -1,    -1,    -1,  2011,    -1,
      -1,    -1,    -1,  1781,   420,  1783,    -1,    -1,   933,    -1,
      -1,    -1,   937,   159,   106,    -1,   108,   942,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,  2040,    -1,    -1,
      -1,    -1,   957,    -1,    72,   960,    -1,    -1,   454,    -1,
     317,   457,    -1,    -1,  1822,    -1,    -1,  1825,    -1,   465,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2071,    -1,
      -1,    -1,    -1,    -1,    -1,  1215,    -1,   483,    -1,    -1,
      -1,   487,    -1,    -1,  1215,   491,  1974,   493,    -1,    -1,
      -1,    -1,    -1,  2011,    -1,   362,    -1,   364,   504,   366,
      -1,    -1,    -1,    -1,  1019,   106,   134,    -1,   136,   110,
     111,   112,   113,   114,   115,   116,   117,   118,  1249,  1887,
     526,   122,  2040,   124,    -1,    -1,    -1,   155,   534,    -1,
    2011,   159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
     546,    -1,  1273,    -1,   411,    13,    14,    15,    16,    17,
      -1,    -1,    -1,  2071,  2011,   156,    -1,    -1,  1298,  2040,
      -1,    -1,    -1,  1931,   570,    -1,   572,   573,    -1,    -1,
     576,  1302,    -1,   579,    -1,  1090,    -1,    -1,    -1,  1094,
      -1,    -1,    -1,  2040,  1099,  1100,    -1,    -1,  1103,    -1,
    2071,    -1,    -1,    -1,    -1,  1335,  1336,  1337,  1113,    -1,
      -1,    -1,  1342,  1343,    72,  1120,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2071,    -1,  2104,    -1,    -1,    -1,
    2108,  2109,    -1,    -1,    -1,    -1,    -1,    -1,   634,    -1,
     636,    -1,    -1,   639,    -1,    -1,    -1,    -1,    -1,    -1,
    1155,    -1,    -1,    -1,    -1,   512,    -1,    -1,    -1,   655,
     656,  2139,   658,  1168,    -1,    -1,    -1,  1172,    -1,    -1,
     666,  1176,    -1,    -1,   670,    -1,   134,  1854,   136,    -1,
      -1,    -1,    -1,   679,  2162,  2043,    -1,  2045,  2166,    -1,
      -1,    -1,    -1,   689,    -1,    -1,   692,   155,    -1,    -1,
      -1,   159,    -1,  2181,    -1,   701,    -1,    -1,   166,   167,
    1215,    -1,    -1,   709,    -1,    -1,   712,   713,    -1,   715,
      -1,    -1,   248,    -1,    -1,    -1,    -1,    -1,   724,    -1,
      -1,   727,   728,   729,    -1,   592,    -1,    -1,    -1,    -1,
      -1,  2099,  2100,    -1,  1249,    -1,    -1,    -1,    -1,  1470,
      -1,    -1,    -1,  1474,  1475,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,  1488,    -1,  1274,
      13,    14,    15,    16,    17,     3,    -1,    -1,    -1,  2137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   783,    -1,    -1,
      -1,    -1,    -1,  1298,    -1,  1516,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   799,   800,    -1,    -1,    -1,    -1,    -1,
      -1,   807,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      59,  1326,  1327,    -1,  2182,    -1,    -1,    -1,    -1,    72,
      -1,   827,   828,  2191,  2011,    -1,   832,    -1,   834,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,   847,    -1,   849,    -1,    -1,    -1,   853,   854,   855,
     856,    -1,    -1,  2040,    -1,   106,   105,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   873,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   134,  1397,   136,  2071,  1400,  1401,    -1,  1403,    -1,
     139,    -1,    -1,   899,   143,    -1,   134,    -1,    -1,    -1,
     149,    -1,   155,  1418,    -1,    -1,   159,  1422,    -1,    -1,
      -1,  1426,    -1,   166,   167,   166,    -1,   155,   156,   168,
      -1,   457,    -1,    -1,   162,    -1,    -1,    -1,   166,   167,
      -1,   937,    -1,    -1,    -1,    -1,   942,    -1,   474,    -1,
      -1,   477,   948,   949,    78,    -1,   195,    -1,    -1,    -1,
      -1,   957,    -1,   959,  1685,  1470,    -1,   206,   207,  1474,
    1475,    -1,    -1,    -1,   970,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,  1488,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,    -1,    -1,    -1,    -1,   238,
    1730,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   247,    -1,
      -1,    -1,    -1,   539,    -1,    -1,    -1,    -1,    -1,   258,
      -1,    -1,  1018,  1019,   263,   264,    -1,   266,  1533,    -1,
      -1,    -1,    -1,  1538,    -1,    56,    57,   276,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   287,    -1,
      -1,   290,    -1,    -1,    -1,   294,   180,    -1,    -1,    -1,
     299,    -1,    -1,    -1,    -1,    -1,   305,    -1,    -1,    -1,
      -1,   310,    93,   312,   106,    -1,    -1,   316,   110,   111,
     112,   113,   114,   115,   116,   117,   118,  1592,  1084,   328,
     122,    -1,   124,    -1,    -1,    -1,  1601,    -1,    -1,    18,
    1605,    -1,    -1,  1099,  1100,    -1,    -1,  1103,    -1,    -1,
      -1,    -1,    -1,    -1,  1110,    -1,  1837,    -1,    -1,   358,
      -1,    -1,   361,   144,   156,    -1,   147,   159,    -1,    -1,
      -1,    -1,    -1,  1854,    -1,   374,    -1,    -1,    -1,   378,
      -1,   162,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,  1155,
      -1,   182,    -1,    -1,    -1,    -1,    -1,   305,    -1,    -1,
      -1,    -1,    62,   194,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1686,    -1,    -1,    -1,    -1,    -1,   106,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,  1197,    -1,    -1,    -1,   226,    -1,    -1,    -1,    -1,
    1206,   101,    13,    14,    15,    16,    17,    -1,    -1,  1215,
      -1,    -1,    -1,   113,    -1,   115,    -1,   117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   221,    -1,    -1,
     159,    -1,    -1,    -1,   483,  1241,  1976,   268,   487,    -1,
      -1,    -1,    -1,  1249,    -1,    -1,   277,   278,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   504,   156,   288,    -1,   159,
     160,    72,    -1,   799,   800,    -1,    -1,  1273,  1274,    -1,
      -1,    -1,   303,    13,   810,    -1,    -1,   813,    -1,  1285,
    2011,    -1,    -1,    -1,    -1,  1291,    -1,    -1,    -1,    -1,
      -1,    -1,  1298,    -1,   358,   106,    -1,   361,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,   338,    -1,  2040,
     374,   211,   561,    -1,   378,   346,   347,    -1,    -1,    -1,
     351,  1327,    -1,   134,    -1,   136,  1332,    -1,    -1,    -1,
     579,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1854,
    2071,    -1,    -1,   879,   155,   156,   595,    -1,    88,    -1,
     886,    -1,    -1,    -1,   890,   166,   167,    -1,   894,   390,
      -1,    -1,   393,    -1,   305,   396,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,   281,    -1,   283,   284,    -1,    -1,    -1,   637,    -1,
     639,  1397,    -1,    -1,  1400,  1401,    -1,  1403,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   655,   656,    -1,    -1,
      -1,    -1,    -1,   561,    -1,   315,    -1,   666,    -1,    -1,
     320,   670,    -1,   487,    -1,    -1,   326,    -1,    -1,    -1,
     679,    -1,    -1,    -1,    -1,   684,    -1,    -1,    -1,  1445,
    1446,  1447,  1448,   692,  1450,  1451,    -1,    -1,    -1,    -1,
    1456,  1457,    -1,    -1,    -1,  1970,    -1,    -1,    -1,   490,
     360,    -1,    -1,    -1,  1470,   365,    -1,   367,  1474,  1475,
      -1,   502,   503,    -1,    -1,    -1,    -1,    -1,    -1,  1485,
      -1,    -1,  1488,    -1,    -1,    -1,    -1,    -1,    -1,   637,
      -1,    -1,    -1,    -1,    -1,   106,  2011,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
    1516,   411,    -1,    -1,    -1,   579,    -1,    -1,    -1,   513,
      -1,   515,    -1,    -1,    -1,  2040,  2041,  1533,    -1,    -1,
      -1,   106,    -1,    -1,   783,   110,   111,   112,   113,   114,
     115,   116,   117,   118,  2059,    -1,    -1,   122,  1084,   124,
     799,   800,  1558,    -1,  1560,    -1,  2071,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   465,   815,    -1,    -1,   180,
     819,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   827,   828,
      -1,   156,    -1,   832,   159,   834,    -1,    -1,    -1,    -1,
      -1,   655,   656,   493,    -1,   495,   496,    -1,   847,    -1,
     849,    -1,    -1,    -1,   853,   854,   855,   856,    -1,    -1,
     641,    -1,   512,    -1,    -1,   679,    -1,    -1,    -1,   650,
     561,    72,    -1,  1159,   873,    -1,  1162,    -1,   692,    -1,
    1166,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,  1649,  1650,    -1,   546,    -1,  1654,    -1,
    1656,   799,    -1,    -1,  1660,   106,  1662,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   815,    -1,    -1,
     570,   819,    -1,    -1,   705,   575,    -1,   577,    -1,  1685,
    1686,    -1,    -1,   134,   933,   136,    -1,    -1,   937,    -1,
      -1,    -1,    -1,   942,    -1,   170,   637,    -1,    -1,   948,
     600,    -1,   602,   603,   155,   156,    -1,    -1,   957,    72,
      -1,   960,    -1,    -1,   614,   166,   167,    -1,   967,   783,
      -1,    -1,    -1,   754,    -1,   666,    -1,   627,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   636,    -1,    -1,  1745,
      -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,    -1,   658,    -1,
     660,   661,    -1,   827,   828,    -1,    -1,  1773,    89,  1018,
    1019,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   682,   683,    -1,    -1,    -1,    -1,    -1,   689,
      -1,   855,   155,   156,    -1,    -1,   159,    -1,    -1,    -1,
      -1,  1807,  1808,   166,   167,    -1,    -1,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,   846,  1822,    -1,    -1,  1825,
      -1,    -1,    -1,    -1,    -1,    -1,   857,    -1,    -1,    -1,
      -1,  1837,    -1,    -1,    -1,    -1,  1372,    -1,    -1,    -1,
      -1,  1090,    -1,    -1,    -1,  1094,    -1,  1383,  1854,    -1,
    1099,  1100,    -1,    -1,  1103,    -1,    -1,    -1,   799,    -1,
      -1,    -1,    -1,    -1,  1113,    -1,    -1,    -1,    -1,   863,
      -1,  1120,   866,    -1,   815,    -1,    -1,    57,   819,    -1,
      -1,  1887,    -1,    -1,    -1,    65,    66,    67,    68,    76,
      -1,   832,    -1,   957,    -1,    -1,   960,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   936,  1155,    -1,    -1,    -1,
      -1,    98,    -1,    -1,    -1,    -1,  1922,    -1,    -1,  1168,
      -1,    -1,    -1,  1172,    -1,  1931,   106,  1176,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,    -1,
      -1,    -1,  1090,    -1,    -1,    -1,  1094,    -1,  1197,    -1,
      -1,    -1,    -1,    -1,    -1,  1019,    -1,  1206,    -1,    -1,
     991,    -1,    -1,    -1,  1970,  1113,  1215,    -1,   155,    -1,
      -1,    -1,  1120,    -1,    -1,    -1,    -1,    -1,    -1,   159,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   933,    -1,  2000,    -1,    -1,   177,    -1,    -1,
    1249,    -1,    -1,   324,   106,  2011,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,    -1,    -1,    -1,
    1168,    -1,    -1,    -1,  1172,  1274,    -1,    -1,  1176,    -1,
      -1,    -1,    -1,    -1,  2040,  2041,    -1,  2043,  2044,    -1,
      -1,    -1,    -1,    -1,    -1,   232,    -1,    -1,  1042,  1298,
     237,   238,  1046,    -1,   180,    -1,   158,    -1,  1307,   959,
      -1,    -1,    -1,    -1,    -1,  2071,    -1,    -1,   170,    -1,
     970,   258,    -1,    -1,    -1,    -1,    -1,  1326,  1327,   979,
      -1,  1075,    -1,    -1,  1078,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,  2099,  2100,    -1,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   185,    -1,    -1,   122,    -1,
     124,    -1,    -1,   106,  1650,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,   314,    -1,    -1,
      -1,  2137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     327,   328,   156,    -1,    -1,   159,    -1,    -1,  1397,  1090,
    1144,  1400,  1401,  1094,  1403,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1103,    -1,    -1,   486,    -1,   488,    -1,  1418,
     163,    -1,  1113,  1422,    -1,     1,   497,  1426,     4,  1120,
      -1,    -1,    -1,    -1,  1084,    -1,    -1,    -1,    -1,    -1,
    1221,  1222,  1223,    -1,    -1,    -1,  1445,  1446,  1447,  1448,
    1449,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
    1110,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,  1470,    -1,    -1,  1255,  1474,  1475,  1168,    -1,    -1,
      -1,  1172,    -1,    59,  1298,  1176,    -1,    -1,    -1,  1488,
      -1,    -1,    -1,    -1,    -1,  1276,    -1,    -1,    -1,    -1,
      -1,  1282,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      -1,    -1,   159,  1327,    -1,    -1,    -1,    -1,    -1,    -1,
    1418,  1807,  1808,    -1,  1422,   101,    -1,    -1,  1426,   105,
      -1,    -1,    -1,    -1,  1533,    -1,    -1,    -1,   106,  1538,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,    -1,    -1,    -1,   491,    -1,    -1,    -1,    -1,  1558,
      -1,    -1,    -1,   139,    -1,    -1,    -1,   143,    -1,    -1,
      -1,    -1,    -1,   149,   404,    -1,   152,    -1,   408,   409,
     156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   418,   419,
     158,   167,   168,  1592,   170,    -1,    -1,   534,    -1,    -1,
      -1,  1345,  1601,   433,   434,    -1,  1605,    -1,    -1,   546,
    1354,    -1,    -1,    -1,    -1,    -1,    -1,   193,    -1,    -1,
      -1,    -1,    -1,  1273,   454,    -1,    -1,    -1,    -1,    -1,
     206,   207,    -1,   570,    -1,   211,  1922,    -1,    -1,   576,
    1538,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,  1302,    -1,    -1,   152,    -1,   487,  1308,    -1,
      -1,    -1,    -1,  1662,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   247,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1460,
    1461,    -1,   179,    -1,    -1,    -1,    -1,  1686,   264,    -1,
     266,    -1,    -1,    -1,  1592,    -1,    -1,   634,    -1,   636,
     276,    -1,    -1,  1601,    -1,   281,    -1,  1605,    -1,    -1,
    1996,   287,    -1,    -1,  2000,    -1,    -1,    -1,    -1,  1533,
      -1,    -1,    -1,    -1,    -1,    -1,   302,  1418,    -1,   305,
      -1,  1422,    -1,    -1,   310,  1426,   312,    -1,  1482,   315,
     316,    -1,    -1,    -1,   320,    -1,  1745,    -1,    -1,    -1,
     326,    -1,    72,    -1,    -1,    -1,    -1,  2043,    -1,   106,
     336,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,  1552,    -1,    -1,   845,    -1,    -1,    -1,    -1,    -1,
      -1,    72,   358,    -1,   360,   361,   106,    -1,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   374,    -1,
      -1,    -1,   378,    -1,    -1,    -1,    -1,  1457,    -1,  1808,
      -1,   158,    -1,  2099,  2100,   106,   136,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,  1571,    -1,    -1,
      -1,    -1,    -1,    -1,  1578,   155,   156,    -1,    -1,  1583,
    1584,  1585,    -1,   134,   420,   136,    -1,  1538,    -1,    -1,
      -1,  2137,    -1,    -1,    -1,  1854,    13,    14,    15,    16,
      17,    -1,    -1,    -1,   155,   156,  1516,    -1,    -1,   940,
     941,    -1,   101,    -1,    -1,   166,   167,   106,   454,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,  1670,
      -1,    -1,    -1,    -1,    -1,   832,    -1,   834,    -1,    -1,
      -1,  1592,    -1,    -1,    -1,    -1,    -1,   483,    -1,    -1,
    1601,   487,    -1,    -1,  1605,    72,   106,   493,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   504,    -1,
     760,   761,   762,   763,   764,   765,   766,   767,   768,   769,
     770,   771,   772,   773,   774,   775,   776,   777,   778,   106,
     526,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   899,    -1,  1035,    -1,   156,    -1,    -1,   159,
      -1,  1970,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,   106,   561,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   572,   573,   155,   156,
      -1,   577,   159,   579,    -1,    -1,  1077,    -1,    -1,   166,
     167,    -1,  2011,    -1,    -1,   845,    -1,    -1,    -1,   595,
    1764,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   155,    -1,  1685,    -1,  1781,   614,  1783,
      -1,  2040,  2041,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   627,    -1,    -1,    -1,  1126,    -1,  1128,    -1,    -1,
    2059,   637,    -1,   639,    -1,    -1,    -1,    -1,    -1,  1140,
     155,  1142,  2071,    -1,    -1,    -1,  1147,  1148,   654,   655,
     656,    -1,   658,    -1,    -1,    -1,  1157,    -1,    -1,   106,
     666,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,  2100,    -1,   679,    -1,  1886,   682,    -1,    -1,    -1,
     686,    -1,  1183,   689,    -1,  1186,   692,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   701,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   709,    -1,    -1,   712,   713,    -1,   715,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   724,    -1,
      -1,   727,   728,   729,    -1,    -1,  1970,    -1,    -1,    -1,
      -1,  2059,    -1,    -1,    -1,    -1,  1103,    -1,  1949,    -1,
      -1,    -1,  1109,  1110,    -1,  1825,    -1,    -1,  1249,    -1,
      -1,    -1,    -1,    -1,  1014,    -1,    -1,  1837,    -1,    -1,
    1020,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1031,    -1,    -1,    -1,    -1,    -1,   783,  1279,    -1,
      -1,    -1,    -1,    -1,    -1,  1286,    -1,  1288,  1289,    -1,
      -1,    -1,    -1,   799,    -1,    -1,  1297,  2041,  1299,    -1,
    1301,   807,    -1,    -1,    -1,    -1,    -1,  1308,    -1,   815,
      -1,  1071,     1,   819,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   827,   828,    -1,    -1,    -1,   832,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   847,  1922,   849,    -1,    -1,    -1,   853,   854,   855,
     856,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   873,    -1,    -1,
      59,    -1,    -1,    -1,     1,    -1,    -1,     4,   155,  1380,
    1381,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   902,    -1,    13,    14,
      15,    16,    17,    -1,    -1,  1406,    -1,    -1,    -1,    -1,
      -1,    -1,  1413,    -1,  1415,    -1,   105,    -1,  1285,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   933,    -1,    -1,
      -1,   937,    59,    -1,    -1,    -1,   942,    -1,  2059,    -1,
    1441,    -1,   948,   949,    -1,    -1,    -1,    -1,    -1,    -1,
     139,   957,    -1,   959,   960,  1215,    -1,    72,    -1,    -1,
     149,    -1,    -1,    -1,  2044,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   168,
      -1,   987,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1018,  1019,    -1,    -1,    -1,    -1,   207,   134,
    1387,   136,   149,    -1,    -1,    -1,    -1,    -1,    -1,   156,
      -1,    -1,    -1,    -1,    -1,    -1,  1537,    -1,  1298,    -1,
     155,   156,    -1,  1544,   159,  1546,    -1,    -1,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,  2137,   247,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1335,  1336,  1337,    -1,    -1,
     207,    -1,  1342,  1343,  1090,    -1,    -1,   276,  1094,  1456,
      -1,    -1,    -1,  1099,  1100,    -1,    -1,  1103,   287,    72,
      -1,    -1,    -1,    -1,    -1,    -1,  1366,  1113,    -1,    -1,
      -1,    -1,    -1,    -1,  1120,    -1,   305,    -1,    -1,    -1,
      -1,  1622,    -1,   312,    -1,    62,    -1,   316,    -1,    -1,
      -1,    -1,    -1,   106,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,    -1,  1407,  1408,  1155,
      -1,    -1,    -1,    -1,   281,    -1,    -1,    -1,    -1,    -1,
      -1,   134,  1168,   136,   101,    -1,  1172,    -1,    -1,   358,
    1176,    -1,   361,    -1,    -1,    -1,   113,   114,    -1,    -1,
      -1,    -1,   155,   156,    -1,   374,    -1,    -1,    -1,   378,
      -1,  1197,    -1,   166,   167,    -1,    -1,    -1,    -1,    -1,
    1206,    -1,    -1,    -1,    13,    14,    15,    16,    17,  1215,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     106,    -1,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,  1241,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1249,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,  1753,  1754,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,  1767,  1273,  1274,   155,
     156,    -1,    -1,    -1,   211,    -1,   162,    -1,    -1,    -1,
     166,   167,  1649,    -1,    -1,  1291,    -1,  1654,    -1,    -1,
      -1,    -1,  1298,  1660,  1554,  1662,  1302,   106,   487,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1326,  1327,    -1,    -1,    -1,   134,  1332,   136,    -1,    -1,
      -1,    -1,   106,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   281,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   561,    -1,    -1,    -1,    -1,    -1,   315,    -1,
      -1,   155,   156,   320,    -1,   159,    -1,    -1,    -1,   326,
     579,  1397,   166,   167,  1400,  1401,    -1,  1403,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   179,   595,    -1,    -1,    -1,
      -1,    -1,  1418,    -1,    -1,    -1,  1422,    -1,    -1,    -1,
    1426,    -1,    -1,   360,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1438,    -1,    -1,    -1,    -1,    -1,    -1,  1445,
    1446,  1447,  1448,  1449,  1450,  1451,    -1,    -1,   637,    -1,
     639,  1457,   579,    -1,    -1,  1822,    -1,    -1,  1825,    -1,
      -1,    -1,    -1,    -1,  1470,    -1,   655,   656,  1474,  1475,
    1730,    -1,    -1,    -1,   411,    -1,    -1,   666,    -1,  1485,
      -1,    -1,  1488,    -1,    -1,    -1,    -1,    -1,    -1,   106,
     679,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,   692,    -1,    -1,    -1,    -1,    -1,    -1,
    1516,    -1,   639,    -1,    -1,    -1,    -1,   134,    -1,    -1,
    1887,    -1,    -1,    -1,    -1,    -1,    -1,  1533,    -1,   656,
      -1,    -1,  1538,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,  2042,   159,    -1,    -1,  1551,    -1,    -1,    -1,   166,
     167,    -1,  1558,    -1,    -1,    -1,   493,    -1,    -1,    -1,
      -1,    -1,    -1,   106,  1931,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   512,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1592,    -1,    -1,    -1,
      -1,   134,    -1,    -1,   783,  1601,  2097,    -1,    -1,  1605,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     799,    -1,   155,   156,    -1,    -1,    -1,  2118,    -1,    -1,
      -1,    -1,    -1,   166,   167,    -1,   815,    -1,    -1,    -1,
     819,    -1,  2133,    -1,    -1,    -1,    -1,    -1,   827,   828,
     577,    -1,    -1,   832,    -1,    -1,    -1,    -1,    -1,    -1,
    1656,    -1,    -1,    -1,    -1,    -1,   783,    -1,   847,    -1,
     849,    -1,    -1,   600,   853,   854,   855,   856,    48,    -1,
      -1,    -1,    -1,    -1,  1934,    -1,  2043,   614,    -1,  1685,
    1686,    -1,    -1,    -1,   873,    -1,    -1,    -1,    -1,    -1,
     627,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,
     106,   828,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,    -1,    -1,    -1,  1976,    -1,    -1,    -1,
     847,   658,   849,    -1,    -1,    -1,   853,   854,   134,   856,
      -1,    -1,  2099,  2100,    -1,    -1,    -1,    -1,    -1,  1745,
      -1,    -1,    -1,   123,   933,   682,   873,    -1,   937,   155,
     156,    -1,   689,   942,    -1,    -1,   136,    -1,   138,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,  1773,   957,    -1,
    2137,   960,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   169,
      -1,   171,    13,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2065,    -1,  2067,    -1,    -1,
     937,    -1,    -1,    -1,    -1,   942,   196,    -1,    -1,    -1,
      -1,   948,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1019,  1837,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2106,    -1,  1854,     3,
      -1,    72,    -1,    -1,    -1,    -1,    -1,   237,    -1,    -1,
      -1,   241,    -1,    -1,   244,   245,    -1,    -1,   248,    -1,
      -1,   251,   252,    -1,   254,  2135,   256,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,  1090,    -1,    -1,    -1,  1094,    -1,    -1,    -1,    -1,
    1099,  1100,    -1,   134,  1103,   136,    -1,  2177,    -1,    -1,
      -1,    -1,    -1,    -1,  1113,    79,    -1,    -1,    -1,    -1,
      -1,  1120,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,   167,   327,    -1,    -1,
     330,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1970,   902,  1155,    -1,    -1,    -1,
      -1,    -1,  1099,  1100,    -1,   355,    -1,    -1,    -1,  1168,
      -1,    -1,    -1,  1172,    -1,    -1,    -1,  1176,    -1,    -1,
     370,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2011,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1218,    -1,    -1,    -1,    -1,
      -1,    -1,   959,    -1,    -1,    -1,  1215,  1230,  1155,    -1,
      -1,    -1,    -1,    -1,  2040,  2041,    -1,    -1,  2044,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2059,   208,    -1,    -1,    -1,    -1,    -1,
    1249,    -1,    -1,    -1,    -1,  2071,    -1,    -1,    -1,    -1,
    1197,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1274,    -1,    -1,  1215,    -1,
      -1,    -1,    -1,    -1,   474,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1298,
      -1,   265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1326,  1327,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1273,  1274,    -1,    -1,
      -1,    -1,   306,    -1,   534,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   318,    -1,    -1,    -1,    -1,    -1,
     550,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   335,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,    -1,   357,    -1,    -1,    -1,    -1,  1397,    -1,
      -1,  1400,  1401,    -1,  1403,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1418,
      -1,    -1,    -1,  1422,    -1,    -1,    -1,  1426,   155,   156,
      -1,    -1,    -1,    -1,   168,    -1,    -1,    -1,    -1,   166,
     167,    -1,    -1,    -1,   634,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   421,    -1,    -1,
    1397,    -1,    -1,  1400,  1401,    -1,    -1,    -1,    -1,    -1,
      -1,  1470,   206,   207,    -1,  1474,  1475,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   674,   675,    -1,    -1,   452,  1488,
      -1,    -1,    -1,  1506,  1507,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   694,    -1,   696,   241,    -1,    -1,
      -1,  1448,    -1,    -1,   248,    -1,   480,    -1,    -1,    -1,
      -1,   485,    -1,    -1,    -1,    -1,  1273,    -1,    -1,    -1,
      -1,    -1,    -1,  1470,  1533,    -1,    -1,  1474,  1475,  1538,
      -1,   505,   506,    -1,    -1,    -1,   510,   511,    -1,    -1,
     514,  1488,    -1,    -1,    -1,  1302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   530,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1516,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   552,    -1,
      -1,    -1,    -1,  1592,    -1,    -1,   330,    -1,    -1,    -1,
      -1,    -1,  1601,    -1,    -1,    -1,  1605,    -1,    -1,    -1,
      -1,    -1,    -1,   803,   804,    -1,    -1,    -1,    -1,    -1,
     810,  1558,    -1,    -1,   358,   359,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   378,   835,    -1,    -1,   838,   839,
      -1,   841,    -1,   843,   844,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   630,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1689,    -1,    -1,   643,
      -1,    -1,  1695,    -1,    -1,    -1,    -1,  1686,    -1,    -1,
      -1,  1438,    -1,    -1,    -1,    -1,   886,    -1,    -1,    -1,
     890,    -1,    -1,   667,   894,    -1,    -1,    -1,    -1,    -1,
    1457,    -1,    -1,  1726,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   457,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     474,   475,    -1,   477,   478,    -1,    -1,    -1,  1685,  1686,
      -1,    -1,    -1,   487,    -1,    -1,    -1,   491,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   730,    -1,    -1,  1516,
     504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   968,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   535,    -1,    -1,    -1,   539,    -1,    -1,  1745,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1834,  1835,    -1,    -1,    -1,    -1,    -1,  1841,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1853,    -1,    -1,    -1,    -1,   579,    -1,    -1,    -1,    -1,
    1863,    -1,  1865,    -1,    -1,  1854,    -1,    -1,   822,    -1,
     824,    -1,    -1,    -1,    -1,  1878,   830,  1880,  1881,  1882,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   858,    -1,    -1,    -1,    -1,    -1,
    1837,   635,    -1,   867,    -1,    -1,    -1,   871,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    -1,    -1,  1854,    -1,  1109,
      -1,   655,   656,    -1,    -1,  1938,    -1,    -1,    -1,  1942,
     101,    -1,   666,    -1,  1947,    -1,   670,    -1,  1685,    -1,
      -1,    -1,    -1,   677,    -1,   679,    -1,    -1,    -1,    -1,
      -1,    -1,   916,    -1,    -1,    -1,    -1,   921,    -1,    -1,
      -1,    -1,    -1,    -1,  1154,    -1,  1156,    -1,    -1,  1159,
      -1,  1970,  1162,    -1,    -1,    -1,  1166,    -1,    -1,    -1,
      -1,   152,    -1,    -1,    -1,   156,    -1,    -1,    -1,  2002,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
      -1,  2014,    -1,    -1,    -1,  2018,    -1,    -1,    -1,    -1,
      -1,   168,  2011,    -1,    -1,    -1,    -1,    -1,    -1,  2032,
      -1,    -1,   193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   207,    -1,    -1,    -1,
     211,  2040,  2041,  1007,    -1,    -1,    -1,    -1,    -1,   783,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2059,  2074,    -1,    -1,    -1,   799,   800,    -1,    -1,    -1,
      -1,    -1,  2071,    -1,  2011,    -1,   810,   811,    -1,   813,
     814,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1837,    -1,    -1,   827,   828,   266,    -1,    -1,   832,  2112,
     834,   835,    48,  2040,  2117,    -1,    -1,   841,    -1,    -1,
     281,    -1,    -1,   847,  1304,   849,    -1,    -1,    -1,   853,
     854,   855,   856,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2145,    -1,  2071,  2148,    -1,  2150,    -1,   873,
      -1,   875,    -1,    -1,    -1,   879,    -1,    -1,   305,    -1,
      -1,    -1,   886,   887,    -1,   326,   890,   891,  2171,    -1,
     894,   895,    -1,    -1,    -1,   336,    -1,   901,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,    -1,    -1,
      -1,    -1,  1372,    -1,    -1,    -1,    -1,   358,    -1,   360,
     136,    -1,   138,  1383,    -1,    -1,  1386,    -1,  1388,  1389,
      -1,   358,    -1,   360,   361,    -1,    -1,    -1,   942,   943,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   374,    -1,    -1,
      -1,   378,    -1,   169,    -1,    -1,    -1,    -1,    -1,  1193,
      -1,    -1,    -1,    -1,    -1,    -1,   970,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    20,   420,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,  1242,    51,
      -1,    53,    -1,   454,  1018,  1019,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2044,   244,   245,
      72,    -1,   248,    -1,    -1,   251,   252,  1497,   254,    -1,
     256,    -1,    -1,    -1,    -1,    -1,   487,    -1,    -1,    -1,
      -1,    -1,   493,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     487,    -1,  1296,    -1,    -1,    -1,   493,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1084,    -1,    -1,    -1,    -1,   526,  1320,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   136,  1099,  1100,    -1,    -1,  1103,
    1104,    -1,    -1,    -1,    -1,    -1,  1110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   572,   573,    -1,   561,    -1,    -1,    -1,   579,   355,
    1600,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1155,   579,    -1,   370,  1159,  1160,    -1,  1162,  1163,
      -1,    -1,  1166,  1167,    -1,    -1,    -1,    -1,  1628,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   614,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1660,    -1,    -1,    -1,    -1,    -1,  1666,    -1,    -1,    -1,
     637,    -1,    -1,    -1,    -1,   656,    -1,   658,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   655,   656,
      -1,   658,    -1,    -1,    -1,    -1,    -1,    -1,   679,   666,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   679,    -1,    -1,   682,    -1,    -1,   474,    -1,
     701,    -1,   689,    -1,    -1,   692,    -1,    -1,   709,    -1,
      -1,   712,   713,    -1,   715,    -1,    -1,    -1,    -1,    -1,
      -1,  1741,    -1,   724,    -1,    -1,   727,   728,   729,    -1,
    1524,    -1,    -1,    -1,  1298,    -1,    -1,    -1,    -1,    -1,
    1304,  1305,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1327,    -1,    -1,    -1,    -1,  1788,  1789,
      -1,    -1,    -1,    -1,   550,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   783,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1814,  1815,   783,    -1,    -1,    -1,
      -1,    -1,  1822,    -1,    -1,    -1,   807,  1827,  1372,  1373,
      -1,    -1,   799,    -1,    -1,    -1,    -1,    -1,    -1,  1383,
    1384,    -1,  1386,    -1,    -1,    -1,    -1,   828,   815,    -1,
      -1,    -1,   819,  1397,    -1,    -1,  1400,  1401,    -1,  1403,
     827,   828,    -1,    -1,    -1,   832,   847,    -1,   849,  1643,
      -1,    -1,   853,   854,   855,   856,    -1,    -1,    -1,    -1,
     847,    -1,   849,    -1,    -1,    -1,   853,   854,   855,   856,
      -1,    -1,   873,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   873,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   674,   675,
      -1,    -1,    -1,    -1,  1924,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   694,    -1,
     696,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   942,    -1,    -1,    -1,    -1,   933,    -1,   949,    -1,
      -1,    -1,  1746,  1747,    -1,   942,    -1,    -1,   959,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1533,
     957,    -1,   959,   960,    -1,    -1,  1996,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1560,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1019,    -1,
      -1,    -1,    -1,   207,    -1,    -1,    -1,   803,   804,    -1,
      -1,    -1,  1019,    -1,   810,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   835,
      -1,    -1,   838,   839,    -1,   841,    -1,   843,   844,    -1,
      -1,    -1,    -1,    -1,    -1,  1869,    -1,    -1,    -1,  2099,
      -1,    -1,    -1,    -1,    -1,    -1,  1650,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1099,  1100,
      -1,    -1,  1666,  1090,    -1,  1899,    -1,  1094,    -1,    -1,
     886,    -1,  1099,  1100,   890,    -1,  1103,    -1,   894,    -1,
      -1,   305,    -1,    -1,    -1,    -1,  1113,    -1,    -1,    -1,
      -1,    -1,    -1,  1120,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1155,    -1,
      -1,  1965,    -1,    -1,   358,  1969,   360,   361,    -1,    -1,
      -1,  1168,    -1,    -1,    -1,  1172,    -1,     3,    -1,  1176,
     374,    -1,   968,    -1,   378,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,  1206,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
    1241,    -1,    -1,  1807,  1808,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,  1823,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1273,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1291,    -1,   108,   109,    -1,    -1,    -1,  1298,    -1,    -1,
      -1,    -1,    -1,   487,    -1,    -1,    -1,    -1,    -1,   493,
      -1,  1298,    -1,    -1,    -1,  1302,    -1,   168,   134,    -1,
     136,    -1,    -1,    -1,    -1,    -1,  1327,    -1,    -1,    -1,
      -1,  1332,    -1,  1109,    -1,    -1,    -1,    -1,    -1,  1326,
    1327,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,   207,    -1,  1922,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1930,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   561,  1154,    -1,
    1156,    -1,    -1,  1159,    -1,    -1,  1162,    -1,    -1,    -1,
    1166,    -1,    -1,    -1,    -1,   579,  1397,    -1,    -1,  1400,
    1401,    -1,  1403,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1397,    -1,    -1,  1400,  1401,    -1,  1403,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     614,  1418,  1996,  1997,    -1,  1422,  2000,    -1,    -1,  1426,
      -1,    -1,    -1,    -1,  1445,  1446,  1447,    -1,    -1,  1450,
    1451,    -1,    -1,   637,   305,    -1,  1457,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   655,   656,    -1,   658,    -1,    -1,    -1,    -1,  2043,
      -1,    -1,   666,    -1,  1485,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   679,    -1,    -1,   682,    -1,
      -1,    -1,    -1,    -1,    -1,   689,    -1,   358,   692,   360,
     361,    -1,    -1,    -1,    -1,  1516,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   374,    -1,    -1,    -1,   378,  1304,    -1,
      -1,    -1,  1533,    -1,    -1,  2099,  2100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1533,    -1,    -1,    -1,
      -1,  1538,    -1,    -1,   420,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1372,    -1,    -1,   783,
      -1,    -1,    -1,    -1,    -1,  1592,    -1,  1383,    -1,    -1,
    1386,    -1,  1388,  1389,  1601,   799,    -1,    -1,  1605,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   815,    -1,    -1,    -1,   819,   487,    -1,    -1,    -1,
      -1,    -1,   493,   827,   828,    -1,    -1,    -1,   832,    -1,
      -1,    -1,    -1,    -1,    -1,  1656,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   847,    -1,   849,    -1,    -1,    -1,   853,
     854,   855,   856,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1685,    -1,    -1,    -1,    -1,   873,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   572,   573,    -1,    -1,
     561,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1497,    -1,    -1,    -1,    -1,    -1,    -1,   579,    -1,
      -1,    -1,    -1,    -1,   193,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   207,   933,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   942,    -1,
      -1,    -1,   221,   614,   223,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1773,   957,    -1,   959,   960,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   637,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   655,   656,    -1,   658,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   666,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1600,    -1,    -1,    -1,   679,    -1,
      -1,   682,    -1,    -1,    -1,  1019,  1837,    -1,   689,    -1,
      -1,   692,    -1,   709,    -1,    -1,    -1,    -1,    -1,   715,
      -1,    -1,  1628,    -1,    -1,    -1,    -1,    -1,   724,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   325,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   743,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1090,    -1,    -1,    -1,
    1094,    -1,    -1,   779,    -1,  1099,  1100,    -1,    -1,  1103,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1113,
      -1,    -1,   783,    -1,    -1,    -1,  1120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   815,  1741,    -1,    -1,   819,  1970,
      -1,  1155,    -1,    -1,    -1,    -1,   827,   828,    -1,    -1,
      -1,   832,    -1,  1970,  1168,    -1,    -1,    -1,  1172,    -1,
      -1,    -1,  1176,    -1,    -1,    -1,   847,    -1,   849,    -1,
      -1,    -1,   853,   854,   855,   856,    -1,    -1,    -1,    -1,
      -1,    -1,  1788,  1789,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   873,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1814,  1815,
    2041,    -1,    -1,  2044,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1827,    -1,    -1,  2041,    -1,   515,  2044,    -1,    -1,
      -1,    -1,   521,    -1,    -1,    -1,    -1,   526,    -1,    -1,
      -1,    -1,  2059,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   933,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   942,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   957,    -1,   959,   960,
      -1,    -1,    -1,    -1,  1298,    -1,    -1,    -1,  1302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1326,  1327,    -1,    -1,    -1,    -1,  1924,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1019,   628,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     669,    -1,    -1,  1397,    -1,    -1,  1400,  1401,    -1,  1403,
    1996,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1418,    -1,    -1,    -1,  1422,  1090,
      -1,    -1,  1426,  1094,    -1,    -1,    -1,   706,  1099,  1100,
      -1,    -1,  1103,    -1,    -1,    -1,    -1,    -1,   717,    -1,
      -1,    -1,  1113,    -1,    -1,    -1,    -1,    -1,    -1,  1120,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   741,   742,    -1,    -1,   745,    -1,   747,    -1,
      -1,    -1,    -1,    -1,   753,    -1,   755,   756,    -1,    -1,
      -1,    -1,    -1,    -1,  1155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1168,    -1,    -1,
      -1,  1172,    -1,    -1,   783,  1176,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   796,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   807,  1533,
      -1,    -1,    -1,    -1,  1538,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   823,    -1,    -1,    -1,    -1,   828,
      -1,    -1,    -1,    -1,    -1,  1241,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   860,    -1,    -1,   863,    -1,    -1,    -1,  1592,    -1,
      -1,    -1,    -1,    -1,    -1,   874,    -1,  1601,    -1,    -1,
      -1,  1605,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   902,    -1,    -1,    -1,  1298,    -1,    -1,
      -1,  1302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1326,  1327,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     949,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     959,   960,    -1,    -1,    -1,    -1,    -1,    -1,   967,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1397,    -1,    -1,  1400,
    1401,    -1,  1403,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1019,    -1,    -1,    -1,    -1,    -1,    -1,  1418,  1027,    -1,
      -1,  1422,    -1,    -1,    -1,  1426,    -1,  1036,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,  1470,  1471,    -1,    -1,  1474,  1475,
      -1,    -1,    -1,    -1,  1480,    -1,    -1,    -1,  1484,  1078,
    1486,    -1,  1488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1533,    -1,    -1,   155,    -1,  1538,   158,   159,
      -1,    -1,  1151,    -1,  1153,   165,   166,   167,   168,   169,
     170,   171,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,  1592,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
    1601,    -1,    -1,    -1,  1605,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1636,    -1,    -1,    -1,    -1,    -1,  1235,  1236,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1970,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1681,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1700,  1701,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1302,   155,   156,    -1,   158,   159,  1308,
      -1,    -1,    -1,    -1,    -1,   166,   167,  2041,    -1,    -1,
    2044,    -1,    -1,    -1,  1323,  1731,    -1,    -1,  1327,    -1,
      -1,    -1,    -1,    -1,    -1,  2059,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1345,    -1,    -1,  1348,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    -1,    -1,  1363,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1839,    -1,    -1,    -1,    -1,  1437,  1438,
    1846,    -1,  1848,    -1,    -1,  1851,  1852,    -1,  1854,    -1,
      -1,    -1,    -1,  1859,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1463,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,
      -1,    -1,    -1,  1482,    -1,    -1,  1485,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,   180,    54,    -1,
      56,    -1,    -1,    -1,  1533,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1542,  1543,    -1,    72,    73,    -1,  1955,
      -1,    -1,    -1,    -1,  1960,  1961,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1571,    -1,  1980,    -1,  1575,   103,   104,  1970,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
     136,    -1,    -1,  2019,    -1,  2021,    -1,    -1,    -1,  2025,
    2026,    -1,    -1,    -1,  2030,  2031,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
    2041,    -1,    -1,  2044,    -1,    -1,    -1,  1656,    -1,    -1,
      -1,    -1,  1661,    -1,    -1,    -1,    -1,    -1,  2059,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2092,  2093,  2094,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2123,  2124,  2125,
      -1,    -1,  1721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1764,    -1,    -1,    -1,    -1,
      -1,    -1,  1771,    -1,    -1,  1774,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,  1800,    73,    74,    -1,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,    -1,
     101,    -1,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,   126,   127,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,    -1,    -1,   158,   159,    -1,
      -1,    18,   163,    -1,   165,   166,   167,   168,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,    -1,   101,    -1,   103,   104,  1977,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,   126,
     127,     1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
      -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,   166,
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
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
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
      63,    64,    65,    -1,    -1,    -1,    69,    -1,     5,    72,
      -1,    -1,    -1,    -1,    77,    78,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,   166,   167,    -1,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,   124,    -1,   126,
     127,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,   166,
     167,   168,   169,   170,   171,     3,     4,     5,     6,     7,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,    -1,   136,   137,    -1,    -1,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,   158,    22,    23,    24,    25,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,   158,   159,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     158,   159,    -1,   108,   109,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,   159,    -1,    -1,    -1,    -1,    -1,
      -1,   166,   167,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,
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
     159,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,
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
     156,    -1,   158,   159,    -1,    -1,    -1,   163,    -1,   165,
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
      -1,   163,    -1,   165,   166,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,   162,    -1,    -1,   165,
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
      -1,    -1,    -1,   155,    -1,   157,   158,   159,    -1,    -1,
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
      54,    -1,    56,    57,   134,    -1,    -1,    -1,    -1,    -1,
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
     116,   117,   118,   119,   120,    -1,   122,   123,   124,    49,
     126,   127,    52,    -1,    54,    -1,    56,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,   124,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171
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
     166,   167,   185,   186,   187,   203,   295,   296,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   306,   309,   312,
     314,   315,   316,   317,   318,   319,   320,   321,   322,   323,
     325,   327,   328,   329,   331,   332,   336,   337,   338,   339,
     340,   342,   348,   349,   350,   351,   362,   367,   400,   403,
     413,   419,   421,   427,   431,   436,   437,   438,   439,   440,
     441,   442,   443,   469,   487,   488,   489,   490,     0,   182,
     106,   186,   203,   299,   301,   312,   315,   318,   328,   332,
     337,   120,   155,    58,    61,    62,    64,   155,   155,   365,
     425,   426,   427,   324,   325,   108,   109,   186,   188,   401,
     402,   188,   155,   413,   155,   155,     4,   106,   108,   109,
     316,   321,   322,   155,   155,   203,   426,   431,   437,   438,
     439,   441,   442,   443,   108,   339,   160,   182,   159,   302,
     312,   315,   436,   440,   486,   487,   490,   491,   180,   183,
     152,   163,   179,   224,   383,    89,   161,   420,   101,   188,
     424,   161,   161,   161,   180,   108,   109,   155,   203,   307,
     308,   431,   432,   433,   434,   435,   436,   440,   444,   445,
     446,   447,   448,   449,   450,   451,   452,   458,     3,    47,
      48,    50,    55,   330,     3,   159,   203,   301,   302,   316,
     320,   322,   333,   338,   416,   436,   440,   490,    69,   299,
     301,   315,   328,   332,   337,   417,   436,   440,    65,   321,
     321,   316,   322,   310,   321,   322,   330,   349,   316,   321,
     316,   158,   425,   161,   183,   155,   163,   232,   425,   425,
       3,   290,   291,   306,   309,   315,   319,   320,   159,   312,
     315,   488,   188,   188,   413,   179,   315,   155,   203,   422,
     431,   432,   436,   445,   449,   159,   203,   302,   490,   414,
     415,    57,    65,    66,    67,    68,   159,   177,   188,   389,
     391,   395,   397,   398,   338,    57,   157,   159,   203,   311,
     315,   319,   327,   328,   334,   335,   336,   337,   341,   348,
     349,   367,   377,   379,   469,   482,   483,   484,   485,   490,
     491,   425,   108,   109,   170,   186,   338,   366,   458,   427,
     155,   396,   397,   155,    13,    88,   155,   188,   428,   429,
     430,   120,   189,   190,    49,    52,    54,    56,    73,   103,
     104,   106,   107,   118,   119,   122,   123,   124,   126,   127,
     155,   159,   165,   168,   169,   170,   171,   184,   185,   189,
     191,   194,   202,   203,   204,   205,   208,   209,   210,   211,
     212,   213,   214,   215,   216,   217,   218,   219,   220,   226,
     338,   157,   159,   202,   203,   219,   221,   312,   338,   381,
     382,   399,   486,   491,   428,   315,   437,   438,   439,   441,
     442,   443,   157,   157,   157,   157,   157,   157,   157,   108,
     159,   186,   312,   469,   488,   159,   166,   203,   221,   301,
     302,   311,   313,   315,   328,   335,   337,   374,   375,   376,
     378,   379,   482,   490,   160,   155,   159,   436,   440,   490,
     155,   161,   106,   158,   159,   163,   185,   187,   221,   384,
     385,   386,   387,   388,    22,   384,   155,   188,   232,   155,
     155,   186,   422,   186,   426,   431,   433,   434,   435,   444,
     446,   447,   448,   450,   451,   452,   315,   432,   445,   449,
     161,   424,   159,   425,   466,   469,   424,   425,   425,   420,
     290,   155,   425,   466,   424,   425,   425,   420,   425,   425,
     315,   422,   155,   155,   314,   315,   312,   315,   160,   182,
     312,   486,   491,   424,   340,   163,   420,   290,   188,   188,
     383,   301,   320,   418,   436,   440,   163,   420,   290,   401,
     315,   328,   315,   315,   108,   339,   108,   109,   186,   338,
     343,   401,   137,   186,   315,   371,   372,   376,   377,   380,
     154,   182,   232,   306,   180,   436,   449,   315,   182,   424,
     155,   424,   183,   221,   426,   431,   315,   155,   188,   411,
     163,   155,   188,   163,   188,   137,   166,   167,   394,   157,
     161,   188,   398,   157,   425,   160,   182,   313,   315,   328,
     335,   337,   481,   482,   490,   491,   155,   159,   167,   179,
     203,   469,   471,   472,   473,   474,   475,   476,   493,   203,
     341,   490,   315,   335,   321,   316,   425,   157,   313,   315,
     483,   313,   469,   483,   186,   366,   458,   363,   163,   366,
     389,   179,   389,   428,   157,   161,   155,   157,   120,   155,
     202,   155,   155,   155,   205,   155,   202,   155,   106,   108,
     109,   316,   321,   322,   155,   202,   202,    19,    21,    85,
     159,   168,   169,   206,   207,   221,   228,   232,   351,   381,
     490,   161,   182,   155,   191,   159,   164,   159,   164,   123,
     125,   126,   127,   155,   158,   159,   163,   164,   205,   205,
     172,   166,   173,   174,   168,   169,   128,   129,   130,   131,
     175,   176,   132,   133,   167,   165,   177,   134,   135,   178,
     157,   161,   158,   182,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   179,   223,   224,   225,   155,
     203,   462,   463,   464,   465,   466,   157,   161,   157,   157,
     157,   157,   157,   157,   157,   155,   425,   466,   469,   155,
     466,   469,   155,   182,   155,   312,   488,   160,   182,   183,
     159,   183,   155,   167,   203,   431,   453,   454,   455,   456,
     457,   458,   459,   460,   461,   137,   490,   161,   183,   161,
     183,   188,   188,   155,   182,   182,   182,   182,   159,   187,
     182,   385,   162,   161,   492,   384,   158,   159,   162,   388,
     399,   155,   189,   182,   179,   431,   433,   434,   435,   444,
     446,   447,   448,   450,   451,   452,   157,   157,   157,   157,
     157,   157,   157,   157,   157,   157,   432,   445,   449,   425,
     179,   160,   182,   383,   232,   420,   371,   383,   232,   422,
     228,   382,   228,   382,   422,   108,   159,   411,   232,   420,
     424,   163,   163,   420,   290,   411,   232,   420,   345,   346,
     344,   163,   157,   161,   157,   161,    70,   292,   293,   180,
     166,   221,   182,   431,   413,   411,   188,   160,   182,   155,
     393,   391,   392,    78,   326,   186,   313,   469,   483,   315,
     319,   490,   371,   472,   473,   474,   160,   182,    18,   221,
     315,   471,   493,   425,   425,   469,   313,   481,   491,   315,
     186,   313,   483,   425,   163,   425,   366,    10,   165,   366,
     368,   369,   163,   157,   382,   157,   157,   429,   156,   195,
     196,   197,   221,   180,   381,   491,   191,   159,   381,   382,
     381,   491,   221,   381,   157,   381,   381,   381,   160,   182,
     157,   168,   169,   207,    18,   317,   157,   161,   157,   166,
     167,   157,   156,   221,   227,   221,   163,   221,   186,   221,
     186,   118,   159,   186,   195,   118,   159,   188,   351,   221,
     195,   186,   205,   208,   208,   208,   209,   209,   210,   210,
     211,   211,   211,   211,   212,   212,   213,   214,   215,   216,
     217,   162,   228,   180,   189,   159,   186,   221,   163,   221,
     371,   463,   464,   465,   315,   462,   425,   425,   221,   382,
     155,   425,   466,   469,   155,   466,   469,   371,   371,   182,
     182,   160,   160,   155,   431,   454,   455,   456,   459,    18,
     315,   453,   457,   155,   425,   475,   493,   425,   425,   493,
     155,   425,   475,   425,   425,   183,   220,   188,   375,   378,
     160,   378,   379,   160,   493,   493,   137,   373,   374,   375,
     373,   375,   373,   188,   182,   219,   220,   221,   423,   492,
     384,   386,   154,   182,   157,   182,   157,   373,   221,   157,
     157,   157,   157,   157,   157,   157,   157,   157,   155,   425,
     466,   469,   155,   425,   466,   469,   155,   425,   466,   469,
     422,    22,   469,   221,   322,   338,   467,   232,   157,   157,
     157,   157,   157,   409,   410,   232,   154,   182,   411,   232,
     420,   410,   232,   163,   163,   163,   352,   137,   376,   377,
     186,   188,   294,    18,    71,    73,    74,    76,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      93,    94,    95,    96,    97,    98,    99,   101,   108,   109,
     121,   155,   159,   188,   228,   229,   230,   231,   232,   233,
     234,   236,   237,   246,   253,   254,   255,   256,   257,   258,
     263,   264,   267,   268,   269,   270,   271,   272,   273,   279,
     280,   281,   295,   315,   319,   421,    70,   183,   183,   373,
     412,   410,   157,   299,   301,   312,   404,   405,   406,   407,
     399,   179,   390,   390,   313,   483,   159,   166,   203,   221,
     338,   221,   315,   157,   157,   157,   157,     5,   315,   425,
     471,   364,   368,   366,   163,   338,   161,   492,   188,   368,
     163,   157,   157,   161,   157,   157,   161,   182,   161,   157,
     157,   157,   161,   157,   205,   157,   157,   157,   205,    18,
     317,   221,   157,   157,   156,   163,   205,   160,   161,   183,
     195,   160,   160,   118,   122,   124,   187,   198,   199,   200,
     157,   198,   160,   161,   154,   219,   162,   157,   198,   183,
     385,   157,   157,   157,   157,   462,   371,   371,   157,   157,
     373,   373,   459,   157,   157,   157,   157,   155,   431,   458,
     453,   457,   371,   371,   160,   183,   493,   161,   183,   157,
     161,   161,   183,   161,   183,   383,   198,   137,   171,   183,
     183,   154,   384,   221,   425,   373,   425,   183,   155,   425,
     466,   469,   155,   425,   466,   469,   155,   425,   466,   469,
     371,   371,   371,   424,   149,   171,   183,   468,   161,   183,
     412,   404,   410,   232,   412,   352,   352,   352,     3,     5,
      10,    73,   154,   296,   303,   304,   312,   315,   353,   358,
     486,   161,   180,   155,    61,    62,   180,   232,   295,   421,
     155,   155,    18,   230,   155,   155,   180,   188,   180,   188,
     166,   188,   163,   229,   155,   155,   155,   230,   155,   232,
     221,   222,   222,    14,   282,   258,   269,   162,   180,   183,
     234,    78,   180,   188,    91,    92,   262,   266,   112,   135,
     261,   111,   134,   265,   261,   380,   315,   294,   160,   160,
     183,   412,   188,   422,   183,   180,   183,   180,   183,   157,
     382,   396,   396,   182,   183,   183,   183,   221,   155,   425,
     475,   469,   314,     5,   166,   183,   221,   366,   492,   163,
     368,    10,   369,   154,   179,   370,   492,   154,   182,   197,
     311,   186,    78,   192,   193,   381,   205,   205,   205,   205,
     205,   163,   385,   156,   221,   161,   154,   201,   159,   199,
     201,   201,   160,   161,   125,   158,   160,   227,   219,   180,
     160,   492,   155,   425,   466,   469,   157,   157,   183,   183,
     157,   155,   425,   466,   469,   155,   425,   475,   431,   425,
     425,   157,   157,   160,   378,   160,   137,   375,   137,   157,
     157,   183,   220,   220,   160,   160,   183,   183,   157,   371,
     371,   371,   157,   157,   157,   383,   161,   221,   221,   322,
     338,   160,   154,   183,   412,   154,   154,   154,   154,   312,
     312,   351,   359,   486,   312,   358,   155,   347,   180,   180,
     155,   162,   203,   354,   355,   361,   431,   432,   445,   449,
     161,   180,   188,   188,   195,   180,   232,   180,   232,   228,
     238,   295,   297,   300,   306,   315,   319,   228,    80,   157,
     238,   149,   150,   151,   156,   157,   180,   228,   247,   248,
     250,   295,   180,   180,   228,   180,   385,   180,   228,   399,
     228,   247,   113,   114,   115,   116,   117,   274,   276,   277,
     180,   100,   180,    84,   155,   157,   425,   154,   180,   180,
     155,   155,   230,   230,   258,   155,   268,   258,   268,   232,
     180,   157,   154,   394,   154,   182,   161,   161,   160,   160,
     160,   183,   371,   221,   221,   183,   160,   183,   163,   154,
     368,   492,   338,   188,   163,   220,   154,   404,   470,   471,
     157,   162,   157,   161,   162,   385,   492,   227,   123,   198,
     199,   159,   199,   159,   199,   160,   154,   371,   157,   157,
     371,   371,   160,   183,   157,   425,   157,   157,   157,   228,
     468,   154,   154,   347,   347,   347,   354,   155,   203,   356,
     357,   466,   477,   478,   479,   480,   180,   161,   180,   354,
     180,   399,   426,   431,   221,   315,   154,   161,   180,   360,
     361,   360,   360,   188,   157,   157,   228,   315,   157,   155,
     230,   157,   149,   150,   151,   171,   180,   251,   252,   230,
     229,   180,   252,   157,   162,   228,   156,   228,   229,   250,
     180,   492,   157,   157,   157,   157,   232,   276,   277,   155,
     221,   155,   189,     1,   230,   205,   259,   228,    75,   110,
     260,   262,    75,   425,   390,   405,   182,   182,   160,   157,
     183,   183,   160,   160,   368,   492,   154,   370,   385,   183,
     157,   221,   193,   221,   492,   154,   160,   160,   198,   198,
     157,   425,   425,   157,   157,   160,   160,   221,   180,   478,
     479,   480,   315,   477,   161,   180,   425,   425,   180,   157,
     431,   425,   230,   230,    77,    78,   163,   241,   242,   243,
     157,   228,    75,   230,   228,   156,   228,    75,   180,    57,
     108,   156,   228,   229,   249,   250,   156,   228,   230,   248,
     252,   252,   180,   228,   154,   163,   243,   230,   230,   155,
     182,   180,   189,   157,   162,   157,   161,   162,   157,   230,
     155,   230,   230,   230,   396,   188,   422,   160,   160,   492,
     154,   492,   154,   154,   160,   160,   157,   157,   157,   477,
     425,   355,    75,     1,   220,   239,   240,   423,     1,   162,
       1,   182,   230,   241,    75,   180,   157,   230,    75,   180,
     171,   171,   230,   229,   188,   252,   252,   180,   228,   249,
     171,   171,    75,   156,   228,   156,   228,   229,   180,     1,
     182,   182,   278,   313,   315,   486,   162,   180,   159,   189,
     283,   284,   285,   205,   195,   228,   261,   154,   154,   155,
     425,   466,   469,   357,   230,   137,     1,   161,   162,   154,
     288,   289,   295,   230,    75,   180,   230,   228,   156,   156,
     228,   156,   228,   156,   228,   229,   156,   228,   156,   228,
     230,   171,   171,   171,   171,   154,   288,   278,   183,   155,
     203,   422,   477,   186,   162,   106,   155,   157,   162,   161,
     157,   157,    75,   257,   371,   220,   239,   242,   244,   245,
     295,   230,   171,   171,   171,   171,   156,   156,   228,   156,
     228,   156,   228,   244,   183,   180,   275,   315,   283,   160,
     220,   180,   283,   285,   230,    75,   157,   230,   235,   183,
     242,   156,   156,   228,   156,   228,   156,   228,   183,   275,
     219,   157,   162,   189,   157,   157,   162,   230,     1,   230,
     154,   235,   154,   157,   232,   189,   286,   155,   180,   286,
     232,   161,   162,   220,   157,   189,   186,   287,   157,   180,
     157,   161,   180,   186
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   181,   182,   183,   184,   184,   184,   184,   184,   185,
     185,   185,   185,   185,   185,   185,   185,   186,   186,   187,
     187,   188,   188,   188,   189,   190,   190,   191,   191,   191,
     191,   191,   191,   191,   191,   191,   191,   191,   191,   191,
     191,   191,   192,   192,   193,   193,   194,   194,   194,   194,
     194,   194,   194,   194,   194,   194,   194,   194,   194,   194,
     194,   194,   194,   194,   194,   194,   194,   194,   194,   194,
     195,   195,   196,   196,   197,   197,   198,   198,   199,   199,
     199,   199,   199,   199,   199,   200,   200,   200,   201,   201,
     202,   202,   202,   202,   202,   202,   202,   202,   202,   202,
     202,   202,   202,   202,   202,   202,   203,   203,   203,   204,
     204,   204,   204,   205,   205,   205,   205,   205,   205,   205,
     205,   205,   206,   206,   206,   206,   207,   207,   208,   208,
     209,   209,   209,   209,   210,   210,   210,   211,   211,   211,
     212,   212,   212,   212,   212,   213,   213,   213,   214,   214,
     215,   215,   216,   216,   217,   217,   218,   218,   219,   219,
     219,   220,   221,   221,   221,   222,   222,   223,   223,   224,
     224,   225,   225,   225,   225,   225,   225,   225,   225,   225,
     225,   225,   226,   226,   227,   227,   227,   227,   228,   228,
     229,   229,   230,   230,   230,   230,   230,   230,   230,   230,
     230,   230,   230,   230,   230,   230,   230,   230,   231,   231,
     232,   232,   233,   233,   234,   234,   234,   234,   234,   235,
     235,   235,   236,   237,   237,   237,   237,   237,   237,   237,
     237,   238,   238,   238,   238,   239,   239,   239,   240,   240,
     241,   241,   241,   241,   241,   242,   242,   243,   244,   244,
     245,   245,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   246,   246,   246,   247,   247,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   249,   249,   250,   250,   250,   251,   251,   252,   252,
     252,   253,   253,   253,   253,   253,   253,   253,   253,   253,
     253,   253,   253,   253,   253,   253,   253,   253,   253,   253,
     253,   254,   254,   255,   256,   257,   258,   258,   259,   259,
     260,   261,   261,   262,   262,   263,   263,   263,   263,   263,
     263,   264,   265,   265,   266,   267,   267,   268,   268,   269,
     269,   269,   270,   271,   272,   273,   273,   273,   274,   274,
     275,   275,   276,   276,   276,   276,   277,   278,   278,   278,
     278,   278,   279,   280,   280,   281,   281,   281,   281,   281,
     282,   282,   283,   283,   284,   284,   285,   285,   286,   286,
     286,   287,   287,   288,   288,   289,   289,   290,   290,   291,
     291,   292,   292,   293,   293,   294,   294,   295,   295,   295,
     296,   296,   297,   297,   297,   297,   297,   298,   298,   298,
     299,   299,   299,   299,   299,   300,   300,   300,   300,   300,
     301,   301,   301,   301,   302,   302,   303,   303,   303,   304,
     304,   304,   304,   304,   305,   305,   306,   306,   306,   306,
     307,   307,   307,   307,   307,   308,   308,   309,   309,   309,
     309,   310,   310,   310,   311,   311,   311,   312,   312,   312,
     313,   313,   313,   314,   314,   315,   315,   316,   316,   317,
     317,   317,   317,   317,   318,   319,   319,   319,   320,   320,
     321,   321,   321,   321,   321,   321,   321,   321,   321,   322,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   324,   324,
     325,   326,   326,   327,   327,   327,   327,   327,   328,   328,
     329,   329,   329,   329,   330,   330,   330,   330,   330,   330,
     331,   331,   331,   331,   332,   333,   332,   332,   334,   334,
     334,   334,   335,   335,   335,   336,   336,   336,   336,   337,
     337,   337,   338,   338,   338,   338,   338,   338,   339,   339,
     339,   340,   340,   341,   341,   343,   342,   344,   342,   345,
     342,   346,   342,   342,   347,   347,   348,   348,   349,   349,
     350,   350,   350,   351,   351,   351,   351,   351,   351,   351,
     351,   352,   352,   353,   353,   353,   353,   353,   353,   353,
     353,   353,   353,   353,   353,   354,   354,   354,   355,   355,
     355,   355,   356,   356,   356,   357,   358,   358,   359,   359,
     360,   360,   361,   362,   362,   363,   362,   362,   364,   362,
     362,   362,   365,   365,   366,   366,   367,   367,   368,   368,
     368,   368,   369,   369,   370,   370,   370,   371,   371,   371,
     371,   372,   372,   372,   372,   373,   373,   373,   373,   373,
     373,   373,   374,   374,   374,   374,   375,   375,   376,   376,
     377,   377,   378,   378,   378,   378,   378,   379,   379,   379,
     379,   379,   380,   380,   381,   381,   381,   382,   382,   383,
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
     429,   430,   430,   430,   431,   431,   432,   432,   432,   432,
     433,   433,   433,   433,   434,   434,   434,   434,   434,   434,
     434,   435,   435,   435,   435,   436,   436,   436,   437,   437,
     437,   437,   437,   438,   438,   438,   438,   439,   439,   439,
     439,   439,   439,   440,   440,   440,   441,   441,   441,   441,
     441,   442,   442,   442,   442,   443,   443,   443,   443,   443,
     443,   444,   444,   445,   445,   445,   445,   446,   446,   446,
     446,   447,   447,   447,   447,   447,   447,   447,   448,   448,
     448,   448,   449,   449,   449,   450,   450,   450,   450,   450,
     451,   451,   451,   451,   452,   452,   452,   452,   452,   452,
     453,   453,   453,   453,   453,   454,   454,   454,   455,   455,
     455,   455,   456,   456,   456,   457,   457,   457,   457,   457,
     458,   458,   459,   459,   459,   460,   460,   461,   461,   462,
     462,   462,   463,   463,   463,   463,   463,   464,   464,   464,
     464,   465,   465,   465,   466,   466,   466,   466,   466,   467,
     467,   467,   467,   467,   467,   468,   468,   469,   469,   469,
     469,   470,   470,   471,   471,   471,   471,   472,   472,   472,
     472,   472,   473,   473,   473,   473,   474,   474,   474,   475,
     475,   475,   476,   476,   476,   476,   476,   476,   477,   477,
     477,   478,   478,   478,   478,   478,   479,   479,   479,   479,
     480,   480,   481,   481,   481,   482,   482,   483,   483,   483,
     483,   483,   483,   484,   484,   484,   484,   484,   484,   484,
     484,   484,   484,   485,   485,   485,   485,   486,   486,   486,
     487,   487,   488,   488,   488,   488,   488,   488,   489,   489,
     489,   489,   489,   489,   490,   490,   490,   491,   491,   491,
     492,   492,   493,   493
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
       2,     4,     4,     4,     6,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     5,     5,     4,     5,     5,
       5,     4,     2,     2,     3,     3,     1,     1,     1,     3,
       1,     3,     3,     3,     1,     3,     3,     1,     3,     3,
       1,     3,     3,     3,     3,     1,     3,     3,     1,     3,
       1,     3,     1,     3,     1,     3,     1,     3,     1,     5,
       4,     1,     1,     3,     6,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     7,     1,     1,     3,     3,     1,     3,
       0,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     4,
       2,     6,     1,     2,     1,     2,     1,     2,     1,     1,
       2,     2,     2,     5,     7,     5,    10,     7,     5,    10,
       7,     1,     1,     1,     2,     1,     3,     1,     1,     3,
       2,     3,     3,     2,     2,     1,     2,     2,     0,     1,
       2,     3,     4,     6,     5,     7,     6,     7,     7,     8,
       4,     6,     5,     7,     1,     3,     4,     5,     4,     3,
       5,     1,     2,     3,     3,     3,     5,     5,     5,     5,
       3,     5,     5,     5,     3,     4,     5,     5,     5,     5,
       7,     7,     7,     7,     7,     7,     7,     2,     3,     4,
       4,     4,     6,     6,     6,     6,     6,     6,     6,     3,
       4,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     4,     2,     3,     3,     2,     3,     2,     3,
       3,     6,     2,     2,     3,     3,     3,     3,     3,     3,
       5,     1,     1,     5,     5,     4,     0,     1,     1,     3,
       4,     1,     1,     4,     6,     3,     5,     5,     5,     8,
       9,     1,     1,     1,     4,     3,     3,     1,     3,     1,
       3,     5,     1,     2,     5,     3,     3,     4,     8,     9,
       0,     2,     1,     1,     1,     1,     2,     1,     2,     2,
       2,     1,     3,     1,     1,     6,     8,    10,    12,    14,
       0,     1,     0,     1,     1,     3,     4,     7,     0,     1,
       3,     1,     3,     0,     1,     1,     2,     0,     1,     2,
       3,     0,     1,     3,     4,     1,     3,     2,     2,     1,
       7,     5,     1,     1,     1,     1,     1,     2,     3,     6,
       3,     3,     4,     2,     3,     1,     2,     2,     3,     8,
       9,     9,     8,     8,     5,     7,     2,     2,     3,     3,
       3,     4,     3,     4,     4,     5,     2,     1,     1,     1,
       3,     3,     2,     4,     6,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     4,     1,     2,     3,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       5,     0,     1,     1,     2,     2,     3,     3,     1,     3,
       1,     2,     2,     2,     4,     4,     4,     4,     1,     1,
       1,     2,     2,     3,     1,     0,     3,     2,     1,     2,
       2,     3,     1,     2,     2,     1,     2,     2,     3,     1,
       2,     2,     1,     2,     3,     1,     2,     3,     1,     3,
       4,     1,     1,     1,     1,     0,     7,     0,     8,     0,
       8,     0,     8,     1,     0,     3,     3,     3,     1,     1,
       2,     1,     1,     1,     2,     1,     2,     1,     2,     1,
       2,     0,     2,     3,     3,     4,     4,     4,     3,     2,
       2,     3,     3,     2,     1,     0,     1,     4,     1,     2,
       2,     2,     0,     1,     4,     1,     2,     3,     1,     2,
       0,     1,     2,     7,     8,     0,     9,     8,     0,    11,
      10,     1,     2,     3,     0,     1,     3,     3,     3,     2,
       5,     4,     1,     1,     0,     2,     5,     0,     1,     1,
       3,     1,     1,     3,     3,     0,     1,     1,     1,     3,
       3,     3,     1,     3,     3,     5,     1,     3,     3,     3,
       2,     3,     1,     3,     3,     4,     1,     1,     1,     1,
       2,     1,     1,     3,     1,     1,     2,     1,     1,     0,
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
       4,     1,     1,     1,     1,     3,     2,     1,     2,     2,
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
       3,     3,     6,     3,     2,     3,     7,     5,     1,     1,
       1,     3,     3,     3,     5,     1,     1,     5,     5,     6,
       6,     0,     1,     1,     3,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     3,     6,     3,     1,
       2,     1,     2,     6,     5,     6,     7,     7,     1,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       6,     3,     1,     1,     2,     1,     1,     2,     3,     2,
       3,     2,     3,     3,     2,     4,     3,     2,     3,     2,
       4,     3,     2,     6,     6,     6,     7,     1,     2,     1,
       1,     1,     2,     3,     2,     3,     2,     3,     3,     4,
       2,     3,     4,     2,     5,     6,     7,     5,     6,     6,
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
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 8449 "Parser/parser.cc"
    break;

  case 3:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8455 "Parser/parser.cc"
    break;

  case 4:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8461 "Parser/parser.cc"
    break;

  case 5:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8467 "Parser/parser.cc"
    break;

  case 6:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8473 "Parser/parser.cc"
    break;

  case 7:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8479 "Parser/parser.cc"
    break;

  case 8:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8485 "Parser/parser.cc"
    break;

  case 20:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8491 "Parser/parser.cc"
    break;

  case 24:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8497 "Parser/parser.cc"
    break;

  case 25:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8503 "Parser/parser.cc"
    break;

  case 26:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8513 "Parser/parser.cc"
    break;

  case 27:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8519 "Parser/parser.cc"
    break;

  case 28:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8525 "Parser/parser.cc"
    break;

  case 29:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8531 "Parser/parser.cc"
    break;

  case 31:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8537 "Parser/parser.cc"
    break;

  case 32:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8543 "Parser/parser.cc"
    break;

  case 33:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8549 "Parser/parser.cc"
    break;

  case 34:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8555 "Parser/parser.cc"
    break;

  case 35:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8565 "Parser/parser.cc"
    break;

  case 36:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8571 "Parser/parser.cc"
    break;

  case 37:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8577 "Parser/parser.cc"
    break;

  case 38:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8583 "Parser/parser.cc"
    break;

  case 39:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8589 "Parser/parser.cc"
    break;

  case 40:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8595 "Parser/parser.cc"
    break;

  case 41:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8601 "Parser/parser.cc"
    break;

  case 43:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8613 "Parser/parser.cc"
    break;

  case 44:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8622 "Parser/parser.cc"
    break;

  case 45:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8628 "Parser/parser.cc"
    break;

  case 47:
#line 772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8634 "Parser/parser.cc"
    break;

  case 48:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8640 "Parser/parser.cc"
    break;

  case 49:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8646 "Parser/parser.cc"
    break;

  case 50:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8652 "Parser/parser.cc"
    break;

  case 51:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8662 "Parser/parser.cc"
    break;

  case 52:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8668 "Parser/parser.cc"
    break;

  case 53:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8675 "Parser/parser.cc"
    break;

  case 54:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8681 "Parser/parser.cc"
    break;

  case 55:
#line 798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8687 "Parser/parser.cc"
    break;

  case 56:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8693 "Parser/parser.cc"
    break;

  case 57:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8699 "Parser/parser.cc"
    break;

  case 58:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8705 "Parser/parser.cc"
    break;

  case 59:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8711 "Parser/parser.cc"
    break;

  case 60:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8717 "Parser/parser.cc"
    break;

  case 61:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8723 "Parser/parser.cc"
    break;

  case 62:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8729 "Parser/parser.cc"
    break;

  case 63:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8735 "Parser/parser.cc"
    break;

  case 64:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8741 "Parser/parser.cc"
    break;

  case 65:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8747 "Parser/parser.cc"
    break;

  case 66:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8753 "Parser/parser.cc"
    break;

  case 67:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8759 "Parser/parser.cc"
    break;

  case 68:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8765 "Parser/parser.cc"
    break;

  case 69:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8775 "Parser/parser.cc"
    break;

  case 70:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8781 "Parser/parser.cc"
    break;

  case 73:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8787 "Parser/parser.cc"
    break;

  case 74:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8793 "Parser/parser.cc"
    break;

  case 77:
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8799 "Parser/parser.cc"
    break;

  case 79:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8805 "Parser/parser.cc"
    break;

  case 80:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8811 "Parser/parser.cc"
    break;

  case 81:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8817 "Parser/parser.cc"
    break;

  case 82:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8823 "Parser/parser.cc"
    break;

  case 83:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8829 "Parser/parser.cc"
    break;

  case 84:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8835 "Parser/parser.cc"
    break;

  case 85:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8841 "Parser/parser.cc"
    break;

  case 86:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8847 "Parser/parser.cc"
    break;

  case 87:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8855 "Parser/parser.cc"
    break;

  case 88:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8861 "Parser/parser.cc"
    break;

  case 89:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8870 "Parser/parser.cc"
    break;

  case 92:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8876 "Parser/parser.cc"
    break;

  case 93:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8882 "Parser/parser.cc"
    break;

  case 94:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8902 "Parser/parser.cc"
    break;

  case 95:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8908 "Parser/parser.cc"
    break;

  case 96:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8914 "Parser/parser.cc"
    break;

  case 97:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8920 "Parser/parser.cc"
    break;

  case 98:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8926 "Parser/parser.cc"
    break;

  case 99:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8932 "Parser/parser.cc"
    break;

  case 100:
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8938 "Parser/parser.cc"
    break;

  case 101:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8944 "Parser/parser.cc"
    break;

  case 102:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8950 "Parser/parser.cc"
    break;

  case 103:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8956 "Parser/parser.cc"
    break;

  case 104:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8962 "Parser/parser.cc"
    break;

  case 105:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8971 "Parser/parser.cc"
    break;

  case 106:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8977 "Parser/parser.cc"
    break;

  case 107:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 8983 "Parser/parser.cc"
    break;

  case 108:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 8989 "Parser/parser.cc"
    break;

  case 109:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 8995 "Parser/parser.cc"
    break;

  case 110:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9001 "Parser/parser.cc"
    break;

  case 111:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9007 "Parser/parser.cc"
    break;

  case 112:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9013 "Parser/parser.cc"
    break;

  case 114:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9019 "Parser/parser.cc"
    break;

  case 115:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9025 "Parser/parser.cc"
    break;

  case 116:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9031 "Parser/parser.cc"
    break;

  case 117:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9037 "Parser/parser.cc"
    break;

  case 118:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9043 "Parser/parser.cc"
    break;

  case 119:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9049 "Parser/parser.cc"
    break;

  case 120:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9055 "Parser/parser.cc"
    break;

  case 121:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9061 "Parser/parser.cc"
    break;

  case 129:
#line 1023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9067 "Parser/parser.cc"
    break;

  case 131:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9073 "Parser/parser.cc"
    break;

  case 132:
#line 1031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9079 "Parser/parser.cc"
    break;

  case 133:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9085 "Parser/parser.cc"
    break;

  case 135:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9091 "Parser/parser.cc"
    break;

  case 136:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9097 "Parser/parser.cc"
    break;

  case 138:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9103 "Parser/parser.cc"
    break;

  case 139:
#line 1049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9109 "Parser/parser.cc"
    break;

  case 141:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9115 "Parser/parser.cc"
    break;

  case 142:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9121 "Parser/parser.cc"
    break;

  case 143:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9127 "Parser/parser.cc"
    break;

  case 144:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9133 "Parser/parser.cc"
    break;

  case 146:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9139 "Parser/parser.cc"
    break;

  case 147:
#line 1069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9145 "Parser/parser.cc"
    break;

  case 149:
#line 1075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9151 "Parser/parser.cc"
    break;

  case 151:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9157 "Parser/parser.cc"
    break;

  case 153:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9163 "Parser/parser.cc"
    break;

  case 155:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9169 "Parser/parser.cc"
    break;

  case 157:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9175 "Parser/parser.cc"
    break;

  case 159:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9181 "Parser/parser.cc"
    break;

  case 160:
#line 1107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9187 "Parser/parser.cc"
    break;

  case 163:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 9199 "Parser/parser.cc"
    break;

  case 164:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9205 "Parser/parser.cc"
    break;

  case 165:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9211 "Parser/parser.cc"
    break;

  case 169:
#line 1141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9217 "Parser/parser.cc"
    break;

  case 170:
#line 1142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9223 "Parser/parser.cc"
    break;

  case 171:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9229 "Parser/parser.cc"
    break;

  case 172:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9235 "Parser/parser.cc"
    break;

  case 173:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9241 "Parser/parser.cc"
    break;

  case 174:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9247 "Parser/parser.cc"
    break;

  case 175:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9253 "Parser/parser.cc"
    break;

  case 176:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9259 "Parser/parser.cc"
    break;

  case 177:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9265 "Parser/parser.cc"
    break;

  case 178:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9271 "Parser/parser.cc"
    break;

  case 179:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9277 "Parser/parser.cc"
    break;

  case 180:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9283 "Parser/parser.cc"
    break;

  case 181:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9289 "Parser/parser.cc"
    break;

  case 182:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9295 "Parser/parser.cc"
    break;

  case 183:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9301 "Parser/parser.cc"
    break;

  case 185:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9307 "Parser/parser.cc"
    break;

  case 186:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9313 "Parser/parser.cc"
    break;

  case 187:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9319 "Parser/parser.cc"
    break;

  case 189:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9325 "Parser/parser.cc"
    break;

  case 190:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9331 "Parser/parser.cc"
    break;

  case 205:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9337 "Parser/parser.cc"
    break;

  case 207:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9343 "Parser/parser.cc"
    break;

  case 208:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9349 "Parser/parser.cc"
    break;

  case 209:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9360 "Parser/parser.cc"
    break;

  case 210:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9366 "Parser/parser.cc"
    break;

  case 211:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9372 "Parser/parser.cc"
    break;

  case 213:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9378 "Parser/parser.cc"
    break;

  case 214:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9384 "Parser/parser.cc"
    break;

  case 215:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9390 "Parser/parser.cc"
    break;

  case 216:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9396 "Parser/parser.cc"
    break;

  case 217:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9402 "Parser/parser.cc"
    break;

  case 220:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9408 "Parser/parser.cc"
    break;

  case 221:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9415 "Parser/parser.cc"
    break;

  case 222:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9421 "Parser/parser.cc"
    break;

  case 223:
#line 1299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9427 "Parser/parser.cc"
    break;

  case 224:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9433 "Parser/parser.cc"
    break;

  case 225:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9439 "Parser/parser.cc"
    break;

  case 226:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9453 "Parser/parser.cc"
    break;

  case 227:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9459 "Parser/parser.cc"
    break;

  case 228:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9465 "Parser/parser.cc"
    break;

  case 229:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9474 "Parser/parser.cc"
    break;

  case 230:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9480 "Parser/parser.cc"
    break;

  case 231:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9486 "Parser/parser.cc"
    break;

  case 232:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9492 "Parser/parser.cc"
    break;

  case 233:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9498 "Parser/parser.cc"
    break;

  case 234:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9504 "Parser/parser.cc"
    break;

  case 235:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9510 "Parser/parser.cc"
    break;

  case 236:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9516 "Parser/parser.cc"
    break;

  case 238:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9522 "Parser/parser.cc"
    break;

  case 239:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9528 "Parser/parser.cc"
    break;

  case 240:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9534 "Parser/parser.cc"
    break;

  case 241:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9540 "Parser/parser.cc"
    break;

  case 242:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9546 "Parser/parser.cc"
    break;

  case 243:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9552 "Parser/parser.cc"
    break;

  case 244:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9558 "Parser/parser.cc"
    break;

  case 246:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9564 "Parser/parser.cc"
    break;

  case 247:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9570 "Parser/parser.cc"
    break;

  case 248:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9576 "Parser/parser.cc"
    break;

  case 250:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9582 "Parser/parser.cc"
    break;

  case 251:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9588 "Parser/parser.cc"
    break;

  case 252:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9594 "Parser/parser.cc"
    break;

  case 253:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9603 "Parser/parser.cc"
    break;

  case 254:
#line 1397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9609 "Parser/parser.cc"
    break;

  case 255:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9615 "Parser/parser.cc"
    break;

  case 256:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9621 "Parser/parser.cc"
    break;

  case 257:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9630 "Parser/parser.cc"
    break;

  case 258:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9636 "Parser/parser.cc"
    break;

  case 259:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9642 "Parser/parser.cc"
    break;

  case 260:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9648 "Parser/parser.cc"
    break;

  case 261:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9657 "Parser/parser.cc"
    break;

  case 262:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9663 "Parser/parser.cc"
    break;

  case 263:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9669 "Parser/parser.cc"
    break;

  case 265:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9688 "Parser/parser.cc"
    break;

  case 266:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9694 "Parser/parser.cc"
    break;

  case 267:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9703 "Parser/parser.cc"
    break;

  case 268:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9709 "Parser/parser.cc"
    break;

  case 269:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9715 "Parser/parser.cc"
    break;

  case 270:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9721 "Parser/parser.cc"
    break;

  case 271:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9727 "Parser/parser.cc"
    break;

  case 272:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9733 "Parser/parser.cc"
    break;

  case 273:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9739 "Parser/parser.cc"
    break;

  case 274:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9748 "Parser/parser.cc"
    break;

  case 275:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9757 "Parser/parser.cc"
    break;

  case 276:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9763 "Parser/parser.cc"
    break;

  case 277:
#line 1483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9772 "Parser/parser.cc"
    break;

  case 278:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9781 "Parser/parser.cc"
    break;

  case 279:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9787 "Parser/parser.cc"
    break;

  case 280:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9793 "Parser/parser.cc"
    break;

  case 281:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9799 "Parser/parser.cc"
    break;

  case 282:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9805 "Parser/parser.cc"
    break;

  case 283:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9811 "Parser/parser.cc"
    break;

  case 284:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9817 "Parser/parser.cc"
    break;

  case 285:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9823 "Parser/parser.cc"
    break;

  case 286:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9829 "Parser/parser.cc"
    break;

  case 287:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9838 "Parser/parser.cc"
    break;

  case 288:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9848 "Parser/parser.cc"
    break;

  case 289:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9854 "Parser/parser.cc"
    break;

  case 290:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9860 "Parser/parser.cc"
    break;

  case 291:
#line 1527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9869 "Parser/parser.cc"
    break;

  case 292:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9879 "Parser/parser.cc"
    break;

  case 293:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9885 "Parser/parser.cc"
    break;

  case 294:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9894 "Parser/parser.cc"
    break;

  case 295:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9904 "Parser/parser.cc"
    break;

  case 296:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9910 "Parser/parser.cc"
    break;

  case 297:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9916 "Parser/parser.cc"
    break;

  case 298:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9922 "Parser/parser.cc"
    break;

  case 299:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9928 "Parser/parser.cc"
    break;

  case 300:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9937 "Parser/parser.cc"
    break;

  case 301:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9947 "Parser/parser.cc"
    break;

  case 302:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9953 "Parser/parser.cc"
    break;

  case 303:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9962 "Parser/parser.cc"
    break;

  case 304:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9972 "Parser/parser.cc"
    break;

  case 305:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9978 "Parser/parser.cc"
    break;

  case 306:
#line 1588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9987 "Parser/parser.cc"
    break;

  case 307:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9997 "Parser/parser.cc"
    break;

  case 308:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10003 "Parser/parser.cc"
    break;

  case 309:
#line 1602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 10012 "Parser/parser.cc"
    break;

  case 310:
#line 1607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 10023 "Parser/parser.cc"
    break;

  case 313:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10029 "Parser/parser.cc"
    break;

  case 314:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10035 "Parser/parser.cc"
    break;

  case 315:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10041 "Parser/parser.cc"
    break;

  case 316:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10047 "Parser/parser.cc"
    break;

  case 317:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10053 "Parser/parser.cc"
    break;

  case 319:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10059 "Parser/parser.cc"
    break;

  case 320:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10065 "Parser/parser.cc"
    break;

  case 321:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10071 "Parser/parser.cc"
    break;

  case 322:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10077 "Parser/parser.cc"
    break;

  case 323:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10083 "Parser/parser.cc"
    break;

  case 324:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10089 "Parser/parser.cc"
    break;

  case 325:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10095 "Parser/parser.cc"
    break;

  case 326:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10101 "Parser/parser.cc"
    break;

  case 327:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10107 "Parser/parser.cc"
    break;

  case 328:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10113 "Parser/parser.cc"
    break;

  case 329:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10119 "Parser/parser.cc"
    break;

  case 330:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10125 "Parser/parser.cc"
    break;

  case 331:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10131 "Parser/parser.cc"
    break;

  case 332:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10137 "Parser/parser.cc"
    break;

  case 333:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10143 "Parser/parser.cc"
    break;

  case 334:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10149 "Parser/parser.cc"
    break;

  case 335:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10155 "Parser/parser.cc"
    break;

  case 336:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10161 "Parser/parser.cc"
    break;

  case 337:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10167 "Parser/parser.cc"
    break;

  case 338:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10173 "Parser/parser.cc"
    break;

  case 339:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 340:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 343:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 344:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10200 "Parser/parser.cc"
    break;

  case 345:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10206 "Parser/parser.cc"
    break;

  case 346:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10212 "Parser/parser.cc"
    break;

  case 349:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10218 "Parser/parser.cc"
    break;

  case 350:
#line 1732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10224 "Parser/parser.cc"
    break;

  case 353:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10230 "Parser/parser.cc"
    break;

  case 354:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10236 "Parser/parser.cc"
    break;

  case 355:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10242 "Parser/parser.cc"
    break;

  case 356:
#line 1751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10248 "Parser/parser.cc"
    break;

  case 357:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10254 "Parser/parser.cc"
    break;

  case 358:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10260 "Parser/parser.cc"
    break;

  case 359:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10266 "Parser/parser.cc"
    break;

  case 360:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10272 "Parser/parser.cc"
    break;

  case 361:
#line 1765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10278 "Parser/parser.cc"
    break;

  case 364:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10284 "Parser/parser.cc"
    break;

  case 365:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10290 "Parser/parser.cc"
    break;

  case 366:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10296 "Parser/parser.cc"
    break;

  case 367:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10302 "Parser/parser.cc"
    break;

  case 368:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10308 "Parser/parser.cc"
    break;

  case 369:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10314 "Parser/parser.cc"
    break;

  case 370:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10320 "Parser/parser.cc"
    break;

  case 371:
#line 1798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10326 "Parser/parser.cc"
    break;

  case 372:
#line 1803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10332 "Parser/parser.cc"
    break;

  case 373:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10338 "Parser/parser.cc"
    break;

  case 374:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10344 "Parser/parser.cc"
    break;

  case 375:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10350 "Parser/parser.cc"
    break;

  case 376:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10356 "Parser/parser.cc"
    break;

  case 377:
#line 1822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10362 "Parser/parser.cc"
    break;

  case 378:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10368 "Parser/parser.cc"
    break;

  case 379:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10374 "Parser/parser.cc"
    break;

  case 380:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10380 "Parser/parser.cc"
    break;

  case 381:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10386 "Parser/parser.cc"
    break;

  case 382:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10392 "Parser/parser.cc"
    break;

  case 383:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10398 "Parser/parser.cc"
    break;

  case 384:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10404 "Parser/parser.cc"
    break;

  case 385:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10410 "Parser/parser.cc"
    break;

  case 386:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10416 "Parser/parser.cc"
    break;

  case 388:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10422 "Parser/parser.cc"
    break;

  case 389:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10428 "Parser/parser.cc"
    break;

  case 390:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10434 "Parser/parser.cc"
    break;

  case 395:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10440 "Parser/parser.cc"
    break;

  case 396:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10446 "Parser/parser.cc"
    break;

  case 397:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10452 "Parser/parser.cc"
    break;

  case 398:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10458 "Parser/parser.cc"
    break;

  case 399:
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10464 "Parser/parser.cc"
    break;

  case 400:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10470 "Parser/parser.cc"
    break;

  case 401:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10476 "Parser/parser.cc"
    break;

  case 402:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10482 "Parser/parser.cc"
    break;

  case 405:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10488 "Parser/parser.cc"
    break;

  case 406:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10494 "Parser/parser.cc"
    break;

  case 407:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10503 "Parser/parser.cc"
    break;

  case 408:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10509 "Parser/parser.cc"
    break;

  case 409:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10515 "Parser/parser.cc"
    break;

  case 410:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10521 "Parser/parser.cc"
    break;

  case 411:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10530 "Parser/parser.cc"
    break;

  case 412:
#line 1928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10539 "Parser/parser.cc"
    break;

  case 413:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10545 "Parser/parser.cc"
    break;

  case 416:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10551 "Parser/parser.cc"
    break;

  case 417:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10557 "Parser/parser.cc"
    break;

  case 419:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10563 "Parser/parser.cc"
    break;

  case 420:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10569 "Parser/parser.cc"
    break;

  case 430:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10575 "Parser/parser.cc"
    break;

  case 431:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10581 "Parser/parser.cc"
    break;

  case 435:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10587 "Parser/parser.cc"
    break;

  case 437:
#line 2010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10593 "Parser/parser.cc"
    break;

  case 438:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10599 "Parser/parser.cc"
    break;

  case 439:
#line 2016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10605 "Parser/parser.cc"
    break;

  case 440:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10611 "Parser/parser.cc"
    break;

  case 441:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10617 "Parser/parser.cc"
    break;

  case 442:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10623 "Parser/parser.cc"
    break;

  case 443:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10629 "Parser/parser.cc"
    break;

  case 444:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10635 "Parser/parser.cc"
    break;

  case 446:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10641 "Parser/parser.cc"
    break;

  case 447:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10647 "Parser/parser.cc"
    break;

  case 448:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10653 "Parser/parser.cc"
    break;

  case 449:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10664 "Parser/parser.cc"
    break;

  case 450:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10670 "Parser/parser.cc"
    break;

  case 451:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10676 "Parser/parser.cc"
    break;

  case 452:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10682 "Parser/parser.cc"
    break;

  case 453:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10688 "Parser/parser.cc"
    break;

  case 454:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10694 "Parser/parser.cc"
    break;

  case 455:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 456:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10709 "Parser/parser.cc"
    break;

  case 457:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10718 "Parser/parser.cc"
    break;

  case 458:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10727 "Parser/parser.cc"
    break;

  case 459:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10738 "Parser/parser.cc"
    break;

  case 460:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10747 "Parser/parser.cc"
    break;

  case 461:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10753 "Parser/parser.cc"
    break;

  case 462:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10759 "Parser/parser.cc"
    break;

  case 463:
#line 2126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10765 "Parser/parser.cc"
    break;

  case 464:
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10773 "Parser/parser.cc"
    break;

  case 465:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10781 "Parser/parser.cc"
    break;

  case 466:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10787 "Parser/parser.cc"
    break;

  case 469:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10802 "Parser/parser.cc"
    break;

  case 470:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 471:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10814 "Parser/parser.cc"
    break;

  case 472:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10820 "Parser/parser.cc"
    break;

  case 473:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10826 "Parser/parser.cc"
    break;

  case 474:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10832 "Parser/parser.cc"
    break;

  case 480:
#line 2186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10842 "Parser/parser.cc"
    break;

  case 493:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10848 "Parser/parser.cc"
    break;

  case 496:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10854 "Parser/parser.cc"
    break;

  case 497:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 499:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10866 "Parser/parser.cc"
    break;

  case 500:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10872 "Parser/parser.cc"
    break;

  case 501:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10878 "Parser/parser.cc"
    break;

  case 502:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10884 "Parser/parser.cc"
    break;

  case 503:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 504:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10896 "Parser/parser.cc"
    break;

  case 506:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 507:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10908 "Parser/parser.cc"
    break;

  case 509:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10914 "Parser/parser.cc"
    break;

  case 510:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 10920 "Parser/parser.cc"
    break;

  case 511:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 10926 "Parser/parser.cc"
    break;

  case 512:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 10932 "Parser/parser.cc"
    break;

  case 513:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 10938 "Parser/parser.cc"
    break;

  case 514:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 10944 "Parser/parser.cc"
    break;

  case 515:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 10950 "Parser/parser.cc"
    break;

  case 516:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 10956 "Parser/parser.cc"
    break;

  case 517:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 10962 "Parser/parser.cc"
    break;

  case 518:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 10968 "Parser/parser.cc"
    break;

  case 519:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 520:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 10980 "Parser/parser.cc"
    break;

  case 521:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 10986 "Parser/parser.cc"
    break;

  case 522:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 10992 "Parser/parser.cc"
    break;

  case 523:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 10998 "Parser/parser.cc"
    break;

  case 524:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11004 "Parser/parser.cc"
    break;

  case 525:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11010 "Parser/parser.cc"
    break;

  case 526:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11016 "Parser/parser.cc"
    break;

  case 527:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11022 "Parser/parser.cc"
    break;

  case 528:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11028 "Parser/parser.cc"
    break;

  case 529:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11034 "Parser/parser.cc"
    break;

  case 530:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11040 "Parser/parser.cc"
    break;

  case 531:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11046 "Parser/parser.cc"
    break;

  case 532:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11052 "Parser/parser.cc"
    break;

  case 533:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11058 "Parser/parser.cc"
    break;

  case 534:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11064 "Parser/parser.cc"
    break;

  case 535:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11070 "Parser/parser.cc"
    break;

  case 536:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11076 "Parser/parser.cc"
    break;

  case 537:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11082 "Parser/parser.cc"
    break;

  case 538:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11088 "Parser/parser.cc"
    break;

  case 539:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11094 "Parser/parser.cc"
    break;

  case 540:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11100 "Parser/parser.cc"
    break;

  case 541:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11106 "Parser/parser.cc"
    break;

  case 542:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11112 "Parser/parser.cc"
    break;

  case 543:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11118 "Parser/parser.cc"
    break;

  case 544:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11124 "Parser/parser.cc"
    break;

  case 545:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11130 "Parser/parser.cc"
    break;

  case 546:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11136 "Parser/parser.cc"
    break;

  case 548:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11142 "Parser/parser.cc"
    break;

  case 550:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 551:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11154 "Parser/parser.cc"
    break;

  case 552:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11160 "Parser/parser.cc"
    break;

  case 554:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 555:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11172 "Parser/parser.cc"
    break;

  case 556:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11178 "Parser/parser.cc"
    break;

  case 557:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11184 "Parser/parser.cc"
    break;

  case 559:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11190 "Parser/parser.cc"
    break;

  case 561:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11196 "Parser/parser.cc"
    break;

  case 562:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11202 "Parser/parser.cc"
    break;

  case 563:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11208 "Parser/parser.cc"
    break;

  case 564:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11214 "Parser/parser.cc"
    break;

  case 565:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11220 "Parser/parser.cc"
    break;

  case 566:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11226 "Parser/parser.cc"
    break;

  case 567:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11232 "Parser/parser.cc"
    break;

  case 568:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11238 "Parser/parser.cc"
    break;

  case 569:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11244 "Parser/parser.cc"
    break;

  case 571:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11250 "Parser/parser.cc"
    break;

  case 572:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11256 "Parser/parser.cc"
    break;

  case 573:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11262 "Parser/parser.cc"
    break;

  case 575:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11268 "Parser/parser.cc"
    break;

  case 576:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11274 "Parser/parser.cc"
    break;

  case 577:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11283 "Parser/parser.cc"
    break;

  case 579:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11289 "Parser/parser.cc"
    break;

  case 580:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11295 "Parser/parser.cc"
    break;

  case 581:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11301 "Parser/parser.cc"
    break;

  case 583:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11307 "Parser/parser.cc"
    break;

  case 584:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11313 "Parser/parser.cc"
    break;

  case 586:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11319 "Parser/parser.cc"
    break;

  case 587:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11325 "Parser/parser.cc"
    break;

  case 588:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11331 "Parser/parser.cc"
    break;

  case 589:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11337 "Parser/parser.cc"
    break;

  case 590:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11343 "Parser/parser.cc"
    break;

  case 591:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11349 "Parser/parser.cc"
    break;

  case 592:
#line 2503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11355 "Parser/parser.cc"
    break;

  case 593:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11361 "Parser/parser.cc"
    break;

  case 594:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11367 "Parser/parser.cc"
    break;

  case 596:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11373 "Parser/parser.cc"
    break;

  case 597:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11379 "Parser/parser.cc"
    break;

  case 598:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11385 "Parser/parser.cc"
    break;

  case 599:
#line 2519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11391 "Parser/parser.cc"
    break;

  case 600:
#line 2521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11397 "Parser/parser.cc"
    break;

  case 605:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11403 "Parser/parser.cc"
    break;

  case 606:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11409 "Parser/parser.cc"
    break;

  case 607:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11418 "Parser/parser.cc"
    break;

  case 608:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11426 "Parser/parser.cc"
    break;

  case 609:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11435 "Parser/parser.cc"
    break;

  case 610:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11444 "Parser/parser.cc"
    break;

  case 611:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11453 "Parser/parser.cc"
    break;

  case 612:
#line 2566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11462 "Parser/parser.cc"
    break;

  case 614:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11468 "Parser/parser.cc"
    break;

  case 615:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11474 "Parser/parser.cc"
    break;

  case 616:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11484 "Parser/parser.cc"
    break;

  case 617:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11503 "Parser/parser.cc"
    break;

  case 620:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11509 "Parser/parser.cc"
    break;

  case 621:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11515 "Parser/parser.cc"
    break;

  case 622:
#line 2615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11521 "Parser/parser.cc"
    break;

  case 623:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11527 "Parser/parser.cc"
    break;

  case 624:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11533 "Parser/parser.cc"
    break;

  case 625:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11539 "Parser/parser.cc"
    break;

  case 626:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11548 "Parser/parser.cc"
    break;

  case 627:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11554 "Parser/parser.cc"
    break;

  case 628:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11563 "Parser/parser.cc"
    break;

  case 629:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11569 "Parser/parser.cc"
    break;

  case 630:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11578 "Parser/parser.cc"
    break;

  case 631:
#line 2648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11584 "Parser/parser.cc"
    break;

  case 632:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11590 "Parser/parser.cc"
    break;

  case 633:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11603 "Parser/parser.cc"
    break;

  case 634:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11612 "Parser/parser.cc"
    break;

  case 635:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11618 "Parser/parser.cc"
    break;

  case 636:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11624 "Parser/parser.cc"
    break;

  case 637:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11637 "Parser/parser.cc"
    break;

  case 638:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11643 "Parser/parser.cc"
    break;

  case 641:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11649 "Parser/parser.cc"
    break;

  case 642:
#line 2688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11655 "Parser/parser.cc"
    break;

  case 645:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11661 "Parser/parser.cc"
    break;

  case 647:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11667 "Parser/parser.cc"
    break;

  case 648:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11673 "Parser/parser.cc"
    break;

  case 649:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11679 "Parser/parser.cc"
    break;

  case 650:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11685 "Parser/parser.cc"
    break;

  case 651:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11691 "Parser/parser.cc"
    break;

  case 652:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11697 "Parser/parser.cc"
    break;

  case 654:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11703 "Parser/parser.cc"
    break;

  case 656:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11709 "Parser/parser.cc"
    break;

  case 657:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11715 "Parser/parser.cc"
    break;

  case 659:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11721 "Parser/parser.cc"
    break;

  case 660:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11727 "Parser/parser.cc"
    break;

  case 662:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11733 "Parser/parser.cc"
    break;

  case 663:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11744 "Parser/parser.cc"
    break;

  case 664:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11758 "Parser/parser.cc"
    break;

  case 665:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11764 "Parser/parser.cc"
    break;

  case 666:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11770 "Parser/parser.cc"
    break;

  case 667:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11776 "Parser/parser.cc"
    break;

  case 668:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11787 "Parser/parser.cc"
    break;

  case 669:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11793 "Parser/parser.cc"
    break;

  case 670:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11799 "Parser/parser.cc"
    break;

  case 672:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11805 "Parser/parser.cc"
    break;

  case 673:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11811 "Parser/parser.cc"
    break;

  case 674:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11817 "Parser/parser.cc"
    break;

  case 675:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11823 "Parser/parser.cc"
    break;

  case 676:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11832 "Parser/parser.cc"
    break;

  case 677:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11841 "Parser/parser.cc"
    break;

  case 678:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11847 "Parser/parser.cc"
    break;

  case 679:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11857 "Parser/parser.cc"
    break;

  case 680:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11863 "Parser/parser.cc"
    break;

  case 681:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 11869 "Parser/parser.cc"
    break;

  case 683:
#line 2844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11875 "Parser/parser.cc"
    break;

  case 684:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11881 "Parser/parser.cc"
    break;

  case 685:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11887 "Parser/parser.cc"
    break;

  case 686:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11893 "Parser/parser.cc"
    break;

  case 687:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11899 "Parser/parser.cc"
    break;

  case 688:
#line 2862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11905 "Parser/parser.cc"
    break;

  case 690:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11911 "Parser/parser.cc"
    break;

  case 693:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11917 "Parser/parser.cc"
    break;

  case 694:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11923 "Parser/parser.cc"
    break;

  case 695:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 11929 "Parser/parser.cc"
    break;

  case 696:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11935 "Parser/parser.cc"
    break;

  case 699:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11941 "Parser/parser.cc"
    break;

  case 700:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11947 "Parser/parser.cc"
    break;

  case 701:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11953 "Parser/parser.cc"
    break;

  case 703:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11959 "Parser/parser.cc"
    break;

  case 704:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11965 "Parser/parser.cc"
    break;

  case 705:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 11971 "Parser/parser.cc"
    break;

  case 707:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 11977 "Parser/parser.cc"
    break;

  case 708:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11983 "Parser/parser.cc"
    break;

  case 709:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11989 "Parser/parser.cc"
    break;

  case 710:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11995 "Parser/parser.cc"
    break;

  case 711:
#line 2925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12001 "Parser/parser.cc"
    break;

  case 713:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12007 "Parser/parser.cc"
    break;

  case 714:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12013 "Parser/parser.cc"
    break;

  case 715:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12019 "Parser/parser.cc"
    break;

  case 720:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12025 "Parser/parser.cc"
    break;

  case 722:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12031 "Parser/parser.cc"
    break;

  case 723:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12037 "Parser/parser.cc"
    break;

  case 726:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12043 "Parser/parser.cc"
    break;

  case 729:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12049 "Parser/parser.cc"
    break;

  case 730:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12055 "Parser/parser.cc"
    break;

  case 731:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12061 "Parser/parser.cc"
    break;

  case 732:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12067 "Parser/parser.cc"
    break;

  case 733:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12073 "Parser/parser.cc"
    break;

  case 734:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12079 "Parser/parser.cc"
    break;

  case 735:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12085 "Parser/parser.cc"
    break;

  case 737:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12091 "Parser/parser.cc"
    break;

  case 738:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12097 "Parser/parser.cc"
    break;

  case 739:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12103 "Parser/parser.cc"
    break;

  case 741:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12109 "Parser/parser.cc"
    break;

  case 743:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12115 "Parser/parser.cc"
    break;

  case 744:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12121 "Parser/parser.cc"
    break;

  case 745:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12127 "Parser/parser.cc"
    break;

  case 746:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12133 "Parser/parser.cc"
    break;

  case 747:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12139 "Parser/parser.cc"
    break;

  case 748:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12145 "Parser/parser.cc"
    break;

  case 750:
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12151 "Parser/parser.cc"
    break;

  case 751:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12157 "Parser/parser.cc"
    break;

  case 752:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12163 "Parser/parser.cc"
    break;

  case 753:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12174 "Parser/parser.cc"
    break;

  case 754:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12180 "Parser/parser.cc"
    break;

  case 755:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12186 "Parser/parser.cc"
    break;

  case 756:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12192 "Parser/parser.cc"
    break;

  case 757:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12201 "Parser/parser.cc"
    break;

  case 758:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12207 "Parser/parser.cc"
    break;

  case 759:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12217 "Parser/parser.cc"
    break;

  case 760:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12223 "Parser/parser.cc"
    break;

  case 761:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12229 "Parser/parser.cc"
    break;

  case 762:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12235 "Parser/parser.cc"
    break;

  case 763:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12241 "Parser/parser.cc"
    break;

  case 764:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12247 "Parser/parser.cc"
    break;

  case 765:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12253 "Parser/parser.cc"
    break;

  case 766:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12259 "Parser/parser.cc"
    break;

  case 767:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12265 "Parser/parser.cc"
    break;

  case 768:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12271 "Parser/parser.cc"
    break;

  case 771:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12277 "Parser/parser.cc"
    break;

  case 772:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12283 "Parser/parser.cc"
    break;

  case 773:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12289 "Parser/parser.cc"
    break;

  case 774:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12295 "Parser/parser.cc"
    break;

  case 776:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12301 "Parser/parser.cc"
    break;

  case 777:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12307 "Parser/parser.cc"
    break;

  case 778:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12313 "Parser/parser.cc"
    break;

  case 779:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12319 "Parser/parser.cc"
    break;

  case 780:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12325 "Parser/parser.cc"
    break;

  case 781:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12331 "Parser/parser.cc"
    break;

  case 782:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12337 "Parser/parser.cc"
    break;

  case 783:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12346 "Parser/parser.cc"
    break;

  case 784:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12355 "Parser/parser.cc"
    break;

  case 785:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12364 "Parser/parser.cc"
    break;

  case 786:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12370 "Parser/parser.cc"
    break;

  case 787:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12379 "Parser/parser.cc"
    break;

  case 788:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12385 "Parser/parser.cc"
    break;

  case 790:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12391 "Parser/parser.cc"
    break;

  case 795:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12397 "Parser/parser.cc"
    break;

  case 796:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12403 "Parser/parser.cc"
    break;

  case 797:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12409 "Parser/parser.cc"
    break;

  case 799:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12415 "Parser/parser.cc"
    break;

  case 800:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12421 "Parser/parser.cc"
    break;

  case 801:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12427 "Parser/parser.cc"
    break;

  case 802:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12433 "Parser/parser.cc"
    break;

  case 804:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12439 "Parser/parser.cc"
    break;

  case 805:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12445 "Parser/parser.cc"
    break;

  case 806:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12451 "Parser/parser.cc"
    break;

  case 807:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12467 "Parser/parser.cc"
    break;

  case 808:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12473 "Parser/parser.cc"
    break;

  case 809:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12479 "Parser/parser.cc"
    break;

  case 810:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12485 "Parser/parser.cc"
    break;

  case 811:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12491 "Parser/parser.cc"
    break;

  case 812:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12497 "Parser/parser.cc"
    break;

  case 813:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12503 "Parser/parser.cc"
    break;

  case 815:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12512 "Parser/parser.cc"
    break;

  case 816:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12518 "Parser/parser.cc"
    break;

  case 817:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12527 "Parser/parser.cc"
    break;

  case 818:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12537 "Parser/parser.cc"
    break;

  case 819:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12546 "Parser/parser.cc"
    break;

  case 820:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12556 "Parser/parser.cc"
    break;

  case 821:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12567 "Parser/parser.cc"
    break;

  case 822:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12577 "Parser/parser.cc"
    break;

  case 823:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12588 "Parser/parser.cc"
    break;

  case 824:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12598 "Parser/parser.cc"
    break;

  case 825:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12609 "Parser/parser.cc"
    break;

  case 826:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12619 "Parser/parser.cc"
    break;

  case 828:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12625 "Parser/parser.cc"
    break;

  case 829:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12631 "Parser/parser.cc"
    break;

  case 830:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12637 "Parser/parser.cc"
    break;

  case 831:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12649 "Parser/parser.cc"
    break;

  case 832:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12660 "Parser/parser.cc"
    break;

  case 833:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12669 "Parser/parser.cc"
    break;

  case 834:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12678 "Parser/parser.cc"
    break;

  case 835:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12684 "Parser/parser.cc"
    break;

  case 836:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12690 "Parser/parser.cc"
    break;

  case 837:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12696 "Parser/parser.cc"
    break;

  case 838:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12705 "Parser/parser.cc"
    break;

  case 839:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12711 "Parser/parser.cc"
    break;

  case 840:
#line 3412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12717 "Parser/parser.cc"
    break;

  case 841:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12723 "Parser/parser.cc"
    break;

  case 846:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12729 "Parser/parser.cc"
    break;

  case 847:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12735 "Parser/parser.cc"
    break;

  case 848:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12745 "Parser/parser.cc"
    break;

  case 849:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12751 "Parser/parser.cc"
    break;

  case 852:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12757 "Parser/parser.cc"
    break;

  case 853:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12763 "Parser/parser.cc"
    break;

  case 854:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12769 "Parser/parser.cc"
    break;

  case 855:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12775 "Parser/parser.cc"
    break;

  case 857:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 858:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12787 "Parser/parser.cc"
    break;

  case 859:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12793 "Parser/parser.cc"
    break;

  case 860:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12799 "Parser/parser.cc"
    break;

  case 862:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12805 "Parser/parser.cc"
    break;

  case 863:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12811 "Parser/parser.cc"
    break;

  case 864:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12817 "Parser/parser.cc"
    break;

  case 865:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12823 "Parser/parser.cc"
    break;

  case 866:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12829 "Parser/parser.cc"
    break;

  case 868:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12835 "Parser/parser.cc"
    break;

  case 869:
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12841 "Parser/parser.cc"
    break;

  case 870:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12847 "Parser/parser.cc"
    break;

  case 871:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12853 "Parser/parser.cc"
    break;

  case 872:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 873:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12865 "Parser/parser.cc"
    break;

  case 874:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12871 "Parser/parser.cc"
    break;

  case 875:
#line 3551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 876:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12883 "Parser/parser.cc"
    break;

  case 877:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12889 "Parser/parser.cc"
    break;

  case 878:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12895 "Parser/parser.cc"
    break;

  case 879:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12901 "Parser/parser.cc"
    break;

  case 880:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12907 "Parser/parser.cc"
    break;

  case 881:
#line 3566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12913 "Parser/parser.cc"
    break;

  case 882:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12919 "Parser/parser.cc"
    break;

  case 883:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12925 "Parser/parser.cc"
    break;

  case 884:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12931 "Parser/parser.cc"
    break;

  case 885:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12937 "Parser/parser.cc"
    break;

  case 887:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12943 "Parser/parser.cc"
    break;

  case 888:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12949 "Parser/parser.cc"
    break;

  case 889:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12955 "Parser/parser.cc"
    break;

  case 890:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 12961 "Parser/parser.cc"
    break;

  case 891:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12967 "Parser/parser.cc"
    break;

  case 892:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12973 "Parser/parser.cc"
    break;

  case 893:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12979 "Parser/parser.cc"
    break;

  case 894:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12985 "Parser/parser.cc"
    break;

  case 895:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12991 "Parser/parser.cc"
    break;

  case 896:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12997 "Parser/parser.cc"
    break;

  case 897:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13003 "Parser/parser.cc"
    break;

  case 898:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13009 "Parser/parser.cc"
    break;

  case 899:
#line 3617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13015 "Parser/parser.cc"
    break;

  case 900:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13021 "Parser/parser.cc"
    break;

  case 901:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13027 "Parser/parser.cc"
    break;

  case 902:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13033 "Parser/parser.cc"
    break;

  case 906:
#line 3641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13039 "Parser/parser.cc"
    break;

  case 907:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13045 "Parser/parser.cc"
    break;

  case 908:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13051 "Parser/parser.cc"
    break;

  case 909:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13057 "Parser/parser.cc"
    break;

  case 910:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13063 "Parser/parser.cc"
    break;

  case 911:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13069 "Parser/parser.cc"
    break;

  case 912:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13075 "Parser/parser.cc"
    break;

  case 913:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13081 "Parser/parser.cc"
    break;

  case 914:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13087 "Parser/parser.cc"
    break;

  case 915:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13093 "Parser/parser.cc"
    break;

  case 916:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13099 "Parser/parser.cc"
    break;

  case 917:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13105 "Parser/parser.cc"
    break;

  case 918:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13111 "Parser/parser.cc"
    break;

  case 919:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13117 "Parser/parser.cc"
    break;

  case 920:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13123 "Parser/parser.cc"
    break;

  case 921:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13132 "Parser/parser.cc"
    break;

  case 922:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13138 "Parser/parser.cc"
    break;

  case 923:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13144 "Parser/parser.cc"
    break;

  case 925:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13150 "Parser/parser.cc"
    break;

  case 926:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13156 "Parser/parser.cc"
    break;

  case 927:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13162 "Parser/parser.cc"
    break;

  case 928:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13168 "Parser/parser.cc"
    break;

  case 929:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13174 "Parser/parser.cc"
    break;

  case 930:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13180 "Parser/parser.cc"
    break;

  case 931:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13186 "Parser/parser.cc"
    break;

  case 932:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13192 "Parser/parser.cc"
    break;

  case 933:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13198 "Parser/parser.cc"
    break;

  case 934:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13204 "Parser/parser.cc"
    break;

  case 935:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13210 "Parser/parser.cc"
    break;

  case 936:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13216 "Parser/parser.cc"
    break;

  case 937:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13222 "Parser/parser.cc"
    break;

  case 938:
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13228 "Parser/parser.cc"
    break;

  case 939:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13234 "Parser/parser.cc"
    break;

  case 940:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13240 "Parser/parser.cc"
    break;

  case 941:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13246 "Parser/parser.cc"
    break;

  case 942:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13252 "Parser/parser.cc"
    break;

  case 944:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13258 "Parser/parser.cc"
    break;

  case 945:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13264 "Parser/parser.cc"
    break;

  case 946:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13270 "Parser/parser.cc"
    break;

  case 947:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13276 "Parser/parser.cc"
    break;

  case 948:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13282 "Parser/parser.cc"
    break;

  case 949:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13288 "Parser/parser.cc"
    break;

  case 950:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13294 "Parser/parser.cc"
    break;

  case 951:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13300 "Parser/parser.cc"
    break;

  case 952:
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13306 "Parser/parser.cc"
    break;

  case 953:
#line 3777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13312 "Parser/parser.cc"
    break;

  case 954:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13318 "Parser/parser.cc"
    break;

  case 955:
#line 3784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13324 "Parser/parser.cc"
    break;

  case 956:
#line 3786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13330 "Parser/parser.cc"
    break;

  case 957:
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13336 "Parser/parser.cc"
    break;

  case 958:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13342 "Parser/parser.cc"
    break;

  case 959:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13348 "Parser/parser.cc"
    break;

  case 960:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13354 "Parser/parser.cc"
    break;

  case 961:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13361 "Parser/parser.cc"
    break;

  case 963:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13367 "Parser/parser.cc"
    break;

  case 964:
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13373 "Parser/parser.cc"
    break;

  case 965:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13379 "Parser/parser.cc"
    break;

  case 966:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13385 "Parser/parser.cc"
    break;

  case 967:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13391 "Parser/parser.cc"
    break;

  case 968:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13397 "Parser/parser.cc"
    break;

  case 969:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13403 "Parser/parser.cc"
    break;

  case 970:
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13409 "Parser/parser.cc"
    break;

  case 971:
#line 3830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13415 "Parser/parser.cc"
    break;

  case 972:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13421 "Parser/parser.cc"
    break;

  case 973:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13427 "Parser/parser.cc"
    break;

  case 974:
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13433 "Parser/parser.cc"
    break;

  case 975:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13439 "Parser/parser.cc"
    break;

  case 976:
#line 3855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13446 "Parser/parser.cc"
    break;

  case 978:
#line 3859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13452 "Parser/parser.cc"
    break;

  case 979:
#line 3861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13458 "Parser/parser.cc"
    break;

  case 980:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13464 "Parser/parser.cc"
    break;

  case 981:
#line 3868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13470 "Parser/parser.cc"
    break;

  case 982:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13476 "Parser/parser.cc"
    break;

  case 983:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13482 "Parser/parser.cc"
    break;

  case 984:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13488 "Parser/parser.cc"
    break;

  case 985:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13494 "Parser/parser.cc"
    break;

  case 986:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13500 "Parser/parser.cc"
    break;

  case 987:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13506 "Parser/parser.cc"
    break;

  case 988:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13512 "Parser/parser.cc"
    break;

  case 990:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13518 "Parser/parser.cc"
    break;

  case 991:
#line 3911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13524 "Parser/parser.cc"
    break;

  case 992:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13530 "Parser/parser.cc"
    break;

  case 993:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13536 "Parser/parser.cc"
    break;

  case 994:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13542 "Parser/parser.cc"
    break;

  case 995:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13548 "Parser/parser.cc"
    break;

  case 996:
#line 3924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13554 "Parser/parser.cc"
    break;

  case 998:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13560 "Parser/parser.cc"
    break;

  case 999:
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13566 "Parser/parser.cc"
    break;

  case 1000:
#line 3934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13572 "Parser/parser.cc"
    break;

  case 1001:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13578 "Parser/parser.cc"
    break;

  case 1002:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13584 "Parser/parser.cc"
    break;

  case 1003:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13590 "Parser/parser.cc"
    break;

  case 1004:
#line 3949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13596 "Parser/parser.cc"
    break;

  case 1005:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13602 "Parser/parser.cc"
    break;

  case 1006:
#line 3954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13608 "Parser/parser.cc"
    break;

  case 1007:
#line 3961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13614 "Parser/parser.cc"
    break;

  case 1009:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13620 "Parser/parser.cc"
    break;

  case 1010:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13626 "Parser/parser.cc"
    break;

  case 1012:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13632 "Parser/parser.cc"
    break;

  case 1013:
#line 3979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13638 "Parser/parser.cc"
    break;

  case 1015:
#line 3985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13644 "Parser/parser.cc"
    break;

  case 1016:
#line 3987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13650 "Parser/parser.cc"
    break;

  case 1017:
#line 3992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13656 "Parser/parser.cc"
    break;

  case 1018:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13662 "Parser/parser.cc"
    break;

  case 1019:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13668 "Parser/parser.cc"
    break;

  case 1020:
#line 3998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13674 "Parser/parser.cc"
    break;

  case 1021:
#line 4032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13680 "Parser/parser.cc"
    break;

  case 1024:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13687 "Parser/parser.cc"
    break;

  case 1025:
#line 4042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13693 "Parser/parser.cc"
    break;

  case 1026:
#line 4044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13699 "Parser/parser.cc"
    break;

  case 1027:
#line 4049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13705 "Parser/parser.cc"
    break;

  case 1028:
#line 4051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13711 "Parser/parser.cc"
    break;

  case 1029:
#line 4053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13717 "Parser/parser.cc"
    break;

  case 1030:
#line 4055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13723 "Parser/parser.cc"
    break;

  case 1031:
#line 4057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13729 "Parser/parser.cc"
    break;

  case 1033:
#line 4063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13735 "Parser/parser.cc"
    break;

  case 1034:
#line 4065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13741 "Parser/parser.cc"
    break;

  case 1035:
#line 4067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13747 "Parser/parser.cc"
    break;

  case 1036:
#line 4072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13753 "Parser/parser.cc"
    break;

  case 1037:
#line 4074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13759 "Parser/parser.cc"
    break;

  case 1038:
#line 4076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13765 "Parser/parser.cc"
    break;

  case 1040:
#line 4083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13771 "Parser/parser.cc"
    break;

  case 1042:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13777 "Parser/parser.cc"
    break;

  case 1043:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13783 "Parser/parser.cc"
    break;

  case 1044:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13789 "Parser/parser.cc"
    break;

  case 1045:
#line 4102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13795 "Parser/parser.cc"
    break;

  case 1046:
#line 4104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13801 "Parser/parser.cc"
    break;

  case 1047:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13807 "Parser/parser.cc"
    break;

  case 1049:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13813 "Parser/parser.cc"
    break;

  case 1050:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13819 "Parser/parser.cc"
    break;

  case 1051:
#line 4128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13825 "Parser/parser.cc"
    break;

  case 1052:
#line 4130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13831 "Parser/parser.cc"
    break;

  case 1053:
#line 4132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13837 "Parser/parser.cc"
    break;

  case 1054:
#line 4134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13843 "Parser/parser.cc"
    break;

  case 1055:
#line 4136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13849 "Parser/parser.cc"
    break;

  case 1057:
#line 4142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13855 "Parser/parser.cc"
    break;

  case 1058:
#line 4144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13861 "Parser/parser.cc"
    break;

  case 1059:
#line 4146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13867 "Parser/parser.cc"
    break;

  case 1060:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13873 "Parser/parser.cc"
    break;

  case 1061:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13879 "Parser/parser.cc"
    break;

  case 1064:
#line 4163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13885 "Parser/parser.cc"
    break;

  case 1067:
#line 4174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13891 "Parser/parser.cc"
    break;

  case 1068:
#line 4176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13897 "Parser/parser.cc"
    break;

  case 1069:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13903 "Parser/parser.cc"
    break;

  case 1070:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13909 "Parser/parser.cc"
    break;

  case 1071:
#line 4182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13915 "Parser/parser.cc"
    break;

  case 1072:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13921 "Parser/parser.cc"
    break;

  case 1073:
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13927 "Parser/parser.cc"
    break;

  case 1074:
#line 4193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13933 "Parser/parser.cc"
    break;

  case 1075:
#line 4195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13939 "Parser/parser.cc"
    break;

  case 1076:
#line 4197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13945 "Parser/parser.cc"
    break;

  case 1077:
#line 4199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13951 "Parser/parser.cc"
    break;

  case 1078:
#line 4202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13957 "Parser/parser.cc"
    break;

  case 1079:
#line 4204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13963 "Parser/parser.cc"
    break;

  case 1080:
#line 4206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13969 "Parser/parser.cc"
    break;

  case 1081:
#line 4208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13975 "Parser/parser.cc"
    break;

  case 1082:
#line 4210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13981 "Parser/parser.cc"
    break;

  case 1083:
#line 4215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13987 "Parser/parser.cc"
    break;

  case 1084:
#line 4217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13993 "Parser/parser.cc"
    break;

  case 1085:
#line 4222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13999 "Parser/parser.cc"
    break;

  case 1086:
#line 4224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14005 "Parser/parser.cc"
    break;

  case 1088:
#line 4251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14011 "Parser/parser.cc"
    break;

  case 1092:
#line 4262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14017 "Parser/parser.cc"
    break;

  case 1093:
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14023 "Parser/parser.cc"
    break;

  case 1094:
#line 4266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14029 "Parser/parser.cc"
    break;

  case 1095:
#line 4268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14035 "Parser/parser.cc"
    break;

  case 1096:
#line 4270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14041 "Parser/parser.cc"
    break;

  case 1097:
#line 4272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14047 "Parser/parser.cc"
    break;

  case 1098:
#line 4279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14053 "Parser/parser.cc"
    break;

  case 1099:
#line 4281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14059 "Parser/parser.cc"
    break;

  case 1100:
#line 4283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14065 "Parser/parser.cc"
    break;

  case 1101:
#line 4285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14071 "Parser/parser.cc"
    break;

  case 1102:
#line 4287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14077 "Parser/parser.cc"
    break;

  case 1103:
#line 4289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14083 "Parser/parser.cc"
    break;

  case 1104:
#line 4294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14089 "Parser/parser.cc"
    break;

  case 1105:
#line 4296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14095 "Parser/parser.cc"
    break;

  case 1106:
#line 4298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14101 "Parser/parser.cc"
    break;

  case 1107:
#line 4303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14107 "Parser/parser.cc"
    break;

  case 1108:
#line 4305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14113 "Parser/parser.cc"
    break;

  case 1109:
#line 4307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14119 "Parser/parser.cc"
    break;

  case 1112:
#line 4331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14125 "Parser/parser.cc"
    break;

  case 1113:
#line 4333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14131 "Parser/parser.cc"
    break;


#line 14135 "Parser/parser.cc"

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
#line 4336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
